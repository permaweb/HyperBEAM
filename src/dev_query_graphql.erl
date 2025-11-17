%%% @doc A GraphQL interface for querying a node's cache. Accessible through the
%%% `~query@1.0/graphql' device key.
-module(dev_query_graphql).
%%% AO-Core API:
-export([handle/3]).
%%% GraphQL Callbacks:
-export([execute/4]).
%%% Submodule helpers:
-export([keys_to_template/1, test_query/3, test_query/4]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%%% Constants.
-define(DEFAULT_QUERY_TIMEOUT, 10000).
-define(START_TIMEOUT, 3000).

%%% `Message' query keys.
-define(MESSAGE_QUERY_KEYS,
    [
        <<"id">>,
        <<"message">>,
        <<"keys">>,
        <<"tags">>,
        <<"name">>,
        <<"value">>,
        <<"cursor">>
    ]
).

%% @doc Returns the complete GraphQL schema.
schema() ->
    hb_util:ok(file:read_file("scripts/schema.gql")).

%% @doc Ensure that the GraphQL schema and context are initialized. Can be 
%% called many times.
ensure_started() -> ensure_started(#{}).
ensure_started(Opts) ->
    case hb_name:lookup(graphql_controller) of
        PID when is_pid(PID) -> ok;
        undefined ->
            Parent = self(),
            PID =
                spawn_link(
                    fun() ->
                        init(Opts),
                        Parent ! {started, self()},
                        receive stop -> ok end
                    end
                ),
            receive {started, PID} -> ok
            after ?START_TIMEOUT -> exit(graphql_start_timeout)
            end
    end.

%% @doc Initialize the GraphQL schema and context. Should only be called once.
init(_Opts) ->
    ?event(graphql_init_called),
    application:ensure_all_started(graphql),
    ?event(graphql_application_started),
    GraphQLOpts =
        #{
            scalars => #{ default => ?MODULE },
            interfaces => #{ default => ?MODULE },
            unions => #{ default => ?MODULE },
            objects => #{ default => ?MODULE },
            enums => #{ default => ?MODULE }
        },
    ok = graphql:load_schema(GraphQLOpts, schema()),
    ?event(graphql_schema_loaded),
    Root =
        {root,
            #{
                query => 'Query',
                interfaces => []
            }
        },
    ok = graphql:insert_schema_definition(Root),
    ?event(graphql_schema_definition_inserted),
    ok = graphql:validate_schema(),
    ?event(graphql_schema_validated),
    hb_name:register(graphql_controller, self()),
    ?event(graphql_controller_registered),
    ok.

handle(_Base, RawReq, Opts) ->
    ?event({request, RawReq}),
    Req =
        case hb_maps:find(<<"query">>, RawReq, Opts) of
            {ok, _} -> RawReq;
            error ->
                % Parse the query, assuming that the request body is a JSON
                % object with the necessary fields.
                hb_json:decode(hb_maps:get(<<"body">>, RawReq, <<>>, Opts))
        end,
    ?event({request, {processed, Req}}),
    Query = hb_maps:get(<<"query">>, Req, <<>>, Opts),
    OpName = hb_maps:get(<<"operationName">>, Req, undefined, Opts),
    Vars = 
        hb_message:remove_all_commitments(
            hb_maps:get(<<"variables">>, Req, #{}, Opts),
            Opts
        ),
    ?event(
        {graphql_run_called,
            {query, Query},
            {operation, OpName},
            {variables, Vars}
        }
    ),
    ensure_started(),
    case graphql:parse(Query) of
        {ok, AST} ->
            ?event(graphql_parsed),
            try
                ?event(graphql_type_checking),
                {ok, #{fun_env := FunEnv, ast := AST2 }} = graphql:type_check(AST),
                ?event(graphql_type_checked_successfully),
                ok = graphql:validate(AST2),
                ?event(graphql_validated),
                Coerced = graphql:type_check_params(FunEnv, OpName, Vars),
                ?event(graphql_type_checked_params),
                Ctx =
                    #{
                        params => Coerced,
                        operation_name => OpName,
                        default_timeout =>
                            hb_opts:get(
                                query_timeout,
                                ?DEFAULT_QUERY_TIMEOUT,
                                Opts
                            ),
                        opts => Opts
                    },
                ?event(graphql_context_created),
                Response = graphql:execute(Ctx, AST2),
                ?event(graphql_executed),
                JSON = hb_json:encode(Response),
                ?event({graphql_response, {bytes, byte_size(JSON)}}),
                {ok,
                    #{
                        <<"content-type">> => <<"application/json">>,
                        <<"body">> => JSON
                    }
                }
            catch
                throw:Error:Stacktrace ->
                    ?event({graphql_error, {error, Error}, {trace, Stacktrace}}),
                    {error, Error}
            end
    end.

%% @doc The main entrypoint for resolving GraphQL elements, called by the
%% GraphQL library. We split the resolution flows into two separated functions:
%% `message_query/4' for the HyperBEAM native API, and `dev_query_arweave:query/4'
%% for the Arweave-compatible API.
execute(#{opts := Opts}, Obj, Field, Args) ->
    ?event({graphql_query, {object, Obj}, {field, Field}, {args, Args}}),
    case lists:member(Field, ?MESSAGE_QUERY_KEYS) of
        true -> message_query(Obj, Field, Args, Opts);
        false -> dev_query_arweave:query(Obj, Field, Args, Opts)
    end.

%% @doc Handle a HyperBEAM `message' query.
message_query(Obj, <<"message">>, #{<<"keys">> := Keys}, Opts) ->
    Template = keys_to_template(Keys),
    ?event(
        {graphql_execute_called,
            {object, Obj},
            {field, <<"message">>},
            {raw_keys, Keys},
            {template, Template}
        }
    ),
    case hb_cache:match(Template, Opts) of
        {ok, [ID | _IDs]} ->
            ?event({graphql_cache_match_found, ID}),
            {ok, Msg} = hb_cache:read(ID, Opts),
            ?event({graphql_cache_read, Msg}),
            {ok, Msg};
        not_found ->
            ?event(graphql_cache_match_not_found),
            {ok, #{<<"id">> => <<"not-found">>, <<"keys">> => #{}}}
    end;
message_query(Msg, Field, _Args, Opts) when Field =:= <<"keys">>; Field =:= <<"tags">> ->
    OnlyKeys =
        hb_maps:to_list(
            hb_private:reset(
                hb_maps:without(
                    [<<"data">>, <<"body">>],
                    hb_message:uncommitted(Msg, Opts),
                    Opts
                )
            ),
            Opts
        ),
    ?event({message_query_keys_or_tags, {object, Msg}, {only_keys, OnlyKeys}}),
    Res = {
        ok,
        [
            {ok,
                #{
                    <<"name">> => Name,
                    <<"value">> => hb_cache:ensure_loaded(Value, Opts)
                }
            }
        ||
            {Name, Value} <- OnlyKeys
        ]
    },
    ?event({message_query_keys_or_tags_result, Res}),
    Res;
message_query(Msg, Field, _Args, Opts)
        when Field =:= <<"name">> orelse Field =:= <<"value">> ->
    ?event({message_query_name_or_value, {object, Msg}, {field, Field}}),
    {ok, hb_maps:get(Field, Msg, null, Opts)};
message_query(Msg = #{ <<"independent_hash">> := _ }, <<"id">>, _Args, Opts) ->
    {ok, hb_maps:get(<<"independent_hash">>, Msg, null, Opts)};
message_query(Msg, <<"id">>, _Args, Opts) ->
    ?event({message_query_id, {object, Msg}}),
    {ok, hb_message:id(Msg, all, Opts)};
message_query(_Msg, <<"cursor">>, _Args, _Opts) ->
    {ok, <<"">>};
message_query(_Obj, _Field, _, _) ->
    {ok, <<"Not found.">>}.

keys_to_template(Keys) ->
    maps:from_list(lists:foldl(
        fun(#{<<"name">> := Name, <<"value">> := Value}, Acc) ->
            [{Name, Value} | Acc];
        (#{<<"name">> := Name, <<"values">> := [Value]}, Acc) ->
            [{Name, Value} | Acc];
        (#{<<"name">> := Name, <<"values">> := Values}, _Acc) ->
            throw(
                {multivalue_tag_search_not_supported, #{
                    <<"name">> => Name,
                    <<"values">> => Values
                }}
            )
        end,
        [],
        Keys
    )).

%%% Test helpers.

test_query(Node, Query, Opts) ->
    test_query(Node, Query, undefined, Opts).
test_query(Node, Query, Variables, Opts) ->
    test_query(Node, Query, Variables, undefined, Opts).
test_query(Node, Query, Variables, OperationName, Opts) ->
    UnencodedPayload =
        maps:filter(
            fun(_, undefined) -> false;
                (_, _) -> true
            end,
            #{
                <<"query">> => Query,
                <<"variables">> => Variables,
                <<"operationName">> => OperationName
            }
        ),
    ?event({test_query_unencoded_payload, UnencodedPayload}),
    {ok, Res} =
        hb_http:post(
            Node,
            #{
                <<"path">> => <<"~query@1.0/graphql">>,
                <<"content-type">> => <<"application/json">>,
                <<"codec-device">> => <<"json@1.0">>,
                <<"body">> => hb_json:encode(UnencodedPayload)
            },
            Opts
        ),
    hb_json:decode(hb_maps:get(<<"body">>, Res, <<>>, Opts)).

%%% Tests

lookup_test() ->
    {ok, Opts, _} = dev_query:test_setup(),
    Node = hb_http_server:start_node(Opts),
    Query =
        <<""" 
            query GetMessage { 
                message(
                    keys: 
                        [
                            { 
                                name: "basic", 
                                value: "binary-value" 
                            }
                        ]
                ) {
                    id
                    keys {
                        name
                        value
                    }
                }
            }
        """>>,
    Res = test_query(Node, Query, Opts),
    ?event({test_response, Res}),
    ?assertMatch(
        #{ <<"data">> := 
            #{ 
                <<"message">> :=
                    #{ 
                        <<"id">> := _,
                        <<"keys">> := 
                            [
                                #{ 
                                    <<"name">> := <<"basic">>,
                                    <<"value">> := <<"binary-value">>
                                },
                                #{ 
                                    <<"name">> := <<"basic-2">>,
                                    <<"value">> := <<"binary-value-2">> 
                                }
                            ] 
                    } 
            } 
        },
        Res
    ).

%%% Tests for the GraphQL interface of the dev_query module.
%%% This test checks if the GraphQL query can be executed with variables.
%%% NEED_TO_BE_FIXED: due to `application:ensure_all_started(graphql)` in `run/4`,
%%% only one test can be run at a time, as it will load the schema and context.
lookup_with_vars_test() ->
    {ok, Opts, _} = dev_query:test_setup(),
    Node = hb_http_server:start_node(Opts),
    Body =
        #{
            <<"path">> => <<"~query@1.0/graphql">>,
            <<"content-type">> => <<"application/json">>,
            <<"codec-device">> => <<"json@1.0">>,
            <<"body">> =>
                hb_json:encode(#{
                    <<"query">> => 
                        <<""" 
                            query GetMessage($keys: [KeyInput]) { 
                                message(
                                    keys: $keys
                                ) {
                                    id
                                    keys {
                                        name
                                        value
                                    }
                                }
                            }
                        """>>,
                    <<"operationName">> => <<"GetMessage">>,
                    <<"variables">> => #{
                        <<"keys">> => 
                            [
                                #{
                                    <<"name">> => <<"basic">>,
                                    <<"value">> => <<"binary-value">>
                                }
                            ]
                    }
                })
        },
    {ok, Res} =
        hb_http:post(
            Node,
            Body,            
            Opts
        ),
    Object = hb_json:decode(hb_maps:get(<<"body">>, Res, <<>>, Opts)),
    ?event({test_response, Object}),
    ?assertMatch(
        #{ <<"data">> := 
            #{ 
                <<"message">> :=
                    #{ 
                        <<"id">> := _,
                        <<"keys">> := 
                            [
                                #{ 
                                    <<"name">> := <<"basic">>,
                                    <<"value">> := <<"binary-value">>
                                },
                                #{ 
                                    <<"name">> := <<"basic-2">>,
                                    <<"value">> := <<"binary-value-2">> 
                                }
                            ] 
                    } 
            } 
        },
        Object
    ).

lookup_without_opname_test() ->
    {ok, Opts, _} = dev_query:test_setup(),
    Node = hb_http_server:start_node(Opts),
    {ok, Res} =
        hb_http:post(
            Node,
            #{
                <<"path">> => <<"~query@1.0/graphql">>,
                <<"content-type">> => <<"application/json">>,
                <<"codec-device">> => <<"json@1.0">>,
                <<"body">> =>
                    hb_json:encode(#{
                        <<"query">> => 
                            <<""" 
                                query($keys: [KeyInput]) { 
                                    message(
                                        keys: $keys
                                    ) {
                                        id
                                        keys {
                                            name
                                            value
                                        }
                                    }
                                }
                            """>>,
                        <<"variables">> => #{
                            <<"keys">> => 
                                [
                                    #{
                                        <<"name">> => <<"basic">>,
                                        <<"value">> => <<"binary-value">>
                                    }
                                ]
                        }
                    })
            },
            Opts
        ),
    Object = hb_json:decode(hb_maps:get(<<"body">>, Res, <<>>, Opts)),
    ?event({test_response, Object}),
    ?assertMatch(
        #{ <<"data">> := 
            #{ 
                <<"message">> :=
                    #{ 
                        <<"id">> := _,
                        <<"keys">> := 
                            [
                                #{ 
                                    <<"name">> := <<"basic">>,
                                    <<"value">> := <<"binary-value">>
                                },
                                #{ 
                                    <<"name">> := <<"basic-2">>,
                                    <<"value">> := <<"binary-value-2">> 
                                }
                            ] 
                    } 
            } 
        },
        Object
    ).