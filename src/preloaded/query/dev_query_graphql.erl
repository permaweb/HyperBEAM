%%% @doc A GraphQL interface for querying a node's cache. Accessible through the
%%% `~query@1.0/graphql' device key.
-module(dev_query_graphql).
%%% AO-Core API:
-export([handle/3]).
%%% GraphQL Callbacks:
-export([execute/4, input/2]).
%%% Submodule helpers:
-export([keys_to_template/1, test_query/3, test_query/4]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("graphql/include/graphql.hrl").
-include_lib("graphql/src/graphql_internal.hrl").
-include_lib("graphql/src/graphql_schema.hrl").
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
        <<"name">>,
        <<"value">>,
        <<"cursor">>
    ]
).

%% @doc Returns the complete GraphQL schema.
schema() ->
    hb_util:ok(file:read_file("scripts/schema.gql")).

%% @doc Ensure that the GraphQL schema and context are initialized. Safe to
%% call many times, including concurrently. `hb_name:singleton/2' guarantees
%% only one process runs `init/1' -- the `graphql' library rejects a second
%% `load_schema' with `entry_already_exists_in_schema'. A `persistent_term'
%% flag lets callers wait for init to finish: `singleton/2' returns as soon
%% as the spawned process is registered, which is before `init/1' completes.
%% All packages of this device share the flag and the controller, as the
%% `graphql' library keeps one schema. The schema calls the module of the
%% package that ran `init/1'.
ensure_started() -> ensure_started(#{}).
ensure_started(Opts) ->
    case persistent_term:get({graphql_controller, ready}, false) of
        true -> ok;
        false ->
            hb_name:singleton(
                graphql_controller,
                fun() ->
                    init(Opts),
                    persistent_term:put({graphql_controller, ready}, true),
                    receive stop -> ok end
                end
            ),
            case hb_util:wait_until(
                    fun() ->
                        persistent_term:get({graphql_controller, ready}, false)
                    end,
                    ?START_TIMEOUT
                ) of
                true -> ok;
                false -> exit(graphql_start_timeout)
            end
    end.

%% @doc Initialize the GraphQL schema and context. Should only be called once.
init(_Opts) ->
    ?event(graphql_init_called),
    application:ensure_all_started(graphql),
    ?event(graphql_application_started),
    Mod = callback_module(),
    GraphQLOpts =
        #{
            scalars => #{ default => Mod },
            interfaces => #{ default => Mod },
            unions => #{ default => Mod },
            objects => #{ default => Mod },
            enums => #{ default => Mod }
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
    ok.

%% @doc Return the module name after packaging/renaming.
callback_module() ->
    {module, Mod} = erlang:fun_info(fun input/2, module),
    Mod.

handle(_Base, RawReq, Opts) ->
    ?event({request, RawReq}),
    Req =
        case hb_maps:find(<<"query">>, RawReq, Opts) of
            {ok, _} -> RawReq;
            error ->
                % Parse the query, assuming that the request body is a JSON
                % object with the necessary fields. Its fields are set over
                % the keys of the request. Any other body carries no query.
                Body = hb_maps:get(<<"body">>, RawReq, <<>>, Opts),
                try hb_json:decode(Body) of
                    Object when is_map(Object) ->
                        {ok, Decoded} =
                            hb_ao:raw(
                                <<"structured@1.0">>,
                                <<"to">>,
                                Object,
                                #{},
                                Opts
                            ),
                        hb_maps:merge(RawReq, Decoded, Opts);
                    _ -> #{}
                catch error:_ -> #{}
                end
        end,
    ?event({request, {processed, Req}}),
    Query =
        case hb_maps:get(<<"query">>, Req, <<>>, Opts) of
            QueryBin when is_binary(QueryBin) -> QueryBin;
            _ -> <<>>
        end,
    OpName = 
        case hb_maps:get(<<"operationname">>, Req, undefined, Opts) of
            Name when is_binary(Name) -> Name;
            _ -> undefined
        end,
    % The `graphql' library takes the variables as Erlang terms without links.
    Vars = 
        case hb_maps:get(<<"variables">>, Req, #{}, Opts) of
            RawVars when is_map(RawVars) ->
                hb_message:uncommitted_deep(
                    hb_cache:ensure_all_loaded(RawVars, Opts),
                    Opts
                );
            _ -> #{}
        end,
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
                Coerced =
                    graphql:type_check_params(
                        FunEnv,
                        OpName,
                        declared_variables(FunEnv, Vars)
                    ),
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
                        opts => Opts#{
                            <<"query-arweave-blocks">> => selects(<<"block">>, AST2),
                            <<"query-arweave-nodes">> => selects(<<"node">>, AST2)
                        },
                        req => Req
                    },
                ?event(graphql_context_created),
                Response =
                    case graphql:execute(Ctx, AST2) of
                        #{ errors := Errors } = Result ->
                            Result#{ errors := graphql:format_errors(Ctx, Errors) };
                        Result -> Result
                    end,
                ?event(graphql_executed),
                JSON = hb_json:encode(typed_binaries(Response, Opts)),
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
                    query_error(Error, Opts)
            end;
        {error, Error} ->
            ?event({graphql_error, {error, Error}}),
            query_error(Error, Opts)
    end.

%% @doc Return parsing and validation failures in the GraphQL response format.
query_error(Error, Opts) ->
    Errors =
        case Error of
            {error, Details} -> graphql:format_errors(#{}, Details);
            {_Phase, {_Line, Module, Reason}} ->
                [#{ message => iolist_to_binary(Module:format_error(Reason)) }]
        end,
    {error, #{
        <<"status">> => 400,
        <<"content-type">> => <<"application/json">>,
        <<"body">> =>
            hb_json:encode(typed_binaries(#{ errors => Errors }, Opts))
    }}.

%% @doc The variables of a request under the names the document declares: the
%% request has them lowercase, and GraphQL variable names are case-sensitive.
declared_variables(FunEnv, Vars) ->
    maps:from_list(
        [
            {Name, Value}
        ||
            VarEnv <- maps:values(FunEnv),
            Name <- maps:keys(VarEnv),
            {ok, Value} <- [maps:find(hb_util:lowercase_key(Name), Vars)]
        ]
    ).

%% @doc Give each binary in a GraphQL response that is not UTF-8 text as its
%% base64url, typed `binary' in the `ao-types' of the object that contains it,
%% as `json@1.0' does. Every response name is kept, including one that starts
%% with `priv': the names are the query's fields and aliases, not message keys.
typed_binaries(Map, Opts) when is_map(Map) ->
    Typed = maps:map(fun(_, Value) -> typed_binaries(Value, Opts) end, Map),
    case maps:filter(fun(_, V) -> is_binary(V) andalso not is_text(V) end, Typed) of
        Binaries when map_size(Binaries) == 0 -> Typed;
        Binaries ->
            {ok, Types} =
                hb_ao:raw(
                    <<"structured@1.0">>,
                    <<"encode-types">>,
                    maps:map(fun(_, _) -> <<"binary">> end, Binaries),
                    #{},
                    Opts
                ),
            maps:merge(
                Typed#{ <<"ao-types">> => Types },
                maps:map(fun(_, Value) -> hb_util:encode(Value) end, Binaries)
            )
    end;
typed_binaries(List, Opts) when is_list(List) ->
    [typed_binaries(Value, Opts) || Value <- List];
typed_binaries(Value, _Opts) -> Value.

%% @doc Check whether a binary is UTF-8 text.
is_text(Bin) ->
    unicode:characters_to_binary(Bin) =:= Bin.

%% @doc Recognize selected fields, including aliases and fragments.
selects(Name, #document{ definitions = Definitions }) -> selects(Name, Definitions);
selects(_Name, []) -> false;
selects(Name, [#field{ selection_set = Selection } = Field | Rest]) ->
    graphql_ast:id(Field) =:= Name orelse
        selects(Name, Selection) orelse selects(Name, Rest);
selects(Name, [#op{ selection_set = Selection } | Rest]) ->
    selects(Name, Selection) orelse selects(Name, Rest);
selects(Name, [#frag{ selection_set = Selection } | Rest]) ->
    selects(Name, Selection) orelse selects(Name, Rest);
selects(Name, [_ | Rest]) -> selects(Name, Rest).

%% @doc The main entrypoint for resolving GraphQL elements, called by the
%% GraphQL library. We split the resolution flows into two separated functions:
%% `message_query/4' for the HyperBEAM native API, and `dev_query_arweave:query/4'
%% for the Arweave-compatible API.
execute(#{object_type := <<"Block">>, opts := Opts}, Block, <<"id">>, _Args) ->
    {ok, hb_maps:get(<<"indep-hash">>, Block, null, Opts)};
execute(#{object_type := Type}, _Obj, <<"id">>, _Args)
        when Type =:= <<"Bundle">>; Type =:= <<"Parent">> ->
    {ok, <<>>};
execute(Ctx = #{opts := Opts}, Obj, Field, Args) ->
    ?event({graphql_query, {object, Obj}, {field, Field}, {args, Args}}),
    case lists:member(Field, ?MESSAGE_QUERY_KEYS) of
        true -> message_query(Obj, Field, Args, Opts);
        false ->
            dev_query_arweave:query(
                Obj,
                Field,
                maps:merge(request_args(Ctx), Args),
                Opts
            )
    end.

request_args(Ctx) ->
    maps:with([<<"force-next-page">>], maps:get(req, Ctx, #{})).

%% @doc No-op on input validation.
input(_TypeID, Val) -> {ok, Val}.

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
        {error, not_found} ->
            ?event(graphql_cache_match_not_found),
            {ok, null}
    end;
message_query(Msg, <<"keys">>, _Args, Opts) ->
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
                    <<"value">> => field_value(Value, Opts)
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
message_query(Msg, <<"id">>, _Args, Opts) ->
    ?event({message_query_id, {object, Msg}}),
    {ok, hb_message:id(Msg, all, Opts)};
message_query(Msg, <<"cursor">>, _Args, Opts) ->
    case hb_maps:find(<<"cursor">>, Msg, Opts) of
        {ok, Cursor} -> {ok, Cursor};
        error -> {ok, hb_util:bin(hb_maps:get(<<"offset">>, Msg, <<>>, Opts))}
    end;
message_query(_Obj, _Field, _, _) ->
    {ok, <<"Not found.">>}.

%% @doc Submessages as IDs, resolving lazy value or ID holders alone.
field_value({link, _, #{ <<"lazy">> := true }} = Link, Opts) ->
    [Value] = maps:values(hb_link:normalize(#{ <<"value">> => Link }, discard, Opts)),
    field_value(hb_link:decode(Value), Opts);
field_value({link, <<"ao:", ID/binary>>, _}, _Opts) when ?IS_ID(ID) -> ID;
field_value({link, URI, _}, _Opts) -> URI;
field_value(Value, Opts) when is_map(Value); is_list(Value) ->
    hb_message:id(Value, all, Opts#{ <<"linkify-mode">> => discard });
field_value(Value, _Opts) -> Value.

keys_to_template(Keys) ->
    maps:from_list(lists:foldl(
        fun(#{<<"name">> := Name, <<"value">> := Value}, Acc) ->
            [{Name, Value} | Acc]
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
                <<"body">> => hb_json:encode(UnencodedPayload)
            },
            Opts
        ),
    hb_json:decode(hb_maps:get(<<"body">>, Res, <<>>, Opts)).

%%% Tests

graphql_request_errors_test() ->
    Opts = #{ <<"store">> => [hb_test_utils:test_store()],
        <<"priv-wallet">> => ar_wallet:new(), <<"port">> => 0 },
    Node = hb_http_server:start_node(Opts),
    try
        lists:foreach(
            fun(Query) ->
                {ok, 400, Headers, Body} = hackney:request(
                    post,
                    <<Node/binary, "/~query@1.0/graphql">>,
                    [{<<"content-type">>, <<"application/json">>}],
                    hb_json:encode(#{ <<"query">> => Query }),
                    [with_body]
                ),
                ?assertEqual(<<"application/json">>,
                    proplists:get_value(<<"content-type">>, Headers)),
                #{ <<"errors">> := [#{ <<"message">> := Message }] } =
                    hb_json:decode(Body),
                ?assert(byte_size(Message) > 0)
            end,
            [<<"{ notAField }">>, <<"query {">>, <<"{ ? }">>]
        )
    after
        cowboy:stop_listener(hb_opts:address(Opts))
    end.

%% @doc A second package of this device answers GraphQL queries after the first
%% package loaded the schema.
second_package_test() ->
    {ok, Opts, _} = dev_query:test_setup(),
    [#{ files := Files } = Group] =
        hb_packager:scan(
            [<<"src/preloaded">>],
            #{ <<"device-roots">> => [<<"dev_query">>] }
        ),
    % Package the device with one more source file: its modules get new names.
    Pkg =
        hb_packager:package(
            Group#{ files => Files#{ <<"second.txt">> => <<"second">> } },
            Opts
        ),
    {ok, ImplID} =
        hb_cache:write(
            hb_packager:impl_message(Pkg, <<"spec-id">>, Opts),
            Opts
        ),
    NodeOpts =
        Opts#{ <<"trusted-devices">> => #{ <<"second-query@1.0">> => ImplID } },
    Req =
        #{
            <<"path">> => <<"graphql/body">>,
            <<"query">> =>
                <<"{ message(keys: [{name: \"basic\", ",
                    "value: \"binary-value\"}]) { id } }">>
        },
    {ok, Body} =
        hb_ao:resolve(#{ <<"device">> => <<"query@1.0">> }, Req, NodeOpts),
    % The second package uses the controller and the schema of the first.
    ?assertEqual(
        {ok, Body},
        hb_ao:resolve(
            #{ <<"device">> => <<"second-query@1.0">> },
            Req,
            NodeOpts
        )
    ).

lookup_test() ->
    {ok, Opts, #{ <<"nested">> := NestedID }} = dev_query:test_setup(),
    {ok, Nested} = hb_cache:read(NestedID, Opts),
    lists:foreach(
        fun({Value, Expected}) ->
            ?assertEqual(
                {ok, [{ok, #{ <<"name">> => <<"value">>, <<"value">> => Expected }}]},
                execute(#{opts => Opts}, #{ <<"value">> => Value }, <<"keys">>, #{})
            )
        end,
        [
            {42, 42},
            {Nested, NestedID},
            {{link, <<"ao:", NestedID/binary>>, #{}}, NestedID}
        ]
    ),
    Node = hb_http_server:start_node(Opts),
    Query =
        <<""" 
            query GetMessage { 
                message(
                    keys: 
                        [
                            { 
                                name: "test-key",
                                value: "test-value"
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
                                    <<"name">> := <<"nested">>,
                                    <<"value">> := NestedID
                                },
                                #{
                                    <<"name">> := <<"test-key">>,
                                    <<"value">> := <<"test-value">>
                                },
                                #{ 
                                    <<"name">> := <<"test-key-2">>,
                                    <<"value">> := <<"test-value-2">>
                                }
                            ] 
                    } 
            } 
        },
        Res
    ).

pending_cursor_prefers_existing_cursor_test() ->
    ?assertEqual(
        {ok, <<"pending=txid">>},
        message_query(
            #{
                <<"cursor">> => <<"pending=txid">>,
                <<"offset">> => #{
                    <<"relative">> => <<"txid">>,
                    <<"offset">> => 1
                }
            },
            <<"cursor">>,
            #{},
            #{}
        )
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
            <<"body">> =>
                hb_json:encode(#{
                    <<"query">> => 
                        <<""" 
                            query GetMessage($Keys: [KeyInput]) { 
                                message(
                                    keys: $Keys
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
                        <<"Keys">> => 
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
