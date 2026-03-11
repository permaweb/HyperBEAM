%%% @doc A device for resolving names to their corresponding values, through the
%%% use of a `resolver' interface. Each `resolver' is a message that can be
%%% given a `key' and returns an associated value. The device will attempt to
%%% match the key against each resolver in turn, and return the value of the
%%% first resolver that matches.
-module(dev_name).
-export([info/1, request/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% Core functionality.

%% @doc Configure the `default' key to proxy to the `resolver/4' function.
%% Exclude the `keys' and `set' keys from being processed by this device, as
%% these are needed to modify the base message itself.
info(_) ->
    #{
        default => fun resolve/4,
        excludes => [<<"keys">>, <<"set">>]
    }.

%% @doc Resolve a name to its corresponding value. The name is given by the key
%% called. For example, `GET /~name@1.0/hello&load=false' grants the value of
%% `hello'. If the `load' key is set to `true', the value is treated as a
%% pointer and its contents is loaded from the cache. For example,
%% `GET /~name@1.0/reference' yields the message at the path specified by the
%% `reference' key.
resolve(Key, _, Req, Opts) ->
    Resolvers = hb_opts:get(name_resolvers, [], Opts),
    ?event({resolvers, Resolvers}),
    case match_resolver(Key, Resolvers, Opts) of
        {ok, Resolved} ->
            case hb_util:atom(hb_ao:get(<<"load">>, Req, true, Opts)) of
                false ->
                    {ok, Resolved};
                true ->
                    hb_cache:read(Resolved, Opts)
            end;
        not_found ->
            not_found
    end.

%% @doc Find the first resolver that matches the key and return its value.
match_resolver(_Key, [], _Opts) -> 
    not_found;
match_resolver(Key, [Resolver | Resolvers], Opts) ->
    case catch execute_resolver(Key, Resolver, Opts) of
        {ok, Value} ->
            ?event({resolver_found, {key, Key}, {value, {string, Value}}}),
            {ok, Value};
        _ ->
            match_resolver(Key, Resolvers, Opts)
    end.

%% @doc Execute a resolver with the given key and return its value.
execute_resolver(Key, Path, Opts) when is_binary(Path) ->
    hb_ao:resolve(
        <<Path/binary, "/", Key/binary>>,
        Opts
    );
execute_resolver(Key, Resolver, Opts) when is_map(Resolver) ->
    ?event({executing, {key, Key}, {resolver, Resolver}}),
    hb_ao:resolve(
        Resolver,
        Key,
        Opts
    ).

%%% `on/request` hook functionality.

%% @doc Implements an `on/request' compatible hook that resolves names given in
%% the `host` key to their corresponding ID and prepends it to the execution path.
request(HookMsg, HookReq, Opts) ->
    ?event({request_hook, {hook_msg, HookMsg}, {hook_req, HookReq}}),
    maybe
        {ok, Req} ?= hb_maps:find(<<"request">>, HookReq, Opts),
        {ok, Host} ?= hb_maps:find(<<"host">>, Req, Opts),
        {ok, Name} ?= name_from_host(Host, hb_opts:get(host, no_host, Opts)),
        {ok, ResolvedMsg} ?= resolve(Name, HookMsg, #{}, Opts),
        ModReq =
            case hb_maps:find(<<"body">>, HookReq, Opts) of
                {ok, [OldBase|Rest]} ->
                    [overlay_loaded(OldBase, ResolvedMsg, Opts)|Rest];
                {ok, []} ->
                    [ResolvedMsg]
            end,
        ?event(
            {request_with_prepended_path,
                {name, Name},
                {full_host, Host},
                {resolved_msg, ResolvedMsg},
                {to_execute, ModReq}
            }
        ),
        {ok, #{ <<"body">> => ModReq }}
    else
        Reason ->
            ?event({request_hook_skip, {reason, Reason}, {hook_req, HookReq}}),
            {ok, HookReq}
    end.

%% @doc Takes a request-given host and the host value in the node message and
%% returns only the name component of the host, if it is present. If no name is
%% present, an empty binary is returned.
name_from_host(Host, no_host) ->
    case hd(binary:split(Host, <<".">>)) of
        <<>> -> {error, <<"No name found in `Host`.">>};
        Name -> {ok, Name}
    end;
name_from_host(ReqHost, RawNodeHost) ->
    NodeHost = uri_string:parse(RawNodeHost),
    ?event({node_host, NodeHost}),
    WithoutNodeHost =
        binary:replace(
            ReqHost,
            maps:get(host, NodeHost),
            <<>>
        ),
    name_from_host(WithoutNodeHost, no_host).

%% @doc Merge the base message with the resolved message, ensuring that `~` as
%% device specifiers are preserved.
overlay_loaded({as, DevID, Base}, Resolved, Opts) ->
    {as, DevID, hb_maps:merge(Base, Resolved, Opts)};
overlay_loaded(Base, Resolved, Opts) ->
    hb_maps:merge(Base, Resolved, Opts).

%%% Tests.

no_resolvers_test() ->
    ?assertEqual(
        not_found,
        resolve(<<"hello">>, #{}, #{}, #{ only => local })
    ).

device_resolver(Msg) ->
    #{
        <<"device">> => #{
            info =>
                fun() ->
                    #{
                        default =>
                            fun(Key, _, _Req, _Opts) ->
                                case maps:get(Key, Msg, not_found) of
                                    not_found -> {error, not_found};
                                    Value -> {ok, Value}
                                end
                            end
                    }
                end
        }
    }.

single_resolver_test() ->
    ?assertEqual(
        {ok, <<"world">>},
        resolve(
            <<"hello">>,
            #{},
            #{ <<"load">> => false },
            #{
                name_resolvers => [
                    #{<<"hello">> => <<"world">>}
                ]
            }
        )
    ).

%% @doc Lookup a name in a message and return it.
message_lookup_test() ->
    ?assertEqual(
        {ok, <<"world">>},
        resolve(
            <<"hello">>,
            #{},
            #{ <<"load">> => false },
            #{
                name_resolvers => [
                    device_resolver(
                        #{<<"hello">> => <<"world">>}
                    )
                ]
            }
        )
    ).

multiple_resolvers_test() ->
    ?assertEqual(
        {ok, <<"bigger-world">>},
        resolve(
            <<"hello">>,
            #{},
            #{ <<"load">> => false },
            #{
                name_resolvers => [
                    device_resolver(
                        #{<<"irrelevant">> => <<"world">>}
                    ),
                    device_resolver(
                        #{<<"hello">> => <<"bigger-world">>}
                    )
                ]
            }
        )
    ).

%% @doc Test that we can resolve messages from a name loaded with the device.
load_and_execute_test() ->
    TestKey = <<"test-key", (hb_util:bin(erlang:system_time(millisecond)))/binary>>,
    {ok, ID} = hb_cache:write(
        #{
            <<"deep">> => <<"PING">>
        },
        #{}
    ),
    ?assertEqual(
        {ok, <<"PING">>},
        hb_ao:resolve_many(
            [
                #{ <<"device">> => <<"name@1.0">> },
                #{ <<"path">> => TestKey },
                #{ <<"path">> => <<"deep">> }
            ],
            #{
                name_resolvers => [
                    device_resolver(#{ <<"irrelevant">> => ID }),
                    device_resolver(#{ TestKey => ID })
                ]
            }
        )
    ).

%% @doc Return an `Opts` for an environment with the default ARNS name export
%% and a temporary store for the test.
arns_opts() ->
    JSONNames = <<"G_gb7SAgogHMtmqycwaHaC6uC-CZ3akACdFv5PUaEE8">>,
    Path = <<JSONNames/binary, "~json@1.0/deserialize&target=data">>,
    hb_http_server:start_node(#{}),
    TempStore = hb_test_utils:test_store(),
    #{
        store =>
            [
                TempStore,
                #{
                    <<"store-module">> => hb_store_gateway,
                    <<"local-store">> => [TempStore]
                }
            ],
        name_resolvers => [Path],
        on => #{
            <<"request">> => #{
                <<"device">> => <<"name@1.0">>
            }
        }
    }.

%% @doc Names from JSON test.
arns_json_snapshot_test() ->
    Opts = arns_opts(),
    ?assertMatch(
        {ok, <<"application/pdf">>},
        hb_ao:resolve_many(
            [
                #{ <<"device">> => <<"name@1.0">> },
                #{ <<"path">> => <<"draft-17_whitepaper">> },
                <<"content-type">>
            ],
            Opts
        )
    ).

arns_host_resolution_test() ->
    Opts = arns_opts(),
    Node = hb_http_server:start_node(Opts),
    ?assertMatch(
        {ok, <<"application/pdf">>},
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"content-type">>,
                <<"host">> => <<"draft-17_whitepaper">>
            },
            %% Host header isn't added automatically if used HTTP2. 
            %% We need host header to resolve the ARNS name.
            Opts#{http_client => gun, opts => #{protocol => http1} }
        )
    ).