%%% @doc A device for resolving names to their corresponding values, through the
%%% use of a `resolver' interface. Each `resolver' is a message that can be
%%% given a `key' and returns an associated value. The device will attempt to
%%% match the key against each resolver in turn, and return the value of the
%%% first resolver that matches.
-module(dev_name).
-export([info/1, request/3]).
%%% Public helpers.
-export([test_arns_opts/0]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).
%% Test cases exported so `all_tests_test_/0' can run them in parallel.
-export([
    no_resolvers_tc/0,
    single_resolver_tc/0,
    message_lookup_tc/0,
    multiple_resolvers_tc/0,
    load_and_execute_tc/0,
    arns_json_snapshot_tc/0,
    arns_host_resolution_tc/0,
    arns_host_resolution_with_node_host_tc/0,
    localhost_root_request_skips_name_resolution_tc/0
]).
-endif.

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
                    maybe_load_resolved(Resolved, Opts)
            end;
        not_found -> not_found
    end.

%% @doc Load a resolved name target if it is a cache reference, otherwise
%% return the resolved value directly.
maybe_load_resolved(Resolved, Opts) when ?IS_ID(Resolved) ->
    hb_cache:read(Resolved, Opts);
maybe_load_resolved(Resolved, Opts) when ?IS_LINK(Resolved) ->
    {ok, hb_cache:ensure_loaded(Resolved, Opts)};
maybe_load_resolved(Resolved, _Opts) ->
    {ok, Resolved}.

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
        {ok, Name} ?=
            name_from_host(
                Host,
                hb_opts:get(node_host, hb_opts:get(host, no_host, Opts), Opts)
            ),
        {ok, ResolvedMsg} ?= resolve(Name, HookMsg, HookReq, Opts),
        ModReq =
            maybe_append_named_message(
                ResolvedMsg,
                hb_util:ok(hb_maps:find(<<"body">>, HookReq, Opts)),
                Opts
            ),
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
        {skip, Reason} ->
            ?event({name_resolution_skipped, {reason, Reason}}),
            {ok, HookReq};
        Other ->
            case maps:get(<<"body">>, HookReq, []) of 
                [] ->
                    ?event({request_hook_404, root_path}),
                    % If no path is provided, we should return 404 if we could
                    % not resolve the name component.
                    {error, #{<<"status">> => 404, <<"body">> => <<"Not Found">>}};
                _ ->
                    ?event({request_hook_skip, {cause, Other}, {hook_req, HookReq}}),
                    {ok, HookReq}
            end
    end.

%% @doc After finding a hit for a named message, we should ensure that it is the
%% base message for the evaluation. If it is already present in the request,
%% however, we should not add it twice. Instead, we must add the version that
%% is loaded (if applicable).
%% 
%% Eg:
%%      base32IDA.hyperbeam/ -> [IDA]
%%      base32IDA.hyperbeam/base64urlIDA/xyz -> [IDA, xyz]
%%      base32IDA.hyperbeam/base64urlIDB/xyz -> [IDA, IDB, xyz]
maybe_append_named_message(ResolvedMsg, [], _Opts) -> [ResolvedMsg];
maybe_append_named_message(ResolvedMsg, OldReq = [OldBase|ReqMsgsRest], Opts) ->
    case permissive_id(OldBase, Opts) == permissive_id(ResolvedMsg, Opts) of
        true when is_map(OldBase) or is_list(OldBase) -> OldReq;
        true -> [ResolvedMsg|ReqMsgsRest];
        false ->
            case is_map(OldBase) andalso hb_maps:get(<<"path">>, OldBase, not_found, Opts) of
                not_found ->
                    ?event(
                        {skipping_old_base,
                            {old_base, OldBase},
                            {resolved_msg, ResolvedMsg}
                        }
                    ),
                    [ResolvedMsg|ReqMsgsRest];
                _ -> [ResolvedMsg, OldBase|ReqMsgsRest]
            end
    end.

%% @doc Takes a message or resolution request (`as` or `resolve`) -- whether in
%% the form of an ID, link, or loaded map -- and returns its ID.
permissive_id(ID, _Opts) when ?IS_ID(ID) -> ID;
permissive_id({link, ID, _LinkOpts}, _Opts) -> ID;
permissive_id({as, _Device, Msg}, Opts) -> permissive_id(Msg, Opts);
permissive_id(Msg, Opts) when is_map(Msg) -> hb_message:id(Msg, signed, Opts).

%% @doc Takes a request-given host and the host value in the node message and
%% returns only the name component of the host, if it is present. If no name is
%% present, an empty binary is returned.
name_from_host(Host, no_host) ->
    % Handle the case where no host key is present in the node message. This 
    % logic is also used when parsing of the host key from the node message
    % fails, or the node message host is not found in the client provided value
    % (node claims to be `x.com`, but the user request is for `abc.y.com`).
    case binary:split(Host, <<".">>, [global, trim_all]) of
        [_Host] -> {skip, <<"No subdomain found in `Host: ", Host/binary, "`.">>};
        [Name|_] -> {ok, Name}
    end;
name_from_host(ReqHost, RawNodeHost) ->
    case uri_string:parse(RawNodeHost) of
        #{ host := NodeHostName } ->
            case binary:split(ReqHost, <<".", NodeHostName/binary>>) of
                [Subdomain, <<>>] -> {ok, Subdomain};
                _ -> name_from_host(ReqHost, no_host)
            end;
        _ -> name_from_host(ReqHost, no_host)
    end.

%%% Tests.

%% @doc Run every test in this module in parallel. Each case uses its own
%% opts/store, so no shared state exists to race on.
all_tests_test_() ->
    {inparallel,
        [
            {atom_to_list(F), fun ?MODULE:F/0}
        ||
            F <- [
                no_resolvers_tc,
                single_resolver_tc,
                message_lookup_tc,
                multiple_resolvers_tc,
                load_and_execute_tc,
                arns_json_snapshot_tc,
                arns_host_resolution_tc,
                arns_host_resolution_with_node_host_tc,
                localhost_root_request_skips_name_resolution_tc
            ]
        ]
    }.

no_resolvers_tc() ->
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

single_resolver_tc() ->
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
message_lookup_tc() ->
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

multiple_resolvers_tc() ->
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
load_and_execute_tc() ->
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
test_arns_opts() ->
    JSONNames = <<"G_gb7SAgogHMtmqycwaHaC6uC-CZ3akACdFv5PUaEE8">>,
    Path = <<JSONNames/binary, "~json@1.0/deserialize&target=data">>,
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
arns_json_snapshot_tc() ->
    Opts = test_arns_opts(),
    ?assertMatch(
        {ok, <<"text/html">>},
        hb_ao:resolve_many(
            [
                #{ <<"device">> => <<"name@1.0">> },
                #{ <<"path">> => <<"001_permabytes">>, <<"load">> => true },
                <<"content-type">>
            ],
            Opts
        )
    ).

arns_host_resolution_tc() ->
    Opts = test_arns_opts(),
    Node = hb_http_server:start_node(Opts),
    ?assertMatch(
        {ok, <<"text/html">>},
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"content-type">>,
                <<"host">> => <<"001_permabytes.localhost">>
            },
            Opts
        )
    ).

arns_host_resolution_with_node_host_tc() ->
    Opts = (test_arns_opts())#{ node_host => <<"http://localhost">>, port => 0 },
    Node = hb_http_server:start_node(Opts),
    ?assertMatch(
        {ok, <<"text/html">>},
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"content-type">>,
                <<"host">> => <<"001_permabytes.localhost">>
            },
            Opts
        )
    ).

localhost_root_request_skips_name_resolution_tc() ->
    Opts = (test_arns_opts())#{ port => 0 },
    Node = hb_http_server:start_node(Opts),
    ?assertMatch(
        {ok,
            #{
                <<"status">> := 307,
                <<"location">> := <<"/~hyperbuddy@1.0/index">>
            }},
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"/">>,
                <<"host">> => <<"localhost">>
            },
            Opts
        )
    ).
