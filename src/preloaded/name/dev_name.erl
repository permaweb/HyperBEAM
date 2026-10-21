%%% @doc A device for resolving names to their corresponding values, through the
%%% use of a `resolver' interface. Each `resolver' is a message that can be
%%% given a `key' and returns an associated value. The device will attempt to
%%% match the key against each resolver in turn, and return the value of the
%%% first resolver that matches.
-module(dev_name).
-specification("../../../specs/name@1.0.md").
-export([device_info/1, request/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% Core functionality.

%% @doc Configure the `default' key to proxy to the `resolver/4' function.
%% Exclude the `keys' and `set' keys from being processed by this device, as
%% these are needed to modify the base message itself.
device_info(_) ->
    #{
        default => fun resolve/4,
        excludes => [<<"committers">>, <<"keys">>, <<"set">>]
    }.

%% @doc Resolve a name to its corresponding value. The name is given by the key
%% called. For example, `GET /~name@1.0/hello&load=false' grants the value of
%% `hello'. If the `load' key is set to `true', the value is treated as a
%% pointer and its contents is loaded from the cache. For example,
%% `GET /~name@1.0/reference' yields the message at the path specified by the
%% `reference' key.
-spec resolve(
    binary(),
    #{ _ => _ },
    #{ load => boolean(), _ => _ },
    #{ _ => _ }
) -> {ok, _} | not_found.
resolve(Key, _, Req, Opts) ->
    Resolvers = hb_opts:get(name_resolvers, [], Opts),
    ?event({resolvers, Resolvers}),
    case match_resolver(Key, Resolvers, Opts) of
        {ok, Resolved} ->
            case hb_util:atom(hb_maps:get(<<"load">>, Req, true, Opts)) of
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
-spec request(
    #{ _ => _ },
    #{ request := #{ host := binary(), _ => _ }, body := _, _ => _ },
    #{ _ => _ }
) ->
    {ok, #{ _ => _ }}
    | {error, #{ status := integer(), body := binary(), _ => _ }}.
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
        ok ?= reject_reserved_host_label(Name, Opts),
        {ok, Resolved} ?=
            resolve(Name, HookMsg, #{ <<"load">> => false }, Opts),
        {ok, ResolvedMsg} ?= maybe_load_resolved(Resolved, Opts),
        ModReq =
            maybe_append_named_message(
                Resolved,
                ResolvedMsg,
                first_part_request(
                    Req,
                    hb_util:ok(hb_maps:find(<<"body">>, HookReq, Opts)),
                    Opts
                ),
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
        {ok, HookReq#{ <<"body">> => ModReq }}
    else
        {reserved_host_label, ReservedName} ->
            ?event({reserved_host_label_rejected, {name, ReservedName}}),
            {error, #{<<"status">> => 404, <<"body">> => <<"Not Found">>}};
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

%% @doc Replace a base that is not a message with the first part of the path,
%% read as a request upon the carried message. The parser gives such a base
%% for a first path part that is a key, an ID or a URI. The request is the one
%% that the parser gives for that part when the path starts with an empty
%% part.
first_part_request(Req, [Base|ReqMsgsRest], Opts) when not is_map(Base) ->
    Path = hb_path:normalize(hb_maps:get(<<"path">>, Req, <<"/">>, Opts)),
    [_Carried, First|_] =
        hb_singleton:from(Req#{ <<"path">> => <<"/", Path/binary>> }, Opts),
    [First|ReqMsgsRest];
first_part_request(_Req, ReqMsgs, _Opts) -> ReqMsgs.

%% @doc After finding a hit for a named message, we should ensure that it is the
%% base message for the evaluation. If it is already present in the request,
%% however, we should not add it twice. Instead, we must add the version that
%% is loaded (if applicable). The first request of the path is the named
%% message if its key is the value that the name resolves to, or the ID of
%% the message loaded from that value.
%% 
%% Eg:
%%      base32IDA.hyperbeam/ -> [IDA]
%%      base32IDA.hyperbeam/key/xyz -> [IDA, key, xyz]
%%      base32IDA.hyperbeam/key~dev/xyz -> [IDA, key~dev, xyz]
%%      base32IDA.hyperbeam/base64urlIDA/xyz -> [IDA, xyz]
%%      base32IDA.hyperbeam/base64urlIDB/xyz -> [IDA, IDB, xyz]
maybe_append_named_message(_Resolved, ResolvedMsg, [], _Opts) -> [ResolvedMsg];
maybe_append_named_message(
    Resolved,
    ResolvedMsg,
    [OldBase|ReqMsgsRest],
    Opts
) ->
    IDs = [Resolved, hb_message:id(ResolvedMsg, signed, Opts)],
    case lists:member(request_key(OldBase, Opts), IDs) of
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

%% @doc Return the key of a request: a binary request is a key, and the key of
%% a message is its `path'. Any other request, such as a subresolution, has no
%% key.
request_key(Key, _Opts) when is_binary(Key) -> Key;
request_key(Req, Opts) when is_map(Req) ->
    hb_maps:get(<<"path">>, Req, not_found, Opts);
request_key(_Req, _Opts) -> not_found.

%% @doc Takes a request-given host and the host value in the node message and
%% returns only the name component of the host, if it is present. If no name is
%% present, an empty binary is returned.
name_from_host(Host, no_host) ->
    % Handle the case where no host key is present in the node message. This 
    % logic is also used when parsing of the host key from the node message
    % fails, or the node message host is not found in the client provided value
    % (node claims to be `x.com`, but the user request is for `abc.y.com`).
    case binary:split(Host, <<".">>, [global, trim_all]) of
        [_Host] ->
            {skip, <<"No subdomain found in `Host: ", Host/binary, "`.">>};
        [Name|_] ->
            case inet:parse_address(hb_util:list(Host)) of
                {ok, _} ->
                    {skip, <<"No subdomain found in `Host: ", Host/binary, "`.">>};
                _ -> {ok, Name}
            end
    end;
name_from_host(ReqHost, RawNodeHost) ->
    % `node-host' is a URL or a host alone, with or without a port. A value
    % that does not parse as a URL with a host, such as `127.0.0.1:8734', is
    % used as it is.
    NodeHostName =
        case uri_string:parse(RawNodeHost) of
            #{ host := ParsedHost } -> ParsedHost;
            _ -> RawNodeHost
        end,
    case binary:split(ReqHost, <<".", NodeHostName/binary>>) of
        [Subdomain, <<>>] -> {ok, Subdomain};
        _ when ReqHost =:= NodeHostName ->
            {skip, <<"No subdomain found in `Host: ", ReqHost/binary, "`.">>};
        _ -> name_from_host(ReqHost, no_host)
    end.

%% @doc Reject host-derived names that the node has reserved for gateway
%% routing. Direct protocol lookups through `~name@1.0/<name>' are intentionally
%% left to `resolve/4'.
reject_reserved_host_label(Name, Opts) ->
    case reserved_host_label(Name, Opts) of
        false -> ok;
        true -> {reserved_host_label, Name}
    end.

reserved_host_label(Name, Opts) ->
    lists:member(
        hb_util:to_lower(Name),
        hb_opts:get(name_reserved_host_labels, [], Opts)
    ).

%%% Tests.

no_resolvers_test_parallel() ->
    ?assertEqual(
        not_found,
        resolve(<<"hello">>, #{}, #{}, #{ <<"only">> => local })
    ).

device_resolver(Msg) ->
    #{
        <<"device">> => #{
            device_info =>
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

single_resolver_test_parallel() ->
    ?assertEqual(
        {ok, <<"world">>},
        resolve(
            <<"hello">>,
            #{},
            #{ <<"load">> => false },
            #{
                <<"name-resolvers">> => [
                    #{<<"hello">> => <<"world">>}
                ]
            }
        )
    ).

%% @doc Lookup a name in a message and return it.
message_lookup_test_parallel() ->
    ?assertEqual(
        {ok, <<"world">>},
        resolve(
            <<"hello">>,
            #{},
            #{ <<"load">> => false },
            #{
                <<"name-resolvers">> => [
                    device_resolver(
                        #{<<"hello">> => <<"world">>}
                    )
                ]
            }
        )
    ).

multiple_resolvers_test_parallel() ->
    ?assertEqual(
        {ok, <<"bigger-world">>},
        resolve(
            <<"hello">>,
            #{},
            #{ <<"load">> => false },
            #{
                <<"name-resolvers">> => [
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
load_and_execute_test_parallel() ->
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
                <<"name-resolvers">> => [
                    device_resolver(#{ <<"irrelevant">> => ID }),
                    device_resolver(#{ TestKey => ID })
                ]
            }
        )
    ).

%% @doc Names from JSON test.
arns_json_snapshot_test_parallel() ->
    Opts = hb_name_test_utils:arns_opts(),
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

arns_host_resolution_test_parallel() ->
    Opts = hb_name_test_utils:arns_opts(),
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

arns_host_resolution_with_node_host_test_parallel() ->
    Opts = (hb_name_test_utils:arns_opts())#{
        <<"node-host">> => <<"http://localhost">>,
        <<"port">> => 0
    },
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

root_request_skips_name_resolution_test_parallel() ->
    BaseOpts =
        #{
            <<"port">> => 0,
            <<"name-resolvers">> => [device_resolver(#{})],
            <<"on">> =>
                #{ <<"request">> => #{ <<"device">> => <<"name@1.0">> } }
        },
    Check =
        fun(Host, ExtraOpts) ->
            Opts = maps:merge(BaseOpts, ExtraOpts),
            Node = hb_http_server:start_node(Opts),
            ?assertMatch(
                {ok,
                    #{
                        <<"status">> := 307,
                        <<"location">> := <<"/~hyperbuddy@1.0/index">>
                    }},
                hb_http:get(
                    Node,
                    #{ <<"path">> => <<"/">>, <<"host">> => Host },
                    Opts
                )
            )
        end,
    Check(<<"localhost">>, #{}),
    Check(<<"127.0.0.1:8734">>, #{}),
    Check(<<"ourweave.net:8734">>, #{ <<"node-host">> => <<"ourweave.net">> }).

reserved_host_label_blocks_host_resolution_test_parallel() ->
    ?assertEqual(
        {error, #{<<"status">> => 404, <<"body">> => <<"Not Found">>}},
        request(
            #{},
            #{
                <<"request">> => #{<<"host">> => <<"WWW.localhost">>},
                <<"body">> => [#{<<"path">> => <<"status">>}]
            },
            #{
                <<"name-reserved-host-labels">> => [<<"www">>, <<"admin">>],
                <<"name-resolvers">> => [
                    device_resolver(#{<<"WWW">> => <<"should-not-resolve">>})
                ]
            }
        )
    ).

reserved_host_labels_do_not_block_direct_resolution_test_parallel() ->
    ?assertEqual(
        {ok, <<"allowed">>},
        resolve(
            <<"www">>,
            #{},
            #{<<"load">> => false},
            #{
                <<"name-reserved-host-labels">> => [<<"www">>],
                <<"name-resolvers">> => [
                    device_resolver(#{<<"www">> => <<"allowed">>})
                ]
            }
        )
    ).

name_from_host_test_parallel() ->
    ?assertMatch({skip, _}, name_from_host(<<"127.0.0.1">>, no_host)),
    ?assertEqual({ok, <<"abc">>}, name_from_host(<<"abc.127.0.0.1">>, no_host)),
    ?assertEqual(
        {ok, <<"sub3.sub2">>},
        name_from_host(<<"sub3.sub2.sub1.abc.xyz">>, <<"sub1.abc.xyz">>)
    ).
