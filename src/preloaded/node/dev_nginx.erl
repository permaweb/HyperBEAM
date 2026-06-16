%%% @doc A request hook that moves nginx-style reverse proxy configuration into
%%% HyperBEAM's node message.
%%%
%%% The device is a no-op unless configured. It supports two pieces commonly
%%% handled in an external nginx config:
%%% <ul>
%%%   <li>Trusting forwarded headers from known proxy peers.</li>
%%%   <li>Applying ordered inbound rewrite routes to the request path.</li>
%%% </ul>
%%%
%%% Example:
%%% <pre>
%%% {
%%%   "nginx-routes": [
%%%     {
%%%       "template": "^/_hb/",
%%%       "match": "^/_hb",
%%%       "with": "",
%%%       "allow-methods": ["GET"]
%%%     }
%%%   ],
%%%   "nginx-deny-unmatched": true,
%%%   "nginx-trust-forwarded-headers": true
%%% }
%%% </pre>
-module(dev_nginx).
-export([request/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_TRUSTED_PROXIES, [
    <<"127.0.0.1">>,
    <<"::1">>,
    <<"localhost">>
]).
-define(DEFAULT_FORWARDED_HOST_HEADERS, [<<"x-forwarded-host">>]).
-define(DEFAULT_FORWARDED_PATH_HEADERS, [
    <<"x-forwarded-uri">>,
    <<"x-original-uri">>,
    <<"x-original-url">>
]).
-define(DEFAULT_FORWARDED_FOR_HEADERS, [
    <<"x-real-ip">>,
    <<"x-forwarded-for">>
]).

%% @doc Apply trusted proxy headers and configured inbound rewrite routes.
request(Base, HookReq, Opts) ->
    ?event(nginx, {request, {base, Base}, {hook_req, HookReq}}),
    case hb_maps:find(<<"request">>, HookReq, Opts) of
        {ok, Req} ->
            case route_request(Base, forwarded(Base, Req, Opts), Opts) of
                {ok, NewReq} ->
                    {ok, update_hook_req(HookReq, Req, NewReq, Opts)};
                {error, ErrorMsg} ->
                    {error, ErrorMsg}
            end;
        error ->
            {ok, HookReq}
    end.

%% @doc Apply forwarded headers if the handler is configured to trust them.
forwarded(Base, Req, Opts) ->
    case trust_forwarded_headers(Base, Opts) of
        true -> trusted_forwarded(Base, Req, Opts);
        false -> Req
    end.

trusted_forwarded(Base, Req, Opts) ->
    PeerIP = hb_private:get(<<"peer-ip">>, Req, no_peer, Opts),
    case trusted_proxy(PeerIP, trusted_proxies(Base, Opts)) of
        true ->
            with_forwarded_path(
                Base,
                with_forwarded_host(
                    Base,
                    with_forwarded_ip(Base, Req, Opts),
                    Opts
                ),
                Opts
            );
        false when PeerIP =/= no_peer ->
            hb_private:set(Req, <<"ip">>, PeerIP, Opts);
        false ->
            Req
    end.

%% @doc Apply the first matching inbound route, if one is configured.
route_request(Base, Req, Opts) ->
    route(Base, Req, routes(Base, Opts), Opts).

route(Base, Req, [], Opts) ->
    case deny_unmatched(Base, Opts) of
        true -> deny(Base, Req, no_matching_route, Opts);
        false -> {ok, Req}
    end;
route(Base, Req, [Route | Rest], Opts) ->
    Template = hb_maps:get(<<"template">>, Route, #{}, Opts),
    case hb_util:template_matches(Req, Template, Opts) of
        true -> maybe_apply_route(Base, Req, Route, Opts);
        false -> route(Base, Req, Rest, Opts)
    end.

maybe_apply_route(Base, Req, Route, Opts) ->
    case method_allowed(Req, Route, Opts) of
        true -> {ok, apply_route(Req, Route, Opts)};
        false -> deny(Base, Req, method_not_allowed, Opts)
    end.

%% @doc Apply a route's path transforms and request key edits.
apply_route(Req, Route, Opts) ->
    Path = hb_maps:get(<<"path">>, Req, <<"/">>, Opts),
    RoutedPath =
        apply_path_suffix(
            Route,
            apply_path_replace(
                Route,
                apply_path_prefix(
                    Route,
                    apply_path_strip(
                        Route,
                        hb_maps:get(<<"path">>, Route, Path, Opts),
                        Opts
                    ),
                    Opts
                ),
                Opts
            ),
            Opts
        ),
    Removed = hb_maps:without(remove_keys(Route, Opts), Req, Opts),
    apply_set(Route, Removed#{ <<"path">> => RoutedPath }, Opts).

apply_path_strip(Route, Path, Opts) ->
    case hb_maps:find(<<"strip-prefix">>, Route, Opts) of
        {ok, Prefix} -> strip_prefix(Path, hb_cache:ensure_loaded(Prefix, Opts));
        error -> Path
    end.

apply_path_prefix(Route, Path, Opts) ->
    case hb_maps:find(<<"prefix">>, Route, Opts) of
        {ok, Prefix} ->
            LoadedPrefix = hb_cache:ensure_loaded(Prefix, Opts),
            <<LoadedPrefix/binary, Path/binary>>;
        error -> Path
    end.

apply_path_replace(Route, Path, Opts) ->
    case {hb_maps:find(<<"match">>, Route, Opts), hb_maps:find(<<"with">>, Route, Opts)} of
        {{ok, Match}, {ok, With}} ->
            re:replace(
                Path,
                hb_cache:ensure_loaded(Match, Opts),
                hb_cache:ensure_loaded(With, Opts),
                [global, {return, binary}]
            );
        _ -> Path
    end.

apply_path_suffix(Route, Path, Opts) ->
    case hb_maps:find(<<"suffix">>, Route, Opts) of
        {ok, Suffix} ->
            LoadedSuffix = hb_cache:ensure_loaded(Suffix, Opts),
            <<Path/binary, LoadedSuffix/binary>>;
        error -> Path
    end.

apply_set(Route, Req, Opts) ->
    hb_maps:fold(
        fun(Key, Value, Acc) ->
            Acc#{ Key => hb_cache:ensure_loaded(Value, Opts) }
        end,
        Req,
        hb_maps:get(<<"set">>, Route, #{}, Opts),
        Opts
    ).

%% @doc Update the hook request and reparse the body when the singleton changed.
update_hook_req(HookReq, Req, Req, _Opts) ->
    HookReq;
update_hook_req(HookReq, _OldReq, NewReq, Opts) ->
    HookReq#{
        <<"request">> => NewReq,
        <<"body">> => hb_singleton:from(NewReq, Opts)
    }.

%% @doc Remove a prefix from a path, preserving a leading slash.
strip_prefix(Prefix, Prefix) ->
    <<"/">>;
strip_prefix(Path, Prefix) ->
    case Path of
        <<Prefix:(byte_size(Prefix))/binary, "/", Rest/binary>> ->
            <<"/", Rest/binary>>;
        <<Prefix:(byte_size(Prefix))/binary, Rest/binary>> ->
            Rest;
        _ ->
            Path
    end.

%% @doc Set the request host from configured forwarded host headers.
with_forwarded_host(Base, Req, Opts) ->
    case first_present(forwarded_host_headers(Base, Opts), Req, Opts) of
        not_found -> Req;
        Host -> Req#{ <<"host">> => first_csv(Host) }
    end.

%% @doc Set the request path from configured forwarded URI headers.
with_forwarded_path(Base, Req, Opts) ->
    case first_present(forwarded_path_headers(Base, Opts), Req, Opts) of
        not_found -> Req;
        Path -> Req#{ <<"path">> => first_csv(Path) }
    end.

%% @doc Set the request IP from configured forwarded address headers.
with_forwarded_ip(Base, Req, Opts) ->
    case first_present(forwarded_for_headers(Base, Opts), Req, Opts) of
        not_found -> Req;
        IP -> hb_private:set(Req, <<"ip">>, first_csv(IP), Opts)
    end.

%% @doc Find the first configured header present in the request.
first_present([], _Req, _Opts) ->
    not_found;
first_present([Key | Rest], Req, Opts) ->
    case hb_maps:find(Key, Req, Opts) of
        {ok, Value} -> Value;
        error -> first_present(Rest, Req, Opts)
    end.

%% @doc Return the first comma-delimited value from a proxy header.
first_csv(Value) when is_binary(Value) ->
    [First | _] = binary:split(Value, <<",">>),
    string:trim(First);
first_csv(Value) ->
    Value.

trusted_proxy(_PeerIP, [<<"*">> | _]) ->
    true;
trusted_proxy(PeerIP, [PeerIP | _]) ->
    true;
trusted_proxy(PeerIP, [_ | Rest]) ->
    trusted_proxy(PeerIP, Rest);
trusted_proxy(_PeerIP, []) ->
    false.

method_allowed(Req, Route, Opts) ->
    case route_methods(Route, Opts) of
        all -> true;
        Methods -> lists:member(normalize_method(request_method(Req, Opts)), Methods)
    end.

route_methods(Route, Opts) ->
    case {
        hb_maps:find(<<"allow-methods">>, Route, Opts),
        hb_maps:find(<<"methods">>, Route, Opts)
    } of
        {{ok, Methods}, _} -> normalize_methods(Methods, Opts);
        {_, {ok, Methods}} -> normalize_methods(Methods, Opts);
        _ -> all
    end.

request_method(Req, Opts) ->
    hb_maps:get(<<"method">>, Req, <<"GET">>, Opts).

normalize_methods(Methods, Opts) ->
    [normalize_method(Method) || Method <- maybe_list(Methods, Opts)].

normalize_method(<<"*">>) ->
    <<"*">>;
normalize_method(Method) ->
    hb_util:bin(string:uppercase(hb_util:list(Method))).

deny(Base, Req, Reason, Opts) ->
    Status = deny_status(Base, Opts),
    ReasonBin = hb_util:bin(Reason),
    {error,
        #{
            <<"status">> => Status,
            <<"reason">> => ReasonBin,
            <<"body">> => deny_body(Base, Req, ReasonBin, Opts)
        }
    }.

routes(Base, Opts) ->
    maybe_list(option(Base, <<"routes">>, <<"nginx-routes">>, [], Opts), Opts).

deny_unmatched(Base, Opts) ->
    hb_util:bool(
        option(
            Base,
            <<"deny-unmatched">>,
            <<"nginx-deny-unmatched">>,
            false,
            Opts
        )
    ).

deny_status(Base, Opts) ->
    hb_util:int(
        option(
            Base,
            <<"deny-status">>,
            <<"nginx-deny-status">>,
            403,
            Opts
        )
    ).

deny_body(Base, _Req, _Reason, Opts) ->
    option(
        Base,
        <<"deny-body">>,
        <<"nginx-deny-body">>,
        <<"Forbidden.">>,
        Opts
    ).

remove_keys(Route, Opts) ->
    maybe_list(hb_maps:get(<<"remove">>, Route, [], Opts), Opts).

trusted_proxies(Base, Opts) ->
    maybe_list(
        option(
            Base,
            <<"trusted-proxies">>,
            <<"nginx-trusted-proxies">>,
            ?DEFAULT_TRUSTED_PROXIES,
            Opts
        ),
        Opts
    ).

trust_forwarded_headers(Base, Opts) ->
    hb_util:atom(
        option(
            Base,
            <<"trust-forwarded-headers">>,
            <<"nginx-trust-forwarded-headers">>,
            false,
            Opts
        )
    ).

forwarded_host_headers(Base, Opts) ->
    maybe_list(
        option(
            Base,
            <<"forwarded-host-headers">>,
            <<"nginx-forwarded-host-headers">>,
            ?DEFAULT_FORWARDED_HOST_HEADERS,
            Opts
        ),
        Opts
    ).

forwarded_path_headers(Base, Opts) ->
    maybe_list(
        option(
            Base,
            <<"forwarded-path-headers">>,
            <<"nginx-forwarded-path-headers">>,
            ?DEFAULT_FORWARDED_PATH_HEADERS,
            Opts
        ),
        Opts
    ).

forwarded_for_headers(Base, Opts) ->
    maybe_list(
        option(
            Base,
            <<"forwarded-for-headers">>,
            <<"nginx-forwarded-for-headers">>,
            ?DEFAULT_FORWARDED_FOR_HEADERS,
            Opts
        ),
        Opts
    ).

option(Base, LocalKey, GlobalKey, Default, Opts) ->
    hb_maps:get(LocalKey, Base, hb_opts:get(GlobalKey, Default, Opts), Opts).

maybe_list(false, _Opts) ->
    [];
maybe_list(undefined, _Opts) ->
    [];
maybe_list(List, _Opts) when is_list(List) ->
    List;
maybe_list(Msg, Opts) when is_map(Msg) ->
    case hb_util:is_ordered_list(Msg, Opts) of
        true -> hb_util:message_to_ordered_list(Msg, Opts);
        false -> [Msg]
    end;
maybe_list(Item, _Opts) ->
    [Item].

%%% Tests

rewrite_route_test() ->
    Base =
        #{
            <<"routes">> =>
                [
                    #{
                        <<"template">> => <<"^/_hb/">>,
                        <<"match">> => <<"^/_hb">>,
                        <<"with">> => <<"">>
                    }
                ]
        },
    HookReq =
        #{
            <<"request">> =>
                #{
                    <<"method">> => <<"GET">>,
                    <<"path">> => <<"/_hb/~meta@1.0/info">>
                },
            <<"body">> => []
        },
    {ok, Res} = request(Base, HookReq, #{}),
    Req = hb_maps:get(<<"request">>, Res),
    ?assertEqual(<<"/~meta@1.0/info">>, hb_maps:get(<<"path">>, Req)),
    ?assertEqual(hb_singleton:from(Req, #{}), hb_maps:get(<<"body">>, Res)).

trusted_forwarded_headers_test() ->
    Req0 =
        hb_private:set(
            hb_private:set(
                #{
                    <<"method">> => <<"GET">>,
                    <<"path">> => <<"/">>,
                    <<"host">> => <<"127.0.0.1">>,
                    <<"x-forwarded-host">> => <<"alice.example">>,
                    <<"x-forwarded-for">> =>
                        <<"203.0.113.10, 127.0.0.1">>
                },
                <<"peer-ip">>,
                <<"127.0.0.1">>,
                #{}
            ),
            <<"ip">>,
            <<"127.0.0.1">>,
            #{}
        ),
    HookReq = #{ <<"request">> => Req0, <<"body">> => [] },
    Base = #{ <<"trust-forwarded-headers">> => true },
    {ok, Res} = request(Base, HookReq, #{}),
    Req = hb_maps:get(<<"request">>, Res),
    ?assertEqual(<<"alice.example">>, hb_maps:get(<<"host">>, Req)),
    ?assertEqual(<<"203.0.113.10">>, hb_private:get(<<"ip">>, Req, #{})).

untrusted_forwarded_headers_test() ->
    Req0 =
        hb_private:set(
            hb_private:set(
                #{
                    <<"method">> => <<"GET">>,
                    <<"path">> => <<"/">>,
                    <<"host">> => <<"127.0.0.1">>,
                    <<"x-forwarded-host">> => <<"alice.example">>,
                    <<"x-forwarded-for">> => <<"203.0.113.10">>
                },
                <<"peer-ip">>,
                <<"198.51.100.1">>,
                #{}
            ),
            <<"ip">>,
            <<"203.0.113.10">>,
            #{}
        ),
    HookReq = #{ <<"request">> => Req0, <<"body">> => [] },
    Base = #{ <<"trust-forwarded-headers">> => true },
    {ok, Res} = request(Base, HookReq, #{}),
    Req = hb_maps:get(<<"request">>, Res),
    ?assertEqual(<<"127.0.0.1">>, hb_maps:get(<<"host">>, Req)),
    ?assertEqual(<<"198.51.100.1">>, hb_private:get(<<"ip">>, Req, #{})).

deny_unmatched_test() ->
    Base =
        #{
            <<"deny-unmatched">> => true,
            <<"routes">> => [#{ <<"template">> => <<"^/allowed$">> }]
        },
    HookReq =
        #{
            <<"request">> =>
                #{
                    <<"method">> => <<"GET">>,
                    <<"path">> => <<"/denied">>
                },
            <<"body">> => []
        },
    ?assertMatch(
        {error, #{ <<"status">> := 403, <<"reason">> := <<"no_matching_route">> }},
        request(Base, HookReq, #{})
    ).

method_restricted_route_test() ->
    Base =
        #{
            <<"routes">> =>
                [
                    #{
                        <<"template">> => <<"^/allowed$">>,
                        <<"allow-methods">> => [<<"GET">>]
                    }
                ]
        },
    HookReq =
        #{
            <<"request">> =>
                #{
                    <<"method">> => <<"POST">>,
                    <<"path">> => <<"/allowed">>
                },
            <<"body">> => []
        },
    ?assertMatch(
        {error, #{ <<"status">> := 403, <<"reason">> := <<"method_not_allowed">> }},
        request(Base, HookReq, #{})
    ).

oracle_route_policy_test() ->
    ID = <<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNO12">>,
    Template =
        <<
            "^/[^/]{43}(~process@1\\.0)?(/compute)?",
            "(/(balances/(0x[0-9a-fA-F]{40}|[^/]{43})|at-slot|now|",
            "token-info(?:/supply)?)|&slot=\\d+)/?$"
        >>,
    Base =
        #{
            <<"deny-unmatched">> => true,
            <<"routes">> =>
                [
                    #{
                        <<"template">> => Template,
                        <<"allow-methods">> => [<<"GET">>]
                    }
                ]
        },
    AllowedReq =
        #{
            <<"request">> =>
                #{
                    <<"method">> => <<"GET">>,
                    <<"path">> => <<"/", ID/binary, "~process@1.0/compute/now">>
                },
            <<"body">> => []
        },
    DeniedMethodReq =
        #{
            <<"request">> =>
                #{
                    <<"method">> => <<"POST">>,
                    <<"path">> => <<"/", ID/binary, "~process@1.0/compute/now">>
                },
            <<"body">> => []
        },
    DeniedPathReq =
        #{
            <<"request">> =>
                #{
                    <<"method">> => <<"GET">>,
                    <<"path">> => <<"/">>
                },
            <<"body">> => []
        },
    ?assertMatch({ok, _}, request(Base, AllowedReq, #{})),
    ?assertMatch(
        {error, #{ <<"reason">> := <<"method_not_allowed">> }},
        request(Base, DeniedMethodReq, #{})
    ),
    ?assertMatch(
        {error, #{ <<"reason">> := <<"no_matching_route">> }},
        request(Base, DeniedPathReq, #{})
    ).

http_rewrite_test() ->
    Opts =
        #{
            <<"port">> => 0,
            <<"on">> =>
                #{
                    <<"request">> =>
                        #{
                            <<"device">> => <<"nginx@1.0">>,
                            <<"routes">> =>
                                [
                                    #{
                                        <<"template">> => <<"^/_hb/">>,
                                        <<"match">> => <<"^/_hb">>,
                                        <<"with">> => <<"">>
                                    }
                                ]
                        }
                }
        },
    Node = hb_http_server:start_node(Opts),
    ?assertMatch(
        {ok, #{ <<"initialized">> := true }},
        hb_http:get(Node, <<"/_hb/~meta@1.0/info">>, Opts)
    ).
