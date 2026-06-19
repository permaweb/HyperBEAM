%%% @doc Request hook that restricts configured devices or paths to the node
%%% owner, using the same signed-message authorization semantics as node
%%% configuration updates.
-module(dev_owner_auth).
-export([request/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PATH_KEYS,
    [
        <<"path">>,
        <<"cron-path">>,
        <<"relay-path">>,
        <<"route-path">>
    ]
).

%% @doc `on/request' hook handler.
request(Base, HookReq, Opts) ->
    case protected_request(Base, HookReq, Opts) of
        false ->
            {ok, HookReq};
        RequiredKeys ->
            authorize(HookReq, RequiredKeys, Opts)
    end.

%% @doc Check whether the hook payload can invoke a protected device/path.
protected_request(Base, HookReq, Opts) ->
    Protected = protected(Base, Opts),
    MaxLinkFollows = link_follow_limit(Base, Opts),
    Request = hb_maps:get(<<"request">>, HookReq, #{}, Opts),
    Body = hb_maps:get(<<"body">>, HookReq, [], Opts),
    RequestProtected = protected_term(Request, Protected, Opts, MaxLinkFollows),
    BodyProtected = protected_term(Body, Protected, Opts, MaxLinkFollows),
    RequiredKeys =
        case {RequestProtected, BodyProtected} of
            {false, false} -> [];
            {true, _} -> signed_request_keys(Request, Opts);
            {false, true} -> [<<"body">> | signed_request_keys(Request, Opts)]
        end,
    case RequiredKeys of
        [] -> false;
        _ -> RequiredKeys
    end.

signed_request_keys(Request, Opts) when is_map(Request) ->
    PresentPathKeys =
        lists:filter(
            fun(Key) -> hb_maps:is_key(Key, Request, Opts) end,
            ?PATH_KEYS
        ),
    PresentDeviceKeys =
        case hb_maps:is_key(<<"device">>, Request, Opts) of
            true -> [<<"device">>];
            false -> []
        end,
    hb_util:unique(PresentPathKeys ++ PresentDeviceKeys);
signed_request_keys(_Request, _Opts) ->
    [].

protected(Base, Opts) ->
    case hb_maps:get(<<"protected">>, Base, [], Opts) of
        List when is_list(List) -> [normalize_path(P) || P <- List];
        One -> [normalize_path(One)]
    end.

link_follow_limit(Base, Opts) ->
    RawLimit =
        hb_maps:get(
            <<"max-link-follows">>,
            Base,
            hb_opts:get(owner_auth_max_link_follows, 16, Opts),
            Opts
        ),
    try max(0, hb_util:int(RawLimit))
    catch _:_ -> 0
    end.

protected_term(Bin, Protected, _Opts) when is_binary(Bin) ->
    path_protected(Bin, Protected);
protected_term(Term, Protected, Opts) ->
    protected_term(Term, Protected, Opts, 16).

protected_term(Link, Protected, Opts, LinksLeft) when ?IS_LINK(Link) ->
    case LinksLeft > 0 of
        true ->
            try protected_term(
                hb_cache:ensure_loaded(Link, Opts),
                Protected,
                Opts,
                LinksLeft - 1
            )
            catch _:_ -> true
            end;
        false ->
            true
    end;
protected_term(Bin, Protected, _Opts, _LinksLeft) when is_binary(Bin) ->
    path_protected(Bin, Protected);
protected_term({as, Device, Msg}, Protected, Opts, LinksLeft) ->
    device_protected(Device, Protected)
        orelse protected_term(Msg, Protected, Opts, LinksLeft);
protected_term({resolve, Msg}, Protected, Opts, LinksLeft) ->
    protected_term(Msg, Protected, Opts, LinksLeft);
protected_term(List, Protected, Opts, LinksLeft) when is_list(List) ->
    lists:any(fun(Item) -> protected_term(Item, Protected, Opts, LinksLeft) end, List);
protected_term(Msg, Protected, Opts, LinksLeft) when is_map(Msg) ->
    device_protected(maps:get(<<"device">>, Msg, not_found), Protected)
        orelse path_keys_protected(Msg, Protected)
        orelse
            lists:any(
                fun({_Key, Value}) -> protected_term(Value, Protected, Opts, LinksLeft) end,
                maps:to_list(Msg)
            );
protected_term(_Other, _Protected, _Opts, _LinksLeft) ->
    false.

path_keys_protected(Msg, Protected) ->
    lists:any(
        fun(Key) ->
            case maps:find(Key, Msg) of
                {ok, Path} -> path_value_protected(Path, Protected);
                error -> false
            end
        end,
        ?PATH_KEYS
    ).

path_value_protected(Path, _Protected) when ?IS_LINK(Path) ->
    true;
path_value_protected(Path, Protected) ->
    try path_protected(Path, Protected)
    catch _:_ -> true
    end.

device_protected(not_found, _Protected) ->
    false;
device_protected(Device, Protected) ->
    DevicePath = normalize_path(Device),
    lists:any(
        fun(Prefix) ->
            DevicePath =:= Prefix orelse DevicePath =:= trim_leading_tilde(Prefix)
        end,
        Protected
    ).

path_protected(Path, Protected) ->
    PathNorm = normalize_path(Path),
    lists:any(
        fun(Prefix) ->
            prefix_match(PathNorm, Prefix)
                orelse prefix_match(PathNorm, trim_leading_tilde(Prefix))
        end,
        Protected
    ).

prefix_match(Path, Prefix) ->
    Path =:= Prefix orelse
        binary:match(Path, <<Prefix/binary, "/">>) =:=
            {0, byte_size(Prefix) + 1}.

normalize_path(Path) ->
    trim_leading_slash(trim_query(hb_util:bin(Path))).

trim_query(Path) ->
    case binary:split(Path, [<<"?">>, <<"#">>, <<"&">>]) of
        [Before | _] -> Before;
        [] -> Path
    end.

trim_leading_slash(<<"/", Rest/binary>>) ->
    trim_leading_slash(Rest);
trim_leading_slash(Path) ->
    Path.

trim_leading_tilde(<<"~", Rest/binary>>) ->
    Rest;
trim_leading_tilde(Path) ->
    Path.

authorize(HookReq, RequiredKeys, Opts) ->
    Request = hb_maps:get(<<"request">>, HookReq, #{}, Opts),
    Meta = hb_device:message_to_device(#{ <<"device">> => <<"meta@1.0">> }, Opts),
    case body_matches_request(HookReq, RequiredKeys, Opts)
            andalso Meta:is_operator(Request, RequiredKeys, Opts) of
        true ->
            {ok, HookReq};
        false ->
            {
                error,
                #{
                    <<"status">> => 401,
                    <<"body">> => <<"Unauthorized">>
                }
            }
    end.

body_matches_request(HookReq, RequiredKeys, Opts) ->
    case lists:member(<<"body">>, RequiredKeys) of
        false ->
            true;
        true ->
            Request = hb_maps:get(<<"request">>, HookReq, #{}, Opts),
            comparable_body(hb_maps:get(<<"body">>, Request, not_found, Opts), Opts)
                =:= comparable_body(
                    hb_maps:get(<<"body">>, HookReq, not_found, Opts),
                    Opts
                )
    end.

comparable_body(Body, Opts) ->
    try hb_message:id(hb_cache:ensure_all_loaded(Body, Opts), Opts)
    catch _:_ ->
        try
            case hb_cache:ensure_all_loaded(Body, Opts) of
                List when is_list(List) -> hb_util:list_to_numbered_message(List);
                Loaded -> Loaded
            end
        catch _:_ ->
            Body
        end
    end.

%%% Tests

unconfigured_request_test_parallel() ->
    HookReq = #{
        <<"request">> => #{ <<"path">> => <<"/~example@1.0/run">> },
        <<"body">> => []
    },
    ?assertEqual({ok, HookReq}, request(#{}, HookReq, #{})).

unprotected_request_test_parallel() ->
    Base = #{ <<"protected">> => [<<"~example@1.0">>] },
    HookReq = #{
        <<"request">> => #{ <<"path">> => <<"/~meta@1.0/info">> },
        <<"body">> => []
    },
    ?assertEqual({ok, HookReq}, request(Base, HookReq, #{})).

unsigned_protected_request_test_parallel() ->
    Owner = ar_wallet:new(),
    Base = #{ <<"protected">> => [<<"~example@1.0">>] },
    HookReq = #{
        <<"request">> => #{ <<"path">> => <<"/~example@1.0/run">> },
        <<"body">> => []
    },
    Opts = #{ <<"priv-wallet">> => Owner },
    ?assertMatch({error, #{ <<"status">> := 401 }}, request(Base, HookReq, Opts)).

unclaimed_operator_rejected_test_parallel() ->
    Base = #{ <<"protected">> => [<<"~example@1.0">>] },
    HookReq = #{
        <<"request">> => #{ <<"path">> => <<"/~example@1.0/run">> },
        <<"body">> => []
    },
    ?assertMatch({error, #{ <<"status">> := 401 }}, request(Base, HookReq, #{})).

signed_protected_request_test_parallel() ->
    Owner = ar_wallet:new(),
    Base = #{ <<"protected">> => [<<"~example@1.0">>] },
    Opts = #{ <<"priv-wallet">> => Owner },
    Signed =
        hb_message:commit(
            #{ <<"path">> => <<"/~example@1.0/run">> },
            Opts
        ),
    HookReq = #{ <<"request">> => Signed, <<"body">> => [] },
    ?assertEqual({ok, HookReq}, request(Base, HookReq, Opts)).

forged_committer_rejected_test_parallel() ->
    Owner = ar_wallet:new(),
    Attacker = ar_wallet:new(),
    Base = #{ <<"protected">> => [<<"~example@1.0">>] },
    OwnerAddr = hb_util:human_id(ar_wallet:to_address(Owner)),
    Signed =
        hb_message:commit(
            #{ <<"path">> => <<"/~example@1.0/run">> },
            #{ <<"priv-wallet">> => Attacker }
        ),
    Forged =
        Signed#{
            <<"commitments">> =>
                hb_maps:map(
                    fun(_ID, Commitment) ->
                        Commitment#{ <<"committer">> => OwnerAddr }
                    end,
                    hb_maps:get(<<"commitments">>, Signed, #{}, #{})
                )
        },
    HookReq = #{ <<"request">> => Forged, <<"body">> => [] },
    ?assertMatch(
        {error, #{ <<"status">> := 401 }},
        request(Base, HookReq, #{ <<"priv-wallet">> => Owner })
    ).

body_protection_requires_signed_body_test_parallel() ->
    Owner = ar_wallet:new(),
    Base = #{ <<"protected">> => [<<"~example@1.0">>] },
    Body = [#{ <<"path">> => <<"/~example@1.0/run">> }],
    Opts = #{
        <<"priv-wallet">> => Owner,
        <<"store">> => hb_test_utils:test_store()
    },
    SignedWithoutBody =
        hb_message:commit(
            #{ <<"path">> => <<"/~relay@1.0/call">> },
            Opts
        ),
    HookReqWithoutBody = #{ <<"request">> => SignedWithoutBody, <<"body">> => Body },
    ?assertMatch(
        {error, #{ <<"status">> := 401 }},
        request(Base, HookReqWithoutBody, Opts)
    ),
    SignedWithBody =
        hb_message:commit(
            #{
                <<"path">> => <<"/~relay@1.0/call">>,
                <<"body">> => Body
            },
            Opts
        ),
    HookReqWithBody = #{ <<"request">> => SignedWithBody, <<"body">> => Body },
    ?assertEqual({ok, HookReqWithBody}, request(Base, HookReqWithBody, Opts)).

link_follow_limit_fails_closed_test_parallel() ->
    Base =
        #{
            <<"protected">> => [<<"~example@1.0">>],
            <<"max-link-follows">> => 0
        },
    HookReq =
        #{
            <<"request">> => #{ <<"path">> => <<"/~relay@1.0/call">> },
            <<"body">> =>
                [{link, <<"unfollowed-body-link">>, #{ <<"type">> => <<"link">> }}]
        },
    ?assertMatch({error, #{ <<"status">> := 401 }}, request(Base, HookReq, #{})).

cron_path_is_protected_test_parallel() ->
    Owner = ar_wallet:new(),
    Base = #{ <<"protected">> => [<<"~example@1.0">>] },
    HookReq = #{
        <<"request">> =>
            #{
                <<"path">> => <<"/~cron@1.0/once">>,
                <<"cron-path">> => <<"/~example@1.0/run">>
            },
        <<"body">> => []
    },
    Opts = #{ <<"priv-wallet">> => Owner },
    ?assertMatch({error, #{ <<"status">> := 401 }}, request(Base, HookReq, Opts)).

relay_path_is_protected_test_parallel() ->
    Owner = ar_wallet:new(),
    Base = #{ <<"protected">> => [<<"~example@1.0">>] },
    HookReq = #{
        <<"request">> =>
            #{
                <<"path">> => <<"/~relay@1.0/call">>,
                <<"relay-path">> => <<"/~example@1.0/run">>
            },
        <<"body">> => []
    },
    Opts = #{ <<"priv-wallet">> => Owner },
    ?assertMatch({error, #{ <<"status">> := 401 }}, request(Base, HookReq, Opts)).

configured_node_protects_copycat_test_parallel() ->
    Owner = ar_wallet:new(),
    NodeOpts =
        #{
            <<"priv-wallet">> => Owner,
            <<"store">> => hb_test_utils:test_store(),
            <<"on">> =>
                #{
                    <<"request">> =>
                        #{
                            <<"device">> => <<"owner-auth@1.0">>,
                            <<"protected">> => [<<"~copycat@1.0">>]
                        }
                }
        },
    Node = hb_http_server:start_node(NodeOpts),
    Req =
        #{
            <<"path">> => <<"/~copycat@1.0/arweave">>,
            <<"mode">> => <<"invalid">>
        },
    ?assertMatch({error, #{ <<"status">> := 401 }}, hb_http:get(Node, Req, #{})),
    SignedReq = hb_message:commit(Req, NodeOpts),
    ?assertMatch({error, <<"Unsupported mode `invalid`", _/binary>>},
        hb_http:get(Node, SignedReq, #{})).

auth_hook_signature_does_not_bypass_copycat_protection_test_parallel() ->
    Owner = ar_wallet:new(),
    NodeOpts =
        #{
            <<"priv-wallet">> => Owner,
            <<"store">> => hb_test_utils:test_store(),
            <<"on">> =>
                #{
                    <<"request">> =>
                        [
                            #{
                                <<"device">> => <<"auth-hook@1.0">>,
                                <<"path">> => <<"request">>,
                                <<"secret-provider">> =>
                                    #{
                                        <<"device">> => <<"cookie@1.0">>
                                    }
                            },
                            #{
                                <<"device">> => <<"owner-auth@1.0">>,
                                <<"protected">> => [<<"~copycat@1.0">>]
                            }
                        ]
                }
        },
    Node = hb_http_server:start_node(NodeOpts),
    Req =
        #{
            <<"path">> => <<"/~copycat@1.0/arweave">>,
            <<"mode">> => <<"invalid">>
        },
    ?assertMatch({error, #{ <<"status">> := 401 }}, hb_http:get(Node, Req, #{})).
