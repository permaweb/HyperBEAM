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
        true ->
            authorize(HookReq, Opts)
    end.

%% @doc Check whether the hook payload can invoke a protected device/path.
protected_request(Base, HookReq, Opts) ->
    Protected = protected(Base, Opts),
    lists:any(
        fun(Term) -> protected_term(Term, Protected, Opts) end,
        [
            hb_maps:get(<<"request">>, HookReq, #{}, Opts),
            hb_maps:get(<<"body">>, HookReq, [], Opts)
        ]
    ).

protected(Base, Opts) ->
    case hb_maps:get(<<"protected">>, Base, [], Opts) of
        List when is_list(List) -> [normalize_path(P) || P <- List];
        One -> [normalize_path(One)]
    end.

protected_term(Bin, Protected, _Opts) when is_binary(Bin) ->
    path_protected(Bin, Protected);
protected_term({as, Device, Msg}, Protected, Opts) ->
    device_protected(Device, Protected) orelse protected_term(Msg, Protected, Opts);
protected_term({resolve, Msg}, Protected, Opts) ->
    protected_term(Msg, Protected, Opts);
protected_term(List, Protected, Opts) when is_list(List) ->
    lists:any(fun(Item) -> protected_term(Item, Protected, Opts) end, List);
protected_term(Msg, Protected, Opts) when is_map(Msg) ->
    device_protected(hb_maps:get(<<"device">>, Msg, not_found, Opts), Protected)
        orelse path_keys_protected(Msg, Protected, Opts)
        orelse
            lists:any(
                fun({_Key, Value}) -> protected_term(Value, Protected, Opts) end,
                hb_maps:to_list(Msg)
            );
protected_term(_Other, _Protected, _Opts) ->
    false.

path_keys_protected(Msg, Protected, Opts) ->
    lists:any(
        fun(Key) ->
            case hb_maps:find(Key, Msg, Opts) of
                {ok, Path} -> path_protected(Path, Protected);
                error -> false
            end
        end,
        ?PATH_KEYS
    ).

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

authorize(HookReq, Opts) ->
    Request = hb_maps:get(<<"request">>, HookReq, #{}, Opts),
    case is_admin(Request, Opts) of
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

%% @doc Same admin authorization semantics used by `~meta@1.0' when updating
%% the node message.
is_admin(Request, Opts) ->
    RequestSigners = hb_message:signers(Request, Opts),
    ValidOperator =
        hb_util:bin(
            hb_opts:get(
                operator,
                case hb_opts:get(priv_wallet, no_viable_wallet, Opts) of
                    no_viable_wallet -> unclaimed;
                    Wallet -> ar_wallet:to_address(Wallet)
                end,
                Opts
            )
        ),
    EncOperator =
        case ValidOperator of
            <<"unclaimed">> -> unclaimed;
            NativeAddress -> hb_util:human_id(NativeAddress)
        end,
    EncOperator == unclaimed orelse lists:member(EncOperator, RequestSigners).

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
