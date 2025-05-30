%%% @doc This module implements the relay device, which is responsible for
%%% relaying messages between nodes and other HTTP(S) endpoints.
%%%
%%% It can be called in either `call' or `cast' mode. In `call' mode, it
%%% returns a `{ok, Result}' tuple, where `Result' is the response from the 
%%% remote peer to the message sent. In `cast' mode, the invocation returns
%%% immediately, and the message is relayed asynchronously. No response is given
%%% and the device returns `{ok, <<"OK">>}'.
%%% 
%%% Example usage:
%%% 
%%% <pre>
%%%     curl /~relay@.1.0/call?method=GET?0.path=https://www.arweave.net/
%%% </pre>
-module(dev_relay).
%%% Execute synchronous and asynchronous relay requests.
-export([call/3, cast/3]).
%%% Re-route requests that would be executed locally to other peers, according
%%% to the node's routing table.
-export([request/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Execute a `call' request using a node's routes.
%% 
%% Supports the following options:
%% - `target': The target message to relay. Defaults to the original message.
%% - `relay-path': The path to relay the message to. Defaults to the original path.
%% - `method': The method to use for the request. Defaults to the original method.
%% - `requires-sign': Whether the request requires signing before dispatching.
%% Defaults to `false'.
call(M1, RawM2, Opts) ->
    {ok, BaseTarget} = hb_message:find_target(M1, RawM2, Opts),
    RelayPath =
        hb_ao:get_first(
            [
                {BaseTarget, <<"path">>},
                {RawM2, <<"relay-path">>},
                {M1, <<"relay-path">>},
                {M1, <<"path">>}
            ],
            Opts
        ),
    RelayMethod =
        hb_ao:get_first(
            [
                {BaseTarget, <<"method">>},
                {RawM2, <<"relay-method">>},
                {M1, <<"relay-method">>},
                {RawM2, <<"method">>},
                {M1, <<"method">>}
            ],
            Opts
        ),
    RelayBody =
        hb_ao:get_first(
            [
                {BaseTarget, <<"body">>},
                {RawM2, <<"relay-body">>},
                {M1, <<"relay-body">>},
                {RawM2, <<"body">>},
                {M1, <<"body">>}
            ],
            Opts
        ),
    TargetMod1 = BaseTarget#{
        <<"method">> => RelayMethod,
        <<"body">> => RelayBody,
        <<"path">> => RelayPath
    },
    TargetMod2 =
        case hb_ao:get(<<"requires-sign">>, BaseTarget, false, Opts) of
            true -> hb_message:commit(TargetMod1, Opts);
            false -> TargetMod1
        end,
    Client =
        case hb_ao:get(<<"http-client">>, BaseTarget, Opts) of
            not_found -> hb_opts:get(relay_http_client, Opts);
            RequestedClient -> RequestedClient
        end,
    ?event({relaying_message, TargetMod2}),
    % Let `hb_http:request/2' handle finding the peer and dispatching the request.
    hb_http:request(TargetMod2, Opts#{ http_client => Client }).

%% @doc Execute a request in the same way as `call/3', but asynchronously. Always
%% returns `<<"OK">>'.
cast(M1, M2, Opts) ->
    spawn(fun() -> call(M1, M2, Opts) end),
    {ok, <<"OK">>}.

%% @doc Preprocess a request to check if it should be relayed to a different node.
request(_Msg1, Msg2, Opts) ->
    {ok,
        #{
            <<"body">> =>
                [
                    #{ <<"device">> => <<"relay@1.0">> },
                    #{
                        <<"path">> => <<"call">>,
                        <<"target">> => <<"body">>,
                        <<"body">> =>
                            hb_ao:get(<<"request">>, Msg2, Opts#{ hashpath => ignore })
                    }
                ]
        }
    }.


%%% Tests

call_get_test() ->
    application:ensure_all_started([hb]),
    {ok, #{<<"body">> := Body}} =
        hb_ao:resolve(
            #{
                <<"device">> => <<"relay@1.0">>,
                <<"method">> => <<"GET">>,
                <<"path">> => <<"https://www.google.com/">>
            },
            <<"call">>,
            #{ protocol => http2 }
        ),
    ?assertEqual(true, byte_size(Body) > 10_000).

%% @doc Test that the `preprocess/3' function re-routes a request to remote
%% peers, according to the node's routing table.
preprocessor_reroute_to_nearest_test() ->
    Nodes = 
        lists:map(
            fun(_) ->
                Node = hb_http_server:start_node(),
                {ok, Address} =
                    hb_http:get(
                        Node,
                        <<"/~meta@1.0/info/address">>,
                        #{}
                    ),
                {Address, Node}
            end,
            lists:seq(1, 3)
        ),
    Node =
        hb_http_server:start_node(#{
            priv_wallet => ar_wallet:new(),
            routes =>
                [
                    #{
                        <<"template">> => <<"/.*/.*/.*">>,
                        <<"strategy">> => <<"Nearest">>,
                        <<"nodes">> =>
                            lists:map(
                                fun({Address, Node}) ->
                                    #{
                                        <<"prefix">> => Node,
                                        <<"wallet">> => Address
                                    }
                                end,
                                Nodes
                            )
                    }
                ],
            on => #{ <<"request">> => #{ <<"device">> => <<"relay@1.0">> } }
        }),
    Res =
        lists:map(
            fun(_) ->
                hb_util:ok(
                    hb_http:get(
                        Node,
                        <<"/~meta@1.0/info/address">>,
                        #{}
                    )
                )
            end,
            lists:seq(1, 3)
        ),
    ?assertEqual(1, sets:size(sets:from_list(Res))).
