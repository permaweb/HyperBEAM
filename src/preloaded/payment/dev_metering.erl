%%% @doc Dynamic pricing device for P4.
%%%
%%% `metering@1.0' records resource usage in the current process during a P4
%%% request/response lifecycle. It is intended to be used as a P4 pricing
%%% device:
%%%
%%%     `estimate/3' opens a process-local metering session.
%%%     `consume/3' increments resource usage during that session.
%%%     `price/3' closes the session and returns the integer charge.
%%%
%%% Calls to `consume/3' outside an active session are no-ops, so callers do
%%% not need to check whether metering is enabled. Resource names are normalized
%%% keys, such as `arweave-bytes' and `beam-reductions'. The device also meters
%%% `request-bytes' and `response-bytes' from the bodies P4 supplies. The
%%% operator sets `metering-rates' in the node message as a map of resource name
%%% to AO token units per resource unit.
-module(dev_metering).
-export([info/1, estimate/3, price/3, is_active/0, consume/3]).

-include_lib("eunit/include/eunit.hrl").

-define(METERING_KEY, {dev_metering, state}).
-define(BEAM_REDUCTIONS, <<"beam-reductions">>).
-define(REQUEST_BYTES, <<"request-bytes">>).
-define(RESPONSE_BYTES, <<"response-bytes">>).

%% @doc Device API information.
info(_) ->
    #{
        exports =>
            [
                <<"estimate">>,
                <<"price">>
            ]
    }.

%% @doc Start a metering session for the request.
estimate(_Base, EstimateReq, Opts) ->
    {reductions, Reductions} = erlang:process_info(self(), reductions),
    erlang:put(
        ?METERING_KEY,
        #{
            start_reductions => Reductions,
            meters => #{}
        }
    ),
    consume(?REQUEST_BYTES, body_size(EstimateReq, Opts), Opts),
    {ok, 0}.

%% @doc Close the metering session and calculate the final AO token price.
price(_Base, PriceReq, Opts) ->
    consume(?RESPONSE_BYTES, body_size(PriceReq, Opts), Opts),
    Rates = hb_opts:get(<<"metering-rates">>, #{}, Opts),
    Price =
        maps:fold(
            fun(Resource, Amount, Acc) ->
                Rate = hb_util:int(hb_maps:get(Resource, Rates, 0, Opts)),
                Acc + (Amount * Rate)
            end,
            0,
            maps:get(
                meters,
                meter_reductions(erlang:get(?METERING_KEY)),
                #{}
            )
        ),
    erlang:erase(?METERING_KEY),
    {ok, Price}.

%% @doc Return whether the current process has an active metering session.
is_active() ->
    erlang:get(?METERING_KEY) =/= undefined.

%% @doc Helper API for other devices.
consume(Resource, Req, Opts) when is_map(Req) ->
    consume(Resource, hb_maps:get(<<"amount">>, Req, 0, Opts), Opts);
consume(Resource, Amount, _Opts) ->
    case erlang:get(?METERING_KEY) of
        undefined ->
            ok;
        State ->
            AmountInt = hb_util:int(Amount),
            case AmountInt >= 0 of
                true ->
                    erlang:put(
                        ?METERING_KEY,
                        add_meter(
                            hb_ao:normalize_key(Resource),
                            AmountInt,
                            State
                        )
                    ),
                    ok;
                false ->
                    error({invalid_meter_amount, Amount})
            end
    end.

%% @doc Size the body P4 hands to the pricing device.
%%
%% `estimate' receives the inbound request and `price' the result, so a body
%% measured here covers whatever the node carried, including payload a device
%% relayed on another node's behalf. A `bundle: true' commitment includes linked
%% content in the body, so those links are loaded before sizing. Otherwise links
%% are sized as links.
%%
%% The size is of the ETF encoding, not the wire form. It is stable and
%% monotone in payload size, but a payer cannot reproduce it from the bytes it
%% sent, and the ratio to wire size varies by request shape.
body_size(Req, Opts) when is_map(Req) ->
    case hb_maps:get(<<"body">>, Req, not_found, Opts) of
        not_found -> 0;
        Body -> term_size(maybe_load_body(Body, Opts))
    end;
body_size(_Req, _Opts) ->
    0.

%% @doc Load linked content included by any bundle commitment.
maybe_load_body(Body, Opts) ->
    Commitments =
        hb_message:commitments(#{ <<"bundle">> => <<"true">> }, Body, Opts),
    case map_size(Commitments) of
        0 -> Body;
        _ -> hb_cache:ensure_all_loaded(Body, Opts)
    end.

%% @doc Return the encoded size of a body term.
term_size(Bin) when is_binary(Bin) -> byte_size(Bin);
term_size(Term) ->
    try erlang:external_size(Term)
    catch _:_ -> 0
    end.

%% @doc Add the process reductions delta to the active metering state.
meter_reductions(undefined) ->
    #{ meters => #{} };
meter_reductions(State = #{ start_reductions := Start }) ->
    {reductions, Current} = erlang:process_info(self(), reductions),
    add_meter(
        ?BEAM_REDUCTIONS,
        max(0, Current - Start),
        State
    ).

%% @doc Add a resource amount to the meter state.
add_meter(Resource, Amount, State) ->
    Meters = maps:get(meters, State, #{}),
    State#{
        meters =>
            Meters#{
                Resource => maps:get(Resource, Meters, 0) + Amount
            }
    }.

%%% Tests

%% @doc Metering outside an active session is a no-op.
inactive_meter_noop_test() ->
    erlang:erase(?METERING_KEY),
    ok = consume(<<"arweave-bytes">>, 5, #{}),
    ?assertEqual(false, is_active()).

%% @doc The helper API meters resources and prices them via configured rates.
consume_price_test() ->
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"metering-rates">> => #{
            <<"arweave-bytes">> => 3,
            ?BEAM_REDUCTIONS => 0
        }
    },
    Metering = #{ <<"device">> => <<"metering@1.0">> },
    {ok, 0} = hb_ao:resolve(Metering, #{ <<"path">> => <<"estimate">> }, Opts),
    ok = consume(<<"arweave-bytes">>, 5, Opts),
    {ok, 15} = hb_ao:resolve(Metering, #{ <<"path">> => <<"price">> }, Opts).

%% @doc Resource consumption is not exposed as an AO-Core key.
consume_is_not_device_key_test() ->
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Metering = #{ <<"device">> => <<"metering@1.0">> },
    ?assertMatch(
        {error, _},
        hb_ao:resolve(
            Metering,
            #{
                <<"path">> => <<"consume">>,
                <<"resource">> => <<"arweave-bytes">>,
                <<"amount">> => 5
            },
            Opts
        )
    ).

%% @doc BEAM reductions are metered between estimate and price.
beam_reductions_price_test() ->
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"metering-rates">> => #{ ?BEAM_REDUCTIONS => 1 }
    },
    Metering = #{ <<"device">> => <<"metering@1.0">> },
    {ok, 0} = hb_ao:resolve(Metering, #{ <<"path">> => <<"estimate">> }, Opts),
    lists:foreach(
        fun(_) -> erlang:phash2(rand:bytes(16)) end,
        lists:seq(1, 10)
    ),
    {ok, Price} = hb_ao:resolve(Metering, #{ <<"path">> => <<"price">> }, Opts),
    ?assert(Price > 0).

%% @doc P4 charges a dynamic metering price during response processing.
p4_response_charge_test() ->
    HostWallet = ar_wallet:new(),
    Wallet = ar_wallet:new(),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    Rate = 2,
    Item =
        hb_message:commit(
            #{
                <<"data">> => <<"metered-bundler-item">>,
                <<"test">> => <<"p4-response-metering">>
            },
            #{ <<"priv-wallet">> => ar_wallet:new() }
        ),
    {ServerHandle, GatewayOpts} =
        hb_mock_server:start_arweave_gateway(
            #{
                price => {200, <<"12345">>},
                tx_anchor => {200, hb_util:encode(rand:bytes(32))}
            }
        ),
    Processor =
        #{
            <<"device">> => <<"p4@1.0">>,
            <<"ledger-device">> => <<"simple-pay@1.0">>,
            <<"pricing-device">> => <<"metering@1.0">>
        },
    BaseOpts =
        GatewayOpts#{
            <<"priv-wallet">> => HostWallet,
            <<"store">> => hb_test_utils:test_store(),
            <<"bundler-max-items">> => 1,
            <<"metering-rates">> => #{
                <<"arweave-bytes">> => Rate,
                ?BEAM_REDUCTIONS => 0
            },
            <<"operator">> => ar_wallet:to_address(HostWallet),
            <<"on">> => #{
                <<"request">> => Processor,
                <<"response">> => Processor
            }
        },
    ItemSize =
        byte_size(
            ar_bundles:serialize(
                hb_message:convert(
                    Item,
                    #{
                        <<"device">> => <<"ans104@1.0">>,
                        <<"bundle">> => true
                    },
                    <<"structured@1.0">>,
                    BaseOpts
                )
            )
        ),
    Opts =
        BaseOpts#{
            <<"simple-pay-ledger">> => #{ Address => (ItemSize * Rate) + 50 }
        },
    try
        Node = hb_http_server:start_node(Opts),
        UploadReq =
            hb_message:commit(
                #{
                    <<"path">> => <<"/~bundler@1.0/tx">>,
                    <<"bundler-subject">> => <<"body">>,
                    <<"body">> => Item
                },
                Opts#{ <<"priv-wallet">> => Wallet }
            ),
        ?assertMatch({ok, _}, hb_http:post(Node, UploadReq, Opts)),
        [_] = hb_mock_server:get_requests(tx, 1, ServerHandle),
        {ok, Balance} =
            hb_http:get(
                Node,
                hb_message:commit(
                    #{ <<"path">> => <<"/~p4@1.0/balance">> },
                    Opts#{ <<"priv-wallet">> => Wallet }
                ),
                Opts
            ),
        ?assertEqual(50, Balance)
    after
        hb_mock_server:stop(ServerHandle)
    end.

%% @doc Request and response body meters contribute to the same P4 session.
request_and_response_bytes_test() ->
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"metering-rates">> => #{
            ?REQUEST_BYTES => 1,
            ?RESPONSE_BYTES => 1,
            ?BEAM_REDUCTIONS => 0
        }
    },
    Metering = #{ <<"device">> => <<"metering@1.0">> },
    Request = #{ <<"body">> => binary:copy(<<"q">>, 1000) },
    Response = #{ <<"body">> => binary:copy(<<"r">>, 50000) },
    {ok, 0} =
        hb_ao:resolve(
            Metering,
            #{ <<"path">> => <<"estimate">>, <<"body">> => Request },
            Opts
        ),
    {ok, Price} =
        hb_ao:resolve(
            Metering,
            #{ <<"path">> => <<"price">>, <<"body">> => Response },
            Opts
        ),
    % Both bodies are charged, so the total tracks the larger one and cannot be
    % explained by the request alone.
    ?assert(Price > 50000),
    ?assert(Price < 60000).

%% @doc Response byte charges grow with the loaded payload.
response_bytes_scale_with_payload_test() ->
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"metering-rates">> => #{ ?RESPONSE_BYTES => 1 }
    },
    Metering = #{ <<"device">> => <<"metering@1.0">> },
    Price =
        fun(Bytes) ->
            {ok, 0} =
                hb_ao:resolve(Metering, #{ <<"path">> => <<"estimate">> }, Opts),
            {ok, P} =
                hb_ao:resolve(
                    Metering,
                    #{
                        <<"path">> => <<"price">>,
                        <<"body">> => #{ <<"body">> => binary:copy(<<"x">>, Bytes) }
                    },
                    Opts
                ),
            P
        end,
    Small = Price(1000),
    Large = Price(1000000),
    % A flat charge means the body is not reaching the meter.
    ?assert(Large > Small * 100).

%% @doc Linked bodies are loaded only when a commitment includes the bundle.
linked_body_metering_respects_bundle_commitment_test() ->
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"priv-wallet">> => ar_wallet:new(),
        <<"metering-rates">> => #{ ?RESPONSE_BYTES => 1 }
    },
    Metering = #{ <<"device">> => <<"metering@1.0">> },
    Payload = binary:copy(<<"x">>, 1000000),
    Price =
        fun(Bundle) ->
            Response =
                hb_message:commit(
                    #{ <<"body">> => #{ <<"payload">> => Payload } },
                    Opts,
                    #{
                        <<"commitment-device">> => <<"httpsig@1.0">>,
                        <<"bundle">> => Bundle
                    }
                ),
            {ok, _} = hb_cache:write(Response, Opts),
            {ok, LinkedResponse} =
                hb_cache:read(hb_message:id(Response, all, Opts), Opts),
            {ok, 0} =
                hb_ao:resolve(Metering, #{ <<"path">> => <<"estimate">> }, Opts),
            {ok, Result} =
                hb_ao:resolve(
                    Metering,
                    #{ <<"path">> => <<"price">>, <<"body">> => LinkedResponse },
                    Opts
                ),
            Result
        end,
    LinkedPrice = Price(false),
    BundledPrice = Price(true),
    ?assert(LinkedPrice < 10000),
    ?assert(BundledPrice > LinkedPrice * 100).

%% @doc Byte meters remain inert when the operator configures no rate.
unpriced_resources_cost_nothing_test() ->
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"metering-rates">> => #{}
    },
    Metering = #{ <<"device">> => <<"metering@1.0">> },
    {ok, 0} =
        hb_ao:resolve(
            Metering,
            #{
                <<"path">> => <<"estimate">>,
                <<"body">> => #{ <<"body">> => binary:copy(<<"x">>, 100000) }
            },
            Opts
        ),
    {ok, 0} =
        hb_ao:resolve(
            Metering,
            #{
                <<"path">> => <<"price">>,
                <<"body">> => #{ <<"body">> => binary:copy(<<"x">>, 100000) }
            },
            Opts
        ).
