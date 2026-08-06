%%% @doc Price leases by the exact-name probability mass they occupy over time.
%%%
%%% The device implements the structured pricing seam of `~spectrum@1.0'. It
%%% owns the probability model, occupancy calculation, bonding curve and its
%%% inverse; the registry retains only the opaque lease metadata it returns.
-module(dev_probability_time).
-implements(<<"probability-time@1.0">>).
-export([info/0, blocks/3, price/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_PROBABILITY_DEVICE, <<"markov@1.0">>).
-define(END, infinity).

%% @doc Use `~message@1.0' for keys this device does not implement.
info() -> #{}.

%% @doc Return the most whole registration blocks a payment can buy.
blocks(Base, Req, Opts) ->
    safely(
        fun() ->
            maybe
                {ok, Payment} ?= amount(<<"blocks">>, Req, Opts),
                true ?= ar(Req, Opts),
                {ok, Params} ?= parameters(Base, Opts),
                {ok, Name} ?= name(Req, Opts),
                {ok, Height, Start} ?= interval(Base, Req, Opts),
                {ok, Weight} ?= weight(Name, Base, Height, Opts),
                {ok, Slabs} ?= slabs(Base, Start, Opts),
                {ok, Bought} ?=
                    affordable(Payment, Slabs, Weight, Params),
                {ok,
                    #{
                        <<"blocks">> => Bought,
                        <<"pricing">> => #{ <<"weight">> => Weight }
                    }}
            else
                _ -> {error, invalid}
            end
        end
    ).

%% @doc Return the winston required to buy a number of registration blocks.
price(Base, Req, Opts) ->
    safely(
        fun() ->
            maybe
                {ok, Blocks} ?= amount(<<"price">>, Req, Opts),
                true ?= ar(Req, Opts),
                {ok, Params} ?= parameters(Base, Opts),
                {ok, Name} ?= name(Req, Opts),
                {ok, Height, Start} ?= interval(Base, Req, Opts),
                {ok, Weight} ?= weight(Name, Base, Height, Opts),
                {ok, Slabs} ?= slabs(Base, Start, Opts),
                {ok, Cost} ?= cost(Blocks, Slabs, Weight, Params),
                {ok, ceil(Cost)}
            else
                _ -> {error, invalid}
            end
        end
    ).

%% @doc Turn arithmetic or malformed linked state into a refused quote.
safely(Fun) ->
    try Fun()
    catch
        _:_ -> {error, invalid}
    end.

%% @doc Read and validate the curve's required parameters.
parameters(Base, Opts) ->
    maybe
        {ok, Target} ?= positive(state(<<"target-occupancy">>, Base, not_found, Opts)),
        true ?= Target < 1.0,
        {ok, Price} ?= positive(state(<<"price-at-target">>, Base, not_found, Opts)),
        {ok, {Target, Price}}
    else
        _ -> {error, invalid}
    end.

%% @doc Read the priced name from the request.
name(Req, Opts) ->
    case field(<<"name">>, Req, not_found, Opts) of
        Name when is_binary(Name) -> {ok, Name};
        _ -> {error, invalid}
    end.

%% @doc Find the current height and first block of the interval being bought.
interval(Base, Req, Opts) ->
    maybe
        {ok, Height} ?=
            non_negative(state(<<"spectrum-height">>, Base, 0, Opts)),
        {ok, Duration} ?=
            non_negative(field(<<"duration">>, Req, 0, Opts)),
        {ok, Height, Height + Duration}
    end.

%% @doc Retain a held lease's weight or calculate a free name's weight.
weight(Name, Base, Height, Opts) ->
    case lease(Name, Base, Opts) of
        {ok, Record} ->
            case held(Record, Height, Opts) of
                true -> lease_weight(Record, Opts);
                false -> new_weight(Name, Base, Opts)
            end;
        not_found -> new_weight(Name, Base, Opts)
    end.

%% @doc Calculate a new lease's probability and optional transformed weight.
new_weight(Name, Base, Opts) ->
    maybe
        {ok, Probability} ?= probability(Name, Base, Opts),
        case state(<<"weighting-device">>, Base, not_found, Opts) of
            not_found -> {ok, Probability};
            Device -> transformed(Name, Probability, Device, Base, Opts)
        end
    end.

%% @doc Calculate an exact-name probability through the configured device.
probability(Name, Base, Opts) ->
    Device =
        state(
            <<"probability-device">>,
            Base,
            ?DEFAULT_PROBABILITY_DEVICE,
            Opts
        ),
    maybe
        true ?= is_binary(Device) orelse is_map(Device),
        {ok, Answer} ?=
            hb_ao:resolve(
                Base#{ <<"device">> => Device },
                #{
                    <<"path">> => <<"likelihood">>,
                    <<"body">> => Name,
                    <<"include-end">> => true,
                    <<"result-mode">> => <<"float">>
                },
                Opts
            ),
        positive(Answer)
    else
        _ -> {error, invalid}
    end.

%% @doc Apply an optional replacement weighting function to a probability.
transformed(Name, Probability, Device, Base, Opts) ->
    maybe
        true ?= is_binary(Device) orelse is_map(Device),
        {ok, Answer} ?=
            hb_ao:resolve(
                Base#{ <<"device">> => Device },
                #{
                    <<"path">> => <<"weight">>,
                    <<"name">> => Name,
                    <<"probability">> => Probability
                },
                Opts
            ),
        positive(Answer)
    else
        _ -> {error, invalid}
    end.

%% @doc Return whether a lease is still retained at a height.
held(Record, Height, Opts) ->
    Grace = integer(<<"grace">>, Record, integer(<<"deadline">>, Record, 0, Opts), Opts),
    Height < Grace.

%% @doc Read the immutable weight retained on a lease.
lease_weight(Record, Opts) ->
    Pricing = hb_maps:get(<<"pricing">>, Record, not_found, Opts),
    case Pricing of
        Map when is_map(Map) -> positive(hb_maps:get(<<"weight">>, Map, not_found, Opts));
        _ -> {error, invalid}
    end.

%% @doc Construct occupancy slabs beginning at `Start'.
slabs(Base, Start, Opts) ->
    Names = hb_cache:ensure_all_loaded(state(<<"names">>, Base, #{}, Opts), Opts),
    case is_map(Names) of
        true ->
            maybe
                {ok, Occupancy, Expiries} ?=
                    occupancy(lists:sort(hb_maps:keys(Names, Opts)), Names, Start, Opts),
                true ?= Occupancy < 1.0,
                {ok, build_slabs(Start, Occupancy, lists:sort(maps:to_list(Expiries)), [])}
            else
                _ -> {error, invalid}
            end;
        false -> {error, invalid}
    end.

%% @doc Sum active weights and group their expiry events.
occupancy(Keys, Names, Start, Opts) ->
    occupancy(Keys, Names, Start, 0.0, #{}, Opts).

%% @doc Fold leases into occupancy without growing with the namespace size.
occupancy([], _Names, _Start, Occupancy, Expiries, _Opts) ->
    {ok, Occupancy, Expiries};
occupancy([Name | Rest], Names, Start, Occupancy, Expiries, Opts) ->
    Record =
        hb_cache:ensure_all_loaded(
            hb_maps:get(Name, Names, not_found, Opts),
            Opts
        ),
    case is_map(Record) of
        true ->
            maybe
                {ok, NewOccupancy, NewExpiries} ?=
                    occupied(Record, Start, Occupancy, Expiries, Opts),
                occupancy(
                    Rest,
                    Names,
                    Start,
                    NewOccupancy,
                    NewExpiries,
                    Opts
                )
            end;
        false -> {error, invalid}
    end.

%% @doc Add one lease to occupancy if its deadline follows the interval start.
occupied(Record, Start, Occupancy, Expiries, Opts) ->
    Deadline = integer(<<"deadline">>, Record, 0, Opts),
    case Deadline > Start of
        false -> {ok, Occupancy, Expiries};
        true ->
            maybe
                {ok, Weight} ?= lease_weight(Record, Opts),
                {ok,
                    Occupancy + Weight,
                    maps:update_with(
                        Deadline,
                        fun(Existing) -> Existing + Weight end,
                        Weight,
                        Expiries
                    )}
            end
    end.

%% @doc Turn expiry events into constant-occupancy intervals and a zero tail.
build_slabs(_At, _Occupancy, [], Acc) ->
    lists:reverse([{?END, 0.0} | Acc]);
build_slabs(At, Occupancy, [{Deadline, Expired} | Rest], Acc) ->
    build_slabs(
        Deadline,
        max(0.0, Occupancy - Expired),
        Rest,
        [{Deadline - At, Occupancy} | Acc]
    ).

%% @doc Return the greatest integer duration whose rounded price is affordable.
affordable(Payment, Slabs, Weight, Params) ->
    maybe
        {ok, Rough} ?= rough_blocks(float(Payment), Slabs, Weight, Params, 0.0, 0),
        adjust(Rough, Payment, Slabs, Weight, Params)
    end.

%% @doc Consume finite occupancy slabs until the payment is exhausted.
rough_blocks(Payment, [{?END, Occupancy}], Weight, Params, Spent, Blocks) ->
    maybe
        {ok, Rate} ?= rate(Occupancy, Weight, Params),
        true ?= Rate > 0.0,
        {ok, Blocks + trunc(max(0.0, Payment - Spent) / Rate)}
    else
        _ -> {error, invalid}
    end;
rough_blocks(Payment, [{Length, Occupancy} | Rest], Weight, Params, Spent, Blocks) ->
    maybe
        {ok, Rate} ?= rate(Occupancy, Weight, Params),
        Price = Rate * Length,
        case Spent + Price =< Payment of
            true ->
                rough_blocks(
                    Payment,
                    Rest,
                    Weight,
                    Params,
                    Spent + Price,
                    Blocks + Length
                );
            false ->
                {ok, Blocks + trunc(max(0.0, Payment - Spent) / Rate)}
        end
    end.

%% @doc Correct the floating division boundary against the normative price.
adjust(Blocks, Payment, Slabs, Weight, Params) ->
    maybe
        {ok, Cost} ?= cost(Blocks, Slabs, Weight, Params),
        case ceil(Cost) =< Payment of
            true -> adjust_up(Blocks, Payment, Slabs, Weight, Params);
            false -> adjust(Blocks - 1, Payment, Slabs, Weight, Params)
        end
    end.

%% @doc Raise a conservative duration to the greatest affordable block.
adjust_up(Blocks, Payment, Slabs, Weight, Params) ->
    maybe
        {ok, Next} ?= cost(Blocks + 1, Slabs, Weight, Params),
        case ceil(Next) =< Payment of
            true -> adjust_up(Blocks + 1, Payment, Slabs, Weight, Params);
            false -> {ok, Blocks}
        end
    end.

%% @doc Sum the curve over an integer number of constant-occupancy blocks.
cost(Blocks, Slabs, Weight, Params) when Blocks >= 0 ->
    cost(Blocks, Slabs, Weight, Params, 0.0);
cost(_Blocks, _Slabs, _Weight, _Params) -> {error, invalid}.

%% @doc Accumulate a price over the remaining occupancy slabs.
cost(0, _Slabs, _Weight, _Params, Acc) -> {ok, Acc};
cost(Blocks, [{?END, Occupancy}], Weight, Params, Acc) ->
    maybe
        {ok, Rate} ?= rate(Occupancy, Weight, Params),
        {ok, Acc + (Blocks * Rate)}
    end;
cost(Blocks, [{Length, Occupancy} | Rest], Weight, Params, Acc) ->
    Used = min(Blocks, Length),
    maybe
        {ok, Rate} ?= rate(Occupancy, Weight, Params),
        cost(Blocks - Used, Rest, Weight, Params, Acc + (Used * Rate))
    end.

%% @doc Calculate the integral cost of adding one weight for one block.
rate(Occupancy, Weight, {Target, Price})
        when Occupancy >= 0.0, Weight > 0.0,
             Target > 0.0, Target < 1.0,
             Occupancy + Weight < 1.0 ->
    Ratio = Weight / (1.0 - Occupancy),
    Integral =
        case Ratio < 1.0e-4 of
            true ->
                (Weight * Occupancy / (1.0 - Occupancy))
                    + log_tail(Ratio, 2, Ratio * Ratio, 0.0);
            false ->
                math:log(
                    (1.0 - Occupancy) /
                        (1.0 - Occupancy - Weight)
                ) - Weight
        end,
    Result = Price * ((1.0 - Target) / Target) * Integral,
    case Result > 0.0 of
        true -> {ok, Result};
        false -> {error, invalid}
    end;
rate(_Occupancy, _Weight, _Params) -> {error, invalid}.

%% @doc Sum the stable tail of `-log(1-X)' after its linear term.
log_tail(_X, 9, _Power, Acc) -> Acc;
log_tail(X, N, Power, Acc) ->
    log_tail(X, N + 1, Power * X, Acc + (Power / N)).

%% @doc Read a non-negative integer request value.
amount(Key, Req, Opts) -> non_negative(field(Key, Req, 0, Opts)).

%% @doc Accept only AR, case-insensitively.
ar(Req, Opts) ->
    case field(<<"token">>, Req, <<"ar">>, Opts) of
        Token when is_binary(Token) ->
            hb_util_string:lowercase(Token) =:= <<"ar">>;
        _ -> false
    end.

%% @doc Read and validate an integer without raising on untrusted input.
non_negative(Value) ->
    case hb_util:safe_int(Value) of
        {ok, Integer} when Integer >= 0 -> {ok, Integer};
        _ -> {error, invalid}
    end.

%% @doc Read a positive numeric parameter or calculated weight.
positive(Value) ->
    case numeric(Value) of
        {ok, Number} when Number > 0.0 -> {ok, Number};
        _ -> {error, invalid}
    end.

%% @doc Coerce a finite integer, float or decimal binary to a float.
numeric(Value) when is_integer(Value) -> {ok, float(Value)};
numeric(Value) when is_float(Value), Value =:= Value -> {ok, Value};
numeric(Value) when is_binary(Value) ->
    try
        {ok, binary_to_float(Value)}
    catch
        _:_ ->
            case hb_util:safe_int(Value) of
                {ok, Integer} -> {ok, float(Integer)};
                _ -> {error, invalid}
            end
    end;
numeric(_Value) -> {error, invalid}.

%% @doc Read an integer field retained on a lease.
integer(Key, Record, Default, Opts) ->
    hb_util:ok_or(
        hb_util:safe_int(hb_maps:get(Key, Record, Default, Opts)),
        Default
    ).

%% @doc Read a lease from the registry without resolving its name as a path.
lease(Name, Base, Opts) ->
    Names = state(<<"names">>, Base, #{}, Opts),
    case hb_cache:ensure_all_loaded(hb_maps:get(Name, Names, not_found, Opts), Opts) of
        Record when is_map(Record) -> {ok, Record};
        _ -> not_found
    end.

%% @doc Read a key of the process state as a plain message.
state(Key, Base, Default, Opts) ->
    hb_ao:get(Key, {as, <<"message@1.0">>, Base}, Default, Opts).

%% @doc Read a request field as plain data.
field(Key, Msg, Default, Opts) -> hb_maps:get(Key, Msg, Default, Opts).

%%% Tests

%% @doc Build a trained order-one model and configured empty registry.
test_base(Opts) ->
    {ok, Trained} =
        hb_ao:resolve(
            #{ <<"device">> => <<"markov@1.0">> },
            #{
                <<"path">> => <<"train">>,
                <<"body">> => [<<"a">>, <<"ab">>, <<"b">>],
                <<"order">> => 1
            },
            Opts#{ <<"hashpath">> => ignore }
        ),
    Trained#{
        <<"target-occupancy">> => 0.5,
        <<"price-at-target">> => 1000000000000,
        <<"spectrum-height">> => 100
    }.

%% @doc Price and blocks are integer inverses and retain the name's weight.
inverse_test() ->
    Opts = #{},
    Base = test_base(Opts),
    {ok, Cost} = price(Base, #{ <<"price">> => 100, <<"name">> => <<"a">> }, Opts),
    {ok, Quote} =
        blocks(
            Base,
            #{ <<"blocks">> => Cost, <<"name">> => <<"a">> },
            Opts
        ),
    ?assertEqual(100, maps:get(<<"blocks">>, Quote)),
    ?assert(maps:get(<<"weight">>, maps:get(<<"pricing">>, Quote)) > 0.0),
    {ok, Before} =
        blocks(
            Base,
            #{ <<"blocks">> => Cost - 1, <<"name">> => <<"a">> },
            Opts
        ),
    ?assert(maps:get(<<"blocks">>, Before) < 100).

%% @doc The implemented curve matches its closed form at a known occupancy.
curve_test() ->
    Opts = #{},
    Candidate =
        #{
            <<"deadline">> => 100,
            <<"grace">> => 100,
            <<"pricing">> => #{ <<"weight">> => 0.1 }
        },
    Base =
        #{
            <<"target-occupancy">> => 0.5,
            <<"price-at-target">> => 1000,
            <<"spectrum-height">> => 99,
            <<"names">> => #{ <<"candidate">> => Candidate }
        },
    {ok, Empty} =
        price(
            Base,
            #{
                <<"price">> => 1,
                <<"name">> => <<"candidate">>,
                <<"duration">> => 1
            },
            Opts
        ),
    ?assertEqual(ceil(1000 * (math:log(1 / 0.9) - 0.1)), Empty),
    Occupied =
        Base#{
            <<"names">> =>
                (maps:get(<<"names">>, Base))#{
                    <<"other">> =>
                        #{
                            <<"deadline">> => 200,
                            <<"pricing">> => #{ <<"weight">> => 0.2 }
                        }
                }
        },
    {ok, Cost} =
        price(
            Occupied,
            #{
                <<"price">> => 1,
                <<"name">> => <<"candidate">>,
                <<"duration">> => 1
            },
            Opts
        ),
    ?assertEqual(ceil(1000 * (math:log(0.8 / 0.7) - 0.1)), Cost).

%% @doc Occupied probability mass raises the price until its lease expires.
occupancy_changes_price_test() ->
    Opts = #{},
    Base = test_base(Opts),
    {ok, First} =
        blocks(
            Base,
            #{ <<"blocks">> => 1000000000, <<"name">> => <<"a">> },
            Opts
        ),
    Weight = maps:get(<<"weight">>, maps:get(<<"pricing">>, First)),
    Occupied =
        Base#{
            <<"names">> =>
                #{
                    <<"a">> =>
                        #{
                            <<"deadline">> => 200,
                            <<"grace">> => 210,
                            <<"pricing">> => #{ <<"weight">> => Weight }
                        }
                }
        },
    {ok, EmptyPrice} =
        price(Base, #{ <<"price">> => 10, <<"name">> => <<"b">> }, Opts),
    {ok, OccupiedPrice} =
        price(Occupied, #{ <<"price">> => 10, <<"name">> => <<"b">> }, Opts),
    {ok, AfterPrice} =
        price(
            Occupied#{ <<"spectrum-height">> => 200 },
            #{ <<"price">> => 10, <<"name">> => <<"b">> },
            Opts
        ),
    ?assert(OccupiedPrice > EmptyPrice),
    ?assertEqual(EmptyPrice, AfterPrice).

%% @doc A quote spanning an expiry charges each constant-occupancy slab.
interval_is_partitioned_at_expiry_test() ->
    Opts = #{},
    Base = test_base(Opts),
    {ok, First} =
        blocks(
            Base,
            #{ <<"blocks">> => 1000000000, <<"name">> => <<"a">> },
            Opts
        ),
    Weight = maps:get(<<"weight">>, maps:get(<<"pricing">>, First)),
    Occupied =
        Base#{
            <<"names">> =>
                #{
                    <<"a">> =>
                        #{
                            <<"deadline">> => 105,
                            <<"grace">> => 110,
                            <<"pricing">> => #{ <<"weight">> => Weight }
                        }
                }
        },
    {ok, Whole} =
        price(Occupied, #{ <<"price">> => 10, <<"name">> => <<"b">> }, Opts),
    {ok, During} =
        price(Occupied, #{ <<"price">> => 5, <<"name">> => <<"b">> }, Opts),
    {ok, After} =
        price(
            Occupied#{ <<"spectrum-height">> => 105 },
            #{ <<"price">> => 5, <<"name">> => <<"b">> },
            Opts
        ),
    ?assert(abs(Whole - (During + After)) =< 1).

%% @doc Missing parameters, unsupported tokens and full occupancy are refused.
invalid_quotes_test() ->
    Opts = #{},
    Base = test_base(Opts),
    ?assertEqual(
        {error, invalid},
        price(
            maps:remove(<<"target-occupancy">>, Base),
            #{ <<"price">> => 1, <<"name">> => <<"a">> },
            Opts
        )
    ),
    ?assertEqual(
        {error, invalid},
        price(Base, #{ <<"price">> => 1, <<"name">> => <<"a">>, <<"token">> => <<"ao">> }, Opts)
    ),
    Full =
        Base#{
            <<"names">> =>
                #{
                    <<"taken">> =>
                        #{
                            <<"deadline">> => 200,
                            <<"pricing">> => #{ <<"weight">> => 0.999999 }
                        }
                }
        },
    ?assertEqual(
        {error, invalid},
        price(Full, #{ <<"price">> => 1, <<"name">> => <<"a">> }, Opts)
    ).
