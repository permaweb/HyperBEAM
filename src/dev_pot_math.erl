-module(dev_pot_math).
-export([
    accumulate_weight_time/4,
    accumulate_reward_per_weighted_unit/3,
    average_weight_time/4,
    units_minted_between/5,
    reward_between/4
]).

accumulate_weight_time(PriorTime, Time, CurrentWeight, CurrentAccumulator) ->
    TimeDelta = Time - PriorTime,
    CurrentAccumulator + (TimeDelta * CurrentWeight).

accumulate_reward_per_weighted_unit(ToMint, TotalWeightedUnits, CurrentAccumulator) ->
    CurrentAccumulator + (ToMint / TotalWeightedUnits).

average_weight_time(PriorTime, Time, StartAccumulator, EndAccumulator) ->
    TimeDelta = Time - PriorTime,
    (EndAccumulator - StartAccumulator) / TimeDelta.

units_minted_between(Minted, Max, Proportion, LastT, T) ->
    Steps = max(T - LastT, 0),
    Remaining = Max - Minted,
    Remaining * (1 - math:pow(1 - Proportion, Steps)).

reward_between(MintedPerWeightedUnit0, MintedPerWeightedUnit, AvgWeight, Qty) ->
    MintedPerWeightedUnitDelta = MintedPerWeightedUnit - MintedPerWeightedUnit0,
    Qty * AvgWeight * MintedPerWeightedUnitDelta.