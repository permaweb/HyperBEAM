%%% @doc Math functions for the dev_pot module. Expresses a model as follows:
%%% 
%%% Accumulators.
%%%     AccResourceW = AccResourceWlast + ((Tn - Tlast) * ResourceW)
%%%     AccRewardPerWU = AccRewardPerWUlast + (ToMint / GlobalWU)
%%% 
%%% Reward calculation.
%%%     AccumulatedUserResourceWeight = AccResourceWn - AccResourceWstart
%%%     DepositTime = Tn - Tlast
%%%     IssuedDuringDeposit = AccRewardPerWU - AccRewardPerWUstart
%%%     UserWU = UserQty * (AccumulatedUserResourceWeight / DepositTime)
%%%     UserReward = IssuedDuringDeposit * UserWU
%%% 
-module(dev_pot_math).
-export([
    accumulate_resource_weight/4,
    accumulate_reward_per_weighted_unit/3,
    user_resource_weight/4,
    units_minted_between/5,
    reward_between/6
]).

units_minted_between(Minted, Max, Proportion, LastT, T) ->
    Steps = max(T - LastT, 0),
    Remaining = Max - Minted,
    Remaining * (1 - math:pow(1 - Proportion, Steps)).

accumulate_resource_weight(PriorTime, Time, CurrentWeight, CurrentAccumulator) ->
    Steps = Time - PriorTime,
    CurrentAccumulator + (Steps * CurrentWeight).

accumulate_reward_per_weighted_unit(ToMint, TotalWeightedUnits, CurrentAccumulator) ->
    CurrentAccumulator + (ToMint / TotalWeightedUnits).

user_resource_weight(_PriorTime, _Time, StartAccumulator, EndAccumulator) ->
    EndAccumulator - StartAccumulator.

reward_between(PriorTime, Time, MintedPerWeightedUnit0, MintedPerWeightedUnit, UserResourceWeight, Qty) ->
    % Calculate the time period.
    Steps = Time - PriorTime,
    % Calculate the total reward given per weighted unit over the time period.
    MintedPerWeightedUnitDelta = MintedPerWeightedUnit - MintedPerWeightedUnit0,
    % Calculate total weighted units of the deposit over the time period.
    UserWeightedUnits = Qty * (UserResourceWeight / Steps),
    % Calculate the total reward given for the deposit over the time period.
    UserWeightedUnits * MintedPerWeightedUnitDelta.