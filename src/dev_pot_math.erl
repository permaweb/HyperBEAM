%% @doc Math functions for the dev_pot module. Expresses a model as follows:
%%% ```
%%% Initialization:
%%%   Global:     Acc = 0,
%%%               TWU = 0.
%%%   Resource:   Acc = 0,
%%%               LastGlobal = Global.Acc.
%%%   User: 	  Qty,
%%%               LastResource = Resource.Acc,
%%%               Global.TWU += Qty * Resource.Weight.
%%%            
%%% Drip:
%%%   Global:     Acc += ToMint / Global.TWU.
%%%   Resource:   Acc += (Global.Acc - LastGlobal) * Weight,
%%%               LastGlobal = Global.Acc.
%%%   User:       Balance += (Resource.Acc - LastResource) * Qty.
%%% 
%%% Modify:
%%%   Weight:     Global.drip(),
%%%               Resource.drip(),
%%%               Global.TWU -= Resource.weight * Resource.Qty
%%%               Global.TWU += NewWeight * Resource.Qty
%%%               Resource.weight = NewWeight.
%%%          
%%%   Deposit:    Global.drip(),
%%%               Resource.drip(),
%%%               Global.TWU -= User.qty * Resource.Weight,
%%%               Resource.Qty -= User.qty,
%%%               User.Initialize(NewQty, Resource)
%%% 
%%% Get Balance:  Global.drip(),
%%%               Resource.drip(),
%%%               User.drip(),
%%%               User.balance.
%%% '''
-module(dev_pot_math).
-export([minted_between/6]).
-export([drip_global/3, drip_resource/4, drip_user/3, drip_user/4]).
-export([bignum_exp/2]).

-define(MAX_EXACT_POWER_DIGITS, 1000).
-define(FIXED_SCALE_DIGITS, [40, 60, 80]).
-define(REWARD_SCALE, 1000000000000000000).

minted_between(Minted, Max, PropN, PropD, LastT, T)
    when not is_integer(Minted) orelse not is_integer(Max)
        orelse not is_integer(PropN) orelse not is_integer(PropD)
        orelse not is_integer(LastT) orelse not is_integer(T) ->
            throw({error, invalid_parameter});
minted_between(Minted, Max, _, _, _, _)
    when Minted < 0 orelse Max < 0 orelse Minted > Max ->
        throw({error, invalid_minted_max_boundaries});
minted_between(_, _, PropN, _, _, _)
    when PropN < 0 ->
        throw({error, invalid_negative_propn});
minted_between(_, _, _, PropD, _, _)
    when PropD =< 0 ->
        throw({error, invalid_non_positive_propd});
minted_between(_, _, PropN, PropD, _, _)
    when PropN > PropD ->
        throw({error, invalid_prop_division});
minted_between(Minted, Max, PropN, PropD, LastT, T) ->
    Steps = max(T - LastT, 0),
    Remaining = Max - Minted,
    minted_between_validated(Remaining, PropN, PropD, Steps).

minted_between_validated(_, _, _, 0) ->
    0;
minted_between_validated(0, _, _, _) ->
    0;
minted_between_validated(_, 0, _, _) ->
    0;
minted_between_validated(Remaining, PropD, PropD, _) ->
    Remaining;
minted_between_validated(Remaining, PropN, PropD, Steps) ->
    case estimated_power_digits(PropD, Steps) =< ?MAX_EXACT_POWER_DIGITS of
        true -> minted_between_exact(Remaining, PropN, PropD, Steps);
        false -> minted_between_fixed_scale(Remaining, PropN, PropD, Steps)
    end.

minted_between_exact(Remaining, PropN, PropD, Steps) ->
    NComplementOverTime = bignum_exp(PropD - PropN, Steps),
    DOverTime = bignum_exp(PropD, Steps),
    case DOverTime of
        0 -> throw({error, division_with_zero_denominator});
        _ -> (Remaining * (DOverTime - NComplementOverTime)) div DOverTime
    end.

minted_between_fixed_scale(Remaining, PropN, PropD, Steps) ->
    case try_fixed_scales(Remaining, PropN, PropD, Steps, ?FIXED_SCALE_DIGITS) of
        {ok, ToMint} -> ToMint;
        unresolved -> throw({error, precision_not_resolved})
    end.

try_fixed_scales(_, _, _, _, []) ->
    unresolved;
try_fixed_scales(Remaining, PropN, PropD, Steps, [ScaleDigits | Rest]) ->
    Scale = bignum_exp(10, ScaleDigits),
    PowerBase = power_base_interval(PropN, PropD, Scale),
    {PowLo, PowHi} = pow_fixed_scale(PowerBase, Steps, Scale),
    case minted_from_power_interval(Remaining, PowLo, PowHi, Scale) of
        {ok, ToMint} -> {ok, ToMint};
        unresolved -> try_fixed_scales(Remaining, PropN, PropD, Steps, Rest)
    end.

power_base_interval(PropN, PropD, Scale) ->
    BaseN = PropD - PropN,
    {
        (BaseN * Scale) div PropD,
        ceil_div(BaseN * Scale, PropD)
    }.

minted_from_power_interval(Remaining, PowLo, PowHi, Scale) ->
    MintLo = (Remaining * (Scale - PowHi)) div Scale,
    MintHi = (Remaining * (Scale - PowLo)) div Scale,
    case MintLo == MintHi of
        true -> {ok, MintLo};
        false -> minted_from_residual_interval(Remaining, PowLo, PowHi, Scale)
    end.

minted_from_residual_interval(Remaining, PowLo, PowHi, Scale) ->
    ResidualLo = max(1, ceil_div(Remaining * PowLo, Scale)),
    ResidualHi = ceil_div(Remaining * PowHi, Scale),
    case ResidualLo == ResidualHi of
        true -> {ok, Remaining - ResidualLo};
        false -> unresolved
    end.

pow_fixed_scale(Base, Steps, Scale) ->
    pow_fixed_scale(Base, Steps, {Scale, Scale}, Scale).

pow_fixed_scale(_, 0, Acc, _) ->
    Acc;
pow_fixed_scale(Base, Steps, Acc, Scale) when Steps rem 2 =:= 1 ->
    NewAcc = mul_interval(Acc, Base, Scale),
    case Steps div 2 of
        0 -> NewAcc;
        NextSteps ->
            pow_fixed_scale(mul_interval(Base, Base, Scale), NextSteps, NewAcc, Scale)
    end;
pow_fixed_scale(Base, Steps, Acc, Scale) ->
    pow_fixed_scale(mul_interval(Base, Base, Scale), Steps div 2, Acc, Scale).

mul_interval({Lo1, Hi1}, {Lo2, Hi2}, Scale) ->
    {
        (Lo1 * Lo2) div Scale,
        ceil_div(Hi1 * Hi2, Scale)
    }.

ceil_div(N, D) ->
    (N + D - 1) div D.

estimated_power_digits(_, 0) ->
    1;
estimated_power_digits(N, Steps) ->
    length(integer_to_list(N)) * Steps.

bignum_exp(_, 0) -> 1;
bignum_exp(X, Y) ->
    do_bignum_exp(X, Y, 1).

do_bignum_exp(_, 0, Acc) ->
    Acc;
do_bignum_exp(X, 1, Acc) ->
    Acc * X;
do_bignum_exp(X, Y, Acc) when Y rem 2 =:= 1 ->
    do_bignum_exp(X * X, Y div 2, Acc * X);
do_bignum_exp(X, Y, Acc) ->
    do_bignum_exp(X * X, Y div 2, Acc).

drip_global(Acc, ToMint, TotalWeightedUnits) when TotalWeightedUnits =:= 0 ->
    {Acc, ToMint};
drip_global(Acc, ToMint, TotalWeightedUnits) ->
    AccDelta = (ToMint * ?REWARD_SCALE) div TotalWeightedUnits,
    Distributed = (AccDelta * TotalWeightedUnits) div ?REWARD_SCALE,
    NewAcc = Acc + AccDelta,
    UndistributedMint = ToMint - Distributed,
    {NewAcc, UndistributedMint}.

drip_resource(ResourceAcc, GlobalAcc, LastGlobalAcc, Weight) ->
    ResourceAcc + ((GlobalAcc - LastGlobalAcc) * Weight).

drip_user(ResourceAcc, LastResourceAcc, UserQty) ->
    drip_user(0, ResourceAcc, LastResourceAcc, UserQty).
drip_user(Balance, ResourceAcc, LastResourceAcc, UserQty) ->
    Balance + (((ResourceAcc - LastResourceAcc) * UserQty) div ?REWARD_SCALE).
