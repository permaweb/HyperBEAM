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
    NComplementOverTime = bignum_exp(PropD - PropN, Steps),
    DOverTime = bignum_exp(PropD, Steps),
    case DOverTime of
        0 -> throw({error, division_with_zero_denominator});
        _ -> (Remaining * (DOverTime - NComplementOverTime)) div DOverTime
    end.

bignum_exp(_, 0) -> 1;
bignum_exp(X, Y) ->
    do_bignum_exp(X, Y, 1).

do_bignum_exp(_, 0, Acc) ->
    Acc;
do_bignum_exp(X, Y, Acc) when Y rem 2 =:= 1 ->
    do_bignum_exp(X * X, Y div 2, Acc * X);
do_bignum_exp(X, Y, Acc) ->
    do_bignum_exp(X * X, Y div 2, Acc).

drip_global(Acc, ToMint, TotalWeightedUnits) when TotalWeightedUnits =:= 0 ->
    {Acc, ToMint};
drip_global(Acc, ToMint, TotalWeightedUnits) ->
    NewAcc = Acc + (ToMint div TotalWeightedUnits),
    UndistributedMint = ToMint rem TotalWeightedUnits,
    {NewAcc, UndistributedMint}.

drip_resource(ResourceAcc, GlobalAcc, LastGlobalAcc, Weight) ->
    ResourceAcc + ((GlobalAcc - LastGlobalAcc) * Weight).

drip_user(ResourceAcc, LastResourceAcc, UserQty) ->
    drip_user(0, ResourceAcc, LastResourceAcc, UserQty).
drip_user(Balance, ResourceAcc, LastResourceAcc, UserQty) ->
    Balance + ((ResourceAcc - LastResourceAcc) * UserQty).
