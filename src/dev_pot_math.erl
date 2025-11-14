%%% @doc Math functions for the dev_pot module. Expresses a model as follows:
%%% 
%%% Initialization:
%%%     Global:   Acc = 0
%%%     Resource: LastGlobal = Global.Acc,
%%%               Acc = 0
%%%     User: User.Qty,
%%%           User.LastResource = Resource.Acc,
%%%           Global.TWU += User.Qty * Resource.weight
%%% Drip
%%%     Global: Acc += ToMint / Global.TWU
%%%     Resource: Acc += (Global.Acc - LastGlobal) * Weight, LastGlobal = Global.Acc
%%%     User: Balance += (Resource.Acc - User.LastResource) * User.Qty
%%% 
%%% Change Weight:
%%%     1. Drip global.
%%%     2. Drip resource.
%%%     3. Resource.weight = NewWeight
-module(dev_pot_math).
-export([
    drip_global/3,
    drip_resource/4,
    drip_user/3,
    drip_user/4,
    units_minted_between/5
]).

units_minted_between(Minted, Max, Proportion, LastT, T) ->
    Steps = max(T - LastT, 0),
    Remaining = Max - Minted,
    Remaining * (1 - math:pow(1 - Proportion, Steps)).

drip_global(Acc, ToMint, TotalWeightedUnits) ->
    Acc + (ToMint / TotalWeightedUnits).

drip_resource(ResourceAcc, GlobalAcc, LastGlobalAcc, Weight) ->
    ResourceAcc + ((GlobalAcc - LastGlobalAcc) * Weight).

drip_user(ResourceAcc, LastResourceAcc, UserQty) ->
    drip_user(0, ResourceAcc, LastResourceAcc, UserQty).
drip_user(Balance, ResourceAcc, LastResourceAcc, UserQty) ->
    Balance + ((ResourceAcc - LastResourceAcc) * UserQty).