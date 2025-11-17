%%% @doc Math functions for the dev_pot module. Expresses a model as follows:
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

minted_between(Minted, Max, PropN, PropD, LastT, T) ->
    Steps = max(T - LastT, 0),
    Remaining = Max - Minted,
    NComplementOverTime = trunc(math:pow(PropD - PropN, Steps)),
    DOverTime = trunc(math:pow(PropD, Steps)),
    Remaining * (DOverTime - NComplementOverTime) div DOverTime.

drip_global(Acc, ToMint, TotalWeightedUnits) ->
    Acc + (ToMint / TotalWeightedUnits).

drip_resource(ResourceAcc, GlobalAcc, LastGlobalAcc, Weight) ->
    ResourceAcc + ((GlobalAcc - LastGlobalAcc) * Weight).

drip_user(ResourceAcc, LastResourceAcc, UserQty) ->
    drip_user(0, ResourceAcc, LastResourceAcc, UserQty).
drip_user(Balance, ResourceAcc, LastResourceAcc, UserQty) ->
    Balance + ((ResourceAcc - LastResourceAcc) * UserQty).
