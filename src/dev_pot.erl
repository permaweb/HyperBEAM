 %%% @doc An experimental real-time on-demand minting model. Instead of minting
%%% all tokens eagerly, this model mints tokens only on-demand. In doing so,
%%% it significantly reduces the computational and message-passing complexity of the
%%% system.
%%% 
%%% h/t to MakerDAO's DSR and MCD rate accumulation system for some inspiration.
%%% 
%%% The core minting model is this:
%%% 1. Maintain a list of all balances for `resources` that lead to the minting
%%%    of tokens.
%%% 2. With each balance, store the `chi` factor at the time of creation.
%%% 3. When the `drip` function is called, for each time-step since the last `drip`,
%%%    calculate the yield that would have accrued to a balance holding one unit
%%%    of the resource at that time: `rate(TimeStep) * (1/sum(deposits))`.
%%% 4. When balances are requested or utilized, calculate the accrued yield by
%%%    subtracting the current `chi` factor and the initial one. Multiply this
%%%    by the number of units in the deposit. Count this with the existing reward
%%%    balance: `total-balance = (chi - chi0) * deposit + existing-balance`.
%%% 5. When the balance or deposit is modified in any way, first accrue the yield
%%%    to the existing balance. Then perform the operation.
%%% 
%%% This device will support delegating resources to other addresses, allowing for
%%% mechanisms like yield-swaps etc to be created downstream. Each delegation
%%% triggers a `Delegation-Notice` message to be sent to the recipient of the
%%% delegation, as well as a proportional increase in the recipient's `deposit`
%%% value. Reciprocally, the delegator's `deposit` value is decreased by the same
%%% amount, while the delegation itself is recorded in the `delegations`
%%% message. When a delegation is revoked, this setup is reversed and a new
%%% `Delegation-Notice` message is sent with `quantity` set to zero.
%%% 
%%% This structure allows downstream minting processes to credit `Delegation-Notice`s
%%% as deposits in their own mechanism. By tracking the delegators and performing
%%% their own mints using the same `pot` functionality as the parent, depositors
%%% in the original process can earn their yield in the form of `child` mints.
%%% Each mint can operate asynchronously and in real-time.
%%% 
%%% The structure of the state is as follows:
%%% 
%%% /chi: Global meta-chi accumulator M used to derive effective per-resource chi.
%%% /resources/ID/weight: The weight of the resource in the minting process.
%%% /resources/ID/total-deposits: The total quantity of units deposited of the
%%% resource.
%%% /resources/ID/deposits/ADDR/quantity: The quantity of the resource deposited
%%% by a specific address.
%%% /resources/ID/deposits/ADDR/chi0: The initial chi factor at the time of the
%%% deposit.
%%% /balances/ADDR: The current minted asset balance of an address.
%%% /minted: The total number of units minted.
%%% /mint-cap: The maximum number of units that can be minted.
%%% /mint-prop: The proportion of the mint-cap that is minted per time-step.
%%% /last-drip: The last time the drip function was called.
%%% /t: The current time-step.
%%% /tw: The total weighted deposits (sum over resources of weight * total-deposits).
%%% 
%%% TODO:
%%% - Add `secure-set` (set guarded by address) for resource-weights and 
%%%   supported resources.
-module(dev_pot).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
%%% Public API.
-export([drip/3]).
%%% `~pot@1.0` Private Utilities.
-export([modify_deposit/5, delegate/6, maybe_liquidate_delegations/5, set_weight/4]).
-export([units_minted_between/5, update_deposit_index/5]).
-export([user/3, balance/2, balances/1, deposit/3, deposits/1, deposits/2]).
%%% Pot Model.

drip(State, Req, Opts) ->
    StateWithNewTime =
        case is_map(Req) andalso hb_maps:find(<<"t">>, Req) of
            {ok, TReq} -> State#{ <<"t">> => TReq };
            _ -> State#{ <<"t">> => hb_maps:get(<<"t">>, State, 0) + 1 }
        end,
    drip(StateWithNewTime, Opts).

drip(S = #{ <<"t">> := T, <<"last-drip">> := Last }, _Opts) when T =:= Last -> S;
drip(S = #{
        <<"t">> := T,
        <<"mint-cap">> := Max,
        <<"mint-prop">> := Proportion
    }, Opts) ->
    AlreadyMinted = hb_maps:get(<<"minted">>, S, 0, Opts),
    LastT = hb_maps:get(<<"last-drip">>, S, 0, Opts),
    TotalWeightedTimeUnits =
        hb_maps:get(<<"total-weighted-units">>, S, 0, Opts) * (T - LastT),
    ToMint = units_minted_between(AlreadyMinted, Max, Proportion, LastT, T),
    MintedPerWeightedTimeUnit = ToMint / TotalWeightedTimeUnits,
    ?event(
        {minting,
            {to_mint, ToMint},
            {total_weighted_time_units, TotalWeightedTimeUnits},
            {minted_per_weighted_time_unit, MintedPerWeightedTimeUnit}
        }),
    S#{
        <<"minted-per-weighted-unit">> =>
            hb_maps:get(<<"minted-per-weighted-unit">>, S, 0, Opts) +
                MintedPerWeightedTimeUnit,
        <<"last-drip">> => T,
        <<"minted">> => AlreadyMinted + ToMint
    }.

drip_resource(ResourceID, S, Opts) ->
    % Get the current time and the resource.
    T = hb_maps:get(<<"t">>, S, 0, Opts),
    Resource = hb_ao:get(<<"/resources/", ResourceID/binary>>, S, #{}, Opts),
    % Accumulate the weight-time since the last resource-specific drip.
    OldAccumulatedWeightTime =
        hb_maps:get(<<"accumulated-weight-time">>, Resource, 0, Opts),
    Weight = hb_ao:get(<<"weight">>, Resource, 0, Opts),
    LastWeightTimeDrip = hb_maps:get(<<"last-weight-time-drip">>, S, 0, Opts),
    TimeDelta = T - LastWeightTimeDrip,
    WeightTimeDelta = (T - LastWeightTimeDrip) * Weight,
    NewAccumulatedWeightTime = OldAccumulatedWeightTime + WeightTimeDelta,
    ?event(
        {drip_resource,
            {resource_id, ResourceID},
            {last_resource_drip, LastWeightTimeDrip},
            {old_accumulated_weight_time, OldAccumulatedWeightTime},
            {weight, Weight},
            {time_delta, TimeDelta},
            {weight_time_delta, WeightTimeDelta},
            {accumulated_weight_time, NewAccumulatedWeightTime}
        }
    ),
    hb_ao:set(
        S,
        #{
            <<"resources">> => #{
                ResourceID =>
                    Resource#{
                        <<"accumulated-weight-time">> => NewAccumulatedWeightTime,
                        <<"last-weight-time-drip">> => T
                    }
            }
        },
        Opts
    ).

balance(Addr, S) ->
    hb_maps:get(Addr, hb_maps:get(<<"balances">>, S, #{}), 0)
        + unclaimed_yield(Addr, S, #{}).

unclaimed_yield(Addr, S, Opts) ->
    ResourceIDs =
        hb_maps:keys(
            hb_ao:get(<<"users/", Addr/binary, "/deposits">>, S, #{}, Opts),
            Opts
        ),
    MintedPerWeightedUnit = hb_maps:get(<<"minted-per-weighted-unit">>, S, 0, Opts),
    lists:sum(
        lists:map(
            fun(ResID) ->
                DrippedS = drip_resource(ResID, S, Opts),
                unclaimed_yield(Addr, ResID, MintedPerWeightedUnit, DrippedS, Opts)
            end,
            ResourceIDs
        )
    ).
unclaimed_yield(Addr, ResourceID, MintedPerWeightedUnit, S, Opts) ->
    Res = hb_ao:get(<<"resources/", ResourceID/binary>>, S, #{}, Opts),
    AccumulatedWeightTime = hb_maps:get(<<"accumulated-weight-time">>, Res, 0, Opts),
    Deposits = hb_maps:get(<<"deposits">>, Res, #{}, Opts),
    case hb_maps:find(Addr, Deposits) of
        error -> 0;
        {ok, #{
                <<"quantity">> := Qty,
                <<"minted-per-weighted-unit-at-deposit">> :=
                    MintedPerWeightedUnitAtDeposit,
                <<"accumulated-weight-time-at-deposit">> :=
                    AccumulatedWeightTimeAtDeposit
            }} ->
            ?no_prod("Remove all floating point arithmetic."),
            Qty *
                (MintedPerWeightedUnit - MintedPerWeightedUnitAtDeposit) *
                (AccumulatedWeightTime - AccumulatedWeightTimeAtDeposit)
    end.

units_minted_between(Minted, Max, Proportion, LastT, T) ->
    Steps = max(T - LastT, 0),
    Remaining = Max - Minted,
    Remaining * (1 - math:pow(1 - Proportion, Steps)).

modify_deposit(Addr, ResourceID, Amount, S0, Opts) ->
    S1 = drip(S0, Opts),
    S = drip_resource(ResourceID, S1, Opts),
    #{ <<"balances">> := Balances, <<"resources">> := Resources } = S,
    ExistingDeposit = deposit(Addr, ResourceID, S),
    MintedPerWeightedUnit = hb_maps:get(<<"minted-per-weighted-unit">>, S, 0, Opts),
    BaseBalance = hb_ao:get(Addr, Balances, 0, Opts),
    NewBalance =
        BaseBalance + unclaimed_yield(Addr, ResourceID, MintedPerWeightedUnit, S, Opts),
    AccumulatedWeightTime =
        hb_ao:get(
            <<ResourceID/binary, "/accumulated-weight-time">>,
            Resources,
            0,
            Opts
        ),
    NewResources =
        hb_ao:set(
            Resources,
            #{
                ResourceID =>
                    #{
                        <<"total-deposits">> =>
                            Amount +
                                hb_ao:get(
                                    <<ResourceID/binary, "/total-deposits">>,
                                    Resources,
                                    0,
                                    Opts
                                ),
                        <<"deposits">> =>
                            #{
                                Addr =>
                                    #{
                                        <<"quantity">> => ExistingDeposit + Amount,
                                        <<"minted-per-weighted-unit-at-deposit">> =>
                                            MintedPerWeightedUnit,
                                        <<"accumulated-weight-time-at-deposit">> =>
                                            AccumulatedWeightTime
                                    }
                            }
                    }
            },
            Opts
        ),
    ?event({resources_after_modify_deposit, NewResources}),
    WeightR = hb_ao:get(<<ResourceID/binary, "/weight">>, NewResources, 0, Opts),
    TotalWeightedUnits = hb_maps:get(<<"total-weighted-units">>, S, 0, Opts),
    S2 =
        S1#{
            <<"resources">> => NewResources,
            <<"total-weighted-units">> => TotalWeightedUnits + (WeightR * Amount),
            <<"balances">> => Balances#{ Addr => NewBalance }
        },
    S3 =
        maybe_liquidate_delegations(
            Addr,
            ResourceID,
            S2,
            Opts
        ),
    update_deposit_index(Addr, ResourceID, deposit(Addr, ResourceID, S3), S3, Opts).

set_weight(ResourceID, Weight, S, Opts) ->
    % Run the global drip to ensure the state is up to date.
    S0 = drip(S, Opts),
    S1 = drip_resource(ResourceID, S0, Opts),
    % Calculate the new total deposited units for the weighted global counter
    % (`/total-weighted-units').
    Resource = hb_ao:get(<<"/resources/", ResourceID/binary>>, S1, #{}, Opts),
    OldWeight = hb_ao:get(<<"weight">>, Resource, 0, Opts),
    ResourceDeposits = hb_ao:get(<<"total-deposits">>, Resource, 0, Opts),
    % Update the total weighted units counter. Subtract the deposits at the old
    % weight first, then add the deposits at the new weight.
    LastTotalWeightedUnits = hb_maps:get(<<"total-weighted-units">>, S1, 0, Opts),
    NewTotalWeightedUnits =
        LastTotalWeightedUnits
            - (OldWeight * ResourceDeposits)
            + (Weight * ResourceDeposits),
    % Update the resource and the global weighted units counter.
    hb_ao:set(
        S1,
        #{
            <<"resources">> => #{
                ResourceID => Resource#{ <<"weight">> => Weight }
            },
            <<"total-weighted-units">> => NewTotalWeightedUnits
        },
        Opts
    ).

delegate(FromAddr, ToAddr, ResourceID, Amount, S, Opts) ->
    ?event(
        {delegating,
            {from_addr, FromAddr},
            {to_addr, ToAddr},
            {resource_id, ResourceID},
            {amount, Amount}
        }
    ),
    S0 = modify_deposit(FromAddr, ResourceID, -Amount, S, Opts),
    S1Unnormalized = modify_deposit(ToAddr, ResourceID, Amount, S0, Opts),
    S1 =
        maybe_liquidate_delegations(
            deposit(ToAddr, ResourceID, S1Unnormalized),
            ToAddr,
            ResourceID,
            S1Unnormalized,
            Opts
        ),
    ExistingQuantity =
        hb_ao:get(
            <<
                "/resources/", 
                ResourceID/binary, 
                "/deposits/", 
                FromAddr/binary, 
                "/delegations/", 
                ToAddr/binary
            >>,
            S1,
            0,
            Opts
        ),
    NewS1 =
        hb_ao:set(
            S1,
            <<
                "/resources/",
                ResourceID/binary,
                "/deposits/",
                FromAddr/binary,
                "/delegations/",
                ToAddr/binary
            >>,
            ExistingQuantity + Amount,
            Opts
        ),
    maybe_liquidate_delegations(
        FromAddr,
        ResourceID,
        NewS1,
        Opts
    ).

%% @doc Recursively liquidate delegations as necessary until the deposit for
%% a delegating address is non-negative.
maybe_liquidate_delegations(Addr, ResourceID, S, Opts) ->
    maybe_liquidate_delegations(
        deposit(Addr, ResourceID, S),
        Addr,
        ResourceID,
        S,
        Opts
    ).
maybe_liquidate_delegations(Deposit, Addr, _Res, S, _Opts) when Deposit >= 0 ->
    ?event({no_liquidation_necessary, {deposit, Deposit}, {addr, Addr}}),
    S;
maybe_liquidate_delegations(Deposit, Addr, ResourceID, S, Opts) ->
    Overdraw = abs(Deposit),
    % Find the existing delegations for this address.
    ExistingDelegations =
        hb_ao:get(
            <<
                "/resources/", 
                ResourceID/binary, 
                "/deposits/", 
                Addr/binary, 
                "/delegations">>,
            S,
            #{},
            Opts
        ),
    % Determine the largest delegation to liquidate.
    LargestDelegation =
        lists:max(
            hb_maps:values(
                hb_private:reset(ExistingDelegations)
            )
        ),
    {LargestDelegationAddr, _} =
        lists:keyfind(
            LargestDelegation,
            2,
            hb_maps:to_list(ExistingDelegations)
        ),
    RevokeAmount = min(Overdraw, LargestDelegation),
    ?event(
        {liquidating_delegation,
            {addr, Addr},
            {overdrawn, Overdraw},
            {recouping, RevokeAmount},
            {largest_delegation, LargestDelegation},
            {delegated_to, LargestDelegationAddr}
        }
    ),
    % Revoke the largest delegation.
    NewS =
        delegate(
            Addr,
            LargestDelegationAddr,
            ResourceID,
            -RevokeAmount,
            S,
            Opts
        ),
    % Recursively liquidate the remaining quantity.
    maybe_liquidate_delegations(
        Deposit + RevokeAmount,
        Addr,
        ResourceID,
        NewS,
        Opts
    ).

%%% Helpers.

deposit(Addr, ResourceID, S) ->
    hb_ao:get(
        <<"/resources/", ResourceID/binary, "/deposits/", Addr/binary, "/quantity">>,
        S,
        0,
        #{}
    ).

balances(S = #{ <<"balances">> := Bs }) ->
    hb_maps:map(fun(Addr, _) -> balance(Addr, S) end, Bs).

deposits(S = #{ <<"resources">> := Resources }) ->
    hb_maps:map(fun(ResourceID, _) -> deposits(ResourceID, S) end, Resources).
deposits(ResourceID, S) ->
    Ds = hb_ao:get(
        <<"/resources/", ResourceID/binary, "/deposits">>,
        S,
        #{},
        #{}
    ),
    hb_maps:map(fun(Addr, _) -> deposit(Addr, ResourceID, S) end, Ds).

user(Addr, S, Opts) ->
    hb_ao:get(<<"/users/", Addr/binary>>, S, #{}, Opts).

update_deposit_index(Addr, ResourceID, Quantity, S, Opts) ->
    Delegations =
        hb_ao:get(
            <<
                "/resources/",
                ResourceID/binary,
                "/deposits/",
                Addr/binary,
                "/delegations"
            >>,
            S,
            #{},
            Opts
        ),
    hb_ao:set(
        S,
        <<"users/", Addr/binary, "/deposits/", ResourceID/binary>>,
        if Quantity == 0 andalso ?IS_EMPTY_MESSAGE(Delegations) -> unset;
        true -> Quantity
        end,
        Opts
    ).