 %%% @doc An experimental real-time on-demand minting model. Instead of minting
%%% all tokens eagerly, this model mints tokens only on-demand. In doing so,
%%% it significantly reduces the computational and message-passing complexity of the
%%% system.
%%% 
%%% h/t to MakerDAO's DSR and MCD rate accumulation system for some inspiration.
%%% 
%%% The core minting model is described in the `dev_pot_math` moduledoc.
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
-export([update_deposit_index/5]).
-export([user/3, balance/2, balances/1, deposit/3, deposits/1, deposits/2]).
%%% Pot Model.

drip(State, Req, Opts) ->
    StateWithNewTime =
        case is_map(Req) andalso hb_maps:find(<<"t">>, Req) of
            {ok, TReq} -> State#{ <<"t">> => TReq };
            _ -> State#{ <<"t">> => hb_maps:get(<<"t">>, State, 0) + 1 }
        end,
    drip_global(StateWithNewTime, Opts).

drip_global(S = #{ <<"t">> := T, <<"last-drip">> := Last }, _Opts) when T =:= Last -> S;
drip_global(S = #{
        <<"t">> := T,
        <<"mint-cap">> := Max,
        <<"mint-prop">> := Proportion
    }, Opts) ->
    AlreadyMinted = hb_maps:get(<<"minted">>, S, 0, Opts),
    LastT = hb_maps:get(<<"last-drip">>, S, 0, Opts),
    TotalWeightedUnits = hb_maps:get(<<"total-weighted-units">>, S, 0, Opts),
    GlobalAcc = hb_maps:get(<<"global-reward-accumulator">>, S, 0, Opts),
    ToMint =
        dev_pot_math:units_minted_between(
            AlreadyMinted,
            Max,
            Proportion,
            LastT,
            T
        ),
    NewGlobalAcc = dev_pot_math:drip_global(GlobalAcc, ToMint, TotalWeightedUnits),
    ?event(
        {minting,
            {to_mint, ToMint},
            {total_weighted_units, TotalWeightedUnits},
            {old_global_reward_accumulator, GlobalAcc},
            {new_global_reward_accumulator, NewGlobalAcc}
        }),
    S#{
        <<"global-reward-accumulator">> => NewGlobalAcc,
        <<"last-drip">> => T,
        <<"minted">> => AlreadyMinted + ToMint
    }.

drip_resource(ResourceID, S, Opts) ->
    % Get the resource.
    Resource = hb_ao:get(<<"/resources/", ResourceID/binary>>, S, #{}, Opts),
    % Accumulate the Reward*CurrentWeight since the last global drip.
    OldAccResourceWeight =
        hb_maps:get(<<"resource-reward-accumulator">>, Resource, 0, Opts),
    Weight = hb_ao:get(<<"weight">>, Resource, 0, Opts),
    GlobalAcc = hb_maps:get(<<"global-reward-accumulator">>, S, 0, Opts),
    LastGlobalAcc = hb_maps:get(<<"last-global-reward-accumulator">>, Resource, 0, Opts),
    NewResourceAcc =
        dev_pot_math:drip_resource(
            OldAccResourceWeight,
            GlobalAcc,
            LastGlobalAcc,
            Weight
        ),
    ?event(
        {drip_resource,
            {resource_id, ResourceID},
            {weight, Weight},
            {old_resource_reward_accumulator, OldAccResourceWeight},
            {new_resource_reward_accumulator, NewResourceAcc}
        }
    ),
    hb_ao:set(
        S,
        #{
            <<"resources">> => #{
                ResourceID =>
                    Resource#{
                        <<"resource-reward-accumulator">> => NewResourceAcc,
                        <<"last-global-reward-accumulator">> => GlobalAcc
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
            hb_private:reset(
                hb_ao:get(<<"users/", Addr/binary, "/deposits">>, S, #{}, Opts)
            ),
            Opts
        ),
    lists:sum(
        lists:map(
            fun(ResID) ->
                unclaimed_yield(Addr, ResID, S, Opts)
            end,
            ResourceIDs
        )
    ).
unclaimed_yield(Addr, ResourceID, UndrippedS, Opts) ->
    GlobalDrippedS = drip_global(UndrippedS, Opts),
    S = drip_resource(ResourceID, GlobalDrippedS, Opts),
    Res = hb_ao:get(<<"resources/", ResourceID/binary>>, S, #{}, Opts),
    ResourceAcc = hb_maps:get(<<"resource-reward-accumulator">>, Res, 0, Opts),
    Deposits = hb_maps:get(<<"deposits">>, Res, #{}, Opts),
    case hb_maps:find(Addr, Deposits) of
        error -> 0;
        {ok, #{
                <<"quantity">> := Qty,
                <<"last-resource-reward-accumulator">> := LastResourceAcc
            }} ->
            ?no_prod("Remove all floating point arithmetic."),
            dev_pot_math:drip_user(ResourceAcc, LastResourceAcc, Qty)
    end.

modify_deposit(Addr, ResourceID, Amount, S0, Opts) ->
    % Drip the global state and the resource, then extract necessary components.
    GlobalDrippedS = drip_global(S0, Opts),
    DrippedS = #{
        <<"balances">> := Balances,
        <<"resources">> := Resources
    } = drip_resource(ResourceID, GlobalDrippedS, Opts),
    ExistingDeposit = deposit(Addr, ResourceID, DrippedS),
    BaseBalance = hb_ao:get(Addr, Balances, 0, Opts),
    NewBalance = BaseBalance + unclaimed_yield(Addr, ResourceID, DrippedS, Opts),
    ResourceAcc =
        hb_ao:get(
            <<ResourceID/binary, "/resource-reward-accumulator">>,
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
                                        <<"last-resource-reward-accumulator">> =>
                                            ResourceAcc
                                    }
                            }
                    }
            },
            Opts
        ),
    ?event({resources_after_modify_deposit, NewResources}),
    WeightR = hb_ao:get(<<ResourceID/binary, "/weight">>, NewResources, 0, Opts),
    TotalWeightedUnits = hb_maps:get(<<"total-weighted-units">>, DrippedS, 0, Opts),
    UpdatedDepositS =
        DrippedS#{
            <<"resources">> => NewResources,
            <<"total-weighted-units">> => TotalWeightedUnits + (WeightR * Amount),
            <<"balances">> => Balances#{ Addr => NewBalance }
        },
    LiquidatedS =
        maybe_liquidate_delegations(
            Addr,
            ResourceID,
            UpdatedDepositS,
            Opts
        ),
    update_deposit_index(
        Addr,
        ResourceID,
        deposit(Addr, ResourceID, LiquidatedS),
        LiquidatedS,
        Opts
    ).

set_weight(ResourceID, Weight, S, Opts) ->
    % Run the global drip to ensure the state is up to date.
    S0 = drip_global(S, Opts),
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