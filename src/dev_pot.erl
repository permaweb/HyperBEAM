 %%% @doc An experimental real-time on-demand minting model. Instead of minting
%%% all tokens eagerly, this model mints tokens only on-demand. In doing so,
%%% it significantly reduces the computational and message-passing complexity of the
%%% system.
%%% 
%%% h/t to MakerDAO's DSR and MCD rate accumulation system, which implements a
%%% different model for another problem domain, but whose approach gave some
%%% inspiration for this model.
%%% 
%%% The core minting model is described in the `dev_pot_math` moduledoc.
%%% 
%%% This device supports delegating resources to other addresses, allowing for
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
-export([deposit/5, withdraw/5, delegate/6, undelegate/6, set_weight/4]).
-export([update_deposit_index/5]).
-export([user/3, balance/2, balances/1, get_deposit/3, get_deposits/1, get_deposits/2]).
-export([claim_yield/3, claim_yield/4]).

%%% Pot Model Functions.

%% @doc Update the state of the pot to reflect the passage of time from the
%% last drip to the present moment. Only drips the global state, deferring
%% per-resource to be executed at resource modification or user drip.
drip(State, Req, Opts) ->
    ?event({ called, State, Req}),
    StateWithNewTime =
        case is_map(Req) andalso hb_maps:find(<<"t">>, Req) of
            {ok, TReq} -> State#{ <<"t">> => TReq };
            _ -> State#{ <<"t">> => hb_maps:get(<<"t">>, State, 0) + 1 }
        end,
    drip_global(StateWithNewTime, Opts).

%% @doc Drip the global state of the pot if necessary, returning the state
%% unchanged if no time has passed since the last drip.
drip_global(S = #{ <<"t">> := T, <<"last-drip">> := Last }, _Opts) when T =:= Last -> S;
drip_global(S = #{
        <<"t">> := T,
        <<"mint-cap">> := Max,
        <<"mint-prop">> := {PropN, PropD}
    }, Opts) ->
    AlreadyMinted = hb_maps:get(<<"minted">>, S, 0, Opts),
    LastT = hb_maps:get(<<"last-drip">>, S, 0, Opts),
    TotalWeightedUnits = hb_maps:get(<<"total-weighted-units">>, S, 0, Opts),
    GlobalAcc = hb_maps:get(<<"accumulator">>, S, 0, Opts),
    ToMint =
        dev_pot_math:minted_between(
            AlreadyMinted,
            Max,
            PropN,
            PropD,
            LastT,
            T
        ),
    UndistributedMint = hb_maps:get(<<"undistributed-mint">>, S, 0, Opts),
    {NewGlobalAcc, NewUndistributedMint} =
        dev_pot_math:drip_global(GlobalAcc, ToMint + UndistributedMint, TotalWeightedUnits),
    ?event(
        {minting,
            {to_mint, ToMint},
            {total_weighted_units, TotalWeightedUnits},
            {old_global_accumulator, GlobalAcc},
            {new_global_accumulator, NewGlobalAcc}
        }),
    S#{
        <<"accumulator">> => NewGlobalAcc,
        <<"last-drip">> => T,
        <<"minted">> => AlreadyMinted + ToMint,
        <<"undistributed-mint">> => NewUndistributedMint
    }.

%% @doc Drip the state of a specific resource in the pot.
drip_resource(ResourceID, S, Opts) ->
    % Get the resource.
    Resource = hb_ao:get(<<"/resources/", ResourceID/binary>>, S, #{}, Opts),
    % Accumulate the Reward*CurrentWeight since the last global drip.
    OldResAcc =
        hb_maps:get(<<"accumulator">>, Resource, 0, Opts),
    Weight = hb_ao:get(<<"weight">>, Resource, 0, Opts),
    GlobalAcc = hb_maps:get(<<"accumulator">>, S, 0, Opts),
    LastGlobalAcc = hb_maps:get(<<"last-global-accumulator">>, Resource, 0, Opts),
    NewResourceAcc =
        dev_pot_math:drip_resource(
            OldResAcc,
            GlobalAcc,
            LastGlobalAcc,
            Weight
        ),
    ?event(
        {drip_resource,
            {resource_id, ResourceID},
            {weight, Weight},
            {global_accumulator, GlobalAcc},
            {old_resource_accumulator, OldResAcc},
            {new_resource_accumulator, NewResourceAcc}
        }
    ),
    hb_ao:set(
        S,
        #{
            <<"resources">> => #{
                ResourceID =>
                    Resource#{
                        <<"accumulator">> => NewResourceAcc,
                        <<"last-global-accumulator">> => GlobalAcc
                    }
            }
        },
        Opts
    ).

%% @doc Get the balance of a specific address in the pot by combining the base
%% balance with the unclaimed yield.
balance(Addr, S) ->
    hb_maps:get(Addr, hb_maps:get(<<"balances">>, S, #{}), 0)
        + unclaimed_yield(Addr, S, #{}).

%% @doc Return the unclaimed yield across all resources for a specific address.
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

%% @doc Calculate yield from already-dripped state (helper function).
%% Returns {Yield, ResourceAccumulator} tuple to avoid double-reads.
%% Assumes the state has been dripped for the given resource.
calculate_yield(Addr, ResourceID, DrippedS, Opts) ->
    Res = hb_ao:get(<<"resources/", ResourceID/binary>>, DrippedS, #{}, Opts),
    ResourceAcc = hb_maps:get(<<"accumulator">>, Res, 0, Opts),
    Deposits = hb_maps:get(<<"deposits">>, Res, #{}, Opts),
    Yield =
        case hb_maps:find(Addr, Deposits) of
            error -> 0;
            {ok, #{
                    <<"quantity">> := Qty,
                    <<"last-resource-accumulator">> := LastResourceAcc
                }} ->
                ?no_prod("Remove all floating point arithmetic."),
                dev_pot_math:drip_user(ResourceAcc, LastResourceAcc, Qty)
        end,
    {Yield, ResourceAcc}.

%% @doc Return the unclaimed yield for a specific address in a specific resource.
unclaimed_yield(Addr, ResourceID, UndrippedS, Opts) ->
    GlobalDrippedS = drip_global(UndrippedS, Opts),
    DrippedS = drip_resource(ResourceID, GlobalDrippedS, Opts),
    {Yield, _} = calculate_yield(Addr, ResourceID, DrippedS, Opts),
    Yield.

%% @doc Claim yield from a specific resource for an address.
%% Updates balance with the yield, resets checkpoint, and updates total-supply.
claim_yield(Addr, ResourceID, Base, Opts) ->
    GlobalDrippedS = drip_global(Base, Opts),
    DrippedS = #{
        <<"balances">> := Balances,
        <<"resources">> := Resources
    } = drip_resource(ResourceID, GlobalDrippedS, Opts),
    { Yield, ResourceAcc } = calculate_yield(Addr, ResourceID, DrippedS, Opts),
    ?event({claiming_yield, {yield, Yield}, {resource, ResourceID}}),
    case Yield of
        0 -> Base;
        _ ->
            BaseBalance = hb_ao:get(Addr, Balances, 0, Opts),
            CurrentSupply = hb_ao:get(<<"total-supply">>, DrippedS, 0, Opts),
            % Reset checkpoint only (don't touch deposit quantity)
            NewResources = hb_ao:set(
                Resources,
                <<
                    ResourceID/binary, 
                    "/deposits/", 
                    Addr/binary, 
                    "/last-resource-accumulator"
                >>,
                ResourceAcc,
                Opts
            ),
            ?event({ 
                updated_states,
                { balance, BaseBalance +Yield},
                { supply, CurrentSupply + Yield}
            }),
            DrippedS#{
                <<"resources">> => NewResources,
                <<"balances">> => Balances#{ Addr => BaseBalance + Yield },
                <<"total-supply">> => CurrentSupply + Yield
            }
    end.

%% @doc Claim yield from all resources for an address.
%% Calls claim_yield/4 for each resource sequentially.
claim_yield(Base, Assignment, Opts) ->
    Addr = hb_ao:get(<<"subject">>, Assignment, #{}, Opts),
    Resources = hb_ao:get(<<"resources">>, Base, #{}, Opts),
    ?event({claiming_all, {resources, Resources}, {address, Addr}}),
    hb_maps:fold(
        fun(ResourceID, _Resource, AccState) ->
            claim_yield(Addr, ResourceID, AccState, Opts)
        end,
        Base,
        hb_maps:without([<<"priv">>],Resources),
        Opts
    ).

%% @doc Deposit a quantity of a resource for a given address.
deposit(Addr, ResourceID, Amount, S0, Opts) when is_integer(Amount), Amount > 0 ->
    modify_deposit_state(Addr, ResourceID, Amount, S0, Opts).

%% @doc Withdraw a quantity of a resource for a given address. If the quantity
%% is insufficient, we'll revoke delegations until the withdrawal can be completed.
withdraw(Addr, ResourceID, Amount, S0, Opts) when is_integer(Amount), Amount > 0 ->
    ExistingDeposit = get_deposit(Addr, ResourceID, S0),
    S1 = liquidate(Addr, ResourceID, Amount - ExistingDeposit, S0, Opts),
    modify_deposit_state(Addr, ResourceID, -Amount, S1, Opts).

%% @doc For a given address, undelegate their delegations until the specified
%% quantity has been reclaimed.
liquidate(Addr, ResourceID, Amount, S, Opts) when Amount =< 0 -> S;
liquidate(Addr, ResourceID, Amount, S, Opts) ->
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
    RevokeAmount = min(Amount, LargestDelegation),
    S0 = undelegate(Addr, LargestDelegationAddr, ResourceID, RevokeAmount, S, Opts),
    liquidate(Addr, ResourceID, Amount - RevokeAmount, S0, Opts).

%% @doc Delegate some quantity of a resource from one address to another.
delegate(FromAddr, ToAddr, ResourceID, Amount, S, Opts) when Amount > 0 ->
    ?event(
        {delegating,
            {from_addr, FromAddr},
            {to_addr, ToAddr},
            {resource_id, ResourceID},
            {amount, Amount}
        }
    ),
    GlobalDrippedS = drip_global(S, Opts),
    DrippedS = #{
        <<"balances">> := Balances
    } = drip_resource(ResourceID, GlobalDrippedS, Opts),
    DelegatorBalance = hb_ao:get(FromAddr, Balances, 0, Opts),
    RecipientBalance = hb_ao:get(ToAddr, Balances, 0, Opts),
    S0 = DrippedS#{
      <<"balances">> => Balances#{
          FromAddr => DelegatorBalance + unclaimed_yield(FromAddr, ResourceID, DrippedS, Opts),
          ToAddr => RecipientBalance + unclaimed_yield(ToAddr, ResourceID, DrippedS, Opts)
      }
    },
    DelegatorDeposit = get_deposit(FromAddr, ResourceID, S),
    S1 =
        hb_ao:set(
            S0,
            <<
                "/resources/",
                ResourceID/binary,
                "/deposits/",
                FromAddr/binary,
                "/quantity"
            >>,
            DelegatorDeposit - Amount,
            Opts
        ),
    ExistingDelegation =
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
    S2 =
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
            ExistingDelegation + Amount,
            Opts
        ),
    RecipientDeposit = get_deposit(ToAddr, ResourceID, S2),
    S3 =
        hb_ao:set(
            S2,
            <<
                "/resources/", 
                ResourceID/binary,
                "/deposits/",
                ToAddr/binary,
                "/quantity"
            >>,
            RecipientDeposit + Amount,
            Opts
        ),
    send_delegation_notice(ToAddr, ResourceID, Amount, S3, Opts).

%% @doc Undelegate some quantity of a resource from one address to another.
undelegate(FromAddr, ToAddr, ResourceID, Amount, S, Opts) when Amount > 0 ->
    RecipientDeposit = get_deposit(ToAddr, ResourceID, S),
    Liquidated = liquidate(ToAddr, ResourceID, Amount - RecipientDeposit, S, Opts),
    GlobalDrippedS = drip_global(Liquidated, Opts),
    DrippedS = #{
        <<"balances">> := Balances
    } = drip_resource(ResourceID, GlobalDrippedS, Opts),
    DelegatorBalance = hb_ao:get(FromAddr, Balances, 0, Opts),
    RecipientBalance = hb_ao:get(ToAddr, Balances, 0, Opts),
    S0 = DrippedS#{
      <<"balances">> => Balances#{
          FromAddr => DelegatorBalance + unclaimed_yield(FromAddr, ResourceID, DrippedS, Opts),
          ToAddr => RecipientBalance + unclaimed_yield(ToAddr, ResourceID, DrippedS, Opts)
      }
    },
    NewRecipientDeposit = get_deposit(ToAddr, ResourceID, S0),
    S1 =
        hb_ao:set(
            S0,
            <<
                "/resources/",
                ResourceID/binary,
                "/deposits/",
                ToAddr/binary,
                "/quantity"
            >>,
            NewRecipientDeposit - Amount,
            Opts
        ),
    DelegatorDeposit = get_deposit(FromAddr, ResourceID, S1),
    S2 =
        hb_ao:set(
            S1,
            <<
                "/resources/",
                ResourceID/binary,
                "/deposits/",
                FromAddr/binary,
                "/quantity"
            >>,
            DelegatorDeposit + Amount,
            Opts
        ),
    ExistingDelegation =
        hb_ao:get(
            <<
                "/resources/",
                ResourceID/binary,
                "/deposits/",
                FromAddr/binary,
                "/delegations/",
                ToAddr/binary
            >>,
            S2,
            0,
            Opts
        ),
    S3 =
        hb_ao:set(
            S2,
            <<
                "/resources/",
                ResourceID/binary,
                "/deposits/",
                FromAddr/binary,
                "/delegations/",
                ToAddr/binary
            >>,
            ExistingDelegation - Amount,
            Opts
        ),
    send_delegation_notice(ToAddr, ResourceID, -Amount, S3, Opts).

%% @doc Set the weight of a specific resource in the pot.
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

%% @doc Update the inverted index for a specific address in a specific resource.
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

%% @doc Add a new `delegation-notice` to the outbox of the state.
send_delegation_notice(Addr, ResourceID, Amount, S, Opts) ->
    Outbox = hb_ao:get(<<"results/outbox">>, S, [], Opts),
    DelegationNotice = #{
        <<"target">> => Addr,
        <<"action">> => <<"delegation-notice">>,
        <<"quantity">> => Amount,
        <<"resource">> => ResourceID
    },
    hb_ao:set(
        S,
        <<"results/outbox">>,
        [DelegationNotice|Outbox],
        Opts
    ).

%%% Helpers.

%% @doc Used by deposit() and withdraw() to update the state of the world. Note that
%% the domain of deposit() and withdraw() are the natural numbers, but the domain of
%% modify_deposit_state() includes negative numbers as well.
modify_deposit_state(Addr, ResourceID, Amount, S0, Opts) ->
    % Drip the global state and the resource, then extract necessary components.
    GlobalDrippedS = drip_global(S0, Opts),
    DrippedS = #{
        <<"balances">> := Balances,
        <<"resources">> := Resources
    } = drip_resource(ResourceID, GlobalDrippedS, Opts),
    ExistingDeposit = get_deposit(Addr, ResourceID, DrippedS),
    BaseBalance = hb_ao:get(Addr, Balances, 0, Opts),
    NewBalance = BaseBalance + unclaimed_yield(Addr, ResourceID, DrippedS, Opts),
    ResourceAcc =
        hb_ao:get(
            <<ResourceID/binary, "/accumulator">>,
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
                                        <<"last-resource-accumulator">> =>
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
    update_deposit_index(
        Addr,
        ResourceID,
        get_deposit(Addr, ResourceID, UpdatedDepositS),
        UpdatedDepositS,
        Opts
    ).

%% @doc Get the deposit quantity for a specific address in a specific resource.
get_deposit(Addr, ResourceID, S) ->
    hb_ao:get(
        <<"/resources/", ResourceID/binary, "/deposits/", Addr/binary, "/quantity">>,
        S,
        0,
        #{}
    ).

%% @doc Get the balances submessage from the state.
balances(S = #{ <<"balances">> := Bs }) ->
    hb_maps:map(fun(Addr, _) -> balance(Addr, S) end, Bs).

%% @doc Return only the deposits submessage for all resources in the state.
get_deposits(S = #{ <<"resources">> := Resources }) ->
    hb_maps:map(fun(ResourceID, _) -> get_deposits(ResourceID, S) end, Resources).
get_deposits(ResourceID, S) ->
    Ds = hb_ao:get(
        <<"/resources/", ResourceID/binary, "/deposits">>,
        S,
        #{},
        #{}
    ),
    hb_maps:map(fun(Addr, _) -> get_deposit(Addr, ResourceID, S) end, Ds).

%% @doc Return the contents of the inverted index for a specific address.
user(Addr, S, Opts) ->
    hb_ao:get(<<"/users/", Addr/binary>>, S, #{}, Opts).
