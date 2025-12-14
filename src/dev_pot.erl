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
-export([info/1, mint/3, deposit/3, withdraw/3, delegate/3, undelegate/3, set_weight/3]).
%%% `~pot@1.0` Private Utilities.
-export([test_drip/3]).
-export([deposit/5, withdraw/5, delegate/6, undelegate/6, set_weight/4]).
-export([update_deposit_index/5]).
-export([user/3, balance/3, balances/1, balances/2]).
-export([get_deposit/4, get_deposits/2, get_deposits/3]).

%%% Pot Model Functions.

info(_S) ->
    #{ exports =>
        [
            <<"mint">>,
            <<"deposit">>,
            <<"withdraw">>,
            <<"delegate">>,
            <<"undelegate">>,
            <<"set_weight">>
        ]
    }.

%% @doc Normalizes the state of the pot for either the global scope or a
%% specific user ID.
mint(RawState, Req, Opts) ->
    State = ensure_initialized(RawState, Req, Opts),
    ?event(debug_mint, {after_ensure_initialized, State}, Opts),
    GloballyDripped = drip_global(State, Req, Opts),
    ?event(debug_mint, {after_drip_global, GloballyDripped}, Opts),
    Res =
        case hb_ao:get(<<"subject">>, Req, <<"global">>, Opts) of
            <<"global">> -> {ok, GloballyDripped};
            Subject -> {ok, drip_user(Subject, GloballyDripped, Opts)}
        end,
    ?event(debug_mint, {mint_result, Res}, Opts),
    Res.

%% @doc Force the `t` of the pot to increase -- either by 1 or the new given `t`
%% value -- and drip globally. Used only for testing purposes.
test_drip(State, Req, Opts) ->
    StateWithNewTime =
        case is_map(Req) andalso hb_maps:find(<<"t">>, Req) of
            {ok, TReq} -> State#{ <<"t">> => TReq };
            _ -> State#{ <<"t">> => hb_maps:get(<<"t">>, State, 0, Opts) + 1 }
        end,
    drip_global(StateWithNewTime, Opts).

%% @doc Drip the global state of the pot if necessary, returning the state
%% unchanged if no time has passed since the last drip. If `Req/timestamp` is
%% provided it will be used as the new `t` for the pot before dripping.
drip_global(State, Req, Opts) ->
    ?event({drip_global, {req, Req}}),
    UpdatedState =
        case hb_maps:get(<<"timestamp">>, Req, undefined, Opts) of
            undefined -> State;
            NewTime ->
                hb_ao:set(
                    State,
                    #{ <<"t">> => NewTime },
                    Opts
                )
        end,
    ?event({drip_global, { updated, UpdatedState}, {req, Req}}),
    drip_global(UpdatedState, Opts).
drip_global(S = #{ <<"t">> := T, <<"last-drip">> := Last }, _) when T == Last -> S;
drip_global(S = #{
        <<"t">> := T,
        <<"mint-cap">> := Max,
        <<"mint-prop-numerator">> := PropN,
        <<"mint-prop-denominator">> := PropD
    }, Opts) ->
    ?event({drip_global, { timestamp, T}}),
    % throw("timestamp very big"),
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
    ?event(debug_test,
        {drip_global,
            {t, T},
            {last_drip, LastT},
            {minted, AlreadyMinted},
            {undistributed_mint, UndistributedMint},
            {total_weighted_units, TotalWeightedUnits},
            {global_accumulator, GlobalAcc},
            {to_mint, ToMint}
        }, Opts),
    {NewGlobalAcc, NewUndistributedMint} =
        dev_pot_math:drip_global(
            GlobalAcc,
            ToMint + UndistributedMint,
            TotalWeightedUnits
        ),
    ?event(debug_test,
        {minting,
            {to_mint, ToMint},
            {total_weighted_units, TotalWeightedUnits},
            {old_global_accumulator, GlobalAcc},
            {new_global_accumulator, NewGlobalAcc}
        }),
    hb_ao:set(
        S,
        #{
            <<"accumulator">> => NewGlobalAcc,
            <<"last-drip">> => T,
            <<"minted">> => AlreadyMinted + ToMint,
            <<"undistributed-mint">> => NewUndistributedMint
        },
        Opts
    ).

%% @doc Drip the state of a specific resource in the pot. Does not drip the
%% global state before doing so.
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

%% @doc Drip all resources for user, add the unclaimed yield to their explicit
%% balance, and reset their unclaimed yield to 0. Does not drip the global state
%% before doing so.
drip_user(Addr, S, Opts) ->
    ResourceIDs =
        hb_maps:keys(
            hb_private:reset(
                hb_ao:get(<<"users/", Addr/binary, "/deposits">>, S, #{}, Opts)
            ),
            Opts
        ),
    lists:foldl(
        fun(ResID, StateAcc) ->
            modify_deposit_state(
                Addr,
                ResID,
                0,
                StateAcc,
                Opts
            )
        end,
        S,
        ResourceIDs
    ).

%% @doc Ensure the base state is initialized, with `t` set to either the new
%% `timestamp` from the request, the existing `t` value, or 0. `last-drip` will
%% be initialized to the same value as `t` if not already set.
ensure_initialized(Base, Req, Opts) ->
    WithT =
        hb_ao:set(
            Base,
            #{
                <<"t">> =>
                    NewT =
                        hb_maps:get(
                            <<"timestamp">>,
                            Req,
                            hb_maps:get(<<"t">>, Base, 0, Opts),
                            Opts
                        )
            },
            Opts
        ),
    hb_ao:set(
        WithT,
        #{
            <<"last-drip">> =>
                hb_ao:get(
                    <<"last-drip">>,
                    WithT,
                    NewT,
                    Opts
                )
        },
        Opts
    ).

%% @doc Get the balance of a specific address in the pot by combining the base
%% balance with the unclaimed yield.
balance(Addr, S, Opts) ->
    hb_maps:get(Addr, hb_maps:get(<<"balances">>, S, #{}, Opts), 0, Opts)
        + unclaimed_yield(Addr, S, Opts).

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
            fun(ResID) -> unclaimed_yield(Addr, ResID, S, Opts) end,
            ResourceIDs
        )
    ).

%% @doc Return the unclaimed yield for a specific address in a specific resource.
unclaimed_yield(Addr, ResourceID, UndrippedS, Opts) ->
    GlobalDrippedS = drip_global(UndrippedS, Opts),
    S = drip_resource(ResourceID, GlobalDrippedS, Opts),
    Res = hb_ao:get(<<"resources/", ResourceID/binary>>, S, #{}, Opts),
    ResourceAcc = hb_maps:get(<<"accumulator">>, Res, 0, Opts),
    Deposits = hb_maps:get(<<"deposits">>, Res, #{}, Opts),
    case hb_maps:find(Addr, Deposits) of
        error -> 0;
        {ok, #{
                <<"quantity">> := Qty,
                <<"last-resource-accumulator">> := LastResourceAcc
            }} ->
            ?no_prod("Remove all floating point arithmetic."),
            dev_pot_math:drip_user(ResourceAcc, LastResourceAcc, Qty)
    end.

%% @doc Deposit a quantity of a resource for a given address.
deposit(State, Req, Opts) ->
    maybe
        {ok, Address} ?= hb_maps:find(<<"address">>, Req, Opts),
        {ok, ResourceID} ?= hb_maps:find(<<"resource-id">>, Req, Opts),
        {ok, Amount} ?= hb_maps:find(<<"amount">>, Req, Opts),
        deposit(Address, ResourceID, Amount, State, Opts)
    else
        Reason -> Reason
    end.
deposit(Addr, ResourceID, Amount, S0, Opts) when is_integer(Amount), Amount > 0 ->
    modify_deposit_state(Addr, ResourceID, Amount, S0, Opts).

%% @doc Withdraw a quantity of a resource for a given address. If the quantity
%% is insufficient, we'll revoke delegations until the withdrawal can be completed.
withdraw(State, Req, Opts) ->
    maybe
        {ok, Address} ?= hb_maps:find(<<"address">>, Req, Opts),
        {ok, ResourceID} ?= hb_maps:find(<<"resource-id">>, Req, Opts),
        {ok, Amount} ?= hb_maps:find(<<"amount">>, Req, Opts),
        withdraw(Address, ResourceID, Amount, State, Opts)
    else
        Reason -> Reason
    end.
withdraw(Addr, ResourceID, Amount, S0, Opts) when is_integer(Amount), Amount > 0 ->
    ExistingDeposit = get_deposit(Addr, ResourceID, S0, Opts),
    S1 = liquidate(Addr, ResourceID, Amount - ExistingDeposit, S0, Opts),
    modify_deposit_state(Addr, ResourceID, -Amount, S1, Opts).

%% @doc For a given address, undelegate their delegations until the specified
%% quantity has been reclaimed.
liquidate(_Addr, _ResourceID, Amount, S, _Opts) when Amount =< 0 -> S;
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
delegate(State, Req, Opts) ->
    maybe
        {ok, FromAddr} ?= hb_maps:find(<<"from-address">>, Req, Opts),
        {ok, ToAddr} ?= hb_maps:find(<<"to-address">>, Req, Opts),
        {ok, ResourceID} ?= hb_maps:find(<<"resource-id">>, Req, Opts),
        {ok, Amount} ?= hb_maps:find(<<"amount">>, Req, Opts),
        delegate(FromAddr, ToAddr, ResourceID, Amount, State, Opts)
    else
        Reason -> Reason
    end.
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
    DelegatorYield = unclaimed_yield(FromAddr, ResourceID, DrippedS, Opts),
    RecipientYield = unclaimed_yield(ToAddr, ResourceID, DrippedS, Opts),
    NewBalances = 
        Balances#{
            FromAddr => DelegatorBalance + DelegatorYield,
            ToAddr => RecipientBalance + RecipientYield
        },
    {ok, S0} = 
        hb_ao:resolve(
            DrippedS,
            #{
                <<"path">> => <<"set">>,
                <<"balances">> => NewBalances
            },
            Opts
        ),
    DelegatorDeposit = get_deposit(FromAddr, ResourceID, S, Opts),
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
    RecipientDeposit = get_deposit(ToAddr, ResourceID, S2, Opts),
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
undelegate(State, Req, Opts) ->
    maybe
        {ok, FromAddr} ?= hb_maps:find(<<"from-address">>, Req, Opts),
        {ok, ToAddr} ?= hb_maps:find(<<"to-address">>, Req, Opts),
        {ok, ResourceID} ?= hb_maps:find(<<"resource-id">>, Req, Opts),
        {ok, Amount} ?= hb_maps:find(<<"amount">>, Req, Opts),
        undelegate(FromAddr, ToAddr, ResourceID, Amount, State, Opts)
    else
        Reason -> Reason
    end.
undelegate(FromAddr, ToAddr, ResourceID, Amount, S, Opts) when Amount > 0 ->
    RecipientDeposit = get_deposit(ToAddr, ResourceID, S, Opts),
    Liquidated = liquidate(ToAddr, ResourceID, Amount - RecipientDeposit, S, Opts),
    GlobalDrippedS = drip_global(Liquidated, Opts),
    DrippedS = #{
        <<"balances">> := Balances
    } = drip_resource(ResourceID, GlobalDrippedS, Opts),
    DelegatorBalance = hb_ao:get(FromAddr, Balances, 0, Opts),
    RecipientBalance = hb_ao:get(ToAddr, Balances, 0, Opts),
    DelegatorYield = unclaimed_yield(FromAddr, ResourceID, DrippedS, Opts),
    RecipientYield = unclaimed_yield(ToAddr, ResourceID, DrippedS, Opts),
    NewBalances =
        Balances#{
            FromAddr => DelegatorBalance + DelegatorYield,
            ToAddr => RecipientBalance + RecipientYield
        },
    {ok, S0} =
        hb_ao:resolve(
            DrippedS,
            #{
                <<"path">> => <<"set">>,
                <<"balances">> => NewBalances
            },
            Opts
        ),
    NewRecipientDeposit = get_deposit(ToAddr, ResourceID, S0, Opts),
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
    DelegatorDeposit = get_deposit(FromAddr, ResourceID, S1, Opts),
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
set_weight(State, Req, Opts) ->
    maybe
        {ok, ResourceID} ?= hb_maps:find(<<"resource-id">>, Req, Opts),
        {ok, Weight} ?= hb_maps:find(<<"weight">>, Req, Opts),
        set_weight(ResourceID, Weight, State, Opts)
    else
        Reason -> Reason
    end.
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

%% @doc Send a `Action: Deposit | Withdraw` notice to a user whose deposit has
%% been modified.
send_delegation_notice(Addr, ResourceID, Amount, S, Opts) ->
    hb_util:ok(
        dev_process_lib:send(
            #{
                <<"target">> => Addr,
                <<"action">> =>
                    if Amount > 0 -> <<"Deposit">>;
                    true -> <<"Withdraw">>
                    end,
                <<"quantity">> => Amount,
                <<"resource">> => ResourceID
            },
            S,
            Opts
        ),
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
    ?event(debug_test_state, {modify_deposit_state, {dripped_s, DrippedS}}, Opts),
    ExistingDeposit = get_deposit(Addr, ResourceID, DrippedS, Opts),
    BaseBalance = hb_ao:get(Addr, Balances, 0, Opts),
    CurrentSupply = hb_ao:get(<<"total-supply">>, S0, 0, Opts),
    Yield = unclaimed_yield(Addr, ResourceID, DrippedS, Opts),
    NewBalance = BaseBalance + Yield,
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
    NewTotalWeightedUnits = TotalWeightedUnits + (WeightR * Amount),
    {ok, NewBalances} =
        hb_ao:resolve(
            Balances,
            #{
                <<"path">> => <<"set">>,
                Addr => NewBalance
            },
            Opts
        ),
    UpdateValues = 
        #{
            <<"resources">> => NewResources,
            <<"total-weighted-units">> => NewTotalWeightedUnits,
            <<"balances">> => NewBalances,
            <<"total-supply">> => CurrentSupply + Yield
        },
    {ok, UpdatedDepositS} =
        hb_ao:resolve(
            DrippedS,
            UpdateValues#{ <<"path">> => <<"set">> },
            Opts
        ),
    update_deposit_index(
        Addr,
        ResourceID,
        get_deposit(Addr, ResourceID, UpdatedDepositS, Opts),
        UpdatedDepositS,
        Opts
    ).

%% @doc Get the deposit quantity for a specific address in a specific resource.
get_deposit(Addr, ResourceID, S, Opts) ->
    hb_ao:get(
        <<"/resources/", ResourceID/binary, "/deposits/", Addr/binary, "/quantity">>,
        S,
        0,
        Opts
    ).

%% @doc Get the balances submessage from the state.
balances(S) -> balances(S, #{}).
balances(S = #{ <<"balances">> := Bs }, Opts) ->
    hb_maps:map(fun(Addr, _) -> balance(Addr, S, Opts) end, hb_private:reset(Bs)).

%% @doc Return only the deposits submessage for all resources in the state.
get_deposits(S = #{ <<"resources">> := Resources }, Opts) ->
    hb_maps:map(
        fun(ResourceID, _) -> get_deposits(ResourceID, S, Opts) end,
        Resources,
        Opts
    ).
get_deposits(ResourceID, S, Opts) ->
    Ds = hb_ao:get(
        <<"/resources/", ResourceID/binary, "/deposits">>,
        S,
        #{},
        Opts
    ),
    hb_maps:map(
        fun(Addr, _) -> get_deposit(Addr, ResourceID, S, Opts) end,
        Ds,
        Opts
    ).

%% @doc Return the contents of the inverted index for a specific address.
user(Addr, S, Opts) ->
    hb_ao:get(<<"/users/", Addr/binary>>, S, #{}, Opts).
