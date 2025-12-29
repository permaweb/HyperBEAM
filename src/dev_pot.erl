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
-export([info/1, mint/3, deposit/3, withdraw/3, delegate/3, undelegate/3]).
-export([register/3, notify/3]).
%%% `~pot@1.0` Private Utilities.
-export([test_drip/3]).
-export([deposit/5, withdraw/5, delegate/6, undelegate/6, register_resource/4]).
-export([update_deposit_index/5]).
-export([user/3, balance/3, balances/1, balances/2]).
%%% Public path helpers.
-export([resource_path/1, resource_acc_path/1, resource_weight_path/1]).
-export([resource_deposit_path/2, resource_total_deposits_path/1]).
-export([delegations_path/2, delegation_qty_path/3]).
-export([deposit_qty_path/2, last_resource_acc_path/2]).

%%% Pot Model Functions.

info(_S) ->
    #{
        exports =>
            [
                <<"mint">>,
                <<"deposit">>,
                <<"withdraw">>,
                <<"delegate">>,
                <<"undelegate">>,
                <<"register">>,
                <<"notify">>
            ]
    }.

%% @doc Normalizes the state of the pot for either the global scope or a
%% specific user ID.
mint(RawState, Req, Opts) ->
    State = ensure_initialized(RawState, Req, Opts),
    ?event(debug_drip, {after_ensure_initialized, State}, Opts),
    GloballyDripped = drip_global(State, Req, Opts),
    ?event(debug_drip, {after_drip_global, GloballyDripped}, Opts),
    case hb_ao:get(<<"subject">>, Req, <<"global">>, Opts) of
        <<"global">> -> {ok, GloballyDripped};
        Subject -> {ok, drip_user(Subject, GloballyDripped, Opts)}
    end.

%% @doc Set the weight of a specific resource in the pot. Valid requesters to
%% change resource parameters are:
%% - The `State/parent' address, if set.
%% - The `mint-authority' address, if set.
%% - The `resource-authority' address for the resource.
register(State, Assignment, Opts) ->
    ?event(debug_pot, {register, Assignment}, Opts),
    maybe
        {ok, {Req, ResourceID, From}} ?= parse_register_req(Assignment, Opts),
        true ?= authorize_register(ResourceID, State, Req, From, Opts),
        WeightedState = maybe_register_weight(ResourceID, Req, State, Opts),
        maybe_register_authority(ResourceID, Req, WeightedState, Opts)
    else
        Reason ->
            ?event(debug_pot, {error, Reason}, Opts),
            {error, Reason}
    end.

%% @doc Force the `t` of the pot to increase -- either by 1 or the new given `t`
%% value -- and drip globally. Used only for testing purposes.
test_drip(State, Req, Opts) ->
    StateWithNewTime =
        case is_map(Req) andalso hb_maps:find(<<"t">>, Req) of
            {ok, TReq} -> State#{ <<"t">> => TReq };
            _ -> State#{ <<"t">> => hb_maps:get(<<"t">>, State, 0, Opts) + 1 }
        end,
    drip_global(StateWithNewTime, Opts).

%% @doc Deposit a quantity of a resource for a given address.
deposit(State, Assignment, Opts) ->
    maybe
        {ok, {Address, ResourceID, Amount}} ?=
            parse_deposit_modification(State, Assignment, Opts),
        deposit(Address, ResourceID, Amount, State, Opts)
    end.
deposit(Addr, ResourceID, Amount, S0, Opts) when is_integer(Amount), Amount > 0 ->
    modify_deposit_state(Addr, ResourceID, Amount, S0, Opts).

%% @doc Withdraw a quantity of a resource for a given address. If the quantity
%% is insufficient, we'll revoke delegations until the withdrawal can be completed.
withdraw(Base, Req, Opts) ->
    maybe
        {ok, {Address, ResourceID, Amount}} ?=
            parse_deposit_modification(Base, Req, Opts),
        withdraw(Address, ResourceID, Amount, Base, Opts)
    end.
withdraw(Addr, ResourceID, Amount, S0, Opts) when is_integer(Amount), Amount > 0 ->
    ExistingDeposit = hb_ao:get(deposit_qty_path(ResourceID, Addr), S0, 0, Opts),
    S1 = liquidate(Addr, ResourceID, Amount - ExistingDeposit, S0, Opts),
    modify_deposit_state(Addr, ResourceID, -Amount, S1, Opts).

%% @doc Delegate some quantity of a resource from one address to another.
delegate(State, Assignment, Opts) ->
    maybe
        {ok, {From, To, Resource, Amount}} ?= 
            parse_delegation_req(Assignment, Opts),
        {ok, delegate(From, To, Resource, Amount, State, Opts)}
    end.
delegate(From, To, ResourceID, Amount, S, Opts) when Amount > 0 ->
    DrippedState = drip_all(ResourceID, [From, To], S, Opts),
    NewState = apply_delegation(From, To, ResourceID, Amount, DrippedState, Opts),
    send_delegation_notice(From, To, ResourceID, Amount, NewState, Opts).

%% @doc Undelegate some quantity of a resource from one address to another.
undelegate(State, Assignment, Opts) ->
    maybe
        {ok, {From, To, Resource, Amount}} ?= 
            parse_delegation_req(Assignment, Opts),
        {ok, undelegate(From, To, Resource, Amount, State, Opts)}
    end.
undelegate(From, To, ResourceID, Amount, S, Opts) when Amount > 0 ->
    RecipientDeposit = hb_ao:get(deposit_qty_path(ResourceID, To), S, 0, Opts),
    LiquidatedS = liquidate(To, ResourceID, Amount - RecipientDeposit, S, Opts),
    DrippedState = drip_all(ResourceID, [From, To], LiquidatedS, Opts),
    UpdatedBalS = settle_yields(From, To, ResourceID, DrippedState, Opts),
    NewState = 
        apply_undelegation(From, To, ResourceID, Amount, UpdatedBalS, Opts),
    send_delegation_notice(From, To, ResourceID, -Amount, NewState, Opts).

%% @doc Interpret `notify' messages as if they were direct deposit/withdrawal
%% requests, if they are sent `from' our `parent' mint process (if set).
notify(State, Assignment, Opts) ->
    maybe
        {ok, {NotifyFrom, Action, OriginalFrom, ForwardedMsg}} ?=
            parse_notification_req(Assignment, Opts),
        dev_token:handle_action(
            Action,
            State,
            #{
                <<"type">> => <<"notification">>,
                <<"original-from">> => OriginalFrom,
                <<"body">> => ForwardedMsg#{ <<"from">> => NotifyFrom }
            },
            Opts
        )
    end.

%% @doc Drip the global state of the pot if necessary, returning the state
%% unchanged if no time has passed since the last drip. If `Req/timestamp` is
%% provided it will be used as the new `t` for the pot before dripping.
drip_global(State, Req, Opts) ->
    drip_global(ensure_initialized(State, Req, Opts), Opts).
drip_global(Link, Opts) when ?IS_LINK(Link) ->
    drip_global(hb_cache:ensure_loaded(Link, Opts), Opts);
drip_global(S = #{ <<"t">> := T, <<"last-drip">> := Last }, Opts)
        when ?IS_LINK(T) orelse ?IS_LINK(Last) ->
    drip_global(
        S#{
            <<"t">> => hb_cache:ensure_loaded(T, Opts),
            <<"last-drip">> => hb_cache:ensure_loaded(Last, Opts)
        },
        Opts
    );
drip_global(S = #{ <<"t">> := T, <<"last-drip">> := Last }, _) when T == Last -> S;
drip_global(S, Opts) ->
    T = hb_ao:get(<<"t">>, S, 0, Opts),
    AlreadyMinted = hb_maps:get(<<"minted">>, S, 0, Opts),
    LastT = hb_maps:get(<<"last-drip">>, S, 0, Opts),
    MintCap = hb_ao:get(<<"mint-cap">>, S, 0, Opts),
    TotalWeightedUnits = hb_maps:get(<<"total-weighted-units">>, S, 0, Opts),
    GlobalAcc = hb_maps:get(<<"accumulator">>, S, 0, Opts),
    PropN = hb_ao:get(<<"mint-prop-numerator">>, S, 0, Opts),
    PropD = hb_ao:get(<<"mint-prop-denominator">>, S, 0, Opts),
    ToMint =
        dev_pot_math:minted_between(
            AlreadyMinted,
            MintCap,
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
        }, 
        Opts
    ),
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
    ?event(debug_pot, 
        {
            drip_resource, 
            {resource_id, ResourceID}, 
            {state, S}
        }, 
        Opts
    ),
    % Get the resource.
    Resource = hb_ao:get(resource_path(ResourceID), S, #{}, Opts),
    % Accumulate the Reward*CurrentWeight since the last global drip.
    OldResAcc = hb_maps:get(<<"accumulator">>, Resource, 0, Opts),
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
    % TODO: Add recieved delegations to the `users/` index, such that we can
    % change the below to only drip resources for which the user is actively
    % participating. This will change the balances `O` complexity from
    % `O(all_resources)` to `O(user.active_resources)`.
    ?no_prod("Only drip resources for which the user has a deposit."),
    ResourceIDs =
        hb_maps:keys(
            hb_private:reset(hb_message:uncommitted(
                hb_ao:get(
                    <<"resources">>,
                    S,
                    #{},
                    Opts
                )
            )),
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
ensure_initialized(RawBase, Req, Opts) ->
    Base = maybe_initialize_subscriptions(RawBase, Req, Opts),
    TimeSource = hb_maps:get(<<"t-source">>, Base, <<"timestamp">>, Opts),
    NewT =
        hb_maps:get(
            TimeSource,
            Req,
            hb_maps:get(<<"t">>, Base, 0, Opts),
            Opts
        ),
    hb_ao:set(
        Base,
        #{
            <<"t">> => NewT,
            <<"last-drip">> => hb_ao:get(<<"last-drip">>, Base, NewT, Opts)
        },
        Opts
    ).

%% @doc If the process has not yet initialized, do so. In either case, return the
%% base state with the subscriptions initialized.
maybe_initialize_subscriptions(Base, Req, Opts) ->
    case hb_maps:get(<<"subscriptions">>, Base, not_found, Opts) of
        not_found -> initialize_subscriptions(Base, Req, Opts);
        _ -> Base
    end.

%% @doc If the process has a `parent' mint set, send a subscription request to
%% the parent process for all `set-weight' messages.
initialize_subscriptions(Base, _Req, Opts) ->
    case hb_maps:get(<<"parent">>, Base, not_found, Opts) of
        not_found -> Base;
        Parent ->
            dev_process_outbox:send_subscription_request(
                Parent,
                <<"register">>,
                Base,
                Opts
            )
    end.

%%% Helpers.

%% @doc Used by deposit() and withdraw() to update the state of the world. Note that
%% the domain of deposit() and withdraw() are the natural numbers, but the domain of
%% modify_deposit_state() includes negative numbers as well.
modify_deposit_state(Addr, ResourceID, Amount, S0, Opts) ->
    % Drip the global state and the resource, then extract necessary components.
    GlobalDrippedS = drip_global(S0, Opts),
    DrippedS = drip_resource(ResourceID, GlobalDrippedS, Opts),
    Balances = 
        hb_ao:get(<<"balances">>, DrippedS, #{ <<"device">> => <<"trie">> }, Opts),
    Resources = hb_ao:get(<<"resources">>, DrippedS, #{}, Opts),
    ?event(
        debug_drip,
        {modify_deposit_state,
            {addr, Addr},
            {resource_id, ResourceID},
            {amount, Amount},
            {resources, Resources},
            {balances, Balances}
        },
        Opts
    ),
    CurrDeposit = hb_ao:get(deposit_qty_path(ResourceID, Addr), DrippedS, 0, Opts),
    CurrBalance = hb_ao:get(<<"balances/",Addr/binary>>, DrippedS, 0, Opts),
    CurrSupply = hb_ao:get(<<"total-supply">>, DrippedS, 0, Opts),
    ResourceAcc = hb_ao:get(resource_acc_path(ResourceID), DrippedS, 0, Opts),
    Weight = hb_ao:get(resource_weight_path(ResourceID), DrippedS, 0, Opts),
    CurrTotalDeposit = 
        hb_ao:get(resource_total_deposits_path(ResourceID), DrippedS, 0, Opts),
    CurrentTWU = hb_maps:get(<<"total-weighted-units">>, DrippedS, 0, Opts),
    Yield = unclaimed_yield(Addr, ResourceID, DrippedS, Opts),
    NewBalance = CurrBalance + Yield,
    NewTWU = CurrentTWU + (Weight * Amount),
    %TODO: Following set and resolve can be merge into one resolve call.
    NewResources =
        hb_ao:set(
            Resources,
            #{
                ResourceID =>
                    #{
                        <<"total-deposits">> =>
                            Amount + CurrTotalDeposit,
                        <<"deposits">> =>
                            #{
                                Addr =>
                                    #{
                                        <<"quantity">> => 
                                            CurrDeposit + Amount,
                                        <<"last-resource-accumulator">> =>
                                            ResourceAcc
                                    }
                            }
                    }
            },
            Opts
        ),
    {ok, NewBalances} =
        hb_ao:resolve(
            Balances,
            #{
                <<"path">> => <<"set">>,
                Addr => NewBalance
            },
            Opts
        ),
    NewVal = 
        #{
            <<"resources">> => NewResources,
            <<"total-weighted-units">> => NewTWU,
            <<"balances">> => NewBalances,
            <<"total-supply">> => CurrSupply + Yield
        },
    {ok, UpdatedDepositS} = 
        hb_ao:resolve(DrippedS, NewVal#{ <<"path">> => <<"set">> }, Opts),
    % TODO: can remove index as it is not being used
    update_deposit_index(
        Addr,
        ResourceID,
        hb_ao:get(deposit_qty_path(ResourceID, Addr), UpdatedDepositS, 0, Opts),
        UpdatedDepositS,
        Opts
    ).

%% @doc Update the inverted index for a specific address in a specific resource.
update_deposit_index(Addr, ResourceID, Quantity, S, Opts) ->
    Delegations = 
        hb_ao:get(delegations_path(ResourceID, Addr), S, #{}, Opts),
    hb_ao:set(
        S,
        <<"users/", Addr/binary, "/deposits/", ResourceID/binary>>,
        if Quantity == 0 andalso ?IS_EMPTY_MESSAGE(Delegations) -> unset;
        true -> Quantity
        end,
        Opts
    ).

%% @doc For a given address, undelegate their delegations until the specified
%% quantity has been reclaimed.
liquidate(_Addr, _ResourceID, Amount, S, _Opts) when Amount =< 0 -> S;
liquidate(Addr, ResourceID, Amount, S, Opts) ->
    Delegations = hb_ao:get(delegations_path(ResourceID, Addr), S, #{}, Opts),
    LargestDelegation =
        lists:max(
            hb_maps:values(
                hb_private:reset(Delegations),
                Opts
            )
        ),
    {LargestDelegationAddr, _} =
        lists:keyfind(
            LargestDelegation,
            2,
            hb_maps:to_list(Delegations, Opts)
        ),
    RevokeAmount = min(Amount, LargestDelegation),
    UndelegatedState = 
        undelegate(Addr, LargestDelegationAddr, ResourceID, RevokeAmount, S, Opts),
    liquidate(Addr, ResourceID, Amount - RevokeAmount, UndelegatedState, Opts).

%% @doc Return the unclaimed yield for a specific address in a specific resource.
unclaimed_yield(Addr, ResourceID, UndrippedS, Opts) ->
    GlobalDrippedS = drip_global(UndrippedS, Opts),
    ?event(debug_pot,
        {unclaimed_yield,
            {resource_id, ResourceID},
            {global_dripped_state, GlobalDrippedS},
            {undripped_state, UndrippedS}
        },
        Opts
    ),
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

%% @doc Authorize a `register` request for a resource.
%% Returns `true` if the request is permitted by any of the following:
%%  - the parent mint,
%%  - the mint authority,
%%  - or the resource-specific authority.
authorize_register(ResID, State, Req, From, Opts) ->
    (hb_maps:get(<<"parent">>, State, no_parent, Opts) =:= From)
    orelse
    dev_security:validate(<<"mint-authority">>, State, Req, From, Opts)
    orelse
    verify_resource_authority(ResID, State, Req, Opts).

%% @doc Apply a resource weight update if a `weight` field is present
%% in the register request.
maybe_register_weight(ResID, Req, State, Opts) ->
    case hb_maps:find(<<"weight">>, Req, Opts) of
        {ok, Weight} -> register_resource(ResID, Weight, State, Opts);
        _ -> State
    end.

%% @doc Apply a resource authority update if a `resource-authority` field
%% is present in the register request.
maybe_register_authority(ResID, Req, State, Opts) ->
    case hb_maps:find(<<"resource-authority">>, Req, Opts) of
        {ok, ResAuth} ->
            register_resource_authority(ResID, ResAuth, State, Opts);
        _ -> State
    end.

%% @doc Set the weight of a specific resource in the pot, updating the pot state
%% as necessary.
register_resource(ResourceID, Weight, S, Opts) ->
    % Run the global drip to ensure the state is up to date.
    S0 = drip_global(S, Opts),
    S1 = drip_resource(ResourceID, S0, Opts),
    % Calculate the new total deposited units for the weighted global counter
    % (`/total-weighted-units').
    Resource = hb_ao:get(resource_path(ResourceID), S1, #{}, Opts),
    OldWeight = hb_ao:get(<<"weight">>, Resource, 0, Opts),
    ResourceDeposits = hb_ao:get(<<"total-deposits">>, Resource, 0, Opts),
    % Update the total weighted units counter. Subtract the deposits at the old
    % weight first, then add the deposits at the new weight.
    LastTWU = hb_maps:get(<<"total-weighted-units">>, S1, 0, Opts),
    NewTotalWeightedUnits =
        LastTWU - (OldWeight * ResourceDeposits) + (Weight * ResourceDeposits),
    % Update the resource and the global weighted units counter.
    AfterSet =
        hb_ao:set(
            S1,
            #{
                <<"resources">> => #{
                    ResourceID => Resource#{ <<"weight">> => Weight }
                },
                <<"total-weighted-units">> => NewTotalWeightedUnits
            },
            Opts
        ),
    send_weight_notice(ResourceID, Weight, AfterSet, Opts).

%% @doc Update the authority record for a specific resource in the pot.
register_resource_authority(ResourceID, Authority, S, Opts) ->
    hb_ao:set(
        S,
        <<"/resources/", ResourceID/binary, "/authority">>,
        Authority,
        Opts
    ).

%% @doc Verify a request against the authority for a specific resource.
verify_resource_authority(ResourceID, Base, Req, Opts) ->
    maybe
        {ok, From} ?=
            hb_maps:find(
                <<"from">>,
                Req,
                <<"No `from' address provided.">>,
                Opts
            ),
        {ok, Resources} =
            hb_maps:find(
                <<"resources">>,
                Base,
                <<"No resources found in mint state.">>,
                Opts
            ),
        {ok, Resource} = 
            hb_maps:find(
                ResourceID,
                Resources,
                <<"Requested resource not initialized in mint state.">>,
                Opts
            ),
        true ?= dev_security:validate(<<"authority">>, Resource, Req, From, Opts)
    end.

%% @doc Drip the global state, a specific resource, and all listed users.
%% Used to ensure delegation participants are fully up to date.
drip_all(ResourceID, Addrs, S, Opts) ->
    ResourceDripped = drip_resource(ResourceID, drip_global(S, Opts), Opts),
    lists:foldl(
        fun(A, Acc) -> drip_user(A, Acc, Opts) end, 
        ResourceDripped, 
        Addrs
    ).

%% @doc Settle unclaimed yields for From and To for a resource.
settle_yields(From, To, ResourceID, S, Opts) ->
    Balances = hb_ao:get(<<"balances">>, S, #{}, Opts),
    FromBal = hb_ao:get(From, Balances, 0, Opts),
    ToBal = hb_ao:get(To, Balances, 0, Opts),
    FromYield = unclaimed_yield(From, ResourceID, S, Opts),
    ToYield = unclaimed_yield(To, ResourceID, S, Opts),
    {ok, NewState} = 
        hb_ao:resolve(
            S, 
            #{ 
                <<"path">> => <<"set">>,
                From => FromBal + FromYield,
                To => ToBal + ToYield 
            }, 
            Opts
        ),
    NewState.

%% @doc Apply a delegation from one user to another for a given resource.
apply_delegation(From, To, ResourceID, Amount, S, Opts) ->
    S1 = update_deposit(From, ResourceID, -Amount, S, Opts),
    S2 = update_delegation(From, To, ResourceID, Amount, S1, Opts),
    S3 = update_deposit(To, ResourceID, Amount, S2, Opts),
    update_last_accumulator(To, ResourceID, S3, Opts).

%% @doc Apply an undelegation from one user to another.
apply_undelegation(From, To, ResourceID, Amount, S, Opts) ->
    S1 = update_deposit(To, ResourceID, -Amount, S, Opts),
    S2 = update_delegation(From, To, ResourceID, -Amount, S1, Opts),
    update_deposit(From, ResourceID, Amount, S2, Opts).

%% @doc Update a deposit quantity using path helpers.
update_deposit(Addr, ResourceID, Delta, S, Opts) ->
    Qty = hb_ao:get(deposit_qty_path(ResourceID, Addr), S, 0, Opts),
    hb_ao:set(S, deposit_qty_path(ResourceID, Addr), Qty + Delta, Opts).

%% @doc Update a delegation quantity using path helpers.
update_delegation(From, To, ResourceID, Delta, S, Opts) ->
    Curr = hb_ao:get(delegation_qty_path(ResourceID, From, To), S, 0, Opts),
    hb_ao:set(S, delegation_qty_path(ResourceID, From, To), Curr + Delta, Opts).

%% @doc Update the last resource accumulator for a user.
update_last_accumulator(Addr, ResourceID, S, Opts) ->
    Acc = hb_ao:get(resource_acc_path(ResourceID), S, 0, Opts),
    hb_ao:set(S, last_resource_acc_path(ResourceID, Addr), Acc, Opts).

%%% Sending Notices.

%% @doc Send a `Action: Deposit | Withdraw` notice to a user whose deposit has
%% been modified.
send_delegation_notice(FromAddr, ToAddr, ResourceID, Amount, S, Opts) ->
    dev_process_outbox:send(
        #{
            <<"target">> => ToAddr,
            <<"action">> =>
                if Amount > 0 -> <<"deposit">>;
                true -> <<"withdraw">>
                end,
            <<"address">> => FromAddr,
            <<"quantity">> => Amount,
            <<"resource">> => ResourceID
        },
        S,
        Opts
    ).

%% @doc Send a `set-weight' update message to all subscribed listeners. We use
%% `notify/3' instead of `send/3' to do this as (by default) there is nobody that
%% will be listening for this message. Clients can call `subscribe' with a
%% `subscribe-target' of `broadcast' and an `subscribe-action' of `set-weight'
%% to be notified of these events.
send_weight_notice(ResourceID, Weight, S, Opts) ->
    dev_process_outbox:notify(
        #{
            <<"action">> => <<"register">>,
            <<"resource">> => ResourceID,
            <<"weight">> => Weight
        },
        S,
        Opts
    ).

%%%% Request Parsers

%% @doc Parse a request to modify a deposit and verify that it originates from
%% the valid resource authority. Returns `{ok, {Address, ResourceID, Amount}}'
%% if the request is valid, otherwise returns `{error, Reason}'.
parse_deposit_modification(Base, Assignment, Opts) ->
    maybe
        Req = hb_ao:get(<<"body">>, Assignment, Opts),
        {ok, Address} ?=
            hb_maps:find(
                <<"address">>,
                Req,
                <<"No `address' provided.">>,
                Opts
            ),
        {ok, ResourceID} ?=
            hb_maps:find(
                <<"resource">>,
                Req,
                <<"No resource ID provided.">>,
                Opts
            ),
        {ok, Amount} ?=
            hb_maps:find(
                <<"quantity">>,
                Req,
                <<"No `quantity' provided.">>,
                Opts
            ),
        true ?= verify_resource_authority(ResourceID, Base, Req, Opts),
        {ok, {Address, ResourceID, Amount}}
    end.

%% @doc Parse a delegation assignement and extract the delegator, recipient,
%% resource identifier, and delegated quantity.
parse_delegation_req(Assignment, Opts) ->
    maybe
        Req = hb_ao:get(<<"body">>, Assignment, Opts),
        {ok, From} ?=
            hb_maps:find(<<"from">>, Req, <<"No `from' provided">>, Opts),
        {ok, To} ?=
            hb_maps:find(<<"address">>, Req, <<"No `address' provided">>, Opts),
        {ok, Resource} ?=
            hb_maps:find(
                <<"resource">>, 
                Req, 
                <<"No `resource' provided">>, 
                Opts
            ),
        {ok, Amt} ?=
            hb_maps:find(
                <<"quantity">>, 
                Req, 
                <<"No `quantity' provided">>, 
                Opts
            ),
        true ?= Amt > 0 orelse {error, invalid_amount},
        {ok, {From, To, Resource, Amt}}
    end.

%% @doc Parse a register assignemnt and extract the request body, resource
%% identifier, and sender address.
parse_register_req(Assignment, Opts) ->
    maybe
        Req = hb_ao:get(<<"body">>, Assignment, Opts),
        {ok, ResID} ?=
            hb_maps:find(
                <<"resource">>,
                Req,
                <<"No `resource' provided to register.">>,
                Opts
            ),
        {ok, From} ?=
            hb_maps:find(
                <<"from">>,
                Req,
                <<"No `from' address provided.">>,
                Opts
            ),
        {ok, {Req, ResID, From}}
    end.

%% @doc Parse a notification assignment and extract the notifier, action,
%% original sender, and forwarded message body.
parse_notification_req(Assignment, Opts) ->
    maybe
        {ok, Req} ?=
            hb_maps:find(
                <<"body">>,
                Assignment,
                <<"Notification is not an assignment.">>,
                Opts
            ),
        {ok, NotifyFrom} ?=
            hb_maps:find(
                <<"from">>,
                Req,
                <<"No `from' address provided.">>,
                Opts
            ),
        ForwardedMsg = dev_process_outbox:original_from_forwarded(Req, Opts),
        {ok, Action} ?= hb_maps:find(<<"action">>, ForwardedMsg, Opts),
        OriginalFrom = 
            hb_maps:get(<<"from">>, ForwardedMsg, <<"unknown">>, Opts),
        {ok, {NotifyFrom, Action, OriginalFrom, ForwardedMsg}}
    end.

%%% Path Generators.

resource_path(ResID) ->
    <<"/resources/", ResID/binary>>.

resource_deposit_path(ResID, Addr) ->
    <<"/resources/", ResID/binary, "/deposits/", Addr/binary>>.

resource_weight_path(ResID) ->
    <<"/resources/", ResID/binary, "/weight">>.

resource_total_deposits_path(ResID) ->
    <<"/resources/", ResID/binary, "/total-deposits">>.

resource_acc_path(ResID) ->
    <<"/resources/", ResID/binary, "/accumulator">>.

deposit_qty_path(ResID, Addr) ->
    <<(resource_deposit_path(ResID, Addr))/binary, "/quantity">>.

last_resource_acc_path(ResID, Addr) ->
    <<(resource_deposit_path(ResID, Addr))/binary, "/last-resource-accumulator">>.

delegations_path(ResID, Addr) ->
    <<(resource_deposit_path(ResID, Addr))/binary, "/delegations">>.

delegation_qty_path(ResID, From, To) ->
    <<(resource_deposit_path(ResID, From))/binary, "/delegations/", To/binary>>.

%% @doc Return the contents of the inverted index for a specific address.
user(Addr, S, Opts) ->
    hb_ao:get(<<"/users/", Addr/binary>>, S, #{}, Opts).

%% @doc Get the balances submessage from the state.
balances(S) -> balances(S, #{}).
balances(S = #{ <<"balances">> := Bs }, Opts) ->
    hb_maps:map(fun(Addr, _) -> balance(Addr, S, Opts) end, hb_private:reset(Bs)).

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