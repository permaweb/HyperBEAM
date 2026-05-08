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
%%% Authority model:
%%% - `deposit` / `withdraw` are authorized per resource by that resource's
%%%   `authority` security policy.
%%% - `weight` updates may be delegated per resource via `weight-authority`.
%%% - resource config changes (`authority*`, `weight-authority*`) remain
%%%   guarded by the pot-wide `mint-authority` policy, or by the configured
%%%   `parent` when the pot is acting as a child mint.
%%% - child mints trust their direct `parent` process for inherited resource
%%%   config and writes; forwarded parent `register` notifications set both
%%%   `resource-authority` and `weight-authority` to that parent process.
%%%
%%% Quantity model:
%%% - `deposit` / `withdraw` request bodies may provide `quantity-scale` to
%%%   normalize source-token raw units into pot units.
%%% - direct `deposit/5` and `withdraw/5` calls already expect normalized pot
%%%   units.
%%% - `delegate` / `undelegate` always move existing normalized pot units and do
%%%   not apply `quantity-scale`.
%%% 
%%% TODO: Add `secure-set` (set guarded by address) for resource-scoped config.
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
-export([get_deposit/4, get_deposits/2, get_deposits/3]).
%%% `~pot@1.0` dev_token:validate_address custom denylist
-define(RESERVED_KEYS, [
    <<"balances">>,
    <<"resources">>,
    <<"users">>,
    <<"total-supply">>,
    <<"minted">>,
    <<"accumulator">>,
    <<"last-drip">>,
    <<"t">>,
    <<"total-weighted-units">>,
    <<"undistributed-mint">>,
    <<"accumulator-remainder">>
]).
-define(POT_QUANTITY_SCALE, 1_000_000_000_000_000_000). % 1e18

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
    RawT = hb_ao:get(<<"t">>, S, 0, Opts),
    AlreadyMinted = hb_maps:get(<<"minted">>, S, 0, Opts),
    LastT = hb_maps:get(<<"last-drip">>, S, 0, Opts),
    T = max(RawT, LastT),
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
    AccumulatorRemainder =
        hb_maps:get(<<"accumulator-remainder">>, S, 0, Opts),
    ?event(debug_test,
        {drip_global,
            {t, T},
            {last_drip, LastT},
            {minted, AlreadyMinted},
            {undistributed_mint, UndistributedMint},
            {accumulator_remainder, AccumulatorRemainder},
            {total_weighted_units, TotalWeightedUnits},
            {global_accumulator, GlobalAcc},
            {to_mint, ToMint}
        }, Opts),
    {NewGlobalAcc, NewUndistributedMint, NewAccumulatorRemainder} =
        dev_pot_math:drip_global(
            GlobalAcc,
            ToMint + UndistributedMint,
            AccumulatorRemainder,
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
            <<"undistributed-mint">> => NewUndistributedMint,
            <<"accumulator-remainder">> => NewAccumulatorRemainder
        },
        Opts
    ).

%% @doc Drip the state of a specific resource in the pot. Does not drip the
%% global state before doing so.
drip_resource(ResourceID, S, Opts) ->
    ?event(debug_pot, {drip_resource, {resource_id, ResourceID}, {state, S}}, Opts),
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

%% @doc Get the balance of a specific address in the pot by combining the base
%% balance with the unclaimed yield.
balance(Addr, S, Opts) ->
    maybe
        true ?= dev_token:validate_address(Addr, ?RESERVED_KEYS),
        hb_maps:get(Addr, hb_maps:get(<<"balances">>, S, #{}, Opts), 0, Opts)
            + unclaimed_yield(Addr, S, Opts)
    end.

%% @doc Return the unclaimed yield across all resources for a specific address.
unclaimed_yield(Addr, S, Opts) ->
    maybe
        true ?= dev_token:validate_address(Addr, ?RESERVED_KEYS),
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
        )
end.

%% @doc Return the unclaimed yield for a specific address in a specific resource.
unclaimed_yield(Addr, ResourceID, UndrippedS, Opts) ->
    maybe
        true ?= dev_token:validate_address(ResourceID, ?RESERVED_KEYS),
        true ?= dev_token:validate_address(Addr, ?RESERVED_KEYS),
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
            end
    end.

%% @doc Deposit a quantity of a resource for a given address.
-spec deposit(map(), map(), map()) -> map() | {error, term()}.
deposit(State, Assignment, Opts) ->
    maybe
        {ok, {Address, ResourceID, Amount}} ?=
            parse_deposit_modification(State, Assignment, Opts),
        StateWithT = ensure_initialized(State, Assignment, Opts),
        deposit(Address, ResourceID, Amount, StateWithT, Opts)
    else
        Reason -> {error, Reason}
    end.
-spec deposit(binary(), binary(), pos_integer(), map(), map()) -> map() | {error, term()}.
deposit(Addr, ResourceID, Amount, S0, Opts) when is_integer(Amount), Amount > 0 ->
    modify_deposit_state(Addr, ResourceID, Amount, S0, Opts);
deposit(_, _, Amount, _, _) when is_integer(Amount), Amount < 0 ->
    {error, <<"Deposit amount must be positive Integer.">>};
deposit(_, _, Amount, _, _) when is_integer(Amount), Amount =:= 0 ->
    {error, <<"Deposit amount must be a non-zero positive Integer.">>};
deposit(_, _, Amount, _, _) when not is_integer(Amount) ->
    {error, <<"Deposit amount must be Integer.">>}.

%% @doc Withdraw a quantity of a resource for a given address. If the quantity
%% is insufficient, we'll revoke delegations until the withdrawal can be completed.
-spec withdraw(map(), map(), map()) -> map() | {error, term()}.
withdraw(Base, Req, Opts) ->
    maybe
        {ok, {Address, ResourceID, Amount}} ?=
            parse_deposit_modification(Base, Req, Opts),
        BaseWithT = ensure_initialized(Base, Req, Opts),
        withdraw(Address, ResourceID, Amount, BaseWithT, Opts)
    end.
-spec withdraw(binary(), binary(), pos_integer(), map(), map()) -> map() | {error, term()}.
withdraw(Addr, ResourceID, Amount, S0, Opts) when is_integer(Amount), Amount > 0 ->
    maybe
        ExistingDepositOrError = get_deposit(Addr, ResourceID, S0, Opts),
        true ?= is_integer(ExistingDepositOrError) orelse ExistingDepositOrError,
        ExistingDeposit = ExistingDepositOrError,
        {ok, S1} ?= liquidate(Addr, ResourceID, Amount - ExistingDeposit, S0, Opts),
        modify_deposit_state(Addr, ResourceID, -Amount, S1, Opts)
    else
        {error, _} = WithdrawErr -> WithdrawErr
    end;
withdraw(_, _, Amount, _, _) when is_integer(Amount), Amount < 0 ->
    {error,  <<"Withdraw amount must be a positive Integer.">>};
withdraw(_, _, Amount, _, _) when is_integer(Amount), Amount =:= 0 ->
    {error, <<"Withdraw amount must be a non-zero positive Integer.">>};
withdraw(_, _, Amount, _, _) when not is_integer(Amount) ->
    {error, <<"Withdraw amount must be an Integer.">>}.

%% @doc Parse a request to modify a deposit and verify that it originates from
%% the valid resource authority. Returns `{ok, {Address, ResourceID,
%% NormalizedAmount}}' if the request is valid, otherwise returns `{error,
%% Reason}'. `quantity-scale` is optional; when omitted, `quantity` is already
%% interpreted as normalized pot units (POT_QUANTITY_SCALE).
parse_deposit_modification(Base, Assignment, Opts) ->
    Req = hb_ao:get(<<"body">>, Assignment, Opts),
    maybe
        {ok, Address} ?=
            hb_maps:find(
                <<"address">>,
                Req,
                <<"No `address' provided.">>,
                Opts
            ),
        true ?= dev_token:validate_address(Address, ?RESERVED_KEYS),
        {ok, ResourceID} ?=
            hb_maps:find(
                <<"resource">>,
                Req,
                <<"No resource ID provided.">>,
                Opts
            ),
        true ?= dev_token:validate_address(ResourceID, ?RESERVED_KEYS),
        {ok, Amount} ?=
            hb_maps:find(
                <<"quantity">>,
                Req,
                <<"No `quantity' provided.">>,
                Opts
            ),
        true ?= verify_resource_authority(ResourceID, Base, Req, Opts),
        QuantityScale =
            hb_maps:get(
                <<"quantity-scale">>,
                Req,
                ?POT_QUANTITY_SCALE,
                Opts
            ),
        {ok, NormalizedAmount} ?= normalize_quantity(Amount, QuantityScale),
        {ok, {Address, ResourceID, NormalizedAmount}}
    end.

%% @doc Verify a request against the resource-local `authority` policy for a
%% specific resource. In prod mode, requests fail closed if the resource has no
%% explicit authority policy configured.
verify_resource_authority(ResourceID, Base, Req, Opts) ->
    maybe
        {ok, From} ?=
            hb_maps:find(
                <<"from">>,
                Req,
                <<"No `from' address provided.">>,
                Opts
            ),
        {ok, Resources} ?=
            hb_maps:find(
                <<"resources">>,
                Base,
                <<"No resources found in mint state.">>,
                Opts
            ),
        {ok, Resource} ?= 
            hb_maps:find(
                ResourceID,
                Resources,
                <<"Requested resource not initialized in mint state.">>,
                Opts
            ),
        true ?=
            dev_security:validate(
                <<"authority">>,
                Resource,
                Req,
                From,
                Opts#{ dev_security_mode => prod }
            )
    end.

%% @doc Interpret forwarded `register' notifications from the configured
%% `parent' mint process. Notifications from any other sender, or notifications
%% forwarding a non-`register' action, are rejected. Child mints trust their
%% direct parent for inherited resource config, so forwarded notifications set
%% both `resource-authority' and `weight-authority' to the parent process ID.
notify(State, Assignment, Opts) ->
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
        {ok, Parent} ?=
            hb_maps:find(
                <<"parent">>,
                State,
                <<"No `parent` configured for notifications">>,
                Opts
            ),
        true ?= dev_token:validate_address(NotifyFrom, ?RESERVED_KEYS),
        true ?= (NotifyFrom =:= Parent) orelse
            {error, <<"Invalid notification source">>},
        ForwardedMsg = dev_process_outbox:original_from_forwarded(Req, Opts),
        {ok, Action} ?=
            hb_maps:find(
                <<"action">>,
                ForwardedMsg,
                Opts
            ),
        true ?= (Action =:= <<"register">>) orelse
            {error, <<"Unsupported notification action">>},
        OriginalFrom = hb_maps:get(<<"from">>, ForwardedMsg, <<"unknown">>, Opts),
        dev_token:handle_action(
            Action,
            State,
            #{
                <<"type">> => <<"notification">>,
                <<"original-from">> => OriginalFrom,
                <<"body">> =>
                    ForwardedMsg#{
                        <<"from">> => NotifyFrom,
                        <<"resource-authority">> => NotifyFrom,
                        <<"weight-authority">> => NotifyFrom
                    }
            },
            Opts)
    end.

%% @doc For a given address, undelegate their delegations until the specified
%% quantity has been reclaimed.
-spec liquidate(binary(), binary(), integer(), map(), map()) -> {ok, map()} | {error, term()}.
liquidate(_Addr, _ResourceID, Amount, S, _Opts) when Amount =< 0 -> {ok, S};
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
    DelegationPairs =
        lists:filter(
            fun({ToAddr, Qty}) ->
                is_binary(ToAddr) andalso is_integer(Qty) andalso Qty > 0
            end,
            hb_maps:to_list(hb_private:reset(ExistingDelegations))
        ),
    ExistingDelegationsVals = lists:map(fun({_ToAddr, Qty}) -> Qty end, DelegationPairs),
    case ExistingDelegationsVals of
        [] -> {error, <<"Insufficient delegated balance to liquidate.">>};
        _ -> LargestDelegation = lists:max(ExistingDelegationsVals),
            {LargestDelegationAddr, _} =
            lists:keyfind(
                LargestDelegation,
                2,
                DelegationPairs
            ),
            RevokeAmount = min(Amount, LargestDelegation),
            case undelegate(Addr, LargestDelegationAddr, ResourceID, RevokeAmount, S, Opts) of
                {error, _} = Err -> Err;
                {ok, S0} -> liquidate(Addr, ResourceID, Amount - RevokeAmount, S0, Opts)
            end
    end.

%% @doc Delegate normalized pot units of a resource from one address to another.
%% `quantity-scale` is only interpreted by deposit/withdraw request parsing.
-spec delegate(map(), map(), map()) -> {ok, map()} | {error, term()}.
delegate(State, Assignment, Opts) ->
    Req = hb_ao:get(<<"body">>, Assignment, Opts),
    maybe
        {ok, FromAddr} ?=
            hb_maps:find(
                <<"from">>,
                Req,
                <<"No `from' address provided.">>,
                Opts
            ),
        true ?= dev_token:validate_address(FromAddr, ?RESERVED_KEYS),
        {ok, ToAddr} ?=
            hb_maps:find(
                <<"address">>,
                Req,
                <<"No recipient `address' to delegate to provided.">>,
                Opts
            ),
        true ?= dev_token:validate_address(ToAddr, ?RESERVED_KEYS),
        {ok, ResourceID} ?=
            hb_maps:find(
                <<"resource">>,
                Req,
                <<"No `resource' ID to delegate on provided.">>,
                Opts
            ),
        true ?= dev_token:validate_address(ResourceID, ?RESERVED_KEYS),
        {ok, Amount} ?=
            hb_maps:find(
                <<"quantity">>,
                Req,
                <<"No `quantity' to delegate provided.">>,
                Opts
            ),
        StateWithT = ensure_initialized(State, Assignment, Opts),
        {ok, NewState} ?=
            delegate(FromAddr, ToAddr, ResourceID, Amount, StateWithT, Opts),
        {ok, NewState}
    else
        {error, _} = Err -> Err;
        Reason -> {error, Reason}
    end.
-spec delegate(binary(), binary(), binary(), pos_integer(), map(), map()) -> {ok, map()} | {error, term()}.
delegate(FromAddr, ToAddr, ResourceID, Amount, S, Opts) when is_integer(Amount), Amount > 0 ->
    maybe
        true ?= dev_token:validate_address(FromAddr, ?RESERVED_KEYS),
        true ?= dev_token:validate_address(ToAddr, ?RESERVED_KEYS),
        true ?= dev_token:validate_address(ResourceID, ?RESERVED_KEYS),
        ?event(
            {delegating,
                {from_addr, FromAddr},
                {to_addr, ToAddr},
                {resource_id, ResourceID},
                {amount, Amount}
            }
        ),
        GlobalDrippedS = drip_global(S, Opts),
        S0 = drip_resource(ResourceID, GlobalDrippedS, Opts),
        S1 = drip_user(FromAddr, S0, Opts),
        S2 = drip_user(ToAddr, S1, Opts),
        % Self-delegation is a noop
        case FromAddr =:= ToAddr of
            true -> {ok, S2};
            false ->
                DelegatorDeposit = get_deposit(FromAddr, ResourceID, S2, Opts),
                case DelegatorDeposit >= Amount of
                    false ->
                        {error, <<"Delegation amount exceeds available deposit.">>};
                    true ->
                        S3 =
                            hb_ao:set(
                                S2,
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
                                S3,
                                0,
                                Opts
                            ),
                        S4 =
                            hb_ao:set(
                                S3,
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
                        ResourceAcc =
                            hb_ao:get(
                                <<"resources/", ResourceID/binary, "/accumulator">>,
                                S4,
                                0,
                                Opts
                            ),
                        RecipientDeposit = get_deposit(ToAddr, ResourceID, S4, Opts),
                        S5 =
                            hb_ao:set(
                                S4,
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
                        S6 =
                            hb_ao:set(
                                S5,
                                <<
                                    "/resources/", 
                                    ResourceID/binary,
                                    "/deposits/",
                                    ToAddr/binary,
                                    "/last-resource-accumulator"
                                >>,
                                ResourceAcc,
                                Opts
                            ),
                        S7 =
                            update_deposit_index(
                                FromAddr,
                                ResourceID,
                                get_deposit(FromAddr, ResourceID, S6, Opts),
                                S6,
                                Opts
                            ),
                        S8 =
                            update_deposit_index(
                                ToAddr,
                                ResourceID,
                                get_deposit(ToAddr, ResourceID, S7, Opts),
                                S7,
                                Opts
                            ),
                        {ok,
                            send_delegation_notice(
                                FromAddr,
                                ToAddr,
                                ResourceID,
                                Amount,
                                S8,
                                Opts
                            )}
                end
        end
end;
delegate(_, _, _, Amount, _, _) when is_integer(Amount), Amount < 0 ->
    {error, <<"Delegate Amount must be a non-negative integer">>};
delegate(_, _, _, Amount, _, _) when is_integer(Amount), Amount =:= 0 ->
    {error, <<"Delegate amount must be a non-zero positive Integer.">>};
delegate(_, _, _, Amount, _, _) when not is_integer(Amount)->
    {error, <<"Delegate Amount must be of integer type">>}.

%% @doc Undelegate normalized pot units of a resource from one address to another.
%% `quantity-scale` is only interpreted by deposit/withdraw request parsing.
-spec undelegate(map(), map(), map()) -> {ok, map()} | {error, term()}.
undelegate(State, Assignment, Opts) ->
    Req = hb_ao:get(<<"body">>, Assignment, Opts),
    maybe
        {ok, FromAddr} ?= hb_maps:find(<<"from">>, Req, Opts),
        {ok, ToAddr} ?= hb_maps:find(<<"address">>, Req, Opts),
        {ok, ResourceID} ?= hb_maps:find(<<"resource">>, Req, Opts),
        {ok, Amount} ?= hb_maps:find(<<"quantity">>, Req, Opts),
        true ?= dev_token:validate_address(FromAddr, ?RESERVED_KEYS),
        true ?= dev_token:validate_address(ToAddr, ?RESERVED_KEYS),
        true ?= dev_token:validate_address(ResourceID, ?RESERVED_KEYS),
        StateWithT = ensure_initialized(State, Assignment, Opts),
        {ok, NewState} ?=
            undelegate(FromAddr, ToAddr, ResourceID, Amount, StateWithT, Opts),
        {ok, NewState}
    else
        {error, _} = Err -> Err;
        Reason -> {error, Reason}
    end.
-spec undelegate(binary(), binary(), binary(), pos_integer(), map(), map()) -> {ok, map()} | {error, term()}.
undelegate(FromAddr, ToAddr, ResourceID, Amount, S, Opts) when is_integer(Amount), Amount > 0 ->
    maybe
        true ?= dev_token:validate_address(FromAddr, ?RESERVED_KEYS),
        true ?= dev_token:validate_address(ToAddr, ?RESERVED_KEYS),
        true ?= dev_token:validate_address(ResourceID, ?RESERVED_KEYS),
        ExistingDelegationBefore =
            case FromAddr =:= ToAddr of
                true -> 0;
                false ->
                    hb_ao:get(
                        <<
                            "/resources/",
                            ResourceID/binary,
                            "/deposits/",
                            FromAddr/binary,
                            "/delegations/",
                            ToAddr/binary
                        >>,
                        S,
                        0,
                        Opts
                    )
            end,
        Validation =
            case FromAddr =:= ToAddr of
                true -> ok;
                false ->
                    case ExistingDelegationBefore >= Amount of
                        true -> ok;
                        false ->
                            {error, <<"Undelegation amount exceeds existing delegation.">>}
                    end
            end,
        case Validation of
            {error, _} = Err -> Err;
            ok ->
                case ensure_undelegation_cover(
                    FromAddr,
                    ToAddr,
                    ResourceID,
                    Amount,
                    S,
                    Opts
                ) of
                    {error, _} = Err -> Err;
                    {ok, Prepared} ->
                        GlobalDrippedS = drip_global(Prepared, Opts),
                        DrippedS = drip_resource(ResourceID, GlobalDrippedS, Opts),
                        S0 = drip_user(FromAddr, DrippedS, Opts),
                        S1 = drip_user(ToAddr, S0, Opts),
                        % Self-undelegation is a noop
                        case FromAddr =:= ToAddr of
                            true -> {ok, S1};
                            false ->
                                NewRecipientDeposit = get_deposit(ToAddr, ResourceID, S1, Opts),
                                S2 =
                                    hb_ao:set(
                                        S1,
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
                                S3 =
                                    hb_ao:set(
                                        S2,
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
                                        S1,
                                        0,
                                        Opts
                                    ),
                                S4 =
                                    hb_ao:set(
                                        S3,
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
                                S5 =
                                    update_deposit_index(
                                        FromAddr,
                                        ResourceID,
                                        get_deposit(FromAddr, ResourceID, S3, Opts),
                                        S4,
                                        Opts
                                    ),
                                S6 =
                                    update_deposit_index(
                                        ToAddr,
                                        ResourceID,
                                        get_deposit(ToAddr, ResourceID, S4, Opts),
                                        S5,
                                        Opts
                                    ),
                                {ok,
                                    send_delegation_notice(
                                        FromAddr,
                                        ToAddr,
                                        ResourceID,
                                        -Amount,
                                        S6,
                                        Opts
                                    )}
                        end
                end
        end
end;
undelegate(_, _, _, Amount, _, _) when is_integer(Amount), Amount < 0 ->
    {error, <<"Undelegate Amount must be a non-negative integer">>};
undelegate(_, _, _, Amount, _, _) when is_integer(Amount), Amount =:= 0 ->
    {error, <<"Undelegate amount must be a non-zero positive Integer.">>};
undelegate(_, _, _, Amount, _, _) when not is_integer(Amount)->
    {error, <<"Undelegate Amount must be of integer type">>}.

%% @doc Repeatedly liquidate the recipient until the requested undelegation is
%% directly coverable, or fail if the outer delegation edge has already been
%% consumed by recursive unwind.
ensure_undelegation_cover(FromAddr, ToAddr, ResourceID, Amount, S, Opts) ->
    ExistingDelegation =
        case FromAddr =:= ToAddr of
            true -> 0;
            false ->
                hb_ao:get(
                    <<
                        "/resources/",
                        ResourceID/binary,
                        "/deposits/",
                        FromAddr/binary,
                        "/delegations/",
                        ToAddr/binary
                    >>,
                    S,
                    0,
                    Opts
                )
        end,
    RecipientDeposit = get_deposit(ToAddr, ResourceID, S, Opts),
    case FromAddr =:= ToAddr of
        true ->
            {ok, S};
        false when ExistingDelegation < Amount ->
            {error, <<"Undelegation amount exceeds existing delegation.">>};
        false when RecipientDeposit >= Amount ->
            {ok, S};
        false ->
            case liquidate(ToAddr, ResourceID, Amount - RecipientDeposit, S, Opts) of
                {error, _} = Err ->
                    Err;
                {ok, Liquidated} ->
                    ensure_undelegation_cover(
                        FromAddr,
                        ToAddr,
                        ResourceID,
                        Amount,
                        Liquidated,
                        Opts
                    )
            end
    end.

%% @doc Resource configuration entrypoint. Validates the target resource and
%% caller, then applies supported resource-scoped config mutations.
%% 
%% Authorization model:
%% - `weight`-only updates may be performed by:
%%   - the configured `parent`
%%   - the pot-wide `mint-authority`
%%   - the resource-local `weight-authority`
%% - updates touching `resource-authority*` or `weight-authority*` are treated
%%   as admin mutations and may only be performed by:
%%   - the configured `parent`
%%   - the pot-wide `mint-authority`
register(State, Assignment, Opts) ->
    ?event(debug_pot, {register, Assignment}, Opts),
    maybe
        Req = hb_ao:get(<<"body">>, Assignment,Opts),
        {ok, ResID} ?= 
            hb_maps:find(
                <<"resource">>, 
                Req,
                <<"No `resource' provided to register.">>, 
                Opts
            ),
        true ?= dev_token:validate_address(ResID, ?RESERVED_KEYS),
        {ok, From} ?= 
            hb_maps:find(
                <<"from">>, 
                Req,
                <<"No `from' address provided.">>, 
                Opts
            ),
        true ?= validate_signer_config(From, Opts),
        HasAdminMutation =
            hb_maps:find(<<"resource-authority">>, Req, Opts) =/= error orelse
            hb_maps:find(<<"resource-authority-required">>, Req, Opts) =/= error orelse
            hb_maps:find(<<"resource-authority-match">>, Req, Opts) =/= error orelse
            hb_maps:find(<<"weight-authority">>, Req, Opts) =/= error orelse
            hb_maps:find(<<"weight-authority-required">>, Req, Opts) =/= error orelse
            hb_maps:find(<<"weight-authority-match">>, Req, Opts) =/= error,
        true ?=
            case {HasAdminMutation, hb_maps:find(<<"weight">>, Req, Opts)} of
                {true, _} ->
                    enforce_resource_config_authority(From, State, Req, Opts);
                {false, {ok, _}} ->
                    enforce_resource_weight_authority(ResID, From, State, Req, Opts);
                _ ->
                    true
            end,
        State2 =
            case hb_maps:find(<<"weight">>, Req, Opts) of
                {ok, Weight} -> register_resource(ResID, Weight, State, Opts);
                _ -> State
            end,
        true ?= is_map(State2) orelse State2,
        State3 =
            case hb_maps:find(<<"weight-authority">>, Req, Opts) of
                {ok, WeightAuth} ->
                    register_resource_weight_authority(
                        ResID,
                        WeightAuth,
                        State2,
                        Opts
                    );
                _ -> State2
            end,
        true ?= is_map(State3) orelse State3,
        State4 =
            case hb_maps:find(<<"weight-authority-required">>, Req, Opts) of
                {ok, WeightRequired} ->
                    register_resource_weight_authority_required(
                        ResID,
                        WeightRequired,
                        State3,
                        Opts
                    );
                _ -> State3
            end,
        true ?= is_map(State4) orelse State4,
        State5 =
            case hb_maps:find(<<"weight-authority-match">>, Req, Opts) of
                {ok, WeightMatch} ->
                    register_resource_weight_authority_match(
                        ResID,
                        WeightMatch,
                        State4,
                        Opts
                    );
                _ -> State4
            end,
        true ?= is_map(State5) orelse State5,
        State6 =
            case hb_maps:find(<<"resource-authority">>, Req, Opts) of
                {ok, ResAuth} ->
                    register_resource_authority(ResID, ResAuth, State5, Opts);
                _ -> State5
            end,
        true ?= is_map(State6) orelse State6,
        State7 =
            case hb_maps:find(<<"resource-authority-required">>, Req, Opts) of
                {ok, ResRequired} ->
                    register_resource_authority_required(
                        ResID,
                        ResRequired,
                        State6,
                        Opts
                    );
                _ -> State6
            end,
        true ?= is_map(State7) orelse State7,
        case hb_maps:find(<<"resource-authority-match">>, Req, Opts) of
            {ok, ResMatch} ->
                register_resource_authority_match(ResID, ResMatch, State7, Opts);
            _ -> State7
        end
    end.
%% @doc Enforce authorization for resource configuration updates.
%% Authorization is evaluated against:
%% - `State/parent`, if set and equal to `from`
%% - `mint-authority` security policy on the pot state, if configured
%% Resource-level write or weight authorities do not grant config authority.
enforce_resource_config_authority(From, State, Req, Opts) ->
    ?event(debug_pot, {enforce_resource_config_authority, Req}, Opts),
    maybe
        AuthRes = case (hb_maps:get(<<"parent">>, State, no_parent, Opts) =:= From) of
            true -> true;
            false -> 
                case dev_security:validate(<<"mint-authority">>, State, Req, From, Opts#{dev_security_mode => prod}) of
                    true -> true;
                    {error, _} ->
                        {error, <<"Caller is not authorized to configure resources.">>}
                end  
        end,
        true ?= AuthRes
    end.
%% @doc Enforce authorization for weight-only updates. Parent may always
%% reconfigure its children, explicit per-resource `weight-authority` can
%% update the weight, and `mint-authority` remains the global override.
enforce_resource_weight_authority(ResourceID, From, State, Req, Opts) ->
    ?event(debug_pot, {enforce_resource_weight_authority, Req}, Opts),
    maybe
        AuthRes =
            case (hb_maps:get(<<"parent">>, State, no_parent, Opts) =:= From) of
                true -> true;
                false ->
                    case verify_weight_authority(ResourceID, State, Req, Opts) of
                        true -> true;
                        {error, _} ->
                            case dev_security:validate(
                                <<"mint-authority">>,
                                State,
                                Req,
                                From,
                                Opts#{ dev_security_mode => prod }
                            ) of
                                true -> true;
                                {error, _} ->
                                    {error, <<"Caller is not authorized to update resource weight.">>}
                            end
                    end
            end,
        true ?= AuthRes
    end.
%% @doc Verify a request against the resource-local `weight-authority` policy
%% for a specific resource. In prod mode, requests fail closed if the resource
%% has no explicit weight-authority policy configured.
verify_weight_authority(ResourceID, Base, Req, Opts) ->
    maybe
        {ok, From} ?=
            hb_maps:find(
                <<"from">>,
                Req,
                <<"No `from' address provided.">>,
                Opts
            ),
        {ok, Resources} ?=
            hb_maps:find(
                <<"resources">>,
                Base,
                <<"No resources found in mint state.">>,
                Opts
            ),
        {ok, Resource} ?=
            hb_maps:find(
                ResourceID,
                Resources,
                <<"Requested resource not initialized in mint state.">>,
                Opts
            ),
        true ?=
            dev_security:validate(
                <<"weight-authority">>,
                Resource,
                Req,
                From,
                Opts#{ dev_security_mode => prod }
            )
    end.
%% @doc Update the authority record for a specific resource in the pot.
register_resource_authority(ResourceID, Authority, S, Opts) ->
    maybe
        true ?= dev_token:validate_address(ResourceID, ?RESERVED_KEYS),
        true ?= validate_signer_config(Authority, Opts),
        hb_ao:set(
            S,
            <<"/resources/", ResourceID/binary, "/authority">>,
            Authority,
            Opts
        )
end.
register_resource_authority_required(ResourceID, Required, S, Opts) ->
    maybe
        true ?= dev_token:validate_address(ResourceID, ?RESERVED_KEYS),
        true ?= validate_signer_config(Required, Opts),
        hb_ao:set(
            S,
            <<"/resources/", ResourceID/binary, "/authority-required">>,
            Required,
            Opts
        )
end.
register_resource_authority_match(ResourceID, Match, S, Opts) ->
    maybe
        true ?= dev_token:validate_address(ResourceID, ?RESERVED_KEYS),
        true ?= validate_match_config(Match),
        hb_ao:set(
            S,
            <<"/resources/", ResourceID/binary, "/authority-match">>,
            Match,
            Opts
        )
end.
%% @doc Update the weight-authority record for a specific resource in the pot.
register_resource_weight_authority(ResourceID, Authority, S, Opts) ->
    maybe
        true ?= dev_token:validate_address(ResourceID, ?RESERVED_KEYS),
        true ?= validate_signer_config(Authority, Opts),
        hb_ao:set(
            S,
            <<"/resources/", ResourceID/binary, "/weight-authority">>,
            Authority,
            Opts
        )
end.
register_resource_weight_authority_required(ResourceID, Required, S, Opts) ->
    maybe
        true ?= dev_token:validate_address(ResourceID, ?RESERVED_KEYS),
        true ?= validate_signer_config(Required, Opts),
        hb_ao:set(
            S,
            <<"/resources/", ResourceID/binary, "/weight-authority-required">>,
            Required,
            Opts
        )
end.
register_resource_weight_authority_match(ResourceID, Match, S, Opts) ->
    maybe
        true ?= dev_token:validate_address(ResourceID, ?RESERVED_KEYS),
        true ?= validate_match_config(Match),
        hb_ao:set(
            S,
            <<"/resources/", ResourceID/binary, "/weight-authority-match">>,
            Match,
            Opts
        )
end.
validate_signer_config(Value, Opts) when is_binary(Value) ->
    try validate_signer_config(hb_util:binary_to_strings(Value), Opts)
    catch
        _:_ -> {error, <<"Signer config must be a valid binary or list.">>}
    end;
validate_signer_config(Value, Opts) when is_list(Value) ->
    case Value of
        [] ->
            {error, <<"Signer config list must not be empty.">>};
        _ ->
            case lists:all(fun is_binary/1, Value) of
                false ->
                    {error, <<"Signer config list must contain binary addresses.">>};
                true ->
                    lists:foldl(
                        fun(Signer, true) ->
                            dev_token:validate_address(Signer, ?RESERVED_KEYS);
                           (_Signer, {error, _} = Err) ->
                            Err
                        end,
                        true,
                        Value
                    )
            end
    end;
validate_signer_config(_, _) ->
    {error, <<"Signer config must be a binary or a list.">>}.
validate_match_config(Match) when is_integer(Match), Match >= 0 -> true;
validate_match_config(Match) when is_binary(Match) ->
    try binary_to_integer(Match) of
        Int when is_integer(Int), Int >= 0 -> true;
        _ -> {error, <<"Match threshold must be a non-negative integer.">>}
    catch
        _:_ -> {error, <<"Match threshold must be a non-negative integer.">>}
    end;
validate_match_config(_) ->
    {error, <<"Match threshold must be a non-negative integer.">>}.

%% @doc Set the weight of a specific resource in the pot, updating the pot state
%% as necessary.
register_resource(ResourceID, Weight, S, Opts) when is_integer(Weight), Weight >= 0 ->
    maybe
        true ?= dev_token:validate_address(ResourceID, ?RESERVED_KEYS),
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
        send_weight_notice(ResourceID, Weight, AfterSet, Opts)
end;

register_resource(_, Weight, _, _) when not is_integer(Weight) ->
    {error, <<"Invalid Weight type.">>};
register_resource(_, Weight, _, _) when is_integer(Weight), Weight < 0 ->
    {error, <<"Weight must be a non-negative integer.">>}.

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
send_delegation_notice(FromAddr, ToAddr, ResourceID, Amount, S, Opts) ->
    dev_process_outbox:send(
        #{
            <<"target">> => ToAddr,
            <<"action">> =>
                if Amount > 0 -> <<"deposit">>;
                true -> <<"withdraw">>
                end,
            <<"address">> => FromAddr,
            <<"quantity">> => abs(Amount),
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

%%% Helpers.

%% @doc Used by deposit() and withdraw() to update the state of the world. Note that
%% the domain of deposit() and withdraw() are the natural numbers, but the domain of
%% modify_deposit_state() includes negative numbers as well.
modify_deposit_state(Addr, ResourceID, Amount, S0, Opts) ->
    % Drip the global state and the resource, then extract necessary components.
    maybe
        true ?= dev_token:validate_address(Addr, ?RESERVED_KEYS),
        true ?= dev_token:validate_address(ResourceID, ?RESERVED_KEYS),
        GlobalDrippedS = drip_global(S0, Opts),
        DrippedS = #{
            <<"balances">> := Balances,
            <<"resources">> := Resources
        } = drip_resource(ResourceID, GlobalDrippedS, Opts),
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
        ExistingDepositOrError = get_deposit(Addr, ResourceID, DrippedS, Opts),
        true ?= is_integer(ExistingDepositOrError) orelse ExistingDepositOrError,
        ExistingDeposit = ExistingDepositOrError,
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
        ?event(debug_drip, {resources_after_modify_deposit, NewResources}, Opts),
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
        )
end.

%% @doc Get the deposit quantity for a specific address in a specific resource.
get_deposit(Addr, ResourceID, S, Opts) ->
    maybe
        true ?= dev_token:validate_address(Addr, ?RESERVED_KEYS),
        true ?= dev_token:validate_address(ResourceID, ?RESERVED_KEYS),
        hb_ao:get(
            <<"/resources/", ResourceID/binary, "/deposits/", Addr/binary, "/quantity">>,
            S,
            0,
            Opts
        )
    end.

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
%% @doc Return deposits for a specific resource, filtering out malformed
%% deposit-address keys from state.
get_deposits(ResourceID, S, Opts) ->
    maybe
        true ?= dev_token:validate_address(ResourceID, ?RESERVED_KEYS),
        Ds = hb_ao:get(
            <<"/resources/", ResourceID/binary, "/deposits">>,
            S,
            #{},
            Opts
        ),
        hb_maps:filtermap(
            fun(Addr, _) -> 
                case get_deposit(Addr, ResourceID, S, Opts) of
                    Qty when is_integer(Qty) -> {true, Qty};
                    _ -> false
                end
            end,
            Ds,
            Opts
        )
end.

%% @doc Return the contents of the inverted index for a specific address.
user(Addr, S, Opts) ->
    maybe
        true ?= dev_token:validate_address(Addr, ?RESERVED_KEYS),
        hb_ao:get(<<"/users/", Addr/binary>>, S, #{}, Opts)
    end.
normalize_quantity(Qty, QuantityScale)
        when is_integer(Qty), is_integer(QuantityScale), QuantityScale > 0 ->
    {ok, (Qty * ?POT_QUANTITY_SCALE) div QuantityScale};
normalize_quantity(_, _) ->
    {error, invalid_quantity_or_scale}.