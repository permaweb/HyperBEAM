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
-export([modify_deposit/5, delegate/6, maybe_liquidate_delegations/5]).
-export([units_minted_between/5, set_user_deposit/5, set_user_delegations/4]).
-export([user/3, balance/2, balances/1, deposit/3, deposits/1, deposits/2]).
%%% Pot Model.

drip(State, Req, Opts) ->
    SWithTime =
        case is_map(Req) andalso hb_maps:find(<<"t">>, Req) of
            {ok, TReq} -> State#{ <<"t">> => TReq };
            _ -> State#{ <<"t">> => hb_maps:get(<<"t">>, State, 0) + 1 }
        end,
    drip(SWithTime, Opts).

drip(S = #{ <<"t">> := T, <<"last-drip">> := Last }, _Opts) when T =:= Last -> S;
drip(S = #{
        <<"t">> := T,
        <<"mint-cap">> := Max,
        <<"mint-prop">> := Proportion
    }, Opts) ->
    Minted = hb_maps:get(<<"minted">>, S, 0, Opts),
    LastT = hb_maps:get(<<"last-drip">>, S, 0, Opts),
    Steps = max(T - LastT, 0),
    case Steps =:= 0 of
        true -> S;
        false ->
            ToMint = units_minted_between(Minted, Max, Proportion, LastT, T),
            TW = hb_maps:get(<<"tw">>, S),
            ?event({minting, {to_mint, ToMint}, {total_weight, TW}}),
            DeltaM = case TW of 0 -> 0; _ -> ToMint / TW end,
            M0 = hb_maps:get(<<"chi">>, S, 0, Opts),
            R = S#{
                <<"chi">> => M0 + DeltaM,
                <<"last-drip">> => T,
                <<"minted">> => Minted + ToMint
            },
            ?event({new_state, R}),
            R
    end.

balance(Addr, S) ->
    ExistingBalance = hb_maps:get(Addr, hb_maps:get(<<"balances">>, S, #{}), 0),
    ResourceIDs =
        hb_maps:keys(
            hb_ao:get(<<"users/", Addr/binary, "/deposits">>, S, #{}, #{}),
            #{}
        ),
    Chi = hb_maps:get(<<"chi">>, S, 0),
    ExistingBalance + 
        lists:sum(
            lists:map(
                fun(ResourceID) ->
                    Res = hb_ao:get(<<"resources/", ResourceID/binary>>, S, #{}, #{}),
                    ResW = hb_maps:get(<<"weight">>, Res, 0),
                    ChiEff = ResW * Chi,
                    Deposits = hb_maps:get(<<"deposits">>, Res, #{}),
                    case hb_maps:find(Addr, Deposits) of
                        error -> 0;
                        {ok, #{ <<"quantity">> := Qty, <<"chi0">> := Chi0 }} ->
                            ?no_prod("Remove all floating point arithmetic."),
                            (ChiEff - Chi0) * Qty
                    end
                end,
                ResourceIDs
            )
        ).

units_minted_between(Minted, Max, Proportion, LastT, T) ->
    Steps = max(T - LastT, 0),
    Remaining = Max - Minted,
    Remaining * (1 - math:pow(1 - Proportion, Steps)).

modify_deposit(Addr, ResourceID, Amount, S0, Opts) ->
    S1 = drip(S0, Opts),
    #{ <<"balances">> := Balances0, <<"resources">> := Resources0 } = S1,
    ExistingDeposit = deposit(Addr, ResourceID, S1),
    NewDepositQty = ExistingDeposit + Amount,
    RealizedBalance = balance(Addr, S1),
    % Reset chi0 for this address across all resources to current chi
    Chi = hb_maps:get(<<"chi">>, S1, 0, Opts),
    NewResources =
        hb_maps:map(
            fun(XResID, Res) ->
                IsDepositRes = XResID =:= ResourceID,
                ResDeposits = hb_maps:get(<<"deposits">>, Res, #{}),
                Entry = hb_maps:get(Addr, ResDeposits, #{}, Opts),
                case IsDepositRes orelse not ?IS_EMPTY_MESSAGE(Entry) of
                    false -> Res;
                    true ->
                        ResWeight = hb_maps:get(<<"weight">>, Res, 0),
                        NewChi0 = ResWeight * Chi,
                        TotalDeposits = hb_maps:get(<<"total-deposits">>, Res, 0, Opts),
                        Res#{
                            <<"deposits">> =>
                                ResDeposits#{
                                    Addr =>
                                        Entry#{
                                            <<"chi0">> => NewChi0,
                                            <<"quantity">> =>
                                                if IsDepositRes -> NewDepositQty;
                                                true ->
                                                    hb_maps:get(<<"quantity">>, Entry, 0, Opts)
                                                end
                                        }
                                },
                            <<"total-deposits">> =>
                                TotalDeposits +
                                    if IsDepositRes -> Amount;
                                    true -> 0
                                    end
                        }
                end
            end,
            Resources0
        ),
    ?event({new_resources, NewResources}),
    WeightR = hb_ao:get(<<ResourceID/binary, "/weight">>, NewResources, 0, Opts),
    Tw0 = hb_maps:get(<<"tw">>, S1),
    S2 =
        S1#{
            <<"resources">> => NewResources,
            <<"tw">> => Tw0 + (WeightR * Amount),
            <<"balances">> => Balances0#{ Addr => RealizedBalance }
        },
    S3 =
        maybe_liquidate_delegations(
            Addr,
            ResourceID,
            S2,
            Opts
        ),
    set_user_deposit(Addr, ResourceID, deposit(Addr, ResourceID, S3), S3, Opts).

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

set_user_deposit(Addr, ResourceID, Quantity, S, Opts) ->
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

set_user_delegations(Addr, Delegations, S, Opts) ->
    hb_ao:set(
        S,
        <<"/users/", Addr/binary, "/delegations">>,
        Delegations,
        Opts
    ).