-module(dev_pot_props).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(USERS, 10).

simulation_test() ->
    hb:init(),
    ok = hb_invariant:state_machine(
        #{
            opts => fun generate_opts/1,
            requests => generate_request(),
            states => fun generate_initial_state/1,
            properties => [
                fun verify_deposit_quantity/4,
                fun verify_delegations/4,
                fun verify_twu/4,
                fun verify_inverted_index/4
            ],
            runs => 3,
            length => 4,
            next => fun next/4,
            users => ?USERS
        }
    ).

generate_opts(#{ users := Users }) ->
    #{
        resources =>
            [
                hb_invariant:string(id)
            ||
                _ <- lists:seq(1, 10)
            ],
        identities => dev_token_props:generate_identities(Users),
        priv_wallet => ar_wallet:new()
    }.

generate_initial_state(Opts) ->
    MintCap = hb_invariant:int(1, 1_000_000_000_000_000),
    PropN = 1 + hb_invariant:int(1, 10_000),
    PropD = PropN + hb_invariant:int(1, 10_000),
    StartWeight = hb_invariant:int(1, 10_000),
    StartQty = hb_invariant:int(1, 1_000_000),
    StartResource = hb_invariant:string(id),
    StartAddr = hb_util:human_id(hb_invariant:pick(dev_token_props:user_wallets(Opts))),
    S0 =
        #{
            <<"device">> => <<"pot@1.0">>,
            <<"t">> => 0,
            <<"last-drip">> => 0,
            <<"mint-cap">> => MintCap,
            <<"mint-prop-numerator">> => PropN,
            <<"mint-prop-denominator">> => PropD,
            <<"resources">> => #{
                StartResource => #{
                    <<"accumulator">> => 1,
                    <<"last-global-accumulator">> => 1,
                    <<"weight">> => StartWeight,
                    <<"total-deposits">> => StartQty,
                    <<"deposits">> => #{
                        StartAddr => #{
                            <<"quantity">> => StartQty,
                            <<"last-resource-accumulator">> => 1 % TODO: randomize this?
                        }
                    }
                }
            },
            <<"balances">> => dev_token_props:generate_initial_balances(Opts),
            <<"users">> => #{
                StartAddr => #{
                    <<"deposits">> => #{
                        StartResource => StartQty
                    }
                }
            }
        },
    % Register every resource we pre-generated for the scenario
    Resources = hb_maps:get(resources, Opts),
    S1 = lists:foldl(
        fun(Resource, State) ->
            dev_pot:register_resource(Resource, hb_invariant:int(), State, Opts)
        end,
        S0,
        Resources
    ),
    % Initialize the "originated deposits" helper table (see also: the 'next'
    % function clause for deposits)
    hb_private:set(
        S1,
        <<"/users/", StartAddr/binary, "/deposits/", StartResource/binary>>,
        StartQty,
        Opts
    ).

generate_request() ->
    [
        %fun deposit_generator/2,
        %fun withdraw_generator/2,
        fun delegate_generator/2
        % fun undelegate_generator/0
    ].

deposit_generator(_State, Opts) ->
    Wallet = hb_invariant:pick(dev_token_props:user_wallets(Opts)),
    hb_message:commit(
        #{
            <<"path">> => <<"deposit">>,
            <<"body">> => #{
                <<"address">> => hb_util:human_id(Wallet),
                <<"quantity">> => hb_invariant:int(1, 1_000_000),
                <<"resource">> => hb_invariant:pick(hb_maps:get(resources, Opts)),
                <<"from">> => <<"foo">>, % TODO: What should this value be?
                <<"t">> => hb_invariant:int(100000)
            }
        },
        Opts#{ priv_wallet => Wallet }
    ).

withdraw_generator(State, Opts) ->
    Users = hb_private:get(<<"users">>, State, #{}, Opts),
    NonzeroOriginalDeposits = get_nonzero_deposits(Users),
    % TODO: no nonzero deposits will cause a spurious test
    % failure. Should we invent technology to handle it?
    {UserAddr, UserResourceID, CurrentQty} = hb_invariant:pick(NonzeroOriginalDeposits),
    Wallet = hb_maps:get(priv_wallet, hb_maps:get(UserAddr, hb_maps:get(identities, Opts))),
    hb_message:commit(
        #{
            <<"path">> => <<"withdraw">>,
            <<"body">> => #{
                <<"address">> => UserAddr,
                <<"quantity">> => hb_invariant:int(1, CurrentQty),
                <<"resource">> => UserResourceID,
                <<"from">> => <<"foo">>, % TODO: What should this value be?
                <<"t">> => hb_invariant:int(100000)
            }
        },
        Opts#{ priv_wallet => Wallet }
    ).

delegate_generator(State, Opts) ->
    Users = hb_maps:get(<<"users">>, State, #{}, Opts),
    NonzeroDeposits = get_nonzero_deposits(hb_private:reset(Users)),
    % TODO: no nonzero deposits will cause a spurious test
    % failure. Should we invent technology to handle it?
    {FromAddr, UserResourceID, CurrentQty} = hb_invariant:pick(NonzeroDeposits),
    Wallet = hb_maps:get(priv_wallet, hb_maps:get(FromAddr, hb_maps:get(identities, Opts))),
    ToAddr = hb_util:human_id(hb_invariant:pick(dev_token_props:user_wallets(Opts))),
    DelegatedQty = hb_invariant:int(1, CurrentQty),
    hb_message:commit(
        #{
            <<"path">> => <<"delegate">>,
            <<"body">> => #{
                <<"address">> => ToAddr,
                <<"quantity">> => DelegatedQty,
                <<"resource">> => UserResourceID,
                <<"from">> => FromAddr, 
                <<"t">> => hb_invariant:int(100000)
            }
        },
        Opts#{ priv_wallet => Wallet }
    ).

verify_deposit_quantity(OldState, Req = #{ <<"path">> := <<"deposit">> }, NewState, Opts) ->
    UnwrappedReq = hb_maps:get(<<"body">>, Req),
    Addr = hb_maps:get(<<"address">>, UnwrappedReq),
    ResourceID = hb_maps:get(<<"resource">>, UnwrappedReq),
    Quantity = hb_maps:get(<<"quantity">>, UnwrappedReq),
    OldDeposit = hb_ao:get(
        <<"/resources/", ResourceID/binary, "/deposits/", Addr/binary, "/quantity">>,
        OldState,
        0,
        Opts
    ),
    NewDeposit = hb_ao:get(
        <<"/resources/", ResourceID/binary, "/deposits/", Addr/binary, "/quantity">>,
        NewState,
        0,
        Opts
    ),
    NewDeposit =:= OldDeposit + Quantity orelse
    {error,
        {bad_deposit_math,
            {old_deposit, OldDeposit},
            {new_deposit, NewDeposit},
            {qty, Quantity}
        }
    };
verify_deposit_quantity(OldState, Req = #{ <<"path">> := <<"withdraw">> }, NewState, Opts) ->
    UnwrappedReq = hb_maps:get(<<"body">>, Req),
    Quantity = hb_maps:get(<<"quantity">>, UnwrappedReq),
    Addr = hb_maps:get(<<"address">>, UnwrappedReq),
    ResourceID = hb_maps:get(<<"resource">>, UnwrappedReq),
    OldDeposit = hb_ao:get(
        <<"/resources/", ResourceID/binary, "/deposits/", Addr/binary, "/quantity">>,
        OldState,
        0,
        Opts
    ),
    NewDeposit = hb_ao:get(
        <<"/resources/", ResourceID/binary, "/deposits/", Addr/binary, "/quantity">>,
        NewState,
        0,
        Opts
    ),
    case OldDeposit >= Quantity of
        true ->
            % No liquidation required
            NewDeposit =:= OldDeposit - Quantity orelse
            {error,
                {bad_withdraw_math_no_liquidation,
                    {old_deposit, OldDeposit},
                    {new_deposit, NewDeposit},
                    {qty, Quantity}
                }
            };
        false ->
            % Liquidation required
            NewDeposit =:= 0 orelse
            {error,
                {bad_withdraw_math_with_liquidation,
                    {old_deposit, OldDeposit},
                    {new_deposit, NewDeposit},
                    {qty, Quantity}
                }
            }
    end;
verify_deposit_quantity(OldState, Req = #{ <<"path">> := <<"delegate">> }, NewState, Opts) ->
    UnwrappedReq = hb_maps:get(<<"body">>, Req),
    FromAddr = hb_maps:get(<<"from">>, UnwrappedReq),
    ToAddr = hb_maps:get(<<"address">>, UnwrappedReq),
    ResourceID = hb_maps:get(<<"resource">>, UnwrappedReq),
    Quantity = hb_maps:get(<<"quantity">>, UnwrappedReq),
    OldDepositDelegator = hb_ao:get(
        <<"/resources/", ResourceID/binary, "/deposits/", FromAddr/binary, "/quantity">>,
        OldState,
        0,
        Opts
    ),
    NewDepositDelegator = hb_ao:get(
        <<"/resources/", ResourceID/binary, "/deposits/", FromAddr/binary, "/quantity">>,
        NewState,
        0,
        Opts
    ),
    OldDepositRecipient = hb_ao:get(
        <<"/resources/", ResourceID/binary, "/deposits/", ToAddr/binary, "/quantity">>,
        OldState,
        0,
        Opts
    ),
    NewDepositRecipient = hb_ao:get(
        <<"/resources/", ResourceID/binary, "/deposits/", ToAddr/binary, "/quantity">>,
        NewState,
        0,
        Opts
    ),
    case FromAddr =:= ToAddr of
        true ->
            % Delegating to yourself
            NewDepositDelegator =:= OldDepositDelegator orelse
            {error,
                {bad_delegate_math_self_delegation,
                    {old_deposit, OldDepositDelegator},
                    {new_deposit, NewDepositDelegator},
                    {qty, Quantity}
                }
            };
        false ->
            % Delegating to someone other than yourself
            NewDepositDelegator =:= OldDepositDelegator - Quantity andalso
            NewDepositRecipient =:= OldDepositRecipient + Quantity orelse
            {error,
                {bad_delegate_math,
                    {old_delegator_deposit, OldDepositDelegator},
                    {new_delegator_deposit, NewDepositDelegator},
                    {old_recipient_deposit, OldDepositRecipient},
                    {new_recipient_deposit, NewDepositRecipient},
                    {qty, Quantity}
                }
            }
    end;
verify_deposit_quantity(_OldState, _Req, _NewState, _Opts) -> true.

verify_delegations(OldState, Req = #{ <<"path">> := <<"withdraw">> }, NewState, Opts) ->
    UnwrappedReq = hb_maps:get(<<"body">>, Req),
    Quantity = hb_maps:get(<<"quantity">>, UnwrappedReq),
    Addr = hb_maps:get(<<"address">>, UnwrappedReq),
    ResourceID = hb_maps:get(<<"resource">>, UnwrappedReq),
    OldDeposit = hb_ao:get(
        <<"/resources/", ResourceID/binary, "/deposits/", Addr/binary, "/quantity">>,
        OldState,
        0,
        Opts
    ),
    OldDelegations =
        hb_private:reset(
            hb_ao:get(
                <<
                "/resources/",
                ResourceID/binary,
                "/deposits/",
                Addr/binary,
                "/delegations"
                >>,
                OldState,
                #{},
                Opts
            )
        ),
    NewDelegations =
        hb_private:reset(
            hb_ao:get(
                <<
                    "/resources/",
                    ResourceID/binary,
                    "/deposits/",
                    Addr/binary,
                    "/delegations"
                >>,
                NewState,
                #{},
                Opts
            )
        ),
    case OldDeposit >= Quantity of
        true ->
            % No liquidation required
            OldDelegations =:= NewDelegations orelse
            {error,
                {delegation_table_mutated,
                    {old_table, OldDelegations},
                    {new_table, NewDelegations}
                }
            };
        false ->
            % Liquidation required
            SumOldDelegations = lists:sum(hb_maps:values(OldDelegations)),
            SumNewDelegations = lists:sum(hb_maps:values(NewDelegations)),
            SumOldDelegations - SumNewDelegations =:=
            Quantity - OldDeposit orelse
            {error,
                {incoherent_liquidation,
                    {old_table, OldDelegations},
                    {new_table, NewDelegations}
                }
            }
    end;
verify_delegations(OldState, Req = #{ <<"path">> := <<"delegate">> }, NewState, Opts) ->
    UnwrappedReq = hb_maps:get(<<"body">>, Req),
    FromAddr = hb_maps:get(<<"from">>, UnwrappedReq),
    ToAddr = hb_maps:get(<<"address">>, UnwrappedReq),
    ResourceID = hb_maps:get(<<"resource">>, UnwrappedReq),
    Quantity = hb_maps:get(<<"quantity">>, UnwrappedReq),
    OldDelegations =
        hb_private:reset(
            hb_ao:get(
                <<
                "/resources/",
                ResourceID/binary,
                "/deposits/",
                FromAddr/binary,
                "/delegations"
                >>,
                OldState,
                #{},
                Opts
            )
        ),
    NewDelegations =
        hb_private:reset(
            hb_ao:get(
                <<
                    "/resources/",
                    ResourceID/binary,
                    "/deposits/",
                    FromAddr/binary,
                    "/delegations"
                >>,
                NewState,
                #{},
                Opts
            )
        ),
    OldDelegatedQty = hb_maps:get(ToAddr, OldDelegations, 0),
    NewDelegatedQty = hb_maps:get(ToAddr, NewDelegations, 0),
    % Self-delegation is a noop
    case FromAddr =:= ToAddr of
        true ->
            NewDelegatedQty =:= OldDelegatedQty orelse
            {error,
                {bad_delegation_math_self_delegation,
                    {old_table, OldDelegations},
                    {new_table, NewDelegations},
                    {qty, Quantity},
                    {from, FromAddr},
                    {to, ToAddr}
                }
            };
        false ->
            NewDelegatedQty =:= OldDelegatedQty + Quantity orelse
            {error,
                {bad_delegation_math,
                    {old_table, OldDelegations},
                    {new_table, NewDelegations},
                    {qty, Quantity},
                    {from, FromAddr},
                    {to, ToAddr}
                }
            }
    end;
verify_delegations(_OldState, _Req, _NewState, _Opts) -> true.

verify_twu(OldState, Req, NewState, Opts) ->
    UnwrappedReq = hb_maps:get(<<"body">>, Req, #{}),
    Quantity = hb_maps:get(<<"quantity">>, UnwrappedReq, 0),
    ResourceID = hb_maps:get(<<"resource">>, UnwrappedReq),
    Weight = hb_ao:get(
        <<"/resources/", ResourceID/binary, "/weight">>,
        NewState,
        0,
        Opts
    ),
    OldTWU = hb_maps:get(<<"total-weighted-units">>, OldState),
    NewTWU = hb_maps:get(<<"total-weighted-units">>, NewState),
    Path = hb_maps:get(<<"path">>, Req),
    Res =
        case Path of
            <<"deposit">> ->
                NewTWU =:= OldTWU + (Quantity * Weight);
            <<"withdraw">> ->
                NewTWU =:= OldTWU - (Quantity * Weight);
            _ -> NewTWU =:= OldTWU
        end,
    Res orelse
    {error,
        {bad_total_weighted_units,
            {old_twu, OldTWU},
            {new_twu, NewTWU},
            {weight, Weight},
            {qty, Quantity}
        }
    }.

verify_inverted_index(_OldState, Req = #{ <<"path">> := <<"deposit">> }, NewState, Opts) ->
    UnwrappedReq = hb_maps:get(<<"body">>, Req),
    Addr = hb_maps:get(<<"address">>, UnwrappedReq),
    ResourceID = hb_maps:get(<<"resource">>, UnwrappedReq),
    do_verify_inverted_index(Addr, ResourceID, NewState, Opts);
verify_inverted_index(_OldState, Req = #{ <<"path">> := <<"withdraw">> }, NewState, Opts) ->
    UnwrappedReq = hb_maps:get(<<"body">>, Req),
    Addr = hb_maps:get(<<"address">>, UnwrappedReq),
    ResourceID = hb_maps:get(<<"resource">>, UnwrappedReq),
    do_verify_inverted_index(Addr, ResourceID, NewState, Opts);
verify_inverted_index(_OldState, Req = #{ <<"path">> := <<"delegate">> }, NewState, Opts) ->
    UnwrappedReq = hb_maps:get(<<"body">>, Req),
    FromAddr = hb_maps:get(<<"from">>, UnwrappedReq),
    ToAddr = hb_maps:get(<<"address">>, UnwrappedReq),
    ResourceID = hb_maps:get(<<"resource">>, UnwrappedReq),
    do_verify_inverted_index(ToAddr, ResourceID, NewState, Opts) andalso
    do_verify_inverted_index(FromAddr, ResourceID, NewState, Opts);
verify_inverted_index(_OldState, _Req, _NewState, _Opts) ->
    true.

do_verify_inverted_index(Addr, ResourceID, State, Opts) ->
    InvertedQty = hb_ao:get(
        <<"/users/", Addr/binary, "/deposits/", ResourceID/binary>>,
        State,
        inverted_qty_not_found,
        Opts
    ),
    DepositQty = hb_ao:get(
        <<"/resources/", ResourceID/binary, "/deposits/", Addr/binary, "/quantity">>,
        State,
        deposit_qty_not_found,
        Opts
    ),
    InvertedQty =:= DepositQty orelse
    {error,
        {bad_inverted_index,
            {inverted_deposit, InvertedQty},
            {deposit_qty, DepositQty}
        }
    }.   

% Note that we keep private state which mirrors the schema of the inverted
% index, but which keeps track of the deposits *originated* by each user.
% That is, deposits without consideration of delegation inflow and outflow.
% This lets us generate coherent withdrawals without unwinding the whole
% delegation table.
next(OldS, Req = #{<<"path">> := <<"deposit">>}, NewState, Opts) ->
    UnwrappedReq = hb_maps:get(<<"body">>, Req),
    Addr = hb_maps:get(<<"address">>, UnwrappedReq),
    ResourceID = hb_maps:get(<<"resource">>, UnwrappedReq),
    Quantity = hb_maps:get(<<"quantity">>, UnwrappedReq),
    CurrentTotalDeposit = hb_private:get(
        <<"/users/", Addr/binary, "/deposits/", ResourceID/binary>>,
        NewState,
        0,
        Opts
    ),
    hb_private:set(
        NewState,
        <<"/users/", Addr/binary, "/deposits/", ResourceID/binary>>,
        CurrentTotalDeposit + Quantity,
        Opts
    );
next(OldS, Req = #{<<"path">> := <<"withdraw">>}, NewState, Opts) ->
    UnwrappedReq = hb_maps:get(<<"body">>, Req),
    Addr = hb_maps:get(<<"address">>, UnwrappedReq),
    ResourceID = hb_maps:get(<<"resource">>, UnwrappedReq),
    Quantity = hb_maps:get(<<"quantity">>, UnwrappedReq),
    CurrentTotalDeposit = hb_private:get(
        <<"/users/", Addr/binary, "/deposits/", ResourceID/binary>>,
        NewState,
        0,
        Opts
    ),
    hb_private:set(
        NewState,
        <<"/users/", Addr/binary, "/deposits/", ResourceID/binary>>,
        CurrentTotalDeposit - Quantity,
        Opts
    );
next(_OldS, _Req, NewS, _Opts) -> NewS.

%%% Helpers

% Operates over the map corresponding to the <<"users">> key in
% the inverted index.
get_nonzero_deposits(Users) ->
    hb_maps:fold(
        fun(Address, DepositsMap, Acc1) ->
            Deposits = hb_private:reset(hb_maps:get(<<"deposits">>, DepositsMap)),
            hb_maps:fold(
                fun(ResourceID, Quantity, Acc2) when Quantity =/= 0 ->
                    [{Address, ResourceID, Quantity} | Acc2];
                    (_, _, Acc2) -> Acc2
                end,
                Acc1,
                Deposits
            )
        end,
        [],
        Users
    ).