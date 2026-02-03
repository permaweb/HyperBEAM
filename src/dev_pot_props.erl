-module(dev_pot_props).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(USERS, 10).
-define(RESOURCES, 10).

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
                fun verify_inverted_index/4,
                fun verify_undistributed_mint/4
            ],
            runs => 3,
            length => 50,
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
                _ <- lists:seq(1, ?RESOURCES)
            ],
        identities => dev_token_props:generate_identities(Users),
        priv_wallet => ar_wallet:new()
    }.

generate_initial_state(Opts) ->
    MintCap = 21_000_000,
    PropN = 1,
    PropD = 1000,
    StartWeight = hb_invariant:int(1, 100_000),
    StartQty = hb_invariant:int(1, 100_000),
    StartResource = hb_invariant:pick(hb_maps:get(resources, Opts)),
    StartAddr = hb_util:human_id(hb_invariant:pick(dev_token_props:user_wallets(Opts))),
    % Pick an address that's not our StartAddr for our initial delegatee
    DelegateeCandidates =
        lists:delete(
            StartAddr,
            hb_maps:keys(hb_maps:get(identities, Opts))
        ),
    DelegateeAddr = hb_invariant:pick(DelegateeCandidates),
    DelegatedAmount = hb_invariant:int(1, StartQty),
    DepositMinusDelegated = StartQty - DelegatedAmount,
    S0 =
        #{
            <<"device">> => <<"pot@1.0">>,
            <<"t">> => 0,
            <<"last-drip">> => 0,
            <<"mint-cap">> => MintCap,
            <<"mint-prop-numerator">> => PropN,
            <<"mint-prop-denominator">> => PropD,
            <<"accumulator">> => 0,
            <<"total-weighted-units">> => StartQty * StartWeight,
            <<"resources">> => #{
                StartResource => #{
                    <<"accumulator">> => 0,
                    <<"last-global-accumulator">> => 0,
                    <<"weight">> => StartWeight,
                    <<"total-deposits">> => StartQty,
                    <<"deposits">> => #{
                        StartAddr => #{
                            <<"quantity">> => DepositMinusDelegated,
                            <<"last-resource-accumulator">> => 0, % TODO: randomize this?
                            <<"delegations">> => #{
                                DelegateeAddr => DelegatedAmount
                            }
                        },
                        DelegateeAddr => #{
                            <<"quantity">> => DelegatedAmount,
                            <<"last-resource-accumulator">> => 0 % TODO: randomize this?
                        }
                    }
                }
            },
            <<"balances">> => dev_token_props:generate_initial_balances(Opts),
            <<"users">> => #{
                StartAddr => #{
                    <<"deposits">> => #{
                        StartResource => DepositMinusDelegated
                    }
                },
                DelegateeAddr => #{
                    <<"deposits">> => #{
                        StartResource => DelegatedAmount
                    }
                }
            }
        },
    % Register every resource we pre-generated for the scenario, not including
    % StartResource which we've already registered in the hand-coded state above
    RegisterableResources =
        lists:delete(
            StartResource,
            hb_maps:get(resources, Opts)
        ),
    S1 = lists:foldl(
        fun(Resource, State) ->
            dev_pot:register_resource(
                Resource,
                hb_invariant:int(1, 100_000),
                State,
                Opts
            )
        end,
        S0,
        RegisterableResources
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
        fun deposit_generator/2,
        fun withdraw_generator/2,
        fun delegate_generator/2,
        fun undelegate_generator/2
    ].

deposit_generator(_State, Opts) ->
    Wallet = hb_invariant:pick(dev_token_props:user_wallets(Opts)),
    {
        deposit,
        hb_message:commit(
            #{
                <<"path">> => <<"deposit">>,
                <<"body">> => #{
                    <<"address">> => hb_util:human_id(Wallet),
                    <<"quantity">> => hb_invariant:int(1, 100_000),
                    <<"resource">> => hb_invariant:pick(hb_maps:get(resources, Opts)),
                    <<"from">> => <<"foo">>, % TODO: What should this value be?
                    <<"t">> => hb_invariant:int(100_000)
                }
            },
            Opts#{ priv_wallet => Wallet }
        )
    }.

withdraw_generator(State, Opts) ->
    % We use our private "originated deposits" helper table because we want to
    % base our withdrawal quantity on deposits without consideration of delegations.
    Users = hb_private:get(<<"users">>, State, #{}, Opts),
    NonzeroOriginalDeposits = get_nonzero_deposits(Users),
    case length(NonzeroOriginalDeposits) =:= 0 of
        true -> {noop, #{}};
        false ->
            {UserAddr, UserResourceID, CurrentQty} =
                hb_invariant:pick(NonzeroOriginalDeposits),
            Wallet =
                hb_maps:get(
                    priv_wallet, 
                    hb_maps:get(UserAddr, hb_maps:get(identities, Opts))
                ),
            {
                withdraw,
                hb_message:commit(
                    #{
                        <<"path">> => <<"withdraw">>,
                        <<"body">> => #{
                            <<"address">> => UserAddr,
                            <<"quantity">> => hb_invariant:int(1, CurrentQty),
                            <<"resource">> => UserResourceID,
                            <<"from">> => <<"foo">>, % TODO: What should this value be?
                            <<"t">> => hb_invariant:int(100_000)
                        }
                    },
                    Opts#{ priv_wallet => Wallet }
                )
            }
    end.

delegate_generator(State, Opts) ->
    Users = hb_maps:get(<<"users">>, State, #{}, Opts),
    NonzeroDeposits = get_nonzero_deposits(hb_private:reset(Users)),
    case length(NonzeroDeposits) =:= 0 of
        true -> {noop, #{}};
        false ->
            {FromAddr, UserResourceID, CurrentQty} =
                hb_invariant:pick(NonzeroDeposits),
            Wallet =
                hb_maps:get(
                    priv_wallet,
                    hb_maps:get(FromAddr, hb_maps:get(identities, Opts))
                ),
            ToAddr =
                hb_util:human_id(
                    hb_invariant:pick(dev_token_props:user_wallets(Opts))
                ),
            DelegatedQty = hb_invariant:int(1, CurrentQty),
            {
                delegate,
                hb_message:commit(
                    #{
                        <<"path">> => <<"delegate">>,
                        <<"body">> => #{
                            <<"address">> => ToAddr,
                            <<"quantity">> => DelegatedQty,
                            <<"resource">> => UserResourceID,
                            <<"from">> => FromAddr, 
                            <<"t">> => hb_invariant:int(100_000)
                        }
                    },
                    Opts#{ priv_wallet => Wallet }
                )
            }
    end.

undelegate_generator(State, Opts) ->
    NonzeroDelegations = get_nonzero_delegations(hb_private:reset(State)),
    case length(NonzeroDelegations) =:= 0 of
        true -> {noop, #{}};
        false ->
            {FromAddr, ToAddr, ResourceID, Qty} =
                hb_invariant:pick(NonzeroDelegations),
            Wallet =
                hb_maps:get(
                    priv_wallet,
                    hb_maps:get(FromAddr, hb_maps:get(identities, Opts))
                ),
            UndelegateQty = hb_invariant:int(1, Qty),
            {
                undelegate,
                hb_message:commit(
                    #{
                        <<"path">> => <<"undelegate">>,
                        <<"body">> => #{
                            <<"address">> => ToAddr,
                            <<"quantity">> => UndelegateQty,
                            <<"resource">> => ResourceID,
                            <<"from">> => FromAddr, 
                            <<"t">> => hb_invariant:int(100_000)
                        }
                    },
                    Opts#{ priv_wallet => Wallet }
                )
            }
    end.

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
verify_deposit_quantity(OldState, Req = #{ <<"path">> := <<"undelegate">> }, NewState, Opts) ->
    UnwrappedReq = hb_maps:get(<<"body">>, Req),
    FromAddr = hb_maps:get(<<"from">>, UnwrappedReq),
    ToAddr = hb_maps:get(<<"address">>, UnwrappedReq),
    ResourceID = hb_maps:get(<<"resource">>, UnwrappedReq),
    Quantity = hb_maps:get(<<"quantity">>, UnwrappedReq),
    OldDepositUndelegator =
        hb_ao:get(
            <<
                "/resources/",
                ResourceID/binary,
                "/deposits/",
                FromAddr/binary,
                "/quantity"
            >>,
            OldState,
            0,
            Opts
        ),
    NewDepositUndelegator =
        hb_ao:get(
                <<
                    "/resources/",
                    ResourceID/binary,
                    "/deposits/",
                    FromAddr/binary,
                    "/quantity"
                >>,
            NewState,
            0,
            Opts
        ),
    OldDepositRecipient = 
        hb_ao:get(
            <<
                "/resources/",
                ResourceID/binary,
                "/deposits/",
                ToAddr/binary,
                "/quantity"
            >>,
            OldState,
            0,
            Opts
        ),
    NewDepositRecipient =
        hb_ao:get(
            <<
                "/resources/",
                ResourceID/binary,
                "/deposits/",
                ToAddr/binary,
                "/quantity"
            >>,
            NewState,
            0,
            Opts
        ),
    UndelegatorDepositOK =
        case FromAddr =:= ToAddr of
            true ->
                % Undelegating to yourself
                NewDepositUndelegator =:= OldDepositUndelegator;
            false ->
                % Undelegating to someone other than yourself
                % We cannot know how the undelegator's deposit changed
                % after undelegation, because the undelegation may have
                % circularly undelegated inflow to the undelegator...
                % so we simply enforce no relation for this case.
                true
        end,
    RecipientDepositOK =
        case FromAddr =:= ToAddr of
            true ->
                % Undelegating to yourself
                true;
            false ->
                % Undelegating to someone other than yourself
                case OldDepositRecipient >= Quantity of
                    true ->
                        % No recipient liquidation required
                        NewDepositRecipient =:= OldDepositRecipient - Quantity;
                    false ->
                        % Recipient liquidation required
                        NewDepositRecipient =:= 0
                end
        end,
    UndelegatorDepositOK andalso RecipientDepositOK orelse
    {error,
        {bad_undelegate_math,
            {address, ToAddr},
            {from, FromAddr},
            {old_undelegator_deposit, OldDepositUndelegator},
            {new_undelegator_deposit, NewDepositUndelegator},
            {old_recipient_deposit, OldDepositRecipient},
            {new_recipient_deposit, NewDepositRecipient},
            {qty, Quantity}
        }
    };
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
    % Self-delegation is a noop
    case FromAddr =:= ToAddr of
        true ->
            NewDelegations =:= OldDelegations orelse
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
            % Note subtle complexity: it's possible that there was no delegation
            % in the old state, so we should use a default value of 0. But no
            % delegation in the new state indicates a malformed schema.
            OldDelegatedQty = hb_maps:get(ToAddr, OldDelegations, 0),
            NewDelegatedQty = hb_maps:get(ToAddr, NewDelegations, undefined),
            NewDelegatedQty =/= undefined andalso
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
verify_delegations(OldState, Req = #{ <<"path">> := <<"undelegate">> }, NewState, Opts) ->
    UnwrappedReq = hb_maps:get(<<"body">>, Req),
    FromAddr = hb_maps:get(<<"from">>, UnwrappedReq),
    ToAddr = hb_maps:get(<<"address">>, UnwrappedReq),
    ResourceID = hb_maps:get(<<"resource">>, UnwrappedReq),
    Quantity = hb_maps:get(<<"quantity">>, UnwrappedReq),
    OldDelegationsUndelegator =
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
    NewDelegationsUndelegator =
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
    OldDelegationsRecipient =
        hb_private:reset(
            hb_ao:get(
                <<
                "/resources/",
                ResourceID/binary,
                "/deposits/",
                ToAddr/binary,
                "/delegations"
                >>,
                OldState,
                #{},
                Opts
            )
        ),
    NewDelegationsRecipient =
        hb_private:reset(
            hb_ao:get(
                <<
                    "/resources/",
                    ResourceID/binary,
                    "/deposits/",
                    ToAddr/binary,
                    "/delegations"
                >>,
                NewState,
                #{},
                Opts
            )
        ),
    OldDepositRecipient = 
        hb_ao:get(
            <<
                "/resources/",
                ResourceID/binary,
                "/deposits/",
                ToAddr/binary,
                "/quantity"
            >>,
            OldState,
            0,
            Opts
        ),
    UndelegatorDelegationOK =
        case FromAddr =:= ToAddr of
            true ->
                % Undelegating to yourself
                NewDelegationsUndelegator =:= OldDelegationsUndelegator;
            false ->
                % Undelegating to someone other than yourself
                % Note subtle complexity: it's possible that there was no delegation
                % in the old state, so we should use a default value of 0. But no
                % delegation in the new state indicates a malformed schema.
                OldDelegatedQty = hb_maps:get(ToAddr, OldDelegationsUndelegator, 0),
                NewDelegatedQty = hb_maps:get(ToAddr, NewDelegationsUndelegator, undefined),
                NewDelegatedQty =/= undefined andalso
                NewDelegatedQty =:= OldDelegatedQty - Quantity
        end,
    RecipientDelegationOK =
        case FromAddr =:= ToAddr of
            true ->
                % Undelegating to yourself
                true;
            false ->
                % Undelegating to someone other than yourself
                case OldDepositRecipient >= Quantity of
                    true ->
                        % No recipient liquidation required
                        NewDelegationsRecipient =:= OldDelegationsRecipient;
                    false ->
                        % Recipient liquidation required
                        SumOldDelegations =
                            lists:sum(hb_maps:values(OldDelegationsRecipient)),
                        SumNewDelegations =
                            lists:sum(hb_maps:values(NewDelegationsRecipient)),
                        SumOldDelegations - SumNewDelegations =:=
                        Quantity - OldDepositRecipient
                end
        end,
    UndelegatorDelegationOK andalso RecipientDelegationOK orelse
    {error,
        {bad_undelegation_math,
            {old_table_undelegator, OldDelegationsUndelegator},
            {new_table_undelegator, NewDelegationsUndelegator},
            {old_table_recipient, OldDelegationsRecipient},
            {new_table_recipient, NewDelegationsRecipient},
            {qty, Quantity},
            {from, FromAddr},
            {to, ToAddr}
        }
    };
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
verify_inverted_index(_OldState, Req = #{ <<"path">> := <<"undelegate">> }, NewState, Opts) ->
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
    Delegations =
        hb_ao:get(
            <<
                "/resources/",
                ResourceID/binary,
                "/deposits/",
                Addr/binary,
                "/delegations"
            >>,
            State,
            #{},
            Opts
        ),
    ExpectedInvertedQty =
        case DepositQty =:= 0 andalso ?IS_EMPTY_MESSAGE(Delegations) of
            true -> inverted_qty_not_found;
            _ -> DepositQty
        end,
    InvertedQty =:= ExpectedInvertedQty orelse
    {error,
        {bad_inverted_index,
            {inverted_deposit, InvertedQty},
            {deposit_qty, DepositQty}
        }
    }.

verify_undistributed_mint(OldState, Req, NewState, Opts) ->
    UserAddrs = hb_maps:keys(hb_maps:get(identities, Opts)),
    OldBalanceSum =
        lists:sum(
            lists:map(
                fun(Addr) -> dev_pot:balance(Addr, OldState, Opts)
                end,
                UserAddrs
            )
        ),
    NewBalanceSum =
        lists:sum(
            lists:map(
                fun(Addr) -> dev_pot:balance(Addr, NewState, Opts)
                end,
                UserAddrs
            )
        ),
    AccumulatedYield = NewBalanceSum - OldBalanceSum,
    OldUndistributedMint = hb_maps:get(<<"undistributed-mint">>, OldState, 0),
    NewUndistributedMint = hb_maps:get(<<"undistributed-mint">>, NewState, 0),
    Minted = hb_maps:get(<<"minted">>, OldState, 0),
    Max = hb_maps:get(<<"mint-cap">>, NewState),
    PropN = hb_maps:get(<<"mint-prop-numerator">>, NewState),
    PropD = hb_maps:get(<<"mint-prop-denominator">>, NewState),
    LastT = hb_maps:get(<<"t">>, OldState),
    T = hb_maps:get(<<"t">>, NewState),
    Path = hb_maps:get(<<"path">>, Req),
    MintedOverDeltaT =
        dev_pot_math:minted_between(Minted, Max, PropN, PropD, LastT, T),
    UndistributedDisbursed = OldUndistributedMint - NewUndistributedMint,
    AccumulatedYield =:= MintedOverDeltaT + UndistributedDisbursed orelse
    {error,
        {bad_undistributed_mint,
            {minted_over_deltat, MintedOverDeltaT},
            {accumulated_yield, AccumulatedYield},
            {new_undistributed_mint, NewUndistributedMint},
            {old_undistributed_mint, OldUndistributedMint},
            {loss, AccumulatedYield - (MintedOverDeltaT + UndistributedDisbursed)},
            {last_t, LastT},
            {t, T}
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

get_nonzero_delegations(State) ->
    Resources = hb_maps:get(<<"resources">>, State),
    hb_maps:fold(
        fun(ResourceID, DepositsMap, Acc1) ->
            Deposits = hb_private:reset(hb_maps:get(<<"deposits">>, DepositsMap, #{})),
            hb_maps:fold(
                fun(FromAddr, DelegationsMap, Acc2) ->
                    Delegations =
                        hb_private:reset(hb_maps:get(<<"delegations">>, DelegationsMap, #{})),
                    hb_maps:fold(
                        fun(ToAddr, Quantity, Acc3) when Quantity =/= 0 ->
                            [{FromAddr, ToAddr, ResourceID, Quantity} | Acc3];
                            (_, _, Acc3) -> Acc3
                        end,
                        Acc2,
                        Delegations
                    )
                end,
                Acc1,
                Deposits
            )
        end,
        [],
        Resources
    ).