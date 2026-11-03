%%% @doc Test vectors for the `security@1.0' preloaded package.
-module(hb_security_test_vectors).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").
-export([device_info/0, balance/3]).

%% @doc Configure the packaged security device and token Balance test endpoint.
opts() ->
    hb:init(),
    BaseOpts = #{
        <<"load-remote-devices">> => false,
        <<"priv-wallet">> => ar_wallet:new(),
        <<"store">> => [hb_test_utils:test_store()]
    },
    DeviceNames =
        [
            <<"security@1.0">>,
            <<"trie@1.0">>,
            <<"message@1.0">>,
            <<"structured@1.0">>,
            <<"httpsig@1.0">>,
            <<"did-ar@1.0">>,
            <<"did@1.0">>,
            <<"hmac@1.0">>,
            <<"match@1.0">>,
            <<"query@1.0">>,
            <<"ao@1.0">>
        ],
    Bootstrap =
        maps:from_list(
            [
                begin
                    {ok, Module} = hb_device_load:reference(Name, BaseOpts),
                    {Name, Module}
                end
            ||
                Name <- DeviceNames
            ]
        ),
    BaseOpts#{
        <<"forge-bootstrap">> =>
            Bootstrap#{ <<"token@1.0">> => ?MODULE }
    }.

%% @doc Test-only implementation of the token Balance contract.
device_info() -> #{ exports => [<<"balance">>] }.

%% @doc Resolve a balance with the deployed token's account-key semantics.
balance(Base, Req, Opts) ->
    ID = hb_ao:get(<<"balance">>, Req, Opts),
    Key =
        case hb_ao:get(<<"swap-device">>, Base, not_found, Opts) of
            not_found -> id_key(ID);
            _ -> ID
        end,
    {ok, hb_ao:get([<<"balances">>, Key], Base, 0, Opts)}.

base(Policy) ->
    Policy#{ <<"device">> => <<"security@1.0">> }.

validate(Key, Policy, From, Opts) ->
    hb_ao:resolve(
        base(Policy),
        #{
            <<"path">> => <<"validate">>,
            <<"key">> => Key,
            <<"from">> => From,
            <<"subject">> => #{}
        },
        Opts
    ).

token_policy(Balances, TotalSupply, Opts) ->
    token_policy(Balances, TotalSupply, #{}, Opts).
token_policy(Balances, TotalSupply, Extra, Opts) ->
    StoredBalances =
        case maps:get(<<"swap-device">>, Extra, not_found) of
            not_found -> canonical_balances(Balances);
            _ ->
                maps:from_list(
                    [{hb_committer:uri(ID), Amount} || ID := Amount <- Balances]
                )
        end,
    {ok, BalanceTrie} =
        hb_ao:resolve(
            #{ <<"device">> => <<"trie@1.0">> },
            StoredBalances#{ <<"path">> => <<"set">> },
            Opts
        ),
    Extra#{
        <<"balances">> => BalanceTrie,
        <<"total-supply">> => TotalSupply
    }.

canonical_balances(Balances) ->
    maps:fold(
        fun(ID, Amount, Acc) ->
            Key = id_key(ID),
            Acc#{ Key => maps:get(Key, Acc, 0) + Amount }
        end,
        #{},
        Balances
    ).

id_key(ID) ->
    hb_util:to_lower(ID).

signer() ->
    Wallet = ar_wallet:new(),
    {hb_util:human_id(ar_wallet:to_address(Wallet)), Wallet}.

sign_subject(Body, RawWallets, Opts) ->
    Wallets = case RawWallets of
        List when is_list(List) -> List;
        Wallet -> [Wallet]
    end,
    lists:foldl(
        fun(Wallet, Msg) ->
            hb_message:commit(Msg, Opts#{ <<"priv-wallet">> => Wallet })
        end,
        Body,
        Wallets
    ).

cache_roundtrip(Msg, Opts) ->
    {ok, _} = hb_cache:write(Msg, Opts),
    {ok, Cached} = hb_cache:read(hb_message:id(Msg, signed, Opts), Opts),
    hb_cache:ensure_all_loaded(Cached, Opts).

sign_assignment(Body, BodyWallets, SchedulerWallet, Opts) ->
    SignedBody = sign_subject(Body, BodyWallets, Opts),
    sign_subject(
        #{
            <<"path">> => <<"compute">>,
            <<"type">> => <<"Assignment">>,
            <<"slot">> => 0,
            <<"body">> => SignedBody
        },
        SchedulerWallet,
        Opts
    ).

%% @doc Assigned messages retain literal field semantics after authentication.
assigned_message_device_test_() ->
    [
        {Codec, {timeout, 60, fun() ->
            Opts = maps:remove(<<"forge-bootstrap">>, opts()),
            {Scheduler, SchedulerWallet} = signer(),
            {Sender, Wallet} = signer(),
            lists:foreach(
                fun({Fields, UnsignedFields, Accepted}) ->
                    Subject = hb_message:commit(
                        Fields#{ <<"action">> => <<"Transfer">> },
                        Opts#{ <<"priv-wallet">> => Wallet },
                        Codec
                    ),
                    Assignment = sign_subject(
                        #{
                            <<"path">> => <<"compute">>,
                            <<"body">> => maps:merge(Subject, UnsignedFields)
                        },
                        SchedulerWallet,
                        Opts
                    ),
                    Result = hb_ao:resolve(
                        base(#{ <<"scheduler">> => Scheduler }),
                        Assignment,
                        Opts
                    ),
                    case Accepted of
                        true ->
                            {ok, Secured} = Result,
                            ?assertEqual(hb_committer:uri(Sender),
                                hb_ao:get(<<"body/from">>, Secured, Opts)),
                            ?assertEqual(<<"message@1.0">>,
                                hb_ao:get(<<"body/device">>, Secured, Opts));
                        false ->
                            ?assertEqual(
                                {skip, <<"Security subject signature verification failed.">>},
                                Result
                            )
                    end
                end,
                [
                    {#{}, #{}, true},
                    {#{}, #{ <<"device">> => <<"untrusted-device">> }, true},
                    {#{ <<"device">> => <<"message@1.0">> }, #{}, true},
                    {#{ <<"device">> => <<"untrusted-device">> }, #{}, false}
                ]
            )
        end}}
    || Codec <- [<<"httpsig@1.0">>, <<"ans104@1.0">>, <<"tx@1.0">>]
    ].

duplicate_authority_match_rejected_vector_test() ->
    Opts = opts(),
    ?assertEqual(
        {error, <<"Too few acceptable committers present.">>},
        validate(
            <<"authority">>,
            #{
                <<"authority">> => [<<"alice">>, <<"bob">>],
                <<"authority-match">> => 2
            },
            [<<"alice">>, <<"alice">>],
            Opts
        )
    ).

comma_separated_authority_config_supported_vector_test() ->
    Opts = opts(),
    ?assertEqual(
        {ok, true},
        validate(
            <<"authority">>,
            #{
                <<"authority">> => <<"\"alice\",\"bob\"">>
            },
            [<<"alice">>, <<"bob">>],
            Opts
        )
    ).

validate_route_uses_explicit_from_vector_test() ->
    Opts = opts(),
    ?assertEqual(
        {ok, true},
        validate(
            <<"set-authority">>,
            #{
                <<"set-authority">> => [<<"alice">>],
                <<"set-authority-required">> => [<<"alice">>]
            },
            <<"alice">>,
            Opts
        )
    ).

compute_hydrates_two_of_three_assignment_body_vector_test() ->
    Opts = opts(),
    {Scheduler, SchedulerWallet} = signer(),
    {AdminA, AdminAWallet} = signer(),
    {AdminB, AdminBWallet} = signer(),
    {AdminC, _AdminCWallet} = signer(),
    Body =
        sign_subject(
            #{ <<"action">> => <<"Set">> },
            [AdminAWallet, AdminBWallet],
            Opts
        ),
    Assignment =
        sign_subject(
            #{
                <<"path">> => <<"compute">>,
                <<"type">> => <<"Assignment">>,
                <<"slot">> => 0,
                <<"body">> => Body
            },
            SchedulerWallet,
            Opts
        ),
    CachedAssignment = cache_roundtrip(Assignment, Opts),
    CachedBody = hb_ao:get(<<"body">>, CachedAssignment, Opts),
    ?assertEqual(2, length(hb_message:signers(CachedBody, Opts))),
    ?assert(hb_message:verify(CachedBody, all, Opts)),
    ?assert(hb_message:verify(CachedAssignment, all, Opts)),
    {ok, SecuredAssignment} =
        hb_ao:resolve(
            base(#{ <<"scheduler">> => Scheduler }),
            CachedAssignment,
            Opts
        ),
    From = hb_ao:get(<<"body/from">>, SecuredAssignment, Opts),
    ?assertEqual(
        lists:sort([hb_committer:uri(AdminA), hb_committer:uri(AdminB)]),
        lists:sort(From)
    ),
    ?assertEqual(
        {ok, true},
        validate(
            <<"set-authority">>,
            #{
                <<"set-authority">> => [AdminA, AdminB, AdminC],
                <<"set-authority-match">> => 2
            },
            From,
            Opts
        )
    ).

delegated_transfer_action_allowed_vector_test() ->
    Opts = opts(),
    {Scheduler, SchedulerWallet} = signer(),
    {DelegatedSender, _DelegatedWallet} = signer(),
    Assignment =
        sign_assignment(
            #{
                <<"action">> => <<"tRaNsFeR">>,
                <<"from-process">> => DelegatedSender
            },
            SchedulerWallet,
            SchedulerWallet,
            Opts
        ),
    {ok, SecuredAssignment} =
        hb_ao:resolve(
            base(
                #{
                    <<"scheduler">> => Scheduler,
                    <<"authority">> => Scheduler,
                    <<"authority-actions">> => [<<"Transfer">>]
                }
            ),
            Assignment,
            Opts
        ),
    ?assertEqual(
        hb_committer:uri(DelegatedSender),
        hb_ao:get(<<"body/from">>, SecuredAssignment, Opts)
    ).

delegated_non_binary_sender_rejected_vector_test() ->
    Opts = opts(),
    {Scheduler, Wallet} = signer(),
    lists:foreach(
        fun(Sender) ->
            Assignment = sign_assignment(
                #{<<"action">> => <<"Transfer">>, <<"from-process">> => Sender},
                Wallet,
                Wallet,
                Opts
            ),
            ?assertEqual(
                {skip, <<"Address must be a binary.">>},
                hb_ao:resolve(
                    base(#{
                        <<"scheduler">> => Scheduler,
                        <<"authority">> => Scheduler,
                        <<"authority-actions">> => [<<"Transfer">>]
                    }),
                    Assignment,
                    Opts
                )
            )
        end,
        [12345, 1.5, true, #{<<"address">> => <<"bad">>}, [#{<<"x">> => 1}]]
    ).

delegated_extra_signer_rejected_vector_test() ->
    Opts = opts(),
    {Scheduler, SchedulerWallet} = signer(),
    {_External, ExternalWallet} = signer(),
    {DelegatedSender, _DelegatedWallet} = signer(),
    Assignment =
        sign_assignment(
            #{
                <<"action">> => <<"Transfer">>,
                <<"from-process">> => DelegatedSender
            },
            [SchedulerWallet, ExternalWallet],
            SchedulerWallet,
            Opts
        ),
    Base =
        base(
            #{
                <<"scheduler">> => Scheduler,
                <<"authority">> => Scheduler,
                <<"authority-actions">> => [<<"Transfer">>]
            }
        ),
    Rejected = {skip, <<"Delegated messages require exactly one signer.">>},
    ?assertEqual(Rejected, hb_ao:resolve(Base, Assignment, Opts)),
    ?assertEqual(
        Rejected,
        hb_ao:resolve(Base, cache_roundtrip(Assignment, Opts), Opts)
    ).

delegated_mint_action_rejected_vector_test() ->
    Opts = opts(),
    {Scheduler, SchedulerWallet} = signer(),
    {DelegatedSender, _DelegatedWallet} = signer(),
    Assignment =
        sign_assignment(
            #{
                <<"action">> => <<"Mint">>,
                <<"from-process">> => DelegatedSender
            },
            SchedulerWallet,
            SchedulerWallet,
            Opts
        ),
    ?assertEqual(
        {skip, <<"Delegated action not allowed.">>},
        hb_ao:resolve(
            base(
                #{
                    <<"scheduler">> => Scheduler,
                    <<"authority">> => Scheduler,
                    <<"authority-actions">> => [<<"Transfer">>]
                }
            ),
            Assignment,
            Opts
        )
    ).

delegated_action_policy_structured_fields_vector_test() ->
    Opts = opts(),
    {Scheduler, SchedulerWallet} = signer(),
    {DelegatedSender, _DelegatedWallet} = signer(),
    Assignment =
        sign_assignment(
            #{
                <<"action">> => <<"Transfer">>,
                <<"from-process">> => DelegatedSender
            },
            SchedulerWallet,
            SchedulerWallet,
            Opts
        ),
    Policy =
        #{
            <<"scheduler">> => Scheduler,
            <<"authority">> => Scheduler
        },
    ?assertEqual(
        {skip, <<"Delegated action not allowed.">>},
        hb_ao:resolve(base(Policy), Assignment, Opts)
    ),
    ?assertMatch(
        {ok, _},
        hb_ao:resolve(
            base(Policy#{ <<"authority-actions">> => <<"Transfer">> }),
            Assignment,
            Opts
        )
    ),
    ?assertEqual(
        {skip, <<"Delegated action not allowed.">>},
        hb_ao:resolve(
            base(Policy#{ <<"authority-actions">> => <<"@">> }),
            Assignment,
            Opts
        )
    ).

delegated_message_requires_action_vector_test() ->
    Opts = opts(),
    {Scheduler, SchedulerWallet} = signer(),
    {DelegatedSender, _DelegatedWallet} = signer(),
    Assignment =
        sign_assignment(
            #{ <<"from-process">> => DelegatedSender },
            SchedulerWallet,
            SchedulerWallet,
            Opts
        ),
    ?assertEqual(
        {skip, <<"Delegated action not allowed.">>},
        hb_ao:resolve(
            base(
                #{
                    <<"scheduler">> => Scheduler,
                    <<"authority">> => Scheduler,
                    <<"authority-actions">> => [<<"Transfer">>]
                }
            ),
            Assignment,
            Opts
        )
    ).

delegated_invalid_action_rejected_vector_test() ->
    Opts = opts(),
    {Scheduler, SchedulerWallet} = signer(),
    {DelegatedSender, _DelegatedWallet} = signer(),
    Assignment =
        sign_assignment(
            #{
                <<"action">> => <<255>>,
                <<"from-process">> => DelegatedSender
            },
            SchedulerWallet,
            SchedulerWallet,
            Opts
        ),
    ?assertEqual(
        {skip, <<"Delegated action not allowed.">>},
        hb_ao:resolve(
            base(
                #{
                    <<"scheduler">> => Scheduler,
                    <<"authority">> => Scheduler,
                    <<"authority-actions">> => [<<"Transfer">>]
                }
            ),
            Assignment,
            Opts
        )
    ).

wallet_action_does_not_require_delegated_policy_vector_test() ->
    Opts = opts(),
    {Scheduler, SchedulerWallet} = signer(),
    {WalletAddress, Wallet} = signer(),
    Assignment =
        sign_assignment(
            #{ <<"action">> => <<"Mint">> },
            Wallet,
            SchedulerWallet,
            Opts
        ),
    {ok, SecuredAssignment} =
        hb_ao:resolve(
            base(#{ <<"scheduler">> => Scheduler }),
            Assignment,
            Opts
        ),
    ?assertEqual(
        hb_committer:uri(WalletAddress),
        hb_ao:get(<<"body/from">>, SecuredAssignment, Opts)
    ).

raw_set_authority_static_policy_allows_exact_from_vector_test() ->
    Opts = opts(),
    Authority = <<1:256>>,
    ?assertEqual(
        {ok, true},
        validate(
            <<"set-authority">>,
            #{ <<"set-authority">> => Authority },
            Authority,
            Opts
        )
    ).

raw_set_authority_static_policy_rejects_non_matching_from_vector_test() ->
    Opts = opts(),
    Authority = <<1:256>>,
    Other = <<2:256>>,
    ?assertEqual(
        {error, <<"Too few acceptable committers present.">>},
        validate(
            <<"set-authority">>,
            #{ <<"set-authority">> => Authority },
            Other,
            Opts
        )
    ).

empty_set_authority_static_policy_rejected_vector_test() ->
    Opts = opts(),
    ?assertEqual(
        {error, <<"Security policy not configured.">>},
        validate(
            <<"set-authority">>,
            #{ <<"set-authority">> => [] },
            <<"alice">>,
            Opts
        )
    ).

empty_binary_set_authority_static_policy_rejected_vector_test() ->
    Opts = opts(),
    ?assertMatch(
        {error, _},
        validate(
            <<"set-authority">>,
            #{ <<"set-authority">> => <<>> },
            <<"alice">>,
            Opts
        )
    ).

set_authority_required_only_static_policy_allowed_vector_test() ->
    Opts = opts(),
    ?assertEqual(
        {ok, true},
        validate(
            <<"set-authority">>,
            #{ <<"set-authority-required">> => [<<"alice">>] },
            <<"alice">>,
            Opts
        )
    ),
    ?assertEqual(
        {error, <<"Required committers not present in message.">>},
        validate(
            <<"set-authority">>,
            #{ <<"set-authority-required">> => [<<"alice">>] },
            <<"bob">>,
            Opts
        )
    ).

set_authority_default_supply_owner_requires_token_state_vector_test() ->
    Opts = opts(),
    ?assertEqual(
        {error, <<"Balances not configured.">>},
        validate(<<"set-authority">>, #{}, <<"alice">>, Opts)
    ).

set_authority_default_supply_owner_allows_full_owner_vector_test() ->
    Opts = opts(),
    Owner = <<"alice">>,
    ?assertEqual(
        {ok, true},
        validate(
            <<"set-authority">>,
            token_policy(#{ Owner => 10 }, 10, Opts),
            Owner,
            Opts
        )
    ).

set_authority_default_supply_owner_rejects_non_owner_vector_test() ->
    Opts = opts(),
    ?assertEqual(
        {error, <<"Supply-threshold owner requirement not satisfied.">>},
        validate(
            <<"set-authority">>,
            token_policy(#{ <<"alice">> => 10 }, 10, Opts),
            <<"bob">>,
            Opts
        )
    ).

set_authority_default_supply_owner_rejects_split_supply_vector_test() ->
    Opts = opts(),
    ?assertEqual(
        {error, <<"Supply-threshold owner requirement not satisfied.">>},
        validate(
            <<"set-authority">>,
            token_policy(#{ <<"alice">> => 5, <<"bob">> => 5 }, 10, Opts),
            <<"alice">>,
            Opts
        )
    ).

set_authority_supply_threshold_bps_allows_half_owner_vector_test() ->
    Opts = opts(),
    ?assertEqual(
        {ok, true},
        validate(
            <<"set-authority">>,
            token_policy(
                #{ <<"alice">> => 5, <<"bob">> => 5 },
                10,
                #{
                    <<"set-authority-template">> => <<"supply-threshold-owner">>,
                    <<"set-authority-threshold-bps">> => 5000
                },
                Opts
            ),
            <<"alice">>,
            Opts
        )
    ).

set_authority_supply_owner_uses_canonical_id_vector_test() ->
    Opts = opts(),
    ?assertEqual(
        {ok, true},
        validate(
            <<"set-authority">>,
            token_policy(#{ <<"alice">> => 10 }, 10, Opts),
            <<"ALICE">>,
            Opts
        )
    ).

set_authority_supply_owner_uses_canonical_swap_id_vector_test() ->
    Opts = opts(),
    Owner = <<"Tb9KgNJ6lg0K9sdagS-mo7w28Nt9_K-9ITy_It7FNKU">>,
    Policy =
        token_policy(
            #{ Owner => 1 },
            1,
            #{ <<"swap-device">> => <<"arweave-swap@1.0">> },
            Opts
        ),
    ?assertEqual(
        {ok, true},
        validate(<<"set-authority">>, Policy, Owner, Opts)
    ),
    % A committer's URI keeps the case of its address: the address in
    % lowercase is another account.
    ?assertMatch(
        {error, _},
        validate(
            <<"set-authority">>,
            Policy,
            hb_util:to_lower(Owner),
            Opts
        )
    ).

set_authority_supply_owner_accepts_wire_supply_vector_test() ->
    Opts = opts(),
    ?assertEqual(
        {ok, true},
        validate(
            <<"set-authority">>,
            token_policy(#{ <<"alice">> => 1 }, <<"1">>, Opts),
            <<"alice">>,
            Opts
        )
    ).

set_authority_static_policy_takes_precedence_vector_test() ->
    Opts = opts(),
    Admin = <<"admin">>,
    Owner = <<"owner">>,
    Policy =
        token_policy(
            #{ Owner => 10 },
            10,
            #{ <<"set-authority">> => Admin },
            Opts
        ),
    ?assertEqual({ok, true}, validate(<<"set-authority">>, Policy, Admin, Opts)),
    ?assertEqual(
        {error, <<"Too few acceptable committers present.">>},
        validate(<<"set-authority">>, Policy, Owner, Opts)
    ).

explicit_supply_owner_template_rejects_non_set_authority_vector_test() ->
    Opts = opts(),
    ?assertEqual(
        {error, <<"Supply-threshold owner template only supports set-authority.">>},
        validate(
            <<"authority">>,
            token_policy(
                #{ <<"alice">> => 10 },
                10,
                #{ <<"authority-template">> => <<"supply-threshold-owner">> },
                Opts
            ),
            <<"alice">>,
            Opts
        )
    ).

explicit_supply_owner_template_rejects_static_keys_vector_test() ->
    Opts = opts(),
    Authority = <<"alice">>,
    ?assertEqual(
        {error, <<"Ambiguous security policy configuration.">>},
        validate(
            <<"set-authority">>,
            token_policy(
                #{ Authority => 10 },
                10,
                #{
                    <<"set-authority-template">> => <<"supply-threshold-owner">>,
                    <<"set-authority">> => Authority
                },
                Opts
            ),
            Authority,
            Opts
        )
    ).

unknown_set_authority_template_rejected_vector_test() ->
    Opts = opts(),
    ?assertEqual(
        {error, <<"Unknown security template.">>},
        validate(
            <<"set-authority">>,
            token_policy(
                #{ <<"alice">> => 10 },
                10,
                #{ <<"set-authority-template">> => <<"unknown-template">> },
                Opts
            ),
            <<"alice">>,
            Opts
        )
    ).

set_authority_match_only_static_policy_rejected_vector_test() ->
    Opts = opts(),
    ?assertEqual(
        {error, <<"Security policy not configured.">>},
        validate(
            <<"set-authority">>,
            #{ <<"set-authority-match">> => 0 },
            <<"alice">>,
            Opts
        )
    ).

set_authority_supply_owner_rejects_path_candidate_vector_test() ->
    Opts = opts(),
    ?assertEqual(
        {error, <<"Address contains unsupported characters.">>},
        validate(
            <<"set-authority">>,
            token_policy(#{ <<"alice">> => 10 }, 10, Opts),
            <<"alice/bob">>,
            Opts
        )
    ).

set_authority_supply_owner_rejects_invalid_threshold_vector_test() ->
    Opts = opts(),
    ?assertEqual(
        {error, <<"Threshold basis points out of range.">>},
        validate(
            <<"set-authority">>,
            token_policy(
                #{ <<"alice">> => 10 },
                10,
                #{
                    <<"set-authority-template">> => <<"supply-threshold-owner">>,
                    <<"set-authority-threshold-bps">> => 0
                },
                Opts
            ),
            <<"alice">>,
            Opts
        )
    ).

prod_mode_requires_explicit_policy_vector_test() ->
    Opts = (opts())#{ dev_security_mode => prod },
    ?assertEqual(
        {error, <<"Security policy not configured.">>},
        validate(<<"authority">>, #{}, [<<"alice">>], Opts)
    ).

prod_mode_allows_explicit_single_signer_policy_vector_test() ->
    Opts = (opts())#{ dev_security_mode => prod },
    ?assertEqual(
        {ok, true},
        validate(
            <<"authority">>,
            #{
                <<"authority">> => <<"alice">>
            },
            [<<"alice">>],
            Opts
        )
    ).

delegated_action_policy_decodes_tokens_as_binaries_vector_test() ->
    Opts = opts(),
    {Scheduler, SchedulerWallet} = signer(),
    {DelegatedSender, _DelegatedWallet} = signer(),
    true = is_atom(compute),
    Assignment =
        sign_assignment(
            #{
                <<"action">> => <<"compute">>,
                <<"from-process">> => DelegatedSender
            },
            SchedulerWallet,
            SchedulerWallet,
            Opts
        ),
    Policy =
        #{
            <<"scheduler">> => Scheduler,
            <<"authority">> => Scheduler,
            <<"authority-actions">> => <<"compute">>
        },
    ?assertMatch({ok, _}, hb_ao:resolve(base(Policy), Assignment, Opts)).
