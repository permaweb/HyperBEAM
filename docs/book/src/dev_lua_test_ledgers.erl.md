# dev_lua_test_ledgers

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_lua_test_ledgers.erl)

A collection of Eunit tests for the `lua@5.3a` device, and the 
`hyper-token.lua` script. These tests are designed to validate the
functionality of both of these components, and to provide examples
of how to use the `lua@5.3a` device.
The module is split into four components:
1. A simple ledger client library.
2. Assertion functions that verify specific invariants about the state
   of ledgers in a test environment.
3. Utility functions for normalizing the state of a test environment.
4. Test cases that generate and manipulate ledger networks in test
   environments.
Many client and utility functions in this module handle the conversion of
wallet IDs to human-readable addresses when found in transfers, balances,
and other fields. This is done to make the test cases more readable and
easier to understand -- be careful if following their patterns in other
contexts to either mimic a similar pattern, or to ensure you pass addresses
in these contexts rather that full wallet objects.

---

### ledger

Generate a Lua process definition message.

```erlang
ledger(Script, Opts) ->
    ledger(Script, #{}, Opts).
```

### ledger

```erlang
ledger(Script, Extra, Opts) ->
    % If the `balance' key is set in the `Extra' map, ensure that any wallets
    % given as keys in the message are converted to human-readable addresses.
```

### lua_script

Generate a Lua `script` key from a file or list of files.

```erlang
lua_script(Files) when is_list(Files) ->
    [
        #{
            <<"content-type">> => <<"application/lua">>,
            <<"module">> => File,
            <<"body">> =>
                hb_util:ok(
                    file:read_file(
                        if is_binary(File) -> binary_to_list(File);
                           true -> File
                        end
                    )
                )
        }
    ||
        File <- Files
    ];
```

### lua_script

Generate a Lua `script` key from a file or list of files.

```erlang
lua_script(File) when is_binary(File) ->
    hd(lua_script([File])).
```

### subledger

Generate a test sub-ledger process definition message.

```erlang
subledger(Root, Opts) ->
    subledger(Root, #{}, Opts).
```

### subledger

```erlang
subledger(Root, Extra, Opts) ->
    BareRoot =
        maps:without(
            [<<"token">>, <<"balance">>],
            hb_message:uncommitted(Root)
        ),
    Proc = 
        hb_message:commit(
            maps:merge(
                BareRoot#{
                    <<"token">> => hb_message:id(Root, all)
                },
                Extra
            ),
            hb_opts:get(priv_wallet, hb:wallet(), Opts)
        ),
    hb_cache:write(Proc, Opts),
    Proc.
```

### transfer

Generate a test transfer message.

```erlang
transfer(ProcMsg, Sender, Recipient, Quantity, Opts) ->
    transfer(ProcMsg, Sender, Recipient, Quantity, undefined, Opts).
```

### transfer

```erlang
transfer(ProcMsg, Sender, Recipient, Quantity, Route, Opts) ->
    MaybeRoute =
        if Route == undefined -> #{};
           true ->
                #{
                    <<"route">> =>
                        if is_map(Route) -> hb_message:id(Route, all);
                        true -> Route
                        end
                }
        end,
    Xfer =
        hb_message:commit(#{
            <<"path">> => <<"push">>,
            <<"body">> =>
                hb_message:commit(MaybeRoute#{
                        <<"action">> => <<"Transfer">>,
                        <<"target">> => hb_message:id(ProcMsg, all),
                        <<"recipient">> => hb_util:human_id(Recipient),
                        <<"quantity">> => Quantity
                    },
                    Sender
                )
            },
            Sender
        ),
    hb_ao:resolve(
        ProcMsg,
        Xfer,
        Opts#{ priv_wallet => hb_opts:get(priv_wallet, hb:wallet(), Opts) }
    ).
```

### register

Request that a peer register with a without sub-ledger.

```erlang
register(ProcMsg, Peer, Opts) when is_map(Peer) ->
    register(ProcMsg, hb_message:id(Peer, all), Opts);
```

### register

Request that a peer register with a without sub-ledger.

```erlang
register(ProcMsg, PeerID, RawOpts) ->
    Opts =
        RawOpts#{
            priv_wallet => hb_opts:get(priv_wallet, hb:wallet(), RawOpts)
        },
    Reg =
        hb_message:commit(
            #{
                <<"path">> => <<"push">>,
                <<"body">> =>
                    hb_message:commit(
                        #{
                            <<"action">> => <<"register-remote">>,
                            <<"target">> => hb_message:id(ProcMsg, all),
                            <<"peer">> => PeerID
                        },
                        Opts
                    )
            },
            Opts
        ),
    hb_ao:resolve(
        ProcMsg,
        Reg,
        Opts
    ).
```

### balance

Retreive a single balance from the ledger.

```erlang
balance(ProcMsg, User, Opts) when not ?IS_ID(User) ->
    balance(ProcMsg, hb_util:human_id(ar_wallet:to_address(User)), Opts);
```

### balance

Retreive a single balance from the ledger.
Get the total balance for an ID across all ledgers in a set.

```erlang
balance(ProcMsg, ID, Opts) ->
    hb_ao:get(<<"now/balance/", ID/binary>>, ProcMsg, 0, Opts).
```

### balance_total

Retreive a single balance from the ledger.
Get the total balance for an ID across all ledgers in a set.

```erlang
balance_total(Procs, ID, Opts) ->
    lists:sum(
        lists:map(
            fun(Proc) -> balance(Proc, ID, Opts) end,
            maps:values(normalize_env(Procs))
        )
    ).
```

### balances

Get the balances of a ledger.

```erlang
balances(ProcMsg, Opts) ->
    balances(now, ProcMsg, Opts).
```

### balances

```erlang
balances(initial, ProcMsg, Opts) ->
    balances(<<"">>, ProcMsg, Opts);
```

### balances

```erlang
balances(Mode, ProcMsg, Opts) when is_atom(Mode) ->
    balances(hb_util:bin(Mode), ProcMsg, Opts);
```

### balances

```erlang
balances(Prefix, ProcMsg, Opts) ->
    Balances = hb_ao:get(<<Prefix/binary, "/balance">>, ProcMsg, #{}, Opts),
    hb_private:reset(hb_cache:ensure_all_loaded(Balances, Opts)).
```

### supply

Get the supply of a ledger, either `now` or `initial`.

```erlang
supply(ProcMsg, Opts) ->
    supply(now, ProcMsg, Opts).
```

### supply

```erlang
supply(Mode, ProcMsg, Opts) ->
    lists:sum(maps:values(balances(Mode, ProcMsg, Opts))).
```

### subledger_supply

Calculate the supply of tokens in all sub-ledgers, from the balances of

```erlang
subledger_supply(RootProc, AllProcs, Opts) ->
    supply(now, RootProc, Opts) - user_supply(RootProc, AllProcs, Opts).
```

### user_supply

Calculate the supply of tokens held by users on a ledger, excluding

```erlang
user_supply(Proc, AllProcs, Opts) ->
    NormProcs = normalize_without_root(Proc, AllProcs),
    SubledgerIDs = maps:keys(NormProcs),
    lists:sum(
        maps:values(
            maps:without(
                SubledgerIDs,
                balances(now, Proc, Opts)
            )
        )
    ).
```

### ledgers

Get the local expectation of a ledger's balances with peer ledgers.

```erlang
ledgers(ProcMsg, Opts) ->
    case hb_cache:ensure_all_loaded(
        hb_ao:get(<<"now/ledgers">>, ProcMsg, #{}, Opts),
        Opts
    ) of
        Msg when is_map(Msg) -> hb_private:reset(Msg);
        [] -> #{}
    end.
```

### map

Generate a complete overview of the test environment's balances and 

```erlang
map(Procs, Opts) ->
    NormProcs = normalize_env(Procs),
    maps:merge_with(
        fun(Key, Balances, Ledgers) ->
            MaybeRoot =
                case maps:get(Key, NormProcs, #{}) of
                    #{ <<"token">> := _ } -> #{};
                    _ -> #{ root => true }
                end,
            MaybeRoot#{
                balances => Balances,
                ledgers => Ledgers
            }
        end,
        maps:map(fun(_, Proc) -> balances(Proc, Opts) end, NormProcs),
        maps:map(fun(_, Proc) -> ledgers(Proc, Opts) end, NormProcs)
    ).
```

### map

```erlang
map(Procs, EnvNames, Opts) ->
    apply_names(map(Procs, Opts), EnvNames, Opts).
```

### apply_names

Apply a map of environment names to elements in either a map or list.

```erlang
apply_names(Map, EnvNames, Opts) ->
    IDs =
        maps:from_list(
            lists:filtermap(
                fun({Key, V}) ->
                    try {true, {hb_util:human_id(Key), V}}
                    catch _:_ ->
                        try {true, {hb_message:id(Key, all), V}}
                        catch _:_ -> false
                        end
                    end
                end,
                maps:to_list(EnvNames)
            )
        ),
    do_apply_names(Map, maps:merge(IDs, EnvNames), Opts).
```

### do_apply_names

```erlang
do_apply_names(Map, EnvNames, Opts) when is_map(Map) ->
    maps:from_list(
        lists:map(
            fun({Key, Proc}) ->
                {
                    apply_names(Key, EnvNames, Opts),
                    apply_names(Proc, EnvNames, Opts)
                }
            end,
            maps:to_list(Map)
        )
    );
```

### do_apply_names

```erlang
do_apply_names(List, EnvNames, Opts) when is_list(List) ->
    lists:map(
        fun(Proc) ->
            apply_names(Proc, EnvNames, Opts)
        end,
        List
    );
```

### do_apply_names

```erlang
do_apply_names(Item, Names, _Opts) when is_map_key(Item, Names) ->
    maps:get(Item, Names);
```

### do_apply_names

```erlang
do_apply_names(Item, Names, _Opts) ->
    try maps:get(hb_util:human_id(Item), Names, Item)
    catch _:_ -> Item
    end.
```

### verify_net

Execute all invariant checks for a pair of root ledger and sub-ledgers.

```erlang
verify_net(RootProc, AllProcs, Opts) ->
    verify_net_supply(RootProc, AllProcs, Opts),
    verify_net_peer_balances(AllProcs, Opts).
```

### verify_root_supply

Verify that the initial supply of tokens on the root ledger is the same

```erlang
verify_root_supply(RootProc, Opts) ->
    ?assert(
        supply(initial, RootProc, Opts) ==
        supply(now, RootProc, Opts) +
            lists:sum(maps:values(ledgers(RootProc, Opts)))
    ).
```

### verify_net_supply

Verify that the sum of all spendable balances held by ledgers in a

```erlang
verify_net_supply(RootProc, AllProcs, Opts) ->
    verify_root_supply(RootProc, Opts),
    StartingRootSupply = supply(initial, RootProc, Opts),
    NormProcsWithoutRoot = normalize_without_root(RootProc, AllProcs),
    SubledgerIDs = maps:keys(NormProcsWithoutRoot),
    RootUserSupply = user_supply(RootProc, NormProcsWithoutRoot, Opts),
    SubledgerSupply = subledger_supply(RootProc, AllProcs, Opts),
    ?event({verify_net_supply, {root, RootUserSupply}, {subledger, SubledgerSupply}}),
    ?assert(
        StartingRootSupply ==
        RootUserSupply + SubledgerSupply
    ).
```

### verify_net_peer_balances

Verify the consistency of all expected ledger balances with their peer

```erlang
verify_net_peer_balances(AllProcs, Opts) ->
    NormProcs = normalize_env(AllProcs),
    maps:map(
        fun(ValidateProc, _) ->
            verify_peer_balances(ValidateProc, NormProcs, Opts)
        end,
        NormProcs
    ).
```

### verify_peer_balances

Verify that a ledger's expectation of its balances with peer ledgers

```erlang
verify_peer_balances(ValidateProc, AllProcs, Opts) ->
    Ledgers = ledgers(ValidateProc, Opts),
    NormProcs = normalize_env(AllProcs),
    maps:map(
        fun(PeerID, ExpectedBalance) ->
            ?assertEqual(
                ExpectedBalance,
                balance(ValidateProc,
                    maps:get(PeerID, NormProcs),
                    Opts
                )
            )
        end,
        Ledgers
    ).
```

### normalize_env

Normalize a set of processes, representing ledgers in a test environment,

```erlang
normalize_env(Procs) when is_map(Procs) ->
    normalize_env(maps:values(Procs));
```

### normalize_env

Normalize a set of processes, representing ledgers in a test environment,

```erlang
normalize_env(Procs) when is_list(Procs) ->
    maps:from_list(
        lists:map(
            fun(Proc) ->
                {hb_message:id(Proc, all), Proc}
            end,
            Procs
        )
    ).
```

### normalize_without_root

Return the normalized environment without the root ledger.

```erlang
normalize_without_root(RootProc, Procs) ->
    maps:without([hb_message:id(RootProc, all)], normalize_env(Procs)).
```

### test_opts

Create a node message for the test that avoids looking up unknown 

```erlang
test_opts() ->
    hb:init(),
    #{}.
```

### transfer_test_

Test the `transfer` function.

```erlang
transfer_test_() -> {timeout, 30, fun transfer/0}.
```

### transfer

Test the `transfer` function.

```erlang
transfer() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    Proc =
        ledger(
            <<"scripts/hyper-token.lua">>,
            #{ <<"balance">> => #{ Alice => 100 } },
            Opts
        ),
    ?assertEqual(100, supply(Proc, Opts)),
    transfer(Proc, Alice, Bob, 1, Opts),
    ?assertEqual(99, balance(Proc, Alice, Opts)),
    ?assertEqual(1, balance(Proc, Bob, Opts)),
    ?assertEqual(100, supply(Proc, Opts)).
```

### transfer_unauthorized_test_

User's must not be able to send tokens they do not own. We test three

```erlang
transfer_unauthorized_test_() -> {timeout, 30, fun transfer_unauthorized/0}.
```

### transfer_unauthorized

User's must not be able to send tokens they do not own. We test three

```erlang
transfer_unauthorized() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    Proc =
        ledger(
            <<"scripts/hyper-token.lua">>,
            #{ <<"balance">> => #{ Alice => 100 } },
            Opts
        ),
    % 1. Transferring a token when the sender has no tokens.
```

### subledger_deposit_test_

Verify that a user can deposit tokens into a sub-ledger.

```erlang
subledger_deposit_test_() -> {timeout, 30, fun subledger_deposit/0}.
```

### subledger_deposit

Verify that a user can deposit tokens into a sub-ledger.

```erlang
subledger_deposit() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    Proc =
        ledger(
            <<"scripts/hyper-token.lua">>,
            #{ <<"balance">> => #{ Alice => 100 } },
            Opts
        ),
    SubLedger = subledger(Proc, Opts),
    % 1. Alice has tokens on the root ledger.
```

### subledger_transfer_test_

Simulate inter-ledger payments between users on a single sub-ledger:

```erlang
subledger_transfer_test_() -> {timeout, 10, fun subledger_transfer/0}.
```

### subledger_transfer

Simulate inter-ledger payments between users on a single sub-ledger:

```erlang
subledger_transfer() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    RootLedger =
        ledger(
            <<"scripts/hyper-token.lua">>,
            #{ <<"balance">> => #{ Alice => 100 } },
            Opts
        ),
    SubLedger = subledger(RootLedger, Opts),
    EnvNames = #{
        Alice => alice,
        Bob => bob,
        RootLedger => root,
        SubLedger => subledger
    },
    % 1. Alice has tokens on the root ledger.
```

### subledger_registration_test_disabled

Verify that peer ledgers on the same token are able to register mutually

```erlang
subledger_registration_test_disabled() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    RootLedger =
        ledger(
            <<"scripts/hyper-token.lua">>,
            #{ <<"balance">> => #{ Alice => 100 } },
            Opts
        ),
    SubLedger1 = subledger(RootLedger, Opts),
    SubLedger2 = subledger(RootLedger, Opts),
    Names = #{
        SubLedger1 => subledger1,
        SubLedger2 => subledger2
    },
    ?event(debug,
        {subledger,
            {sl1, hb_message:id(SubLedger1, none)},
            {sl2, hb_message:id(SubLedger2, none)}
        }
    ),
    % There are no registered peers on either sub-ledger.
```

### single_subledger_to_subledger_test_

```erlang
single_subledger_to_subledger_test_() -> {timeout, 30, fun single_subledger_to_subledger/0}.
```

### single_subledger_to_subledger

```erlang
single_subledger_to_subledger() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    RootLedger =
        ledger(
            <<"scripts/hyper-token.lua">>,
            #{ <<"balance">> => #{ Alice => 100 } },
            Opts
        ),
    SubLedger1 = subledger(RootLedger, Opts),
    SL1ID = hb_message:id(SubLedger1, signed, Opts),
    ?event({sl1ID, SL1ID}),
    SubLedger2 = subledger(RootLedger, Opts),
    SL2ID = hb_message:id(SubLedger2, signed, Opts),
    ?event({sl2ID, SL2ID}),
    Names = #{
        Alice => alice,
        Bob => bob,
        RootLedger => root,
        SubLedger1 => subledger1,
        SubLedger2 => subledger2
    },
    ?event({root_ledger, RootLedger}),
    ?event({sl1, SubLedger1}),
    ?event({sl2, SubLedger2}),
    ?assertEqual(100, balance(RootLedger, Alice, Opts)),
    % 2. Alice sends 90 tokens to herself on SubLedger1.
```

### subledger_to_subledger_test_

Verify that registered sub-ledgers are able to send tokens to each other

```erlang
subledger_to_subledger_test_() -> {timeout, 30, fun subledger_to_subledger/0}.
```

### subledger_to_subledger

Verify that registered sub-ledgers are able to send tokens to each other

```erlang
subledger_to_subledger() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    RootLedger =
        ledger(
            <<"scripts/hyper-token.lua">>,
            #{ <<"balance">> => #{ Alice => 100 } },
            Opts
        ),
    SubLedger1 = subledger(RootLedger, Opts),
    SubLedger2 = subledger(RootLedger, Opts),
    Names = #{
        Alice => alice,
        Bob => bob,
        RootLedger => root,
        SubLedger1 => subledger1,
        SubLedger2 => subledger2
    },
    % 1. Alice has tokens on the root ledger.
```

### unregistered_peer_transfer_test_

Verify that a ledger can send tokens to a peer ledger that is not

```erlang
unregistered_peer_transfer_test_() -> {timeout, 30, fun unregistered_peer_transfer/0}.
```

### unregistered_peer_transfer

Verify that a ledger can send tokens to a peer ledger that is not

```erlang
unregistered_peer_transfer() ->
    Opts = #{},
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    RootLedger =
        ledger(
            <<"scripts/hyper-token.lua">>,
            #{ <<"balance">> => #{ Alice => 100 } },
            Opts
        ),
    SubLedgers = [ subledger(RootLedger, Opts) || _ <- lists:seq(1, 3) ],
    SubLedger1 = lists:nth(1, SubLedgers),
    SubLedger2 = lists:nth(2, SubLedgers),
    SubLedger3 = lists:nth(3, SubLedgers),
    Names = #{
        Alice => alice,
        Bob => bob,
        RootLedger => root,
        SubLedger1 => subledger1,
        SubLedger2 => subledger2,
        SubLedger3 => subledger3
    },
    % 1. Alice has tokens on the root ledger.
```

### multischeduler_test_disabled

Verify that sub-ledgers can request and enforce multiple scheduler

```erlang
multischeduler_test_disabled() -> {timeout, 30, fun multischeduler/0}.
```

### multischeduler

Verify that sub-ledgers can request and enforce multiple scheduler

```erlang
multischeduler() ->
    BaseOpts = test_opts(),
    NodeWallet = ar_wallet:new(),
    Scheduler2 = ar_wallet:new(),
    Scheduler3 = ar_wallet:new(),
    Opts = BaseOpts#{
        priv_wallet => NodeWallet,
        identities => #{
            <<"extra-scheduler">> => #{
                priv_wallet => Scheduler2
            }
        }
    },
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    RootLedger =
        ledger(
            <<"scripts/hyper-token.lua">>,
            ProcExtra = 
                #{
                    <<"balance">> => #{ Alice => 100 },
                    <<"scheduler">> =>
                        [
                            hb_util:human_id(NodeWallet),
                            hb_util:human_id(Scheduler2)
                        ],
                    <<"scheduler-required">> =>
                        [
                            hb_util:human_id(NodeWallet)
                        ]
                },
            Opts
        ),
    % Alice has tokens on the root ledger. She moves them to Bob.
```

### comma_separated_scheduler_list_test

Ensure that the `hyper-token.lua` script can parse comma-separated

```erlang
comma_separated_scheduler_list_test() ->
    NodeWallet = hb:wallet(),
    Scheduler2 = ar_wallet:new(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    Opts = (test_opts())#{ priv_wallet => NodeWallet, identities => #{
        <<"extra-scheduler">> => #{
            priv_wallet => Scheduler2
        }
    } },
    Ledger =
        ledger(
            <<"scripts/hyper-token.lua">>,
            ProcExtra = 
                #{
                    <<"balance">> => #{ Alice => 100 },
                    <<"scheduler">> =>
                        iolist_to_binary(
                            [
                                <<"\"">>,
                                hb_util:human_id(NodeWallet),
                                <<"\",\"">>,
                                hb_util:human_id(Scheduler2),
                                <<"\"">>
                            ]
                        ),
                    <<"scheduler-required">> =>
                        [
                            hb_util:human_id(NodeWallet)
                        ]
                },
            Opts
        ),
    % Alice has tokens on the root ledger. She moves them to Bob.
```

---

*Generated from [dev_lua_test_ledgers.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_lua_test_ledgers.erl)*
