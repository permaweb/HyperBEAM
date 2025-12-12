%%% A collection of Eunit tests for the `lua@5.3a` device, and the 
%%% `hyper-token.lua` script. These tests are designed to validate the
%%% functionality of both of these components, and to provide examples
%%% of how to use the `lua@5.3a` device.
%%% 
%%% The module is split into four components:
%%% 1. A simple ledger client library.
%%% 2. Assertion functions that verify specific invariants about the state
%%%    of ledgers in a test environment.
%%% 3. Utility functions for normalizing the state of a test environment.
%%% 4. Test cases that generate and manipulate ledger networks in test
%%%    environments.
%%% 
%%% Many client and utility functions in this module handle the conversion of
%%% wallet IDs to human-readable addresses when found in transfers, balances,
%%% and other fields. This is done to make the test cases more readable and
%%% easier to understand -- be careful if following their patterns in other
%%% contexts to either mimic a similar pattern, or to ensure you pass addresses
%%% in these contexts rather that full wallet objects.
-module(dev_lua_test_ledgers).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/hb.hrl").
-export([lua_script/1]).

%%% Helper functions and wrappers for `dev_token_lib'.

ledger(Script, Extra, Opts) ->
    dev_token_lib:ledger(
        Extra#{
            <<"execution-device">> => <<"lua@5.3a">>,
            <<"module">> => lua_script(Script)
        },
        Opts
    ).

%% @doc Generate a Lua `script' key from a file or list of files.
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
lua_script(File) when is_binary(File) ->
    hd(lua_script([File])).

%%% Test cases.

%% @doc Test the `transfer` function.
%% 1. Alice has 100 tokens on a root ledger.
%% 2. Alice sends 1 token to Bob.
%% 3. Alice has 99 tokens, and Bob has 1 token.
transfer_test_() -> {timeout, 30, fun transfer/0}.
transfer() ->
    Opts = dev_token_props:opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    Proc =
        ledger(
            <<"scripts/hyper-token.lua">>,
            #{ <<"balances">> => #{ Alice => 100 } },
            Opts
        ),
    ?assertEqual(100, dev_token_lib:supply(Proc, Opts)),
    dev_token_lib:transfer(Proc, Alice, Bob, 1, Opts),
    ?assertEqual(99, dev_token_lib:balance(Proc, Alice, Opts)),
    ?assertEqual(1, dev_token_lib:balance(Proc, Bob, Opts)),
    ?assertEqual(100, dev_token_lib:supply(Proc, Opts)).

%% @doc User's must not be able to send tokens they do not own. We test three
%% cases:
%% 1. Transferring a token when the sender has no tokens.
%% 2. Transferring a token when the sender has less tokens than the amount
%%    being transferred.
%% 3. Transferring a binary-encoded amount of tokens that exceed the quantity
%%    of tokens the sender has available.
transfer_unauthorized_test_() -> {timeout, 30, fun transfer_unauthorized/0}.
transfer_unauthorized() ->
    Opts = dev_token_props:opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    Proc =
        ledger(
            <<"scripts/hyper-token.lua">>,
            #{ <<"balances">> => #{ Alice => 100 } },
            Opts
        ),
    % 1. Transferring a token when the sender has no tokens.
    Result = dev_token_lib:transfer(Proc, Bob, Alice, 1, Opts),
    ?event({unauthorized_transfer, {result, Result}}),
    % 2. Transferring a token when the sender has less tokens than the amount
    %    being transferred.
    dev_token_lib:transfer(Proc, Alice, Bob, 101, Opts),
    ?event({unauthorized_transfer, {result, Result}}),
    ?event({env, dev_token_lib:map([Proc], #{ Alice => alice, Bob => bob }, Opts)}),
    ?assertEqual(100, dev_token_lib:balance(Proc, Alice, Opts)),
    ?assertEqual(0, dev_token_lib:balance(Proc, Bob, Opts)),
    % 3. Transferring a binary-encoded amount of tokens that exceed the quantity
    %    of tokens the sender has available.
    dev_token_lib:transfer(Proc, Alice, Bob, <<"101">>, Opts),
    ?assertEqual(100, dev_token_lib:balance(Proc, Alice, Opts)),
    ?assertEqual(0, dev_token_lib:balance(Proc, Bob, Opts)),
    % Validate the final supply of tokens.
    ?assertEqual(100, dev_token_lib:supply(Proc, Opts)).

%% @doc Verify that a user can deposit tokens into a sub-ledger.
subledger_deposit_test_() -> {timeout, 30, fun subledger_deposit/0}.
subledger_deposit() ->
    Opts = dev_token_props:opts(),
    Alice = ar_wallet:new(),
    Proc =
        ledger(
            <<"scripts/hyper-token.lua">>,
            #{ <<"balances">> => #{ Alice => 100 } },
            Opts
        ),
    SubLedger = dev_token_lib:subledger(Proc, Opts),
    % 1. Alice has tokens on the root ledger.
    ?assertEqual(100, dev_token_lib:balance(Proc, Alice, Opts)),
    % 2. Alice deposits tokens into the sub-ledger.
    dev_token_lib:transfer(Proc, Alice, Alice, 10, SubLedger, Opts),
    ?event({after_deposit, {result, dev_token_lib:map([Proc, SubLedger], Opts)} }),
    ?assertEqual(90, dev_token_lib:balance(Proc, Alice, Opts)),
    ?assertEqual(10, dev_token_lib:balance(SubLedger, Alice, Opts)),
    % Verify all invariants.
    dev_token_lib:verify_net(Proc, [SubLedger], Opts).

%% @doc Simulate inter-ledger payments between users on a single sub-ledger:
%% 1. Alice has tokens on the root ledger.
%% 2. Alice sends tokens to the sub-ledger from the root ledger.
%% 3. Alice sends tokens to Bob on the sub-ledger.
%% 4. Bob sends tokens to Alice on the root ledger.
subledger_transfer_test_() -> {timeout, 10, fun subledger_transfer/0}.
subledger_transfer() ->
    Opts = dev_token_props:opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    RootLedger =
        ledger(
            <<"scripts/hyper-token.lua">>,
            #{ <<"balances">> => #{ Alice => 100 } },
            Opts
        ),
    SubLedger = dev_token_lib:subledger(RootLedger, Opts),
    EnvNames = #{
        Alice => alice,
        Bob => bob,
        RootLedger => root,
        SubLedger => subledger
    },
    % 1. Alice has tokens on the root ledger.
    ?assertEqual(100, dev_token_lib:balance(RootLedger, Alice, Opts)),
    ?event(token_log, {map, dev_token_lib:map([RootLedger], EnvNames, Opts)}),
    % 2. Alice sends tokens to the sub-ledger from the root ledger.
    dev_token_lib:transfer(RootLedger, Alice, Alice, 10, SubLedger, Opts),
    ?assertEqual(90, dev_token_lib:balance(RootLedger, Alice, Opts)),
    ?assertEqual(10, dev_token_lib:balance(SubLedger, Alice, Opts)),
    % 3. Alice sends tokens to Bob on the sub-ledger.
    dev_token_lib:transfer(SubLedger, Alice, Bob, 8, Opts),
    ?event(token_log, 
        {state_after_subledger_user_xfer,
            {names, dev_token_lib:map([RootLedger, SubLedger], EnvNames, Opts)},
            {ids, dev_token_lib:map([RootLedger, SubLedger], Opts)}
        }),
    % 4. Bob sends tokens to Alice on the root ledger.
    dev_token_lib:transfer(SubLedger, Bob, Bob, 7, RootLedger, Opts),
    % Validate the balances of the root and sub-ledgers.
    Map = dev_token_lib:map([RootLedger, SubLedger], EnvNames, Opts),
    ?event(token_log, {map, dev_token_lib:map([RootLedger, SubLedger], Opts)}),
    ?assertEqual(
        #{
            root => #{
                balances => #{ alice => 90, bob => 7.0, subledger => 3.0 },
                ledgers => #{},
                root => true
            },
            subledger => #{
                balances => #{ alice => 2, bob => 1 },
                ledgers => #{}
            }
        },
        Map
    ),
    % Validate all invariants.
    dev_token_lib:verify_net(RootLedger, [SubLedger], Opts).

%% @doc Verify that peer ledgers on the same token are able to register mutually
%% to establish a peer-to-peer connection.
%% 
%% Disabled as explicit peer registration is not required for `hyper-token.lua'
%% to function.
subledger_registration_test_disabled() ->
    Opts = dev_token_props:opts(),
    Alice = ar_wallet:new(),
    RootLedger =
        ledger(
            <<"scripts/hyper-token.lua">>,
            #{ <<"balances">> => #{ Alice => 100 } },
            Opts
        ),
    SubLedger1 = dev_token_lib:subledger(RootLedger, Opts),
    SubLedger2 = dev_token_lib:subledger(RootLedger, Opts),
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
    ?assertEqual(0, map_size(dev_token_lib:ledgers(SubLedger1, Opts))),
    ?assertEqual(0, map_size(dev_token_lib:ledgers(SubLedger2, Opts))),
    % Alice registers with SubLedger1.
    dev_token_lib:register(SubLedger1, SubLedger2, Opts),
    ?event({map, dev_token_lib:map([SubLedger1, SubLedger2], Names, Opts)}),
    ?event({sl1_ledgers, dev_token_lib:ledgers(SubLedger1, Opts)}),
    ?event({sl2_ledgers, dev_token_lib:ledgers(SubLedger2, Opts)}),
    % SubLedger1 and SubLedger2 are now aware of each other.
    ?assertEqual(1, map_size(dev_token_lib:ledgers(SubLedger1, Opts))),
    ?assertEqual(1, map_size(dev_token_lib:ledgers(SubLedger2, Opts))),
    % Alice can send tokens to Bob on SubLedger2.
    dev_token_lib:verify_net(RootLedger, [SubLedger1, SubLedger2], Opts).

single_subledger_to_subledger_test_() ->
    {timeout, 30, fun single_subledger_to_subledger/0}.
single_subledger_to_subledger() ->
    Opts = dev_token_props:opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    RootLedger =
        ledger(
            <<"scripts/hyper-token.lua">>,
            #{ <<"balances">> => #{ Alice => 100 } },
            Opts
        ),
    SubLedger1 = dev_token_lib:subledger(RootLedger, Opts),
    SL1ID = hb_message:id(SubLedger1, signed, Opts),
    ?event({sl1ID, SL1ID}),
    SubLedger2 = dev_token_lib:subledger(RootLedger, Opts),
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
    % 1. At start, Alice has 100 tokens on the root ledger.
    ?assertEqual(100, dev_token_lib:balance(RootLedger, Alice, Opts)),
    % 2. Alice sends 90 tokens to herself on SubLedger1.
    dev_token_lib:transfer(RootLedger, Alice, Alice, 90, SubLedger1, Opts),
    ?event({state2, dev_token_lib:map([RootLedger, SubLedger1, SubLedger2], Names, Opts)}),
    ?assertEqual(10, dev_token_lib:balance(RootLedger, Alice, Opts)),
    ?assertEqual(90, dev_token_lib:balance(SubLedger1, Alice, Opts)),
    % 3. Alice sends 80 tokens to herself on SubLedger2.
    PushRes = dev_token_lib:transfer(SubLedger1, Alice, Alice, 80, SubLedger2, Opts),
    ?event({push_res, PushRes}),
    ?event({state3, dev_token_lib:map([RootLedger, SubLedger1, SubLedger2], Names, Opts)}),
    ?assertEqual(80, dev_token_lib:balance(SubLedger2, Alice, Opts)),
    ?assertEqual(10, dev_token_lib:balance(SubLedger1, Alice, Opts)).

%% @doc Verify that registered sub-ledgers are able to send tokens to each other
%% without the need for messages on the root ledger.
subledger_to_subledger_test_() -> {timeout, 30, fun subledger_to_subledger/0}.
subledger_to_subledger() ->
    Opts = dev_token_props:opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    RootLedger =
        ledger(
            <<"scripts/hyper-token.lua">>,
            #{ <<"balances">> => #{ Alice => 100 } },
            Opts
        ),
    SubLedger1 = dev_token_lib:subledger(RootLedger, Opts),
    SubLedger2 = dev_token_lib:subledger(RootLedger, Opts),
    Names = #{
        Alice => alice,
        Bob => bob,
        RootLedger => root,
        SubLedger1 => subledger1,
        SubLedger2 => subledger2
    },
    % 1. Alice has tokens on the root ledger.
    ?assertEqual(100, dev_token_lib:balance(RootLedger, Alice, Opts)),
    % 2. Alice sends 90 tokens to herself on SubLedger1.
    dev_token_lib:transfer(RootLedger, Alice, Alice, 90, SubLedger1, Opts),
    % 3. Alice sends 10 tokens to Bob on SubLedger2.
    dev_token_lib:transfer(SubLedger1, Alice, Bob, 10, SubLedger2, Opts),
    ?event({map, dev_token_lib:map([RootLedger, SubLedger1, SubLedger2], Names, Opts)}),
    ?assertEqual(10, dev_token_lib:balance(RootLedger, Alice, Opts)),
    ?assertEqual(80, dev_token_lib:balance(SubLedger1, Alice, Opts)),
    ?assertEqual(10, dev_token_lib:balance(SubLedger2, Bob, Opts)),
    dev_token_lib:verify_net(RootLedger, [SubLedger1, SubLedger2], Opts),
    % 5. Bob sends 5 tokens to himself on SubLedger1.
    dev_token_lib:transfer(SubLedger2, Bob, Bob, 5, SubLedger1, Opts),
    dev_token_lib:transfer(SubLedger2, Bob, Alice, 4, SubLedger1, Opts),
    ?event({map, dev_token_lib:map([RootLedger, SubLedger1, SubLedger2], Names, Opts)}),
    ?assertEqual(10, dev_token_lib:balance(RootLedger, Alice, Opts)),
    ?assertEqual(5, dev_token_lib:balance(SubLedger1, Bob, Opts)),
    ?assertEqual(84, dev_token_lib:balance(SubLedger1, Alice, Opts)),
    ?assertEqual(1, dev_token_lib:balance(SubLedger2, Bob, Opts)),
    dev_token_lib:verify_net(RootLedger, [SubLedger1, SubLedger2], Opts).

%% @doc Verify that a ledger can send tokens to a peer ledger that is not
%% registered with it yet. Each peer ledger must have precisely the same process
%% base message, granting transitive security properties: If a peer trusts its
%% own compute and assignment mechanism, then it can trust messages from exact
%% duplicates of itself. In order for this to be safe, the peer ledger network's
%% base process message must implement sufficicient rollback protections and 
%% compute correctness guarantees.
unregistered_peer_transfer_test_() -> {timeout, 30, fun unregistered_peer_transfer/0}.
unregistered_peer_transfer() ->
    Opts = dev_token_props:opts() ,
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    RootLedger =
        ledger(
            <<"scripts/hyper-token.lua">>,
            #{ <<"balances">> => #{ Alice => 100 } },
            Opts
        ),
    SubLedgers = [ dev_token_lib:subledger(RootLedger, Opts) || _ <- lists:seq(1, 3) ],
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
    ?assertEqual(100, dev_token_lib:balance(RootLedger, Alice, Opts)),
    dev_token_lib:transfer(RootLedger, Alice, Alice, 90, SubLedger1, Opts),
    % Verify the state before the multi-hop transfer.
    ?assertEqual(10, dev_token_lib:balance(RootLedger, Alice, Opts)),
    ?assertEqual(90, dev_token_lib:balance(SubLedger1, Alice, Opts)),
    % 4. Alice sends 10 tokens to Bob on SubLedger3, via SubLedger2.
    dev_token_lib:transfer(RootLedger, Alice, Bob, 10, SubLedger2, Opts),
    ?assertEqual(0, dev_token_lib:balance(RootLedger, Alice, Opts)),
    ?assertEqual(90, dev_token_lib:balance(SubLedger1, Alice, Opts)),
    ?assertEqual(10, dev_token_lib:balance(SubLedger2, Bob, Opts)),
    % 5. Bob sends 10 tokens to himself on SubLedger3.
    dev_token_lib:transfer(SubLedger1, Alice, Bob, 50, SubLedger3, Opts),
    % Verify the final state of all ledgers.
    ?event(debug,
        {map,
            dev_token_lib:map(
                [RootLedger, SubLedger1, SubLedger2, SubLedger3],
                Names,
                Opts
            )
        }
    ),
    ?assertEqual(0, dev_token_lib:balance(RootLedger, Alice, Opts)),
    ?assertEqual(40, dev_token_lib:balance(SubLedger1, Alice, Opts)),
    ?assertEqual(10, dev_token_lib:balance(SubLedger2, Bob, Opts)),
    ?assertEqual(50, dev_token_lib:balance(SubLedger3, Bob, Opts)),
    dev_token_lib:verify_net(RootLedger, SubLedgers, Opts).

%% @doc Verify that sub-ledgers can request and enforce multiple scheduler
%% commitments. `hyper-token' always validates that peer `base' processes
%% (the uncommitted process ID without its `scheduler' and `authority' fields)
%% match. It allows us to specify additional constraints on the `scheduler' and
%% `authority' fields while matching against the local ledger's base process
%% message. This test validates the correctness of these constraints.
%% 
%% The grammar supported by `hyper-token.lua' allows for the following, where 
%% `X = scheduler | authority`:
%% - `X`: A list of `X`s that must (by default) be present in the
%%   peer ledger's `X' field.
%% - `X-match`: A count of the number of `X`s that must be present in the
%%   peer ledger's `X' field.
%% - `X-required`: A list of `X`s that always must be present in the
%%   peer ledger's `X' field.
multischeduler_test_disabled() -> {timeout, 30, fun multischeduler/0}.
multischeduler() ->
    BaseOpts = dev_token_props:opts(),
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
                    <<"balances">> => #{ Alice => 100 },
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
    dev_token_lib:transfer(RootLedger, Alice, Bob, 100, Opts),
    ?assertEqual(100, dev_token_lib:balance(RootLedger, Bob, Opts)),
    % Create a new process with with the same schedulers, but do not provide
    % the extra scheduler in the `identities' map.
    OptsWithoutHostWallet = maps:remove(priv_wallet, Opts),
    RootLedger2 =
        ledger(
            <<"scripts/hyper-token.lua">>,
            ProcExtra,
            OptsWithoutHostWallet
        ),
    % Alice has tokens on the root ledger. She tries to move them to Bob.
    dev_token_lib:transfer(RootLedger2, Alice, Bob, 100, OptsWithoutHostWallet),
    % The transfer should fail because only one signature will be provided on 
    % the assignment.
    ?assertEqual(0, dev_token_lib:balance(RootLedger2, Bob, OptsWithoutHostWallet)),
    % The transfer should succeed if:
    % - Set the `authority-required' field to contain the host wallet, while
    % - Setting the `authority-match' field to 1.
    OptsWithoutExtraScheduler = #{ priv_wallet => NodeWallet },
    RootLedger3 =
        ledger(
            <<"scripts/hyper-token.lua">>,
            ProcExtra#{
                <<"scheduler-match">> => 1
            },
            OptsWithoutExtraScheduler
        ),
    dev_token_lib:transfer(RootLedger3, Alice, Bob, 100, OptsWithoutExtraScheduler),
    ?assertEqual(100, dev_token_lib:balance(RootLedger3, Bob, OptsWithoutExtraScheduler)),
    % Ensure that another subledger can be registered to this process with the
    % the necessary scheduler shared, but an additional scheduler not shared.
    % Further, we ensure that the `scheduler-required' field is satisfied by
    % creating a subledger that has two different schedulers, excluding the
    % host wallet.
    OptsWithSchedulers = OptsWithoutExtraScheduler#{
        identities => #{
            <<"scheduler-1">> => #{
                priv_wallet => Scheduler3
            },
            <<"scheduler-2">> => #{
                priv_wallet => Scheduler2
            },
            <<"scheduler-3">> => #{
                priv_wallet => Scheduler3
            }
        }
    },
    % Create 3 subledgers with the same process, but different schedulers. Two
    % that are valid (containing the `scheduler-required' field), and one that
    % is invalid (does not contain the scheduler from `scheduler-required').
    Subledger1 =
        dev_token_lib:subledger(
            RootLedger3,
            #{
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
            OptsWithSchedulers
        ),
    Subledger2 =
        dev_token_lib:subledger(
            RootLedger3,
            #{
                <<"scheduler">> =>
                    [
                        hb_util:human_id(NodeWallet),
                        hb_util:human_id(Scheduler3)
                    ],
                <<"scheduler-required">> =>
                    [hb_util:human_id(NodeWallet)]
            },
            OptsWithSchedulers
        ),
    Subledger3 =
        dev_token_lib:subledger(
            RootLedger3,
            #{
                <<"scheduler-required">> => [hb_util:human_id(NodeWallet)],
                <<"scheduler">> =>
                    [
                        hb_util:human_id(Scheduler2),
                        hb_util:human_id(Scheduler3)
                    ]
            },
            OptsWithSchedulers
        ),
    % Create a map of names for the ledgers for use in logging.
    Names = #{
        Alice => alice,
        Bob => bob,
        RootLedger3 => root,
        Subledger1 => subledger1,
        Subledger2 => subledger2,
        Subledger3 => subledger3
    },
    % Bob has tokens on the root ledger. He moves them to Alice on Subledger1.
    dev_token_lib:transfer(RootLedger3, Bob, Alice, 100, Subledger1, OptsWithSchedulers),
    dev_token_lib:transfer(Subledger1, Alice, Bob, 100, Subledger2, OptsWithSchedulers),
    % Validate the balance has been transferred to Alice on Subledger2.
    ?assertEqual(100, dev_token_lib:balance(Subledger2, Bob, OptsWithSchedulers)),
    % Alice cannot move tokens to Bob on Subledger3, because the
    % `scheduler-required' field is not satisfied by the subledger.
    ?event(debug_base,
        {map,
            dev_token_lib:map(
                [RootLedger3, Subledger1, Subledger2, Subledger3],
                Names,
                OptsWithSchedulers
            )
        }
    ),
    dev_token_lib:transfer(Subledger2, Bob, Alice, 50, Subledger3, OptsWithSchedulers),
    % Validate the balance has not been transferred to Bob on Subledger3.
    ?assertEqual(0, dev_token_lib:balance(Subledger3, Alice, OptsWithSchedulers)),
    dev_token_lib:transfer(Subledger2, Bob, Alice, 50, Subledger1, OptsWithSchedulers),
    % Validate that the remaining balance has been transferred to Alice on
    % Subledger1.
    ?assertEqual(50, dev_token_lib:balance(Subledger1, Alice, OptsWithSchedulers)),
    dev_token_lib:transfer(Subledger1, Alice, Bob, 50, RootLedger3, OptsWithSchedulers),
    % Validate that the balance has been transferred to Bob on the root ledger.
    ?assertEqual(50, dev_token_lib:balance(RootLedger3, Bob, OptsWithSchedulers)).

%% @doc Ensure that the `hyper-token.lua' script can parse comma-separated
%% IDs in the `scheduler' field of a message.
comma_separated_scheduler_list_test() ->
    NodeWallet = hb:wallet(),
    Scheduler2 = ar_wallet:new(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    Opts = (dev_token_props:opts())#{ priv_wallet => NodeWallet, identities => #{
        <<"extra-scheduler">> => #{
            priv_wallet => Scheduler2
        }
    } },
    Ledger =
        ledger(
            <<"scripts/hyper-token.lua">>,
            ProcExtra = 
                #{
                    <<"balances">> => #{ Alice => 100 },
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
    dev_token_lib:transfer(Ledger, Alice, Bob, 100, Opts),
    ?assertEqual(100, dev_token_lib:balance(Ledger, Bob, Opts)).
