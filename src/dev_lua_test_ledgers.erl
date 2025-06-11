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

%%% Ledger client library.
%%%
%%% A simple, thin library for generating ledgers and interacting with
%%% `hyper-token.lua` processes.


%% @doc Generate a Lua process definition message.
ledger(Script, Opts) ->
    ledger(Script, #{}, Opts).


ledger(Script, Extra, Opts) ->
    % If the `balance' key is set in the `Extra' map, ensure that any wallets
    % given as keys in the message are converted to human-readable addresses.
    HostWallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    ModExtra =
        case maps:get(<<"balance">>, Extra, undefined) of
            undefined -> Extra;
            RawBalance ->
                Extra#{
                  <<"balance">> =>
                      maps:from_list(
                        lists:map(
                          fun({ID, Amount}) when ?IS_ID(ID) ->
                                  {hb_util:human_id(ID), Amount};
                             ({Wallet, Amount}) when is_tuple(Wallet) ->
                                  {hb_util:human_id(
                                     ar_wallet:to_address(Wallet)),
                                   Amount}
                          end,
                          maps:to_list(RawBalance)))
                 }
        end,
    Proc =
        hb_message:commit(
          maps:merge(
            #{
              <<"device">> => <<"process@1.0">>,
              <<"type">> => <<"Process">>,
              <<"scheduler-device">> => <<"scheduler@1.0">>,
              <<"scheduler">> => hb_util:human_id(HostWallet),
              <<"execution-device">> => <<"lua@5.3a">>,
              <<"authority">> => hb_util:human_id(HostWallet),
              <<"module">> => lua_script(Script)
             },
            ModExtra),
          Opts#{priv_wallet => HostWallet}),
    hb_cache:write(Proc, Opts),
    Proc.


%% @doc Generate a Lua `script' key from a file or list of files.
lua_script(Files) when is_list(Files) ->
    [ #{
        <<"content-type">> => <<"application/lua">>,
        <<"module">> => File,
        <<"body">> =>
            hb_util:ok(
              file:read_file(
                if
                    is_binary(File) -> binary_to_list(File);
                    true -> File
                end))
       }
      || File <- Files ];
lua_script(File) when is_binary(File) ->
    hd(lua_script([File])).


%% @doc Generate a test sub-ledger process definition message.
subledger(Root, Opts) ->
    subledger(Root, #{}, Opts).


subledger(Root, Extra, Opts) ->
    BareRoot =
        maps:without(
          [<<"token">>, <<"balance">>],
          hb_message:uncommitted(Root)),
    Proc =
        hb_message:commit(
          maps:merge(
            BareRoot#{
              <<"token">> => hb_message:id(Root, all)
             },
            Extra),
          hb_opts:get(priv_wallet, hb:wallet(), Opts)),
    hb_cache:write(Proc, Opts),
    Proc.


%% @doc Generate a test transfer message.
transfer(ProcMsg, Sender, Recipient, Quantity, Opts) ->
    transfer(ProcMsg, Sender, Recipient, Quantity, undefined, Opts).


transfer(ProcMsg, Sender, Recipient, Quantity, Route, Opts) ->
    MaybeRoute =
        if
            Route == undefined -> #{};
            true ->
                #{
                  <<"route">> =>
                      if
                          is_map(Route) -> hb_message:id(Route, all);
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
                                                  Sender)
                           },
                          Sender),
    hb_ao:resolve(
      ProcMsg,
      Xfer,
      Opts#{priv_wallet => hb_opts:get(priv_wallet, hb:wallet(), Opts)}).


%% @doc Request that a peer register with a without sub-ledger.
register(ProcMsg, Peer, Opts) when is_map(Peer) ->
    register(ProcMsg, hb_message:id(Peer, all), Opts);
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
                  Opts)
           },
          Opts),
    hb_ao:resolve(
      ProcMsg,
      Reg,
      Opts).


%% @doc Retreive a single balance from the ledger.
balance(ProcMsg, User, Opts) when not ?IS_ID(User) ->
    balance(ProcMsg, hb_util:human_id(ar_wallet:to_address(User)), Opts);
balance(ProcMsg, ID, Opts) ->
    hb_ao:get(<<"now/balance/", ID/binary>>, ProcMsg, 0, Opts).


%% @doc Get the total balance for an ID across all ledgers in a set.
balance_total(Procs, ID, Opts) ->
    lists:sum(
      lists:map(
        fun(Proc) -> balance(Proc, ID, Opts) end,
        maps:values(normalize_env(Procs)))).


%% @doc Get the balances of a ledger.
balances(ProcMsg, Opts) ->
    balances(now, ProcMsg, Opts).


balances(initial, ProcMsg, Opts) ->
    balances(<<"">>, ProcMsg, Opts);
balances(Mode, ProcMsg, Opts) when is_atom(Mode) ->
    balances(hb_util:bin(Mode), ProcMsg, Opts);
balances(Prefix, ProcMsg, Opts) ->
    Balances = hb_ao:get(<<Prefix/binary, "/balance">>, ProcMsg, #{}, Opts),
    hb_private:reset(hb_cache:ensure_all_loaded(Balances, Opts)).


%% @doc Get the supply of a ledger, either `now` or `initial`.
supply(ProcMsg, Opts) ->
    supply(now, ProcMsg, Opts).


supply(Mode, ProcMsg, Opts) ->
    Loaded = hb_cache:ensure_all_loaded(ProcMsg, Opts),
    lists:sum(maps:values(balances(Mode, Loaded, Opts))).


%% @doc Calculate the supply of tokens in all sub-ledgers, from the balances of
%% the root ledger.
subledger_supply(RootProc, AllProcs, Opts) ->
    supply(now, RootProc, Opts) - user_supply(RootProc, AllProcs, Opts).


%% @doc Calculate the supply of tokens held by users on a ledger, excluding
%% those held in sub-ledgers.
user_supply(Proc, AllProcs, Opts) ->
    NormProcs = normalize_without_root(Proc, AllProcs),
    SubledgerIDs = maps:keys(NormProcs),
    lists:sum(
      maps:values(
        maps:without(
          SubledgerIDs,
          balances(now, Proc, Opts)))).


%% @doc Get the local expectation of a ledger's balances with peer ledgers.
ledgers(ProcMsg, Opts) ->
    case hb_cache:ensure_all_loaded(
           hb_ao:get(<<"now/ledgers">>, ProcMsg, #{}, Opts),
           Opts) of
        Msg when is_map(Msg) -> hb_private:reset(Msg);
        [] -> #{}
    end.


%% @doc Generate a complete overview of the test environment's balances and
%% ledgers. Optionally, a map of environment names can be provided to make the
%% output more readable.
map(Procs, Opts) ->
    NormProcs = normalize_env(Procs),
    maps:merge_with(
      fun(Key, Balances, Ledgers) ->
              MaybeRoot =
                  case maps:get(Key, NormProcs, #{}) of
                      #{<<"token">> := _} -> #{};
                      _ -> #{root => true}
                  end,
              MaybeRoot#{
                balances => Balances,
                ledgers => Ledgers
               }
      end,
      maps:map(fun(_, Proc) -> balances(Proc, Opts) end, NormProcs),
      maps:map(fun(_, Proc) -> ledgers(Proc, Opts) end, NormProcs)).


map(Procs, EnvNames, Opts) ->
    apply_names(map(Procs, Opts), EnvNames, Opts).


%% @doc Apply a map of environment names to elements in either a map or list.
%% Expects a map of `ID or ProcMsg or Wallet => Name' as the `EnvNames' argument,
%% and a potentially deep map or list of elements to apply the names to.
apply_names(Map, EnvNames, Opts) ->
    IDs =
        maps:from_list(
          lists:filtermap(
            fun({Key, V}) ->
                    try
                        {true, {hb_util:human_id(Key), V}}
                    catch
                        _:_ ->
                            try
                                {true, {hb_message:id(Key, all), V}}
                            catch
                                _:_ -> false
                            end
                    end
            end,
            maps:to_list(EnvNames))),
    do_apply_names(Map, maps:merge(IDs, EnvNames), Opts).


do_apply_names(Map, EnvNames, Opts) when is_map(Map) ->
    maps:from_list(
      lists:map(
        fun({Key, Proc}) ->
                {apply_names(Key, EnvNames, Opts),
                 apply_names(Proc, EnvNames, Opts)}
        end,
        maps:to_list(Map)));
do_apply_names(List, EnvNames, Opts) when is_list(List) ->
    lists:map(
      fun(Proc) ->
              apply_names(Proc, EnvNames, Opts)
      end,
      List);
do_apply_names(Item, Names, _Opts) when is_map_key(Item, Names) ->
    maps:get(Item, Names);
do_apply_names(Item, Names, _Opts) ->
    try
        maps:get(hb_util:human_id(Item), Names, Item)
    catch
        _:_ -> Item
    end.


%%% Test ledger network invariants.
%%%
%%% Complex assertions that verify specific invariants about the state of
%%% ledgers in a test environment. These are used to validate the correctness
%%% of the `hyper-token.lua` script. Tested invariants are listed below.
%%%
%%% For every timestep `t_n`, the following invariants must hold:
%%% 1. The root ledger supply at `t_0` must match the current supply.
%%% 2. For every sub-ledger `l`, each expected balance held in `l/now/ledgers`
%%%    must equal the balance found at `peer/now/balance/l`.
%%% 3. The sum of all values in `/now/balance` across all sub-ledgers must
%%%    equal the root ledger's supply.


%% @doc Execute all invariant checks for a pair of root ledger and sub-ledgers.
verify_net(RootProc, AllProcs, Opts) ->
    verify_net_supply(RootProc, AllProcs, Opts),
    verify_net_peer_balances(AllProcs, Opts).


%% @doc Verify that the initial supply of tokens on the root ledger is the same
%% as the current supply. This invariant will not hold for sub-ledgers, as they
%% 'mint' tokens in their local supply when they receive them from other ledgers.
verify_root_supply(RootProc, Opts) ->
    ?assert(
      supply(initial, RootProc, Opts) ==
      supply(now, RootProc, Opts) +
      lists:sum(maps:values(ledgers(RootProc, Opts)))).


%% @doc Verify that the sum of all spendable balances held by ledgers in a
%% test network is equal to the initial supply of tokens.
verify_net_supply(RootProc, AllProcs, Opts) ->
    verify_root_supply(RootProc, Opts),
    StartingRootSupply = supply(initial, RootProc, Opts),
    NormProcsWithoutRoot = normalize_without_root(RootProc, AllProcs),
    SubledgerIDs = maps:keys(NormProcsWithoutRoot),
    RootUserSupply = user_supply(RootProc, NormProcsWithoutRoot, Opts),
    SubledgerSupply = subledger_supply(RootProc, AllProcs, Opts),
    ?event(debug, {verify_net_supply, {root, RootUserSupply}, {subledger, SubledgerSupply}}),
    ?assert(
      StartingRootSupply ==
      RootUserSupply + SubledgerSupply).


%% @doc Verify the consistency of all expected ledger balances with their peer
%% ledgers and the actual balances held.
verify_net_peer_balances(AllProcs, Opts) ->
    NormProcs = normalize_env(AllProcs),
    maps:map(
      fun(ValidateProc, _) ->
              verify_peer_balances(ValidateProc, NormProcs, Opts)
      end,
      NormProcs).


%% @doc Verify that a ledger's expectation of its balances with peer ledgers
%% is consistent with the actual balances held.
verify_peer_balances(ValidateProc, AllProcs, Opts) ->
    Ledgers = ledgers(ValidateProc, Opts),
    NormProcs = normalize_env(AllProcs),
    maps:map(
      fun(PeerID, ExpectedBalance) ->
              ?assertEqual(
                ExpectedBalance,
                balance(ValidateProc,
                        maps:get(PeerID, NormProcs),
                        Opts))
      end,
      Ledgers).


%%% Test utilities.


%% @doc Normalize a set of processes, representing ledgers in a test environment,
%% to a canonical form: A map of `ID => Proc`.
normalize_env(Procs) when is_map(Procs) ->
    normalize_env(maps:values(Procs));
normalize_env(Procs) when is_list(Procs) ->
    maps:from_list(
      lists:map(
        fun(Proc) ->
                {hb_message:id(Proc, all), Proc}
        end,
        Procs)).


%% @doc Return the normalized environment without the root ledger.
normalize_without_root(RootProc, Procs) ->
    maps:without([hb_message:id(RootProc, all)], normalize_env(Procs)).


%% @doc Create a node message for the test that avoids looking up unknown
%% recipients via remote stores. This improves test performance.
test_opts() ->
    hb:init(),
    #{}.


%%% Test cases.


%% @doc Test the `transfer` function.
%% 1. Alice has 100 tokens on a root ledger.
%% 2. Alice sends 1 token to Bob.
%% 3. Alice has 99 tokens, and Bob has 1 token.
transfer_test_() -> {timeout, 30, fun transfer/0}.


transfer() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    Proc =
        ledger(
          <<"scripts/hyper-token.lua">>,
          #{<<"balance">> => #{Alice => 100}},
          Opts),
    ?assertEqual(100, supply(Proc, Opts)),
    transfer(Proc, Alice, Bob, 1, Opts),
    ?assertEqual(99, balance(Proc, Alice, Opts)),
    ?assertEqual(1, balance(Proc, Bob, Opts)),
    ?assertEqual(100, supply(Proc, Opts)).


%% @doc User's must not be able to send tokens they do not own. We test three
%% cases:
%% 1. Transferring a token when the sender has no tokens.
%% 2. Transferring a token when the sender has less tokens than the amount
%%    being transferred.
%% 3. Transferring a binary-encoded amount of tokens that exceed the quantity
%%    of tokens the sender has available.
transfer_unauthorized_test_() -> {timeout, 30, fun transfer_unauthorized/0}.


transfer_unauthorized() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    Proc =
        ledger(
          <<"scripts/hyper-token.lua">>,
          #{<<"balance">> => #{Alice => 100}},
          Opts),
    % 1. Transferring a token when the sender has no tokens.
    Result = transfer(Proc, Bob, Alice, 1, Opts),
    ?event(debug, {unauthorized_transfer, {result, Result}}),
    % 2. Transferring a token when the sender has less tokens than the amount
    %    being transferred.
    transfer(Proc, Alice, Bob, 101, Opts),
    ?event(debug, {unauthorized_transfer, {result, Result}}),
    receive after 1000 -> ok end,
    ?event(debug, {env, map([Proc], #{Alice => alice, Bob => bob}, Opts)}),
    ?assertEqual(100, balance(Proc, Alice, Opts)),
    ?assertEqual(0, balance(Proc, Bob, Opts)),
    % 3. Transferring a binary-encoded amount of tokens that exceed the quantity
    %    of tokens the sender has available.
    transfer(Proc, Alice, Bob, <<"101">>, Opts),
    ?assertEqual(100, balance(Proc, Alice, Opts)),
    ?assertEqual(0, balance(Proc, Bob, Opts)),
    % Validate the final supply of tokens.
    ?assertEqual(100, supply(Proc, Opts)).


%% @doc Verify that a user can deposit tokens into a sub-ledger.
subledger_deposit_test_() -> {timeout, 30, fun subledger_deposit/0}.


subledger_deposit() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    Proc =
        ledger(
          <<"scripts/hyper-token.lua">>,
          #{<<"balance">> => #{Alice => 100}},
          Opts),
    SubLedger = subledger(Proc, Opts),
    % 1. Alice has tokens on the root ledger.
    ?assertEqual(100, balance(Proc, Alice, Opts)),
    % 2. Alice deposits tokens into the sub-ledger.
    transfer(Proc, Alice, Alice, 10, SubLedger, Opts),
    ?event(debug, {after_deposit, {result, map([Proc, SubLedger], Opts)}}),
    ?assertEqual(90, balance(Proc, Alice, Opts)),
    ?assertEqual(10, balance(SubLedger, Alice, Opts)),
    % Verify all invariants.
    verify_net(Proc, [SubLedger], Opts).


%% @doc Simulate inter-ledger payments between users on a single sub-ledger:
%% 1. Alice has tokens on the root ledger.
%% 2. Alice sends tokens to the sub-ledger from the root ledger.
%% 3. Alice sends tokens to Bob on the sub-ledger.
%% 4. Bob sends tokens to Alice on the root ledger.
subledger_transfer_test_() -> {timeout, 10, fun subledger_transfer/0}.


subledger_transfer() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    RootLedger =
        ledger(
          <<"scripts/hyper-token.lua">>,
          #{<<"balance">> => #{Alice => 100}},
          Opts),
    SubLedger = subledger(RootLedger, Opts),
    EnvNames = #{
                 Alice => alice,
                 Bob => bob,
                 RootLedger => root,
                 SubLedger => subledger
                },
    % 1. Alice has tokens on the root ledger.
    ?assertEqual(100, balance(RootLedger, Alice, Opts)),
    ?event(token_log, {map, map([RootLedger], EnvNames, Opts)}),
    % 2. Alice sends tokens to the sub-ledger from the root ledger.
    transfer(RootLedger, Alice, Alice, 10, SubLedger, Opts),
    ?assertEqual(90, balance(RootLedger, Alice, Opts)),
    ?assertEqual(10, balance(SubLedger, Alice, Opts)),
    % 3. Alice sends tokens to Bob on the sub-ledger.
    transfer(SubLedger, Alice, Bob, 8, Opts),
    ?event(token_log,
           {state_after_subledger_user_xfer,
            {names, map([RootLedger, SubLedger], EnvNames, Opts)},
            {ids, map([RootLedger, SubLedger], Opts)}}),
    % 4. Bob sends tokens to Alice on the root ledger.
    transfer(SubLedger, Bob, Bob, 7, RootLedger, Opts),
    % Validate the balances of the root and sub-ledgers.
    Map = map([RootLedger, SubLedger], EnvNames, Opts),
    ?event(token_log, {map, map([RootLedger, SubLedger], Opts)}),
    ?assertEqual(
      #{
        root => #{
                  balances => #{alice => 90, bob => 7.0, subledger => 3.0},
                  ledgers => #{},
                  root => true
                 },
        subledger => #{
                       balances => #{alice => 2, bob => 1},
                       ledgers => #{}
                      }
       },
      Map),
    % Validate all invariants.
    verify_net(RootLedger, [SubLedger], Opts).


%% @doc Verify that peer ledgers on the same token are able to register mutually
%% to establish a peer-to-peer connection.
%%
%% Disabled as explicit peer registration is not required for `hyper-token.lua'
%% to function.
subledger_registration_test_disabled() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    RootLedger =
        ledger(
          <<"scripts/hyper-token.lua">>,
          #{<<"balance">> => #{Alice => 100}},
          Opts),
    SubLedger1 = subledger(RootLedger, Opts),
    SubLedger2 = subledger(RootLedger, Opts),
    Names = #{
              SubLedger1 => subledger1,
              SubLedger2 => subledger2
             },
    ?event(debug,
           {subledger,
            {sl1, hb_message:id(SubLedger1, none)},
            {sl2, hb_message:id(SubLedger2, none)}}),
    % There are no registered peers on either sub-ledger.
    ?assertEqual(0, map_size(ledgers(SubLedger1, Opts))),
    ?assertEqual(0, map_size(ledgers(SubLedger2, Opts))),
    % Alice registers with SubLedger1.
    register(SubLedger1, SubLedger2, Opts),
    ?event(debug, {map, map([SubLedger1, SubLedger2], Names, Opts)}),
    ?event(debug, {sl1_ledgers, ledgers(SubLedger1, Opts)}),
    ?event(debug, {sl2_ledgers, ledgers(SubLedger2, Opts)}),
    % SubLedger1 and SubLedger2 are now aware of each other.
    ?assertEqual(1, map_size(ledgers(SubLedger1, Opts))),
    ?assertEqual(1, map_size(ledgers(SubLedger2, Opts))),
    % Alice can send tokens to Bob on SubLedger2.
    verify_net(RootLedger, [SubLedger1, SubLedger2], Opts).


%% @doc Verify that registered sub-ledgers are able to send tokens to each other
%% without the need for messages on the root ledger.
subledger_to_subledger_test_() -> {timeout, 30, fun subledger_to_subledger/0}.


subledger_to_subledger() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    RootLedger =
        ledger(
          <<"scripts/hyper-token.lua">>,
          #{<<"balance">> => #{Alice => 100}},
          Opts),
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
    ?assertEqual(100, balance(RootLedger, Alice, Opts)),
    % 2. Alice registers with SubLedger1.
    register(SubLedger1, SubLedger2, Opts),
    % 3. Alice sends 90 tokens to herself on SubLedger1.
    transfer(RootLedger, Alice, Alice, 90, SubLedger1, Opts),
    % 4. Alice sends 10 tokens to Bob on SubLedger2.
    transfer(SubLedger1, Alice, Bob, 10, SubLedger2, Opts),
    ?assertEqual(10, balance(RootLedger, Alice, Opts)),
    ?assertEqual(80, balance(SubLedger1, Alice, Opts)),
    ?assertEqual(10, balance(SubLedger2, Bob, Opts)),
    verify_net(RootLedger, [SubLedger1, SubLedger2], Opts),
    % 5. Bob sends 5 tokens to himself on SubLedger1.
    transfer(SubLedger2, Bob, Bob, 5, SubLedger1, Opts),
    transfer(SubLedger2, Bob, Alice, 4, SubLedger1, Opts),
    ?event(debug, {map, map([RootLedger, SubLedger1, SubLedger2], Names, Opts)}),
    ?assertEqual(10, balance(RootLedger, Alice, Opts)),
    ?assertEqual(5, balance(SubLedger1, Bob, Opts)),
    ?assertEqual(84, balance(SubLedger1, Alice, Opts)),
    ?assertEqual(1, balance(SubLedger2, Bob, Opts)),
    verify_net(RootLedger, [SubLedger1, SubLedger2], Opts).


%% @doc Verify that a ledger can send tokens to a peer ledger that is not
%% registered with it yet. Each peer ledger must have precisely the same process
%% base message, granting transitive security properties: If a peer trusts its
%% own compute and assignment mechanism, then it can trust messages from exact
%% duplicates of itself. In order for this to be safe, the peer ledger network's
%% base process message must implement sufficicient rollback protections and
%% compute correctness guarantees.
unregistered_peer_transfer_test_() -> {timeout, 30, fun unregistered_peer_transfer/0}.


unregistered_peer_transfer() ->
    Opts = #{},
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    RootLedger =
        ledger(
          <<"scripts/hyper-token.lua">>,
          #{<<"balance">> => #{Alice => 100}},
          Opts),
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
    ?assertEqual(100, balance(RootLedger, Alice, Opts)),
    transfer(RootLedger, Alice, Alice, 90, SubLedger1, Opts),
    % Verify the state before the multi-hop transfer.
    ?assertEqual(10, balance(RootLedger, Alice, Opts)),
    ?assertEqual(90, balance(SubLedger1, Alice, Opts)),
    % 4. Alice sends 10 tokens to Bob on SubLedger3, via SubLedger2.
    transfer(RootLedger, Alice, Bob, 10, SubLedger2, Opts),
    ?assertEqual(0, balance(RootLedger, Alice, Opts)),
    ?assertEqual(90, balance(SubLedger1, Alice, Opts)),
    ?assertEqual(10, balance(SubLedger2, Bob, Opts)),
    % 5. Bob sends 10 tokens to himself on SubLedger3.
    transfer(SubLedger1, Alice, Bob, 50, SubLedger3, Opts),
    % Verify the final state of all ledgers.
    ?event(debug,
           {map,
            map(
              [RootLedger, SubLedger1, SubLedger2, SubLedger3],
              Names,
              Opts)}),
    ?assertEqual(0, balance(RootLedger, Alice, Opts)),
    ?assertEqual(40, balance(SubLedger1, Alice, Opts)),
    ?assertEqual(10, balance(SubLedger2, Bob, Opts)),
    ?assertEqual(50, balance(SubLedger3, Bob, Opts)),
    verify_net(RootLedger, SubLedgers, Opts).


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
                <<"balance">> => #{Alice => 100},
                <<"scheduler">> =>
                    [hb_util:human_id(NodeWallet),
                     hb_util:human_id(Scheduler2)],
                <<"scheduler-required">> =>
                    [hb_util:human_id(NodeWallet)]
               },
          Opts),
    % Alice has tokens on the root ledger. She moves them to Bob.
    transfer(RootLedger, Alice, Bob, 100, Opts),
    ?assertEqual(100, balance(RootLedger, Bob, Opts)),
    % Create a new process with with the same schedulers, but do not provide
    % the extra scheduler in the `identities' map.
    OptsWithoutHostWallet = maps:remove(priv_wallet, Opts),
    RootLedger2 =
        ledger(
          <<"scripts/hyper-token.lua">>,
          ProcExtra,
          OptsWithoutHostWallet),
    % Alice has tokens on the root ledger. She tries to move them to Bob.
    transfer(RootLedger2, Alice, Bob, 100, OptsWithoutHostWallet),
    % The transfer should fail because only one signature will be provided on
    % the assignment.
    ?assertEqual(0, balance(RootLedger2, Bob, OptsWithoutHostWallet)),
    % The transfer should succeed if:
    % - Set the `authority-required' field to contain the host wallet, while
    % - Setting the `authority-match' field to 1.
    OptsWithoutExtraScheduler = #{priv_wallet => NodeWallet},
    RootLedger3 =
        ledger(
          <<"scripts/hyper-token.lua">>,
          ProcExtra#{
            <<"scheduler-match">> => 1
           },
          OptsWithoutExtraScheduler),
    transfer(RootLedger3, Alice, Bob, 100, OptsWithoutExtraScheduler),
    ?assertEqual(100, balance(RootLedger3, Bob, OptsWithoutExtraScheduler)),
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
        subledger(
          RootLedger3,
          #{
            <<"scheduler">> =>
                [hb_util:human_id(NodeWallet),
                 hb_util:human_id(Scheduler2)],
            <<"scheduler-required">> =>
                [hb_util:human_id(NodeWallet)]
           },
          OptsWithSchedulers),
    Subledger2 =
        subledger(
          RootLedger3,
          #{
            <<"scheduler">> =>
                [hb_util:human_id(NodeWallet),
                 hb_util:human_id(Scheduler3)],
            <<"scheduler-required">> =>
                [hb_util:human_id(NodeWallet)]
           },
          OptsWithSchedulers),
    Subledger3 =
        subledger(
          RootLedger3,
          #{
            <<"scheduler-required">> => [hb_util:human_id(NodeWallet)],
            <<"scheduler">> =>
                [hb_util:human_id(Scheduler2),
                 hb_util:human_id(Scheduler3)]
           },
          OptsWithSchedulers),
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
    transfer(RootLedger3, Bob, Alice, 100, Subledger1, OptsWithSchedulers),
    transfer(Subledger1, Alice, Bob, 100, Subledger2, OptsWithSchedulers),
    % Validate the balance has been transferred to Alice on Subledger2.
    ?assertEqual(100, balance(Subledger2, Bob, OptsWithSchedulers)),
    % Alice cannot move tokens to Bob on Subledger3, because the
    % `scheduler-required' field is not satisfied by the subledger.
    ?event(debug_base,
           {map,
            map(
              [RootLedger3, Subledger1, Subledger2, Subledger3],
              Names,
              OptsWithSchedulers)}),
    transfer(Subledger2, Bob, Alice, 50, Subledger3, OptsWithSchedulers),
    % Validate the balance has not been transferred to Bob on Subledger3.
    ?assertEqual(0, balance(Subledger3, Alice, OptsWithSchedulers)),
    transfer(Subledger2, Bob, Alice, 50, Subledger1, OptsWithSchedulers),
    % Validate that the remaining balance has been transferred to Alice on
    % Subledger1.
    ?assertEqual(50, balance(Subledger1, Alice, OptsWithSchedulers)),
    transfer(Subledger1, Alice, Bob, 50, RootLedger3, OptsWithSchedulers),
    % Validate that the balance has been transferred to Bob on the root ledger.
    ?assertEqual(50, balance(RootLedger3, Bob, OptsWithSchedulers)).


%% @doc Ensure that the `hyper-token.lua' script can parse comma-separated
%% IDs in the `scheduler' field of a message.
comma_separated_scheduler_list_test() ->
    NodeWallet = hb:wallet(),
    Scheduler2 = ar_wallet:new(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    Opts = (test_opts())#{
             priv_wallet => NodeWallet,
             identities => #{
                             <<"extra-scheduler">> => #{
                                                        priv_wallet => Scheduler2
                                                       }
                            }
            },
    Ledger =
        ledger(
          <<"scripts/hyper-token.lua">>,
          ProcExtra =
              #{
                <<"balance">> => #{Alice => 100},
                <<"scheduler">> =>
                    iolist_to_binary(
                      [<<"\"">>,
                       hb_util:human_id(NodeWallet),
                       <<"\",\"">>,
                       hb_util:human_id(Scheduler2),
                       <<"\"">>]),
                <<"scheduler-required">> =>
                    [hb_util:human_id(NodeWallet)]
               },
          Opts),
    % Alice has tokens on the root ledger. She moves them to Bob.
    transfer(Ledger, Alice, Bob, 100, Opts),
    ?assertEqual(100, balance(Ledger, Bob, Opts)).
