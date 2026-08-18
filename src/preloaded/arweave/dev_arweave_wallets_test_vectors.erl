%%% @doc End-to-end vectors for AO-native Arweave account persistence.
-module(dev_arweave_wallets_test_vectors).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(DEVICE, <<"arweave-wallets@2.9">>).
-define(CHILD_VECTOR_WIDTH, 32).
%% @doc Batch order changes neither native root nor AO state identity.
batch_order_test() ->
    Opts = opts(<<"batch-order">>),
    Accounts = accounts(lib_arweave_accounts:page_size() + 64),
    {Left, Right} = lists:split(length(Accounts) div 2, Accounts),
    Forwards = ingest([Left, Right], [], Opts),
    Backwards = ingest([Right, Left], [], Opts),
    ?assertEqual(resolved_root(Forwards, Opts), resolved_root(Backwards, Opts)),
    ?assertEqual(state_id(Forwards, Opts), state_id(Backwards, Opts)),
    ?assertNotEqual(
        resolved_root(Forwards, Opts),
        resolved_root(ingest([Left, tl(Right)], [], Opts), Opts)
    ).

%% @doc Canonical Arweave addresses survive an isolated LMDB restart.
restart_test() ->
    Opts = opts(<<"restart-short">>),
    Accounts =
        [
            {<<177, 0:248>>, {876060014779297, <<>>}},
            {<<105, 189, 0:240>>, {30000, <<>>}},
            {<<53, 236, 30, 178, 0:224>>, {20, <<>>}}
            | accounts()
        ],
    State = ingest([Accounts], [], Opts),
    Root = resolved_root(State, Opts),
    ID = state_id(State, Opts),
    Store = store(Opts),
    ok = hb_store:stop(Store),
    ok = hb_store:start(Store),
    {ok, Cold} = hb_cache:read(ID, Opts),
    ?assertEqual(Root, resolved_root(Cold, Opts)),
    {ok, Account} = get_account(Cold, <<177, 0:248>>, Opts),
    ?assertEqual(
        876060014779297,
        hb_maps:get(<<"balance">>, Account, not_found, Opts)
    ).

%% @doc Empty and short mainnet account keys remain terminal values when their
%% Patricia nodes also own children.
terminal_branch_accounts_test() ->
    Opts = opts(<<"terminal-branch-accounts">>),
    Empty = <<>>,
    Short = <<1:200>>,
    EmptyAccount = {876060014779297, <<>>},
    ShortAccount = {80000000000, <<>>},
    Descendants =
        [
            {<<Short/binary, N:56>>, {N, <<>>}}
        || N <- lists:seq(1, lib_arweave_accounts:page_size() + 1)
        ],
    Accounts = [{Empty, EmptyAccount}, {Short, ShortAccount} | Descendants],
    {Expected, _} = lib_arweave_accounts:root(native_tree(Accounts)),
    State = ingest([Accounts], Expected, Opts),
    ?assertEqual(876060014779297, balance(State, Empty, Opts)),
    ?assertEqual(80000000000, balance(State, Short, Opts)),
    ?assertMatch(
        {ok, _},
        hb_ao:resolve(
            State,
            #{ <<"path">> => <<"verify">>, <<"expected-root">> => Expected },
            Opts
        )
    ),
    Branches =
        [
            Node
        || Node <- maps:values(
            topology_messages(maps:get(<<"tree">>, State), #{}, Opts)
        ),
           maps:get(<<"kind">>, hb_message:uncommitted(Node), not_found) ==
                <<"branch">>
        ],
    ?assertEqual(
        1,
        length(
            [
                Node
            || Node <- Branches,
               maps:is_key(<<"account">>, hb_private:reset(Node))
            ]
        )
    ),
    RemovedNative =
        lib_arweave_accounts:apply_diff(
            #{ Empty => remove, Short => remove },
            native_tree(Accounts)
        ),
    {RemovedRoot, _} = lib_arweave_accounts:root(RemovedNative),
    {ok, Removed} =
        apply_diff(
            cold(State, Opts),
            #{
                hb_util:encode(Empty) => <<"remove">>,
                hb_util:encode(Short) => <<"remove">>
            },
            RemovedRoot,
            Opts
        ),
    ?assertMatch({error, _}, get_account(Removed, Empty, Opts)),
    ?assertMatch({error, _}, get_account(Removed, Short, Opts)),
    {ok, Restored} =
        apply_diff(
            cold(Removed, Opts),
            #{
                hb_util:encode(Empty) => account_message(EmptyAccount),
                hb_util:encode(Short) => account_message(ShortAccount)
            },
            Expected,
            Opts
        ),
    ?assertEqual(tree_id(State, Opts), tree_id(Restored, Opts)),
    ?assertEqual(state_id(State, Opts), state_id(Restored, Opts)).

%% @doc A bounded tree is one page and a sparse change path-copies that page.
node_cardinality_test() ->
    Opts = opts(<<"node-cardinality">>),
    Accounts = accounts(),
    State = ingest([Accounts], [], Opts),
    InitialIDs = topology_ids(State, Opts),
    ?assertEqual(1, sets:size(InitialIDs)),
    [{Address, {Balance, _LastTX}} | _] = Accounts,
    ChangedNative = lib_arweave_accounts:insert(
        Address,
        {Balance + 1, <<>>},
        native_tree(Accounts)
    ),
    {ExpectedRoot, _ChangedMemo} = lib_arweave_accounts:root(ChangedNative),
    {ok, Changed} = apply_diff(State, credit(Address, Balance + 1), [], Opts),
    ?assertEqual(ExpectedRoot, resolved_root(Changed, Opts)),
    ChangedIDs = topology_ids(Changed, Opts),
    ?assertEqual(1, sets:size(sets:subtract(ChangedIDs, InitialIDs))).

%% @doc Every branch dependency is exposed through bounded AO child vectors.
compact_graph_messages_test() ->
    Opts = opts(<<"compact-graph">>),
    State = ingest([accounts(512)], [], Opts),
    Messages = topology_messages(maps:get(<<"tree">>, State), #{}, Opts),
    Branches =
        [
            Node
        || Node <- maps:values(Messages),
           maps:get(<<"kind">>, hb_message:uncommitted(Node)) == <<"branch">>
        ],
    ?assert(Branches =/= []),
    Pages =
        [
            Node
        || Node <- maps:values(Messages),
           maps:get(<<"kind">>, hb_message:uncommitted(Node)) == <<"page">>
        ],
    ?assert(Pages =/= []),
    lists:foreach(
        fun(Node) ->
            ?assert(is_binary(hb_maps:get(<<"body">>, Node, not_found, Opts))),
            ?assertEqual(
                [<<"body">>, <<"count">>, <<"kind">>, <<"root">>],
                lists:sort(maps:keys(hb_private:reset(Node)))
            )
        end,
        Pages
    ),
    lists:foreach(
        fun(Node) ->
            ?assert(is_binary(hb_maps:get(<<"body">>, Node, not_found, Opts))),
            ?assertEqual(
                [<<"body">>, <<"children">>, <<"count">>, <<"kind">>, <<"root">>],
                lists:sort(maps:keys(hb_private:reset(Node)))
            )
        end,
        Branches
    ),
    Vectors =
        [
            Node
        || Node <- maps:values(Messages),
           lists:member(
               maps:get(<<"kind">>, hb_message:uncommitted(Node)),
               [<<"children-leaf">>, <<"children-index">>]
           )
        ],
    ?assert(Vectors =/= []),
    lists:foreach(
        fun(Vector) ->
            Public = hb_private:reset(Vector),
            ?assert(map_size(Public) =< ?CHILD_VECTOR_WIDTH + 2),
            lists:foreach(
                fun({_Key, Value}) ->
                    case ?IS_LINK(Value) of
                        true -> ?assertMatch({link, _, _}, Value);
                        false -> ok
                    end
                end,
                maps:to_list(Public)
            )
        end,
        Vectors
    ).

%% @doc Generic AO traversal can copy the whole account graph to a fresh store.
ordinary_link_graph_copy_test() ->
    SourceOpts = opts(<<"link-graph-source">>),
    State = ingest([accounts(512)], [], SourceOpts),
    Root = resolved_root(State, SourceOpts),
    Loaded = hb_private:reset(
        hb_cache:ensure_all_loaded(State, SourceOpts)
    ),
    CopyOpts = opts(<<"link-graph-copy">>),
    {ok, ID} = hb_cache:write(Loaded, CopyOpts),
    {ok, Copy} = hb_cache:read(ID, CopyOpts),
    ?assertEqual(Root, resolved_root(Copy, CopyOpts)),
    ?assertMatch(
        {ok, _},
        hb_ao:resolve(
            Copy,
            #{ <<"path">> => <<"verify">>, <<"expected-root">> => Root },
            CopyOpts
        )
    ).

%% @doc A cold sparse update uses native path skeletons for replace, insert and
%% removal, while enforcing the block's expected root.
cold_sparse_apply_test() ->
    Opts = opts(<<"cold-sparse">>),
    Accounts = accounts(),
    State = cold(ingest([Accounts], [], Opts), Opts),
    [{Address, {Balance, LastTX}} | Rest] = Accounts,
    NewAddress = <<255, 0:248>>,
    Diff =
        maps:merge(
            credit(Address, Balance + 1),
            #{
                hb_util:encode(NewAddress) => account_message({77, <<>>}),
                hb_util:encode(element(1, hd(Rest))) => <<"remove">>
            }
        ),
    Native =
        lib_arweave_accounts:apply_diff(
            lib_arweave_accounts:diff(Diff, Opts),
            native_tree(Accounts)
        ),
    {Expected, _} = lib_arweave_accounts:root(Native),
    {ok, Applied} = apply_diff(State, Diff, Expected, Opts),
    ?assertEqual(Expected, resolved_root(Applied, Opts)),
    {ok, Account} = get_account(Applied, Address, Opts),
    ?assertEqual(
        Balance + 1,
        hb_maps:get(<<"balance">>, Account, not_found, Opts)
    ),
    ?assertMatch({ok, _}, get_account(Applied, NewAddress, Opts)),
    {error, _} = get_account(Applied, element(1, hd(Rest)), Opts),
    ?assertNotMatch(
        {ok, _},
        apply_diff(State, Diff, hb_util:encode(<<0:384>>), Opts)
    ),
    _ = LastTX.

%% @doc Crossing the page bound deterministically splits the graph frontier.
page_split_test() ->
    Opts = opts(<<"compressed-split">>),
    Existing = accounts(lib_arweave_accounts:page_size()),
    AddedAddress = crypto:hash(sha256, <<"page-split-added">>),
    Added = {AddedAddress, {30, <<>>}},
    Base = cold(ingest([Existing], [], Opts), Opts),
    BeforeIDs = topology_ids(Base, Opts),
    ?assertEqual(1, sets:size(BeforeIDs)),
    {ok, Applied} =
        apply_diff(
            Base,
            #{hb_util:encode(AddedAddress) => account_message({30, <<>>})},
            [],
            Opts
        ),
    Native = native_tree([Added | Existing]),
    {Expected, _} = lib_arweave_accounts:root(Native),
    ?assertEqual(Expected, resolved_root(Applied, Opts)),
    ?assertMatch({ok, _}, get_account(Applied, element(1, hd(Existing)), Opts)),
    ?assertMatch({ok, _}, get_account(Applied, AddedAddress, Opts)),
    AfterIDs = topology_ids(Applied, Opts),
    ?assert(sets:size(AfterIDs) > 1),
    ?assertEqual(sets:new(), sets:intersection(BeforeIDs, AfterIDs)),
    {ok, Merged} =
        apply_diff(
            cold(Applied, Opts),
            #{hb_util:encode(AddedAddress) => <<"remove">>},
            resolved_root(Base, Opts),
            Opts
        ),
    ?assertEqual(tree_message(Base, Opts), tree_message(Merged, Opts)),
    ?assertEqual(tree_id(Base, Opts), tree_id(Merged, Opts)),
    ?assertEqual(state_id(Base, Opts), state_id(Merged, Opts)).

%% @doc Removing a sparse sibling above the page bound elides the native unary
%% node, survives a cold full verification, and restores the original graph ID.
large_unary_elision_test() ->
    Opts = opts(<<"large-unary-elision">>),
    Survivors = clustered_accounts(<<1, 1>>, 257),
    Outlier = clustered_account(<<1, 2>>, 999),
    Base = ingest([[Outlier | Survivors]], [], Opts),
    {Expected, _} = lib_arweave_accounts:root(native_tree(Survivors)),
    {ok, Removed} =
        apply_diff(
            cold(Base, Opts),
            #{hb_util:encode(element(1, Outlier)) => <<"remove">>},
            Expected,
            Opts
        ),
    ColdRemoved = cold(Removed, Opts),
    ?assertEqual(Expected, resolved_root(ColdRemoved, Opts)),
    ?assertMatch(
        {ok, _},
        hb_ao:resolve(
            ColdRemoved,
            #{ <<"path">> => <<"verify">>, <<"expected-root">> => Expected },
            Opts
        )
    ),
    Fresh = ingest([lists:reverse(Survivors)], Expected, Opts),
    ?assertEqual(tree_id(Fresh, Opts), tree_id(ColdRemoved, Opts)),
    ?assertEqual(state_id(Fresh, Opts), state_id(ColdRemoved, Opts)),
    {ok, Restored} =
        apply_diff(
            ColdRemoved,
            #{
                hb_util:encode(element(1, Outlier)) =>
                    account_message(element(2, Outlier))
            },
            resolved_root(Base, Opts),
            Opts
        ),
    ?assertEqual(tree_id(Base, Opts), tree_id(Restored, Opts)),
    ?assertEqual(state_id(Base, Opts), state_id(Restored, Opts)).

%% @doc Returning to a prior native root returns the identical graph and state
%% identity; neither object carries predecessor lineage.
repeated_root_test() ->
    Opts = opts(<<"repeated-root">>),
    Accounts = accounts(),
    Initial = ingest([Accounts], [], Opts),
    [{Address, {Balance, LastTX}} | _] = Accounts,
    {ok, Changed} = apply_diff(Initial, credit(Address, Balance + 1), [], Opts),
    {ok, Restored} =
        apply_diff(
            cold(Changed, Opts),
            #{hb_util:encode(Address) => account_message({Balance, LastTX})},
            resolved_root(Initial, Opts),
            Opts
        ),
    ?assertEqual(tree_id(Initial, Opts), tree_id(Restored, Opts)),
    ?assertEqual(state_id(Initial, Opts), state_id(Restored, Opts)),
    ?assertEqual(not_found, hb_maps:get(<<"previous">>, Restored, not_found, Opts)),
    ?assertEqual(
        not_found,
        hb_maps:get(<<"previous-root">>, Restored, not_found, Opts)
    ).

%% @doc A no-op transition reuses every node and the complete state identity.
no_op_deduplication_test() ->
    Opts = opts(<<"no-op">>),
    Initial = ingest([accounts()], [], Opts),
    Before = topology_ids(Initial, Opts),
    BeforeStats = store_stats(Opts),
    {ok, Unchanged} = apply_diff(cold(Initial, Opts), #{}, [], Opts),
    ?assertEqual(Before, topology_ids(Unchanged, Opts)),
    ?assertEqual(tree_id(Initial, Opts), tree_id(Unchanged, Opts)),
    ?assertEqual(state_id(Initial, Opts), state_id(Unchanged, Opts)),
    ?assertEqual(BeforeStats, store_stats(Opts)).

%% @doc Forking one cold root yields independent immutable descendants.
fork_isolation_test() ->
    Opts = opts(<<"fork-isolation">>),
    Base = cold(ingest([accounts()], [], Opts), Opts),
    [{AddressA, {BalanceA, _}}, {AddressB, {BalanceB, _}} | _] = accounts(),
    {ok, ForkA} = apply_diff(Base, credit(AddressA, BalanceA + 1), [], Opts),
    {ok, ForkB} = apply_diff(Base, credit(AddressB, BalanceB + 1), [], Opts),
    ?assertNotEqual(resolved_root(ForkA, Opts), resolved_root(ForkB, Opts)),
    ?assertEqual(BalanceA, balance(Base, AddressA, Opts)),
    ?assertEqual(BalanceA + 1, balance(ForkA, AddressA, Opts)),
    ?assertEqual(BalanceA, balance(ForkB, AddressA, Opts)).

%% @doc Cold get/apply do not descend into unrelated Patricia subtrees, while
%% a complete verification still detects a broken link there.
path_locality_and_fail_closed_test() ->
    Opts = opts(<<"path-locality">>),
    Accounts = path_accounts(),
    State = ingest([Accounts], [], Opts),
    Target = element(1, hd(Accounts)),
    Broken = poison_unrelated_reference(cold(State, Opts), Target, Opts),
    ?assertMatch({ok, _}, get_account(Broken, Target, Opts)),
    {ok, _Applied} = apply_diff(Broken, credit(Target, 12), [], Opts),
    ?assertEqual(
        <<"invalid-account-tree">>,
        error_code(
            hb_ao:resolve(
                Broken,
                #{
                    <<"path">> => <<"verify">>,
                    <<"expected-root">> => resolved_root(State, Opts)
                },
                Opts
            ),
            Opts
        )
    ),
    Forged = forge_target_account(cold(State, Opts), Target, Opts),
    ?assertEqual(
        <<"invalid-account-tree">>,
        error_code(get_account(Forged, Target, Opts), Opts)
    ),
    Root = hb_maps:get(<<"tree">>, cold(State, Opts), not_found, Opts),
    Inline = (hb_message:uncommitted(State))#{ <<"tree">> => Root },
    ?assertEqual(
        <<"invalid-account-tree">>,
        error_code(get_account(Inline, Target, Opts), Opts)
    ),
    BadCount = forge_root_count(cold(State, Opts), Opts),
    ?assertEqual(
        <<"invalid-account-tree">>,
        error_code(
            hb_ao:resolve(
                BadCount,
                #{
                    <<"path">> => <<"verify">>,
                    <<"expected-root">> => resolved_root(State, Opts)
                },
                Opts
            ),
            Opts
        )
    ).

%% @doc A cold sparse update remains bounded by radix nodes, not account count.
cold_path_read_scope_test() ->
    Opts = opts(<<"cold-read-scope">>),
    Accounts = accounts(10000),
    State = cold(ingest([Accounts], [], Opts), Opts),
    [{Target, {Balance, _LastTX}} | _] = Accounts,
    Tracer = spawn(fun() -> trace_loads(0) end),
    erlang:trace_pattern(
        {hb_cache, read, 2},
        true,
        []
    ),
    erlang:trace(all, true, [call, set_on_spawn, {tracer, Tracer}]),
    try
        {ok, _Applied} =
            apply_diff(State, credit(Target, Balance + 1), [], Opts)
    after
        erlang:trace(all, false, [call, set_on_spawn]),
        erlang:trace_pattern(
            {hb_cache, read, 2},
            false,
            []
        ),
        TraceRef = erlang:trace_delivered(all),
        receive
            {trace_delivered, _Tracee, TraceRef} -> ok
        end
    end,
    Tracer ! {count, self()},
    Reads = receive {trace_count, Count} -> Count end,
    ?assert(Reads >= 4),
    ?assertMatch(ReadCount when ReadCount =< 200, Reads),
    ?assert(sets:size(topology_ids(State, Opts)) > 200).

%% @doc Compact pages and frontier branches have one canonical representation.
rejects_noncanonical_graph_test() ->
    Opts = opts(<<"noncanonical-graph">>),
    Pair = lists:sublist(accounts(), 2),
    CanonicalPage = test_page(root, Pair),
    Body = maps:get(<<"body">>, CanonicalPage),
    [First, Second] = Pair,
    InvalidPages =
        [
            CanonicalPage#{ <<"root">> => hb_util:encode(<<0:384>>) },
            CanonicalPage#{ <<"count">> => 3 },
            CanonicalPage#{ <<"count">> => <<"2">> },
            CanonicalPage#{ <<"count">> => <<"02">> },
            CanonicalPage#{ <<"kind">> => <<"unknown">> },
            CanonicalPage#{ <<"prefix">> => <<"root">> },
            CanonicalPage#{ <<"body">> => nonminimal_balance(Body) },
            CanonicalPage#{
                <<"body">> =>
                    encode_test_accounts([Second, First])
            },
            (test_page(root, [First]))#{
                <<"count">> => 2,
                <<"body">> => encode_test_accounts([First, First])
            },
            test_page(
                root,
                accounts(lib_arweave_accounts:page_size() + 1)
            )
        ],
    lists:foreach(
        fun(Page) ->
            ?assertEqual(
                <<"invalid-account-tree">>,
                verify_error(graph_state(Page, Opts), Opts)
            )
        end,
        InvalidPages
    ),
    SmallBranch = test_branch(
        [
            {<<1>>, test_page(<<1>>, [addressed_account(1, 1)])},
            {<<2>>, test_page(<<2>>, [addressed_account(2, 1)])}
        ],
        Opts
    ),
    ?assertEqual(
        <<"invalid-account-tree">>,
        verify_error(graph_state(SmallBranch, Opts), Opts)
    ),
    PrefixOnePage = test_page(<<1>>, [addressed_account(1, 1)]),
    PrefixTwoPage =
        test_page(
            <<2>>,
            [addressed_account(2, N) || N <- lists:seq(1, 256)]
        ),
    ValidWideBranch = test_branch(
        [{<<1>>, PrefixOnePage}, {<<2>>, PrefixTwoPage}],
        Opts
    ),
    ?assertMatch(
        {ok, _},
        hb_ao:resolve(
            graph_state(ValidWideBranch, Opts),
            #{
                <<"path">> => <<"verify">>,
                <<"expected-root">> => maps:get(<<"root">>, ValidWideBranch)
            },
            Opts
        )
    ),
    WideBody = maps:get(<<"body">>, ValidWideBranch),
    WideSpecs = branch_specs(ValidWideBranch, Opts),
    InvalidBranches =
        [
            ValidWideBranch#{ <<"body">> => <<WideBody/binary, 0>> },
            forge_vector_size(ValidWideBranch, <<"2">>, Opts),
            ValidWideBranch#{
                <<"body">> => encode_branch_specs(lists:reverse(WideSpecs))
            },
            ValidWideBranch#{
                <<"body">> => encode_branch_specs([hd(WideSpecs) | WideSpecs])
            }
        ],
    lists:foreach(
        fun(Branch) ->
            ?assertEqual(
                <<"invalid-account-tree">>,
                verify_error(graph_state(Branch, Opts), Opts)
            )
        end,
        InvalidBranches
    ),
    WideBranch = test_branch(
        [
            {<<1>>, test_page(<<1>>, [addressed_account(3, 1)])},
            {<<2>>, PrefixTwoPage}
        ],
        Opts
    ),
    ?assertEqual(
        <<"invalid-account-tree">>,
        verify_error(graph_state(WideBranch, Opts), Opts)
    ),
    OverlongBranch = test_branch(
        [
            {<<1, 0:248, 1>>, test_page(root, [addressed_account(1, 1)])},
            {<<1, 0:248, 2>>, test_page(root, [addressed_account(1, 2)])}
        ],
        Opts
    ),
    OverlongRoot = test_branch(
        [
            {<<1>>, OverlongBranch},
            {<<2>>, test_page(root, [addressed_account(2, 1)])}
        ],
        Opts
    ),
    ?assertEqual(
        <<"invalid-account-tree">>,
        error_code(
            get_account(
                graph_state(OverlongRoot, Opts),
                <<1, 0:248>>,
                Opts
            ),
            Opts
        )
    ).

%% @doc Internal state and graph writes do not create match-index entries.
internal_match_index_test() ->
    Opts = opts(<<"match-index">>),
    MatchOpts = maps:remove(<<"forge-bootstrap">>, Opts),
    State = ingest([accounts(512)], [], Opts),
    ?assertEqual(
        {error, not_found},
        hb_cache:match(#{ <<"kind">> => <<"page">> }, MatchOpts)
    ),
    ?assertEqual(
        {error, not_found},
        hb_cache:match(
            #{ <<"kind">> => <<"branch">>, <<"count">> => 512 },
            MatchOpts
        )
    ),
    ?assertEqual(
        {error, not_found},
        hb_cache:match(
            #{
                <<"device">> => ?DEVICE,
                <<"root">> => resolved_root(State, Opts)
            },
            MatchOpts
        )
    ),
    Control = #{ <<"control">> => <<"indexed">> },
    {ok, ControlID} = hb_cache:write(Control, MatchOpts),
    ?assertEqual({ok, [ControlID]}, hb_cache:match(Control, MatchOpts)).

%% @doc Bootstrap checks the expected root before making a state durable.
finalize_checks_root_test() ->
    Opts = opts(<<"finalize-root">>),
    Accumulator = insert(#{ <<"device">> => ?DEVICE }, accounts(), Opts),
    Before = store_stats(Opts),
    ?assertNotMatch(
        {ok, _},
        hb_ao:resolve(
            Accumulator,
            #{
                <<"path">> => <<"finalize">>,
                <<"expected-root">> => hb_util:encode(<<0:384>>)
            },
            Opts
        )
    ),
    ?assertEqual(Before, store_stats(Opts)).

%% @doc Legacy slabs and predecessor fields are not empty account states.
rejects_legacy_state_shapes_test() ->
    Opts = opts(<<"legacy-shape">>),
    Legacy =
        #{
            <<"device">> => ?DEVICE,
            <<"root">> => <<>>,
            <<"chunks">> => #{},
            <<"previous-root">> => <<>>,
            <<"previous">> => <<"state-id">>
        },
    State = ingest([accounts()], [], Opts),
    LinkedLegacy =
        (hb_private:reset(State))#{
            <<"chunks">> => #{},
            <<"previous-root">> => <<>>
        },
    lists:foreach(
        fun({Base, Expected}) ->
            Requests =
                [
                    #{ <<"path">> => <<"root">> },
                    #{
                        <<"path">> => <<"verify">>,
                        <<"expected-root">> => Expected
                    },
                    #{ <<"path">> => <<"apply">>, <<"diff">> => #{} }
                ],
            lists:foreach(
                fun(Request) ->
                    ?assertEqual(
                        <<"invalid-account-tree">>,
                        error_code(
                            hb_ao:resolve(Base, Request, Opts),
                            Opts
                        )
                    )
                end,
                Requests
            )
        end,
        [{Legacy, <<>>}, {LinkedLegacy, resolved_root(State, Opts)}]
    ).

%% @doc Generic account boundaries reject malformed and non-canonical values.
rejects_malformed_generic_accounts_test() ->
    Opts = opts(<<"invalid-account">>),
    Address = hb_util:encode(<<1:256>>),
    Valid = account_message({5, <<>>}),
    Invalid =
        [
            42,
            maps:remove(<<"balance">>, Valid),
            Valid#{ <<"balance">> => -1 },
            Valid#{ <<"denomination">> => 0 },
            Valid#{ <<"mining-permission">> => <<"maybe">> },
            Valid#{ <<"last-tx">> => <<"!">> }
        ],
    lists:foreach(
        fun(Account) ->
            ?assertEqual(
                <<"invalid-account">>,
                insert_error(#{Address => Account}, Opts)
            )
        end,
        Invalid
    ),
    ?assertEqual(
        <<"invalid-account">>,
        insert_error(#{ <<"!">> => Valid }, Opts)
    ),
    ?assertEqual(<<"invalid-account">>, insert_error(42, Opts)),
    State = ingest([[{<<1:256>>, {5, <<>>}}]], [], Opts),
    {error, ApplyError} =
        apply_diff(State, #{Address => hd(Invalid)}, [], Opts),
    ?assertEqual(
        <<"invalid-account">>,
        hb_maps:get(<<"message">>, ApplyError, not_found, Opts)
    ),
    {error, ScalarDiffError} =
        hb_ao:resolve(
            State,
            #{ <<"path">> => <<"apply">>, <<"diff">> => 42 },
            Opts
        ),
    ?assertEqual(
        <<"invalid-account">>,
        hb_maps:get(<<"message">>, ScalarDiffError, not_found, Opts)
    ),
    {error, GetError} =
        hb_ao:resolve(
            State,
            #{ <<"path">> => <<"get">>, <<"address">> => <<"a">> },
            Opts
        ),
    ?assertEqual(
        <<"invalid-address">>,
        hb_maps:get(<<"message">>, GetError, not_found, Opts)
    ),
    {error, ManyError} =
        hb_ao:resolve(
            State,
            #{ <<"path">> => <<"get">>, <<"addresses">> => 42 },
            Opts
        ),
    ?assertEqual(
        <<"invalid-address">>,
        hb_maps:get(<<"message">>, ManyError, not_found, Opts)
    ),
    {error, KeyError} =
        hb_ao:resolve(
            State,
            #{
                <<"path">> => <<"get">>,
                <<"addresses">> => #{ <<"not-a-number">> => Address }
            },
            Opts
        ),
    ?assertEqual(
        <<"invalid-address">>,
        hb_maps:get(<<"message">>, KeyError, not_found, Opts)
    ).

%%% Test helpers.

accounts() ->
    accounts(64).

accounts(Count) ->
    [
        {
            crypto:hash(sha256, <<"account-", N:16>>),
            {N * 1000, crypto:hash(sha256, <<"last-tx-", N:16>>)}
        }
    || N <- lists:seq(1, Count)
    ].

clustered_accounts(Prefix, Count) ->
    [clustered_account(Prefix, N) || N <- lists:seq(1, Count)].

clustered_account(Prefix, N) ->
    Tail = binary:part(crypto:hash(sha256, <<Prefix/binary, N:32>>), 0, 30),
    {<<Prefix/binary, Tail/binary>>, {N * 1000, <<>>}}.

path_accounts() ->
    [
        {
            <<
                Prefix,
                (binary:part(
                    crypto:hash(sha256, <<Prefix, N:32>>),
                    0,
                    31
                ))/binary
            >>,
            {Prefix * 1000 + N, <<>>}
        }
    || Prefix <- lists:seq(1, 4),
       N <- lists:seq(1, 80)
    ].

ingest(Pages, ExpectedRoot, Opts) ->
    Accumulator =
        lists:foldl(
            fun(Page, State) -> insert(State, Page, Opts) end,
            #{ <<"device">> => ?DEVICE },
            Pages
        ),
    Request =
        maps:merge(
            #{ <<"path">> => <<"finalize">> },
            case ExpectedRoot of
                [] -> #{};
                _ -> #{ <<"expected-root">> => ExpectedRoot }
            end
        ),
    {ok, State} = hb_ao:resolve(Accumulator, Request, Opts),
    State.

insert(State, Accounts, Opts) ->
    {ok, Inserted} =
        hb_ao:resolve(
            State,
            #{
                <<"path">> => <<"insert">>,
                <<"accounts">> => accounts_message(Accounts)
            },
            Opts
        ),
    Inserted.

accounts_message(Accounts) ->
    maps:from_list(
        [
            {hb_util:encode(Address), account_message(Account)}
        || {Address, Account} <- Accounts
        ]
    ).

account_message(Account) ->
    lib_arweave_accounts:account_message(Account).

resolved_root(State, Opts) ->
    {ok, Result} = hb_ao:resolve(State, <<"root">>, Opts),
    hb_maps:get(<<"root">>, Result, not_found, Opts).

get_account(State, Address, Opts) ->
    hb_ao:resolve(
        State,
        #{ <<"path">> => <<"get">>, <<"address">> => hb_util:encode(Address) },
        Opts
    ).

balance(State, Address, Opts) ->
    {ok, Account} = get_account(State, Address, Opts),
    hb_util:int(hb_maps:get(<<"balance">>, Account, not_found, Opts)).

credit(Address, Balance) ->
    #{hb_util:encode(Address) => account_message({Balance, <<>>})}.

apply_diff(State, Diff, Expected, Opts) ->
    hb_ao:resolve(
        State,
        #{
            <<"path">> => <<"apply">>,
            <<"diff">> => Diff,
            <<"expected-root">> => Expected
        },
        Opts
    ).

native_tree(Accounts) ->
    lib_arweave_accounts:insert_all(Accounts, lib_arweave_accounts:new()).

trace_loads(Count) ->
    receive
        {trace, _PID, call, {hb_cache, read, _Args}} ->
            trace_loads(Count + 1);
        {count, From} ->
            From ! {trace_count, Count}
    end.

verify_error(State, Opts) ->
    hb_maps:get(
        <<"message">>,
        element(
            2,
            hb_ao:resolve(
                State,
                #{
                    <<"path">> => <<"verify">>,
                    <<"expected-root">> => maps:get(<<"root">>, State)
                },
                Opts
            )
        ),
        not_found,
        Opts
    ).

graph_state(Node, Opts) ->
    {ok, ID} = hb_cache:write(Node, Opts#{ <<"match-index">> => false }),
    #{
        <<"device">> => ?DEVICE,
        <<"root">> => maps:get(<<"root">>, Node),
        <<"tree">> => to_link(ID)
    }.

test_page(_Prefix, Accounts) ->
    {Root, _Tree} = lib_arweave_accounts:root(native_tree(Accounts)),
    #{
        <<"kind">> => <<"page">>,
        <<"root">> => Root,
        <<"count">> => length(Accounts),
        <<"body">> => encode_test_accounts(lists:keysort(1, Accounts))
    }.

test_branch(PrefixedPages, Opts) ->
    Sorted = lists:keysort(1, PrefixedPages),
    Specs =
        [
            begin
                {ok, ID} = hb_cache:write(
                    Page,
                    Opts#{ <<"match-index">> => false }
                ),
                {
                    Prefix,
                    hb_util:decode(maps:get(<<"root">>, Page)),
                    maps:get(<<"count">>, Page),
                    to_link(ID)
                }
            end
        || {Prefix, Page} <- Sorted
        ],
    Roots = [Root || {_Prefix, Root, _Count, _Link} <- Specs],
    Children = test_child_vector([Link || {_, _, _, Link} <- Specs], Opts),
    #{
        <<"kind">> => <<"branch">>,
        <<"root">> => hb_util:encode(ar_deep_hash:hash(Roots)),
        <<"count">> =>
            lists:sum(
                [Count || {_Prefix, _Root, Count, _Link} <- Specs]
            ),
        <<"body">> => encode_branch_specs(Specs),
        <<"children">> => Children
    }.

test_child_vector(Links, Opts) ->
    Leaves =
        [
            write_test_vector(<<"children-leaf">>, Group, Opts)
        || Group <- groups(Links, ?CHILD_VECTOR_WIDTH)
        ],
    case Leaves of
        [Leaf] -> Leaf;
        _ -> write_test_vector(<<"children-index">>, Leaves, Opts)
    end.

write_test_vector(Kind, Links, Opts) ->
    Vector =
        maps:merge(
            #{ <<"kind">> => Kind, <<"size">> => length(Links) },
            maps:from_list(
                lists:zip(
                    [integer_to_binary(N) || N <- lists:seq(1, length(Links))],
                    Links
                )
            )
        ),
    {ok, ID} = hb_cache:write(Vector, Opts#{ <<"match-index">> => false }),
    to_link(ID).

groups([], _Width) -> [];
groups(Values, Width) ->
    {Group, Rest} = lists:split(erlang:min(Width, length(Values)), Values),
    [Group | groups(Rest, Width)].

addressed_account(Prefix, N) ->
    Hash = crypto:hash(sha256, <<Prefix, N:32>>),
    {<<Prefix, (binary:part(Hash, 0, 31))/binary>>, {N, <<>>}}.

encode_test_accounts(Accounts) ->
    iolist_to_binary([encode_test_account(Account) || Account <- Accounts]).

encode_test_account({Address, {Balance, LastTX}}) ->
    encode_test_account({Address, {Balance, LastTX, 1, true}});
encode_test_account(
    {Address, {Balance, LastTX, Denomination, MiningPermission}}
) ->
    BalanceBin = binary:encode_unsigned(Balance),
    DenominationBin = binary:encode_unsigned(Denomination),
    Permission = case MiningPermission of true -> 1; false -> 0 end,
    <<
        (byte_size(Address)):32,
        Address/binary,
        (byte_size(BalanceBin)):16,
        BalanceBin/binary,
        (byte_size(LastTX)):32,
        LastTX/binary,
        (byte_size(DenominationBin)):16,
        DenominationBin/binary,
        Permission:8
    >>.

nonminimal_balance(
    <<
        AddressSize:32,
        Address:AddressSize/binary,
        BalanceSize:16,
        Balance:BalanceSize/binary,
        Rest/binary
    >>
) ->
    <<
        AddressSize:32,
        Address/binary,
        (BalanceSize + 1):16,
        0,
        Balance/binary,
        Rest/binary
    >>.

branch_specs(Node, Opts) ->
    Metadata =
        decode_branch_specs(
            hb_maps:get(<<"body">>, Node, not_found, Opts),
            []
        ),
    Links = vector_links(maps:get(<<"children">>, Node), length(Metadata), Opts),
    [
        {Prefix, Root, Count, Link}
    || {{Prefix, Root, Count}, Link} <- lists:zip(Metadata, Links)
    ].

decode_branch_specs(<<>>, Specs) ->
    lists:reverse(Specs);
decode_branch_specs(
    <<
        PrefixSize:16,
        Prefix:PrefixSize/binary,
        Root:48/binary,
        Count:64/unsigned-big,
        Rest/binary
    >>,
    Specs
) ->
    decode_branch_specs(
        Rest,
        [
            {Prefix, Root, Count}
        | Specs
        ]
    ).

encode_branch_specs(Specs) ->
    iolist_to_binary(
        [
            <<
                (byte_size(Prefix)):16,
                Prefix/binary,
                Root/binary,
                Count:64/unsigned-big
            >>
        || {Prefix, Root, Count, _Link} <- Specs
        ]
    ).

vector_links(Link, Size, Opts) when Size =< ?CHILD_VECTOR_WIDTH ->
    Vector = hb_cache:ensure_loaded(Link, Opts),
    [maps:get(integer_to_binary(N), Vector) || N <- lists:seq(1, Size)];
vector_links(Link, Size, Opts) ->
    Index = hb_cache:ensure_loaded(Link, Opts),
    LeafCount =
        (Size + ?CHILD_VECTOR_WIDTH - 1) div ?CHILD_VECTOR_WIDTH,
    lists:append(
        [
            begin
                Leaf = hb_cache:ensure_loaded(
                    maps:get(integer_to_binary(N), Index),
                    Opts
                ),
                LeafSize = erlang:min(
                    ?CHILD_VECTOR_WIDTH,
                    Size - ((N - 1) * ?CHILD_VECTOR_WIDTH)
                ),
                [
                    maps:get(integer_to_binary(I), Leaf)
                || I <- lists:seq(1, LeafSize)
                ]
            end
        || N <- lists:seq(1, LeafCount)
        ]
    ).

topology_ids(State, Opts) ->
    Link = maps:get(<<"tree">>, hb_private:reset(State)),
    topology_ids(Link, sets:new(), Opts).

topology_ids(Link, Seen, Opts) ->
    ID = target_id(Link, Opts),
    case sets:is_element(ID, Seen) of
        true -> Seen;
        false ->
            Node = hb_cache:ensure_loaded(Link, Opts),
            Seen2 = sets:add_element(ID, Seen),
            lists:foldl(
                fun(ChildLink, Acc) -> topology_ids(ChildLink, Acc, Opts) end,
                Seen2,
                message_links(Node)
            )
    end.

topology_messages(Link, Seen, Opts) ->
    ID = target_id(Link, Opts),
    case maps:is_key(ID, Seen) of
        true -> Seen;
        false ->
            Node = hb_cache:ensure_loaded(Link, Opts),
            Seen2 = Seen#{ ID => Node },
            lists:foldl(
                fun(ChildLink, Acc) ->
                    topology_messages(ChildLink, Acc, Opts)
                end,
                Seen2,
                message_links(Node)
            )
    end.

message_links(Node) ->
    Public = hb_private:reset(Node),
    case maps:get(<<"kind">>, Public, not_found) of
        <<"page">> -> [];
        <<"branch">> -> [maps:get(<<"children">>, Public)];
        Kind when Kind == <<"children-leaf">>;
                  Kind == <<"children-index">> ->
            [
                Value
            || {Key, Value} <- maps:to_list(Public),
               Key =/= <<"kind">>,
               Key =/= <<"size">>
            ]
    end.

target_id({link, ID, LinkOpts}, Opts) ->
    case maps:get(<<"lazy">>, LinkOpts, false) of
        true ->
            {ok, TargetID} = hb_cache:read(ID, Opts),
            true = ?IS_ID(TargetID),
            TargetID;
        false ->
            true = ?IS_ID(ID),
            ID
    end.

store_stats(Opts) ->
    #{ <<"db">> := DB } = hb_store:find(store(Opts)),
    ok = elmdb:flush(DB),
    {ok, Stats} =
        elmdb:fold(
            DB,
            fun(Key, Value, {Count, Bytes}) ->
                {Count + 1, Bytes + byte_size(Key) + byte_size(Value)}
            end,
            {0, 0}
        ),
    Stats.

cold(State, Opts) ->
    {ok, Cold} = hb_cache:read(state_id(State, Opts), Opts),
    Cold.

state_id(State, Opts) ->
    hb_message:id(
        hb_private:reset(State),
        none,
        Opts#{ <<"linkify-mode">> => discard }
    ).

tree_id(State, Opts) ->
    target_id(maps:get(<<"tree">>, hb_private:reset(State)), Opts).

tree_message(State, Opts) ->
    hb_private:reset(
        hb_cache:ensure_loaded(
            maps:get(<<"tree">>, hb_private:reset(State)),
            Opts
        )
    ).

poison_unrelated_reference(State, Target, Opts) ->
    Root = hb_maps:get(<<"tree">>, State, not_found, Opts),
    TargetPrefix = binary:part(Target, 0, 1),
    Specs = branch_specs(Root, Opts),
    [{OtherPrefix, _OtherRoot, _OtherCount, _OtherLink} | _] =
        [Spec || Spec = {Prefix, _, _, _} <- Specs, Prefix =/= TargetPrefix],
    BrokenRoot =
        replace_branch_child(
            Root,
            OtherPrefix,
            to_link(hb_util:encode(<<0:256>>)),
            Opts
        ),
    {ok, RootID} =
        hb_cache:write(BrokenRoot, Opts#{ <<"match-index">> => false }),
    (hb_message:uncommitted(State))#{ <<"tree">> => to_link(RootID) }.

forge_target_account(State, Target, Opts) ->
    Root = hb_maps:get(<<"tree">>, State, not_found, Opts),
    RootPrefix = binary:part(Target, 0, 1),
    Specs = branch_specs(Root, Opts),
    {RootPrefix, _RootHash, _RootCount, RootLink} =
        lists:keyfind(RootPrefix, 1, Specs),
    Page = hb_cache:ensure_loaded(RootLink, Opts),
    Body = hb_maps:get(<<"body">>, Page, not_found, Opts),
    ForgedPage =
        (hb_message:uncommitted(Page))#{ <<"body">> => flip_last(Body) },
    {ok, PageID} =
        hb_cache:write(ForgedPage, Opts#{ <<"match-index">> => false }),
    ForgedRoot = replace_branch_child(Root, RootPrefix, to_link(PageID), Opts),
    {ok, RootID} =
        hb_cache:write(ForgedRoot, Opts#{ <<"match-index">> => false }),
    (hb_message:uncommitted(State))#{ <<"tree">> => to_link(RootID) }.

forge_vector_size(Branch, Size, Opts) ->
    Children = hb_private:reset(
        hb_cache:ensure_loaded(maps:get(<<"children">>, Branch), Opts)
    ),
    (hb_message:uncommitted(Branch))#{
        <<"children">> => write_test_message(Children#{ <<"size">> => Size }, Opts)
    }.

replace_branch_child(Root, Prefix, Link, Opts) ->
    Specs = branch_specs(Root, Opts),
    Ordinal = child_ordinal(Specs, Prefix, 1),
    Children = maps:get(<<"children">>, Root),
    NewChildren = replace_vector_child(Children, length(Specs), Ordinal, Link, Opts),
    (hb_message:uncommitted(Root))#{ <<"children">> => NewChildren }.

child_ordinal([{Prefix, _Root, _Count, _Link} | _], Prefix, N) -> N;
child_ordinal([_ | Specs], Prefix, N) -> child_ordinal(Specs, Prefix, N + 1).

replace_vector_child(VectorLink, Size, Ordinal, Link, Opts)
        when Size =< ?CHILD_VECTOR_WIDTH ->
    Vector = hb_private:reset(hb_cache:ensure_loaded(VectorLink, Opts)),
    write_test_message(Vector#{ integer_to_binary(Ordinal) => Link }, Opts);
replace_vector_child(VectorLink, Size, Ordinal, Link, Opts) ->
    Index = hb_private:reset(hb_cache:ensure_loaded(VectorLink, Opts)),
    LeafOrdinal = ((Ordinal - 1) div ?CHILD_VECTOR_WIDTH) + 1,
    ValueOrdinal = ((Ordinal - 1) rem ?CHILD_VECTOR_WIDTH) + 1,
    Key = integer_to_binary(LeafOrdinal),
    Leaf = hb_private:reset(hb_cache:ensure_loaded(maps:get(Key, Index), Opts)),
    NewLeaf = write_test_message(
        Leaf#{ integer_to_binary(ValueOrdinal) => Link },
        Opts
    ),
    write_test_message(Index#{ Key => NewLeaf }, Opts).

write_test_message(Message, Opts) ->
    {ok, ID} = hb_cache:write(Message, Opts#{ <<"match-index">> => false }),
    to_link(ID).

flip_last(Binary) ->
    Size = byte_size(Binary) - 1,
    <<Head:Size/binary, Last>> = Binary,
    <<Head/binary, (Last bxor 16#ff)>>.

forge_root_count(State, Opts) ->
    Root = hb_maps:get(<<"tree">>, State, not_found, Opts),
    Count = hb_util:int(hb_maps:get(<<"count">>, Root, not_found, Opts)),
    Forged = (hb_message:uncommitted(Root))#{ <<"count">> => Count + 1 },
    {ok, ID} = hb_cache:write(Forged, Opts#{ <<"match-index">> => false }),
    (hb_message:uncommitted(State))#{ <<"tree">> => to_link(ID) }.

error_code({error, Error}, Opts) ->
    hb_maps:get(<<"message">>, Error, not_found, Opts).

insert_error(Accounts, Opts) ->
    {error, Error} =
        hb_ao:resolve(
            #{ <<"device">> => ?DEVICE },
            #{ <<"path">> => <<"insert">>, <<"accounts">> => Accounts },
            Opts
        ),
    hb_maps:get(<<"message">>, Error, not_found, Opts).

to_link(ID) ->
    {link, ID, #{ <<"type">> => <<"link">>, <<"lazy">> => false }}.

opts(Tag) ->
    Run = list_to_binary(os:getpid()),
    Root = <<"cache-TEST/arweave-wallets-", Run/binary>>,
    Store = #{
        <<"store-module">> => hb_store_lmdb,
        <<"name">> =>
            case Tag of
                <<"link-graph-copy">> ->
                    <<Root/binary, "-copy">>;
                _ ->
                    Root
            end
    },
    ok = hb_store:reset(Store),
    hb_forge_seed:with_forge_bootstrap(
        #{ <<"store">> => [Store], <<"match-index">> => [Store] },
        fun(Opts) ->
            Seeds = maps:get(<<"forge-bootstrap">>, Opts),
            Opts#{
                <<"forge-bootstrap">> =>
                    Seeds#{
                        ?DEVICE => dev_arweave_wallets,
                        <<"match@1.0">> => dev_match
                    }
            }
        end
    ).

store(Opts) -> hd(hb_opts:get(store, [], Opts)).
