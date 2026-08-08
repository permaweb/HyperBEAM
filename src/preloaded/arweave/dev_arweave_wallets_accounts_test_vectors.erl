%%% @doc Test vectors for Arweave account-tree storage.
-module(dev_arweave_wallets_accounts_test_vectors).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(CHUNK_PREFIX_BYTES, 1).

%%% Tests.

%% @doc The two account forms are distinct preimages, and the collapse rule
%% matches upstream's: a denomination-1, mining-permitted account is the pair,
%% and every other account is the quadruple. Vectors from
%% `src/core/lib/arweave/VENDOR.md'.
account_form_test() ->
    ?assertEqual({12345, <<3:256>>}, account_from_fields(12345, 1, true)),
    ?assertEqual({12345, <<3:256>>, 2, true}, account_from_fields(12345, 2, true)),
    ?assertEqual({12345, <<3:256>>, 1, false}, account_from_fields(12345, 1, false)),
    ?assertNotEqual(
        leaf_hash({12345, <<3:256>>}),
        leaf_hash({12345, <<3:256>>, 1, true})
    ).

%% @doc An account message round-trips through the tuple form without changing
%% the leaf hash -- the property that stops a message boundary from silently
%% re-arity-ing an account.
account_round_trip_test() ->
    lists:foreach(
        fun(Account) ->
            ?assertEqual(
                Account,
                lib_arweave_accounts:account(lib_arweave_accounts:account_message(Account), #{})
            )
        end,
        [
            {12345, <<3:256>>},
            {0, <<>>},
            {12345, <<3:256>>, 2, true},
            {10218569660, <<7:256>>, 1, false}
        ]
    ),
    % The pair and the quadruple it expands to are the same account, so they
    % must collapse to the same tuple.
    ?assertEqual(
        {12345, <<3:256>>},
        lib_arweave_accounts:account(lib_arweave_accounts:account_message({12345, <<3:256>>, 1, true}), #{})
    ).

%% @doc The root does not depend on the order accounts were inserted in. The
%% bootstrap rests on this: a peer pages the wallet list in its own order.
insertion_order_test() ->
    Accounts =
        [ {crypto:strong_rand_bytes(32), {N * 1000, crypto:strong_rand_bytes(32)}}
            || N <- lists:seq(1, 200) ],
    {Root, _} = lib_arweave_accounts:root(lib_arweave_accounts:insert_all(Accounts, lib_arweave_accounts:new())),
    {Reversed, _} = lib_arweave_accounts:root(lib_arweave_accounts:insert_all(lists:reverse(Accounts), lib_arweave_accounts:new())),
    {Shuffled, _} = lib_arweave_accounts:root(lib_arweave_accounts:insert_all(shuffle(Accounts), lib_arweave_accounts:new())),
    ?assertEqual(Root, Reversed),
    ?assertEqual(Root, Shuffled),
    % The negative control: the assertions above would hold vacuously if the
    % root ignored the accounts, so prove a changed account changes the root.
    [{Address, {Balance, LastTX}} | Rest] = Accounts,
    {Mutated, _} = lib_arweave_accounts:root(lib_arweave_accounts:insert_all([{Address, {Balance + 1, LastTX}} | Rest], lib_arweave_accounts:new())),
    ?assertNotEqual(Root, Mutated).

%% @doc The empty tree hashes to the empty binary, not to a digest of nothing.
empty_tree_test() ->
    ?assertEqual({<<>>, lib_arweave_accounts:new()}, lib_arweave_accounts:root(lib_arweave_accounts:new())).

%% @doc A chunk round-trips, and chunk assignment depends only on the address
%% prefix -- so a chunk's identity does not move when its neighbours change.
chunk_test() ->
    Accounts =
        lists:sort(
            [ {crypto:strong_rand_bytes(32), {N, <<>>}} || N <- lists:seq(1, 50) ]
        ),
    ?assertEqual(Accounts, lib_arweave_accounts:decode_chunk(lib_arweave_accounts:encode_chunk(Accounts))),
    Chunks = lib_arweave_accounts:chunks(Accounts),
    ?assertEqual(
        lists:sort(Accounts),
        lists:sort(lists:append(maps:values(Chunks)))
    ),
    lists:foreach(
        fun({Key, Members}) ->
            ?assertEqual([Key], lists:usort([ lib_arweave_accounts:chunk_of(A) || {A, _} <- Members ]))
        end,
        maps:to_list(Chunks)
    ).

%% @doc Mainnet holds accounts whose addresses are shorter than the 32 bytes a
%% key hash produces -- forty-eight of them at height 1975067, one of them
%% empty and holding 876060014779297 winston. They must chunk, store and hash
%% like any other account, since the root covers them.
short_address_test() ->
    Accounts =
        [
            {<<>>, {876060014779297, <<>>}},
            {<<177>>, {30000, <<>>}},
            {<<53, 236, 30, 178, 211, 90, 153>>, {100000000000, <<>>}},
            {<<1:256>>, {1, <<>>}}
        ],
    ?assertEqual(<<"0">>, lib_arweave_accounts:chunk_of(<<>>)),
    ?assertEqual(<<"177">>, lib_arweave_accounts:chunk_of(<<177>>)),
    Chunks = lib_arweave_accounts:chunks(lists:sort(Accounts)),
    ?assertEqual(
        lists:sort(Accounts),
        lists:sort(lists:append(maps:values(Chunks)))
    ),
    {Root, _} = lib_arweave_accounts:root(lib_arweave_accounts:insert_all(Accounts, lib_arweave_accounts:new())),
    ?assertNotEqual(<<>>, Root),
    % Removing the empty-address account must change the root, or it is not
    % actually covered.
    {Without, _} = lib_arweave_accounts:root(lib_arweave_accounts:insert_all(tl(Accounts), lib_arweave_accounts:new())),
    ?assertNotEqual(Root, Without).

%% @doc A malformed page is an error, not a crash, and each malformation is
%% distinguished.
decode_page_rejects_test() ->
    ?assertEqual({error, <<"invalid-wallet-list-page">>}, lib_arweave_accounts:decode_page(<<"nonsense">>)),
    ?assertEqual(
        {error, <<"invalid-wallet-list-page">>},
        lib_arweave_accounts:decode_page(term_to_binary(#{ wallets => [] }))
    ),
    ?assertEqual(
        {error, <<"invalid-wallet-list-cursor">>},
        lib_arweave_accounts:decode_page(term_to_binary(#{ next_cursor => 17, wallets => [] }))
    ),
    ?assertEqual(
        {error, <<"invalid-account">>},
        lib_arweave_accounts:decode_page(
            term_to_binary(
                #{ next_cursor => last, wallets => [{<<1:256>>, {-1, <<>>}}] }
            )
        )
    ),
    ?assertEqual(
        {ok, last, [{<<1:256>>, {5, <<>>}}]},
        lib_arweave_accounts:decode_page(
            term_to_binary(
                #{ next_cursor => last, wallets => [{<<1:256>>, {5, <<>>}}] }
            )
        )
    ).

%% @doc A compressed body is refused on its format rather than decoded. The
%% payloads here are legitimate and would decode fine uncompressed, so what is
%% being pinned is the refusal, not a malformation.
%%
%% `term_to_binary/2' emits the compressed format only when it is smaller than
%% the plain one, so each payload is asserted to actually carry the `<<131, 80>>'
%% header. Without that, a test built on a term too small to compress asserts
%% nothing.
decode_rejects_compressed_test() ->
    Accounts = [ {<<N:256>>, {5, <<>>}} || N <- lists:seq(1, 64) ],
    Page = term_to_binary(#{ next_cursor => last, wallets => Accounts },
        [compressed]),
    Chunk = term_to_binary(Accounts, [compressed]),
    ?assertMatch(<<131, 80, _/binary>>, Page),
    ?assertMatch(<<131, 80, _/binary>>, Chunk),
    ?assertEqual({error, <<"invalid-wallet-list-page">>}, lib_arweave_accounts:decode_page(Page)),
    ?assertError(badarg, lib_arweave_accounts:decode_chunk(Chunk)),
    % The same data uncompressed still decodes, so the guard refuses the format
    % rather than the content.
    ?assertEqual(
        {ok, last, Accounts},
        lib_arweave_accounts:decode_page(term_to_binary(#{ next_cursor => last, wallets => Accounts }))
    ),
    ?assertEqual(Accounts, lib_arweave_accounts:decode_chunk(term_to_binary(Accounts))),
    % A body whose declared inflated size is enormous is refused on the header,
    % before anything is allocated against it: 64 KiB asking for 64 MiB.
    Bomb = term_to_binary(binary:copy(<<0>>, 64 * 1024 * 1024), [compressed]),
    ?assertMatch(<<131, 80, _/binary>>, Bomb),
    ?assert(byte_size(Bomb) < 100_000),
    ?assertEqual({error, <<"invalid-wallet-list-page">>}, lib_arweave_accounts:decode_page(Bomb)).

%% @doc A chunk's accounts are patched in place: an entry replaces, an entry
%% valued `remove' deletes, and the result stays in ascending address order so
%% that an unchanged chunk keeps its content address.
patch_test() ->
    Accounts = [{<<1:256>>, {1, <<>>}}, {<<2:256>>, {2, <<>>}}, {<<3:256>>, {3, <<>>}}],
    ?assertEqual(Accounts, lib_arweave_accounts:patch(Accounts, [])),
    ?assertEqual(
        [{<<1:256>>, {9, <<>>}}, {<<2:256>>, {2, <<>>}}, {<<3:256>>, {3, <<>>}}],
        lib_arweave_accounts:patch(Accounts, [{<<1:256>>, {9, <<>>}}])
    ),
    ?assertEqual(
        [{<<1:256>>, {1, <<>>}}, {<<3:256>>, {3, <<>>}}],
        lib_arweave_accounts:patch(Accounts, [{<<2:256>>, remove}])
    ),
    ?assertEqual(
        [{<<0:256>>, {7, <<>>}} | Accounts],
        lib_arweave_accounts:patch(Accounts, [{<<0:256>>, {7, <<>>}}])
    ),
    ?assertEqual([], lib_arweave_accounts:patch(Accounts, [ {A, remove} || {A, _} <- Accounts ])).

%% @doc A diff applies and reverses back to the tree it started from.
diff_round_trip_test() ->
    Address = <<1:256>>,
    Tree = lib_arweave_accounts:insert_all([{Address, {100, <<>>}}, {<<2:256>>, {5, <<>>}}], lib_arweave_accounts:new()),
    Diff = #{ Address => {200, <<9:256>>}, <<3:256>> => {1, <<>>} },
    Reverse = lib_arweave_accounts:reverse_diff(Diff, Tree),
    ?assertEqual(#{ Address => {100, <<>>}, <<3:256>> => remove }, Reverse),
    {Before, _} = lib_arweave_accounts:root(Tree),
    {After, _} = lib_arweave_accounts:root(lib_arweave_accounts:apply_diff(Diff, Tree)),
    ?assertNotEqual(Before, After),
    {Restored, _} = lib_arweave_accounts:root(lib_arweave_accounts:apply_diff(Reverse, lib_arweave_accounts:apply_diff(Diff, Tree))),
    ?assertEqual(Before, Restored).

%%% Test helpers.

account_from_fields(Balance, Denomination, MiningPermission) ->
    lib_arweave_accounts:account(
        #{
            <<"balance">> => Balance,
            <<"last-tx">> => hb_util:encode(<<3:256>>),
            <<"denomination">> => Denomination,
            <<"mining-permission">> => MiningPermission
        },
        #{}
    ).

leaf_hash(Account) ->
    {Root, _} = lib_arweave_accounts:root(lib_arweave_accounts:insert(<<1:256>>, Account, lib_arweave_accounts:new())),
    Root.

shuffle(List) ->
    [ X || {_, X} <- lists:sort([ {rand:uniform(), X} || X <- List ]) ].
