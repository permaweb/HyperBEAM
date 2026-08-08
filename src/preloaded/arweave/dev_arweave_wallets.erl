%%% @doc An AO-Core interface to Arweave's account tree: the Merkle-Patricia
%%% trie whose root every block header commits to as `wallet-list'.
%%%
%%% That root is the strongest correctness property in the subsystem. It
%%% covers every account after the block was applied, so a transition that is
%%% wrong by one winston -- or that stores an account in the wrong of the two
%%% hash forms, or drops one, or keeps a stale `last-tx' -- produces a
%%% different root. Mainnet is the oracle: the tree assembled from a peer's
%%% `/wallet_list' pages must hash to the value the block signed.
%%%
%%% The device owns four things: parsing a peer's pages, assembling them into
%%% a tree, verifying the tree's root, and the store layout the tree lives in.
%%% Fetching the pages belongs to `~arweave@2.9/bootstrap', which hands each
%%% body to `page/3'.
%%%
%%% An accounts state is a message:
%%%
%%% ```
%%% #{
%%%     <<"device">>   => <<"arweave-wallets@2.9">>,
%%%     <<"root">>     => B64URLRootHash,
%%%     <<"size">>     => AccountCount,
%%%     <<"chunks">>   => #{ <<"0">> => Link, ..., <<"255">> => Link },
%%%     <<"previous">> => B64URLRootHash of the state this was derived from
%%% }
%%% '''
%%%
%%% Chunks are selected by the first byte of the address, not by a running
%%% count: count-based boundaries move for every chunk after an insertion, so
%%% one new account would rewrite the whole tree, whereas prefix boundaries
%%% are fixed and a block rewrites only the chunks holding the accounts it
%%% touched. Each chunk is a separate cache entry reached through a link, so a
%%% `get' loads one chunk rather than the tree. States are indexed in the
%%% store by their root -- the identity the block header and the peer API both
%%% use -- at `~arweave-wallets@2.9/trees/<Root>', which is what `rollback/3'
%%% walks.
%%%
%%% Two properties are load-bearing and are asserted, not assumed. The root
%%% does not depend on the order accounts were inserted in, which is what lets
%%% a peer page the list however it likes. And `apply/3' threads the memoised
%%% tree forward through the state's private section, so applying a block
%%% costs a walk of the accounts it touched rather than of all of them; the
%%% private section never reaches the cache, so it cannot perturb a state's
%%% identity.
%%%
%%% Note that `root/3' shadows the `root' field its own state message carries.
%%% That is deliberate -- the key recomputes the value rather than reporting
%%% the recorded one, so a state whose recorded root is a lie is caught -- and
%%% it is safe only because every internal field read here goes through
%%% `hb_maps', never `hb_ao', and so never re-enters the device.
-module(dev_arweave_wallets).
-implements(<<"arweave-wallets@2.9">>).
-device_libraries([lib_arweave_accounts]).
-compile({no_auto_import, [apply/3]}).
-export([info/1, root/3, verify/3, get/3, apply/3, rollback/3, page/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEVICE, <<"arweave-wallets@2.9">>).

%% @doc Export only the account-tree operations, leaving message manipulation
%% to `message@1.0'.
info(_Base) ->
    #{
        excludes => [<<"keys">>, <<"set">>, <<"set-path">>, <<"remove">>]
    }.

%% @doc Compute the root hash over every account in the tree. The value is
%% recomputed from the accounts rather than read from the state's `root'
%% field, so it is a check on the state rather than a report of it.
root(Base, _Req, Opts) ->
    {Root, _Memoised} = lib_arweave_accounts:root(tree(Base, Opts)),
    {ok, #{ <<"root">> => Root }}.

%% @doc Check the tree's root against the `expected-root' a block committed
%% to. This is the account half of block validation: it passes only if every
%% account, every balance, every `last-tx' and every denomination matches what
%% the block's signer had.
verify(Base, Req, Opts) ->
    maybe
        Expected = required(<<"expected-root">>, Base, Req, Opts),
        {Root, _Memoised} = lib_arweave_accounts:root(tree(Base, Opts)),
        ok ?= match_root(Root, Expected),
        {ok, #{ <<"valid">> => true }}
    end.

%% @doc Return the account stored under `address', or the accounts stored
%% under each of `addresses'. The single-address form loads one chunk; the
%% list form loads one chunk per distinct address prefix.
get(Base, Req, Opts) ->
    case get_first(<<"address">>, Base, Req, not_found, Opts) of
        not_found -> get_many(Base, Req, Opts);
        Address -> get_one(Base, Address, Opts)
    end.

%% @doc Apply one block's account transition and return the new accounts
%% state. `diff' is the sparse set of accounts the block changed -- each an
%% account message, or the binary `remove' -- as produced by
%% `ar_node_utils:update_accounts/3'. When `expected-root' is given, the
%% resulting root must equal it; that is the check the block header commits
%% to, and it fails as `invalid-wallet-list-root'.
apply(Base, Req, Opts) ->
    maybe
        Diff =
            lib_arweave_accounts:diff(
                required(<<"diff">>, Base, Req, Opts),
                Opts
            ),
        Applied = lib_arweave_accounts:apply_diff(Diff, tree(Base, Opts)),
        {Root, Memoised} = lib_arweave_accounts:root(Applied),
        ok ?= match_root(Root, get_first(<<"expected-root">>, Base, Req, [], Opts)),
        {ok, write(Root, Memoised, chunks(Base, Diff, Opts), Base, Opts)}
    end.

%% @doc Unwind `depth' account states, following the chain of states each was
%% derived from. Reorgs are handled by returning to the state the forking
%% block was applied to, rather than by replaying reverse diffs: every state
%% is content-addressed and indexed by its root, so the earlier one is still
%% there to be read.
rollback(Base, Req, Opts) ->
    unwind(Base, hb_util:int(get_first(<<"depth">>, Base, Req, 1, Opts)), Opts).

%% @doc Ingest one `GET /wallet_list/<root>[/<cursor>]' response body into the
%% tree. Returns the accumulated state and the cursor to fetch next, or the
%% binary `last' when the peer has served the final page. The state is only
%% written to the store once the last page has arrived: a partial tree has no
%% meaningful root, so it carries none.
page(Base, Req, Opts) ->
    maybe
        Body = required(<<"body">>, Base, Req, Opts),
        {ok, Cursor, Accounts} ?= decode_page(Body),
        Tree = lib_arweave_accounts:insert_all(Accounts, tree(Base, Opts)),
        {ok,
            #{
                <<"accounts">> => page_state(Cursor, Tree, Base, Opts),
                <<"next-cursor">> => encode_cursor(Cursor)
            }
        }
    end.

%%% Internal functions.

%% @doc Return the accounts state a completed page leaves behind. The last
%% page finishes the tree, so its root is computed and the state is written;
%% every earlier page leaves an accumulator carrying only the tree so far.
page_state(last, Tree, Base, Opts) ->
    {Root, Memoised} = lib_arweave_accounts:root(Tree),
    write(Root, Memoised, chunks(Tree, Opts), Base, Opts);
page_state(_Cursor, Tree, _Base, _Opts) ->
    with_tree(
        #{
            <<"device">> => ?DEVICE,
            <<"size">> => lib_arweave_accounts:count(Tree)
        },
        Tree
    ).

%% @doc Map a peer's cursor onto its wire form. The peer signals exhaustion
%% with an atom, which no message may carry.
encode_cursor(last) -> <<"last">>;
encode_cursor(Cursor) -> hb_util:encode(Cursor).

%% @doc Parse a page body, mapping the parse failures onto the error
%% convention.
decode_page(Body) ->
    case lib_arweave_accounts:decode_page(Body) of
        {error, Message} ->
            {error,
                error_message(Message,
                    <<"The wallet list page could not be parsed.">>)};
        Parsed ->
            Parsed
    end.

%% @doc Write a state over a set of chunk links and index it under its root, so
%% that `rollback/3' can find it. The memoised tree rides along in the private
%% section, which the cache drops, so the state's identity depends only on its
%% accounts.
write(Root, Tree, Chunks, Base, Opts) ->
    State =
        maps:merge(
            #{
                <<"device">> => ?DEVICE,
                <<"root">> => Root,
                <<"size">> => lib_arweave_accounts:count(Tree),
                <<"chunks">> => Chunks
            },
            previous(Base, Opts)
        ),
    {ok, ID} = hb_cache:write(State, Opts),
    ok = hb_store:link(#{ tree_path(Root) => ID }, Opts),
    with_tree(State, Tree).

%% @doc Chunk a whole tree. This is the bootstrap path, where the tree arrives
%% from a peer rather than from a transition, so every chunk is new.
chunks(Tree, Opts) ->
    maps:map(
        fun(_Key, Accounts) -> chunk_link(Accounts, Opts) end,
        lib_arweave_accounts:chunks(lib_arweave_accounts:to_list(Tree))
    ).

%% @doc Rewrite only the chunks a diff touched, carrying the rest over as the
%% links the previous state already holds. A block changes a few dozen accounts
%% out of hundreds of thousands, so re-chunking every account for each block
%% would cost far more than the transition itself. A base that is not yet a
%% written state has no chunks to carry over, so its tree is chunked whole.
chunks(Base, Diff, Opts) ->
    case hb_maps:get(<<"chunks">>, Base, not_found, Opts) of
        not_found ->
            chunks(lib_arweave_accounts:apply_diff(Diff, tree(Base, Opts)), Opts);
        Chunks ->
            maps:fold(
                fun(Key, Entries, Acc) -> patch_chunk(Key, Entries, Acc, Opts) end,
                Chunks,
                lib_arweave_accounts:chunks(maps:to_list(Diff))
            )
    end.

%% @doc Apply one chunk's share of a diff and write the result. A chunk the
%% diff empties is dropped rather than stored as an empty list.
patch_chunk(Key, Entries, Chunks, Opts) ->
    case lib_arweave_accounts:patch(existing_chunk(Key, Chunks, Opts), Entries) of
        [] -> maps:remove(Key, Chunks);
        Patched -> Chunks#{ Key => chunk_link(Patched, Opts) }
    end.

%% @doc The accounts a chunk already holds, or none when the diff creates it.
existing_chunk(Key, Chunks, Opts) ->
    case hb_maps:is_key(Key, Chunks, Opts) of
        false -> [];
        true -> chunk(Key, Chunks, Opts)
    end.

%% @doc Write one chunk and return the link the state carries in its place.
chunk_link(Accounts, Opts) ->
    {ok, ID} =
        hb_cache:write(
            #{ <<"body">> => lib_arweave_accounts:encode_chunk(Accounts) },
            Opts
        ),
    {link, ID, #{ <<"type">> => <<"link">>, <<"lazy">> => false }}.

%% @doc Record the state a new one is derived from, so that a reorg can walk
%% back to it. An initial state has none, and so carries no key.
previous(Base, Opts) ->
    case hb_maps:get(<<"root">>, Base, not_found, Opts) of
        not_found -> #{};
        Root -> #{ <<"previous">> => Root }
    end.

%% @doc The store path a state is indexed under. The root is the identity both
%% the block header and the peer API use for an account tree, so it is the
%% identity the store uses too.
%%
%% `rollback/3' walks `previous' off a caller-supplied base, so a root can
%% arrive from a request. Nothing collapses `..' between here and the
%% filesystem, so a root carrying a separator would be resolved by the OS
%% rather than treated as a name -- which turns a tree lookup into an arbitrary
%% file read. A root is base64url and cannot contain one.
tree_path(Root) when is_binary(Root) ->
    case binary:match(Root, [<<"/">>, <<"..">>, <<0>>]) of
        nomatch ->
            << "~", ?DEVICE/binary, "/trees/", Root/binary >>;
        _ ->
            throw({unsafe_tree_path, Root})
    end.

%% @doc Walk back through the states each was derived from.
unwind(State, 0, _Opts) ->
    {ok, State};
unwind(State, Depth, Opts) ->
    maybe
        {ok, Previous} ?= read(hb_maps:get(<<"previous">>, State, not_found, Opts), Opts),
        unwind(Previous, Depth - 1, Opts)
    end.

%% @doc Read a state back by its root.
read(not_found, _Opts) ->
    {error, error_message(<<"unknown-account-tree">>,
        <<"The state does not record the one it was derived from.">>)};
read(Root, Opts) ->
    case hb_cache:read(tree_path(Root), Opts) of
        {ok, State} -> {ok, State};
        _ -> {error, error_message(<<"unknown-account-tree">>,
            <<"No account tree is stored under that root.">>)}
    end.

%% @doc Return one account as a message.
get_one(Base, Address, Opts) ->
    case lib_arweave_accounts:get(hb_util:decode(Address), tree(Base, Opts)) of
        not_found ->
            {error, error_message(<<"account-not-found">>,
                <<"The account tree holds no account at that address.">>)};
        Account ->
            {ok, lib_arweave_accounts:account_message(Account)}
    end.

%% @doc Return the accounts held under each of `addresses', keyed by address.
%% Addresses the tree does not hold are omitted, matching the sparse map
%% `ar_node_utils:update_accounts/3' expects.
get_many(Base, Req, Opts) ->
    Addresses = required(<<"addresses">>, Base, Req, Opts),
    Decoded =
        [ hb_util:decode(Address)
            || Address <- hb_util:message_to_ordered_list(Addresses, Opts) ],
    {ok,
        maps:fold(
            fun(Address, Account, Acc) ->
                Acc#{
                    hb_util:encode(Address) =>
                        lib_arweave_accounts:account_message(Account)
                }
            end,
            #{},
            lib_arweave_accounts:get_map(tree(Base, Opts), Decoded)
        )
    }.

%% @doc Compare a computed root against the one a block committed to. An
%% absent expectation disables the check with no branch of its own.
match_root(_Root, []) -> ok;
match_root(Root, Root) -> ok;
match_root(_Root, _Expected) ->
    {error, error_message(<<"invalid-wallet-list-root">>,
        <<"The account tree does not hash to the expected root.">>)}.

%% @doc Return the account tree an accounts state stands for. A state that has
%% already been resolved carries its tree, with each node's hash memoised,
%% in its private section; one read back from the cache is rebuilt from its
%% chunks, which costs a walk of every account and so happens once per state
%% rather than once per block.
tree(Base, Opts) ->
    case hb_private:get(<<"tree">>, Base, not_found, Opts) of
        not_found -> from_chunks(Base, Opts);
        Tree -> Tree
    end.

%% @doc Rebuild a tree from the chunks a state links.
from_chunks(Base, Opts) ->
    Chunks = hb_maps:get(<<"chunks">>, Base, #{}, Opts),
    lists:foldl(
        fun(Key, Tree) ->
            lib_arweave_accounts:insert_all(chunk(Key, Chunks, Opts), Tree)
        end,
        lib_arweave_accounts:new(),
        hb_maps:keys(Chunks, Opts)
    ).

%% @doc Load one chunk's accounts.
chunk(Key, Chunks, Opts) ->
    lib_arweave_accounts:decode_chunk(
        hb_maps:get(
            <<"body">>,
            hb_maps:get(Key, Chunks, not_found, Opts),
            not_found,
            Opts
        )
    ).

%% @doc Carry a materialised tree in a state's private section, where it
%% survives resolution but never reaches the cache.
with_tree(State, Tree) ->
    hb_private:set(State, <<"tree">>, Tree, #{}).

%% @doc Read a key from the request, falling back to the base message. Data
%% fields are read with `hb_maps' rather than `hb_ao': this device names a key
%% after a field its own state carries, and an `hb_ao' read would dispatch
%% back into the device instead of returning the value.
get_first(Key, Base, Req, Default, Opts) ->
    case hb_maps:get(Key, Req, not_found, Opts) of
        not_found -> hb_maps:get(Key, Base, Default, Opts);
        Value -> Value
    end.

%% @doc Read a key that has no meaningful default.
required(Key, Base, Req, Opts) ->
    case get_first(Key, Base, Req, not_found, Opts) of
        not_found -> throw({missing_key, Key});
        Value -> Value
    end.

%% @doc Build the standard error body.
error_message(Message, Detail) ->
    #{
        <<"status">> => 422,
        <<"message">> => Message,
        <<"detail">> => Detail
    }.

%%% Tests.

%% @doc A root cannot shape a store path. `rollback/3' walks `previous' off a
%% caller-supplied base, and nothing collapses `..' between here and the
%% filesystem, so a root carrying a separator would turn a tree lookup into an
%% arbitrary file read.
root_cannot_shape_a_store_path_test() ->
    ?assertThrow({unsafe_tree_path, _}, tree_path(<<"../../../secret">>)),
    ?assertThrow({unsafe_tree_path, _}, tree_path(<<"a/b">>)),
    Good = hb_util:encode(crypto:strong_rand_bytes(48)),
    ?assertMatch(<<"~arweave-wallets@2.9/trees/", _/binary>>, tree_path(Good)).

%% @doc The wallet list page frozen from mainnet at height 1975067, and the
%% root of the tree holding exactly its accounts. The page carries a
%% four-element account -- one of only three on mainnet -- so it exercises
%% both account hash forms rather than only the dominant one.
-define(FIXTURE, <<"test/fixtures/arweave/wallet-list-page.etf">>).
-define(FIXTURE_ROOT,
    <<"f0ySGk-nNkIzYO8o3ujSOp3csu8C8OjdyATLmFEght43QATPVQT3jaCox2TOezH8">>).
-define(FIXTURE_SIZE, 2500).
-define(FIXTURE_FOUR_TUPLE,
    <<"3JcDobLkwYuyyi8Knu2s_kGBBhVedN4w_Dg80I8PhcE">>).

opts() ->
    #{ <<"store">> => [hb_test_utils:test_store()] }.

fixture() ->
    {ok, Body} = file:read_file(?FIXTURE),
    Body.

%% @doc Ingest the frozen mainnet page and resolve its root. This is the whole
%% pipeline -- page parse, account decode, tree assembly, both hash forms --
%% measured against a value taken from the live network.
fixture_root_test() ->
    Opts = opts(),
    State = ingest(fixture(), Opts),
    ?assertEqual(?FIXTURE_ROOT, resolved_root(State, Opts)),
    ?assertEqual({ok, true}, verify_message(State, ?FIXTURE_ROOT, Opts)),
    ?assertEqual(?FIXTURE_SIZE, hb_maps:get(<<"size">>, State, not_found, Opts)).

%% @doc The page reports the cursor the peer gave it, so the bootstrap can
%% page on, and reports exhaustion as `last'.
page_cursor_test() ->
    Opts = opts(),
    {ok, Result} = ingest_result(fixture(), Opts),
    ?assertEqual(
        <<"3g6D-RiqIPA7KyEpnqXYnMDfnIhSG7EX9o3E8tiCJho">>,
        hb_maps:get(<<"next-cursor">>, Result, not_found, Opts)
    ),
    Split = accounts(),
    {ok, Last} =
        hb_ao:resolve(
            #{ <<"device">> => ?DEVICE },
            #{ <<"path">> => <<"page">>, <<"body">> => encode_page(last, Split) },
            Opts
        ),
    ?assertEqual(<<"last">>, hb_maps:get(<<"next-cursor">>, Last, not_found, Opts)).

%% @doc A tree assembled from pages served in a different order hashes to the
%% same root. The bootstrap rests on this: the peer chooses the paging order,
%% and the root must still match the one the block signed.
page_order_test() ->
    Opts = opts(),
    Accounts = accounts(),
    {Left, Right} = lists:split(length(Accounts) div 2, Accounts),
    Forwards = ingest_pages([Left, Right], Opts),
    Backwards = ingest_pages([Right, Left], Opts),
    Root = resolved_root(Forwards, Opts),
    ?assertEqual(Root, resolved_root(Backwards, Opts)),
    % The negative control. Both assertions above would hold vacuously if the
    % root ignored the accounts, so prove that dropping one changes it.
    Short = ingest_pages([Left, tl(Right)], Opts),
    ?assertNotEqual(Root, resolved_root(Short, Opts)).

%% @doc A page carrying the short and empty addresses mainnet actually holds
%% is ingested, chunked, stored and read back like any other. The empty
%% address is the one that broke the first version of the chunk assignment.
short_address_page_test() ->
    Opts = opts(),
    Accounts =
        [
            {<<>>, {876060014779297, <<>>}},
            {<<177>>, {30000, <<>>}},
            {<<105, 189>>, {20, <<>>}}
            | accounts()
        ],
    State = ingest(encode_page(last, Accounts), Opts),
    ?assertEqual(length(Accounts), hb_maps:get(<<"size">>, State, not_found, Opts)),
    {ok, Cold} = hb_cache:read(tree_path(resolved_root(State, Opts)), Opts),
    ?assertEqual(resolved_root(State, Opts), resolved_root(Cold, Opts)),
    {ok, Account} =
        hb_ao:resolve(Cold,
            #{ <<"path">> => <<"get">>, <<"address">> => hb_util:encode(<<>>) },
            Opts),
    ?assertEqual(876060014779297, hb_maps:get(<<"balance">>, Account, not_found, Opts)).

%% @doc One winston on one account, out of two and a half thousand taken from
%% mainnet, breaks the root. This is the check the whole device exists to
%% make.
mutate_balance_test() ->
    ?assertEqual(
        {error, <<"invalid-wallet-list-root">>},
        mutated(fun({Address, {Balance, LastTX}}) -> {Address, {Balance + 1, LastTX}} end)
    ).

%% @doc A missing account breaks the root.
mutate_missing_account_test() ->
    ?assertEqual(
        {error, <<"invalid-wallet-list-root">>},
        mutated(fun(_Account) -> drop end)
    ).

%% @doc A wrong `last-tx' breaks the root, even with the balance untouched.
mutate_last_tx_test() ->
    ?assertEqual(
        {error, <<"invalid-wallet-list-root">>},
        mutated(fun({Address, {Balance, _LastTX}}) -> {Address, {Balance, <<1:256>>}} end)
    ).

%% @doc A changed denomination breaks the root -- and note that it changes the
%% account's hash form as well as its contents, which is precisely the
%% substitution that would silently pass if the form were ignored.
mutate_denomination_test() ->
    ?assertEqual(
        {error, <<"invalid-wallet-list-root">>},
        mutated(fun({Address, {Balance, LastTX}}) -> {Address, {Balance, LastTX, 2, true}} end)
    ).

%% @doc Storing a denomination-1, mining-permitted account in the four-element
%% form breaks the root, although every field is unchanged. The two forms are
%% separate hash preimages, so the arity is consensus, not representation.
mutate_account_form_test() ->
    ?assertEqual(
        {error, <<"invalid-wallet-list-root">>},
        mutated(fun({Address, {Balance, LastTX}}) -> {Address, {Balance, LastTX, 1, true}} end)
    ).

%% @doc An extra account breaks the root.
mutate_extra_account_test() ->
    Opts = opts(),
    Extra = {crypto:strong_rand_bytes(32), {1, <<>>}},
    State = ingest(encode_page(last, [Extra | fixture_accounts()]), Opts),
    ?assertEqual(
        {error, <<"invalid-wallet-list-root">>},
        verify_message(State, ?FIXTURE_ROOT, Opts)
    ).

%% @doc A state whose recorded root is a lie does not pass verification: the
%% root is recomputed from the accounts rather than reported from the field.
%% Without this the device's own `root' key would shadow the field and return
%% whatever was written there.
recorded_root_is_not_trusted_test() ->
    Opts = opts(),
    State = ingest(fixture(), Opts),
    Lie = State#{ <<"root">> => hb_util:encode(crypto:strong_rand_bytes(48)) },
    ?assertEqual(?FIXTURE_ROOT, resolved_root(Lie, Opts)),
    ?assertEqual({ok, true}, verify_message(Lie, ?FIXTURE_ROOT, Opts)).

%% @doc A state read back from the store rebuilds its tree from the chunks it
%% links, and hashes to the same root. This is the path a node takes after a
%% restart, and the one that proves the chunk layout is lossless.
cold_state_test() ->
    Opts = opts(),
    State = ingest(fixture(), Opts),
    {ok, Cold} = hb_cache:read(tree_path(?FIXTURE_ROOT), Opts),
    ?assertEqual(not_found, hb_private:get(<<"tree">>, Cold, not_found, Opts)),
    ?assertEqual(?FIXTURE_ROOT, resolved_root(Cold, Opts)),
    ?assertEqual(
        hb_maps:get(<<"size">>, State, not_found, Opts),
        hb_maps:get(<<"size">>, Cold, not_found, Opts)
    ).

%% @doc Accounts are read back with the fields they were stored with, and the
%% four-element form keeps its denomination and mining permission.
get_account_test() ->
    Opts = opts(),
    State = ingest(fixture(), Opts),
    {ok, Banned} =
        hb_ao:resolve(State,
            #{ <<"path">> => <<"get">>, <<"address">> => ?FIXTURE_FOUR_TUPLE },
            Opts),
    ?assertEqual(10218569660, hb_maps:get(<<"balance">>, Banned, not_found, Opts)),
    ?assertEqual(1, hb_maps:get(<<"denomination">>, Banned, not_found, Opts)),
    ?assertEqual(false, hb_maps:get(<<"mining-permission">>, Banned, not_found, Opts)),
    {Address, {Balance, _}} = hd(fixture_accounts()),
    {ok, Ordinary} =
        hb_ao:resolve(State,
            #{ <<"path">> => <<"get">>, <<"address">> => hb_util:encode(Address) },
            Opts),
    ?assertEqual(Balance, hb_maps:get(<<"balance">>, Ordinary, not_found, Opts)),
    ?assertEqual(true, hb_maps:get(<<"mining-permission">>, Ordinary, not_found, Opts)),
    {error, Missing} =
        hb_ao:resolve(State,
            #{
                <<"path">> => <<"get">>,
                <<"address">> => hb_util:encode(crypto:strong_rand_bytes(32))
            },
            Opts),
    ?assertEqual(<<"account-not-found">>, hb_maps:get(<<"message">>, Missing, not_found, Opts)).

%% @doc Several accounts are read at once, and addresses the tree does not
%% hold are omitted rather than reported as errors -- the sparse shape
%% `ar_node_utils:update_accounts/3' takes.
get_addresses_test() ->
    Opts = opts(),
    State = ingest(fixture(), Opts),
    [{First, _}, {Second, _} | _] = fixture_accounts(),
    Absent = crypto:strong_rand_bytes(32),
    {ok, Accounts} =
        hb_ao:resolve(State,
            #{
                <<"path">> => <<"get">>,
                <<"addresses">> =>
                    hb_util:list_to_numbered_message(
                        [ hb_util:encode(A) || A <- [First, Second, Absent] ]
                    )
            },
            Opts),
    ?assertEqual(
        lists:sort([hb_util:encode(First), hb_util:encode(Second)]),
        lists:sort(hb_maps:keys(hb_private:reset(Accounts), Opts))
    ).

%% @doc Applying a diff produces the state the diff describes, and the root it
%% produces is the one the resulting accounts hash to.
apply_test() ->
    Opts = opts(),
    State = ingest(fixture(), Opts),
    [{Address, {Balance, _}} | _] = fixture_accounts(),
    {ok, Applied} = apply_diff(State, credit(Address, Balance + 1000), [], Opts),
    {ok, Account} =
        hb_ao:resolve(Applied,
            #{ <<"path">> => <<"get">>, <<"address">> => hb_util:encode(Address) },
            Opts),
    ?assertEqual(Balance + 1000, hb_maps:get(<<"balance">>, Account, not_found, Opts)),
    {ok, #{ <<"root">> := Root }} = hb_ao:resolve(Applied, <<"root">>, Opts),
    ?assertNotEqual(?FIXTURE_ROOT, Root),
    ?assertEqual(Root, hb_maps:get(<<"root">>, Applied, not_found, Opts)),
    ?assertEqual(?FIXTURE_ROOT, hb_maps:get(<<"previous">>, Applied, not_found, Opts)).

%% @doc An `expected-root' that the transition does not produce is rejected,
%% and an expectation that it does produce is accepted. This is how the block
%% device checks a block's `wallet-list' field.
apply_expected_root_test() ->
    Opts = opts(),
    State = ingest(fixture(), Opts),
    [{Address, {Balance, _}} | _] = fixture_accounts(),
    Diff = credit(Address, Balance + 1000),
    {ok, Applied} = apply_diff(State, Diff, [], Opts),
    Root = hb_maps:get(<<"root">>, Applied, not_found, Opts),
    ?assertMatch({ok, _}, apply_diff(State, Diff, Root, Opts)),
    {error, Error} = apply_diff(State, Diff, ?FIXTURE_ROOT, Opts),
    ?assertEqual(<<"invalid-wallet-list-root">>,
        hb_maps:get(<<"message">>, Error, not_found, Opts)),
    % A diff that credits one winston less must not satisfy the same root.
    {error, OffByOne} =
        apply_diff(State, credit(Address, Balance + 999), Root, Opts),
    ?assertEqual(<<"invalid-wallet-list-root">>,
        hb_maps:get(<<"message">>, OffByOne, not_found, Opts)).

%% @doc Removing an account from the tree is a transition like any other, and
%% the account is gone from the state it produces.
apply_remove_test() ->
    Opts = opts(),
    State = ingest(fixture(), Opts),
    [{Address, _} | _] = fixture_accounts(),
    {ok, Applied} =
        apply_diff(State, #{ hb_util:encode(Address) => <<"remove">> }, [], Opts),
    {error, Error} =
        hb_ao:resolve(Applied,
            #{ <<"path">> => <<"get">>, <<"address">> => hb_util:encode(Address) },
            Opts),
    ?assertEqual(<<"account-not-found">>, hb_maps:get(<<"message">>, Error, not_found, Opts)),
    ?assertEqual(?FIXTURE_SIZE - 1, hb_maps:get(<<"size">>, Applied, not_found, Opts)),
    % A cold read must agree: the removal has to reach the chunk, not only the
    % in-memory tree.
    Root = hb_maps:get(<<"root">>, Applied, not_found, Opts),
    {ok, Cold} = hb_cache:read(tree_path(Root), Opts),
    ?assertEqual(Root, resolved_root(Cold, Opts)),
    ?assertEqual(?FIXTURE_SIZE - 1, hb_maps:get(<<"size">>, Cold, not_found, Opts)).

%% @doc A transition rewrites only the chunks holding the accounts it touched,
%% and carries every other chunk over as the link the previous state already
%% held. Without this a block that changes a dozen accounts would re-chunk all
%% of them, which is the cost the prefix-keyed layout exists to avoid.
apply_rewrites_touched_chunks_test() ->
    Opts = opts(),
    State = ingest(fixture(), Opts),
    [{Address, {Balance, _}} | _] = fixture_accounts(),
    {ok, Applied} = apply_diff(State, credit(Address, Balance + 1000), [], Opts),
    Before = hb_maps:get(<<"chunks">>, State, not_found, Opts),
    After = hb_maps:get(<<"chunks">>, Applied, not_found, Opts),
    Touched = lib_arweave_accounts:chunk_of(Address),
    ?assertEqual(lists:sort(maps:keys(Before)), lists:sort(maps:keys(After))),
    ?assertNotEqual(maps:get(Touched, Before), maps:get(Touched, After)),
    Untouched = lists:delete(Touched, lists:sort(maps:keys(Before))),
    ?assertNotEqual([], Untouched),
    lists:foreach(
        fun(Key) -> ?assertEqual(maps:get(Key, Before), maps:get(Key, After)) end,
        Untouched
    ),
    % The carried-over links must still describe the tree: read the state back
    % cold and confirm it hashes to the root the transition produced.
    Root = hb_maps:get(<<"root">>, Applied, not_found, Opts),
    {ok, Cold} = hb_cache:read(tree_path(Root), Opts),
    ?assertEqual(Root, resolved_root(Cold, Opts)).

%% @doc A reorg unwinds to the state the forking block was applied to, and
%% that state hashes to the root it always did.
rollback_test() ->
    Opts = opts(),
    State = ingest(fixture(), Opts),
    [{Address, {Balance, _}} | _] = fixture_accounts(),
    {ok, One} = apply_diff(State, credit(Address, Balance + 1), [], Opts),
    {ok, Two} = apply_diff(One, credit(Address, Balance + 2), [], Opts),
    {ok, Back} = hb_ao:resolve(Two, #{ <<"path">> => <<"rollback">>, <<"depth">> => 2 }, Opts),
    ?assertEqual(?FIXTURE_ROOT, resolved_root(Back, Opts)),
    {ok, Once} = hb_ao:resolve(Two, <<"rollback">>, Opts),
    ?assertEqual(
        hb_maps:get(<<"root">>, One, not_found, Opts),
        hb_maps:get(<<"root">>, Once, not_found, Opts)
    ),
    {error, TooFar} =
        hb_ao:resolve(Two, #{ <<"path">> => <<"rollback">>, <<"depth">> => 3 }, Opts),
    ?assertEqual(<<"unknown-account-tree">>,
        hb_maps:get(<<"message">>, TooFar, not_found, Opts)).

%% @doc Fetch the live mainnet wallet list at the current tip and check that it
%% hashes to the root the tip block signed. Mainnet is the only oracle the
%% account tree has, and the fixture above is one page of exactly this fetch,
%% frozen. Disabled by default because it pulls roughly 24 MB across 125
%% requests; run it by hand.
%%
%% Peers prune the account tree at depth 100, so only roots within about a
%% hundred blocks of the tip are served -- a historical root answers 404. That
%% is why the anchor must be recent.
mainnet_wallet_list_test_disabled() ->
    Opts = opts(),
    Peer = <<"http://tip-1.arweave.xyz:1984">>,
    Block = hb_json:decode(fetch(<< Peer/binary, "/block/current" >>)),
    Root = hb_maps:get(<<"wallet_list">>, Block, not_found, Opts),
    State = fetch_pages(Peer, Root, <<>>, #{ <<"device">> => ?DEVICE }, Opts),
    ?assertEqual(Root, resolved_root(State, Opts)),
    ?assertEqual({ok, true}, verify_message(State, Root, Opts)).

%% @doc Page the wallet list from a peer, feeding each body to `page/3'.
fetch_pages(Peer, Root, Cursor, State, Opts) ->
    Body = fetch(<< Peer/binary, "/wallet_list/", Root/binary, Cursor/binary >>),
    {ok, Result} =
        hb_ao:resolve(
            State,
            #{ <<"path">> => <<"page">>, <<"body">> => Body },
            Opts
        ),
    Next = hb_maps:get(<<"next-cursor">>, Result, not_found, Opts),
    Accounts = hb_maps:get(<<"accounts">>, Result, not_found, Opts),
    case Next of
        <<"last">> -> Accounts;
        _ -> fetch_pages(Peer, Root, << "/", Next/binary >>, Accounts, Opts)
    end.

%% @doc Fetch a URL's body. The wallet list is served as Erlang term format
%% unless the request asks for JSON, so the body is taken as raw bytes.
fetch(URL) ->
    {ok, {{_, 200, _}, _, Body}} =
        httpc:request(get, {hb_util:list(URL), []}, [], [{body_format, binary}]),
    Body.

%% @doc A page a peer could not have produced is rejected before it can reach
%% the tree, and each malformation is reported distinctly.
reject_malformed_page_test() ->
    Opts = opts(),
    ?assertEqual(
        <<"invalid-wallet-list-page">>,
        page_error(<<"not a term">>, Opts)
    ),
    ?assertEqual(
        <<"invalid-account">>,
        page_error(
            term_to_binary(#{ next_cursor => last, wallets => [{<<1:256>>, {-1, <<>>}}] }),
            Opts
        )
    ).

%%% Test helpers.

%% @doc Resolve `page' with a body, returning the result message.
ingest_result(Body, Opts) ->
    hb_ao:resolve(
        #{ <<"device">> => ?DEVICE },
        #{ <<"path">> => <<"page">>, <<"body">> => Body },
        Opts
    ).

%% @doc Ingest a single, complete page and return the state it leaves.
ingest(Body, Opts) ->
    ingest_pages([page_accounts(Body)], Opts).

%% @doc Ingest a list of account lists as consecutive pages, the last of which
%% exhausts the peer.
ingest_pages(Pages, Opts) ->
    lists:foldl(
        fun({Accounts, Cursor}, State) ->
            {ok, Result} =
                hb_ao:resolve(
                    State,
                    #{
                        <<"path">> => <<"page">>,
                        <<"body">> => encode_page(Cursor, Accounts)
                    },
                    Opts
                ),
            hb_maps:get(<<"accounts">>, Result, not_found, Opts)
        end,
        #{ <<"device">> => ?DEVICE },
        cursored(Pages)
    ).

%% @doc Tag each page but the last with a cursor, as a peer would.
cursored([Last]) -> [{Last, last}];
cursored([Page | Pages]) -> [{Page, crypto:strong_rand_bytes(32)} | cursored(Pages)].

%% @doc Encode an account list as a peer's page body.
encode_page(Cursor, Accounts) ->
    term_to_binary(#{ next_cursor => Cursor, wallets => Accounts }).

%% @doc The accounts carried by a page body.
page_accounts(Body) ->
    {ok, _Cursor, Accounts} = lib_arweave_accounts:decode_page(Body),
    Accounts.

%% @doc The accounts frozen in the mainnet fixture.
fixture_accounts() ->
    page_accounts(fixture()).

%% @doc A small tree of synthetic accounts, for the properties that do not
%% need mainnet data.
accounts() ->
    [ {crypto:strong_rand_bytes(32), {N * 1000, crypto:strong_rand_bytes(32)}}
        || N <- lists:seq(1, 64) ].

%% @doc Ingest the fixture with one account rewritten, and verify the result
%% against the root the untouched fixture hashes to. Returns the error's
%% `message', so that a mutant which still verifies fails the assertion.
mutated(Mutation) ->
    Opts = opts(),
    [First | Rest] = fixture_accounts(),
    Accounts =
        case Mutation(First) of
            drop -> Rest;
            Mutant -> [Mutant | Rest]
        end,
    State = ingest(encode_page(last, Accounts), Opts),
    verify_message(State, ?FIXTURE_ROOT, Opts).

%% @doc Resolve `root' and return the hash it reports. A resolved result also
%% carries the hashpath AO-Core stamps on it, so assertions compare this leaf
%% rather than the whole message.
resolved_root(State, Opts) ->
    {ok, Result} = hb_ao:resolve(State, <<"root">>, Opts),
    hb_maps:get(<<"root">>, Result, not_found, Opts).

%% @doc Verify a state against a root, reducing the result to the stable error
%% name the mutation tests assert on.
verify_message(State, Expected, Opts) ->
    case hb_ao:resolve(State,
            #{ <<"path">> => <<"verify">>, <<"expected-root">> => Expected },
            Opts) of
        {ok, Result} -> {ok, hb_maps:get(<<"valid">>, Result, not_found, Opts)};
        {error, Error} -> {error, hb_maps:get(<<"message">>, Error, not_found, Opts)}
    end.

%% @doc The `message' a malformed page is rejected with.
page_error(Body, Opts) ->
    {error, Error} = ingest_result(Body, Opts),
    hb_maps:get(<<"message">>, Error, not_found, Opts).

%% @doc A one-account diff setting a balance.
credit(Address, Balance) ->
    #{
        hb_util:encode(Address) =>
            #{ <<"balance">> => Balance, <<"last-tx">> => hb_util:encode(<<>>) }
    }.

%% @doc Resolve `apply' with a diff and an optional expected root.
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
