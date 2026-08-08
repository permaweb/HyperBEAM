%%% @doc An AO-Core interface to Arweave's account tree: the Merkle-Patricia
%%% trie whose root every block header commits to as `wallet-list'.
%%%
%%% That root is the strongest correctness property in the subsystem. It
%%% covers every account after the block was applied, so a transition that is
%%% wrong by one winston -- or that stores an account in the wrong of the two
%%% hash forms, or drops one, or keeps a stale `last-tx' -- produces a
%%% different root. A tree assembled from peer `/wallet_list' pages must hash
%%% to the value the block signed.
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
%%% The root does not depend on the order accounts were inserted in, which lets
%%% a peer page the list in any order. `apply/3' memoises the Patricia tree in
%%% the state's private section while that state remains in memory; private
%%% data never reaches the cache and cannot perturb the state's identity.
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
-ifdef(TEST).
-export([tree_path/1]).
-endif.
-include("include/hb.hrl").

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
