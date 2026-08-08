%%% @doc The bridge between Arweave's account tree -- an `ar_patricia_tree'
%%% of bare tuples, which the vendored consensus code operates on -- and the
%%% messages `~arweave-wallets@2.9' exchanges. No other module converts
%%% between the two representations.
%%%
%%% Three facts drive the whole module.
%%%
%%% First, an account is a `{Balance, LastTX}' pair exactly when its
%%% denomination is 1 and mining is permitted, and a `{Balance, LastTX,
%%% Denomination, MiningPermission}' quadruple otherwise.
%%% `ar_block:hash_wallet_list/1' hashes the two forms differently -- the pair
%%% through `ar_deep_hash', the quadruple as a length-prefixed SHA-384
%%% preimage -- so emitting the wrong arity for an otherwise identical account
%%% silently produces a wrong root. `ar_node_utils:update_account/6' draws the
%%% line at `Denomination == 1 andalso MiningPermission == true';
%%% `account/2' draws exactly the same one, which makes the mapping between
%%% the tuple and its message a bijection.
%%%
%%% Second, `ar_patricia_tree:compute_hash/2' memoises each node's hash on the
%%% tree it returns. `root/1' therefore returns the updated tree beside the
%%% hash, and callers must thread it forward: a discarded memoisation turns
%%% every subsequent root from a walk of the accounts a block touched into a
%%% walk of all of them.
%%%
%%% Third, the root does not depend on insertion order. That is what lets a
%%% peer serve `/wallet_list' pages in whatever order it likes and the root
%%% still come out equal to the one the block committed to.
-module(lib_arweave_accounts).
-export([new/0, insert/3, insert_all/2, get/2, delete/2, count/1, to_list/1]).
-export([root/1, account/2, account_message/1, diff/2, get_map/2]).
-export([decode_page/1, encode_chunk/1, decode_chunk/1, chunk_of/1, chunks/1]).
-export([apply_diff/2, reverse_diff/2, patch/2]).
-include("include/hb.hrl").

%% The account tree is stored in fixed chunks selected by the first byte of the
%% address, rather than by a running count. Boundaries chosen by count shift
%% for every chunk after an insertion, so a single new account would rewrite
%% the whole tree; boundaries chosen by prefix are stable, so a block rewrites
%% only the chunks holding the accounts it touched.
-define(CHUNK_PREFIX_BYTES, 1).

%% @doc Return an empty account tree.
new() ->
    ar_patricia_tree:new().

%% @doc Insert an account, given as a tuple, under its address.
insert(Address, Account, Tree) ->
    ar_patricia_tree:insert(Address, Account, Tree).

%% @doc Insert a list of `{Address, Account}' pairs.
insert_all(Accounts, Tree) ->
    lists:foldl(
        fun({Address, Account}, Acc) -> insert(Address, Account, Acc) end,
        Tree,
        Accounts
    ).

%% @doc Return the account stored under an address, or `not_found'.
get(Address, Tree) ->
    ar_patricia_tree:get(Address, Tree).

%% @doc Remove an address from the tree.
delete(Address, Tree) ->
    ar_patricia_tree:delete(Address, Tree).

%% @doc Return the number of accounts in the tree.
count(Tree) ->
    ar_patricia_tree:size(Tree).

%% @doc Return every `{Address, Account}' pair in ascending address order.
%% `ar_patricia_tree:foldr/3' visits keys in descending order, so consing
%% yields the ascending list the chunk encoding depends upon.
to_list(Tree) ->
    ar_patricia_tree:foldr(
        fun(Address, Account, Acc) -> [{Address, Account} | Acc] end,
        [],
        Tree
    ).

%% @doc Compute the tree's root hash, returning it base64url-encoded beside the
%% tree carrying the memoised node hashes. The empty tree hashes to the empty
%% binary rather than to a digest, so it encodes to the empty binary too.
root(Tree) ->
    {Root, Memoised, _UpdateMap} = ar_block:hash_wallet_list(Tree),
    {hb_util:encode(Root), Memoised}.

%% @doc Convert an account message into the tuple the consensus code holds,
%% collapsing to the two-element form under exactly the condition
%% `ar_node_utils:update_account/6' collapses under.
account(Message, Opts) ->
    Balance = hb_util:int(hb_maps:get(<<"balance">>, Message, 0, Opts)),
    LastTX = hb_util:decode(hb_maps:get(<<"last-tx">>, Message, <<>>, Opts)),
    Denomination = hb_util:int(hb_maps:get(<<"denomination">>, Message, 1, Opts)),
    MiningPermission =
        hb_util:atom(hb_maps:get(<<"mining-permission">>, Message, true, Opts)),
    collapse(Balance, LastTX, Denomination, MiningPermission).

%% @doc Convert an account tuple into its message. The two-element form is a
%% denomination-1, mining-permitted account, so it reports those values
%% explicitly; `account/2' collapses them back.
account_message({Balance, LastTX}) ->
    account_message({Balance, LastTX, 1, true});
account_message({Balance, LastTX, Denomination, MiningPermission}) ->
    #{
        <<"balance">> => Balance,
        <<"last-tx">> => hb_util:encode(LastTX),
        <<"denomination">> => Denomination,
        <<"mining-permission">> => MiningPermission
    }.

%% @doc Convert a diff message -- addresses to account messages, or to the
%% binary `remove' -- into the sparse map `apply_diff/2' consumes.
diff(Message, Opts) ->
    hb_maps:fold(
        fun(Address, <<"remove">>, Acc) ->
                Acc#{ hb_util:decode(Address) => remove };
            (Address, Account, Acc) ->
                Acc#{ hb_util:decode(Address) => account(Account, Opts) }
        end,
        #{},
        Message,
        Opts
    ).

%% @doc Collect the accounts stored under the given addresses into a sparse
%% map, omitting those the tree does not hold. Lifted verbatim from
%% `ar_wallets:get_map/2'; it is what builds the `Accounts' argument
%% `ar_node_utils:update_accounts/3' takes.
get_map(Tree, Addresses) ->
    lists:foldl(
        fun(Addr, Acc) ->
            case ar_patricia_tree:get(Addr, Tree) of
                not_found ->
                    Acc;
                Value ->
                    maps:put(Addr, Value, Acc)
            end
        end,
        #{},
        Addresses
    ).

%% @doc Parse one `GET /wallet_list/<root>[/<cursor>]' response body. The
%% response is Erlang term format unless the request asked for JSON, so this
%% decodes it and applies the same guards `ar_serialize:
%% etf_to_wallet_chunk_response/1' applies: a peer that sends a malformed
%% account must be rejected here rather than silently poisoning the root.
%% Every atom the body may legitimately carry appears literally in this
%% module, so the `safe' decode cannot reject a well-formed page.
decode_page(<<131, 80, _/binary>>) ->
    % The compressed external term format declares its inflated size in a header
    % that `binary_to_term/2' allocates against before it inspects anything, so
    % a few kilobytes from a peer can demand an arbitrarily large allocation.
    % `[safe]' does not cover this. A legitimate page is never compressed --
    % `ar_serialize' encodes with plain `term_to_binary/1' -- so the format is
    % refused outright rather than size-limited.
    {error, <<"invalid-wallet-list-page">>};
decode_page(Body) ->
    try binary_to_term(Body, [safe]) of
        #{ next_cursor := Cursor, wallets := Wallets } when is_list(Wallets) ->
            decode_page(Cursor, Wallets);
        _ ->
            {error, <<"invalid-wallet-list-page">>}
    catch
        error:badarg ->
            {error, <<"invalid-wallet-list-page">>}
    end.

decode_page(Cursor, Wallets) when is_binary(Cursor); Cursor == last ->
    case lists:all(fun is_valid_account/1, Wallets) of
        true -> {ok, Cursor, Wallets};
        false -> {error, <<"invalid-account">>}
    end;
decode_page(_Cursor, _Wallets) ->
    {error, <<"invalid-wallet-list-cursor">>}.

%% @doc Encode a list of `{Address, Account}' pairs for storage. This is the
%% encoding the peer itself uses for the same data -- a wallet list page is
%% the term encoding of a map whose `wallets' key holds exactly this list --
%% so a chunk is the page's own payload, and the pairs stay opaque to every
%% consumer but this module.
encode_chunk(Accounts) ->
    term_to_binary(Accounts).

%% @doc Recover the pairs written by `encode_chunk/1'. A compressed body cannot
%% have come from `encode_chunk/1' and is refused for the reason `decode_page/1'
%% gives, so that the store cannot be made to allocate on a value's header.
decode_chunk(<<131, 80, _/binary>>) ->
    error(badarg);
decode_chunk(Bin) ->
    binary_to_term(Bin, [safe]).

%% @doc Return the chunk an address belongs to, as the decimal key the state
%% message indexes its chunks by. Addresses are the SHA-256 of a public key and
%% so are normally 32 bytes, but mainnet holds a few dozen shorter ones and one
%% that is empty; they are right-padded rather than rejected, since they are
%% real accounts holding real balances and the root covers them.
chunk_of(Address) ->
    << Prefix:(?CHUNK_PREFIX_BYTES)/binary, _/binary >> =
        << Address/binary, 0:(?CHUNK_PREFIX_BYTES * 8) >>,
    hb_util:bin(binary:decode_unsigned(Prefix)).

%% @doc Group a list of `{Address, Account}' pairs by chunk, preserving the
%% order they were given in within each group, so that an ascending list yields
%% ascending chunks. Only non-empty chunks appear.
chunks(Accounts) ->
    lists:foldr(
        fun(Pair = {Address, _}, Acc) ->
            Key = chunk_of(Address),
            Acc#{ Key => [Pair | maps:get(Key, Acc, [])] }
        end,
        #{},
        Accounts
    ).

%% @doc Apply a chunk's share of a diff to the accounts that chunk holds,
%% returning them in ascending address order. An entry whose value is `remove'
%% deletes the account. This is what lets a block rewrite only the chunks it
%% touched, rather than re-chunking every account.
patch(Accounts, Entries) ->
    lists:keysort(1,
        maps:to_list(
            lists:foldl(
                fun ({Address, remove}, Acc) ->
                        maps:remove(Address, Acc);
                    ({Address, Account}, Acc) ->
                        Acc#{ Address => Account }
                end,
                maps:from_list(Accounts),
                Entries
            )
        )
    ).

%% @doc Apply a sparse account diff to a tree. Lifted verbatim from
%% `ar_wallets:apply_diff/2'.
apply_diff(Diff, Tree) ->
    maps:fold(
        fun (Addr, remove, Acc) ->
                ar_patricia_tree:delete(Addr, Acc);
            (Addr, {Balance, LastTX}, Acc) ->
                ar_patricia_tree:insert(Addr, {Balance, LastTX}, Acc);
            (Addr, {Balance, LastTX, Denomination, MiningPermission}, Acc) ->
                ar_patricia_tree:insert(Addr,
                    {Balance, LastTX, Denomination, MiningPermission}, Acc)
        end,
        Tree,
        Diff
    ).

%% @doc Build the diff that undoes `Diff' against `Tree'. Lifted verbatim from
%% `ar_wallets:reverse_diff/2'.
reverse_diff(Diff, Tree) ->
    maps:map(
        fun(Addr, _Value) ->
            case ar_patricia_tree:get(Addr, Tree) of
                not_found ->
                    remove;
                Value ->
                    Value
            end
        end,
        Diff
    ).

%%% Internal functions.

%% @doc Choose the account form. The two-element form is not an abbreviation
%% of the four-element one -- they are separate hash preimages -- so this line
%% must sit exactly where `ar_node_utils:update_account/6' draws it.
collapse(Balance, LastTX, 1, true) ->
    {Balance, LastTX};
collapse(Balance, LastTX, Denomination, MiningPermission) ->
    {Balance, LastTX, Denomination, MiningPermission}.

%% @doc The account shapes a peer may legitimately serve. Upstream's guards,
%% expressed as a predicate so that a bad account becomes an error return
%% rather than a function-clause crash.
is_valid_account({Addr, {Balance, LastTX}}) ->
    is_binary(Addr) andalso is_binary(LastTX)
        andalso is_integer(Balance) andalso Balance >= 0;
is_valid_account({Addr, {Balance, LastTX, Denomination, MiningPermission}}) ->
    is_binary(Addr) andalso is_binary(LastTX)
        andalso is_integer(Balance) andalso Balance >= 0
        andalso is_integer(Denomination) andalso Denomination > 0
        andalso is_boolean(MiningPermission);
is_valid_account(_) ->
    false.
