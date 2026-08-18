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
%%% tree it returns and reports the Patricia nodes whose hashes changed.
%%% `root/1' preserves its small public result while `root_update/1' also
%%% returns that update for durable AO-Core storage.
%%%
%%% Third, the root does not depend on insertion order. That is what lets a
%%% peer serve `/wallet_list' pages in whatever order it likes and the root
%%% still come out equal to the one the block committed to.
-module(lib_arweave_accounts).
-export([new/0, insert/3, insert_all/2]).
-export([root/1, root_update/1, account/2, account_message/1]).
-export([address/1, accounts/2, diff/2, get_map/2]).
-export([apply_diff/2]).
-export([node_children/2, node_has_value/2, node_message/4, page_message/3]).
-export([node_spec/2, node_vector/2, subtree_count/2, page_size/0]).
-export([skeleton/4, from_node/2, get_from_node/3]).
-export([load_node/3, resolve_node_link/2, authenticate_node/3]).
-include("include/hb.hrl").

-ifndef(ARWEAVE_ACCOUNT_PAGE_SIZE).
-define(ARWEAVE_ACCOUNT_PAGE_SIZE, 256).
-endif.

-define(CHILD_VECTOR_WIDTH, 32).

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

%% @doc Return the maximum account count in one compact subtree page.
page_size() ->
    ?ARWEAVE_ACCOUNT_PAGE_SIZE.

%% @doc Compute the tree's root and retain its memoised hashes.
root(Tree) ->
    {Root, Memoised, _Update} = ar_block:hash_wallet_list(Tree),
    {hb_util:encode(Root), Memoised}.

%% @doc Compute the root and return the Patricia nodes invalidated since the
%% tree was last hashed. The empty tree has no node and therefore no update.
root_update(Tree) ->
    {Root, Memoised, Update} = ar_block:hash_wallet_list(Tree),
    {hb_util:encode(Root), Memoised, Update}.

%% @doc Convert an account message into the tuple the consensus code holds,
%% collapsing to the two-element form under exactly the condition
%% `ar_node_utils:update_account/6' collapses under.
account(Message, Opts) ->
    account_assert(is_map(Message) orelse ?IS_LINK(Message)),
    Balance = account_integer(required_field(<<"balance">>, Message, Opts)),
    account_assert(Balance >= 0),
    LastTX = checked_decode(required_field(<<"last-tx">>, Message, Opts)),
    Denomination =
        account_integer(required_field(<<"denomination">>, Message, Opts)),
    account_assert(Denomination > 0),
    MiningPermission =
        account_boolean(
            required_field(<<"mining-permission">>, Message, Opts)
        ),
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

%% @doc Decode one canonical base64url account key.
address(Encoded) ->
    checked_decode(Encoded).

%% @doc Convert an AO-Core message of accounts into consensus tuples.
accounts(Message, Opts) ->
    hb_maps:fold(
        fun(Address, Account, Acc) ->
            [{address(Address), account(Account, Opts)} | Acc]
        end,
        [],
        hb_private:reset(Message),
        Opts
    ).

%% @doc Convert a diff message -- addresses to account messages, or to the
%% binary `remove' -- into the sparse map `apply_diff/2' consumes.
diff(Message, Opts) ->
    hb_maps:fold(
        fun(Address, <<"remove">>, Acc) ->
                Acc#{ address(Address) => remove };
            (Address, Account, Acc) ->
                Acc#{ address(Address) => account(Account, Opts) }
        end,
        #{},
        hb_private:reset(Message),
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

%% @doc Return the persistent children of a Patricia node in native order.
node_children({_Hash, Prefix}, Tree) ->
    {_Parent, Children, _NodeHash, _Suffix, _MaybeValue} =
        maps:get(Prefix, Tree),
    [
        {element(3, maps:get(ChildPrefix, Tree)), ChildPrefix}
    || ChildPrefix <- gb_sets:to_list(Children)
    ].

%% @doc Return whether a Patricia node owns an account as well as children.
node_has_value({_Hash, Prefix}, Tree) ->
    {_Parent, _Children, _NodeHash, _Suffix, MaybeValue} =
        maps:get(Prefix, Tree),
    MaybeValue =/= no_value.

%% @doc Project one memoised Patricia node into an immutable AO-Core message.
%% Native summaries and AO child links are committed in separate fields.
node_message({Hash, Prefix}, Tree, ChildSpecs, ChildrenLink) ->
    {_Parent, Children, Hash, _Suffix, MaybeValue} = maps:get(Prefix, Tree),
    false = gb_sets:is_empty(Children),
    Count =
        terminal_count(MaybeValue) +
            lists:sum([ChildCount || {_Ref, {_Link, ChildCount}} <- ChildSpecs]),
    Node =
        #{
            <<"kind">> => <<"branch">>,
            <<"root">> => hb_util:encode(Hash),
            <<"count">> => Count,
            <<"body">> => encode_children(ChildSpecs),
            <<"children">> => ChildrenLink
        },
    case MaybeValue of
        no_value -> Node;
        {v, Account} ->
            Node#{ <<"account">> => account_message(Account) }
    end.

%% @doc Encode one bounded Patricia subtree as a canonical page.
page_message({Hash, Prefix} = Ref, Tree, Opts) ->
    Accounts = lists:keysort(1, subtree_accounts(Ref, Tree, Opts)),
    Count = length(Accounts),
    true = Count > 0 andalso Count =< ?ARWEAVE_ACCOUNT_PAGE_SIZE,
    #{
        <<"kind">> => <<"page">>,
        <<"root">> => hb_util:encode(Hash),
        <<"count">> => Count,
        <<"body">> => encode_accounts(Accounts)
    }.

%% @doc Encode a Patricia map prefix as a safe AO-Core key.
node_key(root) -> <<"root">>;
node_key(Prefix) when is_binary(Prefix) ->
    <<"binary-", (hb_util:encode(Prefix))/binary>>.

%% @doc Return the AO link retained for an unchanged native node.
node_spec(Ref, Tree) ->
    maps:find(Ref, maps:get(account_node_specs, Tree, #{})).

%% @doc Return a hydrated branch's authenticated persistent child vector.
node_vector(Prefix, Tree) ->
    maps:find(Prefix, maps:get(account_branch_vectors, Tree, #{})).

%% @doc Count one native subtree, reusing authenticated persistent summaries.
subtree_count(Ref = {_Hash, Prefix}, Tree) ->
    case node_spec(Ref, Tree) of
        {ok, {_LinkSpec, Count}} -> Count;
        error ->
            {_Parent, Children, _NodeHash, _Suffix, MaybeValue} =
                maps:get(Prefix, Tree),
            terminal_count(MaybeValue) +
                lists:sum(
                    [
                        subtree_count(
                            {element(3, maps:get(ChildPrefix, Tree)), ChildPrefix},
                            Tree
                        )
                    || ChildPrefix <- gb_sets:to_list(Children)
                    ]
                )
    end.

%% @doc Hydrate the pages selected by a sparse transition. Persistent references
%% restore untouched subtrees as hash/count stubs, so the vendored insert,
%% delete and hash code runs unchanged without reading their accounts.
skeleton(RootLink, RootNode, Addresses, Opts) ->
    ok = authenticate_node(RootNode, Opts),
    RootRef = {node_hash(RootNode, Opts), root},
    hydrate_node(
        RootNode,
        root,
        no_parent,
        Addresses,
        remember_node(
            RootRef,
            RootLink,
            node_count(RootNode, Opts),
            #{ size => node_count(RootNode, Opts) }
        ),
        Opts
    ).

%% @doc Rebuild a complete vendored Patricia tree from the persistent graph.
from_node(RootNode, Opts) ->
    ok = authenticate_node(RootNode, Opts),
    {Accounts, Nodes} = accounts_from_node(RootNode, root, #{}, Opts),
    {Root, Tree, Update} = root_update(insert_all(Accounts, new())),
    ok = tree_equal(Root, hb_maps:get(<<"root">>, RootNode, not_found, Opts)),
    ok = tree_assert(map_size(Nodes) =< map_size(Update)),
    ok = validate_nodes(Nodes, Tree, Update, Opts),
    AccountCount = length(Accounts),
    ok = tree_equal(AccountCount, node_count(RootNode, Opts)),
    Tree.

%% @doc Read an account by walking only its persistent Patricia path.
get_from_node(RootNode, Address, Opts) ->
    ok = authenticate_node(RootNode, Opts),
    get_from_node2(RootNode, root, Address, Opts).

%% @doc Load a canonical AO content link and reject inline topology maps.
load_node(Key, {link, ID, LinkOpts}, Opts) when is_map(LinkOpts) ->
    case {
        maps:get(<<"lazy">>, LinkOpts, false),
        maps:get(<<"type">>, LinkOpts, not_found),
        ?IS_ID(ID)
    } of
        {true, <<"link">>, _} ->
            case hb_cache:read(ID, Opts) of
                {ok, TargetID} when ?IS_ID(TargetID) ->
                    loaded_node(Key, TargetID, Opts);
                {ok, _} -> throw({'invalid-account-node-link', Key});
                {error, not_found} -> throw({'invalid-account-node-link', Key});
                Error -> error({'account-node-link-read-failed', Error})
            end;
        {false, <<"link">>, true} -> loaded_node(Key, ID, Opts);
        _ -> throw({'invalid-account-node-link', Key})
    end;
load_node(Key, _Link, _Opts) ->
    throw({'invalid-account-node-link', Key}).

%% @doc Authenticate a root node from its account or declared child roots.
authenticate_node(Node, Opts) ->
    authenticate_node(Node, root, Opts).

%% @doc Authenticate a node at the Patricia prefix carried by its edge.
authenticate_node(Node, Prefix, Opts) ->
    Hash = node_hash(Node, Opts),
    ok = tree_assert(byte_size(Hash) == 48),
    case hb_maps:get(<<"kind">>, Node, not_found, Opts) of
        <<"page">> ->
            ok = validate_node_keys(Node, Opts),
            Accounts = page_accounts(Node, Prefix, Opts),
            {Root, _Tree} = root(insert_all(Accounts, new())),
            ok = tree_equal(Hash, hb_util:decode(Root));
        <<"branch">> ->
            ok = validate_node_keys(Node, Opts),
            Children = child_metadata(Node, Opts),
            ok = validate_link(
                maps:get(
                    <<"children">>,
                    hb_message:uncommitted(Node),
                    not_found
                )
            ),
            ChildPrefixes =
                [
                    Prefix2
                || {_Key, Prefix2, _Root, _Count, _Ordinal} <- Children
                ],
            NodeKey = validate_child_prefixes(Prefix, ChildPrefixes),
            MaybeValue = branch_value(Node, Opts),
            ChildRoots =
                [Root || {_Key, _Prefix, Root, _Count, _Ordinal} <- Children],
            ok = tree_assert(ChildRoots =/= []),
            ChildCount =
                lists:sum(
                    [Count || {_Key, _Prefix, _Root, Count, _Ordinal} <- Children]
                ),
            ok = tree_equal(
                node_count(Node, Opts),
                terminal_count(MaybeValue) + ChildCount
            ),
            ok = tree_assert(node_count(Node, Opts) > page_size()),
            tree_equal(Hash, branch_hash(NodeKey, MaybeValue, ChildRoots));
        _ ->
            throw('invalid-account-tree')
    end.

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

%%% Internal functions.

%% @doc Read a required field from an account message.
required_field(Key, Message, Opts) ->
    case hb_maps:find(Key, Message, Opts) of
        {ok, Value} -> Value;
        error -> throw('invalid-account')
    end.

%% @doc Retain a validated AO link beside the vendored tree.
remember_node(Ref, Link, Count, Tree) ->
    Specs = maps:get(account_node_specs, Tree, #{}),
    Tree#{
        account_node_specs =>
            Specs#{ Ref => {Link, Count} }
    }.

%% @doc Retain one branch vector only while applying an in-flight transition.
remember_vector(Prefix, Node, Children, Tree) ->
    Vectors = maps:get(account_branch_vectors, Tree, #{}),
    Tree#{
        account_branch_vectors =>
            Vectors#{
                Prefix =>
                    {
                        maps:get(<<"children">>, hb_message:uncommitted(Node)),
                        Children
                    }
            }
    }.

%% @doc Require the one canonical field set for each account graph node.
validate_node_keys(Node, Opts) ->
    Public = hb_private:reset(hb_message:uncommitted(Node)),
    Expected =
        case hb_maps:get(<<"kind">>, Node, not_found, Opts) of
            <<"page">> -> [<<"body">>, <<"count">>, <<"kind">>, <<"root">>];
            <<"branch">> ->
                [<<"body">>, <<"children">>, <<"count">>, <<"kind">>, <<"root">>] ++
                    case maps:is_key(<<"account">>, Public) of
                        true -> [<<"account">>];
                        false -> []
                    end;
            _ ->
                throw('invalid-account-tree')
        end,
    Keys = lists:sort(maps:keys(Public)),
    tree_equal(lists:sort(Expected), Keys).

%% @doc Require an ordinary AO link rather than an inline or opaque reference.
validate_link({link, ID, LinkOpts}) when is_map(LinkOpts) ->
    ok = tree_equal(<<"link">>, maps:get(<<"type">>, LinkOpts, not_found)),
    case maps:get(<<"lazy">>, LinkOpts, false) of
        false -> tree_assert(?IS_ID(ID));
        true -> ok
    end;
validate_link(_Link) ->
    throw('invalid-account-tree').

%% @doc Coerce an AO scalar to an account integer or reject it.
account_integer(Value) ->
    case hb_util:safe_int(Value) of
        {ok, Integer} -> Integer;
        {error, invalid} -> throw('invalid-account')
    end.

%% @doc Coerce an AO scalar to an account boolean or reject it.
account_boolean(true) -> true;
account_boolean(false) -> false;
account_boolean(<<"true">>) -> true;
account_boolean(<<"false">>) -> false;
account_boolean(_Value) -> throw('invalid-account').

%% @doc Require an account invariant at the public conversion boundary.
account_assert(true) -> ok;
account_assert(false) -> throw('invalid-account').

%% @doc Convert malformed base64url inside a graph to a tree error.
tree_decode(Encoded) ->
    try checked_decode(Encoded)
    catch
        throw:'invalid-account' -> throw('invalid-account-tree')
    end.

%% @doc Require one structural account-tree invariant.
tree_assert(true) -> ok;
tree_assert(false) -> throw('invalid-account-tree').

%% @doc Require exact equality inside the account graph.
tree_equal(Value, Value) -> ok;
tree_equal(_Expected, _Actual) -> throw('invalid-account-tree').

%% @doc Return a Patricia node required by the authenticated graph.
tree_node(Prefix, Tree) ->
    case maps:find(Prefix, Tree) of
        {ok, Node = {_Parent, _Children, _Hash, _Suffix, _Value}} -> Node;
        _ -> throw('invalid-account-tree')
    end.

%% @doc Decode canonical base64url, rejecting unchecked-decoder aliases.
checked_decode(Encoded) when is_binary(Encoded) ->
    try
        true = is_base64url(Encoded),
        Decoded = hb_util:decode(Encoded),
        Encoded = hb_util:encode(Decoded),
        Decoded
    catch
        _:_ -> throw('invalid-account')
    end;
checked_decode(_Encoded) ->
    throw('invalid-account').

%% @doc True when a binary is structurally valid unpadded base64url.
is_base64url(Encoded) ->
    byte_size(Encoded) rem 4 =/= 1 andalso base64url_chars(Encoded).

base64url_chars(<<>>) -> true;
base64url_chars(<<Char, Rest/binary>>)
        when Char >= $A, Char =< $Z;
             Char >= $a, Char =< $z;
             Char >= $0, Char =< $9;
             Char == $-;
             Char == $_ ->
    base64url_chars(Rest);
base64url_chars(_Encoded) ->
    false.

%% @doc Collect one native subtree, loading only bounded persisted children.
subtree_accounts(Ref = {_Hash, Prefix}, Tree, Opts) ->
    case node_spec(Ref, Tree) of
        {ok, {LinkSpec, _Count}} ->
            Link = resolve_node_link(LinkSpec, Opts),
            Node = load_node(node_key(Prefix), Link, Opts),
            {Accounts, _Nodes} = accounts_from_node(Node, Prefix, #{}, Opts),
            Accounts;
        error ->
            {_Parent, Children, _NodeHash, Suffix, MaybeValue} =
                maps:get(Prefix, Tree),
            Own =
                case MaybeValue of
                    no_value -> [];
                    {v, Account} -> [{full_key(Prefix, Suffix), Account}]
                end,
            lists:foldl(
                fun(ChildPrefix, Acc) ->
                    ChildHash = element(3, maps:get(ChildPrefix, Tree)),
                    subtree_accounts(
                        {ChildHash, ChildPrefix},
                        Tree,
                        Opts
                    ) ++ Acc
                end,
                Own,
                gb_sets:to_list(Children)
            )
    end.

%% @doc Encode ordered native child summaries. Dependencies remain AO links.
encode_children(ChildSpecs) ->
    iolist_to_binary(
        [
            encode_child(
                ChildPrefix,
                ChildHash,
                ChildCount
            )
        || {{ChildHash, ChildPrefix}, {_ChildLink, ChildCount}} <- ChildSpecs
        ]
    ).

encode_child(Prefix, Root, Count)
        when is_binary(Prefix), byte_size(Root) == 48, Count > 0 ->
    <<
        (byte_size(Prefix)):16,
        Prefix/binary,
        Root/binary,
        Count:64/unsigned-big
    >>.

%% @doc Decode the one canonical native-summary representation.
decode_children(Body) when is_binary(Body) ->
    Children = decode_children(Body, []),
    ok = tree_equal(Body, encode_decoded_children(Children)),
    ok = tree_assert(strict_child_prefixes(Children)),
    Children;
decode_children(_Body) ->
    throw('invalid-account-tree').

decode_children(<<>>, Children) ->
    lists:reverse(Children);
decode_children(
    <<
        PrefixSize:16,
        Prefix:PrefixSize/binary,
        Root:48/binary,
        Count:64/unsigned-big,
        Rest/binary
    >>,
    Children
) when Count > 0 ->
    decode_children(
        Rest,
        [
            {
                node_key(Prefix),
                Prefix,
                Root,
                Count,
                length(Children) + 1
            }
        | Children
        ]
    );
decode_children(_Body, _Children) ->
    throw('invalid-account-tree').

encode_decoded_children(Children) ->
    iolist_to_binary(
        [
            encode_child(Prefix, Root, Count)
        || {_Key, Prefix, Root, Count, _Ordinal} <- Children
        ]
    ).

strict_child_prefixes([]) -> true;
strict_child_prefixes([{_Key, Prefix, _Root, _Count, _Ordinal} | Children]) ->
    strict_child_prefixes(Prefix, Children).

strict_child_prefixes(_Previous, []) -> true;
strict_child_prefixes(
    Previous,
    [{_Key, Prefix, _Root, _Count, _Ordinal} | Children]
) ->
    Previous < Prefix andalso strict_child_prefixes(Prefix, Children).

%% @doc Encode sorted accounts in one canonical bounded subtree body.
encode_accounts(Accounts) ->
    iolist_to_binary([encode_account_entry(Entry) || Entry <- Accounts]).

encode_account_entry({Address, Account}) ->
    {Balance, LastTX, Denomination, MiningPermission} = expand(Account),
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

%% @doc Decode and prove the unique binary representation of a subtree page.
decode_accounts(Body, Count) when is_binary(Body), is_integer(Count), Count > 0 ->
    decode_accounts(Body, Count, []);
decode_accounts(_Body, _Count) ->
    throw('invalid-account-tree').

decode_accounts(<<>>, 0, Accounts) ->
    lists:reverse(Accounts);
decode_accounts(
    <<
        AddressSize:32,
        Address:AddressSize/binary,
        BalanceSize:16,
        BalanceBin:BalanceSize/binary,
        LastTXSize:32,
        LastTX:LastTXSize/binary,
        DenominationSize:16,
        DenominationBin:DenominationSize/binary,
        Permission:8,
        Rest/binary
    >>,
    Count,
    Accounts
) when Count > 0, BalanceSize > 0, DenominationSize > 0, Permission =< 1 ->
    Balance = binary:decode_unsigned(BalanceBin),
    Denomination = binary:decode_unsigned(DenominationBin),
    ok = tree_assert(Denomination > 0),
    Account = collapse(Balance, LastTX, Denomination, Permission == 1),
    decode_accounts(Rest, Count - 1, [{Address, Account} | Accounts]);
decode_accounts(_Body, _Count, _Accounts) ->
    throw('invalid-account-tree').

%% @doc Decode and authenticate a bounded page at its graph position.
page_accounts(Node, Prefix, Opts) ->
    Count = node_count(Node, Opts),
    ok = tree_assert(Count =< page_size()),
    Body = hb_maps:get(<<"body">>, Node, not_found, Opts),
    Accounts = decode_accounts(Body, Count),
    ok = tree_equal(Body, encode_accounts(Accounts)),
    ok = tree_equal(Accounts, lists:keysort(1, Accounts)),
    ok = tree_assert(strict_addresses(Accounts)),
    ok = tree_assert(
        lists:all(
            fun({Address, _Account}) ->
                Prefix == root orelse is_prefix(Prefix, Address)
            end,
            Accounts
        )
    ),
    Accounts.

%% @doc Require strictly increasing account addresses in a page.
strict_addresses([]) -> true;
strict_addresses([{Address, _} | Rest]) ->
    strict_addresses(Address, Rest).

strict_addresses(_Previous, []) -> true;
strict_addresses(Previous, [{Address, _} | Rest]) ->
    Previous < Address andalso strict_addresses(Address, Rest).

%% @doc Restore a persistent node into the vendored map representation.
hydrate_node(Node, Prefix, Parent, Addresses, Tree, Opts) ->
    Hash = tree_decode(hb_maps:get(<<"root">>, Node, not_found, Opts)),
    case hb_maps:get(<<"kind">>, Node, not_found, Opts) of
        <<"page">> ->
            hydrate_page(Node, Prefix, Parent, Tree, Opts);
        <<"branch">> ->
            Children = child_metadata(Node, Opts),
            VectorLink =
                maps:get(<<"children">>, hb_message:uncommitted(Node)),
            Size = length(Children),
            Prefixes = [ChildPrefix ||
                {_Key, ChildPrefix, _ChildRoot, _Count, _Ordinal} <- Children],
            NodeKey = validate_child_prefixes(Prefix, Prefixes),
            Suffix = suffix(Prefix, NodeKey),
            MaybeValue = branch_value(Node, Opts),
            Tree2 =
                remember_vector(
                    Prefix,
                    Node,
                    Children,
                    Tree#{
                        Prefix =>
                            {
                                Parent,
                                gb_sets:from_list(Prefixes),
                                Hash,
                                Suffix,
                                MaybeValue
                            }
                    }
                ),
            lists:foldl(
                fun({Key, ChildPrefix, ChildHash, ChildCount, ChildLink}, Acc) ->
                    ChildRef = {ChildHash, ChildPrefix},
                    Wanted =
                        [
                            Address
                        || Address <- Addresses,
                           is_prefix(ChildPrefix, Address)
                        ],
                    case Wanted of
                        [] ->
                            LinkedAcc =
                                remember_node(
                                    ChildRef,
                                    {vector, VectorLink, ChildLink, Size},
                                    ChildCount,
                                    Acc
                                ),
                            LinkedAcc#{
                                ChildPrefix =>
                                    {
                                        Prefix,
                                        gb_sets:new(),
                                        ChildHash,
                                        no_prefix,
                                        no_value
                                    }
                            };
                        _ ->
                            ResolvedLink =
                                child_link(
                                    Node,
                                    ChildLink,
                                    Size,
                                    Opts
                                ),
                            LinkedAcc =
                                remember_node(
                                    ChildRef,
                                    ResolvedLink,
                                    ChildCount,
                                    Acc
                                ),
                            Child =
                                validated_child(
                                    Key,
                                    ChildPrefix,
                                    ChildHash,
                                    ChildCount,
                                    ResolvedLink,
                                    Opts
                                ),
                            hydrate_node(
                                Child,
                                ChildPrefix,
                                Prefix,
                                Wanted,
                                LinkedAcc,
                                Opts
                            )
                    end
                end,
                Tree2,
                Children
            )
    end.

%% @doc Restore a compact complete subtree into a sparse vendored skeleton.
hydrate_page(Node, Prefix, Parent, Tree, Opts) ->
    Accounts = page_accounts(Node, Prefix, Opts),
    {Root, PageTree, _Update} = root_update(insert_all(Accounts, new())),
    ok = tree_equal(
        Root,
        hb_maps:get(<<"root">>, Node, not_found, Opts)
    ),
    transplant_page(Prefix, Parent, PageTree, Tree).

%% @doc Graft a standalone page tree at its native graph prefix.
transplant_page(root, _Parent, PageTree, Tree) ->
    merge_native_nodes(maps:remove(size, PageTree), Tree);
transplant_page(Prefix, Parent, PageTree, Tree) ->
    {_RootParent, RootChildren, _RootHash, _RootSuffix, no_value} =
        tree_node(root, PageTree),
    [SourcePrefix] = gb_sets:to_list(RootChildren),
    {_SourceParent, Children, Hash, SourceSuffix, MaybeValue} =
        tree_node(SourcePrefix, PageTree),
    NodeKey = full_key(SourcePrefix, SourceSuffix),
    TargetSuffix = suffix(Prefix, NodeKey),
    WithoutRoot = maps:remove(root, maps:remove(size, PageTree)),
    WithoutSource = maps:remove(SourcePrefix, WithoutRoot),
    WithTarget =
        WithoutSource#{
            Prefix => {Parent, Children, Hash, TargetSuffix, MaybeValue}
        },
    NativeNodes =
        gb_sets:fold(
            fun(ChildPrefix, Acc) ->
                {_OldParent, ChildChildren, ChildHash, ChildSuffix, ChildValue} =
                    tree_node(ChildPrefix, Acc),
                Acc#{
                    ChildPrefix =>
                        {
                            Prefix,
                            ChildChildren,
                            ChildHash,
                            ChildSuffix,
                            ChildValue
                        }
                }
            end,
            WithTarget,
            Children
        ),
    merge_native_nodes(NativeNodes, Tree).

%% @doc Merge vendored nodes without replacing skeleton metadata.
merge_native_nodes(NativeNodes, Tree) ->
    maps:merge(
        Tree,
        maps:without(
            [size, account_node_specs, account_branch_vectors],
            NativeNodes
        )
    ).

%% @doc Collect every account and node represented by a persistent graph.
accounts_from_node(Node, Prefix, Nodes, Opts) ->
    ok = tree_assert(not maps:is_key(Prefix, Nodes)),
    Nodes2 = Nodes#{ Prefix => Node },
    case hb_maps:get(<<"kind">>, Node, not_found, Opts) of
        <<"page">> ->
            {page_accounts(Node, Prefix, Opts), Nodes2};
        <<"branch">> ->
            Own = branch_accounts(Node, Prefix, Opts),
            lists:foldl(
                fun(
                    {Key, ChildPrefix, ChildRoot, ChildCount, ChildLink},
                    {AccountsAcc, NodesAcc}
                ) ->
                    {ChildAccounts, ChildNodes} =
                        accounts_from_node(
                            validated_child(
                                Key,
                                ChildPrefix,
                                ChildRoot,
                                ChildCount,
                                ChildLink,
                                Opts
                            ),
                            ChildPrefix,
                            NodesAcc,
                            Opts
                        ),
                    {ChildAccounts ++ AccountsAcc, ChildNodes}
                end,
                {Own, Nodes2},
                child_specs(Node, Opts)
            )
    end.

%% @doc Walk the one child whose Patricia prefix matches an address.
get_from_node2(Node, Prefix, Address, Opts) ->
    case hb_maps:get(<<"kind">>, Node, not_found, Opts) of
        <<"page">> ->
            case lists:keyfind(
                    Address,
                    1,
                    page_accounts(Node, Prefix, Opts)) of
                {Address, Account} -> Account;
                false -> not_found
            end;
        <<"branch">> ->
            Children = child_metadata(Node, Opts),
            NodeKey = validate_child_prefixes(
                Prefix,
                [ChildPrefix ||
                    {_Key, ChildPrefix, _Root, _Count, _Ordinal} <- Children]
            ),
            case {Address == NodeKey, branch_value(Node, Opts)} of
                {true, {v, Account}} -> Account;
                _ ->
                    case child_spec(Children, Address) of
                        not_found ->
                            not_found;
                        {Key, ChildPrefix, ChildRoot, ChildCount, Ordinal} ->
                            ChildLink = child_link(
                                Node,
                                Ordinal,
                                length(Children),
                                Opts
                            ),
                            get_from_node2(
                                validated_child(
                                    Key,
                                    ChildPrefix,
                                    ChildRoot,
                                    ChildCount,
                                    ChildLink,
                                    Opts
                                ),
                                ChildPrefix,
                                Address,
                                Opts
                            )
                    end
            end
    end.

%% @doc Select the only child prefix that can contain an address.
child_spec([], _Address) ->
    not_found;
child_spec([Spec = {_Key, Prefix, _Root, _Count, _Ordinal} | Children], Address) ->
    case Prefix == Address orelse
            (Prefix =/= <<>> andalso is_prefix(Prefix, Address)) of
        true -> Spec;
        false -> child_spec(Children, Address)
    end.

%% @doc Load a child and prove its ordered root and prefix context.
validated_child(Key, Prefix, ExpectedRoot, ExpectedCount, ChildLink, Opts) ->
    Child = load_node(Key, ChildLink, Opts),
    ok = tree_equal(ExpectedRoot, node_hash(Child, Opts)),
    ok = tree_equal(ExpectedCount, node_count(Child, Opts)),
    ok = authenticate_node(Child, Prefix, Opts),
    Child.

%% @doc Load a content identifier and cross-check the node's AO identity.
loaded_node(Key, ID, Opts) ->
    case hb_cache:read(ID, Opts) of
        {ok, Node} when is_map(Node) ->
            ok = tree_equal(
                ID,
                hb_message:id(
                    Node,
                    none,
                    Opts#{ <<"linkify-mode">> => discard }
                )
            ),
            Node;
        {ok, _} -> throw({'invalid-account-node-link', Key});
        {error, not_found} -> throw({'invalid-account-node-link', Key});
        Error -> error({'account-node-read-failed', Error})
    end.

%% @doc Align canonical child links with the branch's ordered native roots.
child_specs(Node, Opts) ->
    Metadata = child_metadata(Node, Opts),
    Links = child_links(Node, length(Metadata), Opts),
    [
        {Key, Prefix, Root, Count, Link}
    || {{Key, Prefix, Root, Count, _Ordinal}, Link} <-
        lists:zip(Metadata, Links)
    ].

%% @doc Decode the native summaries without loading any child dependency.
child_metadata(Node, Opts) ->
    decode_children(hb_maps:get(<<"body">>, Node, not_found, Opts)).

%% @doc Load and validate a complete bounded child vector.
child_links(Node, Size, Opts) ->
    Link = maps:get(<<"children">>, hb_message:uncommitted(Node), not_found),
    ok = validate_link(Link),
    Vector = load_node(<<"children">>, Link, Opts),
    vector_links(Vector, Size, Opts).

vector_links(Vector, Size, Opts) when Size =< ?CHILD_VECTOR_WIDTH ->
    ok = validate_vector(Vector, <<"children-leaf">>, Size, Opts),
    vector_values(Vector, Size);
vector_links(Vector, Size, Opts) ->
    Leaves = ceil_div(Size, ?CHILD_VECTOR_WIDTH),
    ok = validate_vector(Vector, <<"children-index">>, Leaves, Opts),
    lists:append(
        [
            begin
                LeafSize = erlang:min(
                    ?CHILD_VECTOR_WIDTH,
                    Size - ((N - 1) * ?CHILD_VECTOR_WIDTH)
                ),
                LeafLink = maps:get(vector_key(N), hb_message:uncommitted(Vector)),
                ok = validate_link(LeafLink),
                Leaf = load_node(vector_key(N), LeafLink, Opts),
                ok = validate_vector(Leaf, <<"children-leaf">>, LeafSize, Opts),
                vector_values(Leaf, LeafSize)
            end
        || N <- lists:seq(1, Leaves)
        ]
    ).

%% @doc Resolve one ordinal without descending through unrelated vector leaves.
child_link(Node, Ordinal, Size, Opts) ->
    Link = maps:get(<<"children">>, hb_message:uncommitted(Node), not_found),
    ok = validate_link(Link),
    vector_child_link(Link, Ordinal, Size, Opts).

%% @doc Resolve a retained direct or vector-addressed child link.
resolve_node_link({vector, Link, Ordinal, Size}, Opts) ->
    vector_child_link(Link, Ordinal, Size, Opts);
resolve_node_link(Link, _Opts) ->
    ok = validate_link(Link),
    Link.

vector_child_link(Link, Ordinal, Size, Opts) ->
    Vector = load_node(<<"children">>, Link, Opts),
    case Size =< ?CHILD_VECTOR_WIDTH of
        true ->
            ok = validate_vector(Vector, <<"children-leaf">>, Size, Opts),
            maps:get(vector_key(Ordinal), hb_message:uncommitted(Vector));
        false ->
            Leaves = ceil_div(Size, ?CHILD_VECTOR_WIDTH),
            ok = validate_vector(Vector, <<"children-index">>, Leaves, Opts),
            LeafOrdinal = ((Ordinal - 1) div ?CHILD_VECTOR_WIDTH) + 1,
            ValueOrdinal = ((Ordinal - 1) rem ?CHILD_VECTOR_WIDTH) + 1,
            LeafLink = maps:get(
                vector_key(LeafOrdinal),
                hb_message:uncommitted(Vector)
            ),
            ok = validate_link(LeafLink),
            Leaf = load_node(vector_key(LeafOrdinal), LeafLink, Opts),
            LeafSize = erlang:min(
                ?CHILD_VECTOR_WIDTH,
                Size - ((LeafOrdinal - 1) * ?CHILD_VECTOR_WIDTH)
            ),
            ok = validate_vector(Leaf, <<"children-leaf">>, LeafSize, Opts),
            maps:get(vector_key(ValueOrdinal), hb_message:uncommitted(Leaf))
    end.

validate_vector(Vector, Kind, Size, Opts) ->
    Public = hb_private:reset(hb_message:uncommitted(Vector)),
    Expected =
        lists:sort(
            [<<"kind">>, <<"size">>] ++
                [vector_key(N) || N <- lists:seq(1, Size)]
        ),
    ok = tree_equal(Expected, lists:sort(maps:keys(Public))),
    ok = tree_equal(Kind, hb_maps:get(<<"kind">>, Vector, not_found, Opts)),
    ok = tree_equal(Size, hb_maps:get(<<"size">>, Vector, not_found, Opts)),
    lists:foreach(
        fun(N) -> ok = validate_link(maps:get(vector_key(N), Public)) end,
        lists:seq(1, Size)
    ).

vector_values(Vector, Size) ->
    Public = hb_message:uncommitted(Vector),
    [maps:get(vector_key(N), Public) || N <- lists:seq(1, Size)].

vector_key(N) -> integer_to_binary(N).

ceil_div(Numerator, Denominator) ->
    (Numerator + Denominator - 1) div Denominator.

%% @doc Hash a branch from its optional account and ordered child roots.
branch_hash(_NodeKey, no_value, [Single]) -> Single;
branch_hash(_NodeKey, no_value, ChildRoots) -> ar_deep_hash:hash(ChildRoots);
branch_hash(NodeKey, {v, Account}, ChildRoots) ->
    ar_deep_hash:hash([account_hash(NodeKey, Account) | ChildRoots]).

%% @doc Compute the vendored leaf commitment for one account.
account_hash(Address, Account) ->
    {Root, _Tree} = root(insert(Address, Account, new())),
    hb_util:decode(Root).

%% @doc Decode the optional terminal account carried by a branch.
branch_value(Node, Opts) ->
    case maps:is_key(<<"account">>, hb_private:reset(hb_message:uncommitted(Node))) of
        false -> no_value;
        true -> {v, account(hb_maps:get(<<"account">>, Node, not_found, Opts), Opts)}
    end.

%% @doc Return the optional branch account as an address/account pair.
branch_accounts(Node, Prefix, Opts) ->
    Children = child_metadata(Node, Opts),
    NodeKey = validate_child_prefixes(
        Prefix,
        [ChildPrefix ||
            {_Key, ChildPrefix, _Root, _Count, _Ordinal} <- Children]
    ),
    case branch_value(Node, Opts) of
        no_value -> [];
        {v, Account} -> [{NodeKey, Account}]
    end.

%% @doc Prove child prefixes form the next Patricia level below a branch.
validate_child_prefixes(root, Prefixes) ->
    ok = tree_assert(
        lists:all(
            fun(Prefix) ->
                is_binary(Prefix) andalso
                    (Prefix == <<>> orelse byte_size(Prefix) == 1)
            end,
            Prefixes
        )
    ),
    ok = tree_assert(length([Prefix || Prefix <- Prefixes, Prefix == <<>>]) =< 1),
    <<>>;
validate_child_prefixes(Prefix, [First | _] = Prefixes)
        when is_binary(Prefix) ->
    ok = tree_assert(is_binary(First)),
    ok = tree_assert(byte_size(First) > byte_size(Prefix)),
    ParentSize = byte_size(First) - 1,
    ParentKey = binary:part(First, 0, ParentSize),
    ok = tree_assert(is_prefix(Prefix, ParentKey)),
    ok = tree_assert(
        lists:all(
            fun(Child) ->
                is_binary(Child) andalso
                    byte_size(Child) == ParentSize + 1 andalso
                    is_prefix(ParentKey, Child)
            end,
            Prefixes
        )
    ),
    ParentKey;
validate_child_prefixes(_Prefix, _Prefixes) ->
    throw('invalid-account-tree').

%% @doc Decode a node's declared native hash.
node_hash(Node, Opts) ->
    tree_decode(hb_maps:get(<<"root">>, Node, not_found, Opts)).

%% @doc Decode the positive account count authenticated by a graph node.
node_count(Node, Opts) ->
    case hb_maps:get(<<"count">>, Node, not_found, Opts) of
        Count when is_integer(Count), Count > 0,
                Count =< 16#ffffffffffffffff ->
            Count;
        _ -> throw('invalid-account-tree')
    end.

%% @doc Prove that every graph node is the canonical node in the rebuilt tree.
validate_nodes(Nodes, Tree, Update, Opts) ->
    maps:foreach(
        fun(Prefix, Node) ->
            {_Parent, Children, Hash, Suffix, MaybeValue} =
                tree_node(Prefix, Tree),
            ok = tree_assert(maps:is_key({Hash, Prefix}, Update)),
            ok = tree_equal(Hash, node_hash(Node, Opts)),
            ok =
                validate_node_value(
                    Prefix,
                    Children,
                    Suffix,
                    MaybeValue,
                    Node,
                    Tree,
                    Opts
                )
        end,
        Nodes
    ).

%% @doc Prove a node's contents against the canonical rebuilt tree.
validate_node_value(Prefix, Children, Suffix, MaybeValue, Node, Tree, Opts) ->
    case hb_maps:get(<<"kind">>, Node, not_found, Opts) of
        <<"page">> ->
            ok = tree_equal(node_hash(Node, Opts), element(3, tree_node(Prefix, Tree))),
            ok = tree_equal(node_count(Node, Opts), length(page_accounts(Node, Prefix, Opts)));
        <<"branch">> ->
            ok = tree_assert(not gb_sets:is_empty(Children)),
            NodeKey = validate_child_prefixes(
                Prefix,
                [ChildPrefix ||
                    {_Key, ChildPrefix, _Root, _Count, _Ordinal} <-
                        child_metadata(Node, Opts)]
            ),
            ok = tree_equal(full_key(Prefix, Suffix), NodeKey),
            ok = tree_equal(MaybeValue, branch_value(Node, Opts)),
            ExpectedRefs =
                node_children(
                    {node_hash(Node, Opts), Prefix},
                    Tree
                ),
            ok = tree_equal(
                ExpectedRefs,
                [
                    {Root, ChildPrefix}
                || {_Key, ChildPrefix, Root, _Count, _Link} <-
                    child_specs(Node, Opts)
                ]
            );
        _ ->
            throw('invalid-account-tree')
    end.

%% @doc Return the suffix of a full key after its Patricia prefix.
suffix(root, <<>>) -> no_prefix;
suffix(Prefix, Key) when is_binary(Prefix), is_binary(Key) ->
    ok = tree_assert(is_prefix(Prefix, Key)),
    binary:part(Key, byte_size(Prefix), byte_size(Key) - byte_size(Prefix));
suffix(_Prefix, _Key) ->
    throw('invalid-account-tree').

%% @doc Join a Patricia map prefix and compressed suffix into a full key.
full_key(root, no_prefix) -> <<>>;
full_key(Prefix, Suffix) -> <<Prefix/binary, Suffix/binary>>.

%% @doc True when Prefix is an initial segment of Binary.
is_prefix(Prefix, Binary) when byte_size(Prefix) =< byte_size(Binary) ->
    Prefix == binary:part(Binary, 0, byte_size(Prefix));
is_prefix(_Prefix, _Binary) ->
    false.

%% @doc Choose the account form. The two-element form is not an abbreviation
%% of the four-element one -- they are separate hash preimages -- so this line
%% must sit exactly where `ar_node_utils:update_account/6' draws it.
collapse(Balance, LastTX, 1, true) ->
    {Balance, LastTX};
collapse(Balance, LastTX, Denomination, MiningPermission) ->
    {Balance, LastTX, Denomination, MiningPermission}.

%% @doc Expand either consensus account tuple to its four semantic fields.
expand({Balance, LastTX}) ->
    {Balance, LastTX, 1, true};
expand({Balance, LastTX, Denomination, MiningPermission}) ->
    {Balance, LastTX, Denomination, MiningPermission}.

%% @doc Count the optional terminal value at a Patricia node.
terminal_count(no_value) -> 0;
terminal_count({v, _Account}) -> 1.
