%%% @doc An AO-Core interface to Arweave's Merkle-Patricia account tree.
%%%
%%% Every state carries the native wallet-list root and an AO-Core link to an
%%% immutable adaptive Patricia graph. Branches commit ordered native summaries
%%% and link to bounded persistent child vectors; bounded complete subtrees pack
%%% their accounts into canonical pages.
%%%
%%% Sparse transitions hydrate only the pages named by their diff. Untouched
%%% content references become hash/count stubs, which lets the vendored
%%% Patricia insert, delete and SHA-384 code run unchanged without descending
%%% into unrelated accounts.
%%%
%%% A private `tree' exists only while bootstrap pages are accumulated. Durable
%%% states execute from their authenticated graph, so private data cannot alter
%%% consensus results.
-module(dev_arweave_wallets).
-implements(<<"arweave-wallets@2.9">>).
-device_libraries([lib_arweave_accounts]).
-compile({no_auto_import, [apply/3]}).
-export([info/1, root/3, verify/3, get/3, apply/3, insert/3, finalize/3]).
-include("include/hb.hrl").

-define(DEVICE, <<"arweave-wallets@2.9">>).
-define(CHILD_VECTOR_WIDTH, 32).

%% @doc Export only account-tree operations, leaving message manipulation to
%% `message@1.0'.
info(_Base) ->
    #{
        excludes => [<<"keys">>, <<"set">>, <<"set-path">>, <<"remove">>]
    }.

%% @doc Recompute the native root from the represented accounts.
root(Base, _Req, Opts) ->
    guarded(
        fun() ->
            {Root, _Memoised} =
                lib_arweave_accounts:root(full_tree(Base, Opts)),
            {ok, #{ <<"root">> => Root }}
        end
    ).

%% @doc Verify every represented account against an expected native root.
verify(Base, Req, Opts) ->
    guarded(
        fun() ->
            maybe
                Expected = required(<<"expected-root">>, Base, Req, Opts),
                {Root, _Memoised} =
                    lib_arweave_accounts:root(full_tree(Base, Opts)),
                ok ?= match_root(Root, Expected),
                {ok, #{ <<"valid">> => true }}
            end
        end
    ).

%% @doc Return one account, or a sparse message for a set of addresses.
get(Base, Req, Opts) ->
    guarded(
        fun() ->
            case get_first(<<"address">>, Base, Req, not_found, Opts) of
                not_found -> get_many(Base, Req, Opts);
                Address -> get_one(Base, Address, Opts)
            end
        end
    ).

%% @doc Apply a sparse Arweave account transition and persist its new graph.
apply(Base, Req, Opts) ->
    guarded(
        fun() ->
            maybe
                {ok, Diff} ?=
                    parse_diff(
                        required(<<"diff">>, Base, Req, Opts),
                        Opts
                    ),
                Tree = transition_tree(Base, maps:keys(Diff), Opts),
                Applied = lib_arweave_accounts:apply_diff(Diff, Tree),
                {Root, Memoised, Update} =
                    lib_arweave_accounts:root_update(Applied),
                ok ?=
                    match_root(
                        Root,
                        get_first(
                            <<"expected-root">>,
                            Base,
                            Req,
                            [],
                            Opts
                        )
                    ),
                {ok, write(Root, Memoised, Update, Opts)}
            end
        end
    ).

%% @doc Insert a generic AO-Core message of accounts into an accumulator.
%% The accumulator remains private until `finalize', allowing peer pages to be
%% folded without exposing their transport encoding to this device.
insert(Base, Req, Opts) ->
    guarded(
        fun() ->
            maybe
                {ok, Accounts} ?=
                    parse_accounts(
                        required(<<"accounts">>, Base, Req, Opts),
                        Opts
                    ),
                Tree =
                    lib_arweave_accounts:insert_all(
                        Accounts,
                        transition_tree(
                            Base,
                            [Address || {Address, _} <- Accounts],
                            Opts
                        )
                    ),
                {ok,
                    with_tree(
                        #{ <<"device">> => ?DEVICE },
                        Tree
                    )
                }
            end
        end
    ).

%% @doc Compute and persist an accumulated account tree.
finalize(Base, Req, Opts) ->
    guarded(
        fun() ->
            maybe
                Tree = full_tree(Base, Opts),
                {Root, Memoised, Update} =
                    lib_arweave_accounts:root_update(Tree),
                ok ?=
                    match_root(
                        Root,
                        get_first(
                            <<"expected-root">>,
                            Base,
                            Req,
                            [],
                            Opts
                        )
                    ),
                {ok, write(Root, Memoised, Update, Opts)}
            end
        end
    ).

%%% Internal functions.

%% @doc Keep malformed public requests and account graphs inside AO errors.
guarded(Fun) ->
    try Fun()
    catch
        throw:{missing, Key} ->
            {error,
                error_message(
                    <<"invalid-request">>,
                    <<"The request is missing ", Key/binary, ".">>
                )
            };
        throw:'invalid-account-tree' -> invalid_tree_error();
        throw:{'invalid-account-node-link', _Key} -> invalid_tree_error()
    end.

%% @doc Return the controlled malformed account-graph error.
invalid_tree_error() ->
    {error,
        error_message(
            <<"invalid-account-tree">>,
            <<"The account graph is malformed or unavailable.">>
        )
    }.

%% @doc Validate a generic AO-Core message at the device boundary.
parse_accounts(Message, Opts) ->
    case is_map(Message) orelse ?IS_LINK(Message) of
        true ->
            try {ok, lib_arweave_accounts:accounts(Message, Opts)}
            catch
                throw:'invalid-account' -> invalid_account_error()
            end;
        false ->
            invalid_account_error()
    end.

%% @doc Validate a generic sparse diff at the device boundary.
parse_diff(Message, Opts) ->
    case is_map(Message) orelse ?IS_LINK(Message) of
        true ->
            try {ok, lib_arweave_accounts:diff(Message, Opts)}
            catch
                throw:'invalid-account' -> invalid_account_error()
            end;
        false ->
            invalid_account_error()
    end.

%% @doc Return the controlled malformed-account error.
invalid_account_error() ->
    {error,
        error_message(
            <<"invalid-account">>,
            <<"An account is not in canonical Arweave form.">>
        )
    }.

%% @doc Decode one canonical base64url account address.
parse_address(Encoded) ->
    try {ok, lib_arweave_accounts:address(Encoded)}
    catch
        throw:'invalid-account' ->
            {error,
                error_message(
                    <<"invalid-address">>,
                    <<"The account address is not canonical base64url.">>
                )
            }
    end.

%% @doc Decode a list of canonical account addresses.
parse_addresses(Message, Opts) ->
    case is_list(Message) orelse is_map(Message) orelse ?IS_LINK(Message) of
        true -> parse_address_message(Message, Opts);
        false -> invalid_addresses_error()
    end.

%% @doc Decode an AO ordered-list message without masking store failures.
parse_address_message(Message, Opts) ->
    try
        {ok,
            [
                lib_arweave_accounts:address(Address)
            || Address <- hb_util:message_to_ordered_list(Message, Opts)
            ]
        }
    catch
        throw:'invalid-account' -> invalid_addresses_error();
        throw:{missing_key, _Details} -> invalid_addresses_error();
        throw:{missing_key, _Key, _Details} -> invalid_addresses_error();
        error:badarg -> invalid_addresses_error()
    end.

%% @doc Return the controlled malformed-address-list error.
invalid_addresses_error() ->
    {error,
        error_message(
            <<"invalid-address">>,
            <<"Account addresses must be a canonical AO message.">>
        )}.

%% @doc Write updated nodes bottom-up and return a history-free state.
write(Root, Tree, Update, Opts) ->
    {TreeLink, _Count, _Written} = persist_root(Root, Tree, Update, Opts),
    State0 =
        #{
            <<"device">> => ?DEVICE,
            <<"root">> => Root
        },
    State =
        case TreeLink of
            none -> State0;
            _ -> State0#{ <<"tree">> => TreeLink }
        end,
    {ok, StateID} = hb_cache:write(State, internal_opts(Opts)),
    {ok, StoredState} = hb_cache:read(StateID, internal_opts(Opts)),
    StateID =
        hb_message:id(
            StoredState,
            none,
            Opts#{ <<"linkify-mode">> => discard }
        ),
    State.

%% @doc Persist the pure root node and return its content link.
persist_root(<<>>, _Tree, _Update, _Opts) ->
    {none, 0, #{}};
persist_root(Root, Tree, Update, Opts) ->
    Hash = hb_util:decode(Root),
    {Link, Count, Written} =
        persist_node({Hash, root}, Tree, Update, #{}, Opts),
    RootNodeID = link_target(Link, Opts),
    {ok, RootNode} = hb_cache:read(RootNodeID, internal_opts(Opts)),
    ok = ensure_node_ref({Hash, root}, RootNode, Opts),
    RootNodeID =
        hb_message:id(
            RootNode,
            none,
            Opts#{ <<"linkify-mode">> => discard }
        ),
    {to_link(RootNodeID), Count, Written}.

%% @doc Persist one invalidated node, reusing unchanged content links.
persist_node(Ref, Tree, Update, Written, Opts) ->
    case maps:find(Ref, Written) of
        {ok, {Link, Count}} ->
            {Link, Count, Written};
        error ->
            persist_node2(Ref, Tree, Update, Written, Opts)
    end.

%% @doc Persist an invalidated node or reuse its hydrated content link.
persist_node2(Ref, Tree, Update, Written, Opts) ->
    case maps:find(Ref, Update) of
        error ->
            case lib_arweave_accounts:node_spec(Ref, Tree) of
                {ok, {Link, Count}} ->
                    {Link, Count, Written#{ Ref => {Link, Count} }};
                error ->
                    persist_node_value(
                        Ref,
                        Tree,
                        Update,
                        Written,
                        Opts
                    )
            end;
        {ok, _Value} ->
            persist_node_value(Ref, Tree, Update, Written, Opts)
    end.

%% @doc Materialise one canonical page or linked native branch.
persist_node_value(Ref, Tree, Update, Written, Opts) ->
    Count = lib_arweave_accounts:subtree_count(Ref, Tree),
    case Count =< lib_arweave_accounts:page_size() of
        true ->
            persist_node_message(
                Ref,
                Count,
                lib_arweave_accounts:page_message(Ref, Tree, Opts),
                Written,
                Opts
            );
        false ->
            ChildRefs = lib_arweave_accounts:node_children(Ref, Tree),
            case {
                element(2, Ref),
                ChildRefs,
                lib_arweave_accounts:node_has_value(Ref, Tree)
            } of
                {Prefix, [ChildRef], false} when Prefix =/= root ->
                    {Link, Count, Written2} =
                        persist_node(ChildRef, Tree, Update, Written, Opts),
                    {Link, Count, Written2#{ Ref => {Link, Count} }};
                _ ->
                    {ChildSpecs, Written2} =
                        persist_nodes(ChildRefs, Tree, Update, Written, Opts),
                    ChildrenLink =
                        persist_child_vector(
                            element(2, Ref),
                            ChildRefs,
                            ChildSpecs,
                            Tree,
                            Opts
                        ),
                    persist_node_message(
                        Ref,
                        Count,
                        lib_arweave_accounts:node_message(
                            Ref,
                            Tree,
                            lists:zip(ChildRefs, ChildSpecs),
                            ChildrenLink
                        ),
                        Written2,
                        Opts
                    )
            end
    end.

%% @doc Persist and authenticate one materialised graph message.
persist_node_message(Ref, Count, Node, Written, Opts) ->
    {ok, ID} = hb_cache:write(Node, internal_opts(Opts)),
    {ok, StoredNode} = hb_cache:read(ID, internal_opts(Opts)),
    ok = ensure_node_ref(Ref, StoredNode, Opts),
    ID =
        hb_message:id(
            StoredNode,
            none,
            Opts#{ <<"linkify-mode">> => discard }
        ),
    Link = to_link(ID),
    {Link, Count, Written#{ Ref => {Link, Count} }}.

%% @doc Persist a list of children while threading local deduplication.
persist_nodes([], _Tree, _Update, Written, _Opts) ->
    {[], Written};
persist_nodes([Ref | Refs], Tree, Update, Written, Opts) ->
    {Link, Count, Written2} = persist_node(Ref, Tree, Update, Written, Opts),
    {Specs, Written3} = persist_nodes(Refs, Tree, Update, Written2, Opts),
    {[{Link, Count} | Specs], Written3}.

%% @doc Persist a radix-32 vector whose leaves expose every child as an AO link.
persist_child_vector(Prefix, ChildRefs, ChildSpecs, Tree, Opts) ->
    case lib_arweave_accounts:node_vector(Prefix, Tree) of
        {ok, {OldVector, OldSpecs}} ->
            case compatible_children(ChildRefs, OldSpecs) of
                true ->
                    update_child_vector(
                        OldVector,
                        OldSpecs,
                        ChildRefs,
                        ChildSpecs,
                        Opts
                    );
                false ->
                    new_child_vector(ChildSpecs, Opts)
            end;
        error ->
            new_child_vector(ChildSpecs, Opts)
    end.

new_child_vector(ChildSpecs, Opts) ->
    Links =
        [
            lib_arweave_accounts:resolve_node_link(Link, Opts)
        || {Link, _Count} <- ChildSpecs
        ],
    Leaves =
        [
            persist_vector_message(<<"children-leaf">>, Group, Opts)
        || Group <- groups(Links, ?CHILD_VECTOR_WIDTH)
        ],
    case Leaves of
        [Leaf] -> Leaf;
        _ -> persist_vector_message(<<"children-index">>, Leaves, Opts)
    end.

compatible_children(ChildRefs, OldSpecs) ->
    [Prefix || {_Hash, Prefix} <- ChildRefs] ==
        [Prefix || {_Key, Prefix, _Root, _Count, _Link} <- OldSpecs].

update_child_vector(OldVector, OldSpecs, ChildRefs, ChildSpecs, Opts) ->
    NewLinks = [Link || {Link, _Count} <- ChildSpecs],
    Changed =
        [
            N
        || {
               N,
               {
                   {_Key, _Prefix, OldRoot, OldCount, _OldLink},
                   {{NewRoot, _NewPrefix}, {_NewLink, NewCount}}
               }
           } <-
            lists:zip(
                lists:seq(1, length(OldSpecs)),
                lists:zip(OldSpecs, lists:zip(ChildRefs, ChildSpecs))
            ),
           OldRoot =/= NewRoot orelse OldCount =/= NewCount
        ],
    case Changed of
        [] -> OldVector;
        _ when length(NewLinks) =< ?CHILD_VECTOR_WIDTH ->
            Vector = lib_arweave_accounts:load_node(
                <<"children">>,
                OldVector,
                Opts
            ),
            persist_vector_message(
                <<"children-leaf">>,
                replace_changed_links(
                    vector_values(Vector, length(NewLinks)),
                    NewLinks,
                    Changed,
                    0,
                    Opts
                ),
                Opts
            );
        _ ->
            update_vector_leaves(OldVector, NewLinks, Changed, Opts)
    end.

update_vector_leaves(OldVector, NewLinks, Changed, Opts) ->
    Index = lib_arweave_accounts:load_node(<<"children">>, OldVector, Opts),
    LeafCount = (length(NewLinks) + ?CHILD_VECTOR_WIDTH - 1) div
        ?CHILD_VECTOR_WIDTH,
    ChangedLeaves =
        lists:usort(
            [((N - 1) div ?CHILD_VECTOR_WIDTH) + 1 || N <- Changed]
        ),
    IndexMap = hb_message:uncommitted(Index),
    LeafLinks =
        [
            case lists:member(N, ChangedLeaves) of
                false -> maps:get(integer_to_binary(N), IndexMap);
                true ->
                    Start = ((N - 1) * ?CHILD_VECTOR_WIDTH) + 1,
                    OldLeaf = lib_arweave_accounts:load_node(
                        integer_to_binary(N),
                        maps:get(integer_to_binary(N), IndexMap),
                        Opts
                    ),
                    LeafSize = erlang:min(
                        ?CHILD_VECTOR_WIDTH,
                        length(NewLinks) - Start + 1
                    ),
                    persist_vector_message(
                        <<"children-leaf">>,
                        replace_changed_links(
                            vector_values(OldLeaf, LeafSize),
                            NewLinks,
                            Changed,
                            Start - 1,
                            Opts
                        ),
                        Opts
                    )
            end
        || N <- lists:seq(1, LeafCount)
        ],
    persist_vector_message(<<"children-index">>, LeafLinks, Opts).

replace_changed_links(OldLinks, NewLinks, Changed, Offset, Opts) ->
    [
        case lists:member(Offset + N, Changed) of
            true ->
                lib_arweave_accounts:resolve_node_link(
                    lists:nth(Offset + N, NewLinks),
                    Opts
                );
            false -> OldLink
        end
    || {N, OldLink} <- lists:zip(lists:seq(1, length(OldLinks)), OldLinks)
    ].

vector_values(Vector, Size) ->
    Public = hb_message:uncommitted(Vector),
    [maps:get(integer_to_binary(N), Public) || N <- lists:seq(1, Size)].

persist_vector_message(Kind, Links, Opts) ->
    Message =
        maps:merge(
            #{ <<"kind">> => Kind, <<"size">> => length(Links) },
            maps:from_list(
                lists:zip(
                    [integer_to_binary(N) || N <- lists:seq(1, length(Links))],
                    Links
                )
            )
        ),
    {ok, ID} = hb_cache:write(Message, internal_opts(Opts)),
    {ok, Stored} = hb_cache:read(ID, internal_opts(Opts)),
    ID = hb_message:id(
        Stored,
        none,
        Opts#{ <<"linkify-mode">> => discard }
    ),
    to_link(ID).

groups([], _Width) -> [];
groups(Values, Width) ->
    {Group, Rest} = lists:split(erlang:min(Width, length(Values)), Values),
    [Group | groups(Rest, Width)].

%% @doc Return one account as a message.
get_one(Base, Encoded, Opts) ->
    maybe
        {ok, Address} ?= parse_address(Encoded),
        case lookup(Base, [Address], Opts) of
            #{ Address := Account } ->
                {ok, lib_arweave_accounts:account_message(Account)};
            _ ->
                {error,
                    error_message(
                        <<"account-not-found">>,
                        <<"The account tree holds no account at that address.">>
                    )
                }
        end
    end.

%% @doc Return the accounts held under a message-ordered list of addresses.
get_many(Base, Req, Opts) ->
    Addresses = required(<<"addresses">>, Base, Req, Opts),
    maybe
        {ok, Decoded} ?=
            parse_addresses(Addresses, Opts),
        {ok,
            maps:fold(
                fun(Address, Account, Acc) ->
                    Acc#{
                        hb_util:encode(Address) =>
                            lib_arweave_accounts:account_message(Account)
                    }
                end,
                #{},
                lookup(Base, Decoded, Opts)
            )
        }
    end.

%% @doc Read only the persistent paths that contain requested accounts.
lookup(Base, Addresses, Opts) ->
    case hb_maps:is_key(<<"tree">>, Base, Opts) of
        true ->
            case root_node(Base, Opts) of
                none -> #{};
                {_Link, Node} ->
                    lists:foldl(
                        fun(Address, Acc) ->
                            case lib_arweave_accounts:get_from_node(
                                    Node, Address, Opts) of
                                not_found -> Acc;
                                Account -> Acc#{ Address => Account }
                            end
                        end,
                        #{},
                        Addresses
                    )
            end;
        false ->
            Tree = unlinked_tree(Base, Opts),
            lib_arweave_accounts:get_map(Tree, Addresses)
    end.

%% @doc Hydrate the updated Patricia paths and hash-stub every sibling.
transition_tree(Base, Addresses, Opts) ->
    case hb_maps:is_key(<<"tree">>, Base, Opts) of
        true ->
            case root_node(Base, Opts) of
                none -> lib_arweave_accounts:new();
                {Link, Node} ->
                    lib_arweave_accounts:skeleton(
                        Link,
                        Node,
                        Addresses,
                        Opts
                    )
            end;
        false ->
            unlinked_tree(Base, Opts)
    end.

%% @doc Materialise every account when a complete root check is required.
full_tree(Base, Opts) ->
    case hb_maps:is_key(<<"tree">>, Base, Opts) of
        true ->
            case root_node(Base, Opts) of
                none -> lib_arweave_accounts:new();
                {_Link, Node} ->
                    lib_arweave_accounts:from_node(Node, Opts)
            end;
        false ->
            unlinked_tree(Base, Opts)
    end.

%% @doc Return an accumulator tree or validate an empty persisted state.
unlinked_tree(Base, Opts) ->
    ok = validate_unlinked_state(Base, Opts),
    case hb_private:get(<<"tree">>, Base, not_found, Opts) of
        not_found ->
            case hb_maps:get(<<"root">>, Base, not_found, Opts) of
                not_found ->
                    lib_arweave_accounts:new();
                <<>> ->
                    lib_arweave_accounts:new()
            end;
        Tree ->
            Tree
    end.

%% @doc Accept only a fresh accumulator or the canonical empty state.
validate_unlinked_state(Base, Opts) ->
    Public = hb_private:reset(hb_message:uncommitted(Base)),
    case lists:sort(maps:keys(Public)) of
        [<<"device">>] ->
            case hb_maps:get(<<"device">>, Public, not_found, Opts) of
                ?DEVICE -> ok;
                _ -> throw('invalid-account-tree')
            end;
        [<<"device">>, <<"root">>] ->
            case {
                hb_maps:get(<<"device">>, Public, not_found, Opts),
                hb_maps:get(<<"root">>, Public, not_found, Opts)
            } of
                {?DEVICE, <<>>} -> ok;
                _ -> throw('invalid-account-tree')
            end;
        _ ->
            throw('invalid-account-tree')
    end.

%% @doc Load the explicit root node link, or identify an empty state.
root_node(Base, Opts) ->
    ok = validate_linked_state(Base, Opts),
    case maps:get(<<"tree">>, Base, not_found) of
        not_found -> none;
        Link ->
            Node = lib_arweave_accounts:load_node(<<"tree">>, Link, Opts),
            NodeRoot = hb_maps:get(<<"root">>, Node, not_found, Opts),
            case hb_maps:get(<<"root">>, Base, not_found, Opts) of
                NodeRoot -> {Link, Node};
                _ -> throw('invalid-account-tree')
            end
    end.

%% @doc Require the canonical linked account-state field set.
validate_linked_state(Base, Opts) ->
    Public = hb_private:reset(hb_message:uncommitted(Base)),
    case lists:sort(maps:keys(Public)) of
        [<<"device">>, <<"root">>, <<"tree">>] ->
            case {
                hb_maps:get(<<"device">>, Public, not_found, Opts),
                hb_maps:get(<<"root">>, Public, not_found, Opts)
            } of
                {?DEVICE, Root} when is_binary(Root) -> ok;
                _ -> throw('invalid-account-tree')
            end;
        _ ->
            throw('invalid-account-tree')
    end.

%% @doc Attach the in-flight bootstrap accumulator without changing its identity.
with_tree(State, Tree) ->
    hb_private:set_priv(State, #{ <<"tree">> => Tree }).

%% @doc Disable reverse-match indexing for internal graph messages.
internal_opts(Opts) ->
    Opts#{ <<"match-index">> => false }.

%% @doc Resolve one lazy field link to its canonical content identifier.
link_target({link, ID, LinkOpts}, Opts) ->
    case maps:get(<<"lazy">>, LinkOpts, false) of
        true ->
            {ok, TargetID} = hb_cache:read(ID, internal_opts(Opts)),
            true = ?IS_ID(TargetID),
            TargetID;
        false ->
            true = ?IS_ID(ID),
            ID
    end.

%% @doc Prove a stored node is the native node its alias claims.
ensure_node_ref({Hash, Prefix}, Node, Opts) ->
    EncodedHash = hb_util:encode(Hash),
    EncodedHash = hb_maps:get(<<"root">>, Node, not_found, Opts),
    ok = lib_arweave_accounts:authenticate_node(Node, Prefix, Opts),
    ok.

%% @doc Build an eager AO-Core link to a stored node.
to_link(ID) ->
    {link, ID, #{ <<"type">> => <<"link">>, <<"lazy">> => false }}.

%% @doc Require a key, preferring the request to the base message.
required(Key, Base, Req, Opts) ->
    case get_first(Key, Base, Req, not_found, Opts) of
        not_found -> throw({missing, Key});
        Value -> Value
    end.

%% @doc Read a key from the request, then the base, then a default.
get_first(Key, Base, Req, Default, Opts) ->
    case hb_maps:find(Key, Req, Opts) of
        {ok, Value} -> Value;
        error -> hb_maps:get(Key, Base, Default, Opts)
    end.

%% @doc Accept an omitted expectation or require exact root equality.
match_root(_Root, []) -> ok;
match_root(Root, Root) -> ok;
match_root(_Root, _Expected) ->
    {error,
        error_message(
            <<"invalid-wallet-list-root">>,
            <<"The account tree does not match the block's wallet-list root.">>
        )
    }.

%% @doc Construct a device error message.
error_message(Code, Description) ->
    #{ <<"message">> => Code, <<"description">> => Description }.
