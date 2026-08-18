%%% @doc An AO-Core interface to Arweave's block index: the
%%% `{indep-hash, weave-size, tx-root}' triplet of every block from genesis.
%%%
%%% A proof of access names a recall byte, so block validation must map an
%%% arbitrary weave offset to the block that wrote it. The index is an immutable
%%% ordered tree whose content links make every historical root independently
%%% readable and forkable.
%%%
%%% Completed leaves form an immutable tree. One bounded tail leaf absorbs normal
%%% appends; only completing a page path-copies the tree's right edge. Branches
%%% hold ordinary AO-Core links and compact subtree boundaries. Callers append
%%% ordered entry messages and interact with the index through semantic operations.
%%%
%%% The prior state message is the transition trust boundary: its AO ID commits
%%% the exact topology links and native root accepted by its parent transition.
%%% `append/3' authenticates and rebuilds the changed path. `verify/3' recursively
%%% proves every untouched subtree when importing an untrusted state.
%%%
%%% A block's `hash-list-merkle' is Arweave's prefix accumulator over every
%%% preceding entry. `root/3' recomputes it from the leaves, while `append/3'
%%% carries the same accumulator forward for constant-cost block validation.
-module(dev_arweave_block_index).
-implements(<<"arweave-block-index@2.9">>).
-export([info/1, bounds/3, at/3, root/3, verify/3, append/3]).
-include("include/hb.hrl").

%%% A leaf stays small enough for one bounded LMDB read. A branch fanout of 32
%%% keeps the current mainnet index four links deep, including its leaf.
-define(LEAF_SIZE, 128).
-define(BRANCH_SIZE, 32).
%%% A stored entry is a 48-byte block hash, a 64-bit weave size, and a tx root
%%% padded to 32 bytes with its true length recorded alongside.
-define(ENTRY_SIZE, 89).

%% @doc Export only the semantic index operations.
info(_Base) ->
    #{
        excludes => [<<"keys">>, <<"set">>, <<"set-path">>, <<"remove">>]
    }.

%% @doc Return the weave range and tx root of the block that wrote `offset'.
bounds(Base, Req, Opts) ->
    topology_result(fun() ->
        canonical_state(Base, Opts),
        case request_integer(<<"offset">>, Req, not_found, Opts) of
            {error, _} = Error -> Error;
            {ok, Offset} ->
                case Offset >= 0 andalso entries(Base, Opts) > 0 of
                    false ->
                        offset_out_of_range();
                    true ->
                        case find_bounds_state(Base, Offset, Opts) of
                            not_found -> offset_out_of_range();
                            {BlockStart, BlockEnd, TXRoot} ->
                                {ok,
                                    #{
                                        <<"block-start">> => BlockStart,
                                        <<"block-end">> => BlockEnd,
                                        <<"tx-root">> => hb_util:encode(TXRoot)
                                    }
                                }
                        end
                end
        end
    end).

%% @doc Return the triplet recorded for the block at `height'.
at(Base, Req, Opts) ->
    topology_result(fun() ->
        canonical_state(Base, Opts),
        case request_integer(<<"height">>, Req, not_found, Opts) of
            {error, _} = Error -> Error;
            {ok, Height} ->
                case Height >= 0 andalso Height < entries(Base, Opts) of
                    false ->
                        {error, error_message(<<"height-out-of-range">>,
                            <<"The index does not cover the given height.">>)};
                    true ->
                        {Hash, WeaveSize, TXRoot} =
                            find_at_state(Base, Height, Opts),
                        {ok,
                            #{
                                <<"indep-hash">> => hb_util:encode(Hash),
                                <<"weave-size">> => WeaveSize,
                                <<"tx-root">> => hb_util:encode(TXRoot)
                            }
                        }
                end
        end
    end).

%% @doc Recompute Arweave's prefix accumulator from every stored entry.
root(Base, _Req, Opts) ->
    topology_result(fun() ->
        canonical_state(Base, Opts),
        {ok, #{ <<"root">> => hb_util:encode(recompute_root(Base, Opts)) }}
    end).

%% @doc Verify the full index against a block's `hash-list-merkle'.
verify(Base, Req, Opts) ->
    topology_result(fun() ->
        canonical_state(Base, Opts),
        case request_root(Req, Opts) of
            {error, _} = Error -> Error;
            {ok, Expected} ->
                case recompute_root(Base, Opts) of
                    Expected -> {ok, #{ <<"valid">> => true }};
                    _ -> invalid_root()
                end
        end
    end).

%% @doc Append one entry, or an ordered AO message under `entries'.
%%
%% A batch may carry `start-height'; it must name the next height. This is the
%% semantic bootstrap interface: decoding a peer's transport belongs to the
%% application layer that constructs these entry messages. Existing untouched
%% subtrees inherit the prior state's validity; changed links are authenticated.
append(Base, Req, Opts) ->
    topology_result(fun() -> append_validated(Base, Req, Opts) end).

append_validated(Base, Req, Opts) ->
    {_, _, Length, _, _} = State = current_state(Base, Opts),
    case request_integer(<<"start-height">>, Req, Length, Opts) of
        {error, _} = Error -> Error;
        {ok, Length} ->
            case requested_entries(Req, Opts) of
                {ok, Parsed} -> extend_validated(Base, State, Parsed, Opts);
                {error, _} = Error -> Error
            end;
        {ok, _} ->
            {error, error_message(<<"non-contiguous-index-range">>,
                <<"The range does not start where the index ends.">>)}
    end.

%%% Internal functions.

topology_result(Fun) ->
    try Fun()
    catch
        throw:{'invalid-block-index-topology', _} -> invalid_topology();
        throw:{'invalid-block-index-link', _} -> invalid_topology();
        throw:'invalid-block-index-node' -> invalid_topology();
        throw:{necessary_message_not_found, _, _} -> invalid_topology()
    end.

%% @doc Extend a validated tree and return a clean public index state.
extend_validated(Base, State, Parsed, Opts) ->
    maybe
        ok ?= storable(Parsed),
        {Completed, Tail, Length, CurrentRoot, LastWeaveSize} =
            State,
        ok ?= monotonic(Parsed, LastWeaveSize),
        ExpectedRoot = fold_entries(Parsed, CurrentRoot),
        {NewCompleted, NewTail} =
            case {Length, Parsed} of
                {0, []} -> {none, none};
                {0, _} -> build_index(Parsed, Opts);
                {_, []} -> {Completed, Tail};
                {_, _} ->
                    append_index(Base, Completed, Tail, Parsed, Opts)
            end,
        true ?= ExpectedRoot == parts_root(NewCompleted, NewTail),
        {ok,
            persisted_state(
                state(Length + length(Parsed), NewCompleted, NewTail),
                Opts
            )
        }
    else
        false ->
            {error, error_message(<<"corrupt-block-index">>,
                <<"The index topology disagrees with its accumulator.">>)};
        {error, _} = Error ->
            Error
    end.

%% @doc Build completed leaves and one mutable tail from a bootstrap batch.
build_index(Parsed, Opts) ->
    {CompletedLeaves, Tail} = split_tail(
        build_leaf_specs(Parsed, 0, 0, <<>>, Opts)
    ),
    {finish_completed(CompletedLeaves, Opts), Tail}.

%% @doc Build and persist bounded leaves, carrying Arweave's root between them.
build_leaf_specs(Parsed, Height, WeaveSize, Root, Opts) ->
    build_leaf_specs(Parsed, Height, WeaveSize, Root, Opts, []).
build_leaf_specs([], _Height, _WeaveSize, _Root, _Opts, Acc) ->
    lists:reverse(Acc);
build_leaf_specs(Parsed, Height, WeaveSize, Root, Opts, Acc) ->
    {Page, Rest} = take(?LEAF_SIZE, Parsed),
    Count = length(Page),
    PageRoot = fold_entries(Page, Root),
    {_, PageMax, _} = lists:last(Page),
    Node =
        #{
            <<"node-type">> => <<"leaf">>,
            <<"count">> => Count,
            <<"start-height">> => Height,
            <<"start-weave-size">> => WeaveSize,
            <<"max-weave-size">> => PageMax,
            <<"start-root">> => hb_util:encode(Root),
            <<"root">> => hb_util:encode(PageRoot),
            <<"body">> => encode_entries(Page)
        },
    build_leaf_specs(
        Rest,
        Height + Count,
        PageMax,
        PageRoot,
        Opts,
        [persisted_spec(Node, Opts) | Acc]
    ).

%% @doc Rewrite only the bounded tail on ordinary appends. When pages become
%% complete, insert them into the immutable tree with one right-edge path copy.
append_index(Base, Completed, Tail, Parsed, Opts) ->
    NewLeaves =
        case Tail of
            none ->
                build_leaf_specs(
                    Parsed,
                    entries(Base, Opts),
                    spec_max(final_part(Completed, Tail)),
                    state_root(Base, Opts),
                    Opts
                );
            _ ->
                TailNode = load_node(<<"tail">>, spec_link(Tail), Opts),
                {Body, Count, Height, WeaveSize, Root} = leaf(TailNode, Opts),
                build_leaf_specs(
                    decode_entries(Body, Count) ++ Parsed,
                    Height,
                    WeaveSize,
                    Root,
                    Opts
                )
        end,
    {CompletedLeaves, NewTail} = split_tail(NewLeaves),
    NewCompleted = append_completed(Completed, CompletedLeaves, Opts),
    {NewCompleted, NewTail}.

%% @doc Keep the last partial page as the mutable tail.
split_tail([]) ->
    {[], none};
split_tail(Specs) ->
    Last = lists:last(Specs),
    case spec_count(Last) < ?LEAF_SIZE of
        true -> {lists:droplast(Specs), Last};
        false -> {Specs, none}
    end.

%% @doc Build the completed tree when a bootstrap produced full leaves.
finish_completed([], _Opts) ->
    none;
finish_completed(Specs, Opts) ->
    finish_root(Specs, Opts).

%% @doc Insert completed leaf links into the right edge as one batch.
append_completed(Completed, [], _Opts) ->
    Completed;
append_completed(none, Leaves, Opts) ->
    finish_root(Leaves, Opts);
append_completed(Completed, Leaves, Opts) ->
    Node = load_node(<<"completed">>, spec_link(Completed), Opts),
    finish_root(append_completed_node(Node, Completed, Leaves, Opts), Opts).

%% @doc Add leaf specifications without rewriting any existing leaf body.
append_completed_node(Node, Existing, Leaves, Opts) ->
    case node_type(Node, Opts) of
        <<"leaf">> ->
            [spec(spec_link(Existing), Node, Opts) | Leaves];
        <<"branch">> ->
            ChildCount = branch_child_count(Node, Opts),
            Boundaries = branch_boundaries(Node, ChildCount, Opts),
            Prefix = [
                child_spec(Node, N, Boundaries)
            ||
                N <- lists:seq(1, ChildCount - 1)
            ],
            LastSpec = child_spec(Node, ChildCount, Boundaries),
            Last = load_node(
                hb_util:bin(ChildCount),
                spec_link(LastSpec),
                Opts
            ),
            make_branch_specs(
                Prefix ++ append_completed_node(Last, LastSpec, Leaves, Opts),
                Opts
            )
    end.

%% @doc Load the completed tree and tail links carried by the public state.
state_parts(Base, Opts) ->
    {
        optional_part(<<"completed">>, Base, Opts),
        optional_part(<<"tail">>, Base, Opts)
    }.

optional_part(Key, Base, Opts) ->
    case maps:get(Key, Base, not_found) of
        not_found -> none;
        Link when ?IS_LINK(Link) ->
            Node = load_node(Key, Link, Opts),
            spec(Link, Node, Opts);
        _ -> throw({'invalid-block-index-link', Key})
    end.

%% @doc Group child specifications into persisted branch nodes.
make_branch_specs(Specs, Opts) ->
    make_branch_specs(Specs, Opts, []).
make_branch_specs([], _Opts, Acc) ->
    lists:reverse(Acc);
make_branch_specs(Specs, Opts, Acc) ->
    {Children, Rest} = take(?BRANCH_SIZE, Specs),
    Count = lists:sum([spec_count(Child) || Child <- Children]),
    Last = lists:last(Children),
    Node0 =
        #{
            <<"node-type">> => <<"branch">>,
            <<"count">> => Count,
            <<"child-count">> => length(Children),
            <<"max-weave-size">> => spec_max(Last),
            <<"root">> => hb_util:encode(spec_root(Last)),
            <<"boundaries">> => encode_boundaries(Children)
        },
    Node = add_children(Children, 1, Node0),
    make_branch_specs(Rest, Opts, [persisted_spec(Node, Opts) | Acc]).

%% @doc Add ordinary AO links and bounded selection metadata to a branch.
add_children([], _N, Node) ->
    Node;
add_children([Child | Rest], N, Node) ->
    Key = hb_util:bin(N),
    add_children(
        Rest,
        N + 1,
        Node#{ Key => spec_link(Child) }
    ).

%% @doc Encode one fixed-width routing boundary for each ordinary child link.
encode_boundaries(Children) ->
    <<
        <<(spec_count(Child)):64, (spec_max(Child)):64>>
    ||
        Child <- Children
    >>.

%% @doc Add parent levels until one content link identifies the whole tree.
finish_root([Final], _Opts) ->
    Final;
finish_root(Specs, Opts) ->
    finish_root(make_branch_specs(Specs, Opts), Opts).

%% @doc Find an entry by subtree counts.
find_at_state(Base, Height, Opts) ->
    case part_link(<<"tail">>, Base) of
        none ->
            find_at(part_node(<<"completed">>, Base, Opts), Height, Opts);
        TailLink ->
            Tail = load_node(<<"tail">>, TailLink, Opts),
            {_Body, _Count, StartHeight, _StartWeaveSize, _Root} =
                leaf(Tail, Opts),
            case Height >= StartHeight of
                true -> find_at(Tail, Height - StartHeight, Opts);
                false ->
                    find_at(
                        part_node(<<"completed">>, Base, Opts),
                        Height,
                        Opts
                    )
            end
    end.

find_at(Node, Height, Opts) ->
    case node_type(Node, Opts) of
        <<"leaf">> ->
            {Body, Count, _StartHeight, _StartWeaveSize, _Root} =
                leaf(Node, Opts),
            ok = valid_topology(Height < Count, 'height-outside-leaf'),
            decode_entry(entry_at(Body, Height));
        <<"branch">> ->
            ChildCount = branch_child_count(Node, Opts),
            Boundaries = branch_boundaries(Node, ChildCount, Opts),
            find_at_child(
                Node,
                Height,
                1,
                ChildCount,
                Boundaries,
                Opts
            )
    end.

%% @doc Select the child whose count covers a relative height.
find_at_child(Node, Height, N, ChildCount, Boundaries, Opts)
        when N =< ChildCount ->
    Count = boundary_count(Boundaries, N),
    case Height < Count of
        true -> find_at(child(Node, N, Opts), Height, Opts);
        false ->
            find_at_child(
                Node,
                Height - Count,
                N + 1,
                ChildCount,
                Boundaries,
                Opts
            )
    end;
find_at_child(_Node, _Height, _N, _ChildCount, _Boundaries, _Opts) ->
    topology_error('invalid-branch-boundaries').

%% @doc Find the first entry whose weave size is strictly above `Offset'.
find_bounds_state(Base, Offset, Opts) ->
    case part_link(<<"tail">>, Base) of
        none ->
            Completed = part_node(<<"completed">>, Base, Opts),
            case node_max(Completed, Opts) > Offset of
                true -> find_bounds(Completed, Offset, Opts);
                false -> not_found
            end;
        TailLink ->
            Tail = load_node(<<"tail">>, TailLink, Opts),
            {_Body, _Count, _Height, StartWeaveSize, _Root} =
                leaf(Tail, Opts),
            case {Offset >= StartWeaveSize, node_max(Tail, Opts) > Offset} of
                {true, true} -> find_bounds(Tail, Offset, Opts);
                {true, false} -> not_found;
                {false, _} ->
                    find_bounds(
                        part_node(<<"completed">>, Base, Opts),
                        Offset,
                        Opts
                    )
            end
    end.

find_bounds(Node, Offset, Opts) ->
    case node_type(Node, Opts) of
        <<"leaf">> ->
            {Body, Count, _Height, StartWeaveSize, _Root} = leaf(Node, Opts),
            Index = first_entry_above(Body, Count, Offset),
            {_, BlockEnd, TXRoot} = decode_entry(entry_at(Body, Index)),
            BlockStart =
                case Index of
                    0 -> StartWeaveSize;
                    _ -> weave_size_at(Body, Index - 1)
                end,
            {BlockStart, BlockEnd, TXRoot};
        <<"branch">> ->
            ChildCount = branch_child_count(Node, Opts),
            Boundaries = branch_boundaries(Node, ChildCount, Opts),
            find_bounds_child(
                Node,
                Offset,
                1,
                ChildCount,
                Boundaries,
                Opts
            )
    end.

%% @doc Select the first child whose maximum weave size is above `Offset'.
find_bounds_child(Node, Offset, N, ChildCount, Boundaries, Opts)
        when N =< ChildCount ->
    case boundary_max(Boundaries, N) > Offset of
        true -> find_bounds(child(Node, N, Opts), Offset, Opts);
        false ->
            find_bounds_child(
                Node,
                Offset,
                N + 1,
                ChildCount,
                Boundaries,
                Opts
            )
    end;
find_bounds_child(_Node, _Offset, _N, _ChildCount, _Boundaries, _Opts) ->
    topology_error('invalid-branch-boundaries').

%% @doc Recompute the accumulator by walking the linked leaves in order.
recompute_root(Base, Opts) ->
    case entries(Base, Opts) of
        0 ->
            ok = valid_topology(
                state_root(Base, Opts) == <<>>,
                'nonempty-root-for-empty-state'
            ),
            ok = valid_topology(
                state_parts(Base, Opts) == {none, none},
                'topology-on-empty-state'
            ),
            <<>>;
        Length ->
            Root = state_root(Base, Opts),
            {Length, 0, 0, <<>>, _MaxWeaveSize, Root} =
                validate_state(Base, Opts),
            Root
    end.

%% @doc Validate an internal subtree and return its semantic and shape summaries.
validate_subtree(Node, LeafSize, Opts) ->
    case node_type(Node, Opts) of
        <<"leaf">> ->
            canonical_keys(
                Node,
                [
                    <<"body">>, <<"count">>, <<"max-weave-size">>,
                    <<"node-type">>, <<"root">>, <<"start-height">>,
                    <<"start-root">>, <<"start-weave-size">>
                ]
            ),
            {Body, Count, Height, WeaveSize, StartRoot} = leaf(Node, Opts),
            ok = valid_topology(
                LeafSize == any orelse Count == LeafSize,
                'partial-completed-leaf'
            ),
            {MaxWeaveSize, Root} =
                validate_body(Body, 0, Count, WeaveSize, StartRoot),
            ok = valid_topology(
                node_max(Node, Opts) == MaxWeaveSize,
                'invalid-leaf-max'
            ),
            ok = valid_topology(
                node_root(Node, Opts) == Root,
                'invalid-leaf-root'
            ),
            {
                {Count, Height, WeaveSize, StartRoot, MaxWeaveSize, Root},
                0,
                1
            };
        <<"branch">> ->
            ChildCount = branch_child_count(Node, Opts),
            canonical_keys(
                Node,
                [
                    <<"boundaries">>, <<"child-count">>, <<"count">>,
                    <<"max-weave-size">>, <<"node-type">>, <<"root">>
                ] ++ [hb_util:bin(N) || N <- lists:seq(1, ChildCount)]
            ),
            Boundaries = branch_boundaries(Node, ChildCount, Opts),
            Validated = [
                validate_child(Node, N, Boundaries, LeafSize, Opts)
            ||
                N <- lists:seq(1, ChildCount)
            ],
            Summaries = [Summary || {Summary, _Level, _Pages} <- Validated],
            {Level, Pages} = canonical_shape(Validated),
            ok = contiguous(Summaries),
            {_, StartHeight, StartWeaveSize, StartRoot, _, _} =
                hd(Summaries),
            {_, _, _, _, MaxWeaveSize, Root} = lists:last(Summaries),
            Count = lists:sum([Count0 || {Count0, _, _, _, _, _} <- Summaries]),
            ok = valid_topology(
                node_count(Node, Opts) == Count,
                'invalid-branch-count'
            ),
            ok = valid_topology(
                node_max(Node, Opts) == MaxWeaveSize,
                'invalid-branch-max'
            ),
            ok = valid_topology(
                node_root(Node, Opts) == Root,
                'invalid-branch-root'
            ),
            {
                {Count, StartHeight, StartWeaveSize, StartRoot,
                    MaxWeaveSize, Root},
                Level,
                Pages
            }
    end.

%% @doc Authenticate the two state links and continuity between their parts.
validate_state(Base, Opts) ->
    {Completed, Tail} = state_parts(Base, Opts),
    ok = valid_topology(
        Completed =/= none orelse Tail =/= none,
        'missing-state-topology'
    ),
    Summaries =
        optional_summary(Completed, ?LEAF_SIZE, <<"completed">>, Opts) ++
            optional_summary(Tail, any, <<"tail">>, Opts),
    ok = contiguous(Summaries),
    {_, StartHeight, StartWeaveSize, StartRoot, _, _} = hd(Summaries),
    ok = valid_topology(
        {StartHeight, StartWeaveSize, StartRoot} == {0, 0, <<>>},
        'invalid-topology-origin'
    ),
    {_, _, _, _, MaxWeaveSize, Root} = lists:last(Summaries),
    Count = lists:sum([Count0 || {Count0, _, _, _, _, _} <- Summaries]),
    ok = valid_topology(entries(Base, Opts) == Count, 'invalid-state-length'),
    ok = valid_topology(state_root(Base, Opts) == Root, 'invalid-state-root'),
    {Count, 0, 0, <<>>, MaxWeaveSize, Root}.

optional_summary(none, _LeafSize, _Key, _Opts) ->
    [];
optional_summary(Spec, LeafSize, Key, Opts) ->
    Part = load_node(Key, spec_link(Spec), Opts),
    {Summary, Level, _Pages} = validate_subtree(Part, LeafSize, Opts),
    case {Key, Level} of
        {<<"tail">>, 0} ->
            ok = valid_topology(
                node_count(Part, Opts) < ?LEAF_SIZE,
                'full-tail-leaf'
            );
        {<<"tail">>, _} ->
            topology_error('non-leaf-tail');
        {<<"completed">>, 0} -> ok;
        {<<"completed">>, _} ->
            ok = valid_topology(
                branch_child_count(Part, Opts) > 1,
                'unary-completed-root'
            )
    end,
    [Summary].

%% @doc Validate a branch's routing metadata against the linked child itself.
validate_child(Node, N, Boundaries, LeafSize, Opts) ->
    Validated = {
        {Count, _Height, _WeaveSize, _StartRoot, Max, _Root},
        _Level,
        _Pages
    } = validate_subtree(child(Node, N, Opts), LeafSize, Opts),
    ok = valid_topology(
        boundary_count(Boundaries, N) == Count,
        'invalid-child-count-boundary'
    ),
    ok = valid_topology(
        boundary_max(Boundaries, N) == Max,
        'invalid-child-max-boundary'
    ),
    Validated.

%% @doc Enforce the unique bottom-up grouping produced by `finish_root'.
canonical_shape(Validated) ->
    Levels = [Level || {_Summary, Level, _Pages} <- Validated],
    [Level | _] = Levels,
    ok = valid_topology(
        lists:all(fun(ChildLevel) -> ChildLevel == Level end, Levels),
        'nonuniform-branch-levels'
    ),
    Capacity = int_pow(?BRANCH_SIZE, Level),
    PageCounts = [Pages || {_Summary, _Level, Pages} <- Validated],
    Last = lists:last(PageCounts),
    ok = valid_topology(
        Last > 0 andalso Last =< Capacity,
        'invalid-trailing-branch-capacity'
    ),
    ok = valid_topology(
        lists:all(
            fun(Pages) -> Pages == Capacity end,
            lists:droplast(PageCounts)
        ),
        'invalid-branch-grouping'
    ),
    {Level + 1, lists:sum(PageCounts)}.

int_pow(_Base, 0) -> 1;
int_pow(Base, Exponent) -> Base * int_pow(Base, Exponent - 1).

%% @doc Validate leaf height, weave, and accumulator continuity in order.
contiguous([]) ->
    ok;
contiguous([_]) ->
    ok;
contiguous([
        {Count, Height, _StartWeave, _StartRoot, Max, Root},
        Next = {_NextCount, NextHeight, NextStartWeave, NextStartRoot,
            _NextMax, _NextRoot}
        | Rest
    ]) ->
    ok = valid_topology(
        NextHeight == Height + Count,
        'non-contiguous-height'
    ),
    ok = valid_topology(NextStartWeave == Max, 'non-contiguous-weave-size'),
    ok = valid_topology(NextStartRoot == Root, 'non-contiguous-root'),
    contiguous([Next | Rest]).

%% @doc Authenticate canonical entries and their monotonic weave order.
validate_body(_Body, Count, Count, WeaveSize, Root) ->
    {WeaveSize, Root};
validate_body(Body, Index, Count, PreviousWeaveSize, Root) ->
    Entry = {_, WeaveSize, _} = decode_entry(entry_at(Body, Index)),
    ok = valid_topology(
        WeaveSize >= PreviousWeaveSize,
        'non-monotonic-leaf'
    ),
    validate_body(
        Body,
        Index + 1,
        Count,
        WeaveSize,
        ar_unbalanced_merkle:root(
            Root,
            Entry,
            fun ar_unbalanced_merkle:hash_block_index_entry/1
        )
    ).

%% @doc Fold Arweave's exact unbalanced Merkle recurrence over entries.
fold_entries(Entries, Root) ->
    lists:foldl(
        fun(Entry, Acc) ->
            ar_unbalanced_merkle:root(
                Acc,
                Entry,
                fun ar_unbalanced_merkle:hash_block_index_entry/1
            )
        end,
        Root,
        Entries
    ).

%% @doc Binary search a leaf for the first weave size above `Offset'.
first_entry_above(Body, Count, Offset) ->
    ok = valid_topology(
        Count > 0 andalso weave_size_at(Body, Count - 1) > Offset,
        offset_outside_leaf
    ),
    search(Body, Offset, 0, Count - 1).

search(_Body, _Offset, Index, Index) ->
    Index;
search(Body, Offset, Lo, Hi) ->
    Mid = Lo + ((Hi - Lo) div 2),
    case weave_size_at(Body, Mid) > Offset of
        true -> search(Body, Offset, Lo, Mid);
        false -> search(Body, Offset, Mid + 1, Hi)
    end.

%% @doc Parse a single request entry or an ordered message of entries.
requested_entries(Req, Opts) ->
    case hb_maps:get(<<"entries">>, Req, not_found, Opts) of
        not_found -> requested_entry(Req, Opts);
        Ordered ->
            case ordered_entries(Ordered, Opts) of
                {ok, Entries} -> requested_entry_list(Entries, Opts, []);
                {error, _} = Error -> Error
            end
    end.

requested_entry(Entry, Opts) ->
    case entry_message(Entry, Opts) of
        {ok, Parsed} -> {ok, [Parsed]};
        {error, _} = Error -> Error
    end.

requested_entry_list([], _Opts, Acc) ->
    {ok, lists:reverse(Acc)};
requested_entry_list([Entry | Rest], Opts, Acc) ->
    case entry_message(Entry, Opts) of
        {ok, Parsed} -> requested_entry_list(Rest, Opts, [Parsed | Acc]);
        {error, _} = Error -> Error
    end.

ordered_entries(Ordered, Opts) when is_map(Ordered) ->
    try hb_util:message_to_ordered_list(Ordered, Opts) of
        Entries when is_list(Entries) -> {ok, Entries}
    catch
        throw:{missing_key, _} -> invalid_entry();
        throw:{necessary_message_not_found, _, _} -> invalid_entry();
        error:badarg -> invalid_entry()
    end;
ordered_entries(_Ordered, _Opts) ->
    invalid_entry().

%% @doc Convert an AO entry message to the accumulator's native triplet.
entry_message(Entry, Opts) ->
    maybe
        {ok, Hash} ?= entry_binary(<<"indep-hash">>, Entry, Opts),
        {ok, WeaveSize} ?= entry_integer(<<"weave-size">>, Entry, Opts),
        {ok, TXRoot} ?= entry_binary(<<"tx-root">>, Entry, Opts),
        {ok, {Hash, WeaveSize, TXRoot}}
    else
        {error, invalid} -> invalid_entry()
    end.

entry_binary(Key, Entry, Opts) when is_map(Entry) ->
    try hb_maps:get(Key, Entry, not_found, Opts) of
        not_found -> {error, invalid};
        Encoded ->
            try canonical_decode(Encoded, Key) of
                Decoded -> {ok, Decoded}
            catch
                throw:{'invalid-base64', _} -> {error, invalid};
                throw:{'noncanonical-base64', _} -> {error, invalid}
            end
    catch
        throw:{necessary_message_not_found, _, _} -> {error, invalid}
    end;
entry_binary(_Key, _Entry, _Opts) ->
    {error, invalid}.

entry_integer(Key, Entry, Opts) when is_map(Entry) ->
    try hb_maps:get(Key, Entry, not_found, Opts) of
        Value ->
            case hb_util:safe_int(Value) of
                {ok, Integer} -> {ok, Integer};
                {error, invalid} -> {error, invalid}
            end
    catch
        throw:{necessary_message_not_found, _, _} -> {error, invalid}
    end;
entry_integer(_Key, _Entry, _Opts) ->
    {error, invalid}.

%% @doc Confirm entries fit the bounded internal leaf representation.
storable([]) ->
    ok;
storable([{Hash, WeaveSize, TXRoot} | Rest])
        when is_binary(Hash) andalso byte_size(Hash) == 48 andalso
            is_integer(WeaveSize) andalso WeaveSize >= 0 andalso
            WeaveSize < (1 bsl 64) andalso is_binary(TXRoot) andalso
            (byte_size(TXRoot) == 0 orelse byte_size(TXRoot) == 32) ->
    storable(Rest);
storable(_Parsed) ->
    invalid_entry().

%% @doc Confirm weave sizes form a non-decreasing sequence.
monotonic([], _Last) ->
    ok;
monotonic([{_, WeaveSize, _} | _], Last) when WeaveSize < Last ->
    {error, error_message(<<"non-monotonic-weave-size">>,
        <<"An entry's weave size is below its predecessor's.">>)};
monotonic([{_, WeaveSize, _} | Rest], _Last) ->
    monotonic(Rest, WeaveSize).

%% @doc Encode semantic entries into a compact internal leaf page.
encode_entries(Parsed) ->
    << <<(encode_entry(Entry))/binary>> || Entry <- Parsed >>.

encode_entry({Hash, WeaveSize, TXRoot}) ->
    Padding = (32 - byte_size(TXRoot)) * 8,
    <<Hash:48/binary, WeaveSize:64, (byte_size(TXRoot)):8,
        TXRoot/binary, 0:Padding>>.

%% @doc Decode all entries in a bounded leaf.
decode_entries(Body, Count) ->
    [decode_entry(entry_at(Body, N)) || N <- lists:seq(0, Count - 1)].

decode_entry(<<Hash:48/binary, WeaveSize:64, TXRootSize:8,
        Padded:32/binary>>) when TXRootSize == 0; TXRootSize == 32 ->
    PaddingSize = 32 - TXRootSize,
    <<TXRoot:TXRootSize/binary, Padding:PaddingSize/binary>> = Padded,
    ok = valid_topology(
        Padding == binary:copy(<<0>>, PaddingSize),
        noncanonical_tx_root_padding
    ),
    {Hash, WeaveSize, TXRoot};
decode_entry(_Entry) ->
    topology_error('invalid-leaf-entry').

entry_at(Body, Index) ->
    binary:part(Body, Index * ?ENTRY_SIZE, ?ENTRY_SIZE).

weave_size_at(Body, Index) ->
    <<WeaveSize:64>> = binary:part(Body, (Index * ?ENTRY_SIZE) + 48, 8),
    WeaveSize.

%% @doc Split at most `Count' items from a list in one traversal.
take(Count, List) ->
    take(Count, List, []).
take(0, Rest, Acc) ->
    {lists:reverse(Acc), Rest};
take(_Count, [], Acc) ->
    {lists:reverse(Acc), []};
take(Count, [Item | Rest], Acc) ->
    take(Count - 1, Rest, [Item | Acc]).

%%% Tree state and node accessors. All message fields use `hb_maps' so linked
%%% values preserve AO-Core's lazy-loading semantics.

%% @doc Accept the virgin device seed or one exact persisted state shape.
canonical_state(Base, Opts) ->
    State = hb_private:reset(hb_message:uncommitted(Base)),
    Keys = lists:sort(maps:keys(State)),
    ok = valid_topology(
        hb_maps:get(<<"device">>, Base, not_found, Opts) ==
            <<"arweave-block-index@2.9">>,
        'invalid-state-device'
    ),
    case Keys of
        [<<"device">>] -> ok;
        _ ->
            Allowed = [
                <<"completed">>, <<"device">>, <<"length">>, <<"root">>,
                <<"tail">>
            ],
            ok = valid_topology(
                lists:all(fun(Key) -> lists:member(Key, Allowed) end, Keys),
                'invalid-state-keys'
            ),
            ok = valid_topology(
                lists:all(
                    fun(Key) -> maps:is_key(Key, State) end,
                    [<<"device">>, <<"length">>, <<"root">>]
                ),
                'missing-state-keys'
            ),
            Length = hb_maps:get(<<"length">>, Base, not_found, Opts),
            ok = valid_topology(
                is_integer(Length) andalso Length >= 0 andalso
                    Length < (1 bsl 64),
                'invalid-state-length'
            ),
            _ = state_root(Base, Opts),
            ok
    end.

%% @doc Load and cross-check the public state and its topology links.
current_state(Base, Opts) ->
    canonical_state(Base, Opts),
    Length = entries(Base, Opts),
    Root = state_root(Base, Opts),
    {Completed, Tail} = state_parts(Base, Opts),
    case Length of
        0 ->
            ok = valid_topology(Root == <<>>, 'nonempty-root-for-empty-state'),
            ok = valid_topology(
                Completed == none andalso Tail == none,
                'topology-on-empty-state'
            ),
            {none, none, 0, <<>>, 0};
        _ ->
            Parts = [Part || Part <- [Completed, Tail], Part =/= none],
            ok = valid_topology(Parts =/= [], 'missing-state-topology'),
            ok = valid_topology(
                lists:sum([spec_count(Part) || Part <- Parts]) == Length,
                'invalid-state-length'
            ),
            Final = lists:last(Parts),
            ok = valid_topology(spec_root(Final) == Root, 'invalid-state-root'),
            {Completed, Tail, Length, Root, spec_max(Final)}
    end.

final_part(_Completed, Tail) when Tail =/= none -> Tail;
final_part(Completed, none) when Completed =/= none -> Completed.

parts_root(none, none) ->
    <<>>;
parts_root(Completed, Tail) ->
    spec_root(final_part(Completed, Tail)).

entries(Base, Opts) ->
    hb_util:int(hb_maps:get(<<"length">>, Base, 0, Opts)).

state_root(Base, Opts) ->
    case hb_maps:get(<<"root">>, Base, <<>>, Opts) of
        <<>> -> <<>>;
        Encoded ->
            try canonical_decode(Encoded, <<"root">>)
            catch
                throw:{'invalid-base64', _} ->
                    topology_error('invalid-state-root');
                throw:{'noncanonical-base64', _} ->
                    topology_error('invalid-state-root')
            end
    end.

%% @doc Validate and expose the bounded fields of a leaf.
leaf(Node, Opts) ->
    Body = hb_maps:get(<<"body">>, Node, not_found, Opts),
    Count = node_count(Node, Opts),
    Height = node_int(<<"start-height">>, Node, Opts),
    WeaveSize = node_int(<<"start-weave-size">>, Node, Opts),
    Root = node_decoded(<<"start-root">>, Node, Opts),
    ok = valid_topology(
        is_binary(Body) andalso Count > 0 andalso
            Count =< ?LEAF_SIZE andalso
            byte_size(Body) == Count * ?ENTRY_SIZE,
        'invalid-leaf-body'
    ),
    ok = valid_topology(
        node_max(Node, Opts) == weave_size_at(Body, Count - 1),
        'invalid-leaf-max'
    ),
    {Body, Count, Height, WeaveSize, Root}.

node_type(Node, Opts) ->
    case hb_maps:get(<<"node-type">>, Node, not_found, Opts) of
        Type when Type == <<"leaf">>; Type == <<"branch">> -> Type;
        _ -> throw('invalid-block-index-node')
    end.

%% @doc Return one optional ordinary topology link from the public state.
part_link(Key, Node) ->
    case maps:get(Key, Node, not_found) of
        not_found -> none;
        Link when ?IS_LINK(Link) -> Link;
        _ -> throw({'invalid-block-index-link', Key})
    end.

%% @doc Load a required topology part from the public state.
part_node(Key, Node, Opts) ->
    case part_link(Key, Node) of
        none -> throw({'invalid-block-index-link', Key});
        Link -> load_node(Key, Link, Opts)
    end.

node_count(Node, Opts) ->
    node_int(<<"count">>, Node, Opts).

node_max(Node, Opts) ->
    node_int(<<"max-weave-size">>, Node, Opts).

node_root(Node, Opts) ->
    node_decoded(<<"root">>, Node, Opts).

node_int(Key, Node, Opts) ->
    Value = hb_maps:get(Key, Node, not_found, Opts),
    ok = valid_topology(
        is_integer(Value) andalso Value >= 0 andalso Value < (1 bsl 64),
        {'invalid-node-integer', Key}
    ),
    Value.

node_decoded(Key, Node, Opts) ->
    try canonical_decode(hb_maps:get(Key, Node, not_found, Opts), Key)
    catch
        throw:{'invalid-base64', _} ->
            topology_error({'invalid-node-binary', Key});
        throw:{'noncanonical-base64', _} ->
            topology_error({'invalid-node-binary', Key})
    end.

%% @doc Reject internal node fields outside the deterministic encoding.
canonical_keys(Node, Expected) ->
    ok = valid_topology(
        lists:sort(
            maps:keys(hb_private:reset(hb_message:uncommitted(Node)))
        ) == lists:sort(Expected),
        'invalid-node-keys'
    ).

%% @doc Return the number of children after checking the branch fanout.
branch_child_count(Node, Opts) ->
    Count = node_int(<<"child-count">>, Node, Opts),
    ok = valid_topology(
        Count > 0 andalso Count =< ?BRANCH_SIZE,
        'invalid-branch-fanout'
    ),
    Count.

%% @doc Load one ordinary child link from a branch.
child(Node, N, Opts) ->
    Key = hb_util:bin(N),
    case maps:get(Key, Node, not_found) of
        Link when ?IS_LINK(Link) -> load_node(Key, Link, Opts);
        _ -> throw({'invalid-block-index-link', Key})
    end.

%% @doc Load and validate a branch's compact child-selection table once.
branch_boundaries(Node, ChildCount, Opts) ->
    Boundaries = hb_maps:get(<<"boundaries">>, Node, not_found, Opts),
    ok = valid_topology(
        is_binary(Boundaries) andalso
            byte_size(Boundaries) == ChildCount * 16,
        'invalid-branch-boundaries'
    ),
    Boundaries.

boundary_count(Boundaries, N) ->
    <<Count:64>> = binary:part(Boundaries, (N - 1) * 16, 8),
    Count.

boundary_max(Boundaries, N) ->
    <<Max:64>> = binary:part(Boundaries, ((N - 1) * 16) + 8, 8),
    Max.

%% @doc Describe an unchanged child without loading it.
child_spec(Node, N, Boundaries) ->
    Key = hb_util:bin(N),
    Link = maps:get(Key, Node, not_found),
    ok = valid_topology(?IS_LINK(Link), {'invalid-child-link', Key}),
    {Link, boundary_count(Boundaries, N),
        boundary_max(Boundaries, N), undefined}.

%% @doc Persist an internal topology node without populating the match index.
persisted_spec(Node, Opts) ->
    {ok, ID} = hb_cache:write(Node, Opts#{ <<"match-index">> => false }),
    {ok, Stored} = hb_cache:read(ID, Opts),
    StoredID = hb_message:id(
        Stored,
        none,
        Opts#{ <<"linkify-mode">> => discard }
    ),
    case StoredID == ID of
        true -> ok;
        false -> erlang:error({'cache-content-mismatch', ID, StoredID})
    end,
    spec(to_link(ID), Stored, Opts).

%% @doc Describe a loaded node for use by its parent.
spec(Link, Node, Opts) ->
    {Link, node_count(Node, Opts), node_max(Node, Opts), node_root(Node, Opts)}.

spec_link({Link, _Count, _Max, _Root}) -> Link.
spec_count({_Link, Count, _Max, _Root}) -> Count.
spec_max({_Link, _Count, Max, _Root}) -> Max.
spec_root(none) -> <<>>;
spec_root({_Link, _Count, _Max, Root}) -> Root.

%% @doc Load a canonical AO content link, rejecting scalar store paths.
load_node(Key, Link = {link, ID, LinkOpts}, Opts) when is_map(LinkOpts) ->
    case {
        maps:get(<<"lazy">>, LinkOpts, false),
        maps:get(<<"type">>, LinkOpts, not_found)
    } of
        {true, <<"link">>} ->
            case hb_cache:read(ID, Opts) of
                {ok, Target} when ?IS_ID(Target) ->
                    loaded_node(Key, to_link(Target), Target, Opts);
                _ -> throw({'invalid-block-index-link', Key})
            end;
        {false, <<"link">>} when ?IS_ID(ID) ->
            loaded_node(Key, Link, ID, Opts);
        _ -> throw({'invalid-block-index-link', Key})
    end;
load_node(Key, _Link, _Opts) ->
    throw({'invalid-block-index-link', Key}).

loaded_node(Key, Link, ID, Opts) ->
    case hb_cache:ensure_loaded(Link, Opts) of
        Node when is_map(Node) ->
            case hb_message:id(
                Node,
                none,
                Opts#{ <<"linkify-mode">> => discard }
            ) =/= ID of
                true -> throw({'invalid-block-index-link', Key});
                false -> Node
            end;
        _ -> throw({'invalid-block-index-link', Key})
    end.

to_link(ID) ->
    {link, ID, #{ <<"type">> => <<"link">>, <<"lazy">> => false }}.

%% @doc Construct the complete public state and its ordinary topology links.
state(0, none, none) ->
    #{
        <<"device">> => <<"arweave-block-index@2.9">>,
        <<"length">> => 0,
        <<"root">> => <<>>
    };
state(Length, Completed, Tail) ->
    State0 = #{
        <<"device">> => <<"arweave-block-index@2.9">>,
        <<"length">> => Length,
        <<"root">> => hb_util:encode(parts_root(Completed, Tail))
    },
    State1 = state_part(<<"completed">>, Completed, State0),
    state_part(<<"tail">>, Tail, State1).

state_part(_Key, none, State) -> State;
state_part(Key, Part, State) -> State#{ Key => spec_link(Part) }.

%% @doc Persist and read back a public state before exposing its content link.
persisted_state(State, Opts) ->
    {ok, ID} = hb_cache:write(State, Opts#{ <<"match-index">> => false }),
    {ok, Stored} = hb_cache:read(ID, Opts),
    StoredID = hb_message:id(
        Stored,
        none,
        Opts#{ <<"linkify-mode">> => discard }
    ),
    case StoredID == ID of
        true -> ok;
        false -> erlang:error({'cache-content-mismatch', ID, StoredID})
    end,
    State.

%% @doc Decode one request integer without allowing runtime conversion errors.
request_integer(Key, Req, Default, Opts) when is_map(Req) ->
    try hb_maps:get(Key, Req, Default, Opts) of
        Value -> decode_integer(Value)
    catch
        throw:{necessary_message_not_found, _, _} -> invalid_integer()
    end.

decode_integer(Value) ->
    case hb_util:safe_int(Value) of
        {ok, Integer} -> {ok, Integer};
        _ -> invalid_integer()
    end.

invalid_integer() ->
    {error, error_message(<<"invalid-block-index-integer">>,
        <<"The request integer is malformed.">>)}.

%% @doc Decode the claimed native root without leaking request parser errors.
request_root(Req, Opts) ->
    try hb_maps:get(<<"expected-root">>, Req, not_found, Opts) of
        not_found -> invalid_root();
        Encoded ->
            try canonical_decode(Encoded, <<"expected-root">>) of
                Root -> {ok, Root}
            catch
                throw:{'invalid-base64', _} -> invalid_root();
                throw:{'noncanonical-base64', _} -> invalid_root()
            end
    catch
        throw:{necessary_message_not_found, _, _} -> invalid_root()
    end.

canonical_decode(Encoded, Key) ->
    Decoded = checked_decode(Encoded, Key),
    case Encoded == hb_util:encode(Decoded) of
        true -> Decoded;
        false -> throw({'noncanonical-base64', Key})
    end.

checked_decode(Value, Key) ->
    case hb_util:safe_decode(Value) of
        {ok, Decoded} -> Decoded;
        {error, _} -> throw({'invalid-base64', Key})
    end.

offset_out_of_range() ->
    {error, error_message(<<"offset-out-of-range">>,
        <<"The offset lies beyond the end of the weave.">>)}.

invalid_root() ->
    {error, error_message(<<"invalid-block-index-root">>,
        <<"The index does not hash to the expected root.">>)}.

invalid_entry() ->
    {error, error_message(<<"invalid-block-index-entry">>,
        <<"An entry does not fit the block index's consensus widths.">>)}.

invalid_topology() ->
    {error, error_message(<<"invalid-block-index-topology">>,
        <<"The index's linked topology is not canonical.">>)}.

valid_topology(true, _Reason) -> ok;
valid_topology(false, Reason) -> topology_error(Reason).

topology_error(Reason) ->
    throw({'invalid-block-index-topology', Reason}).

error_message(Message, Detail) ->
    #{
        <<"status">> => 422,
        <<"message">> => Message,
        <<"detail">> => Detail
    }.
