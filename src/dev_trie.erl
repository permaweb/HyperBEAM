%%% @doc Implements a radix trie.
%%%
%%% This implementation features an optimization which reduces the total number
%%% of messages required to represent the trie by collapsing leaf nodes into
%%% their parent messages -- i.e., "implicit" leaf nodes. This requires some
%%% special case handling during insertion and retrieval, but it can reduce the
%%% total number of messages by more than half.
%%%
%%% Recall that r = 2 ^ x, so a radix-256 trie compares bits in chunks of 8 and
%%% thus each internal node can have at most 256 children; a radix-2 trie compares
%%% bits in chunks of 1 and thus each internal node can have at most 2 children.
%%% (The number of children are defined by the number of permutations given by an
%%% N-bit chunk comparison -- e.g., a 2-bit comparison yields paths
%%% `{00, 11, 01, 10}`, which is why each node in a radix-4 trie can have at-most
%%% 4 children!)
-module(dev_trie).
-export([info/0, keys/2, set/3, get/3, get/4]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%%% @doc What default radix shall we use for the data structure? Setting this to
%%% a value other than 256 will result in undefined behavior.
%%% Sub-byte chunking for divisors of 8 (radix-2, radix-4, radix-16) seems to work,
%%% but cannot be properly normalized.
-define(RADIX, 256).

info() ->
    #{
        default => fun get/4
     }.

keys(Trie, Opts) ->
    collect_keys(Trie, <<>>, Opts, []).

collect_keys(TrieNode, Prefix, Opts, Acc) ->
    EdgeLabels = edges(TrieNode, Opts),
    IsLeafTerminal = length(EdgeLabels) =:= 0,
    NodeValue = hb_maps:find(<<"node-value">>, TrieNode, Opts),
    IsInteriorTerminal =
        case NodeValue of
            error -> false;
            _ -> true
        end,
    IsTerminal = IsLeafTerminal orelse IsInteriorTerminal,
    NewAcc =
        case IsTerminal of
            true -> [Prefix|Acc];
            false -> Acc
        end,
    lists:foldl(
        fun(ChildEdgeLabel, ChildrenAcc) ->
            NewPrefix = <<Prefix/binary, ChildEdgeLabel/binary>>,
            ChildNode = hb_maps:get(ChildEdgeLabel, TrieNode, undefined, Opts),
            case is_map(ChildNode) of
                true ->
                    collect_keys(ChildNode, NewPrefix, Opts, ChildrenAcc);
                false ->
                    % Implicit leaf node
                    [NewPrefix | ChildrenAcc]
            end
        end,
        NewAcc,
        EdgeLabels
    ).

%% @doc Get the value associated with a key from a trie represented in a base
%% message.
get(Key, Trie, Req, Opts) ->
    get(Trie, Req#{<<"key">> => Key}, Opts).
get(TrieNode, Req, Opts) ->
    case hb_maps:find(<<"key">>, Req, Opts) of
        error -> {error, <<"'key' parameter is required for trie lookup.">>};
        {ok, Key} -> retrieve(TrieNode, Key, Opts)
    end.

%% @doc Set keys and their values in the trie.
set(Trie, Req, Opts) ->
    Insertable = hb_maps:without([<<"path">>], Req, Opts),
    KeyVals = hb_maps:to_list(Insertable, Opts),
    {ok, do_set(Trie, KeyVals, Opts)}.
do_set(Trie, [], Opts) ->
    Uncommitted = hb_message:uncommitted_deep(Trie, Opts),
    CommittedTrie = 
        hb_message:commit(
            Uncommitted,
            Opts,
            #{ <<"type">> => <<"hmac-sha256">> }
        ),
    {ok, _} = hb_cache:write(CommittedTrie, Opts),
    CommittedTrie;
do_set(Trie, [{Key, Val} | KeyVals], Opts) ->
    NewTrie = insert(Trie, Key, Val, Opts),
    do_set(NewTrie, KeyVals, Opts).

insert(TrieNode, Key, Val, Opts) ->
    insert(TrieNode, Key, Val, Opts, 0).
insert(TrieNode, Key, Val, Opts, KeyPrefixSizeAcc) ->
    <<_KeyPrefix:KeyPrefixSizeAcc/bitstring, KeySuffix/bitstring>> = Key,
    EdgeLabels = edges(TrieNode, Opts),
    ChunkSize = round(math:log2(?RADIX)),
    case longest_prefix_match(KeySuffix, EdgeLabels, ChunkSize) of
        % NO MATCH: This internal node has no traversible children, because its
        % edge labels do not match any portion of what remains to be matched of
        % our key. If we've matched the entire length of our key on our way here,
        % then it seems we're trying to insert a key which corresponds to the
        % value kept at this very internal node, so we insert it here.
        % If not, we add an edge to a new leaf node that's labeled with the remaining
        % key suffix, and we insert our value into that leaf node. Note the implicit
        % leaf node! In a world with explicit leaf nodes, it would look like:
        % TrieNode#{KeySuffix => #{<<"node-value">> => Val}}
        {EdgeLabel, MatchSize} when MatchSize =:= 0 ->
            case bit_size(KeySuffix) > 0 of
                true ->
                    % Implicit leaf node creation!
                    TrieNode#{KeySuffix => Val};
                false ->
                    TrieNode#{<<"node-value">> => Val}
            end;
        % FULL MATCH: There is a child of this node with an edge label that
        % completely matches *some portion* of what remains to be matched in our
        % key. If the child is a normal node, this is the straightforward recursive
        % case -- we simply traverse to that child and continue. But if the child
        % is an implicit leaf node, we've reached a base case: if the edge label
        % to the implicit leaf node is exactly the same size as the remaining key
        % suffix, then we've effectively discovered that the key we're trying to
        % insert already exists, and its value is kept in an implicit leaf node,
        % so we simply update it. If the edge label *isn't* the same size, we must
        % transform the implicit leaf node into an internal node which marks the
        % terminal value for its key, and add to it an edge representing the
        % remaining key suffix which maps to a new implicit leaf node.
        {EdgeLabel, MatchSize} when MatchSize =:= bit_size(EdgeLabel) ->
            SubTrie = hb_maps:get(EdgeLabel, TrieNode, undefined, Opts),
            case is_map(SubTrie) of
                false ->
                    if
                        bit_size(KeySuffix) =:= bit_size(EdgeLabel) ->
                            TrieNode#{EdgeLabel => Val};
                        true ->
                            <<
                                _KeySuffixPrefix:MatchSize/bitstring,
                                KeySuffixSuffix/bitstring
                            >> = KeySuffix,
                            TrieNode#{
                                EdgeLabel =>
                                    #{
                                        <<"node-value">> => SubTrie,
                                        KeySuffixSuffix => Val
                                    }
                            }
                    end;
                true ->
                    NewSubTrie =
                        insert(
                            SubTrie,
                            Key,
                            Val,
                            Opts,
                            bit_size(EdgeLabel) + KeyPrefixSizeAcc
                        ),
                    TrieNode#{EdgeLabel => NewSubTrie}
            end;
        % PARTIAL MATCH: There is a child of this node with an edge label that
        % partially matches *some portion* of what remains to be matched in our
        % key. This is the node splitting case. We detach the subtrie rooted at
        % the child, transform its dangling edge label into the common portion of
        % the edge label and what remains to be matched in our key, and reattach
        % the new subtrie under a new child.
        {EdgeLabel, MatchSize} ->
            SubTrie = hb_maps:get(EdgeLabel, TrieNode, undefined, Opts),
            NewTrie = hb_maps:remove(EdgeLabel, TrieNode, Opts),
            <<
                EdgeLabelPrefix:MatchSize/bitstring,
                EdgeLabelSuffix/bitstring
            >> = EdgeLabel,
            <<
                _KeySuffixPrefix:MatchSize/bitstring,
                KeySuffixSuffix/bitstring
            >> = KeySuffix,
            case bit_size(KeySuffixSuffix) > 0 of
                true ->
                    NewTrie#{
                        EdgeLabelPrefix => #{
                            EdgeLabelSuffix => SubTrie,
                            % Implicit leaf node!
                            KeySuffixSuffix => Val
                        }
                    };
                false ->
                    NewTrie#{
                        EdgeLabelPrefix => #{
                            EdgeLabelSuffix => SubTrie,
                            <<"node-value">> => Val
                        }
                    }
            end
    end.

retrieve(TrieNode, Key, Opts) ->
    retrieve(TrieNode, Key, Opts, 0).
retrieve(TrieNode, Key, Opts, KeyPrefixSizeAcc) ->
    case KeyPrefixSizeAcc >= bit_size(Key) of
        true ->
            hb_maps:get(<<"node-value">>, TrieNode, {error, not_found}, Opts);
        false ->
            EdgeLabels = edges(TrieNode, Opts),
            <<_KeyPrefix:KeyPrefixSizeAcc/bitstring, KeySuffix/bitstring>> = Key,
            ChunkSize = round(math:log2(?RADIX)),
            case longest_prefix_match(KeySuffix, EdgeLabels, ChunkSize) of
                {_EdgeLabel, MatchSize} when MatchSize =:= 0 ->
                    {error, not_found};
                {EdgeLabel, MatchSize} when MatchSize =:= bit_size(EdgeLabel) ->
                    SubTrie = hb_maps:get(EdgeLabel, TrieNode, undefined, Opts),
                    % Special case handling for implicit leaf nodes: if the
                    % child node corresponding to the edge label is not a map, and
                    % the edge label is *precisely* the same size as the remaining
                    % key suffix, then SubTrie is an implicit leaf node -- i.e.,
                    % it's the value associated with the key we're searching for.
                    % When the edge label is not the same size as the remaining key
                    % suffix, that indicates a search for a nonexistent key with
                    % a partial prefix match on an implicit leaf node -- i.e.,
                    % if "car" is an implicit leaf node but we searched for "card".
                    case is_map(SubTrie) of
                        false ->
                            if
                                bit_size(KeySuffix) =:= bit_size(EdgeLabel) ->
                                    SubTrie;
                                true ->
                                    {error, not_found}
                            end;
                        true ->
                            retrieve(
                                SubTrie,
                                Key,
                                Opts,
                                bit_size(EdgeLabel) + KeyPrefixSizeAcc
                            )
                    end;
                _ -> {error, not_found}
            end
    end.

%% @doc Get a list of edge labels for a given trie node.
edges(TrieNode, Opts) when not is_map(TrieNode) -> [];
edges(TrieNode, Opts) ->
    Filtered = hb_maps:without(
        [
            <<"node-value">>,
            <<"device">>,
            <<"commitments">>,
            <<"priv">>,
            <<"hashpath">>
        ],
        TrieNode,
        Opts
    ),
    hb_maps:keys(Filtered).

%% @doc Compute the longest common binary prefix of A and B, comparing chunks of
%% N bits.
bitwise_lcp(A, B, N) ->
    bitwise_lcp(A, B, N, 0).
bitwise_lcp(A, B, N, Acc) ->
    case {A, B} of
        {<<ChunkA:N, RestA/bits>>, <<ChunkB:N, RestB/bits>>} when ChunkA =:= ChunkB ->
            bitwise_lcp(RestA, RestB, N, Acc + N);
        _ -> Acc
    end.

%% @doc For a given key and list of edge labels, determine which edge label presents
%% the longest prefix match, comparing chunks of N bits. Returns a 2-tuple of
%% {edge label, commonality in bits}.
longest_prefix_match(Key, EdgeLabels, N) ->
    longest_prefix_match({<<>>, 0}, Key, EdgeLabels, N).
longest_prefix_match(Best, _Key, [], _N) -> Best;
longest_prefix_match({BestLabel, BestSize}, Key, [EdgeLabel | EdgeLabels], N) ->
    case bitwise_lcp(Key, EdgeLabel, N) of
        Size when Size > BestSize ->
            longest_prefix_match({EdgeLabel, Size}, Key, EdgeLabels, N);
        _ ->
            longest_prefix_match({BestLabel, BestSize}, Key, EdgeLabels, N)
    end.

%%% Tests
test_opts() ->
    #{
        store => [hb_test_utils:test_store(hb_store_lmdb)],
        priv_wallet => hb:wallet()
    }.
count_nodes(TrieNode, Opts) when not is_map(TrieNode) -> 0;
count_nodes(TrieNode, Opts) ->
    EdgeLabels = edges(TrieNode, Opts),
    CountsChildren =
        [
            count_nodes(hb_maps:get(EdgeLabel, TrieNode, undefined, Opts), Opts)
        ||
            EdgeLabel <- EdgeLabels
        ],
    1 + lists:sum(CountsChildren).

verify_nodes(TrieNode, Opts) when not is_map(TrieNode) -> true;
verify_nodes(TrieNode, Opts) ->
    ThisNode = hb_message:verify(TrieNode, all, Opts),
    EdgeLabels = edges(TrieNode, Opts),
    ChildResults =
        [
            verify_nodes(hb_maps:get(EdgeLabel, TrieNode, undefined, Opts), Opts)
        ||
            EdgeLabel <- EdgeLabels
        ],
    lists:all(fun(X) -> X =:= true end, [ThisNode] ++ ChildResults).

node_count_forwards_test() ->
    Opts = test_opts(),
    Trie = hb_ao:set(
        #{<<"device">> => <<"trie@1.0">>},
        #{
            <<"car">> => 31337,
            <<"card">> => 90210,
            <<"cardano">> => 666,
            <<"carmex">> => 8675309,
            <<"camshaft">> => 777,
            <<"zebra">> => 0
         },
         Opts
    ),
    ?assertEqual(
        4,
        count_nodes(Trie, Opts)
    ).

node_count_backwards_test() ->
    Opts = test_opts(),
    Trie = hb_ao:set(
        #{<<"device">> => <<"trie@1.0">>},
        #{
            <<"zebra">> => 0,
            <<"camshaft">> => 777,
            <<"carmex">> => 8675309,
            <<"cardano">> => 666,
            <<"card">> => 90210,
            <<"car">> => 31337
         },
         Opts
    ),
    ?assertEqual(
        4,
        count_nodes(Trie, Opts)
    ).

basic_topology_forwards_test() ->
    Opts = test_opts(),
    Trie = hb_ao:set(
        #{<<"device">> => <<"trie@1.0">>},
        #{
            <<"car">> => 31337,
            <<"card">> => 90210,
            <<"cardano">> => 666,
            <<"carmex">> => 8675309,
            <<"camshaft">> => 777,
            <<"zebra">> => 0
         },
         Opts
    ),
    ?assert(
        hb_message:match(
            #{
                <<"zebra">> => 0,
                <<"ca">> => #{
                    <<"mshaft">> => 777,
                    <<"r">> => #{
                        <<"node-value">> => 31337,
                        <<"mex">> => 8675309,
                        <<"d">> => #{
                            <<"node-value">> => 90210,
                            <<"ano">> => 666
                        }
                    }
                }
            },
            Trie,
            primary,
            Opts
       )
    ).

basic_topology_backwards_test() ->
    Opts = test_opts(),
    Trie = hb_ao:set(
        #{<<"device">> => <<"trie@1.0">>},
        #{
            <<"zebra">> => 0,
            <<"camshaft">> => 777,
            <<"carmex">> => 8675309,
            <<"cardano">> => 666,
            <<"card">> => 90210,
            <<"car">> => 31337
         },
         Opts
    ),
    ?assert(
        hb_message:match(
            #{
                <<"zebra">> => 0,
                <<"ca">> => #{
                    <<"mshaft">> => 777,
                    <<"r">> => #{
                        <<"node-value">> => 31337,
                        <<"mex">> => 8675309,
                        <<"d">> => #{
                            <<"node-value">> => 90210,
                            <<"ano">> => 666
                        }
                    }
                }
            },
            Trie,
            primary,
            Opts
       )
    ).

basic_retrievability_test() ->
    Opts = test_opts(),
    Trie = hb_ao:set(
        #{<<"device">> => <<"trie@1.0">>},
        #{
            <<"car">> => 31337,
            <<"card">> => 90210,
            <<"cardano">> => 666,
            <<"carmex">> => 8675309,
            <<"camshaft">> => 777,
            <<"zebra">> => 0
         },
         Opts
    ),
    {ok, Path} = hb_cache:write(Trie, Opts),
    ?event(debug_trie, {basic_retrievability_test, {trie, Trie}}),
    ?assertEqual(31337, hb_ao:get(<<"car">>, Trie, Opts)),
    ?assertEqual(90210, hb_ao:get(<<"card">>, Trie, Opts)),
    ?assertEqual(666, hb_ao:get(<<"cardano">>, Trie, Opts)),
    ?assertEqual(8675309, hb_ao:get(<<"carmex">>, Trie, Opts)),
    ?assertEqual(777, hb_ao:get(<<"camshaft">>, Trie, Opts)),
    ?assertEqual(0, hb_ao:get(<<"zebra">>, Trie, Opts)),
    ?assertEqual(not_found, hb_ao:get(<<"cardd">>, Trie, Opts)),
    ?assertEqual(not_found, hb_ao:get(<<"ca">>, Trie, Opts)),
    ?assertEqual(not_found, hb_ao:get(<<"c">>, Trie, Opts)),
    ?assertEqual(not_found, hb_ao:get(<<"zebraa">>, Trie, Opts)),
    ?assertEqual(not_found, hb_ao:get(<<"z">>, Trie, Opts)),
    ?assertEqual(not_found, hb_ao:get(<<"cardan">>, Trie, Opts)),
    ?assertEqual(not_found, hb_ao:get(<<"cardana">>, Trie, Opts)),
    ?assertEqual(not_found, hb_ao:get(<<"carm">>, Trie, Opts)).

basic_key_collection_test() ->
    Opts = test_opts(),
    Trie = hb_ao:set(
        #{<<"device">> => <<"trie@1.0">>},
        #{
            <<"car">> => 31337,
            <<"card">> => 90210,
            <<"cardano">> => 666,
            <<"carmex">> => 8675309,
            <<"camshaft">> => 777,
            <<"zebra">> => 0
         },
         Opts
    ),
    ?assertEqual(
        [
            <<"zebra">>,
            <<"carmex">>,
            <<"cardano">>,
            <<"card">>,
            <<"car">>,
            <<"camshaft">>
        ],
        keys(Trie, Opts)
    ).

verify_test() ->
    Opts = test_opts(),
    Trie = hb_ao:set(
        #{<<"device">> => <<"trie@1.0">>},
        #{
            <<"car">> => 31337,
            <<"card">> => 90210,
            <<"cardano">> => 666,
            <<"carmex">> => 8675309,
            <<"camshaft">> => 777,
            <<"zebra">> => 0
         },
         Opts
    ),
    ?assert(verify_nodes(Trie, Opts)).

large_balance_table_test() ->
    Opts = test_opts(),
    TotalBalances = 3_000,
    Balances =
        maps:from_list(
            [
                {
                    hb_util:human_id(crypto:strong_rand_bytes(32)),
                    hb_util:bin(rand:uniform(1_000_000_000_000))
                }
            ||
                _ <- lists:seq(1, TotalBalances)
            ]
        ),
    {ok, BaseTrie} =
        hb_ao:resolve(
            #{ <<"device">> => <<"trie@1.0">> },
            Balances#{ <<"path">> => <<"set">> },
            Opts
        ),
    UpdateBalanceA =
        lists:nth(
            rand:uniform(TotalBalances),
            maps:keys(Balances)
        ),
    UpdateBalanceB =
        lists:nth(
            rand:uniform(TotalBalances),
            maps:keys(Balances)
        ),
    UpdatedTrie =
        hb_ao:set(
            BaseTrie,
            #{
                UpdateBalanceA => <<"0">>,
                UpdateBalanceB => <<"0">>
            },
            Opts
        ),
    ?assertEqual(
        <<"0">>,
        hb_ao:get(UpdateBalanceA, UpdatedTrie, Opts)
    ),
    ?event(debug_trie, {checked_update, UpdateBalanceA}),
    ?assertEqual(
        <<"0">>,
        hb_ao:get(UpdateBalanceB, UpdatedTrie, Opts)
    ),
    ?event(debug_trie, {checked_update, UpdateBalanceB}).

insertion_cases_test() ->
    Opts = test_opts(),
    Trie1 = hb_ao:set(
        #{<<"device">> => <<"trie@1.0">>},
        #{<<"toronto">> => 1},
        Opts
    ),
    ?assert(
        hb_message:match(
            #{<<"toronto">> => 1},
            Trie1,
            primary,
            Opts
        )
    ),
    Trie2 = hb_ao:set(
        Trie1,
        #{<<"to">> => 2},
        Opts
    ),
    ?assert(
        hb_message:match(
            #{<<"to">> => #{<<"node-value">> => 2, <<"ronto">> => 1}},
            Trie2,
            primary,
            Opts
        )
    ),
    Trie3 = hb_ao:set(
        Trie2,
        #{<<"apple">> => 3},
        Opts
    ),
    ?assert(
        hb_message:match(
            #{
                <<"apple">> => 3,
                <<"to">> => #{<<"node-value">> => 2, <<"ronto">> => 1}
            },
            Trie3,
            primary,
            Opts
        )
    ),
    Trie4 = hb_ao:set(
        Trie3,
        #{<<"town">> => 4},
        Opts
    ),
    ?assert(
        hb_message:match(
            #{
                <<"apple">> => 3,
                <<"to">> => #{
                    <<"node-value">> => 2,
                    <<"ronto">> => 1,
                    <<"wn">> => 4
                }
            },
            Trie4,
            primary,
            Opts
        )
    ),
    Trie5 = hb_ao:set(
        Trie4,
        #{<<"torrent">> => 5},
        Opts
    ),
    ?assert(
        hb_message:match(
            #{
                <<"apple">> => 3,
                <<"to">> => #{
                    <<"r">> => #{<<"rent">> => 5, <<"onto">> => 1},
                    <<"node-value">> => 2,
                    <<"wn">> => 4
                }
            },
            Trie5,
            primary,
            Opts
        )
    ),
    Trie6 = hb_ao:set(
        Trie5,
        #{<<"tor">> => 6},
        Opts
    ),
    ?assert(
        hb_message:match(
            #{
                <<"apple">> => 3,
                <<"to">> => #{
                    <<"r">> => #{
                        <<"rent">> => 5,
                        <<"onto">> => 1,
                        <<"node-value">> => 6
                    },
                    <<"node-value">> => 2,
                    <<"wn">> => 4
                }
            },
            Trie6,
            primary,
            Opts
        )
    ),
    Trie7 = hb_ao:set(
        Trie6,
        #{<<"a">> => 7},
        Opts
    ),
    ?assert(
        hb_message:match(
             #{
                <<"a">> => #{<<"pple">> => 3, <<"node-value">> => 7},
                <<"to">> => #{
                    <<"r">> => #{
                        <<"rent">> => 5,
                        <<"onto">> => 1,
                        <<"node-value">> => 6
                    },
                    <<"node-value">> => 2,
                    <<"wn">> => 4
                }
            },
            Trie7,
            primary,
            Opts
        )
    ),
    Trie8 = hb_ao:set(
        Trie7,
        #{<<"app">> => 8},
        Opts
    ),
    ?assert(
        hb_message:match(
            #{
                <<"a">> => #{
                    <<"pp">> => #{<<"node-value">> => 8, <<"le">> => 3},
                    <<"node-value">> => 7
                },
                <<"to">> => #{
                    <<"r">> => #{
                        <<"rent">> => 5,
                        <<"onto">> => 1,
                        <<"node-value">> => 6
                    },
                    <<"node-value">> => 2,
                    <<"wn">> => 4
                }
            },
            Trie8,
            primary,
            Opts
        )
    ),
    Trie9 = hb_ao:set(
        Trie8,
        #{<<"t">> => 9},
        Opts
    ),
    ?assert(
        hb_message:match(
            #{
                <<"a">> => #{
                    <<"pp">> => #{<<"node-value">> => 8, <<"le">> => 3},
                    <<"node-value">> => 7
                },
                <<"t">> => #{
                    <<"node-value">> => 9,
                    <<"o">> => #{
                        <<"r">> => #{
                            <<"rent">> => 5,
                            <<"onto">> => 1,
                            <<"node-value">> => 6
                        },
                        <<"node-value">> => 2,
                        <<"wn">> => 4
                    }
                }
            },
            Trie9,
            primary,
            Opts
        )
    ).

% In insertion_cases_test(), we constructed a complex trie, one key at a time.
% Here we compare the resultant topology to the same trie constructed *in bulk*.
forwards_bulk_insertion_test() ->
    Opts = test_opts(),
    Trie = hb_ao:set(
        #{<<"device">> => <<"trie@1.0">>},
        #{
            <<"toronto">> => 1,
            <<"to">> => 2,
            <<"apple">> => 3,
            <<"town">> => 4,
            <<"torrent">> => 5,
            <<"tor">> => 6,
            <<"a">> => 7,
            <<"app">> => 8,
            <<"t">> => 9
        },
        Opts
    ),
    ?assert(
        hb_message:match(
            #{
                <<"a">> => #{
                    <<"pp">> => #{<<"node-value">> => 8, <<"le">> => 3},
                    <<"node-value">> => 7
                },
                <<"t">> => #{
                    <<"node-value">> => 9,
                    <<"o">> => #{
                        <<"r">> => #{
                            <<"rent">> => 5,
                            <<"onto">> => 1,
                            <<"node-value">> => 6
                        },
                        <<"node-value">> => 2,
                        <<"wn">> => 4
                    }
                }
            },
            Trie,
            primary,
            Opts
        )
    ).

% Same as fowards_bulk_insertion_test(), except we bulk load the trie with the
% keys reversed.
backwards_bulk_insertion_test() ->
    Opts = test_opts(),
    Trie = hb_ao:set(
        #{<<"device">> => <<"trie@1.0">>},
        #{
            <<"t">> => 9,
            <<"app">> => 8,
            <<"a">> => 7,
            <<"tor">> => 6,
            <<"torrent">> => 5,
            <<"town">> => 4,
            <<"apple">> => 3,
            <<"to">> => 2,
            <<"toronto">> => 1
        },
        Opts
    ),
    ?assert(
        hb_message:match(
            #{
                <<"a">> => #{
                    <<"pp">> => #{<<"node-value">> => 8, <<"le">> => 3},
                    <<"node-value">> => 7
                },
                <<"t">> => #{
                    <<"node-value">> => 9,
                    <<"o">> => #{
                        <<"r">> => #{
                            <<"rent">> => 5,
                            <<"onto">> => 1,
                            <<"node-value">> => 6
                        },
                        <<"node-value">> => 2,
                        <<"wn">> => 4
                    }
                }
            },
            Trie,
            primary,
            Opts
        )
    ).

bulk_update_cases_test() ->
    Opts = test_opts(),
    Trie = hb_ao:set(
        #{<<"device">> => <<"trie@1.0">>},
        #{
            <<"toronto">> => 1,
            <<"to">> => 2,
            <<"apple">> => 3,
            <<"town">> => 4,
            <<"torrent">> => 5,
            <<"tor">> => 6,
            <<"a">> => 7,
            <<"app">> => 8,
            <<"t">> => 9
        },
        Opts
    ),
    ?assert(
        hb_message:match(
            #{
                <<"a">> => #{
                    <<"pp">> => #{<<"node-value">> => 8, <<"le">> => 3},
                    <<"node-value">> => 7
                },
                <<"t">> => #{
                    <<"node-value">> => 9,
                    <<"o">> => #{
                        <<"r">> => #{
                            <<"rent">> => 5,
                            <<"onto">> => 1,
                            <<"node-value">> => 6
                        },
                        <<"node-value">> => 2,
                        <<"wn">> => 4
                    }
                }
            },
            Trie,
            primary,
            Opts
        )
    ),
    UpdatedTrie = hb_ao:set(
        Trie,
        #{
            <<"toronto">> => 40,
            <<"to">> => 50,
            <<"apple">> => 60,
            <<"town">> => 70,
            <<"torrent">> => 80,
            <<"tor">> => 90,
            <<"a">> => 100,
            <<"app">> => 110,
            <<"t">> => 120
        },
        Opts
    ),
    ?assert(
        hb_message:match(
            #{
                <<"a">> => #{
                    <<"pp">> => #{<<"node-value">> => 110, <<"le">> => 60},
                    <<"node-value">> => 100
                },
                <<"t">> => #{
                    <<"node-value">> => 120,
                    <<"o">> => #{
                        <<"r">> => #{
                            <<"rent">> => 80,
                            <<"onto">> => 40,
                            <<"node-value">> => 90
                        },
                        <<"node-value">> => 50,
                        <<"wn">> => 70
                    }
                }
            },
            UpdatedTrie,
            primary,
            Opts
        )
    ),
    ?assertEqual(40, hb_ao:get(<<"toronto">>, UpdatedTrie, Opts)),
    ?assertEqual(50, hb_ao:get(<<"to">>, UpdatedTrie, Opts)),
    ?assertEqual(60, hb_ao:get(<<"apple">>, UpdatedTrie, Opts)),
    ?assertEqual(70, hb_ao:get(<<"town">>, UpdatedTrie, Opts)),
    ?assertEqual(80, hb_ao:get(<<"torrent">>, UpdatedTrie, Opts)),
    ?assertEqual(90, hb_ao:get(<<"tor">>, UpdatedTrie, Opts)),
    ?assertEqual(100, hb_ao:get(<<"a">>, UpdatedTrie, Opts)),
    ?assertEqual(110, hb_ao:get(<<"app">>, UpdatedTrie, Opts)),
    ?assertEqual(120, hb_ao:get(<<"t">>, UpdatedTrie, Opts)),
    ?assertEqual(not_found, hb_ao:get(<<"ap">>, UpdatedTrie, Opts)),
    ?assertEqual(not_found, hb_ao:get(<<"appple">>, UpdatedTrie, Opts)),
    ?assertEqual(not_found, hb_ao:get(<<"top">>, UpdatedTrie, Opts)),
    ?assertEqual(not_found, hb_ao:get(<<"torontor">>, UpdatedTrie, Opts)),
    ?assertEqual(not_found, hb_ao:get(<<"townn">>, UpdatedTrie, Opts)),
    ?assertEqual(not_found, hb_ao:get(<<"toro">>, UpdatedTrie, Opts)),
    ?assertEqual(not_found, hb_ao:get(<<"appapp">>, UpdatedTrie, Opts)),
    ?assertEqual(not_found, hb_ao:get(<<"tt">>, UpdatedTrie, Opts)).
