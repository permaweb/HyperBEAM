%%% @doc Implements a multi-layer prefix tree for efficiently storing large
%%% datasets in nested messages.
%%% 
%%% Each element of the trie is available using simply resolving its name,
%%% despite the underlying data structure. Additionally, calling the AO-Core
%%% `set' function will correctly handle putting the values into the correct
%%% locations in the tree, re-generating only the necessary identifiers.
%%% 
%%% In this implementation of the `trie' structure, `set'ting a value over a
%%% node in the tree that would otherwise be a prefix branch will fully replace
%%% any existing values at that node. For example:
%%% 
%%% ```
%%%     Trie = #{ aaa => 1, aab => 2, aba => 3 }
%%%     set(Trie, #{ aa => 4 }) => #{ aa => 4, aba => 3 }
%%% ```
%%% 
%%% The depth of the prefix trie can be configured by using the `set-depth' key
%%% in the `set' request.
-module(dev_trie).
-export([info/0, get/3, set/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%%% @doc How many prefix layers should new keys be separated into by default?
-define(DEFAULT_LAYERS, 2).

info() -> #{ default => fun get/4 }.

%% @doc Get the value of a key from the trie in a base message. The function
%% calls recursively to find the value, matching the largest prefix of the key
%% as it recurses.
get(Key, Trie, Req, Opts) ->
    get(Trie, Req#{ <<"key">> => Key }, Opts).
get(Trie, Req, Opts) ->
    case hb_maps:find(<<"key">>, Req, Opts) of
        error -> {error, <<"`key' parameter is required for trie lookup.">>};
        {ok, Key} ->
            % If we have a key to search for, find the longest prefix match
            % amongst the keys in the trie and recurse, until there are no more
            % bytes of the key to match on.
            case longest_match(Key, Trie, Opts) of
                no_match ->
                    {error, not_found};
                Key ->
                    {ok, Val} = hb_maps:find(Key, Trie, Opts),
                    case is_trie(Val, Opts) of
                        false -> {ok, Val};
                        true ->
                            case hb_maps:find(<<"branch-value">>, Val, Opts) of
                                {ok, Leaf} -> {ok, Leaf};
                                error -> {ok, Val}
                            end
                    end;
                Prefix ->
                    % Find the child node and the remaining key.
                    get(
                        remove_prefix(Prefix, Key),
                        hb_maps:get(Prefix, Trie, Opts),
                        #{},
                        Opts
                    )
            end
    end.

%% @doc Find the longest match for a key in a message representing a layer of 
%% the trie.
longest_match(Key, Trie, Opts) ->
    TrieKeys = hb_maps:keys(Trie, Opts) -- [<<"device">>],
    longest_match_optimized(Key, TrieKeys).

%% @doc Optimized prefix matching with early termination and better performance
longest_match_optimized(Key, TrieKeys) ->
    case lists:member(Key, TrieKeys) of
        true -> Key;
        false ->
            SortedKeys = 
                lists:sort(
                    fun(A, B) -> 
                        byte_size(A) >= byte_size(B) 
                    end, 
                    TrieKeys
                ),
            longest_match_sorted(Key, SortedKeys, no_match, 0)
    end.

%% @doc Find longest match with early termination
longest_match_sorted(_Key, [], Best, _BestLen) -> Best;
longest_match_sorted(Key, [XKey | Keys], Best, BestLen) ->
    %% Early termination: if remaining keys are shorter than current best, stop
    case byte_size(XKey) =< BestLen of
        true -> Best;
        false ->
            PrefixLen = binary:longest_common_prefix([XKey, Key]),
            case PrefixLen > BestLen of
                true ->
                    NewBest = binary:part(Key, 0, PrefixLen),
                    longest_match_sorted(Key, Keys, NewBest, PrefixLen);
                false ->
                    longest_match_sorted(Key, Keys, Best, BestLen)
            end
    end.

%% @doc Set keys and their values in the trie. The `set-depth' key determines
%% how many layers of the trie the keys should be separated into.
set(Base, Req, Opts) ->
    {ok, do_set(Base, Req, Opts)}.
do_set(Link, Req, Opts) when ?IS_LINK(Link) ->
    do_set(hb_cache:ensure_loaded(Link, Opts), Req, Opts);
do_set(BaseTrie, Req, Opts) ->
    % Insert the leaf node from the request, if it exists.
    Trie = set_immediate(BaseTrie, Req, Opts),
    % Remove the device keys from the request to yield the downstream request.
    Insertable =
        hb_maps:without(
            [<<"branch-value">>, <<"set-depth">>, <<"path">>],
            hb_private:reset(Req),
            Opts
        ),
    ?event(debug_trie,
        {set,
            {base, BaseTrie},
            {after_setting_immediate, Trie},
            {inserting, Insertable}
        }
    ),
    case hb_maps:get(<<"set-depth">>, Req, ?DEFAULT_LAYERS, Opts) of
        0 ->
            % We are inserting the remainder of the trie key in one go into the
            % present level of the structure. 
            merge(Trie, Insertable, Opts);
        SetDepth ->
            set_deeper_keys(Trie, Insertable, SetDepth, Opts)
    end.

%% @doc If there is an immediate value in the request, set it in the trie.
set_immediate(MaybeTrie, Req, Opts) ->
    case hb_maps:find(<<"branch-value">>, Req, Opts) of
        {ok, Immediate} ->
            case is_trie(MaybeTrie, Opts) of
                true ->
                    ?event(debug_trie,
                        {setting_immediate,
                            {trie, MaybeTrie},
                            {immediate, Immediate}
                        }
                    ),
                    reset_unsigned_id(
                        MaybeTrie#{ <<"branch-value">> => Immediate },
                        Opts
                    );
                false ->
                    ?event(debug_trie,
                        {node_is_immediate_value,
                            {non_trie, MaybeTrie},
                            {immediate, Immediate}
                        }
                    ),
                    Immediate
            end;
        error ->
            MaybeTrie
    end.

%% @doc Merge two nodes together in a trie. If the `base` is not a valid trie,
%% we replace it with one -- nesting the existing value as the leaf node as
%% needed.
merge(new_trie, Req, Opts) ->
    % There is no existing value, so we turn the request into our new trie node.
    reset_unsigned_id(Req#{ <<"device">> => <<"trie@1.0">> }, Opts);
merge(Term, Req, _Opts) when ?IS_EMPTY_MESSAGE(Req) ->
    ?event({ignoring_merge_with_empty_message, {base, Term}, {req, Req}}),
    Term;
merge(Term, Req, Opts) ->
    case is_trie(Term, Opts) of
        false ->
            % The existing value at the position in the trie is a terminal value.
            % We set the existing value as the `branch-value`, with the remaining
            % keys from the request being the non-leaf values.
            ?event({merging_with_non_trie, {req, Req}, {existing, Term}}),
            reset_unsigned_id(
                Req#{
                    <<"device">> => <<"trie@1.0">>,
                    <<"branch-value">> => Term
                },
                Opts
            );
        true ->
            ?event({merging_with_trie, Req}),
            reset_unsigned_id(
                (hb_maps:merge(Term, Req, Opts))#{ <<"device">> => <<"trie@1.0">> },
                Opts
            )
    end.

%% @doc Split keys from the request into groups for each sub-branch of the
%% trie that they should be inserted into. Each group is then inserted
%% in a single recursive call.
%% After all groups are inserted, the new trie has its commitments
%% normalized and is returned.
set_deeper_keys(Existing, Empty, _Depth, _Opts) when ?IS_EMPTY_MESSAGE(Empty) ->
    Existing;
set_deeper_keys(MaybeTrie, Insertable, SetDepth, Opts) ->
    Trie =
        case hb_cache:ensure_loaded(MaybeTrie, Opts) of
            new_trie -> #{ <<"device">> => <<"trie@1.0">> };
            Existing when not is_map(Existing) ->
                #{
                    <<"device">> => <<"trie@1.0">>,
                    <<"branch-value">> => Existing
                };
            T -> T
        end,
    reset_unsigned_id(
        hb_maps:fold(
            fun(Subkey, SubReq, Acc) ->
                Acc#{
                    Subkey =>
                        do_set(
                            hb_maps:get(Subkey, Acc, new_trie, Opts),
                            SubReq#{ <<"set-depth">> => SetDepth - 1 },
                            Opts
                        )
                }
            end,
            Trie,
            group_keys(Trie, Insertable, Opts),
            Opts
        ),
        Opts
    ).

%% @doc Reset the unsigned ID of a message.
reset_unsigned_id(Trie, Opts) when is_map(Trie) ->
    Linkified =
        hb_message:convert(
            Trie,
            <<"structured@1.0">>,
            <<"structured@1.0">>,
            Opts
        ),
    WithoutHMac =
        hb_message:without_commitments(
            #{ <<"type">> => <<"unsigned">> },
            Linkified,
            Opts
        ),
    hb_message:commit(WithoutHMac, Opts, #{ <<"type">> => <<"unsigned">> });
reset_unsigned_id(Trie, _Opts) -> Trie.

%% @doc Determine if a given term is a valid `trie@1.0` message representing a
%% node in the structure.
is_trie(Term, Opts) ->
    case hb_cache:ensure_loaded(Term, Opts) of
        Msg when is_map(Msg) ->
            hb_maps:get(<<"device">>, Msg, not_found, Opts) == <<"trie@1.0">>;
        _ ->
            false
    end.

%% @doc Take a request of keys and values, then return a new map of requests
%% with keys split into sub-requests for each best-matching sub-trie of the base.
%% The keys in each sub-request should be updated to have the group prefix
%% removed.
%% 
%% For example, given the following setup:
%% ```
%% Trie = #{ <<"a">> => Trie2, <<"b">> => Trie3 }
%% Req = #{ <<"a1w">> => 1, <<"a2x">> => 2, <<"b1y">> => 3, <<"b2z">> => 4 }
%% ```
%% The function should return:
%% ```
%% #{
%%     <<"a">> => #{ <<"1w">> => 1, <<"2x">> => 2 },
%%     <<"b">> => #{ <<"1y">> => 3, <<"2z">> => 4 }
%% }
%% ```
group_keys(Trie, Req, Opts) ->
    SubReqs = 
        maps:groups_from_list(
            fun(ReqKey) ->
                case longest_match(ReqKey, Trie, Opts) of
                    no_match -> binary:part(ReqKey, 0, 1);
                    BestMatch -> BestMatch
                end
            end,
            hb_maps:keys(Req, Opts) -- [<<>>, <<"set-depth">>]
        ),
    maps:map(
        fun(Subkey, SubKeys) ->
            maps:from_list(
                lists:map(
                    fun(SubReqKey) ->
                        {
                            case remove_prefix(Subkey, SubReqKey) of
                                <<>> -> <<"branch-value">>;
                                RemainingKey -> RemainingKey
                            end,
                            hb_util:ok(hb_maps:find(SubReqKey, Req, Opts))
                        }
                    end,
                    SubKeys
                )
            )
        end,
        SubReqs
    ).

%% @doc Remove the matching component of a string (found with `longest_match')
%% from the beginning of a key.
remove_prefix(no_match, Key) -> Key;
remove_prefix(Prefix, Key) ->
    try
        binary:part(
            Key,
            byte_size(Prefix),
            byte_size(Key) - byte_size(Prefix)
        )
    catch
        error:badarg ->
            ?event(error,
                {could_not_remove_trie_prefix,
                    {prefix, Prefix},
                    {key, Key}
                }
            ),
            throw({could_not_remove_prefix, Prefix, Key})
    end.

%% @doc Verify all commitments inside all layers of a trie. Used in the testing
%% functions of this device.
verify_all(Base, Opts) ->
    case hb_cache:ensure_loaded(Base, Opts) of
        Loaded when is_map(Loaded) ->
            hb_message:verify(Loaded, all, Opts) andalso
                lists:all(
                    fun(V) ->
                        verify_all(hb_cache:ensure_loaded(V, Opts), Opts)
                    end,
                    hb_maps:values(
                        hb_private:reset(
                            hb_message:uncommitted(Loaded, Opts)
                        ),
                        Opts
                    )
                );
        _ ->
            true
    end.

%%% Tests

immediate_get_test() ->
    ?assertEqual(
        1,
        hb_ao:get(
            <<"abc">>,
            #{
                <<"device">> => <<"trie@1.0">>,
                <<"abc">> => 1
            },
            #{}
        )
    ).

immediate_set_test() ->
    Set =
        hb_ao:set(
            #{ <<"device">> => <<"trie@1.0">>, <<"a">> => 1},
            #{ <<"b">> => 2 },
            #{}
        ),
    ?assert(verify_all(Set, #{})),
    ?assert(
        hb_message:match(
            #{ <<"a">> => 1, <<"b">> => 2 },
            Set,
            primary
        )
    ).

second_layer_get_test() ->
    ?assertEqual(
        <<"layer-2">>,
        hb_ao:get(
            <<"ab">>,
            #{
                <<"device">> => <<"trie@1.0">>,
                <<"a">> => #{ <<"b">> => <<"layer-2">> }
            },
            #{}
        )
    ).

second_layer_set_test() ->
    Set =
        hb_ao:set(
            #{ <<"device">> => <<"trie@1.0">>, <<"a">> => #{ <<"b">> => 2 } },
            #{ <<"ac">> => 3 },
            #{}
        ),
    ?assert(verify_all(Set, #{})),
    ?assert(
        hb_message:match(
            #{ <<"a">> => #{ <<"b">> => 2, <<"c">> => 3 } },
            Set,
            primary
        )
    ).

set_multiple_test() ->
    Set =
        hb_ao:set(
            #{ <<"device">> => <<"trie@1.0">>, <<"a">> => #{ <<"b">> => 2 } },
            #{ <<"ac">> => 3, <<"ad">> => 4, <<"ba">> => 5 },
            #{}
        ),
    ?assert(verify_all(Set, #{})),
    ?assert(
        hb_message:match(
            #{
                <<"a">> => #{ <<"b">> => 2, <<"c">> => 3, <<"d">> => 4 },
                <<"b">> => #{ <<"a">> => 5 }
            },
            Set,
            primary
        )
    ).

large_balance_table_test() ->
    TotalBalances = 3_500,
    ?event(debug_test, {large_balance_table_test, {total_balances, TotalBalances}}),
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
    ?event(debug_test, {created_balances, {keys, (Balances)}}),
    {ok, BaseTrie} =
        hb_ao:resolve(
            #{ <<"device">> => <<"trie@1.0">> },
            Balances#{ <<"path">> => <<"set">> },
            #{}
        ),
    %?assert(verify_all(BaseTrie, #{})),
    ?event(debug_test, {created_trie, (BaseTrie)}),
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
            #{}
        ),
    ?event(debug_test, {updated_trie, (UpdatedTrie)}),
    ?event(debug_test, {checking_updates, {keys, [UpdateBalanceA, UpdateBalanceB]}}),
    ?assertEqual(<<"0">>, hb_ao:get(UpdateBalanceA, UpdatedTrie, #{})),
    ?event(debug_test, {checked_update, UpdateBalanceA}),
    ?assertEqual(<<"0">>, hb_ao:get(UpdateBalanceB, UpdatedTrie, #{})),
    ?event(debug_trie, {checked_update, UpdateBalanceB}).

%% @doc Test robust updating of existing terminal values plus adding new ones
update_existing_values_test() ->
    InitialTrie = #{
        <<"device">> => <<"trie@1.0">>,
        <<"alice">> => <<"100">>,
        <<"bob">> => <<"200">>,
        <<"charlie">> => <<"300">>
    },
    UpdatedTrie =
        hb_ao:set(
            InitialTrie,
            #{
                <<"alice">> => <<"150">>,    % Update existing terminal value
                <<"bob">> => <<"250">>,      % Update another existing value  
                <<"diana">> => <<"400">>,    % Add completely new value
                <<"eve">> => <<"500">>,      % Add another new value
                <<"frank">> => <<"600">>     % Add third new value
            },
            #{}
        ),
    ?assert(verify_all(UpdatedTrie, #{})),
    ?assertEqual(<<"150">>, hb_ao:get(<<"alice">>, UpdatedTrie, #{})),
    ?assertEqual(<<"250">>, hb_ao:get(<<"bob">>, UpdatedTrie, #{})),
    ?assertEqual(<<"400">>, hb_ao:get(<<"diana">>, UpdatedTrie, #{})),
    ?assertEqual(<<"500">>, hb_ao:get(<<"eve">>, UpdatedTrie, #{})),
    ?assertEqual(<<"600">>, hb_ao:get(<<"frank">>, UpdatedTrie, #{})),    
    % Verify charlie still exists (wasn't touched in this update)
    ?assertEqual(<<"300">>, hb_ao:get(<<"charlie">>, UpdatedTrie, #{})),
    % Test another round of updates to ensure robustness
    FinalTrie =
        hb_ao:set(
            UpdatedTrie,
            #{
                <<"alice">> => <<"175">>,    % Update alice again
                <<"frank">> => <<"600">>     % Add yet another new value
            },
            #{}
        ),
    % Verify the second round of updates
    AliceResult = hb_ao:get(<<"alice">>, FinalTrie, #{}),
    FrankResult = hb_ao:get(<<"frank">>, FinalTrie, #{}),
    ?event(debug_trie, {alice_retrieval, AliceResult}),
    ?event(debug_trie, {frank_retrieval, FrankResult}),
    ?assertEqual(<<"175">>, AliceResult),
    ?assertEqual(<<"600">>, FrankResult),
    % Ensure all other values are still intact
    ?assertEqual(<<"250">>, hb_ao:get(<<"bob">>, FinalTrie, #{})),
    ?assertEqual(<<"300">>, hb_ao:get(<<"charlie">>, FinalTrie, #{})),
    ?assertEqual(<<"400">>, hb_ao:get(<<"diana">>, FinalTrie, #{})),
    ?assertEqual(<<"500">>, hb_ao:get(<<"eve">>, FinalTrie, #{})).

%% @doc Test keys shorter than the default prefix depth
short_keys_test() ->
    % Test single-byte keys (shorter than DEFAULT_LAYERS = 2)
    ShortKeyTrie = #{
        <<"device">> => <<"trie@1.0">>
    },
    % Insert single-byte keys
    UpdatedTrie =
        hb_ao:set(
            ShortKeyTrie,
            #{
                <<"a">> => <<"value-a">>,
                <<"b">> => <<"value-b">>,
                <<"0">> => <<"value-0">>,
                <<"1">> => <<"value-1">>
            },
            #{}
        ),
    % Verify all short keys can be retrieved
    ?assertEqual(<<"value-a">>, hb_ao:get(<<"a">>, UpdatedTrie, #{})),
    ?assertEqual(<<"value-b">>, hb_ao:get(<<"b">>, UpdatedTrie, #{})),
    ?assertEqual(<<"value-0">>, hb_ao:get(<<"0">>, UpdatedTrie, #{})),
    ?assertEqual(<<"value-1">>, hb_ao:get(<<"1">>, UpdatedTrie, #{})).

%% @doc Test that mixed key lengths work with trie depth calculation
mixed_key_lengths_test() ->
    Trie0 =
        hb_ao:set(
            #{
                <<"device">> => <<"trie@1.0">>
            },
            #{
                <<"x">> => <<"single">>,
                <<"yz">> => <<"double">>,
                <<"abc">> => <<"triple">>
            },
            #{}
        ),
    ?assertEqual(<<"single">>, hb_ao:get(<<"x">>, Trie0, #{})),
    ?assertEqual(<<"double">>, hb_ao:get(<<"yz">>, Trie0, #{})),
    ?assertEqual(<<"triple">>, hb_ao:get(<<"abc">>, Trie0, #{})),
    % Update a short key value.
    Trie1 =
        hb_ao:set(
            Trie0,
            #{
                <<"x">> => <<"updated-single">>,
                <<"ab">> => <<"overwritten-trie">>
            },
            #{}
        ),
    ?assert(verify_all(Trie1, #{})),
    ?assertEqual(<<"updated-single">>, hb_ao:get(<<"x">>, Trie1, #{})),
    ?assertEqual(<<"overwritten-trie">>, hb_ao:get(<<"ab">>, Trie1, #{})).

%% @doc Test trie behavior with custom set-depth
custom_depth_test() ->
    Trie = #{
        <<"device">> => <<"trie@1.0">>
    },
    UpdatedTrie =
        hb_ao:set(
            Trie,
            #{
                <<"set-depth">> => 1,
                <<"very-long-key-1">> => <<"value1">>,
                <<"very-long-key-2">> => <<"value2">>,
                <<"different-prefix">> => <<"value3">>
            },
            #{}
        ),
    ?assert(verify_all(UpdatedTrie, #{})),
    ?assertEqual(
        <<"value1">>, 
        hb_ao:get(<<"very-long-key-1">>, UpdatedTrie, #{})
    ),
    ?assertEqual(
        <<"value2">>, 
        hb_ao:get(<<"very-long-key-2">>, UpdatedTrie, #{})
    ),
    ?assertEqual(
        <<"value3">>, 
        hb_ao:get(<<"different-prefix">>, UpdatedTrie, #{})
    ).

%% @doc Test error conditions and boundary cases  
error_conditions_test() ->
    Trie = #{
        <<"device">> => <<"trie@1.0">>
    },
    ?assertEqual(
        not_found,
        hb_ao:get(<<"nonexistent">>, Trie, #{})
    ),
    ZeroDepthTrie =
        hb_ao:set(
            Trie,
            #{
                <<"set-depth">> => 0,
                <<"key1">> => <<"value1">>,
                <<"key2">> => <<"value2">>
            },
            #{}
        ),
    ?assertEqual(<<"value1">>, hb_ao:get(<<"key1">>, ZeroDepthTrie, #{})),
    ?assertEqual(<<"value2">>, hb_ao:get(<<"key2">>, ZeroDepthTrie, #{})). 

%% @doc Test the critical single-byte key case (like "0" in AO token)
single_byte_key_test() ->
    Trie = #{
        <<"device">> => <<"trie@1.0">>
    },
    UpdatedTrie =
        hb_ao:set(
            Trie,
            #{
                <<"0">> => <<"zero-balance">>,
                <<"1">> => <<"one-balance">>,
                <<"abc">> => <<"normal-key">>
            },
            #{}
        ),
    ?assert(verify_all(UpdatedTrie, #{})),
    ?assertEqual(<<"zero-balance">>, hb_ao:get(<<"0">>, UpdatedTrie, #{})),
    ?assertEqual(<<"one-balance">>, hb_ao:get(<<"1">>, UpdatedTrie, #{})),
    ?assertEqual(<<"normal-key">>, hb_ao:get(<<"abc">>, UpdatedTrie, #{})).

%% @doc Test trie structural integrity with deeper nesting
deep_nesting_test() ->
    Trie = #{
        <<"device">> => <<"trie@1.0">>
    },
    Step1 =
        hb_ao:set(
            Trie,
            #{ <<"l1">> => <<"immediate1">>, <<"l2">> => <<"immediate2">> },
            #{}
        ),
    ?assert(verify_all(Step1, #{})),
    ?event(debug_test, {step1, Step1}),
    ?assertEqual(<<"immediate1">>, hb_ao:get(<<"l1">>, Step1, #{})),
    ?assertEqual(<<"immediate2">>, hb_ao:get(<<"l2">>, Step1, #{})),
    Step2 =
        hb_ao:set(
            Step1,
            #{ <<"l1a">> => <<"value1a">>, <<"l2b">> => <<"value2b">> },
            #{}
        ),
    ?assert(verify_all(Step2, #{})),
    ?event(debug_test, {step2, Step2}),
    ?assertEqual(<<"value1a">>, hb_ao:get(<<"l1a">>, Step2, #{})),
    ?assertEqual(<<"value2b">>, hb_ao:get(<<"l2b">>, Step2, #{})),
    ?assertEqual(<<"immediate1">>, hb_ao:get(<<"l1">>, Step2, #{})),
    ?assertEqual(<<"immediate2">>, hb_ao:get(<<"l2">>, Step2, #{})).