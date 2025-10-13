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

info() ->
    #{
        default => fun get/4
    }.

%% @doc Get the value of a key from the trie in a base message. The function
%% calls recursively to find the value, matching the largest prefix of the key
%% as it recurses.
get(Key, Trie, Req, Opts) ->
    get(Trie, Req#{ <<"key">> => Key }, Opts).
get(Link, Req, Opts) when ?IS_LINK(Link) ->
    get(hb_cache:ensure_loaded(Link, Opts), Req, Opts);
get(Node, Req, Opts) ->
    case hb_maps:find(<<"key">>, Req, Opts) of
        error -> {error, <<"`key' parameter is required for trie lookup.">>};
        {ok, <<>>} ->
            % We have reached the end of the key characters. Return the current
            % node.
            {ok, Node};
        {ok, RemainingKey} when not is_map(Node) ->
            % We have more characters to resolve, but the current node is not a
            % message, so we cannot continue.
            ?event(debug_trie,
                {not_found,
                    {node, Node},
                    {remaining_key, RemainingKey}
                }
            ),
            {error, not_found};
        {ok, Key} ->
            % If we have a key to search for, find the longest prefix match
            % amongst the keys in the trie and recurse, until there are no more
            % bytes of the key to match on.
            case longest_match(Key, Node, Opts) of
                <<>> -> {error, not_found};
                Prefix ->
                    % Find the child node and the remaining key.
                    get(
                        remove_prefix(Prefix, Key),
                        hb_maps:get(Prefix, Node, #{}, Opts),
                        #{},
                        Opts
                    )
            end
    end.

%% @doc Find the longest match for a key in a message representing a layer of 
%% the trie.
longest_match(Key, Trie, Opts) ->
    longest_match(<<>>, Key, hb_maps:keys(Trie, Opts) -- [<<"device">>], Opts).
longest_match(Best, _Key, [], _Opts) -> Best;
longest_match(_Best, Key, [Key | _Keys], _Opts) -> Key;
longest_match(Best, Key, [XKey | Keys], Opts) ->
    case binary:longest_common_prefix([XKey, Key]) of
        NewLength when NewLength > byte_size(Best) ->
            longest_match(binary:part(Key, 0, NewLength), Key, Keys, Opts);
        _ ->
            longest_match(Best, Key, Keys, Opts)
    end.

%% @doc Set keys and their values in the trie. The `set-depth' key determines
%% how many layers of the trie the keys should be separated into.
set(Base, Req, Opts) ->
    {ok, do_set(Base, filter_short(Req, Opts), Opts)}.
do_set(Link, Req, Opts) when ?IS_LINK(Link) ->
    do_set(hb_cache:ensure_loaded(Link, Opts), Req, Opts);
do_set(NonTrie, Req, Opts) when not is_map(NonTrie) ->
    % We are attempting to set a value on a non-trie, so we wipe the entire
    % base and replace it with a new trie.
    do_set(#{}, Req, Opts);
do_set(Trie, Req, Opts) ->
    Insertable = hb_maps:without([<<"set-depth">>, <<"path">>], Req, Opts),
    ?event(debug_trie, {set, {trie, Trie}, {inserting, Insertable}}),
    Depth = hb_maps:get(<<"set-depth">>, Req, ?DEFAULT_LAYERS, Opts),
    % If we are setting a terminal value (indicated by the presence of an empty
    % string key or a depth of 0), we simply return it.
    case {hb_maps:find(<<>>, Insertable, Opts), Depth} of
        {{ok, Terminal}, _} -> Terminal;
        {_, 0} -> hb_ao:set(Trie, Insertable, Opts);
        {_, SetDepth} ->
            % Split keys from the request into groups for each sub-branch of the
            % trie that they should be inserted into. Each group is then inserted
            % in a single recursive call.
            % After all groups are inserted, the new trie has its commitments
            % normalized and is returned.
            NewTrie =
                hb_maps:fold(
                    fun(Subkey, SubReq, Acc) ->
                        Acc#{
                            Subkey =>
                                do_set(
                                    hb_maps:get(Subkey, Acc, #{}, Opts),
                                    SubReq#{
                                        <<"set-depth">> => SetDepth - 1
                                    },
                                    Opts
                                )
                        }
                    end,
                    Trie,
                    group_keys(Trie, Insertable, Opts),
                    Opts
                ),
            Linkified =
                hb_message:convert(
                    NewTrie,
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
            hb_message:commit(WithoutHMac, Opts, #{ <<"type">> => <<"unsigned">> })
    end.

%% @doc Filter all keys that are shorter than the default prefix depth.
filter_short(Req, _Opts) ->
    maps:filter(
        fun(Key, _Value) -> byte_size(Key) >= ?DEFAULT_LAYERS end,
        Req
    ).

%% @doc Take a request of keys and values, then return a new map of requests
%% with keys split into sub-requests for each best-matching sub-trie of the base.
%% The keys in each sub-request should be updated to have the group prefix
%% removed.
%% 
%% For example, given the following setup:
%% ```
%% Trie = #{ <<"a">> => Trie2, <<"b">> => Trie3 }
%% Req = #{ <<"a1">> => 1, <<"a2">> => 2, <<"b1">> => 3, <<"b2">> => 4 }
%% ```
%% The function should return:
%% ```
%% #{
%%     <<"a">> => #{ <<"1">> => 1, <<"2">> => 2 },
%%     <<"b">> => #{ <<"1">> => 3, <<"2">> => 4 }
%% }
%% ```
group_keys(Trie, Req, Opts) ->
    SubReqs = 
        maps:groups_from_list(
            fun(ReqKey) ->
                case longest_match(ReqKey, Trie, Opts) of
                    <<>> -> binary:part(ReqKey, 0, 1);
                    BestMatch -> BestMatch
                end
            end,
            hb_maps:keys(Req, Opts) -- [<<>>, <<"set-depth">>]
        ),
    Res = maps:map(
        fun(Subkey, SubKeys) ->
            maps:from_list(
                lists:map(
                    fun(SubReqKey) ->
                        {
                            remove_prefix(Subkey, SubReqKey),
                            hb_maps:get(SubReqKey, Req, Opts)
                        }
                    end,
                    SubKeys
                )
            )
        end,
        SubReqs
    ),
    ?event({grouped_keys, {explicit, Res}}),
    Res.

remove_prefix(Prefix, Key) ->
    binary:part(
        Key,
        byte_size(Prefix),
        byte_size(Key) - byte_size(Prefix)
    ).

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
    ?assert(
        hb_message:match(
            #{ <<"a">> => 1, <<"b">> => 2 },
            hb_ao:set(
                #{ <<"device">> => <<"trie@1.0">>, <<"a">> => 1},
                #{ <<"b">> => 2 },
                #{}
            ),
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
    ?assert(
        hb_message:match(
            #{ <<"a">> => #{ <<"b">> => 2, <<"c">> => 3 } },
            hb_ao:set(
                #{ <<"device">> => <<"trie@1.0">>, <<"a">> => #{ <<"b">> => 2 } },
                #{ <<"ac">> => 3 },
                #{}
            ),
            primary
        )
    ).

set_multiple_test() ->
    ?assert(
        hb_message:match(
            #{
                <<"a">> => #{ <<"b">> => 2, <<"c">> => 3, <<"d">> => 4 },
                <<"b">> => #{ <<"a">> => 5 }
            },
            hb_ao:set(
                #{ <<"device">> => <<"trie@1.0">>, <<"a">> => #{ <<"b">> => 2 } },
                #{ <<"ac">> => 3, <<"ad">> => 4, <<"ba">> => 5 },
                #{}
            ),
            primary
        )
    ).

not_found_test() ->
    hb:init(),
    ?assertEqual(
        not_found,
        hb_ao:get(
            <<"ac">>,
            #{
                <<"device">> => <<"trie@1.0">>,
                <<"a">> => #{ <<"b">> => <<"layer-2">> }
            },
            #{}
        )
    ),
    ?assertEqual(
        not_found,
        hb_ao:get(
            <<"abcde">>,
            #{
                <<"device">> => <<"trie@1.0">>,
                <<"a">> => #{ <<"b">> => <<"layer-2">> }
            },
            #{}
        )
    ).

large_balance_table_test() ->
    TotalBalances = 3_000,
    ?event(debug_trie, {large_balance_table_test, {total_balances, TotalBalances}}),
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
    ?event({created_balances, {keys, maps:size(Balances)}}),
    {ok, BaseTrie} =
        hb_ao:resolve(
            #{ <<"device">> => <<"trie@1.0">> },
            Balances#{ <<"path">> => <<"set">> },
            #{}
        ),
    ?event(debug_trie, {created_trie, maps:size(BaseTrie)}),
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
    ?event(debug_trie, {updated_trie, maps:size(UpdatedTrie)}),
    ?event(debug_trie, {checking_updates, {keys, [UpdateBalanceA, UpdateBalanceB]}}),
    ?assertEqual(
        <<"0">>,
        hb_ao:get(UpdateBalanceA, UpdatedTrie, #{})
    ),
    ?event(debug_trie, {checked_update, UpdateBalanceA}),
    ?assertEqual(
        <<"0">>,
        hb_ao:get(UpdateBalanceB, UpdatedTrie, #{})
    ),
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

%% @doc Test commitment integrity after setting and re-setting keys
commitment_integrity_test() ->
    Wallet = hb:wallet(),
    InitialMsg = hb_message:commit(#{
        <<"device">> => <<"trie@1.0">>,
        <<"key1">> => <<"value1">>,
        <<"key2">> => <<"value2">>
    }, Wallet),
    % Verify initial commitments exist
    InitialCommitted = hb_message:committed(InitialMsg, all, #{}),
    ?assert(length(InitialCommitted) > 0),
    % Update the trie with individual key updates
    UpdatedMsg =
        hb_ao:set(
            InitialMsg,
            #{
                <<"key1">> => <<"updated-value1">>,
                <<"key3">> => <<"new-value3">>
            },
            #{}
        ),
    % Verify commitments are maintained after update
    UpdatedCommitted = hb_message:committed(UpdatedMsg, all, #{}),
    ?assert(length(UpdatedCommitted) > 0),
    ?assertEqual(
        <<"updated-value1">>,
        hb_ao:get(<<"key1">>, UpdatedMsg, #{})
    ),
    ?assertEqual(
        <<"value2">>,
        hb_ao:get(<<"key2">>, UpdatedMsg, #{})
    ),
    ?assertEqual(
        <<"new-value3">>,
        hb_ao:get(<<"key3">>, UpdatedMsg, #{})
    ).

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
    ?assertEqual(
        <<"value-a">>,
        hb_ao:get(<<"a">>, UpdatedTrie, #{})
    ),
    ?assertEqual(
        <<"value-b">>,
        hb_ao:get(<<"b">>, UpdatedTrie, #{})
    ),
    ?assertEqual(
        <<"value-0">>,
        hb_ao:get(<<"0">>, UpdatedTrie, #{})
    ),
    ?assertEqual(
        <<"value-1">>,
        hb_ao:get(<<"1">>, UpdatedTrie, #{})
    ).

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
            #{ <<"level1">> => <<"value1">> },
            #{}
        ),
    
    Step2 =
        hb_ao:set(
            Step1,
            #{ <<"level2">> => <<"value2">> },
            #{}
        ),
    ?assertEqual(<<"value1">>, hb_ao:get(<<"level1">>, Step2, #{})),
    ?assertEqual(<<"value2">>, hb_ao:get(<<"level2">>, Step2, #{})),
    Step3 =
        hb_ao:set(
            Trie,
            #{ <<"level1a">> => <<"value1a">>, <<"level1b">> => <<"value1b">> },
            #{}
        ),
    ?assertEqual(<<"value1a">>, hb_ao:get(<<"level1a">>, Step3, #{})),
    ?assertEqual(<<"value1b">>, hb_ao:get(<<"level1b">>, Step3, #{})).
