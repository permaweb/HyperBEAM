%%% @doc Implements a multi-layer prefix tree for efficiently storing large
%%% datasets in nested messages.
%%% 
%%% Each element of the tree is available using simply resolving its name,
%%% despite the underlying data structure. Additionally, calling the AO-Core
%%% `set' function will correctly handle putting the values into the correct
%%% locations in the tree, re-generating only the necessary identifiers.
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
get(Trie, Req, Opts) ->
    case hb_maps:get(<<"key">>, Req, undefined, Opts) of
        undefined ->
            % If we don't have a key to search for, return an error.
            {error, not_found};
        Key ->
            % If we have a key to search for, find the longest prefix match
            % amongst the keys in the trie and recurse, until there are no more
            % bytes of the key to match on.
            case longest_match(Key, Trie, Opts) of
                <<>> -> {error, not_found};
                Match ->
                    % Find the child node and the remaining key.
                    Child = hb_maps:get(Match, Trie, Opts),
                    Remaining =
                        binary:part(
                            Key,
                            byte_size(Match),
                            byte_size(Key) - byte_size(Match)
                        ),
                    case Remaining of
                        <<>> ->
                            % If we have no remaining bytes, return the child node.
                            {ok, Child};
                        _ ->
                            % If we have remaining bytes, recurse.
                            get(Remaining, Child, #{}, Opts)
                    end
            end
    end.

%% @doc Find the longest match for a key in a message representing a layer of 
%% the trie.
longest_match(Key, Trie, Opts) ->
    longest_match(<<>>, Key, hb_maps:keys(Trie, Opts), Opts).
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
set(Trie, Req, Opts) ->
    SystemKeys = [<<"set-depth">>, <<"path">>],
    Insertable = hb_maps:without(SystemKeys, Req, Opts),
    MaxKeyLength = case hb_maps:keys(Insertable, Opts) of
        [] -> 1;
        Keys -> lists:max([byte_size(K) || K <- Keys])
    end,
    DefaultDepth = min(?DEFAULT_LAYERS, MaxKeyLength),
    case hb_maps:get(<<"set-depth">>, Req, DefaultDepth, Opts) of
        0 ->
            % Insert the keys and values into this level of the trie without
            % further subdivision. Handle empty keys specially.
            case maps:take(<<>>, Insertable) of
                {Value, #{}} -> Value;
                {_Value, RestInsertable} -> hb_ao:set(Trie, RestInsertable, Opts);
                % ATTENTION NEEDED HERE:
                error -> hb_ao:set(Trie, Insertable, Opts) % Not sure if this is needed
            end;
        SetDepth ->
            % Split keys from the request into groups for each sub-branch of the
            % trie that they should be inserted into. Each group is then inserted
            % in a single recursive call.
            % After all groups are inserted, the new trie has its commitments
            % normalized and is returned.
            NewTrie =
                hb_maps:fold(
                    fun(Subkey, SubReq, Acc) ->
                        ?event({set, {subkey, Subkey}, {subreq, SubReq}}),
                        case hb_maps:find(Subkey, Acc, Opts) of
                            {ok, Subtrie} ->
                                Acc#{
                                    Subkey =>
                                        set(
                                            Subtrie,
                                            SubReq#{
                                                <<"set-depth">> => SetDepth - 1
                                            },
                                            Opts
                                        )
                                };
                            error ->
                                % Create a new empty subtrie for this subkey
                                Acc#{
                                    Subkey =>
                                        set(
                                            #{},
                                            SubReq#{
                                                <<"set-depth">> => SetDepth - 1
                                            },
                                            Opts
                                        )
                                }
                        end
                    end,
                    Trie,
                    group_keys(Trie, Insertable, Opts),
                    Opts
                ),
            normalize_id(NewTrie, Opts)
    end.

%% @doc Update the unsigned ID of a layer of the trie.
normalize_id(Trie, Opts) ->
    WithoutHMac =
        hb_message:without_commitments(
            #{ <<"type">> => <<"unsigned">> },
            Trie,
            Opts
        ),
    {ok, #{ <<"commitments">> := Commitments }} =
        dev_message:commit(
            WithoutHMac,
            #{ <<"type">> => <<"unsigned">> },
            Opts
        ),
    WithoutHMac#{ <<"commitments">> => Commitments }.

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
            hb_maps:keys(Req, Opts)
        ),
    maps:map(
        fun(Subkey, SubKeys) ->
            maps:from_list(
                lists:map(
                    fun(SubReqKey) ->
                        {
                            binary:part(
                                SubReqKey,
                                byte_size(Subkey),
                                byte_size(SubReqKey) - byte_size(Subkey)
                            ),
                            hb_maps:get(SubReqKey, Req, Opts)
                        }
                    end,
                    SubKeys
                )
            )
        end,
        SubReqs
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

large_balance_table_test() ->
    application:ensure_all_started([prometheus, prometheus_cowboy, hb]),
    TotalBalances = 350_000,
    Engine = <<"event">>,
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
        dev_profile:eval(
            fun() ->
                hb_ao:resolve(
                    #{ <<"device">> => <<"trie@1.0">> },
                    Balances#{ <<"path">> => <<"set">> },
                    #{}
                )
            end,
            #{
                <<"engine">> => Engine,
                <<"mode">> => <<"merge">>
            },
            #{}
        ),
    ?event(debug_trie, {created_trie, maps:size(BaseTrie)}),
    UpdateBalanceA = lists:nth(rand:uniform(TotalBalances), maps:keys(Balances)),
    UpdateBalanceB = lists:nth(rand:uniform(TotalBalances), maps:keys(Balances)),
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