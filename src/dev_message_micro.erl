-module(dev_message_micro).
-export([commit/3, set/3, do_deep_merge/3]).
-export([verify/3, commitments/3]).
-export([keys/3, flatten/3]).
-export([committed/3]).
-export([match/2, match/3, match/4]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEVICE_KEYS, [
    <<"id">>,
    <<"commitments">>,
    <<"committers">>,
    <<"keys">>,
    <<"path">>,
    <<"set">>,
    <<"remove">>,
    <<"verify">>
]).
to(Base, Req, Opts) ->
    CodecDevice =
        maps:get(
            <<"codec-device">>,
            Req,
            hb_opts:get(
                codec_device,
                no_viable_codec_device,
                Opts
            )
        ),
    hb_ao_micro:resolve(
        #{ <<"device">> => CodecDevice, <<"path">> => <<"to">>, <<"...">> => Base },
        Req,
        Opts#{ cache_control => [<<"no-store">>] }
    ).
from(Base, Req, Opts) ->
    CodecDevice =
        maps:get(
            <<"codec-device">>,
            Req,
            hb_opts:get(
                codec_device,
                no_viable_codec_device,
                Opts
            )
        ),
    hb_ao_micro:resolve(
        #{ <<"device">> => CodecDevice, <<"path">> => <<"from">>, <<"...">> => Base },
        Req,
        Opts#{ cache_control => [<<"no-store">>] }
    ).
-spec commit(any(), any(), any()) -> {ok, map()}.
commit(Base, Req, Opts) ->
    ?event(debug_test, {commit, {base, Base}, {req, {explicit, Req}}}, Opts),
    CommitmentDevice =
        maps:get(
            <<"commitment-device">>,
            Req,
            hb_opts:get(
                commitment_device,
                no_viable_commitment_device,
                Opts
            )
        ),
    hb_ao_micro:resolve(
        #{ <<"device">> => CommitmentDevice, <<"...">> => Base },
        Req,
        Opts#{ cache_control => [<<"no-store">>] }
    ).

commitments(Base, Req, Opts) ->
    ?event(debug_test, {commitments, {base, Base}, {explicit_base, {explicit, Base}}, {req, Req}}, Opts),
    % TODO: Get base ID with extra call to 'write'
    {ok, Path} = hb_cache_micro:write(Base, Opts),
    ?event(debug_test, {commitments, {path, Path}}),
    {ok, Matches} =
        hb_cache_micro:match(
            #{ <<"...">> => Path },
            Opts#{ micro_cache => true }
        ),
    ?event(debug_test, {commitments, {matches, Matches}}),
    {ok, Matches}.
%% @doc Deep merge keys in a message. Takes a map of key-value pairs and sets
%% them in the message, overwriting any existing values.
set(Base, NewValuesMsg, Opts) ->
    OriginalPriv = hb_private:from_message(Base),
    % Filter mode and `undefined` (ignored) keys from the message to be set.
    NewValues =
        hb_maps:filter(
            fun(Key, Value) ->
                (Value =/= undefined) andalso
                    not lists:member(Key, ?DEVICE_KEYS ++ [<<"set-mode">>])
            end,
            NewValuesMsg,
            Opts
        ),
    % Combine with deep merge or if `set-mode` is `explicit' then just merge
    % replacing each key directly.
    AfterMerge =
        case maps:get(<<"set-mode">>, NewValuesMsg, <<"deep">>) of
            <<"explicit">> ->
                Merged = NewValues#{ <<"...">> => Base },
                ?event(
                    debug_test,
                    {explicitly_merging, {base, Base}, {new_values, NewValues}}
                ),
                Merged;
            <<"deep">> ->
                ?event(debug_test,
                    {doing_deep_merge,
                        {base, Base},
                        {new_values, NewValues}
                    }
                ),
                do_deep_merge(Base, NewValues, Opts)
        end,
    {ok, Normalized} = commit(AfterMerge, #{ <<"type">> => <<"unsigned">> }, Opts),
    {ok, hb_private:set_priv(Normalized, OriginalPriv)}.


%% @doc Deep merge keys in a message, utilizing the set device of any child
%% keys that are themselves messages.
do_deep_merge(Base, Req, Opts) ->
    WithDeeplyMerged =
        maps:map(
            fun(Key, NewDeepMsg)
                    when ?IS_MESSAGE(NewDeepMsg) andalso
                    ?IS_MESSAGE(map_get(Key, Base)) ->
                OldDeepMsg = map_get(Key, Base),
                ?event(
                    debug_test,
                    {deeply_merging,
                        {key, Key},
                        {old_deep_msg, OldDeepMsg},
                        {new_deep_msg, NewDeepMsg}
                    },
                    Opts
                ),
                AfterMerge = hb_ao:set(OldDeepMsg, NewDeepMsg, Opts),
                ?event(debug_test, {after_merge, AfterMerge}, Opts),
                AfterMerge;
            (_, V) -> V
            end,
            Req
        ),
    WithDeeplyMerged#{ <<"...">> => Base }.

-spec verify(#{ commitment_device => binary() }, any(), any()) -> {ok, map()}.
verify(Base, Req, Opts) ->
    CommitmentDevice =
        maps:get(
            <<"commitment-device">>,
            Base,
            hb_opts:get(
                commitment_device,
                no_viable_commitment_device,
                Opts
            )
        ),
    hb_ao_micro:resolve(
        #{ <<"device">> => CommitmentDevice, <<"...">> => Base },
        Req,
        Opts#{ cache_control => [<<"no-store">>] }
    ).

committed(Base, Req, Opts) ->
    {ok, ReqType} = 
        case hb_ao_micro:resolve(Req, <<"type">>, Opts) of
            {ok, Type} ->
                {ok, Type};
            {error, not_found} ->
                {ok, <<"normal">>}
        end,
    {ok, Committed} = 
        case ReqType of 
            <<"extended">> ->
                case hb_ao_micro:resolve(Base, <<"...">>, Opts) of
                    {ok, Outer} ->
                        OuterCommitted =
                            remove_extension(
                                hb_util:ok(
                                    hb_ao_micro:resolve(
                                        Outer,
                                        #{ 
                                            <<"path">> => <<"committed">>,
                                            <<"type">> => <<"extended">>
                                        },
                                        Opts
                                    )
                                )
                            ),
                        InnerCommitted = 
                            remove_extension(
                                hb_util:ok(
                                    hb_ao_micro:resolve(
                                        Base,
                                        #{ <<"path">> => <<"committed">> },
                                        Opts
                                    )
                                )
                            ),
                        {ok, lists:sort(OuterCommitted ++ InnerCommitted)};
                    {error, not_found} ->
                        hb_ao_micro:resolve(Base, <<"committed">>, Opts)
                end;
            <<"signed">> ->
                case maps:get(<<"committer">>, Base, not_found) of
                    not_found ->
                        case hb_ao_micro:resolve(Base, <<"...">>, Opts) of
                            {ok, Outer} ->
                                hb_ao_micro:resolve(
                                    Outer, 
                                    #{ 
                                        <<"path">> => <<"committed">>,
                                        <<"type">> => <<"signed">>
                                    },
                                    Opts
                                );
                            {error, not_found} ->
                                {ok, []}
                        end;
                    _Committer ->
                        hb_ao_micro:resolve(Base, <<"committed">>, Opts)
                end;
            <<"signed-extended">> ->
                case maps:get(<<"committer">>, Base, not_found) of
                    not_found ->
                        case hb_ao_micro:resolve(Base, <<"...">>, Opts) of
                            {ok, Outer} ->
                                hb_ao_micro:resolve(
                                    Outer,
                                    #{
                                        <<"path">> => <<"committed">>,
                                        <<"type">> => <<"signed-extended">>
                                    },
                                    Opts
                                );
                            {error, not_found} ->
                                {ok, []}
                        end;
                    _Committer ->
                        hb_ao_micro:resolve(
                            Base,
                            #{ 
                                <<"path">> => <<"committed">>,
                                <<"type">> => <<"extended">>
                            },
                            Opts
                        )
                end;
            _ -> hb_ao_micro:resolve(Base, <<"committed">>, Opts)
        end,
    committed_to_list(Committed).

committed_to_list(Committed) ->
    case Committed of 
        CommList when is_list(CommList) ->
            {ok, CommList};
        <<"l:", CommBin/binary>> ->
            {ok, binary:split(CommBin, <<",">>, [global])};
        _ ->
            {error, not_found}
    end.
remove_extension(Committed) ->
    lists:delete(<<"...">>, Committed).

flatten(Base, Req, Opts) -> 
    ?event({flattening, {base, Base}, {req, Req}}),
    case hb_ao_micro:resolve(Base, <<"...">>, Opts) of
        {ok, Outer} ->
            OuterWithout = 
                maps:without(
                    [<<"id">>, <<"committed">>],
                    Outer
                ),
            ?event({flattening, {outer, OuterWithout}}),
            BaseWithout = 
                maps:without(
                    [<<"...">>, <<"id">>, <<"committed">>],
                    Base
                ),
            ?event({flattening, {base_without, BaseWithout}}),
            Merged = maps:merge(OuterWithout, BaseWithout),
            ?event({flattening, {merged, {explicit, Merged}}}),
            hb_ao_micro:resolve(Merged, <<"flatten">>, Opts);
        {error, not_found} ->
            {ok, Base}
    end.
keys(Base, Req, Opts) ->
    ?event(debug_test, {keys, {base, Base}}),
    {ok, Flattened} = hb_ao_micro:resolve(Base, <<"flatten">>, Opts),
    ?event(debug_test, {keys, {flattened, Flattened}}),
    Keys = maps:keys(Flattened),
    ?event(debug_test, {keys, {keys, Keys}}),
    {ok, Keys}.

match(Map1, Map2) ->
    match(Map1, Map2, strict).
match(Map1, Map2, Mode) ->
    match(Map1, Map2, Mode, #{}).
match(Map1, Map2, Mode, Opts) ->
    try unsafe_match(Map1, Map2, Mode, [], Opts)
    catch
        throw:{mismatch, Type, Path, Val1, Val2} ->
            {mismatch, Type, Path, Val1, Val2};
        _:Details:St -> {error, {Details, {trace, St}}}
    end.

unsafe_match(RawMap1, RawMap2, Mode, Path, Opts) ->
    Map1 = hb_cache_micro:with_only_committed(RawMap1, Opts),
    Map2 = hb_cache_micro:with_only_committed(RawMap2, Opts),
    ?event(unsafe_match, {unsafe_match_maps, {map1, Map1}, {map2, Map2}}),
    Keys1 = maps:keys(Map1),
    Keys2 = maps:keys(Map2),
    ?event(unsafe_match, {unsafe_match_key, {keys1, Keys1}, {keys2, Keys2}}),
    case Keys1 == Keys2 of
        true ->
            lists:all(
                fun(Key) ->
                    {ok, Val1} = hb_ao_micro:resolve(Map1, Key, Opts),
                    {ok, Val2} = hb_ao_micro:resolve(Map2, Key, Opts),
                    ?event(
                        unsafe_match,
                        {matching_key, {key, Key}, {val1, Val1}, {val2, Val2}}
                    ),
                    case Val1 == Val2 of
                        false -> throw({mismatch, value, Key, Val1, Val2});
                        true -> true
                    end
                end,
                Keys1
            );
        false ->
            throw({mismatch, keys, Keys1, Keys2})
    end.

%% Tests
test_opts() ->
    application:ensure_all_started(hb),
    #{
        store => 
            [
                hb_test_utils:test_store(hb_store_lmdb), 
                hb_test_utils:test_store(hb_store_preloaded)
            ],
        priv_wallet => hb:wallet()
    }.
commit_signed_test() -> 
    Opts = test_opts(),
    ?event({test_opts, Opts}),
    Item = #{ <<"a">> => <<"1">>, <<"b">> => <<"2">> },
    {ok, Path} = hb_cache_micro:write(Item, Opts),
    {ok, CommittedItem} = 
        hb_ao_micro:resolve(
            Item,
            #{
                <<"commitment-device">> => <<"httpsig@1.0">>,
                <<"device">> => <<"message@1.0">>,
                <<"type">> => <<"signed">>,
                <<"path">> => <<"commit">>
            },
            Opts
        ),
    ?event(new_commit_test, {committed_item, CommittedItem}),
    ?assertEqual(
        {ok, <<"1">>},
        hb_ao_micro:resolve(
            [CommittedItem, <<"...">>, <<"a">>],
            Opts
        )
    ),
    ?assertEqual(
        {ok, <<"2">>},
        hb_ao_micro:resolve(
            [CommittedItem, <<"...">>, <<"b">>],
            Opts
        )
    ),
    % TODO: Id of the committed item is the SignedID - what should we compare it
    % to? We currently are not writing it to the cache.
    % ?assertEqual(
    %     hb_ao_micro:get(
    %         <<"id">>,
    %         CommittedItem,
    %         Opts
    %     )
    % ),
    ?assertEqual(
        <<"httpsig@1.0">>,
        hb_ao_micro:get(<<"commitment-device">>, CommittedItem, Opts)
    ),
    % TODO: Should this be a list?
    ?assertEqual(
        <<"l:...,commitment-device,committed,committer,keyid,signature,type">>,
        hb_ao_micro:get(<<"committed">>, CommittedItem, Opts)
    ),
    ?assertEqual(
        hb_util:human_id(ar_wallet:to_address(hb:wallet())),
        hb_ao_micro:get(<<"committer">>, CommittedItem, Opts)
    ),
    ?assertEqual(
        <<"rsa-pss-sha512">>,
        hb_ao_micro:get(<<"type">>, CommittedItem, Opts)
    ).

commit_unsigned_test() -> 
    application:ensure_all_started(hb),
    #{store := Stores} = hb_opts:default_message(),
    [PreloadedStore] =
        lists:filter(
            fun(#{ <<"store-module">> := hb_store_preloaded }) -> true;
                (_) -> false
            end,
            Stores
        ),
    Opts = 
        #{ 
            store => 
                [
                    #{ 
                        <<"store-module">> => hb_store_lmdb,
                        <<"name">> => <<"cache-TEST/lmdb">>
                    }, 
                    PreloadedStore
                ],
            priv_wallet => hb:wallet()
        },
    Item = #{ <<"a">> => <<"1">>, <<"b">> => <<"2">> },
    {ok, Path} = hb_cache_micro:write(Item, Opts),
    {ok, CommittedItem} = 
        hb_ao_micro:resolve(
            Item,
            #{
                <<"commitment-device">> => <<"httpsig@1.0">>,
                <<"device">> => <<"message@1.0">>,
                <<"type">> => <<"unsigned">>,
                <<"path">> => <<"commit">>
            },
            Opts
        ),
    ?assertEqual(
        {ok, <<"1">>},
        hb_ao_micro:resolve(
            [CommittedItem, <<"...">>, <<"a">>],
            Opts
        )
    ),
    ?assertEqual(
        {ok, <<"2">>},
        hb_ao_micro:resolve(
            [CommittedItem, <<"...">>, <<"b">>],
            Opts
        )
    ),
    % TODO: Id of the committed item is the SignedID - what should we compare it
    % to? We currently are not writing it to the cache.
    % ?assertEqual(
    %     hb_ao_micro:get(
    %         <<"id">>,
    %         CommittedItem,
    %         Opts
    %     )
    % ),
    ?assertEqual(
        <<"httpsig@1.0">>,
        hb_ao_micro:get(<<"commitment-device">>, CommittedItem, Opts)
    ),
    % TODO: Should this be a list?
    ?assertEqual(
        <<"l:...,commitment-device,committed,keyid,signature,type">>,
        hb_ao_micro:get(<<"committed">>, CommittedItem, Opts)
    ),
    ?assertEqual(
        <<"hmac-sha256">>,
        hb_ao_micro:get(<<"type">>, CommittedItem, Opts)
    ).

list_commitments_test() ->
    Opts = test_opts(),
    Item = #{ <<"a">> => <<"1">>, <<"b">> => <<"2">> },
    {ok, SignedCommittedItem} = 
        hb_ao_micro:resolve(
            Item,
            #{
                <<"commitment-device">> => <<"httpsig@1.0">>,
                <<"device">> => <<"message@1.0">>,
                <<"type">> => <<"signed">>,
                <<"path">> => <<"commit">>
            },
            Opts
        ),
    {ok, UnsignedCommittedItem} = 
        hb_ao_micro:resolve(
            Item,
            #{
                <<"commitment-device">> => <<"httpsig@1.0">>,
                <<"device">> => <<"message@1.0">>,
                <<"type">> => <<"unsigned">>,
                <<"path">> => <<"commit">>
            },
            Opts
        ),
    ?event(debug_test,
        {
            known_commitments,
            {
                signed_committed_item,
                SignedCommittedItem
            },
            {
                unsigned_committed_item,
                UnsignedCommittedItem
            }
        }
    ),
    {ok, Commitments} =
        hb_ao_micro:resolve(
            Item,
            <<"commitments">>,
            Opts
        ),
    ?event(list_commitments, {comms, Commitments}),
    ok.
full_commitments_test() ->
    Opts = test_opts(),
    DeepInnerItem = #{ <<"e">> => <<"5">>, <<"f">> => <<"6">> },
    {ok, DeepInnerPath} = hb_cache_micro:write(DeepInnerItem, Opts),
    InnerItem = #{ <<"c">> => <<"3">>, <<"d">> => <<"4">>, <<"...">> => DeepInnerItem },
    {ok, InnerPath} = hb_cache_micro:write(InnerItem, Opts),
    OuterItem = 
        #{ 
            <<"a">> => <<"1">>,
            <<"b">> => <<"2">>,
            <<"...">> => InnerPath
        },
    {ok, OuterPath} = hb_cache_micro:write(OuterItem, Opts),
    {ok, CommittedItem} = hb_ao_micro:resolve(
        OuterPath,
        #{
            <<"commitment-device">> => <<"httpsig@1.0">>,
            <<"device">> => <<"message@1.0">>,
            <<"type">> => <<"signed">>,
            <<"path">> => <<"commit">>
        },
        Opts
    ),
    NestedSignedItem = #{ <<"g">> => <<"7">>, <<"...">> => CommittedItem },
    {ok, NestedSignedPath} = hb_cache_micro:write(NestedSignedItem, Opts),
    {ok, DeepInnerItemCommitted} = 
        hb_ao_micro:resolve(
            DeepInnerPath,
            #{ <<"path">> => <<"committed">> },
            Opts
        ),
    ?assertEqual(
        [<<"e">>, <<"f">>],
        DeepInnerItemCommitted
    ),
    {ok, InnerItemCommitted} = 
        hb_ao_micro:resolve(
            InnerPath,
            #{ <<"path">> => <<"committed">> },
            Opts
        ),
    ?assertEqual(
        [<<"...">>, <<"c">>, <<"d">>],
        InnerItemCommitted
    ),
    ?event(debug_full_commitments_test, {inner_item_committed, InnerItemCommitted}),
    {ok, OuterItemCommitted} = 
        hb_ao_micro:resolve(
            OuterPath,
            #{ <<"path">> => <<"committed">> },
            Opts
        ),
    ?assertEqual(
        [<<"...">>, <<"a">>, <<"b">>],
        OuterItemCommitted
    ),
    {ok, SignedItemCommitted} = 
        hb_ao_micro:resolve(
            CommittedItem,
            #{ <<"path">> => <<"committed">> },
            Opts
        ),
    ?assertEqual(
        [
            <<"...">>,
            <<"commitment-device">>,
            <<"committed">>,
            <<"committer">>,
            <<"keyid">>,
            <<"signature">>,
            <<"type">>
        ],
        SignedItemCommitted
    ),
    {ok, NestedSignedItemCommitted} = 
        hb_ao_micro:resolve(
            NestedSignedPath,
            #{ <<"path">> => <<"committed">> },
            Opts
        ),
    ?assertEqual(
        [
            <<"...">>,
            <<"g">>
        ],
        NestedSignedItemCommitted
    ),
    {ok, DeepInnerItemExtendedCommitted} = 
        hb_ao_micro:resolve(
            DeepInnerPath,
            #{ <<"path">> => <<"committed">>, <<"type">> => <<"extended">>},
            Opts
        ),
    ?assertEqual(
        [<<"e">>, <<"f">>],
        DeepInnerItemExtendedCommitted
    ),
    {ok, InnerItemExtendedCommitted} = 
        hb_ao_micro:resolve(
            InnerPath,
            #{ <<"path">> => <<"committed">>, <<"type">> => <<"extended">>},
            Opts
        ),
    ?assertEqual(
        [<<"c">>, <<"d">>, <<"e">>, <<"f">>],
        InnerItemExtendedCommitted
    ),
    {ok, OuterItemExtendedCommitted} = 
        hb_ao_micro:resolve(
            OuterPath,
            #{ <<"path">> => <<"committed">>, <<"type">> => <<"extended">>},
            Opts
        ),
    ?assertEqual(
        [<<"a">>, <<"b">>, <<"c">>, <<"d">>, <<"e">>, <<"f">>],
        OuterItemExtendedCommitted
    ),
    {ok, CommittedItemExtendedCommitted} = 
        hb_ao_micro:resolve(
            CommittedItem,
            #{ <<"path">> => <<"committed">>, <<"type">> => <<"extended">> },
            Opts
        ),
    ?assertEqual(
        [
            <<"a">>,
            <<"b">>,
            <<"c">>,
            <<"commitment-device">>,
            <<"committed">>,
            <<"committer">>,
            <<"d">>,
            <<"e">>,
            <<"f">>,
            <<"keyid">>,
            <<"signature">>,
            <<"type">>
        ],
        CommittedItemExtendedCommitted
    ),
    {ok, NestedSignedItemExtendedCommitted} = 
        hb_ao_micro:resolve(
            NestedSignedPath,
            #{ <<"path">> => <<"committed">>, <<"type">> => <<"extended">> },
            Opts
        ),
    ?assertEqual(
        [
            <<"a">>,
            <<"b">>,
            <<"c">>,
            <<"commitment-device">>,
            <<"committed">>,
            <<"committer">>,
            <<"d">>,
            <<"e">>,
            <<"f">>,
            <<"g">>,
            <<"keyid">>,
            <<"signature">>,
            <<"type">>
        ],
        NestedSignedItemExtendedCommitted
    ),
    {ok, DeepInnerItemSignedCommitted} = 
        hb_ao_micro:resolve(
            DeepInnerPath,
            #{ <<"path">> => <<"committed">>, <<"type">> => <<"signed">>},
            Opts
        ),
    ?assertEqual(
        [],
        DeepInnerItemSignedCommitted
    ),
    {ok, InnerItemSignedCommitted} = 
        hb_ao_micro:resolve(
            InnerPath,
            #{ <<"path">> => <<"committed">>, <<"type">> => <<"signed">>},
            Opts
        ),
    ?assertEqual(
        [],
        InnerItemSignedCommitted
    ),
    {ok, OuterItemSignedCommitted} = 
        hb_ao_micro:resolve(
            OuterPath,
            #{ <<"path">> => <<"committed">>, <<"type">> => <<"signed">>},
            Opts
        ),
    ?assertEqual(
        [],
        OuterItemSignedCommitted
    ),
    {ok, CommittedItemSignedCommitted} = 
        hb_ao_micro:resolve(
            CommittedItem,
            #{ <<"path">> => <<"committed">>, <<"type">> => <<"signed">>},
            Opts
        ),
    ?assertEqual(
        [
            <<"...">>,
            <<"commitment-device">>,
            % TODO: Committed should probably not be here. Remove from codecs.
            <<"committed">>,
            <<"committer">>,
            <<"keyid">>,
            <<"signature">>,
            <<"type">>
        ],
        CommittedItemSignedCommitted
    ),
    {ok, NestedSignedItemSignedCommitted} = 
        hb_ao_micro:resolve(
            NestedSignedPath,
            #{ <<"path">> => <<"committed">>, <<"type">> => <<"signed">> },
            Opts
        ),
    ?assertEqual(
        [
            <<"...">>,
            <<"commitment-device">>,
            % TODO: Committed should probably not be here. Remove from codecs.
            <<"committed">>,
            <<"committer">>,
            <<"keyid">>,
            <<"signature">>,
            <<"type">>
        ],
        NestedSignedItemSignedCommitted
    ),
    {ok, DeepInnerItemSignedExtendedCommitted} = 
        hb_ao_micro:resolve(
            DeepInnerPath,
            #{ <<"path">> => <<"committed">>, <<"type">> => <<"signed-extended">> },
            Opts
        ),
    ?assertEqual(
        [],
        DeepInnerItemSignedExtendedCommitted
    ),
    {ok, InnerItemSignedExtendedCommitted} = 
        hb_ao_micro:resolve(
            InnerPath,
            #{ <<"path">> => <<"committed">>, <<"type">> => <<"signed-extended">> },
            Opts
        ),
    ?assertEqual(
        [],
        InnerItemSignedExtendedCommitted
    ),
    {ok, OuterItemSignedExtendedCommitted} = 
        hb_ao_micro:resolve(
            OuterPath,
            #{ <<"path">> => <<"committed">>, <<"type">> => <<"signed-extended">> },
            Opts
        ),
    ?assertEqual(
        [],
        OuterItemSignedExtendedCommitted
    ),
    {ok, CommittedItemSignedExtendedCommitted} = 
        hb_ao_micro:resolve(
            CommittedItem,
            #{ <<"path">> => <<"committed">>, <<"type">> => <<"signed-extended">> },
            Opts
        ),
    ?assertEqual(
        [
            <<"a">>,
            <<"b">>,
            <<"c">>,
            <<"commitment-device">>,
            % TODO: Committed should probably not be here. Remove from codecs.
            <<"committed">>,
            <<"committer">>,
            <<"d">>,
            <<"e">>,
            <<"f">>,
            <<"keyid">>,
            <<"signature">>,
            <<"type">>
        ],
        CommittedItemSignedExtendedCommitted
    ),
    {ok, NestedSignedItemSignedExtendedCommitted} = 
        hb_ao_micro:resolve(
            NestedSignedPath,
            #{ <<"path">> => <<"committed">>, <<"type">> => <<"signed-extended">> },
            Opts
        ),
    ?assertEqual(
        [
            <<"a">>,
            <<"b">>,
            <<"c">>,
            <<"commitment-device">>,
            % TODO: Committed should probably not be here. Remove from codecs.
            <<"committed">>,
            <<"committer">>,
            <<"d">>,
            <<"e">>,
            <<"f">>,
            <<"keyid">>,
            <<"signature">>,
            <<"type">>
        ],
        NestedSignedItemSignedExtendedCommitted
    ).
flatten_test() ->
    Opts = test_opts(),
    Item = 
        #{ 
            <<"a">> => <<"1">>,
            <<"b">> => <<"2">>
        },
    {ok, Flattened} = hb_ao_micro:resolve(Item, <<"flatten">>, Opts),
    ?event(flatten_test, {flattened, Flattened}),
    ?assertEqual(
        {ok, <<"1">>},
        hb_ao_micro:resolve(Flattened, <<"a">>, Opts)
    ),
    ?assertEqual(
        {ok, <<"2">>},
        hb_ao_micro:resolve(Flattened, <<"b">>, Opts)
    ).
flatten_nested_test() ->
    Opts = test_opts(),
    Item = 
        #{ 
            <<"a">> => <<"1">>,
            <<"b">> => <<"2">>,
            <<"nested">> => #{ <<"c">> => <<"3">>, <<"d">> => <<"4">> }
        },
    {ok, Flattened} = hb_ao_micro:resolve(Item, <<"flatten">>, Opts),
    ?event(flatten_test, {flattened, Flattened}),
    ?assertEqual(
        {ok, <<"1">>},
        hb_ao_micro:resolve(Flattened, <<"a">>, Opts)
    ),
    ?assertEqual(
        {ok, <<"2">>},
        hb_ao_micro:resolve(Flattened, <<"b">>, Opts)
    ),
    ?assertEqual(
        {error, not_found},
        hb_ao_micro:resolve(Flattened, <<"c">>, Opts)
    ),
    ?assertEqual(
        {error, not_found},
        hb_ao_micro:resolve(Flattened, <<"d">>, Opts)
    ),
    Nested = hb_ao_micro:get(<<"nested">>, Flattened, Opts),
    ?assertEqual(
        {ok, <<"3">>},
        hb_ao_micro:resolve(Nested, <<"c">>, Opts)
    ),
    ?assertEqual(
        {ok, <<"4">>},
        hb_ao_micro:resolve(Nested, <<"d">>, Opts)
    ).
flatten_nested_expansion_test() ->
    Opts = test_opts(),
    Item = 
        #{ 
            <<"a">> => <<"1">>,
            <<"b">> => <<"2">>,
            <<"...">> => #{ <<"a">> => <<"3">>, <<"c">> => <<"4">> }
        },
    {ok, Flattened} = hb_ao_micro:resolve(Item, <<"flatten">>, Opts),
    ?event(flatten_test, {flattened, Flattened}),
    ?assertEqual(
        {ok, <<"1">>},
        hb_ao_micro:resolve(Flattened, <<"a">>, Opts)
    ),
    ?assertEqual(
        {ok, <<"2">>},
        hb_ao_micro:resolve(Flattened, <<"b">>, Opts)
    ),
    ?assertEqual(
        {ok, <<"4">>},
        hb_ao_micro:resolve(Flattened, <<"c">>, Opts)
    ).

%%% TODO: This points out a potentially bigger problem of cache infinite loops 
%%% When we flatten a message (BaseID) that does not have a <<"...">> key,
%%% the "result" is simply the original message. So, a hashpath is written to
%%% BaseID/flatten => BaseID. When hitting this key, since flatten is recursive,
%%% it will try to flatten the message again, causing an infinite loop.
double_flatten_test_() ->
    {timeout, 10, fun no_op/0}.
no_op() -> ok.
double_flatten() ->
    Opts = test_opts(),
    Item = 
        #{ 
            <<"a">> => <<"1">>,
            <<"b">> => <<"2">>,
            <<"...">> => #{ <<"a">> => <<"3">>, <<"c">> => <<"4">> }
        },
    {ok, Flattened} = hb_ao_micro:resolve(Item, <<"flatten">>, Opts),
    ?event(flatten_test, {flattened, Flattened}),
    {ok, DoubleFlattened} = hb_ao_micro:resolve(Flattened, <<"flatten">>, Opts),
    ?event(flatten_test, {double_flattened, DoubleFlattened}),
    ?assertEqual(
        {ok, <<"1">>},
        hb_ao_micro:resolve(Flattened, <<"a">>, Opts)
    ),
    ?assertEqual(
        {ok, <<"2">>},
        hb_ao_micro:resolve(Flattened, <<"b">>, Opts)
    ),
    ?assertEqual(
        {ok, <<"4">>},
        hb_ao_micro:resolve(Flattened, <<"c">>, Opts)
    ).
%%% TODO:
%%% Should this returns a, b or a, b, committed, id?
keys_test() ->
    Opts = test_opts(),
    Item = 
        #{ 
            <<"a">> => <<"1">>,
            <<"b">> => <<"2">>
            % <<"...">> => #{ <<"a">> => <<"3">>, <<"c">> => <<"4">> }
        },
    {ok, Keys} = hb_ao_micro:resolve(Item, <<"keys">>, Opts),
    ?event(keys_test, {keys, Keys}),
    ?assertEqual(
        [<<"a">>, <<"b">>, <<"committed">>, <<"id">>],
        Keys
    ).
keys_nested_test() ->
    Opts = test_opts(),
    Item = 
        #{ 
            <<"a">> => <<"1">>,
            <<"b">> => <<"2">>,
            <<"z">> => <<"26">>,
            <<"nested">> => #{ <<"c">> => <<"3">>, <<"d">> => <<"4">> }
        },
    {ok, Keys} = hb_ao_micro:resolve(Item, <<"keys">>, Opts),
    ?event(keys_test, {keys, Keys}),
    ?assertEqual(
        % TODO: These are alphabetically sorted - is that good?
        [<<"a">>, <<"b">>, <<"committed">>, <<"id">>, <<"nested">>, <<"z">>],
        Keys
    ).
keys_nested_expansion_test() ->
    Opts = test_opts(),
    Item = 
        #{ 
            <<"a">> => <<"1">>,
            <<"b">> => <<"2">>,
            <<"z">> => <<"26">>,
            <<"...">> =>
                #{ 
                    <<"a">> => <<"inner">>,
                    <<"c">> => <<"3">>,
                    <<"d">> => <<"4">>
                }
        },
    {ok, Keys} = hb_ao_micro:resolve(Item, <<"keys">>, Opts),
    ?event(keys_test, {keys, Keys}),
    ?assertEqual(
        % TODO: These are alphabetically sorted - is that good?
        [<<"a">>, <<"b">>, <<"c">>, <<"committed">>, <<"d">>, <<"id">>, <<"z">>],
        Keys
    ).

%%% Test ID 
id_test() -> 
    Opts = test_opts(),
    Item = #{ <<"a">> => <<"1">>, <<"b">> => <<"2">> },
    {ok, Path} = hb_cache_micro:write(Item, Opts),
    {ok, ID} = hb_ao_micro:resolve(Item, <<"id">>, Opts),
    ?event(id_test, {id, ID}),
    ?assertEqual(
        Path,
        ID
    ).