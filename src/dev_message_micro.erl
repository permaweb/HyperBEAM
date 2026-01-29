-module(dev_message_micro).
-export([commit/3, set/3, do_deep_merge/3]).
-export([verify/3, commitments/3]).
-export([keys/3, flatten/3]).
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
-spec commit(any(), #{ commitment_device => binary() }, any()) -> {ok, map()}.

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
commit(Base, Req, Opts) ->
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
    % TODO: Cache is returning committed of form 
    % #{ <<"1">> => <<"a">>, <<"2">> => <<"b">>, <<"ao-types">> => ".list" }
    % Need to add handling for ordered lists
    Committed = hb_ao_micro:get(<<"committed">>, CommittedItem, Opts),
    ?assertEqual(
        {ok, <<"a">>},
        hb_ao_micro:resolve([Committed, <<"1">>], Opts)
    ),
    ?assertEqual(
        {ok, <<"b">>},
        hb_ao_micro:resolve([Committed, <<"2">>], Opts)
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
    % TODO: Cache is returning committed of form 
    % #{ <<"1">> => <<"a">>, <<"2">> => <<"b">>, <<"ao-types">> => ".list" }
    % Need to add handling for ordered lists
    Committed = hb_ao_micro:get(<<"committed">>, CommittedItem, Opts),
    ?assertEqual(
        {ok, <<"a">>},
        hb_ao_micro:resolve([Committed, <<"1">>], Opts)
    ),
    ?assertEqual(
        {ok, <<"b">>},
        hb_ao_micro:resolve([Committed, <<"2">>], Opts)
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
    {timeout, 10, fun double_flatten/0}.
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