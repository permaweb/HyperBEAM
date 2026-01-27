-module(dev_message_micro).
-export([commit/3, set/3, do_deep_merge/3]).
-include("include/hb.hrl").

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
commit(Base, Req, Opts) ->
    {ok, CommitmentDevice} =
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
new_commit_test() -> 
    Opts = #{ store => [#{ <<"store-module">> => hb_store_fs, <<"name">> => <<"cache-TEST/fs">> }] },
    Item = #{ <<"a">> => 1, <<"b">> => 2 },
    {ok, CommittedItem} = 
        hb_ao_micro:resolve(
            Item,
            #{ 
                <<"path">> => <<"commit-micro">>,
                <<"commitment-device">> => <<"httpsig@1.0">>,
                <<"type">> => <<"signed">>
            },
            Opts
        ),
    ?event(new_commit_test, {committed_item, CommittedItem}),

    % {ok, Path} = hb_cache_micro:write(CommittedItem, Opts),
    % {ok, Result} = hb_cache_micro:read(Path, Opts),
    % {ok, ResA} = hb_cache_micro:read(<<Path/binary, "/a">>, Opts),
    % {ok, ResB} = hb_cache_micro:read(<<Path/binary, "/b">>, Opts),
    % ?event(new_commit_test, {done, {path, Path}, {result, Result}, {res_a, ResA}, {res_b, ResB}}),
    ok.