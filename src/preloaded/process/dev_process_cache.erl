
%%% @doc A wrapper around the hb_cache module that provides a more
%%% convenient interface for reading the result of a process at a given slot or
%%% message ID.
-module(dev_process_cache).
-export([fresh/3, fresh/4]).
-export([latest/2, latest/3, latest/4]).
-export([read/2, read/3, refresh/3, write/4]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Read the result of a process at a given slot.
read(ProcID, Opts) ->
    hb_util:ok(latest(ProcID, Opts)).
read(ProcID, SlotRef, RawOpts) ->
    Opts = lib_process:cache_opts(RawOpts),
    ?event({reading_computed_result, ProcID, SlotRef}),
    Path = path(ProcID, SlotRef, Opts),
    hb_cache:read(Path, Opts).

%% @doc Write a process computation result to the cache.
write(ProcID, Slot, Msg, RawOpts) ->
    Opts = lib_process:cache_opts(RawOpts),
    % Write the item to the cache in the root of the store.
    {ok, Root} = hb_cache:write(hb_private:reset(Msg), Opts),
    % Link the item to the path in the store by slot number.
    SlotNumPath = path(ProcID, Slot, Opts),
    hb_cache:link(Root, SlotNumPath, Opts),
    % Link the item to the message ID path in the store.
    MsgIDPath =
        path(
            ProcID,
            ID = hb_message:id(Msg, uncommitted, Opts),
            Opts
        ),
    ?event(
        {linking_id,
            {proc_id, ProcID},
            {slot, Slot},
            {id, ID},
            {path, MsgIDPath}
        }
    ),
    hb_cache:link(Root, MsgIDPath, Opts),
    ok = refresh(ProcID, Slot, Opts),
    % Return the slot number path.
    {ok, SlotNumPath}.

%% @doc Mark the process as refreshed at the current clock time.
refresh(ProcID, Slot, RawOpts) ->
    Opts = lib_process:cache_opts(RawOpts),
    CachedSlot = latest_slot(ProcID, lib_process:scoped_opts(Opts)),
    case CachedSlot of
        {ok, LatestSlot} when Slot < LatestSlot ->
            ok;
        _ ->
            Store = hb_opts:get(store, no_viable_store, Opts),
            hb_store:write(
                Store,
                #{ refreshed_path(ProcID) => refreshed_at(Slot, clock(Opts)) },
                Opts
            )
    end.

refreshed_at(Slot, Timestamp) ->
    iolist_to_binary([
        integer_to_binary(Slot),
        <<":">>,
        integer_to_binary(Timestamp)
    ]).

refreshed_path(ProcID) ->
    path(ProcID, <<"refreshed-at">>, #{}).

%% @doc Return whether the latest cached process output is fresh enough for
%% `/now' to serve from cache under the effective `max-age'.
fresh(ProcID, Req, RawOpts) ->
    Opts = lib_process:scoped_opts(RawOpts),
    case effective_max_age(Req, Opts) of
        infinity ->
            true;
        _MaxAge ->
            case latest_slot(ProcID, Opts) of
                {ok, Slot} -> fresh(ProcID, Slot, Req, Opts);
                {error, not_found} -> false
            end
    end.
fresh(ProcID, Slot, Req, RawOpts) ->
    Opts = lib_process:scoped_opts(RawOpts),
    case effective_max_age(Req, Opts) of
        infinity ->
            true;
        MaxAge ->
            case read_refreshed_at(ProcID, Opts) of
                {ok, Slot, RefreshedAt} -> clock(Opts) =< RefreshedAt + MaxAge;
                _ -> false
            end
    end.

%% @doc Read the timestamp of the last refresh of a process.
read_refreshed_at(ProcID, Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    case hb_store:read(Store, refreshed_path(ProcID), Opts) of
        {ok, RefreshedAt} -> parse_refreshed_at(RefreshedAt);
        _ -> undefined
    end.

parse_refreshed_at(RefreshedAt) ->
    case binary:split(RefreshedAt, <<":">>) of
        [Slot, Timestamp] ->
            {ok, hb_util:int(Slot), hb_util:int(Timestamp)};
        _ ->
            undefined
    end.

%% @doc Calculate the effective maximum age of a process cache entry.
effective_max_age(Req, Opts) ->
    case lib_process:only_if_cached(Req, Opts) of
        true ->
            infinity;
        false ->
            case max_age_from_request(Req, Opts) of
                {ok, MaxAge} ->
                    normalize_max_age(MaxAge);
                error ->
                    normalize_max_age(
                        hb_opts:get(process_now_max_age, infinity, Opts)
                    )
            end
    end.

max_age_from_request(Req, Opts) when is_map(Req) ->
    hb_maps:find(<<"max-age">>, Req, Opts);
max_age_from_request(_Req, _Opts) ->
    error.

normalize_max_age(infinity) -> infinity;
normalize_max_age(<<"infinity">>) -> infinity;
normalize_max_age(RawMaxAge) -> hb_util:int(RawMaxAge).

%% @doc Return the current clock time. Allows the option to override the clock
%% time with a custom value for test use.
clock(Opts) ->
    case hb_opts:get(process_clock, undefined, Opts) of
        undefined -> erlang:system_time(second);
        Time -> hb_util:int(Time)
    end.

%% @doc Calculate the path of a result, given a process ID and a slot.
path(ProcID, Ref, Opts) ->
    path(ProcID, Ref, [], Opts).
path(ProcID, Ref, PathSuffix, _Opts) ->
    hb_path:to_binary(
        [
            <<"computed">>,
            hb_util:human_id(ProcID)
        ] ++
        case Ref of
            Int when is_integer(Int) -> ["slot", integer_to_binary(Int)];
            root -> [];
            slot_root -> ["slot"];
            _ -> [Ref]
        end ++ PathSuffix
    ).

%% @doc Retrieve the latest slot for a given process. Optionally state a limit
%% on the slot number to search for, as well as a required path that the slot
%% must have.
latest(ProcID, Opts) -> latest(ProcID, [], Opts).
latest(ProcID, RequiredPath, Opts) ->
    latest(ProcID, RequiredPath, undefined, Opts).
latest(ProcID, RawRequiredPath, Limit, RawOpts) ->
    Opts = lib_process:scoped_opts(RawOpts),
    % Convert the required path to a list of _binary_ keys.
    RequiredPath =
        case RawRequiredPath of
            undefined -> [];
            [] -> [];
            _ ->
                hb_path:term_to_path_parts(
                    RawRequiredPath,
                    Opts
                )
        end,
    ?event({required_path_converted, {proc_id, ProcID}, {required_path, RequiredPath}}),
    Path = path(ProcID, slot_root, Opts),
    AllSlots = slots(ProcID, Opts),
    ?event({all_slots, {proc_id, ProcID}, {slots, AllSlots}}),
    CappedSlots =
        case Limit of
            undefined -> AllSlots;
            _ -> lists:filter(fun(Slot) -> Slot =< Limit end, AllSlots)
        end,
    ?event(
        {finding_latest_slot,
            {proc_id, hb_util:human_id(ProcID)},
            {limit, Limit},
            {path, Path},
            {slots_in_range, CappedSlots}
        }
    ),
    % Find the highest slot that has the necessary path.
    BestSlot =
        first_with_path(
            ProcID,
            RequiredPath,
            lists:reverse(lists:sort(CappedSlots)),
            Opts
        ),
    case BestSlot of
        {failure, _} = Failure ->
            Failure;
        {error, _} = Error ->
            Error;
        not_found ->
            % No slot found with the necessary path was found.
            {error, not_found};
        {ok, SlotNum, Msg} ->
            {ok, SlotNum, Msg}
    end.

%% @doc Find the latest assignment with the requested path suffix.
first_with_path(_ProcID, _Required, [], _Opts) ->
    not_found;
first_with_path(ProcID, [], [Slot | Rest], Opts) ->
    read_candidate(ProcID, [], Slot, Rest, Opts);
first_with_path(ProcID, RequiredPath, [Slot | Rest], Opts) ->
    read_candidate(ProcID, RequiredPath, Slot, Rest, Opts).

read_candidate(ProcID, RequiredPath, Slot, Rest, Opts) ->
    RawPath = path(ProcID, Slot, Opts),
    ?event({trying_slot, {slot, Slot}, {path, RawPath}}),
    case hb_cache:read(RawPath, Opts) of
        {error, not_found} ->
            first_with_path(ProcID, RequiredPath, Rest, Opts);
        {failure, _} = Failure ->
            Failure;
        {error, _} = Error ->
            Error;
        {ok, Msg} ->
            case path_exists(RequiredPath, Msg, Opts) of
                true -> {ok, Slot, Msg};
                false -> first_with_path(ProcID, RequiredPath, Rest, Opts)
            end
    end.

latest_slot(ProcID, Opts) ->
    case lists:sort(slots(ProcID, Opts)) of
        [] -> {error, not_found};
        Slots -> {ok, lists:last(Slots)}
    end.

slots(ProcID, Opts) ->
    hb_cache:list_numbered(path(ProcID, slot_root, Opts), Opts).

path_exists([], _Msg, _Opts) ->
    true;
path_exists([Key | Rest], Msg, Opts) ->
    Found =
        case hb_link:is_link_key(Key) andalso is_map(Msg) of
            true ->
                case maps:find(hb_link:remove_link_specifier(Key), Msg) of
                    {ok, Link} when ?IS_LINK(Link) -> {ok, Link};
                    _ -> error
                end;
            false ->
                hb_maps:find(Key, Msg, Opts)
        end,
    case Found of
        {ok, Next} -> path_exists(Rest, Next, Opts);
        error -> false
    end.

%%% Tests

process_cache_suite_test_() ->
    hb_store:generate_test_suite(
        [
            {
                "write and read process outputs",
                fun(Store) ->
                    test_write_and_read_output(#{ <<"store">> => [Store] })
                end
            },
            {
                "find latest output (with path)",
                fun(Store) ->
                    find_latest_outputs(#{ <<"store">> => [Store] })
                end
            },
            {
                "honor max-age when checking process cache freshness",
                fun(Store) ->
                    freshness_max_age(#{ <<"store">> => [Store] })
                end
            }
        ],
        hb_store:test_stores()
    ).

%% @doc Test for writing multiple computed outputs, then getting them by
%% their slot number and by their signed and unsigned IDs.
test_write_and_read_output(Opts) ->
    Proc = hb_cache:test_signed(
        #{ <<"test-item">> => hb_cache:test_unsigned(<<"test-body-data">>) }),
    ProcID = hb_util:human_id(hb_ao:get(id, Proc)),
    Item1 = hb_cache:test_signed(<<"Simple signed output #1">>),
    Item2 = hb_cache:test_unsigned(<<"Simple unsigned output #2">>),
    {ok, Path0} = write(ProcID, 0, Item1, Opts),
    {ok, Path1} = write(ProcID, 1, Item2, Opts),
    {ok, DirectReadItem1} = hb_cache:read(Path0, Opts),
    ?assert(hb_message:match(Item1, DirectReadItem1)),
    {ok, DirectReadItem2} = hb_cache:read(Path1, Opts),
    ?assert(hb_message:match(Item2, DirectReadItem2)),
    {ok, ReadItem1BySlotNum} = read(ProcID, 0, Opts),
    ?assert(hb_message:match(Item1, ReadItem1BySlotNum)),
    {ok, ReadItem2BySlotNum} = read(ProcID, 1, Opts),
    ?assert(hb_message:match(Item2, ReadItem2BySlotNum)),
    {ok, ReadItem1ByID} =
        read(ProcID, hb_util:human_id(hb_ao:get(id, Item1)), Opts),
    ?assert(hb_message:match(Item1, ReadItem1ByID)),
    {ok, ReadItem2ByID} =
        read(ProcID, hb_util:human_id(hb_message:id(Item2, all)), Opts),
    ?assert(hb_message:match(Item2, ReadItem2ByID)).

%% @doc Test for retrieving the latest computed output for a process.
find_latest_outputs(Opts) ->
    % Create test environment.
    Store = hb_opts:get(store, no_viable_store, Opts),
    ResetRes = hb_store:reset(Store),
    ?event({reset_store, {result, ResetRes}, {store, Store}}),
    ProcID = hb_util:human_id(crypto:strong_rand_bytes(32)),
    ProcessRef = <<"test-process-ref">>,
    % Create messages for the slots, with only the middle slot having a
    % `/Process' field, while the top slot has a `/Deep/Process' field.
    Msg0 = #{ <<"Results">> => #{ <<"Result-Number">> => 0 } },
    Base =
        #{ 
            <<"Results">> => #{ <<"Result-Number">> => 1 }, 
            <<"Process">> => ProcessRef
        },
    Req =
        #{ 
            <<"Results">> => #{ <<"Result-Number">> => 2 }, 
            <<"Deep">> => #{ <<"Process">> => ProcessRef }
        },
    % Write the messages to the cache.
    {ok, _} = write(ProcID, 0, Msg0, Opts),
    {ok, _} = write(ProcID, 1, Base, Opts),
    {ok, _} = write(ProcID, 2, Req, Opts),
    ?event(wrote_items),
    % Read the messages with various qualifiers.
    {ok, 2, RawReadReq} = latest(ProcID, Opts),
    ReadReq = hb_cache:ensure_all_loaded(RawReadReq, Opts),
    ?event({read_latest, ReadReq}),
    ?assertEqual(2, maps:get(<<"Result-Number">>, maps:get(<<"Results">>, ReadReq))),
    ?assertEqual(ProcessRef, maps:get(<<"Process">>, maps:get(<<"Deep">>, ReadReq))),
    ?event(read_latest_slot_without_qualifiers),
    {ok, 1, RawReadBaseRequired} = latest(ProcID, <<"Process">>, Opts),
    ReadBaseRequired = hb_cache:ensure_all_loaded(RawReadBaseRequired, Opts),
    ?event({read_latest_with_process, ReadBaseRequired}),
    ?assertEqual(
        1,
        maps:get(<<"Result-Number">>, maps:get(<<"Results">>, ReadBaseRequired))
    ),
    ?assertEqual(ProcessRef, maps:get(<<"Process">>, ReadBaseRequired)),
    ?event(read_latest_slot_with_shallow_key),
    {ok, 2, RawReadReqRequired} = latest(ProcID, <<"Deep/Process">>, Opts),
    ReadReqRequired = hb_cache:ensure_all_loaded(RawReadReqRequired, Opts),
    ?assertEqual(
        2,
        maps:get(<<"Result-Number">>, maps:get(<<"Results">>, ReadReqRequired))
    ),
    ?assertEqual(ProcessRef, maps:get(<<"Process">>, maps:get(<<"Deep">>, ReadReqRequired))),
    ?event(read_latest_slot_with_deep_key),
    {ok, 2, _} = latest(ProcID, <<"Deep+link">>, Opts),
    ?event(read_latest_slot_with_link_key),
    {ok, 1, RawReadBase} = latest(ProcID, [], 1, Opts),
    ReadBase = hb_cache:ensure_all_loaded(RawReadBase, Opts),
    ?assertEqual(1, maps:get(<<"Result-Number">>, maps:get(<<"Results">>, ReadBase))),
    ?assertEqual(ProcessRef, maps:get(<<"Process">>, ReadBase)).

%% @doc Test for serving `/now' from cache only while the cache is fresh enough.
freshness_max_age(Opts) ->
    ProcID = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Slot = 1,
    SlotResult = #{
        <<"device">> => <<"process@1.0">>,
        <<"at-slot">> => Slot,
        <<"results">> => #{ <<"number">> => 1 }
    },
    % Assert that the process is not fresh by default.
    ?assertEqual(
        false,
        fresh(
            ProcID,
            #{ <<"max-age">> => 60 },
            Opts#{ <<"process-clock">> => 100 }
        )
    ),
    % Write the slot result to the cache at clock time 100.
    {ok, _} = write(ProcID, Slot, SlotResult, Opts#{ <<"process-clock">> => 100 }),
    {ok, 1, RawReadSlotResult} = latest(ProcID, Opts),
    ReadSlotResult = hb_cache:ensure_all_loaded(RawReadSlotResult, Opts),
    ?assertEqual(<<"process@1.0">>, maps:get(<<"device">>, ReadSlotResult)),
    ?assertEqual(1, maps:get(<<"number">>, maps:get(<<"results">>, ReadSlotResult))),
    ?assertEqual({ok, Slot, 100}, read_refreshed_at(ProcID, Opts)),
    % Assert that writing an older slot does not refresh the latest cached slot.
    OlderSlot = 0,
    OlderSlotResult = SlotResult#{
        <<"at-slot">> => OlderSlot,
        <<"results">> => #{ <<"number">> => 0 }
    },
    {ok, _} =
        write(
            ProcID,
            OlderSlot,
            OlderSlotResult,
            Opts#{ <<"process-clock">> => 200 }
        ),
    ?assertEqual({ok, Slot, 100}, read_refreshed_at(ProcID, Opts)),
    ?assertEqual(
        false,
        fresh(
            ProcID,
            #{ <<"max-age">> => 60 },
            Opts#{ <<"process-clock">> => 250 }
        )
    ),
    % Assert that the process is fresh exactly at the max-age.
    ?assertEqual(
        true,
        fresh(
            ProcID,
            #{ <<"max-age">> => 60 },
            Opts#{ <<"process-clock">> => 160 }
        )
    ),
    % Assert that the process is not fresh after the max-age.
    ?assertEqual(
        false,
        fresh(
            ProcID,
            #{ <<"max-age">> => 60 },
            Opts#{ <<"process-clock">> => 161 }
        )
    ),
    % Assert that the process is fresh if the max-age is infinity.
    ?assertEqual(
        true,
        fresh(
            ProcID,
            #{ <<"max-age">> => <<"infinity">> },
            Opts#{ <<"process-clock">> => 1000 }
        )
    ),
    % Assert that the process is fresh if the only-if-cached flag is set.
    ?assertEqual(
        true,
        fresh(
            ProcID,
            #{
                <<"cache-control">> => [<<"only-if-cached">>],
                <<"max-age">> => 0
            },
            Opts#{ <<"process-clock">> => 1000 }
        )
    ),
    % Assert that the max age is read as a fallback from the node opts
    ?assertEqual(
        true,
        fresh(
            ProcID,
            #{},
            Opts#{
                <<"process-clock">> => 160,
                <<"process-now-max-age">> => 60
            }
        )
    ),
    ?assertEqual(
        false,
        fresh(
            ProcID,
            #{},
            Opts#{
                <<"process-clock">> => 161,
                <<"process-now-max-age">> => 60
            }
        )
    ),
    % A scheduler check of an unchanged slot refreshes its cache age.
    ok = refresh(ProcID, Slot, Opts#{ <<"process-clock">> => 250 }),
    ?assertEqual({ok, Slot, 250}, read_refreshed_at(ProcID, Opts)).

%% @doc Process cache writes go only to `process-store' when configured.
isolated_process_store_test() ->
    MainStore = hb_test_utils:test_store(hb_store_fs, <<"process-main">>),
    ProcessStore = hb_test_utils:test_store(hb_store_fs, <<"process-isolated">>),
    Opts = #{
        <<"store">> => [MainStore],
        <<"process-store">> => [ProcessStore]
    },
    hb_store:start(MainStore),
    hb_store:start(ProcessStore),
    ProcID = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Msg = #{ <<"results">> => #{ <<"ok">> => <<"stored">> } },
    {ok, Path} = write(ProcID, 1, Msg, Opts),
    MainOpts = #{ <<"store">> => [MainStore] },
    ?assertMatch({error, not_found}, hb_cache:read(Path, MainOpts)),
    ?assertMatch({ok, _}, read(ProcID, 1, Opts)),
    ?assertMatch({ok, 1, _}, latest(ProcID, Opts)),
    hb_store:reset(MainStore),
    ?assertMatch({ok, _}, read(ProcID, 1, Opts)),
    ?assertMatch({ok, 1, _}, latest(ProcID, Opts)),
    hb_store:reset(ProcessStore),
    ?assertMatch({error, not_found}, read(ProcID, 1, Opts)),
    ?assertMatch({error, not_found}, latest(ProcID, Opts)).
