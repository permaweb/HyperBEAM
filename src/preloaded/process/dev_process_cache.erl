
%%% @doc A wrapper around the hb_cache module that provides a more
%%% convenient interface for reading the result of a process at a given slot or
%%% message ID.
-module(dev_process_cache).
-export([fresh/4, latest/2, latest/3, latest/4]).
-export([read/2, read/3, refresh/3, write/4]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Read the result of a process at a given slot.
read(ProcID, Opts) ->
    hb_util:ok(latest(ProcID, Opts)).
read(ProcID, SlotRef, RawOpts) ->
    Opts = cache_opts(RawOpts),
    ?event({reading_computed_result, ProcID, SlotRef}),
    Path = path(ProcID, SlotRef, Opts),
    hb_cache:read(Path, Opts).

%% @doc Write a process computation result to the cache.
write(ProcID, Slot, Msg, RawOpts) ->
    Opts = cache_opts(RawOpts),
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

%% @doc Advance the process freshness marker to `Slot'. Scheduler observations
%% fence partially computed state by recording their target before computation.
%% A write to that target records its completion time.
refresh(ProcID, Slot, RawOpts) ->
    Opts = cache_opts(RawOpts),
    case latest_target(ProcID, scoped_opts(Opts)) of
        {ok, LatestSlot, _Timestamp} when Slot < LatestSlot ->
            ok;
        {ok, _LatestSlot, _Timestamp} ->
            write_target(ProcID, Slot, Opts);
        not_found ->
            write_target(ProcID, Slot, Opts);
        {error, _} = Error ->
            Error;
        {failure, _} = Failure ->
            Failure
    end.

%% @doc Return whether `Slot' is the completed scheduler target and is within
%% the effective maximum age for the `/now' request.
fresh(ProcID, Slot, Req, RawOpts) ->
    Opts = scoped_opts(RawOpts),
    case effective_max_age(Req, Opts) of
        infinity ->
            true;
        MaxAge ->
            case latest_target(ProcID, Opts) of
                {ok, Slot, Timestamp} -> clock(Opts) =< Timestamp + MaxAge;
                _ -> false
            end
    end.

%% @doc Write the latest scheduler target and its observation time.
write_target(ProcID, Slot, Opts) ->
    {ok, TargetID} =
        hb_cache:write(
            #{ <<"slot">> => Slot, <<"timestamp">> => clock(Opts) },
            Opts
        ),
    hb_cache:link(TargetID, target_path(ProcID), Opts).

%% @doc Read the latest scheduler target and observation time.
latest_target(ProcID, Opts) ->
    case hb_cache:read(target_path(ProcID), Opts) of
        {ok, Target} ->
            maybe
                {ok, Slot} ?= hb_maps:find(<<"slot">>, Target, Opts),
                {ok, Timestamp} ?= hb_maps:find(<<"timestamp">>, Target, Opts),
                {ok, hb_util:int(Slot), hb_util:int(Timestamp)}
            else
                _ -> {error, 'invalid-process-cache-target'}
            end;
        {error, not_found} ->
            not_found;
        Other ->
            Other
    end.

%% @doc Calculate the effective maximum age for a process `/now' request.
effective_max_age(Req, Opts) ->
    case only_if_cached(Req, Opts) of
        true ->
            infinity;
        false ->
            normalize_max_age(
                hb_maps:get(
                    <<"max-age">>,
                    Req,
                    hb_opts:get(process_now_max_age, infinity, Opts),
                    Opts
                )
            )
    end.

%% @doc Return whether cache-control requires a cached result.
only_if_cached(Req, Opts) ->
    RawCacheControl =
        hb_maps:get(
            <<"cache-control">>,
            Req,
            hb_opts:get(cache_control, [], Opts),
            Opts
        ),
    CacheControl =
        case RawCacheControl of
            Values when is_list(Values) -> Values;
            Value -> [Value]
        end,
    lists:member(
        <<"only-if-cached">>,
        lists:map(fun hb_ao:normalize_key/1, CacheControl)
    ).

%% @doc Normalize max-age values, treating malformed values as immediately
%% stale rather than serving an unbounded cached result.
normalize_max_age(infinity) -> infinity;
normalize_max_age(<<"infinity">>) -> infinity;
normalize_max_age(RawMaxAge) ->
    try hb_util:int(RawMaxAge) of
        MaxAge when MaxAge >= 0 -> MaxAge;
        _ -> 0
    catch
        _:_ -> 0
    end.

%% @doc Return the wall clock used for process freshness checks.
clock(Opts) ->
    hb_util:int(
        hb_opts:get(process_clock, erlang:system_time(second), Opts)
    ).

%% @doc Return the stable path of a process freshness marker.
target_path(ProcID) ->
    path(ProcID, <<"latest">>, #{}).

%% @doc Select the process cache store without changing other node stores.
cache_opts(Opts) ->
    Opts#{
        <<"store">> =>
            hb_opts:get(
                process_store,
                hb_opts:get(store, no_viable_store, Opts),
                Opts
            )
    }.

%% @doc Restrict process cache reads to the configured store scope.
scoped_opts(RawOpts) ->
    Opts = cache_opts(RawOpts),
    hb_store:scope(
        Opts,
        hb_opts:get(process_cache_scope, local, Opts)
    ).

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
    Opts = scoped_opts(RawOpts),
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
    AllSlots = hb_cache:list_numbered(Path, Opts),
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
        SlotNum ->
            % Found. Return the slot number and the message at that slot.
            {ok, Msg} = hb_cache:read(path(ProcID, SlotNum, Opts), Opts),
            {ok, SlotNum, Msg}
    end.

%% @doc Find the latest assignment with the requested path suffix.
first_with_path(_ProcID, _Required, [], _Opts) ->
    not_found;
first_with_path(ProcID, RequiredPath, [Slot | Rest], Opts) ->
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
            case required_path_exists(RequiredPath, Msg, Opts) of
                true -> Slot;
                false -> first_with_path(ProcID, RequiredPath, Rest, Opts)
            end
    end.

%% @doc Check a required message path without loading unrelated process state.
required_path_exists([], _Msg, _Opts) ->
    true;
required_path_exists([Key | Rest], Msg, Opts) ->
    case hb_link:is_link_key(Key) of
        true ->
            Loaded = hb_cache:ensure_loaded(Msg, Opts),
            case maps:find(hb_link:remove_link_specifier(Key), Loaded) of
                {ok, Link} when ?IS_LINK(Link) ->
                    case Rest of
                        [] -> true;
                        _ ->
                            required_path_exists(
                                Rest,
                                hb_cache:ensure_loaded(Link, Opts),
                                Opts
                            )
                    end;
                _ ->
                    false
            end;
        false ->
            case hb_maps:find(Key, Msg, Opts) of
                {ok, Next} -> required_path_exists(Rest, Next, Opts);
                error -> false
            end
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
                "honor max-age for cached process state",
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
    Proc1 = hb_process_test_vectors:aos_process(Opts),
    ProcID = hb_util:human_id(hb_ao:get(id, Proc1, Opts)),
    % Create messages for the slots, with only the middle slot having a
    % `/Process' field, while the top slot has a `/Deep/Process' field.
    Msg0 = #{ <<"Results">> => #{ <<"Result-Number">> => 0 } },
    Base =
        #{ 
            <<"Results">> => #{ <<"Result-Number">> => 1 }, 
            <<"Process">> => Proc1 
        },
    Req =
        #{ 
            <<"Results">> => #{ <<"Result-Number">> => 2 }, 
            <<"Deep">> => #{ <<"Process">> => Proc1 } 
        },
    % Write the messages to the cache.
    {ok, _} = write(ProcID, 0, Msg0, Opts),
    {ok, _} = write(ProcID, 1, Base, Opts),
    {ok, _} = write(ProcID, 2, Req, Opts),
    ?event(wrote_items),
    % Read the messages with various qualifiers.
    {ok, 2, ReadReq} = latest(ProcID, Opts),
    ?event({read_latest, ReadReq}),
    ?assert(hb_message:match(Req, ReadReq, strict, Opts)),
    ?event(read_latest_slot_without_qualifiers),
    {ok, 1, ReadBaseRequired} = latest(ProcID, <<"Process">>, Opts),
    ?event({read_latest_with_process, ReadBaseRequired}),
    ?assert(hb_message:match(Base, ReadBaseRequired, strict, Opts)),
    ?event(read_latest_slot_with_shallow_key),
    {ok, 2, ReadReqRequired} = latest(ProcID, <<"Deep/Process">>, Opts),
    ?assert(hb_message:match(Req, ReadReqRequired, strict, Opts)),
    ?event(read_latest_slot_with_deep_key),
    {ok, 2, _} = latest(ProcID, <<"Deep+link">>, Opts),
    ?event(read_latest_slot_with_link_key),
    {ok, 1, ReadBase} = latest(ProcID, [], 1, Opts),
    ?assert(hb_message:match(Base, ReadBase, strict, Opts)).

%% @doc Test that freshness follows the latest observed scheduler target.
freshness_max_age(Opts) ->
    ProcID = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Slot = 1,
    Msg = #{
        <<"device">> => <<"process@1.0">>,
        <<"at-slot">> => Slot,
        <<"results">> => #{ <<"number">> => Slot }
    },
    ?assertNot(fresh(
        ProcID,
        Slot,
        #{ <<"max-age">> => 60 },
        Opts#{ <<"process-clock">> => 100 }
    )),
    {ok, _} = write(
        ProcID,
        Slot,
        Msg,
        Opts#{ <<"process-clock">> => 100 }
    ),
    ?assertEqual(
        {ok, Slot, 100},
        latest_target(ProcID, scoped_opts(Opts))
    ),
    ?assert(fresh(
        ProcID,
        Slot,
        #{ <<"max-age">> => 60 },
        Opts#{ <<"process-clock">> => 160 }
    )),
    ?assertNot(fresh(
        ProcID,
        Slot,
        #{ <<"max-age">> => 60 },
        Opts#{ <<"process-clock">> => 161 }
    )),
    TargetSlot = Slot + 1,
    ok = refresh(
        ProcID,
        TargetSlot,
        Opts#{ <<"process-clock">> => 200 }
    ),
    ?assertNot(fresh(
        ProcID,
        Slot,
        #{ <<"max-age">> => 60 },
        Opts#{ <<"process-clock">> => 200 }
    )),
    {ok, _} = write(
        ProcID,
        Slot,
        Msg,
        Opts#{ <<"process-clock">> => 201 }
    ),
    ?assertEqual(
        {ok, TargetSlot, 200},
        latest_target(ProcID, scoped_opts(Opts))
    ),
    {ok, _} = write(
        ProcID,
        TargetSlot,
        Msg#{ <<"at-slot">> => TargetSlot },
        Opts#{ <<"process-clock">> => 201 }
    ),
    ?assert(fresh(
        ProcID,
        TargetSlot,
        #{ <<"max-age">> => 60 },
        Opts#{ <<"process-clock">> => 201 }
    )),
    ?assert(fresh(
        ProcID,
        TargetSlot,
        #{
            <<"cache-control">> => [<<"only-if-cached">>],
            <<"max-age">> => 0
        },
        Opts#{ <<"process-clock">> => 1000 }
    )),
    ?assertNot(fresh(
        ProcID,
        TargetSlot,
        #{ <<"max-age">> => <<"invalid">> },
        Opts#{ <<"process-clock">> => 202 }
    )),
    ?assert(fresh(
        ProcID,
        TargetSlot,
        #{},
        Opts#{
            <<"process-clock">> => 261,
            <<"process-now-max-age">> => 60
        }
    )).

%% @doc Test that configured process cache storage is isolated from the main
%% node store.
isolated_process_store_test() ->
    MainStore = hb_test_utils:test_store(
        hb_store_volatile,
        <<"process-cache-main">>
    ),
    ProcessStore = hb_test_utils:test_store(
        hb_store_volatile,
        <<"process-cache-isolated">>
    ),
    ok = hb_store:start(MainStore),
    ok = hb_store:start(ProcessStore),
    Opts = #{
        <<"store">> => [MainStore],
        <<"process-store">> => [ProcessStore]
    },
    ProcID = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Msg = #{ <<"results">> => #{ <<"ok">> => <<"stored">> } },
    {ok, Path} = write(ProcID, 1, Msg, Opts),
    ?assertEqual(
        {error, not_found},
        hb_cache:read(Path, #{ <<"store">> => [MainStore] })
    ),
    ?assertMatch({ok, _}, read(ProcID, 1, Opts)),
    ?assertMatch({ok, 1, _}, latest(ProcID, Opts)),
    ok = hb_store:stop(MainStore),
    ok = hb_store:stop(ProcessStore).
