
%%% @doc A wrapper around the hb_cache module that provides a more
%%% convenient interface for reading the result of a process at a given slot or
%%% message ID.
-module(dev_process_cache).
-export([latest/2, latest/3, latest/4, read/2, read/3, write/4]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Read the result of a process at a given slot.
read(ProcID, Opts) ->
    hb_util:ok(latest(ProcID, Opts)).
read(ProcID, SlotRef, Opts) ->
    ?event({reading_computed_result, ProcID, SlotRef}),
    Path = lib_process:cache_path(ProcID, SlotRef, Opts),
    hb_cache:read(Path, Opts).

%% @doc Write a process computation result to the cache. The implementation
%% lives in `lib_process' as the single canonical copy, shared with (and
%% resolvable from) the vm device package; this is a thin delegation.
write(ProcID, Slot, Msg, Opts) ->
    lib_process:cache_write(ProcID, Slot, Msg, Opts).

%% @doc Retrieve the latest slot for a given process. Optionally state a limit
%% on the slot number to search for, as well as a required path that the slot
%% must have. The implementation lives in `lib_process' as the single canonical
%% copy, resolvable from both the process and vm device packages.
latest(ProcID, Opts) ->
    lib_process:cache_latest(ProcID, Opts).
latest(ProcID, RequiredPath, Opts) ->
    lib_process:cache_latest(ProcID, RequiredPath, Opts).
latest(ProcID, RawRequiredPath, Limit, RawOpts) ->
    lib_process:cache_latest(ProcID, RawRequiredPath, Limit, RawOpts).

%%% Tests

process_cache_suite_test_() ->
    hb_store:generate_test_suite(
        [
            {"write and read process outputs", fun test_write_and_read_output/1},
            {"find latest output (with path)", fun find_latest_outputs/1}
        ],
        [
            {Name, Opts}
        ||
            {Name, Opts} <- hb_store:test_stores()
        ]
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
    Proc1 = hb_process_test_vectors:aos_process(),
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
    ?assert(hb_message:match(Req, ReadReq)),
    ?event(read_latest_slot_without_qualifiers),
    {ok, 1, ReadBaseRequired} = latest(ProcID, <<"Process">>, Opts),
    ?event({read_latest_with_process, ReadBaseRequired}),
    ?assert(hb_message:match(Base, ReadBaseRequired)),
    ?event(read_latest_slot_with_shallow_key),
    {ok, 2, ReadReqRequired} = latest(ProcID, <<"Deep/Process">>, Opts),
    ?assert(hb_message:match(Req, ReadReqRequired)),
    ?event(read_latest_slot_with_deep_key),
    {ok, 1, ReadBase} = latest(ProcID, [], 1, Opts),
    ?assert(hb_message:match(Base, ReadBase)).
