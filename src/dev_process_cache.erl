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
    Path = path(ProcID, SlotRef, Opts),
    hb_cache:read(Path, Opts).


%% @doc Write a process computation result to the cache.
write(ProcID, Slot, Msg, Opts) ->
    % Write the item to the cache in the root of the store.
    {ok, Root} = hb_cache:write(Msg, Opts),
    % Link the item to the path in the store by slot number.
    SlotNumPath = path(ProcID, Slot, Opts),
    hb_cache:link(Root, SlotNumPath, Opts),
    % Link the item to the message ID path in the store.
    MsgIDPath =
        path(
          ProcID,
          ID = hb_util:human_id(hb_ao:get(id, Msg, Opts)),
          Opts),
    ?event(
      {linking_id,
       {proc_id, ProcID},
       {slot, Slot},
       {id, ID},
       {path, MsgIDPath}}),
    hb_cache:link(Root, MsgIDPath, Opts),
    % Return the slot number path.
    {ok, SlotNumPath}.


%% @doc Calculate the path of a result, given a process ID and a slot.
path(ProcID, Ref, Opts) ->
    path(ProcID, Ref, [], Opts).


path(ProcID, Ref, PathSuffix, Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    hb_store:path(
      Store,
      [<<"computed">>,
       hb_util:human_id(ProcID)] ++
      case Ref of
          Int when is_integer(Int) -> ["slot", integer_to_binary(Int)];
          root -> [];
          slot_root -> ["slot"];
          _ -> [Ref]
      end ++ PathSuffix).


%% @doc Retrieve the latest slot for a given process. Optionally state a limit
%% on the slot number to search for, as well as a required path that the slot
%% must have.
latest(ProcID, Opts) -> latest(ProcID, [], Opts).


latest(ProcID, RequiredPath, Opts) ->
    latest(ProcID, RequiredPath, undefined, Opts).


latest(ProcID, RawRequiredPath, Limit, Opts) ->
    ?event({latest_called, {proc_id, ProcID}, {required_path, RawRequiredPath}, {limit, Limit}, {opts, Opts}}),
    % Convert the required path to a list of _binary_ keys.
    RequiredPath =
        case RawRequiredPath of
            undefined -> [];
            [] -> [];
            _ ->
                hb_path:term_to_path_parts(
                  RawRequiredPath,
                  Opts)
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
       {slots_in_range, CappedSlots}}),
    % Find the highest slot that has the necessary path.
    BestSlot =
        first_with_path(
          ProcID,
          RequiredPath,
          lists:reverse(lists:sort(CappedSlots)),
          Opts),
    case BestSlot of
        not_found ->
            % No slot found with the necessary path was found.
            not_found;
        SlotNum ->
            % Found. Return the slot number and the message at that slot.
            {ok, Msg} = hb_cache:read(path(ProcID, SlotNum, Opts), Opts),
            {ok, SlotNum, Msg}
    end.


%% @doc Find the latest assignment with the requested path suffix.
first_with_path(ProcID, RequiredPath, Slots, Opts) ->
    first_with_path(
      ProcID,
      RequiredPath,
      Slots,
      Opts,
      hb_opts:get(store, no_viable_store, Opts)).


first_with_path(_ProcID, _Required, [], _Opts, _Store) ->
    not_found;
first_with_path(ProcID, RequiredPath, [Slot | Rest], Opts, Store) ->
    RawPath = path(ProcID, Slot, RequiredPath, Opts),
    ResolvedPath = hb_store:resolve(Store, RawPath),
    ?event({trying_slot, {slot, Slot}, {path, RawPath}, {resolved_path, ResolvedPath}}),
    case hb_store:type(Store, ResolvedPath) of
        not_found ->
            first_with_path(ProcID, RequiredPath, Rest, Opts, Store);
        _ ->
            Slot
    end.


%%% Tests


process_cache_suite_test_() ->
    hb_store:generate_test_suite(
      [{"write and read process outputs", fun test_write_and_read_output/1},
       {"find latest output (with path)", fun find_latest_outputs/1}],
      [ {Name, Opts}
        || {Name, Opts} <- hb_store:test_stores() ]).


%% @doc Test for writing multiple computed outputs, then getting them by
%% their slot number and by their signed and unsigned IDs.
test_write_and_read_output(Opts) ->
    Proc = hb_cache:test_signed(
             #{<<"test-item">> => hb_cache:test_unsigned(<<"test-body-data">>)}),
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
    Proc1 = dev_process:test_aos_process(),
    ProcID = hb_util:human_id(hb_ao:get(id, Proc1, Opts)),
    % Create messages for the slots, with only the middle slot having a
    % `/Process' field, while the top slot has a `/Deep/Process' field.
    Msg0 = #{<<"Results">> => #{<<"Result-Number">> => 0}},
    Msg1 =
        #{
          <<"Results">> => #{<<"Result-Number">> => 1},
          <<"Process">> => Proc1
         },
    Msg2 =
        #{
          <<"Results">> => #{<<"Result-Number">> => 2},
          <<"Deep">> => #{<<"Process">> => Proc1}
         },
    % Write the messages to the cache.
    {ok, _} = write(ProcID, 0, Msg0, Opts),
    {ok, _} = write(ProcID, 1, Msg1, Opts),
    {ok, _} = write(ProcID, 2, Msg2, Opts),
    ?event(wrote_items),
    % Read the messages with various qualifiers.
    {ok, 2, ReadMsg2} = latest(ProcID, Opts),
    ?event({read_latest, ReadMsg2}),
    ?assert(hb_message:match(Msg2, ReadMsg2)),
    ?event(read_latest_slot_without_qualifiers),
    {ok, 1, ReadMsg1Required} = latest(ProcID, <<"Process">>, Opts),
    ?event({read_latest_with_process, ReadMsg1Required}),
    ?assert(hb_message:match(Msg1, ReadMsg1Required)),
    ?event(read_latest_slot_with_shallow_key),
    {ok, 2, ReadMsg2Required} = latest(ProcID, <<"Deep/Process">>, Opts),
    ?assert(hb_message:match(Msg2, ReadMsg2Required)),
    ?event(read_latest_slot_with_deep_key),
    {ok, 1, ReadMsg1} = latest(ProcID, [], 1, Opts),
    ?assert(hb_message:match(Msg1, ReadMsg1)).
