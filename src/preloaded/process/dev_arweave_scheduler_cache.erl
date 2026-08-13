%%% @doc Persistent cache for `~arweave-scheduler@1.0'. The cache holds one
%%% global chain synchronization record, one materialization record per
%%% process, the inverted target index, and contiguous process assignments.
%%% State records are content-addressed messages whose stable paths link to
%%% their latest value.
-module(dev_arweave_scheduler_cache).
-export([opts/1]).
-export([read_global/1, write_global/2]).
-export([read_process/2, write_process/3, list_processes/1]).
-export([write_header/2, read_header/2]).
-export([write_block/3, read_block/2]).
-export([ensure_target_root/2]).
-export([write_target/4, write_targets/2, read_target/3, list_targets/2]).
-export([write_assignment/2, read_assignment/3]).
-export([assignments_to_bundle/4]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(CACHE_PREFIX, <<"~arweave-scheduler@1.0">>).

%% @doc Use the scheduler store when one is configured.
opts(Opts) ->
    Opts#{
        <<"store">> =>
            hb_opts:get(
                scheduler_store,
                hb_opts:get(store, no_viable_store, Opts),
                Opts
            )
    }.

%% @doc Read and write the linked global synchronization record.
read_global(Opts) -> read_state(global_path(), Opts).
write_global(State, Opts) -> write_state(global_path(), State, Opts).

%% @doc Read and write a process's linked materialization record.
read_process(ProcessID, Opts) ->
    read_state(process_path(ProcessID), Opts).
write_process(ProcessID, State, Opts) ->
    write_state(process_path(ProcessID), State, Opts).

%% @doc List processes for which a materialization record exists.
list_processes(RawOpts) ->
    Opts = opts(RawOpts),
    Store = hb_opts:get(store, no_viable_store, Opts),
    case hb_store:list(Store, process_root(), Opts) of
        {ok, ProcessIDs} -> ProcessIDs;
        _ -> []
    end.

%% @doc Cache a signed transaction header under its TXID.
write_header(Header, RawOpts) ->
    hb_cache:write(Header, opts(RawOpts)).

%% @doc Read a cached signed transaction header by TXID.
read_header(TXID, RawOpts) ->
    hb_cache:read(TXID, opts(RawOpts)).

%% @doc Cache and read canonical block messages under scheduler-owned height
%% paths. This avoids coupling the scheduler package to the Arweave device's
%% private block-cache module.
write_block(Height, Block, RawOpts) ->
    Opts = opts(RawOpts),
    Store = hb_opts:get(store, no_viable_store, Opts),
    case hb_cache:write(Block, Opts) of
        {ok, BlockID} ->
            case hb_store:link(Store, #{ block_path(Height) => BlockID }, Opts) of
                ok -> {ok, BlockID};
                Error -> Error
            end;
        Error -> Error
    end.

read_block(Height, RawOpts) ->
    Opts = opts(RawOpts),
    try hb_cache:read(block_path(Height), Opts) of
        {ok, Block} -> {ok, hb_cache:ensure_all_loaded(Block, Opts)};
        {error, not_found} -> not_found;
        Error -> Error
    catch
        throw:{necessary_message_not_found, _, _} -> not_found;
        throw:{could_not_read_lazy_link, _, _, _} -> not_found
    end.

%% @doc Link an address and ordinate to the cached transaction header's TXID.
write_target(Address, Ordinate, TXID, RawOpts) ->
    write_targets([{Address, Ordinate, TXID}], RawOpts).

%% @doc Link all targets found in one block in a single store operation.
write_targets(Targets, RawOpts) ->
    write_targets_map(
        maps:from_list(
            [
                {target_path(Address, Ordinate), hb_util:human_id(TXID)}
            ||
                {Address, Ordinate, TXID} <- Targets
            ]
        ),
        RawOpts
    ).

write_targets_map(Targets, _RawOpts) when map_size(Targets) =:= 0 -> ok;
write_targets_map(Targets, RawOpts) ->
    Opts = opts(RawOpts),
    Store = hb_opts:get(store, no_viable_store, Opts),
    hb_store:link(Store, Targets, Opts).

%% @doc Resolve an indexed target's signed TXID and read its cached header.
read_target(Address, Ordinate, RawOpts) ->
    Opts = opts(RawOpts),
    Store = hb_opts:get(store, no_viable_store, Opts),
    case resolve_target(Store, target_path(Address, Ordinate), Opts) of
        {ok, TXID} ->
            case hb_cache:read(TXID, Opts) of
                {ok, Header} -> {ok, TXID, Header};
                {error, not_found} -> not_found
            end;
        {error, not_found} -> not_found
    end.

resolve_target(Store, Path, Opts) when not is_list(Store) ->
    resolve_target([Store], Path, Opts);
resolve_target([], _Path, _Opts) -> {error, not_found};
resolve_target([Store | Rest], Path, Opts) ->
    case hb_store:resolve(Store, Path, Opts) of
        {ok, Path} -> resolve_target(Rest, Path, Opts);
        {ok, TXID} -> {ok, TXID};
        _ -> resolve_target(Rest, Path, Opts)
    end.

%% @doc Ensure that listing an address can distinguish no targets from failure.
ensure_target_root(Address, RawOpts) ->
    Opts = opts(RawOpts),
    hb_store:group(
        hb_opts:get(store, no_viable_store, Opts),
        target_root(Address),
        Opts
    ).

%% @doc List the ordinates that target an address. Callers parse and sort the
%% numeric components; no store ordering guarantee is assumed.
list_targets(Address, RawOpts) ->
    Opts = opts(RawOpts),
    Store = hb_opts:get(store, no_viable_store, Opts),
    hb_store:list(Store, target_root(Address), Opts).

%% @doc Write a deterministic assignment and link its process slot to it.
write_assignment(Assignment, RawOpts) ->
    Opts = opts(RawOpts),
    Store = hb_opts:get(store, no_viable_store, Opts),
    ProcessID = hb_maps:get(<<"process">>, Assignment, not_found, Opts),
    Slot = hb_maps:get(<<"slot">>, Assignment, not_found, Opts),
    case hb_cache:write(Assignment, Opts) of
        {ok, _} ->
            hb_store:link(
                Store,
                #{
                    assignment_path(ProcessID, Slot) =>
                        hb_message:id(Assignment, signed, Opts)
                },
                Opts
            );
        Error -> Error
    end.

%% @doc Read an assignment by its contiguous process slot.
read_assignment(ProcessID, Slot, RawOpts) ->
    Opts = opts(RawOpts),
    Store = hb_opts:get(store, no_viable_store, Opts),
    case hb_store:resolve(Store, assignment_path(ProcessID, Slot), Opts) of
        {ok, Resolved} ->
            case hb_cache:read(Resolved, Opts) of
                {ok, RawAssignment} ->
                    {ok,
                        hb_cache:ensure_all_loaded(
                            hb_message:normalize_commitments(RawAssignment, Opts),
                            Opts
                        )
                    };
                {error, not_found} -> not_found
            end;
        {error, not_found} -> not_found
    end.

%% @doc Format a schedule response without claiming that one tip hash or
%% timestamp sequenced every historical assignment in the response.
assignments_to_bundle(ProcessID, Assignments, More, RawOpts) ->
    Opts = RawOpts#{
        <<"hashpath">> => ignore,
        <<"cache-control">> => [<<"no-cache">>, <<"no-store">>],
        <<"await-inprogress">> => false
    },
    {ok,
        #{
            <<"type">> => <<"schedule">>,
            <<"process">> => hb_util:human_id(ProcessID),
            <<"continues">> => hb_util:atom(More),
            <<"assignments">> =>
                hb_message:normalize_commitments(
                    hb_maps:from_list(
                        [
                            {
                                hb_maps:get(
                                    <<"slot">>, Assignment, not_found, Opts
                                ),
                                Assignment
                            }
                        ||
                            Assignment <- Assignments
                        ]
                    ),
                    Opts
                )
        }
    }.

read_state(Path, RawOpts) ->
    Opts = opts(RawOpts),
    Store = hb_opts:get(store, no_viable_store, Opts),
    case hb_store:resolve(Store, Path, Opts) of
        {ok, Resolved} ->
            case hb_cache:read(Resolved, Opts) of
                {ok, State} -> {ok, hb_cache:ensure_all_loaded(State, Opts)};
                Error -> Error
            end;
        {error, not_found} -> not_found
    end.

write_state(Path, State, RawOpts) ->
    Opts = opts(RawOpts),
    Store = hb_opts:get(store, no_viable_store, Opts),
    case hb_cache:write(State, Opts) of
        {ok, StateID} ->
            case hb_store:link(Store, #{ Path => StateID }, Opts) of
                ok -> {ok, StateID};
                Error -> Error
            end;
        Error -> Error
    end.

global_path() ->
    hb_path:to_binary([?CACHE_PREFIX, <<"sync">>, <<"global">>]).

process_root() ->
    hb_path:to_binary([?CACHE_PREFIX, <<"sync">>, <<"process">>]).

process_path(ProcessID) ->
    hb_path:to_binary([process_root(), hb_util:human_id(ProcessID)]).

target_root(Address) ->
    hb_path:to_binary(
        [?CACHE_PREFIX, <<"targets">>, hb_util:human_id(Address)]
    ).

target_path(Address, Ordinate) ->
    hb_path:to_binary([target_root(Address), Ordinate]).

assignment_path(ProcessID, Slot) ->
    hb_path:to_binary(
        [
            ?CACHE_PREFIX,
            <<"assignments">>,
            hb_util:human_id(ProcessID),
            hb_ao:normalize_key(Slot)
        ]
    ).

block_path(Height) ->
    hb_path:to_binary([?CACHE_PREFIX, <<"blocks">>, hb_util:bin(Height)]).

%%% Tests

linked_state_and_target_test() ->
    Store = hb_test_utils:test_store(hb_store_volatile, <<"ar-sched-cache">>),
    ok = hb_store:start(Store),
    Opts = #{ <<"store">> => [Store], <<"priv-wallet">> => ar_wallet:new() },
    ProcessID = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Header =
        hb_message:commit(
            #{ <<"target">> => ProcessID },
            Opts,
            #{ <<"commitment-device">> => <<"tx@1.0">> }
        ),
    TXID = hb_util:human_id(hb_message:id(Header, signed, Opts)),
    Global =
        #{ <<"from">> => 100, <<"to">> => 101, <<"block-hash">> => TXID },
    {ok, _} = write_global(Global, Opts),
    ?assertEqual({ok, Global}, read_global(Opts)),
    Process =
        #{
            <<"process">> => ProcessID,
            <<"spawn-ordinate">> => <<"100-2">>,
            <<"synced-to">> => 101,
            <<"next-slot">> => 1
        },
    {ok, _} = write_process(ProcessID, Process, Opts),
    ?assertEqual({ok, Process}, read_process(ProcessID, Opts)),
    ?assertEqual([ProcessID], list_processes(Opts)),
    {ok, _} = write_header(Header, Opts),
    ok = write_target(ProcessID, <<"101-3">>, TXID, Opts),
    ?assertEqual({ok, [<<"101-3">>]}, list_targets(ProcessID, Opts)),
    EmptyStore = hb_test_utils:test_store(hb_store_volatile, <<"empty">>),
    ok = hb_store:start(EmptyStore),
    ReadOpts = Opts#{ <<"scheduler-store">> => [EmptyStore, Store] },
    {ok, ReadTXID, ReadHeader} = read_target(ProcessID, <<"101-3">>, ReadOpts),
    ?assertEqual(TXID, ReadTXID),
    ?assertEqual(
        TXID,
        hb_util:human_id(hb_message:id(ReadHeader, signed, Opts))
    ),
    ok = hb_store:stop(EmptyStore),
    ok = hb_store:stop(Store).
