%%% @doc Persistent storage for `~arweave-scheduler@1.0'. Consensus messages
%%% live in the node's main store. This module transfers only transaction
%%% messages needed by schedules into a mandatory, separate LMDB store and
%%% keeps the scheduler's global frontier, process frontiers, target index, and
%%% assignments there.
%%%
%%% Target aliases are published before the global frontier alias. They may be
%%% visible while the previous frontier is still current, which is harmless:
%%% process materialization filters every target by that durable frontier. The
%%% global alias is the publication boundary, not a claim of store-wide
%%% atomicity.
-module(lib_arweave_scheduler_cache).
-export([opts/1]).
-export([read_global/1, publish_global/3]).
-export([read_process/2, write_process/3]).
-export([write_header/3]).
-export([ensure_target_root/2, read_target/3, list_targets/2]).
-export([write_assignment/2, read_assignment/3]).
-export([assignments_to_bundle/4]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(CACHE_PREFIX, <<"~arweave-scheduler@1.0">>).

%% @doc Select and validate the scheduler's isolated LMDB store domain.
opts(RawOpts) ->
    case hb_opts:get(scheduler_store, undefined, RawOpts) of
        undefined ->
            error({'scheduler-store-required', undefined});
        SchedulerStore ->
            SchedulerSpecs = store_specs(SchedulerStore),
            case
                SchedulerSpecs =/= [] andalso
                    lists:all(fun lmdb_capable/1, SchedulerSpecs)
            of
                false ->
                    error({'scheduler-store-not-lmdb', SchedulerStore});
                true ->
                    MainStore = hb_opts:get(store, no_viable_store, RawOpts),
                    SchedulerIDs = store_identities(SchedulerSpecs),
                    MainIDs = store_identities(store_specs(MainStore)),
                    case
                        lists:any(
                            fun(ID) -> lists:member(ID, MainIDs) end,
                            SchedulerIDs
                        )
                    of
                        true ->
                            error(
                                {
                                    'scheduler-store-overlaps-main',
                                    SchedulerIDs -- (SchedulerIDs -- MainIDs)
                                }
                            );
                        false ->
                            RawOpts#{
                                <<"store">> => SchedulerStore,
                                <<"match-index">> => false
                            }
                    end
            end
    end.

%% @doc Read the durable global synchronization record.
read_global(RawOpts) ->
    read_fully(global_path(), opts(RawOpts)).

%% @doc Publish target aliases, verify them, then publish the global alias last.
publish_global(State, Targets, RawOpts) ->
    Opts = opts(RawOpts),
    maybe
        {ok, StateID, StoredState} ?= write_unlinked_state(State, Opts),
        {ok, TargetLinks} ?= write_target_entries(Targets, Opts),
        ok ?= link_targets(TargetLinks, Opts),
        ok ?= verify_targets(Targets, Opts),
        ok ?= link(StateID, global_path(), Opts),
        {ok, ReadState} ?= read_fully(global_path(), Opts),
        ok ?= require_exact(StoredState, ReadState, <<"global-state">>),
        {ok, StateID}
    end.

%% @doc Read and write one process's materialization frontier.
read_process(ProcessID, RawOpts) ->
    read_fully(process_path(ProcessID), opts(RawOpts)).
write_process(ProcessID, State, RawOpts) ->
    write_state(process_path(ProcessID), State, opts(RawOpts)).

%% @doc Load a validated transaction root from the main store and copy it into
%% the scheduler store under the same signed transaction identifier. Ordinary
%% links in transaction tags remain links; their targets belong to the
%% applications that created them, not the scheduler.
write_header(TXID, Header, RawMainOpts) ->
    SchedulerOpts = opts(RawMainOpts),
    ExpectedID = hb_util:human_id(TXID),
    Loaded = hb_cache:ensure_loaded(Header, RawMainOpts),
    Stored =
        hb_link:decode_all_links(
            hb_link:normalize(
                hb_private:reset(Loaded),
                discard,
                RawMainOpts
            )
        ),
    case hb_util:human_id(hb_message:id(Stored, signed, RawMainOpts)) of
        ExpectedID ->
            maybe
                {ok, ID} ?= hb_cache:write(Stored, SchedulerOpts),
                {ok, ReadHeader} ?=
                    read_header_with_opts(ExpectedID, SchedulerOpts),
                ok ?= require_header(ExpectedID, ReadHeader, SchedulerOpts),
                {ok, ID}
            end;
        ActualID ->
            failure(
                <<"header-id-mismatch">>,
                #{ <<"expected">> => ExpectedID, <<"actual">> => ActualID }
            )
    end.

%% @doc Read a transaction header while preserving its ordinary child links.
read_header(TXID, RawOpts) ->
    read_header_with_opts(hb_util:human_id(TXID), opts(RawOpts)).

%% @doc Ensure listing an address distinguishes no targets from a read failure.
ensure_target_root(Address, RawOpts) ->
    Opts = opts(RawOpts),
    hb_store:group(
        hb_opts:get(store, no_viable_store, Opts),
        target_root(Address),
        Opts
    ).

%% @doc Resolve a target alias and fully load its transaction header.
read_target(Address, Ordinate, RawOpts) ->
    read_target_with_opts(Address, Ordinate, opts(RawOpts)).

%% @doc List the ordinates targeting an address from the scheduler store.
list_targets(Address, RawOpts) ->
    Opts = opts(RawOpts),
    case
        hb_store:list(
            hb_opts:get(store, no_viable_store, Opts),
            target_root(Address),
            Opts
        )
    of
        {ok, Targets} ->
            {ok, hb_cache:ensure_all_loaded(Targets, Opts)};
        {error, not_found} ->
            not_found;
        Error ->
            Error
    end.

%% @doc Write an assignment, link its process slot, and read the slot back.
write_assignment(Assignment, RawOpts) ->
    Opts = opts(RawOpts),
    Stored = hb_private:reset(Assignment),
    ProcessID = hb_maps:get(<<"process">>, Stored, not_found, Opts),
    Slot = hb_maps:get(<<"slot">>, Stored, not_found, Opts),
    maybe
        {ok, ID} ?= hb_cache:write(Stored, Opts),
        SignedID = hb_message:id(Stored, signed, Opts),
        ok ?= link(SignedID, assignment_path(ProcessID, Slot), Opts),
        {ok, ReadAssignment} ?=
            read_assignment_with_opts(ProcessID, Slot, Opts),
        ok ?= require_assignment(ProcessID, Slot, ReadAssignment, Opts),
        {ok, ID}
    end.

%% @doc Read and fully load an assignment by its contiguous process slot.
read_assignment(ProcessID, Slot, RawOpts) ->
    read_assignment_with_opts(ProcessID, Slot, opts(RawOpts)).

%% @doc Format a schedule without claiming one tip hash sequenced every
%% historical assignment in the response.
assignments_to_bundle(ProcessID, Assignments, More, RawOpts) ->
    Opts =
        (opts(RawOpts))#{
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

%%% Internal storage operations.

%% @doc Write and read back state content before any stable alias names it.
write_unlinked_state(State, Opts) ->
    Stored = hb_private:reset(hb_cache:ensure_all_loaded(State, Opts)),
    maybe
        {ok, ID} ?= hb_cache:write(Stored, Opts),
        {ok, ReadState} ?= read_fully(ID, Opts),
        ok ?= require_exact(Stored, ReadState, <<"unlinked-state">>),
        {ok, ID, Stored}
    end.

%% @doc Publish one linked state record and verify the stable alias.
write_state(Path, State, Opts) ->
    maybe
        {ok, ID, Stored} ?= write_unlinked_state(State, Opts),
        ok ?= link(ID, Path, Opts),
        {ok, ReadState} ?= read_fully(Path, Opts),
        ok ?= require_exact(Stored, ReadState, <<"linked-state">>),
        {ok, ID}
    end.

%% @doc Write and read back target records before any target alias names them.
write_target_entries(Targets, Opts) ->
    write_target_entries(Targets, [], Opts).

write_target_entries([], Links, _Opts) ->
    {ok, lists:reverse(Links)};
write_target_entries(
        [{Address, Ordinate, BlockHash, TXID} | Targets],
        Links,
        Opts
    ) ->
    Target =
        #{
            <<"block-hash">> => BlockHash,
            <<"transaction">> => hb_util:human_id(TXID)
        },
    maybe
        {ok, ID, _Stored} ?= write_unlinked_state(Target, Opts),
        write_target_entries(
            Targets,
            [{Address, Ordinate, ID} | Links],
            Opts
        )
    end.

%% @doc Link every target in one request. The global alias is not part of it.
link_targets([], _Opts) ->
    ok;
link_targets(Targets, Opts) ->
    Links =
        maps:from_list(
            [
                {
                    target_path(Address, Ordinate),
                    ID
                }
            ||
                {Address, Ordinate, ID} <- Targets
            ]
        ),
    hb_store:link(hb_opts:get(store, no_viable_store, Opts), Links, Opts).

%% @doc Verify every published target resolves to its expected scheduler header.
verify_targets([], _Opts) ->
    ok;
verify_targets(
        [{Address, Ordinate, BlockHash, TXID} | Targets],
        Opts
    ) ->
    ExpectedID = hb_util:human_id(TXID),
    case read_target_with_opts(Address, Ordinate, Opts) of
        {ok, BlockHash, ExpectedID, _Header} ->
            verify_targets(Targets, Opts);
        {ok, ActualBlockHash, ActualID, _Header} ->
            failure(
                <<"target-id-mismatch">>,
                #{
                    <<"expected-block">> => BlockHash,
                    <<"actual-block">> => ActualBlockHash,
                    <<"expected">> => ExpectedID,
                    <<"actual">> => ActualID
                }
            );
        not_found ->
            failure(
                <<"target-not-readable">>,
                #{ <<"ordinate">> => Ordinate }
            );
        Error ->
            Error
    end.

%% @doc Read one target using already validated scheduler options.
read_target_with_opts(Address, Ordinate, Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    Path = target_path(Address, Ordinate),
    case resolve_target(Store, Path, Opts) of
        {ok, TargetID} -> read_target_entry(TargetID, Opts);
        not_found ->
            not_found;
        Error ->
            Error
    end.

%% @doc Read the block-bound target record and its copied transaction header.
read_target_entry(TargetID, Opts) ->
    case read_fully(TargetID, Opts) of
        {ok, Target} ->
            BlockHash = hb_maps:get(<<"block-hash">>, Target, not_found, Opts),
            TXID = hb_maps:get(<<"transaction">>, Target, not_found, Opts),
            case {
                is_binary(BlockHash) andalso byte_size(BlockHash) =:= 64,
                is_binary(TXID) andalso byte_size(TXID) =:= 43
            } of
                {true, true} ->
                    case read_header_with_opts(TXID, Opts) of
                        {ok, Header} -> {ok, BlockHash, TXID, Header};
                        not_found -> not_found;
                        Error -> Error
                    end;
                _ ->
                    failure(
                        <<"invalid-target-entry">>,
                        #{ <<"target">> => TargetID }
                    )
            end;
        not_found ->
            not_found;
        Error ->
            Error
    end.

%% @doc Resolve across a scheduler store domain without accepting an unchanged
%% path from one store as a successful target lookup.
resolve_target(Store, Path, Opts) when not is_list(Store) ->
    resolve_target([Store], Path, Opts);
resolve_target([], _Path, _Opts) ->
    not_found;
resolve_target([Store | Rest], Path, Opts) ->
    case hb_store:resolve(Store, Path, Opts) of
        {ok, Path} -> resolve_target(Rest, Path, Opts);
        {ok, TXID} -> {ok, TXID};
        {error, not_found} -> resolve_target(Rest, Path, Opts);
        Error -> Error
    end.

%% @doc Read a header using already validated scheduler options.
read_header_with_opts(TXID, Opts) ->
    case read_shallow(TXID, Opts) of
        {ok, Header} ->
            case require_header(TXID, Header, Opts) of
                ok -> {ok, Header};
                Error -> Error
            end;
        Other ->
            Other
    end.

%% @doc Read an assignment using already validated scheduler options.
read_assignment_with_opts(ProcessID, Slot, Opts) ->
    Path = assignment_path(ProcessID, Slot),
    try hb_cache:read(Path, Opts) of
        {ok, Assignment} ->
            Body = hb_maps:get(<<"body">>, Assignment, not_found, Opts),
            case Body of
                not_found ->
                    failure(
                        <<"invalid-assignment">>,
                        #{ <<"process">> => ProcessID, <<"slot">> => Slot }
                    );
                _ ->
                    {ok,
                        hb_message:normalize_commitments(
                            Assignment#{ <<"body">> => Body },
                            Opts
                        )
                    }
            end;
        {error, not_found} ->
            not_found;
        Error ->
            Error
    catch
        throw:{necessary_message_not_found, _, _} -> not_found;
        throw:{could_not_read_lazy_link, _, _, _} -> not_found
    end.

%% @doc Read one message layer without resolving its ordinary child links.
read_shallow(Path, Opts) ->
    try hb_cache:read(Path, Opts) of
        {ok, Message} -> {ok, Message};
        {error, not_found} -> not_found;
        Error -> Error
    catch
        throw:{necessary_message_not_found, _, _} -> not_found;
        throw:{could_not_read_lazy_link, _, _, _} -> not_found
    end.

%% @doc Read a message and recursively load every scheduler-store dependency.
read_fully(Path, Opts) ->
    try hb_cache:read(Path, Opts) of
        {ok, Message} ->
            {ok, hb_cache:ensure_all_loaded(Message, Opts)};
        {error, not_found} ->
            not_found;
        Error ->
            Error
    catch
        throw:{necessary_message_not_found, _, _} -> not_found;
        throw:{could_not_read_lazy_link, _, _, _} -> not_found
    end.

%% @doc Link one existing cache ID to a stable scheduler path.
link(ID, Path, Opts) ->
    hb_store:link(
        hb_opts:get(store, no_viable_store, Opts),
        #{ Path => ID },
        Opts
    ).

%% @doc Confirm a scheduler header is still named by its signed transaction ID.
require_header(ExpectedID, Header, Opts) ->
    case hb_util:human_id(hb_message:id(Header, signed, Opts)) of
        ExpectedID -> ok;
        ActualID ->
            failure(
                <<"header-id-mismatch">>,
                #{ <<"expected">> => ExpectedID, <<"actual">> => ActualID }
            )
    end.

%% @doc Confirm the linked slot reads back the assignment that named it.
require_assignment(ProcessID, Slot, Assignment, Opts) ->
    ExpectedSlot = hb_ao:normalize_key(Slot),
    case
        {
            hb_maps:get(<<"process">>, Assignment, not_found, Opts),
            hb_ao:normalize_key(
                hb_maps:get(<<"slot">>, Assignment, not_found, Opts)
            )
        }
    of
        {ProcessID, ExpectedSlot} ->
            ok;
        {ActualProcessID, ActualSlot} ->
            failure(
                <<"assignment-readback-mismatch">>,
                #{
                    <<"expected-process">> => ProcessID,
                    <<"expected-slot">> => ExpectedSlot,
                    <<"actual-process">> => ActualProcessID,
                    <<"actual-slot">> => ActualSlot
                }
            )
    end.

%% @doc Confirm a cache round trip reproduced the exact message.
require_exact(Expected, Expected, _Subject) ->
    ok;
require_exact(_Expected, _Actual, Subject) ->
    failure(<<"cache-readback-mismatch">>, #{ <<"subject">> => Subject }).

%% @doc Return a structured scheduler cache failure.
failure(Reason, Detail) ->
    {
        error,
        #{ <<"status">> => 500, <<"reason">> => Reason, <<"detail">> => Detail }
    }.

%%% Store-domain validation.

%% @doc Normalize a store descriptor or structured store list.
store_specs(undefined) -> [];
store_specs(no_viable_store) -> [];
store_specs(Store) when is_list(Store) -> Store;
store_specs(Store = #{ <<"store-module">> := _ }) -> [Store];
store_specs(Stores) when is_map(Stores) ->
    hb_util:message_to_ordered_list(Stores, #{});
store_specs(_Other) -> [].

%% @doc Whether a store is LMDB or a multi-store containing only LMDB domains.
lmdb_capable(#{ <<"store-module">> := hb_store_lmdb }) ->
    true;
lmdb_capable(#{ <<"store-module">> := hb_store_multi, <<"stores">> := Stores }) ->
    Specs = store_specs(Stores),
    Specs =/= [] andalso lists:all(fun lmdb_capable/1, Specs);
lmdb_capable(_Store) ->
    false.

%% @doc Collect every wrapper and leaf `{module, name}' store identity.
store_identities(Stores) ->
    lists:usort(lists:append([store_identity(Store) || Store <- Stores])).

store_identity(Store = #{ <<"store-module">> := Module }) ->
    Own = {Module, maps:get(<<"name">>, Store, Module)},
    case Store of
        #{ <<"store-module">> := hb_store_multi, <<"stores">> := Stores } ->
            [Own | store_identities(store_specs(Stores))];
        _ ->
            [Own]
    end;
store_identity(_InvalidStore) ->
    [].

%%% Stable scheduler paths.

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

%%% Tests.

%% @doc Consensus and scheduler data survive a transfer and scheduler restart
%% without either LMDB becoming an implicit fallback for the other.
dual_lmdb_boundary_and_restart_test() ->
    MainStore = hb_test_utils:test_store(hb_store_lmdb, <<"ar-sched-main">>),
    SchedulerStore =
        hb_test_utils:test_store(hb_store_lmdb, <<"ar-sched-isolated">>),
    ok = hb_store:start(MainStore),
    ok = hb_store:start(SchedulerStore),
    RawOpts =
        #{
            <<"store">> => [MainStore],
            <<"scheduler-store">> => [SchedulerStore],
            <<"priv-wallet">> => ar_wallet:new()
        },
    ProcessID = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Header =
        hb_message:commit(
            #{ <<"target">> => ProcessID, <<"data">> => <<"scheduler-body">> },
            RawOpts,
            #{ <<"commitment-device">> => <<"tx@1.0">> }
        ),
    TXID = hb_message:id(Header, signed, RawOpts),
    {ok, _} = hb_cache:write(Header, RawOpts),
    HeaderLink =
        {link, TXID, #{ <<"type">> => <<"link">>, <<"lazy">> => false }},
    try
        ?assertEqual(not_found, read_header(TXID, RawOpts)),
        {ok, _HeaderID} = write_header(TXID, HeaderLink, RawOpts),
        {ok, ReadHeader} = read_header(TXID, RawOpts),
        ?assertEqual(TXID, hb_message:id(ReadHeader, signed, opts(RawOpts))),
        BlockHash = hb_util:encode(crypto:hash(sha384, <<"block-101">>)),
        State =
            #{
                <<"from">> => 100,
                <<"to">> => 101,
                <<"block-hash">> => BlockHash
            },
        {ok, _GlobalID} =
            publish_global(
                State,
                [{ProcessID, <<"101-3">>, BlockHash, TXID}],
                RawOpts
            ),
        ?assertEqual({ok, State}, read_global(RawOpts)),
        ?assertEqual({ok, [<<"101-3">>]}, list_targets(ProcessID, RawOpts)),
        ?assertMatch(
            {ok, BlockHash, TXID, _},
            read_target(ProcessID, <<"101-3">>, RawOpts)
        ),
        Process =
            #{
                <<"process">> => ProcessID,
                <<"spawn-ordinate">> => <<"100-2">>,
                <<"synced-to">> => 101,
                <<"next-slot">> => 1
            },
        {ok, _ProcessStateID} = write_process(ProcessID, Process, RawOpts),
        ?assertEqual({ok, Process}, read_process(ProcessID, RawOpts)),
        Assignment =
            #{
                <<"process">> => ProcessID,
                <<"slot">> => 0,
                <<"body">> => HeaderLink,
                <<"type">> => <<"Assignment">>
            },
        {ok, _AssignmentID} = write_assignment(Assignment, RawOpts),
        {ok, ReadAssignment} = read_assignment(ProcessID, 0, RawOpts),
        ?assertEqual(
            TXID,
            hb_message:id(
                hb_maps:get(<<"body">>, ReadAssignment, not_found, RawOpts),
                signed,
                opts(RawOpts)
            )
        ),
        {ok, Bundle} =
            assignments_to_bundle(ProcessID, [ReadAssignment], false, RawOpts),
        ?assertEqual(
            <<"schedule">>,
            hb_maps:get(<<"type">>, Bundle, not_found, RawOpts)
        ),
        ?assertEqual(
            {error, not_found},
            hb_cache:read(global_path(), RawOpts)
        ),
        ok = hb_store:stop(MainStore),
        ok = hb_store:stop(SchedulerStore),
        ok = hb_store:start(SchedulerStore),
        ?assertEqual({ok, State}, read_global(RawOpts)),
        ?assertMatch(
            {ok, BlockHash, TXID, _},
            read_target(ProcessID, <<"101-3">>, RawOpts)
        ),
        ?assertEqual({ok, Process}, read_process(ProcessID, RawOpts)),
        ?assertMatch({ok, _}, read_assignment(ProcessID, 0, RawOpts))
    after
        hb_store:stop(MainStore),
        hb_store:stop(SchedulerStore)
    end.

%% @doc The scheduler store is mandatory, LMDB-backed, and disjoint from main.
invalid_scheduler_store_boundary_test() ->
    MainStore = hb_test_utils:test_store(hb_store_lmdb, <<"ar-sched-required">>),
    ?assertError(
        {'scheduler-store-required', undefined},
        opts(#{ <<"store">> => [MainStore] })
    ),
    ?assertError(
        {'scheduler-store-not-lmdb', _},
        opts(
            #{
                <<"store">> => [MainStore],
                <<"scheduler-store">> =>
                    [hb_test_utils:test_store(hb_store_volatile)]
            }
        )
    ),
    ?assertError(
        {'scheduler-store-overlaps-main', _},
        opts(
            #{
                <<"store">> => [MainStore],
                <<"scheduler-store">> => [MainStore]
            }
        )
    ).
