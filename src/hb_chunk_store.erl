%%% @doc Centralized chunk fetching service with dedup, QoS priority
%%% scheduling, and store-backed caching. Callers request sets of chunk
%%% offsets via `ensure_chunks/2` which blocks until all are fetched and
%%% written to a configurable `hb_store`. Workers fetch and decode chunks
%%% directly into the store — the gen_server only handles lightweight
%%% metadata notifications, never chunk binaries.
-module(hb_chunk_store).
-behaviour(gen_server).
-export([start_link/1, ensure_chunks/2, get/2, get_meta/2, put/3,
         has_stream/2, get_stream/2, materialize/1, content_digest/1]).
-export([resolve_store/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-include("include/hb.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_MAX_WORKERS, 10).
-define(MAX_RETRIES, 3).

-record(request, {
    ref       :: reference(),
    caller    :: pid(),
    mon       :: reference(),
    store_id  :: term(),
    opts      :: map(),
    pending   :: [integer()],          %% offsets not yet dispatched
    remaining :: sets:set(integer())   %% offsets not yet stored
}).

-record(state, {
    requests       :: #{reference() => #request{}},
    rotation       :: queue:queue(reference()),  %% round-robin order
    offset_status  :: #{{integer(), term()} => pending | fetching | stored},
    offset_waiters :: #{{integer(), term()} => [reference()]},
    inflight       :: #{{integer(), term()} => pid()},
    retries        :: #{{integer(), term()} => non_neg_integer()},
    max_workers    :: non_neg_integer(),
    active_workers :: non_neg_integer()
}).

%% -- Public API --

%% @doc Start the chunk stream server under the supervision tree.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Request that all offsets in `Offsets` be fetched and stored.
%% Blocks the caller until every offset is present in the store
%% determined by `Opts`. Workers are dispatched via round-robin across
%% active requests for fair bandwidth sharing.
ensure_chunks(Offsets, Opts) ->
    Ref = make_ref(),
    Mon = monitor(process, ?SERVER),
    gen_server:cast(?SERVER, {ensure, Offsets, Opts, self(), Ref}),
    receive
        {chunk_stream_ready, Ref, Result} ->
            demonitor(Mon, [flush]),
            Result;
        {'DOWN', Mon, process, _, Reason} ->
            {error, {chunk_stream_down, Reason}}
    end.

%% @doc Read a single chunk from the store. Returns the full
%% `{AbsStart, AbsEnd, Binary}` tuple or `not_found`.
get(Offset, Opts) ->
    Store = resolve_store(Opts),
    read_chunk_info(Offset, Store).

%% @doc Read only the metadata (no chunk binary) for a given offset.
%% Returns `{ok, {AbsStart, AbsEnd}}` or `not_found`.
get_meta(Offset, Opts) ->
    Store = resolve_store(Opts),
    case hb_store:read(Store, chunk_key(Offset)) of
        not_found ->
            not_found;
        {ok, <<AbsStart:64/big, AbsEnd:64/big, _Binary/binary>>} ->
            {ok, {AbsStart, AbsEnd}};
        {ok, _Malformed} ->
            not_found
    end.

%% @doc Write a chunk from its raw Arweave JSON response. Extracts the
%% base64 chunk, absolute_end_offset, and computes AbsStart internally.
%% The QueryOffset is only used for a secondary index key.
put(QueryOffset, ChunkJSON, Opts) when is_map(ChunkJSON) ->
    Chunk = hb_util:decode(maps:get(<<"chunk">>, ChunkJSON)),
    AbsEnd = hb_util:int(maps:get(<<"absolute_end_offset">>, ChunkJSON)),
    AbsStart = AbsEnd - byte_size(Chunk) + 1,
    Store = resolve_store(Opts),
    write_chunk_info(QueryOffset, AbsStart, AbsEnd, Chunk, Store).

%% @doc Check whether all offsets are present in the store.
has_stream(Offsets, Opts) ->
    Store = resolve_store(Opts),
    lists:all(
        fun(O) ->
            case read_chunk_info(O, Store) of
                {ok, _} -> true;
                not_found -> false
            end
        end,
        Offsets
    ).

%% @doc Build a streaming iterator that reads one chunk at a time
%% from the store. Returns `{ok, Fun}` where `Fun() -> {ChunkInfo, NextFun} | done`.
get_stream(Offsets, Opts) ->
    Store = resolve_store(Opts),
    {ok, build_stream(Offsets, Store)}.

%% @doc Materialize a chunk_stream descriptor into a contiguous binary.
%% Reads chunks one at a time from the store, trims first/last to the
%% requested range, and assembles the result. Used by callers that need
%% the full body (checksums, signatures, etc.).
materialize(Bin) when is_binary(Bin) -> Bin;
materialize(IOList) when is_list(IOList) -> iolist_to_binary(IOList);
materialize({chunk_stream, StreamFun, RangeStart, RangeEnd}) ->
    iolist_to_binary(
        materialize_loop(StreamFun, RangeStart, RangeEnd, true)
    ).

materialize_loop(StreamFun, RangeStart, RangeEnd, IsFirst) ->
    case StreamFun() of
        {{AbsStart, AbsEnd, Chunk}, NextFun} ->
            Trimmed =
                case IsFirst andalso AbsStart < RangeStart of
                    true ->
                        Skip = RangeStart - AbsStart,
                        binary:part(Chunk, Skip, byte_size(Chunk) - Skip);
                    false ->
                        Chunk
                end,
            Final =
                case AbsEnd >= RangeEnd of
                    true ->
                        Excess = AbsEnd - RangeEnd,
                        binary:part(Trimmed, 0,
                            byte_size(Trimmed) - Excess);
                    false ->
                        Trimmed
                end,
            case AbsEnd >= RangeEnd of
                true -> [Final];
                false ->
                    [Final |
                        materialize_loop(
                            NextFun, RangeStart, RangeEnd, false
                        )]
            end;
        done ->
            []
    end.

%% @doc Compute the SHA-256 content digest of a chunk_stream by streaming
%% through chunks one at a time. Never holds the full body in memory.
content_digest({chunk_stream, StreamFun, RangeStart, RangeEnd}) ->
    Ctx = crypto:hash_init(sha256),
    FinalCtx = digest_loop(StreamFun, RangeStart, RangeEnd, true, Ctx),
    crypto:hash_final(FinalCtx);
content_digest(Bin) when is_binary(Bin) ->
    crypto:hash(sha256, Bin).

digest_loop(StreamFun, RangeStart, RangeEnd, IsFirst, Ctx) ->
    case StreamFun() of
        {{AbsStart, AbsEnd, Chunk}, NextFun} ->
            Trimmed =
                case IsFirst andalso AbsStart < RangeStart of
                    true ->
                        Skip = RangeStart - AbsStart,
                        binary:part(Chunk, Skip, byte_size(Chunk) - Skip);
                    false ->
                        Chunk
                end,
            Final =
                case AbsEnd >= RangeEnd of
                    true ->
                        Excess = AbsEnd - RangeEnd,
                        binary:part(Trimmed, 0,
                            byte_size(Trimmed) - Excess);
                    false ->
                        Trimmed
                end,
            Ctx1 = crypto:hash_update(Ctx, Final),
            case AbsEnd >= RangeEnd of
                true -> Ctx1;
                false ->
                    digest_loop(NextFun, RangeStart, RangeEnd, false, Ctx1)
            end;
        done ->
            Ctx
    end.

%% -- gen_server callbacks --

init(Opts) ->
    MaxWorkers = hb_opts:get(
        chunk_stream_max_workers, ?DEFAULT_MAX_WORKERS, Opts
    ),
    {ok, #state{
        requests       = #{},
        rotation       = queue:new(),
        offset_status  = #{},
        offset_waiters = #{},
        inflight       = #{},
        retries        = #{},
        max_workers    = MaxWorkers,
        active_workers = 0
    }}.

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast({ensure, Offsets, Opts, CallerPid, Ref}, State) ->
    SId = store_id(Opts),
    Mon = monitor(process, CallerPid),
    Request = #request{
        ref       = Ref,
        caller    = CallerPid,
        mon       = Mon,
        store_id  = SId,
        opts      = Opts,
        pending   = Offsets,
        remaining = sets:from_list(Offsets)
    },
    State1 = State#state{
        requests = maps:put(Ref, Request, State#state.requests),
        rotation = queue:in(Ref, State#state.rotation)
    },
    State2 = register_offsets(Offsets, SId, Ref, State1),
    State3 = maybe_notify(Ref, State2),
    State4 = dispatch_workers(State3),
    {noreply, State4};

handle_cast({chunk_stored, Offset, StoreId, _AbsStart, _AbsEnd}, State) ->
    Key = {Offset, StoreId},
    State1 = State#state{
        offset_status = maps:put(Key, stored, State#state.offset_status),
        inflight = maps:remove(Key, State#state.inflight),
        active_workers = State#state.active_workers - 1
    },
    Waiters = maps:get(Key, State1#state.offset_waiters, []),
    State2 = lists:foldl(
        fun(Ref, S) -> mark_offset_done(Ref, Offset, S) end,
        State1,
        Waiters
    ),
    State3 = State2#state{
        offset_waiters = maps:remove(Key, State2#state.offset_waiters)
    },
    State4 = dispatch_workers(State3),
    {noreply, State4};

handle_cast({chunk_failed, Offset, StoreId, Reason}, State) ->
    Key = {Offset, StoreId},
    Retries = maps:get(Key, State#state.retries, 0),
    State1 = State#state{
        inflight = maps:remove(Key, State#state.inflight),
        active_workers = State#state.active_workers - 1
    },
    State2 = case Retries < ?MAX_RETRIES of
        true ->
            requeue_offset(Offset, StoreId, Key, State1);
        false ->
            fail_waiters(Key, Reason, State1)
    end,
    State3 = dispatch_workers(State2),
    {noreply, State3};

handle_cast(_Unknown, State) ->
    {noreply, State}.

handle_info({'DOWN', MonRef, process, Pid, _Reason}, State) ->
    case find_request_by_mon(MonRef, State) of
        {ok, Ref} ->
            {noreply, cleanup_request(Ref, State)};
        not_found ->
            case find_inflight_by_pid(Pid, State) of
                {ok, Key} ->
                    gen_server:cast(self(), {chunk_failed, element(1, Key),
                        element(2, Key), worker_crashed}),
                    {noreply, State};
                not_found ->
                    {noreply, State}
            end
    end;

handle_info(_Unknown, State) ->
    {noreply, State}.

terminate(_Reason, #state{inflight = Inflight, requests = Requests}) ->
    %% Kill orphan workers so they don't keep fetching after we die.
    maps:foreach(
        fun(_Key, Pid) -> exit(Pid, shutdown) end,
        Inflight
    ),
    %% Notify all blocked callers so they don't hang forever.
    maps:foreach(
        fun(_Ref, #request{caller = Caller, ref = R}) ->
            Caller ! {chunk_stream_ready, R, {error, shutting_down}}
        end,
        Requests
    ),
    ok.

%% -- Internal: register offsets --

%% @doc Register offsets for dedup and waiter tracking. Offsets already
%% stored are immediately marked done. The actual dispatch order is
%% controlled by the round-robin rotation, not a priority queue.
register_offsets([], _SId, _Ref, State) ->
    State;
register_offsets([Offset | Rest], SId, Ref, State) ->
    Key = {Offset, SId},
    State1 = case maps:get(Key, State#state.offset_status, undefined) of
        stored ->
            mark_offset_done(Ref, Offset, State);
        undefined ->
            State#state{
                offset_status = maps:put(Key, pending,
                    State#state.offset_status),
                offset_waiters = maps:update_with(Key,
                    fun(Ws) -> [Ref | Ws] end, [Ref],
                    State#state.offset_waiters)
            };
        _PendingOrFetching ->
            State#state{
                offset_waiters = maps:update_with(Key,
                    fun(Ws) -> [Ref | Ws] end, [Ref],
                    State#state.offset_waiters)
            }
    end,
    register_offsets(Rest, SId, Ref, State1).

%% -- Internal: dispatch (round-robin) --

dispatch_workers(#state{active_workers = Active, max_workers = Max} = State)
        when Active >= Max ->
    State;
dispatch_workers(State) ->
    case pick_next_offset(State, queue:len(State#state.rotation)) of
        {none, State1} ->
            State1;
        {Offset, SId, Opts, State1} ->
            Key = {Offset, SId},
            Store = resolve_store(Opts),
            Server = self(),
            WorkerPid = spawn(
                fun() -> worker_run(Offset, SId, Store, Opts, Server) end
            ),
            monitor(process, WorkerPid),
            State2 = State1#state{
                offset_status = maps:put(Key, fetching,
                    State1#state.offset_status),
                inflight = maps:put(Key, WorkerPid,
                    State1#state.inflight),
                active_workers = State1#state.active_workers + 1
            },
            dispatch_workers(State2)
    end.

%% @doc Pick the next offset to dispatch using round-robin across
%% active requests. Each call rotates to the next request and pops
%% one offset from its pending list. Skips requests with empty
%% pending lists and offsets already stored/fetching.
pick_next_offset(_State, 0) ->
    {none, _State};
pick_next_offset(State, Tries) ->
    case queue:out(State#state.rotation) of
        {empty, _} ->
            {none, State};
        {{value, Ref}, Rest} ->
            case maps:get(Ref, State#state.requests, undefined) of
                undefined ->
                    pick_next_offset(State#state{rotation = Rest},
                        Tries - 1);
                #request{pending = []} ->
                    pick_next_offset(
                        State#state{rotation = queue:in(Ref, Rest)},
                        Tries - 1);
                Req = #request{pending = [Offset | Tail]} ->
                    Key = {Offset, Req#request.store_id},
                    Req1 = Req#request{pending = Tail},
                    State1 = State#state{
                        requests = maps:put(Ref, Req1,
                            State#state.requests),
                        rotation = queue:in(Ref, Rest)
                    },
                    case maps:get(Key, State1#state.offset_status,
                        undefined) of
                        pending ->
                            {Offset, Req#request.store_id,
                                Req#request.opts, State1};
                        _ ->
                            pick_next_offset(State1, Tries)
                    end
            end
    end.

%% -- Internal: worker --

%% @doc Worker process: fetch chunk JSON, write to store, notify server.
%% Wrapped in try/catch so a malformed response or store error sends
%% chunk_failed instead of crashing (which would orphan the offset).
worker_run(Offset, StoreId, _Store, Opts, Server) ->
    try
        case dev_arweave:get_chunk(Offset, Opts) of
            {ok, JSON} ->
                put(Offset, JSON, Opts),
                AbsEnd = hb_util:int(
                    maps:get(<<"absolute_end_offset">>, JSON)),
                Chunk = hb_util:decode(maps:get(<<"chunk">>, JSON)),
                AbsStart = AbsEnd - byte_size(Chunk) + 1,
                gen_server:cast(Server,
                    {chunk_stored, Offset, StoreId, AbsStart, AbsEnd});
            {error, Reason} ->
                gen_server:cast(Server,
                    {chunk_failed, Offset, StoreId, Reason})
        end
    catch Class:Reason2 ->
        gen_server:cast(Server,
            {chunk_failed, Offset, StoreId, {Class, Reason2}})
    end.

%% -- Internal: notification --

%% @doc Check if a request has all offsets done and notify caller.
maybe_notify(Ref, State) ->
    case maps:get(Ref, State#state.requests, undefined) of
        undefined ->
            State;
        #request{remaining = Rem, caller = Caller} ->
            case sets:is_empty(Rem) of
                true ->
                    Caller ! {chunk_stream_ready, Ref, ok},
                    cleanup_request(Ref, State);
                false ->
                    State
            end
    end.

%% @doc Mark one offset as done in a request's remaining set.
mark_offset_done(Ref, Offset, State) ->
    case maps:get(Ref, State#state.requests, undefined) of
        undefined ->
            State;
        Req = #request{remaining = Rem} ->
            Req1 = Req#request{remaining = sets:del_element(Offset, Rem)},
            State1 = State#state{
                requests = maps:put(Ref, Req1, State#state.requests)
            },
            maybe_notify(Ref, State1)
    end.

%% @doc Notify all waiters of a failed offset and clean up.
fail_waiters(Key, Reason, State) ->
    Waiters = maps:get(Key, State#state.offset_waiters, []),
    lists:foreach(
        fun(Ref) ->
            case maps:get(Ref, State#state.requests, undefined) of
                undefined -> ok;
                #request{caller = Caller} ->
                    Caller ! {chunk_stream_ready, Ref, {error, Reason}}
            end
        end,
        Waiters
    ),
    State1 = lists:foldl(fun(Ref, S) -> cleanup_request(Ref, S) end,
        State, Waiters),
    State1#state{
        offset_status = maps:remove(Key, State1#state.offset_status),
        offset_waiters = maps:remove(Key, State1#state.offset_waiters),
        retries = maps:remove(Key, State1#state.retries)
    }.

%% -- Internal: cleanup --

cleanup_request(Ref, State) ->
    case maps:get(Ref, State#state.requests, undefined) of
        undefined ->
            State;
        #request{mon = Mon, store_id = SId, remaining = Rem} ->
            demonitor(Mon, [flush]),
            State1 = State#state{
                requests = maps:remove(Ref, State#state.requests)
            },
            remove_waiter_refs(Ref, SId, sets:to_list(Rem), State1)
    end.

%% @doc Remove a request's Ref from offset_waiters. If an offset has
%% no remaining waiters, remove it from status/retries maps. Stale
%% queue entries are left in gb_trees and skipped by dispatch_workers.
remove_waiter_refs(_Ref, _SId, [], State) ->
    State;
remove_waiter_refs(Ref, SId, [Offset | Rest], State) ->
    Key = {Offset, SId},
    OldWaiters = maps:get(Key, State#state.offset_waiters, []),
    NewWaiters = lists:delete(Ref, OldWaiters),
    State1 = case NewWaiters of
        [] ->
            State#state{
                offset_waiters = maps:remove(Key, State#state.offset_waiters),
                offset_status = maps:remove(Key, State#state.offset_status),
                retries = maps:remove(Key, State#state.retries)
            };
        _ ->
            State#state{
                offset_waiters = maps:put(Key, NewWaiters,
                    State#state.offset_waiters)
            }
    end,
    remove_waiter_refs(Ref, SId, Rest, State1).

%% -- Internal: store --

resolve_store(Opts) ->
    hb_opts:get(chunk_stream_store, default_store(), Opts).

default_store() ->
    #{
        <<"store-module">> => hb_store_fs,
        <<"name">> => <<"cache-chunks">>
    }.

store_id(Opts) ->
    Store = resolve_store(Opts),
    Mod = maps:get(<<"store-module">>, Store, undefined),
    Name = maps:get(<<"name">>, Store, <<>>),
    {Mod, Name}.

%% @doc Store key for a chunk by offset. We use the raw offset as key —
%% each chunk is stored under both its query offset and its AbsEnd to
%% enable lookups by any known offset.
chunk_key(Offset) ->
    <<"chunk/", (integer_to_binary(Offset))/binary>>.

%% @doc Write a chunk to the store under both the query offset and AbsEnd
%% keys. This enables lookups by either the original query offset or the
%% canonical AbsEnd. The value encodes AbsStart and AbsEnd alongside the
%% binary so a single read retrieves everything needed.
write_chunk_info(QueryOffset, AbsStart, AbsEnd, Binary, Store) ->
    Value = <<AbsStart:64/big, AbsEnd:64/big, Binary/binary>>,
    hb_store:write(Store, chunk_key(QueryOffset), Value),
    case QueryOffset =:= AbsEnd of
        true -> ok;
        false -> hb_store:write(Store, chunk_key(AbsEnd), Value)
    end.

%% @doc Read a chunk from the store by any offset within its range.
%% Normalizes to the bucket key, then decodes the stored metadata.
read_chunk_info(Offset, Store) ->
    case hb_store:read(Store, chunk_key(Offset)) of
        not_found ->
            not_found;
        {ok, <<AbsStart:64/big, AbsEnd:64/big, Binary/binary>>} ->
            {ok, {AbsStart, AbsEnd, Binary}};
        {ok, _Malformed} ->
            not_found
    end.

%% -- Internal: streaming --

build_stream([], _Store) ->
    fun() -> done end;
build_stream([Offset | Rest], Store) ->
    fun() ->
        {ok, ChunkInfo} = read_chunk_info(Offset, Store),
        {ChunkInfo, build_stream(Rest, Store)}
    end.

%% @doc Re-add a failed offset to the first waiter request's pending
%% list so it gets retried in the next round-robin cycle.
requeue_offset(Offset, _StoreId, Key, State) ->
    Retries = maps:get(Key, State#state.retries, 0),
    Waiters = maps:get(Key, State#state.offset_waiters, []),
    State1 = State#state{
        offset_status = maps:put(Key, pending, State#state.offset_status),
        retries = maps:put(Key, Retries + 1, State#state.retries)
    },
    case Waiters of
        [FirstRef | _] ->
            case maps:get(FirstRef, State1#state.requests, undefined) of
                undefined -> State1;
                Req ->
                    State1#state{
                        requests = maps:put(FirstRef,
                            Req#request{
                                pending = Req#request.pending ++ [Offset]
                            },
                            State1#state.requests)
                    }
            end;
        [] -> State1
    end.

%% -- Internal: lookup helpers --

find_request_by_mon(MonRef, #state{requests = Reqs}) ->
    maps:fold(
        fun(Ref, #request{mon = M}, not_found) when M =:= MonRef ->
                {ok, Ref};
            (_, _, Acc) ->
                Acc
        end,
        not_found,
        Reqs
    ).

find_inflight_by_pid(Pid, #state{inflight = Inflight}) ->
    maps:fold(
        fun(Key, P, not_found) when P =:= Pid -> {ok, Key};
            (_, _, Acc) -> Acc
        end,
        not_found,
        Inflight
    ).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

test_store() ->
    #{
        <<"store-module">> => hb_store_fs,
        <<"name">> => <<"cache-chunks-test">>
    }.

test_opts() ->
    #{chunk_stream_store => test_store()}.

make_chunk_json(AbsEnd, Chunk) ->
    #{
        <<"chunk">> => hb_util:encode(Chunk),
        <<"absolute_end_offset">> => AbsEnd
    }.

%% @doc Basic put/get roundtrip via the store.
put_get_test() ->
    Opts = test_opts(),
    Store = resolve_store(Opts),
    hb_store:start(Store),
    Chunk = crypto:strong_rand_bytes(1024),
    AbsEnd = 385455258378486,
    AbsStart = AbsEnd - byte_size(Chunk) + 1,
    JSON = make_chunk_json(AbsEnd, Chunk),
    ok = ?MODULE:put(AbsEnd, JSON, Opts),
    {ok, {AbsStart, AbsEnd, Chunk}} = ?MODULE:get(AbsEnd, Opts),
    hb_store:reset(Store).

%% @doc get returns not_found for missing chunks.
get_not_found_test() ->
    Opts = test_opts(),
    Store = resolve_store(Opts),
    hb_store:start(Store),
    not_found = ?MODULE:get(99999, Opts),
    hb_store:reset(Store).

%% @doc Chunk is readable by both query offset and AbsEnd.
dual_key_test() ->
    Opts = test_opts(),
    Store = resolve_store(Opts),
    hb_store:start(Store),
    Chunk = crypto:strong_rand_bytes(256),
    AbsEnd = 385455258262143,
    AbsStart = AbsEnd - byte_size(Chunk) + 1,
    QueryOffset = 385455258116343,
    JSON = make_chunk_json(AbsEnd, Chunk),
    ok = ?MODULE:put(QueryOffset, JSON, Opts),
    {ok, {AbsStart, AbsEnd, Chunk}} = ?MODULE:get(QueryOffset, Opts),
    {ok, {AbsStart, AbsEnd, Chunk}} = ?MODULE:get(AbsEnd, Opts),
    hb_store:reset(Store).

%% @doc Same AbsEnd from different query offsets shares entry.
dedup_by_absend_test() ->
    Opts = test_opts(),
    Store = resolve_store(Opts),
    hb_store:start(Store),
    Chunk = crypto:strong_rand_bytes(256),
    AbsEnd = 1262143,
    AbsStart = AbsEnd - byte_size(Chunk) + 1,
    JSON = make_chunk_json(AbsEnd, Chunk),
    ok = ?MODULE:put(1000001, JSON, Opts),
    ok = ?MODULE:put(1000002, JSON, Opts),
    {ok, {AbsStart, AbsEnd, Chunk}} = ?MODULE:get(1000001, Opts),
    {ok, {AbsStart, AbsEnd, Chunk}} = ?MODULE:get(1000002, Opts),
    {ok, {AbsStart, AbsEnd, Chunk}} = ?MODULE:get(AbsEnd, Opts),
    hb_store:reset(Store).

%% @doc has_stream returns true only when all offsets are present.
has_stream_test() ->
    Opts = test_opts(),
    Store = resolve_store(Opts),
    hb_store:start(Store),
    ok = ?MODULE:put(262143, make_chunk_json(262143, <<"a">>), Opts),
    ok = ?MODULE:put(524287, make_chunk_json(524287, <<"b">>), Opts),
    ?assert(?MODULE:has_stream([262143, 524287], Opts)),
    ?assertNot(?MODULE:has_stream([262143, 524287, 999999], Opts)),
    hb_store:reset(Store).

%% @doc get_stream yields chunks one at a time then done.
get_stream_test() ->
    Opts = test_opts(),
    Store = resolve_store(Opts),
    hb_store:start(Store),
    ok = ?MODULE:put(262143, make_chunk_json(262143, <<"a">>), Opts),
    ok = ?MODULE:put(524287, make_chunk_json(524287, <<"b">>), Opts),
    {ok, Fun} = ?MODULE:get_stream([262143, 524287], Opts),
    {{262143, 262143, <<"a">>}, Fun2} = Fun(),
    {{524287, 524287, <<"b">>}, Fun3} = Fun2(),
    done = Fun3(),
    hb_store:reset(Store).

%% @doc Malformed values return not_found, not crash.
partial_write_test() ->
    Opts = test_opts(),
    Store = resolve_store(Opts),
    hb_store:start(Store),
    hb_store:write(Store, chunk_key(42), <<"too_short">>),
    not_found = ?MODULE:get(42, Opts),
    hb_store:reset(Store).

%% @doc ensure_chunks monitor detects server death.
ensure_chunks_monitor_test() ->
    {ok, Pid} = gen_server:start({local, test_chunk_stream},
        ?MODULE, #{}, []),
    Ref = make_ref(),
    Self = self(),
    spawn(fun() ->
        Mon = monitor(process, Pid),
        gen_server:cast(Pid,
            {ensure, [1], test_opts(), self(), Ref}),
        exit(Pid, kill),
        Result = receive
            {chunk_stream_ready, Ref, R} -> R;
            {'DOWN', Mon, process, _, Reason} ->
                {error, {chunk_stream_down, Reason}}
        after 2000 -> timeout
        end,
        Self ! {Ref, Result}
    end),
    receive
        {Ref, {error, {chunk_stream_down, _}}} -> ok
    after 5000 -> ?assert(false)
    end.

%% @doc Round-robin serves requests fairly.
round_robin_ordering_test() ->
    Ref1 = make_ref(),
    Ref2 = make_ref(),
    SId = {hb_store_fs, <<"test">>},
    Req1 = #request{ref = Ref1, caller = self(), mon = make_ref(),
        store_id = SId, opts = #{}, pending = [100, 200],
        remaining = sets:from_list([100, 200])},
    Req2 = #request{ref = Ref2, caller = self(), mon = make_ref(),
        store_id = SId, opts = #{}, pending = [300, 400],
        remaining = sets:from_list([300, 400])},
    State0 = #state{
        requests = #{Ref1 => Req1, Ref2 => Req2},
        rotation = queue:from_list([Ref1, Ref2]),
        offset_status = #{
            {100, SId} => pending, {200, SId} => pending,
            {300, SId} => pending, {400, SId} => pending
        },
        offset_waiters = #{},
        inflight = #{}, retries = #{},
        max_workers = 10, active_workers = 0
    },
    {First, _, _, State1} = pick_next_offset(State0, 2),
    {Second, _, _, _} = pick_next_offset(State1, 2),
    ?assertEqual(100, First),
    ?assertEqual(300, Second).

%% @doc Caller death removes request from state.
caller_death_cleanup_test() ->
    Opts = test_opts(),
    Store = resolve_store(Opts),
    hb_store:start(Store),
    Ref = make_ref(),
    SId = store_id(Opts),
    Mon = make_ref(),
    Req = #request{
        ref = Ref,
        caller = self(),
        mon = Mon,
        store_id = SId,
        opts = Opts,
        pending = [100, 200],
        remaining = sets:from_list([100, 200])
    },
    State0 = #state{
        requests = #{Ref => Req},
        rotation = queue:from_list([Ref]),
        offset_status = #{
            {100, SId} => pending,
            {200, SId} => pending
        },
        offset_waiters = #{
            {100, SId} => [Ref],
            {200, SId} => [Ref]
        },
        inflight = #{},
        retries = #{},
        max_workers = 10,
        active_workers = 0
    },
    State1 = cleanup_request(Ref, State0),
    ?assertEqual(#{}, State1#state.requests),
    ?assertEqual(#{}, State1#state.offset_waiters),
    ?assertEqual(#{}, State1#state.offset_status),
    hb_store:reset(Store).

-endif.
