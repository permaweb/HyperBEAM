%%% @doc A scheduler for AO processes that uses the Arweave network itself
%%% as the sequencing layer. The device implements the same interface as
%%% `~scheduler@1.0' from the perspective of consumers (`~process@1.0' et
%%% al), but assignments are not minted by a scheduling authority: they are
%%% implicit in the order that Arweave blocks include L1 transactions that
%%% target the process.
%%%
%%% The model is as follows:
%%% <ul>
%%%   <li>A process is spawned by uploading its process message to Arweave
%%%       as a `tx@1.0'-committed L1 transaction. The transaction ID is the
%%%       process ID, and the process message occupies slot 0 of its own
%%%       schedule.</li>
%%%   <li>Every L1 transaction whose `target' field is the process ID is a
%%%       message in the process's schedule. Slots follow the canonical weave
%%%       order (ascending weave offset, which is ascending block order). The
%%%       transaction's weave `offset' -- not a scheduler-assigned nonce -- is
%%%       its on-chain position, and it is recorded on the assignment.</li>
%%%   <li>Discovery is <em>local-first</em>: the node indexes the relevant
%%%       blocks itself with `~copycat@1.0' and then queries its own
%%%       `~query@1.0' GraphQL endpoint (`transactions(recipients:
%%%       [ProcessID], sort: HEIGHT_ASC)') for the base-layer transactions
%%%       addressed to the process, ordered by the local weave-offset index.
%%%       A node may instead be configured to query a remote gateway
%%%       (`arweave_scheduler_query_source => remote'). There is no bespoke
%%%       store index -- discovery uses the node's existing Arweave index and
%%%       the device's own schedule cache.</li>
%%% </ul>
%%%
%%% A process message may widen what it is sequenced by, with a
%%% `scheduler-mode' key:
%%% <ul>
%%%   <li>`target' (the default): the process's messages are the transactions
%%%       addressed to it, as above.</li>
%%%   <li>`all': every data-free base-layer transaction is a message in the
%%%       process's schedule, whoever it is addressed to. This is what lets a
%%%       process observe value moving between two other addresses -- a
%%%       payment it is owed but is not a party to. Its schedule is enumerated
%%%       from the block headers rather than by query, in canonical chain
%%%       order (blocks ascending, then each block's own transaction order),
%%%       and each assignment records the `block-height' that sequenced it, so
%%%       a process in this mode has a clock. The process's own transaction is
%%%       not re-assigned: it remains slot 0 alone.</li>
%%% </ul>
%%% The mode is read from the process message itself -- an L1 transaction --
%%% so it is chain data like the rest of the schedule, and is fixed for the
%%% life of the process.
%%%
%%% Assignment <em>bodies are transaction headers only</em>: each is the
%%% data-free header that `~copycat@1.0/arweave' cached while indexing the
%%% range (read from the node's stores), so the schedule never carries (nor
%%% depends on the availability of) a transaction's data. A header still
%%% carries the tx@1.0 signature, so its committed ID is the transaction ID
%%% and it verifies independently. Assignments are
%%% deterministic derivations of chain data and are left uncommitted: every
%%% node that reads the same blocks converges on an identical schedule
%%% without trusting a scheduler wallet. Only `tx@1.0' commitments are
%%% accepted for dispatch -- ANS-104 data items and HTTPSig messages cannot
%%% be sequenced by the base layer.
%%%
%%% `POST /schedule' dispatches a user's presigned transaction to the
%%% network on their behalf. Only its header is relayed: because the schedule
%%% is built from headers, a message carrying data is rejected rather than
%%% dispatched, so its slot can never depend on chunk availability. No slot is
%%% returned at dispatch time: the transaction receives its slot once it is
%%% included in a block. Note
%%% that Arweave refuses to create accounts with a zero balance
%%% (`validate_overspend'), and a process ID is a fresh account: the first
%%% message addressed to a process must therefore carry a `quantity' of at
%%% least 1 winston (and, until the account exists, the sender's `reward'
%%% must cover the network's new-account fee).
%%%
%%% Each process's synchronization tracks the contiguous block range
%%% `[spawn-height, synced-to]' it has indexed -- the range the node can
%%% attest to. A sync only indexes the blocks above `synced-to' (up to the
%%% confirmed tip), so a repeated read with no new blocks does no network
%%% work, and `/status' reports each tracked process's range. The device
%%% honors the following node options:
%%% <ul>
%%%   <li>`arweave_scheduler_confirmation_depth': blocks under the network
%%%       tip that must elapse before a block is considered final
%%%       (default: 10).</li>
%%%   <li>`arweave_scheduler_sync_chunk': blocks indexed per synchronization
%%%       pass, bounding the work redone if a pass is interrupted
%%%       (default: 1000).</li>
%%%   <li>`arweave_scheduler_max_height': optional hard upper bound on the
%%%       synced height, pinning a schedule to an immutable block range
%%%       (used primarily in tests).</li>
%%% </ul>
-module(dev_arweave_scheduler).
-implements(<<"arweave-scheduler@1.0">>).
-device_libraries([lib_process, lib_scheduler, lib_arweave_common]).
%%% AO-Core API functions:
-export([info/0]).
%%% Scheduling functions:
-export([schedule/3, router/4]).
%%% CU-flow functions:
-export([slot/3, status/3, next/3]).
-export([checkpoint/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The default number of blocks beneath the network tip at which we
%%% consider a block final.
-define(DEFAULT_CONFIRMATION_DEPTH, 10).
%%% The default number of blocks indexed per synchronization pass. A sync
%%% persists its progress after each chunk, so this bounds the work redone if
%%% a pass is interrupted and keeps the first sync of a long-lived process
%%% resumable. Overridable with the `arweave_scheduler_sync_chunk' option.
-define(DEFAULT_SYNC_CHUNK_BLOCKS, 1000).
%%% The number of transactions requested per query page.
-define(QUERY_PAGE_SIZE, 100).
%%% The Arweave device that we resolve chain data through.
-define(ARWEAVE_DEVICE, <<"~arweave@2.9">>).
%%% The sequencing mode of a process whose message does not name one: only the
%%% transactions addressed to it are its messages.
-define(DEFAULT_MODE, <<"target">>).

%% @doc This device uses a default handler to route requests to the correct
%% function.
info() ->
    #{
        exports =>
            [
                <<"status">>,
                <<"next">>,
                <<"schedule">>,
                <<"slot">>,
                <<"init">>,
                <<"checkpoint">>
            ],
        excludes => [set, keys],
        default => fun router/4
    }.

%% @doc The default handler for the device: route all unrecognized requests
%% to `schedule'.
router(_, Base, Req, Opts) ->
    ?event({arweave_scheduler_router_called, {req, Req}}),
    schedule(Base, Req, Opts).

%% @doc Return the next assignment for a process. Assumes that `Base' is a
%% `dev_process' or similar message, having an `at-slot' key. If the next
%% slot is not present in the local cache, the schedule is synchronized
%% from Arweave before the read is retried.
next(Base, Req, Opts) ->
    ProcID = lib_process:process_id(Base, Req, Opts),
    LastProcessed = lib_scheduler:at_slot(Base, Opts),
    ?event(next, {arweave_next, {proc_id, ProcID}, {last, LastProcessed}}),
    maybe
        {ok, Assignment} ?= find_assignment(ProcID, LastProcessed + 1, Opts),
        {ok, #{ <<"body">> => Assignment, <<"state">> => Base }}
    end.

%% @doc Read an assignment from the local cache, synchronizing the schedule
%% from Arweave if it is not yet present.
find_assignment(ProcID, Slot, Opts) ->
    case dev_arweave_scheduler_cache:read(ProcID, Slot, Opts) of
        {ok, Assignment} -> {ok, Assignment};
        not_found ->
            maybe
                {ok, _} ?= sync(ProcID, Opts),
                case dev_arweave_scheduler_cache:read(ProcID, Slot, Opts) of
                    {ok, Assignment} -> {ok, Assignment};
                    not_found -> lib_scheduler:slot_unavailable()
                end
            end
    end.

%% @doc A router for choosing between getting the existing schedule, or
%% dispatching a new message to Arweave.
schedule(Base, Req, Opts) ->
    ?event({resolving_arweave_schedule_request, {req, Req}}),
    case hb_util:key_to_atom(hb_ao:get(<<"method">>, Req, <<"GET">>, Opts)) of
        post -> post_schedule(Base, Req, Opts);
        get -> get_schedule(Base, Req, Opts)
    end.

%% @doc Generate and return the schedule for a process, optionally between
%% two slots -- labelled as `from' and `to'.
get_schedule(Base, Req, Opts) ->
    ProcID = hb_util:human_id(lib_scheduler:find_target_id(Base, Req, Opts)),
    {From, To} = lib_scheduler:parse_slot_range(Req, Opts),
    maybe
        {ok, #{ <<"next-slot">> := NextSlot }} ?= sync(ProcID, Opts),
        Latest = NextSlot - 1,
        RequestedTo =
            case To of
                undefined -> Latest;
                _ -> min(To, Latest)
            end,
        {Assignments, More} =
            lib_scheduler:read_assignment_range(
                dev_arweave_scheduler_cache, ProcID, From, RequestedTo, Opts),
        dev_arweave_scheduler_cache:assignments_to_bundle(
            ProcID,
            Assignments,
            More,
            Opts
        )
    end.

%% @doc Dispatch a new message to Arweave. The message must carry a signed
%% `tx@1.0' commitment: the caller signs the L1 transaction themselves
%% (including its `anchor' and `reward'), and this device relays it to the
%% network. The message is also written to the local cache, so that it is
%% servable by ID before the network has propagated and indexed it.
post_schedule(Base, Req, Opts) ->
    maybe
        {ok, ToSched} ?= lib_scheduler:load_message_to_schedule(Base, Req, Opts),
        do_post_schedule(Base, Req, ToSched, Opts)
    end.

do_post_schedule(Base, Req, ToSched, Opts) ->
    ProcID = lib_scheduler:find_target_id(Base, Req, ToSched, Opts),
    ?event({arweave_post_schedule, {proc_id, ProcID}}),
    maybe
        {ok, OnlyCommitted} ?= lib_scheduler:only_committed(ToSched, Opts),
        ok ?= ensure_tx_committed(OnlyCommitted, Opts),
        dispatch(ProcID, OnlyCommitted, Opts)
    end.

%% @doc Ensure that the given message carries a signed, valid `tx@1.0'
%% commitment. The Arweave scheduler accepts only L1 transactions: ANS-104
%% and HTTPSig commitments cannot be sequenced by the base layer.
ensure_tx_committed(Msg, Opts) ->
    Devices = hb_message:commitment_devices(Msg, Opts),
    Signers = hb_message:signers(Msg, Opts),
    case {lists:member(<<"tx@1.0">>, Devices), Signers} of
        {false, _} ->
            {error,
                #{
                    <<"status">> => 422,
                    <<"body">> =>
                        <<"The Arweave scheduler only accepts messages ",
                            "committed with tx@1.0.">>
                }
            };
        {true, []} ->
            {error,
                #{
                    <<"status">> => 422,
                    <<"body">> => <<"Message must be signed.">>
                }
            };
        {true, _} ->
            case hb_message:verify(Msg, signers, Opts) of
                true -> ok;
                false ->
                    {error,
                        #{
                            <<"status">> => 400,
                            <<"body">> => <<"Message is not valid.">>
                        }
                    }
            end
    end.

%% @doc Relay a committed `tx@1.0' message to the Arweave network. Only the
%% transaction header is dispatched: this scheduler sequences headers, never
%% data. A message carrying data is therefore rejected rather than uploaded --
%% its chunks might be unavailable when the schedule is later read. The same
%% header-only rule governs synchronization, so a data-carrying transaction
%% posted directly to Arweave is still sequenced safely, by its header alone.
dispatch(ProcID, Msg, Opts) ->
    TX = hb_message:convert(Msg, <<"tx@1.0">>, Opts),
    case TX#tx.data of
        <<>> -> dispatch_header(ProcID, TX, Msg, Opts);
        _ ->
            {error,
                #{
                    <<"status">> => 422,
                    <<"body">> =>
                        <<"The Arweave scheduler sequences transaction ",
                            "headers only; a message carrying data cannot ",
                            "be scheduled.">>
                }
            }
    end.

dispatch_header(ProcID, TX, Msg, Opts) ->
    TXID = hb_util:human_id(TX#tx.id),
    ?event({dispatching_tx, {id, {explicit, TXID}}, {proc_id, ProcID}}),
    maybe
        {ok, _} ?= post_tx_header(TX, Opts),
        {ok, _} = hb_cache:write(Msg, Opts),
        % `tx-id' rather than `id': the reserved `id' key would be shadowed
        % by the receipt message's own ID when read by consumers.
        {ok,
            #{
                <<"status">> => 202,
                <<"tx-id">> => TXID,
                <<"process">> => hb_util:human_id(ProcID),
                <<"body">> =>
                    <<"Transaction dispatched to Arweave. It will receive ",
                        "a slot in the schedule once mined.">>
            }
        }
    end.

%% @doc Post a transaction header to the network, via the node's `/arweave'
%% route. The route may broadcast to multiple nodes: acceptance by any one
%% of them places the transaction in the network's mempool, so the best
%% response wins, mirroring `~arweave@2.9's handling.
post_tx_header(TX, Opts) ->
    Res =
        hb_http:request(
            #{
                <<"method">> => <<"POST">>,
                <<"path">> => <<"/arweave/tx">>,
                <<"body">> => hb_json:encode(ar_tx:tx_to_json_struct(TX))
            },
            no_result_cache(Opts)
        ),
    lib_arweave_common:best_response(Res).

%% @doc Returns information about the current slot for a process.
slot(Base, Req, Opts) ->
    ProcID = hb_util:human_id(lib_scheduler:find_target_id(Base, Req, Opts)),
    ?event({getting_current_slot, {proc_id, ProcID}}),
    maybe
        {ok, #{ <<"next-slot">> := NextSlot }} ?= sync(ProcID, Opts),
        {ok,
            #{
                <<"process">> => ProcID,
                <<"current">> => NextSlot - 1,
                <<"cache-control">> => <<"no-store">>
            }
        }
    end.

%% @doc Returns information about the scheduler, including -- for each process
%% it tracks -- the contiguous block range it has indexed and can attest to.
status(_Base, _Req, Opts) ->
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    ProcIDs = dev_arweave_scheduler_cache:list_processes(Opts),
    {ok,
        #{
            <<"address">> => hb_util:human_id(ar_wallet:to_address(Wallet)),
            <<"processes">> =>
                hb_maps:from_list([ {P, attestable_range(P, Opts)} || P <- ProcIDs ]),
            <<"cache-control">> => <<"no-store">>
        }
    }.

%% @doc The block range a process has been indexed over, as it currently stands
%% in the cache (no synchronization is triggered). `from' is the spawn block and
%% `to' is the confirmed height up to which the schedule is complete; `current'
%% is the latest materialized slot.
attestable_range(ProcID, Opts) ->
    case dev_arweave_scheduler_cache:read_state(ProcID, Opts) of
        {ok, #{
            <<"spawn-height">> := Spawn,
            <<"synced-to">> := SyncedTo,
            <<"next-slot">> := NextSlot
        }} ->
            #{
                <<"from">> => Spawn,
                <<"to">> => SyncedTo,
                <<"current">> => NextSlot - 1
            };
        _ -> #{}
    end.

%% @doc Returns the current state of the scheduler.
checkpoint(State) -> {ok, State}.

%%% Synchronization

%% @doc Synchronize the schedule of a process from Arweave, extending the
%% contiguously-indexed block range `[spawn-height, synced-to]' up to the
%% confirmed network tip. Only the blocks above `synced-to' are indexed; a
%% process already synced to the tip does no network work at all.
sync(ProcID, Opts) ->
    maybe
        {ok, State} ?= ensure_initialized(ProcID, Opts),
        {ok, Upper} ?= confirmed_tip(Opts),
        do_sync(ProcID, State, Upper, Opts)
    end.

%% @doc Extend the synced range up to `Upper', one bounded chunk at a time. The
%% range grows contiguously from `synced-to + 1', and `{next-slot, synced-to}'
%% are persisted together after each chunk, so an interrupted sync resumes at
%% the start of the unfinished chunk (re-materializing is idempotent) rather
%% than re-deriving the whole history.
do_sync(_ProcID, State = #{ <<"synced-to">> := SyncedTo }, Upper, _Opts)
        when SyncedTo >= Upper ->
    {ok, State};
do_sync(ProcID, State = #{ <<"synced-to">> := SyncedTo, <<"mode">> := Mode },
        Upper, Opts) ->
    From = SyncedTo + 1,
    To = min(SyncedTo + sync_chunk_blocks(Opts), Upper),
    ?event(
        {arweave_scheduler_sync,
            {proc_id, ProcID},
            {mode, Mode},
            {from, From},
            {to, To},
            {target, Upper}
        }
    ),
    maybe
        {ok, Ordered} ?= discover(ProcID, Mode, From, To, Opts),
        {ok, NewState} ?= materialize(ProcID, State, Ordered, To, Opts),
        do_sync(ProcID, NewState, Upper, Opts)
    end.

%% @doc The number of blocks to index per synchronization pass.
sync_chunk_blocks(Opts) ->
    hb_util:int(
        hb_opts:get(arweave_scheduler_sync_chunk, ?DEFAULT_SYNC_CHUNK_BLOCKS, Opts)
    ).

%% @doc Side-effecting and mutable internal resolutions must never be served
%% from (or written to) the resolution cache: nodes default to caching every
%% HTTP resolution, which would otherwise freeze the indexing runs, queries
%% and tip lookups at their first result.
no_result_cache(Opts) ->
    Opts#{
        <<"hashpath">> => ignore,
        <<"cache-control">> => [<<"no-cache">>, <<"no-store">>]
    }.

%% @doc Discover the base-layer transactions that a process is sequenced by
%% within a block range, in canonical weave order, entirely from the node's own
%% index. `target' mode records each transaction's weave offset and caches its
%% header. `all' mode caches only data-free headers.
%%
%% In `target' mode the recipient match is served from the node's own
%% `~query@1.0' GraphQL endpoint, and each match is annotated with its offset --
%% the sort key and the `offset' recorded on the assignment. No gateway is
%% queried by default. In `all' mode there is nothing to match, so no query is
%% run: the range's block headers are walked directly (see
%% `enumerate_blocks/4').
%%
%% Returns `{Extra, TXID}' pairs in the order they are to be assigned, where
%% `Extra' is the sequencing detail recorded on each assignment. Bundled data
%% items are excluded, as this scheduler sequences the base layer only.
discover(ProcID, <<"all">>, From, To, Opts) ->
    maybe
        ok ?= ensure_headers(From, To, Opts),
        {ok, Located} ?= enumerate_blocks(ProcID, From, To, Opts),
        base_layer_blocks(Located, Opts)
    end;
discover(ProcID, _Mode, From, To, Opts) ->
    maybe
        ok ?= ensure_offsets(From, To, Opts),
        {ok, IDs} ?= query_recipients(ProcID, From, To, Opts),
        {ok,
            [
                {#{ <<"offset">> => Offset }, TXID}
            ||
                {Offset, TXID} <- base_layer_offsets(IDs, Opts)
            ]
        }
    end.

%% @doc Enumerate every transaction in a block range from the block headers
%% themselves, in canonical chain order: blocks ascending by height, then each
%% block's transactions in the order that block lists them. This is the same
%% source `~copycat@1.0/arweave' walks while indexing, and `ensure_offsets' has
%% already cached the blocks locally, so the enumeration is normally a local
%% read. The process's own transaction is skipped: it is already slot 0.
%%
%% Returns `{Height, TXID}' pairs. Unlike a query result these are ordered by
%% construction, and that order is total -- weave offset is not, because every
%% transaction that carries no data shares the offset of the one before it.
enumerate_blocks(_ProcID, From, To, _Opts) when From > To -> {ok, []};
enumerate_blocks(ProcID, From, To, Opts) ->
    maybe
        {ok, Block} ?=
            hb_ao:resolve(
                <<?ARWEAVE_DEVICE/binary, "/block=", (hb_util:bin(From))/binary>>,
                no_result_cache(Opts)
            ),
        {ok, Rest} ?= enumerate_blocks(ProcID, From + 1, To, Opts),
        {ok,
            [
                {From, TXID}
            ||
                TXID <- hb_maps:get(<<"txs">>, Block, [], Opts),
                TXID =/= hb_util:human_id(ProcID)
            ] ++ Rest
        }
    end.

%% @doc Ensure every data-free transaction header in a block range is cached.
ensure_headers(From, To, _Opts) when From > To -> ok;
ensure_headers(From, To, Opts) ->
    maybe
        {ok, _} ?=
            hb_ao:resolve(
                <<
                    "~copycat@1.0/arweave&mode=headers&reindex=false",
                    "&from=", (hb_util:bin(To))/binary,
                    "&to=", (hb_util:bin(From))/binary
                >>,
                no_result_cache(Opts)
            ),
        ok ?= headers_indexed(From, To, Opts)
    end.

headers_indexed(From, To, Opts) ->
    #{ <<"index-store">> := Store } = hb_store_arweave:store_from_opts(Opts),
    Missing =
        lists:dropwhile(
            fun(Height) ->
                case hb_store:read(
                    Store,
                    <<"block/", (hb_util:bin(Height))/binary, "/mode">>,
                    Opts
                ) of
                    {ok, Mode} ->
                        lists:member(
                            Mode,
                            [<<"headers">>, <<"shallow">>, <<"deep">>, <<"full">>]
                        );
                    _ ->
                        false
                end
            end,
            lists:seq(From, To)
        ),
    case Missing of
        [] -> ok;
        [Height | _] ->
            {error,
                #{
                    <<"status">> => 503,
                    <<"reason">> =>
                        <<"Block range is not fully indexed locally.">>,
                    <<"block-height">> => Height
                }
            }
    end.

%% @doc Ensure the node's local Arweave index covers the block range, so that
%% every transaction in it has both a weave offset and a locally-cached header.
%% `~copycat@1.0/arweave' in `shallow' mode records an ID->offset entry for each
%% transaction and caches its (data-free) header -- populating the `field-target'
%% match index that `query_recipients' reads. `reindex=false' skips blocks the
%% node has already indexed (for any process), so the run is idempotent,
%% incremental, and shared: overlapping ranges across processes are not
%% re-fetched.
ensure_offsets(From, To, _Opts) when From > To -> ok;
ensure_offsets(From, To, Opts) ->
    maybe
        {ok, _} ?=
            hb_ao:resolve(
                <<
                    "~copycat@1.0/arweave&mode=shallow&reindex=false",
                    "&from=", (hb_util:bin(To))/binary,
                    "&to=", (hb_util:bin(From))/binary
                >>,
                no_result_cache(Opts)
            ),
        ok
    end.

%% @doc Query for the transactions addressed to a process within a block
%% range, ordered by weave position, returning their IDs. By default the query
%% is served by the node's own `~query@1.0' index (`local'), whose `field-target'
%% matches are populated by `~copycat@1.0/arweave' as it indexes the range (see
%% `ensure_offsets'); a node may instead be configured
%% (`arweave_scheduler_query_source => remote') to query a remote gateway.
query_recipients(ProcID, From, To, Opts) ->
    Source = hb_opts:get(arweave_scheduler_query_source, local, Opts),
    Query =
        <<
            "query($after: String) { transactions(",
                "recipients: [\"", (hb_util:human_id(ProcID))/binary, "\"], ",
                "block: { min: ", (hb_util:bin(From))/binary,
                    ", max: ", (hb_util:bin(To))/binary, " }, ",
                "sort: HEIGHT_ASC, ",
                "first: ", (hb_util:bin(?QUERY_PAGE_SIZE))/binary,
                ", after: $after",
            ") { pageInfo { hasNextPage } edges { cursor node { id } } } }"
        >>,
    query_pages(Source, Query, undefined, [], Opts).

query_pages(Source, Query, After, Acc, Opts) ->
    Variables =
        case After of
            undefined -> #{};
            _ -> #{ <<"after">> => After }
        end,
    maybe
        {ok, Transactions} ?= run_query(Source, Query, Variables, Opts),
        Edges = hb_maps:get(<<"edges">>, Transactions, [], Opts),
        IDs = Acc ++ [ edge_id(E, Opts) || E <- Edges ],
        HasNext =
            hb_util:atom(
                hb_ao:get(
                    <<"pageInfo/hasNextPage">>,
                    Transactions,
                    false,
                    Opts#{ <<"hashpath">> => ignore }
                )
            ),
        case {HasNext, Edges} of
            {true, [_ | _]} ->
                Cursor =
                    hb_maps:get(<<"cursor">>, lists:last(Edges), undefined, Opts),
                query_pages(Source, Query, Cursor, IDs, Opts);
            _ ->
                {ok, [ ID || ID <- IDs, is_binary(ID) ]}
        end
    end.

edge_id(Edge, Opts) ->
    hb_maps:get(<<"id">>, hb_maps:get(<<"node">>, Edge, #{}, Opts), undefined, Opts).

%% @doc Run a GraphQL `transactions' query, returning its connection. `local'
%% resolves the node's own `~query@1.0/graphql' endpoint; `remote' posts to
%% the configured `gateway' directly. The remote path deliberately does not use
%% `hb_client_gateway:query': its admissibility gate rejects legitimately-empty
%% ranges, and the racing `/graphql' route lets AO-search gateways that do not
%% index arbitrary L1 transactions win with empty-but-200 responses. Here an
%% empty range is an authoritative answer.
run_query(local, Query, Variables, Opts) ->
    to_transactions(
        hb_ao:resolve(
            #{ <<"device">> => <<"query@1.0">> },
            #{
                <<"path">> => <<"graphql">>,
                <<"method">> => <<"POST">>,
                <<"query">> => Query,
                <<"variables">> => Variables
            },
            no_result_cache(Opts)
        ),
        Opts
    );
run_query(remote, Query, Variables, Opts) ->
    Gateway = hb_opts:get(gateway, <<"https://arweave.net">>, Opts),
    to_transactions(
        hb_http:post(
            Gateway,
            #{
                <<"path">> => <<"/graphql">>,
                <<"content-type">> => <<"application/json">>,
                <<"body">> =>
                    hb_json:encode(
                        #{ <<"query">> => Query, <<"variables">> => Variables }
                    )
            },
            no_result_cache(Opts)
        ),
        Opts
    ).

to_transactions({ok, Response}, Opts) ->
    Decoded =
        hb_json:decode(hb_ao:get(<<"body">>, Response, <<"{}">>, Opts)),
    {ok,
        hb_ao:get(
            <<"data/transactions">>,
            Decoded,
            #{},
            Opts#{ <<"hashpath">> => ignore }
        )
    };
to_transactions({error, Reason}, _Opts) ->
    {error,
        #{
            <<"status">> => 502,
            <<"reason">> => <<"Arweave query failed.">>,
            <<"detail">> => Reason
        }
    }.

%% @doc Annotate each matched transaction with its weave offset from the
%% local index, keeping only the base-layer (`tx@1.0') transactions and
%% dropping bundled data items (indexed under the `ans104@1.0' codec) and any
%% that are not yet locally indexed. The result is sorted by offset, which is
%% the canonical weave order.
base_layer_offsets(IDs, Opts) ->
    Store = hb_store_arweave:store_from_opts(Opts),
    lists:keysort(
        1,
        lists:filtermap(
            fun(ID) -> base_layer_offset(Store, ID, Opts) end,
            IDs
        )
    ).

base_layer_offset(Store, ID, Opts) ->
    case offset_entry(Store, ID, Opts) of
        {ok, <<"tx@1.0">>, Offset} -> {true, {Offset, ID}};
        _ -> false
    end.

%% @doc Keep the data-free headers cached by the headers-mode pass.
base_layer_blocks(Located, Opts) ->
    {ok,
        [
            {#{ <<"block-height">> => Height }, ID}
        ||
            {Height, ID} <- Located,
            data_free(ID, Opts)
        ]
    }.

data_free(ID, Opts) ->
    LocalOpts = hb_store:scope(Opts, local),
    try
        case hb_cache:read(ID, LocalOpts) of
            {ok, Header} ->
                (hb_message:convert(
                    Header, <<"tx@1.0">>, LocalOpts))#tx.data_size =:= 0;
            _ ->
                false
        end
    catch
        _:_ -> false
    end.

%% @doc Read a transaction's local index entry, returning its codec device and
%% weave offset.
offset_entry(Store, ID, Opts) ->
    case hb_store_arweave:read_offset(Store, ID, Opts) of
        {ok,
            #{
                <<"codec-device">> := Codec,
                <<"start-offset">> := Offset
            }} when is_integer(Offset) ->
            {ok, Codec, Offset};
        _ -> not_found
    end.

%% @doc Materialize a chunk's assignments and record that the range is synced
%% to `To'. The discovered list holds only the messages in blocks above the
%% already-synced range, so they are exactly the next slots -- appended from
%% `next-slot' with no de-duplication. `next-slot' and `synced-to' are then
%% persisted together, so the two never diverge.
materialize(ProcID, State = #{ <<"next-slot">> := NextSlot }, Ordered, To, Opts) ->
    assign(ProcID, State, NextSlot, Ordered, To, Opts).

assign(ProcID, State, Slot, [], To, Opts) ->
    NewState = State#{ <<"next-slot">> => Slot, <<"synced-to">> => To },
    ok = dev_arweave_scheduler_cache:write_state(ProcID, NewState, Opts),
    {ok, NewState};
assign(ProcID, State, Slot, [{Extra, TXID} | Rest], To, Opts) ->
    case read_tx_header(TXID, Opts) of
        {ok, Msg} ->
            ok = write_assignment(ProcID, Slot, Extra, Msg, Opts),
            assign(ProcID, State, Slot + 1, Rest, To, Opts);
        {error, Err} ->
            ?event(
                error,
                {arweave_scheduler_tx_unavailable,
                    {proc_id, ProcID},
                    {tx, {explicit, TXID}}
                }
            ),
            {error, Err}
    end.

%% @doc Generate and store the synthetic assignment for a message. Mirrors the
%% assignments minted by `~scheduler@1.0', but the on-chain position is the
%% transaction's weave `offset' rather than a scheduler-assigned nonce (joined,
%% in `all' mode, by the `block-height' that sequenced it). Every field derives
%% from chain data, so the assignment is deterministic across nodes and is left
%% uncommitted.
write_assignment(ProcID, Slot, Extra, Msg, Opts) ->
    BaseAssignment =
        lib_scheduler:base_assignment(
            hb_util:human_id(ProcID),
            Slot,
            Msg,
            Opts
        ),
    Assignment = hb_maps:merge(BaseAssignment, Extra, Opts),
    ?event(
        {minting_assignment,
            {proc_id, ProcID},
            {slot, Slot},
            {extra, Extra}
        }
    ),
    dev_arweave_scheduler_cache:write(Assignment, Opts).

%% @doc Find or create the persisted synchronization state for a process.
ensure_initialized(ProcID, Opts) ->
    case dev_arweave_scheduler_cache:read_state(ProcID, Opts) of
        {ok, State} -> {ok, State};
        _ -> initialize(ProcID, Opts)
    end.

%% @doc First contact with a process: locate its spawn block, index it, read
%% the process header, and mint the slot 0 assignment from the process
%% message itself. The canonical process header is also written to the cache
%% so that `<ProcessID>~process@1.0' resolves to the verifying tx@1.0 form
%% ahead of any lossier gateway-derived copy. If the spawn is not yet
%% confirmed, initialization fails and is retried on the next synchronization.
initialize(ProcID, Opts) ->
    ?event({initializing_arweave_schedule, {proc_id, ProcID}}),
    maybe
        {ok, SpawnHeight} ?= spawn_height(ProcID, Opts),
        ok ?= ensure_offsets(SpawnHeight, SpawnHeight, Opts),
        {ok, Offset} ?= tx_offset(ProcID, Opts),
        {ok, Process} ?= read_tx_header(ProcID, Opts),
        {ok, _} = hb_cache:write(Process, Opts),
        Mode = mode(Process, Opts),
        ok =
            write_assignment(
                ProcID,
                0,
                slot_zero(Mode, Offset, SpawnHeight),
                Process,
                Opts
            ),
        % `synced-to' starts one below the spawn block: slot 0 is the process
        % itself, and no message-bearing block has been indexed yet. The first
        % sync begins its range at the spawn block, catching any messages mined
        % alongside the process.
        State =
            #{
                <<"next-slot">> => 1,
                <<"spawn-height">> => SpawnHeight,
                <<"synced-to">> => SpawnHeight - 1,
                <<"mode">> => Mode
            },
        ok = dev_arweave_scheduler_cache:write_state(ProcID, State, Opts),
        {ok, State}
    end.

%% @doc The sequencing mode a process message asks for. A mode the device does
%% not implement falls back to `target' rather than erroring: the process
%% message is immutable, so a typo in a spawn tag would otherwise wedge the
%% process permanently.
mode(Process, Opts) ->
    case hb_ao:get(<<"scheduler-mode">>, Process, ?DEFAULT_MODE, Opts) of
        <<"all">> -> <<"all">>;
        _ -> ?DEFAULT_MODE
    end.

%% @doc The sequencing detail recorded on slot 0. The process message is its
%% own first message, so in `all' mode it carries the height of its spawn block
%% -- the process's clock starts at the block it was created in.
slot_zero(<<"all">>, Offset, SpawnHeight) ->
    #{ <<"offset">> => Offset, <<"block-height">> => SpawnHeight };
slot_zero(_Mode, Offset, _SpawnHeight) ->
    #{ <<"offset">> => Offset }.

%% @doc Read an L1 transaction as a header-only message from the node's stores.
%% `~copycat@1.0/arweave' caches the (data-free) header locally while indexing,
%% so the read is normally served straight from the local store; a miss falls
%% through the rest of the store chain (which may reach the header faster than a
%% specific Arweave host). The header carries the transaction's tags and its
%% tx@1.0 signature -- so its committed ID is the transaction ID and it verifies
%% -- and no data is attached at this layer, so the schedule never depends on
%% the availability of any transaction's data.
%% The header is taken as it is read, links and all. Forcing it to load in full
%% would demand a transaction's data -- the very thing this schedule promises
%% never to depend on -- and any transaction on the network can be a message
%% here, so most of them carry some. Loading only what happens to be available
%% locally would be worse than either: two nodes would mint different
%% assignments for the same transaction.
%% Only the node's own stores are consulted, because only they are known to
%% hold what this schedule wants: the indexing pass caches a data-free header
%% for every plain transaction it walks. Letting the read fall through to a
%% gateway would answer with the whole transaction instead -- data, or a bundle
%% decoded into items whose contents are not on this node -- and the schedule
%% would then depend on the availability of data it promises never to touch.
%% Anything not cached locally is fetched as a header in its own right.
read_tx_header(TXID, Opts) ->
    case hb_cache:read(TXID, hb_store:scope(Opts, local)) of
        {ok, Msg} -> {ok, Msg};
        _ -> fetch_tx_header(TXID, Opts)
    end.

%% @doc Fetch a transaction's data-free header from Arweave. The indexing pass
%% caches the headers of plain L1 transactions as it goes, but deliberately not
%% those of bundles -- and an `all'-mode schedule sequences every transaction in
%% a block, bundles included. Their headers are fetched on demand rather than
%% failing the whole range; `exclude-data' keeps the schedule header-only, so it
%% still never depends on the availability of a transaction's data.
fetch_tx_header(TXID, Opts) ->
    Res =
        hb_ao:resolve(
            <<
                ?ARWEAVE_DEVICE/binary, "/tx",
                "&tx=", TXID/binary,
                "&exclude-data=true"
            >>,
            no_result_cache(Opts)
        ),
    case Res of
        {ok, Msg} -> {ok, Msg};
        _ ->
            {error,
                #{
                    <<"status">> => 503,
                    <<"reason">> =>
                        <<"Transaction header is not yet retrievable ",
                            "from Arweave.">>,
                    <<"tx">> => TXID
                }
            }
    end.

%% @doc Read a transaction's weave offset from the local index. A transaction
%% that has not yet been indexed has no offset.
tx_offset(TXID, Opts) ->
    Store = hb_store_arweave:store_from_opts(Opts),
    case offset_entry(Store, TXID, Opts) of
        {ok, _Codec, Offset} -> {ok, Offset};
        not_found ->
            {error,
                #{
                    <<"status">> => 503,
                    <<"reason">> =>
                        <<"Transaction is not yet indexed locally.">>,
                    <<"tx">> => TXID
                }
            }
    end.

%% @doc Locate the block height that includes a transaction, via the Arweave
%% `tx status' API. An unconfirmed transaction has no block height: a process
%% cannot be scheduled against until its spawn is confirmed.
spawn_height(TXID, Opts) ->
    Res =
        hb_http:request(
            #{
                <<"method">> => <<"GET">>,
                <<"path">> => <<"/arweave/tx/", TXID/binary, "/status">>
            },
            no_result_cache(Opts)
        ),
    case lib_arweave_common:best_response(Res) of
        {ok, Response} ->
            Status =
                hb_json:decode(hb_ao:get(<<"body">>, Response, <<"{}">>, Opts)),
            case hb_maps:get(<<"block_height">>, Status, undefined, Opts) of
                undefined -> {error, unconfirmed_process(TXID)};
                Height -> {ok, hb_util:int(Height)}
            end;
        _ -> {error, unconfirmed_process(TXID)}
    end.

unconfirmed_process(TXID) ->
    #{
        <<"status">> => 404,
        <<"reason">> => <<"Process is not yet confirmed on Arweave.">>,
        <<"process">> => TXID
    }.

%% @doc The height up to which the schedule may safely be extended: the
%% network tip less the configured confirmation depth, optionally capped by
%% the `arweave_scheduler_max_height' node option.
confirmed_tip(Opts) ->
    maybe
        {ok, Height} ?=
            hb_ao:resolve(
                <<?ARWEAVE_DEVICE/binary, "/current/height">>,
                no_result_cache(Opts)
            ),
        Depth =
            hb_util:int(
                hb_opts:get(
                    arweave_scheduler_confirmation_depth,
                    ?DEFAULT_CONFIRMATION_DEPTH,
                    Opts
                )
            ),
        Confirmed = hb_util:int(Height) - Depth,
        case hb_opts:get(arweave_scheduler_max_height, undefined, Opts) of
            undefined -> {ok, Confirmed};
            Max -> {ok, min(Confirmed, hb_util:int(Max))}
        end
    end.

%%% Tests

%%% The permanent test fixture: a `lua@5.3a' process scheduled by this
%%% device, seeded onto Arweave with three state-transformation messages
%%% (`test/arweave-scheduler-test.lua'; all transactions carry
%%% `test-suite: arweave-scheduler' tags). `?FIXTURE_MSG2' was posted
%%% directly to arweave.net, never passing through a HyperBEAM node: its
%%% presence in the schedule proves foreign-message indexing. Arweave is
%%% permanent, so these tests are repeatable against the live network.
%%% Spawn block 1958986; messages at 1958993, 1958994 and 1958995.
-define(FIXTURE_MODULE, <<"_GeSyZbQkmqWk6YzL-tjIqJ-2hakIkg-9k127DpVfO8">>).
-define(FIXTURE_PROCESS, <<"q3SycbYpO1lz-S6V2kd7FG3DIZ3AU0NrKKxWw4C3yos">>).
-define(FIXTURE_MSG1, <<"Y8GnKytC57VUk7W7FlGnD1oH4OAwFHyP-pJPGCJBa3Y">>).
-define(FIXTURE_MSG2, <<"luf1fFmhi0RMIZNnFmv1QgK2SJU5plAFQ3ZxndUsLpk">>).
-define(FIXTURE_MSG3, <<"UThcMtT6zy0pJ7iXUsMTFUMu3Xhv51EAjn5BzqHMhR4">>).
-define(FIXTURE_MAX_HEIGHT, 1958995).

test_opts() ->
    TestStore = hb_test_utils:test_store(),
    IndexStore = hb_test_utils:test_store(),
    % The full default config is merged in so the node has the Arweave routes
    % that `~copycat@1.0' and `~arweave@2.9' resolve through.
    (hb_opts:default_message())#{
        <<"store">> => [
            TestStore,
            % The local Arweave offset index that discovery orders by.
            #{
                <<"store-module">> => hb_store_arweave,
                <<"name">> => <<"cache-arweave">>,
                <<"index-store">> => [IndexStore]
            },
            % Serves the Lua module transaction's data (its source) and the
            % transaction headers `~copycat@1.0/arweave' fetches while indexing.
            #{
                <<"store-module">> => hb_store_gateway,
                <<"local-store">> => [TestStore]
            }
        ],
        <<"arweave-index-store">> => #{ <<"index-store">> => [IndexStore] },
        <<"arweave-index-workers">> => 8,
        <<"arweave-scheduler-confirmation-depth">> => 1,
        <<"arweave-scheduler-max-height">> => ?FIXTURE_MAX_HEIGHT,
        <<"priv-wallet">> => ar_wallet:new()
    }.

%% @doc Scheduling a message that does not carry a `tx@1.0' commitment must
%% be rejected.
reject_non_tx_message_test() ->
    Opts = #{ <<"priv-wallet">> => ar_wallet:new() },
    Msg =
        hb_message:commit(
            #{
                <<"target">> => hb_util:human_id(crypto:strong_rand_bytes(32)),
                <<"test-key">> => <<"test-value">>
            },
            Opts
        ),
    ?assertMatch(
        {error, #{ <<"status">> := 422 }},
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave-scheduler@1.0">> },
            #{
                <<"path">> => <<"schedule">>,
                <<"method">> => <<"POST">>,
                <<"body">> => Msg
            },
            Opts
        )
    ).

%% @doc A validly `tx@1.0'-committed message that carries data must also be
%% rejected: this scheduler sequences transaction headers only, so it will not
%% dispatch (nor later depend on the chunk availability of) a message's data.
reject_data_message_test() ->
    Opts = #{ <<"priv-wallet">> => ar_wallet:new() },
    Msg =
        hb_message:commit(
            #{
                <<"target">> => hb_util:human_id(crypto:strong_rand_bytes(32)),
                <<"data">> => <<"unschedulable-data">>
            },
            Opts,
            #{ <<"commitment-device">> => <<"tx@1.0">> }
        ),
    ?assertMatch(
        {error, #{ <<"status">> := 422 }},
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave-scheduler@1.0">> },
            #{
                <<"path">> => <<"schedule">>,
                <<"method">> => <<"POST">>,
                <<"body">> => Msg
            },
            Opts
        )
    ).

%% @doc Base-layer annotation keeps only tx@1.0 offsets and sorts by offset.
base_layer_offsets_test() ->
    Store = hb_test_utils:test_store(),
    hb_store:start(Store),
    ArwStore = #{ <<"index-store">> => [Store] },
    Opts = #{ <<"arweave-index-store">> => ArwStore },
    Late = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Early = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Bundled = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Unindexed = hb_util:human_id(crypto:strong_rand_bytes(32)),
    ok = hb_store_arweave:write_offset(ArwStore, Late, <<"tx@1.0">>, 200, 1),
    ok = hb_store_arweave:write_offset(ArwStore, Early, <<"tx@1.0">>, 100, 1),
    ok = hb_store_arweave:write_offset(ArwStore, Bundled, <<"ans104@1.0">>, 150, 1),
    ?assertEqual(
        [{100, Early}, {200, Late}],
        base_layer_offsets([Late, Bundled, Early, Unindexed], Opts)
    ).

%% @doc The sequencing mode is read from the process message, and anything the
%% device does not implement leaves the process sequenced as normal rather than
%% wedging it: the process message cannot be corrected once it is on-chain.
mode_test() ->
    ?assertEqual(<<"all">>, mode(#{ <<"scheduler-mode">> => <<"all">> }, #{})),
    ?assertEqual(?DEFAULT_MODE, mode(#{}, #{})),
    ?assertEqual(
        ?DEFAULT_MODE,
        mode(#{ <<"scheduler-mode">> => <<"sideways">> }, #{})
    ).

%% @doc A process sequenced by the whole chain has a clock from slot 0; one
%% sequenced by its own messages keeps the assignment shape it always had.
slot_zero_test() ->
    ?assertEqual(
        #{ <<"offset">> => 7, <<"block-height">> => 3 },
        slot_zero(<<"all">>, 7, 3)
    ),
    ?assertEqual(#{ <<"offset">> => 7 }, slot_zero(?DEFAULT_MODE, 7, 3)).

%% @doc Write a block into the node's block cache at the height pseudo-path
%% `~copycat@1.0/arweave' caches it under, so the enumerator reads it locally
%% rather than from the network.
write_test_block(Height, TXs, Opts) ->
    {ok, MsgID} =
        hb_cache:write(#{ <<"height">> => Height, <<"txs">> => TXs }, Opts),
    hb_cache:link(
        MsgID,
        hb_path:to_binary(
            [?ARWEAVE_DEVICE, <<"block">>, <<"height">>, hb_util:bin(Height)]
        ),
        Opts
    ),
    ok.

%% @doc In `all' mode the schedule is enumerated from the block headers, in
%% chain order, and the process's own transaction is not re-assigned: it is
%% already slot 0.
enumerate_blocks_test() ->
    Store = hb_test_utils:test_store(),
    hb_store:start(Store),
    Opts = #{ <<"store">> => [Store] },
    ProcID = hb_util:human_id(crypto:strong_rand_bytes(32)),
    First = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Second = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Third = hb_util:human_id(crypto:strong_rand_bytes(32)),
    ok = write_test_block(10, [First, ProcID], Opts),
    ok = write_test_block(11, [Second, Third], Opts),
    ?assertEqual(
        {ok, [{10, First}, {11, Second}, {11, Third}]},
        enumerate_blocks(ProcID, 10, 11, Opts)
    ).

%% @doc Block-enumerated transactions keep chain order and only locally cached,
%% data-free transaction headers.
base_layer_blocks_test() ->
    Store = hb_test_utils:test_store(),
    hb_store:start(Store),
    Opts = #{ <<"store">> => [Store], <<"priv-wallet">> => ar_wallet:new() },
    DataFree =
        hb_message:commit(
            #{ <<"target">> => hb_util:human_id(crypto:strong_rand_bytes(32)) },
            Opts,
            #{ <<"commitment-device">> => <<"tx@1.0">> }
        ),
    DataBearing =
        hb_message:commit(
            #{ <<"data">> => <<"not-a-message">> },
            Opts,
            #{ <<"commitment-device">> => <<"tx@1.0">> }
        ),
    DataFreeID = hb_message:id(DataFree, signed, Opts),
    DataBearingID = hb_message:id(DataBearing, signed, Opts),
    {ok, _} = hb_cache:write(DataFree, Opts),
    {ok, _} = hb_cache:write(DataBearing, Opts),
    ?assertEqual(
        {ok, [{#{ <<"block-height">> => 10 }, DataFreeID}]},
        base_layer_blocks(
            [{10, DataFreeID}, {10, DataBearingID}, {10, <<"missing">>}],
            Opts
        )
    ).

%% @doc An `all'-mode assignment records the height that sequenced it as well
%% as its weave offset, and both survive the cache round trip that a process
%% reads its schedule back through. This is the contract
%% `~arweave-swap@1.0' reads its clock from.
all_mode_assignment_test() ->
    Store = hb_test_utils:test_store(),
    hb_store:start(Store),
    Opts = #{ <<"store">> => [Store], <<"priv-wallet">> => ar_wallet:new() },
    ProcID = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Msg =
        hb_message:commit(
            #{ <<"target">> => ProcID },
            Opts,
            #{ <<"commitment-device">> => <<"tx@1.0">> }
        ),
    ok =
        write_assignment(
            ProcID,
            1,
            #{ <<"offset">> => 42, <<"block-height">> => 1958986 },
            Msg,
            Opts
        ),
    {ok, Assignment} = dev_arweave_scheduler_cache:read(ProcID, 1, Opts),
    ?assertEqual(1, hb_util:int(hb_ao:get(<<"slot">>, Assignment, Opts))),
    ?assertEqual(42, hb_util:int(hb_ao:get(<<"offset">>, Assignment, Opts))),
    ?assertEqual(
        1958986,
        hb_util:int(hb_ao:get(<<"block-height">>, Assignment, Opts))
    ).

%% @doc The mode is pinned in the persisted state, so a schedule can never be
%% half-derived in each mode.
state_mode_round_trip_test() ->
    Store = hb_test_utils:test_store(),
    hb_store:start(Store),
    Opts = #{ <<"store">> => [Store] },
    ProcID = hb_util:human_id(crypto:strong_rand_bytes(32)),
    State =
        #{
            <<"next-slot">> => 1,
            <<"spawn-height">> => 10,
            <<"synced-to">> => 9,
            <<"mode">> => <<"all">>
        },
    ok = dev_arweave_scheduler_cache:write_state(ProcID, State, Opts),
    ?assertEqual({ok, State}, dev_arweave_scheduler_cache:read_state(ProcID, Opts)).

%% @doc `/status' reports each tracked process's contiguously-indexed block
%% range straight from the cache, without triggering a synchronization.
status_attestable_range_test() ->
    Store = hb_test_utils:test_store(),
    hb_store:start(Store),
    Opts = #{ <<"store">> => [Store], <<"priv-wallet">> => ar_wallet:new() },
    ProcID = hb_util:human_id(crypto:strong_rand_bytes(32)),
    ok =
        dev_arweave_scheduler_cache:write_state(
            ProcID,
            #{
                <<"next-slot">> => 4,
                <<"spawn-height">> => 100,
                <<"synced-to">> => 150,
                <<"mode">> => ?DEFAULT_MODE
            },
            Opts
        ),
    {ok, #{ <<"processes">> := Processes }} = status(#{}, #{}, Opts),
    ?assertEqual(
        #{ <<"from">> => 100, <<"to">> => 150, <<"current">> => 3 },
        hb_maps:get(ProcID, Processes, not_found, Opts)
    ).

%% @doc Synchronize the fixture schedule from the live network, retrying on
%% transient indexing failures (for example, gateway rate limits while
%% fetching block transaction headers).
fixture_sync(_Opts, 0) -> {error, fixture_sync_failed};
fixture_sync(Opts, Attempts) ->
    Res =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave-scheduler@1.0">> },
            #{
                <<"path">> => <<"schedule">>,
                <<"method">> => <<"GET">>,
                <<"target">> => ?FIXTURE_PROCESS
            },
            Opts
        ),
    case Res of
        {ok, Schedule} -> {ok, Schedule};
        {error, _} ->
            timer:sleep(5000),
            fixture_sync(Opts, Attempts - 1)
    end.

%% @doc Read the fixture schedule from the live network and check that the
%% assignments arrive in seeded order, including the message that never
%% passed through a HyperBEAM node.
fixture_schedule_test_() ->
    {timeout, 1200, fun fixture_schedule/0}.
fixture_schedule() ->
    Opts = test_opts(),
    {ok, Schedule} = fixture_sync(Opts, 5),
    Assignments =
        hb_ao:normalize_keys(
            hb_ao:get(<<"assignments">>, Schedule, Opts),
            Opts
        ),
    SlotIDs =
        lists:map(
            fun(Slot) ->
                Assignment =
                    hb_maps:get(hb_util:bin(Slot), Assignments, not_found, Opts),
                ?assertEqual(
                    Slot,
                    hb_util:int(hb_ao:get(<<"slot">>, Assignment, Opts))
                ),
                Body = hb_ao:get(<<"body">>, Assignment, Opts),
                % The body is a header: it verifies and its committed ID is
                % the transaction ID, but it carries no data payload -- the
                % schedule never depends on data availability.
                ?assert(hb_message:verify(Body, all, Opts)),
                BodyKeys = hb_maps:keys(hb_message:uncommitted(Body, Opts), Opts),
                ?assertNot(lists:member(<<"data">>, BodyKeys)),
                ?assertNot(lists:member(<<"body">>, BodyKeys)),
                hb_message:id(Body, signed, Opts)
            end,
            [0, 1, 2, 3]
        ),
    ?assertEqual(
        [?FIXTURE_PROCESS, ?FIXTURE_MSG1, ?FIXTURE_MSG2, ?FIXTURE_MSG3],
        SlotIDs
    ).

%% @doc Compute the fixture process from its Arweave schedule and check the
%% state transformations applied in order: `setstate 1000', `addstate 337',
%% then `querystate' reporting the result.
fixture_lua_e2e_test_() ->
    {timeout, 1200, fun fixture_lua_e2e/0}.
fixture_lua_e2e() ->
    Opts = test_opts(),
    % Prime the schedule first: synchronization indexes the spawn block, so
    % the process message is read back as its canonical tx@1.0 decoding
    % (rather than the gateway store's lossy representation).
    {ok, _} = fixture_sync(Opts, 5),
    {ok, RawProcess} = hb_cache:read(?FIXTURE_PROCESS, Opts),
    Process = hb_cache:ensure_all_loaded(RawProcess, Opts),
    % Luerl represents Lua numbers as floats.
    ?assertEqual(
        {ok, 1000.0},
        hb_ao:resolve(
            Process,
            #{ <<"path">> => <<"compute/state">>, <<"slot">> => 1 },
            Opts
        )
    ),
    ?assertEqual(
        {ok, 1337.0},
        hb_ao:resolve(
            Process,
            #{ <<"path">> => <<"compute/state">>, <<"slot">> => 2 },
            Opts
        )
    ),
    ?assertEqual(
        {ok, <<"state=1337.0">>},
        hb_ao:resolve(
            Process,
            #{
                <<"path">> => <<"compute/results/output/body">>,
                <<"slot">> => 3
            },
            Opts
        )
    ),
    ?assertEqual({ok, 1337.0}, hb_ao:resolve(Process, <<"now/state">>, Opts)).

%% @doc A first sync indexes the range in bounded chunks, persisting its
%% progress after each. With a chunk size smaller than the fixture's span --
%% so the messages fall in different chunks -- the schedule is still assembled
%% in order, and `/status' reports the range synced all the way to the tip.
chunked_sync_test_() ->
    {timeout, 1200, fun chunked_sync/0}.
chunked_sync() ->
    Opts = (test_opts())#{ <<"arweave-scheduler-sync-chunk">> => 4 },
    {ok, Schedule} = fixture_sync(Opts, 5),
    Assignments =
        hb_ao:normalize_keys(
            hb_ao:get(<<"assignments">>, Schedule, Opts),
            Opts
        ),
    SlotIDs =
        lists:map(
            fun(Slot) ->
                Assignment =
                    hb_maps:get(hb_util:bin(Slot), Assignments, not_found, Opts),
                hb_message:id(hb_ao:get(<<"body">>, Assignment, Opts), signed, Opts)
            end,
            [0, 1, 2, 3]
        ),
    ?assertEqual(
        [?FIXTURE_PROCESS, ?FIXTURE_MSG1, ?FIXTURE_MSG2, ?FIXTURE_MSG3],
        SlotIDs
    ),
    % A completed sync makes the whole fixture range attestable: the spawn
    % block through the tip.
    {ok, #{ <<"processes">> := Processes }} = status(#{}, #{}, Opts),
    Range = hb_maps:get(?FIXTURE_PROCESS, Processes, not_found, Opts),
    ?assertEqual(?FIXTURE_MAX_HEIGHT, hb_util:int(hb_ao:get(<<"to">>, Range, Opts))),
    ?assertEqual(3, hb_util:int(hb_ao:get(<<"current">>, Range, Opts))).
