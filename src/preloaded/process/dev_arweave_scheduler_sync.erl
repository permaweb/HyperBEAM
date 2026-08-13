%%% @doc Global Arweave synchronization for `~arweave-scheduler@1.0'. A
%%% single serialized pass walks each confirmed block once and builds the
%%% inverted `targets/<address>/<ordinate> -> TXID' index used by every
%%% process schedule.
-module(dev_arweave_scheduler_sync).
-export([sync/1, process/2, from_height/1, confirmed_tip/1]).
-export([fetch_block/2, fetch_header/2, spawn_ordinate/2]).
-export([targets/1, ordinate/2, parse_ordinate/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(ARWEAVE_DEVICE, <<"~arweave@2.9">>).
-define(DEFAULT_CONFIRMATION_DEPTH, 10).
-define(DEFAULT_FROM_HEIGHT, 1978888).
-define(DEFAULT_HEADER_WORKERS, 16).
-define(DEFAULT_SYNC_INTERVAL_MS, 1000).
-define(SYNC_TABLE, dev_arweave_scheduler_sync_checks).

%% @doc Bring the global target index to the confirmed chain frontier. The
%% transaction is serialized per scheduler store, and repeated callers within
%% the short check interval reuse the completed global record without another
%% network-tip request.
sync(Opts) ->
    CacheOpts = dev_arweave_scheduler_cache:opts(Opts),
    Store = hb_opts:get(store, no_viable_store, CacheOpts),
    ensure_sync_table(),
    case recent_state(Store, Opts) of
        {ok, _} = Recent -> Recent;
        stale ->
            global:trans(
                {?MODULE, Store},
                fun() -> maybe_sync(Store, Opts) end
            )
    end.

%% @doc Synchronize the global index and materialize one process's relevant
%% entries into contiguous AO slots. Each process is serialized independently;
%% the completed global record bounds entries visible during materialization.
process(ProcessID, Opts) ->
    case canonical_process_id(ProcessID) of
        {ok, HumanProcessID} ->
            CacheOpts = dev_arweave_scheduler_cache:opts(Opts),
            Store = hb_opts:get(store, no_viable_store, CacheOpts),
            maybe
                {ok, Global} ?= sync(Opts),
                global:trans(
                    {?MODULE, Store, HumanProcessID},
                    fun() ->
                        materialize_process(HumanProcessID, Global, Opts)
                    end
                )
            end;
        error ->
            {error,
                #{
                    <<"status">> => 422,
                    <<"reason">> =>
                        <<"Arweave process ID must be a canonical TXID.">>
                }
            }
    end.

maybe_sync(Store, Opts) ->
    case recent_state(Store, Opts) of
        {ok, _} = Recent -> Recent;
        stale ->
            StateResult = dev_arweave_scheduler_cache:read_global(Opts),
            Result = do_sync(StateResult, Opts),
            case Result of
                {ok, State} ->
                    ets:insert(
                        ?SYNC_TABLE,
                        {Store, erlang:monotonic_time(millisecond), State}
                    );
                _ -> ok
            end,
            Result
    end.

recent_state(Store, Opts) ->
    Interval =
        hb_util:int(
            hb_opts:get(
                arweave_scheduler_sync_interval,
                ?DEFAULT_SYNC_INTERVAL_MS,
                Opts
            )
        ),
    Now = erlang:monotonic_time(millisecond),
    ConfiguredFrom = from_height(Opts),
    case ets:lookup(?SYNC_TABLE, Store) of
        [{Store, CheckedAt, State}]
                when Interval > 0, Now - CheckedAt < Interval ->
            case hb_util:int(hb_maps:get(<<"from">>, State, -1, #{})) of
                ConfiguredFrom -> {ok, State};
                _ -> stale
            end;
        _ -> stale
    end.

do_sync(StateResult, Opts) ->
    From = from_height(Opts),
    maybe
        {ok, Upper} ?= confirmed_tip(Opts),
        {ok, State} ?= initial_state(StateResult, From),
        sync_blocks(State, Upper, Opts)
    end.

initial_state(not_found, From) ->
    {ok, #{ <<"from">> => From, <<"to">> => From - 1 }};
initial_state({ok, State}, From) ->
    case hb_util:int(hb_maps:get(<<"from">>, State, -1, #{})) of
        From -> {ok, State};
        StoredFrom ->
            {error,
                #{
                    <<"status">> => 409,
                    <<"reason">> =>
                        <<"Configured scheduler rollout height does not match ",
                            "the existing global synchronization record.">>,
                    <<"configured-from">> => From,
                    <<"stored-from">> => StoredFrom
                }
            }
    end;
initial_state({error, not_found}, From) -> initial_state(not_found, From);
initial_state(Error, _From) -> Error.

sync_blocks(State = #{ <<"to">> := To }, Upper, _Opts) when To >= Upper ->
    {ok, State};
sync_blocks(State = #{ <<"to">> := To }, Upper, Opts) ->
    Height = To + 1,
    maybe
        {ok, Block} ?= fetch_block(Height, Opts),
        ok ?= validate_block(Height, State, Block, Opts),
        {ok, _} ?=
            dev_arweave_scheduler_cache:write_block(Height, Block, Opts),
        ok ?= index_block(Height, Block, Opts),
        BlockHash = hb_maps:get(<<"indep_hash">>, Block, not_found, Opts),
        true ?= is_binary(BlockHash),
        NewState =
            State#{
                <<"to">> => Height,
                <<"block-hash">> => BlockHash
            },
        {ok, _} ?=
            dev_arweave_scheduler_cache:write_global(NewState, Opts),
        sync_blocks(NewState, Upper, Opts)
    else
        false ->
            {error,
                #{
                    <<"status">> => 502,
                    <<"reason">> => <<"Arweave block has no canonical hash.">>,
                    <<"block-height">> => Height
                }
            };
        Error -> Error
    end.

materialize_process(ProcessID, Global, Opts) ->
    maybe
        {ok, State} ?= ensure_process(ProcessID, Global, Opts),
        materialize_targets(ProcessID, State, Global, Opts)
    end.

ensure_process(ProcessID, Global, Opts) ->
    case dev_arweave_scheduler_cache:read_process(ProcessID, Opts) of
        {ok, State} -> {ok, State};
        not_found -> initialize_process(ProcessID, Global, Opts);
        {error, not_found} -> initialize_process(ProcessID, Global, Opts);
        Error -> Error
    end.

initialize_process(
        ProcessID,
        Global = #{ <<"from">> := From, <<"to">> := To },
        Opts
    ) ->
    maybe
        {ok, Height, Index, SpawnOrdinate} ?= spawn_ordinate(ProcessID, Opts),
        ok ?= require_covered_spawn(ProcessID, Height, From, To),
        {ok, _, #tx{ data_size = 0 }} ?= fetch_header(ProcessID, Opts),
        ok ?=
            write_assignment(
                ProcessID,
                0,
                Height,
                Index,
                ProcessID,
                Opts
            ),
        State =
            #{
                <<"process">> => ProcessID,
                <<"spawn-ordinate">> => SpawnOrdinate,
                <<"synced-to">> => Height - 1,
                <<"next-slot">> => 1
            },
        {ok, _} ?=
            dev_arweave_scheduler_cache:write_process(ProcessID, State, Opts),
        {ok, State}
    else
        {ok, _, #tx{}} ->
            {error,
                #{
                    <<"status">> => 422,
                    <<"reason">> =>
                        <<"An Arweave-scheduled process must be data-free.">>,
                    <<"process">> => ProcessID
                }
            };
        Error -> Error
    end.

require_covered_spawn(_ProcessID, Height, From, _To) when Height < From ->
    {error,
        #{
            <<"status">> => 409,
            <<"reason">> =>
                <<"Process predates the Arweave scheduler rollout height.">>,
            <<"spawn-height">> => Height,
            <<"indexed-from">> => From
        }
    };
require_covered_spawn(ProcessID, Height, _From, To) when Height > To ->
    {error,
        #{
            <<"status">> => 425,
            <<"reason">> =>
                <<"Process spawn block is not yet in the confirmed index.">>,
            <<"process">> => ProcessID,
            <<"spawn-height">> => Height,
            <<"indexed-to">> => To
        }
    };
require_covered_spawn(_ProcessID, _Height, _From, _To) -> ok.

materialize_targets(
        _ProcessID,
        State = #{ <<"synced-to">> := SyncedTo },
        #{ <<"to">> := GlobalTo },
        _Opts
    ) when SyncedTo >= GlobalTo ->
    {ok, State};
materialize_targets(ProcessID, State, Global, Opts) ->
    TargetOrdinates =
        lists:filtermap(
            fun(Ordinate) ->
                case parse_ordinate(Ordinate) of
                    {ok, Position} -> {true, {Position, Ordinate}};
                    error -> false
                end
            end,
            dev_arweave_scheduler_cache:list_targets(ProcessID, Opts)
        ),
    Selected = select_targets(lists:sort(TargetOrdinates), State, Global),
    write_process_assignments(ProcessID, Selected, State, Global, Opts).

select_targets(
        Targets,
        #{
            <<"spawn-ordinate">> := SpawnOrdinate,
            <<"synced-to">> := SyncedTo
        },
        #{ <<"to">> := GlobalTo }
    ) ->
    {ok, SpawnPosition} = parse_ordinate(SpawnOrdinate),
    [
        {Position, Ordinate}
    ||
        {Position = {Height, _Index}, Ordinate} <- Targets,
        Position > SpawnPosition,
        Height > SyncedTo,
        Height =< GlobalTo
    ].

write_process_assignments(
        ProcessID,
        Targets,
        State = #{ <<"next-slot">> := NextSlot },
        Global,
        Opts
    ) ->
    case write_process_assignments(ProcessID, Targets, NextSlot, Opts) of
        {ok, NewNextSlot} ->
            NewState =
                State#{
                    <<"synced-to">> => hb_maps:get(<<"to">>, Global, Opts),
                    <<"next-slot">> => NewNextSlot
                },
            case dev_arweave_scheduler_cache:write_process(
                ProcessID,
                NewState,
                Opts
            ) of
                {ok, _} -> {ok, NewState};
                Error -> Error
            end;
        Error -> Error
    end.

write_process_assignments(_ProcessID, [], Slot, _Opts) -> {ok, Slot};
write_process_assignments(
        ProcessID,
        [{{Height, Index}, Ordinate} | Rest],
        Slot,
        Opts
    ) ->
    case dev_arweave_scheduler_cache:read_target(ProcessID, Ordinate, Opts) of
        {ok, TXID, _Header} ->
            case write_assignment(ProcessID, Slot, Height, Index, TXID, Opts) of
                ok ->
                    write_process_assignments(
                        ProcessID,
                        Rest,
                        Slot + 1,
                        Opts
                    );
                Error -> Error
            end;
        not_found ->
            {error,
                #{
                    <<"status">> => 500,
                    <<"reason">> => <<"Indexed scheduler target is missing.">>,
                    <<"ordinate">> => Ordinate
                }
            }
    end.

write_assignment(ProcessID, Slot, Height, Index, TXID, Opts) ->
    Body =
        {link,
            TXID,
            #{ <<"type">> => <<"link">>, <<"lazy">> => false }
        },
    Assignment =
        #{
            <<"path">> => <<"compute">>,
            <<"data-protocol">> => <<"ao">>,
            <<"variant">> => <<"ao.N.1">>,
            <<"process">> => ProcessID,
            <<"epoch">> => <<"0">>,
            <<"slot">> => Slot,
            <<"block-height">> => Height,
            <<"block-index">> => Index,
            <<"body">> => Body,
            <<"type">> => <<"Assignment">>
        },
    dev_arweave_scheduler_cache:write_assignment(Assignment, Opts).

%% @doc Index one complete block. Header fetches are concurrency-limited and
%% preserve the block's transaction order. The caller advances the global
%% record only if every transaction was examined successfully.
index_block(Height, Block, Opts) ->
    TXIDs =
        [
            hb_util:human_id(TXID)
        ||
            TXID <- hb_maps:get(<<"txs">>, Block, [], Opts)
        ],
    Indexed =
        hb_pmap:parallel_map(
            lists:zip(lists:seq(0, length(TXIDs) - 1), TXIDs),
            fun({Index, TXID}) -> index_transaction(Height, Index, TXID, Opts) end,
            hb_util:int(
                hb_opts:get(
                    arweave_scheduler_header_workers,
                    ?DEFAULT_HEADER_WORKERS,
                    Opts
                )
            )
        ),
    case collect_targets(Indexed, []) of
        {ok, Targets} ->
            dev_arweave_scheduler_cache:write_targets(Targets, Opts);
        Error -> Error
    end.

index_transaction(Height, Index, TXID, Opts) ->
    case fetch_header(TXID, Opts) of
        {ok, _Header, TX} ->
            Ordinate = ordinate(Height, Index),
            {ok,
                [
                    {Address, Ordinate, TXID}
                ||
                    Address <- transaction_targets(TX)
                ]
            };
        Error -> Error
    end.

transaction_targets(TX = #tx{ data_size = 0 }) -> targets(TX);
transaction_targets(#tx{}) -> [].

collect_targets([], Targets) -> {ok, lists:append(lists:reverse(Targets))};
collect_targets([{ok, Entries} | Rest], Targets) ->
    collect_targets(Rest, [Entries | Targets]);
collect_targets([Error | _], _Targets) -> Error.

%% @doc The fixed protocol rollout height. Nodes may override it explicitly,
%% but a persisted index refuses to continue if that value later changes.
from_height(Opts) ->
    hb_util:int(
        hb_opts:get(arweave_scheduler_from, ?DEFAULT_FROM_HEIGHT, Opts)
    ).

%% @doc The highest block safe to index. A configured maximum pins the chain
%% frontier and is also useful for deterministic isolated tests.
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

%% @doc Read a block locally when possible, otherwise fetch and cache it.
fetch_block(Height, Opts) ->
    case dev_arweave_scheduler_cache:read_block(Height, Opts) of
        {ok, Block} -> {ok, Block};
        _ -> fetch_block_remote(Height, Opts)
    end.

fetch_block_remote(Height, Opts) ->
    case hb_ao:resolve(
        <<?ARWEAVE_DEVICE/binary, "/block&block=", (hb_util:bin(Height))/binary>>,
        no_result_cache(dev_arweave_scheduler_cache:opts(Opts))
    ) of
        {ok, Block} -> {ok, Block};
        _ ->
            {error,
                #{
                    <<"status">> => 503,
                    <<"reason">> => <<"Arweave block is not retrievable.">>,
                    <<"block-height">> => Height
                }
            }
    end.

%% @doc Read a signed transaction header locally when possible, otherwise
%% fetch it without data. The signed commitment must reproduce the requested
%% TXID before it is admitted to the scheduler store.
fetch_header(TXID, Opts) ->
    case dev_arweave_scheduler_cache:read_header(TXID, Opts) of
        {ok, Header} -> validate_header(TXID, Header, Opts);
        _ -> fetch_header_remote(TXID, Opts)
    end.

fetch_header_remote(TXID, Opts) ->
    case hb_ao:resolve(
        <<
            ?ARWEAVE_DEVICE/binary, "/tx&tx=", TXID/binary,
            "&exclude-data=true"
        >>,
        no_result_cache(Opts)
    ) of
        {ok, Header} -> cache_header(TXID, Header, Opts);
        _ ->
            {error,
                #{
                    <<"status">> => 503,
                    <<"reason">> =>
                        <<"Arweave transaction header is not retrievable.">>,
                    <<"tx">> => TXID
                }
            }
    end.

cache_header(TXID, Header, Opts) ->
    case validate_header(TXID, Header, Opts) of
        {ok, Header, TX = #tx{ data_size = 0 }} ->
            case dev_arweave_scheduler_cache:write_header(Header, Opts) of
                {ok, _} -> {ok, Header, TX};
                Error -> Error
            end;
        {ok, Header, TX} -> {ok, Header, TX};
        Error -> Error
    end.

validate_header(TXID, Header, Opts) ->
    try
        TX = hb_message:convert(Header, <<"tx@1.0">>, Opts),
        HeaderTXID = hb_util:human_id(TX#tx.id),
        case
            HeaderTXID =:= TXID andalso
                ar_tx:verify_tx_id(hb_util:native_id(TXID), TX) andalso
                hb_message:verify(Header, signers, Opts)
        of
            true -> {ok, Header, TX};
            _ ->
                {error,
                    #{
                        <<"status">> => 502,
                        <<"reason">> =>
                            <<"Arweave header signature or TXID is invalid.">>,
                        <<"expected">> => TXID,
                        <<"actual">> => HeaderTXID
                    }
                }
        end
    catch
        _:_ ->
            {error,
                #{
                    <<"status">> => 502,
                    <<"reason">> => <<"Invalid Arweave transaction header.">>,
                    <<"tx">> => TXID
                }
            }
    end.

%% @doc Return the native recipient plus a comma-separated `Assign-To' list.
%% Invalid members and duplicate tag instances contribute no instructed
%% targets; the complete result is deduplicated.
targets(TX) ->
    Native =
        case TX#tx.target of
            Target when is_binary(Target), byte_size(Target) =:= 32 ->
                [hb_util:human_id(Target)];
            _ -> []
        end,
    AssignValues =
        [
            Value
        ||
            {Key, Value} <- TX#tx.tags,
            hb_util:to_lower(hb_ao:normalize_key(Key)) =:= <<"assign-to">>
        ],
    Assigned =
        case AssignValues of
            [Value] when is_binary(Value) ->
                lists:filtermap(
                    fun canonical_address/1,
                    binary:split(Value, <<",">>, [global])
                );
            _ -> []
        end,
    lists:usort(Native ++ Assigned).

canonical_address(Raw) ->
    Address = trim_ascii(Raw),
    try
        Native = hb_util:native_id(Address),
        case byte_size(Native) =:= 32 andalso hb_util:human_id(Native) =:= Address of
            true -> {true, Address};
            false -> false
        end
    catch
        _:_ -> false
    end.

canonical_process_id(ProcessID) ->
    try canonical_address(hb_util:human_id(ProcessID)) of
        {true, HumanProcessID} -> {ok, HumanProcessID};
        false -> error
    catch
        _:_ -> error
    end.

trim_ascii(<<C, Rest/binary>>)
        when C =:= $\s; C =:= $\t; C =:= $\r; C =:= $\n ->
    trim_ascii(Rest);
trim_ascii(Bin) -> trim_ascii_right(Bin, byte_size(Bin)).

trim_ascii_right(_, 0) -> <<>>;
trim_ascii_right(Bin, Len) ->
    case binary:at(Bin, Len - 1) of
        C when C =:= $\s; C =:= $\t; C =:= $\r; C =:= $\n ->
            trim_ascii_right(Bin, Len - 1);
        _ -> binary:part(Bin, 0, Len)
    end.

%% @doc Locate the process transaction's exact canonical block position.
spawn_ordinate(ProcessID, Opts) ->
    TXID = hb_util:human_id(ProcessID),
    maybe
        {ok, Height} ?= transaction_height(TXID, Opts),
        {ok, Block} ?= fetch_block(Height, Opts),
        ok ?= validate_spawn_block_height(Height, Block, Opts),
        {ok, Index} ?=
            find_index(TXID, hb_maps:get(<<"txs">>, Block, [], Opts), 0),
        {ok, Height, Index, ordinate(Height, Index)}
    end.

validate_spawn_block_height(Height, Block, Opts) ->
    case hb_util:int(hb_maps:get(<<"height">>, Block, -1, Opts)) of
        Height -> ok;
        ActualHeight ->
            {error,
                #{
                    <<"status">> => 502,
                    <<"reason">> =>
                        <<"Process status returned the wrong Arweave block.">>,
                    <<"expected">> => Height,
                    <<"actual">> => ActualHeight
                }
            }
    end.

transaction_height(TXID, Opts) ->
    Result =
        hb_http:request(
            #{
                <<"method">> => <<"GET">>,
                <<"path">> => <<"/arweave/tx/", TXID/binary, "/status">>
            },
            no_result_cache(Opts)
        ),
    case best_response(Result) of
        {ok, Response} ->
            Status =
                hb_json:decode(
                    hb_maps:get(<<"body">>, Response, <<"{}">>, Opts)
                ),
            case hb_maps:get(<<"block_height">>, Status, undefined, Opts) of
                undefined -> unconfirmed_process(TXID);
                Height -> {ok, hb_util:int(Height)}
            end;
        _ -> unconfirmed_process(TXID)
    end.

find_index(_TXID, [], _Index) ->
    {error,
        #{
            <<"status">> => 502,
            <<"reason">> =>
                <<"Process TXID is absent from its reported Arweave block.">>
        }
    };
find_index(TXID, [Candidate | Rest], Index) ->
    case hb_util:human_id(Candidate) of
        TXID -> {ok, Index};
        _ -> find_index(TXID, Rest, Index + 1)
    end.

unconfirmed_process(TXID) ->
    {error,
        #{
            <<"status">> => 404,
            <<"reason">> => <<"Process is not yet confirmed on Arweave.">>,
            <<"process">> => TXID
        }
    }.

best_response({error, {no_viable_responses, Responses}}) ->
    best_response(Responses);
best_response([]) -> {error, no_viable_responses};
best_response(Responses) when is_list(Responses) ->
    hd(
        lists:sort(
            fun({_, A}, {_, B}) ->
                response_status(A) =< response_status(B)
            end,
            Responses
        )
    );
best_response(Response) -> Response.

response_status(Response) when is_map(Response) ->
    maps:get(<<"status">>, Response, 999);
response_status(_) -> 999.

validate_block(Height, State, Block, Opts) ->
    ActualHeight = hb_util:int(hb_maps:get(<<"height">>, Block, -1, Opts)),
    Previous = hb_maps:get(<<"previous_block">>, Block, not_found, Opts),
    ExpectedPrevious = hb_maps:get(<<"block-hash">>, State, not_found, Opts),
    case {
        ActualHeight =:= Height,
        ExpectedPrevious =:= not_found orelse Previous =:= ExpectedPrevious
    } of
        {true, true} -> ok;
        {false, _} ->
            {error,
                #{
                    <<"status">> => 502,
                    <<"reason">> => <<"Arweave block height mismatch.">>,
                    <<"expected">> => Height,
                    <<"actual">> => ActualHeight
                }
            };
        {true, false} ->
            {error,
                #{
                    <<"status">> => 409,
                    <<"reason">> =>
                        <<"Arweave block does not extend the indexed chain.">>,
                    <<"block-height">> => Height
                }
            }
    end.

%% @doc Canonical, lossless transaction position within the weave.
ordinate(Height, Index) ->
    <<(integer_to_binary(Height))/binary, "-", (integer_to_binary(Index))/binary>>.

parse_ordinate(Ordinate) ->
    case binary:split(Ordinate, <<"-">>, [global]) of
        [Height, Index] ->
            try
                Position = {binary_to_integer(Height), binary_to_integer(Index)},
                case Position of
                    {H, I} when H >= 0, I >= 0 -> {ok, Position};
                    _ -> error
                end
            catch _:_ -> error
            end;
        _ -> error
    end.

no_result_cache(Opts) ->
    Opts#{
        <<"hashpath">> => ignore,
        <<"cache-control">> => [<<"no-cache">>, <<"no-store">>]
    }.

ensure_sync_table() ->
    Owner =
        hb_name:singleton(
            {?MODULE, ?SYNC_TABLE},
            fun() ->
                ets:new(
                    ?SYNC_TABLE,
                    [
                        named_table,
                        public,
                        set,
                        {read_concurrency, true},
                        {write_concurrency, true}
                    ]
                ),
                sync_table_owner()
            end
        ),
    Ref = make_ref(),
    Owner ! {ready, self(), Ref},
    receive {Ref, ok} -> ?SYNC_TABLE end.

sync_table_owner() ->
    receive
        {ready, Caller, Ref} ->
            Caller ! {Ref, ok},
            sync_table_owner()
    end.

%%% Tests

targets_test() ->
    Native = crypto:strong_rand_bytes(32),
    NativeAddress = hb_util:human_id(Native),
    A = hb_util:human_id(crypto:strong_rand_bytes(32)),
    B = hb_util:human_id(crypto:strong_rand_bytes(32)),
    TX =
        #tx{
            target = Native,
            tags =
                [
                    {<<"Assign-To">>,
                        <<" ", A/binary, ",", B/binary, ",,", A/binary,
                            ",", NativeAddress/binary, ",invalid ">>}
                ]
        },
    ?assertEqual(
        lists:usort([NativeAddress, A, B]),
        targets(TX)
    ),
    ?assertEqual(
        [NativeAddress],
        targets(
            TX#tx{
                tags =
                    [
                        {<<"Assign-To">>, A},
                        {<<"assign-to">>, B}
                    ]
            }
        )
    ),
    ?assertEqual([], transaction_targets(TX#tx{ data_size = 1 })),
    ?assertEqual({ok, A}, canonical_process_id(A)),
    ?assertEqual(error, canonical_process_id(<<"not-a-process">>)).

ordinate_test() ->
    ?assertEqual(<<"1966084-23">>, ordinate(1966084, 23)),
    ?assertEqual({ok, {1966084, 23}}, parse_ordinate(<<"1966084-23">>)),
    ?assertEqual(error, parse_ordinate(<<"1966084">>)),
    ?assertEqual(error, parse_ordinate(<<"-1-2">>)).

block_validation_test() ->
    State = #{ <<"block-hash">> => <<"previous">> },
    Block = #{ <<"height">> => 101, <<"previous_block">> => <<"previous">> },
    ?assertEqual(ok, validate_block(101, State, Block, #{})),
    ?assertMatch({error, _}, validate_block(100, State, Block, #{})),
    ?assertMatch(
        {error, _},
        validate_block(
            101,
            State,
            Block#{ <<"previous_block">> => <<"other">> },
            #{}
        )
    ).

%% @doc Materialization filters at the exact spawn ordinate, sorts numeric
%% positions, writes dense slots, and is deterministic when replayed.
sparse_materialization_test() ->
    Store = hb_test_utils:test_store(hb_store_volatile, <<"ar-sched-sync">>),
    ok = hb_store:start(Store),
    Opts = #{ <<"store">> => [Store], <<"priv-wallet">> => ar_wallet:new() },
    ProcessID = hb_util:human_id(crypto:strong_rand_bytes(32)),
    HeaderA = hb_message:commit(#{ <<"message">> => <<"a">> }, Opts),
    HeaderB = hb_message:commit(#{ <<"message">> => <<"b">> }, Opts),
    TXIDA = hb_util:human_id(hb_message:id(HeaderA, signed, Opts)),
    TXIDB = hb_util:human_id(hb_message:id(HeaderB, signed, Opts)),
    {ok, _} = dev_arweave_scheduler_cache:write_header(HeaderA, Opts),
    {ok, _} = dev_arweave_scheduler_cache:write_header(HeaderB, Opts),
    ok =
        dev_arweave_scheduler_cache:write_target(
            ProcessID,
            <<"101-0">>,
            TXIDB,
            Opts
        ),
    ok =
        dev_arweave_scheduler_cache:write_target(
            ProcessID,
            <<"100-0">>,
            TXIDA,
            Opts
        ),
    ok =
        dev_arweave_scheduler_cache:write_target(
            ProcessID,
            <<"100-10">>,
            TXIDB,
            Opts
        ),
    ok =
        dev_arweave_scheduler_cache:write_target(
            ProcessID,
            <<"100-2">>,
            TXIDA,
            Opts
        ),
    State =
        #{
            <<"process">> => ProcessID,
            <<"spawn-ordinate">> => <<"100-1">>,
            <<"synced-to">> => 99,
            <<"next-slot">> => 1
        },
    Global =
        #{ <<"from">> => 100, <<"to">> => 101, <<"block-hash">> => TXIDB },
    {ok, NewState} = materialize_targets(ProcessID, State, Global, Opts),
    ?assertEqual(4, hb_maps:get(<<"next-slot">>, NewState, Opts)),
    ?assertEqual(101, hb_maps:get(<<"synced-to">>, NewState, Opts)),
    {ok, Assignment1} =
        dev_arweave_scheduler_cache:read_assignment(ProcessID, 1, Opts),
    {ok, Assignment2} =
        dev_arweave_scheduler_cache:read_assignment(ProcessID, 2, Opts),
    {ok, Assignment3} =
        dev_arweave_scheduler_cache:read_assignment(ProcessID, 3, Opts),
    ?assertEqual(100, hb_maps:get(<<"block-height">>, Assignment1, Opts)),
    ?assertEqual(2, hb_maps:get(<<"block-index">>, Assignment1, Opts)),
    ?assertEqual(100, hb_maps:get(<<"block-height">>, Assignment2, Opts)),
    ?assertEqual(10, hb_maps:get(<<"block-index">>, Assignment2, Opts)),
    ?assertEqual(101, hb_maps:get(<<"block-height">>, Assignment3, Opts)),
    ?assertEqual(0, hb_maps:get(<<"block-index">>, Assignment3, Opts)),
    AssignmentIDs =
        [
            hb_message:id(Assignment1, signed, Opts),
            hb_message:id(Assignment2, signed, Opts),
            hb_message:id(Assignment3, signed, Opts)
        ],
    {ok, ReplayState} = materialize_targets(ProcessID, State, Global, Opts),
    ?assertEqual(NewState, ReplayState),
    {ok, Replay1} =
        dev_arweave_scheduler_cache:read_assignment(ProcessID, 1, Opts),
    {ok, Replay2} =
        dev_arweave_scheduler_cache:read_assignment(ProcessID, 2, Opts),
    {ok, Replay3} =
        dev_arweave_scheduler_cache:read_assignment(ProcessID, 3, Opts),
    ?assertEqual(
        AssignmentIDs,
        [
            hb_message:id(Replay1, signed, Opts),
            hb_message:id(Replay2, signed, Opts),
            hb_message:id(Replay3, signed, Opts)
        ]
    ),
    ?assertEqual(
        {ok, NewState},
        materialize_targets(ProcessID, NewState, Global, Opts)
    ),
    ?assertEqual(
        {ok, NewState},
        materialize_targets(
            ProcessID,
            NewState,
            Global#{ <<"to">> => 100 },
            Opts
        )
    ),
    ok = hb_store:stop(Store).
