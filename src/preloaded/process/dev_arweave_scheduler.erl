%%% @doc An AO scheduler whose ordering is the canonical Arweave base layer.
%%% One global synchronizer indexes each confirmed block once, routing every
%%% data-free transaction header to its native recipient and the processes in
%%% its `Assign-To' instruction. Process schedules then materialize only their
%%% own routed entries as contiguous AO slots.
-module(dev_arweave_scheduler).
-implements(<<"arweave-scheduler@1.0">>).
-device_libraries([lib_process]).
-export([info/0, router/4]).
-export([schedule/3, next/3, slot/3, status/3, sync/3, checkpoint/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(MAX_ASSIGNMENT_QUERY_LEN, 1000).

info() ->
    #{
        exports =>
            [
                <<"status">>,
                <<"sync">>,
                <<"next">>,
                <<"schedule">>,
                <<"slot">>,
                <<"init">>,
                <<"checkpoint">>
            ],
        excludes => [set, keys],
        default => fun router/4
    }.

router(_, Base, Req, Opts) -> schedule(Base, Req, Opts).

%% @doc Return the next dense assignment after the process's current slot.
next(Base, Req, Opts) ->
    ProcessID = hb_util:human_id(lib_process:process_id(Base, Req, Opts)),
    LastProcessed =
        hb_util:int(
            hb_maps:get(
                <<"at-slot">>,
                Base,
                -1,
                Opts#{ <<"hashpath">> => ignore }
            )
        ),
    maybe
        {ok, _} ?= dev_arweave_scheduler_sync:process(ProcessID, Opts),
        {ok, Assignment} ?=
            find_assignment(ProcessID, LastProcessed + 1, Opts),
        {ok, #{ <<"body">> => Assignment, <<"state">> => Base }}
    end.

find_assignment(ProcessID, Slot, Opts) ->
    case dev_arweave_scheduler_cache:read_assignment(ProcessID, Slot, Opts) of
        {ok, Assignment} -> {ok, Assignment};
        not_found ->
            {error,
                #{
                    <<"status">> => 404,
                    <<"reason">> =>
                        <<"Requested slot not yet available in schedule.">>
                }
            }
    end.

%% @doc Read a schedule or relay a presigned data-free L1 transaction.
schedule(Base, Req, Opts) ->
    case hb_util:key_to_atom(hb_maps:get(<<"method">>, Req, <<"GET">>, Opts)) of
        post -> post_schedule(Base, Req, Opts);
        get -> get_schedule(Base, Req, Opts)
    end.

get_schedule(Base, Req, Opts) ->
    ProcessID = find_process_id(Base, Req, Opts),
    {From, RequestedTo} = slot_range(Req, Opts),
    maybe
        {ok, #{ <<"next-slot">> := NextSlot }} ?=
            dev_arweave_scheduler_sync:process(ProcessID, Opts),
        Latest = NextSlot - 1,
        To =
            case RequestedTo of
                undefined -> Latest;
                _ -> min(RequestedTo, Latest)
            end,
        CappedTo = min(To, From + ?MAX_ASSIGNMENT_QUERY_LEN),
        Assignments = read_assignments(ProcessID, From, CappedTo, Opts),
        dev_arweave_scheduler_cache:assignments_to_bundle(
            ProcessID,
            Assignments,
            CappedTo < To,
            Opts
        )
    end.

post_schedule(Base, Req, Opts) ->
    ProcessID = find_process_id(Base, Req, Opts),
    maybe
        {ok, ToSchedule} ?= load_message(Base, Req, Opts),
        {ok, Committed} ?= committed(ToSchedule, Opts),
        {ok, TX} ?= valid_transaction(Committed, Opts),
        Targets = dev_arweave_scheduler_sync:targets(TX),
        true ?= lists:member(ProcessID, Targets),
        {ok, _} ?=
            hb_ao:resolve(
                {as, <<"arweave@2.9">>, Committed},
                #{
                    <<"path">> => <<"tx">>,
                    <<"method">> => <<"POST">>,
                    <<"target">> => <<"base">>
                },
                Opts
            ),
        {ok, _} = dev_arweave_scheduler_cache:write_header(Committed, Opts),
        {ok,
            #{
                <<"status">> => 202,
                <<"txid">> => hb_util:human_id(TX#tx.id),
                <<"targets">> => Targets,
                <<"body">> =>
                    <<"Transaction dispatched to Arweave. It will receive ",
                        "a slot after confirmation.">>
            }
        }
    else
        false ->
            {error,
                #{
                    <<"status">> => 422,
                    <<"reason">> =>
                        <<"Transaction does not target the requested process.">>,
                    <<"process">> => ProcessID
                }
            };
        Error -> Error
    end.

%% @doc Return the current contiguous slot after synchronizing this process.
slot(Base, Req, Opts) ->
    ProcessID = find_process_id(Base, Req, Opts),
    maybe
        {ok, #{ <<"next-slot">> := NextSlot }} ?=
            dev_arweave_scheduler_sync:process(ProcessID, Opts),
        {ok,
            #{
                <<"process">> => ProcessID,
                <<"current">> => NextSlot - 1,
                <<"cache-control">> => <<"no-store">>
            }
        }
    end.

%% @doc Proactively advance the shared global target index without touching a
%% particular process schedule.
sync(_Base, _Req, Opts) ->
    dev_arweave_scheduler_sync:sync(Opts).

%% @doc Report the global covered interval without scanning every process.
status(_Base, _Req, Opts) ->
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    case dev_arweave_scheduler_cache:read_global(Opts) of
        {ok, Global} ->
            {ok,
                #{
                    <<"address">> =>
                        hb_util:human_id(ar_wallet:to_address(Wallet)),
                    <<"sync">> => Global,
                    <<"cache-control">> => <<"no-store">>
                }
            };
        _ ->
            {ok,
                #{
                    <<"address">> =>
                        hb_util:human_id(ar_wallet:to_address(Wallet)),
                    <<"cache-control">> => <<"no-store">>
                }
            }
    end.

checkpoint(State) -> {ok, State}.

find_process_id(Base, Req, Opts) ->
    case hb_maps:get(<<"target">>, Req, not_found, Opts) of
        Target when Target =/= not_found -> hb_util:human_id(Target);
        not_found ->
            hb_util:human_id(lib_process:process_id(Base, Req, Opts))
    end.

slot_range(Req, Opts) ->
    From =
        case hb_maps:get(<<"from">>, Req, not_found, Opts) of
            not_found -> 0;
            FromValue -> max(0, hb_util:int(FromValue))
        end,
    To =
        case hb_maps:get(<<"to">>, Req, not_found, Opts) of
            not_found -> undefined;
            ToValue -> hb_util:int(ToValue)
        end,
    {From, To}.

read_assignments(_ProcessID, From, To, _Opts) when From > To -> [];
read_assignments(ProcessID, Slot, To, Opts) ->
    case dev_arweave_scheduler_cache:read_assignment(ProcessID, Slot, Opts) of
        {ok, Assignment} ->
            [Assignment | read_assignments(ProcessID, Slot + 1, To, Opts)];
        not_found -> []
    end.

load_message(Base, Req, Opts) ->
    Subject = hb_maps:get(<<"subject">>, Req, not_found, Opts),
    Raw =
        case Subject of
            <<"base">> -> Base;
            <<"self">> -> Req;
            not_found -> hb_maps:get(<<"body">>, Req, Req, Opts);
            Key -> hb_maps:get(Key, Req, not_found, Opts)
        end,
    case Raw of
        not_found -> missing_message();
        _ ->
            try {ok, hb_cache:ensure_all_loaded(Raw, Opts)}
            catch
                error:{necessary_message_not_found, _, _} -> missing_message()
            end
    end.

missing_message() ->
    {error,
        #{
            <<"status">> => 404,
            <<"reason">> => <<"Cannot fully load message to schedule.">>
        }
    }.

committed(Message, Opts) ->
    case hb_message:with_only_committed(Message, Opts) of
        {ok, Committed} -> {ok, Committed};
        {error, Reason} ->
            {error,
                #{
                    <<"status">> => 400,
                    <<"reason">> =>
                        <<"Committed components cannot be validated.">>,
                    <<"detail">> => Reason
                }
            }
    end.

valid_transaction(Message, Opts) ->
    case {
        lists:member(
            <<"tx@1.0">>,
            hb_message:commitment_devices(Message, Opts)
        ),
        hb_message:signers(Message, Opts),
        hb_message:verify(Message, signers, Opts)
    } of
        {true, [_ | _], true} ->
            try hb_message:convert(Message, <<"tx@1.0">>, Opts) of
                TX = #tx{ data_size = 0 } -> {ok, TX};
                #tx{} ->
                    {error,
                        #{
                            <<"status">> => 422,
                            <<"reason">> =>
                                <<"The Arweave scheduler accepts data-free ",
                                    "transaction headers only.">>
                        }
                    }
            catch
                _:_ -> invalid_transaction()
            end;
        _ -> invalid_transaction()
    end.

invalid_transaction() ->
    {error,
        #{
            <<"status">> => 422,
            <<"require-codec">> => <<"tx@1.0">>,
            <<"reason">> =>
                <<"Message must have a valid signed tx@1.0 commitment.">>
        }
    }.

%%% Tests

slot_range_test() ->
    ?assertEqual({0, undefined}, slot_range(#{}, #{})),
    ?assertEqual(
        {0, 42},
        slot_range(#{ <<"from">> => -5, <<"to">> => 42 }, #{})
    ).

invalid_commitment_requires_tx_codec_test() ->
    ProcessID = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Opts =
        #{
            <<"priv-wallet">> => ar_wallet:new(),
            <<"store">> => [hb_test_utils:test_store()]
        },
    Message = hb_message:commit(#{ <<"target">> => ProcessID }, Opts),
    ?assertMatch(
        {error,
            #{
                <<"status">> := 422,
                <<"require-codec">> := <<"tx@1.0">>
            }},
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave-scheduler@1.0">> },
            #{
                <<"path">> => <<"schedule">>,
                <<"method">> => <<"POST">>,
                <<"target">> => ProcessID,
                <<"body">> => Message
            },
            Opts
        )
    ).
