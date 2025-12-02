%%% @doc A decentralized scheduler that stores assignments as Arweave
%%% transactions. It consumes the `copycat/tx' hook (via `on_tx/3`) to ingest
%%% scheduler-tagged transactions and writes them into the scheduler cache.
%%% 
%%% API (compatible with `~process@1.0`):
%%%   - GET /slot            -> latest known slot from cache
%%%   - GET /schedule        -> cached assignments (rangeable via `from`/`to`)
%%%   - POST /schedule       -> publishes a scheduler transaction to Arweave
%%%   - GET /next            -> next assignment after `at-slot` on the request
%%%   - on_tx                -> hook handler to ingest scheduler transactions
%%% 
%%% Notes:
%%%   - Operators MUST ensure Arweave transactions are synced (e.g. via
%%%     `~copycat@1.0` with `include-txs=true` and chronological scans) before
%%%     exposing this scheduler. The on-chain order determines slot order.
%%%   - Transactions are identified by tags: `data-protocol=arweave-scheduler`,
%%%     `variant=1.0`, and must include a `process` tag. No `data` field is
%%%     used—payload is fully encoded in tags.
-module(dev_arweave_scheduler).
-export([info/0, router/4, schedule/3, slot/3, next/3, on_tx/3]).
-export([get_schedule/3, post_schedule/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(MAX_ASSIGNMENT_QUERY_LEN, 1000).

info() ->
    #{
        exports => [<<"schedule">>, <<"slot">>, <<"next">>, <<"on_tx">>],
        default => fun router/4
    }.

router(_, Base, Req, Opts) ->
    schedule(Base, Req, Opts).

%% @doc Route between GET/POST schedule requests.
schedule(Base, Req, Opts) ->
    case hb_util:key_to_atom(hb_ao:get(<<"method">>, Req, <<"GET">>, Opts)) of
        post -> post_schedule(Base, Req, Opts);
        get -> get_schedule(Base, Req, Opts)
    end.

%% @doc Publish a scheduler transaction to Arweave (no local slot assignment).
post_schedule(Base, Req, Opts) ->
    RawToSched = find_message_to_schedule(Base, Req, Opts),
    case hb_message:with_only_committed(RawToSched, Opts) of
        {ok, OnlyCommitted} ->
            ProcID = find_proc_id(Base, Req, OnlyCommitted, Opts),
            Stripped = hb_maps:without([<<"data">>], OnlyCommitted, Opts),
            Payload =
                hb_maps:merge(
                    #{
                        <<"data-protocol">> => <<"arweave-scheduler">>,
                        <<"variant">> => <<"1.0">>,
                        <<"process">> => ProcID
                    },
                    Stripped,
                    Opts
                ),
            case sign_with_price(Payload, Opts) of
                {ok, SignedMsg} ->
                    case dev_arweave:post_tx(Base, SignedMsg, Opts) of
                        {ok, Res} ->
                            {ok,
                                #{
                                    <<"status">> => 202,
                                    <<"body">> => Res,
                                    <<"tx-id">> =>
                                        hb_message:id(
                                            SignedMsg,
                                            signed,
                                            Opts
                                        )
                                }
                            };
                        Error -> Error
                    end;
                {error, Err} ->
                    {error,
                        #{
                            <<"status">> => 502,
                            <<"reason">> => Err
                        }
                    }
            end;
        {error, Err} ->
            {error,
                #{
                    <<"status">> => 400,
                    <<"reason">> => Err
                }
            }
    end.

%% @doc Return cached schedule for a process.
get_schedule(Base, Req, Opts) ->
    ProcID = find_proc_id(Base, Req, Opts),
    From0 = hb_ao:get(<<"from">>, Req, 0, Opts),
    From = case From0 < 0 of true -> 0; false -> hb_util:int(From0) end,
    To =
        case hb_ao:get(<<"to">>, Req, not_found, Opts) of
            not_found -> undefined;
            Val -> hb_util:int(Val)
        end,
    {Assignments, More} = read_assignments(ProcID, From, To, Opts),
    Format = hb_ao:get(<<"accept">>, Req, <<"application/http">>, Opts),
    case uri_string:percent_decode(Format) of
        <<"application/aos-2">> ->
            dev_scheduler_formats:assignments_to_aos2(
                ProcID,
                Assignments,
                More,
                Opts
            );
        _ ->
            dev_scheduler_formats:assignments_to_bundle(
                ProcID,
                Assignments,
                More,
                Opts
            )
    end.

%% @doc Next assignment after `at-slot' on the base/request.
next(Base, Req, Opts) ->
    ProcID = find_proc_id(Base, Req, Opts),
    LastProcessed =
        hb_util:int(
            hb_ao:get(<<"at-slot">>, Base, -1, Opts#{ hashpath => ignore })
        ),
    TargetSlot = LastProcessed + 1,
    case dev_scheduler_cache:read(ProcID, TargetSlot, scheduler_opts(Opts)) of
        {ok, Assignment} ->
            {ok, #{ <<"body">> => Assignment, <<"state">> => Base }};
        not_found ->
            {error,
                #{
                    <<"status">> => 404,
                    <<"reason">> =>
                        <<"Requested slot not yet available in schedule.">>
                }
            }
    end.

%% @doc Current slot from cache.
slot(Base, Req, Opts) ->
    ProcID = find_proc_id(Base, Req, Opts),
    Current =
        case dev_scheduler_cache:latest(ProcID, scheduler_opts(Opts)) of
            not_found -> -1;
            {Slot, _} -> Slot
        end,
    {Timestamp, Height, Hash} = ar_timestamp:get(),
    {ok,
        #{
            <<"process">> => ProcID,
            <<"current">> => Current,
            <<"timestamp">> => Timestamp,
            <<"block-height">> => Height,
            <<"block-hash">> => Hash,
            <<"cache-control">> => <<"no-store">>
        }
    }.

%% @doc Hook target: ingest scheduler-tagged Arweave txs from copycat.
on_tx(_Base, Req, Opts) ->
    TxRec = maps:get(<<"tx-record">>, Req, undefined),
    Decoded =
        case TxRec of
            undefined ->
                hb_ao:get(<<"body">>, Req, Req, Opts);
            _ ->
                hb_util:ok(dev_codec_tx:from(TxRec, #{}, Opts))
        end,
    TxMsg = hb_cache:ensure_all_loaded(Decoded, Opts),
    case is_scheduler_tx(TxMsg, Opts) of
        false -> {ok, ignored};
        true ->
            case hb_ao:get(<<"process">>, TxMsg, not_found, Opts) of
                not_found ->
                    {error, #{ <<"reason">> => <<"Scheduler tx missing `process` tag.">> }};
                ProcVal ->
                    ProcID = hb_util:human_id(ProcVal),
                    TxID =
                        case TxRec of
                            #tx{} -> hb_util:encode(ar_tx:id(TxRec, signed));
                            _ -> hb_ao:get(<<"id">>, TxMsg, <<>>, Opts)
                        end,
                    case already_indexed(ProcID, TxID, Opts) of
                        true -> {ok, already_cached};
                        false ->
                            Assignment =
                                build_assignment(
                                    ProcID,
                                    TxID,
                                    TxMsg,
                                    Req,
                                    Opts
                                ),
                            write_assignment(Assignment, ProcID, TxID, Opts),
                            {ok, Assignment}
                    end
            end
    end.

%%% Helpers

read_assignments(ProcID, From, undefined, Opts) ->
    case dev_scheduler_cache:latest(ProcID, scheduler_opts(Opts)) of
        not_found -> {[], false};
        {Slot, _} -> read_assignments(ProcID, From, Slot, Opts)
    end;
read_assignments(ProcID, From, RequestedTo, Opts) ->
    SchedulerOpts = scheduler_opts(Opts),
    To =
        case (RequestedTo - From) > ?MAX_ASSIGNMENT_QUERY_LEN of
            true -> From + ?MAX_ASSIGNMENT_QUERY_LEN;
            false -> RequestedTo
        end,
    Assignments =
        read_slots(
            ProcID,
            From,
            To,
            SchedulerOpts
        ),
    {Assignments, To < RequestedTo}.

read_slots(_ProcID, Current, To, _Opts) when Current > To -> [];
read_slots(ProcID, Current, To, Opts) ->
    case dev_scheduler_cache:read(ProcID, Current, Opts) of
        {ok, Assignment} ->
            [hb_cache:ensure_all_loaded(Assignment, Opts)
                | read_slots(ProcID, Current + 1, To, Opts)];
        not_found ->
            read_slots(ProcID, Current + 1, To, Opts)
    end.

find_proc_id(Base, Req, Opts) ->
    case hb_ao:get(<<"process">>, Req, not_found, Opts) of
        not_found ->
            case hb_ao:get(<<"process">>, Base, not_found, Opts) of
                not_found ->
                    dev_process_lib:process_id(Base, Req, Opts);
                Proc -> hb_util:human_id(Proc)
            end;
        Proc ->
            hb_util:human_id(Proc)
    end.

find_proc_id(Base, Req, ToSched, Opts) ->
    case hb_ao:get(<<"type">>, ToSched, not_found, Opts) of
        <<"Process">> -> dev_process_lib:process_id(ToSched, #{}, Opts);
        _ ->
            case hb_ao:get(<<"process">>, ToSched, not_found, Opts) of
                not_found -> find_proc_id(Base, Req, Opts);
                Proc -> hb_util:human_id(Proc)
            end
    end.

find_message_to_schedule(_Base, Req, Opts) ->
    Subject =
        hb_ao:get(
            <<"subject">>,
            Req,
            not_found,
            Opts#{ hashpath => ignore }
        ),
    case Subject of
        <<"self">> -> Req;
        not_found ->
            hb_ao:get(<<"body">>, Req, Req, Opts#{ hashpath => ignore });
        Subject ->
            hb_ao:get(Subject, Req, Opts#{ hashpath => ignore })
    end.

is_scheduler_tx(TxMsg, Opts) ->
    Proto = hb_maps:get(<<"data-protocol">>, TxMsg, <<>>, Opts),
    Var = hb_maps:get(<<"variant">>, TxMsg, <<>>, Opts),
    hb_util:to_lower(Proto) == <<"arweave-scheduler">> andalso
        hb_util:to_lower(Var) == <<"1.0">>.

scheduler_opts(Opts) ->
    Opts#{
        store =>
            hb_opts:get(
                scheduler_store,
                hb_opts:get(store, no_viable_store, Opts),
                Opts
            )
    }.

next_slot(ProcID, Opts) ->
    case dev_scheduler_cache:latest(ProcID, scheduler_opts(Opts)) of
        not_found -> 0;
        {Slot, _} -> Slot + 1
    end.

build_assignment(ProcID, TxID, TxMsg, Req, Opts) ->
    {Timestamp, Height, Hash} = ar_timestamp:get(),
    BlockHeight = hb_maps:get(<<"block-height">>, Req, Height, Opts),
    BlockHash = hb_maps:get(<<"block-hash">>, Req, hb_util:human_id(Hash), Opts),
    Body =
        hb_maps:without(
            [<<"data-protocol">>, <<"variant">>],
            TxMsg,
            Opts
        ),
    Slot = next_slot(ProcID, Opts),
    PathBin =
        case hb_path:from_message(request, Body, Opts) of
            undefined -> <<"compute">>;
            Path -> hb_path:to_binary(Path)
        end,
    #{
        <<"variant">> => <<"ao.N.1">>,
        <<"data-protocol">> => <<"ao">>,
        <<"type">> => <<"Assignment">>,
        <<"tx-id">> => TxID,
        <<"process">> => ProcID,
        <<"slot">> => Slot,
        <<"block-height">> => BlockHeight,
        <<"block-hash">> => BlockHash,
        <<"timestamp">> => Timestamp,
        <<"path">> => PathBin,
        <<"body">> => Body
    }.

already_indexed(ProcID, TxID, Opts) ->
    case TxID of
        <<>> -> false;
        _ ->
            case hb_cache:read(tx_link_path(ProcID, TxID, scheduler_opts(Opts)), Opts) of
                {ok, _} -> true;
                _ -> false
            end
    end.

write_assignment(Assignment, ProcID, TxID, Opts) ->
    SchedulerOpts = scheduler_opts(Opts),
    % Write to scheduler cache for slot linkage.
    _ = dev_scheduler_cache:write(Assignment, SchedulerOpts),
    % Ensure a root path we can link by tx id.
    {ok, RootPath} = hb_cache:write(Assignment, SchedulerOpts),
    hb_cache:link(RootPath, tx_link_path(ProcID, TxID, SchedulerOpts), SchedulerOpts).

tx_link_path(ProcID, TxID, Opts) ->
    hb_store:path(
        hb_opts:get(store, no_viable_store, Opts),
        [
            <<"~arweave-scheduler@1.0">>,
            <<"tx">>,
            hb_util:human_id(ProcID),
            hb_util:human_id(TxID)
        ]
    ).

sign_with_price(Payload, Opts) ->
    Wallet = hb_opts:get(priv_wallet, no_viable_wallet, Opts),
    case dev_codec_tx:to(Payload, #{}, Opts) of
        {ok, TX} ->
            DataSize = TX#tx.data_size,
            PriceRes =
                case hb_opts:get(arweave_price_fun, undefined, Opts) of
                    undefined ->
                        dev_arweave:price(
                            #{},
                            #{ <<"size">> => DataSize },
                            Opts
                        );
                    Fun when is_function(Fun, 3) ->
                        Fun(#{}, #{ <<"size">> => DataSize }, Opts)
                end,
            AnchorRes =
                case hb_opts:get(arweave_anchor_fun, undefined, Opts) of
                    undefined -> dev_arweave:tx_anchor(#{}, #{}, Opts);
                    Fun2 when is_function(Fun2, 3) ->
                        Fun2(#{}, #{}, Opts)
                end,
            case {PriceRes, AnchorRes} of
                {{ok, Price}, {ok, Anchor}} ->
                    Signed =
                        ar_tx:sign(
                            TX#tx{ reward = Price, anchor = Anchor },
                            Wallet
                        ),
                    {
                        ok,
                        hb_message:convert(
                            Signed,
                            #{ <<"device">> => <<"structured@1.0">> },
                            #{ <<"device">> => <<"tx@1.0">> },
                            Opts
                        )
                    };
                {{error, _} = Err, _} -> Err;
                {_, {error, _} = Err} -> Err
            end;
        Error -> {error, Error}
    end.

%%% Tests

basic_slot_and_next_test() ->
    Store = hb_test_utils:test_store(),
    hb_store:start(Store),
    Opts = scheduler_opts(#{ store => [Store] }),
    ProcID = hb_util:encode(crypto:strong_rand_bytes(32)),
    Assignment =
        #{
            <<"variant">> => <<"ao.N.1">>,
            <<"process">> => ProcID,
            <<"slot">> => 0,
            <<"body">> => #{ <<"hello">> => <<"world">> }
        },
    ok = dev_scheduler_cache:write(Assignment, Opts),
    {ok, SlotRes} = slot(#{}, #{ <<"process">> => ProcID }, Opts),
    ?assertMatch(#{ <<"current">> := 0 }, SlotRes),
    {ok, Next} = next(#{ <<"at-slot">> => -1 }, #{ <<"process">> => ProcID }, Opts),
    ?assertMatch(#{ <<"body">> := _ }, Next).

on_tx_writes_assignment_test() ->
    Store = hb_test_utils:test_store(),
    hb_store:start(Store),
    Opts = scheduler_opts(#{ store => [Store] }),
    ProcID = hb_util:encode(crypto:strong_rand_bytes(32)),
    TxMsg = #{
        <<"data-protocol">> => <<"arweave-scheduler">>,
        <<"variant">> => <<"1.0">>,
        <<"process">> => ProcID,
        <<"type">> => <<"Message">>,
        <<"payload">> => <<"hi">>
    },
    {ok, _} = on_tx(#{}, #{ <<"body">> => TxMsg, <<"block-height">> => 1 }, Opts),
    {ok, Cached} = dev_scheduler_cache:read(ProcID, 0, Opts),
    ?assertEqual(ProcID, hb_ao:get(<<"process">>, Cached, Opts)),
    ?assertEqual(0, hb_ao:get(<<"slot">>, Cached, Opts)).

ordered_processing_after_sync_test() ->
    Store = hb_test_utils:test_store(),
    hb_store:start(Store),
    {ok, ScriptBin} = file:read_file("scripts/sched_append.lua"),
    Wallet = hb:wallet(),
    ProcMsg =
        #{
            <<"device">> => <<"process@1.0">>,
            <<"type">> => <<"Process">>,
            <<"execution-device">> => <<"lua@5.3a">>,
            <<"module">> =>
                #{
                    <<"content-type">> => <<"application/lua">>,
                    <<"body">> => ScriptBin
                },
            <<"scheduler-device">> => <<"arweave-scheduler@1.0">>
        },
    ProcID = hb_message:id(hb_message:commit(ProcMsg, Wallet), signed, #{}),
    TxIds =
        [
            hb_util:encode(crypto:strong_rand_bytes(32)),
            hb_util:encode(crypto:strong_rand_bytes(32)),
            hb_util:encode(crypto:strong_rand_bytes(32))
        ],
    IdToChar =
        maps:from_list(
            lists:zip(
                TxIds,
                [<<"A">>, <<"B">>, <<"C">>]
            )
        ),
    Block =
        #{
            <<"height">> => 1,
            <<"indep_hash">> => hb_util:encode(crypto:strong_rand_bytes(32)),
            <<"hash">> => hb_util:encode(crypto:strong_rand_bytes(32)),
            <<"txs">> => TxIds
        },
    BaseOpts = #{
        store => [Store],
        scheduler_store => [Store],
        priv_wallet => Wallet,
        mode => debug
    },
    lists:foreach(
        fun(TxId) ->
            {ok, _} =
                on_tx(
                    #{},
                    #{
                        <<"body">> =>
                            #{
                                <<"data-protocol">> => <<"arweave-scheduler">>,
                                <<"variant">> => <<"1.0">>,
                                <<"process">> => ProcID,
                                <<"type">> => <<"Message">>,
                                <<"id">> => TxId,
                                <<"char">> => maps:get(TxId, IdToChar),
                                <<"block-height">> =>
                                    hb_maps:get(<<"height">>, Block, BaseOpts)
                            }
                    },
                    BaseOpts
                )
        end,
        TxIds
    ),
    Slots = dev_scheduler_cache:list(ProcID, BaseOpts),
    ?assertEqual([0, 1, 2], lists:sort(Slots)),
    OrderedChars =
        [
            begin
                {ok, Assignment} =
                    dev_scheduler_cache:read(ProcID, Slot, BaseOpts),
                hb_ao:get(<<"body/char">>, Assignment, BaseOpts)
            end
         || Slot <- lists:sort(Slots)
        ],
    ?assertEqual([<<"A">>, <<"B">>, <<"C">>], OrderedChars).

post_schedule_fetches_price_and_anchor_test() ->
    Price = 12345,
    Anchor = rand:bytes(32),
    Store = hb_test_utils:test_store(),
    hb_store:start(Store),
    ProcID = hb_util:encode(crypto:strong_rand_bytes(32)),
    Body = #{
        <<"type">> => <<"Message">>,
        <<"process">> => ProcID,
        <<"hello">> => <<"world">>
    },
    Payload =
        hb_maps:merge(
            #{
                <<"data-protocol">> => <<"arweave-scheduler">>,
                <<"variant">> => <<"1.0">>,
                <<"process">> => ProcID
            },
            Body,
            #{}
        ),
    Opts =
        #{
            priv_wallet => hb:wallet(),
            store => [Store],
            arweave_price_fun =>
                fun(_, _, _) -> {ok, Price} end,
            arweave_anchor_fun =>
                fun(_, _, _) -> {ok, Anchor} end
        },
    {ok, SignedMsg} = sign_with_price(Payload, Opts),
    TxId = hb_message:id(SignedMsg, signed, Opts),
    {ok, TX} = dev_codec_tx:to(SignedMsg, #{}, Opts),
    ?assertEqual(Price, TX#tx.reward),
    ?assertEqual(Anchor, TX#tx.anchor),
    ?assertMatch(<<_Id:43/binary>>, TxId),
    ok.
