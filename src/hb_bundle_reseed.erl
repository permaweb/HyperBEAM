%%% @doc Reseed bundle transactions, or transactions from selected blocks, by
%%% rebuilding their chunk proofs and posting them back to Arweave nodes.
-module(hb_bundle_reseed).
-export([main/1]).

-include("include/hb.hrl").

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(DEFAULT_LOG_SUFFIX, <<".failed.log">>).
-define(DEFAULT_NODE_POST_CONCURRENCY, 5).
-define(DEFAULT_TURBO_URL, <<"https://turbo-gateway.com">>).

main(Args) ->
    case parse_args(Args) of
        {ok, Opts} ->
            case run(Opts) of
                ok ->
                    ok;
                {error, Reason} ->
                    print_error(Reason),
                    {error, Reason}
            end;
        {error, Reason} ->
            print_error(Reason),
            print_usage(),
            {error, Reason}
    end.

run(Opts) ->
    ok = ensure_started(),
    with_log(
        maps:get(log, Opts),
        fun(LogDevice) ->
            case process_source(LogDevice, Opts) of
                Stats when is_map(Stats) ->
                    print_summary(Stats, maps:get(log, Opts)),
                    ok;
                {error, _} = Error ->
                    Error
            end
        end
    ).

parse_args(Args) ->
    parse_args(
        Args,
        #{
            blocks => undefined,
            input => <<"-">>,
            log => default_log_path(<<"-">>),
            node_post_concurrency => ?DEFAULT_NODE_POST_CONCURRENCY,
            positionals => [],
            target_successes => infinity,
            turbo_url => ?DEFAULT_TURBO_URL
        }
    ).

parse_args([], Opts = #{ positionals := Positionals }) ->
    normalize_positionals(Opts#{ positionals => lists:reverse(Positionals) });
parse_args(["-h" | _], _Opts) ->
    {error, help_requested};
parse_args(["--help" | _], _Opts) ->
    {error, help_requested};
parse_args(["--blocks", RawValue | Rest], Opts) ->
    case parse_blocks(RawValue) of
        {ok, Heights} ->
            parse_args(Rest, Opts#{ blocks => Heights });
        {error, _} = Error ->
            Error
    end;
parse_args([[$-, $-, $b, $l, $o, $c, $k, $s, $= | RawValue] | Rest], Opts) ->
    case parse_blocks(RawValue) of
        {ok, Heights} ->
            parse_args(Rest, Opts#{ blocks => Heights });
        {error, _} = Error ->
            Error
    end;
parse_args(["--log", Log | Rest], Opts) ->
    parse_args(Rest, Opts#{ log => list_to_binary(Log) });
parse_args(["--node-post-concurrency", RawValue | Rest], Opts) ->
    case parse_positive_integer(RawValue, node_post_concurrency) of
        {ok, Value} ->
            parse_args(Rest, Opts#{ node_post_concurrency => Value });
        {error, _} = Error ->
            Error
    end;
parse_args(["--target-successes", RawValue | Rest], Opts) ->
    case parse_positive_integer(RawValue, target_successes) of
        {ok, Value} ->
            parse_args(Rest, Opts#{ target_successes => Value });
        {error, _} = Error ->
            Error
    end;
parse_args(["--turbo-url", URL | Rest], Opts) ->
    parse_args(Rest, Opts#{ turbo_url => list_to_binary(URL) });
parse_args(["--" | Rest], Opts) ->
    parse_args_positionals(Rest, Opts);
parse_args([Arg = [$- | _] | _], _Opts) ->
    {error, {unknown_flag, list_to_binary(Arg)}};
parse_args([Arg | Rest], Opts = #{ positionals := Positionals }) ->
    parse_args(Rest, Opts#{ positionals => [list_to_binary(Arg) | Positionals] }).

parse_args_positionals([], Opts) ->
    parse_args([], Opts);
parse_args_positionals([Arg | Rest], Opts = #{ positionals := Positionals }) ->
    parse_args_positionals(
        Rest,
        Opts#{ positionals => [list_to_binary(Arg) | Positionals] }
    ).

normalize_positionals(Opts = #{ blocks := undefined, positionals := [] }) ->
    {ok, maps:remove(positionals, Opts)};
normalize_positionals(
    Opts = #{ blocks := undefined, positionals := [Input] }
) ->
    {ok,
        maps:remove(
            positionals,
            Opts#{
                input => Input,
                log => default_log_path(Input)
            }
        )};
normalize_positionals(Opts = #{ blocks := _Heights, positionals := [] }) ->
    {ok, maps:remove(positionals, Opts)};
normalize_positionals(#{ blocks := _Heights }) ->
    {error, blocks_with_input};
normalize_positionals(_Opts) ->
    {error, too_many_positionals}.

parse_positive_integer(RawValue, Key) ->
    try list_to_integer(RawValue) of
        Value when Value > 0 ->
            {ok, Value};
        _ ->
            {error, {invalid_integer, Key, list_to_binary(RawValue)}}
    catch
        error:badarg ->
            {error, {invalid_integer, Key, list_to_binary(RawValue)}}
    end.

default_log_path(<<"-">>) ->
    <<"bundle-reseed", ?DEFAULT_LOG_SUFFIX/binary>>;
default_log_path(Input) ->
    <<Input/binary, ?DEFAULT_LOG_SUFFIX/binary>>.

ensure_started() ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(ssl),
    {ok, _} = application:ensure_all_started(gun),
    ok = hb:init(),
    ok = hb_http:start(),
    hb_http_client:setup_conn(#{}),
    case whereis(hb_http_client) of
        undefined ->
            {ok, _} = hb_http_client:start_link(#{});
        _ ->
            ok
    end,
    ok.

initial_stats() ->
    #{
        blocks => 0,
        block_failures => 0,
        processed => 0,
        roots => #{},
        successes => 0,
        skipped => 0,
        proofs => 0,
        tx_posts => 0,
        chunk_posts => 0
    }.

process_source(LogDevice, Opts) ->
    case maps:get(blocks, Opts) of
        undefined ->
            with_input(
                maps:get(input, Opts),
                fun(InputDevice) ->
                    process_bundle_ids(
                        InputDevice,
                        LogDevice,
                        Opts,
                        initial_stats()
                    )
                end
            );
        Heights ->
            process_blocks(Heights, LogDevice, Opts, initial_stats())
    end.

process_bundle_ids(InputDevice, LogDevice, Opts, Stats) ->
    case target_reached(Stats, Opts) of
        true ->
            Stats;
        false ->
            case io:get_line(InputDevice, "") of
                eof ->
                    Stats;
                RawLine ->
                    case parse_bundle_id(RawLine) of
                        skip ->
                            process_bundle_ids(InputDevice, LogDevice, Opts, Stats);
                        ID ->
                            NextStats =
                                process_one_bundle(ID, LogDevice, Opts, Stats),
                            process_bundle_ids(
                                InputDevice,
                                LogDevice,
                                Opts,
                                NextStats
                            )
                    end
            end
    end.

process_blocks([], _LogDevice, _Opts, Stats) ->
    Stats;
process_blocks(Heights, LogDevice, Opts, Stats) ->
    case target_reached(Stats, Opts) of
        true ->
            Stats;
        false ->
            process_blocks_continue(Heights, LogDevice, Opts, Stats)
    end.

process_blocks_continue([], _LogDevice, _Opts, Stats) ->
    Stats;
process_blocks_continue([Height | Rest], LogDevice, Opts, Stats) ->
    print_block_stage(Height, fetch),
    TriedStats = Stats#{ blocks => maps:get(blocks, Stats) + 1 },
    case fetch_block_tx_ids(Height, Opts) of
        {ok, TXIDs} ->
            print_block_loaded(Height, length(TXIDs)),
            NextStats =
                process_block_tx_ids(
                    Height,
                    TXIDs,
                    1,
                    length(TXIDs),
                    LogDevice,
                    Opts,
                    TriedStats
                ),
            print_block_complete(Height, length(TXIDs), TriedStats, NextStats),
            process_blocks(Rest, LogDevice, Opts, NextStats);
        {error, Reason} ->
            log_block_skip(LogDevice, Height, Reason),
            print_block_skip(Height, Reason),
            process_blocks(
                Rest,
                LogDevice,
                Opts,
                TriedStats#{
                    block_failures => maps:get(block_failures, TriedStats) + 1
                }
            )
    end.

process_block_tx_ids(
    _Height,
    [],
    _Index,
    _Total,
    _LogDevice,
    _Opts,
    Stats
) ->
    Stats;
process_block_tx_ids(
    Height,
    TXIDs,
    Index,
    Total,
    LogDevice,
    Opts,
    Stats
) ->
    case target_reached(Stats, Opts) of
        true ->
            Stats;
        false ->
            process_block_tx_ids_continue(
                Height,
                TXIDs,
                Index,
                Total,
                LogDevice,
                Opts,
                Stats
            )
    end.

process_block_tx_ids_continue(
    Height,
    [ID | Rest],
    Index,
    Total,
    LogDevice,
    Opts,
    Stats
) ->
    print_block_tx(Height, Index, Total, ID),
    NextStats = process_one_bundle(ID, LogDevice, Opts, Stats),
    process_block_tx_ids(
        Height,
        Rest,
        Index + 1,
        Total,
        LogDevice,
        Opts,
        NextStats
    ).

target_reached(Stats, Opts) ->
    case maps:get(target_successes, Opts) of
        infinity -> false;
        Target -> maps:get(successes, Stats) >= Target
    end.

parse_bundle_id(RawLine) ->
    Line = trim(unicode:characters_to_binary(RawLine)),
    case Line of
        <<>> ->
            skip;
        <<"#", _/binary>> ->
            skip;
        ID when byte_size(ID) =:= 43 ->
            ID;
        _ ->
            skip
    end.

parse_blocks(RawValue) ->
    parse_blocks(trim(unicode:characters_to_binary(RawValue)), []).

parse_blocks(<<>>, _Acc) ->
    {error, empty_blocks};
parse_blocks(RawValue, Acc) ->
    Segments = binary:split(RawValue, <<",">>, [global]),
    case parse_block_segments(Segments, Acc) of
        {ok, Heights} ->
            {ok, dedupe_heights(Heights)};
        {error, _} = Error ->
            Error
    end.

parse_block_segments([], Acc) ->
    {ok, lists:reverse(Acc)};
parse_block_segments([Segment | Rest], Acc) ->
    case parse_block_segment(trim(Segment)) of
        {ok, Heights} ->
            parse_block_segments(Rest, lists:reverse(Heights) ++ Acc);
        {error, _} = Error ->
            Error
    end.

parse_block_segment(<<>>) ->
    {error, empty_block_segment};
parse_block_segment(Segment) ->
    case binary:split(Segment, <<"-">>, [global]) of
        [Height] ->
            case parse_block_height(trim(Height)) of
                {ok, Parsed} -> {ok, [Parsed]};
                {error, _} = Error -> Error
            end;
        [FromRaw, ToRaw] ->
            case
                {
                    parse_block_height(trim(FromRaw)),
                    parse_block_height(trim(ToRaw))
                }
            of
                {{ok, From}, {ok, To}} when From =< To ->
                    {ok, lists:seq(From, To)};
                {{ok, From}, {ok, To}} ->
                    {ok, lists:seq(From, To, -1)};
                {{error, _} = Error, _} ->
                    Error;
                {_, {error, _} = Error} ->
                    Error
            end;
        _ ->
            {error, {invalid_block_segment, Segment}}
    end.

parse_block_height(RawValue) ->
    try binary_to_integer(RawValue) of
        Height when Height >= 0 ->
            {ok, Height};
        _ ->
            {error, {invalid_block_height, RawValue}}
    catch
        error:badarg ->
            {error, {invalid_block_height, RawValue}}
    end.

dedupe_heights(Heights) ->
    lists:reverse(
        element(
            1,
            lists:foldl(
                fun(Height, {Acc, Seen}) ->
                    case maps:is_key(Height, Seen) of
                        true ->
                            {Acc, Seen};
                        false ->
                            {[Height | Acc], Seen#{ Height => true }}
                    end
                end,
                {[], #{}},
                Heights
            )
        )
    ).

trim(Binary) ->
    trim_right(trim_left(Binary)).

trim_left(<<C, Rest/binary>>) when C =:= $\s; C =:= $\t; C =:= $\n; C =:= $\r ->
    trim_left(Rest);
trim_left(Binary) ->
    Binary.

trim_right(Binary) ->
    trim_right(Binary, byte_size(Binary)).

trim_right(_Binary, 0) ->
    <<>>;
trim_right(Binary, Size) ->
    case binary:at(Binary, Size - 1) of
        C when C =:= $\s; C =:= $\t; C =:= $\n; C =:= $\r ->
            trim_right(Binary, Size - 1);
        _ ->
            binary:part(Binary, 0, Size)
    end.

process_one_bundle(ID, LogDevice, Opts, Stats) ->
    Processed = maps:get(processed, Stats) + 1,
    io:format("START ~ts~n", [ID]),
    case reseed_bundle(ID, Opts) of
        {ok, Result} ->
            RootTXID = maps:get(root_tx_id, Result),
            case maps:is_key(RootTXID, maps:get(roots, Stats)) of
                true ->
                    print_bundle_duplicate(ID, RootTXID),
                    Stats#{
                        processed => Processed,
                        skipped => maps:get(skipped, Stats) + 1
                    };
                false ->
                    print_bundle_success(ID, Result),
                    Stats#{
                        processed => Processed,
                        roots => (maps:get(roots, Stats))#{ RootTXID => true },
                        successes => maps:get(successes, Stats) + 1,
                        proofs => maps:get(proofs, Stats) + maps:get(proofs, Result),
                        tx_posts => maps:get(tx_posts, Stats) + maps:get(tx_posts, Result),
                        chunk_posts =>
                            maps:get(chunk_posts, Stats)
                                + maps:get(chunk_posts, Result)
                    }
            end;
        {skip, Stage, Reason} ->
            log_skip(LogDevice, ID, Stage, Reason),
            print_bundle_skip(ID, Stage, Reason),
            Stats#{
                processed => Processed,
                skipped => maps:get(skipped, Stats) + 1
            }
    end.

reseed_bundle(ID, Opts) ->
    with_bundle_data(
        ID,
        Opts,
        fun(RootTXID, TX, StartOffset) ->
            print_bundle_stage(ID, RootTXID, proof_build),
            Proofs = build_proofs(TX),
            print_bundle_proofs(ID, RootTXID, length(Proofs)),
            case seed_proofs(RootTXID, Proofs, StartOffset, Opts) of
                {ok, ChunkPostCount} ->
                    {ok,
                        #{
                            data_size => TX#tx.data_size,
                            root_tx_id => RootTXID,
                            proofs => length(Proofs),
                            tx_posts => 0,
                            chunk_posts => ChunkPostCount
                        }};
                {error, Stage, Reason} ->
                    {skip, Stage, Reason}
            end
        end
    ).

with_bundle_data(ID, Opts, Fun) ->
    print_bundle_stage(ID, turbo_fetch),
    case fetch_turbo(ID, Opts) of
        {ok, Turbo} ->
            RootTXID = maps:get(root_tx_id, Turbo),
            print_bundle_stage(ID, RootTXID, root_tx_data_fetch),
            case fetch_root_tx_data(RootTXID, Turbo, Opts) of
                {ok, Data} ->
                    print_bundle_stage(ID, RootTXID, tx_header_fetch),
                    case fetch_tx_header(RootTXID, Opts) of
                        {ok, HeaderMsg} ->
                            print_bundle_stage(ID, RootTXID, tx_offset_fetch),
                            case fetch_tx_start_offset(RootTXID, Opts) of
                                {ok, StartOffset} ->
                                    print_bundle_stage(ID, RootTXID, tx_verify),
                                    case build_verified_tx(HeaderMsg, Data, Opts) of
                                        {ok, TX} -> Fun(RootTXID, TX, StartOffset);
                                        {error, Reason} -> {skip, tx_verify, Reason}
                                    end;
                                {error, Reason} ->
                                    {skip, tx_offset_fetch, #{ id => RootTXID, reason => Reason }}
                            end;
                        {error, Reason} ->
                            {skip, tx_header_fetch, #{ id => RootTXID, reason => Reason }}
                    end;
                {error, Reason} ->
                    {skip, root_turbo_fetch, #{ id => RootTXID, reason => Reason }}
            end;
        {error, Reason} ->
            {skip, turbo_fetch, Reason}
    end.

fetch_turbo(ID, Opts) ->
    URL = <<(maps:get(turbo_url, Opts))/binary, "/", ID/binary>>,
    Request = {binary_to_list(URL), []},
    case httpc:request(get, Request, [{autoredirect, true}], [{body_format, binary}]) of
        {ok, {{_, 200, _}, Headers, Body}} ->
            NormalizedHeaders = normalize_headers(Headers),
            {ok,
                #{
                    id => ID,
                    body => Body,
                    headers => NormalizedHeaders,
                    root_tx_id =>
                        maps:get(
                            <<"x-ar-io-root-transaction-id">>,
                            NormalizedHeaders,
                            ID
                        )
                }};
        {ok, {{_, Status, _}, Headers, Body}} ->
            {error,
                #{
                    id => ID,
                    status => Status,
                    headers => normalize_headers(Headers),
                    body => Body
                }};
        {error, Reason} ->
            {error, {http_error, Reason}}
    end.

fetch_root_tx_data(RootTXID, #{ id := RootTXID, body := Body }, _Opts) ->
    {ok, Body};
fetch_root_tx_data(RootTXID, _Turbo, Opts) ->
    case hb_http:request(
        #{
            <<"method">> => <<"GET">>,
            <<"path">> => <<"/arweave/raw/", RootTXID/binary>>
        },
        request_opts(Opts)
    ) of
        {ok, Response} ->
            {ok, maps:get(<<"body">>, Response, <<>>)};
        Error ->
            {error, Error}
    end.

normalize_headers(Headers) ->
    maps:from_list(
        lists:map(
            fun({Key, Value}) ->
                {
                    hb_util:to_lower(list_to_binary(Key)),
                    list_to_binary(Value)
                }
            end,
            Headers
        )
    ).

fetch_tx_header(ID, Opts) ->
    hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"tx">>,
            <<"tx">> => ID,
            <<"exclude-data">> => true
        },
        Opts
    ).

fetch_tx_start_offset(ID, Opts) ->
    case hb_http:request(
        #{
            <<"method">> => <<"GET">>,
            <<"path">> => <<"/arweave/tx/", ID/binary, "/offset">>
        },
        request_opts(Opts)
    ) of
        {ok, Response} ->
            OffsetMsg = hb_json:decode(maps:get(<<"body">>, Response)),
            EndOffset = hb_util:int(maps:get(<<"offset">>, OffsetMsg)),
            Size = hb_util:int(maps:get(<<"size">>, OffsetMsg)),
            {ok, EndOffset - Size};
        Error ->
            {error, Error}
    end.

build_verified_tx(HeaderMsg, Data, Opts) ->
    TXHeader = hb_message:convert(
        HeaderMsg,
        <<"tx@1.0">>,
        <<"structured@1.0">>,
        Opts
    ),
    TX = TXHeader#tx{ data = Data },
    case
        TX#tx.data_size =:= byte_size(Data)
            andalso ar_tx:verify(TX)
    of
        true ->
            {ok, TX};
        false ->
            {error,
                #{
                    data_size => TX#tx.data_size,
                    fetched_size => byte_size(Data),
                    id => hb_util:human_id(TX#tx.id)
                }}
    end.

build_proofs(TX) ->
    Data = TX#tx.data,
    DataRoot = TX#tx.data_root,
    DataSize = TX#tx.data_size,
    Mode = ar_tx:chunking_mode(TX#tx.format),
    Chunks = ar_tx:chunk_binary(Mode, ?DATA_CHUNK_SIZE, Data),
    SizeTaggedChunks = ar_tx:chunks_to_size_tagged_chunks(Chunks),
    SizeTaggedChunkIDs = ar_tx:sized_chunks_to_sized_chunk_ids(SizeTaggedChunks),
    {_Root, DataTree} = ar_merkle:generate_tree(SizeTaggedChunkIDs),
    lists:filtermap(
        fun({Chunk, Offset}) ->
            case Chunk of
                <<>> ->
                    false;
                _ ->
                    DataPath = ar_merkle:generate_path(DataRoot, Offset - 1, DataTree),
                    {true,
                        #{
                            chunk => Chunk,
                            data_path => DataPath,
                            offset => Offset - 1,
                            data_size => DataSize,
                            data_root => DataRoot
                        }}
            end
        end,
        SizeTaggedChunks
    ).

fetch_block_tx_ids(Height, Opts) ->
    case fetch_block_header(Height, Opts) of
        {ok, Block} ->
            case hb_maps:get(<<"txs">>, Block, undefined, Opts) of
                TXIDs when is_list(TXIDs) ->
                    {ok, TXIDs};
                TXIDs ->
                    {error, {invalid_block_txs, Height, TXIDs}}
            end;
        Error ->
            {error, Error}
    end.

fetch_block_header(Height, Opts) ->
    hb_ao:resolve(
        <<
            "~arweave@2.9/block=",
            (hb_util:bin(Height))/binary
        >>,
        Opts
    ).

seed_proofs(_ID, [], _StartOffset, _Opts) ->
    {ok, 0};
seed_proofs(ID, Proofs, StartOffset, Opts) ->
    seed_proofs(ID, Proofs, 1, length(Proofs), StartOffset, Opts).

seed_proofs(_ID, [], _Index, _Total, _StartOffset, _Opts) ->
    {ok, 0};
seed_proofs(ID, [Proof | Rest], Index, Total, StartOffset, Opts) ->
    LocalOffset = maps:get(offset, Proof),
    AbsoluteOffset = StartOffset + LocalOffset + 1,
    maybe_print_proof_progress(ID, Index, Total, LocalOffset, AbsoluteOffset),
    case seed_one_proof(ID, Proof, AbsoluteOffset, Opts) of
        {ok, Count} ->
            case seed_proofs(ID, Rest, Index + 1, Total, StartOffset, Opts) of
                {ok, RestCount} -> {ok, Count + RestCount};
                Error -> Error
            end;
        {error, Stage, Reason} ->
            {error, Stage, Reason}
    end.

seed_one_proof(ID, Proof, AbsoluteOffset, Opts) ->
    Serialized = hb_json:encode(proof_request(Proof)),
    LocalOffset = maps:get(offset, Proof),
    case route_nodes(
        #{
            <<"method">> => <<"POST">>,
            <<"path">> => <<"/arweave/chunk">>,
            <<"route-by">> => AbsoluteOffset
        },
        Opts
    ) of
        {ok, Nodes} ->
            Results = post_to_nodes(Nodes, Serialized, Opts),
            case ensure_all_200(Results) of
                ok ->
                    {ok, length(Results)};
                {error, Failed} ->
                    {error,
                        proof_post,
                        #{
                            id => ID,
                            offset => LocalOffset,
                            absolute_offset => AbsoluteOffset,
                            failed => Failed
                        }}
            end;
        {error, Reason} ->
            {error,
                chunk_route,
                #{ id => ID, offset => LocalOffset, reason => Reason }}
    end.

proof_request(Proof) ->
    #{
        <<"chunk">> => hb_util:encode(maps:get(chunk, Proof)),
        <<"data_path">> => hb_util:encode(maps:get(data_path, Proof)),
        <<"offset">> => integer_to_binary(maps:get(offset, Proof)),
        <<"data_size">> => integer_to_binary(maps:get(data_size, Proof)),
        <<"data_root">> => hb_util:encode(maps:get(data_root, Proof))
    }.

route_nodes(RouteRequest, Opts) ->
    case dev_router:route(RouteRequest, Opts) of
        {ok, #{ <<"nodes">> := Nodes }} ->
            {ok, hb_util:message_to_ordered_list(Nodes, Opts)};
        {ok, Node = #{ <<"uri">> := _ }} ->
            {ok, [Node]};
        {ok, URI} when is_binary(URI) ->
            {ok, [#{ <<"uri">> => URI, <<"opts">> => #{} }]};
        Error ->
            Error
    end.

post_to_nodes(Nodes, Body, Opts) ->
    hb_pmap:parallel_map(
        Nodes,
        fun(Node) -> post_to_node(Node, Body, Opts) end,
        maps:get(node_post_concurrency, Opts)
    ).

post_to_node(Node, Body, Opts) ->
    post_to_node(Node, Body, Opts, 3).

post_to_node(Node, Body, Opts, AttemptsLeft) ->
    URI = maps:get(<<"uri">>, Node, maps:get(<<"prefix">>, Node, <<"unknown">>)),
    case hb_http:post(Node, #{ <<"body">> => Body }, request_opts(Opts)) of
        {ok, Response} ->
            Result = #{
                uri => URI,
                status => response_status(Response),
                response => Response
            },
            maybe_retry_post(Node, Body, Opts, AttemptsLeft, Result);
        Error ->
            Result = #{
                uri => URI,
                status => error,
                response => Error
            },
            maybe_retry_post(Node, Body, Opts, AttemptsLeft, Result)
    end.

maybe_retry_post(_Node, _Body, _Opts, _AttemptsLeft, Result = #{ status := 200 }) ->
    Result;
maybe_retry_post(Node, Body, Opts, AttemptsLeft, Result) when AttemptsLeft > 1 ->
    case retryable_post_result(Result) of
        true ->
            print_retry(
                maps:get(<<"uri">>, Node, maps:get(<<"prefix">>, Node, <<"unknown">>)),
                AttemptsLeft - 1,
                Result
            ),
            timer:sleep(250),
            post_to_node(Node, Body, Opts, AttemptsLeft - 1);
        false ->
            Result
    end;
maybe_retry_post(_Node, _Body, _Opts, _AttemptsLeft, Result) ->
    Result.

retryable_post_result(#{ status := error }) ->
    true;
retryable_post_result(#{ status := Status }) when is_integer(Status), Status >= 500 ->
    true;
retryable_post_result(_) ->
    false.

response_status(Response) ->
    hb_util:int(hb_ao:get(<<"status">>, Response, 0, #{})).

ensure_all_200(Results) ->
    Failed =
        [
            Result
        ||
            Result <- Results,
            maps:get(status, Result) =/= 200
        ],
    case Failed of
        [] -> ok;
        _ -> {error, Failed}
    end.

request_opts(Opts) ->
    Opts#{
        cache_control => [<<"no-cache">>, <<"no-store">>],
        http_only_result => false
    }.

with_input(<<"-">>, Fun) ->
    Fun(standard_io);
with_input(Path, Fun) ->
    case file:open(binary_to_list(Path), [read]) of
        {ok, Device} ->
            try Fun(Device)
            after file:close(Device)
            end;
        {error, Reason} ->
            {error, {open_input_failed, Path, Reason}}
    end.

with_log(Path, Fun) ->
    case file:open(binary_to_list(Path), [write]) of
        {ok, Device} ->
            try Fun(Device)
            after file:close(Device)
            end;
        {error, Reason} ->
            {error, {open_log_failed, Path, Reason}}
    end.

log_skip(LogDevice, ID, Stage, Reason) ->
    io:format(
        LogDevice,
        "~ts\t~ts\t~ts~n",
        [ID, atom_to_binary(Stage), format_reason(Reason)]
    ).

log_block_skip(LogDevice, Height, Reason) ->
    io:format(
        LogDevice,
        "block=~B\tblock_fetch\t~ts~n",
        [Height, format_reason(Reason)]
    ).

format_reason(Reason) ->
    unicode:characters_to_binary(io_lib:format("~0tp", [Reason])).

print_block_stage(Height, Stage) ->
    io:format("BLOCK ~B stage=~ts~n", [Height, atom_to_binary(Stage)]).

print_block_loaded(Height, TXCount) ->
    io:format("BLOCK ~B txs=~B~n", [Height, TXCount]).

print_block_tx(Height, Index, Total, ID) ->
    io:format("BLOCK ~B tx=~B/~B id=~ts~n", [Height, Index, Total, ID]).

print_block_complete(Height, TXCount, Before, After) ->
    io:format(
        "BLOCK ~B done txs=~B successes=~B skipped=~B proofs=~B "
        "chunk_posts=~B~n",
        [
            Height,
            TXCount,
            maps:get(successes, After) - maps:get(successes, Before),
            maps:get(skipped, After) - maps:get(skipped, Before),
            maps:get(proofs, After) - maps:get(proofs, Before),
            maps:get(chunk_posts, After) - maps:get(chunk_posts, Before)
        ]
    ).

print_block_skip(Height, Reason) ->
    io:format(
        standard_error,
        "BLOCK ~B stage=block_fetch reason=~ts~n",
        [Height, format_reason(Reason)]
    ).

print_bundle_stage(ID, Stage) ->
    io:format("DO ~ts stage=~ts~n", [ID, atom_to_binary(Stage)]).

print_bundle_stage(ID, RootTXID, Stage) ->
    io:format(
        "DO ~ts root_tx=~ts stage=~ts~n",
        [ID, RootTXID, atom_to_binary(Stage)]
    ).

print_bundle_proofs(ID, RootTXID, Count) ->
    io:format(
        "DO ~ts root_tx=~ts stage=proof_seed proofs=~B~n",
        [ID, RootTXID, Count]
    ).

maybe_print_proof_progress(ID, Index, Total, LocalOffset, AbsoluteOffset) ->
    case should_log_proof(Index, Total) of
        true ->
            io:format(
                "PROOF ~ts ~B/~B offset=~B absolute_offset=~B~n",
                [ID, Index, Total, LocalOffset, AbsoluteOffset]
            );
        false ->
            ok
    end.

should_log_proof(Index, Total) ->
    Total =< 10
        orelse Index =:= 1
        orelse Index =:= Total
        orelse (Index rem 25) =:= 0.

print_retry(URI, AttemptsLeft, Result) ->
    io:format(
        standard_error,
        "RETRY uri=~ts left=~B status=~ts~n",
        [URI, AttemptsLeft, retry_status(Result)]
    ).

retry_status(#{ status := error }) ->
    <<"error">>;
retry_status(#{ status := Status }) when is_integer(Status) ->
    integer_to_binary(Status).

print_bundle_success(ID, Result) ->
    io:format(
        "OK ~ts root_tx=~ts data_size=~B proofs=~B tx_posts=~B chunk_posts=~B~n",
        [
            ID,
            maps:get(root_tx_id, Result),
            maps:get(data_size, Result),
            maps:get(proofs, Result),
            maps:get(tx_posts, Result),
            maps:get(chunk_posts, Result)
        ]
    ).

print_bundle_duplicate(ID, RootTXID) ->
    io:format(
        standard_error,
        "SKIP ~ts stage=duplicate_root_tx reason=~ts~n",
        [ID, RootTXID]
    ).

print_bundle_skip(ID, Stage, Reason) ->
    io:format(
        standard_error,
        "SKIP ~ts stage=~ts reason=~ts~n",
        [ID, atom_to_binary(Stage), format_reason(Reason)]
    ).

print_summary(Stats, LogPath) ->
    SummaryArgs =
        [
            maps:get(processed, Stats),
            maps:get(successes, Stats),
            maps:get(skipped, Stats),
            maps:get(proofs, Stats),
            maps:get(tx_posts, Stats),
            maps:get(chunk_posts, Stats),
            LogPath
        ],
    case
        {
            maps:get(blocks, Stats, 0),
            maps:get(block_failures, Stats, 0)
        }
    of
        {0, 0} ->
            io:format(
                standard_error,
                "Processed ~B bundle IDs. Successes: ~B. Skipped: ~B. "
                "Proofs: ~B. TX posts: ~B. Chunk posts: ~B. Log: ~ts~n",
                SummaryArgs
            );
        {Blocks, BlockFailures} ->
            io:format(
                standard_error,
                "Blocks: ~B. Block fetch failures: ~B. Processed ~B bundle "
                "IDs. Successes: ~B. Skipped: ~B. Proofs: ~B. TX posts: ~B. "
                "Chunk posts: ~B. Log: ~ts~n",
                [Blocks, BlockFailures | SummaryArgs]
            )
    end.

print_error(help_requested) ->
    ok;
print_error(Reason) ->
    io:format(standard_error, "error: ~ts~n", [format_reason(Reason)]).

print_usage() ->
    io:format(
        standard_error,
        "usage: http-server-short-bundle-reseed [--log PATH] "
        "[--blocks HEIGHT|FROM-TO|LIST] "
        "[--node-post-concurrency N] [--target-successes N] "
        "[--turbo-url URL] [INPUT|-]~n",
        []
    ).

-ifdef(EUNIT).

parse_blocks_single_height_test() ->
    ?assertEqual({ok, [123]}, parse_blocks("123")).

parse_blocks_range_test() ->
    ?assertEqual({ok, [7, 8, 9]}, parse_blocks("7-9")).

parse_blocks_descending_range_test() ->
    ?assertEqual({ok, [9, 8, 7]}, parse_blocks("9-7")).

parse_blocks_list_and_dedupe_test() ->
    ?assertEqual({ok, [5, 8, 6, 7]}, parse_blocks("5,8,6-8,5")).

normalize_positionals_rejects_input_with_blocks_test() ->
    ?assertEqual(
        {error, blocks_with_input},
        normalize_positionals(#{
            blocks => [42],
            input => <<"-">>,
            log => default_log_path(<<"-">>),
            node_post_concurrency => ?DEFAULT_NODE_POST_CONCURRENCY,
            positionals => [<<"ids.txt">>],
            target_successes => infinity,
            turbo_url => ?DEFAULT_TURBO_URL
        })
    ).

-endif.
