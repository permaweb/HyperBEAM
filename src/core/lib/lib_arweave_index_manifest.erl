%%% @doc The manifest of L1 transaction boundaries a scan consumes.
%%%
%%% The scanner needs, per L1 transaction in its range: the absolute padded
%%% weave offset its data begins at, the data's size, and -- when a source
%%% can say -- its txid and whether it is a bundle. Boundaries and sizes
%%% derive exactly from a local chunk index: every chunk row carries its end
%%% offset relative to its transaction, so the transaction's start is the
%%% difference and its size is the largest relative offset seen. Txids do
%%% not: an L1 txid is a hash over the signed transaction header, which is
%%% not in the weave data, so it can only be joined in from block metadata.
%%% A manifest without txids is complete except that top-level items get no
%%% `bundled-in' row; the scan itself never needs the network.
%%%
%%% The file is fixed-width and sorted by start offset:
%%%
%%% ```
%%% << "AIMF", 1, 0, 0, 0,
%%%    << Start:64, Size:64, Flags:8, TXID:32/binary >>* >>
%%% '''
%%%
%%% Flags: bit 0 -- txid present; bit 1 -- known bundle; bit 2 -- known not
%%% a bundle. An absent txid is 32 zero bytes; unknown bundlehood leaves the
%%% probe to the scanner's own structural check.
-module(lib_arweave_index_manifest).
-export([write/2, load/3, from_chunk_index/3, enrich/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The file's magic and version.
-define(MAGIC, << "AIMF", 1, 0, 0, 0 >>).

%%% The key prefix a chunk index files its rows under, per storage module.
-define(INDEX_PREFIX, <<"~arweave@2.9/storage">>).

%%% How often a failed gateway request is retried, and the pause between
%%% attempts.
-define(FETCH_ATTEMPTS, 3).
-define(FETCH_PAUSE, 500).

%% @doc Write a manifest of transaction specs, sorting by start offset.
write(Path, Txs) ->
    Sorted =
        lists:sort(
            fun(A, B) ->
                maps:get(<<"start">>, A) =< maps:get(<<"start">>, B)
            end,
            Txs
        ),
    ok = filelib:ensure_path(filename:dirname(Path)),
    file:write_file(Path, [?MAGIC | [record(Tx) || Tx <- Sorted]]).

%% @doc The transactions of a manifest whose data intersects `[From, To)',
%% in ascending start order.
load(Path, From, To) ->
    maybe
        {ok, << Magic:8/binary, Records/binary >>} ?= file:read_file(Path),
        true ?= Magic == ?MAGIC orelse {error, <<"manifest-magic-invalid">>},
        {ok,
            [
                Spec
            ||
                << Start:64, Size:64, Flags:8, TXID:32/binary >> <= Records,
                Start + Size > From andalso Start < To,
                (Spec = spec(Start, Size, Flags, TXID)) /= invalid
            ]
        }
    else
        {ok, _Short} -> {error, <<"manifest-magic-invalid">>};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Derive boundary records from the chunk index a mining node keeps,
%% for one storage module's rows, and write them as a manifest. The index
%% carries no txids, so none are recorded.
%%
%% Each chunk row names its transaction's start (`AbsoluteEndOffset -
%% RelativeOffset') and raises its size high-water (`RelativeOffset'). The
%% index is one fold: its keys are flat -- the importer writes rows without
%% group markers, so a store-level walk has nothing to resolve -- and a
%% partition is tens of millions of rows, which is one sequential pass of
%% the file rather than a call per bucket. `arweave-index-store' names the
%% index's directory and capacity in `hb_store_lmdb''s own option spelling.
from_chunk_index(Path, StoreID, Opts) ->
    Store = hb_opts:get(<<"arweave-index-store">>, #{}, Opts),
    Prefix =
        <<
            (?INDEX_PREFIX)/binary,
            "/",
            (hb_util:bin(StoreID))/binary,
            "/chunks/"
        >>,
    maybe
        {ok, Env} ?=
            elmdb:env_open(
                hb_maps:get(<<"name">>, Store, <<"index">>, Opts),
                [{map_size, hb_util:int(
                    hb_maps:get(<<"capacity">>, Store, 268435456000, Opts))}]
            ),
        {ok, DB} ?= elmdb:db_open(Env, []),
        {ok, Boundaries} ?=
            elmdb:fold(
                DB,
                fun(Key, Value, Acc) -> chunk_row(Key, Value, Prefix, Acc) end,
                #{}
            ),
        ok = elmdb:env_close(Env),
        Txs =
            [
                #{ <<"start">> => Start, <<"size">> => Size }
            ||
                {Start, Size} <- lists:sort(maps:to_list(Boundaries))
            ],
        ok ?= write(Path, Txs),
        {ok, length(Txs)}
    end.

%% @doc Join txids into a manifest from a gateway's block metadata.
%%
%% The weave layout inside a block is reconstructible: its transactions,
%% sorted as `ar_block:generate_size_tagged_list_from_txs/2' sorts them, each
%% claim their padded data size in order from the block's own start, which is
%% the previous block's weave size. The gateway supplies the transaction ids
%% and data sizes per block; folding them reproduces each transaction's
%% absolute start, and a manifest record whose start and size both agree
%% takes the txid. The reconstruction is verified per block -- the fold must
%% land exactly on the block's reported weave size -- and a block that does
%% not verify (a v1 transaction's sort position, a metadata gap) contributes
%% nothing rather than a wrong id.
%%
%% One REST fetch per block and one GraphQL page per hundred transactions;
%% nothing per item. `arweave-index-gateway' names the gateway.
enrich(Path, Opts) ->
    maybe
        {ok, All} ?= load(Path, 0, 1 bsl 62),
        Needed =
            [Tx || Tx <- All, not maps:is_key(<<"id">>, Tx)],
        {ok, Enriched, Report} ?= enriched(All, Needed, Opts),
        ok ?= write(Path, Enriched),
        {ok, Report}
    end.

%%% Internal functions.

%% @doc The manifest with block metadata joined in, and the join's counters.
%% Blocks are fetched and reconstructed concurrently -- the layout of one
%% block depends only on its predecessor's weave size, which the summary
%% fetch pass supplies -- so the join is bounded by the gateway's patience,
%% not by chain length.
enriched(All, [], _Opts) ->
    {ok, All, #{ <<"enriched">> => 0, <<"blocks">> => 0 }};
enriched(All, Needed, Opts) ->
    From = lists:min([maps:get(<<"start">>, Tx) || Tx <- Needed]),
    To =
        lists:max(
            [maps:get(<<"start">>, Tx) + maps:get(<<"size">>, Tx) || Tx <- Needed]),
    maybe
        {ok, Tip} ?= tip_height(Opts),
        {ok, First} ?= covering_height(From, Tip, Opts),
        {ok, Last} ?= covering_height(To - 1, Tip, Opts),
        Summaries =
            hb_pmap:parallel_map(
                lists:seq(First - 1, Last),
                fun(Height) -> summary(Height, Opts) end,
                fetch_workers(Opts)
            ),
        Weaves =
            maps:from_list(
                [{H, W} || #{ <<"height">> := H, <<"weave-size">> := W } <- Summaries]),
        Reconstructed =
            hb_pmap:parallel_map(
                [S || S = #{ <<"height">> := H } <- Summaries, H >= First],
                fun(Summary) -> reconstructed(Summary, Weaves, Opts) end,
                fetch_workers(Opts)
            ),
        Merged =
            lists:foldl(
                fun({ok, Map}, Acc) -> maps:merge(Acc, Map);
                    (dropped, Acc) -> Acc
                end,
                #{},
                Reconstructed
            ),
        Joined = [joined(Tx, Merged) || Tx <- All],
        AlreadyNamed = length(All) - length(Needed),
        Named = length([Tx || Tx <- Joined, maps:is_key(<<"id">>, Tx)]),
        {ok, Joined,
            #{
                <<"enriched">> => Named - AlreadyNamed,
                <<"needed">> => length(Needed),
                <<"blocks">> => length(Reconstructed),
                <<"blocks-dropped">> =>
                    length([dropped || dropped <- Reconstructed])
            }
        }
    end.

%% @doc One block's summary: its height, weave size and transaction ids.
%% Fetched inside a parallel map, so a failure is thrown rather than
%% returned; `hb_pmap' surfaces it as the run's failure.
summary(Height, Opts) ->
    case fetch_json(
            << "/block/height/", (hb_util:bin(Height))/binary >>, Opts) of
        {ok, Block} ->
            #{
                <<"height">> => Height,
                <<"weave-size">> => weave_size(Block, Opts),
                <<"txs">> => hb_maps:get(<<"txs">>, Block, [], Opts)
            };
        {error, Reason} ->
            throw({'block-fetch-failed', Height, Reason})
    end.

%% @doc One block's transaction starts, or `dropped' when the block does not
%% reconstruct exactly.
reconstructed(Summary, Weaves, Opts) ->
    #{ <<"height">> := Height, <<"weave-size">> := Expected } = Summary,
    BlockStart = maps:get(Height - 1, Weaves),
    case maps:get(<<"txs">>, Summary) of
        [] when BlockStart == Expected ->
            {ok, #{}};
        Ids ->
            case block_starts(Height, BlockStart, Expected, Ids, Opts) of
                {ok, Assignments} -> {ok, Assignments};
                dropped -> dropped;
                {error, Reason} -> throw({'block-txs-failed', Height, Reason})
            end
    end.

%% @doc One manifest record with its assignment applied, when one agrees.
joined(Tx = #{ <<"id">> := _ }, _Assignments) ->
    Tx;
joined(Tx = #{ <<"start">> := Start, <<"size">> := Size }, Assignments) ->
    case maps:get(Start, Assignments, not_found) of
        {Size, ID} -> Tx#{ <<"id">> => ID };
        _ -> Tx
    end.

%% @doc The gateway's current height.
tip_height(Opts) ->
    maybe
        {ok, Info} ?= fetch_json(<<"/info">>, Opts),
        {ok, hb_util:int(hb_maps:get(<<"height">>, Info, 0, Opts))}
    end.

%% @doc The lowest height whose weave size exceeds the offset, by binary
%% search over the gateway's blocks.
covering_height(Offset, Tip, Opts) ->
    covering_height(Offset, 0, Tip, Opts).

covering_height(_Offset, Lo, Hi, _Opts) when Lo >= Hi ->
    {ok, Lo};
covering_height(Offset, Lo, Hi, Opts) ->
    Mid = (Lo + Hi) div 2,
    maybe
        {ok, Block} ?=
            fetch_json(
                << "/block/height/", (hb_util:bin(Mid))/binary >>, Opts),
        case weave_size(Block, Opts) > Offset of
            true -> covering_height(Offset, Lo, Mid, Opts);
            false -> covering_height(Offset, Mid + 1, Hi, Opts)
        end
    end.

%% @doc One block's transaction starts from its GraphQL metadata, if the
%% reconstruction lands exactly on the block's reported weave size.
block_starts(Height, BlockStart, Expected, Ids, Opts) ->
    maybe
        {ok, Txs} ?= block_txs(Ids, Opts),
        Sorted = lists:sort(fun({IDA, _}, {IDB, _}) -> IDA =< IDB end, Txs),
        {End, Assignments} =
            lists:foldl(
                fun({ID, Size}, {Pos, List}) ->
                    {
                        Pos + ar_tx:get_weave_size_increase(Size, Height),
                        case Size of
                            0 -> List;
                            _ -> [{Pos, {Size, ID}} | List]
                        end
                    }
                end,
                {BlockStart, []},
                Sorted
            ),
        case End == Expected of
            true ->
                {ok, maps:from_list(Assignments)};
            false ->
                ?event(arweave_index,
                    {block_reconstruction_mismatch,
                        {height, Height},
                        {expected, Expected},
                        {reconstructed, End}
                    }
                ),
                dropped
        end
    end.

%% @doc A block's transactions from the gateway's GraphQL API, as
%% `{NativeID, DataSize}' pairs, one query per hundred of the block's own
%% txids. Filtering by ids rather than by block matters: the block filter
%% answers with every bundled item the block carries -- hundreds of pages
%% where the block's own transaction list is one.
block_txs(Ids, Opts) ->
    block_txs(Ids, Opts, []).

block_txs([], _Opts, Acc) ->
    {ok, Acc};
block_txs(Ids, Opts, Acc) ->
    {Chunk, Rest} = lists:split(min(100, length(Ids)), Ids),
    Quoted =
        lists:join(
            <<",">>,
            [<< "\"", ID/binary, "\"" >> || ID <- Chunk]
        ),
    Query =
        iolist_to_binary(
            [
                <<"query { transactions(ids: [">>,
                Quoted,
                <<"], first: 100) "
                    "{ edges { node { id data { size } } } } }">>
            ]
        ),
    maybe
        {ok, Result} ?= fetch_graphql(Query, Opts),
        Edges =
            hb_maps:get(
                <<"edges">>,
                hb_maps:get(
                    <<"transactions">>,
                    hb_maps:get(<<"data">>, Result, #{}, Opts),
                    #{},
                    Opts
                ),
                [],
                Opts
            ),
        Txs =
            [
                {
                    hb_util:native_id(hb_maps:get(<<"id">>, Node, <<>>, Opts)),
                    hb_util:int(
                        hb_maps:get(
                            <<"size">>,
                            hb_maps:get(<<"data">>, Node, #{}, Opts),
                            0,
                            Opts
                        )
                    )
                }
            ||
                Edge <- Edges,
                (Node = hb_maps:get(<<"node">>, Edge, #{}, Opts)) /= #{}
            ],
        % A transaction the gateway has no node for reconstructs nothing;
        % the count mismatch fails the block's weave-size check rather
        % than assigning around the gap.
        block_txs(Rest, Opts, Txs ++ Acc)
    end.

%% @doc A block's cumulative weave size.
weave_size(Block, Opts) ->
    hb_util:int(hb_maps:get(<<"weave_size">>, Block, 0, Opts)).

%% @doc Fetch and decode one JSON resource from the gateway, with retries.
fetch_json(Path, Opts) ->
    fetched(
        fun() -> hb_http:get(gateway(Opts), Path, Opts) end,
        ?FETCH_ATTEMPTS
    ).

%% @doc Run one GraphQL query against the gateway, with retries.
fetch_graphql(Query, Opts) ->
    fetched(
        fun() ->
            hb_http:post(
                gateway(Opts),
                #{
                    <<"path">> => <<"/graphql">>,
                    <<"content-type">> => <<"application/json">>,
                    <<"body">> => hb_json:encode(#{ <<"query">> => Query })
                },
                Opts
            )
        end,
        ?FETCH_ATTEMPTS
    ).

%% @doc Decode a fetch's JSON body, retrying the request while attempts
%% remain.
fetched(Fun, Attempts) ->
    case Fun() of
        {ok, Res} ->
            case maps:get(<<"body">>, Res, not_found) of
                not_found -> retried(Fun, Attempts, no_body);
                Body -> {ok, hb_json:decode(Body)}
            end;
        {error, Reason} ->
            retried(Fun, Attempts, Reason)
    end.

retried(_Fun, Attempts, Reason) when Attempts =< 1 ->
    {error, Reason};
retried(Fun, Attempts, _Reason) ->
    timer:sleep(?FETCH_PAUSE),
    fetched(Fun, Attempts - 1).

%% @doc The gateway block metadata is joined in from.
gateway(Opts) ->
    hb_opts:get(<<"arweave-index-gateway">>, <<"https://arweave.net">>, Opts).

%% @doc How many gateway fetches run concurrently.
fetch_workers(Opts) ->
    hb_util:int(hb_opts:get(<<"arweave-index-fetch-workers">>, 8, Opts)).

%% @doc One transaction's fixed-width record.
record(Spec) ->
    Start = maps:get(<<"start">>, Spec),
    Size = maps:get(<<"size">>, Spec),
    {IDFlag, TXID} =
        case maps:get(<<"id">>, Spec, undefined) of
            undefined -> {0, << 0:256 >>};
            ID -> {1, hb_util:native_id(ID)}
        end,
    BundleFlag =
        case maps:get(<<"bundle">>, Spec, undefined) of
            true -> 2;
            false -> 4;
            undefined -> 0
        end,
    << Start:64, Size:64, (IDFlag bor BundleFlag):8, TXID/binary >>.

%% @doc One record's spec map, or `invalid' for flag bits this version does
%% not write.
spec(Start, Size, Flags, TXID) when Flags =< 7 ->
    Base = #{ <<"start">> => Start, <<"size">> => Size },
    WithID =
        case Flags band 1 of
            0 -> Base;
            1 -> Base#{ <<"id">> => TXID }
        end,
    case Flags band 6 of
        0 -> WithID;
        2 -> WithID#{ <<"bundle">> => true };
        4 -> WithID#{ <<"bundle">> => false };
        _ -> invalid
    end;
spec(_Start, _Size, _Flags, _TXID) ->
    invalid.

%% @doc Raise one chunk row's transaction to the boundary map. Keys outside
%% the module's `chunks' prefix, and values this decoder does not know, pass
%% through untouched: the fold crosses every row the index holds.
chunk_row(Key, Value, Prefix, Acc) ->
    PrefixSize = byte_size(Prefix),
    case Key of
        << Prefix:PrefixSize/binary, _Bucket:20/binary, "/", End:20/binary >> ->
            case Value of
                << 1:8, _ChunkSize:32, RelativeOffset:64, _/binary >> ->
                    Start = hb_util:int(End) - RelativeOffset,
                    maps:update_with(
                        Start,
                        fun(Size) -> max(Size, RelativeOffset) end,
                        RelativeOffset,
                        Acc
                    );
                _Unknown ->
                    Acc
            end;
        _Other ->
            Acc
    end.

%%% Tests.

%% @doc Specs round-trip through the file, sorted and bounded by the load
%% range, with flags surviving.
round_trip_test() ->
    Path =
        filename:join(
            os:getenv("TMPDIR", "/tmp"),
            <<
                "hb-index-manifest-",
                (hb_util:encode(crypto:strong_rand_bytes(6)))/binary
            >>
        ),
    ID = crypto:hash(sha256, <<"an l1 txid">>),
    Txs =
        [
            #{ <<"start">> => 2097152, <<"size">> => 1000, <<"bundle">> => false },
            #{ <<"start">> => 0, <<"size">> => 262144, <<"id">> => ID,
                <<"bundle">> => true },
            #{ <<"start">> => 524288, <<"size">> => 300000 }
        ],
    ok = write(Path, Txs),
    ?assertEqual(
        {ok,
            [
                #{ <<"start">> => 0, <<"size">> => 262144, <<"id">> => ID,
                    <<"bundle">> => true },
                #{ <<"start">> => 524288, <<"size">> => 300000 }
            ]
        },
        load(Path, 0, 1000000)
    ),
    ?assertEqual(
        {ok, [#{ <<"start">> => 2097152, <<"size">> => 1000,
            <<"bundle">> => false }]},
        load(Path, 2097153, 2097154)
    ),
    ?assertEqual({ok, []}, load(Path, 3000000, 4000000)),
    ok = file:write_file(Path, <<"not a manifest">>),
    ?assertEqual({error, <<"manifest-magic-invalid">>}, load(Path, 0, 1)).
