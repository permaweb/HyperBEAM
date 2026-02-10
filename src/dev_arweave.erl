%%% @doc A device that provides access to Arweave network information, relayed
%%% from a designated node.
%%%
%%% The node(s) that are used to query data may be configured by altering the
%%% `/arweave` route in the node's configuration message.
-module(dev_arweave).
-export([info/1, info/3, default/4]).
-export([tx/3, chunk/3, block/3, current/3, status/3, price/3, tx_anchor/3]).
-export([
    graphql/3,
    vdf/3,
    spora/3,
    ledger/3,
    gossip/3,
    network_block/3,
    validate_network_block/3,
    validate_network_chain/3
]).
-export([post_tx/3, post_tx/4, post_binary_ans104/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

info(_Opts) ->
    #{
        default => fun default/4
    }.

info(_Base, _Req, _Opts) ->
    {ok,
        #{
            <<"name">> => <<"arweave@2.9-pre">>,
            <<"description">> => <<"Arweave API and AO-native Arweave subdevices">>,
            <<"exports">> =>
                [
                    <<"status">>,
                    <<"tx">>,
                    <<"chunk">>,
                    <<"block">>,
                    <<"current">>,
                    <<"price">>,
                    <<"tx-anchor">>,
                    <<"graphql">>,
                    <<"spora">>,
                    <<"vdf">>,
                    <<"ledger">>,
                    <<"gossip">>,
                    <<"network-block">>,
                    <<"validate-network-block">>,
                    <<"validate-network-chain">>
                ]
        }
    }.

default(<<"set">>, Base, Req, Opts) ->
    dev_message:set(Base, Req, Opts);
default(<<"keys">>, Base, _Req, _Opts) ->
    dev_message:keys(Base);
default(<<"status">>, Base, Req, Opts) ->
    status(Base, Req, Opts);
default(<<"tx">>, Base, Req, Opts) ->
    tx(Base, Req, Opts);
default(<<"chunk">>, Base, Req, Opts) ->
    chunk(Base, Req, Opts);
default(<<"block">>, Base, Req, Opts) ->
    block(Base, Req, Opts);
default(<<"current">>, Base, Req, Opts) ->
    current(Base, Req, Opts);
default(<<"price">>, Base, Req, Opts) ->
    price(Base, Req, Opts);
default(<<"tx-anchor">>, Base, Req, Opts) ->
    tx_anchor(Base, Req, Opts);
default(<<"graphql">>, Base, Req, Opts) ->
    graphql(Base, Req, Opts);
default(<<"spora">>, Base, Req, Opts) ->
    spora(Base, Req, Opts);
default(<<"vdf">>, Base, Req, Opts) ->
    vdf(Base, Req, Opts);
default(<<"vdf2">>, Base, Req, Opts) ->
    vdf(Base, Req, Opts);
default(<<"vdf3">>, Base, Req, Opts) ->
    vdf(Base, Req, Opts);
default(<<"vdf4">>, Base, Req, Opts) ->
    vdf(Base, Req, Opts);
default(<<"nonce-limiter">>, Base, Req, Opts) ->
    spora(Base, Req, Opts);
default(<<"nonce_limiter">>, Base, Req, Opts) ->
    spora(Base, Req, Opts);
default(<<"ledger">>, Base, Req, Opts) ->
    ledger(Base, Req, Opts);
default(<<"gossip">>, Base, Req, Opts) ->
    gossip(Base, Req, Opts);
default(<<"network-block">>, Base, Req, Opts) ->
    network_block(Base, Req, Opts);
default(<<"validate-network-block">>, Base, Req, Opts) ->
    validate_network_block(Base, Req, Opts);
default(<<"validate-network-chain">>, Base, Req, Opts) ->
    validate_network_chain(Base, Req, Opts);
default(<<"peers">>, Base, Req, Opts) ->
    gossip(Base, Req#{<<"action">> => <<"peers">>}, Opts);
default(<<"peer">>, Base, Req, Opts) ->
    gossip(Base, Req#{<<"action">> => <<"peer">>}, Opts);
default(<<"block_announcement">>, Base, Req, Opts) ->
    gossip(Base, Req#{<<"action">> => <<"block">>}, Opts);
default(Key, _Base, Req, Opts) ->
    Method = hb_maps:get(<<"method">>, Req, <<"GET">>, Opts),
    KeyBin = hb_util:bin(Key),
    Path = <<"/", KeyBin/binary, (path_suffix(Req, Opts))/binary>>,
    request(Method, Path, Req, Opts).

%% @doc Proxy the `/info' endpoint from the Arweave node.
status(_Base, _Request, Opts) ->
    request(<<"GET">>, <<"/info">>, Opts).

%% @doc Returns the given transaction, if known to the client node(s), as an
%% AO-Core message.
tx(Base, Request, Opts) ->
    case hb_maps:get(<<"method">>, Request, <<"GET">>, Opts) of
        <<"POST">> -> post_tx(Base, Request, Opts);
        <<"GET">> ->
            case subkey(Request, <<>>, Opts) of
                <<"pending">> ->
                    gossip(Base, Request#{<<"action">> => <<"pending">>}, Opts);
                <<"ready_for_mining">> ->
                    gossip(Base, Request#{<<"action">> => <<"ready_for_mining">>}, Opts);
                <<>> ->
                    get_tx(Base, Request, Opts);
                _ ->
                    get_tx(Base, Request, Opts)
            end
    end.

%% @doc Upload either an ans104 or an L1 transaction to Arweave.
%% Ensures that uploaded transactions are stored in the local cache after a
%% successful response has been received.
%% 
%% Note: When uploading ans104 transactions, this function will use the
%% node's default bundler. If instead you want to use this node as a bundler
%% you should use the ~bundler@1.0 device.
post_tx(Base, Request, Opts) ->
    case hb_message:commitment_devices(Request, Opts) of
        [Device] -> post_tx(Base, Request, Opts, Device);
        [] -> 
            ?event(warning,
                {no_commitment_devices,
                    {request, Request},
                    {base, Base}
                }
            ),
            {error, <<"No commitment found on `POST tx` request.">>};
        Devices ->
            ?event({too_many_commitment_devices, Devices}),
            {error, too_many_commitment_devices}
    end.

post_tx(Base, Request, Opts, <<"tx@1.0">>) ->
    ?event({{request, Request}, {base, Base}}),
    TX = hb_message:convert(Request, <<"tx@1.0">>, Opts),
    ?event({tx, TX}),
    JSON = ar_tx:tx_to_json_struct(TX#tx{ data = <<>> }),
    Serialized = hb_json:encode(JSON),
    ?event({serialized_tx, {explicit, Serialized}}),
    TXResponse = hb_http:post(
        hb_opts:get(gateway, not_found, Opts),
        #{
            <<"path">> => <<"/tx">>,
            <<"body">> => Serialized
        },
        Opts
    ),
    case TXResponse of
        {ok, _} ->
            ?event({uploaded_arweave_tx, {request, Request}, {result, TXResponse}}),
            CacheRes = hb_cache:write(Request, Opts),
            ?event(
                {cache_uploaded_message,
                    {msg, Request},
                    {status,
                        case CacheRes of {ok, _} -> ok;
                        _ -> failed
                        end
                    }
                }
            ),
            TXResponse;
        Else -> Else
    end;
post_tx(Base, Request, Opts, <<"ans104@1.0">>) ->
    ?event({{request, Request}, {base, Base}, {opts, Opts}}),
    TX = hb_message:convert(Request, <<"ans104@1.0">>, Opts),
    ?event({tx, TX}),
    Serialized = ar_bundles:serialize(TX),
    ?event({serialized_tx, Serialized}),
    post_binary_ans104(Serialized, Opts).

post_binary_ans104(SerializedTX, Opts) ->
    hb_http:post(
        hb_opts:get(bundler_ans104, not_found, Opts),
        #{
            <<"path">> => <<"/tx">>,
            <<"content-type">> => <<"application/octet-stream">>,
            <<"body">> => SerializedTX
        },
        Opts#{
            http_client =>
                hb_opts:get(bundler_ans104_http_client, httpc, Opts)
        }
    ).

%% @doc Get a transaction ID from the Arweave node, as indicated by the `tx` key
%% in the request or base message. If the `data' key is present and set to
%% `false', the data is not retrieved and added to the response. If the `data'
%% key is set to `always', transactions for which the header is available but
%% the data is not will lead to an error. Otherwise, just the header will be
%% returned.
get_tx(Base, Request, Opts) ->
    case find_txid(Base, Request, Opts) of
        not_found -> {error, not_found};
        TXID -> request(<<"GET">>, <<"/tx/", TXID/binary>>, Opts)
    end.

chunk(Base, Request, Opts) ->
    case hb_maps:get(<<"method">>, Request, <<"GET">>, Opts) of
        <<"POST">> -> post_chunk(Base, Request, Opts);
        <<"GET">> -> get_chunk(Base, Request, Opts)
    end.

get_chunk(Base, Request, Opts) ->
    ChunkRef0 = read_any([<<"offset">>, <<"chunk">>], Base, Request, not_found, Opts),
    ChunkRef =
        case ChunkRef0 of
            not_found ->
                case subkey(Request, <<>>, Opts) of
                    <<>> -> not_found;
                    <<"chunk">> -> not_found;
                    <<"offset">> -> not_found;
                    Value -> Value
                end;
            Value ->
                Value
        end,
    case ChunkRef of
        not_found ->
            request(<<"GET">>, <<"/chunk">>, Request, Opts);
        _ ->
            request(
                <<"GET">>,
                <<"/chunk/", (hb_util:bin(ChunkRef))/binary>>,
                Request,
                Opts
            )
    end.

post_chunk(_Base, Request, Opts) ->
    Serialized = hb_json:encode(Request),
    ?event({uploading_chunk, {explicit, Serialized}}),
    hb_http:post(
        hb_opts:get(gateway, not_found, Opts),
        #{
            <<"path">> => <<"/chunk">>,
            <<"body">> => Serialized
        },
        Opts
    ).

add_data(TXID, TXHeader, Opts) ->
    case data(TXID, Opts) of
        {ok, Data} ->
            TX = TXHeader#tx{ data = Data },
            ?event(
                {retrieved_tx_with_data,
                    {id, TXID},
                    {data_size, byte_size(Data)},
                    {tx, TX}
                }
            ),
            {ok, TX};
        {error, Reason} ->
            ?event(
                {data_retrieval_failed_after_header,
                    {id, TXID},
                    {error, Reason}
                }
            ),
            {error, Reason}
    end.

%% @doc Retrieve the data of a transaction from Arweave.
data(TXID, Opts) ->
    ?event({retrieving_tx_data, {tx, TXID}}),
    request(<<"GET">>, <<"/raw/", TXID/binary>>, Opts).

%% @doc Retrieve (and cache) block information from Arweave. If the `block' key
%% is present, it is used to look up the associated block. If it is of Arweave
%% block hash length (43 characters), it is used as an ID. If it is parsable as
%% an integer, it is used as a block height. If it is not present, the current
%% block is used.
block(Base, Request, Opts) ->
    Block = read_any([<<"block">>], Base, Request, not_found, Opts),
    case Block of
        <<"current">> -> current(Base, Request, Opts);
        not_found -> current(Base, Request, Opts);
        ID when ?IS_ID(ID) -> block({id, ID}, Opts);
        MaybeHeight ->
            try hb_util:int(MaybeHeight) of
              Int -> block({height, Int}, Opts)
            catch
                _:_ ->
                    {
                        error,
                        <<"Invalid block reference `", MaybeHeight/binary, "`">>
                    }
            end
    end.
block({id, ID}, Opts) ->
    case hb_cache:read(ID, Opts) of
        {ok, Block} ->
            ?event({retrieved_block_from_cache, {id, ID}}),
            {ok, Block};
        not_found ->
            request(<<"GET">>, <<"/block/hash/", ID/binary>>, Opts)
    end;
block({height, Height}, Opts) ->
    case dev_arweave_block_cache:read(Height, Opts) of
        {ok, Block} ->
            ?event({retrieved_block_from_cache, {height, Height}}),
            {ok, Block};
        not_found ->
            request(
                <<"GET">>,
                <<"/block/height/", (hb_util:bin(Height))/binary>>,
                Opts
            )
    end.

%% @doc Retrieve the current block information from Arweave.
current(_Base, _Request, Opts) ->
    request(<<"GET">>, <<"/block/current">>, Opts).

price(Base, Request, Opts) ->
    Size = read_any([<<"size">>], Base, Request, not_found, Opts),
    case Size of
        not_found ->
            {error, not_found};
        _ ->
            request(<<"GET">>, <<"/price/", (hb_util:bin(Size))/binary>>, Opts)
    end.

tx_anchor(_Base, _Request, Opts) ->
    request(<<"GET">>, <<"/tx_anchor">>, Opts).

graphql(Base, Request, Opts) ->
    case hb_maps:get(<<"method">>, Request, <<"POST">>, Opts) of
        <<"POST">> ->
            Query = hb_maps:get(<<"query">>, Request, not_found, Opts),
            Body = hb_maps:get(<<"body">>, Request, not_found, Opts),
            case {Query, Body} of
                {not_found, not_found} ->
                    request(<<"POST">>, <<"/graphql">>, Request, Opts);
                _ ->
                    dev_query_graphql:handle(Base, Request, Opts)
            end;
        _ ->
            request(<<"GET">>, <<"/graphql">>, Request, Opts)
    end.

vdf(Base, Request, Opts) ->
    dev_arweave_vdf:default(subkey(Request, <<"compute">>, Opts), Base, Request, Opts).

spora(Base, Request, Opts) ->
    dev_arweave_spora:default(subkey(Request, <<"compute">>, Opts), Base, Request, Opts).

ledger(Base, Request, Opts) ->
    dev_arweave_ledger:default(subkey(Request, <<"validate-block">>, Opts), Base, Request, Opts).

gossip(Base, Request, Opts) ->
    dev_arweave_gossip:default(subkey(Request, <<"tx">>, Opts), Base, Request, Opts).

network_block(Base, Request, Opts) ->
    BlockRef =
        read_any(
            [<<"block">>, <<"height">>, <<"hash">>, <<"id">>],
            Base,
            Request,
            <<"current">>,
            Opts
        ),
    case BlockRef of
        BlockMap when is_map(BlockMap) ->
            {ok, BlockMap};
        <<"latest">> ->
            current(Base, Request, Opts);
        <<"current">> ->
            current(Base, Request, Opts);
        ID when ?IS_ID(ID) ->
            block({id, ID}, Opts);
        MaybeHeight ->
            case hb_util:safe_int(MaybeHeight) of
                {ok, Height} when Height >= 0 ->
                    block({height, Height}, Opts);
                _ ->
                    {error, invalid_block_reference}
            end
    end.

validate_network_block(Base, Request, Opts) ->
    case network_block(Base, Request, Opts) of
        {error, Reason} ->
            {error, Reason};
        {ok, Block} ->
            MaybeProvidedTXs =
                read_req_any([<<"txs-full">>, <<"txs_full">>], Request, not_found, Opts),
            TXSource =
                case maybe_to_tx_records(MaybeProvidedTXs, Opts) of
                    {ok, [_ | _] = TXRecordsProvided} ->
                        {ok, TXRecordsProvided};
                    _ ->
                        TXIDs = normalize_tx_ids(hb_maps:get(<<"txs">>, Block, [], Opts)),
                        fetch_tx_headers(TXIDs, Opts)
                end,
            case TXSource of
                {error, Reason2} ->
                    {error, Reason2};
                {ok, TXRecords} ->
                    StateForLedger =
                        case read_req_any([<<"state">>], Request, not_found, Opts) of
                            ProvidedState when is_map(ProvidedState) ->
                                ProvidedState;
                            _ ->
                                synthetic_state_for_block(Block, TXRecords, Opts)
                        end,
                    BlockForLedger =
                        Block#{
                            <<"txs">> => TXRecords
                        },
                    LedgerReq =
                        #{
                            <<"path">> => <<"validate-block">>,
                            <<"state">> => StateForLedger,
                            <<"block">> => BlockForLedger
                        },
                    case hb_ao:resolve(
                        #{<<"device">> => dev_arweave_ledger},
                        LedgerReq,
                        Opts
                    ) of
                        {ok, Validation} ->
                            {ok,
                                #{
                                    <<"valid">> => hb_maps:get(<<"valid">>, Validation, false, Opts),
                                    <<"validation">> => Validation,
                                    <<"tx-count">> => length(TXRecords),
                                    <<"block-hash-valid">> =>
                                        hb_maps:get(<<"block-hash-valid">>, Validation, false, Opts),
                                    <<"block-height">> => hb_maps:get(<<"height">>, Block, 0, Opts),
                                    <<"block-hash">> =>
                                        hb_maps:get(<<"hash">>, Block, <<>>, Opts)
                                }
                            };
                        Err ->
                            Err
                    end
            end
    end.

validate_network_chain(Base, Request, Opts) ->
    CountRaw = read_req_any([<<"count">>, <<"blocks">>], Request, 50, Opts),
    Count =
        case hb_util:safe_int(CountRaw) of
            {ok, C} when C > 0 -> min(C, 500);
            _ -> 50
        end,
    StartRef = read_req_any([<<"start-block">>, <<"start_block">>], Request, not_found, Opts),
    BlockReq =
        case StartRef of
            not_found -> Request;
            _ -> Request#{<<"block">> => StartRef}
        end,
    case network_block(Base, BlockReq, Opts) of
        {error, Reason} ->
            {error, Reason};
        {ok, StartBlock} ->
            case hb_util:safe_int(hb_maps:get(<<"height">>, StartBlock, -1, Opts)) of
                {ok, StartHeight} when StartHeight > 0 ->
                    EndHeight = max(1, StartHeight - Count + 1),
                    case block({height, EndHeight - 1}, Opts) of
                        {error, Reason2} ->
                            {error, Reason2};
                        {ok, PrevBlock} ->
                            PrevWalletRoot = hb_maps:get(<<"wallet_list">>, PrevBlock, <<>>, Opts),
                            PrevIndepHash = hb_maps:get(<<"indep_hash">>, PrevBlock, <<>>, Opts),
                            Heights = lists:seq(EndHeight, StartHeight),
                            InitialState =
                                #{
                                    <<"height">> => EndHeight - 1,
                                    <<"balances">> => #{},
                                    <<"pending-reward">> => 0,
                                    <<"tx-history">> => [],
                                    <<"last-block-hash">> => PrevIndepHash
                                },
                            case validate_chain_heights(
                                Heights,
                                InitialState,
                                PrevWalletRoot,
                                PrevIndepHash,
                                Opts,
                                []
                            ) of
                                {ok, FinalState, Results} ->
                                    {ok,
                                        #{
                                            <<"valid">> => true,
                                            <<"count">> => length(Results),
                                            <<"from-height">> => EndHeight,
                                            <<"to-height">> => StartHeight,
                                            <<"results">> => Results,
                                            <<"final-state-height">> =>
                                                hb_maps:get(<<"height">>, FinalState, 0, Opts)
                                        }
                                    };
                                {error, ErrorInfo} ->
                                    {ok,
                                        #{
                                            <<"valid">> => false,
                                            <<"error">> => error_to_bin(ErrorInfo, Opts),
                                            <<"from-height">> => EndHeight,
                                            <<"to-height">> => StartHeight
                                        }
                                    }
                            end
                    end;
                _ ->
                    {error, invalid_start_block}
            end
    end.

%%% Internal Functions

%% @doc Find the transaction ID to retrieve from Arweave based on the request or
%% base message.
find_txid(Base, Request, Opts) ->
    read_any([<<"tx">>], Base, Request, not_found, Opts).

subkey(Request, Default, Opts) ->
    Raw = read_req_any([<<"action">>, <<"path">>], Request, Default, Opts),
    First = hb_util:bin(Raw),
    Trimmed = trim_leading_slash(First),
    case binary:split(Trimmed, <<"/">>, [global]) of
        [<<>>] -> Default;
        [Head | _] -> Head
    end.

trim_leading_slash(<<"/", Rest/binary>>) ->
    trim_leading_slash(Rest);
trim_leading_slash(Bin) ->
    Bin.

path_suffix(Request, Opts) ->
    case hb_maps:get(<<"path">>, Request, <<>>, Opts) of
        <<>> ->
            <<>>;
        Path ->
            PathBin = hb_util:bin(Path),
            case binary:at(PathBin, 0) of
                $/ -> PathBin;
                _ -> <<"/", PathBin/binary>>
            end
    end.

maybe_copy_key(Key, From, To, Opts) ->
    case hb_maps:find(Key, From, Opts) of
        {ok, Value} -> hb_maps:put(Key, Value, To, Opts);
        error -> To
    end.

read_req_any(Keys, Req, Default, Opts) ->
    read_req_any(Keys, Req, Default, Opts, undefined).

read_any(Keys, Base, Req, Default, Opts) ->
    read_req_any(Keys, Req, Default, Opts, Base).

read_req_any([], _Req, Default, _Opts, _Base) ->
    Default;
read_req_any([Key | Rest], Req, Default, Opts, Base) ->
    case hb_maps:find(Key, Req, Opts) of
        {ok, Value} ->
            Value;
        error ->
            case is_map(Base) of
                true ->
                    case hb_maps:find(Key, Base, Opts) of
                        {ok, Value2} -> Value2;
                        error -> read_req_any(Rest, Req, Default, Opts, Base)
                    end;
                false ->
                    read_req_any(Rest, Req, Default, Opts, Base)
            end
    end.

normalize_tx_ids(TXIDs) when is_list(TXIDs) ->
    [hb_util:bin(TXID) || TXID <- TXIDs, is_binary(TXID)];
normalize_tx_ids(_) ->
    [].

fetch_tx_headers(TXIDs, Opts) ->
    lists:foldl(
        fun(TXID, {ok, Acc}) ->
                case fetch_tx_header(TXID, Opts) of
                    {ok, TX} ->
                        {ok, Acc ++ [TX]};
                    {error, Reason} ->
                        {error, {tx_header_failed, TXID, Reason}};
                    Other ->
                        {error, {tx_header_failed, TXID, Other}}
                end;
           (_, Err = {error, _}) ->
                Err
        end,
        {ok, []},
        TXIDs
    ).

fetch_tx_header(TXID, Opts) ->
    Req = #{
        <<"path">> => <<"/arweave/tx/", TXID/binary>>,
        <<"method">> => <<"GET">>
    },
    case http_request_with_retry(Req, Opts) of
        {ok, #{<<"body">> := Body}} ->
            try
                TXJSON0 = hb_json:decode(Body),
                TXJSON = maybe_enrich_tx_json_owner(TXID, TXJSON0, Opts),
                {ok, ar_tx:json_struct_to_tx(TXJSON)}
            catch
                _Class:Reason:Stacktrace ->
                    {error, {invalid_tx_header, TXID, Reason, Stacktrace}}
            end;
        {error, _} ->
            {error, tx_header_not_found};
        _ ->
            {error, tx_header_not_found}
    end.

maybe_enrich_tx_json_owner(TXID, TXJSON, Opts) when is_map(TXJSON) ->
    OwnerEnc = hb_maps:get(<<"owner">>, TXJSON, <<>>, Opts),
    SignatureEnc = hb_maps:get(<<"signature">>, TXJSON, <<>>, Opts),
    case {OwnerEnc, signature_len_from_encoded(SignatureEnc)} of
        {<<>>, 65} ->
            case fetch_tx_owner_key_graphql(TXID, Opts) of
                {ok, OwnerKeyEnc} when is_binary(OwnerKeyEnc), byte_size(OwnerKeyEnc) > 0 ->
                    TXJSON#{<<"owner">> => OwnerKeyEnc};
                _ ->
                    TXJSON
            end;
        _ ->
            TXJSON
    end;
maybe_enrich_tx_json_owner(_TXID, TXJSON, _Opts) ->
    TXJSON.

signature_len_from_encoded(SigEnc) when is_binary(SigEnc) ->
    try
        byte_size(hb_util:decode(SigEnc))
    catch
        _:_ -> 0
    end;
signature_len_from_encoded(_) ->
    0.

fetch_tx_owner_key_graphql(TXID, Opts) ->
    Query = <<"query($id:ID!){transaction(id:$id){owner{key}}}">>,
    Body =
        hb_json:encode(
            #{
                <<"query">> => Query,
                <<"variables">> => #{<<"id">> => TXID}
            }
        ),
    case httpc_post_graphql_with_retry(Body, 5) of
        {ok, RespBody} ->
            try
                Resp = hb_json:decode(RespBody),
                case hb_util:deep_get(<<"data/transaction/owner/key">>, Resp, not_found, Opts) of
                    OwnerKey when is_binary(OwnerKey), byte_size(OwnerKey) > 0 ->
                        {ok, OwnerKey};
                    _ ->
                        {error, owner_key_not_found}
                end
            catch
                _:_ -> {error, invalid_graphql_owner_response}
            end;
        {error, Reason} ->
            {error, Reason};
        Other ->
            {error, Other}
    end.

httpc_post_graphql_with_retry(_Body, 0) ->
    {error, graphql_lookup_failed};
httpc_post_graphql_with_retry(Body, AttemptsLeft) ->
    Request =
        {
            "https://arweave.net/graphql",
            [{"content-type", "application/json"}],
            "application/json",
            Body
        },
    case httpc:request(post, Request, [{timeout, 10000}], [{body_format, binary}]) of
        {ok, {{_Version, Status, _ReasonPhrase}, _Headers, RespBody}}
                when Status >= 200, Status < 300 ->
            {ok, RespBody};
        {ok, {{_Version, Status, _ReasonPhrase}, _Headers, _RespBody}}
                when Status >= 500, AttemptsLeft > 1 ->
            timer:sleep((6 - AttemptsLeft) * 100),
            httpc_post_graphql_with_retry(Body, AttemptsLeft - 1);
        {ok, {{_Version, Status, _ReasonPhrase}, _Headers, RespBody}} ->
            {error, {graphql_http_status, Status, RespBody}};
        {error, _Reason} when AttemptsLeft > 1 ->
            timer:sleep((6 - AttemptsLeft) * 100),
            httpc_post_graphql_with_retry(Body, AttemptsLeft - 1);
        {error, Reason} ->
            {error, Reason}
    end.

maybe_to_tx_records(not_found, _Opts) ->
    {error, no_tx_records};
maybe_to_tx_records(TXs, Opts) when is_list(TXs) ->
    lists:foldl(
        fun(TX, {ok, Acc}) ->
                case maybe_to_tx_record(TX, Opts) of
                    {ok, TXRecord} -> {ok, Acc ++ [TXRecord]};
                    Err -> Err
                end;
           (_, Err = {error, _}) ->
                Err
        end,
        {ok, []},
        TXs
    );
maybe_to_tx_records(_, _Opts) ->
    {error, no_tx_records}.

maybe_to_tx_record(TX, _Opts) when is_record(TX, tx) ->
    {ok, TX};
maybe_to_tx_record(TX, Opts) when is_map(TX) ->
    try
        {ok, hb_message:convert(TX, <<"tx@1.0">>, Opts)}
    catch
        _:_ -> {error, invalid_tx_record}
    end;
maybe_to_tx_record(_, _Opts) ->
    {error, invalid_tx_record}.

synthetic_state_for_block(Block, TXRecords, Opts) ->
    Height =
        case hb_util:safe_int(hb_maps:get(<<"height">>, Block, 0, Opts)) of
            {ok, H} when H > 0 -> H - 1;
            _ -> 0
        end,
    PrevHash =
        case hb_maps:find(<<"previous-block">>, Block, Opts) of
            {ok, Value} ->
                Value;
            error ->
                hb_maps:get(<<"previous_block">>, Block, <<>>, Opts)
        end,
    Balances =
        lists:foldl(
            fun(TX, Acc) ->
                SenderRaw = ar_wallet:to_address(TX#tx.owner, TX#tx.signature_type),
                Sender = hb_util:encode(SenderRaw),
                Spend = TX#tx.quantity + TX#tx.reward,
                Existing = hb_maps:get(Sender, Acc, 0, Opts),
                hb_maps:put(Sender, Existing + Spend, Acc, Opts)
            end,
            #{},
            TXRecords
        ),
    #{
        <<"height">> => Height,
        <<"balances">> => Balances,
        <<"pending-reward">> => 0,
        <<"tx-history">> => [],
        <<"last-block-hash">> => PrevHash
    }.

validate_chain_heights([], State, _PrevWalletRoot, _PrevIndepHash, _Opts, Acc) ->
    {ok, State, lists:reverse(Acc)};
validate_chain_heights([Height | Rest], State, PrevWalletRoot, PrevIndepHash, Opts, Acc) ->
    case block({height, Height}, Opts) of
        {error, Reason} ->
            {error, {fetch_block_failed, Height, Reason}};
        {ok, Block} ->
            case validate_block_link(Block, Height, PrevIndepHash, Opts) of
                {error, Reason2} ->
                    {error, {chain_link_invalid, Height, Reason2}};
                ok ->
                    TXIDs = normalize_tx_ids(hb_maps:get(<<"txs">>, Block, [], Opts)),
                    case fetch_tx_headers(TXIDs, Opts) of
                        {error, Reason3} ->
                            {error, {fetch_txs_failed, Height, Reason3}};
                        {ok, TXRecords} ->
                            case ensure_sender_balances(State, TXRecords, PrevWalletRoot, Opts) of
                                {error, Reason4} ->
                                    {error, {load_sender_balances_failed, Height, Reason4}};
                                {ok, StateWithSenders} ->
                                    StateForBlock =
                                        StateWithSenders#{
                                            <<"height">> => Height - 1,
                                            <<"last-block-hash">> => PrevIndepHash
                                        },
                                    ValidateReq =
                                        #{
                                            <<"path">> => <<"validate-network-block">>,
                                            <<"block">> => Block,
                                            <<"txs-full">> => TXRecords,
                                            <<"state">> => StateForBlock
                                        },
                                    case hb_ao:resolve(
                                        #{<<"device">> => dev_arweave},
                                        ValidateReq,
                                        Opts
                                    ) of
                                        {ok, Validation} ->
                                            case hb_maps:get(<<"valid">>, Validation, false, Opts) of
                                                false ->
                                                    {error,
                                                        {
                                                            validate_block_failed,
                                                            Height,
                                                            hb_maps:get(
                                                                <<"validation">>,
                                                                Validation,
                                                                Validation,
                                                                Opts
                                                            )
                                                        }
                                                    };
                                                true ->
                                                    CurrentWalletRoot =
                                                        hb_maps:get(
                                                            <<"wallet_list">>,
                                                            Block,
                                                            PrevWalletRoot,
                                                            Opts
                                                        ),
                                                    NextPrevIndepHash =
                                                        hb_maps:get(
                                                            <<"indep_hash">>,
                                                            Block,
                                                            PrevIndepHash,
                                                            Opts
                                                        ),
                                                    LedgerValidation =
                                                        hb_maps:get(
                                                            <<"validation">>,
                                                            Validation,
                                                            #{},
                                                            Opts
                                                        ),
                                                    StateAfterMaybe =
                                                        hb_maps:get(
                                                            <<"state-after">>,
                                                            LedgerValidation,
                                                            StateForBlock,
                                                            Opts
                                                        ),
                                                    StateAfter =
                                                        case is_map(StateAfterMaybe) of
                                                            true -> StateAfterMaybe;
                                                            false -> StateForBlock
                                                        end,
                                                    NextState =
                                                        StateAfter#{
                                                            <<"height">> => Height,
                                                            <<"last-block-hash">> =>
                                                                NextPrevIndepHash
                                                        },
                                                    Result =
                                                        #{
                                                            <<"height">> => Height,
                                                            <<"tx-count">> => length(TXRecords),
                                                            <<"block-hash-valid">> =>
                                                                hb_maps:get(
                                                                    <<"block-hash-valid">>,
                                                                    Validation,
                                                                    false,
                                                                    Opts
                                                                ),
                                                            <<"valid">> => true
                                                        },
                                                    validate_chain_heights(
                                                        Rest,
                                                        NextState,
                                                        CurrentWalletRoot,
                                                        NextPrevIndepHash,
                                                        Opts,
                                                        [Result | Acc]
                                                    )
                                            end;
                                        Other ->
                                            {error, {resolve_validate_failed, Height, Other}}
                                    end
                            end
                    end
            end
    end.

validate_block_link(Block, ExpectedHeight, PrevIndepHash, Opts) ->
    HeightRaw = hb_maps:get(<<"height">>, Block, -1, Opts),
    PrevRaw =
        case hb_maps:find(<<"previous_block">>, Block, Opts) of
            {ok, V} -> V;
            error -> hb_maps:get(<<"previous-block">>, Block, <<>>, Opts)
        end,
    case hb_util:safe_int(HeightRaw) of
        {ok, H} when H =:= ExpectedHeight ->
            case PrevRaw =:= PrevIndepHash of
                true -> ok;
                false -> {error, {previous_block_mismatch, PrevRaw, PrevIndepHash}}
            end;
        _ ->
            {error, {height_mismatch, HeightRaw, ExpectedHeight}}
    end.

ensure_sender_balances(State, TXRecords, PrevWalletRoot, Opts) ->
    Balances0 = hb_maps:get(<<"balances">>, State, #{}, Opts),
    Senders =
        lists:usort(
            [
                hb_util:encode(ar_wallet:to_address(TX#tx.owner, TX#tx.signature_type))
             ||
                TX <- TXRecords
            ]
        ),
    case lists:foldl(
        fun(Sender, {ok, BalancesAcc}) ->
                case fetch_wallet_balance_at_root(PrevWalletRoot, Sender, Opts) of
                    {ok, Balance} ->
                        {ok, hb_maps:put(Sender, Balance, BalancesAcc, Opts)};
                    {error, Reason} ->
                        {error, {sender_balance_lookup_failed, Sender, Reason}}
                end;
           (_, Error = {error, _}) ->
                Error
        end,
        {ok, Balances0},
        Senders
    ) of
        {ok, Balances} ->
            {ok, State#{<<"balances">> => Balances}};
        Error ->
            Error
    end.

fetch_wallet_balance_at_root(WalletRoot, Address, Opts)
        when is_binary(WalletRoot), is_binary(Address), byte_size(WalletRoot) > 0 ->
    Req =
        #{
            <<"path">> =>
                <<"/arweave/wallet_list/", WalletRoot/binary, "/", Address/binary, "/balance">>,
            <<"method">> => <<"GET">>
        },
    case http_request_with_retry(Req, Opts) of
        {ok, #{<<"body">> := Body}} ->
            case hb_util:safe_int(Body) of
                {ok, Balance} -> {ok, Balance};
                _ -> {error, invalid_balance_response}
            end;
        {error, Reason} ->
            {error, Reason};
        Other ->
            {error, Other}
    end;
fetch_wallet_balance_at_root(_WalletRoot, _Address, _Opts) ->
    {error, invalid_wallet_root_or_address}.

error_to_bin(Reason, _Opts) when is_binary(Reason) ->
    Reason;
error_to_bin(Reason, _Opts) when is_atom(Reason) ->
    hb_util:bin(atom_to_binary(Reason));
error_to_bin(Reason, _Opts) ->
    hb_util:bin(io_lib:format("~p", [Reason])).

%% @doc Make a request to the Arweave node and parse the response into an
%% AO-Core message. Most Arweave API responses are in JSON format, but without
%% a `content-type' header. Subsequently, we parse the response manually and
%% pass it back as a message.
request(Method, Path, Opts) ->
    request(Method, Path, #{}, Opts).
request(Method, Path, Req, Opts) ->
    ?event({arweave_request, {method, Method}, {path, Path}}),
    BaseReq =
        #{
            <<"path">> => <<"/arweave", Path/binary>>,
            <<"method">> => Method
        },
    Req1 = maybe_copy_key(<<"body">>, Req, BaseReq, Opts),
    Req2 = maybe_copy_key(<<"content-type">>, Req, Req1, Opts),
    Req3 = maybe_copy_key(<<"accept">>, Req, Req2, Opts),
    Res = http_request_with_retry(Req3, Opts),
    to_message(Path, Res, Opts).

http_request_with_retry(Req, Opts) ->
    RequestFun = fun() -> hb_http:request(Req, Opts) end,
    http_request_with_retry(RequestFun, 5, undefined).

http_request_with_retry(_RequestFun, 0, Last) when Last =/= undefined ->
    Last;
http_request_with_retry(RequestFun, AttemptsLeft, _Last) ->
    Res = safe_http_request(RequestFun),
    case should_retry_http_result(Res) of
        true when AttemptsLeft > 1 ->
            timer:sleep((6 - AttemptsLeft) * 100),
            http_request_with_retry(RequestFun, AttemptsLeft - 1, Res);
        _ ->
            Res
    end.

safe_http_request(RequestFun) ->
    try
        RequestFun()
    catch
        _Class:Reason ->
            {error, Reason}
    end.

should_retry_http_result({error, _}) ->
    true;
should_retry_http_result({failure, #{<<"status">> := Status}})
        when is_integer(Status), Status >= 500 ->
    true;
should_retry_http_result(_) ->
    false.

%% @doc Transform a response from the Arweave node into an AO-Core message.
to_message(_Path, {error, #{ <<"status">> := 404 }}, _Opts) ->
    {error, not_found};
to_message(_Path, {error, _}, _Opts) ->
    {error, client_error};
to_message(_Path, {failure, _}, _Opts) ->
    {error, server_error};
to_message(Path = <<"/tx/", TXID/binary>>, {ok, #{ <<"body">> := Body }}, Opts) ->
    TXHeader = ar_tx:json_struct_to_tx(hb_json:decode(Body)),
    ?event(
        {arweave_tx_response,
            {path, Path},
            {raw_body, {explicit, Body}},
            {body, {explicit, hb_json:decode(Body)}},
            {tx, TXHeader}
        }
    ),
    {ok, TX} = add_data(TXID, TXHeader, Opts),
    {
        ok,
        hb_message:convert(
            TX,
            <<"structured@1.0">>,
            <<"tx@1.0">>,
            Opts
        )
    };
to_message(Path = <<"/raw/", _/binary>>, {ok, #{ <<"body">> := Body }}, _Opts) ->
    ?event(
        {arweave_raw_response,
            {path, Path},
            {data_size, byte_size(Body)}
        }
    ),
    {ok, Body};
to_message(Path = <<"/block/", _/binary>>, {ok, #{ <<"body">> := Body }}, Opts) ->
    Block = hb_message:convert(Body, <<"structured@1.0">>, <<"json@1.0">>, Opts),
    ?event(
        {arweave_block_response,
            {path, Path},
            {block, Block}
        }
    ),
    CacheRes = dev_arweave_block_cache:write(Block, Opts),
    ?event(
        {cached_arweave_block,
            {path, Path},
            {result, CacheRes}
        }
    ),
    {ok, Block};
to_message(<<"/price/", _/binary>>, {ok, #{ <<"body">> := Body }}, _Opts) ->
    Price = hb_util:int(Body),
    {ok, Price};
to_message(<<"/tx_anchor">>, {ok, #{ <<"body">> := Body }}, _Opts) ->
    Anchor = hb_util:decode(Body),
    {ok, Anchor};
to_message(Path, {ok, #{ <<"body">> := Body }}, Opts) ->
    % All other responses that are `OK' status are converted from JSON to an
    % AO-Core message.
    ?event(
        {arweave_json_response,
            {path, Path},
            {body_size, byte_size(Body)}
        }
    ),
    {
        ok,
        hb_message:convert(
            Body,
            <<"structured@1.0">>,
            <<"json@1.0">>,
            Opts
        )
    }.

%%% Tests

resolve_arweave(Path, Req, Opts) ->
    hb_ao:resolve(
        #{<<"device">> => dev_arweave},
        Req#{<<"path">> => Path},
        test_opts(Opts)
    ).

test_opts(Opts) ->
    case maps:is_key(store, Opts) of
        true -> Opts;
        false -> Opts#{store => [hb_test_utils:test_store()]}
    end.

delegates_vdf_compute_test() ->
    PrevOutput = hb_util:decode(<<"f_z7RLug8etm3SrmRf-xPwXEL0ZQ_xHng2A5emRDQBw">>),
    {ok, Res} =
        resolve_arweave(
            <<"vdf">>,
            #{
                <<"action">> => <<"compute">>,
                <<"step-number">> => 2,
                <<"prev-output">> => hb_util:encode(PrevOutput),
                <<"iteration-count">> => 2
            },
            #{}
        ),
    ?assert(hb_maps:get(<<"output">>, Res, not_found, #{}) =/= not_found).

delegates_spora_entropy_test() ->
    {ok, 1200} =
        resolve_arweave(
            <<"spora">>,
            #{
                <<"action">> => <<"entropy-reset-point">>,
                <<"prev-step-number">> => 1199,
                <<"step-number">> => 1200
            },
            #{}
        ).

delegates_ledger_validate_test() ->
    {ok, Res} =
        resolve_arweave(
            <<"ledger">>,
            #{
                <<"action">> => <<"validate-tx">>,
                <<"tx">> => #{}
            },
            #{}
        ),
    ?assertEqual(false, hb_maps:get(<<"valid">>, Res, true, #{})).

delegates_gossip_tx_test() ->
    Opts = #{store => [hb_test_utils:test_store()]},
    {ok, #{<<"accepted">> := true}} =
        resolve_arweave(
            <<"gossip">>,
            #{
                <<"action">> => <<"tx">>,
                <<"method">> => <<"POST">>,
                <<"tx">> => #{<<"hello">> => <<"world">>}
            },
            Opts
        ),
    {ok, Listed} =
        resolve_arweave(
            <<"gossip">>,
            #{
                <<"action">> => <<"tx">>,
                <<"method">> => <<"GET">>
            },
            Opts
        ),
    ?assertEqual(1, hb_maps:get(<<"count">>, Listed, 0, #{})).

validate_network_block_with_provided_txs_test() ->
    SenderWallet = ar_wallet:new(),
    RecipientWallet = ar_wallet:new(),
    MinerWallet = ar_wallet:new(),
    RecipientAddr = ar_wallet:to_address(RecipientWallet),
    MinerAddr = hb_util:encode(ar_wallet:to_address(MinerWallet)),
    SenderKey = hb_util:encode(ar_wallet:to_address(SenderWallet)),
    InitialState =
        #{
            <<"height">> => 0,
            <<"balances">> => #{SenderKey => 10000},
            <<"pending-reward">> => 0,
            <<"tx-history">> => []
        },
    TX0 =
        #tx{
            format = 2,
            target = RecipientAddr,
            quantity = 1000,
            reward = 1,
            data = <<>>,
            data_size = 0
        },
    SignedTX = ar_tx:sign(TX0, SenderWallet),
    TXMsg = hb_message:convert(SignedTX, <<"structured@1.0">>, <<"tx@1.0">>, #{}),
    {ok, Block} =
        hb_ao:resolve(
            #{<<"device">> => dev_arweave_ledger},
            #{
                <<"path">> => <<"generate-block">>,
                <<"state">> => InitialState,
                <<"txs">> => [TXMsg],
                <<"reward-addr">> => MinerAddr,
                <<"timestamp">> => 1000
            },
            #{}
        ),
    {ok, Validation} =
        resolve_arweave(
            <<"validate-network-block">>,
            #{
                <<"block">> => Block,
                <<"txs-full">> => [TXMsg]
            },
            #{}
        ),
    ?assertEqual(true, hb_maps:get(<<"valid">>, Validation, false, #{})).

validate_latest_network_block_live_test_() ->
    case os:getenv("HB_LIVE_ARWEAVE_TESTS") of
        false ->
            [];
        _ ->
            {timeout, 300, fun run_validate_latest_network_block_live/0}
    end.

run_validate_latest_network_block_live() ->
    ok = ensure_http_clients_started(),
    {ok, Validation} =
        resolve_arweave(
            <<"validate-network-block">>,
            #{<<"block">> => <<"current">>},
            #{gateway => <<"https://arweave.net">>}
        ),
    ?assertEqual(true, hb_maps:get(<<"valid">>, Validation, false, #{})),
    ?assertEqual(true, hb_maps:get(<<"block-hash-valid">>, Validation, false, #{})).

validate_recent_network_blocks_live_test_() ->
    case os:getenv("HB_LIVE_ARWEAVE_TESTS") of
        false ->
            [];
        _ ->
            {timeout, 900, fun run_validate_recent_network_blocks_live/0}
    end.

run_validate_recent_network_blocks_live() ->
    ok = ensure_http_clients_started(),
    GatewayOpts = #{gateway => <<"https://arweave.net">>},
    {ok, CurrentBlock} =
        resolve_arweave(
            <<"network-block">>,
            #{<<"block">> => <<"current">>},
            GatewayOpts
        ),
    CurrentHeight = hb_maps:get(<<"height">>, CurrentBlock, 0, #{}),
    HeightCandidates = [CurrentHeight, max(0, CurrentHeight - 1), max(0, CurrentHeight - 2)],
    Heights = lists:usort(HeightCandidates),
    lists:foreach(
        fun(Height) ->
            {ok, Validation} =
                resolve_arweave(
                    <<"validate-network-block">>,
                    #{<<"block">> => Height},
                    GatewayOpts
                ),
            ?assertEqual(true, hb_maps:get(<<"valid">>, Validation, false, #{})),
            ?assertEqual(true, hb_maps:get(<<"block-hash-valid">>, Validation, false, #{}))
        end,
        Heights
    ).

validate_last_50_network_blocks_live_test_() ->
    case os:getenv("HB_LIVE_ARWEAVE_TESTS") of
        false ->
            [];
        _ ->
            {timeout, 3600, fun run_validate_last_50_network_blocks_live/0}
    end.

run_validate_last_50_network_blocks_live() ->
    ok = ensure_http_clients_started(),
    {ok, Validation} =
        resolve_arweave(
            <<"validate-network-chain">>,
            #{
                <<"block">> => <<"current">>,
                <<"count">> => 50
            },
            #{gateway => <<"https://arweave.net">>}
        ),
    ?assertEqual(true, hb_maps:get(<<"valid">>, Validation, false, #{})),
    ?assertEqual(50, hb_maps:get(<<"count">>, Validation, 0, #{})).

ensure_http_clients_started() ->
    ok = ensure_app_started(ssl),
    ok = ensure_app_started(inets),
    ok.

ensure_app_started(App) ->
    case application:ensure_all_started(App) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        _ -> ok
    end.

post_ans104_tx_test() ->
    ServerOpts = #{ store => [hb_test_utils:test_store()] },
    Server = hb_http_server:start_node(ServerOpts),
    ClientOpts =
        #{
            store => [hb_test_utils:test_store()],
            priv_wallet => hb:wallet()
        },
    Msg =
        hb_message:commit(
            #{
                <<"variant">> => <<"ao.N.1">>,
                <<"type">> => <<"Process">>,
                <<"data">> => <<"test-data">>
            },
            ClientOpts,
            #{ <<"commitment-device">> => <<"ans104@1.0">> }
        ),
    {ok, PostRes} =
        hb_http:post(
            Server,
            Msg#{
                <<"path">> => <<"/~arweave@2.9-pre/tx">>,
                <<"codec-device">> => <<"ans104@1.0">>
            },
            ClientOpts
        ),
    ?assertMatch(#{ <<"status">> := 200 }, PostRes),
    SignedID = hb_message:id(Msg, signed, ClientOpts),
    {ok, GetRes} =
        hb_http:get(
            Server, <<"/", SignedID/binary>>,
            ClientOpts
        ),
    ?assertMatch(
        #{
            <<"status">> := 200,
            <<"variant">> := <<"ao.N.1">>,
            <<"type">> := <<"Process">>,
            <<"data">> := <<"test-data">>
        },
        GetRes
    ),
    ok.

get_tx_basic_data_test() ->
    Node = hb_http_server:start_node(),
    Path = <<"/~arweave@2.9-pre/tx=ptBC0UwDmrUTBQX3MqZ1lB57ex20ygwzkjjCrQjIx3o">>,
    {ok, Structured} = hb_http:get(Node, Path, #{}),
    ?event(debug_test, {structured_tx, Structured}),
    ?assert(hb_message:verify(Structured, all, #{})),
    % Hash the data to make it easier to match
    StructuredWithHash = Structured#{
        <<"data">> => hb_util:encode(
            crypto:hash(sha256, (maps:get(<<"data">>, Structured)))
        )
    },
    ExpectedMsg = #{
        <<"data">> => <<"PEShWA1ER2jq7CatAPpOZ30TeLrjOSpaf_Po7_hKPo4">>,
        <<"reward">> => <<"482143296">>,
        <<"anchor">> => <<"XTzaU2_m_hRYDLiXkcleOC4zf5MVTXIeFWBOsJSRrtEZ8kM6Oz7EKLhZY7fTAvKq">>,
        <<"content-type">> => <<"application/json">>
    },
    ?assert(hb_message:match(ExpectedMsg, StructuredWithHash, only_present)),
    ok.

get_tx_rsa_nested_bundle_test() ->
    Node = hb_http_server:start_node(),
    Path = <<"/~arweave@2.9-pre/tx=bndIwac23-s0K11TLC1N7z472sLGAkiOdhds87ZywoE">>,
    {ok, Root} = hb_http:get(Node, Path, #{}),
    ?event(debug_test, {root, Root}),
    ?assert(hb_message:verify(Root, all, #{})),

    ChildPath = <<Path/binary, "/1/2">>,
    {ok, Child} = hb_http:get(Node, ChildPath, #{}),
    ?event(debug_test, {child, Child}),
    ?assert(hb_message:verify(Child, all, #{})),

    {ok, ExpectedChild} =
        hb_ao:resolve(
            Root,
            <<"1/2">>,
            #{}
        ),
    ?assert(hb_message:match(ExpectedChild, Child, only_present)),

    ManualChild = #{
        <<"data">> => <<"{\"totalTickedRewardsDistributed\":0,\"distributedEpochIndexes\":[],\"newDemandFactors\":[],\"newEpochIndexes\":[],\"tickedRewardDistributions\":[],\"newPruneGatewaysResults\":[{\"delegateStakeReturned\":0,\"stakeSlashed\":0,\"gatewayStakeReturned\":0,\"delegateStakeWithdrawing\":0,\"prunedGateways\":[],\"slashedGateways\":[],\"gatewayStakeWithdrawing\":0}]}">>,
        <<"data-protocol">> => <<"ao">>,
        <<"from-module">> => <<"cbn0KKrBZH7hdNkNokuXLtGryrWM--PjSTBqIzw9Kkk">>,
        <<"from-process">> => <<"agYcCFJtrMG6cqMuZfskIkFTGvUPddICmtQSBIoPdiA">>,
        <<"anchor">> => <<"MDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAyODAxODg">>,
        <<"reference">> => <<"280188">>,
        <<"target">> => <<"1R5QEtX53Z_RRQJwzFWf40oXiPW2FibErT_h02pu8MU">>,
        <<"type">> => <<"Message">>,
        <<"variant">> => <<"ao.TN.1">>
    },
    ?assert(hb_message:match(ManualChild, Child, only_present)),
    ok.

%% @TODO: This test is disabled because it takes too long to run. Re-enable
%% once some performance optimizations are implemented.
get_tx_rsa_large_bundle_test_disabled() ->
    {timeout, 300, fun() ->
        Node = hb_http_server:start_node(),
        Path = <<"/~arweave@2.9-pre/tx=VifINXnMxLwJXOjHG5uM0JssiylR8qvajjj7HlzQvZA">>,
        {ok, Root} = hb_http:get(Node, Path, #{}),
        ?event(debug_test, {root, Root}),
        ?assert(hb_message:verify(Root, all, #{})),
        ok
    end}.

get_bad_tx_test() ->
    Node = hb_http_server:start_node(),
    Path = <<"/~arweave@2.9-pre/tx=INVALID-ID">>,
    Res = hb_http:get(Node, Path, #{}),
    ?assertEqual({ok, client_error}, Res).

%% @doc: helper test to generate and write a dataitem to disk so that we
%% can validate it using 3rd-party js libraries and gateways.
serialize_data_item_test_disabled() ->
    DataItem = ar_bundles:sign_item(
        #tx{
            data = <<"Hello from HyperBEAM test!">>,
            tags = [
                {<<"content-type">>, <<"text/plain">>},
                {<<"test-tag">>, <<"test-value">>},
                {<<"app-name">>, <<"HyperBEAM">>}
            ]
        },
        hb:wallet()
    ),
    SerializedItem = ar_bundles:serialize(DataItem),
    % Write to disk in the test directory
    OutputPath = filename:join([
        "test",
        "arbundles.js",
        "hyperbeam-test-item.bin"
    ]),
    ok = filelib:ensure_dir(OutputPath),
    ok = file:write_file(OutputPath, SerializedItem),
    ?event({wrote_data_item, {path, OutputPath}, {size, byte_size(SerializedItem)}}),
    ?assert(filelib:is_file(OutputPath)),
    % Read it back and verify it deserializes correctly
    {ok, ReadData} = file:read_file(OutputPath),
    VerifiedItem = ar_bundles:deserialize(ReadData),
    ?assertEqual(DataItem#tx.data, VerifiedItem#tx.data),
    ?assertEqual(length(DataItem#tx.tags), length(VerifiedItem#tx.tags)),
    ?assert(ar_bundles:verify_item(VerifiedItem)),
    ok.
