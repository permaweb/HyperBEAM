%%% @doc A device that provides access to Arweave network information, relayed
%%% from a designated node.
%%%
%%% The node(s) that are used to query data may be configured by altering the
%%% `/arweave` route in the node's configuration message.
-module(dev_arweave).
-export([info/1, info/3, default/4]).
-export([tx/3, chunk/3, block/3, current/3, status/3, price/3, tx_anchor/3]).
-export([graphql/3, vdf/3, spora/3, ledger/3, gossip/3]).
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
                    <<"gossip">>
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
                    gossip(Base, Request#{<<"action">> => <<"tx/pending">>}, Opts);
                <<"ready_for_mining">> ->
                    gossip(Base, Request#{<<"action">> => <<"tx/ready_for_mining">>}, Opts);
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
        <<"GET">> -> {error, not_implemented}
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
    Block =
        hb_ao:get_first(
            [
                {Request, <<"block">>},
                {Base, <<"block">>}
            ],
            not_found,
            Opts
        ),
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
    Size =
        hb_ao:get_first(
            [
                {Request, <<"size">>},
                {Base, <<"size">>}
            ],
            not_found,
            Opts
        ),
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

%%% Internal Functions

%% @doc Find the transaction ID to retrieve from Arweave based on the request or
%% base message.
find_txid(Base, Request, Opts) ->
    hb_ao:get_first(
        [
            {Request, <<"tx">>},
            {Base, <<"tx">>}
        ],
        not_found,
        Opts
    ).

subkey(Request, Default, Opts) ->
    Raw = hb_ao:get_first([{Request, <<"action">>}, {Request, <<"path">>}], Default, Opts),
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
    Res =
        hb_http:request(
            Req3,
            Opts
        ),
    to_message(Path, Res, Opts).

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

delegates_vdf_compute_test() ->
    PrevOutput = hb_util:decode(<<"f_z7RLug8etm3SrmRf-xPwXEL0ZQ_xHng2A5emRDQBw">>),
    {ok, Res} =
        vdf(
            #{},
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
        spora(
            #{},
            #{
                <<"action">> => <<"entropy-reset-point">>,
                <<"prev-step-number">> => 1199,
                <<"step-number">> => 1200
            },
            #{}
        ).

delegates_ledger_validate_test() ->
    {ok, Res} =
        ledger(
            #{},
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
        gossip(
            #{},
            #{
                <<"action">> => <<"tx">>,
                <<"method">> => <<"POST">>,
                <<"tx">> => #{<<"hello">> => <<"world">>}
            },
            Opts
        ),
    {ok, Listed} =
        gossip(
            #{},
            #{
                <<"action">> => <<"tx">>,
                <<"method">> => <<"GET">>
            },
            Opts
        ),
    ?assertEqual(1, hb_maps:get(<<"count">>, Listed, 0, #{})).

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
    ?assertEqual({error, not_found}, Res).

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
