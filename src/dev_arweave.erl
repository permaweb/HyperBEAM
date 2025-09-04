%%% @doc A device that provides access to Arweave network information, relayed
%%% from a designated node.
%%%
%%% The node(s) that are used to query data may be configured by altering the
%%% `/arweave` route in the node's configuration message.
-module(dev_arweave).
-export([tx/3, block/3, current/3, status/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Proxy the `/info' endpoint from the Arweave node.
status(_Base, _Request, Opts) ->
    request(<<"GET">>, <<"/info">>, Opts).

%% @doc Returns the given transaction, if known to the client node(s), as an
%% AO-Core message.
tx(Base, Request, Opts) ->
    case hb_maps:get(<<"method">>, Request, <<"GET">>, Opts) of
        <<"POST">> -> post_tx(Base, Request, Opts);
        <<"GET">> -> get_tx(Base, Request, Opts)
    end.

%% @doc Upload a transaction to Arweave, using the node's default bundler (see
%% `hb_client:upload/2' for more details). Ensures that uploaded transactions are
%% stored in the local cache after a successful response has been received.
post_tx(_Base, Request, Opts) ->
    case hb_client:upload(Request, Opts) of
        Res = {ok, _} ->
            ?event(arweave, {uploaded, Request}),
            CacheRes = hb_cache:write(Request, Opts),
            ?event(arweave,
                {cache_uploaded_message,
                    {msg, Request},
                    {status,
                        case CacheRes of {ok, _} -> ok;
                        _ -> failed
                        end
                    }
                }
            ),
            Res;
        Res ->
            Res
    end.

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

%% @doc Handle the optional adding of data to the transaction header, depending
%% on the request. Semantics of the `data' key are described in the `get_tx/3'
%% documentation.
maybe_add_data(TXID, TXHeader, Base, Request, Opts) ->
    GetData =
        hb_util:atom(hb_ao:get_first(
            [
                {Request, <<"data">>},
                {Base, <<"data">>}
            ],
            true,
            Opts
        )),
    case hb_util:atom(GetData) of
        false ->
            {ok, TXHeader};
        _ ->
            case add_data(TXID, TXHeader, Opts) of
                {ok, TX} -> {ok, TX};
                {error, Reason} ->
                    if GetData =/= always -> {ok, TXHeader};
                    true -> {error, Reason}
                    end
            end
    end.

add_data(TXID, TXHeader, Opts) ->
    case data(TXID, Opts) of
        {ok, Data} ->
            TX = TXHeader#tx{ data = Data },
            ?event(
                arweave,
                {retrieved_tx_with_data,
                    {id, TXID},
                    {data_size, byte_size(Data)},
                    {tx, TX}
                }
            ),
            {ok, TX};
        {error, Reason} ->
            ?event(arweave,
                {data_retrieval_failed_after_header,
                    {id, TXID},
                    {error, Reason}
                }
            ),
            {error, Reason}
    end.

%% @doc Retrieve the data of a transaction from Arweave.
data(TXID, Opts) ->
    ?event(arweave, {retrieving_tx_data, {tx, TXID}}),
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
            ?event(arweave, {retrieved_block_from_cache, {id, ID}}),
            {ok, Block};
        not_found ->
            request(<<"GET">>, <<"/block/hash/", ID/binary>>, Opts)
    end;
block({height, Height}, Opts) ->
    case dev_arweave_block_cache:read(Height, Opts) of
        {ok, Block} ->
            ?event(arweave, {retrieved_block_from_cache, {height, Height}}),
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

%% @doc Make a request to the Arweave node and parse the response into an
%% AO-Core message. Most Arweave API responses are in JSON format, but without
%% a `content-type' header. Subsequently, we parse the response manually and
%% pass it back as a message.
request(Method, Path, Opts) ->
    ?event(arweave, {arweave_request, {method, Method}, {path, Path}}),
    Res =
        hb_http:request(
            #{
                <<"path">> => <<"/arweave", Path/binary>>,
                <<"method">> => Method
            },
            Opts
        ),
    to_message(Path, Res, Opts).

%% @doc Transform a response from the Arweave node into an AO-Core message.
to_message(_Path, {error, #{ <<"status">> := 404 }}, _Opts) ->
    {error, not_found};
to_message(Path = <<"/tx/", TXID/binary>>, {ok, #{ <<"body">> := Body }}, Opts) ->
    TXHeader = ar_tx:json_struct_to_tx(hb_json:decode(Body)),
    ?event(arweave,
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
    ?event(arweave,
        {arweave_raw_response,
            {path, Path},
            {data_size, byte_size(Body)}
        }
    ),
    {ok, Body};
to_message(Path = <<"/block/", _/binary>>, {ok, #{ <<"body">> := Body }}, Opts) ->
    Block = hb_message:convert(Body, <<"structured@1.0">>, <<"json@1.0">>, Opts),
    ?event(arweave,
        {arweave_block_response,
            {path, Path},
            {block, Block}
        }
    ),
    CacheRes = dev_arweave_block_cache:write(Block, Opts),
    ?event(arweave,
        {cached_arweave_block,
            {path, Path},
            {result, CacheRes}
        }
    ),
    {ok, Block};
to_message(Path, {ok, #{ <<"body">> := Body }}, Opts) ->
    % All other responses that are `OK' status are converted from JSON to an
    % AO-Core message.
    ?event(arweave,
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
get_tx_test() ->
    Node = hb_http_server:start_node(),
    Path = <<"/~arweave@2.9-pre/tx?tx=MH1M9gedlPsu7-57-N0S_-F1OBoYacLYLe5jL0Tvr9c">>,
    {ok, StructuredTX} = hb_http:get(Node, Path, #{}),
    ?event(debug_test, {structured_tx, StructuredTX}),
    ?assert(hb_message:verify(StructuredTX, all, #{})).


get_bad_tx_test() ->
    Node = hb_http_server:start_node(),
    Path = <<"/~arweave@2.9-pre/tx?tx=INVALID-ID">>,
    Res = hb_http:get(Node, Path, #{}),
    ?assertEqual({error, not_found}, Res).
