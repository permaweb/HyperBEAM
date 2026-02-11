%%% @doc A stateless device for fetching Ethereum contract event logs.
%%%
%%% Every request is self-contained — the caller passes all parameters
%%% (RPC URL, contract, event signature, mode) in each call. No session
%%% state, no cookies, no configure step.
%%%
%%% The device is RPC-provider agnostic. It speaks standard Ethereum
%%% JSON-RPC (`eth_blockNumber', `eth_getLogs') and works with Alchemy,
%%% Moralis, Infura, public RPCs, or any endpoint that implements the
%%% Ethereum JSON-RPC spec.
%%%
%%% Usage:
%%% <pre>
%%%     curl /~eth-events@1.0/get-data&amp;rpc-url=https://eth.rpc.com&amp;contract=0xABC...
%%% </pre>
-module(dev_eth_events).
-export([info/1, get_data/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_BLOCK_OFFSET, 40).
-define(DEFAULT_ROLLING_WINDOW, 50).

%% @doc Device metadata — exports only `get-data'.
info(_Base) ->
    #{ exports => [<<"get-data">>] }.

%% @doc Fetch Ethereum contract event logs.
%% Required params: `rpc-url', `contract'.
%% Optional: `api-key', `api-key-header', `event-signature', `mode', `raw'.
%% Modes: `latest-only' (default), `incremental', `rolling-window', `range'.
get_data(_Base, Req, Opts) ->
    maybe
        {ok, RpcConf, Contract, EventSig, Mode, Raw} ?= parse_params(Req, Opts),
        ?event(
            {eth_events_get_data, 
                {mode, Mode}, 
                {contract, Contract},
                {event, EventSig}
            }
        ),
        {ok, FromBlock, ToBlock, CurrentBlock} ?=
            resolve_block_range(Mode, Req, RpcConf, Opts),
        FilterParams =
            build_filter_params(Contract, EventSig, FromBlock, ToBlock),
        {ok, Events} ?=
            rpc(RpcConf, <<"eth_getLogs">>, [FilterParams], Opts),
        Decoded = decode_events(Raw, EventSig, Events),
        {ok, build_response(
            Decoded, FromBlock, ToBlock, CurrentBlock, Contract
        )}
    else
        {error, Reason} -> {error, Reason}
    end.

%% @doc Validate that a required parameter is present.
require(Key, Req, Opts) ->
    case hb_ao:get(Key, Req, not_found, Opts) of
        not_found -> {error, <<Key/binary, " parameter required.">>};
        Value -> {ok, Value}
    end.

%% @doc Extract and normalize all request parameters once.
%% Returns {ok, Params} or {error, Reason}.
parse_params(Req, Opts) ->
    maybe
        {ok, RpcUrl} ?= require(<<"rpc-url">>, Req, Opts),
        {ok, Contract} ?= require(<<"contract">>, Req, Opts),
        {
            ok, 
            #{
                url => RpcUrl,
                api_key => hb_ao:get(<<"api-key">>, Req, <<>>, Opts),
                api_key_header => hb_ao:get(
                    <<"api-key-header">>, Req, <<"x-api-key">>, Opts
                )
            },
            Contract,
            hb_ao:get(<<"event-signature">>, Req, not_found, Opts),
            hb_ao:get(<<"mode">>, Req, <<"latest-only">>, Opts),
            hb_ao:get(<<"raw">>, Req, <<"false">>, Opts)
        }
    else
        {error, Reason} -> {error, Reason}
    end.

%% @doc Resolve the block range for a given mode.
%% Returns {ok, FromBlock, ToBlock, CurrentBlock} or {error, Reason}.
resolve_block_range(Mode, Req, RpcConfig, Opts) ->
    maybe
        {ok, CurrentBlock} ?=
            rpc(RpcConfig, <<"eth_blockNumber">>, [], Opts),
        {ok, FromBlock, ToBlock} ?= calculate_block_range(
            Mode, Req, CurrentBlock, Opts
        ),
        {ok, FromBlock, ToBlock, CurrentBlock}
    else
        {error, Reason} -> {error, Reason}
    end.

%% @doc Calculate from/to block based on mode and current block.
calculate_block_range(<<"latest-only">>, _Req, CurrentBlock, _Opts) ->
    {ok, CurrentBlock, CurrentBlock};
calculate_block_range(<<"range">>, Req, _CurrentBlock, Opts) ->
    maybe
        {ok, FromBlock} ?= require(<<"from-block">>, Req, Opts),
        {ok, ToBlock} ?= require(<<"to-block">>, Req, Opts),
        {ok, ensure_hex(FromBlock), ensure_hex(ToBlock)}
    else
        {error, Reason} -> {error, Reason}
    end;
calculate_block_range(<<"incremental">>, Req, CurrentBlock, Opts) ->
    Offset = hb_ao:get(<<"block-offset">>, Req, ?DEFAULT_BLOCK_OFFSET, Opts),
    CurrentBlockInt = hex_to_int(CurrentBlock),
    TargetBlockInt = max(0, CurrentBlockInt - hb_util:int(Offset)),
    FromBlockInt = 
        case hb_ao:get(<<"last-block">>, Req, not_found, Opts) of
            not_found -> TargetBlockInt;
            LastBlock -> min(hex_to_int(LastBlock) + 1, TargetBlockInt)
        end,
    {ok, int_to_hex(FromBlockInt), int_to_hex(TargetBlockInt)};
calculate_block_range(<<"rolling-window">>, Req, CurrentBlock, Opts) ->
    Window = hb_ao:get(<<"block-window">>, Req, ?DEFAULT_ROLLING_WINDOW, Opts),
    CurrentBlockInt = hex_to_int(CurrentBlock),
    FromBlock = max(0, CurrentBlockInt - hb_util:int(Window)),
    {ok, int_to_hex(FromBlock), CurrentBlock};
calculate_block_range(Mode, _Req, _CurrentBlock, _Opts) ->
    {error, <<"Unknown mode: ", Mode/binary>>}.

%% @doc Make a JSON-RPC call and parse the result in one step.
%% Combines the relay call with response parsing.
rpc(RpcConfig, Method, Params, Opts) ->
    case call_rpc(RpcConfig, Method, Params, Opts) of
        {ok, RelayResponse} ->
            parse_rpc_response(RelayResponse);
        {error, Reason} ->
            {error, Reason}
    end.

call_rpc(
        #{ url := Url, api_key := ApiKey, api_key_header := ApiKeyHeader },
        Method,
        Params,
        Opts
    ) ->
    Body = hb_json:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => Method,
        <<"params">> => Params,
        <<"id">> => 1
    }),
    ?event({eth_events_rpc_call, {method, Method}, {body, {string, Body}}}),
    BaseRequest = #{
        <<"device">> => <<"relay@1.0">>,
        <<"method">> => <<"POST">>,
        <<"path">> => Url,
        <<"content-type">> => <<"application/json">>,
        <<"body">> => Body
    },
    Request = case ApiKey of
        <<>> -> BaseRequest;
        _ -> BaseRequest#{ ApiKeyHeader => ApiKey }
    end,
    hb_ao:resolve(Request, <<"call">>, Opts).

parse_rpc_response(RelayResponse) when is_map(RelayResponse) ->
    case hb_maps:get(<<"body">>, RelayResponse, <<>>, #{}) of
        <<>> ->
            {error, <<"Empty response body from RPC.">>};
        Body ->
            parse_json_rpc_body(Body)
    end.

parse_json_rpc_body(Body) when is_binary(Body) ->
    try hb_json:decode(Body) of
        Decoded -> extract_rpc_result(Decoded)
    catch
        _:_ -> {error, <<"Invalid JSON in RPC response.">>}
    end;
parse_json_rpc_body(Body) when is_map(Body) ->
    extract_rpc_result(Body).

extract_rpc_result(#{<<"result">> := Result}) ->
    {ok, Result};
extract_rpc_result(#{<<"error">> := #{<<"message">> := Msg}}) ->
    {error, Msg};
extract_rpc_result(#{<<"error">> := Error}) ->
    {error, Error};
extract_rpc_result(_) ->
    {error, <<"Unexpected JSON-RPC response.">>}.

%%% Response building
build_response(Events, FromBlock, ToBlock, CurrentBlock, Contract) ->
    #{
        <<"data">> => hb_json:encode(Events),
        <<"from-block">> => FromBlock,
        <<"from-block-number">> => hex_to_int(FromBlock),
        <<"to-block">> => ToBlock,
        <<"to-block-number">> => hex_to_int(ToBlock),
        <<"latest-block">> => CurrentBlock,
        <<"latest-block-number">> => hex_to_int(CurrentBlock),
        <<"contract">> => Contract,
        <<"status">> => 200
    }.

%% @doc Build `eth_getLogs' filter parameters.
build_filter_params(Contract, EventSig, FromBlock, ToBlock) ->
    Base = #{
        <<"fromBlock">> => ensure_hex(FromBlock),
        <<"toBlock">> => ensure_hex(ToBlock),
        <<"address">> => Contract
    },
    case EventSig of
        not_found -> Base;
        <<>> -> Base;
        _ ->
            Sigs = split_event_signatures(EventSig),
            Hashes = lists:map(fun keccak256_topic/1, Sigs),
            Topics = case Hashes of
                [Single] -> [Single];
                Multiple -> [Multiple]
            end,
            Base#{ <<"topics">> => Topics }
    end.

keccak256_topic(EventSig) ->
    Hash = hb_keccak:keccak_256(EventSig),
    <<"0x", (hb_util:to_hex(Hash))/binary>>.

%% @doc Split a comma-separated list of event signatures.
%% Splits on ")," boundaries to avoid breaking on commas inside param lists.
split_event_signatures(EventSig) ->
    Parts = binary:split(EventSig, <<"),">>, [global]),
    restore_closing_parens(Parts).

restore_closing_parens([Last]) -> [Last];
restore_closing_parens([H | T]) ->
    [<<H/binary, ")">> | restore_closing_parens(T)].

%%% Event decoding

%% @doc Decode events based on raw flag and event signature.
%% raw=true: return events as-is from RPC.
%% raw=false, no sig: decode block metadata only, topics/data stay hex.
%% raw=false + sig: full ABI decode.
decode_events(<<"true">>, _EventSig, Events) ->
    Events;
decode_events(_, NoSig, Events) when NoSig =:= not_found; NoSig =:= <<>> ->
    lists:map(fun decode_event_metadata/1, Events);
decode_events(_, EventSig, Events) ->
    Sigs = split_event_signatures(EventSig),
    SigMap = maps:from_list(lists:map(
        fun(Sig) ->
            {Name, Types} = parse_event_signature(Sig),
            Hash = keccak256_topic(Sig),
            {Hash, {Name, Types}}
        end,
        Sigs
    )),
    lists:map(
        fun(Event) -> decode_event_by_topic(Event, SigMap) end,
        Events
    ).

%% @doc Extract common event metadata shared by all decode paths.
event_metadata(Event) ->
    #{
        <<"block-number">> =>
            hex_to_int(hb_maps:get(<<"blockNumber">>, Event, <<"0x0">>)),
        <<"transaction-hash">> =>
            hb_maps:get(<<"transactionHash">>, Event, <<>>),
        <<"log-index">> =>
            hex_to_int(hb_maps:get(<<"logIndex">>, Event, <<"0x0">>)),
        <<"removed">> => hb_maps:get(<<"removed">>, Event, false)
    }.

%% @doc Decode only block metadata (hex to decimal), keep topics/data raw.
%% Used when no event-signature is provided.
decode_event_metadata(Event) when is_map(Event) ->
    (event_metadata(Event))#{
        <<"address">> => hb_maps:get(<<"address">>, Event, <<>>),
        <<"topics">> => hb_maps:get(<<"topics">>, Event, []),
        <<"data">> => hb_maps:get(<<"data">>, Event, <<>>)
    }.

%% @doc Fully decode an event log using the parsed event signature.
decode_event(Event, Name, Types) when is_map(Event) ->
    Topics = hb_maps:get(<<"topics">>, Event, []),
    Data = hb_maps:get(<<"data">>, Event, <<"0x">>),
    IndexedTopics = 
        case Topics of
            [_ | Rest] -> Rest;
            [] -> []
        end,
    NumIndexed = length(IndexedTopics),
    SplitAt = min(NumIndexed, length(Types)),
    {IndexedTypes, DataTypes} = lists:split(SplitAt, Types),
    IndexedArgs = lists:zipwith(
        fun(Topic, Type) -> decode_topic(Topic, Type) end,
        IndexedTopics,
        IndexedTypes
    ),
    DataArgs = decode_data_params(Data, DataTypes),
    (event_metadata(Event))#{
        <<"event">> => Name,
        <<"args">> => IndexedArgs ++ DataArgs
    }.

%% @doc Decode an event by matching its topic[0] against a signature map.
%% Falls back to metadata-only decoding if no matching signature is found.
decode_event_by_topic(Event, SigMap) when is_map(Event) ->
    Topics = hb_maps:get(<<"topics">>, Event, []),
    case Topics of
        [TopicHash | _] ->
            case maps:get(TopicHash, SigMap, not_found) of
                {Name, Types} ->
                    decode_event(Event, Name, Types);
                not_found ->
                    decode_event_metadata(Event)
            end;
        [] ->
            decode_event_metadata(Event)
    end.

%% @doc Parse event signature into name and type list.
parse_event_signature(Sig) ->
    case binary:match(Sig, <<"(">>) of
        {Pos, _} ->
            Name = binary:part(Sig, 0, Pos),
            TypesStart = Pos + 1,
            TypesLen = byte_size(Sig) - TypesStart - 1,
            TypesStr = binary:part(Sig, TypesStart, TypesLen),
            Types = case TypesStr of
                <<>> -> [];
                _ -> binary:split(TypesStr, <<",">>, [global])
            end,
            {Name, Types};
        nomatch ->
            {Sig, []}
    end.

%% @doc Decode a single indexed topic value based on its ABI type.
decode_topic(<<"0x", Hex/binary>>, Type) ->
    decode_value(Hex, Type).

%% @doc Decode non-indexed params from the data field.
%% Data is a hex string of concatenated 32-byte ABI-encoded values.
decode_data_params(<<"0x">>, _Types) -> [];
decode_data_params(<<"0x", HexData/binary>>, Types) ->
    Words = split_into_words(HexData),
    Len = min(length(Words), length(Types)),
    lists:zipwith(
        fun(Word, Type) -> decode_value(Word, Type) end,
        lists:sublist(Words, Len),
        lists:sublist(Types, Len)
    );
decode_data_params(_, _Types) -> [].

%% @doc Split hex data into 64-char (32-byte) words.
split_into_words(<<>>) -> [];
split_into_words(Hex) when byte_size(Hex) >= 64 ->
    <<Word:64/binary, Rest/binary>> = Hex,
    [Word | split_into_words(Rest)];
split_into_words(Hex) ->
    [Hex].

%% @doc Decode a single 32-byte hex word based on ABI type.
decode_value(HexWord, <<"address">>) ->
    Len = byte_size(HexWord),
    AddrHex = case Len >= 40 of
        true -> binary:part(HexWord, Len - 40, 40);
        false -> HexWord
    end,
    <<"0x", (string:lowercase(AddrHex))/binary>>;
decode_value(HexWord, <<"bool">>) ->
    case binary_to_integer(HexWord, 16) of
        0 -> false;
        _ -> true
    end;
decode_value(HexWord, <<"uint", _/binary>>) ->
    integer_to_binary(binary_to_integer(HexWord, 16));
decode_value(HexWord, <<"int", BitsBin/binary>>) ->
    Bits = case BitsBin of
        <<>> -> 256;
        _ -> hb_util:int(BitsBin)
    end,
    Raw = binary_to_integer(HexWord, 16),
    MaxPos = 1 bsl (Bits - 1),
    Signed = case Raw >= MaxPos of
        true -> Raw - (1 bsl Bits);
        false -> Raw
    end,
    integer_to_binary(Signed);
decode_value(HexWord, _Type) ->
    <<"0x", HexWord/binary>>.

%%% Hex conversion helpers

hex_to_int(<<"0x", Hex/binary>>) ->
    binary_to_integer(Hex, 16);
hex_to_int(Bin) when is_binary(Bin) ->
    try binary_to_integer(Bin, 10)
    catch _:_ -> binary_to_integer(Bin, 16)
    end;
hex_to_int(Int) when is_integer(Int) ->
    Int.

int_to_hex(Int) when is_integer(Int) ->
    <<"0x", (integer_to_binary(Int, 16))/binary>>.

ensure_hex(<<"0x", _/binary>> = H) -> H;
ensure_hex(<<"latest">>) -> <<"latest">>;
ensure_hex(<<"pending">>) -> <<"pending">>;
ensure_hex(<<"earliest">>) -> <<"earliest">>;
ensure_hex(Int) when is_integer(Int) -> int_to_hex(Int);
ensure_hex(Bin) when is_binary(Bin) ->
    try int_to_hex(hb_util:int(Bin))
    catch _:_ -> Bin
    end.

%%% Tests

%% Zero-pad a hex string to 64 chars (32 bytes).
pad32(Hex) ->
    Padding = binary:copy(<<"0">>, 64 - byte_size(Hex)),
    <<Padding/binary, Hex/binary>>.

calculate_block_range_test() ->
    Opts = #{ },
    ?assertEqual(
        {ok, <<"0x1F4">>, <<"0x1F4">>},
        calculate_block_range(<<"latest-only">>, #{}, <<"0x1F4">>, #{})
    ),
    ?assertEqual(
        {ok, <<"0x1CC">>, <<"0x1CC">>},
        calculate_block_range(
            <<"incremental">>,
            #{ <<"block-offset">> => <<"40">> },
            <<"0x1F4">>, Opts
        )
    ),
    ?assertEqual(
        {ok, <<"0x191">>, <<"0x1CC">>},
        calculate_block_range(
            <<"incremental">>,
            #{
                <<"block-offset">> => <<"40">>, 
                <<"last-block">> => <<"0x190">>
            },
            <<"0x1F4">>, Opts
        )
    ),
    ?assertEqual(
        {ok, <<"0x1CC">>, <<"0x1CC">>},
        calculate_block_range(
            <<"incremental">>,
            #{
                <<"block-offset">> => <<"40">>, 
                <<"last-block">> => <<"0x1CC">>
            },
            <<"0x1F4">>, Opts
        )
    ),
    ?assertEqual(
        {ok, <<"0x190">>, <<"0x1F4">>},
        calculate_block_range(
            <<"rolling-window">>,
            #{ <<"block-window">> => <<"100">> },
            <<"0x1F4">>, Opts
        )
    ),
    ?assertEqual(
        {ok, <<"0x100">>, <<"0x200">>},
        calculate_block_range(
            <<"range">>,
            #{
                <<"from-block">> => <<"0x100">>,
                <<"to-block">> => <<"0x200">>
            },
            <<"0x1F4">>, Opts
        )
    ),
    ?assertMatch(
        {error, <<"Unknown mode: bad">>},
        calculate_block_range(<<"bad">>, #{}, <<"0x1F4">>, #{})
    ).

parse_rpc_response_test() ->
    Encode = fun(M) -> hb_json:encode(M) end,
    {ok, Result} = 
        parse_rpc_response(
            #{
                <<"body">> => Encode(#{
                    <<"jsonrpc">> => <<"2.0">>, 
                    <<"id">> => 1,
                    <<"result">> => [#{ <<"data">> => <<"0x123">> }]
                })
            }
        ),
    ?assertEqual([#{ <<"data">> => <<"0x123">> }], Result).

decode_value_test() ->
    lists:foreach(
        fun({Hex, Type, Expected}) ->
            ?assertEqual(Expected, decode_value(Hex, Type))
        end, 
        [
            {
                pad32(<<"11b815efb8f581194ae79006d24e0d814b7697f6">>),
                <<"address">>,
                <<"0x11b815efb8f581194ae79006d24e0d814b7697f6">>
            },
            {pad32(<<"3a5da3d3">>), <<"uint256">>, <<"979215315">>},
            {pad32(<<"64">>), <<"int256">>, <<"100">>},
            {binary:copy(<<"f">>, 64), <<"int256">>, <<"-1">>},
            {pad32(<<"1">>), <<"bool">>, true},
            {pad32(<<"0">>), <<"bool">>, false}
        ]
    ).

decode_full_transfer_event_test() ->
    Event = #{
        <<"address">> => <<"0xdac17f958d2ee523a2206206994597c13d831ec7">>,
        <<"blockNumber">> => <<"0x174a1f7">>,
        <<"logIndex">> => <<"0xa">>,
        <<"transactionHash">> => <<"0x95a0ebe99bd1c3bac">>,
        <<"removed">> => false,
        <<"topics">> => [
            <<"0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef">>,
            <<"0x00000000000000000000000011b815efb8f581194ae79006d24e0d814b7697f6">>,
            <<"0x00000000000000000000000051c72848c68a965f66fa7a88855f9f7784502a7f">>
        ],
        <<"data">> =>
            <<"0x000000000000000000000000000000000000000000000000000000003a5da3d3">>
    },
    Decoded = decode_event(
        Event, 
        <<"Transfer">>,
        [<<"address">>, <<"address">>, <<"uint256">>]
    ),
    ?assertMatch(
        #{
            <<"event">> := <<"Transfer">>,
            <<"args">> := [
                <<"0x11b815efb8f581194ae79006d24e0d814b7697f6">>,
                <<"0x51c72848c68a965f66fa7a88855f9f7784502a7f">>,
                <<"979215315">>
            ],
            <<"block-number">> := 24420855,
            <<"log-index">> := 10,
            <<"removed">> := false
        },
        Decoded
    ).

decode_multi_sig_event_test() ->
    TransferSig = <<"Transfer(address,address,uint256)">>,
    ApprovalSig = <<"Approval(address,address,uint256)">>,
    MultiSig = <<TransferSig/binary, ",", ApprovalSig/binary>>,
    Addr1 = <<"0x", (pad32(
        <<"11b815efb8f581194ae79006d24e0d814b7697f6">>))/binary>>,
    Addr2 = <<"0x", (pad32(
        <<"51c72848c68a965f66fa7a88855f9f7784502a7f">>))/binary>>,
    Amount = <<"0x", (pad32(<<"3a5da3d3">>))/binary>>,
    TransferEvent = #{
        <<"topics">> => [keccak256_topic(TransferSig), Addr1, Addr2],
        <<"data">> => Amount,
        <<"blockNumber">> => <<"0x100">>,
        <<"logIndex">> => <<"0x1">>,
        <<"transactionHash">> => <<"0xabc">>,
        <<"removed">> => false
    },
    ApprovalEvent = #{
        <<"topics">> => [keccak256_topic(ApprovalSig), Addr1, Addr2],
        <<"data">> => Amount,
        <<"blockNumber">> => <<"0x101">>,
        <<"logIndex">> => <<"0x2">>,
        <<"transactionHash">> => <<"0xdef">>,
        <<"removed">> => false
    },
    [Decoded1, Decoded2] = decode_events(<<"false">>, MultiSig,
        [TransferEvent, ApprovalEvent]),
    ?assertEqual(<<"Transfer">>, maps:get(<<"event">>, Decoded1)),
    ?assertEqual(<<"Approval">>, maps:get(<<"event">>, Decoded2)),
    ?assertEqual(
        [
            <<"0x11b815efb8f581194ae79006d24e0d814b7697f6">>,
            <<"0x51c72848c68a965f66fa7a88855f9f7784502a7f">>,
            <<"979215315">>
        ],
        maps:get(<<"args">>, Decoded1)
    ),
    ?assertEqual(maps:get(<<"args">>, Decoded1), maps:get(<<"args">>, Decoded2)).
