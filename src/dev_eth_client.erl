%%% @doc A stateless device for interacting with Ethereum via JSON-RPC.
%%%
%%% Provides three keys:
%%% - `get-data': Fetch event logs (`eth_getLogs') with block range modes
%%% - `call': Execute read-only contract calls (`eth_call') with ABI
%%%   encoding/decoding
%%% - `rpc': Generic JSON-RPC passthrough for any `eth_*' method
%%%
%%% Every request is self-contained — the caller passes all parameters
%%% (RPC URL, contract, function, etc.) in each call. No session state,
%%% no cookies, no configure step.
%%%
%%% The device is RPC-provider agnostic. It speaks standard Ethereum
%%% JSON-RPC and works with Alchemy, Moralis, Infura, public RPCs, or
%%% any endpoint that implements the Ethereum JSON-RPC spec.
%%%
%%% Usage:
%%% <pre>
%%%     curl /~eth-client@1.0/get-data&amp;rpc-url=...&amp;contract=0xABC
%%%     curl /~eth-client@1.0/call&amp;rpc-url=...&amp;to=0xABC&amp;function=totalSupply()&amp;returns=uint256
%%%     curl /~eth-client@1.0/rpc&amp;rpc-url=...&amp;rpc-method=eth_blockNumber
%%% </pre>
-module(dev_eth_client).
-export([info/1, get_data/3, call/3, rpc/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_OFFSET, 40).

%%% Exported keys

%% @doc Device metadata — exports `get-data', `call', and `rpc'.
info(_Base) ->
    #{ exports => [<<"get-data">>, <<"call">>, <<"rpc">>] }.

%% @doc Execute `eth_call' with ABI encoding/decoding.
%% Required: `rpc-url', `to', `function' (e.g. "balanceOf(address)").
%% Optional: `args' (comma-separated), `returns' (e.g. "uint256"),
%%           `block' (default "latest"), `from', `value', `gas'.
call(Base, Req, Opts) ->
    maybe
        {ok, RpcConf} ?= parse_rpc_config(Base, Req, Opts),
        {ok, To} ?= require(<<"to">>, Base, Req, Opts),
        {ok, FunctionSig} ?= require(<<"function">>, Base, Req, Opts),
        Args = parse_args(require(<<"args">>, Base, Req, <<>>, Opts)),
        Block = require(<<"block">>, Base, Req, <<"latest">>, Opts),
        ReturnsSpec = require(<<"returns">>, Base, Req, not_found, Opts),
        CallData = encode_call_data(FunctionSig, Args),
        CallObj = build_call_object(To, CallData, Req, Opts),
        ?event({eth_call,
            {function, FunctionSig}, {to, To}, {data, CallData}
        }),
        {ok, {ResolvedBlock, BlockNumber}} ?=
            case Block of
                <<"latest">> ->
                    case do_rpc(RpcConf, <<"eth_blockNumber">>, [], Opts) of
                        {ok, CB} -> {ok, {CB, hex_to_int(CB)}};
                        {error, Reason2} -> {error, Reason2}
                    end;
                _ ->
                    {ok, {Block, hex_to_int(Block)}}
            end,
        {ok, Result} ?=
            do_rpc(RpcConf, <<"eth_call">>, [CallObj, ResolvedBlock], Opts),
        DecodedResult = maybe_decode(Result, ReturnsSpec),
        {ok, #{
            <<"body">> => #{
                <<"data">> => format_result(DecodedResult),
                <<"raw">> => Result,
                <<"block">> => ResolvedBlock,
                <<"block-number">> => BlockNumber
            },
            <<"status">> => 200
        }}
    else
        {error, Reason} -> {error, #{
            <<"body">> => #{ <<"error">> => Reason },
            <<"status">> => 400
        }}
    end.

%% @doc Generic JSON-RPC passthrough — any `eth_*' method.
%% Required params: `rpc-url', `rpc-method'.
%% Optional: `rpc-params' (JSON array, default `"[]"').
rpc(Base, Req, Opts) ->
    maybe
        {ok, RpcConf} ?= parse_rpc_config(Base, Req, Opts),
        {ok, Method} ?= require(<<"rpc-method">>, Base, Req, Opts),
        Params = parse_json_params(
            require(<<"rpc-params">>, Base, Req, <<"[]">>, Opts)
        ),
        ?event({eth_rpc, {method, Method}, {params, Params}}),
        {ok, Result} ?= do_rpc(RpcConf, Method, Params, Opts),
        {ok, #{
            <<"body">> => #{ <<"data">> => format_result(Result) },
            <<"status">> => 200
        }}
    else
        {error, Reason} -> {error, #{
            <<"body">> => #{ <<"error">> => Reason },
            <<"status">> => 400
        }}
    end.

%% @doc Fetch Ethereum contract event logs.
%% Required params: `rpc-url', `contract'.
%% Optional: `api-key', `api-key-header', `event-signature', `mode', `raw'.
%% Modes: `latest-only' (default), `incremental', `rolling-window', `range'.
get_data(Base, Req, Opts) ->
    maybe
        {ok, RpcConf, Contract, EventSig, Mode, Raw} ?=
            parse_params(Base, Req, Opts),
        ?event(
            {eth_get_data,
                {mode, Mode},
                {contract, Contract},
                {event, EventSig}
            }
        ),
        {ok, FromBlock, ToBlock, CurrentBlock} ?=
            resolve_block_range(Mode, Base, Req, RpcConf, Opts),
        FilterParams =
            build_filter(Contract, EventSig, FromBlock, ToBlock),
        {ok, Events} ?=
            do_rpc(RpcConf, <<"eth_getLogs">>, [FilterParams], Opts),
        Decoded = decode_events(Raw, EventSig, Events),
        {ok, build_response(
            Decoded, FromBlock, ToBlock, CurrentBlock, Contract
        )}
    else
        {error, Reason} -> {error, Reason}
    end.

%%% Parameter parsing
require(Key, Base, Req, Opts) ->
    case require(Key, Base, Req, not_found, Opts) of
        not_found -> {error, <<Key/binary, " parameter required.">>};
        Value -> {ok, Value}
    end.
require(Key, Base, Req, Default, Opts) ->
    hb_ao:get_first([{Req, Key}, {Base, Key}], Default, Opts).

parse_args(<<>>) -> [];
parse_args(not_found) -> [];
parse_args(Args) when is_binary(Args) ->
    binary:split(Args, <<",">>, [global]);
parse_args(Args) when is_list(Args) -> Args.

parse_rpc_config(Base, Req, Opts) ->
    maybe
        {ok, RpcUrl} ?= require(<<"rpc-url">>, Base, Req, Opts),
        {ok, #{
            url => RpcUrl,
            api_key => require(<<"api-key">>, Base, Req, <<>>, Opts),
            api_key_header => require(
                <<"api-key-header">>, Base, Req, <<"x-api-key">>, Opts
            )
        }}
    else
        {error, Reason} -> {error, Reason}
    end.

parse_params(Base, Req, Opts) ->
    maybe
        {ok, RpcConf} ?= parse_rpc_config(Base, Req, Opts),
        {ok, Contract} ?= require(<<"contract">>, Base, Req, Opts),
        {
            ok,
            RpcConf,
            Contract,
            require(<<"event-signature">>, Base, Req, not_found, Opts),
            require(<<"mode">>, Base, Req, <<"latest-only">>, Opts),
            require(<<"raw">>, Base, Req, <<"false">>, Opts)
        }
    else
        {error, Reason} -> {error, Reason}
    end.

parse_json_params(Bin) when is_binary(Bin) ->
    try hb_json:decode(Bin) of
        List when is_list(List) -> List;
        _ -> []
    catch
        _:_ -> []
    end;
parse_json_params(List) when is_list(List) -> List;
parse_json_params(_) -> [].

%%% RPC communication
do_rpc(RpcConfig, Method, Params, Opts) ->
    case send_rpc(RpcConfig, Method, Params, Opts) of
        {ok, RelayResponse} ->
            parse_rpc_response(RelayResponse, Opts);
        {error, Reason} ->
            {error, Reason}
    end.

send_rpc(
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
    ?event({eth_rpc_call, {method, Method}, {body, {string, Body}}}),
    BaseRequest = #{
        <<"device">> => <<"relay@1.0">>,
        <<"method">> => <<"POST">>,
        <<"path">> => Url,
        <<"content-type">> => <<"application/json">>,
        <<"body">> => Body
    },
    Request =
        case ApiKey of
            <<>> -> BaseRequest;
            _ -> BaseRequest#{ ApiKeyHeader => ApiKey }
        end,
    hb_ao:resolve(Request, <<"call">>, Opts).

parse_rpc_response(RelayResponse, Opts) when is_map(RelayResponse) ->
    case hb_maps:get(<<"body">>, RelayResponse, <<>>, Opts) of
        <<>> ->
            {error, <<"Empty response body from RPC.">>};
        Body when is_binary(Body) ->
            try hb_json:decode(Body) of
                Decoded -> extract_result(Decoded)
            catch
                _:_ -> {error, <<"Invalid JSON in RPC response.">>}
            end;
        Body when is_map(Body) ->
            extract_result(Body)
    end.

extract_result(#{<<"result">> := Result}) ->
    {ok, Result};
extract_result(#{<<"error">> := #{<<"message">> := Msg}}) ->
    {error, Msg};
extract_result(#{<<"error">> := Error}) ->
    {error, Error};
extract_result(_) ->
    {error, <<"Unexpected JSON-RPC response.">>}.

%%% Block range resolution
resolve_block_range(Mode, Base, Req, RpcConfig, Opts) ->
    maybe
        {ok, CurrentBlock} ?=
            do_rpc(RpcConfig, <<"eth_blockNumber">>, [], Opts),
        {ok, FromBlock, ToBlock} ?= calculate_block_range(
            Mode, Base, Req, CurrentBlock, Opts
        ),
        {ok, FromBlock, ToBlock, CurrentBlock}
    else
        {error, Reason} -> {error, Reason}
    end.

calculate_block_range(<<"latest-only">>, _Base, _Req, CurrentBlock, _Opts) ->
    {ok, CurrentBlock, CurrentBlock};
calculate_block_range(<<"range">>, Base, Req, _CurrentBlock, Opts) ->
    maybe
        {ok, FromBlock} ?= require(<<"from-block">>, Base, Req, Opts),
        {ok, ToBlock} ?= require(<<"to-block">>, Base, Req, Opts),
        {ok, ensure_hex(FromBlock), ensure_hex(ToBlock)}
    else
        {error, Reason} -> {error, Reason}
    end;
calculate_block_range(<<"incremental">>, Base, Req, CurrentBlock, Opts) ->
    Offset = require(<<"block-offset">>, Base, Req, ?DEFAULT_OFFSET, Opts),
    CurrentBlockInt = hex_to_int(CurrentBlock),
    TargetBlockInt = max(0, CurrentBlockInt - hb_util:int(Offset)),
    FromBlockInt =
        case require(<<"last-block">>, Base, Req, not_found, Opts) of
            not_found -> TargetBlockInt;
            LastBlock -> min(hex_to_int(LastBlock) + 1, TargetBlockInt)
        end,
    {ok, int_to_hex(FromBlockInt), int_to_hex(TargetBlockInt)};
calculate_block_range(<<"rolling-window">>, Base, Req, CurrentBlock, Opts) ->
    Window = require(<<"block-window">>, Base, Req, ?DEFAULT_OFFSET, Opts),
    CurrentBlockInt = hex_to_int(CurrentBlock),
    FromBlock = max(0, CurrentBlockInt - hb_util:int(Window)),
    {ok, int_to_hex(FromBlock), CurrentBlock};
calculate_block_range(Mode, _Base, _Req, _CurrentBlock, _Opts) ->
    {error, <<"Unknown mode: ", Mode/binary>>}.

%%% Event log filtering
build_filter(Contract, EventSig, FromBlock, ToBlock) ->
    Base = #{
        <<"fromBlock">> => ensure_hex(FromBlock),
        <<"toBlock">> => ensure_hex(ToBlock),
        <<"address">> => Contract
    },
    case EventSig of
        not_found -> Base;
        <<>> -> Base;
        _ ->
            Sigs = split_event_sigs(EventSig),
            Hashes = [keccak256_topic(S) || S <- Sigs],
            Topics = case Hashes of
                [Single] -> [Single];
                Multiple -> [Multiple]
            end,
            Base#{ <<"topics">> => Topics }
    end.

decode_events(<<"true">>, _EventSig, Events) ->
    Events;
decode_events(_, NoSig, Events) when NoSig =:= not_found; NoSig =:= <<>> ->
    [decode_event_metadata(E) || E <- Events];
decode_events(_, EventSig, Events) ->
    Sigs = split_event_sigs(EventSig),
    SigMap = maps:from_list([
        begin
            {Name, Types} = parse_event_signature(Sig),
            Hash = keccak256_topic(Sig),
            {Hash, {Name, Types}}
        end
        || Sig <- Sigs
    ]),
    [decode_event_full(E, SigMap) || E <- Events].

%% @doc Fully decode an event by matching its topic[0] against a signature map.
%% Falls back to metadata-only decoding if no matching signature is found.
decode_event_full(Event, SigMap) when is_map(Event) ->
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
        fun(<<"0x", Hex/binary>>, Type) -> decode_value(Hex, Type) end,
        IndexedTopics,
        IndexedTypes
    ),
    DataArgs = decode_data_params(Data, DataTypes),
    (event_metadata(Event))#{
        <<"event">> => Name,
        <<"args">> => IndexedArgs ++ DataArgs
    }.

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

decode_event_metadata(Event) when is_map(Event) ->
    (event_metadata(Event))#{
        <<"address">> => hb_maps:get(<<"address">>, Event, <<>>),
        <<"topics">> => hb_maps:get(<<"topics">>, Event, []),
        <<"data">> => hb_maps:get(<<"data">>, Event, <<>>)
    }.

build_response(Events, FromBlock, ToBlock, CurrentBlock, Contract) ->
    #{
        <<"body">> => #{
            <<"data">> => hb_json:encode(Events),
            <<"from-block">> => FromBlock,
            <<"from-block-number">> => hex_to_int(FromBlock),
            <<"to-block">> => ToBlock,
            <<"to-block-number">> => hex_to_int(ToBlock),
            <<"latest-block">> => CurrentBlock,
            <<"latest-block-number">> => hex_to_int(CurrentBlock),
            <<"contract">> => Contract
        },
        <<"status">> => 200
    }.

%%% ABI encoding
encode_call_data(FunctionSig, Args) ->
    Selector = function_selector(FunctionSig),
    {_Name, Types} = parse_event_signature(FunctionSig),
    EncodedArgs = [
        abi_encode_value(hb_util:bin(Arg), Type)
        || {Arg, Type} <- lists:zip(Args, Types)
    ],
    <<"0x", Selector/binary, (iolist_to_binary(EncodedArgs))/binary>>.

function_selector(FunctionSig) ->
    Hash = hb_keccak:keccak_256(FunctionSig),
    <<First4:4/binary, _/binary>> = Hash,
    hb_util:to_hex(First4).

abi_encode_value(Value, <<"address">>) ->
    left_pad_hex(strip_0x(Value), 64);
abi_encode_value(Value, <<"bool">>) ->
    case Value of
        <<"true">> -> left_pad_hex(<<"1">>, 64);
        <<"false">> -> left_pad_hex(<<"0">>, 64);
        <<"1">> -> left_pad_hex(<<"1">>, 64);
        _ -> left_pad_hex(<<"0">>, 64)
    end;
abi_encode_value(Value, <<"uint", _/binary>>) ->
    Int = hb_util:int(Value),
    left_pad_hex(integer_to_binary(Int, 16), 64);
abi_encode_value(Value, <<"int", _/binary>>) ->
    Int = hb_util:int(Value),
    case Int >= 0 of
        true ->
            left_pad_hex(integer_to_binary(Int, 16), 64);
        false ->
            TwosComp = (1 bsl 256) + Int,
            left_pad_hex(integer_to_binary(TwosComp, 16), 64)
    end;
abi_encode_value(Value, <<"bytes32">>) ->
    right_pad_hex(strip_0x(Value), 64);
abi_encode_value(Value, _Type) ->
    left_pad_hex(strip_0x(Value), 64).

build_call_object(To, Data, Req, Opts) ->
    Base = #{
        <<"to">> => To,
        <<"data">> => Data
    },
    lists:foldl(
        fun({Key, Field}, Acc) ->
            case hb_ao:get(Key, Req, not_found, Opts) of
                not_found -> Acc;
                Val -> Acc#{ Field => Val }
            end
        end,
        Base,
        [
            {<<"from">>, <<"from">>},
            {<<"value">>, <<"value">>},
            {<<"gas">>, <<"gas">>}
        ]
    ).

%%% ABI decoding
maybe_decode(Result, not_found) -> Result;
maybe_decode(Result, <<>>) -> Result;
maybe_decode(Result, ReturnsSpec) ->
    decode_call_result(Result, hb_util:bin(ReturnsSpec)).

decode_call_result(<<"0x", HexData/binary>>, <<"string">>) ->
    decode_abi_string(HexData);
decode_call_result(<<"0x", HexData/binary>>, <<"bytes">>) ->
    <<"0x", (decode_abi_dynamic_bytes(HexData))/binary>>;
decode_call_result(<<"0x", HexData/binary>>, ReturnsSpec) ->
    Types = binary:split(hb_util:bin(ReturnsSpec), <<",">>, [global]),
    Words = split_words(HexData),
    Len = min(length(Words), length(Types)),
    Results = lists:zipwith(
        fun(Word, Type) -> decode_value(Word, Type) end,
        lists:sublist(Words, Len),
        lists:sublist(Types, Len)
    ),
    case Results of
        [Single] -> Single;
        Multiple -> Multiple
    end;
decode_call_result(Result, _ReturnsSpec) ->
    Result.

decode_abi_string(HexData) ->
    hex_to_bin(decode_abi_dynamic_bytes(HexData)).

decode_abi_dynamic_bytes(HexData) ->
    Words = split_words(HexData),
    case Words of
        [_Offset, LenHex | DataWords] ->
            Len = binary_to_integer(LenHex, 16),
            DataHex = iolist_to_binary(DataWords),
            HexBytes = min(Len * 2, byte_size(DataHex)),
            binary:part(DataHex, 0, HexBytes);
        _ ->
            HexData
    end.

decode_data_params(<<"0x">>, _Types) -> [];
decode_data_params(<<"0x", HexData/binary>>, Types) ->
    Words = split_words(HexData),
    Len = min(length(Words), length(Types)),
    lists:zipwith(
        fun(Word, Type) -> decode_value(Word, Type) end,
        lists:sublist(Words, Len),
        lists:sublist(Types, Len)
    );
decode_data_params(_, _Types) -> [].

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

%%% Helpers
keccak256_topic(EventSig) ->
    Hash = hb_keccak:keccak_256(EventSig),
    <<"0x", (hb_util:to_hex(Hash))/binary>>.

split_event_sigs(EventSig) ->
    Parts = binary:split(EventSig, <<"),">>, [global]),
    restore_parens(Parts).

restore_parens([Last]) -> [Last];
restore_parens([H | T]) ->
    [<<H/binary, ")">> | restore_parens(T)].

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

split_words(<<>>) -> [];
split_words(Hex) when byte_size(Hex) >= 64 ->
    <<Word:64/binary, Rest/binary>> = Hex,
    [Word | split_words(Rest)];
split_words(Hex) ->
    [Hex].

format_result(R) when is_binary(R) -> R;
format_result(R) when is_integer(R) -> integer_to_binary(R);
format_result(R) -> hb_json:encode(R).

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

strip_0x(<<"0x", Hex/binary>>) -> Hex;
strip_0x(Hex) -> Hex.

left_pad_hex(Hex, Len) ->
    HexLower = string:lowercase(Hex),
    Pad = max(0, Len - byte_size(HexLower)),
    <<(binary:copy(<<"0">>, Pad))/binary, HexLower/binary>>.

right_pad_hex(Hex, Len) ->
    HexLower = string:lowercase(Hex),
    Pad = max(0, Len - byte_size(HexLower)),
    <<HexLower/binary, (binary:copy(<<"0">>, Pad))/binary>>.

hex_to_bin(<<>>) -> <<>>;
hex_to_bin(<<H:2/binary, Rest/binary>>) ->
    Byte = binary_to_integer(H, 16),
    <<Byte, (hex_to_bin(Rest))/binary>>.

%%% Tests
%%
%% Integration tests against public Ethereum contracts.
%% Uses public RPC and well-known mainnet contracts.

-define(RPC_URL, <<"https://eth.merkle.io">>).
%% Lido stETH
-define(STETH, <<"0xae7ab96520DE3A18E5e111B5EaAb095312D7fE84">>).
%% Our deployed Greeter contract
-define(GREETER, <<"0x7f0776D52a02FB6BBBA7099f3e62347BE9630032">>).

rpc_block_number_test() ->
    Req = #{
        <<"rpc-url">> => ?RPC_URL,
        <<"rpc-method">> => <<"eth_blockNumber">>
    },
    {ok, Resp} = rpc(#{}, Req, #{}),
    Result = hb_maps:get(<<"data">>, hb_maps:get(<<"body">>, Resp)),
    ?assertMatch(<<"0x", _/binary>>, Result),
    ?assertEqual(200, hb_maps:get(<<"status">>, Resp)).

rpc_missing_method_test() ->
    Req = #{ <<"rpc-url">> => ?RPC_URL },
    {error, Resp} = rpc(#{}, Req, #{}),
    ?assertEqual(400, hb_maps:get(<<"status">>, Resp)).

call_total_supply_test() ->
    Req = #{
        <<"rpc-url">> => ?RPC_URL,
        <<"to">> => ?STETH,
        <<"function">> => <<"totalSupply()">>,
        <<"returns">> => <<"uint256">>
    },
    {ok, Resp} = call(#{}, Req, #{}),
    Body = hb_maps:get(<<"body">>, Resp),
    Result = hb_maps:get(<<"data">>, Body),
    ?assert(is_binary(Result)),
    ?assert(byte_size(Result) > 10),
    Raw = hb_maps:get(<<"raw">>, Body),
    ?assertMatch(<<"0x", _/binary>>, Raw).

call_greet_test() ->
    Req = #{
        <<"rpc-url">> => ?RPC_URL,
        <<"to">> => ?GREETER,
        <<"function">> => <<"greet()">>,
        <<"returns">> => <<"string">>
    },
    {ok, Resp} = call(#{}, Req, #{}),
    Body = hb_maps:get(<<"body">>, Resp),
    ?assertEqual(<<"Hello HB! from ETH">>, hb_maps:get(<<"data">>, Body)).

call_missing_to_test() ->
    Req = #{
        <<"rpc-url">> => ?RPC_URL,
        <<"function">> => <<"totalSupply()">>
    },
    {error, Resp} = call(#{}, Req, #{}),
    ?assertEqual(400, hb_maps:get(<<"status">>, Resp)).

call_base_fallback_test() ->
    Base = #{
        <<"rpc-url">> => ?RPC_URL,
        <<"to">> => ?GREETER,
        <<"returns">> => <<"string">>
    },
    Req = #{ <<"function">> => <<"greet()">> },
    {ok, Resp} = call(Base, Req, #{}),
    Body = hb_maps:get(<<"body">>, Resp),
    ?assertEqual(<<"Hello HB! from ETH">>, hb_maps:get(<<"data">>, Body)).

get_data_latest_test() ->
    Req = #{
        <<"rpc-url">> => ?RPC_URL,
        <<"contract">> => ?STETH,
        <<"mode">> => <<"latest-only">>
    },
    {ok, Resp} = get_data(#{}, Req, #{}),
    Body = hb_maps:get(<<"body">>, Resp),
    ?assertEqual(200, hb_maps:get(<<"status">>, Resp)),
    ?assert(is_integer(hb_maps:get(<<"latest-block-number">>, Body))),
    ?assertEqual(?STETH, hb_maps:get(<<"contract">>, Body)).
