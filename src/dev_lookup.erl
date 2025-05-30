%%% @doc A device that looks up an ID from a local store and returns it, honoring
%%% the `accept' key to return the correct format.
-module(dev_lookup).
-export([read/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% @doc Fetch a resource from the cache using "target" ID extracted from the message
read(_M1, M2, Opts) ->
    ID = hb_ao:get(<<"target">>, M2, Opts),
    ?event({lookup, {id, ID}, {opts, Opts}}),
    case hb_cache:read(ID, Opts) of
        {ok, RawRes} ->
            % We are sending the result over the wire, so make sure it is
            % fully loaded, to save the recipient latency.
            Res = hb_cache:ensure_all_loaded(RawRes),
            ?event({lookup_result, Res}),
            case hb_ao:get(<<"accept">>, M2, Opts) of
                <<"application/aos-2">> ->
                    Struct = dev_json_iface:message_to_json_struct(Res, Opts),
                    {ok,
                        #{
                            <<"body">> => hb_json:encode(Struct),
                            <<"content-type">> => <<"application/aos-2">>
                        }};
                _ ->
                    {ok, Res}
            end;
        not_found ->
            ?event({lookup_not_found, ID}),
            {error, not_found}
    end.

%%% Tests

binary_lookup_test() ->
    Bin = <<"Simple unsigned data item">>,
    {ok, ID} = hb_cache:write(Bin, #{}),
    {ok, RetrievedBin} = read(#{}, #{ <<"target">> => ID }, #{}),
    ?assertEqual(Bin, RetrievedBin).

message_lookup_test() ->
    Msg = #{ <<"test-key">> => <<"test-value">>, <<"data">> => <<"test-data">> },
    {ok, ID} = hb_cache:write(Msg, #{}),
    {ok, RetrievedMsg} = read(#{}, #{ <<"target">> => ID }, #{}),
    ?assertEqual(Msg, RetrievedMsg).

aos2_message_lookup_test() ->
    Msg = #{ <<"test-key">> => <<"test-value">>, <<"data">> => <<"test-data">> },
    {ok, ID} = hb_cache:write(Msg, #{}),
    {ok, RetrievedMsg} =
        read(
            #{},
            #{ <<"target">> => ID, <<"accept">> => <<"application/aos-2">> },
            #{}
        ),
    Decoded = hb_json:decode(hb_ao:get(<<"body">>, RetrievedMsg, #{})),
    ?assertEqual(<<"test-data">>, hb_ao:get(<<"data">>, Decoded, #{})).

http_lookup_test() ->
    Store = #{
        <<"store-module">> => hb_store_fs,
        <<"prefix">> => <<"cache-mainnet">>
    },
    Opts = #{ store => [Store] },
    Msg = #{ <<"test-key">> => <<"test-value">>, <<"data">> => <<"test-data">> },
    {ok, ID} = hb_cache:write(Msg, Opts),
    Node = hb_http_server:start_node(Opts),
    Wallet = hb:wallet(),
    Req = hb_message:commit(#{
        <<"path">> => <<"/~lookup@1.0/read?target=", ID/binary>>,
        <<"device">> => <<"lookup@1.0">>,
        <<"accept">> => <<"application/aos-2">>
    }, Wallet),
    {ok, Res} = hb_http:post(Node, Req, Opts),
    Decoded = hb_json:decode(hb_ao:get(<<"body">>, Res, Opts)),
    ?assertEqual(<<"test-data">>, hb_ao:get(<<"data">>, Decoded, Opts)).