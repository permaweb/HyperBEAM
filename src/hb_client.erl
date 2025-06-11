-module(hb_client).
%% AO-Core API and HyperBEAM Built-In Devices
-export([resolve/4, routes/2, add_route/3]).
%% Arweave node API
-export([arweave_timestamp/0]).
%% Arweave bundling and data access API
-export([upload/2, upload/3]).
%% Tests
-include_lib("eunit/include/eunit.hrl").

-include("include/hb.hrl").

%%% AO-Core API and HyperBEAM Built-In Devices


%% @doc Resolve a message pair on a remote node.
%% The message pair is first transformed into a singleton request, by
%% prefixing the keys in both messages for the path segment that they relate to,
%% and then adjusting the "Path" field from the second message.
resolve(Node, Msg1, Msg2, Opts) ->
    TABM2 =
        hb_ao:set(
          #{
            <<"path">> => hb_ao:get(<<"path">>, Msg2, <<"/">>, Opts),
            <<"2.path">> => unset
           },
          prefix_keys(<<"2.">>, Msg2, Opts),
          Opts#{hashpath => ignore}),
    hb_http:post(
      Node,
      hb_maps:merge(prefix_keys(<<"1.">>, Msg1, Opts), TABM2, Opts),
      Opts).


prefix_keys(Prefix, Message, Opts) ->
    hb_maps:fold(
      fun(Key, Val, Acc) ->
              hb_maps:put(<<Prefix/binary, Key/binary>>, Val, Acc, Opts)
      end,
      #{},
      hb_message:convert(Message, tabm, Opts),
      Opts).


routes(Node, Opts) ->
    resolve(Node,
            #{
              <<"device">> => <<"Router@1.0">>
             },
            #{
              <<"path">> => <<"routes">>,
              <<"method">> => <<"GET">>
             },
            Opts).


add_route(Node, Route, Opts) ->
    resolve(Node,
            Route#{
              <<"device">> => <<"Router@1.0">>
             },
            #{
              <<"path">> => <<"routes">>,
              <<"method">> => <<"POST">>
             },
            Opts).


%%% Arweave node API


%% @doc Grab the latest block information from the Arweave gateway node.
arweave_timestamp() ->
    case hb_opts:get(mode) of
        debug -> {0, 0, <<0:256>>};
        prod ->
            {ok, {{_, 200, _}, _, Body}} =
                httpc:request(
                  <<(hb_opts:get(gateway))/binary, "/block/current">>),
            Fields = hb_json:decode(Body),
            Timestamp = hb_maps:get(<<"timestamp">>, Fields),
            Hash = hb_maps:get(<<"indep_hash">>, Fields),
            Height = hb_maps:get(<<"height">>, Fields),
            {Timestamp, Height, Hash}
    end.


%%% Bundling and data access API


%% @doc Upload a data item to the bundler node.
%% Note: Uploads once per commitment device. Callers should filter the
%% commitments to only include the ones they are interested in, if this is not
%% the desired behavior.
upload(Msg, Opts) ->
    UploadResults =
        lists:map(
          fun(Device) ->
                  upload(Msg, Opts, Device)
          end,
          hb_message:commitment_devices(Msg, Opts)),
    {ok, UploadResults}.


upload(Msg, Opts, <<"httpsig@1.0">>) ->
    case hb_opts:get(bundler_httpsig, not_found, Opts) of
        not_found ->
            {error, no_httpsig_bundler};
        Bundler ->
            ?event({uploading_item, Msg}),
            hb_http:post(Bundler, <<"/tx">>, Msg, Opts)
    end;
upload(Msg, Opts, <<"ans104@1.0">>) when is_map(Msg) ->
    ?event({msg_to_convert, Msg}),
    Converted = hb_message:convert(Msg, <<"ans104@1.0">>, Opts),
    ?event({msg_to_tx_res, {converted, Converted}}),
    Serialized = ar_bundles:serialize(Converted),
    ?event({converted_msg_to_tx, Serialized}),
    upload(Serialized, Opts, <<"ans104@1.0">>);
upload(Serialized, Opts, <<"ans104@1.0">>) when is_binary(Serialized) ->
    ?event({uploading_item, Serialized}),
    hb_http:post(
      hb_opts:get(bundler_ans104, not_found, Opts),
      #{
        <<"path">> => <<"/tx">>,
        <<"content-type">> => <<"application/octet-stream">>,
        <<"body">> => Serialized
       },
      Opts#{
        http_client =>
            hb_opts:get(bundler_ans104_http_client, httpc, Opts)
       }).


%%% Tests


upload_empty_raw_ans104_test() ->
    Serialized = ar_bundles:serialize(
                   ar_bundles:sign_item(#tx{
                                          data = <<"TEST">>
                                         },
                                        hb:wallet())),
    ?event({uploading_item, Serialized}),
    Result = upload(Serialized, #{}, <<"ans104@1.0">>),
    ?event({upload_result, Result}),
    ?assertMatch({ok, _}, Result).


upload_raw_ans104_test() ->
    Serialized = ar_bundles:serialize(
                   ar_bundles:sign_item(#tx{
                                          data = <<"TEST">>,
                                          tags = [{<<"test-tag">>, <<"test-value">>}]
                                         },
                                        hb:wallet())),
    ?event({uploading_item, Serialized}),
    Result = upload(Serialized, #{}, <<"ans104@1.0">>),
    ?event({upload_result, Result}),
    ?assertMatch({ok, _}, Result).


upload_raw_ans104_with_anchor_test() ->
    Serialized = ar_bundles:serialize(
                   ar_bundles:sign_item(#tx{
                                          data = <<"TEST">>,
                                          last_tx = crypto:strong_rand_bytes(32),
                                          tags = [{<<"test-tag">>, <<"test-value">>}]
                                         },
                                        hb:wallet())),
    ?event({uploading_item, Serialized}),
    Result = upload(Serialized, #{}, <<"ans104@1.0">>),
    ?event({upload_result, Result}),
    ?assertMatch({ok, _}, Result).


upload_empty_message_test() ->
    Msg = #{<<"data">> => <<"TEST">>},
    Committed = hb_message:commit(Msg, hb:wallet(), <<"ans104@1.0">>),
    Result = upload(Committed, #{}, <<"ans104@1.0">>),
    ?event({upload_result, Result}),
    ?assertMatch({ok, _}, Result).


upload_single_layer_message_test() ->
    Msg = #{
            <<"data">> => <<"TEST">>,
            <<"basic">> => <<"value">>,
            <<"integer">> => 1
           },
    Committed = hb_message:commit(Msg, hb:wallet(), <<"ans104@1.0">>),
    Result = upload(Committed, #{}, <<"ans104@1.0">>),
    ?event({upload_result, Result}),
    ?assertMatch({ok, _}, Result).
