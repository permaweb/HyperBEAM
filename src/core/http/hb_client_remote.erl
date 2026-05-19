-module(hb_client_remote).
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
resolve(Node, Base, Req, Opts) ->
    TABM2 =
        hb_ao:set(
            #{
                <<"path">> => hb_ao:get(<<"path">>, Req, <<"/">>, Opts),
                <<"2.path">> => unset
            },
        prefix_keys(<<"2.">>, Req, Opts),
        Opts#{ <<"hashpath">> => ignore }
    ),
    hb_http:post(
        Node,
        hb_maps:merge(prefix_keys(<<"1.">>, Base, Opts), TABM2, Opts),
        Opts
    ).

prefix_keys(Prefix, Message, Opts) ->
    hb_maps:fold(
        fun(Key, Val, Acc) ->
            hb_maps:put(<<Prefix/binary, Key/binary>>, Val, Acc, Opts)
        end,
        #{},
        hb_message:convert(Message, tabm, Opts),
		Opts
    ).

routes(Node, Opts) ->
    resolve(Node,
        #{
            <<"device">> => <<"Router@1.0">>
        },
        #{
            <<"path">> => <<"routes">>,
            <<"method">> => <<"GET">>
        },
        Opts
    ).

add_route(Node, Route, Opts) ->
    resolve(Node,
        Route#{
            <<"device">> => <<"Router@1.0">>
        },
        #{
            <<"path">> => <<"routes">>,
            <<"method">> => <<"POST">>
        },
        Opts
    ).


%%% Arweave node API

%% @doc Grab the latest block information from the Arweave gateway node.
arweave_timestamp() ->
    case hb_opts:get(mode) of
        debug -> {0, 0, hb_util:human_id(<<0:256>>)};
        prod ->
            {ok, {{_, 200, _}, _, Body}} =
                httpc:request(
                    <<(hb_opts:get(gateway))/binary, "/block/current">>
                ),
            Fields = hb_json:decode(hb_util:bin(Body)),
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
            hb_message:commitment_devices(Msg, Opts)
        ),
    {ok, UploadResults}.
upload(Msg, Opts, <<"httpsig@1.0">>) ->
    case hb_opts:get(bundler_httpsig, not_found, Opts) of
        not_found ->
            {error, no_httpsig_bundler};
        Bundler ->
            ?event({uploading_item, Msg}),
            hb_http:post(Bundler, <<"/tx">>, Msg, Opts)
    end;
upload(Msg, Opts, _CommitmentDevice) ->
    ?event({uploading_item, Msg}),
    hb_ao:raw(
        <<"arweave@2.9">>,
        <<"tx">>,
        #{},
        Msg#{ <<"method">> => <<"POST">> },
        Opts
    ).

%%% Tests

upload_test_opts() ->
    #{
        <<"bundler-ans104">> => hb_http_server:start_node(#{}),
        <<"priv-wallet">> => hb:wallet()
    }.

upload_empty_message_test() ->
    Opts = upload_test_opts(),
    Msg = #{ <<"data">> => <<"TEST">> },
    Committed =
        hb_message:commit(
            Msg,
            Opts,
            <<"ans104@1.0">>
        ),
    Result = upload(Committed, Opts, <<"ans104@1.0">>),
    ?event({upload_result, Result}),
    ?assertMatch({ok, _}, Result).

upload_single_layer_message_test() ->
    Opts = upload_test_opts(),
    Msg = #{
        <<"data">> => <<"TEST">>,
        <<"basic">> => <<"value">>,
        <<"integer">> => 1
    },
    Committed =
        hb_message:commit(
            Msg,
            Opts,
            <<"ans104@1.0">>
        ),
    Result = upload(Committed, Opts, <<"ans104@1.0">>),
    ?event({upload_result, Result}),
    ?assertMatch({ok, _}, Result).
