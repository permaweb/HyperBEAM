%%% @doc A device that offers a bundling service for HyperBEAM users and other
%%% devices/nodes.
%%%
%%% The role of a bundler in the Arweave ecosystem is to create a single nested
%%% transaction that contains multiple data items. Because an extremely large
%%% number of items can be written to the network using only one transaction
%%% (max 2^256 bytes of combined data and headers), they allow the network to
%%% scale to without practical limits.
%%%
%%% When users post to the `~bundler@1.0' device, their request is written to
%%% the node's internal cache, and added to a queue of requests to be bundled.
%%% Once the queue reaches the node-operator's desired size, it is automatically
%%% bundled into one transaction, signed and dispatched to the network. Writing
%%% the message to the cache before transmission ensures that the message is
%%% available for reading instantly (`optimistically'), even before the
%%% transaction is dispatched.
-module(dev_bundler).
-export([tx/3, item/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% Default options.
-define(SERVER_NAME, bundler_server).
-define(DEFAULT_MAX_SIZE, 100_000_000). % 100 MB.
-define(DEFAULT_MAX_IDLE_TIME, 300_000). % 5 minutes.
-define(DEFAULT_MAX_ITEMS, 1000).

%%% Public interface.

%% @doc An alias for `item/3'.
tx(Base, Req, Opts) ->
    item(Base, Req, Opts).

%% @doc Implements an Arweave/`up.arweave.net'-compatible endpoint for
%% bundling messages. 
item(Base, Req, Opts) ->
    PID = ensure_server(Opts),
    PID ! {item, self(), Ref = make_ref(), Base, Req},
    receive
        {response, Ref, Res} -> Res
    end.

%%% Bundling server.

%% @doc Return the PID of the bundler server. If the server is not running,
%% it is started and registered with the name `?SERVER_NAME'.
ensure_server(Opts) ->
    case hb_name:lookup(?SERVER_NAME) of
        undefined ->
            PID = spawn(fun() -> init(Opts) end),
            hb_name:register(?SERVER_NAME, PID),
            hb_name:lookup(?SERVER_NAME);
        PID -> PID
    end.

%% @doc Initialize the bundler server.
init(Opts) ->
    server(
        #{
            max_size => hb_opts:get(bundler_max_size, ?DEFAULT_MAX_SIZE, Opts),
            max_idle_time => hb_opts:get(bundler_max_idle_time, ?DEFAULT_MAX_IDLE_TIME, Opts),
            max_items => hb_opts:get(bundler_max_items, ?DEFAULT_MAX_ITEMS, Opts),
            queue => [],
            bytes => 0
        },
        Opts
    ).

%% @doc The main loop of the bundler server. Simply waits for messages to be
%% added to the queue, and then dispatches them when the queue is large enough.
server(State = #{ max_idle_time := MaxIdleTime }, Opts) ->
    receive
        {item, From, Ref, _Base, Req} ->
            From ! {response, Ref, {ok, <<"Message queued.">>}},
            server(maybe_dispatch(add_item(Req, State, Opts), Opts), Opts);
        stop ->
            exit(normal)
    after MaxIdleTime ->
        server(dispatch(State, Opts), Opts)
    end.

%% @doc Add an item to the queue. Update the state with the new queue and
%% approximate total byte size of the queue.
add_item(Req, State = #{ queue := Queue, bytes := Bytes }, Opts) ->
    {ok, TABM} = dev_codec_ans104:deserialize(Req, #{}, Opts),
    ItemSize = erlang:external_size(TABM),
    {ok, _} = hb_cache:write(TABM, Opts),
    State#{
        queue => [TABM | Queue],
        bytes => Bytes + ItemSize
    }.

%% @doc Dispatch the queue if it is ready.
maybe_dispatch(State, Opts) ->
    case dispatchable(State, Opts) of
        true -> dispatch(State, Opts);
        false -> State
    end.

%% @doc Returns whether the queue is dispatchable.
dispatchable(#{ queue := Q, max_items := MaxLen }, Opts) when length(Q) >= MaxLen ->
    true;
dispatchable(#{ bytes := Bytes, max_size := MaxSize }, Opts) when Bytes >= MaxSize ->
    true;
dispatchable(_State, _Opts) ->
    false.

%% @doc Dispatch the queue.
dispatch(State = #{ queue := Q }, Opts) ->
    {ok, Bundle} = dev_codec_tx:to(lists:reverse(Q), #{}, Opts),
    Price = hb_util:ok(get_price(Bundle#tx.data_size, Opts)),
    Anchor = hb_util:ok(get_anchor(Opts)),
    Wallet = hb_opts:get(priv_wallet, no_viable_wallet, Opts),
    Signed = ar_tx:sign(Bundle#tx{ anchor = Anchor, reward = Price }, Wallet),
    TABM = hb_message:convert(Signed, tabm, #{ <<"device">> => <<"tx@1.0">>, <<"bundle">> => true }, Opts),
    {ok, Result} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave@2.9-pre">> },
            TABM#{ <<"path">> => <<"tx">>, <<"method">> => <<"POST">> },
            Opts
        ),
    server(State#{ queue => [], bytes => 0 }, Opts).

get_price(DataSize, Opts) ->
    hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9-pre">> },
        #{ <<"path">> => <<"get_price">>, <<"size">> => DataSize },
        Opts
    ).

get_anchor(Opts) ->
    hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9-pre">> },
        #{ <<"path">> => <<"/tx_anchor">> },
        Opts
    ).

basic_test() ->
    ServerOpts = #{
        bundler_max_items => 3,
        priv_wallet => hb:wallet()
    },
    ClientOpts = #{},
    Node = hb_http_server:start_node(ServerOpts),



    Serialized1 = ar_bundles:serialize(
        ar_bundles:sign_item(#tx{
            data = <<"ONE">>,
            tags = [{<<"tag1">>, <<"value1">>}]
        }, hb:wallet())
    ),

    Serialized2 = ar_bundles:serialize(
        ar_bundles:sign_item(#tx{
            data = <<"TWO">>,
            tags = [{<<"tag2">>, <<"value2">>}]
        }, hb:wallet())
    ),

    Serialized3 = ar_bundles:serialize(
        ar_bundles:sign_item(#tx{
            data = <<"THREE">>,
            tags = [{<<"tag3">>, <<"value3">>}]
        }, hb:wallet())
    ),

    Result1 = hb_http:post(
        Node,
        #{
            <<"device">> => <<"bundler@1.0">>,
            <<"path">> => <<"/tx">>,
            <<"content-type">> => <<"application/octet-stream">>,
            <<"body">> => Serialized1
        },
        ClientOpts
    ),
    ?event(debug_test, {result, Result1}),

    Result2 = hb_http:post(
        Node,
        #{
            <<"device">> => <<"bundler@1.0">>,
            <<"path">> => <<"/tx">>,
            <<"content-type">> => <<"application/octet-stream">>,
            <<"body">> => Serialized2
        },
        ClientOpts
    ),
    ?event(debug_test, {result, Result2}),

    Result3 = hb_http:post(
        Node,
        #{
            <<"device">> => <<"bundler@1.0">>,
            <<"path">> => <<"/tx">>,
            <<"content-type">> => <<"application/octet-stream">>,
            <<"body">> => Serialized3
        },
        ClientOpts
    ),
    ?event(debug_test, {result, Result3}),

    timer:sleep(5000),

    ok.