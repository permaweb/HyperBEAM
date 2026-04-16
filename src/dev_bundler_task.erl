%%% @doc Implements the different bundling primitives:
%%% - post_tx: Building and posting an L1 transaction
%%% - build_proofs:Chunking up the bundle data and building the chunk proofs
%%% - post_proof: Seeding teh chunks to the Arweave network
-module(dev_bundler_task).
-export([worker_loop/0, log_task/3, format_timestamp/0]).
%%% Test-only exports.
-export([data_items_to_tx/2]).
-include("include/hb.hrl").
-include("include/dev_bundler.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Worker loop - executes tasks and reports back to dispatcher.
worker_loop() ->
    receive
        {execute_task, DispatcherPID, Task} ->
            case execute_task(Task) of
                {ok, Value} ->
                    DispatcherPID ! {task_complete, self(), Task, Value};
                {error, Reason} ->
                    DispatcherPID ! {task_failed, self(), Task, Reason}
            end,

            worker_loop();
        stop ->
            exit(normal)
    end.

%% @doc Execute a specific task.
execute_task(#task{type = post_tx, data = Items, opts = Opts} = Task) ->
    try
        ?event(debug_bundler, log_task(executing_task, Task, [])),
        case build_signed_tx(Items, Opts) of
            {ok, SignedTX} ->
                Committed = hb_message:convert(
                    SignedTX,
                    #{ <<"device">> => <<"structured@1.0">>, <<"bundle">> => true },
                    #{ <<"device">> => <<"tx@1.0">>, <<"bundle">> => true },
                    Opts),
                ?event(bundler_short, log_task(posting_tx,
                    Task,
                    [{tx, {explicit, hb_message:id(Committed, signed, Opts)}}]
                )),
                PostTXResponse = dev_arweave:post_tx_header(
                    SignedTX,
                    Opts
                ),
                case PostTXResponse of
                    {ok, _Result} ->
                        dev_bundler_cache:write_tx(
                            Committed,
                            Items,
                            Opts
                        ),
                        {ok, Committed};
                    {_, ErrorReason} -> {error, ErrorReason}
                end;
            {error, {PriceErr, AnchorErr}} ->
                ?event(bundler_short,
                    log_task(task_failed, Task, [
                        {price, PriceErr},
                        {anchor, AnchorErr}
                    ])),
                {error, {PriceErr, AnchorErr}}
        end
    catch
        _:Err:Stack ->
            ?event(bundler_short, log_task(task_failed, Task, [{error, Err}])),
            ?event(bundler_upload_error,
                log_task(task_failed, Task, [{error, Err}, {trace, Stack}])),
            {error, Err}
    end;

execute_task(#task{type = build_proofs, data = CommittedTX, opts = Opts} = Task) ->
    try
        ?event(debug_bundler, log_task(executing_task, Task, [])),
        % Calculate chunks and proofs
        TX = hb_message:convert(
            CommittedTX, <<"tx@1.0">>, <<"structured@1.0">>, Opts),
        Data = TX#tx.data,
        DataRoot = TX#tx.data_root,
        DataSize = TX#tx.data_size,
        Mode = ar_tx:chunking_mode(TX#tx.format),
        Chunks = ar_tx:chunk_binary(Mode, ?DATA_CHUNK_SIZE, Data),
        ?event(bundler_short, {building_proofs,
            {bundle, Task#task.bundle_id},
            {data_size, DataSize},
            {num_chunks, length(Chunks)}}),
        SizeTaggedChunks = ar_tx:chunks_to_size_tagged_chunks(Chunks),
        SizeTaggedChunkIDs = ar_tx:sized_chunks_to_sized_chunk_ids(SizeTaggedChunks),
        {_Root, DataTree} = ar_merkle:generate_tree(SizeTaggedChunkIDs),
        % Build proof list
        Proofs = lists:filtermap(
            fun({Chunk, Offset}) ->
                case Chunk of
                    <<>> -> false;
                    _ ->
                        DataPath = ar_merkle:generate_path(
                            DataRoot, Offset - 1, DataTree),
                        Proof = #{
                            chunk => Chunk,
                            data_path => DataPath,
                            offset => Offset - 1,
                            data_size => DataSize,
                            data_root => DataRoot
                        },
                        {true, Proof}
                end
            end,
            SizeTaggedChunks
        ),
        % -1 because the `?event(...)' macro increments the counter by 1.
        hb_event:increment(bundler_short, built_proofs, length(Proofs) - 1),
        ?event(
            bundler_short,
            {built_proofs,
                {bundle, Task#task.bundle_id},
                {num_proofs, length(Proofs)}
            },
            Opts
        ),
        {ok, Proofs}
    catch
        _:Err:_Stack ->
            ?event(bundler_short, log_task(task_failed, Task, [{error, Err}])),
            {error, Err}
    end;

execute_task(#task{type = post_proof, data = Proof, opts = Opts} = Task) ->
    #{chunk := Chunk, data_path := DataPath, offset := Offset,
      data_size := DataSize, data_root := DataRoot} = Proof,
    ?event(debug_bundler, log_task(executing_task, Task, [])),
    Request = #{
        <<"chunk">> => hb_util:encode(Chunk),
        <<"data_path">> => hb_util:encode(DataPath),
        <<"offset">> => integer_to_binary(Offset),
        <<"data_size">> => integer_to_binary(DataSize),
        <<"data_root">> => hb_util:encode(DataRoot)
    },
    try
        Response = dev_arweave:post_chunk(Request, Opts),
        case Response of
            {ok, _} -> {ok, proof_posted};
            {error, Reason} -> {error, Reason}
        end
    catch
        _:Err:_Stack ->
            ?event(bundler_short, log_task(task_failed, Task, [{error, Err}])),
            {error, Err}
    end.

%% @doc Build and sign a bundle TX without posting it.
build_signed_tx(Items, Opts) ->
    TX = data_items_to_tx(Items, Opts),
    DataSize = TX#tx.data_size,
    PriceResult = get_price(DataSize, Opts),
    AnchorResult = get_anchor(Opts),
    case {PriceResult, AnchorResult} of
        {{ok, Price}, {ok, Anchor}} ->
            Wallet = hb_opts:get(priv_wallet, no_viable_wallet, Opts),
            SignedTX = 
                dev_arweave_common:normalize(
                    ar_tx:sign(
                        TX#tx{anchor = Anchor, reward = Price},
                        Wallet
                    )
                ),
            {ok, SignedTX};
        {PriceErr, AnchorErr} ->
            {error, {PriceErr, AnchorErr}}
    end.

data_items_to_tx(Items, Opts) ->
    List = lists:map(
        fun(Item) -> 
            hb_message:convert(
                Item,
                #{ <<"device">> => <<"ans104@1.0">>, <<"bundle">> => true },
                <<"structured@1.0">>,
                Opts
            )
        end,
        lists:reverse(Items)),
    dev_arweave_common:normalize(#tx{
        format = 2,
        data = List
    }).

get_price(DataSize, Opts) ->
    hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{ <<"path">> => <<"/price">>, <<"size">> => DataSize },
        Opts
    ).

get_anchor(Opts) ->
    hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{ <<"path">> => <<"/tx_anchor">> },
        Opts
    ).

%%%===================================================================
%%% Logging
%%%===================================================================

%% @doc Return a complete task event tuple for logging.
log_task(Event, Task, ExtraLogs) ->
    erlang:list_to_tuple([Event | format_task(Task) ++ ExtraLogs]).

%% @doc Format a task for logging.
format_task(#task{bundle_id = BundleID, type = post_tx, data = DataItems}) ->
    [
        {task_type, post_tx},
        {timestamp, format_timestamp()},
        {bundle, BundleID},
        {num_items, length(DataItems)}
    ];
format_task(#task{bundle_id = BundleID, type = build_proofs, data = CommittedTX}) ->
    [
        {task_type, build_proofs},
        {timestamp, format_timestamp()},
        {bundle, BundleID},
        {tx, {explicit, hb_message:id(CommittedTX, signed, #{})}}
    ];
format_task(#task{bundle_id = BundleID, type = post_proof, data = Proof}) ->
    Offset = maps:get(offset, Proof),
    [
        {task_type, post_proof},
        {timestamp, format_timestamp()},
        {bundle, BundleID},
        {offset, Offset}
    ].

%% @doc Format erlang:timestamp() as a user-friendly RFC3339 string with milliseconds.
format_timestamp() ->
    {MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
    Millisecs = (MegaSecs * 1000000 + Secs) * 1000 + (MicroSecs div 1000),
    calendar:system_time_to_rfc3339(Millisecs, [{unit, millisecond}, {offset, "Z"}]).

build_signed_tx_test() ->
    Anchor = rand:bytes(32),
    Price = 12345,
    {ServerHandle, NodeOpts} = dev_bundler:start_mock_gateway(#{
        price => {200, integer_to_binary(Price)},
        tx_anchor => {200, hb_util:encode(Anchor)}
    }),
    TestOpts = NodeOpts#{
        priv_wallet => ar_wallet:new(),
        store => hb_test_utils:test_store()
    },
    try
        Timestamp = 12344567,
        ListValue = [<<"a">>, <<"b">>, <<"c">>],
        StructuredItems = [
            #{
                <<"body">> => <<"body1">>,
                <<"tag1">> => <<"value1">>,
                <<"timestamp">> => Timestamp
            },
            #{
                <<"body">> => <<"body3">>,
                <<"tag3">> => <<"value3">>,
                <<"list">> => ListValue
            },
            #{
                <<"body">> => <<"body2">>,
                <<"tag2">> => <<"value2">>
            }
        ],
        Items = [
            hb_message:commit(
                Item,
                TestOpts,
                #{ <<"device">> => <<"ans104@1.0">>, <<"bundle">> => true }
            )
        || Item <- StructuredItems],
        {ok, SignedTX} = build_signed_tx(Items, TestOpts),
        ?assert(ar_tx:verify(SignedTX)),
        ?assertEqual(Anchor, SignedTX#tx.anchor),
        ?assertEqual(Price, SignedTX#tx.reward),
        ?event(debug_test, {signed_tx, SignedTX}),
        BundledTX = ar_bundles:deserialize(SignedTX),
        ?event(debug_test, {bundled_tx, BundledTX}),
        BundledItems = hb_util:numbered_keys_to_list(BundledTX#tx.data, #{}),
        lists:foreach(
            fun(Item) ->
                ?assert(ar_bundles:verify_item(Item))
            end,
            BundledItems
        ),
        BundledStructuredItems = [
            hb_message:convert(
                Item,
                <<"structured@1.0">>,
                <<"ans104@1.0">>,
                TestOpts
            )
        || Item <- BundledItems],
        ?assertEqual(lists:reverse(Items), BundledStructuredItems),
        ok
    after
        hb_mock_server:stop(ServerHandle)
    end.

build_signed_tx_on_arbundles_js_test() ->
    Anchor = rand:bytes(32),
    Price = 12345,
    {ServerHandle, NodeOpts} = dev_bundler:start_mock_gateway(#{
        price => {200, integer_to_binary(Price)},
        tx_anchor => {200, hb_util:encode(Anchor)}
    }),
    TestOpts = NodeOpts#{
        priv_wallet => hb:wallet(),
        store => hb_test_utils:test_store()
    },
    try
        % Load an arweave.js-created dataitem
        Item = ar_bundles:deserialize(
            hb_util:ok(
                file:read_file(<<"test/arbundles.js/ans104-item.bundle">>)
            )
        ),
        ?event(debug_test, {item, Item}),
        ?assert(ar_bundles:verify_item(Item)),
        % Load an arweave.js-created list bundle
        {ok, Bin} = file:read_file(<<"test/arbundles.js/ans104-list-bundle.bundle">>),
        BundledItem = ar_bundles:sign_item(#tx{
            format = ans104,
            data = Bin,
            data_size = byte_size(Bin),
            tags = [
                {<<"Bundle-Format">>, <<"binary">>},
                {<<"Bundle-Version">>, <<"2.0.0">>}
            ]
        }, hb:wallet()),
        ?event(debug_test, {bundled_item, BundledItem}),
        ?assert(ar_bundles:verify_item(BundledItem)),
        % Convert both dataitems to structured messages
        ItemStructured = hb_message:convert(Item,
            #{ <<"device">> => <<"structured@1.0">>, <<"bundle">> => true },
            #{ <<"device">> => <<"ans104@1.0">>, <<"bundle">> => true },
            TestOpts),
        ?event(debug_test, {item_structured, ItemStructured}),
        ?assert(hb_message:verify(ItemStructured, all, TestOpts)),
        BundledItemStructured = hb_message:convert(BundledItem,
            #{ <<"device">> => <<"structured@1.0">>, <<"bundle">> => true },
            #{ <<"device">> => <<"ans104@1.0">>, <<"bundle">> => true },
            TestOpts),
        ?event(debug_test, {bundled_item_structured, BundledItemStructured}),
        ?assert(hb_message:verify(BundledItemStructured, all, TestOpts)),
        % Use build_signed_tx/2 to mimic the bundler worker logic.
        {ok, SignedTX} = build_signed_tx(
            [ItemStructured, BundledItemStructured],
            TestOpts
        ),
        ?event(debug_test, {signed_tx, SignedTX}),
        ?assert(ar_tx:verify(SignedTX)),
        % Convert the signed TX to a structured message
        StructuredTX = hb_message:convert(SignedTX,
            #{ <<"device">> => <<"structured@1.0">>, <<"bundle">> => true },
            #{ <<"device">> => <<"tx@1.0">>, <<"bundle">> => true },
            TestOpts),
        % ?event(debug_test, {structured_tx, StructuredTX}),
        ?assert(hb_message:verify(StructuredTX, all, TestOpts)),
        % Convert back to an L1 TX
        SignedTXRoundtrip = hb_message:convert(StructuredTX,
            #{ <<"device">> => <<"tx@1.0">>, <<"bundle">> => true },
            #{ <<"device">> => <<"structured@1.0">>, <<"bundle">> => true },
            TestOpts),
        ?event(debug_test, {signed_tx_roundtrip, SignedTXRoundtrip}),
        ?assert(ar_tx:verify(SignedTXRoundtrip)),
        ?assertEqual(SignedTX, SignedTXRoundtrip),
        ok
    after
        hb_mock_server:stop(ServerHandle)
    end.