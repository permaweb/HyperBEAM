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
                    SignedTX, <<"structured@1.0">>, <<"tx@1.0">>, Opts),
                ?event(bundler_short, log_task(posting_tx,
                    Task,
                    [{tx, {explicit, hb_message:id(Committed, signed, Opts)}}]
                )),
                PostTXResponse = hb_ao:resolve(
                    #{ <<"device">> => <<"arweave@2.9">> },
                    Committed#{
                        <<"path">> => <<"tx">>,
                        <<"method">> => <<"POST">>
                    },
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
        hb_event:record(bundler_short, built_proofs, length(Proofs) - 1),
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
        Response =
            hb_ao:resolve(
                #{ <<"device">> => <<"arweave@2.9">> },
                Request#{
                    <<"path">> => <<"chunk">>,
                    <<"method">> => <<"POST">>
                },
                Opts
            ),
        case Response of
            {ok, _} -> {ok, proof_posted};
            {error, Reason} -> {error, Reason};
            {failure, Reason} = Reason -> {error, Reason}
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
                ar_tx:normalize(
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
    ar_tx:normalize(#tx{
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

build_signed_tx_on_arbundles_js_test() ->
    Anchor = rand:bytes(32),
    Price = 12345,
    {ServerHandle, NodeOpts} = hb_mock_server:start_arweave_gateway(#{
        price => {200, integer_to_binary(Price)},
        tx_anchor => {200, hb_util:encode(Anchor)}
    }),
    TestOpts = NodeOpts#{
        <<"priv-wallet">> => hb:wallet(),
        <<"store">> => hb_test_utils:test_store()
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
            <<"structured@1.0">>,
            <<"ans104@1.0">>,
            TestOpts),
        ?event(debug_test, {item_structured, ItemStructured}),
        ?assert(hb_message:verify(ItemStructured, all, TestOpts)),
        BundledItemStructured = hb_message:convert(BundledItem,
            <<"structured@1.0">>,
            <<"ans104@1.0">>,
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
            <<"structured@1.0">>,
            <<"tx@1.0">>,
            TestOpts),
        % ?event(debug_test, {structured_tx, StructuredTX}),
        ?assert(hb_message:verify(StructuredTX, all, TestOpts)),
        % Convert back to an L1 TX
        SignedTXRoundtrip = hb_message:convert(StructuredTX,
            <<"tx@1.0">>,
            <<"structured@1.0">>,
            TestOpts),
        ?event(debug_test, {signed_tx_roundtrip, SignedTXRoundtrip}),
        ?assert(ar_tx:verify(SignedTXRoundtrip)),
        ?assertEqual(SignedTX, SignedTXRoundtrip),
        ok
    after
        hb_mock_server:stop(ServerHandle)
    end.

%% Test that a nested dataitem is handled correctly by the bundler flow.
%% This test focuses in on the conversion that happens between building
%% the signed bundle TX and building the bundle proofs.
bundle_convert_real_data_test() ->
    Item = inlined_broken_item(),
    Anchor = rand:bytes(32),
    Price = 12345,
    {ServerHandle, NodeOpts} = hb_mock_server:start_arweave_gateway(#{
        price => {200, integer_to_binary(Price)},
        tx_anchor => {200, hb_util:encode(Anchor)}
    }),
    TestOpts = NodeOpts#{
        <<"priv-wallet">> => ar_wallet:new(),
        <<"store">> => hb_test_utils:test_store()
    },
    try
        {ok, SignedTX} = build_signed_tx([Item], TestOpts),
        ?assert(ar_tx:verify(SignedTX)),
        Committed = hb_message:convert(
            SignedTX, <<"structured@1.0">>, <<"tx@1.0">>, TestOpts),
        %% This convert is exactly what build_proofs runs.
        TX = hb_message:convert(
            Committed, <<"tx@1.0">>, <<"structured@1.0">>, TestOpts),
        ?assert(ar_tx:verify(TX))
    after
        hb_mock_server:stop(ServerHandle)
    end.

bundle_convert_minimal_test() ->
    Anchor = rand:bytes(32),
    Price = 12345,
    {ServerHandle, NodeOpts} = hb_mock_server:start_arweave_gateway(#{
        price => {200, integer_to_binary(Price)},
        tx_anchor => {200, hb_util:encode(Anchor)}
    }),
    TestOpts = NodeOpts#{
        <<"priv-wallet">> => ar_wallet:new(),
        <<"store">> => hb_test_utils:test_store()
    },
    try
        Item = hb_message:commit(
            #{ <<"key">> => <<"value">>,
               <<"body">> => #{ <<"a">> => <<"b">> } },
            TestOpts, #{<<"device">> => <<"ans104@1.0">>}),
        {ok, SignedTX} = build_signed_tx([Item], TestOpts),
        ?assert(ar_tx:verify(SignedTX)),
        Committed = hb_message:convert(
            SignedTX, <<"structured@1.0">>, <<"tx@1.0">>, TestOpts),
        TX = hb_message:convert(
            Committed, <<"tx@1.0">>, <<"structured@1.0">>, TestOpts),
        ?assert(ar_tx:verify(TX))
    after
        hb_mock_server:stop(ServerHandle)
    end.

%% @doc Drive a nested tree of items signed in mixed bundle states through
%% the bundler flow: each child is signed with bundle=true OR bundle=false,
%% then we build the bundle TX, sign it, convert through structured@1.0 and
%% back to tx@1.0, and assert nothing was inflated and every commitment
%% still verifies. This exercises the full `hint-device' plumbing across a
%% mixed tree, mirroring the production scenario that motivated the fix.
bundle_convert_mixed_tree_verify_test() ->
    Anchor = rand:bytes(32),
    Price = 12345,
    {ServerHandle, NodeOpts} = hb_mock_server:start_arweave_gateway(#{
        price => {200, integer_to_binary(Price)},
        tx_anchor => {200, hb_util:encode(Anchor)}
    }),
    TestOpts = NodeOpts#{
        <<"priv-wallet">> => ar_wallet:new(),
        <<"store">> => hb_test_utils:test_store()
    },
    try
        %% Build three items. The first carries a child signed bundle=false,
        %% the second a child signed bundle=true, the third has no nested
        %% child at all. The L1 bundle TX therefore contains items that
        %% would individually each round-trip with a different bundle state.
        InnerFalse = hb_message:commit(
            #{ <<"leaf-tag">> => <<"leaf-false">>,
               <<"leaf-list">> => [1, 2, 3] },
            TestOpts,
            #{ <<"device">> => <<"ans104@1.0">>, <<"bundle">> => false }),
        ?assert(hb_message:verify(InnerFalse, all, TestOpts)),
        InnerTrue = hb_message:commit(
            #{ <<"leaf-tag">> => <<"leaf-true">>,
               <<"leaf-list">> => [4, 5, 6] },
            TestOpts,
            #{ <<"device">> => <<"ans104@1.0">>, <<"bundle">> => true }),
        ?assert(hb_message:verify(InnerTrue, all, TestOpts)),
        ItemA = hb_message:commit(
            #{ <<"item-tag">> => <<"a">>, <<"inner">> => InnerFalse },
            TestOpts,
            #{ <<"device">> => <<"ans104@1.0">>, <<"bundle">> => true }),
        ?assert(hb_message:verify(ItemA, all, TestOpts)),
        ItemB = hb_message:commit(
            #{ <<"item-tag">> => <<"b">>, <<"inner">> => InnerTrue },
            TestOpts,
            #{ <<"device">> => <<"ans104@1.0">>, <<"bundle">> => false }),
        ?assert(hb_message:verify(ItemB, all, TestOpts)),
        ItemC = hb_message:commit(
            #{ <<"item-tag">> => <<"c">> },
            TestOpts,
            #{ <<"device">> => <<"ans104@1.0">>, <<"bundle">> => false }),
        ?assert(hb_message:verify(ItemC, all, TestOpts)),
        {ok, SignedTX} = build_signed_tx([ItemA, ItemB, ItemC], TestOpts),
        ?assert(ar_tx:verify(SignedTX)),
        Committed = hb_message:convert(
            SignedTX, <<"structured@1.0">>, <<"tx@1.0">>, TestOpts),
        ?event(debug_test, {committed, {explicit, Committed}}),
        ?assert(hb_message:verify(Committed, all, TestOpts)),
        %% Convert back to TX (same path build_proofs uses) and check that
        %% the data did not inflate.
        TX = hb_message:convert(
            Committed, <<"tx@1.0">>, <<"structured@1.0">>, TestOpts),
        ?assert(ar_tx:verify(TX))
    after
        hb_mock_server:stop(ServerHandle)
    end.

%% Hardcoded item, structurally identical to one observed in a broken
%% production bundle (TXID -BTiilFCWd2kB3oOdCpPDJLGXhjeNxIeMH3kerPXKCM).
%% AO "Assignment" message with `body`, two commitments (HMAC + RSA-PSS),
%% per-event commitments inside the body. All public key / signature
%% bytes are real (from production) since the structured form encodes
%% them.
inlined_broken_item() ->
    #{<<"base-hashpath">> =>
          <<"w_l6KLmO8OeEM6vmdwX1HwdCDmHiOlhUyAeNdjwpspU/p4CQHPCo629uDl8seMpWN5Z4EZpRK6bUNPbGAoOIkrs">>,
      <<"block-hash">> => <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,
      <<"block-height">> => 0,
      <<"block-timestamp">> => 0,
      <<"body">> =>
          #{<<"commitments">> =>
                #{<<"HkUAI3fWd3uHltdfyHzLU5IreUtmIIqv45ZxsC12psI">> =>
                      #{<<"commitment-device">> => <<"httpsig@1.0">>,
                        <<"committed">> =>
                            [<<"event">>, <<"reference">>, <<"status-class">>],
                        <<"committer">> =>
                            <<"mfj2T6f_3stKQk7fctrbpZKUfu4V6MQKCH-YHLtFnOY">>,
                        <<"keyid">> =>
                            <<"publickey:kvgGBmHtEOxZTuJuJuHVqBe51aLpSDvZrzj5RjNEbw8NrYm3GB9+BEKdYD+fHZ0H775PJf8mosGapkP6pB8h8ZEEc6AuOo+lTJ9SKEnYit1Q6YG5Dg306EDfm0dpMU3zKe9pE4CIHf3ffCDqa1Xh4c1zdcFqKyofeT8PWIGQZCScA8rYG+aG2Z/y6QduyxBgzFfITdzeXbnJONmZEwPEA29LeDWmCxA7CSfE6W8+2aDW75qQjETRVXzxou0I0tsc3uXzd9E0/yU6NbDi93sIBiO8z2pNbGrMGIfpH+4dirH/YZNBW8PBgOLjnpe5yoPrT+cI8OoEX27u/al/rkPG4u0wBlxnollSr/lXc5HIn6AWvpSF7nuXcmdtG8Y8RK95h4YZ9d6CG3tSOTvSk7wK8IH97hScB16EpiuT6Xi/TPYh3PwVC/VDxLMox19v1eP1riHC/nkIroerCmaIwGfxI2XNUgQzaTcygjT0DFbbLZFakCZpJ+0+u2/S1I4EbdpdcChWqrA8psUlyR3sbhhPDpEP1ldNO+08OyW/PMfMwXEkVR+WHM2t5m9cyZwjpes6epCQQWjMIkwqeIRZWwo607iJKsXgd5n+73FWytVWNS1mOH1nDDkfDXOq4R60B6C+2k0As14b2Dv4eXZstbr8KbtVIHit/IytBohieLpMcF0=">>,
                        <<"signature">> =>
                            <<"fZgshLexhVcpiQ1sBhwa_eoDn97vvc6IoJtwntc8VJTSDAokQ0RyThcjqhcgtF04kl4T986lZrVptAXkiKwog-gH1vnJX1T2yGAM-ZlTNTTmdLE7OIQhvs26-0_L3poPUSEjHsZ1vU2RpUUvKLIEQdCwlgTXGx54ZGB6feYXMn9e01tZPEdTVD0AcALa5G55aqyH3Lde5KXx4vOgdvWaCr772dXZ6C8249UG02SIHy3xvp1UdkLtzIbvSY9n5UzC1Bt-b5JftIijmVuIv3oI0_y9rRxGYLm2m7VusHwYjRdFAjN5X_NvYpWx25b62CNNLwprJfDqhllZsDz6PjnhRh9ZocOP3OLrarW0owFt0dfDRt3VBYaksUYTem-9YWtzS3Qa7kZSB754xtOW62wvu3kVH2sNB5C9SoXmheoPUjNLa4qXQv4-NJPF4wVdj8QxM0mYO0KQZfCUZtXhYYaqwRmS2aMyUrca1xjPOkD0nr7B1IS805O08fTkN6YcMluUH93myL4VbPPa2v1V2k-B-OlP4AzOn9F1uzk5ek--K_-2QdC63vgm4EKv8XqBoipUJ0Fe0jKUsE9iLZJddoMrYrsQCp8WMWX7iGaP6zJU2tbMpkAl-rr_Hc8xUkJ3eBd6pQcw-1MQ8EK7trPnjQD0EQZAG2HYj87HG-qCX3l9o8w">>,
                        <<"type">> => <<"rsa-pss-sha512">>},
                  <<"asDiK4CqvjJf2d9FFf3r3-xCrs1jA8ee9tWUp43BuWk">> =>
                      #{<<"commitment-device">> => <<"httpsig@1.0">>,
                        <<"committed">> =>
                            [<<"event">>, <<"reference">>, <<"status-class">>],
                        <<"keyid">> => <<"constant:ao">>,
                        <<"signature">> =>
                            <<"asDiK4CqvjJf2d9FFf3r3-xCrs1jA8ee9tWUp43BuWk">>,
                        <<"type">> => <<"hmac-sha256">>}},
            <<"event">> => <<"is_admissible">>,
            <<"reference">> =>
                <<"HnbIWJdkG4CCwHCiycMKMmv2posdcTJ5xFcZ9lpTQQs">>,
            <<"status-class">> => <<"success">>},
      <<"commitments">> =>
          #{<<"KT4ZXa_nhnWTfNJdVwOPDwHNN3eqYs_o3JoYv_odNvE">> =>
                #{<<"commitment-device">> => <<"httpsig@1.0">>,
                  <<"committed">> =>
                      [<<"ao-types">>, <<"base-hashpath">>, <<"block-hash">>,
                       <<"block-height">>, <<"block-timestamp">>, <<"body">>,
                       <<"data-protocol">>, <<"epoch">>, <<"path">>,
                       <<"process">>, <<"slot">>, <<"timestamp">>, <<"type">>,
                       <<"variant">>],
                  <<"keyid">> => <<"constant:ao">>,
                  <<"signature">> =>
                      <<"KT4ZXa_nhnWTfNJdVwOPDwHNN3eqYs_o3JoYv_odNvE">>,
                  <<"type">> => <<"hmac-sha256">>},
            <<"j1KSZD2tQOXpYvbPaqmLyRN6OOXxGa20bkgfeCj4a30">> =>
                #{<<"bundle">> => <<"false">>,
                  <<"commitment-device">> => <<"ans104@1.0">>,
                  <<"committed">> =>
                      [<<"ao-types">>, <<"base-hashpath">>, <<"block-hash">>,
                       <<"block-height">>, <<"block-timestamp">>, <<"body">>,
                       <<"data-protocol">>, <<"epoch">>, <<"path">>,
                       <<"process">>, <<"slot">>, <<"timestamp">>, <<"type">>,
                       <<"variant">>],
                  <<"committer">> =>
                      <<"n_XZJhUnmldNFo4dhajoPZWhBXuJk-OcQr5JQ49c4Zo">>,
                  <<"keyid">> =>
                      <<"publickey:9BXuilimqVo7fpnoToPHZwqL7w_C0Qn4N3egeJRy05-nSpUv1vyp9xHbVLKVMPnJsie5Awt_xxob_jDvXSmE1fDsUpNnFurxG88UWN4zSNi87EfOorDQjHPRUqKPIYvg6xqPCpXPpOccJbFuack3ltQKtF5XLoaKWbsPdUtMquRXrbJgnGeOvXhQhbKa4xJKwGmjVC_LpY5FQ8j-cOlBOVVe_B7KF4eWG3sJf-z59MJQOaAozyU2iZpsuhslkTNVj8sM9CqkSfyD8EjEZdfF088IM_dJgk6ehIDHbx3FcGVnxpUHkXEnJFAlXRzdqmNb84QXsTNOHqwQPZ3q5wPRWS6iUaNxfeS_SsR6otIJgrYq04LYJLcpHuKGp53-b8tTeIvDFcmS2_kPijPqPINbf9c5uH0mxMNomB-8rVDIkIZ6Ojc_M0JnaQSk2rYPq8qRy2PuvAFyo1zeGM-2Bo4GNl9dMnfIr_Q6MlxRUwAwLHdOt0BJkxEBfOIw3MkB2d-SiVWtxG1Uqib7Iu_yn3j9DwzUOHjRQTse07giNDXRMsr1ml_sCK3bIetUFVnjjnoTNDEItDSck7lTFgvCdyXKkvXtSiNHkW8TCbdTDY0hBJzLVheKDb_cCyfmcTKo5ql2sWsZYCC7XybKdRMxU2HNNIUSpcDvhnTwv5-oq42Lmqc">>,
                  <<"signature">> =>
                      <<"sE9TuQTsMCHhaSOmHF-Wqu8QBbSNMSeSztiE1b0tJfmSNOe1nKPmMcCZN1rHD8L9xQWJw4hSVUbChwt4QReTz2IoXFz1NT80F1qCY2x3uFMFxgUHb2abTQW_-VNjFGWFe-sguwYLAIZGYoJ9a2g1EJCRfksk9iOWXRt7j_yIBixKATq-QsEWdcwfBsEUYWq-IRI1RdPAr9ToZeQ13TtWWYxcRbKHwxJ1M58p2CuLCi1OXVmENLjacAawuhBjGV4oTQ1-QBap-JOjB6kRTXtWjNGnMTPF01edFJIxgRncnODrTO_ehz6qkFH6iMhI9oV4w5VcRCKnNM7fxTXKj6DeiuAb1KrirpzohzsTLautMqRhst8gSViBlftd4XoVCDVscawuz8yPDyJoDxhIIup7mO51QSmNVTM6JpSEsG-CbXa64aECBOq7_x-ld9xHyNvCCSHetSJ3EBiJDWHE8XCurePGJ6GLeggugQ85LxgsRaLDm9UIlbMhopkK4X-SyXz5_pGwUSegLa1QHWWxnIaS5zTm0f4yi_YiBmgmS27v28T-nTzOHuBGTl8yUWVG_CKAELjFVREm5I7h4UuDQuFoXlkkFW22-Gyx5tZh1eSxRpl1NOwhyGc9O-6TIR46t1BhlItitOoi6JEf26JjTmwJWF7kR8xyahCYWtHFEkzpob4">>,
                  <<"type">> => <<"rsa-pss-sha256">>}},
      <<"data-protocol">> => <<"ao">>,
      <<"epoch">> => <<"0">>,
      <<"path">> => <<"compute">>,
      <<"process">> => <<"1V65_gzlifHH_surfFzL6HGfRlLJuEX_y0VbPHwIKec">>,
      <<"slot">> => 180901,
      <<"timestamp">> => 1778975170441,
      <<"type">> => <<"Assignment">>,
      <<"variant">> => <<"ao.N.1">>}.
