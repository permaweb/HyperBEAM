%%% @doc A dispatcher for the bundler device (dev_bundler). This module
%%% manages a worker pool to handle bundle building, TX posting, proof
%%% generation, and chunk seeding. Server-side dispatch state lives in
%%% `dev_bundler'; this module only owns worker execution.
-module(dev_bundler_dispatch).
-export([worker_loop/0, format_task/1, format_timestamp/0]).
-include("include/hb.hrl").
-include("include/dev_bundler.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Format a task for logging.
format_task(#task{bundle_id = BundleID, type = post_tx, data = DataItems}) ->
    {post_tx, {timestamp, format_timestamp()}, {bundle, BundleID},
        {num_items, length(DataItems)}};
format_task(#task{bundle_id = BundleID, type = build_proofs, data = CommittedTX}) ->
    {build_proofs, {timestamp, format_timestamp()}, {bundle, BundleID},
        {tx, {explicit, hb_message:id(CommittedTX, signed, #{})}}};
format_task(#task{bundle_id = BundleID, type = post_proof, data = Proof}) ->
    Offset = maps:get(offset, Proof),
    {post_proof, {timestamp, format_timestamp()}, {bundle, BundleID},
        {offset, Offset}}.

%% @doc Format erlang:timestamp() as a user-friendly RFC3339 string with milliseconds.
format_timestamp() ->
    {MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
    Millisecs = (MegaSecs * 1000000 + Secs) * 1000 + (MicroSecs div 1000),
    calendar:system_time_to_rfc3339(Millisecs, [{unit, millisecond}, {offset, "Z"}]).

%%% Worker implementation

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
        ?event(bundler_debug, {execute_task, format_task(Task)}),
        % Get price and anchor
        {ok, TX} = dev_codec_tx:to(lists:reverse(Items), #{}, #{}),
        DataSize = TX#tx.data_size,
        PriceResult = get_price(DataSize, Opts),
        AnchorResult = get_anchor(Opts),
        case {PriceResult, AnchorResult} of
            {{ok, Price}, {ok, Anchor}} ->
                % Sign the TX
                Wallet = hb_opts:get(priv_wallet, no_viable_wallet, Opts),
                SignedTX = ar_tx:sign(TX#tx{ anchor = Anchor, reward = Price }, Wallet),
                % Convert and post
                Committed = hb_message:convert(
                    SignedTX,
                    #{ <<"device">> => <<"structured@1.0">>, <<"bundle">> => true },
                    #{ <<"device">> => <<"tx@1.0">>, <<"bundle">> => true },
                    Opts),
                ?event(bundler_short, {posting_tx,
                    {tx, {explicit, hb_message:id(Committed, signed, Opts)}}}),
                PostTXResponse = hb_ao:resolve(
                    #{ <<"device">> => <<"arweave@2.9">> },
                    Committed#{
                        <<"path">> => <<"/tx">>,
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
            {PriceErr, AnchorErr} ->
                ?event(bundle_short, {post_tx_failed,
                    format_task(Task),
                    {price, PriceErr},
                    {anchor, AnchorErr}}),
                {error, {PriceErr, AnchorErr}}
        end
    catch
        _:Err:_Stack -> 
            ?event(bundle_short, {post_tx_failed,
                format_task(Task),
                {error, Err}}),
            {error, Err}
    end;

execute_task(#task{type = build_proofs, data = CommittedTX, opts = Opts} = Task) ->
    try
        ?event(bundler_debug, {execute_task, format_task(Task)}),
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
            ?event(bundler_short, {build_proofs_failed,
                format_task(Task),
                {error, Err}}),
            {error, Err}
    end;

execute_task(#task{type = post_proof, data = Proof, opts = Opts} = Task) ->
    #{chunk := Chunk, data_path := DataPath, offset := Offset,
      data_size := DataSize, data_root := DataRoot} = Proof,
    ?event(bundler_debug, {execute_task, format_task(Task)}),
    Request = #{
        <<"chunk">> => hb_util:encode(Chunk),
        <<"data_path">> => hb_util:encode(DataPath),
        <<"offset">> => integer_to_binary(Offset),
        <<"data_size">> => integer_to_binary(DataSize),
        <<"data_root">> => hb_util:encode(DataRoot)
    },
    try
        Serialized = hb_json:encode(Request),
        Response = dev_arweave:post_json_chunk(Serialized, Opts),
        case Response of
            {ok, _} -> {ok, proof_posted};
            {error, Reason} -> {error, Reason}
        end
    catch
        _:Err:_Stack ->
            ?event(bundler_short, {post_proof_failed,
                format_task(Task),
                {error, Err}}),
            {error, Err}
    end.

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

