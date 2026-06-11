%%% @doc A read-only store sourcing immutable LBRY claim-output evidence by
%%% display-order outpoint (`txid:nout'). The raw transaction is fetched and
%%% txid-verified through `hb_store_lbry_transaction', then the requested
%%% output is parsed into a claim-output message with a native
%%% `lbry-claim@1.0' commitment. With the `kind' store option set to
%%% `channel', the output must be a channel claim and the returned message
%%% carries the normalized channel public key under a `lbry-channel@1.0'
%%% commitment instead. With `kind' set to `stream', the output must be a
%%% stream claim and the returned message additionally carries the
%%% descriptor `sd_hash' under a `lbry-stream@1.0' commitment.
%%%
%%% Claim-ID and name lookup is a locator concern and deliberately not part
%%% of this store: outpoints are immutable evidence, claim IDs are not
%%% provably current without a ClaimTrie proof.
%%%
%%% With the `walk-ancestry' store option, update outputs additionally get
%%% their create ancestry walked through `hb_store_lbry_transaction': a
%%% complete signature-authorized chain back to the create upgrades the
%%% evidence to an `ancestor-hash160-outpoint' commitment with the ancestry
%%% embedded under `claim-ancestry'. When no complete supported proof can be
%%% built, the output keeps its honest `asserted-claim-id' label. The
%%% `ancestry-depth-limit' option bounds the walk.
-module(hb_store_lbry_claim_output).
-export([scope/0, scope/1, type/3, read/3, resolve/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

scope() -> remote.
scope(_) -> scope().

resolve(_StoreOpts, #{ <<"resolve">> := Key }, _NodeOpts) ->
    case parse_outpoint(Key) of
        {ok, TxID, Nout} ->
            {ok, <<TxID/binary, ":", (integer_to_binary(Nout))/binary>>};
        _ ->
            {error, not_found}
    end.

type(StoreOpts, #{ <<"type">> := Key }, NodeOpts) ->
    case read(StoreOpts, #{ <<"read">> => Key }, NodeOpts) of
        {ok, _} -> {ok, simple};
        Error -> Error
    end.

read(StoreOpts, #{ <<"read">> := Key }, NodeOpts) ->
    case parse_outpoint(Key) of
        {ok, TxID, Nout} ->
            Result = fetch_output(StoreOpts, TxID, Nout, NodeOpts),
            ?event(lbry_claim_output,
                {claim_output_read,
                    {txid, TxID},
                    {nout, Nout},
                    {kind, kind(StoreOpts, NodeOpts)},
                    {result, result_class(Result)}},
                NodeOpts
            ),
            Result;
        _ ->
            {error, not_found}
    end.

fetch_output(StoreOpts, TxID, Nout, NodeOpts) ->
    maybe
        {ok, TxMsg} ?=
            hb_store_lbry_transaction:read(
                StoreOpts,
                #{ <<"read">> => TxID },
                NodeOpts
            ),
        Raw = maps:get(<<"raw">>, TxMsg),
        {ok, Ancestry} ?= output_ancestry(StoreOpts, Raw, Nout, NodeOpts),
        case kind(StoreOpts, NodeOpts) of
            <<"channel">> ->
                hb_lbry_commitment:channel_output_message(Raw, Nout, Ancestry);
            <<"stream">> ->
                hb_lbry_commitment:stream_claim_message(Raw, Nout, Ancestry);
            _ ->
                hb_lbry_commitment:claim_output_message(Raw, Nout, Ancestry)
        end
    end.

%% @doc Build the create-ancestry proof for an update output when the
%% `walk-ancestry' option is enabled. Conditions that merely prevent the
%% upgrade leave the output on its `asserted' label; inconsistent evidence
%% fails the read.
%% Store options may arrive from JSON node configuration, where booleans
%% and integers are carried in their encoded binary forms.
output_ancestry(StoreOpts, Raw, Nout, NodeOpts) ->
    case hb_maps:get(<<"walk-ancestry">>, StoreOpts, false, NodeOpts) of
        true -> walk_ancestry(StoreOpts, Raw, Nout, NodeOpts);
        <<"true">> -> walk_ancestry(StoreOpts, Raw, Nout, NodeOpts);
        _ -> {ok, undefined}
    end.

walk_ancestry(StoreOpts, Raw, Nout, NodeOpts) ->
    Result =
        hb_lbry_ancestry:build(
            Raw,
            Nout,
            transaction_fetcher(StoreOpts, NodeOpts),
            depth_limit(StoreOpts, NodeOpts)
        ),
    case Result of
        {ok, Entries} ->
            {ok, Entries};
        {error, not_an_update} ->
            {ok, undefined};
        {degrade, Reason} ->
            ?event(lbry_claim_output,
                {ancestry_degraded, {nout, Nout}, {reason, Reason}},
                NodeOpts
            ),
            {ok, undefined};
        {error, _} = Error ->
            Error
    end.

depth_limit(StoreOpts, NodeOpts) ->
    hb_lbry_ancestry:depth_limit(
        hb_maps:get(<<"ancestry-depth-limit">>, StoreOpts, undefined, NodeOpts)
    ).

transaction_fetcher(StoreOpts, NodeOpts) ->
    fun(TxID) -> fetch_parent_transaction(TxID, StoreOpts, NodeOpts, 2) end.

%% The SDK proxy intermittently fails single requests, and an ancestry walk
%% multiplies the exposure: one fetch per input per hop. A transient failure
%% gets one retry before it degrades the proof to `asserted'.
fetch_parent_transaction(TxID, StoreOpts, NodeOpts, Attempts) ->
    case
        hb_store_lbry_transaction:read(
            StoreOpts,
            #{ <<"read">> => TxID },
            NodeOpts
        )
    of
        {ok, TxMsg} ->
            {ok, maps:get(<<"raw">>, TxMsg)};
        _Error when Attempts > 1 ->
            timer:sleep(300),
            fetch_parent_transaction(TxID, StoreOpts, NodeOpts, Attempts - 1);
        Error ->
            Error
    end.

kind(StoreOpts, NodeOpts) ->
    hb_maps:get(<<"kind">>, StoreOpts, <<"claim">>, NodeOpts).

parse_outpoint(Key) when is_binary(Key) ->
    case binary:split(Key, <<":">>) of
        [TxID, NoutBin] ->
            case {valid_txid(TxID), parse_nout(NoutBin)} of
                {true, {ok, Nout}} -> {ok, hb_util:to_lower(TxID), Nout};
                _ -> {error, invalid_outpoint}
            end;
        _ ->
            {error, invalid_outpoint}
    end;
parse_outpoint(_) ->
    {error, invalid_outpoint}.

parse_nout(NoutBin) ->
    try binary_to_integer(NoutBin) of
        Nout when Nout >= 0 -> {ok, Nout};
        _ -> {error, invalid_nout}
    catch
        _:_ -> {error, invalid_nout}
    end.

valid_txid(TxID) when is_binary(TxID), byte_size(TxID) == 64 ->
    try binary:decode_hex(TxID) of
        Decoded -> byte_size(Decoded) == 32
    catch
        _:_ -> false
    end;
valid_txid(_) ->
    false.

result_class({ok, _}) -> ok;
result_class({error, _}) -> error;
result_class({failure, _}) -> failure.

%%% Tests

read_returns_committed_claim_output_test() ->
    application:ensure_all_started(inets),
    TxID = <<"51d3cd6a27420addb648347410233931b862ab52660c1dba58806b5b0f38a460">>,
    {ok, Server, Handle} = proxy_server(hb_lbry_tx:task0_tx_hex()),
    try
        Store = store(Server),
        {ok, Msg} =
            read(
                Store,
                #{ <<"read">> => <<TxID/binary, ":0">> },
                #{ <<"http-client">> => httpc }
            ),
        ?assertEqual(<<"lbry-claim@1.0">>, maps:get(<<"device">>, Msg)),
        ?assertEqual(
            <<"9cc7f0e3de8db3b2ffd6dc0b4f1a0f0ca48a6b49">>,
            maps:get(<<"claim-id">>, Msg)
        ),
        ?assertEqual(<<"create">>, maps:get(<<"claim-op">>, Msg)),
        ?assertEqual(TxID, maps:get(<<"txid">>, Msg)),
        ?assertEqual(0, maps:get(<<"nout">>, Msg)),
        ?assertEqual(
            true,
            hb_message:verify(Msg, #{ <<"commitment-ids">> => <<"all">> }, #{})
        )
    after
        hb_mock_server:stop(Handle)
    end.

read_rejects_non_claim_output_test() ->
    application:ensure_all_started(inets),
    TxID = <<"51d3cd6a27420addb648347410233931b862ab52660c1dba58806b5b0f38a460">>,
    {ok, Server, Handle} = proxy_server(hb_lbry_tx:task0_tx_hex()),
    try
        Store = store(Server),
        ?assertEqual(
            {error, missing_claim_output},
            read(
                Store,
                #{ <<"read">> => <<TxID/binary, ":1">> },
                #{ <<"http-client">> => httpc }
            )
        )
    after
        hb_mock_server:stop(Handle)
    end.

read_rejects_channel_kind_for_stream_claim_test() ->
    application:ensure_all_started(inets),
    TxID = <<"51d3cd6a27420addb648347410233931b862ab52660c1dba58806b5b0f38a460">>,
    {ok, Server, Handle} = proxy_server(hb_lbry_tx:task0_tx_hex()),
    try
        Store = (store(Server))#{ <<"kind">> => <<"channel">> },
        ?assertEqual(
            {error, {missing_field, 2}},
            read(
                Store,
                #{ <<"read">> => <<TxID/binary, ":0">> },
                #{ <<"http-client">> => httpc }
            )
        )
    after
        hb_mock_server:stop(Handle)
    end.

read_rejects_invalid_outpoint_test() ->
    ?assertEqual(
        {error, not_found},
        read(#{}, #{ <<"read">> => <<"not-an-outpoint">> }, #{})
    ),
    ?assertEqual(
        {error, not_found},
        read(#{}, #{ <<"read">> => <<"aabb:0">> }, #{})
    ).

read_walks_ancestry_for_updates_test() ->
    application:ensure_all_started(inets),
    {CreateRaw, [UpdateRaw], ClaimID} = hb_lbry_ancestry:test_chain(1),
    {ok, Server, Handle} = chain_proxy_server([CreateRaw, UpdateRaw]),
    try
        Store = (store(Server))#{ <<"walk-ancestry">> => true },
        UpdateTxID = hb_lbry_tx:txid(UpdateRaw),
        {ok, Msg} =
            read(
                Store,
                #{ <<"read">> => <<UpdateTxID/binary, ":0">> },
                #{ <<"http-client">> => httpc }
            ),
        ?assertEqual(ClaimID, maps:get(<<"claim-id">>, Msg)),
        ?assertEqual(
            <<"ancestor-derived">>,
            maps:get(<<"claim-proof-strength">>, Msg)
        ),
        ?assertMatch([_], maps:get(<<"claim-ancestry">>, Msg)),
        [Commitment] = maps:values(maps:get(<<"commitments">>, Msg)),
        ?assertEqual(
            <<"ancestor-hash160-outpoint">>,
            maps:get(<<"type">>, Commitment)
        ),
        ?assertEqual(
            true,
            hb_message:verify(Msg, #{ <<"commitment-ids">> => <<"all">> }, #{})
        )
    after
        hb_mock_server:stop(Handle)
    end.

read_degrades_to_asserted_without_ancestry_test() ->
    % The create transaction is not available from the backend: the update
    % keeps its honest asserted label instead of failing or overclaiming.
    application:ensure_all_started(inets),
    {_CreateRaw, [UpdateRaw], _ClaimID} = hb_lbry_ancestry:test_chain(1),
    {ok, Server, Handle} = chain_proxy_server([UpdateRaw]),
    try
        Store = (store(Server))#{ <<"walk-ancestry">> => true },
        UpdateTxID = hb_lbry_tx:txid(UpdateRaw),
        {ok, Msg} =
            read(
                Store,
                #{ <<"read">> => <<UpdateTxID/binary, ":0">> },
                #{ <<"http-client">> => httpc }
            ),
        ?assertEqual(<<"asserted">>, maps:get(<<"claim-proof-strength">>, Msg)),
        ?assertEqual(false, maps:is_key(<<"claim-ancestry">>, Msg)),
        [Commitment] = maps:values(maps:get(<<"commitments">>, Msg)),
        ?assertEqual(<<"asserted-claim-id">>, maps:get(<<"type">>, Commitment)),
        ?assertEqual(
            true,
            hb_message:verify(Msg, #{ <<"commitment-ids">> => <<"all">> }, #{})
        )
    after
        hb_mock_server:stop(Handle)
    end.

proxy_server(Hex) ->
    Response =
        hb_json:encode(#{
            <<"jsonrpc">> => <<"2.0">>,
            <<"result">> => #{ <<"hex">> => Hex },
            <<"id">> => 1
        }),
    hb_mock_server:start([{"/api/v1/proxy", proxy, {200, Response}}]).

%% A proxy that serves `transaction_show' for the given raw transactions by
%% txid, so ancestry walks can fetch multiple parents.
chain_proxy_server(Raws) ->
    Index =
        maps:from_list(
            [{hb_lbry_tx:txid(Raw), hb_util:to_hex(Raw)} || Raw <- Raws]
        ),
    Handler =
        fun(Req) ->
            Request = hb_json:decode(maps:get(<<"body">>, Req)),
            TxID = hb_util:deep_get([<<"params">>, <<"txid">>], Request, #{}),
            Response =
                case maps:get(TxID, Index, undefined) of
                    undefined ->
                        #{
                            <<"jsonrpc">> => <<"2.0">>,
                            <<"error">> => #{ <<"message">> => <<"not found">> },
                            <<"id">> => 1
                        };
                    Hex ->
                        #{
                            <<"jsonrpc">> => <<"2.0">>,
                            <<"result">> => #{ <<"hex">> => Hex },
                            <<"id">> => 1
                        }
                end,
            {200, hb_json:encode(Response)}
        end,
    hb_mock_server:start([{"/api/v1/proxy", proxy, Handler}]).

store(Server) ->
    #{
        <<"store-module">> => ?MODULE,
        <<"lbry-proxy-node">> => Server,
        <<"http-client">> => httpc
    }.
