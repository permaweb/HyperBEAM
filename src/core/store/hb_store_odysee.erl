%%% @doc Read-only Odysee source store.
%%%
%%% This store sources public Odysee objects and returns normalized HyperBEAM
%%% messages carrying source commitments. It is intentionally a
%%% store, not another playback adapter: callers can place it below a local
%%% cache or behind `hb_store_remote_node' and then verify the returned message
%%% through normal `hb_message:verify/3'.
-module(hb_store_odysee).
-export([start/3, stop/3, reset/3, scope/0, scope/1]).
-export([read/3, type/3, resolve/3, list/3]).
-export([write/3, group/3, link/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(ODYSEE_COMMITMENT_DEVICE, <<"odysee@1.0">>).
-define(LBRY_BLOB_COMMITMENT_DEVICE, <<"lbry-blob@1.0">>).
-define(LBRY_STREAM_DESCRIPTOR_COMMITMENT_DEVICE, <<"lbry-stream-descriptor@1.0">>).
-define(LBRY_CLAIM_OUTPUT_COMMITMENT_DEVICE, <<"lbry-claim-output@1.0">>).
-define(LBRY_TRANSACTION_COMMITMENT_DEVICE, <<"lbry-transaction@1.0">>).
-define(SHA384_HEX_SIZE, 96).
-define(DEFAULT_BLOB_BASE_URLS, [
    <<"https://blobcache-eu.odycdn.com">>,
    <<"https://blobcache-us.odycdn.com">>,
    <<"https://blobcache.lbry.com">>
]).

start(_StoreOpts, _Req, _NodeOpts) ->
    ok.

stop(_StoreOpts, _Req, _NodeOpts) ->
    ok.

reset(_StoreOpts, _Req, _NodeOpts) ->
    ok.

scope() ->
    remote.

scope(#{ <<"scope">> := Scope }) ->
    Scope;
scope(_StoreOpts) ->
    scope().

resolve(_StoreOpts, #{ <<"resolve">> := Key }, _NodeOpts) ->
    {ok, normalize_key(Key)}.

type(StoreOpts, #{ <<"type">> := Key }, NodeOpts) ->
    case read(StoreOpts, #{ <<"read">> => Key }, NodeOpts) of
        {ok, Msg} when is_map(Msg) -> {ok, composite};
        {ok, _Bin} -> {ok, simple};
        Error -> Error
    end.

list(_StoreOpts, _Req, _NodeOpts) ->
    {error, not_found}.

write(_StoreOpts, _Req, _NodeOpts) ->
    {error, read_only}.

group(_StoreOpts, _Req, _NodeOpts) ->
    {error, read_only}.

link(_StoreOpts, _Req, _NodeOpts) ->
    {error, read_only}.

%% @doc Read a public Odysee object by a stable store path.
read(StoreOpts, #{ <<"read">> := Key }, NodeOpts) ->
    Path = canonical_read_path(normalize_key(Key)),
    case fixture(Path, StoreOpts, NodeOpts) of
        {ok, Msg} ->
            Type = infer_type(Path, Msg, NodeOpts),
            commit_result(enrich_surface(Path, Type, Msg), Type, NodeOpts);
        not_found ->
            read_live(Path, StoreOpts, NodeOpts)
    end.

read_live(<<"odysee/claim/", Encoded/binary>>, StoreOpts, NodeOpts) ->
    maybe
        {ok, URI} ?= decode_component(Encoded),
        {ok, Claim} ?=
            hb_ao:raw(
                <<"odysee-claim@1.0">>,
                <<"resolve">>,
                #{},
                #{ <<"url">> => URI },
                store_node_opts(StoreOpts, NodeOpts)
            ),
        commit_result(Claim, <<"claim">>, NodeOpts)
    else
        Error -> Error
    end;
read_live(<<"odysee/claim-id/", Encoded/binary>>, StoreOpts, NodeOpts) ->
    maybe
        {ok, ClaimID} ?= decode_component(Encoded),
        {ok, Search} ?=
            hb_ao:raw(
                <<"odysee-claim@1.0">>,
                <<"search">>,
                #{},
                #{ <<"claim_id">> => ClaimID, <<"page_size">> => 1 },
                store_node_opts(StoreOpts, NodeOpts)
            ),
        {ok, Claim} ?= claim_from_search(Search, ClaimID, NodeOpts),
        commit_result(Claim, <<"claim">>, NodeOpts)
    else
        Error -> Error
    end;
read_live(<<"odysee/stream/", Encoded/binary>>, StoreOpts, NodeOpts) ->
    maybe
        {ok, URI} ?= decode_component(Encoded),
        {ok, Stream} ?=
            hb_ao:raw(
                <<"odysee-stream@1.0">>,
                <<"stream">>,
                #{},
                #{ <<"url">> => URI },
                store_node_opts(StoreOpts, NodeOpts)
            ),
        commit_result(Stream, <<"stream">>, NodeOpts)
    else
        Error -> Error
    end;
read_live(<<"odysee/stream-id/", Encoded/binary>>, StoreOpts, NodeOpts) ->
    maybe
        {ok, ClaimID} ?= decode_component(Encoded),
        {ok, Search} ?=
            hb_ao:raw(
                <<"odysee-claim@1.0">>,
                <<"search">>,
                #{},
                #{
                    <<"claim_id">> => ClaimID,
                    <<"claim_type">> => [<<"stream">>],
                    <<"page_size">> => 1
                },
                store_node_opts(StoreOpts, NodeOpts)
            ),
        {ok, Claim} ?= claim_from_search(Search, ClaimID, NodeOpts),
        {ok, Stream} ?=
            hb_ao:raw(
                <<"odysee-stream@1.0">>,
                <<"stream">>,
                Claim,
                #{},
                store_node_opts(StoreOpts, NodeOpts)
            ),
        commit_result(Stream, <<"stream">>, NodeOpts)
    else
        Error -> Error
    end;
read_live(<<"odysee/channel-id/", Encoded/binary>>, StoreOpts, NodeOpts) ->
    read_live(<<"odysee/channel/", Encoded/binary>>, StoreOpts, NodeOpts);
read_live(<<"odysee/channel/", Encoded/binary>>, StoreOpts, NodeOpts) ->
    maybe
        {ok, ChannelID} ?= decode_component(Encoded),
        {ok, Search} ?=
            hb_ao:raw(
                <<"odysee-claim@1.0">>,
                <<"search">>,
                #{},
                #{
                    <<"claim_id">> => ChannelID,
                    <<"claim_type">> => [<<"channel">>],
                    <<"page_size">> => 1
                },
                store_node_opts(StoreOpts, NodeOpts)
            ),
        {ok, Claim} ?= claim_from_search(Search, ChannelID, NodeOpts),
        {ok, Channel} ?=
            hb_ao:raw(
                <<"odysee-channel@1.0">>,
                <<"channel">>,
                #{},
                #{ <<"claim">> => Claim },
                store_node_opts(StoreOpts, NodeOpts)
            ),
        commit_result(Channel, <<"channel">>, NodeOpts)
    else
        Error -> Error
    end;
read_live(<<"odysee/claim-proof/", Rest/binary>>, StoreOpts, NodeOpts) ->
    maybe
        {ok, TxID, NOut} ?= claim_proof_path(Rest),
        {ok, Transaction} ?=
            hb_ao:raw(
                <<"odysee-claim@1.0">>,
                <<"transaction">>,
                #{},
                #{ <<"txid">> => TxID },
                store_node_opts(StoreOpts, NodeOpts)
            ),
        {ok, Proof} ?=
            hb_ao:raw(
                <<"odysee-claim-proof@1.0">>,
                <<"verify">>,
                Transaction,
                #{ <<"txid">> => TxID, <<"nout">> => NOut },
                store_node_opts(StoreOpts, NodeOpts)
            ),
        ok ?= require_valid_proof(Proof, NodeOpts),
        commit_result(Proof, <<"claim-proof">>, NodeOpts)
    else
        Error -> Error
    end;
read_live(<<"odysee/transaction/", Encoded/binary>>, StoreOpts, NodeOpts) ->
    maybe
        {ok, TxID0} ?= decode_component(Encoded),
        TxID = normalize_hex(TxID0),
        ok ?= require_hex_size(TxID, 64, invalid_txid),
        {ok, Transaction} ?=
            hb_ao:raw(
                <<"odysee-claim@1.0">>,
                <<"transaction">>,
                #{},
                #{ <<"txid">> => TxID },
                store_node_opts(StoreOpts, NodeOpts)
            ),
        {ok, Msg} ?= transaction_message(Transaction, TxID, NodeOpts),
        commit_result(Msg, <<"transaction">>, NodeOpts)
    else
        Error -> Error
    end;
read_live(<<"odysee/stream-descriptor/", SDHash/binary>>, StoreOpts, NodeOpts) ->
    read_live(<<"odysee/descriptor/", SDHash/binary>>, StoreOpts, NodeOpts);
read_live(<<"odysee/descriptor-id/", SDHash/binary>>, StoreOpts, NodeOpts) ->
    read_live(<<"odysee/descriptor/", SDHash/binary>>, StoreOpts, NodeOpts);
read_live(<<"odysee/descriptor/", SDHash/binary>>, StoreOpts, NodeOpts) ->
    maybe
        {ok, Desc} ?=
            hb_ao:raw(
                <<"odysee-stream-descriptor@1.0">>,
                <<"fetch">>,
                #{},
                #{ <<"sd-hash">> => SDHash },
                store_node_opts(StoreOpts, NodeOpts)
            ),
        commit_result(Desc, <<"stream-descriptor">>, NodeOpts)
    else
        Error -> Error
    end;
read_live(<<"odysee/comment-id/", Encoded/binary>>, StoreOpts, NodeOpts) ->
    read_live(<<"odysee/comment/", Encoded/binary>>, StoreOpts, NodeOpts);
read_live(<<"odysee/comment/", Encoded/binary>>, StoreOpts, NodeOpts) ->
    maybe
        {ok, CommentID} ?= decode_component(Encoded),
        {ok, Comment} ?=
            hb_ao:raw(
                <<"odysee-comment@1.0">>,
                <<"by-id">>,
                #{},
                #{ <<"comment-id">> => CommentID },
                store_node_opts(StoreOpts, NodeOpts)
            ),
        commit_result(Comment, <<"comment">>, NodeOpts)
    else
        Error -> Error
    end;
read_live(<<"odysee/comment-reaction/", Encoded/binary>>, StoreOpts, NodeOpts) ->
    maybe
        {ok, CommentID} ?= decode_component(Encoded),
        {ok, Reaction} ?=
            hb_ao:raw(
                <<"odysee-reaction@1.0">>,
                <<"list">>,
                #{},
                #{ <<"comment-ids">> => CommentID },
                store_node_opts(StoreOpts, NodeOpts)
            ),
        commit_result(enrich_surface(<<"odysee/comment-reaction/", CommentID/binary>>, <<"comment-reaction">>, Reaction), <<"comment-reaction">>, NodeOpts)
    else
        Error -> Error
    end;
read_live(<<"odysee/file-view-count/", Encoded/binary>>, StoreOpts, NodeOpts) ->
    maybe
        {ok, ClaimID} ?= decode_component(Encoded),
        {ok, Counts} ?=
            hb_ao:raw(
                <<"odysee-file@1.0">>,
                <<"view-count">>,
                #{},
                #{ <<"claim-id">> => ClaimID },
                store_node_opts(StoreOpts, NodeOpts)
            ),
        commit_result(enrich_surface(<<"odysee/file-view-count/", ClaimID/binary>>, <<"file-view-count">>, Counts), <<"file-view-count">>, NodeOpts)
    else
        Error -> Error
    end;
read_live(<<"odysee/file-reaction/", Encoded/binary>>, StoreOpts, NodeOpts) ->
    maybe
        {ok, ClaimID} ?= decode_component(Encoded),
        {ok, Reaction} ?=
            hb_ao:raw(
                <<"odysee-file-reaction@1.0">>,
                <<"list">>,
                #{},
                #{ <<"claim-ids">> => ClaimID },
                store_node_opts(StoreOpts, NodeOpts)
            ),
        commit_result(enrich_surface(<<"odysee/file-reaction/", ClaimID/binary>>, <<"file-reaction">>, Reaction), <<"file-reaction">>, NodeOpts)
    else
        Error -> Error
    end;
read_live(<<"odysee/subscription-count/", Encoded/binary>>, StoreOpts, NodeOpts) ->
    maybe
        {ok, ClaimID} ?= decode_component(Encoded),
        {ok, Counts} ?=
            hb_ao:raw(
                <<"odysee-subscription@1.0">>,
                <<"sub-count">>,
                #{},
                #{ <<"claim-id">> => ClaimID },
                store_node_opts(StoreOpts, NodeOpts)
            ),
        commit_result(enrich_surface(<<"odysee/subscription-count/", ClaimID/binary>>, <<"subscription-count">>, Counts), <<"subscription-count">>, NodeOpts)
    else
        Error -> Error
    end;
read_live(<<"odysee/blob-id/", Encoded/binary>>, StoreOpts, NodeOpts) ->
    read_live(<<"odysee/blob/", Encoded/binary>>, StoreOpts, NodeOpts);
read_live(<<"odysee/blob/", Encoded/binary>>, StoreOpts, NodeOpts) ->
    maybe
        {ok, BlobHash0} ?= decode_component(Encoded),
        BlobHash = normalize_hex(BlobHash0),
        ok ?= require_sha384_hex(BlobHash),
        {ok, Body} ?= fetch_blob(BlobHash, StoreOpts, NodeOpts),
        commit_result(blob_message(BlobHash, Body), <<"blob">>, NodeOpts)
    else
        Error -> Error
    end;
read_live(_Path, _StoreOpts, _NodeOpts) ->
    {error, not_found}.

fixture(Path, StoreOpts, Opts) ->
    Fixtures = hb_maps:get(<<"fixtures">>, StoreOpts, #{}, Opts),
    case hb_maps:get(Path, Fixtures, not_found, Opts) of
        not_found -> not_found;
        Msg when is_map(Msg) -> {ok, Msg};
        Msg -> {ok, hb_cache:ensure_all_loaded(Msg, Opts)}
    end.

commit_result(Msg0, Type, Opts)
        when is_map(Msg0), Type =:= <<"blob">>;
        is_map(Msg0), Type =:= <<"stream-descriptor">>;
        is_map(Msg0), Type =:= <<"transaction">> ->
    native_source_message(Type, Msg0, Opts);
commit_result(Msg0, <<"claim-proof">> = Type, Opts) when is_map(Msg0) ->
    case native_claim_output_message(Msg0, Opts) of
        {ok, Msg} -> {ok, Msg};
        {error, not_native_claim_output} -> commit_surface_result(Msg0, Type, Opts);
        Error -> Error
    end;
commit_result(Msg0, Type, Opts) when is_map(Msg0) ->
    commit_surface_result(Msg0, Type, Opts);
commit_result(Bin, _Type, _Opts) when is_binary(Bin) ->
    {ok, Bin}.

commit_surface_result(Msg0, Type, Opts) ->
    Msg = source_message(Type, Msg0),
    CommitmentDevice = commitment_device(Type),
    case has_commitment_device(Msg, CommitmentDevice, Opts)
        andalso hb_message:verify(
            Msg,
            #{ <<"committers">> => <<"none">>, <<"commitment-ids">> => <<"all">> },
            Opts
        )
    of
        true ->
            committed_surface(Msg, Opts);
        false ->
            case hb_ao:raw(CommitmentDevice, <<"commit">>, Msg, #{ <<"type">> => Type }, Opts) of
                {ok, Committed} -> committed_surface(Committed, Opts);
                Error -> Error
            end
    end.

committed_surface(Msg, Opts) ->
    hb_message:with_only_committed(Msg, Opts).

commitment_device(<<"blob">>) ->
    ?LBRY_BLOB_COMMITMENT_DEVICE;
commitment_device(<<"stream-descriptor">>) ->
    ?LBRY_STREAM_DESCRIPTOR_COMMITMENT_DEVICE;
commitment_device(<<"claim-proof">>) ->
    ?LBRY_CLAIM_OUTPUT_COMMITMENT_DEVICE;
commitment_device(<<"transaction">>) ->
    ?LBRY_TRANSACTION_COMMITMENT_DEVICE;
commitment_device(_Type) ->
    ?ODYSEE_COMMITMENT_DEVICE.

source_message(<<"blob">>, Msg) ->
    Msg#{ <<"device">> => ?LBRY_BLOB_COMMITMENT_DEVICE };
source_message(<<"stream-descriptor">>, Msg) ->
    Msg#{ <<"device">> => ?LBRY_STREAM_DESCRIPTOR_COMMITMENT_DEVICE };
source_message(<<"claim-proof">>, Msg) ->
    Msg#{ <<"device">> => ?LBRY_CLAIM_OUTPUT_COMMITMENT_DEVICE };
source_message(<<"transaction">>, Msg) ->
    Msg#{ <<"device">> => ?LBRY_TRANSACTION_COMMITMENT_DEVICE };
source_message(_Type, Msg) ->
    Msg.

enrich_surface(<<"odysee/comment-reaction/", CommentID/binary>> = Path, <<"comment-reaction">>, Msg) ->
    Msg#{
        <<"comment-id">> => CommentID,
        <<"comment-reaction-store-path">> => Path
    };
enrich_surface(<<"odysee/file-view-count/", ClaimID/binary>> = Path, <<"file-view-count">>, Msg) ->
    Msg#{
        <<"claim-id">> => ClaimID,
        <<"file-view-count-store-path">> => Path
    };
enrich_surface(<<"odysee/file-reaction/", ClaimID/binary>> = Path, <<"file-reaction">>, Msg) ->
    Msg#{
        <<"claim-id">> => ClaimID,
        <<"file-reaction-store-path">> => Path
    };
enrich_surface(<<"odysee/subscription-count/", ClaimID/binary>> = Path, <<"subscription-count">>, Msg) ->
    Msg#{
        <<"claim-id">> => ClaimID,
        <<"subscription-count-store-path">> => Path
    };
enrich_surface(_Path, _Type, Msg) ->
    Msg.

has_commitment_device(Msg, Device, Opts) ->
    Commitments = hb_maps:get(<<"commitments">>, Msg, #{}, Opts),
    lists:any(
        fun(Commitment) ->
            hb_maps:get(<<"commitment-device">>, Commitment, not_found, Opts)
                =:= Device
        end,
        maps:values(Commitments)
    ).

infer_type(<<"odysee/claim/", _/binary>>, _Msg, _Opts) ->
    <<"claim">>;
infer_type(<<"odysee/claim-id/", _/binary>>, _Msg, _Opts) ->
    <<"claim">>;
infer_type(<<"odysee/stream/", _/binary>>, _Msg, _Opts) ->
    <<"stream">>;
infer_type(<<"odysee/stream-id/", _/binary>>, _Msg, _Opts) ->
    <<"stream">>;
infer_type(<<"odysee/channel-id/", _/binary>>, _Msg, _Opts) ->
    <<"channel">>;
infer_type(<<"odysee/channel/", _/binary>>, _Msg, _Opts) ->
    <<"channel">>;
infer_type(<<"odysee/claim-proof/", _/binary>>, _Msg, _Opts) ->
    <<"claim-proof">>;
infer_type(<<"odysee/transaction/", _/binary>>, _Msg, _Opts) ->
    <<"transaction">>;
infer_type(<<"odysee/stream-descriptor/", _/binary>>, _Msg, _Opts) ->
    <<"stream-descriptor">>;
infer_type(<<"odysee/descriptor-id/", _/binary>>, _Msg, _Opts) ->
    <<"stream-descriptor">>;
infer_type(<<"odysee/descriptor/", _/binary>>, _Msg, _Opts) ->
    <<"stream-descriptor">>;
infer_type(<<"odysee/comment-id/", _/binary>>, _Msg, _Opts) ->
    <<"comment">>;
infer_type(<<"odysee/comment/", _/binary>>, _Msg, _Opts) ->
    <<"comment">>;
infer_type(<<"odysee/comment-reaction/", _/binary>>, _Msg, _Opts) ->
    <<"comment-reaction">>;
infer_type(<<"odysee/file-view-count/", _/binary>>, _Msg, _Opts) ->
    <<"file-view-count">>;
infer_type(<<"odysee/file-reaction/", _/binary>>, _Msg, _Opts) ->
    <<"file-reaction">>;
infer_type(<<"odysee/subscription-count/", _/binary>>, _Msg, _Opts) ->
    <<"subscription-count">>;
infer_type(<<"odysee/blob-id/", _/binary>>, _Msg, _Opts) ->
    <<"blob">>;
infer_type(<<"odysee/blob/", _/binary>>, _Msg, _Opts) ->
    <<"blob">>;
infer_type(_Path, Msg, Opts) when is_map(Msg) ->
    case hb_maps:get(<<"device">>, Msg, not_found, Opts) of
        <<"odysee-claim@1.0">> -> <<"claim">>;
        <<"odysee-stream@1.0">> -> <<"stream">>;
        <<"odysee-stream-descriptor@1.0">> -> <<"stream-descriptor">>;
        <<"lbry-stream-descriptor@1.0">> -> <<"stream-descriptor">>;
        <<"odysee-channel@1.0">> -> <<"channel">>;
        <<"odysee-comment@1.0">> -> <<"comment">>;
        <<"odysee-reaction@1.0">> -> <<"comment-reaction">>;
        <<"odysee-file@1.0">> -> <<"file-view-count">>;
        <<"odysee-file-reaction@1.0">> -> <<"file-reaction">>;
        <<"odysee-subscription@1.0">> -> <<"subscription-count">>;
        <<"odysee-blob@1.0">> -> <<"blob">>;
        <<"lbry-blob@1.0">> -> <<"blob">>;
        <<"odysee-claim-proof@1.0">> -> <<"claim-proof">>;
        <<"lbry-claim@1.0">> -> <<"claim-proof">>;
        <<"lbry-claim-output@1.0">> -> <<"claim-proof">>;
        <<"lbry-transaction@1.0">> -> <<"transaction">>;
        _ -> <<"source">>
    end;
infer_type(_Path, _Msg, _Opts) ->
    <<"source">>.

claim_from_search(Search, ClaimID, Opts) ->
    Claims = hb_maps:get(<<"claims">>, Search, [], Opts),
    Matches = [
        Claim
    ||
        Claim <- Claims,
        hb_maps:get(<<"claim-id">>, Claim, not_found, Opts) =:= ClaimID
    ],
    case Matches of
        [Claim | _] -> {ok, Claim};
        [] -> {error, claim_not_found}
    end.

blob_message(BlobHash, Body) ->
    (hb_lbry_commitment:blob_message(BlobHash, Body))#{
        <<"content-type">> => <<"application/octet-stream">>,
        <<"blob-store-path">> => <<"odysee/blob/", BlobHash/binary>>,
        <<"blob-size">> => byte_size(Body)
    }.

transaction_message(Transaction, TxID, Opts) ->
    maybe
        TxHex = hb_maps:get(<<"tx-hex">>, Transaction, not_found, Opts),
        true ?= is_binary(TxHex),
        {ok, Raw} ?= decode_tx_hex(TxHex),
        {ok, Msg} ?= native_transaction_message(Raw, TxID),
        {ok, Msg#{
            <<"content-type">> => <<"application/vnd.lbry.transaction">>,
            <<"tx-size">> => byte_size(Raw),
            <<"tx-store-path">> => <<"odysee/transaction/", TxID/binary>>
        }}
    else
        false -> {error, tx_hex_not_found};
        not_found -> {error, tx_hex_not_found};
        Other -> Other
    end.

native_source_message(<<"blob">>, Msg, Opts) ->
    maybe
        Hash = hb_maps:get(<<"blob-hash">>, Msg, not_found, Opts),
        true ?= is_binary(Hash),
        Body = native_bytes([<<"data">>, <<"body">>], Msg, Opts),
        true ?= is_binary(Body),
        {ok, Body} ?= verify_blob_body(normalize_hex(Hash), Body),
        {ok, blob_message(normalize_hex(Hash), Body)}
    else
        false -> {error, invalid_blob};
        not_found -> {error, invalid_blob};
        Error -> Error
    end;
native_source_message(<<"stream-descriptor">>, Msg, Opts) ->
    maybe
        SDHash = hb_maps:get(<<"sd-hash">>, Msg, not_found, Opts),
        true ?= is_binary(SDHash),
        Raw = native_bytes([<<"raw">>, <<"body">>], Msg, Opts),
        true ?= is_binary(Raw),
        {ok, Descriptor} ?=
            hb_lbry_commitment:descriptor_message(Raw, normalize_hex(SDHash)),
        {ok, Descriptor#{
            <<"content-type">> => <<"application/vnd.lbry.stream-descriptor+json">>,
            <<"descriptor-store-path">> => <<"odysee/descriptor/", (normalize_hex(SDHash))/binary>>
        }}
    else
        false -> {error, invalid_stream_descriptor};
        not_found -> {error, invalid_stream_descriptor};
        Error -> Error
    end;
native_source_message(<<"transaction">>, Msg, Opts) ->
    maybe
        TxID = hb_maps:get(<<"txid">>, Msg, not_found, Opts),
        true ?= is_binary(TxID),
        {ok, Raw} ?= transaction_bytes(Msg, Opts),
        {ok, TxMsg} ?= native_transaction_message(Raw, normalize_hex(TxID)),
        {ok, TxMsg#{
            <<"content-type">> => <<"application/vnd.lbry.transaction">>,
            <<"tx-size">> => byte_size(Raw),
            <<"tx-store-path">> => <<"odysee/transaction/", (normalize_hex(TxID))/binary>>
        }}
    else
        false -> {error, invalid_transaction};
        not_found -> {error, invalid_transaction};
        Error -> Error
    end.

native_claim_output_message(Msg, Opts) ->
    case hb_maps:get(<<"device">>, Msg, not_found, Opts) of
        <<"lbry-claim@1.0">> ->
            case
                hb_message:verify(
                    Msg,
                    #{ <<"commitment-ids">> => <<"all">> },
                    Opts
                )
            of
                true -> {ok, Msg};
                false -> {error, invalid_claim_output}
            end;
        _ ->
            {error, not_native_claim_output}
    end.

native_bytes([], _Msg, _Opts) ->
    not_found;
native_bytes([Key | Rest], Msg, Opts) ->
    case hb_maps:get(Key, Msg, not_found, Opts) of
        Bytes when is_binary(Bytes) -> Bytes;
        _ -> native_bytes(Rest, Msg, Opts)
    end.

transaction_bytes(Msg, Opts) ->
    case native_bytes([<<"raw">>, <<"body">>], Msg, Opts) of
        Raw when is_binary(Raw) ->
            {ok, Raw};
        _ ->
            case first_hex([<<"tx-hex">>, <<"hex">>, <<"raw-hex">>], Msg, Opts) of
                Hex when is_binary(Hex) -> decode_tx_hex(Hex);
                _ -> {error, missing_raw_transaction}
            end
    end.

first_hex([], _Msg, _Opts) ->
    not_found;
first_hex([Key | Rest], Msg, Opts) ->
    case hb_maps:get(Key, Msg, not_found, Opts) of
        Hex when is_binary(Hex) -> Hex;
        _ -> first_hex(Rest, Msg, Opts)
    end.

native_transaction_message(Raw, TxID) ->
    maybe
        {ok, Msg} ?= hb_lbry_commitment:transaction_message(Raw),
        TxID ?= hb_maps:get(<<"txid">>, Msg, not_found, #{}),
        {ok, Msg}
    else
        Other -> Other
    end.

fetch_blob(BlobHash, StoreOpts, NodeOpts) ->
    Opts = store_node_opts(StoreOpts, NodeOpts),
    fetch_blob(BlobHash, blob_urls(BlobHash, Opts), Opts, []).

fetch_blob(BlobHash, [], _Opts, Errors) ->
    {error, {blob_fetch_failed, BlobHash, lists:reverse(Errors)}};
fetch_blob(BlobHash, [URL | Rest], Opts, Errors) ->
    case fetch_blob_url(BlobHash, URL, Opts) of
        {ok, _Body} = OK -> OK;
        Error -> fetch_blob(BlobHash, Rest, Opts, [{URL, Error} | Errors])
    end.

fetch_blob_url(BlobHash, URL, Opts) ->
    case hb_http:request(#{ <<"method">> => <<"GET">>, <<"path">> => URL }, Opts) of
        {ok, #{ <<"status">> := Status, <<"body">> := Body }}
                when is_integer(Status), Status >= 200, Status < 300, is_binary(Body) ->
            verify_blob_body(BlobHash, Body);
        {ok, #{ <<"body">> := Body }} when is_binary(Body) ->
            verify_blob_body(BlobHash, Body);
        {ok, Body} when is_binary(Body) ->
            verify_blob_body(BlobHash, Body);
        {ok, Other} ->
            {error, {blob_response_without_body, Other}};
        Error ->
            Error
    end.

verify_blob_body(BlobHash, Body) ->
    case sha384_hex(Body) of
        BlobHash -> {ok, Body};
        Other -> {error, {blob_hash_mismatch, BlobHash, Other}}
    end.

claim_proof_path(Rest) ->
    case binary:split(Rest, <<"/">>) of
        [EncodedTxID, EncodedNOut] ->
            maybe
                {ok, TxID0} ?= decode_component(EncodedTxID),
                TxID = normalize_hex(TxID0),
                ok ?= require_hex_size(TxID, 64, invalid_txid),
                {ok, NOutBin} ?= decode_component(EncodedNOut),
                {ok, NOut} ?= non_negative_integer(NOutBin),
                {ok, TxID, NOut}
            end;
        _ ->
            {error, invalid_claim_proof_path}
    end.

require_valid_proof(Proof, Opts) ->
    case hb_maps:get(<<"valid">>, Proof, false, Opts) of
        true -> ok;
        _ -> {error, invalid_claim_proof}
    end.

blob_urls(BlobHash, Opts) ->
    TemplateURLs = [
        binary:replace(Template, <<"{hash}">>, BlobHash, [global])
    ||
        Template <- opt_values(
            [
                <<"blob-url-template">>,
                <<"blob-url-templates">>,
                <<"lbry-blob-url-template">>,
                <<"lbry-blob-url-templates">>
            ],
            [],
            Opts
        ),
        is_binary(Template)
    ],
    BaseURLs = [
        blob_url(BaseURL, BlobHash)
    ||
        BaseURL <- opt_values(
            [<<"blob-base-url">>, <<"blob-base-urls">>, <<"lbry-blob-base-url">>, <<"lbry-blob-base-urls">>],
            ?DEFAULT_BLOB_BASE_URLS,
            Opts
        ),
        is_binary(BaseURL),
        byte_size(BaseURL) > 0
    ],
    TemplateURLs ++ BaseURLs.

blob_url(BaseURL, BlobHash) ->
    CleanBaseURL =
        case binary:at(BaseURL, byte_size(BaseURL) - 1) of
            $/ -> binary:part(BaseURL, 0, byte_size(BaseURL) - 1);
            _ -> BaseURL
        end,
    <<CleanBaseURL/binary, "/blob?hash=", BlobHash/binary>>.

opt_values([], Default, _Opts) ->
    list_values(Default);
opt_values([Key | Rest], Default, Opts) ->
    case hb_maps:get(Key, Opts, not_found, Opts) of
        not_found -> opt_values(Rest, Default, Opts);
        Value -> list_values(Value)
    end.

list_values(Values) when is_list(Values) ->
    Values;
list_values(Value) ->
    [Value].

require_sha384_hex(Hex) when is_binary(Hex), byte_size(Hex) =:= ?SHA384_HEX_SIZE ->
    ok;
require_sha384_hex(_Hex) ->
    {error, invalid_blob_hash}.

require_hex_size(Hex, Size, _Error) when is_binary(Hex), byte_size(Hex) =:= Size ->
    ok;
require_hex_size(_Hex, _Size, Error) ->
    {error, Error}.

decode_tx_hex(Hex) when is_binary(Hex) ->
    try {ok, binary:decode_hex(normalize_hex(Hex))}
    catch _:_ -> {error, invalid_tx_hex}
    end.

non_negative_integer(Bin) when is_binary(Bin) ->
    try
        Int = binary_to_integer(Bin),
        case Int >= 0 of
            true -> {ok, Int};
            false -> {error, invalid_nout}
        end
    catch _:_ ->
        {error, invalid_nout}
    end.

sha384_hex(Bin) ->
    hb_util:to_hex(crypto:hash(sha384, Bin)).

normalize_hex(Hex) when is_binary(Hex) ->
    hb_util:bin(string:lowercase(binary_to_list(Hex))).

store_node_opts(StoreOpts, NodeOpts) ->
    hb_maps:merge(
        maps:without(
            [
                <<"fixtures">>,
                <<"store-module">>,
                <<"name">>,
                <<"scope">>
            ],
            StoreOpts
        ),
        NodeOpts
    ).

normalize_key(Key) ->
    Path = hb_path:to_binary(Key),
    case Path of
        <<"/", Rest/binary>> -> Rest;
        _ -> Path
    end.

canonical_read_path(Path) ->
    case classify_native_path(Path) of
        {ok, NativePath} -> NativePath;
        _ -> Path
    end.

classify_native_path(<<TxID:64/binary, ":", NOut/binary>>) ->
    case valid_hex_size(TxID, 32) andalso valid_uint(NOut) of
        true -> {ok, <<"odysee/claim-proof/", TxID/binary, "/", NOut/binary>>};
        false -> not_found
    end;
classify_native_path(Path) ->
    case {valid_hex_size(Path, 48), valid_hex_size(Path, 32)} of
        {true, _} -> {ok, <<"odysee/blob/", Path/binary>>};
        {_, true} -> {ok, <<"odysee/transaction/", Path/binary>>};
        _ -> not_found
    end.

valid_hex_size(Hex, Bytes) when is_binary(Hex), byte_size(Hex) =:= Bytes * 2 ->
    try binary:decode_hex(Hex) of
        Decoded -> byte_size(Decoded) =:= Bytes
    catch
        _:_ -> false
    end;
valid_hex_size(_Hex, _Bytes) ->
    false.

valid_uint(Bin) when is_binary(Bin), byte_size(Bin) > 0 ->
    try binary_to_integer(Bin) of
        Int -> Int >= 0
    catch
        _:_ -> false
    end;
valid_uint(_Bin) ->
    false.

decode_component(Encoded) ->
    try {ok, hb_util:bin(uri_string:percent_decode(Encoded))}
    catch _:_ -> {error, invalid_odysee_store_path}
    end.

bare_sha384_read_returns_native_blob_test() ->
    Bytes = <<"encrypted blob payload">>,
    Hash = hb_lbry_stream_descriptor:blob_hash(Bytes),
    Store = #{
        <<"store-module">> => ?MODULE,
        <<"fixtures">> => #{
            <<"odysee/blob/", Hash/binary>> => #{
                <<"device">> => <<"odysee-blob@1.0">>,
                <<"body">> => Bytes,
                <<"blob-hash">> => Hash
            }
        }
    },
    {ok, Msg} = read(Store, #{ <<"read">> => Hash }, #{}),
    ?assertEqual(<<"lbry-blob@1.0">>, maps:get(<<"device">>, Msg)),
    ?assertEqual(Hash, maps:get(<<"blob-hash">>, Msg)),
    ?assertEqual(Bytes, maps:get(<<"data">>, Msg)),
    ?assertEqual(
        hb_lbry_commitment:content_digest_sha384(Bytes),
        maps:get(<<"content-digest">>, Msg)
    ),
    ?assertEqual(
        true,
        hb_message:verify(Msg, #{ <<"commitment-ids">> => <<"all">> }, #{})
    ).

direct_sha384_get_returns_native_blob_test() ->
    Bytes = <<"encrypted blob payload">>,
    Hash = hb_lbry_stream_descriptor:blob_hash(Bytes),
    Store = #{
        <<"store-module">> => ?MODULE,
        <<"fixtures">> => #{
            <<"odysee/blob/", Hash/binary>> => #{
                <<"device">> => <<"odysee-blob@1.0">>,
                <<"body">> => Bytes,
                <<"blob-hash">> => Hash
            }
        }
    },
    {ok, Msg} =
        hb_ao:resolve(
            #{ <<"path">> => <<"/", Hash/binary>> },
            #{ <<"store">> => [Store] }
        ),
    ?assertEqual(<<"lbry-blob@1.0">>, maps:get(<<"device">>, Msg)),
    ?assertEqual(Hash, maps:get(<<"blob-hash">>, Msg)),
    ?assertEqual(Bytes, maps:get(<<"data">>, Msg)).

direct_sha384_http_get_exposes_native_signature_input_test() ->
    application:ensure_all_started(inets),
    Bytes = <<"encrypted blob payload">>,
    Hash = hb_lbry_stream_descriptor:blob_hash(Bytes),
    Store = #{
        <<"store-module">> => ?MODULE,
        <<"fixtures">> => #{
            <<"odysee/blob/", Hash/binary>> => #{
                <<"device">> => <<"odysee-blob@1.0">>,
                <<"body">> => Bytes,
                <<"blob-hash">> => Hash
            }
        }
    },
    Node = hb_http_server:start_node(#{ <<"store">> => [Store] }),
    URL = binary_to_list(<<Node/binary, Hash/binary>>),
    {ok, {{_, 200, _}, Headers, Body}} =
        httpc:request(get, {URL, []}, [], [{body_format, binary}]),
    ?assertEqual(Bytes, Body),
    SignatureInput = http_header(<<"signature-input">>, Headers),
    ?assertNotEqual(not_found, SignatureInput),
    ?assertNotEqual(
        nomatch,
        binary:match(SignatureInput, <<"\"content-digest\"">>)
    ),
    ?assertNotEqual(
        nomatch,
        binary:match(SignatureInput, <<"alg=\"lbry-blob@1.0/sha-384\"">>)
    ),
    ?assertNotEqual(
        nomatch,
        binary:match(SignatureInput, <<"native-id=\"", Hash/binary, "\"">>)
    ).

bare_txid_read_returns_native_transaction_test() ->
    Raw = binary:decode_hex(hb_lbry_tx:task0_tx_hex()),
    {ok, TxMsg} = hb_lbry_commitment:transaction_message(Raw),
    TxID = maps:get(<<"txid">>, TxMsg),
    Store = #{
        <<"store-module">> => ?MODULE,
        <<"fixtures">> => #{
            <<"odysee/transaction/", TxID/binary>> => TxMsg
        }
    },
    {ok, Msg} = read(Store, #{ <<"read">> => TxID }, #{}),
    ?assertEqual(<<"lbry-transaction@1.0">>, maps:get(<<"device">>, Msg)),
    ?assertEqual(TxID, maps:get(<<"txid">>, Msg)),
    ?assertEqual(Raw, maps:get(<<"raw">>, Msg)),
    ?assertEqual(
        true,
        hb_message:verify(Msg, #{ <<"commitment-ids">> => <<"all">> }, #{})
    ).

direct_txid_get_returns_native_transaction_test() ->
    Raw = binary:decode_hex(hb_lbry_tx:task0_tx_hex()),
    {ok, TxMsg} = hb_lbry_commitment:transaction_message(Raw),
    TxID = maps:get(<<"txid">>, TxMsg),
    Store = #{
        <<"store-module">> => ?MODULE,
        <<"fixtures">> => #{
            <<"odysee/transaction/", TxID/binary>> => TxMsg
        }
    },
    {ok, Msg} =
        hb_ao:resolve(
            #{ <<"path">> => <<"/", TxID/binary>> },
            #{ <<"store">> => [Store] }
    ),
    ?assertEqual(<<"lbry-transaction@1.0">>, maps:get(<<"device">>, Msg)),
    ?assertEqual(TxID, maps:get(<<"txid">>, Msg)),
    ?assertEqual(Raw, maps:get(<<"raw">>, Msg)).

direct_txid_http_get_exposes_native_signature_input_test() ->
    application:ensure_all_started(inets),
    Raw = binary:decode_hex(hb_lbry_tx:task0_tx_hex()),
    {ok, TxMsg} = hb_lbry_commitment:transaction_message(Raw),
    TxID = maps:get(<<"txid">>, TxMsg),
    Store = #{
        <<"store-module">> => ?MODULE,
        <<"fixtures">> => #{
            <<"odysee/transaction/", TxID/binary>> => TxMsg
        }
    },
    Node = hb_http_server:start_node(#{ <<"store">> => [Store] }),
    URL = binary_to_list(<<Node/binary, TxID/binary>>),
    {ok, {{_, 200, _}, Headers, _Body}} =
        httpc:request(get, {URL, []}, [], [{body_format, binary}]),
    SignatureInput = http_header(<<"signature-input">>, Headers),
    ?assertNotEqual(not_found, SignatureInput),
    ?assertNotEqual(
        nomatch,
        binary:match(SignatureInput, <<"alg=\"lbry-transaction@1.0/sha-256d\"">>)
    ),
    ?assertNotEqual(
        nomatch,
        binary:match(SignatureInput, <<"native-id=\"", TxID/binary, "\"">>)
    ).

direct_outpoint_get_returns_native_claim_output_test() ->
    Raw = binary:decode_hex(hb_lbry_tx:task0_tx_hex()),
    TxID = hb_lbry_tx:txid(Raw),
    Outpoint = <<TxID/binary, ":0">>,
    {ok, ClaimOutput} = hb_lbry_commitment:claim_output_message(Raw, 0),
    Store = #{
        <<"store-module">> => ?MODULE,
        <<"fixtures">> => #{
            <<"odysee/claim-proof/", TxID/binary, "/0">> => ClaimOutput
        }
    },
    ?assertEqual(
        <<"odysee/claim-proof/", TxID/binary, "/0">>,
        canonical_read_path(Outpoint)
    ),
    {ok, StoreMsg} = read(Store, #{ <<"read">> => Outpoint }, #{}),
    ?assertEqual(<<"lbry-claim@1.0">>, maps:get(<<"device">>, StoreMsg)),
    {ok, Msg} =
        hb_ao:resolve(
            #{ <<"path">> => <<"/", Outpoint/binary>> },
            #{ <<"store">> => [Store] }
        ),
    ?assertEqual(<<"lbry-claim@1.0">>, maps:get(<<"device">>, Msg)),
    ?assertEqual(TxID, maps:get(<<"txid">>, Msg)),
    ?assertEqual(0, maps:get(<<"nout">>, Msg)),
    ?assertEqual(
        true,
        hb_message:verify(Msg, #{ <<"commitment-ids">> => <<"all">> }, #{})
    ).

http_header(Name, Headers) ->
    LowerName = hb_util:bin(string:lowercase(hb_util:bin(Name))),
    case [
        hb_util:bin(Value)
     ||
        {Key, Value} <- Headers,
        hb_util:bin(string:lowercase(hb_util:bin(Key))) == LowerName
    ] of
        [Value | _] -> Value;
        [] -> not_found
    end.
