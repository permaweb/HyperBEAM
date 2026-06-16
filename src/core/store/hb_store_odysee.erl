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
    Path = normalize_key(Key),
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
        Msg -> {ok, hb_cache:ensure_all_loaded(Msg, Opts)}
    end.

commit_result(Msg0, Type, Opts) when is_map(Msg0) ->
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
    end;
commit_result(Bin, _Type, _Opts) when is_binary(Bin) ->
    {ok, Bin}.

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
    #{
        <<"device">> => <<"lbry-blob@1.0">>,
        <<"content-type">> => <<"application/octet-stream">>,
        <<"body">> => Body,
        <<"blob-hash">> => BlobHash,
        <<"blob-store-path">> => <<"odysee/blob/", BlobHash/binary>>,
        <<"blob-size">> => byte_size(Body)
    }.

transaction_message(Transaction, TxID, Opts) ->
    maybe
        TxHex = hb_maps:get(<<"tx-hex">>, Transaction, not_found, Opts),
        true ?= is_binary(TxHex),
        {ok, Raw} ?= decode_tx_hex(TxHex),
        TxID ?= hb_lbry_tx:txid(Raw),
        {ok, _Parsed} ?= hb_lbry_tx:parse(Raw),
        {ok, #{
            <<"device">> => <<"lbry-transaction@1.0">>,
            <<"content-type">> => <<"application/vnd.lbry.transaction">>,
            <<"body">> => Raw,
            <<"txid">> => TxID,
            <<"tx-size">> => byte_size(Raw),
            <<"tx-store-path">> => <<"odysee/transaction/", TxID/binary>>
        }}
    else
        false -> {error, tx_hex_not_found};
        not_found -> {error, tx_hex_not_found};
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

decode_component(Encoded) ->
    try {ok, hb_util:bin(uri_string:percent_decode(Encoded))}
    catch _:_ -> {error, invalid_odysee_store_path}
    end.
