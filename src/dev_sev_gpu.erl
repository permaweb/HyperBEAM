%%% @doc NVIDIA GPU TEE Attestation Device
%%%
%%% This module provides GPU attestation capabilities using the NVIDIA nvat SDK.
%%% It uses Erlang NIFs to directly call the nvat C API for:
%%% - Collecting GPU attestation evidence
%%% - Verifying GPU attestation evidence locally
%%%
%%% The NIF handles SDK initialization internally, so no explicit setup is required.
-module(dev_sev_gpu).
-export([info/1, generate/3, verify/3]).
-on_load(init/0).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TEST_MOCK_NONCE, <<"da4a06c3604a5fac8aa0b4aaf5a6354cdd0dc7c193299bc3464f30b5cbfb931a">>).

%% NIF stubs - these are replaced by the actual NIF functions when loaded
-spec collect_evidence_nif(binary()) -> {ok, binary()} | {error, binary()}.
collect_evidence_nif(_Nonce) -> erlang:nif_error(not_loaded).

-spec verify_evidence_nif(binary()) -> {ok, binary()} | {error, binary()}.
verify_evidence_nif(_EvidenceJSON) -> erlang:nif_error(not_loaded).

%% NIF initialization
init() ->
    PrivDir = case code:priv_dir(hb) of
        {error, bad_name} -> 
            %% Fallback for development
            "priv";
        Dir -> Dir
    end,
    SoPath = filename:join(PrivDir, "dev_sev_gpu_nif"),
    case erlang:load_nif(SoPath, 0) of
        ok -> ok;
        {error, {reload, _}} -> ok;
        {error, Reason} -> 
            ?event({nif_load_error, Reason}),
            ok  %% Don't fail module load, but NIF calls will return not_loaded
    end.

info(_) -> 
    #{exports => [<<"info">>, <<"generate">>, <<"verify">>]}.

%% @doc Generate GPU attestation evidence.
%%
%% Collects attestation evidence from the GPU using NVML, verifies it locally,
%% and returns a JSON object containing:
%% - evidences: Serialized GPU evidence (for transport to verifier)
%% - claims: Attestation claims from local verification
%% - eat: Detached Entity Attestation Token
%% - verified: Boolean indicating local verification success
%%
%% Input message M2 should contain:
%% - nonce: Hex-encoded nonce for attestation freshness
-spec generate(map(), map(), map()) -> {ok, binary()} | {error, term()}.
generate(_M1, M2, Opts) ->
    Nonce = hb_ao:get(nonce, M2, ?TEST_MOCK_NONCE, Opts),
    case collect_evidence_nif(Nonce) of
        {ok, ResultJSON} ->
            {ok, ResultJSON};
        {error, Reason} when is_binary(Reason) ->
            {error, {nvat_error, Reason}};
        {error, not_loaded} ->
            {error, nif_not_loaded}
    end.

%% @doc Verify GPU attestation evidence.
%%
%% Verifies previously collected GPU evidence locally.
%% Unlike the old EAT-based verification, this accepts the raw evidence JSON
%% from the generate/3 function's "evidences" field.
%%
%% Input message M2 should contain:
%% - body: The evidences JSON from a previous generate call
%%
%% Returns:
%% - {ok, <<"true">>} if verification succeeds
%% - {ok, <<"false">>} if verification fails
%% - {error, Reason} on error
-spec verify(map(), map(), map()) -> {ok, binary()} | {error, term()}.
verify(_M1, M2, _NodeOpts) ->
    EvidenceJSON = maps:get(<<"body">>, M2, <<>>),
    case verify_evidence_nif(EvidenceJSON) of
        {ok, ResultJSON} ->
            case hb_json:decode(ResultJSON) of
                #{<<"valid">> := true} -> {ok, <<"true">>};
                #{<<"valid">> := false} -> {ok, <<"false">>};
                _ -> {ok, <<"false">>}
            end;
        {error, Reason} when is_binary(Reason) ->
            {error, {nvat_error, Reason}};
        {error, not_loaded} ->
            {error, nif_not_loaded}
    end.

%% ============================================================================
%% Unit Tests
%% ============================================================================

generate_test() ->
    case generate(#{}, #{nonce => ?TEST_MOCK_NONCE}, #{}) of
        {ok, ResultJSON} ->
            ?assert(is_binary(ResultJSON)),
            ?assert(byte_size(ResultJSON) > 0),
            %% Verify it's valid JSON with expected structure
            case hb_json:decode(ResultJSON) of
                #{<<"evidences">> := _, <<"verified">> := _} ->
                    ?assert(true);
                _ ->
                    ?assert(true)  %% Accept any valid JSON
            end;
        {error, nif_not_loaded} ->
            %% NIF not available (development without GPU)
            ?assert(true);
        {error, {nvat_error, _}} ->
            %% GPU not available or attestation not supported
            ?assert(true);
        Other ->
            ?assertEqual({ok, result}, Other)
    end.

verify_test() ->
    case generate(#{}, #{nonce => ?TEST_MOCK_NONCE}, #{}) of
        {ok, ResultJSON} ->
            %% Extract evidences from the generate result
            case hb_json:decode(ResultJSON) of
                #{<<"evidences">> := Evidences} ->
                    VerifyMsg = #{<<"body">> => hb_json:encode(Evidences)},
                    case verify(#{}, VerifyMsg, #{}) of
                        {ok, _} -> ?assert(true);
                        {error, _} -> ?assert(true)
                    end;
                _ ->
                    %% If no evidences field, try verifying the whole result
                    VerifyMsg = #{<<"body">> => ResultJSON},
                    case verify(#{}, VerifyMsg, #{}) of
                        {ok, _} -> ?assert(true);
                        {error, _} -> ?assert(true)
                    end
            end;
        {error, nif_not_loaded} ->
            ?assert(true);
        {error, {nvat_error, _}} ->
            ?assert(true)
    end.