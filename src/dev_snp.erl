%%% @doc This device provides an interface for validating and generating AMD SEV-SNP 
%%% commitment reports.
%%%
%%% AMD SEV-SNP (Secure Encrypted Virtualization - Secure Nested Paging) is a 
%%% hardware-based security technology that provides confidential computing 
%%% capabilities. This module handles the cryptographic validation of attestation 
%%% reports and the generation of commitment reports for trusted execution environments.
%%%
%%% The device supports two main operations:
%%% 1. Verification of remote node attestation reports with comprehensive validation
%%% 2. Generation of local attestation reports for proving node identity and software integrity
-module(dev_snp).
-export([generate/3, verify/3]).
-include("include/hb.hrl").

%% @doc Verify an AMD SEV-SNP commitment report message.
%% Delegates to snp_verification module.
-spec verify(M1 :: term(), M2 :: term(), NodeOpts :: map()) ->
    {ok, boolean()} | {error, term()}.
verify(M1, M2, NodeOpts) ->
    snp_verification:verify(M1, M2, NodeOpts).

%% @doc Generate an AMD SEV-SNP commitment report and emit it as a message.
%% Delegates to snp_generate module.
-spec generate(M1 :: term(), M2 :: term(), Opts :: map()) ->
    {ok, map()} | {error, term()}.
generate(M1, M2, Opts) ->
    snp_generate:generate(M1, M2, Opts).