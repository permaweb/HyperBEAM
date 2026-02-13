%%% @doc Nonce generation and validation for SNP commitment reports.
%%%
%%% This module handles the generation and validation of nonces used in
%%% AMD SEV-SNP attestation reports. Nonces bind reports to specific
%%% verification requests by combining the node's address and message ID.
-module(snp_nonce).
-export([generate_nonce/2, report_data_matches/3]).
-include("include/hb.hrl").

%% Type definitions
-type nonce() :: binary().  % Nonce is a binary formed by concatenating address and node message ID

%% @doc Generate the nonce to use in the SNP commitment report.
%%
%% This function creates a unique nonce by concatenating the node's native
%% address and message ID. This nonce is embedded in the hardware attestation
%% report to bind it to a specific verification request.
%%
%% @param RawAddress The node's raw address identifier
%% @param RawNodeMsgID The raw node message identifier
%% @returns A binary nonce formed by concatenating the native address and message ID
-spec generate_nonce(RawAddress :: binary(), RawNodeMsgID :: binary()) -> nonce().
generate_nonce(RawAddress, RawNodeMsgID) ->
    Address = hb_util:native_id(RawAddress),
    NodeMsgID = hb_util:native_id(RawNodeMsgID),
    << Address/binary, NodeMsgID/binary >>.

%% @doc Validate that the report data matches the expected nonce.
%%
%% This function ensures that the nonce in the SNP report was generated
%% using the same address and node message ID that are expected for this
%% verification request.
%%
%% @param Address The node's address used in nonce generation
%% @param NodeMsgID The node message ID used in nonce generation  
%% @param ReportData The actual nonce data from the SNP report
%% @returns `true' if the report data matches the expected nonce, `false' otherwise
-spec report_data_matches(Address :: binary(), NodeMsgID :: binary(), 
    ReportData :: binary()) -> boolean().
report_data_matches(Address, NodeMsgID, ReportData) ->
    ExpectedNonce = generate_nonce(Address, NodeMsgID),
    % Log nonce summary instead of full values for security
    NonceHash = crypto:hash(sha256, ExpectedNonce),
    ReportDataHash = crypto:hash(sha256, ReportData),
    ?event(snp_short, {nonce_validation, #{
        expected_nonce_size => byte_size(ExpectedNonce),
        expected_nonce_hash => snp_util:binary_to_hex_string(NonceHash),
        report_data_size => byte_size(ReportData),
        report_data_hash => snp_util:binary_to_hex_string(ReportDataHash),
        match => (ExpectedNonce == ReportData)
    }}),
    ExpectedNonce == ReportData.

