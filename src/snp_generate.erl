%%% @doc Generation functions for SNP commitment reports.
%%%
%%% This module handles the generation of SNP attestation reports, including
%%% wallet validation, nonce generation, report creation, and message packaging.
-module(snp_generate).
-export([generate/3]).
-include("include/hb.hrl").
-include("include/snp_constants.hrl").

%% Type definitions
-type report_message() :: map().  % Report message map with keys: local-hashes, nonce, address, node-message, report

%% Helper function to validate configuration options
-spec validate_generate_config(Opts :: map()) -> {ok, map()} | {error, term()}.
validate_generate_config(Opts) ->
    maybe
        % Validate wallet (required)
        {ok, _} ?= validate_wallet(Opts),
        % Validate snp_trusted (required)
        {ok, _} ?= validate_snp_trusted(Opts),
        {ok, Opts}
    else
        {error, Reason} -> {error, Reason};
        Error -> {error, {config_validation_error, Error}}
    end.

%% Helper function to validate wallet configuration
%% Wallets are tuples: {{KeyType, Priv, Pub}, {KeyType, Pub}}
-spec validate_wallet(Opts :: map()) -> {ok, tuple()} | {error, term()}.
validate_wallet(Opts) ->
    case hb_opts:get(priv_wallet, no_viable_wallet, Opts) of
        no_viable_wallet -> 
            ?event(snp_error, {config_validation_failed, #{
                option => <<"priv_wallet">>,
                reason => <<"no_viable_wallet">>,
                expected => <<"A valid cryptographic wallet tuple">>,
                suggestion => <<"Ensure priv_wallet is provided in the configuration options or can be created automatically.">>
            }}),
            {error, {missing_wallet, <<"priv_wallet is required but not available">>}};
        Wallet when is_tuple(Wallet), tuple_size(Wallet) =:= 2 -> 
            % Validate it's a valid wallet by trying to get the address
            try
                _Address = ar_wallet:to_address(Wallet),
                ?event(snp, {wallet_validated, #{is_tuple => true}}),
                {ok, Wallet}
            catch
                _:_ ->
                    ActualType = snp_util:get_type_name(Wallet),
                    ?event(snp_error, {config_validation_failed, #{
                        option => <<"priv_wallet">>,
                        actual_type => ActualType,
                        expected => <<"valid wallet tuple">>,
                        suggestion => <<"priv_wallet must be a valid wallet tuple from ar_wallet:new() or ar_wallet:load_keyfile().">>
                    }}),
                    {error, {invalid_wallet_type, <<"priv_wallet must be a valid wallet tuple">>}}
            end;
        InvalidWallet -> 
            ActualType = snp_util:get_type_name(InvalidWallet),
            ?event(snp_error, {config_validation_failed, #{
                option => <<"priv_wallet">>,
                actual_type => ActualType,
                expected => <<"wallet tuple">>,
                suggestion => <<"priv_wallet must be a wallet tuple (from ar_wallet:new() or ar_wallet:load_keyfile()).">>
            }}),
            {error, {invalid_wallet_type, <<"priv_wallet must be a wallet tuple">>}}
    end.

%% Helper function to validate snp_trusted configuration
-spec validate_snp_trusted(Opts :: map()) -> {ok, [map()]} | {error, term()}.
validate_snp_trusted(Opts) ->
    case hb_opts:get(snp_trusted, [#{}], Opts) of
        [] -> 
            ?event(snp_error, {config_validation_failed, #{
                option => <<"snp_trusted">>,
                reason => <<"empty_list">>,
                expected => <<"Non-empty list of trusted software configuration maps">>,
                suggestion => <<"snp_trusted must contain at least one trusted software configuration map.">>
            }}),
            {error, {empty_trusted_configs, <<"snp_trusted cannot be empty">>}};
        TrustedList when is_list(TrustedList) -> 
            % Validate each trusted config in the list
            validate_trusted_configs_list(TrustedList, 0);
        InvalidTrusted -> 
            ActualType = snp_util:get_type_name(InvalidTrusted),
            ?event(snp_error, {config_validation_failed, #{
                option => <<"snp_trusted">>,
                actual_type => ActualType,
                expected => <<"list of maps">>,
                suggestion => <<"snp_trusted must be a list of maps, each containing trusted software configuration.">>
            }}),
            {error, {invalid_trusted_type, <<"snp_trusted must be a list">>}}
    end.

%% Helper function to validate each trusted config in the list
-spec validate_trusted_configs_list(TrustedList :: [map()], Index :: non_neg_integer()) -> 
    {ok, [map()]} | {error, term()}.
validate_trusted_configs_list(TrustedList, StartIndex) ->
    validate_trusted_configs_list(TrustedList, StartIndex, []).

validate_trusted_configs_list([], _Index, Acc) ->
    {ok, lists:reverse(Acc)};
validate_trusted_configs_list([Config | Rest], Index, Acc) ->
    case is_map(Config) of
        true -> 
            % Validate that config contains at least some expected keys
            % (We don't require all committed parameters, but at least one should be present)
            ConfigKeys = maps:keys(Config),
            BinaryKeys = [K || K <- ConfigKeys, is_binary(K)],
            AtomKeys = [K || K <- ConfigKeys, is_atom(K)],
            AllKeys = BinaryKeys ++ AtomKeys,
            case length(AllKeys) > 0 of
                true -> 
                    % Accumulate the validated config
                    validate_trusted_configs_list(Rest, Index + 1, [Config | Acc]);
                false -> 
                    ?event(snp_error, {config_validation_failed, #{
                        option => <<"snp_trusted">>,
                        index => Index,
                        reason => <<"empty_config_map">>,
                        expected => <<"Map with at least one configuration key">>,
                        suggestion => <<"Each trusted software configuration must contain at least one key (e.g., firmware, kernel, vcpus, etc.).">>
                    }}),
                    {error, {empty_trusted_config, Index, <<"Trusted config at index ", (hb_util:bin(integer_to_list(Index)))/binary, " is empty">>}}
            end;
        false -> 
            ActualType = snp_util:get_type_name(Config),
            ?event(snp_error, {config_validation_failed, #{
                option => <<"snp_trusted">>,
                index => Index,
                actual_type => ActualType,
                expected => <<"map">>,
                suggestion => <<"Each element in snp_trusted must be a map containing trusted software configuration.">>
            }}),
            {error, {invalid_trusted_config_type, Index, <<"Config at index ", (hb_util:bin(integer_to_list(Index)))/binary, " must be a map">>}}
    end.

%% Helper function to generate attestation report via NIF only (no mock fallback).
%% If the NIF is not loaded, returns {error, nif_not_loaded} so production never
%% uses process-dictionary or fake report data.
-spec generate_attestation_report(ReportData :: binary()) -> {ok, binary()} | {error, term()}.
generate_attestation_report(ReportData) ->
    {ReportTimeMicros, ReportResult} = timer:tc(fun() ->
        try
            snp_nif:generate_attestation_report(
                ReportData,
                ?REPORT_DATA_VERSION
            )
        catch
            error:{nif_error, _} ->
                ?event(snp_short, {nif_not_loaded, #{operation => <<"generate_attestation_report">>}}),
                {error, nif_not_loaded};
            error:undef ->
                % NIF not loaded: stubs raise undef when NIF module load failed
                ?event(snp_short, {nif_not_loaded, #{operation => <<"generate_attestation_report">>}}),
                {error, nif_not_loaded}
        end
    end),
    ReportTimeMs = ReportTimeMicros / 1000,
    ?event(snp_short, {report_generation_time_ms, ReportTimeMs}),
    ReportResult.

%% Helper function to convert report binary to JSON map
-spec convert_report_binary_to_json(ReportBinary :: binary()) -> {ok, map()} | {error, term()}.
convert_report_binary_to_json(ReportBinary) ->
    case snp_nif:report_binary_to_json(ReportBinary) of
        {ok, Map} -> {ok, Map};
        {error, ConvertReason} -> {error, {report_conversion_failed, ConvertReason}};
        Map when is_map(Map) -> {ok, Map};
        UnexpectedFormat -> {error, {unexpected_report_format, UnexpectedFormat}}
    end.

%% @doc Generate an AMD SEV-SNP commitment report and emit it as a message.
%%
%% This function creates a hardware-backed attestation report containing all
%% necessary data to validate the node's identity and software configuration.
%% The generation process performs the following operations:
%% 1. Loads and validates the provided configuration options
%% 2. Retrieves or creates a cryptographic wallet for node identity
%% 3. Generates a unique nonce using the node's address and message ID
%% 4. Extracts trusted software configuration from local options
%% 5. Generates the hardware attestation report using the NIF interface
%% 6. Packages the report with all verification data into a message
%%
%% Required configuration in Opts map:
%% - priv_wallet: Node's cryptographic wallet (created if not provided)
%% - snp_trusted: List of trusted software configurations (represents the 
%% configuration of the local node generating the report)
%%
%% @param _M1 Ignored parameter (for compatibility with dev_message interface)
%% @param _M2 Ignored parameter (for compatibility with dev_message interface)
%% @param Opts A map of configuration options for report generation:
%%   - priv_wallet: map() - Node's cryptographic wallet (created if not provided)
%%   - snp_trusted: [map()] - List of trusted software configurations
%% @returns `{ok, Map}' on success with the complete report message containing:
%%   - <<"local-hashes">>: map() - Trusted software hashes
%%   - <<"nonce">>: binary() - Encoded nonce
%%   - <<"address">>: binary() - Node address
%%   - <<"node-message">>: map() - Node message
%%   - <<"report">>: binary() - JSON-encoded SNP report
%%   or `{error, Reason}' on failure with error details
-spec generate(M1 :: term(), M2 :: term(), Opts :: map()) ->
    {ok, report_message()} | {error, term()}.
generate(_M1, _M2, Opts) ->
    maybe
        LoadedOpts = hb_cache:ensure_all_loaded(Opts, Opts),
        ?event(snp, {generate_opts, {explicit, LoadedOpts}}),  % Verbose: full opts
        % Validate configuration options
        {ok, _} ?= validate_generate_config(LoadedOpts),
        % Validate wallet availability
        {ok, ValidWallet} ?= validate_wallet(LoadedOpts),
        % Generate address and node message components
        Address = hb_util:human_id(ar_wallet:to_address(ValidWallet)),
        NodeMsg = hb_private:reset(LoadedOpts),
        {ok, PublicNodeMsgID} ?= dev_message:id(
            NodeMsg,
            #{ <<"committers">> => <<"none">> },
            LoadedOpts
        ),
        RawPublicNodeMsgID = hb_util:native_id(PublicNodeMsgID),
        ?event(snp, {snp_node_msg, NodeMsg}),  % Verbose: full node message
        % Generate the commitment report components
        ?event(snp_short, {snp_address, byte_size(Address)}),
        ReportData = snp_nonce:generate_nonce(Address, RawPublicNodeMsgID),
        ?event(snp_short, {snp_report_data, byte_size(ReportData)}),
        % Extract local hashes (already validated by validate_generate_config)
        {ok, ValidTrustedList} ?= validate_snp_trusted(LoadedOpts),
        {ok, ValidLocalHashes} ?= 
            case ValidTrustedList of
                [FirstConfig | _] -> {ok, FirstConfig};
                _ -> {error, invalid_trusted_configs_format}
            end,
        ?event(snp, {snp_local_hashes, {explicit, ValidLocalHashes}}),  % Verbose: full hashes
        % Generate the hardware attestation report
        {ok, ReportBinary} ?= generate_attestation_report(ReportData),
        % Convert binary to JSON for storage/transmission
        {ok, ReportMap} ?= convert_report_binary_to_json(ReportBinary),
        ReportJSON = hb_json:encode(ReportMap),
        ?event(snp, {snp_report_json, ReportJSON}),  % Verbose: full report JSON
        ?event(snp_short, {snp_report_generated, #{report_size => byte_size(ReportJSON)}}),  % Flow: report generated
        % Package the complete report message
        ReportMsg = #{
            <<"local-hashes">> => ValidLocalHashes,
            <<"nonce">> => hb_util:encode(ReportData),
            <<"address">> => Address,
            <<"node-message">> => NodeMsg,
            <<"report">> => ReportJSON
        },
        ?event(snp, {snp_report_msg, ReportMsg}),  % Verbose: full report message
        {ok, ReportMsg}
    else
        {error, GenerateError} -> {error, GenerateError};
        GenerateError -> {error, GenerateError}
    end.

