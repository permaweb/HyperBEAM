%%% @doc Message extraction and normalization for SNP commitment reports.
%%%
%%% This module handles the extraction and normalization of SNP commitment
%%% messages from the input, including extracting the report, address, and
%%% node message ID.
-module(snp_message).
-export([extract_and_normalize_message/2, extract_node_message_id/2, validate_message_structure/1]).
-include("include/hb.hrl").

%% @doc Extract and normalize the SNP commitment message from the input.
%%
%% This function processes the raw message and extracts all necessary components
%% for verification:
%% 1. Searches for a `body' key in the message, using it as the report source
%% 2. Applies message commitment and signing filters
%% 3. Extracts and decodes the JSON report
%% 4. Normalizes the message structure by merging report data
%% 5. Extracts the node address and message ID
%%
%% @param M2 The input message containing the SNP report
%% @param NodeOpts A map of configuration options
%% @returns `{ok, {Msg, Address, NodeMsgID, ReportJSON, MsgWithJSONReport, Report}}'
%% on success with all extracted components, or `{error, Reason}' on failure.
%% Msg is the message without the report; report-derived fields (e.g. policy) must
%% be read from Report, not from Msg, so trust/debug/measurement use only
%% message or signed-report data.
-spec extract_and_normalize_message(M2 :: term(), NodeOpts :: map()) ->
    {ok, {map(), binary(), binary(), binary(), map(), map()}} | {error, term()}.
extract_and_normalize_message(M2, NodeOpts) ->
    maybe
        % Validate message structure early
        ?event(snp, {node_opts, {explicit, NodeOpts}}),
        case validate_message_structure(M2) of
            ok -> ok;
            {error, ValidationErrors} ->
                ?event(snp_error, {message_structure_validation_failed, #{
                    operation => <<"extract_and_normalize_message">>,
                    validation_errors => ValidationErrors,
                    suggestion => <<"Ensure the message contains all required fields: 'report' (JSON string), 'address' (binary), and optionally 'node-message' or 'node-message-id'.">>
                }}),
                throw({error, {validation_failed, ValidationErrors}})
        end,
        % Search for a `body' key in the message, and if found use it as the source
        % of the report. If not found, use the message itself as the source.
        RawMsg = hb_ao:get(<<"body">>, M2, M2, NodeOpts#{ hashpath => ignore }),
        ?event(snp, {msg, {explicit, RawMsg}}),
        MsgWithJSONReport =
            hb_util:ok(
                hb_message:with_only_committed(
                    hb_message:with_only_committers(
                        RawMsg,
                        hb_message:signers(
                    RawMsg,
                            NodeOpts
                        ),
                        NodeOpts
                    ),
                    NodeOpts
                )
            ),
        ?event(snp_short, {msg_with_json_report, {explicit, MsgWithJSONReport}}),
        % Normalize the request message: do NOT merge report JSON into Msg.
        % Report may contain attacker-controlled keys; merging would let them
        % override local-hashes, address, policy, etc. used for trust/debug/
        % measurement checks before the report signature is verified.
        ReportJSON = hb_ao:get(<<"report">>, MsgWithJSONReport, NodeOpts),
        {ok, Report} = snp_util:safe_json_decode(ReportJSON),
        Msg = maps:without([<<"report">>], MsgWithJSONReport),
        ?event(snp_temp, {snp_message_normalized, #{msg_keys => maps:keys(Msg), report_not_merged => true}}),

        % Extract address and node message ID from the message (not from Report)
        Address = hb_ao:get(<<"address">>, Msg, NodeOpts),
        ?event(snp_short, {snp_address, Address}),
        {ok, NodeMsgID} ?= extract_node_message_id(Msg, NodeOpts),
        ?event(snp_short, {snp_node_msg_id, NodeMsgID}),
        {ok, {Msg, Address, NodeMsgID, ReportJSON, MsgWithJSONReport, Report}}
    else
        {error, Reason} -> {error, Reason};
        Error -> {error, Error}
    end.

%% @doc Extract the node message ID from the SNP message.
%%
%% This function handles the extraction of the node message ID, which can be
%% provided either directly as a field or embedded within a node message that
%% needs to be processed to generate the ID.
%%
%% @param Msg The normalized SNP message
%% @param NodeOpts A map of configuration options
%% @returns `{ok, NodeMsgID}' on success with the extracted ID, or
%% `{error, missing_node_msg_id}' if no ID can be found
-spec extract_node_message_id(Msg :: map(), NodeOpts :: map()) ->
    {ok, binary()} | {error, missing_node_msg_id}.
extract_node_message_id(Msg, NodeOpts) ->
    case {hb_ao:get(<<"node-message">>, Msg, NodeOpts#{ hashpath => ignore }),
          hb_ao:get(<<"node-message-id">>, Msg, NodeOpts)} of
        {undefined, undefined} ->
            {error, missing_node_msg_id};
        {undefined, ID} ->
            {ok, ID};
        {NodeMsg, _} ->
            dev_message:id(NodeMsg, #{}, NodeOpts)
    end.

%% @doc Validate message structure for required fields and types.
%% Validates that the message contains all required fields with correct types.
%% @param Message The message to validate (can be a map or any term)
%% @returns ok if valid, {error, [ValidationErrors]} if invalid
-spec validate_message_structure(Message :: term()) -> ok | {error, [binary()]}.
validate_message_structure(Message) when is_map(Message) ->
    ValidationErrors = [],
    ValidationErrors1 = validate_report_field(Message, ValidationErrors),
    ValidationErrors2 = validate_address_field(Message, ValidationErrors1),
    case ValidationErrors2 of
        [] -> ok;
        Errors -> {error, Errors}
    end;
validate_message_structure(Message) ->
    % If message is not a map, we can't validate it here
    % It might be processed later, so we allow it but log a warning
    ?event(snp, {message_structure_validation_skipped, #{
        message_type => case Message of
            B when is_binary(B) -> <<"binary">>;
            L when is_list(L) -> <<"list">>;
            _ -> <<"other">>
        end,
        reason => <<"Message is not a map, validation will be performed during extraction">>
    }}),
    ok.

%% Validate report field
-spec validate_report_field(Message :: map(), Errors :: [binary()]) -> [binary()].
validate_report_field(Message, Errors) ->
    case maps:get(<<"report">>, Message, undefined) of
        undefined ->
            % Check if report might be in body
            case maps:get(<<"body">>, Message, undefined) of
                undefined ->
                    ErrorMsg = <<"Missing required field 'report': The message must contain a 'report' field with the SNP report JSON, or a 'body' field containing the report.">>,
                    [ErrorMsg | Errors];
                _ ->
                    % Body exists, validation will happen during extraction
                    Errors
            end;
        Report when is_binary(Report) ->
            % Validate it's valid JSON
            case snp_util:safe_json_decode(Report) of
                {ok, _ReportMap} ->
                    Errors;
                {error, _Reason} ->
                    ErrorMsg = <<"Invalid 'report' field type: expected valid JSON string that decodes to a map, got invalid JSON.">>,
                    [ErrorMsg | Errors]
            end;
        Report when is_map(Report) ->
            % Report is already decoded, which is fine
            Errors;
        Invalid ->
            ErrorMsg = <<"Invalid 'report' field type: expected binary (JSON string) or map, got ", 
                (hb_util:bin(case Invalid of
                    L when is_list(L) -> "list";
                    I when is_integer(I) -> "integer";
                    _ -> "other"
                end))/binary, ".">>,
            [ErrorMsg | Errors]
    end.

%% Validate address field
-spec validate_address_field(Message :: map(), Errors :: [binary()]) -> [binary()].
validate_address_field(Message, Errors) ->
    case maps:get(<<"address">>, Message, undefined) of
        undefined ->
            % Address might be in NodeOpts, so we don't fail here
            % It will be checked during extraction
            Errors;
        Address when is_binary(Address) ->
            case byte_size(Address) of
                0 ->
                    ErrorMsg = <<"Invalid 'address' field: address cannot be empty.">>,
                    [ErrorMsg | Errors];
                _ ->
                    Errors
            end;
        Invalid ->
            ErrorMsg = <<"Invalid 'address' field type: expected binary, got ", 
                (hb_util:bin(case Invalid of
                    M when is_map(M) -> "map";
                    L when is_list(L) -> "list";
                    I when is_integer(I) -> "integer";
                    _ -> "other"
                end))/binary, ".">>,
            [ErrorMsg | Errors]
    end.

