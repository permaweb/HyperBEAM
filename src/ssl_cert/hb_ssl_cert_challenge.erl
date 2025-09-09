%%% @doc SSL Certificate challenge management module.
%%%
%%% This module handles DNS challenge validation, polling, and status management
%%% for SSL certificate requests. It provides functions to validate challenges
%%% with Let's Encrypt, poll for completion, and handle timeouts and retries.
%%%
%%% The module implements the complete challenge validation workflow including
%%% initial validation triggering, status polling, and result formatting.
-module(hb_ssl_cert_challenge).

-include("include/ssl_cert_records.hrl").
-include("include/hb.hrl").

%% Public API
-export([
    validate_dns_challenges_state/2,
    validate_challenges_with_timeout/3,
    poll_challenge_status/6,
    poll_order_until_valid/3,
    format_challenges_for_response/1,
    extract_challenge_info/1
]).

%% Type specifications
-spec validate_dns_challenges_state(request_state(), map()) -> 
    {ok, map()} | {error, map()}.
-spec validate_challenges_with_timeout(acme_account(), [map()], integer()) -> 
    [validation_result()].
-spec poll_challenge_status(acme_account(), dns_challenge(), string(), integer(), integer(), integer()) -> 
    validation_result().
-spec poll_order_until_valid(acme_account(), request_state(), integer()) -> 
    {valid | processing, request_state()} | {error, term()}.
-spec format_challenges_for_response([map()]) -> [map()].

%% @doc Validates DNS challenges and manages the complete validation workflow.
%%
%% This function orchestrates the challenge validation process including:
%% 1. Extracting challenges from state
%% 2. Validating each challenge with timeout
%% 3. Handling order finalization if all challenges pass
%% 4. Managing retries for failed challenges
%% 5. Polling order status until completion
%%
%% @param State The current request state
%% @param Opts Configuration options
%% @returns {ok, ValidationResponse} or {error, ErrorResponse}
validate_dns_challenges_state(State, Opts) ->
    case State of
        State when is_map(State) ->
            % Reconstruct account and challenges from stored state
            Account = hb_ssl_cert_state:extract_account_from_state(State),
            Challenges = maps:get(<<"challenges">>, State, []),
            % Validate each challenge with Let's Encrypt (with timeout)
            ValidationResults = validate_challenges_with_timeout(
                Account, Challenges, ?CHALLENGE_DEFAULT_TIMEOUT_SECONDS),
            % Check if all challenges are valid
            AllValid = lists:all(fun(Result) ->
                maps:get(<<"status">>, Result) =:= ?ACME_STATUS_VALID
            end, ValidationResults),
            case AllValid of
                true ->
                    ?event(ssl_cert, {ssl_cert_all_challenges_valid}),
                    handle_all_challenges_valid(State, Account, ValidationResults, Opts);
                false ->
                    ?event(ssl_cert, {ssl_cert_some_challenges_failed}),
                    handle_some_challenges_failed(State, Account, Challenges, ValidationResults, Opts)
            end;
        _ ->
            {error, #{<<"status">> => 400, <<"error">> => <<"Invalid request state">>}}
    end.

%% @doc Validates DNS challenges with Let's Encrypt with polling and timeout.
%% 
%% This function triggers validation for each challenge and then polls the status
%% until each challenge reaches a final state (valid/invalid) or times out.
%% ACME challenge validation is asynchronous, so we need to poll repeatedly.
%%
%% @param Account ACME account record
%% @param Challenges List of DNS challenges
%% @param TimeoutSeconds Timeout for validation in seconds
%% @returns List of validation results
validate_challenges_with_timeout(Account, Challenges, TimeoutSeconds) ->
    ?event(ssl_cert, {ssl_cert_validating_challenges_with_timeout, TimeoutSeconds}),
    StartTime = erlang:system_time(second),
    lists:map(fun(Challenge) ->
        {Domain, ChallengeRecord} = extract_challenge_info(Challenge),
        % First, trigger the challenge validation
        ?event(ssl_cert, {ssl_cert_triggering_challenge_validation, Domain}),
        case hb_acme_client:validate_challenge(Account, ChallengeRecord) of
            {ok, InitialStatus} ->
                ?event(ssl_cert, {ssl_cert_challenge_initial_status, Domain, InitialStatus}),
                % Now poll until we get a final status
                poll_challenge_status(Account, ChallengeRecord, Domain, StartTime, TimeoutSeconds, 1);
            {error, Reason} ->
                ?event(ssl_cert, {ssl_cert_challenge_trigger_failed, Domain, Reason}),
                #{<<"domain">> => hb_util:bin(Domain),
                  <<"status">> => <<"failed">>,
                  <<"error">> => hb_util:bin(io_lib:format("Failed to trigger validation: ~p", [Reason]))}
        end
    end, Challenges).

%% @doc Polls a challenge status until it reaches a final state or times out.
%%
%% @param Account ACME account record
%% @param ChallengeRecord DNS challenge record  
%% @param Domain Domain name for logging
%% @param StartTime When validation started
%% @param TimeoutSeconds Total timeout in seconds
%% @param AttemptNum Current attempt number
%% @returns Validation result map
poll_challenge_status(Account, ChallengeRecord, Domain, StartTime, TimeoutSeconds, AttemptNum) ->
    ElapsedTime = erlang:system_time(second) - StartTime,
    case ElapsedTime < TimeoutSeconds of
        false ->
            ?event(ssl_cert, {ssl_cert_validation_timeout_reached, Domain, AttemptNum}),
            #{<<"domain">> => hb_util:bin(Domain),
              <<"status">> => <<"timeout">>,
              <<"error">> => <<"Validation timeout reached">>,
              <<"attempts">> => AttemptNum};
        true ->
            % Use POST-as-GET to check challenge status without re-triggering
            case hb_acme_client:get_challenge_status(Account, ChallengeRecord) of
                {ok, Status} ->
                    ?event(ssl_cert, {ssl_cert_challenge_poll_status, Domain, Status, AttemptNum}),
                    StatusBin = hb_util:bin(Status),
                    case StatusBin of
                        ?ACME_STATUS_VALID ->
                            ?event(ssl_cert, {ssl_cert_challenge_validation_success, Domain, AttemptNum}),
                            #{<<"domain">> => hb_util:bin(Domain),
                              <<"status">> => ?ACME_STATUS_VALID,
                              <<"attempts">> => AttemptNum};
                        ?ACME_STATUS_INVALID ->
                            ?event(ssl_cert, {ssl_cert_challenge_validation_failed, Domain, AttemptNum}),
                            #{<<"domain">> => hb_util:bin(Domain),
                              <<"status">> => ?ACME_STATUS_INVALID,
                              <<"error">> => <<"Challenge validation failed">>,
                              <<"attempts">> => AttemptNum};
                        _ when StatusBin =:= ?ACME_STATUS_PENDING; StatusBin =:= ?ACME_STATUS_PROCESSING ->
                            % Still processing, wait and poll again
                            ?event(ssl_cert, {ssl_cert_challenge_still_processing, Domain, Status, AttemptNum}),
                            timer:sleep(?CHALLENGE_POLL_DELAY_SECONDS * 1000),
                            poll_challenge_status(Account, ChallengeRecord, Domain, StartTime, 
                                                TimeoutSeconds, AttemptNum + 1);
                        _ ->
                            % Unknown status, treat as error
                            ?event(ssl_cert, {ssl_cert_challenge_unknown_status, Domain, Status, AttemptNum}),
                            #{<<"domain">> => hb_util:bin(Domain),
                              <<"status">> => StatusBin,
                              <<"error">> => hb_util:bin(io_lib:format("Unknown status: ~s", [Status])),
                              <<"attempts">> => AttemptNum}
                    end;
                {error, Reason} ->
                    ?event(ssl_cert, {ssl_cert_challenge_poll_error, Domain, Reason, AttemptNum}),
                    #{<<"domain">> => hb_util:bin(Domain),
                      <<"status">> => <<"error">>,
                      <<"error">> => hb_util:bin(io_lib:format("Polling error: ~p", [Reason])),
                      <<"attempts">> => AttemptNum}
            end
    end.

%% @doc Poll order status until valid or timeout.
%%
%% @param Account ACME account record
%% @param State Current request state
%% @param TimeoutSeconds Timeout in seconds
%% @returns {Status, UpdatedState} or {error, Reason}
poll_order_until_valid(Account, State, TimeoutSeconds) ->
    Start = erlang:system_time(second),
    poll_order_until_valid_loop(Account, State, TimeoutSeconds, Start).

%% @doc Formats challenges for user-friendly HTTP response.
%%
%% This function converts internal challenge representations to a format
%% suitable for API responses, including DNS record instructions for
%% different DNS providers.
%%
%% @param Challenges List of DNS challenge maps from stored state
%% @returns Formatted challenge list for HTTP response
format_challenges_for_response(Challenges) ->
    lists:map(fun(Challenge) ->
        {Domain, DnsValue} = case Challenge of
            #{<<"domain">> := D, <<"dns_value">> := V} -> 
                {hb_util:list(D), hb_util:list(V)};
            #{domain := D, dns_value := V} -> 
                {D, V};
            Rec when is_record(Rec, dns_challenge) -> 
                {Rec#dns_challenge.domain, Rec#dns_challenge.dns_value}
        end,
        RecordName = "_acme-challenge." ++ Domain,
        #{
            <<"domain">> => hb_util:bin(Domain),
            <<"record_name">> => hb_util:bin(RecordName),
            <<"record_value">> => hb_util:bin(DnsValue),
            <<"instructions">> => #{
                <<"cloudflare">> => hb_util:bin("Add TXT record: _acme-challenge with value " ++ DnsValue),
                <<"route53">> => hb_util:bin("Create TXT record " ++ RecordName ++ " with value " ++ DnsValue),
                <<"manual">> => hb_util:bin("Create DNS TXT record for " ++ RecordName ++ " with value " ++ DnsValue)
            }
        }
    end, Challenges).

%%%--------------------------------------------------------------------
%%% Internal Functions
%%%--------------------------------------------------------------------

%% @doc Handles the case where all challenges are valid.
%%
%% @param State Current request state
%% @param Account ACME account record
%% @param ValidationResults Challenge validation results
%% @param Opts Configuration options
%% @returns {ok, Response} or {error, ErrorResponse}
handle_all_challenges_valid(State, Account, ValidationResults, Opts) ->
    % Check current order status to avoid re-finalizing
    OrderMap = maps:get(<<"order">>, State),
    CurrentOrderStatus = hb_util:bin(maps:get(<<"status">>, OrderMap, ?ACME_STATUS_PENDING)),
    case CurrentOrderStatus of
        ?ACME_STATUS_VALID ->
            {ok, #{<<"status">> => 200,
                   <<"body">> => #{
                       <<"message">> => <<"Order already valid">>,
                       <<"results">> => ValidationResults,
                       <<"order_status">> => ?ACME_STATUS_VALID,
                       <<"request_state">> => State
                   }}};
        ?ACME_STATUS_PROCESSING ->
            {ok, #{<<"status">> => 200,
                   <<"body">> => #{
                       <<"message">> => <<"Order finalization in progress">>,
                       <<"results">> => ValidationResults,
                       <<"order_status">> => ?ACME_STATUS_PROCESSING,
                       <<"request_state">> => State
                   }}};
        _ ->
            % Finalize the order to get certificate URL
            Order = hb_ssl_cert_state:extract_order_from_state(State),
            case hb_acme_client:finalize_order(Account, Order, Opts) of
                {ok, FinalizedOrder} ->
                    ?event(ssl_cert, {ssl_cert_order_finalized}),
                    % Update state with finalized order and store the wallet-based CSR private key
                    UpdatedState = hb_ssl_cert_state:update_order_in_state(State, FinalizedOrder),
                    % Poll order until valid
                    PollResult = poll_order_until_valid(Account, UpdatedState, ?ORDER_POLL_TIMEOUT_SECONDS),
                    case PollResult of
                        {valid, PolledState} ->
                            {ok, #{<<"status">> => 200, 
                                   <<"body">> => #{
                                       <<"message">> => <<"Order valid; ready to download">>,
                                       <<"results">> => ValidationResults,
                                       <<"order_status">> => ?ACME_STATUS_VALID,
                                       <<"request_state">> => PolledState,
                                       <<"next_step">> => <<"download">>
                                   }}};
                        {processing, PolledState} ->
                            {ok, #{<<"status">> => 200, 
                                   <<"body">> => #{
                                       <<"message">> => <<"Order finalization in progress">>,
                                       <<"results">> => ValidationResults,
                                       <<"order_status">> => ?ACME_STATUS_PROCESSING,
                                       <<"request_state">> => PolledState
                                   }}};
                        {error, PollReason} ->
                            {error, #{<<"status">> => 500,
                                     <<"error">> => hb_util:bin(io_lib:format("Order polling failed: ~p", [PollReason]))}}
                    end;
                {error, FinalizeReason} ->
                    ?event(ssl_cert, {ssl_cert_finalization_failed, {reason, FinalizeReason}}),
                    {ok, #{<<"status">> => 200, 
                           <<"body">> => #{
                               <<"message">> => <<"DNS challenges validated but finalization pending">>,
                               <<"results">> => ValidationResults,
                               <<"order_status">> => ?ACME_STATUS_PROCESSING,
                               <<"request_state">> => State,
                               <<"next_step">> => <<"retry_download_later">>
                           }}}
            end
    end.

%% @doc Handles the case where some challenges failed.
%%
%% @param State Current request state
%% @param Account ACME account record
%% @param Challenges Original challenges
%% @param ValidationResults Challenge validation results
%% @param Opts Configuration options
%% @returns {ok, Response}
handle_some_challenges_failed(State, Account, Challenges, ValidationResults, Opts) ->
    % Optional in-call retry for failed challenges
    Config = maps:get(<<"config">>, State, #{}),
    DnsWaitSec = maps:get(dns_propagation_wait, Config, 30),
    RetryTimeout = maps:get(validation_timeout, Config, ?CHALLENGE_DEFAULT_TIMEOUT_SECONDS),
    % Determine which domains succeeded
    ValidDomains = [maps:get(<<"domain">>, R) || R <- ValidationResults,
                                             maps:get(<<"status">>, R) =:= ?ACME_STATUS_VALID],
    % Build a list of challenges to retry (non-valid ones)
    RetryChallenges = [C || C <- Challenges,
                           begin
                               DomainBin = case C of
                                   #{<<"domain">> := D} -> D;
                                   #{domain := D} -> hb_util:bin(D);
                                   _ -> <<>>
                               end,
                               not lists:member(DomainBin, ValidDomains)
                           end],
    case RetryChallenges of
        [] ->
            % Nothing to retry; return original results
            {ok, #{<<"status">> => 200, 
                   <<"body">> => #{
                       <<"message">> => <<"DNS challenges validation completed with some failures">>,
                       <<"results">> => ValidationResults,
                       <<"request_state">> => State,
                       <<"next_step">> => <<"check_dns_and_retry">>
                   }}};
        _ ->
            ?event(ssl_cert, {ssl_cert_retrying_failed_challenges, length(RetryChallenges)}),
            timer:sleep(DnsWaitSec * 1000),
            RetryResults = validate_challenges_with_timeout(Account, RetryChallenges, RetryTimeout),
            % Merge retry results into the original results by domain (retry wins)
            OrigMap = maps:from_list([{maps:get(<<"domain">>, R), R} || R <- ValidationResults]),
            RetryMap = maps:from_list([{maps:get(<<"domain">>, R), R} || R <- RetryResults]),
            MergedMap = maps:merge(OrigMap, RetryMap),
            MergedResults = [V || {_K, V} <- maps:to_list(MergedMap)],
            AllValidAfterRetry = lists:all(fun(R) -> 
                maps:get(<<"status">>, R) =:= ?ACME_STATUS_VALID 
            end, MergedResults),
            case AllValidAfterRetry of
                true ->
                    % Proceed as in the success path with merged results
                    handle_all_challenges_valid(State, Account, MergedResults, Opts);
                false ->
                    {ok, #{<<"status">> => 200, 
                           <<"body">> => #{
                               <<"message">> => <<"DNS challenges validation completed with some failures (retry attempted)">>,
                               <<"results">> => MergedResults,
                               <<"request_state">> => State,
                               <<"next_step">> => <<"check_dns_and_retry">>
                           }}}
            end
    end.

%% @doc Extracts challenge information from various challenge formats.
%%
%% @param Challenge Challenge in map or record format
%% @returns {Domain, ChallengeRecord}
extract_challenge_info(Challenge) ->
    case Challenge of
        #{<<"domain">> := D, <<"token">> := T, <<"key_authorization">> := K, <<"dns_value">> := V, <<"url">> := U} ->
            DomainStr = hb_util:list(D),
            {DomainStr, #dns_challenge{
                domain=DomainStr, 
                token=hb_util:list(T), 
                key_authorization=hb_util:list(K), 
                dns_value=hb_util:list(V), 
                url=hb_util:list(U)
            }};
        #{domain := D, token := T, key_authorization := K, dns_value := V, url := U} ->
            {D, #dns_challenge{domain=D, token=T, key_authorization=K, dns_value=V, url=U}};
        Rec when is_record(Rec, dns_challenge) -> 
            {Rec#dns_challenge.domain, Rec}
    end.

%% @doc Internal loop for polling order status.
%%
%% @param Account ACME account record
%% @param State Current request state
%% @param TimeoutSeconds Timeout in seconds
%% @param Start Start time
%% @returns {Status, UpdatedState} or {error, Reason}
poll_order_until_valid_loop(Account, State, TimeoutSeconds, Start) ->
    OrderMap = maps:get(<<"order">>, State),
    OrderUrl = hb_util:list(maps:get(<<"url">>, OrderMap)),
    case erlang:system_time(second) - Start < TimeoutSeconds of
        false -> {processing, State};
        true ->
            case hb_acme_client:get_order(Account, OrderUrl) of
                {ok, Resp} ->
                    StatusBin = hb_util:bin(maps:get(<<"status">>, Resp, ?ACME_STATUS_PROCESSING)),
                    CertUrl = maps:get(<<"certificate">>, Resp, undefined),
                    UpdatedOrderMap = OrderMap#{
                        <<"status">> => StatusBin,
                        <<"certificate">> => case CertUrl of 
                            undefined -> <<>>; 
                            _ -> hb_util:bin(CertUrl) 
                        end
                    },
                    UpdatedState = State#{ <<"order">> => UpdatedOrderMap, <<"status">> => StatusBin },
                    case StatusBin of
                        ?ACME_STATUS_VALID -> {valid, UpdatedState};
                        _ -> timer:sleep(?ORDER_POLL_DELAY_SECONDS * 1000),
                             poll_order_until_valid_loop(Account, UpdatedState, TimeoutSeconds, Start)
                    end;
                {error, Reason} -> {error, Reason}
            end
    end.
