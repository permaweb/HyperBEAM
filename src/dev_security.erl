%%% @doc Security parameter enforcement for AO `~process@1.0' devices. Calling
%%% `compute' upon this device results in a modified version of the `Request'
%%% being returned, containing security-normalized keys (`from', etc). In the
%%% event that the request does not pass the security requirements of the 
%%% base process state a `{skip, State}' tuple is returned. Upon receipt, the
%%% caller is expected to disregard the request and return the orginal `Base'
%%% for the interaction in an unmodified form.
-module(dev_security).
-include_lib("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
%%% Device API.
-export([compute/3]).
%%% Public utility API.
-export([validate/4, validate/5]).

%% @doc Compute the security-normalized request.
compute(Base, Req, Opts) ->
    ?event(security_debug, {compute_called, {base, Base}, {req, Req}}, Opts),
    maybe
        {ok, SecureReq1} ?= validate_assignment(Base, Req, Opts),
        {ok, _SecureReq2} ?= validate_authority(Base, SecureReq1, Opts)
    else
        {error, Reason} ->
            ?event(
                security_error,
                {security_error,
                    {process, dev_process_lib:process_id(Base, Opts)},
                    {slot, hb_maps:get(<<"slot">>, Req, no_slot, Opts)},
                    {reason, Reason}
                },
                Opts
            ),
            {skip, Reason}
    end.

%% @doc Validate that an assignment is trusted based on scheduler constraints.
validate_assignment(Base, Assignment, Opts) ->
    case validate(<<"scheduler">>, Base, Assignment, Opts) of
        true ->
            {ok, Assignment};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Validate that a request has proper authority, adding a `from' key to the
%% assigned message such that downstream callers can refer to a verified sender
%% (for replies, etc) -- whether an end-user wallet or another process.
validate_authority(Base, Assignment, Opts) ->
    Msg = hb_ao:get(<<"body">>, Assignment, undefined, Opts),
    Signers = hb_message:signers(Msg, Opts),
    case hb_ao:get(<<"from-process">>, Msg, undefined, Opts) of
        undefined ->
            {
                ok,
                hb_ao:set(
                    Assignment,
                    <<"body/from">>,
                    maybe_single(Signers, Opts),
                    Opts
                )
            };
        Sender ->
            case validate(<<"authority">>, Base, Msg, Opts) of
                true ->
                    {
                        ok,
                        hb_ao:set(
                            Assignment,
                            <<"body/from">>,
                            Sender,
                            Opts
                        )
                    };
                {error, Reason} -> {error, Reason}
            end
    end.

%% @doc If a message purporting to be from a process satisfies the compute
%% authority constraints, return true, otherwise return false.
validate(Key, Base, SubjectMsg, Opts) ->
    validate(Key, Base, SubjectMsg, hb_message:signers(SubjectMsg, Opts), Opts).
validate(Key, Base, SubjectMsg, RawFrom, Opts) ->
    maybe
        %% Dedup identities so duplicate committers cannot satisfy min-N thresholds.
        From = lists:uniq(as_list(RawFrom, Opts)),
        ValidOrError = as_signer_config_list(hb_ao:get(Key, Base, [], Opts), Opts),
        true ?= is_list(ValidOrError) orelse ValidOrError,
        Valid = lists:uniq(ValidOrError),
        RequiredListOrError =
            as_signer_config_list(
                hb_ao:get(<<Key/binary, "-required">>, Base, [], Opts),
                Opts
            ),
        true ?= is_list(RequiredListOrError) orelse RequiredListOrError,
        RequiredList = lists:uniq(RequiredListOrError),
        DefaultThresholdN = case length(Valid) of
            0 -> 0;
            _ -> 1
        end,
        
        MatchRaw = hb_ao:get(<<Key/binary, "-match">>, Base, not_found, Opts),
        MatchOrError = safe_match(MatchRaw, DefaultThresholdN, length(Valid)),
        true ?= is_integer(MatchOrError) orelse MatchOrError,
        Match = MatchOrError,
        ?event(security_debug,
            {validate_authority,
                {subject_ids, From},
                {intent, compute},
                {valid_options, Valid},
                {required, RequiredList},
                {base, Base},
                {message, SubjectMsg}
            },
            Opts
        ),
        satisfies_constraints(Key, From, RequiredList, Valid, Match, Opts)
end.

%% @doc Validate that the request satisfies the given constraints.
%% Returns true if:
%% 1. At least `Match` elements from `Subject` are in `All`
%% 2. All elements in `Required` are in Subject
satisfies_constraints(Intent, MsgCommitters, Required, Valid, ValidCount, Opts) ->
    % Normalize inputs to lists
    MsgCommitterList = as_list(MsgCommitters, Opts),
    ValidList = as_list(Valid, Opts),
    RequiredList = as_list(Required, Opts),
    % Are there at least `ValidCount' valid committers present in the message?
    PresentAcceptableCommitters = count_common(MsgCommitterList, ValidList),
    SatisfiesAcceptable =
        (PresentAcceptableCommitters >= ValidCount) orelse
            {error, <<"Too few acceptable committers present.">>},
    % Are all required committers present in the message?
    PresentRequiredCommitters = count_common(MsgCommitterList, RequiredList),
    SatisfiesRequired =
        (PresentRequiredCommitters == length(RequiredList)) orelse
            {error, <<"Required committers not present in message.">>},
    % Must have at least `Match' common elements AND all `Required' elements
    Res =
        case SatisfiesAcceptable of
            true -> SatisfiesRequired;
            Error -> Error
        end,
    ?event(
        security_short,
        {constraint_check,
            {intent, Intent},
            {message_committers, length(MsgCommitterList)},
            {acceptable_committers, length(ValidList)},
            {present_acceptable_committers, PresentAcceptableCommitters},
            {satisfies_acceptable, SatisfiesAcceptable},
            {required_committers, length(RequiredList)},
            {all_required_are_present, SatisfiesRequired},
            {result, Res}
        },
        Opts
    ),
    Res.

%% @doc Count elements that appear in both lists.
count_common(ListA, ListB) -> length([X || X <- ListA, lists:member(X, ListB)]).

%% @doc Normalize value to a list.
as_list(Value, _Opts) when is_list(Value) -> Value;
as_list(Value, _Opts) -> [Value].

%% @doc Normalize signer config values. Supports true lists and comma-separated
%% binary encodings used in process security configuration.
as_signer_config_list(Value, _Opts) when is_list(Value) -> Value;
as_signer_config_list(Value, _Opts) when is_binary(Value) ->
    hb_util:binary_to_strings(Value);
as_signer_config_list(_Value, _Opts) -> 
    {error, <<"Signer config must be a binary or a list.">>}.
%% @doc Normalize and validate a `*-match` threshold against the acceptable
%% signer set `Valid`. Let `ValidLen = |Valid|`. If no explicit threshold is
%% provided, the default threshold is `0` when `ValidLen = 0` and `1` when
%% `ValidLen > 0`. Explicit thresholds must be integer-like and satisfy:
%% `Match = 0` iff `ValidLen = 0`; otherwise `1 =< Match =< ValidLen`.
safe_match(_Match, _Default, ValidLen) when not is_integer(ValidLen) ->
    {error, <<"Invalid Valid list length type.">>};
safe_match(_Match, _Default, ValidLen) when is_integer(ValidLen), ValidLen < 0 ->
    {error, <<"Valid list length must be a non-negative integer.">>};
safe_match(_Match, Default, _ValidLen) when not is_integer(Default) ->
    {error, <<"Invalid Default type.">>};
safe_match(_Match, Default, _ValidLen) when Default < 0 ->
    {error, <<"Default must be a non-negative integer.">>};
safe_match(_Match, Default, ValidLen) when Default > ValidLen ->
    {error, <<"Default must be integer less than or equal to ValidLen.">>};
safe_match(not_found, Default, _ValidLen) when is_integer(Default), Default >= 0 ->
    Default;
safe_match(Match, _Default, ValidLen) when is_integer(Match) ->
    case {Match, ValidLen} of
        {0, 0} -> 0;
        {M, V} when M > 0 andalso M =< V -> M;
        _ -> {error, <<"Invalid Match threshold.">>}
    end;
safe_match(Match, _Default, ValidLen) when is_binary(Match)->
    try binary_to_integer(Match) of
        IntMatch -> safe_match(IntMatch, _Default, ValidLen)
    catch
        _:_ -> {error, <<"Invalid Match threshold.">>}
    end;
safe_match(_, _, _) ->
    {error, <<"Invalid Match threshold.">>}.

%% @doc Return the single element of a list if there is only one, else return
%% the list.
maybe_single([SingleElement], _Opts) -> SingleElement;
maybe_single(List, _Opts) -> List.

duplicate_authority_match_rejected_test() ->
    ?assertEqual(
        {error, <<"Too few acceptable committers present.">>},
        validate(
            <<"authority">>,
            #{
                <<"authority">> => [<<"alice">>, <<"bob">>],
                <<"authority-match">> => 2
            },
            #{},
            [<<"alice">>, <<"alice">>],
            #{}
        )
    ).

comma_separated_authority_config_supported_test() ->
    ?assertEqual(
        true,
        validate(
            <<"authority">>,
            #{
                <<"authority">> => <<"\"alice\",\"bob\"">>
            },
            #{},
            [<<"alice">>, <<"bob">>],
            #{}
        )
    ).
