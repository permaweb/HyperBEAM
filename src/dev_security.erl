%%% @doc A flexible security validation device with assessment message support.
%%%
%%% Provides customizable validation for assignments and authority, following
%%% the pattern from hyper-token.lua. Supports both custom assessment messages
%%% and constraint-based validation.
%%%
%%% ## State Structure
%%%
%%% The security device expects the following fields in the base state:
%%%
%%% - `authority`: List of addresses that are allowed to authorize requests
%%% - `authority-required`: Addresses that must be present (default: [])
%%% - `authority-match`: Minimum number of matches needed (default: length of authority)
%%% - `scheduler`: List of addresses that can schedule assignments
%%% - `scheduler-required`: Schedulers that must be present (default: [])
%%% - `scheduler-match`: Minimum number of schedulers needed (default: length of scheduler)
%%% - `assess`: Map of custom assessment messages:
%%%   - `authority`: Custom validator for authority checks
%%%   - `scheduler`: Custom validator for scheduler checks
%%%
%%% ## API
%%%
%%% ### validate-assignment
%%% Validates that an assignment is trusted based on scheduler constraints.
%%%
%%% Request:
%%% ```
%%% #{
%%%   <<"path">> => <<"validate-assignment">>,
%%%   <<"subject">> => Assignment  % The assignment to validate
%%% }
%%% ```
%%%
%%% Returns: `{ok, true | false}`
%%%
%%% ### validate-authority
%%% Validates that a request has proper authority based on authority constraints.
%%%
%%% Request:
%%% ```
%%% #{
%%%   <<"path">> => <<"validate-authority">>,
%%%   <<"subject">> => Request  % The request to validate
%%% }
%%% ```
%%%
%%% Returns: `{ok, true | false}`
%%%
%%% ### validate-constraints
%%% Validates that a subject list satisfies given constraints.
%%%
%%% Request:
%%% ```
%%% #{
%%%   <<"path">> => <<"validate-constraints">>,
%%%   <<"subject">> => [<<"addr1">>, <<"addr2">>],
%%%   <<"all">> => [<<"addr1">>, <<"addr2">>, <<"addr3">>],
%%%   <<"required">> => [<<"addr1">>],
%%%   <<"match">> => 1
%%% }
%%% ```
%%%
%%% Returns: `{ok, true | false}`

-module(dev_security).
-export([compute/3]).
-include_lib("include/hb.hrl").

%% @doc Main entry point for security validation.
compute(Base, Req, Opts) ->
    Path = hb_ao:get(<<"path">>, Req, <<"validate-assignment">>, Opts),
    route(Path, Base, Req, Opts).

%% @doc Route to the appropriate validation function.
route(<<"validate-assignment">>, Base, Req, Opts) ->
    Subject = hb_ao:get(<<"subject">>, Req, Opts),
    Result = validate_assignment(Base, Subject, Opts),
    {ok, Result};
route(<<"validate-authority">>, Base, Req, Opts) ->
    Subject = hb_ao:get(<<"subject">>, Req, Opts),
    Result = validate_authority(Base, Subject, Opts),
    {ok, Result};
route(<<"validate-constraints">>, _Base, Req, Opts) ->
    Subject = hb_ao:get(<<"subject">>, Req, Opts),
    All = hb_ao:get(<<"all">>, Req, [], Opts),
    Required = hb_ao:get(<<"required">>, Req, [], Opts),
    Match = hb_ao:get(<<"match">>, Req, length(All), Opts),
    Result = satisfies_constraints(Subject, All, Required, Match, Opts),
    {ok, Result};
route(_, _Base, _Req, _Opts) ->
    {error, <<"Unknown security validation path.">>}.

%% @doc Validate that an assignment is trusted based on scheduler constraints.
validate_assignment(Base, Assignment, Opts) ->
    Assess = hb_ao:get(<<"assess/scheduler">>, Base, not_found, Opts),
    Scheduler = hb_ao:get(<<"scheduler">>, Base, [], Opts),
    Required = hb_ao:get(<<"scheduler-required">>, Base, [], Opts),
    Match = hb_ao:get(<<"scheduler-match">>, Base, length(Scheduler), Opts),

    satisfies_constraints_or_assess(
        Assignment,
        Assess,
        Scheduler,
        Required,
        Match,
        Opts
    ).

%% @doc Validate that a request has proper authority.
validate_authority(Base, Request, Opts) ->
    case hb_ao:get(<<"assess/authority">>, Base, undefined, Opts) of
        undefined ->
            % No assessment message, use constraint checking
            Signers = hb_message:signers(Request, Opts),
            Authority = hb_ao:get(<<"authority">>, Base, [], Opts),
            Required = hb_ao:get(<<"authority-required">>, Base, [], Opts),
            Match = hb_ao:get(<<"authority-match">>, Base, length(Authority), Opts),
            satisfies_constraints(Signers, Authority, Required, Match, Opts);
        AssessMsg ->
            % Run the assessment message
            eval_assessment_message(Request, AssessMsg, Opts)
    end.

%% @doc Evaluate an assessment message.
eval_assessment_message(Subject, AssessMsg, Opts) ->
    % Run the assessment message
    ?event({running_assessment, AssessMsg, Subject}),
    case hb_ao:resolve(AssessMsg, Subject, Opts) of
        {ok, true} ->
            ?event({assessment_passed}),
            true;
        {ok, false} ->
            ?event({assessment_failed, {message, AssessMsg}, {subject, Subject}}),
            false;
        {ok, Other} ->
            ?event({assessment_returned_non_boolean, Other}),
            false;
        {error, Reason} ->
            ?event({assessment_error, Reason}),
            false
    end.

%% @doc Check if subject satisfies list constraints.
%% Returns true if:
%% 1. At least `Match` elements from Subject are in All
%% 2. All elements in Required are in Subject
satisfies_constraints(Subject, All, Required, Match, Opts) ->
    % Normalize inputs to lists
    SubjectList = normalize_to_list(Subject, Opts),
    AllList = normalize_to_list(All, Opts),
    RequiredList = normalize_to_list(Required, Opts),
    CommonCount = count_common(SubjectList, AllList),
    RequiredCount = count_common(RequiredList, SubjectList),
    ?event({constraint_check,
        {subject, SubjectList},
        {all, AllList},
        {required, RequiredList},
        {match, Match},
        {common_count, CommonCount},
        {required_count, RequiredCount}
    }),
    % Must have at least Match common elements AND all required elements
    (CommonCount >= Match) andalso (RequiredCount == length(RequiredList)).

%% @doc Count elements that appear in both lists.
count_common(ListA, ListB) ->
    length([X || X <- ListA, lists:member(X, ListB)]).

%% @doc Normalize value to a list.
normalize_to_list(Value, _Opts) when is_list(Value) ->
    Value;
normalize_to_list(Value, _Opts) ->
    [Value].
