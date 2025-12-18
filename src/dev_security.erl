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
-export([compute/3]).

%% @doc Compute the security-normalized request.
compute(Base, Req, Opts) ->
    ?event(security_debug, {compute_called, {base, Base}, {req, Req}}, Opts),
    maybe
        {ok, SecureReq1} ?= validate_assignment(Base, Req, Opts),
        {ok, SecureReq2} ?= validate_authority(Base, SecureReq1, Opts),
        {ok, _SecureReq3} ?= validate_secure_route(Base, SecureReq2, Opts)
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
    Signers = hb_message:signers(Assignment, Opts),
    Scheduler = as_list(hb_ao:get(<<"scheduler">>, Base, [], Opts), Opts),
    Required = hb_ao:get(<<"scheduler-required">>, Base, [], Opts),
    Match = hb_ao:get(<<"scheduler-match">>, Base, length(Scheduler), Opts),
    case satisfies_constraints(assignment, Signers, Required, Scheduler, Match, Opts) of
        true -> {ok, Assignment};
        false -> {error, <<"Assignment does not satisfy scheduler constraints.">>}
    end.

%% @doc Validate that a request is authorized to perform secured actions.
validate_secure_route(Base, Assignment, Opts) ->
    Req = hb_ao:get(<<"body">>, Assignment, Opts),
    case hb_ao:get(<<"match-prefix">>, Req, undefined, Opts) of
        undefined ->
            {ok, Assignment};
        _ ->
            case do_authorize_route(Req, Base, Opts) of
                ok -> {ok, Assignment};
                Error -> Error
            end
    end.

%% @doc Check if the request satisfies authority constraints for the action.
do_authorize_route(Req, Base, Opts) ->
    Action = hb_maps:find(<<"action">>, Req, <<"Request have no Action">>, Opts),
    ReqBy = hb_maps:find(<<"from">>, Req, <<"Request have no From">>, Opts),
    Authority =
        as_list(
            hb_ao:get(<<Action/binary, "-authority">>, Base, [], Opts),
            Opts
        ),
    Required = hb_ao:get(<<Action/binary, "-required">>, Base, [], Opts),
    Match = hb_ao:get(<<Action/binary, "-match">>, Base, length(Authority), Opts),
    case satisfies_constraints(
            secure_route,
            ReqBy,
            Required,
            Authority,
            Match,
            Opts
         ) of
        true  -> ok;
        false -> {error, <<"Route not Authorized.">>}
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
            case do_validate_authority(Base, Msg, Signers, Opts) of
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
                false ->
                    {
                        error,
                        <<
                            "Assigned message does not satisfy ",
                            "compute authority constraints."
                        >>
                    }
            end
    end.

%% @doc If a message purporting to be from a process satisfies the compute
%% authority constraints, return true, otherwise return false.
do_validate_authority(Base, Msg, Signers, Opts) ->
    Authority = as_list(hb_ao:get(<<"authority">>, Base, [], Opts), Opts),
    Required = hb_ao:get(<<"authority-required">>, Base, [], Opts),
    ?event(security_debug,
        {validate_authority,
            {intent, compute},
            {committers, Signers},
            {authority, Authority},
            {required, Required},
            {base, Base},
            {message, Msg}
        },
        Opts
    ),
    Match = hb_ao:get(<<"authority-match">>, Base, length(Authority), Opts),
    satisfies_constraints(compute, Signers, Required, Authority, Match, Opts).

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
    SatisfiesAcceptable = PresentAcceptableCommitters >= ValidCount,
    % Are all required committers present in the message?
    PresentRequiredCommitters = count_common(MsgCommitterList, RequiredList),
    SatisfiesRequired = PresentRequiredCommitters == length(RequiredList),
    % Must have at least `Match' common elements AND all `Required' elements
    Res = SatisfiesAcceptable andalso SatisfiesRequired,
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

%% @doc Return the single element of a list if there is only one, else return
%% the list.
maybe_single([SingleElement], _Opts) -> SingleElement;
maybe_single(List, _Opts) -> List.