%%% @doc Security parameter enforcement for AO `~process@1.0' devices. Calling
%%% `compute' upon this device results in a modified version of the `Request'
%%% being returned, containing security-normalized keys (`from', etc). In the
%%% event that the request does not pass the security requirements of the
%%% base process state a `{skip, State}' tuple is returned. Upon receipt, the
%%% caller is expected to disregard the request and return the orginal `Base'
%%% for the interaction in an unmodified form.
-module(dev_security).
-include("include/hb.hrl").
-implements(<<"security@1.0">>).
%%% Device API.
-export([device_info/0, compute/3, validate/3]).
%%% Public helpers.
-export([validate_address/3]).

%% @doc Return the public security device API.
device_info() ->
    #{
        exports => [<<"compute">>, <<"validate">>]
    }.

%% @doc Compute the security-normalized request.
compute(Base, Req, Opts) ->
    ?event(security_debug, {compute_called, {base, Base}, {req, Req}}, Opts),
    maybe
        {ok, HydratedReq} ?= hydrate_assignment_body(Req, Opts),
        {ok, SecureReq1} ?= validate_assignment(Base, HydratedReq, Opts),
        {ok, _SecureReq2} ?= validate_authority(Base, SecureReq1, Opts)
    else
        {error, Reason} ->
            ?event(
                security_error,
                {security_error,
                    {slot, hb_maps:get(<<"slot">>, Req, no_slot, Opts)},
                    {reason, Reason}
                },
                Opts
            ),
            {skip, Reason}
    end.

%% @doc Restore a cache-linked body's commitment set before verifying the
%% scheduler's commitment over the Assignment. A raw map update is intentional:
%% `hb_ao:set' would invalidate the outer commitment we are reconstructing.
hydrate_assignment_body(Assignment, Opts) when is_map(Assignment) ->
    case hb_ao:get(<<"body">>, Assignment, not_found, Opts) of
        Body when is_map(Body) ->
            case hydrate_subject(Body, Opts) of
                {ok, HydratedBody} ->
                    % Preserve commitments while forcing literal field access.
                    {ok, Assignment#{
                        <<"body">> => HydratedBody#{ <<"device">> => <<"message@1.0">> }
                    }};
                {error, _} = Error ->
                    Error
            end;
        _ ->
            {error, <<"Security subject must be a message.">>}
    end;
hydrate_assignment_body(_Assignment, _Opts) ->
    {error, <<"Security subject must be a message.">>}.

%% @doc Validate a caller-controlled security intent through the device API.
%% Expected request keys:
%% - `key`: the base security policy prefix to validate, such as
%%   `set-authority`.
%% - `from`: optional identity or identities to validate. If omitted, the
%%   subject message signers are used.
%% - `subject`: optional message used for signer extraction and event context.
validate(Base, Req, Opts) ->
    maybe
        Key = hb_ao:get(<<"key">>, Req, not_found, Opts),
        true ?= (Key =/= not_found) orelse {error, <<"Security key not found.">>},
        SubjectMsg = hb_ao:get(<<"subject">>, Req, Req, Opts),
        Res =
            case hb_ao:get(<<"from">>, Req, not_found, Opts) of
                not_found -> validate(Key, Base, SubjectMsg, Opts);
                From -> validate(Key, Base, SubjectMsg, From, Opts)
            end,
        true ?= Res,
        {ok, true}
    else
        {error, Reason} -> {error, Reason}
    end.

%% @doc Validate that an assignment is trusted based on scheduler constraints.
validate_assignment(Base, Assignment, Opts) ->
    case arweave_scheduler_assignment(Base, Assignment, Opts) of
        true -> {ok, Assignment};
        false -> validate_signed_assignment(Base, Assignment, Opts)
    end.

validate_signed_assignment(Base, Assignment, Opts) ->
    maybe
        {ok, Signers} ?= verified_signers(Assignment, Opts),
        true ?= validate(<<"scheduler">>, Base, Assignment, Signers, Opts),
        {ok, Assignment}
    else
        {error, Reason} -> {error, Reason}
    end.

arweave_scheduler_assignment(Base, Assignment, Opts) ->
    hb_ao:get(<<"scheduler-device">>, Base, undefined, Opts) =:= <<"arweave-scheduler@1.0">>
        andalso hb_ao:get(<<"type">>, Assignment, undefined, Opts) =:= <<"Assignment">>
        andalso hb_ao:get(<<"path">>, Assignment, undefined, Opts) =:= <<"compute">>
        andalso hb_ao:get(<<"process">>, Assignment, undefined, Opts) =:= current_process_id(Base, Opts)
        andalso hb_message:verify(Assignment, #{ <<"commitment-ids">> => <<"all">> }, Opts).

current_process_id(Base, Opts) ->
    hb_message:id(hb_ao:get(<<"process">>, Base, #{}, Opts), signed, Opts).

%% @doc Validate that a request has proper authority, adding a `from' key to the
%% assigned message such that downstream callers can refer to a verified sender
%% (for replies, etc) -- whether an end-user wallet or another process.
validate_authority(Base, Assignment, Opts) ->
    Msg = hb_ao:get(<<"body">>, Assignment, undefined, Opts),
    maybe
        {ok, Signers} ?= verified_signers(Msg, Opts),
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
                maybe
                    %% A delegated sender must be one valid identity, never a
                    %% list interpreted as multiple authorization candidates.
                    true ?= validate_address(Sender, [], Opts),
                    true ?= (length(Signers) =:= 1) orelse
                        {error, <<"Delegated messages require exactly one signer.">>},
                    true ?= validate(<<"authority">>, Base, Msg, Signers, Opts),
                    true ?= validate_authority_action(Base, Msg, Opts),
                    {
                        ok,
                        hb_ao:set(
                            Assignment,
                            <<"body/from">>,
                            Sender,
                            Opts
                        )
                    }
                end
        end
    end.

%% @doc Restrict process-delegated identities to explicitly configured actions.
%% Wallet-signed messages do not carry `from-process' and do not enter this path.
validate_authority_action(Base, Msg, Opts) ->
    try
        Action = hb_ao:get(<<"action">>, Msg, not_found, Opts),
        Allowed = authority_actions(Base, Opts),
        Valid =
            is_binary(Action) andalso byte_size(Action) > 0 andalso
                is_list(Allowed) andalso Allowed =/= [] andalso
                lists:all(
                    fun(Item) -> is_binary(Item) andalso byte_size(Item) > 0 end,
                    Allowed
                ),
        case Valid andalso lists:member(
            hb_util:to_lower(Action),
            [hb_util:to_lower(Item) || Item <- Allowed]
        ) of
            true -> true;
            false -> {error, <<"Delegated action not allowed.">>}
        end
    catch
        _:_ -> {error, <<"Delegated action not allowed.">>}
    end.

authority_actions(Base, Opts) ->
    case hb_ao:get(<<"authority-actions">>, Base, [], Opts) of
        Allowed when is_list(Allowed) ->
            Allowed;
        Encoded when is_binary(Encoded) ->
            decode_authority_actions(Encoded);
        _ ->
            []
    end.

decode_authority_actions(Encoded) ->
    try
        [
            hb_structured_fields:from_bare_item(Item)
        ||
            {item, Item, _Params} <- hb_structured_fields:parse_list(Encoded)
        ]
    catch
        _:_ -> []
    end.

%% @doc If a message purporting to be from a process satisfies the compute
%% authority constraints, return true, otherwise return false.
validate(Key, Base, SubjectMsg, Opts) ->
    validate(
        Key,
        Base,
        SubjectMsg,
        [hb_util:committer_address(S) || S <- hb_message:signers(SubjectMsg, Opts)],
        Opts
    ).
validate(Key, Base, SubjectMsg, RawFrom, Opts) ->
    Template = security_template(Key, Base, Opts),
    validate_with_template(Template, Key, Base, SubjectMsg, RawFrom, Opts).

%% @doc Return only signer identities whose commitments verify against the
%% subject. Signer metadata alone is not authentication and an empty signer set
%% must never satisfy a security policy.
verified_signers(SubjectMsg, Opts) when is_map(SubjectMsg) ->
    try
        Signers = lists:uniq(hb_message:signers(SubjectMsg, Opts)),
        case Signers of
            [] ->
                {error, <<"Security subject has no signers.">>};
            _ ->
                case hb_message:verify(SubjectMsg, Signers, Opts) of
                    true -> {ok, [hb_util:committer_address(S) || S <- Signers]};
                    false ->
                        {error, <<"Security subject signature verification failed.">>}
                end
        end
    catch
        _:_ -> {error, <<"Security subject signature verification failed.">>}
    end;
verified_signers(_SubjectMsg, _Opts) ->
    {error, <<"Security subject must be a message.">>}.

%% @doc Explicit commitment reads are required for aggregate cache links. This
%% helper is only used for a body bound by an outer Assignment commitment. Do not
%% expand a body that already carries signers: doing so would allow a later
%% commitment to change the signer set of an existing Assignment.
hydrate_subject(SubjectMsg, Opts) ->
    try
        case hb_message:signers(SubjectMsg, Opts) of
            [] -> {ok, hb_cache:read_all_commitments(SubjectMsg, Opts)};
            _ -> {ok, SubjectMsg}
        end
    catch
        _:_ -> {error, <<"Security subject signature verification failed.">>}
    end.

validate_with_template(<<"static-signer-set">>, Key, Base, SubjectMsg, RawFrom, Opts) ->
    maybe
        true ?=
            case requires_static_policy(Key, Opts) of
                true ->
                    has_static_signer_policy(Key, Base, Opts)
                        orelse {error, <<"Security policy not configured.">>};
                false ->
                    true
            end,
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
        true ?= valid_static_signer_policy(Key, Base, Valid, RequiredList, Opts),
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
    end;
validate_with_template(<<"supply-threshold-owner">>, Key, _Base, _SubjectMsg, _RawFrom, _Opts)
        when Key =/= <<"set-authority">> ->
    {error, <<"Supply-threshold owner template only supports set-authority.">>};
validate_with_template(<<"supply-threshold-owner">>, Key, Base, _SubjectMsg, RawFrom, Opts) ->
    maybe
        true ?= (not has_static_policy(Key, Base, Opts))
            orelse {error, <<"Ambiguous security policy configuration.">>},
        {ok, Candidate} ?= single_authority_candidate(RawFrom, Opts),
        {ok, Balance} ?= candidate_balance(Candidate, Base, Opts),
        {ok, TotalSupply} ?= total_supply(Base, Opts),
        {ok, ThresholdBps} ?= threshold_bps(Key, Base, Opts),
        true ?= (Balance =< TotalSupply)
            orelse {error, <<"Balance exceeds total supply.">>},
        Res =
            (Balance * 10000 >= TotalSupply * ThresholdBps)
            orelse {error, <<"Supply-threshold owner requirement not satisfied.">>},
        ?event(
            security_short,
            {supply_threshold_owner_check,
                {intent, Key},
                {candidate, Candidate},
                {balance, Balance},
                {total_supply, TotalSupply},
                {threshold_bps, ThresholdBps},
                {result, Res}
            },
            Opts
        ),
        Res
    end;
validate_with_template(_Template, _Key, _Base, _SubjectMsg, _RawFrom, _Opts) ->
    {error, <<"Unknown security template.">>}.

security_template(Key, Base, Opts) ->
    case hb_ao:get(<<Key/binary, "-template">>, Base, not_found, Opts) of
        not_found ->
            default_template(Key, Base, Opts);
        Template ->
            Template
    end.

default_template(<<"set-authority">>, Base, Opts) ->
    case has_static_policy(<<"set-authority">>, Base, Opts) of
        true -> <<"static-signer-set">>;
        false -> <<"supply-threshold-owner">>
    end;
default_template(_Key, _Base, _Opts) ->
    <<"static-signer-set">>.

requires_static_policy(<<"set-authority">>, _Opts) ->
    true;
requires_static_policy(_Key, Opts) ->
    is_prod_mode(Opts).

is_prod_mode(Opts) ->
    case maps:get(dev_security_mode, Opts, maps:get(<<"dev-security-mode">>, Opts, dev)) of
        prod -> true;
        <<"prod">> -> true;
        _ -> false
    end.

has_static_policy(Key, Base, Opts) ->
    hb_ao:get(Key, Base, not_found, Opts) =/= not_found orelse
    hb_ao:get(<<Key/binary, "-required">>, Base, not_found, Opts) =/= not_found orelse
    hb_ao:get(<<Key/binary, "-match">>, Base, not_found, Opts) =/= not_found.

has_static_signer_policy(Key, Base, Opts) ->
    hb_ao:get(Key, Base, not_found, Opts) =/= not_found orelse
    hb_ao:get(<<Key/binary, "-required">>, Base, not_found, Opts) =/= not_found.

valid_static_signer_policy(Key, Base, Valid, Required, Opts) ->
    case requires_static_policy(Key, Opts) orelse has_static_policy(Key, Base, Opts) of
        false ->
            true;
        true ->
            case Valid ++ Required of
                [] ->
                    {error, <<"Security policy not configured.">>};
                Signers ->
                    lists:all(fun valid_static_signer/1, Signers)
                        orelse {error, <<"Security signer cannot be empty.">>}
            end
    end.

valid_static_signer(Signer) when is_binary(Signer) ->
    byte_size(Signer) > 0;
valid_static_signer(_Signer) ->
    true.

single_authority_candidate(RawFrom, Opts) ->
    case lists:uniq(as_list(RawFrom, Opts)) of
        [Candidate] ->
            maybe
                true ?= validate_address(Candidate, [], Opts),
                {ok, Candidate}
            end;
        [] ->
            {error, <<"Authority candidate not found.">>};
        _ ->
            {error, <<"Supply-threshold owner requires exactly one candidate.">>}
    end.

candidate_balance(Candidate, Base, Opts) ->
    case hb_ao:get(<<"balances">>, Base, not_found, Opts) of
        not_found ->
            {error, <<"Balances not configured.">>};
        _Balances ->
            case hb_ao:raw(
                <<"token@1.0">>,
                <<"balance">>,
                Base,
                #{ <<"balance">> => Candidate },
                Opts
            ) of
                {ok, Balance} when is_integer(Balance), Balance >= 0 ->
                    {ok, Balance};
                {ok, Balance} when is_integer(Balance) ->
                    {error, <<"Balance cannot be negative.">>};
                {ok, _Balance} ->
                    {error, <<"Balance must be an integer.">>};
                {error, not_found} ->
                    {ok, 0};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

total_supply(Base, Opts) ->
    case hb_ao:get(<<"total-supply">>, Base, not_found, Opts) of
        not_found ->
            {error, <<"Total supply not configured.">>};
        Raw ->
            case parse_integer(Raw) of
                TotalSupply when TotalSupply > 0 -> {ok, TotalSupply};
                TotalSupply when is_integer(TotalSupply) ->
                    {error, <<"Total supply must be positive.">>};
                Error -> Error
            end
    end.

threshold_bps(Key, Base, Opts) ->
    ThresholdRaw = hb_ao:get(<<Key/binary, "-threshold-bps">>, Base, 10000, Opts),
    case parse_integer(ThresholdRaw) of
        ThresholdBps when
                is_integer(ThresholdBps),
                ThresholdBps >= 1,
                ThresholdBps =< 10000 ->
            {ok, ThresholdBps};
        ThresholdBps when is_integer(ThresholdBps) ->
            {error, <<"Threshold basis points out of range.">>};
        Error ->
            Error
    end.

parse_integer(Value) when is_integer(Value) ->
    Value;
parse_integer(Value) when is_binary(Value) ->
    try binary_to_integer(Value) of
        Int -> Int
    catch
        _:_ -> {error, <<"Integer value is invalid.">>}
    end;
parse_integer(_Value) ->
    {error, <<"Integer value is invalid.">>}.

id_key(ID) -> hb_util:to_lower(hb_util:bin(ID)).

validate_address(Address, _, _) when not is_binary(Address) ->
    {error, <<"Address must be a binary.">>};
validate_address(Address, CustomList, Opts) ->
    validate_address(Address, CustomList, Opts, id_key(Address)).

validate_address(Address, CustomList, Opts, IDKey)
        when is_binary(Address), is_list(CustomList) ->
    CanonicalCustomKeys = [id_key(Key) || Key <- CustomList, is_binary(Key)],
    case byte_size(Address) of
        0 -> {error, <<"Address cannot be empty.">>};
        N when N > 128 -> {error, <<"Address is too long.">>};
        _ ->
            TrieReservedKeys = trie_reserved_keys(Opts),
            maybe
                true ?= (IDKey =/= <<"path">>)
                    orelse {error, <<"Address uses the reserved path key.">>},
                true ?= (not is_device_key(IDKey, Opts))
                    orelse {error, <<"Address uses a reserved device key.">>},
                true ?= (not is_reserved_trie_key(Address, TrieReservedKeys))
                    orelse {error, <<"Address uses a reserved trie internal key.">>},
                true ?= (not is_reserved_trie_key(IDKey, TrieReservedKeys))
                    orelse {error, <<"Address uses a reserved trie internal key.">>},
                true ?= (not is_reserved_custom_key(Address, CustomList))
                    orelse {error, <<"Address is a reserved custom key.">>},
                true ?= (not is_reserved_custom_key(IDKey, CanonicalCustomKeys))
                    orelse {error, <<"Address is a reserved custom key.">>},
                true ?= valid_address_chars(Address)
                    orelse {error, <<"Address contains unsupported characters.">>}
            end
    end;
validate_address(_, _, _, _) ->
    {error, <<"Address must be a binary.">>}.

is_device_key(Key, Opts) ->
    lists:any(
        fun(Device) ->
            case hb_device:message_to_fun(
                #{<<"device">> => Device}, Key, Opts
            ) of
                {ok, _, _} -> true;
                {add_key, _, _} -> false
            end
        end,
        [<<"message@1.0">>, <<"trie@1.0">>]
    ).

trie_reserved_keys(Opts) ->
    {ok, Trie} = hb_device_load:reference(<<"trie@1.0">>, Opts),
    maps:get(reserved, Trie:device_info(), []).

is_reserved_trie_key(Key, ReservedKeys) ->
    lists:member(Key, ReservedKeys).

is_reserved_custom_key(Key, List) when is_binary(Key), is_list(List) ->
    lists:member(Key, List);
is_reserved_custom_key(_, _) ->
    false.

valid_address_chars(<<>>) ->
    true;
valid_address_chars(<<Char, Rest/binary>>) when
        Char >= $A, Char =< $Z;
        Char >= $a, Char =< $z;
        Char >= $0, Char =< $9;
        Char =:= $_;
        Char =:= $- ->
    valid_address_chars(Rest);
valid_address_chars(_) ->
    false.

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
