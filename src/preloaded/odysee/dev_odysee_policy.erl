%%% @doc Signed Odysee content-policy artifact evaluator.
%%%
%%% The policy itself is a normal AO-Core message with
%%% `device = odysee-policy@1.0' and a signed `rules' key. Rules match only
%%% public LBRY/Odysee identifiers and optional country codes; private auth
%%% context is deliberately outside this device's matching surface.
-module(dev_odysee_policy).
-implements(<<"odysee-policy@1.0">>).
-export([info/1, evaluate/3, enforce/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEVICE, <<"odysee-policy@1.0">>).

%% @doc Return the public device API.
info(_Opts) ->
    #{ exports => [<<"evaluate">>, <<"enforce">>] }.

%% @doc Evaluate a signed policy artifact for the target message.
evaluate(Base, Req, Opts) ->
    safe(fun() ->
        case load_policy(Base, Req, Opts) of
            not_found ->
                {ok, allow_decision(<<"no-policy">>, not_found)};
            {ok, Policy} ->
                case verify_policy(Policy, Opts) of
                    true ->
                        {ok, decide(Policy, target_context(Base, Req, Opts), Opts)};
                    false ->
                        {error, unsigned_or_invalid_policy}
                end;
            Error ->
                Error
        end
    end).

%% @doc Evaluate and return a serving response when policy denies the target.
enforce(Base, Req, Opts) ->
    case evaluate(Base, Req, Opts) of
        {ok, Decision = #{ <<"decision">> := <<"deny">> }} ->
            {ok, denied_response(Decision)};
        Other ->
            Other
    end.

safe(Fun) ->
    try Fun() of
        Res -> Res
    catch
        _:{error, Reason} -> {error, Reason};
        _:Reason -> {error, Reason}
    end.

load_policy(Base, Req, Opts) ->
    case configured_policy(Base, Req, Opts) of
        not_found -> not_found;
        Policy -> normalize_policy(Policy, Opts)
    end.

configured_policy(Base, Req, Opts) ->
    case
        first_found(
            [
                {Req, <<"odysee-policy">>},
                {Req, <<"policy">>},
                {Base, <<"odysee-policy">>},
                {Base, <<"policy">>}
            ],
            Opts
        )
    of
        not_found ->
            hb_opts:get(
                <<"odysee-policy">>,
                hb_opts:get(odysee_policy, not_found, Opts),
                Opts
            );
        Policy ->
            Policy
    end.

normalize_policy(Policy, Opts) when ?IS_LINK(Policy) ->
    normalize_policy(hb_cache:ensure_loaded(Policy, Opts), Opts);
normalize_policy(Policy, _Opts) when is_map(Policy) ->
    {ok, Policy};
normalize_policy(Policy, Opts) when is_binary(Policy) ->
    case try_decode_json(Policy) of
        {ok, Msg} when is_map(Msg) -> normalize_policy(Msg, Opts);
        _ -> {error, invalid_policy}
    end;
normalize_policy(_Policy, _Opts) ->
    {error, invalid_policy}.

try_decode_json(Bin) ->
    try {ok, hb_json:decode(Bin)}
    catch _:_ -> error
    end.

verify_policy(Policy, Opts) ->
    Commitments = hb_maps:get(<<"commitments">>, Policy, #{}, Opts),
    is_map(Commitments)
        andalso map_size(Commitments) > 0
        andalso hb_maps:get(<<"device">>, Policy, not_found, Opts) =:= ?DEVICE
        andalso hb_message:verify(
            Policy,
            #{ <<"commitment-ids">> => <<"all">> },
            Opts
        )
        andalso signed_key(<<"device">>, Policy, Opts)
        andalso signed_key(<<"rules">>, Policy, Opts).

signed_key(Key, Policy, Opts) ->
    lists:member(Key, hb_message:committed(Policy, all, Opts)).

decide(Policy, Context, Opts) ->
    Rules = hb_maps:get(<<"rules">>, Policy, [], Opts),
    case first_matching_rule(Rules, Context, Opts) of
        not_found ->
            allow_decision(<<"no-matching-rule">>, policy_id(Policy, Opts));
        Rule ->
            decision_from_rule(Rule, Policy, Opts)
    end.

first_matching_rule([], _Context, _Opts) ->
    not_found;
first_matching_rule([Rule | Rest], Context, Opts) when is_map(Rule) ->
    case rule_matches(Rule, Context, Opts) of
        true -> Rule;
        false -> first_matching_rule(Rest, Context, Opts)
    end;
first_matching_rule([_ | Rest], Context, Opts) ->
    first_matching_rule(Rest, Context, Opts);
first_matching_rule(_Rules, _Context, _Opts) ->
    not_found.

decision_from_rule(Rule, Policy, Opts) ->
    PolicyID = policy_id(Policy, Opts),
    RuleID = hb_maps:get(<<"id">>, Rule, not_found, Opts),
    Reason = hb_maps:get(<<"reason">>, Rule, <<"content-policy">>, Opts),
    case normalize_action(hb_maps:get(<<"action">>, Rule, <<"deny">>, Opts)) of
        <<"allow">> ->
            (allow_decision(Reason, PolicyID))#{
                <<"matched-rule-id">> => RuleID
            };
        <<"deny">> ->
            deny_decision(Reason, PolicyID, RuleID)
    end.

normalize_action(Action) ->
    case hb_util:to_lower(hb_util:bin(Action)) of
        <<"allow">> -> <<"allow">>;
        <<"permit">> -> <<"allow">>;
        <<"pass">> -> <<"allow">>;
        _ -> <<"deny">>
    end.

allow_decision(Reason, PolicyID) ->
    #{
        <<"device">> => ?DEVICE,
        <<"status">> => 200,
        <<"decision">> => <<"allow">>,
        <<"reason">> => Reason,
        <<"policy-id">> => PolicyID
    }.

deny_decision(Reason, PolicyID, RuleID) ->
    #{
        <<"device">> => ?DEVICE,
        <<"status">> => 451,
        <<"decision">> => <<"deny">>,
        <<"reason">> => Reason,
        <<"policy-id">> => PolicyID,
        <<"matched-rule-id">> => RuleID
    }.

denied_response(Decision) ->
    Body = hb_json:encode(Decision),
    #{
        <<"status">> => 451,
        <<"reason">> => hb_maps:get(<<"reason">>, Decision, <<"content-policy">>, #{}),
        <<"content-type">> => <<"application/json">>,
        <<"content-length">> => byte_size(Body),
        <<"body">> => Body
    }.

policy_id(Policy, Opts) ->
    try hb_message:id(Policy, signed, Opts)
    catch _:_ -> not_found
    end.

rule_matches(Rule, Context, Opts) ->
    Checks = selector_checks(),
    {HadSelector, Matched} =
        lists:foldl(
            fun(Check, {Had, Acc}) ->
                case match_selector(Check, Rule, Context, Opts) of
                    absent -> {Had, Acc};
                    true -> {true, Acc};
                    false -> {true, false}
                end
            end,
            {false, true},
            Checks
        ),
    HadSelector andalso Matched.

selector_checks() ->
    [
        {<<"claim-id">>, [<<"claim-id">>, <<"claim-ids">>]},
        {<<"channel-id">>, [<<"channel-id">>, <<"channel-ids">>]},
        {<<"sd-hash">>, [<<"sd-hash">>, <<"sd-hashes">>, <<"descriptor-id">>]},
        {<<"blob-hash">>, [<<"blob-hash">>, <<"blob-hashes">>, <<"blob-id">>]},
        {<<"source-hash">>, [<<"source-hash">>, <<"source-hashes">>]},
        {<<"txid">>, [<<"txid">>, <<"txids">>]},
        {<<"outpoint">>, [<<"outpoint">>, <<"outpoints">>]},
        {<<"country">>, [<<"country">>, <<"countries">>, <<"geo-country">>]}
    ].

match_selector({ContextKey, RuleKeys}, Rule, Context, Opts) ->
    case first_value(RuleKeys, Rule, Opts) of
        not_found ->
            absent;
        RuleValue ->
            case hb_maps:get(ContextKey, Context, not_found, Opts) of
                not_found -> false;
                ContextValue -> value_matches(ContextValue, RuleValue)
            end
    end.

value_matches(_ContextValue, <<"*">>) ->
    true;
value_matches(ContextValue, RuleValues) when is_list(RuleValues) ->
    lists:any(
        fun(RuleValue) -> value_matches(ContextValue, RuleValue) end,
        RuleValues
    );
value_matches(ContextValues, RuleValue) when is_list(ContextValues) ->
    lists:any(
        fun(ContextValue) -> value_matches(ContextValue, RuleValue) end,
        ContextValues
    );
value_matches(ContextValue, RuleValue) ->
    normalize_value(ContextValue) =:= normalize_value(RuleValue).

normalize_value(Value) when is_binary(Value) ->
    hb_util:to_lower(Value);
normalize_value(Value) ->
    hb_util:to_lower(hb_util:bin(Value)).

target_context(Base, Req, Opts) ->
    Claim = claim_context(Base, Req, Opts),
    Value = nested_map(Claim, <<"value">>, Opts),
    Source = nested_map(Value, <<"source">>, Opts),
    Msg0 =
        lists:foldl(
            fun({ContextKey, Sources}, Acc) ->
                put_optional(
                    {ContextKey, first_context_value(Sources, Base, Req, Opts)},
                    Acc
                )
            end,
            #{},
            direct_context_sources()
        ),
    Msg1 =
        lists:foldl(
            fun({ContextKey, Sources}, Acc) ->
                put_optional(
                    {ContextKey, first_context_value(Sources, Claim, #{}, Opts)},
                    Acc
                )
            end,
            Msg0,
            claim_context_sources()
        ),
    Msg2 =
        put_optional(
            {<<"sd-hash">>,
                first_context_value(
                    [
                        {Source, <<"sd-hash">>},
                        {Source, <<"sd_hash">>},
                        {Msg1, <<"sd-hash">>}
                    ],
                    #{},
                    #{},
                    Opts
                )},
            Msg1
        ),
    Msg3 =
        put_optional(
            {<<"source-hash">>,
                first_context_value(
                    [
                        {Source, <<"hash">>},
                        {Source, <<"source-hash">>},
                        {Msg2, <<"source-hash">>}
                    ],
                    #{},
                    #{},
                    Opts
                )},
            Msg2
        ),
    Msg4 =
        put_optional(
            {<<"channel-id">>, signing_channel_id(Claim, Msg3, Opts)},
            Msg3
        ),
    put_optional({<<"outpoint">>, outpoint(Msg4, Opts)}, Msg4).

direct_context_sources() ->
    [
        {<<"claim-id">>, [<<"claim-id">>, <<"claim_id">>]},
        {<<"channel-id">>, [<<"channel-id">>, <<"channel_id">>]},
        {<<"sd-hash">>, [<<"sd-hash">>, <<"sd_hash">>, <<"descriptor-id">>]},
        {<<"blob-hash">>, [<<"blob-hash">>, <<"blob_hash">>, <<"blob-id">>]},
        {<<"source-hash">>, [<<"source-hash">>, <<"source_hash">>]},
        {<<"txid">>, [<<"txid">>]},
        {<<"nout">>, [<<"nout">>]},
        {<<"outpoint">>, [<<"outpoint">>]},
        {<<"country">>, [
            <<"country">>,
            <<"geo-country">>,
            <<"geo_country">>,
            <<"cf-ipcountry">>,
            <<"x-country">>
        ]}
    ].

claim_context_sources() ->
    [
        {<<"claim-id">>, [<<"claim-id">>, <<"claim_id">>]},
        {<<"txid">>, [<<"txid">>]},
        {<<"nout">>, [<<"nout">>]}
    ].

first_context_value([], _Base, _Req, _Opts) ->
    not_found;
first_context_value([{Map, Key} | Rest], Base, Req, Opts) when is_map(Map) ->
    case hb_maps:get(Key, Map, not_found, Opts) of
        not_found -> first_context_value(Rest, Base, Req, Opts);
        Value -> Value
    end;
first_context_value([Key | Rest], Base, Req, Opts) ->
    case first_found([{Req, Key}, {Base, Key}], Opts) of
        not_found -> first_context_value(Rest, Base, Req, Opts);
        Value -> Value
    end.

claim_context(Base, Req, Opts) ->
    case first_found([{Req, <<"claim">>}, {Base, <<"claim">>}], Opts) of
        Claim when is_map(Claim) -> Claim;
        _ ->
            case first_found([{Req, <<"claim-message">>}, {Base, <<"claim-message">>}], Opts) of
                ClaimMsg when is_map(ClaimMsg) ->
                    hb_maps:get(<<"claim">>, ClaimMsg, ClaimMsg, Opts);
                _ ->
                    #{}
            end
    end.

nested_map(Map, Key, Opts) when is_map(Map) ->
    case hb_maps:get(Key, Map, #{}, Opts) of
        Nested when is_map(Nested) -> Nested;
        _ -> #{}
    end;
nested_map(_Map, _Key, _Opts) ->
    #{}.

signing_channel_id(Claim, Context, Opts) ->
    case hb_maps:get(<<"channel-id">>, Context, not_found, Opts) of
        not_found -> signing_channel_id(Claim, Opts);
        ChannelID -> ChannelID
    end.

signing_channel_id(Claim, Opts) when is_map(Claim) ->
    case first_value([<<"signing-channel">>, <<"signing_channel">>], Claim, Opts) of
        Channel when is_map(Channel) ->
            first_value([<<"claim-id">>, <<"claim_id">>], Channel, Opts);
        _ ->
            not_found
    end;
signing_channel_id(_Claim, _Opts) ->
    not_found.

outpoint(Context, Opts) ->
    case hb_maps:get(<<"outpoint">>, Context, not_found, Opts) of
        not_found ->
            case {hb_maps:get(<<"txid">>, Context, not_found, Opts),
                  hb_maps:get(<<"nout">>, Context, not_found, Opts)} of
                {TxID, NOut} when is_binary(TxID) ->
                    <<TxID/binary, ":", (hb_util:bin(NOut))/binary>>;
                _ ->
                    not_found
            end;
        Outpoint ->
            Outpoint
    end.

first_value([], _Map, _Opts) ->
    not_found;
first_value([Key | Rest], Map, Opts) when is_map(Map) ->
    case hb_maps:get(Key, Map, not_found, Opts) of
        not_found -> first_value(Rest, Map, Opts);
        Value -> Value
    end;
first_value(_Keys, _Map, _Opts) ->
    not_found.

first_found([], _Opts) ->
    not_found;
first_found([{Msg, Key} | Rest], Opts) when is_map(Msg) ->
    case hb_maps:get(Key, Msg, not_found, Opts) of
        not_found -> first_found(Rest, Opts);
        Value -> Value
    end;
first_found([_ | Rest], Opts) ->
    first_found(Rest, Opts).

put_optional({_Key, not_found}, Msg) -> Msg;
put_optional({Key, Value}, Msg) -> Msg#{ Key => Value }.

-ifdef(TEST).

signed_policy(Rules) ->
    hb_message:commit(
        #{
            <<"device">> => ?DEVICE,
            <<"policy-version">> => <<"1">>,
            <<"rules">> => Rules
        },
        #{ <<"priv-wallet">> => ar_wallet:new() }
    ).

deny_rule() ->
    #{
        <<"id">> => <<"dmca-gps">>,
        <<"action">> => <<"deny">>,
        <<"reason">> => <<"dmca">>,
        <<"claim-id">> => <<"346c1fed0fbc2f0b3ecc8bf3915aa8aaa029c169">>
    }.

evaluate_allows_without_policy_test() ->
    {ok, Decision} = evaluate(#{}, #{}, #{}),
    ?assertEqual(<<"allow">>, hb_maps:get(<<"decision">>, Decision, #{})),
    ?assertEqual(<<"no-policy">>, hb_maps:get(<<"reason">>, Decision, #{})).

evaluate_denies_signed_claim_rule_test() ->
    Policy = signed_policy([deny_rule()]),
    {ok, Decision} =
        evaluate(
            #{ <<"claim-id">> => <<"346c1fed0fbc2f0b3ecc8bf3915aa8aaa029c169">> },
            #{ <<"odysee-policy">> => Policy },
            #{}
        ),
    ?assertEqual(<<"deny">>, hb_maps:get(<<"decision">>, Decision, #{})),
    ?assertEqual(451, hb_maps:get(<<"status">>, Decision, #{})),
    ?assertEqual(<<"dmca-gps">>, hb_maps:get(<<"matched-rule-id">>, Decision, #{})).

evaluate_rejects_unsigned_policy_test() ->
    Policy = #{
        <<"device">> => ?DEVICE,
        <<"rules">> => [deny_rule()]
    },
    ?assertEqual(
        {error, unsigned_or_invalid_policy},
        evaluate(
            #{ <<"claim-id">> => <<"346c1fed0fbc2f0b3ecc8bf3915aa8aaa029c169">> },
            #{ <<"odysee-policy">> => Policy },
            #{}
        )
    ).

country_rule_requires_country_match_test() ->
    Policy =
        signed_policy([
            #{
                <<"id">> => <<"geo-us">>,
                <<"action">> => <<"deny">>,
                <<"reason">> => <<"geoblock">>,
                <<"claim-id">> => <<"346c1fed0fbc2f0b3ecc8bf3915aa8aaa029c169">>,
                <<"countries">> => [<<"US">>]
            }
        ]),
    Base = #{ <<"claim-id">> => <<"346c1fed0fbc2f0b3ecc8bf3915aa8aaa029c169">> },
    {ok, Allow} =
        evaluate(Base, #{ <<"odysee-policy">> => Policy, <<"country">> => <<"CA">> }, #{}),
    {ok, Deny} =
        evaluate(Base, #{ <<"odysee-policy">> => Policy, <<"country">> => <<"US">> }, #{}),
    ?assertEqual(<<"allow">>, hb_maps:get(<<"decision">>, Allow, #{})),
    ?assertEqual(<<"deny">>, hb_maps:get(<<"decision">>, Deny, #{})),
    ?assertEqual(<<"geoblock">>, hb_maps:get(<<"reason">>, Deny, #{})).

enforce_returns_451_response_test() ->
    Policy = signed_policy([deny_rule()]),
    {ok, Res} =
        enforce(
            #{ <<"claim-id">> => <<"346c1fed0fbc2f0b3ecc8bf3915aa8aaa029c169">> },
            #{ <<"odysee-policy">> => Policy },
            #{}
        ),
    Body = hb_json:decode(hb_maps:get(<<"body">>, Res, #{})),
    ?assertEqual(451, hb_maps:get(<<"status">>, Res, #{})),
    ?assertEqual(<<"deny">>, hb_maps:get(<<"decision">>, Body, #{})).

-endif.
