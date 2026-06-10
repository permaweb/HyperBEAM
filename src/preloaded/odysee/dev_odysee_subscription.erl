%%% @doc Odysee internal API subscription compatibility device.
%%%
%%% This device exposes read-only `/subscription/sub_count' responses as
%%% AO-Core messages. Follow/unfollow mutations remain outside this adapter.
-module(dev_odysee_subscription).
-implements(<<"odysee-subscription@1.0">>).
-export([info/1, sub_count/3, normalize/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEVICE, <<"odysee-subscription@1.0">>).
-define(DEFAULT_API_URL, <<"https://api.odysee.com">>).

%% @doc Return the public device API.
info(_Opts) ->
    #{ exports => [<<"sub-count">>, <<"normalize">>] }.

%% @doc Return normalized `/subscription/sub_count' data.
sub_count(Base, Req, Opts) ->
    safe(fun() ->
        maybe
            {ok, Counts, Raw} ?= count_result(Base, Req, Opts),
            {ok, ClaimIDs} ?= claim_ids(Base, Req, Counts, Opts),
            normalize_counts(Counts, Raw, ClaimIDs)
        else
            Error -> Error
        end
    end).

%% @doc Normalize supplied subscription count data without fetching.
normalize(Base, Req, Opts) ->
    safe(fun() ->
        case result_candidate(Base, Req, Opts) of
            {ok, Counts, Raw} ->
                maybe
                    {ok, ClaimIDs} ?= claim_ids(Base, Req, Counts, Opts),
                    normalize_counts(Counts, Raw, ClaimIDs)
                end;
            not_found ->
                {error, sub_count_result_not_found}
        end
    end).

safe(Fun) ->
    try Fun() of
        Res -> Res
    catch
        _:{error, Reason} -> {error, Reason};
        _:Reason -> {error, Reason}
    end.

count_result(Base, Req, Opts) ->
    case result_candidate(Base, Req, Opts) of
        {ok, _Counts, _Raw} = Candidate ->
            Candidate;
        not_found ->
            maybe
                {ok, Params} ?= count_params(Base, Req, Opts),
                api_request(Params, Base, Req, Opts)
            end
    end.

result_candidate(Base, Req, Opts) ->
    Candidates = [
        {Req, <<"sub-count-result">>},
        {Req, <<"sub_count_result">>},
        {Req, <<"counts">>},
        {Req, <<"sub-counts">>},
        {Req, <<"data">>},
        {Req, <<"result">>},
        {Req, <<"body">>},
        {Base, <<"sub-count-result">>},
        {Base, <<"sub_count_result">>},
        {Base, <<"counts">>},
        {Base, <<"sub-counts">>},
        {Base, <<"data">>},
        {Base, <<"result">>},
        {Base, <<"body">>}
    ],
    case result_from_value(Base, Opts) of
        {ok, _Counts, _Raw} = Result -> Result;
        not_found -> result_from_fields(Candidates, Opts)
    end.

result_from_fields([], _Opts) ->
    not_found;
result_from_fields([{Msg, Key} | Rest], Opts) when is_map(Msg) ->
    case hb_maps:get(Key, Msg, not_found, Opts) of
        not_found -> result_from_fields(Rest, Opts);
        Value ->
            case result_from_value(Value, Opts) of
                {ok, _Counts, _Raw} = Result -> Result;
                not_found -> result_from_fields(Rest, Opts)
            end
    end;
result_from_fields([_ | Rest], Opts) ->
    result_from_fields(Rest, Opts).

result_from_value(Value, Opts) when is_list(Value) ->
    result_from_counts(Value, hb_json:encode(Value), Opts);
result_from_value(Value, Opts) when is_map(Value) ->
    case result_from_map(Value, hb_json:encode(Value), Opts) of
        {ok, _Counts, _Raw} = Result -> Result;
        _ -> not_found
    end;
result_from_value(Value, Opts) when is_binary(Value) ->
    case try_decode_json(Value) of
        {ok, Decoded} -> result_from_decoded(Decoded, Value, Opts);
        _ -> not_found
    end;
result_from_value(_Value, _Opts) ->
    not_found.

result_from_decoded(Decoded, Raw, Opts) when is_list(Decoded) ->
    result_from_counts(Decoded, Raw, Opts);
result_from_decoded(Decoded, Raw, Opts) when is_map(Decoded) ->
    case result_from_map(Decoded, Raw, Opts) of
        {ok, _Counts, _Raw} = Result -> Result;
        _ -> not_found
    end;
result_from_decoded(_Decoded, _Raw, _Opts) ->
    not_found.

result_from_map(Msg, Raw, Opts) when is_map(Msg) ->
    case hb_maps:get(<<"error">>, Msg, not_found, Opts) of
        not_found -> result_from_success_map(Msg, Raw, Opts);
        Error -> {error, {subscription_api_error, Error}}
    end;
result_from_map(_Msg, _Raw, _Opts) ->
    {error, invalid_sub_count_result}.

result_from_success_map(Msg, Raw, Opts) ->
    case hb_maps:get(<<"success">>, Msg, true, Opts) of
        false ->
            {error, {subscription_api_error, hb_maps:get(<<"data">>, Msg, Msg, Opts)}};
        _ ->
            Result0 = hb_maps:get(<<"data">>, Msg, Msg, Opts),
            Result =
                case Result0 of
                    ResultMsg when is_map(ResultMsg) -> hb_maps:get(<<"result">>, ResultMsg, ResultMsg, Opts);
                    _ -> Result0
                end,
            case Result of
                Counts when is_list(Counts) -> result_from_counts(Counts, Raw, Opts);
                _ -> {error, invalid_sub_count_result}
            end
    end.

result_from_counts(Counts, Raw, _Opts) ->
    case lists:all(fun is_number/1, Counts) of
        true -> {ok, Counts, Raw};
        false -> {error, invalid_sub_count_result}
    end.

normalize_counts(Counts, Raw, ClaimIDs) ->
    case length(Counts) =:= length(ClaimIDs) of
        true ->
            {ok, #{
                <<"device">> => ?DEVICE,
                <<"content-type">> => <<"application/json">>,
                <<"body">> => Raw,
                <<"result">> => Counts,
                <<"counts">> => Counts,
                <<"sub-counts">> => Counts,
                <<"claim-ids">> => ClaimIDs,
                <<"by-claim-id">> => maps:from_list(lists:zip(ClaimIDs, Counts))
            }};
        false ->
            {error, sub_count_claim_id_mismatch}
    end.

count_params(Base, Req, Opts) ->
    case private_credential_present(Base, Req, Opts) of
        true ->
            {error, private_credentials_not_allowed};
        false ->
            Params = api_params(maps:merge(map_or_empty(Base), map_or_empty(Req)), Opts),
            case hb_maps:get(<<"claim_id">>, Params, not_found, Opts) of
                not_found -> {error, claim_id_not_found};
                _ClaimID -> {ok, Params}
            end
    end.

api_params(Params0, Opts) ->
    Params1 = put_alias(<<"claim_id">>, <<"claim-id">>, Params0, Opts),
    Params2 = put_alias(<<"claim_id">>, <<"claim_ids">>, Params1, Opts),
    Params3 = put_alias(<<"claim_id">>, <<"claim-ids">>, Params2, Opts),
    maps:without(control_keys() ++ request_metadata_keys() ++ private_credential_keys(), Params3).

control_keys() ->
    [
        <<"auth-token">>,
        <<"body">>,
        <<"claim-id">>,
        <<"claim-ids">>,
        <<"claim_ids">>,
        <<"content-type">>,
        <<"counts">>,
        <<"data">>,
        <<"device">>,
        <<"method">>,
        <<"odysee-api-url">>,
        <<"odysee_api_url">>,
        <<"path">>,
        <<"result">>,
        <<"sub-count-result">>,
        <<"sub-count-url">>,
        <<"sub-counts">>,
        <<"sub_count_result">>,
        <<"sub_count_url">>
    ].

request_metadata_keys() ->
    [
        <<"accept">>,
        <<"accept-bundle">>,
        <<"accept-language">>,
        <<"authorization">>,
        <<"connection">>,
        <<"content-length">>,
        <<"cookie">>,
        <<"host">>,
        <<"origin">>,
        <<"priv">>,
        <<"referer">>,
        <<"sec-ch-ua">>,
        <<"sec-ch-ua-mobile">>,
        <<"sec-ch-ua-platform">>,
        <<"sec-fetch-dest">>,
        <<"sec-fetch-mode">>,
        <<"sec-fetch-site">>,
        <<"user-agent">>
    ].

private_credential_keys() ->
    [
        <<"auth_token">>,
        <<"auth-token">>,
        <<"authorization">>,
        <<"access_token">>,
        <<"access-token">>,
        <<"refresh_token">>,
        <<"refresh-token">>
    ].

private_credential_present(Base, Req, Opts) ->
    first_param(private_credential_keys(), Base, Req, Opts) =/= not_found.

put_alias(Target, Source, Params, Opts) ->
    case hb_maps:get(Target, Params, not_found, Opts) of
        not_found ->
            case hb_maps:get(Source, Params, not_found, Opts) of
                not_found -> Params;
                Value -> Params#{ Target => Value }
            end;
        _Value ->
            Params
    end.

api_request(Params, Base, Req, Opts) ->
    Body = form_body(Params),
    Msg = #{
        <<"method">> => <<"POST">>,
        <<"path">> => sub_count_url(Base, Req, Opts),
        <<"content-type">> => <<"application/x-www-form-urlencoded">>,
        <<"body">> => Body
    },
    case hb_http:request(Msg, Opts) of
        {ok, #{ <<"body">> := RespBody }} when is_binary(RespBody) -> decode_api_body(RespBody, Opts);
        {ok, RespBody} when is_binary(RespBody) -> decode_api_body(RespBody, Opts);
        {ok, Other} -> {error, {subscription_response_without_body, Other}};
        Error -> Error
    end.

decode_api_body(Body, Opts) ->
    maybe
        {ok, Decoded} ?= try_decode_json(Body),
        case Decoded of
            Counts when is_list(Counts) -> result_from_counts(Counts, Body, Opts);
            Msg when is_map(Msg) -> result_from_map(Msg, Body, Opts);
            _ -> {error, invalid_sub_count_result}
        end
    end.

sub_count_url(Base, Req, Opts) ->
    case first_found(
        [
            {Req, <<"sub-count-url">>},
            {Req, <<"sub_count_url">>},
            {Base, <<"sub-count-url">>},
            {Base, <<"sub_count_url">>}
        ],
        Opts
    ) of
        not_found -> <<(trim_trailing_slash(api_url(Base, Req, Opts)))/binary, "/subscription/sub_count">>;
        URL -> URL
    end.

api_url(Base, Req, Opts) ->
    case first_found(
        [
            {Req, <<"odysee-api-url">>},
            {Req, <<"odysee_api_url">>},
            {Base, <<"odysee-api-url">>},
            {Base, <<"odysee_api_url">>}
        ],
        Opts
    ) of
        not_found -> hb_opts:get(<<"odysee-api-url">>, ?DEFAULT_API_URL, Opts);
        URL -> URL
    end.

claim_ids(Base, Req, Counts, Opts) ->
    case first_param([<<"claim_id">>, <<"claim-id">>, <<"claim_ids">>, <<"claim-ids">>], Base, Req, Opts) of
        not_found ->
            case Counts of
                [_ | _] -> {error, claim_id_not_found};
                [] -> {ok, []}
            end;
        ClaimIDCSV when is_binary(ClaimIDCSV) ->
            {ok, split_csv(ClaimIDCSV)};
        ClaimIDs when is_list(ClaimIDs) ->
            {ok, [hb_util:bin(ClaimID) || ClaimID <- ClaimIDs]};
        ClaimID ->
            {ok, [hb_util:bin(ClaimID)]}
    end.

split_csv(CSV) ->
    [Part || Part <- binary:split(CSV, <<",">>, [global]), Part =/= <<>>].

trim_trailing_slash(<<>>) ->
    <<>>;
trim_trailing_slash(Bin) ->
    Size = byte_size(Bin),
    case Bin of
        <<Prefix:(Size - 1)/binary, "/">> -> trim_trailing_slash(Prefix);
        _ -> Bin
    end.

form_body(Params) ->
    Pairs =
        [
            {binary_to_list(hb_util:bin(Key)), binary_to_list(form_value(Value))}
        ||
            {Key, Value} <- maps:to_list(Params),
            Value =/= undefined,
            Value =/= null,
            Value =/= not_found
        ],
    iolist_to_binary(uri_string:compose_query(Pairs)).

form_value(Value) when is_binary(Value) -> Value;
form_value(Value) when is_integer(Value) -> integer_to_binary(Value);
form_value(Value) when is_float(Value) -> float_to_binary(Value);
form_value(true) -> <<"true">>;
form_value(false) -> <<"false">>;
form_value(Value) when is_map(Value); is_list(Value) -> hb_json:encode(Value);
form_value(Value) -> hb_util:bin(Value).

map_or_empty(Map) when is_map(Map) -> Map;
map_or_empty(_Value) -> #{}.

first_param(Keys, Base, Req, Opts) ->
    first_found([{Req, Key} || Key <- Keys] ++ [{Base, Key} || Key <- Keys], Opts).

first_found([], _Opts) ->
    not_found;
first_found([{Msg, Key} | Rest], Opts) when is_map(Msg) ->
    case hb_maps:get(Key, Msg, not_found, Opts) of
        not_found -> first_found(Rest, Opts);
        Value -> Value
    end;
first_found([_ | Rest], Opts) ->
    first_found(Rest, Opts).

try_decode_json(Raw) ->
    try {ok, hb_json:decode(Raw)}
    catch _:_ -> {error, invalid_json}
    end.

-ifdef(TEST).

sub_count_internal_api_json_test() ->
    Raw = hb_json:encode(#{
        <<"success">> => true,
        <<"data">> => [169000, 42]
    }),
    {ok, Msg} = sub_count(
        #{},
        #{ <<"claim_id">> => <<"channel-1,channel-2">>, <<"body">> => Raw },
        #{}
    ),
    ?assertEqual([169000, 42], hb_maps:get(<<"result">>, Msg, #{})),
    ?assertEqual([<<"channel-1">>, <<"channel-2">>], hb_maps:get(<<"claim-ids">>, Msg, #{})),
    ?assertEqual(169000, hb_maps:get(<<"channel-1">>, hb_maps:get(<<"by-claim-id">>, Msg, #{}), #{})).

sub_count_accepts_supplied_counts_test() ->
    {ok, Msg} = sub_count(
        #{},
        #{ <<"claim-ids">> => <<"channel-1,channel-2">>, <<"counts">> => [7, 8] },
        #{}
    ),
    ?assertEqual([7, 8], hb_maps:get(<<"sub-counts">>, Msg, #{})).

sub_count_rejects_mismatched_claim_ids_test() ->
    ?assertEqual(
        {error, sub_count_claim_id_mismatch},
        sub_count(#{}, #{ <<"claim_id">> => <<"channel-1">>, <<"counts">> => [7, 8] }, #{})
    ).

count_params_normalizes_aliases_and_strips_control_fields_test() ->
    {ok, Params} = count_params(
        #{ <<"odysee-api-url">> => <<"http://api">>, <<"claim-ids">> => <<"channel-1">> },
        #{ <<"body">> => <<"{}">> },
        #{}
    ),
    ?assertEqual(#{ <<"claim_id">> => <<"channel-1">> }, Params).

count_params_rejects_private_credentials_test() ->
    ?assertEqual(
        {error, private_credentials_not_allowed},
        count_params(#{ <<"claim-id">> => <<"channel-1">> }, #{ <<"auth_token">> => <<"tok">> }, #{})
    ).

form_body_encodes_params_test() ->
    ?assertEqual(
        <<"claim_id=channel-1">>,
        form_body(#{ <<"claim_id">> => <<"channel-1">> })
    ).

sub_count_requires_claim_id_for_fetch_test() ->
    ?assertEqual({error, claim_id_not_found}, sub_count(#{}, #{}, #{})).

sub_count_url_uses_configurable_base_test() ->
    ?assertEqual(
        <<"http://api/subscription/sub_count">>,
        sub_count_url(#{ <<"odysee-api-url">> => <<"http://api/">> }, #{}, #{})
    ).

-endif.
