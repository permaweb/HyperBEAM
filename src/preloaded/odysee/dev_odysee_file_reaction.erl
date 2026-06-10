%%% @doc Odysee internal API file reaction compatibility device.
%%%
%%% This device exposes read-only `/reaction/list' responses as AO-Core
%%% messages. Signed `/reaction/react' mutations remain outside this adapter.
-module(dev_odysee_file_reaction).
-implements(<<"odysee-file-reaction@1.0">>).
-export([info/1, list/3, normalize/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEVICE, <<"odysee-file-reaction@1.0">>).
-define(DEFAULT_API_URL, <<"https://api.odysee.com">>).

%% @doc Return the public device API.
info(_Opts) ->
    #{ exports => [<<"list">>, <<"normalize">>] }.

%% @doc Return a normalized `/reaction/list' response.
list(Base, Req, Opts) ->
    safe(fun() ->
        maybe
            {ok, Result, Raw} ?= list_result(Base, Req, Opts),
            normalize_list(Result, Raw, Opts)
        else
            Error -> Error
        end
    end).

%% @doc Normalize supplied reaction data without fetching.
normalize(Base, Req, Opts) ->
    safe(fun() ->
        case result_candidate(Base, Req, Opts) of
            {ok, Result, Raw} -> normalize_list(Result, Raw, Opts);
            not_found -> {error, reaction_result_not_found}
        end
    end).

safe(Fun) ->
    try Fun() of
        Res -> Res
    catch
        _:{error, Reason} -> {error, Reason};
        _:Reason -> {error, Reason}
    end.

list_result(Base, Req, Opts) ->
    case result_candidate(Base, Req, Opts) of
        {ok, _Result, _Raw} = Candidate ->
            Candidate;
        not_found ->
            maybe
                {ok, Params} ?= list_params(Base, Req, Opts),
                api_request(Params, Base, Req, Opts)
            end
    end.

result_candidate(Base, Req, Opts) ->
    Candidates = [
        {Req, <<"file-reaction-result">>},
        {Req, <<"file_reaction_result">>},
        {Req, <<"reaction-result">>},
        {Req, <<"reaction_result">>},
        {Req, <<"data">>},
        {Req, <<"result">>},
        {Req, <<"body">>},
        {Base, <<"file-reaction-result">>},
        {Base, <<"file_reaction_result">>},
        {Base, <<"reaction-result">>},
        {Base, <<"reaction_result">>},
        {Base, <<"data">>},
        {Base, <<"result">>},
        {Base, <<"body">>}
    ],
    case result_from_value(Base, Opts) of
        {ok, _Result, _Raw} = Result -> Result;
        not_found -> result_from_fields(Candidates, Opts)
    end.

result_from_fields([], _Opts) ->
    not_found;
result_from_fields([{Msg, Key} | Rest], Opts) when is_map(Msg) ->
    case hb_maps:get(Key, Msg, not_found, Opts) of
        not_found -> result_from_fields(Rest, Opts);
        Value ->
            case result_from_value(Value, Opts) of
                {ok, _Result, _Raw} = Result -> Result;
                not_found -> result_from_fields(Rest, Opts)
            end
    end;
result_from_fields([_ | Rest], Opts) ->
    result_from_fields(Rest, Opts).

result_from_value(Value, Opts) when is_map(Value) ->
    case result_from_map(Value, hb_json:encode(Value), Opts) of
        {ok, _Result, _Raw} = Result -> Result;
        _ -> not_found
    end;
result_from_value(Value, Opts) when is_binary(Value) ->
    case try_decode_json(Value) of
        {ok, Decoded} -> result_from_decoded(Decoded, Value, Opts);
        _ -> not_found
    end;
result_from_value(_Value, _Opts) ->
    not_found.

result_from_decoded(Decoded, Raw, Opts) when is_map(Decoded) ->
    case result_from_map(Decoded, Raw, Opts) of
        {ok, _Result, _Raw} = Result -> Result;
        _ -> not_found
    end;
result_from_decoded(_Decoded, _Raw, _Opts) ->
    not_found.

result_from_map(Msg, Raw, Opts) when is_map(Msg) ->
    case hb_maps:get(<<"error">>, Msg, not_found, Opts) of
        not_found -> result_from_success_map(Msg, Raw, Opts);
        Error -> {error, {reaction_api_error, Error}}
    end;
result_from_map(_Msg, _Raw, _Opts) ->
    {error, invalid_reaction_result}.

result_from_success_map(Msg, Raw, Opts) ->
    case hb_maps:get(<<"success">>, Msg, true, Opts) of
        false ->
            {error, {reaction_api_error, hb_maps:get(<<"data">>, Msg, Msg, Opts)}};
        _ ->
            Result0 = hb_maps:get(<<"data">>, Msg, Msg, Opts),
            Result = hb_maps:get(<<"result">>, Result0, Result0, Opts),
            case is_reaction_result(Result, Opts) of
                true -> {ok, Result, Raw};
                false -> {error, invalid_reaction_result}
            end
    end.

normalize_list(Result, Raw, Opts) ->
    MyReactions = map_value([<<"my_reactions">>, <<"my-reactions">>], Result, Opts),
    OthersReactions = map_value([<<"others_reactions">>, <<"others-reactions">>], Result, Opts),
    {ok, #{
        <<"device">> => ?DEVICE,
        <<"content-type">> => <<"application/json">>,
        <<"body">> => Raw,
        <<"result">> => Result,
        <<"my_reactions">> => MyReactions,
        <<"my-reactions">> => MyReactions,
        <<"others_reactions">> => OthersReactions,
        <<"others-reactions">> => OthersReactions,
        <<"claim-ids">> => reaction_claim_ids(MyReactions, OthersReactions)
    }}.

is_reaction_result(Result, Opts) when is_map(Result) ->
    has_any([<<"my_reactions">>, <<"my-reactions">>, <<"others_reactions">>, <<"others-reactions">>], Result, Opts);
is_reaction_result(_Result, _Opts) ->
    false.

list_params(Base, Req, Opts) ->
    case private_credential_present(Base, Req, Opts) of
        true ->
            {error, private_credentials_not_allowed};
        false ->
            Params = api_params(maps:merge(map_or_empty(Base), map_or_empty(Req)), Opts),
            case hb_maps:get(<<"claim_ids">>, Params, not_found, Opts) of
                not_found -> {error, claim_ids_not_found};
                _ClaimIDs -> {ok, Params}
            end
    end.

api_params(Params0, Opts) ->
    Params1 = put_alias(<<"claim_ids">>, <<"claim-ids">>, Params0, Opts),
    maps:without(control_keys() ++ request_metadata_keys() ++ private_credential_keys(), Params1).

control_keys() ->
    [
        <<"auth-token">>,
        <<"body">>,
        <<"claim-ids">>,
        <<"content-type">>,
        <<"data">>,
        <<"device">>,
        <<"file-reaction-result">>,
        <<"file_reaction_result">>,
        <<"method">>,
        <<"odysee-api-url">>,
        <<"odysee_api_url">>,
        <<"path">>,
        <<"reaction-result">>,
        <<"reaction-url">>,
        <<"reaction_result">>,
        <<"reaction_url">>,
        <<"result">>
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
    first_found([{Req, Key} || Key <- private_credential_keys()] ++ [{Base, Key} || Key <- private_credential_keys()], Opts)
        =/= not_found.

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
        <<"path">> => reaction_url(Base, Req, Opts),
        <<"content-type">> => <<"application/x-www-form-urlencoded">>,
        <<"body">> => Body
    },
    case hb_http:request(Msg, Opts) of
        {ok, #{ <<"body">> := RespBody }} when is_binary(RespBody) -> decode_api_body(RespBody, Opts);
        {ok, RespBody} when is_binary(RespBody) -> decode_api_body(RespBody, Opts);
        {ok, Other} -> {error, {reaction_response_without_body, Other}};
        Error -> Error
    end.

decode_api_body(Body, Opts) ->
    maybe
        {ok, Decoded} ?= try_decode_json(Body),
        result_from_map(Decoded, Body, Opts)
    end.

reaction_url(Base, Req, Opts) ->
    case first_found(
        [
            {Req, <<"reaction-url">>},
            {Req, <<"reaction_url">>},
            {Base, <<"reaction-url">>},
            {Base, <<"reaction_url">>}
        ],
        Opts
    ) of
        not_found -> <<(trim_trailing_slash(api_url(Base, Req, Opts)))/binary, "/reaction/list">>;
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

map_value(Keys, Result, Opts) ->
    case first_value(Keys, Result, Opts) of
        Value when is_map(Value) -> Value;
        _ -> #{}
    end.

reaction_claim_ids(MyReactions, OthersReactions) ->
    lists:usort(maps:keys(MyReactions) ++ maps:keys(OthersReactions)).

map_or_empty(Map) when is_map(Map) -> Map;
map_or_empty(_Value) -> #{}.

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

has_any([], _Map, _Opts) ->
    false;
has_any([Key | Rest], Map, Opts) ->
    case hb_maps:get(Key, Map, not_found, Opts) of
        not_found -> has_any(Rest, Map, Opts);
        _ -> true
    end.

try_decode_json(Raw) ->
    try {ok, hb_json:decode(Raw)}
    catch _:_ -> {error, invalid_json}
    end.

-ifdef(TEST).

list_result_normalizes_file_reactions_test() ->
    Result = reaction_result(),
    {ok, Msg} = list(#{}, #{ <<"result">> => Result }, #{}),
    ?assertEqual(<<"odysee-file-reaction@1.0">>, hb_maps:get(<<"device">>, Msg, #{})),
    ?assertEqual([<<"claim-1">>, <<"claim-2">>], hb_maps:get(<<"claim-ids">>, Msg, #{})),
    ?assertEqual(
        #{ <<"claim-1">> => #{ <<"like">> => 1, <<"dislike">> => 0 } },
        hb_maps:get(<<"my_reactions">>, Msg, #{})
    ),
    ?assertEqual(
        #{ <<"like">> => 53, <<"dislike">> => 1 },
        hb_maps:get(<<"claim-1">>, hb_maps:get(<<"others_reactions">>, Msg, #{}), #{})
    ).

list_accepts_internal_api_json_test() ->
    Raw = hb_json:encode(#{
        <<"success">> => true,
        <<"data">> => reaction_result()
    }),
    {ok, Msg} = list(#{}, #{ <<"body">> => Raw }, #{}),
    ?assertEqual(Raw, hb_maps:get(<<"body">>, Msg, #{})),
    ?assertEqual([<<"claim-1">>, <<"claim-2">>], hb_maps:get(<<"claim-ids">>, Msg, #{})).

list_params_normalizes_aliases_and_strips_control_fields_test() ->
    {ok, Params} = list_params(
        #{ <<"odysee-api-url">> => <<"http://api">>, <<"claim-ids">> => <<"claim-1">> },
        #{ <<"body">> => <<"{}">> },
        #{}
    ),
    ?assertEqual(#{ <<"claim_ids">> => <<"claim-1">> }, Params).

list_params_rejects_private_credentials_test() ->
    ?assertEqual(
        {error, private_credentials_not_allowed},
        list_params(#{ <<"claim-ids">> => <<"claim-1">> }, #{ <<"auth_token">> => <<"tok">> }, #{})
    ).

form_body_encodes_params_test() ->
    ?assertEqual(
        <<"claim_ids=claim-1">>,
        form_body(#{ <<"claim_ids">> => <<"claim-1">> })
    ).

list_requires_claim_ids_for_fetch_test() ->
    ?assertEqual({error, claim_ids_not_found}, list(#{}, #{}, #{})).

reaction_url_uses_configurable_base_test() ->
    ?assertEqual(
        <<"http://api/reaction/list">>,
        reaction_url(#{ <<"odysee-api-url">> => <<"http://api/">> }, #{}, #{})
    ).

reaction_result() ->
    #{
        <<"my_reactions">> => #{
            <<"claim-1">> => #{ <<"like">> => 1, <<"dislike">> => 0 }
        },
        <<"others_reactions">> => #{
            <<"claim-1">> => #{ <<"like">> => 53, <<"dislike">> => 1 },
            <<"claim-2">> => #{ <<"like">> => 4, <<"dislike">> => 0 }
        }
    }.

-endif.
