%%% @doc Odysee Commentron reaction compatibility device.
%%%
%%% This device exposes read-only `reaction.List' responses as AO-Core messages
%%% while preserving the raw API response for frontend compatibility.
-module(dev_odysee_reaction).
-implements(<<"odysee-reaction@1.0">>).
-export([info/1, list/3, normalize/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEVICE, <<"odysee-reaction@1.0">>).
-define(DEFAULT_COMMENT_URL, <<"https://comments.odysee.com/api/v2">>).

%% @doc Return the public device API.
info(_Opts) ->
    #{ exports => [<<"list">>, <<"normalize">>] }.

%% @doc Return a normalized `reaction.List' response.
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
                api_request(<<"reaction.List">>, Params, Base, Req, Opts)
            end
    end.

result_candidate(Base, Req, Opts) ->
    Candidates = [
        {Req, <<"reaction-result">>},
        {Req, <<"reaction_result">>},
        {Req, <<"result">>},
        {Req, <<"body">>},
        {Base, <<"reaction-result">>},
        {Base, <<"reaction_result">>},
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
        not_found ->
            Result = hb_maps:get(<<"result">>, Msg, Msg, Opts),
            case is_reaction_result(Result, Opts) of
                true -> {ok, Result, Raw};
                false -> {error, invalid_reaction_result}
            end;
        Error ->
            {error, {reaction_api_error, Error}}
    end;
result_from_map(_Msg, _Raw, _Opts) ->
    {error, invalid_reaction_result}.

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
        <<"comment-ids">> => reaction_comment_ids(MyReactions, OthersReactions)
    }}.

is_reaction_result(Result, Opts) when is_map(Result) ->
    has_any([<<"my_reactions">>, <<"my-reactions">>, <<"others_reactions">>, <<"others-reactions">>], Result, Opts);
is_reaction_result(_Result, _Opts) ->
    false.

list_params(Base, Req, Opts) ->
    Params = api_params(maps:merge(map_or_empty(Base), map_or_empty(Req)), Opts),
    case hb_maps:get(<<"comment_ids">>, Params, not_found, Opts) of
        not_found -> {error, comment_ids_not_found};
        _CommentIDs -> {ok, Params}
    end.

api_params(Params0, Opts) ->
    Params1 = put_alias(<<"comment_ids">>, <<"comment-ids">>, Params0, Opts),
    Params2 = put_alias(<<"channel_id">>, <<"channel-id">>, Params1, Opts),
    Params3 = put_alias(<<"channel_name">>, <<"channel-name">>, Params2, Opts),
    Params4 = put_alias(<<"signing_ts">>, <<"signing-ts">>, Params3, Opts),
    maps:without(control_keys() ++ request_metadata_keys(), Params4).

control_keys() ->
    [
        <<"body">>,
        <<"comment-ids">>,
        <<"channel-id">>,
        <<"channel-name">>,
        <<"comment-url">>,
        <<"comment_url">>,
        <<"content-type">>,
        <<"device">>,
        <<"method">>,
        <<"path">>,
        <<"reaction-result">>,
        <<"reaction_result">>,
        <<"result">>,
        <<"signing-ts">>
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

api_request(Method, Params, Base, Req, Opts) ->
    Payload = hb_json:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => Method,
        <<"params">> => Params,
        <<"id">> => 1
    }),
    Msg = #{
        <<"method">> => <<"POST">>,
        <<"path">> => comment_url(Method, Base, Req, Opts),
        <<"content-type">> => <<"application/json">>,
        <<"body">> => Payload
    },
    case hb_http:request(Msg, Opts) of
        {ok, #{ <<"body">> := Body }} when is_binary(Body) -> decode_api_body(Body, Opts);
        {ok, Body} when is_binary(Body) -> decode_api_body(Body, Opts);
        {ok, Other} -> {error, {reaction_response_without_body, Other}};
        Error -> Error
    end.

decode_api_body(Body, Opts) ->
    maybe
        {ok, Decoded} ?= try_decode_json(Body),
        result_from_map(Decoded, Body, Opts)
    end.

comment_url(Method, Base, Req, Opts) ->
    URL =
        case first_found(
            [
                {Req, <<"comment-url">>},
                {Req, <<"comment_url">>},
                {Base, <<"comment-url">>},
                {Base, <<"comment_url">>}
            ],
            Opts
        ) of
            not_found -> hb_opts:get(<<"odysee-comment-url">>, ?DEFAULT_COMMENT_URL, Opts);
            Found -> Found
        end,
    Separator =
        case binary:match(URL, <<"?">>) of
            nomatch -> <<"?">>;
            _ -> <<"&">>
        end,
    <<URL/binary, Separator/binary, "m=", Method/binary>>.

map_value(Keys, Result, Opts) ->
    case first_value(Keys, Result, Opts) of
        Value when is_map(Value) -> Value;
        _ -> #{}
    end.

reaction_comment_ids(MyReactions, OthersReactions) ->
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

list_result_normalizes_reactions_test() ->
    Result = reaction_result(),
    {ok, Msg} = list(#{}, #{ <<"result">> => Result }, #{}),
    ?assertEqual(<<"odysee-reaction@1.0">>, hb_maps:get(<<"device">>, Msg, #{})),
    ?assertEqual([<<"c1">>, <<"c2">>], hb_maps:get(<<"comment-ids">>, Msg, #{})),
    ?assertEqual(
        #{ <<"c1">> => [<<"like">>] },
        hb_maps:get(<<"my_reactions">>, Msg, #{})
    ),
    ?assertEqual(
        #{ <<"like">> => 3 },
        hb_maps:get(<<"c1">>, hb_maps:get(<<"others_reactions">>, Msg, #{}), #{})
    ).

list_accepts_raw_json_test() ->
    Raw = hb_json:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"result">> => reaction_result(),
        <<"id">> => 1
    }),
    {ok, Msg} = list(#{}, #{ <<"body">> => Raw }, #{}),
    ?assertEqual(Raw, hb_maps:get(<<"body">>, Msg, #{})),
    ?assertEqual([<<"c1">>, <<"c2">>], hb_maps:get(<<"comment-ids">>, Msg, #{})).

list_params_normalizes_aliases_and_strips_control_fields_test() ->
    {ok, Params} = list_params(
        #{ <<"comment-url">> => <<"http://comments">>, <<"comment-ids">> => <<"c1,c2">> },
        #{ <<"body">> => <<"{}">>, <<"channel-id">> => <<"channel-1">>, <<"signature">> => <<"sig">> },
        #{}
    ),
    ?assertEqual(#{
        <<"comment_ids">> => <<"c1,c2">>,
        <<"channel_id">> => <<"channel-1">>,
        <<"signature">> => <<"sig">>
    }, Params).

list_requires_comment_ids_for_fetch_test() ->
    ?assertEqual({error, comment_ids_not_found}, list(#{}, #{}, #{})).

reaction_result() ->
    #{
        <<"my_reactions">> => #{
            <<"c1">> => [<<"like">>]
        },
        <<"others_reactions">> => #{
            <<"c1">> => #{ <<"like">> => 3 },
            <<"c2">> => #{ <<"dislike">> => 1 }
        }
    }.

-endif.
