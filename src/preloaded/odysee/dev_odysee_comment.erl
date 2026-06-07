%%% @doc Odysee Commentron compatibility device.
%%%
%%% This device exposes read-only Commentron rows as AO-Core messages. It keeps
%%% raw API responses beside normalized fields and preserves signature inputs
%%% for later verification against LBRY channel public keys.
-module(dev_odysee_comment).
-implements(<<"odysee-comment@1.0">>).
-export([info/1, list/3, by_id/3, normalize/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEVICE, <<"odysee-comment@1.0">>).
-define(DEFAULT_COMMENT_URL, <<"https://comments.odysee.com/api/v2">>).

%% @doc Return the public device API.
info(_Opts) ->
    #{ exports => [<<"list">>, <<"by-id">>, <<"normalize">>] }.

%% @doc Return a normalized `comment.List' response.
list(Base, Req, Opts) ->
    safe(fun() ->
        maybe
            {ok, Result, Raw} ?= list_result(Base, Req, Opts),
            normalize_list(Result, Raw, Opts)
        else
            Error -> Error
        end
    end).

%% @doc Return a normalized `comment.ByID' response.
by_id(Base, Req, Opts) ->
    safe(fun() ->
        maybe
            {ok, Result, Raw} ?= by_id_result(Base, Req, Opts),
            normalize_by_id(Result, Raw, Opts)
        else
            Error -> Error
        end
    end).

%% @doc Normalize supplied comment data without fetching.
normalize(Base, Req, Opts) ->
    safe(fun() ->
        case result_candidate(Base, Req, Opts) of
            {ok, Result, Raw} ->
                case result_kind(Result, Opts) of
                    list -> normalize_list(Result, Raw, Opts);
                    by_id -> normalize_by_id(Result, Raw, Opts);
                    comment -> normalize_single_comment(Result, Raw, Opts)
                end;
            not_found ->
                {error, comment_not_found}
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
                api_request(<<"comment.List">>, Params, Base, Req, Opts)
            end
    end.

by_id_result(Base, Req, Opts) ->
    case result_candidate(Base, Req, Opts) of
        {ok, _Result, _Raw} = Candidate ->
            Candidate;
        not_found ->
            maybe
                {ok, CommentID} ?= comment_id(Base, Req, Opts),
                Params0 = #{ <<"comment_id">> => CommentID },
                Params =
                    put_optional(
                        {<<"with_ancestors">>, first_found(
                            [
                                {Req, <<"with-ancestors">>},
                                {Req, <<"with_ancestors">>},
                                {Base, <<"with-ancestors">>},
                                {Base, <<"with_ancestors">>}
                            ],
                            Opts
                        )},
                        Params0
                    ),
                api_request(<<"comment.ByID">>, Params, Base, Req, Opts)
            end
    end.

result_candidate(Base, Req, Opts) ->
    Candidates = [
        {Req, <<"result">>},
        {Req, <<"comment-result">>},
        {Req, <<"comment_result">>},
        {Req, <<"comments">>},
        {Req, <<"items">>},
        {Req, <<"item">>},
        {Req, <<"comment">>},
        {Req, <<"body">>},
        {Base, <<"result">>},
        {Base, <<"comment-result">>},
        {Base, <<"comment_result">>},
        {Base, <<"comments">>},
        {Base, <<"items">>},
        {Base, <<"item">>},
        {Base, <<"comment">>},
        {Base, <<"body">>}
    ],
    candidate_from_fields(Candidates, Opts).

candidate_from_fields([], _Opts) ->
    not_found;
candidate_from_fields([{Msg, Key} | Rest], Opts) when is_map(Msg) ->
    case hb_maps:get(Key, Msg, not_found, Opts) of
        not_found -> candidate_from_fields(Rest, Opts);
        Value ->
            case candidate_from_value(Value, Opts) of
                {ok, _Result, _Raw} = Candidate -> Candidate;
                not_found -> candidate_from_fields(Rest, Opts)
            end
    end;
candidate_from_fields([_ | Rest], Opts) ->
    candidate_from_fields(Rest, Opts).

candidate_from_value(Value, Opts) when is_binary(Value) ->
    case try_decode_json(Value) of
        {ok, Decoded} -> decoded_candidate(Decoded, Value, Opts);
        _ -> not_found
    end;
candidate_from_value(Value, Opts) ->
    decoded_candidate(Value, hb_json:encode(Value), Opts).

decoded_candidate(Msg, Raw, Opts) when is_map(Msg) ->
    case hb_maps:get(<<"result">>, Msg, not_found, Opts) of
        not_found ->
            case recognizable_result(Msg, Opts) of
                true -> {ok, Msg, Raw};
                false -> not_found
            end;
        Result ->
            {ok, Result, Raw}
    end;
decoded_candidate(Items, Raw, _Opts) when is_list(Items) ->
    {ok, #{ <<"items">> => Items }, Raw};
decoded_candidate(_Msg, _Raw, _Opts) ->
    not_found.

recognizable_result(Msg, Opts) ->
    has_any([<<"items">>, <<"item">>, <<"comments">>, <<"comment">>], Msg, Opts)
        orelse first_value([<<"comment_id">>, <<"comment-id">>, <<"id">>], Msg, Opts) =/= not_found.

result_kind(Result, Opts) when is_map(Result) ->
    case has_any([<<"items">>, <<"comments">>], Result, Opts) of
        true -> list;
        false ->
            case first_value([<<"comment_id">>, <<"comment-id">>, <<"id">>], Result, Opts) of
                not_found ->
                    case has_any([<<"item">>, <<"comment">>, <<"ancestors">>], Result, Opts) of
                        true -> by_id;
                        false -> comment
                    end;
                _CommentID ->
                    comment
            end
    end;
result_kind(Result, _Opts) when is_list(Result) ->
    list;
result_kind(_Result, _Opts) ->
    comment.

normalize_list(Result, Raw, Opts) when is_list(Result) ->
    normalize_list(#{ <<"items">> => Result }, Raw, Opts);
normalize_list(Result, Raw, Opts) ->
    maybe
        {ok, Comments} ?= normalize_comments(list_items(Result, Opts), Opts),
        Msg0 = #{
            <<"device">> => ?DEVICE,
            <<"content-type">> => <<"application/json">>,
            <<"body">> => Raw,
            <<"comments">> => Comments,
            <<"comment-ids">> => [hb_maps:get(<<"comment-id">>, Comment, Opts) || Comment <- Comments]
        },
        Optional = [
            {<<"total-items">>, first_value([<<"total_items">>, <<"total-items">>], Result, Opts)},
            {<<"total-filtered-items">>,
                first_value([<<"total_filtered_items">>, <<"total-filtered-items">>], Result, Opts)},
            {<<"total-pages">>, first_value([<<"total_pages">>, <<"total-pages">>], Result, Opts)},
            {<<"page">>, first_value([<<"page">>], Result, Opts)},
            {<<"page-size">>, first_value([<<"page_size">>, <<"page-size">>], Result, Opts)}
        ],
        {ok, lists:foldl(fun put_optional/2, Msg0, Optional)}
    end.

normalize_by_id(Result, Raw, Opts) when is_map(Result) ->
    case first_value([<<"comment_id">>, <<"comment-id">>, <<"id">>], Result, Opts) of
        not_found ->
            maybe
                {ok, Comment} ?= normalize_comment(by_id_item(Result, Opts), Opts),
                {ok, Ancestors} ?= normalize_comments(
                    first_value([<<"ancestors">>], Result, Opts),
                    Opts
                ),
                {ok,
                    #{
                        <<"device">> => ?DEVICE,
                        <<"content-type">> => <<"application/json">>,
                        <<"body">> => Raw,
                        <<"comment">> => Comment,
                        <<"comment-id">> => hb_maps:get(<<"comment-id">>, Comment, Opts),
                        <<"ancestors">> => Ancestors
                    }
                }
            end;
        _CommentID ->
            normalize_single_comment(Result, Raw, Opts)
    end;
normalize_by_id(Comment, Raw, Opts) ->
    normalize_single_comment(Comment, Raw, Opts).

normalize_single_comment(Comment, Raw, Opts) ->
    maybe
        {ok, Norm} ?= normalize_comment(Comment, Opts),
        {ok,
            #{
                <<"device">> => ?DEVICE,
                <<"content-type">> => <<"application/json">>,
                <<"body">> => Raw,
                <<"comment">> => Norm,
                <<"comment-id">> => hb_maps:get(<<"comment-id">>, Norm, Opts)
            }
        }
    end.

list_items(Result, Opts) ->
    case first_value([<<"items">>, <<"comments">>], Result, Opts) of
        not_found -> [];
        Items -> Items
    end.

by_id_item(Result, Opts) ->
    case first_value([<<"item">>, <<"comment">>, <<"items">>], Result, Opts) of
        [Item | _] -> Item;
        Item -> Item
    end.

normalize_comments(not_found, _Opts) ->
    {ok, []};
normalize_comments(Comments, Opts) when is_list(Comments) ->
    normalize_comments(Comments, Opts, []);
normalize_comments(Comment, Opts) when is_map(Comment) ->
    maybe
        {ok, Norm} ?= normalize_comment(Comment, Opts),
        {ok, [Norm]}
    end;
normalize_comments(_Comments, _Opts) ->
    {error, invalid_comments}.

normalize_comments([], _Opts, Acc) ->
    {ok, lists:reverse(Acc)};
normalize_comments([Comment | Rest], Opts, Acc) ->
    maybe
        {ok, Norm} ?= normalize_comment(Comment, Opts),
        normalize_comments(Rest, Opts, [Norm | Acc])
    end.

normalize_comment(Comment, Opts) when is_map(Comment) ->
    maybe
        {ok, CommentID} ?= required_first([<<"comment_id">>, <<"comment-id">>, <<"id">>], Comment, Opts),
        Text = first_value([<<"comment">>, <<"body">>, <<"text">>], Comment, Opts),
        Msg0 = #{
            <<"device">> => ?DEVICE,
            <<"source">> => Comment,
            <<"comment-id">> => CommentID
        },
        Optional = [
            {<<"comment">>, Text},
            {<<"claim-id">>, first_value([<<"claim_id">>, <<"claim-id">>], Comment, Opts)},
            {<<"parent-id">>, first_value([<<"parent_id">>, <<"parent-id">>], Comment, Opts)},
            {<<"channel-id">>, first_value([<<"channel_id">>, <<"channel-id">>], Comment, Opts)},
            {<<"channel-name">>, first_value([<<"channel_name">>, <<"channel-name">>], Comment, Opts)},
            {<<"channel-url">>, first_value([<<"channel_url">>, <<"channel-url">>], Comment, Opts)},
            {<<"timestamp">>, first_value([<<"timestamp">>, <<"created_at">>, <<"created-at">>], Comment, Opts)},
            {<<"updated-at">>, first_value([<<"updated_at">>, <<"updated-at">>], Comment, Opts)},
            {<<"signature">>, first_value([<<"signature">>], Comment, Opts)},
            {<<"signing-ts">>, first_value([<<"signing_ts">>, <<"signing-ts">>], Comment, Opts)},
            {<<"is-pinned">>, first_value([<<"is_pinned">>, <<"is-pinned">>], Comment, Opts)},
            {<<"replies">>, first_value([<<"replies">>], Comment, Opts)},
            {<<"support-amount">>, first_value([<<"support_amount">>, <<"support-amount">>], Comment, Opts)},
            {<<"support-tx-id">>, first_value([<<"support_tx_id">>, <<"support-tx-id">>], Comment, Opts)},
            {<<"sticker">>, first_value([<<"sticker">>], Comment, Opts)},
            {<<"mentioned-channels">>,
                first_value([<<"mentioned_channels">>, <<"mentioned-channels">>], Comment, Opts)},
            {<<"removed">>, first_value([<<"removed">>, <<"abandoned">>], Comment, Opts)},
            {<<"hidden">>, first_value([<<"hidden">>, <<"is_hidden">>, <<"is-hidden">>], Comment, Opts)},
            {<<"blocked">>, first_value([<<"blocked">>, <<"is_blocked">>, <<"is-blocked">>], Comment, Opts)},
            {<<"moderation">>, moderation_fields(Comment, Opts)}
        ],
        with_signature_context(lists:foldl(fun put_optional/2, Msg0, Optional), Text, Opts)
    end;
normalize_comment(_Comment, _Opts) ->
    {error, invalid_comment}.

with_signature_context(Msg, not_found, _Opts) ->
    {ok, Msg};
with_signature_context(Msg, Text, Opts) ->
    case hb_maps:get(<<"signature">>, Msg, not_found, Opts) of
        not_found ->
            {ok, Msg};
        _Signature ->
            {ok, Msg#{
                <<"signed-field">> => <<"comment">>,
                <<"signed-message">> => Text,
                <<"signature-verification">> => <<"not-verified">>
            }}
    end.

moderation_fields(Comment, Opts) ->
    Fields = [
        {<<"mod-channel-id">>, first_value([<<"mod_channel_id">>, <<"mod-channel-id">>], Comment, Opts)},
        {<<"mod-channel-name">>, first_value([<<"mod_channel_name">>, <<"mod-channel-name">>], Comment, Opts)},
        {<<"creator-channel-id">>,
            first_value([<<"creator_channel_id">>, <<"creator-channel-id">>], Comment, Opts)},
        {<<"creator-channel-name">>,
            first_value([<<"creator_channel_name">>, <<"creator-channel-name">>], Comment, Opts)},
        {<<"blocked-channel-id">>,
            first_value([<<"blocked_channel_id">>, <<"blocked-channel-id">>], Comment, Opts)},
        {<<"blocked-by-channel-id">>,
            first_value([<<"blocked_by_channel_id">>, <<"blocked-by-channel-id">>], Comment, Opts)}
    ],
    case lists:foldl(fun put_optional/2, #{}, Fields) of
        Empty when map_size(Empty) =:= 0 -> not_found;
        Moderation -> Moderation
    end.

list_params(Base, Req, Opts) ->
    Params0 =
        params_from(
            [
                {<<"page">>, [<<"page">>]},
                {<<"page_size">>, [<<"page-size">>, <<"page_size">>]},
                {<<"claim_id">>, [<<"claim-id">>, <<"claim_id">>]},
                {<<"author_claim_id">>, [<<"author-claim-id">>, <<"author_claim_id">>]},
                {<<"parent_id">>, [<<"parent-id">>, <<"parent_id">>]},
                {<<"top_level">>, [<<"top-level">>, <<"top_level">>]},
                {<<"channel_id">>, [<<"channel-id">>, <<"channel_id">>]},
                {<<"channel_name">>, [<<"channel-name">>, <<"channel_name">>]},
                {<<"sort_by">>, [<<"sort-by">>, <<"sort_by">>]},
                {<<"is_protected">>, [<<"is-protected">>, <<"is_protected">>]},
                {<<"requestor_channel_id">>,
                    [<<"requestor-channel-id">>, <<"requestor_channel_id">>, <<"requester-channel-id">>]},
                {<<"requestor_channel_name">>,
                    [<<"requestor-channel-name">>, <<"requestor_channel_name">>, <<"requester-channel-name">>]},
                {<<"signature">>, [<<"signature">>]},
                {<<"signing_ts">>, [<<"signing-ts">>, <<"signing_ts">>]},
                {<<"environment">>, [<<"environment">>]}
            ],
            Base,
            Req,
            Opts
        ),
    case maps:is_key(<<"claim_id">>, Params0) orelse maps:is_key(<<"author_claim_id">>, Params0) of
        true ->
            {ok, Params0};
        false ->
            maybe
                {ok, ClaimID} ?= claim_id(Base, Req, Opts),
                {ok, Params0#{ <<"claim_id">> => ClaimID }}
            end
    end.

params_from(Mappings, Base, Req, Opts) ->
    lists:foldl(
        fun({OutKey, Keys}, Params) ->
            case first_param(Keys, Base, Req, Opts) of
                not_found -> Params;
                Value -> Params#{ OutKey => Value }
            end
        end,
        #{},
        Mappings
    ).

first_param([], _Base, _Req, _Opts) ->
    not_found;
first_param([Key | Rest], Base, Req, Opts) ->
    case first_found([{Req, Key}, {Base, Key}], Opts) of
        not_found -> first_param(Rest, Base, Req, Opts);
        Value -> Value
    end.

claim_id(Base, Req, Opts) ->
    case first_param([<<"claim-id">>, <<"claim_id">>], Base, Req, Opts) of
        not_found -> claim_id_from_claim_or_uri(Base, Req, Opts);
        ClaimID -> {ok, ClaimID}
    end.

claim_id_from_claim_or_uri(Base, Req, Opts) ->
    case first_claim(Base, Req, Opts) of
        Claim when is_map(Claim) ->
            required_first([<<"claim_id">>, <<"claim-id">>], Claim, Opts);
        not_found ->
            case first_param([<<"uri">>, <<"url">>], Base, Req, Opts) of
                not_found -> {error, claim_id_not_found};
                _URI ->
                    maybe
                        {ok, ClaimMsg} ?= hb_ao:raw(<<"lbry-claim@1.0">>, <<"resolve">>, Base, Req, Opts),
                        required_first([<<"claim-id">>, <<"claim_id">>], ClaimMsg, Opts)
                    end
            end
    end.

first_claim(Base, Req, Opts) ->
    case first_found([{Req, <<"claim">>}, {Base, <<"claim">>}], Opts) of
        not_found -> not_found;
        ClaimMsg when is_map(ClaimMsg) -> hb_maps:get(<<"claim">>, ClaimMsg, ClaimMsg, Opts);
        _ -> not_found
    end.

comment_id(Base, Req, Opts) ->
    case first_param([<<"comment-id">>, <<"comment_id">>, <<"id">>], Base, Req, Opts) of
        not_found -> {error, comment_id_not_found};
        CommentID -> {ok, CommentID}
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
        {ok, Other} -> {error, {comment_response_without_body, Other}};
        Error -> Error
    end.

decode_api_body(Body, Opts) ->
    maybe
        {ok, Decoded} ?= try_decode_json(Body),
        case hb_maps:get(<<"error">>, Decoded, not_found, Opts) of
            not_found -> {ok, hb_maps:get(<<"result">>, Decoded, Decoded, Opts), Body};
            Error -> {error, {comment_api_error, Error}}
        end
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

required_first(Keys, Map, Opts) ->
    case first_value(Keys, Map, Opts) of
        not_found -> {error, {missing, hd(Keys)}};
        Value -> {ok, Value}
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

has_any([], _Map, _Opts) ->
    false;
has_any([Key | Rest], Map, Opts) ->
    case hb_maps:get(Key, Map, not_found, Opts) of
        not_found -> has_any(Rest, Map, Opts);
        _ -> true
    end.

put_optional({_Key, not_found}, Msg) -> Msg;
put_optional({Key, Value}, Msg) -> Msg#{ Key => Value }.

try_decode_json(Raw) ->
    try {ok, hb_json:decode(Raw)}
    catch _:_ -> {error, invalid_json}
    end.

-ifdef(TEST).

list_result_normalizes_comments_test() ->
    Result = #{
        <<"items">> => [comment(), reply_comment()],
        <<"total_items">> => 2,
        <<"total_filtered_items">> => 2,
        <<"total_pages">> => 1
    },
    {ok, Msg} = list(#{}, #{ <<"result">> => Result }, #{}),
    Comments = hb_maps:get(<<"comments">>, Msg, #{}),
    ?assertEqual([<<"c1">>, <<"c2">>], hb_maps:get(<<"comment-ids">>, Msg, #{})),
    ?assertEqual(2, hb_maps:get(<<"total-items">>, Msg, #{})),
    ?assertEqual(<<"Science.">>, hb_maps:get(<<"comment">>, hd(Comments), #{})),
    ?assertEqual(<<"not-verified">>, hb_maps:get(<<"signature-verification">>, hd(Comments), #{})).

list_result_accepts_raw_json_test() ->
    Raw = hb_json:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"result">> => #{ <<"items">> => [comment()], <<"total_items">> => 1 },
        <<"id">> => 1
    }),
    {ok, Msg} = list(#{}, #{ <<"body">> => Raw }, #{}),
    ?assertEqual(Raw, hb_maps:get(<<"body">>, Msg, #{})),
    ?assertEqual([<<"c1">>], hb_maps:get(<<"comment-ids">>, Msg, #{})).

by_id_normalizes_item_and_ancestors_test() ->
    Result = #{ <<"item">> => reply_comment(), <<"ancestors">> => [comment()] },
    {ok, Msg} = by_id(#{}, #{ <<"result">> => Result }, #{}),
    ?assertEqual(<<"c2">>, hb_maps:get(<<"comment-id">>, Msg, #{})),
    ?assertEqual(<<"c1">>, hb_maps:get(<<"parent-id">>, hb_maps:get(<<"comment">>, Msg, #{}), #{})),
    ?assertEqual(1, length(hb_maps:get(<<"ancestors">>, Msg, #{}))).

by_id_accepts_raw_comment_result_test() ->
    {ok, Msg} = by_id(#{}, #{ <<"result">> => comment() }, #{}),
    ?assertEqual(<<"c1">>, hb_maps:get(<<"comment-id">>, Msg, #{})).

normalize_single_comment_test() ->
    {ok, Msg} = normalize(#{}, #{ <<"comment">> => comment() }, #{}),
    Norm = hb_maps:get(<<"comment">>, Msg, #{}),
    ?assertEqual(<<"c1">>, hb_maps:get(<<"comment-id">>, Norm, #{})),
    ?assertEqual(<<"comment">>, hb_maps:get(<<"signed-field">>, Norm, #{})).

list_requires_claim_or_author_for_fetch_test() ->
    ?assertEqual({error, claim_id_not_found}, list(#{}, #{}, #{})).

comment() ->
    #{
        <<"comment_id">> => <<"c1">>,
        <<"claim_id">> => <<"claim-1">>,
        <<"channel_id">> => <<"channel-1">>,
        <<"channel_name">> => <<"@veritasium">>,
        <<"channel_url">> => <<"lbry://@veritasium#f">>,
        <<"comment">> => <<"Science.">>,
        <<"timestamp">> => 1710000000,
        <<"signature">> => <<"signature-bytes">>,
        <<"signing_ts">> => <<"1710000000">>,
        <<"replies">> => 1,
        <<"is_pinned">> => false
    }.

reply_comment() ->
    (comment())#{
        <<"comment_id">> => <<"c2">>,
        <<"parent_id">> => <<"c1">>,
        <<"comment">> => <<"Reply.">>,
        <<"signature">> => <<"reply-signature">>
    }.

-endif.
