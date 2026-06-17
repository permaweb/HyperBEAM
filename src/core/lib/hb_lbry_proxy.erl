-module(hb_lbry_proxy).
-export([call/3, claim/2, claim_search/2, resolve/2, transaction_show/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_PROXY_NODE, <<"https://api.na-backend.odysee.com">>).
-define(PROXY_PATH, <<"/api/v1/proxy">>).

claim(ClaimIDOrName, Opts) ->
    case valid_claim_id(ClaimIDOrName) of
        true -> claim_search(ClaimIDOrName, Opts);
        false -> resolve(ClaimIDOrName, Opts)
    end.

claim_search(ClaimID, Opts) ->
    Params = #{
        <<"claim_ids">> => [hb_util:to_lower(ClaimID)],
        <<"page">> => 1,
        <<"page_size">> => 1,
        <<"no_totals">> => true
    },
    case call(<<"claim_search">>, Params, Opts) of
        {ok, #{ <<"items">> := [Claim | _] }} -> {ok, Claim};
        {ok, #{ <<"items">> := [] }} -> {error, not_found};
        Other -> Other
    end.

resolve(NameOrURL, Opts) ->
    URL = ensure_lbry_url(NameOrURL),
    Params = #{ <<"urls">> => [URL] },
    case call(<<"resolve">>, Params, Opts) of
        {ok, Result} ->
            case maps:get(URL, Result, undefined) of
                undefined -> {error, not_found};
                Claim -> {ok, Claim}
            end;
        Other ->
            Other
    end.

transaction_show(TxID, Opts) ->
    call(<<"transaction_show">>, #{ <<"txid">> => hb_util:to_lower(TxID) }, Opts).

call(Method, Params, Opts) ->
    Body =
        hb_json:encode(#{
            <<"jsonrpc">> => <<"2.0">>,
            <<"method">> => Method,
            <<"params">> => Params,
            <<"id">> => 1
        }),
    Path = <<?PROXY_PATH/binary, "?m=", Method/binary>>,
    Node = hb_maps:get(<<"lbry-proxy-node">>, Opts, ?DEFAULT_PROXY_NODE, Opts),
    HTTPOpts =
        Opts#{
            <<"http-client">> =>
                hb_maps:get(<<"http-client">>, Opts, httpc, Opts)
        },
    ?event(lbry_proxy, {proxy_request, {method, Method}, {node, Node}}, Opts),
    Result =
        case hb_http_client:request(
            #{
                peer => Node,
                path => Path,
                method => <<"POST">>,
                headers => #{ <<"content-type">> => <<"application/json-rpc">> },
                body => Body
            },
            HTTPOpts
        ) of
            {ok, 200, _Headers, RespBody} ->
                decode_proxy_response(RespBody);
            {ok, Status, _Headers, RespBody} when Status < 500 ->
                {error, {http_status, Status, RespBody}};
            {ok, Status, _Headers, RespBody} ->
                {failure, {http_status, Status, RespBody}};
            {error, Reason} ->
                {failure, Reason}
        end,
    ?event(lbry_proxy,
        {proxy_result, {method, Method}, {result, result_class(Result)}},
        Opts
    ),
    Result.

result_class({ok, _}) -> ok;
result_class({error, _}) -> error;
result_class({failure, _}) -> failure.

decode_proxy_response(RespBody) ->
    try hb_json:decode(RespBody) of
        #{ <<"result">> := Result } -> {ok, Result};
        #{ <<"error">> := Error } -> {error, Error};
        Other -> {error, {invalid_proxy_response, Other}}
    catch
        _:_ ->
            {error, invalid_proxy_json}
    end.

ensure_lbry_url(<<"lbry://", _/binary>> = URL) ->
    URL;
ensure_lbry_url(<<"http://", _/binary>> = URL) ->
    web_url_to_lbry(URL);
ensure_lbry_url(<<"https://", _/binary>> = URL) ->
    web_url_to_lbry(URL);
ensure_lbry_url(Name) when is_binary(Name) ->
    <<"lbry://", Name/binary>>.

web_url_to_lbry(URL) ->
    case uri_string:parse(URL) of
        #{host := Host, path := Path} when is_binary(Host), is_binary(Path) ->
            case odysee_host(Host) of
                true -> odysee_path_to_lbry(Path);
                false -> URL
            end;
        _ ->
            URL
    end.

odysee_host(Host0) ->
    Host = hb_util:to_lower(Host0),
    Host == <<"odysee.com">> orelse
        Host == <<"lbry.tv">> orelse
        has_suffix(Host, <<".odysee.com">>) orelse
        has_suffix(Host, <<".lbry.tv">>).

has_suffix(Bin, Suffix) when byte_size(Bin) > byte_size(Suffix) ->
    binary:part(Bin, byte_size(Bin) - byte_size(Suffix), byte_size(Suffix)) == Suffix;
has_suffix(_, _) ->
    false.

odysee_path_to_lbry(<<"/", Path/binary>>) when byte_size(Path) > 0 ->
    Parts = binary:split(Path, <<"/">>, [global]),
    LBRYParts =
        lists:map(
            fun(Part) ->
                unicode:characters_to_binary(
                    uri_string:percent_decode(
                        claim_separator_to_hash(Part)
                    )
                )
            end,
            Parts
        ),
    <<"lbry://", (join_path(LBRYParts))/binary>>;
odysee_path_to_lbry(_) ->
    <<"lbry://">>.

join_path([]) ->
    <<>>;
join_path([Part]) ->
    Part;
join_path([Part | Rest]) ->
    <<Part/binary, "/", (join_path(Rest))/binary>>.

claim_separator_to_hash(Part) ->
    case binary:matches(Part, <<":">>) of
        [] ->
            Part;
        Matches ->
            {Pos, 1} = lists:last(Matches),
            <<Name:Pos/binary, ":", ClaimID/binary>> = Part,
            <<Name/binary, "#", ClaimID/binary>>
    end.

valid_claim_id(ClaimID) when is_binary(ClaimID), byte_size(ClaimID) == 40 ->
    try binary:decode_hex(ClaimID) of
        Decoded -> byte_size(Decoded) == 20
    catch
        _:_ -> false
    end;
valid_claim_id(_) ->
    false.

claim_search_uses_minimal_proxy_request_test() ->
    ClaimID = <<"64bdbe210b3d9ba616f3a197ea3e0388e360f5e8">>,
    Claim = #{ <<"claim_id">> => ClaimID },
    Response =
        hb_json:encode(#{
            <<"jsonrpc">> => <<"2.0">>,
            <<"result">> => #{ <<"items">> => [Claim] },
            <<"id">> => 1
        }),
    {ok, Server, Handle} = hb_mock_server:start([
        {"/api/v1/proxy", proxy, {200, Response}}
    ]),
    try
        {ok, Claim} =
            claim_search(
                ClaimID,
                #{ <<"lbry-proxy-node">> => Server, <<"http-client">> => httpc }
            ),
        [Req] = hb_mock_server:get_requests(Handle, proxy),
        ?assertEqual(<<"m=claim_search">>, maps:get(<<"qs">>, Req)),
        Sent = hb_json:decode(maps:get(<<"body">>, Req)),
        Params = maps:get(<<"params">>, Sent),
        ?assertEqual([ClaimID], maps:get(<<"claim_ids">>, Params)),
        ?assertEqual(true, maps:get(<<"no_totals">>, Params)),
        ?assertEqual(1, maps:get(<<"page_size">>, Params))
    after
        hb_mock_server:stop(Handle)
    end.

resolve_uses_lbry_url_test() ->
    URL = <<"lbry://sample#abc">>,
    Claim = #{ <<"permanent_url">> => URL },
    Response =
        hb_json:encode(#{
            <<"jsonrpc">> => <<"2.0">>,
            <<"result">> => #{ URL => Claim },
            <<"id">> => 1
        }),
    {ok, Server, Handle} = hb_mock_server:start([
        {"/api/v1/proxy", proxy, {200, Response}}
    ]),
    try
        ?assertEqual(
            {ok, Claim},
            resolve(
                URL,
                #{ <<"lbry-proxy-node">> => Server, <<"http-client">> => httpc }
            )
        )
    after
        hb_mock_server:stop(Handle)
    end.

resolve_converts_odysee_web_url_test() ->
    URL = <<"https://odysee.com/@channel:abc/video:def">>,
    LBRYURL = <<"lbry://@channel#abc/video#def">>,
    Claim = #{ <<"canonical_url">> => LBRYURL },
    Response =
        hb_json:encode(#{
            <<"jsonrpc">> => <<"2.0">>,
            <<"result">> => #{ LBRYURL => Claim },
            <<"id">> => 1
        }),
    {ok, Server, Handle} = hb_mock_server:start([
        {"/api/v1/proxy", proxy, {200, Response}}
    ]),
    try
        ?assertEqual(
            {ok, Claim},
            resolve(
                URL,
                #{ <<"lbry-proxy-node">> => Server, <<"http-client">> => httpc }
            )
        )
    after
        hb_mock_server:stop(Handle)
    end.
