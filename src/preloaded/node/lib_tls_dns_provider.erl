%%% @doc Shared utilities for TLS DNS provider devices.
-module(lib_tls_dns_provider).
-export([with_config/4, relative_name/2, request/6, problem/1]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

-define(REQUEST_TIMEOUT, 30000).
-define(RESPONSE_LIMIT, 1024 * 1024).

%% @doc Validate common provider configuration before invoking `Fun'.
with_config(Base, Opts, DefaultAPIURL, Fun) ->
    Zone = hb_util:to_lower(hb_maps:get(<<"dns-zone">>, Base, <<>>, Opts)),
    Token = token(hb_maps:get(<<"priv-dns-api-token">>, Base, undefined, Opts)),
    APIURL = hb_maps:get(<<"dns-api-url">>, Base, DefaultAPIURL, Opts),
    case {valid_name(Zone), Token, valid_api_url(APIURL)} of
        {true, {ok, Value}, true} ->
            try Fun(Zone, Value, APIURL)
            catch throw:{acme, Reason} -> {error, Reason}
            end;
        {false, _, _} -> {error, 'invalid-acme-dns-zone'};
        {_, {error, Reason}, _} -> {error, Reason};
        {_, _, false} -> {error, 'invalid-acme-dns-api-url'}
    end.

%% @doc Validate a DNS provider API token.
token(undefined) -> {error, 'acme-dns-api-token-missing'};
token(Token) when is_binary(Token), byte_size(Token) > 0 -> {ok, Token};
token(_) -> {error, 'invalid-acme-dns-api-token'}.

%% @doc Return a DNS record name relative to `Zone'.
relative_name(Zone, Zone) -> <<"@">>;
relative_name(Record, Zone) ->
    Suffix = <<".", Zone/binary>>,
    Size = byte_size(Record) - byte_size(Suffix),
    case Size > 0 andalso binary:part(Record, Size, byte_size(Suffix))
            =:= Suffix of
        true -> binary:part(Record, 0, Size);
        false -> throw({acme, 'acme-dns-record-outside-zone'})
    end.

%% @doc Return whether a DNS name is non-empty and syntactically valid.
valid_name(Name) when is_binary(Name), byte_size(Name) > 0 ->
    re:run(Name, <<"^[a-z0-9.-]+$">>, [{capture, none}]) =:= match;
valid_name(_) -> false.

%% @doc Return whether a provider API URL is a safe HTTPS base URL.
valid_api_url(URL) when is_binary(URL) ->
    try
        URI = uri_string:parse(URL),
        hb_util:to_lower(hb_util:bin(maps:get(scheme, URI, <<>>))) =:= <<"https">>
            andalso maps:is_key(host, URI)
            andalso not maps:is_key(userinfo, URI)
            andalso not maps:is_key(query, URI)
            andalso not maps:is_key(fragment, URI)
    catch _:_ -> false
    end;
valid_api_url(_) -> false.

%% @doc Send an authenticated request to a DNS provider API.
request(APIURL, Path, Method, Token, Body, Opts) ->
    URL = <<APIURL/binary, Path/binary>>,
    URI = uri_string:parse(URL),
    Peer = uri_string:recompose(
        (maps:without([path, query], URI))#{path => <<>>}
    ),
    RequestPath = case maps:find(query, URI) of
        {ok, Query} -> <<(hb_util:bin(maps:get(path, URI)))/binary,
            "?", (hb_util:bin(Query))/binary>>;
        error -> hb_util:bin(maps:get(path, URI))
    end,
    hb_http_client:request(#{
        peer => Peer,
        path => RequestPath,
        method => Method,
        headers => #{
            <<"authorization">> => <<"Bearer ", Token/binary>>,
            <<"content-type">> => <<"application/json">>,
            <<"user-agent">> => <<"HyperBEAM ACME">>
        },
        body => Body,
        limit => ?RESPONSE_LIMIT
    }, http_options(Opts)).

%% @doc Build bounded HTTP options for DNS provider API requests.
http_options(Opts) ->
    Opts#{
        <<"protocol">> => http1,
        <<"http-retry">> => 0,
        <<"http-client-connect-timeout">> => ?REQUEST_TIMEOUT,
        <<"http-client-send-timeout">> => ?REQUEST_TIMEOUT,
        <<"http-client-tls-ca">> => public_key:cacerts_get()
    }.

%% @doc Decode a provider error body when it contains JSON.
problem(Body) ->
    try hb_json:decode(Body) catch _:_ -> Body end.

%%% Tests

%% @doc Test common provider configuration validation.
provider_validation_test() ->
    ?assertEqual(false, valid_api_url(<<"http://api.example/v2">>)),
    ?assertEqual({error, 'invalid-acme-dns-api-token'}, token(<<>>)),
    ?assertEqual({error, 'acme-dns-api-token-missing'}, token(undefined)).

%% @doc Test conversion of fully qualified records to zone-relative names.
relative_name_test() ->
    ?assertEqual(<<"_acme-challenge.hb">>, relative_name(
        <<"_acme-challenge.hb.ajprincipe.pt">>, <<"ajprincipe.pt">>
    )),
    ?assertThrow({acme, 'acme-dns-record-outside-zone'}, relative_name(
        <<"_acme-challenge.other.example">>, <<"ajprincipe.pt">>
    )).

%% @doc Test preserving a configured HTTP client in provider request options.
configured_http_client_test() ->
    HTTPOpts = http_options(#{ <<"http-client">> => httpc }),
    ?assertEqual(httpc, hb_opts:get(http_client, undefined, HTTPOpts)).
