%%% @doc RFC 8555 ACME client for certificates carrying the node wallet's key.
%%% `obtain/5' performs one synchronous issuance against the configured ACME
%%% directory: create an account and order, validate domains, submit the node's
%%% CSR, then download the leaf-first certificate chain. The node wallet signs
%%% the CSR; a separate account wallet signs the ACME requests.
%%%
%%% HTTP-01 and DNS-01 records are published through a caller-supplied callback.
%%% This module does not start listeners, store challenge records or schedule
%%% renewal. Those responsibilities belong to `dev_tls'. Wildcard domains
%%% require DNS-01; validation never falls back to another challenge type.
%%%
%%% ACME requests require HTTPS and use the system CA certificates unless
%%% `acme/ca-certificate' supplies a PEM trust chain in the TLS message. Responses
%%% are size-limited, individual requests have timeouts, and a shared issuance
%%% deadline is checked before requests and polling waits. Protocol and transport
%%% errors are returned to the caller rather than retried indefinitely.
-module(dev_tls_acme).
-export([obtain/5, dns_name/1]).
-include("include/ar.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(ISSUANCE_TIMEOUT, 180000).
-define(POLL_INTERVAL, 1000).
-define(REQUEST_TIMEOUT, 30000).
-define(RESPONSE_LIMIT, 2 * 1024 * 1024).

%% @doc Obtain a certificate chain for `Wallet' using the TLS configuration message.
%% Return `{ok, Chain}' with leaf-first DER certificates, or `{error, Reason}'.
%% The challenge callback must return `ok' for `{put, Key, Value}' and receives
%% `{delete, Key}' when validation finishes or fails. HTTP keys are tokens with
%% key-authorization values; DNS keys are `{Name, Token}' with TXT digest values.
obtain(TLS, Wallet, AccountWallet, Challenge, Opts) ->
    try
        {ACME, DirectoryURL, Domains, ChallengeType} = config(TLS, Opts),
        State0 = #{
            wallet => Wallet,
            account_wallet => AccountWallet,
            nonce => undefined,
            kid => undefined,
            thumbprint => account_thumbprint(AccountWallet),
            challenge_type => ChallengeType,
            http_opts => http_options(ACME, Opts),
            deadline => erlang:monotonic_time(millisecond) + ?ISSUANCE_TIMEOUT
        },
        State1 = State0#{directory => get_json(DirectoryURL, State0)},
        State2 = create_account(State1),
        {Order, OrderURL, State3} = create_order(Domains, State2),
        State4 = authorize(maps:get(<<"authorizations">>, Order),
            Challenge, State3),
        {_Ready, State5} = poll(OrderURL, <<"ready">>, State4),
        State6 = finalize(maps:get(<<"finalize">>, Order), Domains, State5),
        {Valid, State7} = poll(OrderURL, <<"valid">>, State6),
        {_Headers, PEM, _State8} = expect(jws_post(
            maps:get(<<"certificate">>, Valid), post_as_get, State7
        ), [200]),
        hb_tls:certificate_chain(PEM)
    catch
        throw:{acme, Reason} -> {error, Reason};
        _:Reason -> {error, Reason}
    end.

%% @doc Read ACME settings and require terms agreement, HTTPS and a supported
%% challenge type before making requests. Return normalized domain identifiers.
config(TLS, Opts) ->
    ACME = case hb_maps:get(<<"acme">>, TLS, undefined, Opts) of
        Value when is_map(Value) -> Value;
        _ -> throw({acme, 'invalid-acme-config'})
    end,
    require(
        hb_maps:get(<<"terms-of-service-agreed">>, ACME, false, Opts) =:= true,
        'acme-terms-not-agreed'
    ),
    DirectoryURL = hb_maps:get(<<"directory-url">>, ACME, undefined, Opts),
    require(is_binary(DirectoryURL) andalso byte_size(DirectoryURL) > 0,
        'invalid-acme-directory-url'),
    request_parts(DirectoryURL),
    ChallengeType = hb_maps:get(<<"challenge-type">>, ACME, <<"http-01">>, Opts),
    require(lists:member(ChallengeType, [<<"http-01">>, <<"dns-01">>]),
        'invalid-acme-challenge-type'),
    case ChallengeType of
        <<"dns-01">> ->
            require(valid_domain(
                hb_maps:get(<<"dns-nameserver">>, ACME, undefined, Opts),
                <<"http-01">>
            ), 'invalid-acme-dns-nameserver');
        <<"http-01">> -> ok
    end,
    Domains = domains(hb_maps:get(<<"domains">>, TLS, undefined, Opts),
        ChallengeType),
    {ACME, DirectoryURL, Domains, ChallengeType}.

%% @doc Raise a tagged ACME error when a protocol or configuration check fails.
require(true, _Reason) -> ok;
require(false, Reason) -> throw({acme, Reason}).

%% @doc Require nonempty domain identifiers and normalize their case.
%% Wildcards must be compatible with the selected challenge type.
domains(Domains, ChallengeType) when is_list(Domains), Domains =/= [] ->
    require(lists:all(fun(Domain) -> valid_domain(Domain, ChallengeType) end,
        Domains), 'invalid-tls-domains'),
    [hb_util:to_lower(Domain) || Domain <- Domains];
domains(_, _) ->
    throw({acme, 'invalid-tls-domains'}).

%% @doc Wildcards are a single leading label and require DNS validation.
valid_domain(<<"*.", Domain/binary>>, <<"dns-01">>) ->
    valid_domain(Domain, <<"http-01">>);
valid_domain(Domain, _) when is_binary(Domain), byte_size(Domain) > 0 ->
    binary:match(Domain, <<"*">>) =:= nomatch;
valid_domain(_, _) -> false.

%% @doc Hash the canonical public JWK for the RFC 7638 account thumbprint.
%% Its lexicographic field order and absence of whitespace are significant.
account_thumbprint(Wallet) ->
    #{<<"e">> := E, <<"n">> := N} = jwk(Wallet),
    Canonical = <<
        "{\"e\":\"", E/binary,
        "\",\"kty\":\"RSA\",\"n\":\"", N/binary, "\"}"
    >>,
    hb_util:encode(crypto:hash(sha256, Canonical)).

%% @doc Create or retrieve the account, retaining its URL for later JWS requests.
create_account(State) ->
    {Headers, _Body, State1} = expect(jws_post(
        directory_url(<<"newAccount">>, State),
        #{
            <<"termsOfServiceAgreed">> => true
        },
        jwk,
        State
    ), [200, 201]),
    case header(<<"location">>, Headers) of
        not_found -> throw({acme, 'acme-account-location-missing'});
        KID -> State1#{kid => KID}
    end.

%% @doc Create an order for the DNS identifiers and retain its URL.
create_order(Domains, State) ->
    Payload = #{<<"identifiers">> => [
        #{<<"type">> => <<"dns">>, <<"value">> => Domain}
    || Domain <- Domains]},
    {Headers, Body, State1} = expect(jws_post(
        directory_url(<<"newOrder">>, State), Payload, State
    ), [201]),
    case header(<<"location">>, Headers) of
        not_found -> throw({acme, 'acme-order-location-missing'});
        URL -> {json(Body), URL, State1}
    end.

%% @doc Validate each pending authorization through the challenge callback.
%% Published records remain available during validation and are removed on exit,
%% including when polling or a later authorization fails.
authorize([], _Challenge, State) -> State;
authorize([URL | Rest], Challenge, State) ->
    {_Headers, Body, State1} = expect(jws_post(URL, post_as_get, State), [200]),
    Authorization = json(Body),
    case maps:get(<<"status">>, Authorization) of
        <<"valid">> -> authorize(Rest, Challenge, State1);
        _ ->
            Type = maps:get(challenge_type, State1),
            Selected = challenge(Authorization, Type),
            Token = maps:get(<<"token">>, Selected),
            validate_token(Token),
            KeyAuthorization = <<Token/binary, ".",
                (maps:get(thumbprint, State1))/binary>>,
            {Key, Value} = challenge_record(Type, Authorization, Token,
                KeyAuthorization),
            try
                ok = Challenge({put, Key, Value}),
                {_H, _B, State2} = expect(jws_post(
                    maps:get(<<"url">>, Selected), #{}, State1
                ), [200, 202]),
                {_Valid, State3} = poll(URL, <<"valid">>, State2),
                authorize(Rest, Challenge, State3)
            after
                Challenge({delete, Key})
            end
    end.

%% @doc Select the configured validation method without falling back to another.
challenge(Authorization, Type) ->
    case [Challenge || Challenge <- maps:get(<<"challenges">>, Authorization, []),
            maps:get(<<"type">>, Challenge, undefined) =:= Type] of
        [Challenge | _] -> Challenge;
        [] -> throw({acme, {'acme-challenge-not-offered', Type}})
    end.

%% @doc Build the callback key and value for an HTTP or DNS challenge.
%% HTTP-01 serves the key authorization; DNS-01 serves its base64url SHA-256
%% digest. DNS keys retain the token so concurrent authorizations can coexist.
challenge_record(<<"http-01">>, _Authorization, Token, KeyAuthorization) ->
    {Token, KeyAuthorization};
challenge_record(<<"dns-01">>, Authorization, Token, KeyAuthorization) ->
    #{<<"type">> := <<"dns">>, <<"value">> := Domain} =
        maps:get(<<"identifier">>, Authorization),
    {{dns_name(Domain), Token},
        hb_util:encode(crypto:hash(sha256, KeyAuthorization))}.

%% @doc Return the lowercase DNS challenge name, shared by an apex and wildcard.
dns_name(<<"*.", Domain/binary>>) -> dns_name(Domain);
dns_name(Domain) -> <<"_acme-challenge.", (hb_util:to_lower(Domain))/binary>>.

%% @doc Finalize the order with a base64url CSR signed by the node wallet.
finalize(URL, Domains, State) ->
    {_Headers, _Body, State1} = expect(jws_post(
        URL,
        #{<<"csr">> => hb_util:encode(hb_tls:csr(maps:get(wallet, State), Domains))},
        State
    ), [200, 202]),
    State1.

%% @doc Poll an order or authorization until its expected state or an error.
%% Respect numeric Retry-After delays within the shared issuance deadline.
poll(URL, Expected, State) ->
    deadline(State),
    {Headers, Body, State1} = expect(jws_post(URL, post_as_get, State), [200]),
    Object = json(Body),
    case maps:get(<<"status">>, Object, undefined) of
        Expected -> {Object, State1};
        <<"invalid">> -> throw({acme, {'acme-object-invalid', Object}});
        _ ->
            wait(retry_after(Headers, ?POLL_INTERVAL), State1),
            poll(URL, Expected, State1)
    end.

%% @doc Sign a request using the registered account URL as its key identifier.
jws_post(URL, Payload, State) -> jws_post(URL, Payload, kid, State).

%% @doc Allow one bad-nonce retry, authenticating with a JWK or account URL.
jws_post(URL, Payload, Auth, State) ->
    jws_post(URL, Payload, Auth, State, 2).

%% @doc Send an RS256 JWS and retain its response nonce for the next request.
%% POST-as-GET uses an empty payload, distinct from a JSON object. A bad nonce
%% retries with the response nonce or a fresh one, within the attempt budget.
jws_post(_URL, _Payload, _Auth, _State, 0) ->
    {error, 'acme-bad-nonce'};
jws_post(URL, Payload, Auth, State0, Retries) ->
    deadline(State0),
    State = ensure_nonce(State0),
    Protected0 = #{
        <<"alg">> => <<"RS256">>,
        <<"nonce">> => maps:get(nonce, State),
        <<"url">> => URL
    },
    Protected = case Auth of
        jwk -> Protected0#{<<"jwk">> => jwk(maps:get(account_wallet, State))};
        kid -> Protected0#{<<"kid">> => maps:get(kid, State)}
    end,
    Protected64 = hb_util:encode(hb_json:encode(Protected)),
    Payload64 = case Payload of
        post_as_get -> <<>>;
        _ -> hb_util:encode(hb_json:encode(Payload))
    end,
    SigningInput = <<Protected64/binary, ".", Payload64/binary>>,
    Body = hb_json:encode(#{
        <<"protected">> => Protected64,
        <<"payload">> => Payload64,
        <<"signature">> => hb_util:encode(rsa_sign(
            maps:get(account_wallet, State), SigningInput
        ))
    }),
    Headers = #{
        <<"content-type">> => <<"application/jose+json">>,
        <<"user-agent">> => <<"HyperBEAM ACME">>
    },
    case request(URL, <<"POST">>, Headers, Body, State) of
        {ok, Status, ResponseHeaders, ResponseBody} ->
            State1 = State#{nonce => case header(
                <<"replay-nonce">>, ResponseHeaders
            ) of not_found -> undefined; Nonce -> Nonce end},
            case is_bad_nonce(Status, ResponseBody) of
                true -> jws_post(URL, Payload, Auth, State1, Retries - 1);
                false -> {ok, Status, ResponseHeaders, ResponseBody, State1}
            end;
        {error, _} = Error -> Error
    end.

%% @doc Reuse a response nonce or fetch one from the directory's newNonce endpoint.
ensure_nonce(#{nonce := Nonce} = State)
        when is_binary(Nonce), byte_size(Nonce) > 0 -> State;
ensure_nonce(State) ->
    {Headers, _Body} = expect(request(
        directory_url(<<"newNonce">>, State),
        <<"GET">>,
        #{<<"user-agent">> => <<"HyperBEAM ACME">>},
        <<>>,
        State
    ), [200, 204]),
    case header(<<"replay-nonce">>, Headers) of
        not_found -> throw({acme, 'acme-nonce-missing'});
        Nonce -> State#{nonce => Nonce}
    end.

%% @doc Make a size-limited HTTPS request after checking the issuance deadline.
request(URL, Method, Headers, Body, State) ->
    deadline(State),
    {Peer, Path} = request_parts(URL),
    hb_http_client:request(#{
        peer => Peer,
        path => Path,
        method => Method,
        headers => Headers,
        body => Body,
        limit => ?RESPONSE_LIMIT
    }, maps:get(http_opts, State)).

%% @doc Fetch an unsigned JSON resource, such as the ACME directory.
get_json(URL, State) ->
    {_Headers, Body} = expect(request(
        URL,
        <<"GET">>,
        #{<<"user-agent">> => <<"HyperBEAM ACME">>},
        <<>>,
        State
    ), [200]),
    json(Body).

%% @doc Unwrap accepted HTTP responses or raise an ACME error with issuer details.
expect({ok, Status, Headers, Body}, Statuses) ->
    case lists:member(Status, Statuses) of
        true -> {Headers, Body};
        false ->
            Reason = {'unexpected-acme-status', Status, problem(Body)},
            case retry_after(Headers, 0) of
                Delay when Delay > 0 -> throw({acme, {'acme-retry-after',
                    erlang:system_time(millisecond) + Delay, Reason}});
                _ -> throw({acme, Reason})
            end
    end;
expect({ok, Status, Headers, Body, State}, Statuses) ->
    case expect({ok, Status, Headers, Body}, Statuses) of
        {Headers, Body} -> {Headers, Body, State}
    end;
expect({error, Reason}, _Statuses) ->
    throw({acme, Reason}).

%% @doc Split an HTTPS URL into peer and request path, preserving its query.
%% Reject other schemes and embedded user credentials.
request_parts(URL) ->
    URI = uri_string:parse(URL),
    Scheme = hb_util:to_lower(hb_util:bin(maps:get(scheme, URI, <<>>))),
    require(Scheme =:= <<"https">> andalso maps:is_key(host, URI)
        andalso not maps:is_key(userinfo, URI), {'invalid-acme-url', URL}),
    Peer = uri_string:recompose(
        (maps:without([query, fragment], URI))#{path => <<>>}
    ),
    Path = hb_util:bin(maps:get(path, URI, <<"/">>)),
    case maps:find(query, URI) of
        {ok, Query} -> {Peer, <<Path/binary, "?", (hb_util:bin(Query))/binary>>};
        error -> {Peer, Path}
    end.

%% @doc Decode a required JSON response or raise an ACME protocol error.
json(Body) ->
    case decode_json(Body) of
        {ok, Value} -> Value;
        error -> throw({acme, 'invalid-acme-json'})
    end.

%% @doc Return decoded JSON or `error' without throwing on malformed input.
decode_json(Body) ->
    try {ok, hb_json:decode(Body)} catch _:_ -> error end.

%% @doc Read a required endpoint URL from the issuer's directory.
directory_url(Key, State) ->
    case maps:get(Key, maps:get(directory, State), undefined) of
        URL when is_binary(URL) -> URL;
        _ -> throw({acme, {'acme-directory-key-missing', Key}})
    end.

%% @doc Encode only the RSA public modulus and exponent as a JWK.
jwk(#{ <<"type">> := Type = ?RSA_SIGN_TYPE, <<"owner">> := N }) ->
    {rsa, E} = ar_tx:deserialize_sig_type(Type),
    #{
        <<"e">> => hb_util:encode(binary:encode_unsigned(E)),
        <<"kty">> => <<"RSA">>,
        <<"n">> => hb_util:encode(N)
    }.

%% @doc Sign the ACME JWS input using RS256 and the account wallet.
rsa_sign(Wallet = #{ <<"type">> := Type = ?RSA_SIGN_TYPE }, Data) ->
    #{ <<"owner">> := N, <<"priv">> := #{ <<"d">> := D } } = Wallet,
    {rsa, E} = ar_tx:deserialize_sig_type(Type),
    crypto:sign(rsa, sha256, Data,
        [E, binary:decode_unsigned(N), binary:decode_unsigned(D)],
        [{rsa_padding, rsa_pkcs1_padding}]).

%% @doc Read a lowercase response header, returning `not_found' when absent.
header(Name, Headers) ->
    proplists:get_value(Name, Headers, not_found).

%% @doc Recognize an ACME badNonce response eligible for the bounded JWS retry.
is_bad_nonce(400, Body) ->
    case decode_json(Body) of
        {ok, #{<<"type">> := Type}} ->
            binary:match(Type, <<"badNonce">>) =/= nomatch;
        _ -> false
    end;
is_bad_nonce(_, _) -> false.

%% @doc Preserve issuer error details as decoded JSON or the original body.
problem(Body) ->
    case decode_json(Body) of {ok, Problem} -> Problem; error -> Body end.

%% @doc Honor both delta-seconds and HTTP-date forms of Retry-After.
retry_after(Headers, Default) ->
    Value = header(<<"retry-after">>, Headers),
    try max(0, binary_to_integer(Value) * 1000)
    catch _:_ ->
        try
            Time = httpd_util:convert_request_date(binary_to_list(Value)),
            max(0, (calendar:datetime_to_gregorian_seconds(Time)
                - 62167219200) * 1000 - erlang:system_time(millisecond))
        catch _:_ -> Default
        end
    end.

%% @doc Wait only when the requested polling delay fits before the deadline.
wait(Delay, State) ->
    case deadline(State) > Delay of
        true -> timer:sleep(Delay);
        false -> throw({acme, 'acme-timeout'})
    end.

%% @doc Return the remaining issuance time in milliseconds, or raise a timeout.
deadline(State) ->
    Remaining = maps:get(deadline, State)
        - erlang:monotonic_time(millisecond),
    case Remaining > 0 of
        true -> Remaining;
        false -> throw({acme, 'acme-timeout'})
    end.

%% @doc Configure outbound ACME timeouts and CA trust independently of the listener.
%% HTTP/1 here applies only to issuer requests, not inbound TLS connections.
http_options(ACME, Opts) ->
    CA = case hb_maps:get(<<"ca-certificate">>, ACME, not_found, Opts) of
        not_found -> public_key:cacerts_get();
        PEM ->
            {ok, Chain} = hb_tls:certificate_chain(PEM),
            Chain
    end,
    #{
        <<"http-client">> => gun,
        <<"protocol">> => http1,
        <<"http-retry">> => 0,
        <<"http-client-connect-timeout">> => ?REQUEST_TIMEOUT,
        <<"http-client-send-timeout">> => ?REQUEST_TIMEOUT,
        <<"http-client-tls-ca">> => CA
    }.

%% @doc Require a nonempty challenge token containing only base64url characters.
validate_token(Token) when is_binary(Token), byte_size(Token) > 0 ->
    require(re:run(Token, <<"^[A-Za-z0-9_-]+$">>, [{capture, none}]) =:= match,
        'invalid-acme-token');
validate_token(_) -> throw({acme, 'invalid-acme-token'}).

%%% Tests

%% @doc Preserve issuer retry deadlines instead of looping on rejected orders.
retry_after_test() ->
    ?assertEqual(7000, retry_after([{<<"retry-after">>, <<"7">>}], 1)),
    ?assertEqual(1000, retry_after([], 1000)),
    ?assertEqual(1000, retry_after([{<<"retry-after">>, <<"bad">>}], 1000)),
    Future = erlang:system_time(second) + 120,
    Date = httpd_util:rfc1123_date(
        calendar:system_time_to_local_time(Future, second)),
    Delay = retry_after([{<<"retry-after">>, list_to_binary(Date)}], 0),
    ?assert(Delay > 118000 andalso Delay =< 120000),
    ?assertThrow({acme, {'acme-retry-after', _, _}}, expect(
        {ok, 429, [{<<"retry-after">>, <<"7">>}], <<"rate limited">>}, [200])).

%% @doc Reject plaintext issuer URLs and recognize nonce-rejection responses.
protocol_validation_test() ->
    ?assertThrow({acme, {'invalid-acme-url', _}},
        request_parts(<<"http://acme.example/directory">>)),
    ?assert(is_bad_nonce(400, hb_json:encode(#{
        <<"type">> => <<"urn:ietf:params:acme:error:badNonce">>
    }))).

%% @doc DNS-01 hashes the authorization, keeps token identity, and scopes '*.'.
dns_challenge_test() ->
    ?assertEqual(
        {{<<"_acme-challenge.example.test">>, <<"token">>},
            <<"61rBZ_4knHblO0MNoxFsXZ_eTFUHum0B6IVRbhvUn5I">>},
        challenge_record(
            <<"dns-01">>,
            #{
                <<"identifier">> =>
                    #{ <<"type">> => <<"dns">>, <<"value">> => <<"Example.Test">> }
            },
            <<"token">>,
            <<"token.thumbprint">>
        )
    ),
    ?assertEqual(<<"_acme-challenge.example.test">>, dns_name(<<"*.Example.Test">>)),
    ?assertEqual([<<"*.example.test">>], domains([<<"*.Example.Test">>], <<"dns-01">>)),
    ?assertThrow({acme, 'invalid-tls-domains'},
        domains([<<"*.example.test">>], <<"http-01">>)),
    ?assertThrow({acme, 'invalid-tls-domains'},
        domains([<<"*.*.example.test">>], <<"dns-01">>)),
    ?assertThrow({acme, {'acme-challenge-not-offered', <<"dns-01">>}},
        challenge(#{ <<"challenges">> => [#{ <<"type">> => <<"http-01">> }] },
            <<"dns-01">>)).
