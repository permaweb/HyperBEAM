%%% @doc Bounded RFC 8555 client for a node-wallet certificate.
-module(dev_tls_acme).
-export([obtain/5]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

-define(ISSUANCE_TIMEOUT, 180000).
-define(POLL_INTERVAL, 1000).
-define(REQUEST_TIMEOUT, 30000).
-define(RESPONSE_LIMIT, 2 * 1024 * 1024).

obtain(TLS, Wallet, AccountWallet, Challenge, Opts) ->
    try
        {ACME, DirectoryURL, Domains} = config(TLS, Opts),
        State0 = #{
            wallet => Wallet,
            account_wallet => AccountWallet,
            nonce => undefined,
            kid => undefined,
            thumbprint => account_thumbprint(AccountWallet),
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
        {ok, certificate_chain(PEM)}
    catch
        throw:{acme, Reason} -> {error, Reason};
        _:Reason -> {error, Reason}
    end.

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
    Domains = domains(hb_maps:get(<<"domains">>, TLS, undefined, Opts)),
    {ACME, DirectoryURL, Domains}.

require(true, _Reason) -> ok;
require(false, Reason) -> throw({acme, Reason}).

domains(Domains) when is_list(Domains), Domains =/= [] ->
    require(lists:all(fun(Domain) ->
        is_binary(Domain) andalso byte_size(Domain) > 0
            andalso binary:match(Domain, <<"*">>) =:= nomatch
    end, Domains), 'invalid-tls-domains'),
    [hb_util:to_lower(Domain) || Domain <- Domains];
domains(_) ->
    throw({acme, 'invalid-tls-domains'}).

account_thumbprint(Wallet) ->
    #{<<"e">> := E, <<"n">> := N} = jwk(Wallet),
    Canonical = <<
        "{\"e\":\"", E/binary,
        "\",\"kty\":\"RSA\",\"n\":\"", N/binary, "\"}"
    >>,
    hb_util:encode(crypto:hash(sha256, Canonical)).

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

authorize([], _Challenge, State) -> State;
authorize([URL | Rest], Challenge, State) ->
    {_Headers, Body, State1} = expect(jws_post(URL, post_as_get, State), [200]),
    Authorization = json(Body),
    case maps:get(<<"status">>, Authorization) of
        <<"valid">> -> authorize(Rest, Challenge, State1);
        _ ->
            HTTPChallenge = http_challenge(Authorization),
            Token = maps:get(<<"token">>, HTTPChallenge),
            validate_token(Token),
            KeyAuthorization = <<Token/binary, ".",
                (maps:get(thumbprint, State1))/binary>>,
            ok = Challenge({put, Token, KeyAuthorization}),
            try
                {_H, _B, State2} = expect(jws_post(
                    maps:get(<<"url">>, HTTPChallenge), #{}, State1
                ), [200, 202]),
                {_Valid, State3} = poll(URL, <<"valid">>, State2),
                authorize(Rest, Challenge, State3)
            after
                Challenge({delete, Token})
            end
    end.

http_challenge(Authorization) ->
    case [Challenge || Challenge <- maps:get(<<"challenges">>, Authorization, []),
            maps:get(<<"type">>, Challenge, undefined) =:= <<"http-01">>] of
        [Challenge | _] -> Challenge;
        [] -> throw({acme, 'acme-http-01-not-offered'})
    end.

finalize(URL, Domains, State) ->
    {_Headers, _Body, State1} = expect(jws_post(
        URL,
        #{<<"csr">> => hb_util:encode(csr(maps:get(wallet, State), Domains))},
        State
    ), [200, 202]),
    State1.

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

jws_post(URL, Payload, State) -> jws_post(URL, Payload, kid, State).
jws_post(URL, Payload, Auth, State) ->
    jws_post(URL, Payload, Auth, State, 2).

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

get_json(URL, State) ->
    {_Headers, Body} = expect(request(
        URL,
        <<"GET">>,
        #{<<"user-agent">> => <<"HyperBEAM ACME">>},
        <<>>,
        State
    ), [200]),
    json(Body).

expect({ok, Status, Headers, Body}, Statuses) ->
    case lists:member(Status, Statuses) of
        true -> {Headers, Body};
        false -> throw({acme, {'unexpected-acme-status', Status, problem(Body)}})
    end;
expect({ok, Status, Headers, Body, State}, Statuses) ->
    case expect({ok, Status, Headers, Body}, Statuses) of
        {Headers, Body} -> {Headers, Body, State}
    end;
expect({error, Reason}, _Statuses) ->
    throw({acme, Reason}).

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

json(Body) ->
    case decode_json(Body) of
        {ok, Value} -> Value;
        error -> throw({acme, 'invalid-acme-json'})
    end.

decode_json(Body) ->
    try {ok, hb_json:decode(Body)} catch _:_ -> error end.

certificate_chain(PEM) ->
    case [DER || {'Certificate', DER, not_encrypted} <-
            public_key:pem_decode(PEM)] of
        [] -> throw({acme, 'invalid-acme-certificate-chain'});
        Chain -> Chain
    end.

directory_url(Key, State) ->
    case maps:get(Key, maps:get(directory, State), undefined) of
        URL when is_binary(URL) -> URL;
        _ -> throw({acme, {'acme-directory-key-missing', Key}})
    end.

jwk({{{rsa, E}, _D, N}, {{rsa, E}, N}}) ->
    #{
        <<"e">> => hb_util:encode(binary:encode_unsigned(E)),
        <<"kty">> => <<"RSA">>,
        <<"n">> => hb_util:encode(N)
    }.

rsa_sign({{{rsa, E}, D, N}, {{rsa, E}, N}}, Data) ->
    crypto:sign(rsa, sha256, Data,
        [E, binary:decode_unsigned(N), binary:decode_unsigned(D)],
        [{rsa_padding, rsa_pkcs1_padding}]).

header(Name, Headers) ->
    proplists:get_value(Name, Headers, not_found).

is_bad_nonce(400, Body) ->
    case decode_json(Body) of
        {ok, #{<<"type">> := Type}} ->
            binary:match(Type, <<"badNonce">>) =/= nomatch;
        _ -> false
    end;
is_bad_nonce(_, _) -> false.

problem(Body) ->
    case decode_json(Body) of {ok, Problem} -> Problem; error -> Body end.

retry_after(Headers, Default) ->
    try binary_to_integer(header(<<"retry-after">>, Headers)) * 1000
    catch _:_ -> Default
    end.

wait(Delay, State) ->
    case deadline(State) > Delay of
        true -> timer:sleep(Delay);
        false -> throw({acme, 'acme-timeout'})
    end.

deadline(State) ->
    Remaining = maps:get(deadline, State)
        - erlang:monotonic_time(millisecond),
    case Remaining > 0 of
        true -> Remaining;
        false -> throw({acme, 'acme-timeout'})
    end.

http_options(ACME, Opts) ->
    CA = case hb_maps:get(<<"ca-certificate">>, ACME, not_found, Opts) of
        not_found -> public_key:cacerts_get();
        PEM -> certificate_chain(PEM)
    end,
    #{
        <<"http-client">> => gun,
        <<"protocol">> => http1,
        <<"http-retry">> => 0,
        <<"http-client-connect-timeout">> => ?REQUEST_TIMEOUT,
        <<"http-client-send-timeout">> => ?REQUEST_TIMEOUT,
        <<"http-client-tls-ca">> => CA
    }.

validate_token(Token) when is_binary(Token), byte_size(Token) > 0 ->
    require(re:run(Token, <<"^[A-Za-z0-9_-]+$">>, [{capture, none}]) =:= match,
        'invalid-acme-token');
validate_token(_) -> throw({acme, 'invalid-acme-token'}).

%% @doc Create a SAN PKCS#10 request using the node wallet's exact key.
csr(Wallet, Domains) ->
    Names = [binary_to_list(Domain) || Domain <- Domains],
    Extensions = [{asn1_OPENTYPE, public_key:der_encode(
        'Extensions', [#'Extension'{
            extnID = ?'id-ce-subjectAltName',
            critical = false,
            extnValue = public_key:der_encode('GeneralNames',
                [{dNSName, Name} || Name <- Names])
        }]
    )}],
    {Info, EncodedInfo} = csr_info(Wallet, Names, Extensions,
        'CertificationRequestInfo_attributes_SETOF'),
    {ok, Encoded} = 'PKCS-10':encode(
        'CertificationRequest',
        #'CertificationRequest'{
            certificationRequestInfo = Info,
            signatureAlgorithm = #'CertificationRequest_signatureAlgorithm'{
                algorithm = ?'sha256WithRSAEncryption',
                parameters = {asn1_OPENTYPE, <<5, 0>>}
            },
            signature = rsa_sign(Wallet, EncodedInfo)
        }
    ),
    Encoded.

csr_info(Wallet, Names, Extensions, AttributeRecord) ->
    Info = #'CertificationRequestInfo'{
        version = 0,
        subject = distinguished_name(hd(Names)),
        subjectPKInfo = csr_public_key_info(Wallet),
        attributes = [{AttributeRecord,
            {1, 2, 840, 113549, 1, 9, 14}, Extensions}]
    },
    try
        {ok, Encoded} = 'PKCS-10':encode('CertificationRequestInfo', Info),
        {Info, Encoded}
    catch error:_ when AttributeRecord =/=
            'AttributePKCS-10' ->
        csr_info(Wallet, Names, Extensions, 'AttributePKCS-10')
    end.

csr_public_key_info(Wallet) ->
    #'CertificationRequestInfo_subjectPKInfo'{
        algorithm = #'CertificationRequestInfo_subjectPKInfo_algorithm'{
            algorithm = ?'rsaEncryption',
            parameters = {asn1_OPENTYPE, <<5, 0>>}
        },
        subjectPublicKey = public_key:der_encode(
            'RSAPublicKey', wallet_public_key(Wallet)
        )
    }.

wallet_public_key({{{rsa, E}, _D, N}, {{rsa, E}, N}}) ->
    #'RSAPublicKey'{
        publicExponent = E,
        modulus = binary:decode_unsigned(N)
    }.

distinguished_name(Name) ->
    {rdnSequence, [[#'AttributeTypeAndValue'{
        type = ?'id-at-commonName',
        value = {utf8String, Name}
    }]]}.

%%% Tests

csr_key_test() ->
    Wallet = ar_wallet:load_keyfile("test/key-1.json"),
    Encoded = csr(Wallet, [<<"localhost">>, <<"node.example">>]),
    CSR = public_key:der_decode('CertificationRequest', Encoded),
    Info = CSR#'CertificationRequest'.certificationRequestInfo,
    {ok, EncodedInfo} = 'PKCS-10':encode('CertificationRequestInfo', Info),
    ?assert(public_key:verify(
        EncodedInfo,
        sha256,
        CSR#'CertificationRequest'.signature,
        wallet_public_key(Wallet)
    )),
    CSRKey = Info#'CertificationRequestInfo'.subjectPKInfo,
    ?assertEqual(wallet_public_key(Wallet), public_key:der_decode(
        'RSAPublicKey',
        CSRKey#'CertificationRequestInfo_subjectPKInfo'.subjectPublicKey
    )).

protocol_validation_test() ->
    ?assertThrow({acme, {'invalid-acme-url', _}},
        request_parts(<<"http://acme.example/directory">>)),
    ?assert(is_bad_nonce(400, hb_json:encode(#{
        <<"type">> => <<"urn:ietf:params:acme:error:badNonce">>
    }))).
