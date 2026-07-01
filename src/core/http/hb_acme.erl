%%% @doc ACME v2 client for obtaining certificates via the HTTP-01 challenge.
%%% Ported from Peter's DNS-01 client (commit 50dcdb3b25 `hb_acme_client'),
%%% adapted to HTTP-01 and current conventions. No external JOSE dependency:
%%% account and certificate keys are RSA, generated in-memory and never written
%%% to disk (this runs in a TEE). The module opens no listening socket; the
%%% caller serves the key authorization through the `publish'/`unpublish'
%%% callbacks in Config.
-module(hb_acme).
-export([obtain/2, configure/1, config/0]).
-include_lib("public_key/include/public_key.hrl").

%% A single authorization may take a while to validate; poll with a cap.
-define(POLL_INTERVAL_MS, 2000).
-define(POLL_ATTEMPTS, 30).
%% A server may reject an otherwise-valid nonce (RFC 8555 6.5); retry with a
%% fresh one this many times.
-define(NONCE_RETRIES, 5).
%% Stored ACME config (binary-keyed) for the renewal device to read back.
-define(CONFIG, {?MODULE, config}).

%% @doc Obtain a certificate for the configured domains using HTTP-01.
%% Config keys: domains, email, directory_url, publish, unpublish, and an
%% optional http_opts proplist of ssl options for the ACME HTTPS client.
%% Returns the certificate chain and private key as PEM binaries.
obtain(Config, _Opts) ->
    do_obtain(normalize_config(Config)).

%% @doc Store the node's ACME config so the renewal device can read it back.
configure(AcmeConfig) ->
    persistent_term:put(?CONFIG, AcmeConfig),
    ok.

%% @doc Read the stored ACME config, or `undefined' when none is set.
config() ->
    persistent_term:get(?CONFIG, undefined).

do_obtain(Config) ->
    #{domains := Domains, directory_url := DirUrl} = Config,
    try
        Dir = fetch_directory(Config, DirUrl),
        AcctKey = generate_rsa_key(),
        Kid = new_account(Config, Dir, AcctKey),
        {OrderUrl, FinalizeUrl, AuthzUrls} = new_order(Config, Dir, AcctKey, Kid, Domains),
        ok = run_authorizations(Config, Dir, AcctKey, Kid, AuthzUrls),
        {ok, CsrDer, CertKey} = generate_csr(Domains),
        ok = finalize(Config, Dir, AcctKey, Kid, FinalizeUrl, CsrDer),
        CertUrl = poll_order(Config, Dir, AcctKey, Kid, OrderUrl),
        CertPem = download_certificate(Config, Dir, AcctKey, Kid, CertUrl),
        {ok, #{<<"cert">> => CertPem, <<"key">> => key_to_pem(CertKey)}}
    catch
        throw:Reason -> {error, Reason};
        Class:Reason:Stack -> {error, {Class, Reason, Stack}}
    end.

%% normalize_config/1 accepts binary-keyed config and pulls the fields we use
%% into an atom-keyed map, defaulting http_opts to [].
normalize_config(Config) ->
    #{
        domains => maps:get(<<"domains">>, Config, []),
        email => maps:get(<<"email">>, Config, <<>>),
        directory_url => to_list(maps:get(<<"directory_url">>, Config, <<>>)),
        publish => maps:get(<<"publish">>, Config, undefined),
        unpublish => maps:get(<<"unpublish">>, Config, undefined),
        http_opts => maps:get(<<"http_opts">>, Config, [])
    }.

%%%--------------------------------------------------------------------
%%% ACME flow
%%%--------------------------------------------------------------------

fetch_directory(Config, DirUrl) ->
    case http_get(Config, DirUrl) of
        {ok, _Status, _Headers, Body} -> hb_json:decode(Body);
        {error, Reason} -> throw({directory_fetch_failed, Reason})
    end.

%% newAccount: RSA account key in the JWS header as a jwk (no kid yet). The
%% account URL returned in the Location header becomes the kid for later
%% requests.
new_account(Config, Dir, AcctKey) ->
    Url = maps:get(<<"newAccount">>, Dir),
    Payload = #{
        <<"termsOfServiceAgreed">> => true,
        <<"contact">> => [<<"mailto:", (to_bin(maps:get(email, Config)))/binary>>]
    },
    case jws_request(Config, Dir, Url, Payload, AcctKey, undefined) of
        {ok, _Resp, Headers} ->
            case header(<<"location">>, Headers) of
                undefined -> throw(account_location_missing);
                Kid -> Kid
            end;
        {error, Reason} -> throw({account_creation_failed, Reason})
    end.

%% newOrder for the domains; returns the order URL (Location), the finalize URL
%% and the per-domain authorization URLs.
new_order(Config, Dir, AcctKey, Kid, Domains) ->
    Url = maps:get(<<"newOrder">>, Dir),
    Identifiers =
        [#{<<"type">> => <<"dns">>, <<"value">> => to_bin(D)} || D <- Domains],
    Payload = #{<<"identifiers">> => Identifiers},
    case jws_request(Config, Dir, Url, Payload, AcctKey, Kid) of
        {ok, Resp, Headers} ->
            OrderUrl = header(<<"location">>, Headers),
            Finalize = maps:get(<<"finalize">>, Resp),
            Authzs = maps:get(<<"authorizations">>, Resp),
            {OrderUrl, Finalize, Authzs};
        {error, Reason} -> throw({order_creation_failed, Reason})
    end.

%% For each authorization: fetch it, pick the http-01 challenge, publish the
%% key authorization, trigger the challenge, poll until valid, then unpublish.
run_authorizations(Config, Dir, AcctKey, Kid, AuthzUrls) ->
    lists:foreach(
        fun(AuthzUrl) ->
            run_authorization(Config, Dir, AcctKey, Kid, AuthzUrl)
        end,
        AuthzUrls
    ),
    ok.

run_authorization(Config, Dir, AcctKey, Kid, AuthzUrl) ->
    Authz = fetch_authorization(Config, Dir, AcctKey, Kid, AuthzUrl),
    Challenge = pick_http01(maps:get(<<"challenges">>, Authz)),
    Token = maps:get(<<"token">>, Challenge),
    ChallengeUrl = maps:get(<<"url">>, Challenge),
    KeyAuth = key_authorization(Token, AcctKey),
    Publish = maps:get(publish, Config),
    Unpublish = maps:get(unpublish, Config),
    ok = Publish(Token, KeyAuth),
    try
        trigger_challenge(Config, Dir, AcctKey, Kid, ChallengeUrl),
        ok = poll_authorization(Config, Dir, AcctKey, Kid, AuthzUrl)
    after
        catch Unpublish(Token)
    end.

%% Authorizations are POST-as-GET (empty body) in ACME v2.
fetch_authorization(Config, Dir, AcctKey, Kid, AuthzUrl) ->
    case jws_request(Config, Dir, AuthzUrl, post_as_get, AcctKey, Kid) of
        {ok, Resp, _Headers} -> Resp;
        {error, Reason} -> throw({authorization_fetch_failed, Reason})
    end.

%% HTTP-01 (RFC 8555 8.3): select the challenge with type "http-01". The DNS-01
%% client selected "dns-01" here instead.
pick_http01(Challenges) ->
    Matches =
        [C || C <- Challenges, maps:get(<<"type">>, C, undefined) =:= <<"http-01">>],
    case Matches of
        [Challenge | _] -> Challenge;
        [] -> throw(http01_challenge_not_found)
    end.

%% Trigger the server's validation by POSTing an empty JSON object {} to the
%% challenge URL, JWS-signed. Same as DNS-01.
trigger_challenge(Config, Dir, AcctKey, Kid, ChallengeUrl) ->
    case jws_request(Config, Dir, ChallengeUrl, #{}, AcctKey, Kid) of
        {ok, _Resp, _Headers} -> ok;
        {error, Reason} -> throw({challenge_trigger_failed, Reason})
    end.

poll_authorization(Config, Dir, AcctKey, Kid, AuthzUrl) ->
    poll_authorization(Config, Dir, AcctKey, Kid, AuthzUrl, ?POLL_ATTEMPTS).

poll_authorization(_Config, _Dir, _AcctKey, _Kid, _AuthzUrl, 0) ->
    throw(authorization_poll_timeout);
poll_authorization(Config, Dir, AcctKey, Kid, AuthzUrl, N) ->
    Authz = fetch_authorization(Config, Dir, AcctKey, Kid, AuthzUrl),
    case maps:get(<<"status">>, Authz) of
        <<"valid">> -> ok;
        <<"pending">> -> retry_authorization(Config, Dir, AcctKey, Kid, AuthzUrl, N);
        <<"processing">> -> retry_authorization(Config, Dir, AcctKey, Kid, AuthzUrl, N);
        Status -> throw({authorization_failed, Status, Authz})
    end.

retry_authorization(Config, Dir, AcctKey, Kid, AuthzUrl, N) ->
    timer:sleep(?POLL_INTERVAL_MS),
    poll_authorization(Config, Dir, AcctKey, Kid, AuthzUrl, N - 1).

%% finalize: submit the base64url DER CSR to the order's finalize URL.
finalize(Config, Dir, AcctKey, Kid, FinalizeUrl, CsrDer) ->
    Payload = #{<<"csr">> => base64url(CsrDer)},
    case jws_request(Config, Dir, FinalizeUrl, Payload, AcctKey, Kid) of
        {ok, _Resp, _Headers} -> ok;
        {error, Reason} -> throw({finalize_failed, Reason})
    end.

%% Poll the order (POST-as-GET) until valid, then return its certificate URL.
poll_order(Config, Dir, AcctKey, Kid, OrderUrl) ->
    poll_order(Config, Dir, AcctKey, Kid, OrderUrl, ?POLL_ATTEMPTS).

poll_order(_Config, _Dir, _AcctKey, _Kid, _OrderUrl, 0) ->
    throw(order_poll_timeout);
poll_order(Config, Dir, AcctKey, Kid, OrderUrl, N) ->
    case jws_request(Config, Dir, OrderUrl, post_as_get, AcctKey, Kid) of
        {ok, Resp, _Headers} ->
            case maps:get(<<"status">>, Resp) of
                <<"valid">> -> maps:get(<<"certificate">>, Resp);
                <<"processing">> -> retry_order(Config, Dir, AcctKey, Kid, OrderUrl, N);
                <<"ready">> -> retry_order(Config, Dir, AcctKey, Kid, OrderUrl, N);
                Status -> throw({order_failed, Status, Resp})
            end;
        {error, Reason} -> throw({order_poll_failed, Reason})
    end.

retry_order(Config, Dir, AcctKey, Kid, OrderUrl, N) ->
    timer:sleep(?POLL_INTERVAL_MS),
    poll_order(Config, Dir, AcctKey, Kid, OrderUrl, N - 1).

%% Download the issued chain (POST-as-GET); returned PEM is cert + intermediates.
download_certificate(Config, Dir, AcctKey, Kid, CertUrl) ->
    case jws_request(Config, Dir, CertUrl, post_as_get, AcctKey, Kid, raw) of
        {ok, Body, _Headers} -> Body;
        {error, Reason} -> throw({certificate_download_failed, Reason})
    end.

%%%--------------------------------------------------------------------
%%% JWS
%%%--------------------------------------------------------------------

jws_request(Config, Dir, Url, Payload, AcctKey, Kid) ->
    jws_request(Config, Dir, Url, Payload, AcctKey, Kid, json).

jws_request(Config, Dir, Url, Payload, AcctKey, Kid, ResultMode) ->
    jws_request(Config, Dir, Url, Payload, AcctKey, Kid, ResultMode, ?NONCE_RETRIES).

%% Build and POST a JWS. The protected header carries alg/nonce/url plus either
%% the jwk (account creation) or the kid (everything after). Payload is the
%% base64url of the JSON body, or empty for a POST-as-GET. Result is decoded as
%% JSON, or returned raw for the certificate download. On a rejected nonce we
%% fetch a fresh one and retry.
jws_request(_Config, _Dir, _Url, _Payload, _AcctKey, _Kid, _ResultMode, 0) ->
    {error, too_many_bad_nonces};
jws_request(Config, Dir, Url, Payload, AcctKey, Kid, ResultMode, Attempts) ->
    Nonce = fresh_nonce(Config, Dir),
    Protected = protected_header(Url, AcctKey, Kid, Nonce),
    ProtectedB64 = base64url(hb_json:encode(Protected)),
    PayloadB64 = encode_payload(Payload),
    SigningInput = <<ProtectedB64/binary, ".", PayloadB64/binary>>,
    Signature = base64url(public_key:sign(SigningInput, sha256, AcctKey)),
    Jws = #{
        <<"protected">> => ProtectedB64,
        <<"payload">> => PayloadB64,
        <<"signature">> => Signature
    },
    Headers = [{"content-type", "application/jose+json"}],
    case http_post(Config, Url, Headers, hb_json:encode(Jws)) of
        {ok, Status, RespHeaders, Body} when Status >= 200, Status < 300 ->
            {ok, decode_result(ResultMode, Body), RespHeaders};
        {ok, Status, _RespHeaders, Body} ->
            Decoded = safe_decode(Body),
            case is_bad_nonce(Decoded) of
                true ->
                    jws_request(Config, Dir, Url, Payload, AcctKey, Kid,
                                ResultMode, Attempts - 1);
                false ->
                    {error, {http_error, Status, Decoded}}
            end;
        {error, Reason} ->
            {error, {connection_failed, Reason}}
    end.

is_bad_nonce(#{<<"type">> := <<"urn:ietf:params:acme:error:badNonce">>}) -> true;
is_bad_nonce(_) -> false.

protected_header(Url, AcctKey, undefined, Nonce) ->
    #{
        <<"alg">> => <<"RS256">>,
        <<"jwk">> => jwk(AcctKey),
        <<"nonce">> => Nonce,
        <<"url">> => to_bin(Url)
    };
protected_header(Url, _AcctKey, Kid, Nonce) ->
    #{
        <<"alg">> => <<"RS256">>,
        <<"kid">> => to_bin(Kid),
        <<"nonce">> => Nonce,
        <<"url">> => to_bin(Url)
    }.

encode_payload(post_as_get) -> <<>>;
encode_payload(Payload) -> base64url(hb_json:encode(Payload)).

decode_result(raw, Body) -> Body;
decode_result(json, <<>>) -> #{};
decode_result(json, Body) -> safe_decode(Body).

%% newNonce via HEAD; the nonce comes back in the Replay-Nonce header.
fresh_nonce(Config, Dir) ->
    NonceUrl = maps:get(<<"newNonce">>, Dir),
    case http_head(Config, NonceUrl) of
        {ok, _Status, Headers, _Body} ->
            case header(<<"replay-nonce">>, Headers) of
                undefined -> throw(nonce_missing);
                Nonce -> Nonce
            end;
        {error, Reason} -> throw({nonce_fetch_failed, Reason})
    end.

%% JWK for an RSA key (RFC 7517). The thumbprint relies on hb_json:encode
%% emitting keys in lexicographic order (e, kty, n), which it does.
jwk(#'RSAPrivateKey'{modulus = N, publicExponent = E}) ->
    #{
        <<"kty">> => <<"RSA">>,
        <<"n">> => base64url(unsigned(N)),
        <<"e">> => base64url(unsigned(E))
    }.

%% RFC 7638 thumbprint: sha256 of the canonical JWK JSON, base64url.
jwk_thumbprint(AcctKey) ->
    Canonical = hb_json:encode(jwk(AcctKey)),
    base64url(crypto:hash(sha256, Canonical)).

%% HTTP-01 key authorization (RFC 8555 8.1): token "." thumbprint, served
%% verbatim. DNS-01 additionally sha256-hashes this and base64url-encodes it for
%% the TXT record; HTTP-01 does NOT hash.
key_authorization(Token, AcctKey) ->
    Thumbprint = jwk_thumbprint(AcctKey),
    <<Token/binary, ".", Thumbprint/binary>>.

%%%--------------------------------------------------------------------
%%% Keys and CSR
%%%--------------------------------------------------------------------

generate_rsa_key() ->
    public_key:generate_key({rsa, 2048, 65537}).

%% PKCS#10 CSR with the domains as SAN (RFC 8555; CN is the first domain), cert
%% keypair in memory. RSA algorithm parameters must be an explicit ASN.1 NULL,
%% encoded as {asn1_OPENTYPE, <<5,0>>}, since Go's x509 (and RFC 3279) reject
%% absent parameters. The extensionRequest value is a pre-encoded Extensions DER
%% wrapped as an open type.
generate_csr(Domains) ->
    try
        CertKey = generate_rsa_key(),
        #'RSAPrivateKey'{modulus = N, publicExponent = E} = CertKey,
        PubKey = #'RSAPublicKey'{modulus = N, publicExponent = E},
        PubDer = public_key:der_encode('RSAPublicKey', PubKey),
        SANs = [{dNSName, to_list(D)} || D <- Domains],
        SanDer = public_key:der_encode('SubjectAltName', SANs),
        Ext = #'Extension'{
            extnID = ?'id-ce-subjectAltName',
            critical = false,
            extnValue = SanDer
        },
        ExtsDer = public_key:der_encode('Extensions', [Ext]),
        PubKeyInfo = #'SubjectPublicKeyInfo'{
            algorithm = #'AlgorithmIdentifier'{
                algorithm = ?'rsaEncryption',
                parameters = {asn1_OPENTYPE, <<5, 0>>}
            },
            subjectPublicKey = PubDer
        },
        CsrInfo = #'CertificationRequestInfo'{
            version = v1,
            subject = subject(hd(Domains)),
            subjectPKInfo = PubKeyInfo,
            attributes = [#'Attribute'{
                type = ?'pkcs-9-at-extensionRequest',
                values = [{asn1_OPENTYPE, ExtsDer}]
            }]
        },
        CsrInfoDer = public_key:der_encode('CertificationRequestInfo', CsrInfo),
        Signature = public_key:sign(CsrInfoDer, sha256, CertKey),
        Csr = #'CertificationRequest'{
            certificationRequestInfo = CsrInfo,
            signatureAlgorithm = #'AlgorithmIdentifier'{
                algorithm = ?'sha256WithRSAEncryption',
                parameters = {asn1_OPENTYPE, <<5, 0>>}
            },
            signature = Signature
        },
        {ok, public_key:der_encode('CertificationRequest', Csr), CertKey}
    catch
        Class:Reason -> {error, {csr_generation_failed, Class, Reason}}
    end.

subject(CommonName) ->
    {rdnSequence, [[#'AttributeTypeAndValue'{
        type = ?'id-at-commonName',
        value = {utf8String, to_bin(CommonName)}
    }]]}.

key_to_pem(Key) ->
    public_key:pem_encode([public_key:pem_entry_encode('RSAPrivateKey', Key)]).

%%%--------------------------------------------------------------------
%%% HTTP (inets/httpc, so the caller can pass ssl opts for a test CA)
%%%--------------------------------------------------------------------

http_get(Config, Url) ->
    http_request(Config, get, {to_list(Url), default_headers()}).

http_head(Config, Url) ->
    http_request(Config, head, {to_list(Url), default_headers()}).

http_post(Config, Url, Headers, Body) ->
    {_, ContentType} = lists:keyfind("content-type", 1, Headers),
    http_request(Config, post, {to_list(Url), default_headers(), ContentType, Body}).

%% ACME servers require a User-Agent on every request (Pebble rejects its
%% absence with 400; Let's Encrypt requires it too).
default_headers() ->
    [{"user-agent", "hyperbeam-acme/1.0"}].

http_request(Config, Method, Request) ->
    ensure_started(),
    SslOpts = maps:get(http_opts, Config, []),
    HttpOpts = [{ssl, SslOpts} || SslOpts =/= []],
    Opts = [{full_result, true}, {body_format, binary}],
    case httpc:request(Method, Request, HttpOpts, Opts) of
        {ok, {{_Version, Status, _Reason}, RespHeaders, Body}} ->
            {ok, Status, normalize_headers(RespHeaders), Body};
        {error, Reason} ->
            {error, Reason}
    end.

ensure_started() ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(ssl),
    ok.

%% httpc returns header names as lowercased strings; key everything by binary.
normalize_headers(Headers) ->
    [{to_bin(string:lowercase(K)), to_bin(V)} || {K, V} <- Headers].

header(Name, Headers) ->
    case lists:keyfind(Name, 1, Headers) of
        {Name, Value} -> Value;
        false -> undefined
    end.

%%%--------------------------------------------------------------------
%%% Helpers
%%%--------------------------------------------------------------------

%% base64url without padding (RFC 4648 5). Ported from Peter's
%% base64url_encode/1 but binary-native and stripping via binary:replace, so it
%% does not depend on the b64veryfast NIF.
base64url(Data) when is_binary(Data) ->
    Encoded = base64:encode(Data),
    NoPlus = binary:replace(Encoded, <<"+">>, <<"-">>, [global]),
    NoSlash = binary:replace(NoPlus, <<"/">>, <<"_">>, [global]),
    binary:replace(NoSlash, <<"=">>, <<>>, [global]).

unsigned(Int) -> binary:encode_unsigned(Int).

safe_decode(Body) ->
    try hb_json:decode(Body)
    catch _:_ -> #{<<"raw">> => Body}
    end.

to_bin(V) when is_binary(V) -> V;
to_bin(V) when is_list(V) -> list_to_binary(V);
to_bin(V) when is_atom(V) -> atom_to_binary(V, utf8).

to_list(V) when is_list(V) -> V;
to_list(V) when is_binary(V) -> binary_to_list(V);
to_list(V) when is_atom(V) -> atom_to_list(V).

%%% Tests

-include_lib("eunit/include/eunit.hrl").

%% base64url strips padding and uses the url-safe alphabet.
base64url_test() ->
    ?assertEqual(<<"AQAB">>, base64url(<<1, 0, 1>>)),
    ?assertEqual(<<>>, base64url(<<>>)),
    %% 0xFB 0xFF -> "+/" in standard base64, "-_" url-safe, padding stripped.
    ?assertEqual(<<"-_8">>, base64url(<<16#FB, 16#FF>>)).

%% The JWK thumbprint input is canonical: keys sorted as e, kty, n.
jwk_canonical_order_test() ->
    Key = generate_rsa_key(),
    Json = hb_json:encode(jwk(Key)),
    ?assertEqual({0, 5}, binary:match(Json, <<"{\"e\":">>)),
    ?assertNotEqual(nomatch, binary:match(Json, <<"\"kty\":\"RSA\"">>)).

%% HTTP-01 key authorization is token "." thumbprint, served verbatim (no hash).
key_authorization_test() ->
    Key = generate_rsa_key(),
    Token = <<"tok123">>,
    KeyAuth = key_authorization(Token, Key),
    [Tok, Thumb] = binary:split(KeyAuth, <<".">>),
    ?assertEqual(Token, Tok),
    ?assertEqual(jwk_thumbprint(Key), Thumb).

%% pick_http01 selects the http-01 challenge, not dns-01.
pick_http01_test() ->
    Challenges = [
        #{<<"type">> => <<"dns-01">>, <<"token">> => <<"d">>},
        #{<<"type">> => <<"http-01">>, <<"token">> => <<"h">>}
    ],
    ?assertEqual(<<"h">>, maps:get(<<"token">>, pick_http01(Challenges))),
    ?assertThrow(http01_challenge_not_found, pick_http01([#{<<"type">> => <<"dns-01">>}])).

%% A full CSR for multiple domains encodes, re-decodes, and self-verifies.
generate_csr_test() ->
    Domains = [<<"example.com">>, <<"www.example.com">>],
    {ok, CsrDer, CertKey} = generate_csr(Domains),
    Decoded = public_key:der_decode('CertificationRequest', CsrDer),
    #'CertificationRequest'{
        certificationRequestInfo = RI,
        signature = Sig
    } = Decoded,
    #'RSAPrivateKey'{modulus = N, publicExponent = E} = CertKey,
    PubKey = #'RSAPublicKey'{modulus = N, publicExponent = E},
    RIDer = public_key:der_encode('CertificationRequestInfo', RI),
    ?assert(public_key:verify(RIDer, sha256, Sig, PubKey)).

%% The cert key serializes to a PEM that hb_tls:entry_opts can pem_decode.
key_to_pem_test() ->
    Pem = key_to_pem(generate_rsa_key()),
    [{KeyTag, _Der, _} | _] = public_key:pem_decode(Pem),
    ?assertEqual('RSAPrivateKey', KeyTag).

%% End-to-end issuance against a local Pebble ACME server, skipped unless Pebble
%% is reachable on :14000. If Pebble issues a parseable cert, the whole JWS /
%% JWK thumbprint / keyAuthorization / CSR / finalize / download chain is proven
%% correct (Pebble verifies every signature and fetches the challenge for real).
pebble_obtain_test_() ->
    case pebble_running() of
        true -> {timeout, 60, fun pebble_obtain/0};
        false -> []
    end.

pebble_running() ->
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    Req = {"https://localhost:14000/dir", [{"user-agent", "hyperbeam-acme/1.0"}]},
    case httpc:request(get, Req, [{ssl, [{verify, verify_none}]}], []) of
        {ok, {{_, 200, _}, _, _}} -> true;
        _ -> false
    end.

pebble_obtain() ->
    Mgmt = "http://localhost:8055",
    %% The publish callback serves the token via challtestsrv (stands in for our
    %% own :80 listener, which is the next slice). Pebble fetches it to validate.
    Publish = fun(Token, KeyAuth) ->
        post_json(Mgmt ++ "/add-http01",
                  #{<<"token">> => Token, <<"content">> => KeyAuth}),
        ok
    end,
    Unpublish = fun(Token) ->
        post_json(Mgmt ++ "/del-http01", #{<<"token">> => Token}),
        ok
    end,
    Config = #{
        <<"domains">> => [<<"pebble-test.example">>],
        <<"email">> => <<"acme@example.com">>,
        <<"directory_url">> => <<"https://localhost:14000/dir">>,
        <<"publish">> => Publish,
        <<"unpublish">> => Unpublish,
        <<"http_opts">> => [{verify, verify_none}]
    },
    {ok, #{<<"cert">> := CertPem, <<"key">> := KeyPem}} = obtain(Config, #{}),
    Chain = public_key:pem_decode(CertPem),
    ?debugFmt("Pebble issued a cert: ~p PEM bytes, ~p chain entries",
              [byte_size(CertPem), length(Chain)]),
    ?assertMatch([{'Certificate', _, _} | _], Chain),
    ?assertMatch([{'RSAPrivateKey', _, _} | _], public_key:pem_decode(KeyPem)).

post_json(Url, Map) ->
    httpc:request(post,
        {Url, [], "application/json", hb_json:encode(Map)},
        [], [{body_format, binary}]).
