%%% @doc ACME v2 client (RFC 8555) for obtaining certificates via HTTP-01.
%%% No JOSE dependency: keys are RSA, generated in memory, never written to
%%% disk. The caller serves the key authorization via the `publish'/`unpublish'
%%% callbacks in Config.
-module(hb_acme).
-export([obtain/2]).
-include_lib("public_key/include/public_key.hrl").

-define(POLL_INTERVAL_MS, 2000).
-define(POLL_ATTEMPTS, 30).
%% Retries for nonces rejected by the server (RFC 8555 6.5).
-define(NONCE_RETRIES, 5).
%% Per-request bounds so a stalled CA endpoint fails the pass instead of
%% hanging the renewal worker.
-define(HTTP_OPTS, [{timeout, 30000}, {connect_timeout, 10000}, {autoredirect, false}]).

%% @doc Obtain a certificate over HTTP-01. Config keys: domains, email,
%% directory_url, publish, unpublish, optional http_opts (ssl opts for the ACME
%% client). Returns the chain and key as PEM binaries.
obtain(Config, _Opts) ->
    do_obtain(normalize_config(Config)).

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

%% newAccount: jwk in the JWS header; the Location header becomes the kid.
%% `contact' is only sent when an email is configured (LE rejects "mailto:").
new_account(Config, Dir, AcctKey) ->
    Url = maps:get(<<"newAccount">>, Dir),
    Base = #{<<"termsOfServiceAgreed">> => true},
    Payload =
        case to_bin(maps:get(email, Config)) of
            <<>> -> Base;
            Email -> Base#{<<"contact">> => [<<"mailto:", Email/binary>>]}
        end,
    case jws_request(Config, Dir, Url, Payload, AcctKey, undefined) of
        {ok, _Resp, Headers} ->
            case header(<<"location">>, Headers) of
                undefined -> throw(account_location_missing);
                Kid -> Kid
            end;
        {error, Reason} -> throw({account_creation_failed, Reason})
    end.

new_order(Config, Dir, AcctKey, Kid, Domains) ->
    Url = maps:get(<<"newOrder">>, Dir),
    Identifiers =
        [#{<<"type">> => <<"dns">>, <<"value">> => to_bin(D)} || D <- Domains],
    case jws_request(Config, Dir, Url, #{<<"identifiers">> => Identifiers}, AcctKey, Kid) of
        {ok, Resp, Headers} ->
            {header(<<"location">>, Headers),
             maps:get(<<"finalize">>, Resp),
             maps:get(<<"authorizations">>, Resp)};
        {error, Reason} -> throw({order_creation_failed, Reason})
    end.

%% Per authorization: publish the key authorization, trigger, poll to valid.
run_authorizations(Config, Dir, AcctKey, Kid, AuthzUrls) ->
    lists:foreach(
        fun(AuthzUrl) -> run_authorization(Config, Dir, AcctKey, Kid, AuthzUrl) end,
        AuthzUrls
    ).

run_authorization(Config, Dir, AcctKey, Kid, AuthzUrl) ->
    Authz = fetch_authorization(Config, Dir, AcctKey, Kid, AuthzUrl),
    Challenge = pick_http01(maps:get(<<"challenges">>, Authz)),
    Token = maps:get(<<"token">>, Challenge),
    KeyAuth = key_authorization(Token, AcctKey),
    Publish = maps:get(publish, Config),
    Unpublish = maps:get(unpublish, Config),
    ok = Publish(Token, KeyAuth),
    try
        trigger_challenge(Config, Dir, AcctKey, Kid, maps:get(<<"url">>, Challenge)),
        ok = poll_authorization(Config, Dir, AcctKey, Kid, AuthzUrl)
    after
        catch Unpublish(Token)
    end.

fetch_authorization(Config, Dir, AcctKey, Kid, AuthzUrl) ->
    case jws_request(Config, Dir, AuthzUrl, post_as_get, AcctKey, Kid) of
        {ok, Resp, _Headers} -> Resp;
        {error, Reason} -> throw({authorization_fetch_failed, Reason})
    end.

pick_http01(Challenges) ->
    case [C || C <- Challenges, maps:get(<<"type">>, C, undefined) =:= <<"http-01">>] of
        [Challenge | _] -> Challenge;
        [] -> throw(http01_challenge_not_found)
    end.

%% Trigger validation: POST {} to the challenge URL.
trigger_challenge(Config, Dir, AcctKey, Kid, ChallengeUrl) ->
    case jws_request(Config, Dir, ChallengeUrl, #{}, AcctKey, Kid) of
        {ok, _Resp, _Headers} -> ok;
        {error, Reason} -> throw({challenge_trigger_failed, Reason})
    end.

poll_authorization(Config, Dir, AcctKey, Kid, AuthzUrl) ->
    poll(
        fun() ->
            Authz = fetch_authorization(Config, Dir, AcctKey, Kid, AuthzUrl),
            case maps:get(<<"status">>, Authz) of
                <<"valid">> -> ok;
                S when S =:= <<"pending">>; S =:= <<"processing">> -> retry;
                Status -> throw({authorization_failed, Status, Authz})
            end
        end,
        authorization_poll_timeout
    ).

%% Poll the order until valid, returning its certificate URL.
poll_order(Config, Dir, AcctKey, Kid, OrderUrl) ->
    poll(
        fun() ->
            case jws_request(Config, Dir, OrderUrl, post_as_get, AcctKey, Kid) of
                {ok, Resp, _Headers} ->
                    case maps:get(<<"status">>, Resp) of
                        <<"valid">> -> maps:get(<<"certificate">>, Resp);
                        S when S =:= <<"processing">>; S =:= <<"ready">> -> retry;
                        Status -> throw({order_failed, Status, Resp})
                    end;
                {error, Reason} -> throw({order_poll_failed, Reason})
            end
        end,
        order_poll_timeout
    ).

poll(Fun, Timeout) -> poll(Fun, Timeout, ?POLL_ATTEMPTS).
poll(_Fun, Timeout, 0) -> throw(Timeout);
poll(Fun, Timeout, N) ->
    case Fun() of
        retry -> timer:sleep(?POLL_INTERVAL_MS), poll(Fun, Timeout, N - 1);
        Result -> Result
    end.

finalize(Config, Dir, AcctKey, Kid, FinalizeUrl, CsrDer) ->
    case jws_request(Config, Dir, FinalizeUrl, #{<<"csr">> => base64url(CsrDer)}, AcctKey, Kid) of
        {ok, _Resp, _Headers} -> ok;
        {error, Reason} -> throw({finalize_failed, Reason})
    end.

%% POST-as-GET; the returned PEM is the leaf plus intermediates.
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

jws_request(_Config, _Dir, _Url, _Payload, _AcctKey, _Kid, _ResultMode, 0) ->
    {error, too_many_bad_nonces};
jws_request(Config, Dir, Url, Payload, AcctKey, Kid, ResultMode, Attempts) ->
    Nonce = fresh_nonce(Config, Dir),
    ProtectedB64 = base64url(hb_json:encode(protected_header(Url, AcctKey, Kid, Nonce))),
    PayloadB64 = encode_payload(Payload),
    SigningInput = <<ProtectedB64/binary, ".", PayloadB64/binary>>,
    Jws = #{
        <<"protected">> => ProtectedB64,
        <<"payload">> => PayloadB64,
        <<"signature">> => base64url(public_key:sign(SigningInput, sha256, AcctKey))
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
    #{<<"alg">> => <<"RS256">>, <<"jwk">> => jwk(AcctKey),
      <<"nonce">> => Nonce, <<"url">> => to_bin(Url)};
protected_header(Url, _AcctKey, Kid, Nonce) ->
    #{<<"alg">> => <<"RS256">>, <<"kid">> => to_bin(Kid),
      <<"nonce">> => Nonce, <<"url">> => to_bin(Url)}.

encode_payload(post_as_get) -> <<>>;
encode_payload(Payload) -> base64url(hb_json:encode(Payload)).

decode_result(raw, Body) -> Body;
decode_result(json, <<>>) -> #{};
decode_result(json, Body) -> safe_decode(Body).

%% newNonce via HEAD; the nonce arrives in the Replay-Nonce header.
fresh_nonce(Config, Dir) ->
    case http_head(Config, maps:get(<<"newNonce">>, Dir)) of
        {ok, _Status, Headers, _Body} ->
            case header(<<"replay-nonce">>, Headers) of
                undefined -> throw(nonce_missing);
                Nonce -> Nonce
            end;
        {error, Reason} -> throw({nonce_fetch_failed, Reason})
    end.

%% RFC 7517 JWK. The thumbprint relies on hb_json:encode emitting keys in
%% lexicographic order (e, kty, n), which it does.
jwk(#'RSAPrivateKey'{modulus = N, publicExponent = E}) ->
    #{
        <<"kty">> => <<"RSA">>,
        <<"n">> => base64url(binary:encode_unsigned(N)),
        <<"e">> => base64url(binary:encode_unsigned(E))
    }.

%% RFC 7638 thumbprint: sha256 of the canonical JWK JSON.
jwk_thumbprint(AcctKey) ->
    base64url(crypto:hash(sha256, hb_json:encode(jwk(AcctKey)))).

%% RFC 8555 8.1: token "." thumbprint, served verbatim (HTTP-01 does not hash).
key_authorization(Token, AcctKey) ->
    <<Token/binary, ".", (jwk_thumbprint(AcctKey))/binary>>.

%%%--------------------------------------------------------------------
%%% Keys and CSR
%%%--------------------------------------------------------------------

generate_rsa_key() ->
    public_key:generate_key({rsa, 2048, 65537}).

%% PKCS#10 CSR with the domains as SAN, CN = first domain, keypair in memory.
%% RSA algorithm parameters must be an explicit ASN.1 NULL ({asn1_OPENTYPE,
%% <<5,0>>}): Go's x509 (RFC 3279) rejects absent parameters. The
%% extensionRequest value is pre-encoded Extensions DER wrapped as an open type.
generate_csr(Domains) ->
    try
        CertKey = generate_rsa_key(),
        #'RSAPrivateKey'{modulus = N, publicExponent = E} = CertKey,
        PubDer = public_key:der_encode(
            'RSAPublicKey',
            #'RSAPublicKey'{modulus = N, publicExponent = E}
        ),
        SanDer = public_key:der_encode(
            'SubjectAltName',
            [{dNSName, to_list(D)} || D <- Domains]
        ),
        ExtsDer = public_key:der_encode('Extensions', [
            #'Extension'{
                extnID = ?'id-ce-subjectAltName',
                critical = false,
                extnValue = SanDer
            }
        ]),
        CsrInfo = #'CertificationRequestInfo'{
            version = v1,
            subject = subject(hd(Domains)),
            subjectPKInfo = #'SubjectPublicKeyInfo'{
                algorithm = #'AlgorithmIdentifier'{
                    algorithm = ?'rsaEncryption',
                    parameters = {asn1_OPENTYPE, <<5, 0>>}
                },
                subjectPublicKey = PubDer
            },
            attributes = [#'Attribute'{
                type = ?'pkcs-9-at-extensionRequest',
                values = [{asn1_OPENTYPE, ExtsDer}]
            }]
        },
        CsrInfoDer = public_key:der_encode('CertificationRequestInfo', CsrInfo),
        Csr = #'CertificationRequest'{
            certificationRequestInfo = CsrInfo,
            signatureAlgorithm = #'AlgorithmIdentifier'{
                algorithm = ?'sha256WithRSAEncryption',
                parameters = {asn1_OPENTYPE, <<5, 0>>}
            },
            signature = public_key:sign(CsrInfoDer, sha256, CertKey)
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

%% ACME servers reject requests without a User-Agent.
default_headers() ->
    [{"user-agent", "hyperbeam-acme/1.0"}].

http_request(Config, Method, Request) ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(ssl),
    HttpOpts =
        ?HTTP_OPTS ++
        [{ssl, SslOpts} || SslOpts <- [maps:get(http_opts, Config, [])], SslOpts =/= []],
    Opts = [{full_result, true}, {body_format, binary}],
    case httpc:request(Method, Request, HttpOpts, Opts) of
        {ok, {{_Version, Status, _Reason}, RespHeaders, Body}} ->
            {ok, Status, normalize_headers(RespHeaders), Body};
        {error, Reason} ->
            {error, Reason}
    end.

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

%% base64url without padding (RFC 4648 5), binary-native: no b64veryfast NIF.
base64url(Data) when is_binary(Data) ->
    Encoded = base64:encode(Data),
    NoPlus = binary:replace(Encoded, <<"+">>, <<"-">>, [global]),
    NoSlash = binary:replace(NoPlus, <<"/">>, <<"_">>, [global]),
    binary:replace(NoSlash, <<"=">>, <<>>, [global]).

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

base64url_test() ->
    ?assertEqual(<<"AQAB">>, base64url(<<1, 0, 1>>)),
    ?assertEqual(<<>>, base64url(<<>>)),
    ?assertEqual(<<"-_8">>, base64url(<<16#FB, 16#FF>>)).

jwk_canonical_order_test() ->
    Key = generate_rsa_key(),
    Json = hb_json:encode(jwk(Key)),
    ?assertEqual({0, 5}, binary:match(Json, <<"{\"e\":">>)),
    ?assertNotEqual(nomatch, binary:match(Json, <<"\"kty\":\"RSA\"">>)).

key_authorization_test() ->
    Key = generate_rsa_key(),
    KeyAuth = key_authorization(<<"tok123">>, Key),
    [Tok, Thumb] = binary:split(KeyAuth, <<".">>),
    ?assertEqual(<<"tok123">>, Tok),
    ?assertEqual(jwk_thumbprint(Key), Thumb).

pick_http01_test() ->
    Challenges = [
        #{<<"type">> => <<"dns-01">>, <<"token">> => <<"d">>},
        #{<<"type">> => <<"http-01">>, <<"token">> => <<"h">>}
    ],
    ?assertEqual(<<"h">>, maps:get(<<"token">>, pick_http01(Challenges))),
    ?assertThrow(http01_challenge_not_found, pick_http01([#{<<"type">> => <<"dns-01">>}])).

generate_csr_test() ->
    {ok, CsrDer, CertKey} = generate_csr([<<"example.com">>, <<"www.example.com">>]),
    #'CertificationRequest'{
        certificationRequestInfo = RI,
        signature = Sig
    } = public_key:der_decode('CertificationRequest', CsrDer),
    #'RSAPrivateKey'{modulus = N, publicExponent = E} = CertKey,
    RIDer = public_key:der_encode('CertificationRequestInfo', RI),
    ?assert(public_key:verify(
        RIDer, sha256, Sig,
        #'RSAPublicKey'{modulus = N, publicExponent = E}
    )).

key_to_pem_test() ->
    [{KeyTag, _Der, _} | _] = public_key:pem_decode(key_to_pem(generate_rsa_key())),
    ?assertEqual('RSAPrivateKey', KeyTag).
