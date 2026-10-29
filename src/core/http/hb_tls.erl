%%% @doc TLS policy and node-wallet key adapter.
-module(hb_tls).
-export([config/1, certificate_expiry/1, install/3, socket_options/2]).
-export([csr/2, certificate_chain/1]).
-export([wallet/1, validate/3, certificate_lifetime/1, notify/1]).
-include("include/ar.hrl").
-include_lib("public_key/include/public_key.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(UNIX_EPOCH, 62167219200).

config(NodeMsg) ->
    case hb_opts:get(tls, false, NodeMsg) of
        false -> false;
        TLS ->
            case hb_cache:ensure_all_loaded(TLS, NodeMsg) of
                Config when is_map(Config) -> Config;
                Invalid -> error({'invalid-tls-config', Invalid})
            end
    end.

%% @doc Select only the configured identity, without changing the node identity.
wallet(Opts) ->
    NodeOpts = hb_http_server:with_node_committer(Opts),
    case hb_ao:get(<<"tls/identity">>, NodeOpts, not_found, NodeOpts) of
        not_found -> {ok, hb_opts:get(priv_wallet, no_viable_wallet, NodeOpts)};
        Identity ->
            case hb_opts:as(Identity, NodeOpts) of
                {ok, Scoped} ->
                    {ok, hb_opts:get(priv_wallet, no_viable_wallet, Scoped)};
                {error, _} -> {error, 'tls-identity-unavailable'}
            end
    end.

%% @doc Wake an existing TLS lifecycle after a node-options update.
notify(ServerID) ->
    case hb_name:lookup({<<"tls@1.0">>, ServerID}) of
        undefined -> ok;
        PID -> PID ! options_changed, ok
    end.

%% @doc Validate an imported chain independently of the peer that supplied it.
%% The configured SAN set must match exactly; trust is local, never peer-supplied.
validate(Wallet, [Leaf | _] = Chain, TLS) ->
    try
        {ok, _} = socket_options(Wallet, Chain),
        Certificate = public_key:pkix_decode_cert(Leaf, otp),
        TBS = Certificate#'OTPCertificate'.tbsCertificate,
        Extensions = TBS#'OTPTBSCertificate'.extensions,
        #'Extension'{extnValue = SANs} =
            lists:keyfind(?'id-ce-subjectAltName', 2, Extensions),
        Names = lists:usort([hb_util:to_lower(hb_util:bin(Name))
            || {dNSName, Name} <- SANs]),
        true = Names =/= [] andalso Names =:= lists:usort([
            hb_util:to_lower(Domain) || Domain <- maps:get(<<"domains">>, TLS)
        ]),
        true = server_usage(Extensions),
        true = certificate_expiry(Chain) > erlang:system_time(millisecond),
        Roots = case maps:get(<<"ca-certificates">>, TLS, undefined) of
            undefined -> public_key:cacerts_get();
            PEM -> {ok, CAs} = certificate_chain(PEM), CAs
        end,
        true = lists:any(fun(Root) ->
            Path = lists:reverse(lists:delete(Root, Chain)),
            case public_key:pkix_path_validation(Root, Path, []) of
                {ok, _} -> true;
                _ -> false
            end
        end, Roots),
        ok
    catch _:_ -> {error, 'invalid-tls-certificate'}
    end;
validate(_, _, _) -> {error, 'invalid-tls-certificate'}.

%% @doc Restrict any declared key usage to a key usable for TLS server signing.
server_usage(Extensions) ->
    lists:all(fun({OID, Required}) ->
        case lists:keyfind(OID, 2, Extensions) of
            false -> true;
            #'Extension'{extnValue = Values} -> lists:member(Required, Values)
        end
    end, [
        {?'id-ce-keyUsage', digitalSignature},
        {?'id-ce-extKeyUsage', ?'id-kp-serverAuth'}
    ]).

%% @doc Return the signed lifetime, not a lifetime asserted by a peer.
certificate_lifetime([Leaf | _] = Chain) ->
    Certificate = public_key:pkix_decode_cert(Leaf, otp),
    TBS = Certificate#'OTPCertificate'.tbsCertificate,
    certificate_expiry(Chain) - certificate_time(
        TBS#'OTPTBSCertificate'.validity#'Validity'.notBefore
    ).

%% @doc Decode a PEM certificate chain, with the leaf certificate first.
certificate_chain(PEM) ->
    try
        case [DER || {'Certificate', DER, not_encrypted} <-
                public_key:pem_decode(PEM)] of
            [] -> {error, 'invalid-certificate-chain'};
            Chain -> {ok, Chain}
        end
    catch
        _:_ -> {error, 'invalid-certificate-chain'}
    end.

%% @doc Create a SAN PKCS#10 request using the node wallet's exact key.
csr(Wallet = #{ <<"type">> := Type = ?RSA_SIGN_TYPE }, Domains) ->
    #{ <<"owner">> := N, <<"priv">> := #{ <<"d">> := D } } = Wallet,
    {rsa, E} = ar_tx:deserialize_sig_type(Type),
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
            signature = crypto:sign(rsa, sha256, EncodedInfo,
                [E, binary:decode_unsigned(N), binary:decode_unsigned(D)],
                [{rsa_padding, rsa_pkcs1_padding}])
        }
    ),
    Encoded.

%% @doc Encode the request attributes using the running OTP's ASN.1 schema.
csr_info(Wallet, Names, Extensions, AttributeRecord) ->
    Info = #'CertificationRequestInfo'{
        version = 0,
        subject = {rdnSequence, [[#'AttributeTypeAndValue'{
            type = ?'id-at-commonName',
            value = {utf8String, hd(Names)}
        }]]},
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

%% @doc Represent the wallet's RSA key in a certificate request.
csr_public_key_info(#{ <<"type">> := Type, <<"owner">> := N }) ->
    {rsa, E} = ar_tx:deserialize_sig_type(Type),
    #'CertificationRequestInfo_subjectPKInfo'{
        algorithm = #'CertificationRequestInfo_subjectPKInfo_algorithm'{
            algorithm = ?'rsaEncryption',
            parameters = {asn1_OPENTYPE, <<5, 0>>}
        },
        subjectPublicKey = public_key:der_encode(
            'RSAPublicKey', rsa_public_key(E, N)
        )
    }.

%% @doc Replace a listener's leaf without dropping established connections.
install(ServerID, Wallet, Chain) ->
    case socket_options(Wallet, Chain) of
        {error, _} = Error -> Error;
        {ok, TLSOpts} ->
            try
                TransportOpts = ranch:get_transport_options(ServerID),
                SocketOpts = maps:get(socket_opts, TransportOpts, []),
                Keep = lists:foldl(
                    fun(Key, Opts) -> lists:keydelete(Key, 1, Opts) end,
                    SocketOpts,
                    [port, certs_keys]
                ),
                NewOpts = TransportOpts#{socket_opts =>
                    [{port, ranch:get_port(ServerID)} | TLSOpts ++ Keep]},
                ok = ranch:suspend_listener(ServerID),
                try
                    ok = ranch:set_transport_options(ServerID, NewOpts),
                    ok = ranch:resume_listener(ServerID)
                catch
                    Class:Reason:Stack ->
                        ranch:resume_listener(ServerID),
                        erlang:raise(Class, Reason, Stack)
                end
            catch
                _:UpdateReason ->
                    {error, {'tls-listener-update-failed', UpdateReason}}
            end
    end.

%% @doc Millisecond Unix expiry of the leaf certificate.
certificate_expiry([Leaf | _]) ->
    Certificate = public_key:pkix_decode_cert(Leaf, otp),
    TBS = Certificate#'OTPCertificate'.tbsCertificate,
    certificate_time(TBS#'OTPTBSCertificate'.validity#'Validity'.notAfter).

certificate_time({generalTime, Time}) ->
    certificate_time(Time);
certificate_time({utcTime, [Y1, Y2 | Rest]}) ->
    Century = case [Y1, Y2] >= "50" of true -> "19"; false -> "20" end,
    certificate_time(Century ++ [Y1, Y2 | Rest]);
certificate_time(Time) ->
    {ok, [Y, M, D, H, I, S], _} =
        io_lib:fread("~4d~2d~2d~2d~2d~2d", Time),
    1000 * (calendar:datetime_to_gregorian_seconds(
        {{Y, M, D}, {H, I, S}}
    ) - ?UNIX_EPOCH).

%% @doc Build SSL options only when the leaf carries the exact wallet key.
socket_options(Wallet = #{ <<"type">> := Type = ?RSA_SIGN_TYPE },
        [Leaf | _] = Chain) ->
    #{ <<"owner">> := N } = Wallet,
    {rsa, E} = ar_tx:deserialize_sig_type(Type),
    Key = ar_wallet:rsa_private_key(Wallet),
    try
        Certificate = public_key:pkix_decode_cert(Leaf, otp),
        TBS = Certificate#'OTPCertificate'.tbsCertificate,
        SPKI = TBS#'OTPTBSCertificate'.subjectPublicKeyInfo,
        true = SPKI#'OTPSubjectPublicKeyInfo'.subjectPublicKey =:=
            rsa_public_key(E, N),
        Sign = fun(Data, Digest, Options) ->
            public_key:sign(Data, Digest, Key, Options)
        end,
        {ok, [{certs_keys, [#{
            cert => Chain,
            key => #{algorithm => rsa, sign_fun => Sign}
        }]}]}
    catch
        error:{badmatch, false} -> {error, 'certificate-key-mismatch'};
        _:_ -> {error, 'invalid-certificate-chain'}
    end;
socket_options(_, _) ->
    {error, 'unsupported-tls-wallet'}.

rsa_public_key(E, N) ->
    #'RSAPublicKey'{
        publicExponent = E,
        modulus = binary:decode_unsigned(N)
    }.

%%% Tests

csr_key_test() ->
    Wallet = #{ <<"type">> := Type, <<"owner">> := N } =
        ar_wallet:load_keyfile("test/key-1.json"),
    % Loading the key file keeps the primes of the key.
    ?assertMatch(#{ <<"priv">> := #{ <<"p">> := _, <<"q">> := _ } }, Wallet),
    % Loading the key with only `n' and `d' recovers the same primes.
    ?assertEqual(Wallet, ar_wallet:load_keyfile("test/key-1-no-primes.json")),
    {rsa, E} = ar_tx:deserialize_sig_type(Type),
    Encoded = csr(Wallet, [<<"localhost">>, <<"node.example">>]),
    CSR = public_key:der_decode('CertificationRequest', Encoded),
    Info = CSR#'CertificationRequest'.certificationRequestInfo,
    {ok, EncodedInfo} = 'PKCS-10':encode('CertificationRequestInfo', Info),
    ?assert(public_key:verify(
        EncodedInfo,
        sha256,
        CSR#'CertificationRequest'.signature,
        rsa_public_key(E, N)
    )),
    CSRKey = Info#'CertificationRequestInfo'.subjectPKInfo,
    ?assertEqual(rsa_public_key(E, N), public_key:der_decode(
        'RSAPublicKey',
        CSRKey#'CertificationRequestInfo_subjectPKInfo'.subjectPublicKey
    )).
