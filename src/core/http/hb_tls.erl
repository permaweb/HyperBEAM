%%% @doc TLS listener options for HyperBEAM's HTTP server. Builds the ssl socket
%%% options from the node message `tls' block and answers per-handshake
%%% certificate lookups via an `sni_fun'. Certificates are held per-host in
%%% `persistent_term', so a node can serve multiple domains (SNI) and rotate a
%%% certificate live (by refreshing the table) without restarting the listener.
-module(hb_tls).
-export([socket_opts/1, refresh/1, install/3, sni_lookup/1, expiry/1]).
-export([self_signed/1, fingerprints/0]).
-include_lib("eunit/include/eunit.hrl").

-include_lib("public_key/include/public_key.hrl").

-define(CERTS, {?MODULE, certs}).

%% @doc Build the ssl listener options from the `tls' block of the node message,
%% or `no_tls' when no `tls' block is present (the caller then starts a plain
%% listener, so existing nodes are unaffected).
socket_opts(NodeMsg) ->
    case maps:get(<<"tls">>, NodeMsg, not_found) of
        not_found ->
            no_tls;
        Tls when is_map(Tls) ->
            ok = refresh(NodeMsg),
            [
                {sni_fun, fun ?MODULE:sni_lookup/1},
                {versions, versions(Tls)},
                %% HTTP/2 only: advertise just h2 over ALPN. Clients that offer
                %% only http/1.1 fail the handshake. Clients that send no ALPN
                %% are caught by the sub-h2 guard in hb_http_server.
                {alpn_preferred_protocols, [<<"h2">>]},
                {honor_cipher_order, true}
            ]
    end.

%% @doc (Re)build the per-host certificate table and store it in
%% `persistent_term'. Called when the listener is created and, for live
%% rotation, whenever the node `tls' configuration changes.
refresh(NodeMsg) ->
    Tls = maps:get(<<"tls">>, NodeMsg, #{}),
    Table = build_table(maps:get(<<"certs">>, Tls, [])),
    Acme = maps:get(<<"acme">>, Tls, #{}),
    AcmeDomains = maps:get(<<"domains">>, Acme, []),
    persistent_term:put(?CERTS, bootstrap(Table, AcmeDomains)),
    ok.

%% For every ACME domain with no configured cert, add a short-lived self-signed
%% bootstrap entry so the :443 listener can come up with no cert on disk; the
%% ACME cron then replaces it live. The first bootstrap becomes the default when
%% none was configured, so an unknown-SNI handshake still completes.
bootstrap(Table, Domains) ->
    lists:foldl(
        fun(Domain, Acc) ->
            case maps:is_key(Domain, Acc) of
                true -> Acc;
                false ->
                    {CertPem, KeyPem} = self_signed(Domain),
                    Opts = entry_opts(#{<<"cert">> => CertPem, <<"key">> => KeyPem}),
                    Acc1 = Acc#{Domain => Opts},
                    case maps:is_key(default, Acc1) of
                        true -> Acc1;
                        false -> Acc1#{default => Opts}
                    end
            end
        end,
        Table,
        Domains
    ).

%% @doc Live-install a renewed certificate for a single host: rebuilds that
%% host's entry in the in-memory table so the next TLS handshake serves the new
%% cert, with no restart and no disk write. In-flight connections are
%% unaffected. This is the seam an ACME renewal calls after fetching a cert.
install(Domain, CertPem, KeyPem) ->
    NewOpts = entry_opts(#{ <<"cert">> => CertPem, <<"key">> => KeyPem }),
    Table = persistent_term:get(?CERTS, #{}),
    OldOpts = maps:get(Domain, Table, undefined),
    Table1 = Table#{ Domain => NewOpts },
    %% If this host was also serving as the default entry, rotate that too.
    Table2 =
        case maps:get(default, Table, undefined) of
            OldOpts when OldOpts =/= undefined -> Table1#{ default => NewOpts };
            _ -> Table1
        end,
    persistent_term:put(?CERTS, Table2),
    ok.

%% @doc The ssl `sni_fun' callback. Returns the certificate options for the
%% requested server name, falling back to a `default' entry. An unknown name
%% with no default returns `[]', which fails the handshake closed.
sni_lookup(ServerName) ->
    Table = persistent_term:get(?CERTS, #{}),
    maps:get(list_to_binary(ServerName), Table, maps:get(default, Table, [])).

%% @doc Days until the certificate currently installed for `Domain' expires, or
%% `undefined' when no certificate is found. Reads the leaf cert from the
%% per-host table (inline DER or `certfile'), decodes its notAfter validity and
%% returns `(NotAfter - now) div 86400'. This is the seam an ACME renewal scans
%% to decide whether a domain is due.
expiry(Domain) ->
    Table = persistent_term:get(?CERTS, #{}),
    Opts = maps:get(Domain, Table, maps:get(default, Table, undefined)),
    case leaf_der(Opts) of
        undefined -> undefined;
        Der ->
            #'OTPCertificate'{tbsCertificate = TBS} =
                public_key:pkix_decode_cert(Der, otp),
            #'OTPTBSCertificate'{validity = #'Validity'{notAfter = NotAfter}} = TBS,
            (asn1_time_to_unix(NotAfter) - os:system_time(second)) div 86400
    end.

%% @doc The SPKI (public-key) fingerprint of each serving cert, keyed by host:
%% `base64(sha256(SubjectPublicKeyInfo))'. Computed LIVE from the current cert
%% table, so it reflects the self-signed bootstrap and any ACME-renewed cert.
%% This matches what a client computes with
%% `openssl x509 -pubkey | openssl pkey -pubin -outform der | dgst -sha256',
%% so a client can pin the served cert against the node's signed info. The
%% private key is never exposed.
fingerprints() ->
    Table = persistent_term:get(?CERTS, #{}),
    maps:fold(
        fun(default, _Opts, Acc) -> Acc;
           (Domain, Opts, Acc) when is_binary(Domain) ->
                case leaf_der(Opts) of
                    undefined -> Acc;
                    Der -> Acc#{ Domain => spki_fingerprint(Der) }
                end
        end,
        #{},
        Table
    ).

%% base64(sha256(DER of the cert's SubjectPublicKeyInfo)). `plain' decoding
%% keeps the SPKI in its re-encodable ASN.1 form so der_encode reproduces the
%% exact bytes a standard client hashes.
spki_fingerprint(Der) ->
    Cert = public_key:pkix_decode_cert(Der, plain),
    SPKI = (Cert#'Certificate'.tbsCertificate)#'TBSCertificate'.subjectPublicKeyInfo,
    base64:encode(crypto:hash(sha256, public_key:der_encode('SubjectPublicKeyInfo', SPKI))).

%% The leaf DER for a host entry: inline `{cert, [Leaf|_]}', or the first
%% 'Certificate' read from a `{certfile, Path}'. `undefined' for no entry.
leaf_der(undefined) -> undefined;
leaf_der(Opts) ->
    case proplists:get_value(cert, Opts) of
        [Der | _] -> Der;
        Der when is_binary(Der) -> Der;
        undefined ->
            case proplists:get_value(certfile, Opts) of
                undefined -> undefined;
                File ->
                    {ok, Pem} = file:read_file(File),
                    case [D || {'Certificate', D, _} <- public_key:pem_decode(Pem)] of
                        [Leaf | _] -> Leaf;
                        [] -> undefined
                    end
            end
    end.

%% ASN.1 cert time to unix seconds. utcTime is a 2-digit year (RFC 5280:
%% >= 50 is 19YY, else 20YY); generalTime is a 4-digit year. Subtracting the
%% gregorian seconds of 1970-01-01 (62167219200) converts to a unix timestamp.
asn1_time_to_unix({utcTime, Time}) ->
    [Y1, Y2 | Rest] = Time,
    YY = list_to_integer([Y1, Y2]),
    Year = if YY >= 50 -> 1900 + YY; true -> 2000 + YY end,
    gregorian_seconds(Year, Rest) - 62167219200;
asn1_time_to_unix({generalTime, Time}) ->
    {Year, Rest} = lists:split(4, Time),
    gregorian_seconds(list_to_integer(Year), Rest) - 62167219200.

%% Rest is "MMDDHHMMSSZ" for both time forms.
gregorian_seconds(Year, [M1, M2, D1, D2, H1, H2, Mi1, Mi2, S1, S2 | _]) ->
    DateTime =
        {{Year, list_to_integer([M1, M2]), list_to_integer([D1, D2])},
         {list_to_integer([H1, H2]), list_to_integer([Mi1, Mi2]),
          list_to_integer([S1, S2])}},
    calendar:datetime_to_gregorian_seconds(DateTime).

%% Build #{ Host => CertKeyOpts } from the configured entries. A single-cert
%% node serves its certificate for any SNI; a multi-cert node serves the
%% per-host certificate and fails closed for an unmatched name unless an entry
%% sets `default => true'.
build_table(Certs) ->
    Entries = [{Entry, entry_opts(Entry)} || Entry <- Certs],
    Table = lists:foldl(fun add_entry/2, #{}, Entries),
    case {Entries, maps:is_key(default, Table)} of
        {[{_, Opts}], false} -> Table#{ default => Opts };
        _ -> Table
    end.

add_entry({Entry, Opts}, Acc) ->
    WithHosts =
        lists:foldl(
            fun(Domain, Map) -> Map#{ Domain => Opts } end,
            Acc,
            maps:get(<<"domains">>, Entry, [])
        ),
    case maps:get(<<"default">>, Entry, false) of
        true -> WithHosts#{ default => Opts };
        _ -> WithHosts
    end.

%% Inline PEM is parsed to in-memory DER, so the private key is never written to
%% disk. File paths are passed through to ssl as `certfile'/`keyfile'.
entry_opts(#{ <<"cert">> := CertPem, <<"key">> := KeyPem }) ->
    CertDERs = [Der || {'Certificate', Der, _} <- public_key:pem_decode(CertPem)],
    [{KeyTag, KeyDer, _} | _] = public_key:pem_decode(KeyPem),
    [{cert, CertDERs}, {key, {KeyTag, KeyDer}}];
entry_opts(#{ <<"certfile">> := CertFile, <<"keyfile">> := KeyFile }) ->
    [{certfile, binary_to_list(CertFile)}, {keyfile, binary_to_list(KeyFile)}].

%% @doc Generate a short-lived self-signed certificate for `Domain', returning
%% {CertPem, KeyPem}. The RSA key is generated in memory and never written to
%% disk. Used as a bootstrap so a node with an `acme' block but no configured
%% cert can bring up its TLS listener; the ACME cron then replaces it live.
%% OTP 28 ASN.1 is picky: the algorithm parameters must be an explicit ASN.1
%% NULL as {asn1_OPENTYPE, <<5,0>>}, and the SAN extnValue must be the decoded
%% [{dNSName, _}] list (pkix_sign re-encodes it), not pre-encoded DER.
self_signed(Domain) ->
    Key = public_key:generate_key({rsa, 2048, 65537}),
    #'RSAPrivateKey'{modulus = N, publicExponent = E} = Key,
    Now = os:system_time(second),
    Subject = subject(Domain),
    SigAlg = #'SignatureAlgorithm'{
        algorithm = ?'sha256WithRSAEncryption',
        parameters = {asn1_OPENTYPE, <<5, 0>>}
    },
    TBS = #'OTPTBSCertificate'{
        version = v3,
        serialNumber = erlang:unique_integer([positive]),
        signature = SigAlg,
        issuer = Subject,
        validity = #'Validity'{
            notBefore = unix_to_utc(Now - 300),
            notAfter = unix_to_utc(Now + 7 * 86400)
        },
        subject = Subject,
        subjectPublicKeyInfo = #'OTPSubjectPublicKeyInfo'{
            algorithm = #'PublicKeyAlgorithm'{
                algorithm = ?'rsaEncryption',
                parameters = {asn1_OPENTYPE, <<5, 0>>}
            },
            subjectPublicKey = #'RSAPublicKey'{modulus = N, publicExponent = E}
        },
        extensions = [#'Extension'{
            extnID = ?'id-ce-subjectAltName',
            critical = false,
            extnValue = [{dNSName, binary_to_list(Domain)}]
        }]
    },
    Der = public_key:pkix_sign(TBS, Key),
    CertPem = public_key:pem_encode([{'Certificate', Der, not_encrypted}]),
    KeyPem = public_key:pem_encode([public_key:pem_entry_encode('RSAPrivateKey', Key)]),
    {CertPem, KeyPem}.

subject(CommonName) ->
    {rdnSequence, [[#'AttributeTypeAndValue'{
        type = ?'id-at-commonName',
        value = {utf8String, CommonName}
    }]]}.

%% Unix seconds to an ASN.1 utcTime "YYMMDDHHMMSSZ" (RFC 5280 validity is < 2050,
%% so utcTime is correct here and matches asn1_time_to_unix/1 above).
unix_to_utc(Unix) ->
    {{Y, Mo, D}, {H, Mi, S}} =
        calendar:gregorian_seconds_to_datetime(Unix + 62167219200),
    Str = lists:flatten(io_lib:format(
        "~2..0w~2..0w~2..0w~2..0w~2..0w~2..0wZ", [Y rem 100, Mo, D, H, Mi, S])),
    {utcTime, Str}.

%% TLS version floor: 1.2 and 1.3 by default, or 1.3 only when requested.
versions(Tls) ->
    case maps:get(<<"min-version">>, Tls, <<"tlsv1.2">>) of
        <<"tlsv1.3">> -> ['tlsv1.3'];
        _ -> ['tlsv1.2', 'tlsv1.3']
    end.

%%% Tests

%% @doc Boot a node with an inline-PEM TLS config and confirm it terminates TLS
%% as HTTP/2 (serving a real request through the pipeline) and refuses HTTP/1.1.
tls_node_test() ->
    {ok, CertPem} = file:read_file("test/test-tls.pem"),
    {ok, KeyPem} = file:read_file("test/test-tls.key"),
    URL =
        hb_http_server:start_node(#{
            <<"tls">> => #{
                <<"certs">> => [#{
                    <<"domains">> => [<<"localhost">>],
                    <<"cert">> => CertPem,
                    <<"key">> => KeyPem
                }]
            }
        }),
    Port = url_port(URL),
    %% An HTTP/2 client is served through the normal pipeline.
    {ok, Conn} =
        gun:open("localhost", Port,
            #{transport => tls,
              tls_opts => [{verify, verify_none}],
              protocols => [http2]}),
    {ok, http2} = gun:await_up(Conn, 5000),
    StreamRef = gun:get(Conn, "/~meta@1.0/info"),
    {response, _, Status, _} = gun:await(Conn, StreamRef, 5000),
    gun:close(Conn),
    ?assertEqual(200, Status),
    %% An http/1.1-only client is refused at the TLS layer (ALPN mismatch).
    ?assertMatch({error, _},
        ssl:connect("127.0.0.1", Port,
            [{verify, verify_none},
             {server_name_indication, "localhost"},
             {alpn_advertised_protocols, [<<"http/1.1">>]}], 2000)).

url_port(URL) ->
    [_, AfterHost] = binary:split(URL, <<"localhost:">>),
    [PortBin | _] = binary:split(AfterHost, <<"/">>),
    binary_to_integer(PortBin).

%% @doc Drive the real hb_http_client (gun backend, protocol http2) against a
%% real HyperBEAM TLS node (h2) and a plain http/1.1-only TLS server. Both must
%% return 200: the h2-only node proves h2 was negotiated, the http/1.1-only
%% server proves the [http2, http] fallback.
real_client_test() ->
    {ok, CertPem} = file:read_file("test/test-tls.pem"),
    {ok, KeyPem} = file:read_file("test/test-tls.key"),
    H2Url =
        hb_http_server:start_node(#{
            <<"tls">> => #{
                <<"certs">> => [#{
                    <<"domains">> => [<<"localhost">>],
                    <<"cert">> => CertPem,
                    <<"key">> => KeyPem
                }]
            }
        }),
    H2Port = url_port(H2Url),
    Dispatch =
        cowboy_router:compile(
            [{'_', [{"/", cowboy_static, {file, "test/test-tls.pem"}}]}]),
    {ok, _} =
        cowboy:start_tls(hb_tls_h1_peer,
            #{socket_opts =>
                [{port, 0},
                 {certfile, "test/test-tls.pem"},
                 {keyfile, "test/test-tls.key"},
                 {alpn_preferred_protocols, [<<"http/1.1">>]}]},
            #{env => #{dispatch => Dispatch}}),
    H1Port = ranch:get_port(hb_tls_h1_peer),
    Opts = #{http_client => gun, protocol => http2, http_retry => 0,
             http_client_tls_opts => [{verify, verify_none}]},
    {ok, H2Status, _, _} =
        hb_http_client:request(
            #{peer => peer(H2Port), path => <<"/~meta@1.0/info">>,
              method => <<"GET">>, headers => #{}, body => <<>>}, Opts),
    {ok, H1Status, _, _} =
        hb_http_client:request(
            #{peer => peer(H1Port), path => <<"/">>,
              method => <<"GET">>, headers => #{}, body => <<>>}, Opts),
    cowboy:stop_listener(hb_tls_h1_peer),
    ?debugFmt(
        "real hb_http_client (gun, protocol=http2): h2-only HB node -> ~p "
        "(h2 negotiated), http/1.1-only peer -> ~p (fell back to http/1.1)",
        [H2Status, H1Status]),
    ?assertEqual(200, H2Status),
    ?assertEqual(200, H1Status).

peer(Port) ->
    list_to_binary("https://localhost:" ++ integer_to_list(Port)).

%% @doc Prove a renewed certificate is installed live: the running listener
%% serves a different certificate after hb_tls:install/3, with no restart.
live_rotation_test() ->
    {ok, Cert1} = file:read_file("test/test-tls.pem"),
    {ok, Key1} = file:read_file("test/test-tls.key"),
    URL =
        hb_http_server:start_node(#{
            <<"tls">> => #{
                <<"certs">> => [#{
                    <<"domains">> => [<<"localhost">>],
                    <<"cert">> => Cert1,
                    <<"key">> => Key1
                }]
            }
        }),
    Port = url_port(URL),
    Served1 = served_cert(Port),
    {ok, Cert2} = file:read_file("test/test-tls-alt.pem"),
    {ok, Key2} = file:read_file("test/test-tls-alt.key"),
    ok = hb_tls:install(<<"localhost">>, Cert2, Key2),
    Served2 = served_cert(Port),
    ?debugFmt(
        "live cert rotation, no restart: served DER changed (~p -> ~p bytes)",
        [byte_size(Served1), byte_size(Served2)]),
    ?assertNotEqual(Served1, Served2).

served_cert(Port) ->
    {ok, S} =
        ssl:connect("127.0.0.1", Port,
            [{verify, verify_none},
             {server_name_indication, "localhost"},
             {alpn_advertised_protocols, [<<"h2">>]}], 4000),
    {ok, Der} = ssl:peercert(S),
    ssl:close(S),
    Der.

%% @doc expiry/1 reads the installed leaf cert's notAfter and returns the days
%% remaining. The alt test cert is valid years out, so a positive integer; an
%% unknown domain with no default returns undefined.
expiry_test() ->
    {ok, CertPem} = file:read_file("test/test-tls-alt.pem"),
    {ok, KeyPem} = file:read_file("test/test-tls-alt.key"),
    persistent_term:put(?CERTS, #{}),
    ok = install(<<"expiry-test.localhost">>, CertPem, KeyPem),
    Days = expiry(<<"expiry-test.localhost">>),
    ?debugFmt("installed cert expires in ~p days", [Days]),
    ?assert(is_integer(Days)),
    ?assert(Days > 0),
    ?assertEqual(undefined, expiry(<<"no-such-domain.localhost">>)).

%% @doc self_signed/1 yields a decodable cert + key PEM pair, and once installed
%% expiry/1 reports the ~7-day validity of the bootstrap cert.
self_signed_test() ->
    {CertPem, KeyPem} = self_signed(<<"boot.example">>),
    ?assertMatch([{'Certificate', _, _} | _], public_key:pem_decode(CertPem)),
    ?assertMatch([{'RSAPrivateKey', _, _} | _], public_key:pem_decode(KeyPem)),
    persistent_term:put(?CERTS, #{}),
    ok = install(<<"boot.example">>, CertPem, KeyPem),
    Days = expiry(<<"boot.example">>),
    ?debugFmt("self-signed bootstrap cert expires in ~p days", [Days]),
    ?assert(is_integer(Days)),
    ?assert(Days > 0),
    ?assert(Days =< 7).

%% @doc fingerprints/0 reads the live cert table and yields, for each host, the
%% base64 SPKI pin. It must equal the pin a real client computes with the
%% standard openssl pipeline, proving the published binding is client-verifiable.
fingerprints_test() ->
    {ok, CertPem} = file:read_file("test/test-tls.pem"),
    {ok, KeyPem} = file:read_file("test/test-tls.key"),
    persistent_term:put(?CERTS, #{}),
    _ = socket_opts(#{
        <<"tls">> => #{
            <<"certs">> => [#{
                <<"domains">> => [<<"localhost">>],
                <<"cert">> => CertPem,
                <<"key">> => KeyPem
            }]
        }
    }),
    Fps = fingerprints(),
    Fp = maps:get(<<"localhost">>, Fps),
    ?assert(is_binary(Fp)),
    OpenSsl = list_to_binary(string:trim(os:cmd(
        "openssl x509 -in test/test-tls.pem -pubkey -noout"
        " | openssl pkey -pubin -outform der"
        " | openssl dgst -sha256 -binary | openssl base64"))),
    ?debugFmt("SPKI pin: fingerprints()=~s openssl=~s", [Fp, OpenSsl]),
    ?assertEqual(OpenSsl, Fp).
