%%% @doc End-to-end examples for supplied and ACME-issued node-wallet certificates.
-module(hb_tls_examples).
-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

%% @doc Bootstrap a CSR over HTTP, then serve a supplied chain over verified H2.
certificate_file_test_() ->
    {timeout, 60, fun certificate_file/0}.
certificate_file() ->
    Hostname = atom_to_binary(localhost),
    Key = public_key:generate_key({rsa, 2048, 65537}),
    E = Key#'RSAPrivateKey'.publicExponent,
    N = binary:encode_unsigned(Key#'RSAPrivateKey'.modulus),
    D = binary:encode_unsigned(Key#'RSAPrivateKey'.privateExponent),
    Wallet = ar_wallet:new({rsa, E}, D, N),
    ServerID = hb:address(Wallet),
    Store = [hb_test_utils:test_store()],
    Opts = #{
        <<"priv-wallet">> => Wallet,
        <<"store">> => Store,
        <<"port">> => 0,
        <<"protocol">> => http2,
        <<"cache-control">> => [<<"no-cache">>, <<"no-store">>]
    },
    Path = "test/tls-certificate-" ++ binary_to_list(ServerID) ++ ".pem",
    {ok, Guard} = gen_tcp:listen(0, [{ip, {127, 0, 0, 1}}]),
    {ok, {_, GuardPort}} = inet:sockname(Guard),
    try
        URL = hb_http_server:start_node(Opts),
        {ok, CSRReply} = hb_http:get(URL, <<
            "/~tls@1.0/csr?domains%2Blist="
            "%5C%22localhost%5C%22,%20%5C%22*.localhost%5C%22"
        >>, #{}),
        PEM = hb_maps:get(<<"body">>, CSRReply),
        verify_csr(PEM, Wallet),
        ?assertMatch({error, #{ <<"status">> := 400 }},
            hb_http:get(URL, <<"/~tls@1.0/csr">>, #{})),
        % Request fields and configured defaults retain normal link semantics.
        {ok, NamesID} = hb_cache:write(
            #{ <<"1">> => <<"localhost">>, <<"2">> => <<"*.localhost">> }, Opts
        ),
        TLS = #{ <<"domains">> => {link, <<"ao:", NamesID/binary>>, #{}} },
        {ok, TLSID} = hb_cache:write(TLS, Opts),
        {ok, Linked} = hb_ao:resolve(#{ <<"device">> => <<"tls@1.0">> },
            #{
                <<"path">> => <<"csr">>,
                <<"domains">> => {link, <<"ao:", NamesID/binary>>, #{}}
            },
            Opts),
        ?assertEqual(PEM, hb_maps:get(<<"body">>, Linked)),
        {ok, Default} = hb_ao:resolve(#{ <<"device">> => <<"tls@1.0">> },
            <<"csr">>,
            Opts#{ <<"tls">> => {link, <<"ao:", TLSID/binary>>, #{}} }),
        ?assertEqual(PEM, hb_maps:get(<<"body">>, Default)),
        ok = cowboy:stop_listener(ServerID),
        % A locally issued chain exercises file loading without any ACME service.
        Certificates = public_key:pkix_test_data(#{
            root => [{digest, sha256}],
            peer => [{key, Key}, {digest, sha256}, {extensions, [
                #'Extension'{
                    extnID = ?'id-ce-subjectAltName',
                    critical = false,
                    extnValue = [{dNSName, "localhost"}, {dNSName, "*.localhost"}]
                }
            ]}]
        }),
        Leaf = proplists:get_value(cert, Certificates),
        CAs = proplists:get_value(cacerts, Certificates),
        ok = file:write_file(Path, public_key:pem_encode([
            {'Certificate', DER, not_encrypted} || DER <- [Leaf | CAs]
        ])),
        {ok, PathID} = hb_cache:write(hb_util:bin(Path), Opts),
        FileOpts = Opts#{ <<"tls">> => #{
            <<"certificate-path">> => {link, PathID, #{ <<"lazy">> => true }},
            <<"domains">> => [<<"localhost">>, <<"*.localhost">>],
            <<"acme">> => #{ <<"http-port">> => GuardPort }
        } },
        SecureURL = hb_http_server:start_node(FileOpts),
        #{scheme := <<"https">>, port := Port} = uri_string:parse(SecureURL),
        ?assertEqual(Leaf, peer_certificate(Hostname, Port)),
        HTTP2 = http2_connection(Hostname, Port, CAs),
        try http2_address(HTTP2, ServerID) after gun:close(HTTP2) end,
        ?assertEqual(undefined, hb_name:lookup({<<"tls@1.0">>, ServerID})),
        % Request the node by `localhost', the host that its certificate is for.
        HostnameURL =
            <<"https://", Hostname/binary, ":", (hb_util:bin(Port))/binary>>,
        {ok, DefaultHTTP} = hb_http:get(HostnameURL, <<"/~tls@1.0/csr">>, #{
            <<"http-client">> => gun,
            <<"protocol">> => http2,
            <<"http-client-tls-ca">> => CAs
        }),
        ?assertEqual(PEM, hb_maps:get(<<"body">>, DefaultHTTP)),
        ok = cowboy:stop_listener(ServerID),
        ?assertError({badmatch, {error, 'certificate-key-mismatch'}},
            hb_http_server:start_node(FileOpts#{
                <<"priv-wallet">> => ar_wallet:new()
            })),
        ok = file:write_file(Path, <<"Not a certificate">>),
        ?assertError({badmatch, {error, #{ <<"status">> := 500 }}},
            hb_http_server:start_node(FileOpts)),
        ok = file:delete(Path),
        ?assertError({badmatch, {error, #{ <<"status">> := 500 }}},
            hb_http_server:start_node(FileOpts)),
        ?assertEqual(undefined, hb_name:lookup({<<"tls@1.0">>, ServerID}))
    after
        catch cowboy:stop_listener(ServerID),
        stop_runtime({<<"tls@1.0">>, ServerID}),
        gen_tcp:close(Guard),
        file:delete(Path),
        hb_store:reset(Store)
    end.

%% @doc Independently check the public key, signature and requested SANs.
verify_csr(PEM, #{ <<"type">> := Type, <<"owner">> := N }) ->
    {rsa, E} = ar_tx:deserialize_sig_type(Type),
    [{'CertificationRequest', DER, not_encrypted}] = public_key:pem_decode(PEM),
    CSR = public_key:der_decode('CertificationRequest', DER),
    Info = CSR#'CertificationRequest'.certificationRequestInfo,
    PublicKey = #'RSAPublicKey'{
        publicExponent = E,
        modulus = binary:decode_unsigned(N)
    },
    {ok, EncodedInfo} = 'PKCS-10':encode('CertificationRequestInfo', Info),
    ?assert(public_key:verify(EncodedInfo, sha256,
        CSR#'CertificationRequest'.signature, PublicKey)),
    SPKI = Info#'CertificationRequestInfo'.subjectPKInfo,
    ?assertEqual(PublicKey, public_key:der_decode('RSAPublicKey',
        SPKI#'CertificationRequestInfo_subjectPKInfo'.subjectPublicKey)),
    [{_, {1, 2, 840, 113549, 1, 9, 14}, [{asn1_OPENTYPE, Extensions}]}] =
        Info#'CertificationRequestInfo'.attributes,
    [#'Extension'{extnID = ?'id-ce-subjectAltName', extnValue = SANs}] =
        public_key:der_decode('Extensions', Extensions),
    ?assertEqual([{dNSName, "localhost"}, {dNSName, "*.localhost"}],
        public_key:der_decode('GeneralNames', SANs)).

%% @doc Run the Pebble example when its environment has been configured.
pebble_test_() ->
    case os:getenv("HB_PEBBLE_DIRECTORY_URL") of
        false -> [];
        _ -> {timeout, 300, fun pebble/0}
    end.

%% @doc Issue, serve, renew, and verify a node-wallet certificate with Pebble.
pebble() ->
    DirectoryURL = os:getenv("HB_PEBBLE_DIRECTORY_URL"),
    {ok, ACMECACertificate} =
        file:read_file(os:getenv("HB_PEBBLE_CA")),
    {ok, IssuerCACertificate} =
        file:read_file(os:getenv("HB_PEBBLE_ISSUER_CA")),
    IssuerCAs = [DER || {'Certificate', DER, not_encrypted} <-
        public_key:pem_decode(IssuerCACertificate)],
    Wallet = ar_wallet:load_keyfile("test/key-1.json"),
    Domain = <<"host.docker.internal">>,
    DNSPort = case os:getenv("HB_PEBBLE_DNS_PORT") of
        false -> false;
        Value -> list_to_integer(Value)
    end,
    ACME = #{
        <<"directory-url">> => hb_util:bin(DirectoryURL),
        <<"http-port">> => 5002,
        <<"ca-certificate">> => ACMECACertificate,
        <<"terms-of-service-agreed">> => true
    },
    NodeOpts = #{
        <<"priv-wallet">> => Wallet,
        <<"port">> => 0,
        <<"protocol">> => http2,
        <<"tls">> => #{
            <<"domains">> => [Domain],
            <<"acme">> => ACME
        }
    },
    {Opts, HTTPGuard} = case DNSPort of
        false -> {NodeOpts, undefined};
        _ ->
            {ok, Guard} = gen_tcp:listen(5002, [{ip, {127, 0, 0, 1}}]),
            On = hb_opts:get(on),
            {NodeOpts#{
                <<"dns">> => #{
                    <<"port">> => DNSPort,
                    <<"address">> => <<"127.0.0.1">>
                },
                <<"on">> => On#{
                    <<"start">> => #{ <<"device">> => <<"dns@1.0">> },
                    <<"dns-resolve">> => #{ <<"device">> => <<"tls@1.0">> }
                },
                <<"tls">> => #{
                    <<"domains">> => [Domain, <<"*.", Domain/binary>>],
                    <<"acme">> => ACME#{
                        <<"challenge-type">> => <<"dns-01">>,
                        <<"dns-nameserver">> => <<"ns.", Domain/binary>>
                    }
                }
            }, Guard}
    end,
    ServerID = hb_util:human_id(ar_wallet:to_address(Wallet)),
    Committer = hb_test_utils:committer(Wallet),
    RuntimeName = {<<"tls@1.0">>, ServerID},
    ResolverOrder = inet_db:res_option(lookup),
    ok = inet_db:set_lookup([file | lists:delete(file, ResolverOrder)]),
    ok = inet_db:add_host({127, 0, 0, 1},
        [hb_util:list(Domain), "child." ++ hb_util:list(Domain)]),
    ClientOpts = #{
        <<"http-client">> => gun,
        <<"http-client-tls-ca">> => IssuerCAs,
        <<"protocol">> => http2
    },
    try
        URL = hb_http_server:start_node(Opts),
        #{port := Port} = uri_string:parse(URL),
        PublicURL = <<"https://", Domain/binary, ":",
            (integer_to_binary(Port))/binary, "/">>,
        FirstCertificate = peer_certificate(Domain, Port),
        ?assertMatch({ok, _},
            hb_tls:socket_options(Wallet, [FirstCertificate])),
        ?assertEqual({error, 'certificate-key-mismatch'},
            hb_tls:socket_options(ar_wallet:load_keyfile("test/key-2.json"),
                [FirstCertificate])),
        {ok, Info} = hb_http:get(PublicURL, <<"/~meta@1.0/info">>, ClientOpts),
        ?assertEqual([Committer], hb_message:signers(Info, ClientOpts)),
        ?assert(hb_message:verify(Info, all, ClientOpts)),
        HTTP2 = http2_connection(Domain, Port, IssuerCAs),
        http2_address(HTTP2, ServerID),
        case DNSPort of
            false ->
                ?assertMatch({error, #{ <<"status">> := 404 }}, hb_http:get(
                    <<"http://localhost:5002/">>, <<"/~meta@1.0/info">>,
                    #{ <<"protocol">> => http1 }
                ));
            _ ->
                wildcard_certificate(FirstCertificate, Domain),
                WildcardHTTP2 = http2_connection(<<"child.", Domain/binary>>,
                    Port, IssuerCAs),
                http2_address(WildcardHTTP2, ServerID),
                gun:close(WildcardHTTP2),
                dns_clean(DNSPort, Domain)
        end,
        {ok, EstablishedSocket} = ssl:connect(
            hb_util:list(Domain),
            Port,
            [
                {verify, verify_peer},
                {cacerts, IssuerCAs},
                {active, false},
                {mode, binary},
                {alpn_advertised_protocols, [<<"http/1.1">>]}
            ],
            5000
        ),
        ?assertEqual({ok, <<"http/1.1">>},
            ssl:negotiated_protocol(EstablishedSocket)),
        RuntimePID = hb_name:lookup(RuntimeName),
        RuntimePID ! renew,
        ?assert(hb_util:wait_until(fun() ->
            try peer_certificate(Domain, Port) =/= FirstCertificate
            catch _:_ -> false
            end
        end, 180000)),
        ?assertMatch({ok, _}, hb_tls:socket_options(Wallet,
            [peer_certificate(Domain, Port)])),
        case DNSPort of
            false -> ok;
            _ ->
                wildcard_certificate(peer_certificate(Domain, Port), Domain),
                dns_clean(DNSPort, Domain)
        end,
        http2_address(HTTP2, ServerID),
        gun:close(HTTP2),
        RenewedHTTP2 = http2_connection(Domain, Port, IssuerCAs),
        http2_address(RenewedHTTP2, ServerID),
        gun:close(RenewedHTTP2),
        ok = ssl:send(EstablishedSocket, <<
            "GET /~meta@1.0/info/address HTTP/1.1\r\n",
            "Host: host.docker.internal\r\n",
            "Connection: close\r\n\r\n"
        >>),
        ?assertNotEqual(nomatch, binary:match(
            recv_ssl_response(EstablishedSocket, <<>>), <<" 200 ">>
        )),
        case DNSPort of
            false -> ok;
            _ -> dns_failure(DNSPort, Domain, Port, ServerID)
        end
    after
        stop_runtime(RuntimeName),
        stop_dns(DNSPort),
        case HTTPGuard of undefined -> ok; _ -> gen_tcp:close(HTTPGuard) end,
        inet_db:del_host({127, 0, 0, 1}),
        inet_db:set_lookup(ResolverOrder),
        catch cowboy:stop_listener(ServerID),
        catch cowboy:stop_listener({tls_http_01, ServerID})
    end.

%% @doc An apex and wildcard must both survive issuance and renewal.
wildcard_certificate(DER, Domain) ->
    Certificate = public_key:pkix_decode_cert(DER, otp),
    TBS = Certificate#'OTPCertificate'.tbsCertificate,
    #'Extension'{extnValue = Names} = lists:keyfind(
        ?'id-ce-subjectAltName', #'Extension'.extnID, TBS#'OTPTBSCertificate'.extensions
    ),
    ?assertEqual(lists:sort([hb_util:list(Domain), "*." ++ hb_util:list(Domain)]),
        lists:sort([Name || {dNSName, Name} <- Names])).

%% @doc Both transports return uncached NODATA after challenge cleanup.
dns_clean(Port, Domain) ->
    lists:foreach(fun(TCP) ->
        {ok, Reply} = inet_res:resolve("_acme-challenge." ++ hb_util:list(Domain),
            in, txt, [
                {nameservers, [{{127, 0, 0, 1}, Port}]},
                {usevc, TCP}, {retry, 1}, {timeout, 1000}
            ]),
        ?assert(inet_dns:header(inet_dns:msg(Reply, header), aa)),
        ?assertEqual([], inet_dns:msg(Reply, anlist)),
        [SOA] = inet_dns:msg(Reply, nslist),
        ?assertEqual(soa, inet_dns:rr(SOA, type)),
        ?assertEqual(0, inet_dns:rr(SOA, ttl))
    end, [false, true]).

%% @doc Failed validation clears TXT values and leaves the served certificate intact.
dns_failure(DNSPort, Domain, TLSPort, ServerID) ->
    Certificate = peer_certificate(Domain, TLSPort),
    NodeOpts = hb_http_server:get_opts(#{ <<"http-server">> => ServerID }),
    Priv = #{ <<"tls">> => #{
        <<"server-id">> => ServerID,
        <<"lifecycle-capability">> => make_ref()
    } },
    Opts = hb_private:set(NodeOpts#{
        <<"cache-control">> => [<<"no-cache">>, <<"no-store">>]
    }, Priv, NodeOpts),
    Request = hb_private:set(#{ <<"path">> => <<"obtain">> }, Priv, Opts),
    stop_runtime({<<"tls@1.0">>, ServerID}),
    stop_dns(DNSPort),
    {ok, _} = hb_ao:resolve(#{ <<"device">> => <<"dns@1.0">> }, <<"start">>,
        Opts#{ <<"on">> => #{} }),
    ?assertMatch({error, #{ <<"status">> := 500 }},
        hb_ao:resolve(#{ <<"device">> => <<"tls@1.0">> }, Request, Opts)),
    {ok, Reply} = hb_ao:resolve(#{ <<"device">> => <<"tls@1.0">> }, #{
        <<"path">> => <<"dns-resolve">>,
        <<"name">> => <<"_acme-challenge.", Domain/binary>>,
        <<"type">> => <<"txt">>
    }, Opts),
    ?assertEqual([], hb_maps:get(<<"answers">>, Reply)),
    ?assertEqual(Certificate, peer_certificate(Domain, TLSPort)),
    stop_dns(DNSPort),
    {ok, _} = hb_ao:resolve(#{ <<"device">> => <<"dns@1.0">> }, <<"start">>, Opts),
    {ok, Recovered} = hb_ao:resolve(
        #{ <<"device">> => <<"tls@1.0">> }, Request, Opts
    ),
    ?assertMatch({ok, _}, hb_tls:socket_options(
        hb_opts:get(priv_wallet, no_viable_wallet, Opts),
        hb_maps:get(<<"certificate-chain">>, Recovered)
    )),
    dns_clean(DNSPort, Domain).

%% @doc Release only the example's listener at its explicitly configured port.
stop_dns(false) -> ok;
stop_dns(Port) ->
    lists:foreach(fun(PID) ->
        Ref = monitor(process, PID),
        PID ! stop,
        receive {'DOWN', Ref, process, PID, _} -> ok end
    end, [PID || {{_Module, {127, 0, 0, 1}, BoundPort}, PID} <- hb_name:all(),
        BoundPort =:= Port]).

%% @doc Negotiate HTTP/2 over a CA-verified TLS connection.
http2_connection(Domain, Port, CAs) ->
    {ok, PID} = gun:open(hb_util:list(Domain), Port, #{
        transport => tls,
        protocols => [http2],
        retry => 0,
        tls_opts => [{verify, verify_peer}, {cacerts, CAs}]
    }),
    ?assertEqual({ok, http2}, gun:await_up(PID, 5000)),
    PID.

%% @doc Resolve the node address through an established HTTP/2 connection.
http2_address(PID, Address) ->
    Ref = gun:get(PID, <<"/~meta@1.0/info/address">>),
    ?assertMatch({response, nofin, 200, _}, gun:await(PID, Ref, 5000)),
    ?assertEqual({ok, Address}, gun:await_body(PID, Ref, 5000)).

%% @doc Stop the singleton runtime if the example started it.
stop_runtime(Name) ->
    case hb_name:lookup(Name) of
        PID when is_pid(PID) ->
            PID ! {stop, self()},
            receive {stopped, PID} -> ok end;
        undefined -> ok
    end.

%% @doc Read an HTTP response until the server closes its TLS connection.
recv_ssl_response(Socket, Acc) ->
    case ssl:recv(Socket, 0, 5000) of
        {ok, Data} -> recv_ssl_response(Socket, <<Acc/binary, Data/binary>>);
        {error, closed} -> Acc
    end.

%% @doc Return the leaf certificate currently served by the node.
peer_certificate(Domain, Port) ->
    {ok, Socket} = ssl:connect(
        hb_util:list(Domain),
        Port,
        [{verify, verify_none}, {active, false}],
        5000
    ),
    {ok, Certificate} = ssl:peercert(Socket),
    ok = ssl:close(Socket),
    Certificate.
