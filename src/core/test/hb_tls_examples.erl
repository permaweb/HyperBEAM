%%% @doc End-to-end examples for node-wallet TLS against a real ACME server.
-module(hb_tls_examples).
-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

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
    URL = hb_http_server:start_node(#{
        <<"priv-wallet">> => Wallet,
        <<"port">> => 0,
        <<"protocol">> => http2,
        <<"tls">> => #{
            <<"domains">> => [Domain],
            <<"acme">> => #{
                <<"directory-url">> => hb_util:bin(DirectoryURL),
                <<"http-port">> => 5002,
                <<"ca-certificate">> => ACMECACertificate,
                <<"terms-of-service-agreed">> => true
            }
        }
    }),
    #{port := Port} = uri_string:parse(URL),
    ServerID = hb_util:human_id(ar_wallet:to_address(Wallet)),
    RuntimeName = {<<"tls@1.0">>, ServerID},
    PublicURL = <<"https://", Domain/binary, ":",
        (integer_to_binary(Port))/binary, "/">>,
    ResolverOrder = inet_db:res_option(lookup),
    ok = inet_db:set_lookup([file | lists:delete(file, ResolverOrder)]),
    ok = inet_db:add_host({127, 0, 0, 1}, [hb_util:list(Domain)]),
    ClientOpts = #{
        <<"http-client">> => gun,
        <<"http-client-tls-ca">> => IssuerCAs,
        <<"protocol">> => http2
    },
    try
        FirstCertificate = peer_certificate(Domain, Port),
        ?assertMatch({ok, _},
            hb_tls:socket_options(Wallet, [FirstCertificate])),
        ?assertEqual({error, 'certificate-key-mismatch'},
            hb_tls:socket_options(ar_wallet:load_keyfile("test/key-2.json"),
                [FirstCertificate])),
        {ok, _} = hb_http:get(PublicURL, <<"/~meta@1.0/info">>, ClientOpts),
        ?assertMatch({error, #{ <<"status">> := 404 }}, hb_http:get(
            <<"http://localhost:5002/">>, <<"/~meta@1.0/info">>,
            #{ <<"protocol">> => http1 }
        )),
        {ok, EstablishedSocket} = ssl:connect(
            hb_util:list(Domain),
            Port,
            [
                {verify, verify_none},
                {active, false},
                {mode, binary},
                {alpn_advertised_protocols, [<<"http/1.1">>]}
            ],
            5000
        ),
        RuntimePID = hb_name:lookup(RuntimeName),
        RuntimePID ! renew,
        ?assert(hb_util:wait_until(fun() ->
            try peer_certificate(Domain, Port) =/= FirstCertificate
            catch _:_ -> false
            end
        end, 180000)),
        ?assertMatch({ok, _}, hb_tls:socket_options(Wallet,
            [peer_certificate(Domain, Port)])),
        ok = ssl:send(EstablishedSocket, <<
            "GET /~meta@1.0/info/address HTTP/1.1\r\n",
            "Host: host.docker.internal\r\n",
            "Connection: close\r\n\r\n"
        >>),
        ?assertNotEqual(nomatch, binary:match(
            recv_ssl_response(EstablishedSocket, <<>>), <<" 200 ">>
        ))
    after
        stop_runtime(RuntimeName),
        inet_db:del_host({127, 0, 0, 1}),
        inet_db:set_lookup(ResolverOrder),
        catch cowboy:stop_listener(ServerID),
        catch cowboy:stop_listener({tls_http_01, ServerID})
    end.

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

%% @doc The wallet-key self-signed fallback: accepted by socket_options (its
%% SPKI is the wallet key), refused for any other wallet, and carrying the
%% structural binding (sha256 of the served modulus is the wallet address).
self_signed_chain_test() ->
    Wallet = ar_wallet:load_keyfile("test/key-1.json"),
    Chain = [Leaf] = hb_tls:self_signed_chain(Wallet, [<<"fallback.example">>]),
    ?assertMatch({ok, _}, hb_tls:socket_options(Wallet, Chain)),
    ?assertEqual({error, 'certificate-key-mismatch'}, hb_tls:socket_options(
        ar_wallet:load_keyfile("test/key-2.json"), Chain)),
    ?assert(hb_tls:certificate_expiry(Chain) > erlang:system_time(millisecond)),
    Certificate = public_key:pkix_decode_cert(Leaf, otp),
    SPKI = (Certificate#'OTPCertificate'.tbsCertificate)
        #'OTPTBSCertificate'.subjectPublicKeyInfo,
    #'RSAPublicKey'{modulus = Modulus} =
        SPKI#'OTPSubjectPublicKeyInfo'.subjectPublicKey,
    ?assertEqual(ar_wallet:to_address(Wallet),
        crypto:hash(sha256, binary:encode_unsigned(Modulus))).

%% @doc A node with an unreachable ACME directory still boots, serving the
%% wallet-key fallback over TLS to an http/1.1 client that sends capitalized
%% header names.
fallback_boot_test_() ->
    {timeout, 60, fun fallback_boot/0}.

fallback_boot() ->
    Wallet = ar_wallet:load_keyfile("test/key-1.json"),
    URL = hb_http_server:start_node(#{
        <<"priv-wallet">> => Wallet,
        <<"port">> => 0,
        <<"protocol">> => http2,
        <<"tls">> => #{
            <<"domains">> => [<<"localhost">>],
            <<"acme">> => #{
                <<"directory-url">> => <<"https://localhost:1/dir">>,
                <<"http-port">> => 0,
                <<"terms-of-service-agreed">> => true
            }
        }
    }),
    #{port := Port} = uri_string:parse(URL),
    ServerID = hb_util:human_id(ar_wallet:to_address(Wallet)),
    try
        Leaf = peer_certificate(<<"localhost">>, Port),
        ?assertMatch({ok, _}, hb_tls:socket_options(Wallet, [Leaf])),
        {ok, Socket} = ssl:connect("localhost", Port,
            [{verify, verify_none}, {active, false}, {mode, binary},
             {alpn_advertised_protocols, [<<"http/1.1">>]}], 5000),
        ok = ssl:send(Socket, <<
            "GET /~meta@1.0/info/address HTTP/1.1\r\n",
            "HOST: localhost\r\n",
            "X-Legacy-Client: TRUE\r\n",
            "Connection: close\r\n\r\n"
        >>),
        Response = recv_ssl_response(Socket, <<>>),
        ?assertNotEqual(nomatch, binary:match(Response, <<" 200 ">>)),
        ?assertNotEqual(nomatch, binary:match(
            Response, hb_util:human_id(ar_wallet:to_address(Wallet))))
    after
        stop_runtime({<<"tls@1.0">>, ServerID}),
        catch cowboy:stop_listener(ServerID),
        catch cowboy:stop_listener({tls_http_01, ServerID})
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
