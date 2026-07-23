%%% @doc HTTP-01 challenge listener for hb_acme: a minimal plaintext listener
%%% (port 80 in production) that answers ACME HTTP-01 challenges from an
%%% in-memory token store and 301-redirects everything else to https. It holds
%%% no private key and runs no application logic. The hb_acme `publish' and
%%% `unpublish' callbacks write to the token store this listener serves from.
-module(hb_acme_http).
-export([start/2, stop/0, publish/2, unpublish/1, challenge_response/1]).
-export([init/2]).

-define(CHALLENGES, {?MODULE, challenges}).
-define(LISTENER, hb_acme_http01).

%% @doc Start the challenge/redirect listener on Port (80 in production).
start(Port, _Opts) ->
    Dispatch = cowboy_router:compile([{'_', [{'_', ?MODULE, []}]}]),
    cowboy:start_clear(?LISTENER,
        #{socket_opts => [{port, Port}]},
        #{env => #{dispatch => Dispatch}}).

stop() ->
    cowboy:stop_listener(?LISTENER).

%% @doc Publish a key authorization for a token (the hb_acme publish callback).
publish(Token, KeyAuth) ->
    Map = persistent_term:get(?CHALLENGES, #{}),
    persistent_term:put(?CHALLENGES, Map#{Token => KeyAuth}),
    ok.

unpublish(Token) ->
    Map = persistent_term:get(?CHALLENGES, #{}),
    persistent_term:put(?CHALLENGES, maps:remove(Token, Map)),
    ok.

challenge_response(Token) ->
    maps:get(Token, persistent_term:get(?CHALLENGES, #{}), undefined).

%% cowboy handler: serve a published challenge verbatim, else 301 to https.
init(Req, State) ->
    Reply =
        case cowboy_req:path(Req) of
            <<"/.well-known/acme-challenge/", Token/binary>> ->
                case challenge_response(Token) of
                    undefined ->
                        cowboy_req:reply(404, #{}, <<>>, Req);
                    KeyAuth ->
                        cowboy_req:reply(200,
                            #{<<"content-type">> => <<"application/octet-stream">>},
                            KeyAuth, Req)
                end;
            Path ->
                Host = cowboy_req:host(Req),
                cowboy_req:reply(301,
                    #{<<"location">> => <<"https://", Host/binary, Path/binary>>},
                    <<>>, Req)
        end,
    {ok, Reply, State}.

%%% Tests

-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

%% Serves a published token verbatim, 404s an unknown token, 301s anything else.
listener_test() ->
    application:ensure_all_started(inets),
    application:ensure_all_started(cowboy),
    {ok, _} = start(0, #{}),
    Port = ranch:get_port(?LISTENER),
    Base = "http://localhost:" ++ integer_to_list(Port),
    publish(<<"tok-abc">>, <<"tok-abc.keyauth">>),
    ?assertEqual({200, <<"tok-abc.keyauth">>},
                 req(Base ++ "/.well-known/acme-challenge/tok-abc")),
    ?assertMatch({404, _}, req(Base ++ "/.well-known/acme-challenge/nope")),
    {301, Headers, _} = req_full(Base ++ "/some/path"),
    Loc = list_to_binary(proplists:get_value("location", Headers, "")),
    ?assertMatch(<<"https://", _/binary>>, Loc),
    unpublish(<<"tok-abc">>),
    ?assertEqual(undefined, challenge_response(<<"tok-abc">>)),
    stop().

req(Url) ->
    {S, _, B} = req_full(Url),
    {S, B}.

req_full(Url) ->
    {ok, {{_, S, _}, H, B}} =
        httpc:request(get, {Url, []}, [{autoredirect, false}], [{body_format, binary}]),
    {S, H, B}.

%% The Pebble integration tests below are skipped unless a local Pebble ACME
%% server (letsencrypt/pebble) is reachable on :14000, with challtestsrv on
%% :8055 and validation pointed at this host.
pebble_up() ->
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    case httpc:request(get,
            {"https://localhost:14000/dir", [{"user-agent", "hb"}]},
            [{ssl, [{verify, verify_none}]}], []) of
        {ok, {{_, 200, _}, _, _}} -> true;
        _ -> false
    end.

set_default_dns(Ip) ->
    httpc:request(post,
        {"http://localhost:8055/set-default-ipv4", [],
         "application/json", hb_json:encode(#{<<"ip">> => list_to_binary(Ip)})},
        [], []).

%% Boot a node from a tls.acme-only config: self-signed in-memory bootstrap,
%% :5002 challenge listener (Pebble's validation port), 3-second renewal cron.
pebble_node(Domain, RenewBeforeDays) ->
    hb_http_server:start_node(#{
        <<"tls">> => #{
            <<"acme">> => #{
                <<"email">> => <<"acme@example.com">>,
                <<"directory_url">> => <<"https://localhost:14000/dir">>,
                <<"domains">> => [Domain],
                <<"challenge-port">> => 5002,
                <<"check-interval">> => <<"3-seconds">>,
                <<"renew-before-days">> => RenewBeforeDays,
                <<"http_opts">> => [{verify, verify_none}]
            }
        }
    }).

%% Re-issue, proven off the live TLS socket: boot a node, let the wired cron
%% drive issuance (boot wiring + cron arming + ~cert@1.0 + this listener), and
%% require the served cert to be CA-signed (issuer =/= subject, so not the
%% bootstrap) and then replaced by a second CA-signed cert with a different
%% serial. No restart, no direct function calls.
pebble_reissue_test_() ->
    case pebble_up() of
        true -> {timeout, 90, fun pebble_reissue/0};
        false -> []
    end.

pebble_reissue() ->
    Domain = <<"reissue.example">>,
    set_default_dns("192.168.65.254"),
    catch stop(),
    TlsPort = tls_port(pebble_node(Domain, 100000)),
    try
        {S1, I1, Sub1} = poll_cert(TlsPort, Domain,
                                   fun({_, I, Sub}) -> I =/= Sub end, 25),
        {S2, I2, Sub2} = poll_cert(TlsPort, Domain,
                                   fun({S, _, _}) -> S =/= S1 end, 25),
        ?debugFmt("re-issue on the live TLS socket: CA-signed serial ~p -> ~p, "
                  "no restart", [S1, S2]),
        ?assert(I1 =/= Sub1),
        ?assert(I2 =/= Sub2),
        ?assertNotEqual(S1, S2)
    after
        set_default_dns("10.30.50.3")
    end.

%% The cert the TLS server actually serves, as {serial, issuer, subject}.
served_id(Port, Domain) ->
    #'OTPCertificate'{tbsCertificate = TBS} =
        public_key:pkix_decode_cert(served_der(Port, Domain), otp),
    {TBS#'OTPTBSCertificate'.serialNumber,
     TBS#'OTPTBSCertificate'.issuer,
     TBS#'OTPTBSCertificate'.subject}.

poll_cert(_Port, _Domain, _Pred, 0) -> error(cert_condition_not_met);
poll_cert(Port, Domain, Pred, N) ->
    Id = served_id(Port, Domain),
    case Pred(Id) of
        true -> Id;
        false -> timer:sleep(1500), poll_cert(Port, Domain, Pred, N - 1)
    end.

tls_port(URL) ->
    [_, After] = binary:split(URL, <<"localhost:">>),
    [P | _] = binary:split(After, <<"/">>),
    binary_to_integer(P).

served_der(Port, Domain) ->
    {ok, S} = ssl:connect("127.0.0.1", Port,
        [{verify, verify_none},
         {server_name_indication, binary_to_list(Domain)},
         {alpn_advertised_protocols, [<<"h2">>]}], 4000),
    {ok, Der} = ssl:peercert(S),
    ssl:close(S),
    Der.

%% Boot a node (bootstrap cert stays: renew-before-days 0 is never due), GET
%% the wallet-signed ~meta@1.0/info, and confirm the published
%% `tls.key-fingerprints' pin equals the SPKI pin of the cert served on the TLS
%% port (computed with the standard openssl pipeline a real client would use).
%% Also confirm no cert or key material is exposed under `tls'.
tls_fingerprint_info_test_() ->
    case pebble_up() of
        true -> {timeout, 60, fun tls_fingerprint_info/0};
        false -> []
    end.

tls_fingerprint_info() ->
    Domain = <<"fpinfo.example">>,
    catch stop(),
    Port = tls_port(pebble_node(Domain, 0)),
    %% start_node reports http:// even for a TLS node; gun derives TLS from the
    %% https scheme on a non-443 port (hackney/httpc key transport off :443).
    Peer = list_to_binary("https://localhost:" ++ integer_to_list(Port)),
    Opts = #{http_client => gun, protocol => http2, http_retry => 0,
             http_client_tls_opts => [{verify, verify_none}]},
    {ok, Res} = hb_http:get(Peer, <<"/~meta@1.0/info">>, Opts),
    Published = hb_ao:get(<<"tls/key-fingerprints/", Domain/binary>>, Res, Opts),
    ?assert(is_binary(Published)),
    ServedPin = served_spki_pin(Port, Domain),
    ?debugFmt("cert-wallet binding: published pin=~s served pin=~s",
              [Published, ServedPin]),
    ?assertEqual(ServedPin, Published),
    ?assertEqual(not_found, hb_ao:get(<<"tls/key">>, Res, Opts)),
    ?assertEqual(not_found, hb_ao:get(<<"tls/cert">>, Res, Opts)),
    ?assertEqual(not_found, hb_ao:get(<<"tls/certs">>, Res, Opts)),
    ?assertEqual(not_found, hb_ao:get(<<"tls/acme">>, Res, Opts)).

%% SPKI pin of the served cert via openssl, independent of hb_tls internals.
served_spki_pin(Port, Domain) ->
    Der = served_der(Port, Domain),
    Pem = public_key:pem_encode([{'Certificate', Der, not_encrypted}]),
    File = "/tmp/hb-served-" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".pem",
    ok = file:write_file(File, Pem),
    Pin = string:trim(os:cmd(
        "openssl x509 -in " ++ File ++ " -pubkey -noout"
        " | openssl pkey -pubin -outform der"
        " | openssl dgst -sha256 -binary | openssl base64")),
    file:delete(File),
    list_to_binary(Pin).
