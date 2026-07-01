%%% @doc HTTP-01 challenge listener for hb_acme: a minimal plaintext listener
%%% (port 80 in production) that answers ACME HTTP-01 challenges from an
%%% in-memory token store and 301-redirects everything else to https. It holds
%%% no private key and runs no application logic, so it adds no trust surface
%%% beyond a public challenge token and a redirect. The hb_acme `publish' and
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

%% Capstone: dev_cert renews a domain end to end, with Pebble validating the
%% HTTP-01 challenge against THIS listener (not challtestsrv). Points Pebble's
%% DNS (challtestsrv) at the host, runs our listener on Pebble's validation
%% port, drives dev_cert:renew, and confirms the cert is obtained and installed
%% live. Skipped unless Pebble is reachable; restores the DNS afterwards.
pebble_renew_integration_test_() ->
    case pebble_up() of
        true -> {timeout, 90, fun pebble_renew_integration/0};
        false -> []
    end.

pebble_up() ->
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    case httpc:request(get,
            {"https://localhost:14000/dir", [{"user-agent", "hb"}]},
            [{ssl, [{verify, verify_none}]}], []) of
        {ok, {{_, 200, _}, _, _}} -> true;
        _ -> false
    end.

pebble_renew_integration() ->
    application:ensure_all_started(cowboy),
    Domain = <<"capstone.example">>,
    set_default_dns("192.168.65.254"),    %% Pebble validates against the host
    catch stop(),
    {ok, _} = start(5002, #{}),           %% our listener on Pebble's httpPort
    load_dev_cert(),
    hb_acme:configure(#{
        <<"domains">> => [Domain],
        <<"email">> => <<"acme@example.com">>,
        <<"directory_url">> => <<"https://localhost:14000/dir">>,
        <<"http_opts">> => [{verify, verify_none}],
        <<"renew-before-days">> => 30
    }),
    try
        {ok, #{<<"body">> := #{<<"results">> := Results}}} =
            dev_cert:renew(#{}, #{}, #{}),
        ?assertEqual(renewed, maps:get(Domain, Results)),
        Days = hb_tls:expiry(Domain),
        ?debugFmt("capstone: dev_cert obtained + installed via our :80 listener; "
                  "cert valid ~p days", [Days]),
        ?assert(is_integer(Days) andalso Days > 0)
    after
        stop(),
        set_default_dns("10.30.50.3")     %% restore for the hb_acme test
    end.

set_default_dns(Ip) ->
    httpc:request(post,
        {"http://localhost:8055/set-default-ipv4", [],
         "application/json", hb_json:encode(#{<<"ip">> => list_to_binary(Ip)})},
        [], []).

%% dev_cert lives in src/preloaded and is packaged by the Forge build, not the
%% rebar ebin, so load it directly to exercise its real orchestration here.
load_dev_cert() ->
    os:cmd("erlc -I src/core -o /tmp src/preloaded/node/dev_cert.erl"),
    {module, dev_cert} = code:load_abs("/tmp/dev_cert"),
    ok.

%% Full production path: boot a node from a tls.acme config and confirm the
%% WIRED cron (armed by hb_http_server) fires on its own, renews via Pebble
%% through the in-node :80 listener and the store-resolved ~cert@1.0 device, and
%% the served cert flips live on the TLS port with no restart. This is the only
%% test that exercises boot wiring + cron arming + cron firing together.
pebble_auto_renew_test_() ->
    case pebble_up() of
        true -> {timeout, 90, fun pebble_auto_renew/0};
        false -> []
    end.

pebble_auto_renew() ->
    Domain = <<"autorenew.example">>,
    set_default_dns("192.168.65.254"),   %% Pebble validates against the host
    catch stop(),
    %% No tls.certs: the node self-signs an in-memory bootstrap for the ACME
    %% domain, brings up :443 with it, then the wired cron flips it to Pebble's.
    URL = hb_http_server:start_node(#{
        <<"tls">> => #{
            <<"acme">> => #{
                <<"email">> => <<"acme@example.com">>,
                <<"directory_url">> => <<"https://localhost:14000/dir">>,
                <<"domains">> => [Domain],
                <<"challenge-port">> => 5002,         %% Pebble's validation port
                <<"check-interval">> => <<"3-seconds">>,
                <<"renew-before-days">> => 100000,    %% force the bootstrap cert due
                <<"http_opts">> => [{verify, verify_none}]
            }
        }
    }),
    TlsPort = tls_port(URL),
    try
        Before = served_der(TlsPort, Domain),
        After = wait_for_change(TlsPort, Domain, Before, 25),
        ?debugFmt("auto-renew via WIRED cron: served cert changed ~p -> ~p bytes, "
                  "no restart", [byte_size(Before), byte_size(After)]),
        ?assertNotEqual(Before, After)
    after
        set_default_dns("10.30.50.3")
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

wait_for_change(_Port, _Domain, _Before, 0) -> error(cert_never_renewed);
wait_for_change(Port, Domain, Before, N) ->
    timer:sleep(1500),
    case served_der(Port, Domain) of
        Before -> wait_for_change(Port, Domain, Before, N - 1);
        Changed -> Changed
    end.
