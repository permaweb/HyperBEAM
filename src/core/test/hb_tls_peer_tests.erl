%%% @doc Named TLS identity, public certificate exchange and admission controls.
-module(hb_tls_peer_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").
-include_lib("kernel/include/file.hrl").

named_identity_test_() -> {timeout, 120, fun named_identity/0}.

%% @doc Load the supplied configurations through the actual operator codec.
configuration_test() ->
    Root = "docs/run/named-identity-tls/",
    {ok, Config} = hb_opts:load(Root ++ "member.json," ++
        Root ++ "joiner.json," ++ Root ++ "dns-authority.json", #{}),
    TLS = hb_tls:config(Config),
    ?assertEqual(<<"zone/gateway">>, maps:get(<<"identity">>, TLS)),
    ?assertEqual(443, maps:get(<<"port">>, TLS)),
    ?assertEqual(5, length(maps:get(<<"peers">>, TLS))),
    ?assertEqual(2, length(maps:get(<<"dns-peers">>, maps:get(<<"acme">>, TLS)))),
    ?assertEqual(false, maps:get(<<"zone-init-allow">>, Config)),
    ?assertEqual([<<"measurement@1.0">>, <<"zone@1.0">>, <<"dns@1.0">>],
        [hb_maps:get(<<"device">>, Hook) || Hook <- hb_hook:find(<<"start">>, Config)]).

%% @doc Five independent nodes admit one identity without replacing their own.
named_identity() ->
    {Wallet, Chain, CA} = certificates([<<"localhost">>, <<"*.localhost">>]),
    TLS = #{
        <<"identity">> => <<"zone/web">>,
        <<"port">> => 0,
        <<"domains">> => [<<"localhost">>, <<"*.localhost">>],
        <<"ca-certificates">> => pem(CA),
        <<"renewal-jitter">> => 0,
        <<"acme">> => #{ <<"challenge-type">> => <<"dns-01">>,
            <<"dns-nameserver">> => <<"ns.localhost">> }
    },
    ok = hb_tls:validate(Wallet, Chain, TLS),
    ?assertMatch({error, _}, hb_tls:validate(ar_wallet:new(), Chain, TLS)),
    ?assertMatch({error, _}, hb_tls:validate(Wallet, Chain,
        TLS#{ <<"domains">> => [<<"wrong.localhost">>] })),
    {_, _, OtherCA} = certificates([<<"localhost">>]),
    ?assertMatch({error, _}, hb_tls:validate(Wallet, Chain,
        TLS#{ <<"ca-certificates">> => pem(OtherCA) })),
    First = start_member(TLS, Wallet, Chain),
    try
        {FirstURL, FirstOpts, _} = First,
        Peers = TLS#{ <<"peers">> => [FirstURL] },
        Others = [start_member(Peers, Wallet, []) || _ <- lists:seq(1, 4)],
        try
            lists:foreach(fun(Member) -> admit(Member, Wallet) end,
                [First | Others]),
            lists:foreach(fun(Member) -> verify_member(Member, Chain, CA) end,
                [First | Others]),
            challenge_controls(First, Wallet),
            % The current public certificate is available through ordinary HTTP.
            {ok, Public} = hb_http:get(FirstURL,
                <<"/~tls@1.0/certificate">>, #{}),
            ?assertEqual({ok, Chain}, hb_tls:certificate_chain(
                hb_maps:get(<<"body">>, Public))),
            ?assertEqual(nomatch, binary:match(hb_maps:get(<<"body">>, Public),
                <<"PRIVATE KEY">>)),
            ?assertMatch({error, #{ <<"status">> := 404 }},
                hb_http:get(FirstURL, <<"/~tls@1.0/obtain">>, #{})),
            % Repeated private timer hints must adopt, not create fresh orders.
            PID = hb_name:lookup({<<"tls@1.0">>, hb_opts:address(FirstOpts)}),
            PID ! renew,
            verify_member(First, Chain, CA),
            Last = {_, LastOpts, _} = lists:last(Others),
            restart_member(Last, Wallet, Chain, CA),
            ID = hb_opts:address(LastOpts),
            Current = hb_http_server:get_opts(#{ <<"http-server">> => ID }),
            % Remove the identity while a real peer read is still outstanding.
            erlang:suspend_process(PID),
            try
                hb_name:lookup({<<"tls@1.0">>, ID}) ! renew,
                ?assert(hb_util:wait_until(fun() ->
                    {messages, Messages} = process_info(PID, messages),
                    lists:any(fun
                        ({certificate, _, _}) -> true;
                        (_) -> false
                    end, Messages)
                end, 5000)),
                ok = hb_http_server:set_opts(Current#{ <<"identities">> => #{} }),
                ?assert(hb_util:wait_until(fun() ->
                    not is_integer(catch ranch:get_port({tls, ID}))
                end, 5000))
            after erlang:resume_process(PID)
            end,
            ?assert(is_integer(ranch:get_port(ID)))
        after lists:foreach(fun stop_member/1, Others)
        end
    after stop_member(First)
    end.

%% @doc Reject expired, future, wrong-use and untrusted chains, not just bad keys.
certificate_validation_test() ->
    lists:foreach(fun(Options) ->
        {Wallet, Chain, CA} = certificates([<<"localhost">>], Options),
        ?assertMatch({error, 'invalid-tls-certificate'},
            hb_tls:validate(Wallet, Chain, #{
                <<"domains">> => [<<"localhost">>],
                <<"ca-certificates">> => pem(CA)
            }))
    end, [
        [{validity, {{2020, 1, 1}, {2020, 1, 2}}}],
        [{validity, {{2099, 1, 1}, {2099, 1, 2}}}],
        [{extensions, [#'Extension'{extnID = ?'id-ce-extKeyUsage',
            critical = false, extnValue = [?'id-kp-clientAuth']}]}],
        [{extensions, [#'Extension'{extnID = ?'id-ce-keyUsage',
            critical = true, extnValue = [keyEncipherment]}]}]
    ]),
    {Wallet, Chain, CA} = certificates([<<"localhost">>]),
    {_, _, OtherCA} = certificates([<<"localhost">>]),
    ?assertEqual(ok, hb_tls:validate(Wallet, Chain, #{
        <<"domains">> => [<<"localhost">>],
        <<"ca-certificates">> => pem(OtherCA ++ CA)
    })).

%% @doc A failed order's account and retry gate survive listener restart.
retry_recovery_test_() -> {timeout, 30, fun retry_recovery/0}.

retry_recovery() ->
    Wallet = ar_wallet:new(),
    TLS = #{ <<"identity">> => <<"zone/web">>, <<"port">> => 0,
        <<"domains">> => [<<"localhost">>], <<"renewal-jitter">> => 0,
        <<"acme">> => #{ <<"challenge-type">> => <<"dns-01">> }
    },
    Member = {_, Opts, Path} = start_member(TLS, Wallet, []),
    ID = hb_opts:address(Opts),
    try
        install_identity(Opts, Wallet),
        ?assert(hb_util:wait_until(fun() -> filelib:is_file(Path) end, 5000)),
        cowboy:stop_listener(ID),
        ?assert(hb_util:wait_until(fun() ->
            hb_name:lookup({<<"tls@1.0">>, ID}) =:= undefined
        end, 5000)),
        {ok, Saved} = file:read_file(Path),
        #{next_attempt := Until, account_wallet := Account} = binary_to_term(Saved),
        ?assert(Until > erlang:system_time(millisecond)),
        {ok, #file_info{mode = Mode}} = file:read_file_info(Path),
        ?assertEqual(8#600, Mode band 8#777),
        URL = hb_http_server:start_node(Opts),
        install_identity(Opts, Wallet),
        PID = hb_name:lookup({<<"tls@1.0">>, ID}),
        lists:foreach(fun(_) -> PID ! renew end, lists:seq(1, 5)),
        ?assertMatch({error, #{ <<"status">> := 404 }},
            hb_http:get(URL, <<"/~tls@1.0/obtain">>, #{})),
        {ok, Retained} = file:read_file(Path),
        #{next_attempt := Later, account_wallet := SameAccount} = binary_to_term(Retained),
        ?assertEqual(Until, Later),
        ?assertEqual(hb:address(Account), hb:address(SameAccount)),
        ?assertMatch({'EXIT', _}, catch ranch:get_port({tls, ID}))
    after stop_member(Member)
    end.

%% @doc A stale or unavailable peer cannot replace a later valid certificate.
peer_monotonicity_test_() -> {timeout, 30, fun peer_monotonicity/0}.

peer_monotonicity() ->
    Key = public_key:generate_key({rsa, 2048, 65537}),
    Root = public_key:pkix_test_root_cert("TLS test root", [{digest, sha256}]),
    {{Year, Month, Day}, _} = calendar:universal_time(),
    Yesterday = calendar:gregorian_days_to_date(
        calendar:date_to_gregorian_days({Year, Month, Day}) - 1),
    Options = [{key, Key}, {root, Root}],
    {Wallet, Old, CA} = certificates([<<"localhost">>],
        [{validity, {Yesterday, {Year + 1, Month, Day}}} | Options]),
    {Wallet, New, CA} = certificates([<<"localhost">>],
        [{validity, {Yesterday, {Year + 2, Month, Day}}} | Options]),
    TLS = #{ <<"identity">> => <<"zone/web">>, <<"port">> => 0,
        <<"domains">> => [<<"localhost">>], <<"ca-certificates">> => pem(CA),
        <<"acme">> => #{ <<"challenge-type">> => <<"dns-01">> } },
    ?assertEqual(ok, hb_tls:validate(Wallet, Old, TLS)),
    ?assertEqual(ok, hb_tls:validate(Wallet, New, TLS)),
    Stale = {OldURL, _, _} = start_member(TLS, Wallet, Old),
    Fresh = {NewURL, _, _} = start_member(TLS#{ <<"peers">> => [OldURL] }, Wallet, New),
    Follower = {FollowerURL, FollowerOpts, Path} = start_member(
        TLS#{ <<"peers">> => [OldURL, NewURL] }, Wallet, Old),
    try
        lists:foreach(fun(M) -> admit(M, Wallet) end, [Stale, Fresh, Follower]),
        verify_member(Fresh, New, CA),
        verify_member(Follower, New, CA),
        stop_member(Fresh),
        PID = hb_name:lookup({<<"tls@1.0">>, hb_opts:address(FollowerOpts)}),
        erlang:trace(PID, true, ['receive']),
        try
            PID ! renew,
            receive
                {trace, PID, 'receive', {fleet_result, {ok, #{chain := New}}}} -> ok
            after 10000 -> error('missing-peer-check-result')
            end,
            {ok, Public} = hb_http:get(FollowerURL, <<"/~tls@1.0/certificate">>, #{}),
            ?assertEqual({ok, New}, hb_tls:certificate_chain(hb_maps:get(<<"body">>, Public)))
        after erlang:trace(PID, false, ['receive'])
        end,
        verify_member(Follower, New, CA),
        {ok, Binary} = file:read_file(Path),
        ?assertEqual(New, maps:get(chain, binary_to_term(Binary)))
    after lists:foreach(fun stop_member/1, [Stale, Fresh, Follower])
    end.

%% @doc Start before the inherited key exists; there must be no TLS fallback key.
start_member(TLS, Wallet, Chain) ->
    NodeWallet = ar_wallet:new(),
    Store = [hb_test_utils:test_store()],
    Path = <<"test/tls-state-", (hb:address(NodeWallet))/binary>>,
    case Chain of
        [] -> ok;
        _ -> ok = file:write_file(Path, term_to_binary(#{
            context => {hb:address(Wallet), maps:get(<<"domains">>, TLS)},
            account_wallet => ar_wallet:new(), chain => Chain, next_attempt => 0
        }))
    end,
    Opts = #{ <<"priv-wallet">> => NodeWallet,
        <<"store">> => Store, <<"port">> => 0, <<"protocol">> => http2,
        <<"tls">> => TLS#{ <<"state-path">> => Path },
        <<"cache-control">> => [<<"no-cache">>, <<"no-store">>] },
    URL = hb_http_server:start_node(Opts),
    ?assertEqual(<<"http">>, maps:get(scheme, uri_string:parse(URL))),
    ?assertMatch({'EXIT', _}, catch ranch:get_port({tls, hb_opts:address(Opts)})),
    {URL, Opts, Path}.

%% @doc Exercise the same options installation used by attested zone admission.
admit({_, Opts, _}, Wallet) ->
    install_identity(Opts, Wallet),
    ID = hb_opts:address(Opts),
    ?assert(hb_util:wait_until(fun() ->
        is_integer(catch ranch:get_port({tls, ID}))
    end, 15000)).

install_identity(Opts, Wallet) ->
    ID = hb_opts:address(Opts),
    Current = hb_http_server:get_opts(#{ <<"http-server">> => ID }),
    ?assertMatch({error, 'tls-identity-unavailable'}, hb_tls:wallet(Current)),
    ok = hb_http_server:set_opts(Current#{ <<"identities">> => #{
        <<"zone/web">> => #{ <<"priv-wallet">> => Wallet }
    } }).

%% @doc Verify the zone leaf and distinct node identity over CA-verified H2.
verify_member({_, Opts, _}, [Leaf | _], CA) ->
    ID = hb_opts:address(Opts),
    Port = ranch:get_port({tls, ID}),
    {ok, Socket} = ssl:connect("localhost", Port, [
        {verify, verify_peer}, {cacerts, CA}, {active, false},
        {customize_hostname_check, [
            {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
        ]}
    ], 5000),
    try ?assertEqual({ok, Leaf}, ssl:peercert(Socket))
    after ssl:close(Socket)
    end,
    {ok, Info} = hb_http:get(
        <<"https://localhost:", (integer_to_binary(Port))/binary>>,
        <<"/~meta@1.0/info/address">>, #{
            <<"protocol">> => http2, <<"http-client">> => gun,
            <<"http-client-tls-ca">> => CA
        }),
    ?assertEqual(ID, Info).

%% @doc Restart only the test member and recover its chain from its private file.
restart_member(Member = {_, Opts, _}, Wallet, Chain, CA) ->
    ID = hb_opts:address(Opts),
    ok = cowboy:stop_listener(ID),
    ?assert(hb_util:wait_until(fun() ->
        hb_name:lookup({<<"tls@1.0">>, ID}) =:= undefined
    end, 5000)),
    hb_http_server:start_node(Opts),
    admit(Member, Wallet),
    verify_member(Member, Chain, CA).

%% @doc Signed publications remain scoped, expire, and cannot trigger issuance.
challenge_controls({URL, Opts, _}, Wallet) ->
    Base = #{ <<"path">> => <<"/~tls@1.0/challenge">>,
        <<"method">> => <<"POST">>, <<"type">> => <<"TLS-DNS-Challenge">>,
        <<"name">> => <<"_acme-challenge.localhost">>,
        <<"token">> => <<"test-token">>,
        <<"value">> => hb_util:encode(crypto:hash(sha256, <<"test">>)),
        <<"action">> => <<"put">>,
        <<"expires">> => erlang:system_time(second) + 2 },
    Send = fun(Msg, Key) ->
        Client = #{ <<"priv-wallet">> => Key,
            <<"http-codec">> => <<"flat@1.0">> },
        hb_http:post(URL, hb_message:commit(Msg, Client), Client)
    end,
    ?assertMatch({error, #{ <<"status">> := 403 }}, Send(Base, ar_wallet:new())),
    ?assertMatch({error, #{ <<"status">> := 403 }},
        Send(Base#{ <<"name">> => <<"_acme-challenge.other.test">> }, Wallet)),
    ?assertMatch({error, #{ <<"status">> := 403 }},
        hb_http:post(URL, Base, #{ <<"http-codec">> => <<"flat@1.0">> })),
    ?assertMatch({error, #{ <<"status">> := 403 }},
        Send(Base#{ <<"expires">> => erlang:system_time(second) + 3600 }, Wallet)),
    ?assertMatch({error, #{ <<"status">> := 403 }},
        Send(Base#{ <<"type">> => <<"Other-Purpose">> }, Wallet)),
    ?assertMatch({ok, _}, Send(Base, Wallet)),
    Resolve = fun() ->
        hb_ao:resolve(#{ <<"device">> => <<"tls@1.0">> }, #{
            <<"path">> => <<"dns-resolve">>,
            <<"name">> => <<"_acme-challenge.localhost">>
        }, Opts)
    end,
    {ok, Present} = Resolve(),
    ?assertMatch([_], hb_maps:get(<<"answers">>, Present)),
    ?assert(hb_util:wait_until(fun() ->
        {ok, Reply} = Resolve(), hb_maps:get(<<"answers">>, Reply) =:= []
    end, 4000)),
    ?assertMatch({error, #{ <<"status">> := 403 }}, Send(Base, Wallet)).

stop_member({_, Opts, Path}) ->
    ID = hb_opts:address(Opts),
    PID = hb_name:lookup({<<"tls@1.0">>, ID}),
    Ref = case PID of
        undefined -> undefined;
        _ -> erlang:monitor(process, PID)
    end,
    catch cowboy:stop_listener(ID),
    case Ref of
        undefined -> ok;
        _ -> receive {'DOWN', Ref, process, PID, _} -> ok
            after 5000 -> error('tls-runtime-survived-listener')
            end
    end,
    ?assertMatch({'EXIT', _}, catch ranch:get_port({tls, ID})),
    file:delete(Path),
    hb_store:reset(hb_opts:get(store, [], Opts)).

%% @doc Generate real RSA keys and an OTP-issued test certificate chain.
certificates(Domains) ->
    certificates(Domains, []).

certificates(Domains, Options) ->
    Key = case proplists:get_value(key, Options) of
        undefined -> public_key:generate_key({rsa, 2048, 65537});
        Supplied -> Supplied
    end,
    Wallet = ar_wallet:new({rsa, Key#'RSAPrivateKey'.publicExponent},
        binary:encode_unsigned(Key#'RSAPrivateKey'.privateExponent),
        binary:encode_unsigned(Key#'RSAPrivateKey'.modulus)),
    Certificates = public_key:pkix_test_data(#{
        root => proplists:get_value(root, Options, [{digest, sha256}]),
        peer => [{key, Key}, {digest, sha256}, {extensions, [
            #'Extension'{extnID = ?'id-ce-subjectAltName', critical = false,
                extnValue = [{dNSName, binary_to_list(D)} || D <- Domains]}
        | proplists:get_value(extensions, Options, [])]} |
            proplists:delete(key, proplists:delete(root,
                proplists:delete(extensions, Options)))]
    }),
    CA = proplists:get_value(cacerts, Certificates),
    {Wallet, [proplists:get_value(cert, Certificates) | CA], CA}.

pem(Chain) -> public_key:pem_encode([
    {'Certificate', DER, not_encrypted} || DER <- Chain
]).

%% @doc Real ACME issuance and timer-driven renewal with two DNS authorities.
pebble_fleet_test_() ->
    case os:getenv("HB_TLS_PEER_PEBBLE") of
        false -> [];
        _ -> {timeout, 300, fun pebble_fleet/0}
    end.

pebble_fleet() ->
    {ok, ACMECert} = file:read_file(os:getenv("HB_PEBBLE_CA")),
    {ok, IssuerPEM} = file:read_file(os:getenv("HB_PEBBLE_ISSUER_CA")),
    {ok, CAs} = hb_tls:certificate_chain(IssuerPEM),
    Wallet = ar_wallet:new(),
    Guards = [begin
        {ok, Guard} = gen_tcp:listen(0, []),
        {ok, {_, Port}} = inet:sockname(Guard),
        {Guard, Port}
    end || _ <- lists:seq(1, 5)],
    URLs = [<<"http://localhost:", (integer_to_binary(P))/binary>>
        || {_, P} <- Guards],
    TLS = #{ <<"identity">> => <<"zone/web">>, <<"port">> => 0,
        <<"domains">> => [<<"localhost">>, <<"*.localhost">>],
        <<"ca-certificates">> => IssuerPEM, <<"peers">> => URLs,
        <<"acme">> => #{
            <<"directory-url">> => hb_util:bin(os:getenv("HB_TLS_PEER_PEBBLE")),
            <<"ca-certificate">> => ACMECert,
            <<"terms-of-service-agreed">> => true,
            <<"challenge-type">> => <<"dns-01">>,
            <<"dns-nameserver">> => <<"ns.localhost">>,
            <<"dns-peers">> => lists:sublist(URLs, 2)
        }
    },
    Members = [begin
        NodeWallet = ar_wallet:new(),
        Path = <<"test/tls-state-", (hb:address(NodeWallet))/binary>>,
        Node = #{ <<"priv-wallet">> => NodeWallet,
            <<"port">> => Port, <<"protocol">> => http2,
            <<"store">> => [hb_test_utils:test_store()],
            <<"tls">> => TLS#{ <<"state-path">> => Path,
                <<"renewal-jitter">> => case N of 1 -> 0; _ -> 12 end },
            <<"cache-control">> => [<<"no-cache">>, <<"no-store">>]
        },
        Opts = case N =< 2 of
            true -> Node#{ <<"dns">> => #{
                    <<"address">> => <<"127.0.0.1">>, <<"port">> => 18652 + N
                }, <<"on">> => #{
                    <<"start">> => #{ <<"device">> => <<"dns@1.0">> },
                    <<"dns-resolve">> => #{ <<"device">> => <<"tls@1.0">> }
                } };
            false -> Node
        end,
        gen_tcp:close(Guard),
        {hb_http_server:start_node(Opts), Opts, Path}
    end || {N, {Guard, Port}} <- lists:zip(lists:seq(1, 5), Guards)],
    try
        lists:foreach(fun({_, Opts, _}) -> install_identity(Opts, Wallet) end,
            Members),
        ?assert(hb_util:wait_until(fun() -> lists:all(fun({_, O, _}) ->
            is_integer(catch ranch:get_port({tls, hb_opts:address(O)}))
        end, Members) end, 45000)),
        [{URL, FirstOpts, _} | _] = Members,
        {ok, First} = hb_http:get(URL, <<"/~tls@1.0/certificate">>, #{}),
        {ok, Chain} = hb_tls:certificate_chain(hb_maps:get(<<"body">>, First)),
        ok = hb_tls:validate(Wallet, Chain, TLS),
        % Concurrent issuers may obtain different valid leaves for the same key.
        lists:foreach(fun(Member = {Peer, _, _}) ->
            {ok, Public} = hb_http:get(Peer, <<"/~tls@1.0/certificate">>, #{}),
            {ok, Served} = hb_tls:certificate_chain(hb_maps:get(<<"body">>, Public)),
            ?assertEqual(ok, hb_tls:validate(Wallet, Served, TLS)),
            verify_member(Member, Served, CAs)
        end, Members),
        dns_authorities(lists:sublist(URLs, 2), Wallet),
        Port = ranch:get_port({tls, hb_opts:address(FirstOpts)}),
        {ok, Connection} = gun:open("localhost", Port, #{
            transport => tls, protocols => [http2], retry => 0,
            tls_opts => [{verify, verify_peer}, {cacerts, CAs}]
        }),
        try
            ?assertEqual({ok, http2}, gun:await_up(Connection, 5000)),
            % No private renewal hint: the real short-lived certificate expires.
            ?assert(hb_util:wait_until(fun() ->
                {ok, Reply} = hb_http:get(URL, <<"/~tls@1.0/certificate">>, #{}),
                {ok, Current} = hb_tls:certificate_chain(hb_maps:get(<<"body">>, Reply)),
                hb_tls:certificate_expiry(Current) > hb_tls:certificate_expiry(Chain)
            end, 150000)),
            Ref = gun:get(Connection, <<"/~meta@1.0/info/address">>),
            ?assertMatch({response, nofin, 200, _}, gun:await(Connection, Ref, 5000)),
            ?assertEqual({ok, hb_opts:address(FirstOpts)},
                gun:await_body(Connection, Ref, 5000)),
            {ok, Renewed} = hb_http:get(URL, <<"/~tls@1.0/certificate">>, #{}),
            {ok, NewChain} = hb_tls:certificate_chain(hb_maps:get(<<"body">>, Renewed)),
            ok = hb_tls:validate(Wallet, NewChain, TLS),
            ?assert(hb_util:wait_until(fun() -> lists:all(fun({Peer, _, _}) ->
                {ok, Reply} = hb_http:get(Peer, <<"/~tls@1.0/certificate">>, #{}),
                {ok, C} = hb_tls:certificate_chain(hb_maps:get(<<"body">>, Reply)),
                hb_tls:certificate_expiry(C) >= hb_tls:certificate_expiry(NewChain)
            end, Members) end, 60000))
        after gun:close(Connection)
        end,
        authority_failure(TLS, Wallet, hd(URLs))
    after
        lists:foreach(fun stop_member/1, Members),
        lists:foreach(fun(PID) -> PID ! stop end,
            [PID || {{_, {127, 0, 0, 1}, Port}, PID} <- hb_name:all(),
                Port =:= 18653 orelse Port =:= 18654])
    end.

%% @doc Partial DNS publication fails closed, cleans up and retains a retry gate.
authority_failure(TLS, Wallet, Authority) ->
    ACME = maps:get(<<"acme">>, TLS),
    Unavailable = {AbsentURL, _, _} = start_member(TLS, Wallet, []),
    stop_member(Unavailable),
    Member = {URL, Opts, Path} = start_member(TLS#{
        <<"peers">> => [], <<"renewal-jitter">> => 0,
        <<"acme">> => ACME#{
            <<"dns-peers">> => [Authority, AbsentURL]
        }
    }, Wallet, []),
    ID = hb_opts:address(Opts),
    PID = hb_name:lookup({<<"tls@1.0">>, ID}),
    erlang:trace(PID, true, ['receive']),
    try
        install_identity(Opts, Wallet),
        receive
            {trace, PID, 'receive', {fleet_result,
                    {issued, {error, Reason}, _}}} ->
                ?assertEqual({badmatch, {error, 'tls-dns-publication-failed'}}, Reason)
        after 30000 -> error('missing-publication-failure')
        end,
        ?assertMatch({error, #{ <<"status">> := 404 }},
            hb_http:get(URL, <<"/~tls@1.0/certificate">>, #{})),
        ?assertMatch({'EXIT', _}, catch ranch:get_port({tls, ID})),
        {ok, Saved} = file:read_file(Path),
        Until = maps:get(next_attempt, binary_to_term(Saved)),
        ?assert(Until > erlang:system_time(millisecond)),
        {ok, Reply} = inet_res:resolve("_acme-challenge.localhost", in, txt, [
            {nameservers, [{{127, 0, 0, 1}, 18653}]},
            {retry, 1}, {timeout, 1000}
        ]),
        ?assertEqual([], inet_dns:msg(Reply, anlist)),
        receive
            {trace, PID, 'receive', renew} -> ok
        after 35000 -> error('peer-check-blocked-by-order-backoff')
        end,
        receive
            {trace, PID, 'receive', {fleet_result,
                {ok, #{next_attempt := Until}}}} -> ok
        after 5000 -> error('missing-peer-check-result')
        end,
        ?assertMatch({error, #{ <<"status">> := 404 }},
            hb_http:get(URL, <<"/~tls@1.0/certificate">>, #{})),
        {ok, Retained} = file:read_file(Path),
        ?assertEqual(Until, maps:get(next_attempt, binary_to_term(Retained)))
    after
        erlang:trace(PID, false, ['receive']),
        stop_member(Member)
    end.

%% @doc Both DNS authorities serve signed publications over UDP and TCP.
dns_authorities(URLs, Wallet) ->
    Record = #{ <<"path">> => <<"/~tls@1.0/challenge">>,
        <<"method">> => <<"POST">>, <<"type">> => <<"TLS-DNS-Challenge">>,
        <<"name">> => <<"_acme-challenge.localhost">>,
        <<"token">> => <<"authority-test">>,
        <<"value">> => hb_util:encode(crypto:hash(sha256, <<"authorities">>)),
        <<"expires">> => erlang:system_time(second) + 120 },
    Client = #{ <<"priv-wallet">> => Wallet, <<"http-codec">> => <<"flat@1.0">> },
    lists:foreach(fun(Action) ->
        Signed = hb_message:commit(Record#{ <<"action">> => Action }, Client),
        lists:foreach(fun(URL) -> ?assertMatch({ok, _},
            hb_http:post(URL, Signed, Client)) end, URLs),
        lists:foreach(fun({Port, TCP}) ->
            {ok, Reply} = inet_res:resolve("_acme-challenge.localhost", in, txt, [
                {nameservers, [{{127, 0, 0, 1}, Port}]},
                {usevc, TCP}, {retry, 1}, {timeout, 1000}
            ]),
            ?assert(inet_dns:header(inet_dns:msg(Reply, header), aa)),
            Answers = [inet_dns:rr(RR, data) || RR <- inet_dns:msg(Reply, anlist)],
            ?assertEqual(Action =:= <<"put">>, lists:member(
                [binary_to_list(maps:get(<<"value">>, Record))], Answers))
        end, [{P, TCP} || P <- [18653, 18654], TCP <- [false, true]])
    end, [<<"put">>, <<"delete">>]).
