%%% @doc TLS certificates and domain validation for the node's RSA wallet.
%%% This device supplies certificates to `hb_http_server', using the node wallet
%%% or the named `tls/identity'. Certificates must match that wallet.
%%%
%%% The interface is as follows:
%%%
%%% - `csr': Return a PEM certificate signing request for the request's `domains',
%%%   falling back to `tls/domains' in the node message. The CSR can be submitted
%%%   to any issuer without disclosing the private key.
%%% - `request': An `on/request' handler that routes HTTP-01 challenge paths to
%%%   `well-known', preserving the rest of the hook message.
%%% - `well-known': Serve the active HTTP-01 authorization for a `token'.
%%% - `dns-resolve': An `on/dns-resolve' handler serving authoritative TXT, NS and
%%%   SOA records for the configured DNS-01 challenge names. Other names and
%%%   classes are refused.
%%% - `obtain': Supply a leaf-first DER certificate chain during listener startup.
%%%   This key requires the listener's private lifecycle capability; it is not a
%%%   public certificate-issuance endpoint.
%%% - `certificate': Return the public chain from the peer-renewal lifecycle.
%%% - `challenge': Accept a signed, scoped DNS publication from the TLS identity.
%%%
%%% Setting `tls/port' adds HTTPS after the ordinary AO-Core listener starts.
%%% This mode requires DNS-01 and a private `tls/state-path'. It adopts valid
%%% peer certificates before attempting issuance and retains retry deadlines.
%%%
%%% Certificate configuration is read from the node's `tls' message. Setting
%%% `tls/certificate-path' without `tls/port' loads a leaf-first PEM chain from
%%% disk and bypasses ACME and automatic renewal.
%%%
%%% Without a certificate file, `tls/domains' and the `tls/acme' message configure
%%% automated issuance. ACME requires a `directory-url' and explicit agreement
%%% through `terms-of-service-agreed'. The `challenge-type' defaults to `http-01';
%%% `dns-01' also requires `dns-nameserver' and a running DNS listener whose
%%% resolution hook calls this device.
%%%
%%% ACME challenges and renewal timers belong to an `hb_name' singleton for the
%%% listener. Issuance runs in a linked worker so validation requests can be
%%% answered while the client waits for the issuer. Renewal replaces the live
%%% certificate without closing established connections.
-module(dev_tls).
-device_libraries([lib_meta, lib_process]).
-export([device_info/1]).
-export([request/3, well_known/3, dns_resolve/3, csr/3, obtain/3]).
-export([certificate/3, challenge/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(CALL_TIMEOUT, 5000).
-define(MAX_TIMER_MS, 16#ffffffff).
-define(RENEW_BEFORE_MS, 30 * 24 * 60 * 60 * 1000).
-define(RENEW_RETRY_MS, 60 * 60 * 1000).

%% @doc Expose the device keys while keeping certificate lifecycle helpers private.
device_info(_) ->
    #{ exports => [
        <<"request">>, <<"well-known">>, <<"dns-resolve">>, <<"csr">>,
        <<"obtain">>, <<"certificate">>, <<"challenge">>
    ] }.

%% @doc Return a PEM CSR for request domains, defaulting to `tls/domains'.
%% The selected TLS wallet signs the request; no key material is returned.
csr(_Base, Request, Opts) ->
    try
        RawDomains = hb_ao:get_first(
            [{Request, <<"domains">>}, {Opts, <<"tls/domains">>}], [], Opts
        ),
        Domains = hb_util:message_to_ordered_list(
            hb_cache:ensure_all_loaded(RawDomains, Opts), Opts
        ),
        true = Domains =/= [] andalso lists:all(
            fun(Domain) -> is_binary(Domain) andalso byte_size(Domain) > 0 end,
            Domains
        ),
        {ok, Wallet} = hb_tls:wallet(Opts),
        DER = hb_tls:csr(Wallet, Domains),
        {ok, #{
            <<"status">> => 200,
            <<"content-type">> => <<"text/plain">>,
            <<"cache-control">> => <<"no-store">>,
            <<"body">> => public_key:pem_encode([
                {'CertificationRequest', DER, not_encrypted}
            ])
        }}
    catch
        _:_ -> {error, #{
            <<"status">> => 400,
            <<"body">> => <<"CSR requires domain names and an RSA node wallet.">>
        }}
    end.

%% @doc Serve only the public certificate chain; this never starts issuance.
certificate(_Base, _Request, Opts) ->
    case call(hb_name:lookup(runtime_name(hb_opts:address(Opts))),
            certificate, ?CALL_TIMEOUT) of
        {ok, Chain} -> {ok, #{
            <<"status">> => 200,
            <<"content-type">> => <<"application/pem-certificate-chain">>,
            <<"cache-control">> => <<"no-store">>,
            <<"body">> => public_key:pem_encode([
                {'Certificate', DER, not_encrypted} || DER <- Chain
            ])
        }};
        _ -> not_found()
    end.

%% @doc Accept only a signed, scoped, short-lived DNS challenge from our identity.
%% Authorization projects committed fields before any record is interpreted.
challenge(_Base, Request, Opts) ->
    try
        {ok, Wallet} = hb_tls:wallet(Opts),
        {ok, Verified} = lib_meta:authorized(
            hb_opts:committer(#{ <<"priv-wallet">> => Wallet }), Request, Opts
        ),
        #{ <<"type">> := <<"TLS-DNS-Challenge">>,
            <<"method">> := <<"POST">>,
            <<"name">> := Name, <<"token">> := Token,
            <<"expires">> := Expires, <<"action">> := Action } = Verified,
        TLS = hb_tls:config(Opts),
        true = lists:member(Name, [dev_tls_acme:dns_name(Domain)
            || Domain <- maps:get(<<"domains">>, TLS)]),
        true = valid_token(Token, 256),
        Now = erlang:system_time(second),
        true = is_integer(Expires) andalso Expires > Now
            andalso Expires =< Now + 300,
        Operation = case Action of
            <<"put">> ->
                Value = hb_maps:get(<<"value">>, Verified, undefined, Opts),
                true = valid_token(Value, 43) andalso byte_size(Value) =:= 43,
                {publish, {Name, Token}, Value, Expires};
            <<"delete">> -> {delete, {Name, Token}}
        end,
        ok = call(hb_name:lookup(runtime_name(hb_opts:address(Opts))),
            Operation, ?CALL_TIMEOUT),
        {ok, #{ <<"status">> => 200, <<"body">> => <<"OK">> }}
    catch _:_ ->
        {error, #{ <<"status">> => 403,
            <<"body">> => <<"Invalid TLS challenge publication.">> }}
    end.

valid_token(Value, Max) when is_binary(Value), byte_size(Value) > 0,
        byte_size(Value) =< Max ->
    re:run(Value, <<"^[A-Za-z0-9_-]+$">>, [{capture, none}]) =:= match;
valid_token(_, _) -> false.

%% @doc Rewrite an exact HTTP-01 challenge path to a `well-known' resolution.
%% The inbound HTTP request is nested under `request' in the hook message;
%% its replacement resolution is returned under `body'. Other paths return 404.
request(_Base, HookRequest, Opts) ->
    Request = hb_maps:get(<<"request">>, HookRequest, #{}, Opts),
    Path = hb_maps:get(<<"path">>, Request, <<>>, Opts),
    case binary:split(Path, <<"/">>, [global]) of
        [<<>>, <<".well-known">>, <<"acme-challenge">>, Token]
                when Token =/= <<>> ->
            {ok, HookRequest#{ <<"body">> => [
                #{ <<"device">> => <<"tls@1.0">> },
                #{
                    <<"path">> => <<"well-known">>,
                    <<"method">> => hb_maps:get(
                        <<"method">>, Request, <<"GET">>, Opts
                    ),
                    <<"token">> => Token
                }
            ] }};
        _ -> not_found()
    end.

%% @doc Serve an active HTTP-01 authorization for the request's `token'.
%% Only GET is accepted. Missing tokens return 404 and other methods return 405;
%% all responses prohibit caching.
well_known(_Base, Request, Opts) ->
    case hb_maps:get(<<"method">>, Request, <<"GET">>, Opts) of
        <<"GET">> ->
            Token = hb_maps:get(<<"token">>, Request, undefined, Opts),
            case call(hb_name:lookup(runtime_name(server_id(Opts))),
                    {get, Token}, ?CALL_TIMEOUT) of
                {ok, Authorization} -> {ok, #{
                    <<"status">> => 200,
                    <<"content-type">> => <<"text/plain">>,
                    <<"cache-control">> => <<"no-store">>,
                    <<"body">> => Authorization
                }};
                _ -> not_found()
            end;
        _ ->
            {error, #{
                <<"status">> => 405,
                <<"allow">> => <<"GET">>,
                <<"cache-control">> => <<"no-store">>,
                <<"body">> => <<"Method not allowed.">>
            }}
    end.

%% @doc Return a non-cacheable response for an unavailable path or challenge.
not_found() ->
    {error, #{
        <<"status">> => 404,
        <<"cache-control">> => <<"no-store">>,
        <<"body">> => <<"Not found.">>
    }}.

%% @doc Resolve an IN-class DNS question using the node wallet's TLS singleton.
%% Return an authoritative reply message, `not_authorized' outside the challenge
%% zones, or `failure' if the singleton cannot be reached.
dns_resolve(_Base, Request, Opts) ->
    case hb_maps:get(<<"class">>, Request, <<"in">>, Opts) of
        <<"in">> ->
            Name = hb_util:to_lower(hb_maps:get(<<"name">>, Request, <<>>, Opts)),
            Type = hb_maps:get(<<"type">>, Request, <<"txt">>, Opts),
            ServerID = hb_opts:address(Opts),
            case call(hb_name:lookup(runtime_name(ServerID)),
                    {dns, Name, Type}, ?CALL_TIMEOUT) of
                {error, not_authorized} -> {error, not_authorized};
                {error, Reason} -> {failure, hb_util:bin(Reason)};
                Reply -> Reply
            end;
        _ -> {error, not_authorized}
    end.

%% @doc Serve only the configured DNS-01 names, including their NS and SOA.
%% Multiple active tokens share a TXT answer set. Empty answers include an SOA
%% with zero negative-cache lifetime, allowing subsequent challenges to appear.
dns_reply(Name, Type, #{tls := TLS, challenges := Challenges}) ->
    ACME = maps:get(<<"acme">>, TLS),
    Names = [dev_tls_acme:dns_name(Domain) || Domain <- maps:get(<<"domains">>, TLS)],
    case maps:get(<<"challenge-type">>, ACME, <<"http-01">>) =:= <<"dns-01">>
            andalso lists:member(Name, Names) of
        false -> {error, not_authorized};
        true ->
            Nameserver = maps:get(<<"dns-nameserver">>, ACME),
            SOA = dns_record(Name, <<"soa">>, #{
                <<"mname">> => Nameserver,
                <<"rname">> => <<"hostmaster.", Nameserver/binary>>,
                <<"serial">> => 1,
                <<"refresh">> => 3600,
                <<"retry">> => 600,
                <<"expire">> => 86400,
                <<"minimum">> => 0
            }),
            Answers = case Type of
                <<"txt">> ->
                    [dns_record(Name, <<"txt">>, [Value])
                        || {{Zone, _Token}, Record} <- maps:to_list(Challenges),
                            Zone =:= Name,
                            Value <- challenge_value(Record)];
                <<"ns">> -> [dns_record(Name, <<"ns">>, NS) || NS <-
                    maps:get(<<"dns-nameservers">>, ACME, [Nameserver])];
                <<"soa">> -> [SOA];
                _ -> []
            end,
            {ok, #{
                <<"authoritative">> => true,
                <<"answers">> => Answers,
                <<"authority">> => case Answers of [] -> [SOA]; _ -> [] end,
                <<"cache-control">> => <<"no-store">>
            }}
    end.

%% @doc Expired publications are absent even if their cleanup timer is delayed.
challenge_value({Value, Expires}) ->
    case Expires > erlang:system_time(second) of
        true -> [Value];
        false -> []
    end;
challenge_value(Value) -> [Value].

%% @doc Build an IN-class record with zero TTL to avoid caching challenge data.
dns_record(Name, Type, Data) ->
    #{
        <<"name">> => Name,
        <<"type">> => Type,
        <<"class">> => <<"in">>,
        <<"ttl">> => 0,
        <<"data">> => Data
    }.

%% @doc Return a certificate chain only to the listener with its capability.
%% Both request and node message must carry the same private reference. A valid
%% call returns `certificate-chain', or a 500 response if acquisition fails.
obtain(_Base, Request, Opts) ->
    RequestCapability = hb_private:get(
        <<"tls/lifecycle-capability">>, Request, undefined, Opts
    ),
    OptsCapability = hb_private:get(
        <<"tls/lifecycle-capability">>, Opts, not_found, Opts
    ),
    case is_reference(RequestCapability)
            andalso RequestCapability =:= OptsCapability of
        false -> not_found();
        true ->
            case hb_tls:config(Opts) of
                #{ <<"port">> := _ } ->
                    PID = ensure_started(Opts),
                    PID ! options_changed,
                    {ok, #{ <<"status">> => 202 }};
                _ -> case certificate(Opts) of
                {ok, Chain} -> {ok, #{
                    <<"status">> => 200,
                    <<"certificate-chain">> => Chain
                }};
                {error, Reason} -> {error, #{
                    <<"status">> => 500,
                    <<"body">> => hb_util:bin(io_lib:format("~p", [Reason]))
                }}
                end
            end
    end.

%% @doc Load a configured PEM chain, or obtain one through the ACME singleton.
%% An unreadable or invalid file is an error, not a fallback to ACME.
certificate(Opts) ->
    case hb_ao:get(<<"tls/certificate-path">>, Opts, not_found, Opts) of
        not_found -> call(ensure_started(Opts), obtain, infinity);
        Path ->
            case file:read_file(Path) of
                {ok, PEM} -> hb_tls:certificate_chain(PEM);
                {error, Reason} -> {error, {'certificate-file', Reason}}
            end
    end.

%% @doc Find or start the listener's singleton with its own ACME account wallet.
%% Fully load the TLS configuration before passing it to the runtime process.
ensure_started(Opts) ->
    TLS = hb_tls:config(Opts),
    true = is_map(TLS),
    ServerID = server_id(Opts),
    hb_name:singleton(runtime_name(ServerID), fun() ->
        try
            case maps:is_key(<<"port">>, TLS) of
                true -> erlang:monitor(process,
                    ranch_server:get_listener_sup(ServerID));
                false -> ok
            end,
            loop(#{
                server_id => ServerID,
                tls => TLS,
                wallet => case hb_tls:wallet(Opts) of
                    {ok, Wallet} -> Wallet;
                    {error, _} -> undefined
                end,
                opts => Opts,
                account_wallet => ar_wallet:new(),
                challenges => #{},
                operation => idle
            })
        after cowboy:stop_listener({tls, ServerID})
        end
    end).

%% @doc Serialize issuance and renewal while serving active challenge records.
%% Only one issuance operation runs at a time. Stopping the singleton also
%% terminates its linked issuance worker.
loop(State) ->
    receive
        options_changed when map_get(operation, State) =:= idle ->
            case maps:is_key(<<"port">>, maps:get(tls, State)) of
                true -> loop(check(State));
                false -> loop(State)
            end;
        options_changed ->
            case hb_tls:wallet(live_opts(State)) of
                {ok, Wallet} when Wallet =:= map_get(wallet, State) -> ok;
                _ -> cowboy:stop_listener({tls, maps:get(server_id, State)})
            end,
            loop(State);
        {certificate, From, Ref} ->
            From ! {Ref, case maps:get(chain, State, []) of
                [] -> {error, not_found};
                Chain -> {ok, Chain}
            end},
            loop(State);
        {{publish, Token, Value, Expires}, From, Ref} ->
            Challenges = maps:get(challenges, State),
            case maps:get(Token, Challenges, undefined) of
                {Value, Expires} ->
                    From ! {Ref, ok},
                    loop(State);
                _ -> case map_size(Challenges) < 64
                        orelse maps:is_key(Token, Challenges) of
                true ->
                    erlang:send_after(max(1, Expires
                        - erlang:system_time(second)) * 1000,
                        self(), {expire, Token, Expires}),
                    From ! {Ref, ok},
                    loop(State#{challenges => Challenges#{
                        Token => {Value, Expires}
                    }});
                false ->
                    From ! {Ref, {error, 'tls-challenge-capacity'}},
                    loop(State)
                end
            end;
        {expire, Token, Expires} ->
            Challenges = maps:get(challenges, State),
            case maps:find(Token, Challenges) of
                {ok, {_, Expires}} ->
                    loop(State#{challenges => maps:remove(Token, Challenges)});
                _ -> loop(State)
            end;
        {fleet_result, Result} -> loop(fleet_complete(Result, State));
        {begin_issue, Generation} when map_get(operation, State) =:= idle ->
            case maps:get(generation, State, undefined) of
                Generation -> loop(check(State#{ready_to_issue => true}));
                _ -> loop(State)
            end;
        {obtain, From, Ref} when map_get(operation, State) =:= idle ->
            loop(issue({obtain, From, Ref}, State));
        {obtain, From, Ref} ->
            From ! {Ref, {error, 'tls-issuance-in-progress'}},
            loop(State);
        {{put, Token, Authorization}, From, Ref} ->
            From ! {Ref, ok},
            Challenges = maps:get(challenges, State),
            loop(State#{challenges => Challenges#{Token => Authorization}});
        {{delete, Token}, From, Ref} ->
            From ! {Ref, ok},
            loop(State#{challenges => maps:remove(
                Token, maps:get(challenges, State)
            )});
        {{get, Token}, From, Ref} ->
            From ! {Ref, maps:find(Token, maps:get(challenges, State))},
            loop(State);
        {{dns, Name, Type}, From, Ref} ->
            From ! {Ref, dns_reply(Name, Type, State)},
            loop(State);
        {acme_result, Result} when map_get(operation, State) =/= idle ->
            loop(complete(Result, State));
        renew when map_get(operation, State) =:= idle ->
            case maps:is_key(<<"port">>, maps:get(tls, State)) of
                true -> loop(check(State));
                false -> loop(issue(renew, State))
            end;
        renew -> loop(State);
        {renew_after, Delay} -> loop(schedule(Delay, State));
        {'DOWN', _, process, _, _} ->
            exit(shutdown);
        {stop, From} ->
            hb_name:unregister(runtime_name(maps:get(server_id, State))),
            From ! {stopped, self()},
            exit(shutdown);
        _ -> loop(State)
    end.

%% @doc A listener disappearing during a worker reply ends its TLS lifecycle.
live_opts(#{server_id := ServerID}) ->
    try hb_http_server:get_opts(#{ <<"http-server">> => ServerID })
    catch error:badarg -> exit(shutdown)
    end.

%% @doc Check peers in a worker while DNS and certificate reads remain available.
check(State) ->
    Opts = live_opts(State),
    case hb_tls:wallet(Opts) of
        {error, _} ->
            cowboy:stop_listener({tls, maps:get(server_id, State)}),
            schedule(30000, State);
        {ok, Wallet} ->
            Parent = self(),
            spawn_link(fun() ->
                Result = try
                    Loaded = load_state(State#{opts => Opts, wallet => Wallet}),
                    TLS = maps:get(tls, Loaded),
                    Existing = maps:get(chain, Loaded, []),
                    Current = case hb_tls:validate(Wallet, Existing, TLS) of
                        ok -> Existing;
                        _ -> []
                    end,
                    Chain = dev_tls_peer:best(Current, TLS, Wallet, Opts),
                    {ok, Loaded#{chain => Chain}}
                catch _:_ -> {error, 'tls-state-or-peer-check-failed'}
                end,
                Parent ! {fleet_result, Result}
            end),
            State#{operation => checking}
    end.

%% @doc Install a validated candidate, then decide whether issuance is due.
fleet_complete({ok, Candidate0}, State) ->
    Candidate = maps:merge(Candidate0,
        maps:with([challenges, timer, generation], State)),
    Chain = maps:get(chain, Candidate),
    Idle = Candidate#{operation => idle},
    try
        {ok, Wallet} = hb_tls:wallet(live_opts(Idle)),
        Wallet = maps:get(wallet, Idle),
        case Chain of
            [] -> ok;
            _ ->
                ok = save_state(Idle),
                case maps:get(installed, State, []) =:= Chain andalso
                        is_integer(catch ranch:get_port({tls,
                            maps:get(server_id, State)})) of
                    true -> ok;
                    false -> ok = hb_http_server:install_tls(
                        maps:get(opts, Idle), Chain)
                end
        end,
        Installed = Idle#{installed => Chain},
        case renewal_due(Installed) andalso erlang:system_time(millisecond)
                >= maps:get(next_attempt, Idle, 0) of
            true ->
                case maps:get(ready_to_issue, Idle, false) of
                    true -> fleet_issue(Installed#{ready_to_issue => false});
                    false ->
                        Generation = make_ref(),
                        Jitter = maps:get(<<"renewal-jitter">>,
                            maps:get(tls, Idle), 300) * 1000,
                        erlang:send_after(rand:uniform(max(1, Jitter)),
                            self(), {begin_issue, Generation}),
                        Installed#{generation => Generation}
                end;
            false -> schedule(check_delay(Idle), Installed#{ready_to_issue => false})
        end
    catch _:_ ->
        case hb_tls:wallet(live_opts(State)) of
            {ok, CurrentWallet} when CurrentWallet =:= map_get(wallet, Candidate) -> ok;
            _ -> cowboy:stop_listener({tls, maps:get(server_id, State)})
        end,
        retry('tls-certificate-install-failed', State#{operation => idle})
    end;
fleet_complete({issued, {ok, Chain}, Issuing}, State) ->
    case hb_tls:validate(maps:get(wallet, Issuing), Chain,
            maps:get(tls, Issuing)) of
        ok -> fleet_complete({ok, Issuing#{chain => Chain,
            next_attempt => erlang:system_time(millisecond)
                + min(?RENEW_RETRY_MS, hb_tls:certificate_lifetime(Chain) div 3)
        }}, State);
        {error, Reason} ->
            fleet_complete({issued, {error, Reason}, Issuing}, State)
    end;
fleet_complete({issued, {error, Reason}, Issuing}, State) ->
    Delay = case Reason of
        {'acme-retry-after', Until, _} -> max(?RENEW_RETRY_MS,
            Until - erlang:system_time(millisecond));
        _ -> ?RENEW_RETRY_MS
    end,
    Idle = (maps:merge(Issuing, maps:with([challenges], State)))#{operation => idle,
        next_attempt => erlang:system_time(millisecond) + Delay},
    case catch save_state(Idle) of
        ok -> retry(Reason, Idle);
        _ -> retry('tls-state-write-failed', Idle)
    end;
fleet_complete({error, Reason}, State) ->
    retry(Reason, State#{operation => idle}).

%% @doc Persist the retry gate before any account or order request is sent.
fleet_issue(State) ->
    Parent = self(),
    Issuing = State#{next_attempt =>
        erlang:system_time(millisecond) + ?RENEW_RETRY_MS},
    ok = save_state(Issuing),
    spawn_link(fun() ->
        Challenge = fun(Action) -> publish(Action, Parent, Issuing) end,
        Result = dev_tls_acme:obtain(
            maps:get(tls, Issuing), maps:get(wallet, Issuing),
            maps:get(account_wallet, Issuing), Challenge, maps:get(opts, Issuing)
        ),
        Parent ! {fleet_result, {issued, Result, Issuing}}
    end),
    Issuing#{operation => issuing}.

%% @doc Publish locally and to all DNS authorities before requesting validation.
publish({put, {Name, Token} = Key, Value}, Parent, State) ->
    % Leave clock-skew margin inside the authority's five-minute limit.
    Expires = erlang:system_time(second) + 240,
    Record = #{ <<"name">> => Name, <<"token">> => Token,
        <<"value">> => Value, <<"expires">> => Expires,
        <<"action">> => <<"put">> },
    ok = call(Parent, {publish, Key, Value, Expires}, ?CALL_TIMEOUT),
    dev_tls_peer:publish(Record, maps:get(tls, State),
        maps:get(wallet, State), maps:get(opts, State));
publish({delete, {Name, Token} = Key}, Parent, State) ->
    call(Parent, {delete, Key}, ?CALL_TIMEOUT),
    dev_tls_peer:publish(#{ <<"name">> => Name, <<"token">> => Token,
        <<"expires">> => erlang:system_time(second) + 240,
        <<"action">> => <<"delete">> }, maps:get(tls, State),
        maps:get(wallet, State), maps:get(opts, State)).

%% @doc Renew in the final third of the signed lifetime, at most 30 days early.
renewal_due(#{chain := []}) -> true;
renewal_due(#{chain := Chain}) ->
    hb_tls:certificate_expiry(Chain) - erlang:system_time(millisecond)
        =< min(?RENEW_BEFORE_MS, hb_tls:certificate_lifetime(Chain) div 3).

%% @doc Check at least twice before expiry, with a six-hour maximum interval.
check_delay(State) ->
    case maps:get(chain, State, []) of
        [] -> 30000;
        Chain -> max(1000, min(6 * 60 * 60 * 1000,
            (hb_tls:certificate_expiry(Chain)
                - erlang:system_time(millisecond)) div 2))
    end.

%% @doc Load private lifecycle state only after the named identity is installed.
%% The explicit parent directory must already exist on the protected volume.
load_state(State = #{loaded := true, wallet := Wallet,
        context := {Address, _}}) ->
    Address = hb:address(Wallet),
    State;
load_state(State = #{tls := TLS, wallet := Wallet}) ->
    Path = maps:get(<<"state-path">>, TLS),
    true = filelib:is_dir(filename:dirname(Path)),
    true = maps:get(<<"challenge-type">>, maps:get(<<"acme">>, TLS))
        =:= <<"dns-01">>,
    Context = {hb:address(Wallet), maps:get(<<"domains">>, TLS)},
    Saved = case file:read_file(Path) of
        {error, enoent} -> #{context => Context};
        {ok, Binary} when byte_size(Binary) =< 262144 ->
            #{context := Context} = binary_to_term(Binary, [safe]);
        _ -> error('tls-state-unreadable')
    end,
    Loaded = maps:merge(State,
        maps:with([account_wallet, chain, next_attempt], Saved)),
    Loaded#{loaded => true, context => Context}.

%% @doc Atomically retain the account, chain and retry deadline outside AO stores.
save_state(#{tls := TLS} = State) ->
    Path = maps:get(<<"state-path">>, TLS),
    Temp = <<(hb_util:bin(Path))/binary, ".",
        (hb_util:bin(erlang:unique_integer([positive])))/binary>>,
    Saved = maps:with([context, account_wallet, chain, next_attempt], State),
    try
        {ok, File} = file:open(Temp, [write, binary, exclusive, raw]),
        try
            ok = file:change_mode(Temp, 8#600),
            ok = file:write(File, term_to_binary(Saved)),
            ok = file:sync(File)
        after file:close(File)
        end,
        file:rename(Temp, Path)
    after file:delete(Temp)
    end.

%% @doc Run ACME in a linked worker, leaving the singleton free to answer queries.
issue(Operation, State) ->
    Parent = self(),
    spawn_link(fun() ->
        Challenge = fun(Action) -> call(Parent, Action, ?CALL_TIMEOUT) end,
        Parent ! {acme_result, dev_tls_acme:obtain(
            maps:get(tls, State),
            maps:get(wallet, State),
            maps:get(account_wallet, State),
            Challenge,
            maps:get(tls, State)
        )}
    end),
    State#{operation => Operation}.

%% @doc Reply to a startup caller or install a renewed chain on the live listener.
%% Successful issuance schedules renewal; failed renewal schedules a retry.
complete(Result, State = #{operation := {obtain, From, Ref}}) ->
    From ! {Ref, Result},
    case Result of
        {ok, Chain} -> schedule_certificate(Chain, State#{operation => idle});
        {error, _} -> State#{operation => idle}
    end;
complete({ok, Chain}, State = #{operation := renew}) ->
    Idle = State#{operation => idle},
    case hb_tls:install(
        maps:get(server_id, State), maps:get(wallet, State), Chain
    ) of
        ok -> schedule_certificate(Chain, Idle);
        {error, Reason} -> retry(Reason, Idle)
    end;
complete({error, Reason}, State = #{operation := renew}) ->
    retry(Reason, State#{operation => idle}).

%% @doc Renew 30 days before expiry, or halfway through a shorter remaining life.
schedule_certificate(Chain, State) ->
    Remaining = hb_tls:certificate_expiry(Chain)
        - erlang:system_time(millisecond),
    Delay = case Remaining > 2 * ?RENEW_BEFORE_MS of
        true -> Remaining - ?RENEW_BEFORE_MS;
        false when Remaining > 0 -> max(1000, Remaining div 2);
        false -> ?RENEW_RETRY_MS
    end,
    schedule(Delay, State).

%% @doc Schedule renewal, splitting delays that exceed the timer's maximum range.
schedule(Delay, State) when Delay > ?MAX_TIMER_MS ->
    schedule_message(?MAX_TIMER_MS, {renew_after, Delay - ?MAX_TIMER_MS}, State);
schedule(Delay, State) ->
    schedule_message(Delay, renew, State).

%% @doc Keep one scheduled lifecycle check per node.
schedule_message(Delay, Message, State) ->
    case maps:get(timer, State, undefined) of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,
    State#{timer => erlang:send_after(Delay, self(), Message)}.

%% @doc Report a failure; peer checks need not wait for the next allowed order.
retry(Reason, State) ->
    ?event(tls, {acme_renewal_failed, {reason, Reason}}),
    Delay = case maps:is_key(<<"port">>, maps:get(tls, State)) of
        true -> min(?RENEW_RETRY_MS, check_delay(State));
        false -> ?RENEW_RETRY_MS
    end,
    schedule(Delay, State).

%% @doc Call the singleton with a reply reference and the caller's timeout.
%% A missing process or unanswered request returns a TLS runtime error.
call(undefined, _Request, _Timeout) ->
    {error, 'tls-runtime-not-found'};
call(PID, Request, Timeout) ->
    Ref = make_ref(),
    PID ! {Request, self(), Ref},
    receive {Ref, Response} -> Response
    after Timeout -> {error, 'tls-runtime-timeout'}
    end.

%% @doc Scope the singleton's registered name to its HTTP listener.
runtime_name(ServerID) -> {<<"tls@1.0">>, ServerID}.

%% @doc Read the listener identity supplied privately by `hb_http_server'.
server_id(Opts) ->
    hb_private:get(<<"tls/server-id">>, Opts, undefined, Opts).

%%% Tests

%% @doc Route challenge requests without claiming unrelated HTTP paths.
request_hook_test() ->
    Hook = #{ <<"request">> => #{
        <<"path">> => <<"/.well-known/acme-challenge/AbC_123-xy">>,
        <<"method">> => <<"GET">>
    }},
    ?assertMatch({ok, #{ <<"body">> := [_, #{
        <<"path">> := <<"well-known">>, <<"token">> := <<"AbC_123-xy">>
    }] }}, request(#{}, Hook, #{})),
    ?assertMatch({error, #{ <<"status">> := 404 }},
        request(#{}, #{ <<"request">> => #{ <<"path">> => <<"/other">> } }, #{})).

%% @doc Reject certificate acquisition without the listener's private capability.
lifecycle_requires_private_capability_test() ->
    ?assertMatch({error, #{ <<"status">> := 404 }}, obtain(#{}, #{}, #{})).

%% @doc DNS hooks preserve linked questions, concurrent tokens and zone scope.
dns_challenges_test() ->
    Wallet = ar_wallet:new(),
    Store = [hb_test_utils:test_store()],
    hb_store:start(Store),
    Opts0 = #{
        <<"priv-wallet">> => Wallet,
        <<"store">> => Store,
        <<"cache-control">> => [<<"no-cache">>, <<"no-store">>],
        <<"on">> => #{ <<"dns-resolve">> => #{ <<"device">> => <<"tls@1.0">> } }
    },
    {ok, TLSID} = hb_cache:write(#{
        <<"domains">> => [<<"example.test">>, <<"*.example.test">>],
        <<"acme">> => #{
            <<"challenge-type">> => <<"dns-01">>,
            <<"dns-nameserver">> => <<"ns.example.test">>
        }
    }, Opts0),
    Opts = hb_private:set(
        Opts0#{ <<"tls">> => {link, TLSID, #{}} },
        #{ <<"tls">> => #{ <<"server-id">> => hb:address(Wallet) } },
        Opts0
    ),
    PID = ensure_started(Opts),
    Name = <<"_acme-challenge.example.test">>,
    {ok, NameID} = hb_cache:write(<<"_ACME-CHALLENGE.Example.Test">>, Opts),
    Question = #{
        <<"path">> => <<"resolve">>,
        <<"name">> => {link, NameID, #{}},
        <<"type">> => <<"txt">>,
        <<"class">> => <<"in">>
    },
    Resolve = fun(Req) ->
        hb_ao:resolve(#{ <<"device">> => <<"dns@1.0">> }, Req, Opts)
    end,
    try
        ok = call(PID, {put, {Name, <<"first">>}, <<"first-digest">>}, 1000),
        ok = call(PID, {put, {Name, <<"second">>}, <<"second-digest">>}, 1000),
        {ok, Reply} = Resolve(Question),
        ?assert(hb_maps:get(<<"authoritative">>, Reply)),
        Answers = hb_maps:get(<<"answers">>, Reply),
        ?assertEqual([[<<"first-digest">>], [<<"second-digest">>]],
            lists:sort([hb_maps:get(<<"data">>, RR) || RR <- Answers])),
        ?assert(lists:all(fun(RR) -> hb_maps:get(<<"ttl">>, RR) =:= 0 end,
            Answers)),
        ok = call(PID, {delete, {Name, <<"first">>}}, 1000),
        {ok, Remaining} = Resolve(Question),
        ?assertMatch([#{ <<"data">> := [<<"second-digest">>] }],
            hb_maps:get(<<"answers">>, Remaining)),
        ok = call(PID, {delete, {Name, <<"second">>}}, 1000),
        {ok, Empty} = Resolve(Question),
        ?assertEqual([], hb_maps:get(<<"answers">>, Empty)),
        ?assertMatch([#{ <<"type">> := <<"soa">>, <<"ttl">> := 0 }],
            hb_maps:get(<<"authority">>, Empty)),
        {ok, NS} = Resolve(Question#{ <<"type">> => <<"ns">> }),
        ?assertMatch([#{ <<"data">> := <<"ns.example.test">> }],
            hb_maps:get(<<"answers">>, NS)),
        {ok, SOA} = Resolve(Question#{ <<"type">> => <<"soa">> }),
        ?assertMatch([#{ <<"data">> := #{ <<"minimum">> := 0 } }],
            hb_maps:get(<<"answers">>, SOA)),
        ?assertEqual({error, not_authorized}, Resolve(Question#{
            <<"name">> => <<"_acme-challenge.other.test">>
        })),
        ?assertEqual({error, not_authorized}, Resolve(Question#{
            <<"class">> => <<"ch">>
        }))
    after
        PID ! {stop, self()},
        receive {stopped, PID} -> ok end,
        hb_store:reset(Store)
    end.
