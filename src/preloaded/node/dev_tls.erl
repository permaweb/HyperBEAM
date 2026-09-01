%%% @doc Node-wallet TLS and ACME renewal device.
-module(dev_tls).
-export([info/1, request/3, well_known/3, obtain/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(CALL_TIMEOUT, 5000).
-define(MAX_TIMER_MS, 16#ffffffff).
-define(RENEW_BEFORE_MS, 30 * 24 * 60 * 60 * 1000).
-define(RENEW_RETRY_MS, 60 * 60 * 1000).

info(_) ->
    #{ exports => [<<"request">>, <<"well-known">>, <<"obtain">>] }.

%% @doc Route the exact HTTP-01 path through the normal AO-Core hook.
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

%% @doc Serve an active key authorization from the singleton.
well_known(_Base, Request, Opts) ->
    case hb_maps:get(<<"method">>, Request, <<"GET">>, Opts) of
        <<"GET">> ->
            Token = hb_maps:get(<<"token">>, Request, undefined, Opts),
            case call(hb_name:lookup(runtime_name(server_id(Opts))),
                    {get, Token}, ?CALL_TIMEOUT) of
                {ok, Authorization} -> {ok, #{
                    <<"status">> => 200,
                    <<"content-type">> => <<"text/plain">>,
                    <<"cache-control">> => [<<"no-store">>],
                    <<"body">> => Authorization
                }};
                _ -> not_found()
            end;
        _ ->
            {error, #{
                <<"status">> => 405,
                <<"allow">> => <<"GET">>,
                <<"cache-control">> => [<<"no-store">>],
                <<"body">> => <<"Method not allowed.">>
            }}
    end.

not_found() ->
    {error, #{
        <<"status">> => 404,
        <<"cache-control">> => [<<"no-store">>],
        <<"body">> => <<"Not found.">>
    }}.

%% @doc Obtain the boot certificate behind an unforgeable private capability.
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
            %% Bounded above the ACME client's own 180s issuance deadline.
            case call(ensure_started(Opts), obtain, 200000) of
                {ok, Chain} -> {ok, #{
                    <<"status">> => 200,
                    <<"certificate-chain">> => Chain
                }};
                {error, Reason} -> {error, #{
                    <<"status">> => 500,
                    <<"body">> => hb_util:bin(io_lib:format("~p", [Reason]))
                }}
            end
    end.

ensure_started(Opts) ->
    TLS = hb_tls:config(Opts),
    true = is_map(TLS),
    ServerID = server_id(Opts),
    hb_name:singleton(runtime_name(ServerID), fun() ->
        process_flag(trap_exit, true),
        loop(#{
            server_id => ServerID,
            tls => TLS,
            wallet => hb_opts:get(priv_wallet, no_viable_wallet, Opts),
            account_wallet => ar_wallet:new(),
            challenges => #{},
            operation => idle
        })
    end).

loop(State) ->
    receive
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
        {acme_result, Result} when map_get(operation, State) =/= idle ->
            loop(complete(Result, State));
        renew when map_get(operation, State) =:= idle ->
            loop(renew(State));
        renew -> loop(State);
        {renew_after, Delay} -> loop(schedule(Delay, State));
        {'EXIT', _, normal} -> loop(State);
        {'EXIT', _, Reason} when map_get(operation, State) =/= idle ->
            loop(complete({error, {'tls-worker-died', Reason}}, State));
        {stop, From} ->
            hb_name:unregister(runtime_name(maps:get(server_id, State))),
            From ! {stopped, self()},
            exit(shutdown);
        _ -> loop(State)
    end.

%% @doc A pending chain means the last install failed after a successful
%% issuance: retry the install without spending another issuance against the
%% CA's rate limits. Re-issue only if there is no pending chain, or it will
%% not outlive the next retry cycle.
renew(State) ->
    Chain = maps:get(pending_chain, State, undefined),
    Usable = Chain =/= undefined andalso
        hb_tls:certificate_expiry(Chain)
            - erlang:system_time(millisecond) > ?RENEW_RETRY_MS,
    case Usable of
        true -> complete({ok, Chain}, State#{operation => renew});
        false -> issue(renew, State)
    end.

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

complete(Result, State = #{operation := {obtain, From, Ref}}) ->
    From ! {Ref, Result},
    case Result of
        {ok, Chain} -> schedule_certificate(Chain, State#{operation => idle});
        {error, Reason} -> retry(Reason, State#{operation => idle})
    end;
complete({ok, Chain}, State = #{operation := renew}) ->
    Idle = State#{operation => idle},
    case hb_tls:install(
        maps:get(server_id, State), maps:get(wallet, State), Chain
    ) of
        ok -> schedule_certificate(Chain, maps:remove(pending_chain, Idle));
        {error, Reason} -> retry(Reason, Idle#{pending_chain => Chain})
    end;
complete({error, Reason}, State = #{operation := renew}) ->
    retry(Reason, State#{operation => idle}).

schedule_certificate(Chain, State) ->
    Remaining = hb_tls:certificate_expiry(Chain)
        - erlang:system_time(millisecond),
    Delay = case Remaining > 2 * ?RENEW_BEFORE_MS of
        true -> Remaining - ?RENEW_BEFORE_MS;
        false when Remaining > 0 -> max(1000, Remaining div 2);
        false -> ?RENEW_RETRY_MS
    end,
    schedule(Delay, State).

schedule(Delay, State) when Delay > ?MAX_TIMER_MS ->
    erlang:send_after(?MAX_TIMER_MS, self(),
        {renew_after, Delay - ?MAX_TIMER_MS}),
    State;
schedule(Delay, State) ->
    erlang:send_after(Delay, self(), renew),
    State.

retry(Reason, State) ->
    ?event(tls, {acme_renewal_failed, {reason, Reason}}),
    schedule(?RENEW_RETRY_MS, State).

call(undefined, _Request, _Timeout) ->
    {error, 'tls-runtime-not-found'};
call(PID, Request, Timeout) ->
    Ref = make_ref(),
    PID ! {Request, self(), Ref},
    receive {Ref, Response} -> Response
    after Timeout -> {error, 'tls-runtime-timeout'}
    end.

runtime_name(ServerID) -> {<<"tls@1.0">>, ServerID}.

server_id(Opts) ->
    hb_private:get(<<"tls/server-id">>, Opts, undefined, Opts).

%%% Tests

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

lifecycle_requires_private_capability_test() ->
    ?assertMatch({error, #{ <<"status">> := 404 }}, obtain(#{}, #{}, #{})).
