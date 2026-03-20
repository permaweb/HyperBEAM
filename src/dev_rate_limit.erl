%%% @doc A basic rate limiter device. It is intended for use as a `~hook@1.0`
%%% `on/request` handler. It limits the number of requests per time period from a
%%% given IP address, returning a 429 status code and response if the limit is
%%% exceeded.
%%% 
%%% The device can be configured with the following node message options:
%%% 
%%% ```
%%%     rate_limit_requests: The maximum number of requests per period from a
%%%                          given user.
%%%                          Default: 1000.
%%%     rate_limit_period:   The rate at which peer's fully recharge balances.
%%%                          Default: 60 (unit: seconds).
%%%     rate_limit_max:      The maximum `balance' that a peer may hold.
%%%                          Default: 1000.
%%%     rate_limit_min:      The minimum `balance' that a peer may hold.
%%%                          Default: -1000.
%%%     rate_limit_exempt: A list of peer IDs that are exempt from the limit.
%%%                          Default: [].
%%% ```
%%% 
%%% Notably, the `balance` of a user -- in terms of their available limit -- may
%%% become _negative_ if they continue to make calls even after exceeding their
%%% limit. The effect of this is that users that make too many requests to the
%%% server repeatedly simply receive no further service. The `rate_limit_min`
%%% option can be used to specify the minimum balance that users will hit. Any
%%% further requests are rejected but do not diminish their balance further.
-module(dev_rate_limit).
-export([request/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(LOOKUP_TIMEOUT, 1000).
-define(DEFAULT_MAX, 1_000).
-define(DEFAULT_MIN, -1_000).
-define(DEFAULT_REQS, 1000).
-define(DEFAULT_PERIOD, 60).

%% @doc `on/request' handler that triggers rate limit counting and returns a
%% 429 status code and response if the limit is exceeded. The response includes
%% a `retry-after' header that indicates the number of seconds the client should
%% wait before making the next request.
request(_, Msg, Opts) ->
    ?event(rate_limit, {request, {msg, Msg}}),
    Reference = request_reference(hb_maps:get(<<"request">>, Msg, #{}, Opts), Opts),
    case is_limited(Reference, Opts) of
        {true, Balance} ->
            ?event(
                rate_limit,
                {rate_limit_exceeded, {caller, Reference}, {balance, Balance}}
            ),
            RechargeRate =
                hb_opts:get(rate_limit_requests, ?DEFAULT_REQS, Opts) /
                hb_opts:get(rate_limit_period, ?DEFAULT_PERIOD, Opts),
            RawRetryAfter = ceil(abs(Balance) / RechargeRate), % ...seconds
            % If the node config specifies a `min` balance of `0`, callers may
            % have a non-negative balance but still be rate-limited. In this case,
            % we bump the `retry-after` to 1 second so as not to confuse the
            % caller.
            RetryAfter =
                if RawRetryAfter =< 0.0 -> 1;
                true -> RawRetryAfter
                end,
            RetryAfterBin = hb_util:bin(RetryAfter),
            ?event(
                rate_limit,
                {rate_limit_exceeded,
                    {caller, Reference},
                    {balance, Balance},
                    {retry_after, RetryAfterBin}
                }
            ),
            % Transform the given request into a request to return a 429 status
            % code and response.
            {error,
                #{
                    <<"status">> => 429,
                    <<"reason">> => <<"rate-limited">>,
                    <<"body">> => <<"Rate limit exceeded.">>,
                    <<"retry-after">> => RetryAfterBin
                }
            };
        false ->
            ?event(rate_limit, {rate_limit_allowed, {caller, Reference}}),
            {ok, Msg}
    end.

%% @doc The singleton ID of the rate limiter server. This allows us to run 
%% multiple rate limiters on the same node if needed, each with its own
%% configuration, but with all of the callers sharing the same rate limiter
%% server.
server_id(Opts) ->
    {?MODULE, hb_util:human_id(hb_opts:get(priv_wallet, undefined, Opts))}.

%% @doc Determine the reference of the caller. Presently only the `ip` form
%% may be used to identify the caller.
request_reference(Msg, Opts) -> hb_private:get(<<"ip">>, Msg, Opts).

%% @doc Check if the caller is limited according to the current state of the
%% rate limiter server.
is_limited(Reference, Opts) ->
    PID = ensure_rate_limiter_started(Opts),
    PID ! {request, self(), Reference},
    receive
        {incremented, Balance} when Balance > 0 -> false;
        {incremented, Balance} when Balance =< 0 -> {true, Balance}
    after ?LOOKUP_TIMEOUT ->
        ?event(warning, {rate_limit_timeout, restarting}),
        hb_name:unregister(server_id(Opts)),
        is_limited(Reference, Opts)
    end.

%% @doc Ensure that the rate limiter server is started and return the PID of
%% the server. In the event of two instanteous spawns, one of the new processes 
%% will fail with an error and the other will succeed. The effect to the caller
%% is the same: A rate limiter is available to query.
ensure_rate_limiter_started(Opts) ->
    ServerID = server_id(Opts),
    hb_name:singleton(
        ServerID,
        fun() -> start_server(ServerID, Opts) end
    ).

start_server(ServerID, Opts) ->
    % Exit the process if we cannot register the server ID.
    Reqs = hb_opts:get(rate_limit_requests, ?DEFAULT_REQS, Opts),
    Period = hb_opts:get(rate_limit_period, ?DEFAULT_PERIOD, Opts),
    Max = hb_opts:get(rate_limit_max, ?DEFAULT_MAX, Opts),
    Min = hb_opts:get(rate_limit_min, ?DEFAULT_MIN, Opts),
    Exempt = hb_opts:get(rate_limit_exempt, [], Opts),
    ?event(
        rate_limit,
        {started_rate_limiter,
            {server_id, ServerID},
            {reqs, Reqs},
            {period, Period},
            {max, Max},
            {min, Min},
            {exempt, Exempt}
        }
    ),
    server_loop(
        #{
            reqs => Reqs,
            period => Period,
            max => Max,
            min => Min,
            peers => #{ Ref => infinity || Ref <- Exempt }
        }
    ).

%% @doc The main loop of the rate limiter server. Only responds to two messages:
%% - `{request, Self, Reference}': Debit the account of the given reference by 1.
%% - `{balance, PID, Reference}': Return the current balance of the given reference.
%% The `balance` call is not presently used, but seems sensible to have.
server_loop(State) ->
    ?event({server_loop, {state, State}}),
    receive
        {request, PID, Reference} ->
            NewState = debit(Reference, 1, State, Now = erlang:system_time(millisecond)),
            ?event({state_after_debit, NewState}),
            Balance = account_balance(Reference, NewState, Now),
            ?event(
                rate_limit_short,
                {rate_limit_debited, {target, Reference}, {balance, Balance}}
            ),
            PID ! {incremented, Balance},
            server_loop(NewState);
        {balance, PID, Reference} ->
            PID ! {balance, account_balance(Reference, State)},
            server_loop(State)
    end.

%% @doc Debit the account of the given reference by the given quantity.
debit(Ref, Amount, State = #{ peers := Peers, min := Min }, Now) ->
    case account_balance(Ref, State, Now) of
        infinity -> State;
        Balance ->
            State#{
                peers =>
                    Peers#{
                        Ref =>
                            #{
                                balance => max(Min, Balance - Amount),
                                last => Now
                            }
                    }
            }
    end.

%% @doc Calculate the current balance for a user, including unused capacity 
%% accrued since the last interaction.
account_balance(Reference, State) ->
    account_balance(Reference, State, erlang:system_time(millisecond)).
account_balance(
        Reference,
        #{ max := Max, reqs := Reqs, period := Period, peers := Peers },
        Time
    ) ->
    ?event({account_balance, {target, Reference}, {peers, Peers}, {time, Time}}),
    case maps:get(Reference, Peers, not_found) of
        infinity -> infinity;
        not_found -> Max;
        #{ balance := Balance, last := LastInteraction } ->
            RechargeRate = Reqs / (Period * 1000),
            RechargedSinceLast = (Time - LastInteraction) * RechargeRate,
            min(Max, Balance + RechargedSinceLast)
    end.

%%% Tests

rate_limit_test() ->
    ServerOpts = #{
        rate_limit_requests => 2,
        rate_limit_period => 1,
        rate_limit_max => 2,
        on =>
            #{
                <<"request">> =>
                    #{
                        <<"device">> => <<"rate-limit@1.0">>
                    }
            }
    },
    ServerNode = hb_http_server:start_node(ServerOpts),
    ?assertMatch(
        {ok, _},
        hb_http:get(ServerNode, <<"id">>, #{})
    ),
    ?debug_wait(100),
    ?assertMatch(
        {ok, _},
        hb_http:get(ServerNode, <<"id">>, #{})
    ),
    ?debug_wait(100),
    ?assertMatch(
        {error, #{ <<"status">> := 429 }},
        hb_http:get(ServerNode, <<"id">>, #{})
    ).

rate_limit_reset_test() ->
    ServerOpts = #{
        rate_limit_requests => 2,
        rate_limit_period => 1,
        rate_limit_max => 2,
        rate_limit_min => 0,
        rate_limit_exempt => [],
        on =>
            #{
                <<"request">> =>
                    #{
                        <<"device">> => <<"rate-limit@1.0">>
                    }
            }
    },
    ServerNode = hb_http_server:start_node(ServerOpts),
    ?assertMatch({ok, _}, hb_http:get(ServerNode, <<"id">>, #{})),
    ?assertMatch({ok, _}, hb_http:get(ServerNode, <<"id">>, #{})),
    ?assertMatch(
        {error, #{ <<"status">> := 429 }},
        hb_http:get(ServerNode, <<"id">>, #{})
    ),
    timer:sleep(1_000),
    ?assertMatch({ok, _}, hb_http:get(ServerNode, <<"id">>, #{})).