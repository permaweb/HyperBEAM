%%% @doc A basic rate limiter device. It is intended for use as a
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
-device_libraries([lib_volatile_ledger]).
-export([request/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_MAX, 1_000).
-define(DEFAULT_MIN, -1_000).
-define(DEFAULT_REQS, 1000).
-define(DEFAULT_PERIOD, 60).

%% @doc `on/request' handler that triggers rate limit counting and returns a
%% 429 status code and response if the limit is exceeded. The response includes
%% a `retry-after' header that indicates the number of seconds the client should
%% wait before making the next request.
-spec request(#{ _ => _ }, #{ request := #{ _ => _ }, _ => _ }, #{ _ => _ }) ->
    {ok, #{ _ => _ }}
    | {error,
        #{ status := integer(), reason := binary(), body := binary(), _ => _ }
    }.
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

%% @doc The ledger of the rate limiter's balances on the node. Every caller
%% starts with `rate_limit_max', and recharges `rate_limit_requests' every
%% `rate_limit_period' seconds. The first caller's options configure the
%% ledger, and all callers share it.
ledger(Opts) ->
    Reqs = hb_opts:get(rate_limit_requests, ?DEFAULT_REQS, Opts),
    Period = hb_opts:get(rate_limit_period, ?DEFAULT_PERIOD, Opts),
    Max = hb_opts:get(rate_limit_max, ?DEFAULT_MAX, Opts),
    Min = hb_opts:get(rate_limit_min, ?DEFAULT_MIN, Opts),
    Exempt = hb_opts:get(rate_limit_exempt, [], Opts),
    #{
        <<"name">> => <<"rate-limit@1.0">>,
        <<"balances">> => #{ Ref => infinity || Ref <- Exempt },
        <<"default">> => Max,
        <<"recharge">> => Reqs / (Period * 1000),
        <<"max">> => Max,
        <<"min">> => Min
    }.

%% @doc Determine the reference of the caller. Presently only the `ip` form
%% may be used to identify the caller.
request_reference(Msg, Opts) -> hb_private:get(<<"ip">>, Msg, Opts).

%% @doc Debit the caller's balance by one request, and check whether it is
%% limited.
is_limited(Reference, Opts) ->
    Balance = lib_volatile_ledger:debit(ledger(Opts), Reference, 1, Opts),
    ?event(
        rate_limit_short,
        {rate_limit_debited, {target, Reference}, {balance, Balance}}
    ),
    case Balance > 0 of
        true -> false;
        false -> {true, Balance}
    end.

%%% Tests

rate_limit_test() ->
    ServerOpts = #{
        <<"rate-limit-requests">> => 2,
        <<"rate-limit-period">> => 1,
        <<"rate-limit-max">> => 2,
        <<"on">> =>
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
        hb_http:get(ServerNode, <<"//id">>, #{})
    ),
    ?debug_wait(100),
    ?assertMatch(
        {ok, _},
        hb_http:get(ServerNode, <<"//id">>, #{})
    ),
    ?debug_wait(100),
    ?assertMatch(
        {error, #{ <<"status">> := 429 }},
        hb_http:get(ServerNode, <<"//id">>, #{})
    ).

rate_limit_reset_test() ->
    ServerOpts = #{
        <<"rate-limit-requests">> => 2,
        <<"rate-limit-period">> => 1,
        <<"rate-limit-max">> => 2,
        <<"rate-limit-min">> => 0,
        <<"rate-limit-exempt">> => [],
        <<"on">> =>
            #{
                <<"request">> =>
                    #{
                        <<"device">> => <<"rate-limit@1.0">>
                    }
            }
    },
    ServerNode = hb_http_server:start_node(ServerOpts),
    ?assertMatch({ok, _}, hb_http:get(ServerNode, <<"//id">>, #{})),
    ?assertMatch({ok, _}, hb_http:get(ServerNode, <<"//id">>, #{})),
    ?assertMatch(
        {error, #{ <<"status">> := 429 }},
        hb_http:get(ServerNode, <<"//id">>, #{})
    ),
    timer:sleep(1_000),
    ?assertMatch({ok, _}, hb_http:get(ServerNode, <<"//id">>, #{})).
