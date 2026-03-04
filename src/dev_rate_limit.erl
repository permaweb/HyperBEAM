%%% @doc A basic rate limiter device. It is intended for use as a `~hook@1.0`
%%% `on/request` handler. It limits the number of requests per minute from a
%%% given IP address, returning a 429 status code and response if the limit is
%%% exceeded.
%%% 
%%% The device can be configured with the following node message options:
%%% 
%%% ```
%%%     rate_limit: The maximum number of requests per minute from a given IP
%%%                 address. Default: 1,000.
%%%     rate_limit_exempt: A list of peer IDs that are exempt from the rate
%%%                 limit. Default: [].
%%% ```
-module(dev_rate_limit).
-export([request/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(LOOKUP_TIMEOUT, 1000).
-define(DEFAULT_RATE_LIMIT, 1000).
-define(DEFAULT_BUCKET_TIME, 60).

request(_, Msg, Opts) ->
    ?event(rate_limit, {request, {msg, Msg}}),
    Reference = request_reference(hb_maps:get(<<"request">>, Msg, #{}, Opts), Opts),
    case check_limit(Reference, Opts) of
        true ->
            ?event(rate_limit, {rate_limit_exceeded, {caller, Reference}}),
            % Transform the given request into a request to return a 429 status
            % code and response.
            {ok,
                #{
                    <<"body">> =>
                        [
                            #{
                                <<"status">> => 429,
                                <<"reason">> => <<"rate-limited">>,
                                <<"body">> => <<"Rate limit exceeded.">>
                            }
                        ]
                }
            };
        false ->
            ?event(rate_limit, {rate_limit_allowed, {caller, Reference}}),
            {ok, Msg}
    end.

server_id(Opts) ->
    {?MODULE, hb_util:human_id(hb_opts:get(priv_wallet, undefined, Opts))}.

%% @doc Determine the reference of the caller.
request_reference(Msg, Opts) ->
    hb_maps:get(<<"ao-peer">>, Msg, undefined, Opts).

check_limit(IP, Opts) ->
    PID = ensure_rate_limiter_started(Opts),
    PID ! {request, Self = self(), IP},
    receive
        {rate_limit_result, Result} -> Result
    after ?LOOKUP_TIMEOUT ->
        ?event(warning, {rate_limit_timeout, restarting}),
        hb_name:unregister(server_id(Opts)),
        check_limit(IP, Opts)
    end.

ensure_rate_limiter_started(Opts) ->
    ServerID = server_id(Opts),
    case hb_name:lookup({?MODULE, ServerID}) of
        PID when is_pid(PID) -> PID;
        undefined ->
            spawn(
                fun() ->
                    hb_name:register({?MODULE, ServerID}, self()),
                    Limit =
                        hb_opts:get(
                            rate_limit,
                            ?DEFAULT_RATE_LIMIT,
                            Opts
                        ),
                    BucketTime =
                        hb_opts:get(
                            rate_limit_bucket_time,
                            ?DEFAULT_BUCKET_TIME,
                            Opts
                        ),
                    ExemptPeers = hb_opts:get(rate_limit_exempt, [], Opts),
                    ?event(
                        rate_limit,
                        {started_rate_limiter,
                            {server_id, ServerID},
                            {limit, Limit},
                            {exempt_peers, ExemptPeers}
                        }
                    ),
                    server_loop(
                        #{
                            limit => Limit,
                            peers => #{ IP => infinity || IP <- ExemptPeers },
                            bucket_time => BucketTime
                        }
                    )
                end
            )
    end.

server_loop(State) ->
    receive
        {request, Self, IP} ->
            NewState = increment(IP, State),
            Self ! {rate_limit_result, is_limited(IP, NewState)},
            server_loop(NewState)
    end.

increment(IP, #{ bucket_time := BucketTime } = State) ->
    increment(IP, erlang:system_time(second) div BucketTime, State).
increment(IP, Bucket, S = #{ peers := Peers }) ->
    case maps:get(IP, Peers, #{}) of
        infinity -> S;
        #{ since := Bucket, count := Count } ->
            S#{ peers => Peers#{ IP => #{ since => Bucket, count => Count + 1 }}};
        _ ->
            S#{ peers => Peers#{ IP => #{ since => Bucket, count => 1 }}}
    end.

%% @doc Check if the IP is limited. Assumes the IP is in the state (added by
%% increment/2).
is_limited(IP, #{ peers := Peers }) when map_get(IP, Peers) =:= infinity -> false;
is_limited(IP, #{ limit := Limit, peers := Peers }) ->
    maps:get(count, maps:get(IP, Peers, #{}), 0) > Limit.

%%% Tests

rate_limit_test() ->
    ServerOpts = #{
        rate_limit => 2,
        rate_limit_exempt => [],
        rate_limit_bucket_time => 10_000,
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
    ?assertMatch(
        {ok, _},
        hb_http:get(ServerNode, <<"id">>, #{})
    ),
    ?assertMatch(
        {error, #{ <<"status">> := 429 }},
        hb_http:get(ServerNode, <<"id">>, #{})
    ).

rate_limit_reset_test() ->
    ServerOpts = #{
        rate_limit => 2,
        rate_limit_exempt => [],
        rate_limit_bucket_time => 2,
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
    timer:sleep(2_000),
    ?assertMatch({ok, _}, hb_http:get(ServerNode, <<"id">>, #{})).