%%% @doc The supervisor for the gun HTTP client worker pool.
-module(hb_http_client_sup).
-behaviour(supervisor).
-export([start_link/1, init/1]).

-define(DEFAULT_HTTP_CLIENT_POOL_SIZE, 32).
-define(DEFAULT_HTTP_CLIENT_POOL_MAX_OVERFLOW, 16).

start_link(Opts) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Opts).

init(Opts) ->
    PoolName = hb_http_client_pool,
    PoolSize =
        hb_opts:get(
            http_client_pool_size,
            ?DEFAULT_HTTP_CLIENT_POOL_SIZE,
            Opts
        ),
    MaxOverflow =
        hb_opts:get(
            http_client_pool_max_overflow,
            ?DEFAULT_HTTP_CLIENT_POOL_MAX_OVERFLOW,
            Opts
        ),
    PoolOpts = [
        {name, {local, PoolName}},
        {worker_module, hb_http_client},
        {size, PoolSize},
        {max_overflow, MaxOverflow}
    ],
    {ok,
        {
            {one_for_one, 5, 10},
            [poolboy:child_spec(PoolName, PoolOpts, Opts)]
        }
    }.
