%%% @doc The supervisor for the gun HTTP client wrapper.
-module(hb_http_client_sup).
-behaviour(supervisor).
-export([start_link/1, init/1]).

%% The number of milliseconds the supervisor gives every process for shutdown.
-ifdef(DEBUG).
-define(SHUTDOWN_TIMEOUT, 10000).
-else.
-define(SHUTDOWN_TIMEOUT, 30000).
-endif.

-define(CHILD(I, Type, Opts), {I, {I, start_link, Opts}, permanent, ?SHUTDOWN_TIMEOUT, Type, [I]}).


start_link(Opts) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Opts).


init(Opts) ->
    {ok, {{one_for_one, 5, 10}, [?CHILD(hb_http_client, worker, Opts)]}}.
