%%%-------------------------------------------------------------------
%% @doc The main HyperBEAM application module.
%% @end
%%%-------------------------------------------------------------------

-module(hb_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("include/hb.hrl").

start(_StartType, _StartArgs) ->
    hb:init(),
    {ok, Supervisor} = hb_sup:start_link(),
    ok = hb_name:start(),
    _TimestampServer = ar_timestamp:start(),
    {ok, _Listener, ServerID} = hb_http_server:start_application(),
    {ok, Supervisor, ServerID}.

stop(ServerID) ->
    cowboy:stop_listener(ServerID).
