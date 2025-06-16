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
    hb_sup:start_link(),
    ok = dev_scheduler_registry:start(),
    _TimestampServer = ar_timestamp:start(),
    % Start notification manager if notify_device is configured
    case hb_opts:get(notify_device, undefined, #{}) of
        undefined -> ok;
        _ -> dev_notify:start_notification_manager()
    end,
    {ok, _} = hb_http_server:start().

stop(_State) ->
    % Stop notification manager if running
    dev_notify:stop_notification_manager(),
    ok.