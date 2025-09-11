%%%-------------------------------------------------------------------
%% @doc The main HyperBEAM application module.
%% @end
%%%-------------------------------------------------------------------

-module(hb_app).

-behaviour(application).

-export([start/2, prep_stop/1, stop/1]).

-include("include/hb.hrl").

start(_StartType, _StartArgs) ->
    hb:init(),
    hb_sup:start_link(),
    ok = dev_scheduler_registry:start(),
    _TimestampServer = ar_timestamp:start(),
    {ok, _} = hb_http_server:start().

prep_stop(State) ->
    maybe
        {ok, Opts} ?= find_lmdb_store(),
        hb_store_lmdb:flush(Opts) 
    end,
    State.

stop(_State) ->
    maybe
        {ok, Opts} ?= find_lmdb_store(),
        hb_store_lmdb:stop(Opts) 
    end,
    ok.

find_lmdb_store() ->
    Stores = maps:get(store, hb_opts:default_message()),
    Pred = fun(S) -> maps:get(<<"store-module">>, S) == hb_store_lmdb end,
    case lists:search(Pred, Stores) of
        {value, Opts} -> {ok, Opts};
        false -> not_found
    end.