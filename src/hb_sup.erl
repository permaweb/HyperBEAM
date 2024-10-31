%%%-------------------------------------------------------------------
%% @doc The HyperBEAM top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(hb_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                intensity => 0,
                period => 1},

    RocksDBOptions = hb_opts:get(local_store),
    ChildSpecs = [
        #{id => hb_store_rocksdb, start => {hb_store_rocksdb, start_link, [RocksDBOptions]}}
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions