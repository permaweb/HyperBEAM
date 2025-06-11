-module(hb_sup).
-behaviour(supervisor).
-export([start_link/0, start_link/1, init/1]).
-define(SERVER, ?MODULE).
-include("include/hb.hrl").


start_link() ->
    start_link(#{}).


start_link(Opts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Opts).


%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init(Opts) ->
    SupFlags = #{
                 strategy => one_for_all,
                 intensity => 0,
                 period => 1
                },
    StoreChildren = store_children(hb_opts:get(store, [], Opts)),
    GunChild =
        #{
          id => hb_http_client,
          start => {hb_http_client, start_link, [Opts]},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [hb_http_client]
         },
    {ok, {SupFlags, [GunChild | StoreChildren]}}.


%% @doc Generate a child spec for stores in the given Opts.
store_children(Store) when not is_list(Store) ->
    store_children([Store]);
store_children([]) -> [];
store_children([RocksDBOpts = #{<<"store-module">> := hb_store_rocksdb} | Rest]) ->
    [#{
       id => hb_store_rocksdb,
       start => {hb_store_rocksdb, start_link, [RocksDBOpts]}
      }] ++ store_children(Rest);
store_children([_ | Rest]) ->
    store_children(Rest).
