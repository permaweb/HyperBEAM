%%% @doc A harness that keeps a HyperBEAM node in sync with live Arweave
%%% mainnet, validating every block, and writes an auditable record of what it
%%% did.
%%%
%%% This is evidence tooling, not production code -- but it drives the node the
%%% way production does. `~cron@1.0/every' calls `~arweave@2.9/sync' on a
%%% timer, exactly as an operator would configure it; the harness itself only
%%% bootstraps, starts the cron, and then watches from outside. Nothing here
%%% validates anything, and nothing here can make the node appear more in sync
%%% than it is.
%%%
%%% Observation is deliberately independent of the thing observed. The tip is
%%% read back through the node's own HTTP API rather than from process state,
%%% and compared against a peer's `/info'. Every line of the log is a fact
%%% observed at a point in time; nothing is summarised, because a summary that
%%% disagrees with the lines above it is how a run gets mistaken for a success.
%%%
%%% Run it via `scripts/arweave-live-sync.sh'.
-module(hb_arweave_live_sync).
-export([run/1]).
-include("include/hb.hrl").

%% @doc How often `~cron@1.0' should call `sync'. Mainnet produces a block
%% every ~122 seconds; polling somewhat faster keeps the observed lag small
%% without meaningfully loading the peers.
-define(SYNC_INTERVAL, <<"30-seconds">>).

%% @doc How often to compare the local tip against the network's.
-define(OBSERVE_INTERVAL, 30_000).

%% @doc Bootstrap, start the cron, then observe until `duration' seconds have
%% elapsed. `Args' is a map of binary keys: `duration', `log', `peers'.
run(Args) ->
    Log = maps:get(<<"log">>, Args, <<"evidence/live-sync.log">>),
    Duration = hb_util:int(maps:get(<<"duration">>, Args, <<"3900">>)),
    {ok, File} = file:open(Log, [append, {encoding, utf8}]),
    Opts = node_opts(Args),
    Node = hb_http_server:start_node(Opts),
    log(File, start, #{
        <<"node">> => Node,
        <<"duration-seconds">> => Duration,
        <<"peers">> => hb_opts:get(arweave_untrusted_peers, [], Opts),
        <<"randomx-mode">> => hb_opts:get(arweave_randomx_mode, <<"light">>, Opts),
        <<"schedulers">> => erlang:system_info(schedulers)
    }),
    Deadline = erlang:monotonic_time(millisecond) + (Duration * 1000),
    case bootstrap(File, Node, Opts) of
        ok ->
            ok = start_cron(File, Node, Opts),
            observe(File, Node, Opts, Deadline),
            log(File, finished, #{}),
            file:close(File);
        {error, Error} ->
            log(File, bootstrap_failed, #{ <<"error">> => fmt(Error) }),
            file:close(File),
            halt(1)
    end.

%%% Internal functions.

%% @doc Build the node message. Peers are the only thing a run needs to vary;
%% everything else is left at its default so the run exercises the
%% configuration an operator would actually get.
node_opts(Args) ->
    Peers =
        binary:split(
            maps:get(<<"peers">>, Args, <<"http://tip-1.arweave.xyz:1984">>),
            <<",">>,
            [global]
        ),
    #{
        <<"store">> =>
            [
                #{
                    <<"store-module">> => hb_store_lmdb,
                    <<"name">> =>
                        maps:get(<<"store">>, Args, <<"cache-arweave-live/lmdb">>)
                }
            ],
        <<"priv-wallet">> => ar_wallet:new(),
        <<"arweave-untrusted-peers">> => Peers,
        <<"arweave-trusted-peers">> => Peers,
        % The VDF chain dominates validation and verification runs through the
        % OpenSSL back-end, so the default `schedulers div 2' leaves the node
        % near parity and unable to close a backlog. Measured here: 18-51 s per
        % block against a ~122 s interval.
        <<"arweave-vdf-threads">> => erlang:system_info(schedulers) - 2,
        % A pass acts on the peer tips it read when it started, so a long batch
        % acts on a stale view and sees a reorg late. Five blocks is ~3 minutes
        % at the measured rate: enough to close a backlog faster than mainnet
        % extends it, while re-reading the tips often enough to react.
        <<"arweave-sync-batch">> => 5
    }.

%% @doc Establish the initial chain state. This is the only moment the node
%% trusts anything, so it is recorded in full -- including how long it took,
%% which is dominated by verifying the VDF chain from the checkpoint.
%%
%% A node restarted against a store it already bootstrapped is answered
%% `already-bootstrapped', which is success rather than failure: the chain the
%% guard is protecting is exactly the one this run wants to extend. Treating it
%% as an error made a supervised restart impossible -- the node would come back,
%% be told it already had a chain, and halt.
bootstrap(File, Node, Opts) ->
    Started = erlang:monotonic_time(millisecond),
    Result = hb_http:get(Node, <<"/~arweave@2.9/bootstrap">>, Opts),
    Elapsed = erlang:monotonic_time(millisecond) - Started,
    case Result of
        {ok, State} ->
            log(File, bootstrapped, #{
                <<"elapsed-ms">> => Elapsed,
                <<"height">> => hb_ao:get(<<"block/height">>, State, 0, Opts),
                <<"indep-hash">> =>
                    hb_ao:get(<<"block/indep-hash">>, State, <<>>, Opts)
            }),
            ok;
        {error, Body} when is_map(Body) ->
            resume(File, hb_maps:get(<<"message">>, Body, <<>>, Opts), Body);
        Error ->
            {error, Error}
    end.

%% @doc An already-bootstrapped node resumes; anything else is a real failure.
resume(File, <<"already-bootstrapped">>, _Body) ->
    log(File, resumed, #{
        <<"note">> =>
            <<"the node already had a validated chain; extending it">>
    }),
    ok;
resume(_File, _Message, Body) ->
    {error, Body}.

%% @doc Point `~cron@1.0/every' at `~arweave@2.9/sync'. This is the production
%% wiring, not a harness loop -- the node keeps itself up to date from here on
%% and the harness only watches.
start_cron(File, Node, Opts) ->
    Result =
        hb_http:post(
            Node,
            <<"/~cron@1.0/every">>,
            hb_message:commit(
                #{
                    <<"cron-path">> => <<"/~arweave@2.9/sync">>,
                    <<"interval">> => ?SYNC_INTERVAL
                },
                Opts
            ),
            Opts
        ),
    log(File, cron_started, #{
        <<"interval">> => ?SYNC_INTERVAL,
        <<"cron-path">> => <<"/~arweave@2.9/sync">>,
        <<"result">> => fmt(Result)
    }),
    ok.

%% @doc Watch until the deadline, recording how the locally-validated tip
%% compares to what the network reports.
observe(File, Node, Opts, Deadline) ->
    case erlang:monotonic_time(millisecond) of
        Now when Now >= Deadline ->
            ok;
        _ ->
            compare_tips(File, Node, Opts),
            timer:sleep(?OBSERVE_INTERVAL),
            observe(File, Node, Opts, Deadline)
    end.

%% @doc Read the node's tip over its own HTTP API and compare it against a
%% peer. Both values are recorded even when they agree -- the point of the log
%% is that the comparison can be re-checked, not that it was made.
compare_tips(File, Node, Opts) ->
    Local = hb_http:get(Node, <<"/~arweave@2.9/tip">>, Opts),
    case {Local, network_tip(Opts)} of
        {{ok, State}, {ok, NetworkHeight, NetworkHash}} ->
            LocalHeight =
                hb_util:int(hb_ao:get(<<"block/height">>, State, 0, Opts)),
            LocalHash = hb_ao:get(<<"block/indep-hash">>, State, <<>>, Opts),
            log(File, tip_comparison, #{
                <<"local-height">> => LocalHeight,
                <<"network-height">> => NetworkHeight,
                <<"lag-blocks">> => NetworkHeight - LocalHeight,
                <<"local-indep-hash">> => LocalHash,
                <<"network-indep-hash">> => NetworkHash,
                <<"hashes-match">> => LocalHash =:= NetworkHash,
                % Whether this state was produced with the account tree
                % attached. A node validating without it still checks the
                % consensus layer, but not the wallet-list root, the account
                % transition, or any per-transaction rule -- so a run that does
                % not record this cannot be read as evidence of full
                % validation.
                <<"accounts-checked">> =>
                    hb_ao:get(<<"accounts-checked">>, State, false, Opts)
            });
        {LocalError, {ok, NetworkHeight, _}} ->
            log(File, tip_read_failed, #{
                <<"error">> => fmt(LocalError),
                <<"network-height">> => NetworkHeight
            });
        {_, NetworkError} ->
            log(File, network_tip_failed, #{ <<"error">> => fmt(NetworkError) })
    end.

%% @doc Ask a peer what it believes the tip to be. This is an observation of
%% the network, never an input to validation.
%%
%% An Arweave peer answers `/info' with a JSON document, so the response body is
%% decoded here rather than read as message keys -- the peer is not an AO-Core
%% node and its reply carries no `structured@1.0' typing.
network_tip(Opts) ->
    [Peer | _] = hb_opts:get(arweave_untrusted_peers, [], Opts),
    case hb_http:get(Peer, <<"/info">>, Opts) of
        {ok, Response} ->
            Info = hb_json:decode(hb_ao:get(<<"body">>, Response, <<"{}">>, Opts)),
            {ok,
                hb_util:int(hb_maps:get(<<"height">>, Info, 0, Opts)),
                hb_maps:get(<<"current">>, Info, <<>>, Opts)
            };
        Error ->
            {error, Error}
    end.

%% @doc Append one event to the log as a single JSON line, so the run can be
%% replayed and checked mechanically rather than read for vibes.
log(File, Event, Fields) ->
    Line =
        hb_json:encode(
            Fields#{
                <<"event">> => hb_util:bin(Event),
                <<"at">> =>
                    hb_util:bin(
                        calendar:system_time_to_rfc3339(
                            erlang:system_time(second),
                            [{offset, "Z"}]
                        )
                    )
            }
        ),
    ok = file:write(File, [Line, $\n]),
    io:format("~s~n", [Line]).

%% @doc Render an arbitrary term for the log without letting it crash the run.
fmt(Term) ->
    hb_util:bin(io_lib:format("~0p", [Term])).
