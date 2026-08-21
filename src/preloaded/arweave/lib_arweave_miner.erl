%%% @doc The mining session: the one long-running thing a miner is.
%%%
%%% Everything else in `~arweave-mining@2.9' is a bounded resolution over an
%%% explicit step. This is what keeps calling them. A verifiable delay function
%%% produces one step a second and cannot be hurried, so a miner is defined by
%%% following that production rather than by any amount of work it does per
%%% call: the search that matters is the one running on the step that exists
%%% now. There is no bounded resolution that expresses "keep up", so this is a
%%% process, and it is the only new one.
%%%
%%% It holds three things and derives everything else. The first is the
%%% subscription to `lib_arweave_vdf_timeline', which pushes each step the
%%% moment it is computed. The second is the parent this node is building on,
%%% re-read each step so that a block landing moves the search onto it. The
%%% third is a bounded set of in-flight searches, one per partition per step,
%%% with a queue in front of it that drops the oldest work first.
%%%
%%% That last is the whole of the back-pressure design, and it is deliberate.
%%% Steps arrive on a clock this node does not control, so a miner that cannot
%%% search a step within a step falls behind for as long as it runs. Queueing
%%% the backlog would search older and older steps; dropping the newest would
%%% search the wrong ones. Dropping the oldest keeps the search on the newest
%%% steps the machine can afford and reports how many it could not, which is a
%%% number an operator can act on -- more partitions than the host can read, or
%%% a step rate it cannot match.
%%%
%%% A tip change costs almost nothing, and that is a property of the protocol
%%% rather than of this design. H0 is taken over the step's output, the
%%% partition, the *parent's* seed, the address and the packing difficulty, and
%%% the seed rotates only at an entropy reset line -- so a block arriving inside
%%% an epoch changes the difficulty, the height, the weave size and two other
%%% numbers, and not one byte of what is read from disk or hashed. In-flight
%%% searches are left running across it.
-module(lib_arweave_miner).
-export([start/2, stop/1, status/1]).
-include("include/hb.hrl").

%%% How many searches may be in flight at once when the node message names no
%%% limit. One search is one partition at one step: a range read and up to a
%%% range's worth of hashes, so the useful bound is what the disks under the
%%% storage modules can serve rather than what the schedulers can run.
-define(DEFAULT_WORKERS, 4).

%%% How long a control call waits. The session answers between steps and never
%%% inside one, and a step it is dispatching is a handful of message sends, so
%%% anything approaching this is a session that is stuck rather than busy.
-define(CONTROL_TIMEOUT, 5000).

%%% How many searches may be waiting. Beyond this the oldest are dropped: a
%%% step older than the queue is one the chain has moved past.
-define(QUEUE_MULTIPLE, 2).

%% @doc Start mining, and keep mining. One call.
%%
%% Answers with what the session is doing rather than waiting for it to do
%% anything: the point of the call is that the caller is released and the
%% mining continues. Calling it again on a running session re-reads the node
%% message and carries on, so an operator changing an option does not have to
%% stop first.
start(Request, Opts) ->
    call(fun(Reply, Ref) -> {start, Reply, Ref, Request, Opts} end, Opts).

%% @doc Stop searching. The session stays, holding nothing, and starts again on
%% the next `start'. In-flight searches are left to finish rather than killed:
%% they are pure, they are bounded, and a killed one would leave its own
%% worker's storage read half-done.
stop(Opts) ->
    call(fun(Reply, Ref) -> {stop, Reply, Ref} end, Opts).

%% @doc What the session is doing: the step it last saw, the step it last
%% searched, how far behind that leaves it, and the counters since it started.
status(Opts) ->
    call(fun(Reply, Ref) -> {status, Reply, Ref} end, Opts).

%%% Internal functions.

%% @doc The session process, resolved by name so that one node has one.
%%
%% `hb_name' is BEAM-global, so the name carries what distinguishes one node
%% from another in this VM, exactly as the timeline's does.
session(Opts) ->
    hb_name:singleton(name(Opts), fun miner/0).

name(Opts) ->
    {
        arweave_miner,
        case hb_opts:get(<<"http-server">>, not_found, Opts) of
            not_found -> hb_opts:get(store, [], Opts);
            Server -> hb_util:bin(Server)
        end
    }.

%% @doc Ask the session something and wait for its answer.
%%
%% The reply alias is dropped whatever happens, so an answer arriving after the
%% caller gave up does not sit in a mailbox that has moved on -- the same care
%% the timeline takes for the same reason.
call(Message, Opts) ->
    Session = session(Opts),
    Ref = erlang:monitor(process, Session),
    Reply = alias([reply]),
    Session ! Message(Reply, Ref),
    Answer = await(Session, Ref),
    unalias(Reply),
    Answer.

await(Session, Ref) ->
    receive
        {answered, Ref, Answer} ->
            erlang:demonitor(Ref, [flush]),
            {ok, Answer};
        {'DOWN', Ref, process, Session, Reason} ->
            {error,
                #{
                    <<"status">> => 500,
                    <<"message">> => <<"miner-down">>,
                    <<"detail">> => hb_util:bin(io_lib:format("~p", [Reason]))
                }
            }
    after ?CONTROL_TIMEOUT ->
        erlang:demonitor(Ref, [flush]),
        {error,
            #{
                <<"status">> => 500,
                <<"message">> => <<"miner-unresponsive">>,
                <<"detail">> =>
                    <<"The mining session did not answer a control message.">>
            }
        }
    end.

%% @doc The session loop. It has nothing to do until it is started, and once
%% started it is driven entirely by the steps the timeline pushes at it.
miner() ->
    miner(idle()).

miner(State) ->
    receive
        Message -> miner(handle(Message, State))
    end.

%% @doc A session holding nothing.
idle() ->
    #{
        running => false,
        opts => #{},
        request => #{},
        parent => none,
        epoch => none,
        partitions => [],
        step => none,
        searched => none,
        queue => [],
        workers => #{},
        limit => ?DEFAULT_WORKERS,
        started => none,
        counts => counts()
    }.

counts() ->
    #{
        <<"steps">> => 0,
        <<"dispatched">> => 0,
        <<"completed">> => 0,
        <<"dropped">> => 0,
        <<"nonces">> => 0,
        <<"partials">> => 0,
        <<"solutions">> => 0,
        <<"blocks">> => 0,
        <<"stale">> => 0,
        <<"errors">> => 0
    }.

%% @doc Take a control message, a step, or a search's result.
handle({start, From, Ref, Request, Opts}, State) ->
    Started = running(Request, Opts, State),
    From ! {answered, Ref, report(Started)},
    Started;
handle({stop, From, Ref}, State) ->
    Stopped = State#{ running => false, queue => [] },
    ?event(arweave_mining, {miner_stopped, {counts, maps:get(counts, State)}}),
    From ! {answered, Ref, report(Stopped)},
    Stopped;
handle({status, From, Ref}, State) ->
    From ! {answered, Ref, report(State)},
    State;
handle({vdf_step, Step, Output, Seed, Difficulty}, State) ->
    {Newest, Latest, Epoch, Passed} =
        latest(Step, Output, {Seed, Difficulty}, 0),
    stepped(Newest, Latest, Epoch, passed(Passed, State));
handle({searched, Pid, Result}, State) ->
    dispatch(completed(Pid, Result, State));
handle({'DOWN', _Ref, process, Pid, Reason}, State = #{ workers := Workers }) ->
    case maps:is_key(Pid, Workers) of
        true -> dispatch(died(Pid, Reason, State));
        false -> State
    end;
handle(_Message, State) ->
    State.

%% @doc Take the newest step already waiting, and say how many were passed
%% over to reach it.
%%
%% Deriving a step's session costs a materialised parent and a retarget
%% calculation, and doing that for a step the chain has already left behind is
%% work spent on a search that cannot win. So the mailbox is drained first and
%% only the newest step is derived. At mainnet's one step a second nothing is
%% ever passed over; when something is, `dropped' says so, and that is the
%% number that says this machine is behind.
latest(Step, Output, Epoch, Passed) ->
    receive
        {vdf_step, Next, NextOutput, NextSeed, NextDifficulty} ->
            latest(Next, NextOutput, {NextSeed, NextDifficulty}, Passed + 1)
    after 0 ->
        {Step, Output, Epoch, Passed}
    end.

%% @doc Count the steps that were seen but never derived.
passed(0, State) ->
    State;
passed(Passed, State) ->
    add(<<"dropped">>, Passed, add(<<"steps">>, Passed, State)).

%% @doc Begin, or begin again with a new node message.
%%
%% Subscribing more than once would deliver each step more than once, so a
%% session already running only takes the new request.
running(Request, Opts, State = #{ running := true }) ->
    State#{
        request => Request,
        opts => Opts,
        limit => limit(Request, Opts)
    };
running(Request, Opts, State) ->
    lib_arweave_vdf_timeline:subscribe(self(), Opts),
    ?event(arweave_mining,
        {miner_started, {limit, limit(Request, Opts)}}),
    primed(
        lib_arweave_vdf_timeline:head(Opts),
        State#{
            running => true,
            request => Request,
            opts => Opts,
            limit => limit(Request, Opts),
            partitions => partitions(Request, Opts),
            started => os:system_time(second),
            counts => counts()
        }
    ).

%% @doc The partitions to search at every step, asked for once.
%%
%% They are a property of the weave source rather than of a step, and asking
%% per step would put a resolution in the loop that takes steps -- which is the
%% one place in this design that must never block. A node whose modules change
%% is a node that is restarted or `start'ed again.
partitions(Request, Opts) ->
    Resolved =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave-mining@2.9">> },
            Request#{ <<"path">> => <<"partitions">> },
            Opts
        ),
    case Resolved of
        {ok, Answer} ->
            hb_util:message_to_ordered_list(
                hb_maps:get(<<"partitions">>, Answer, #{}, Opts), Opts);
        {error, Error} ->
            ?event(arweave_mining, {miner_partitions_failed, {error, Error}}),
            []
    end.

%% @doc Begin on the step the timeline already holds, rather than on the next
%% one it computes.
%%
%% Subscribing only says what happens next, and what happens next is a step the
%% timeline may not owe for a while: it runs ahead of its anchor and then waits
%% to be re-anchored, so a miner started between blocks would sit idle until the
%% next one landed. The head is the step to search now, and it reaches the
%% session by the same path every other step does.
primed(not_running, State) ->
    State;
primed(Head, State) ->
    self() !
        {
            vdf_step,
            hb_util:int(maps:get(<<"global-step-number">>, Head)),
            maps:get(<<"nonce-limiter-output">>, Head),
            maps:get(<<"seed">>, Head),
            maps:get(<<"difficulty">>, Head)
        },
    State.

%% @doc How many searches may run at once.
limit(Request, Opts) ->
    case hb_maps:get(<<"workers">>, Request, [], Opts) of
        [] ->
            hb_util:int(
                hb_opts:get(<<"arweave-mining-workers">>, ?DEFAULT_WORKERS,
                    Opts));
        Named ->
            hb_util:int(Named)
    end.

%% @doc A step arrived. Queue one search per partition and fill the workers.
%%
%% The session message is rebuilt for each step because the seed data a step
%% falls under depends on the step -- a range crossing an entropy reset line
%% takes the epoch after it -- and because rebuilding it is how a block that
%% has landed since the last step reaches the search.
stepped(_Step, _Output, _Epoch, State = #{ running := false }) ->
    State;
stepped(Step, Output, Epoch, State = #{ partitions := Partitions }) ->
    Counted = count(<<"steps">>, State#{ step => Step }),
    dispatch(
        queued(
            [{Step, Output, Partition} || Partition <- Partitions],
            retired(Epoch, Counted)
        )
    ).

%% @doc Drop queued work belonging to a superseded epoch.
%%
%% The epoch is the pair a search is valid under -- the seed and the VDF
%% difficulty -- and it changes only at an entropy reset line or a VDF
%% retarget. Work queued under the old pair recalls ranges the new one does not
%% address, so it is not merely late, it is wrong.
retired(Epoch, State = #{ epoch := Epoch }) ->
    State;
retired(Epoch, State = #{ epoch := none }) ->
    State#{ epoch => Epoch };
retired(Epoch, State = #{ queue := Queue }) ->
    ?event(arweave_mining,
        {miner_epoch_changed, {dropped, length(Queue)}}),
    add(<<"dropped">>, length(Queue), State#{ epoch => Epoch, queue => [] }).

%% @doc Add work, dropping the oldest when the queue is longer than the workers
%% can drain. What is dropped is a step the chain has moved past.
queued(Work, State = #{ queue := Queue, limit := Limit }) ->
    Bound = Limit * ?QUEUE_MULTIPLE,
    Wanted = Queue ++ Work,
    case length(Wanted) - Bound of
        Over when Over =< 0 ->
            State#{ queue => Wanted };
        Over ->
            ?event(arweave_mining, {miner_behind, {dropped, Over}}),
            add(<<"dropped">>, Over,
                State#{ queue => lists:nthtail(Over, Wanted) })
    end.

%% @doc Start searches until the workers are full or the queue is empty.
dispatch(State = #{ running := false }) ->
    State;
dispatch(State = #{ queue := [] }) ->
    State;
dispatch(State = #{ workers := Workers, limit := Limit })
        when map_size(Workers) >= Limit ->
    State;
dispatch(State = #{ queue := [{Step, Output, Partition} | Rest] }) ->
    dispatch(started(Step, Output, Partition, State#{ queue => Rest })).

%% @doc Search one partition at one step, in a process of its own.
%%
%% A search is a bounded, idempotent resolution of the device's own `solve'
%% key, so it can run anywhere and nothing here needs to know how mining works.
%% It runs beside the session rather than in it because the session must stay
%% able to take the next step while this one is being searched.
started(Step, Output, Partition,
        State = #{ workers := Workers, opts := Opts, request := Request }) ->
    Miner = self(),
    {Pid, _Ref} =
        spawn_monitor(
            fun() ->
                Miner !
                    {
                        searched,
                        self(),
                        hb_ao:resolve(
                            #{ <<"device">> => <<"arweave-mining@2.9">> },
                            Request#{
                                <<"path">> => <<"search">>,
                                <<"global-step-number">> => Step,
                                <<"nonce-limiter-output">> =>
                                    hb_util:encode(Output),
                                <<"partition-number">> => Partition
                            },
                            Opts
                        )
                    }
            end
        ),
    count(<<"dispatched">>,
        State#{ workers => Workers#{ Pid => {Step, Partition} } }).

%% @doc Record a finished search and act on what it found.
completed(Pid, Result, State = #{ workers := Workers }) ->
    At = maps:get(Pid, Workers, {none, none}),
    answered(
        Result,
        At,
        count(<<"completed">>,
            State#{ workers => maps:remove(Pid, Workers) })
    ).

answered({ok, Answer}, {Step, Partition}, State = #{ opts := Opts }) ->
    found(
        hb_maps:get(<<"solution">>, Answer, false, Opts),
        hb_maps:get(<<"block">>, Answer, not_found, Opts),
        {Step, Partition},
        staled(
            hb_maps:get(<<"stale">>, Answer, false, Opts),
            add(
                <<"nonces">>,
                hb_util:int(
                    hb_maps:get(<<"nonces-searched">>, Answer, 0, Opts)),
                State#{ searched => newest_searched(Step, State) }
            )
        )
    );
answered({error, Error}, _At, State) ->
    ?event(arweave_mining, {miner_search_failed, {error, Error}}),
    count(<<"errors">>, State).

%% @doc The newest step a search has finished on. Searches finish out of order,
%% so this only ever moves forward.
newest_searched(Step, #{ searched := Searched })
        when is_integer(Step), is_integer(Searched) ->
    max(Step, Searched);
newest_searched(Step, _State) when is_integer(Step) ->
    Step;
newest_searched(_Step, #{ searched := Searched }) ->
    Searched.

%% @doc A search whose process died rather than answering.
died(Pid, Reason, State = #{ workers := Workers }) ->
    ?event(arweave_mining,
        {miner_search_died, {at, maps:get(Pid, Workers, unknown)},
            {reason, Reason}}),
    count(<<"errors">>, State#{ workers => maps:remove(Pid, Workers) }).

%% @doc Count a step the parent has already passed.
%%
%% A timeline running behind the chain offers nothing but these, so it is a
%% counter rather than an error: `stale' climbing while `nonces' does not is a
%% node whose nonce limiter cannot keep pace with the network, which is a fact
%% about the machine and the one an operator needs told.
staled(false, State) ->
    State;
staled(true, State) ->
    count(<<"stale">>, State).

%% @doc Count what a search found. The block was built, checked and handed to
%% the `arweave-mined-block' hook by the device, in the worker; nothing here
%% publishes it, because a block leaving this node is an act with consequences
%% outside it and the hook is where an operator attaches that.
%%
%% `solutions' is what the search found and `blocks' is what survived being
%% built and checked. They differ when a solution cannot become a block, which
%% is a fault worth seeing rather than a rarity worth hiding.
found(false, _Block, _At, State) ->
    State;
found(true, not_found, {Step, Partition}, State) ->
    ?event(arweave_mining,
        {miner_solution_refused, {step, Step}, {partition, Partition}}),
    count(<<"errors">>, count(<<"solutions">>, State));
found(true, _Block, {Step, Partition}, State) ->
    ?event(arweave_mining,
        {miner_solution, {step, Step}, {partition, Partition}}),
    count(<<"blocks">>, count(<<"solutions">>, State)).

%% @doc What the session is doing, as a message.
report(State = #{ counts := Counts }) ->
    #{
        running := Running,
        step := Step,
        searched := Searched,
        queue := Queue,
        workers := Workers,
        limit := Limit,
        started := Started
    } = State,
    Counts#{
        <<"running">> => Running,
        <<"newest-step">> => none_to_zero(Step),
        <<"searched-step">> => none_to_zero(Searched),
        <<"behind">> => behind(Step, Searched),
        <<"queued">> => length(Queue),
        <<"in-flight">> => map_size(Workers),
        <<"workers">> => Limit,
        <<"uptime">> => uptime(Started)
    }.

%% @doc How many steps lie between the newest seen and the newest searched.
%% This is the number that says whether the machine is keeping up.
behind(Step, Searched) when is_integer(Step), is_integer(Searched) ->
    max(0, Step - Searched);
behind(_Step, _Searched) ->
    0.

uptime(none) -> 0;
uptime(Started) -> os:system_time(second) - Started.

none_to_zero(none) -> 0;
none_to_zero(Value) -> Value.

count(Key, State) ->
    add(Key, 1, State).

add(Key, By, State = #{ counts := Counts }) ->
    State#{ counts => Counts#{ Key => maps:get(Key, Counts, 0) + By } }.
