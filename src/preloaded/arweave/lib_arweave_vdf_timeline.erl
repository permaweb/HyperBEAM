%%% @doc The node's own run of the nonce limiter, kept alongside the weave so
%%% that a block's steps are mostly already computed when the block arrives.
%%%
%%% Verifying a block's VDF costs the real time that block covers, divided by
%%% the workers it is spread over, because the block supplies each step's output
%%% and the steps can therefore be checked independently. Computing the timeline
%%% has no such parallelism -- that is what a delay function is -- so this
%%% process runs one chain on one thread and, on this hardware, at slightly
%%% under one step per second.
%%%
%%% Running just under real time is enough, and this is the point of the whole
%%% arrangement. The timeline is not required to stay ahead of the weave; it is
%%% required to leave only a small remainder. A block arriving `N' seconds after
%%% the one before it finds `N * (1 - rate)' steps uncomputed, and those are
%%% verified the old way, in parallel, at many steps per second. At 0.9 steps/s
%%% a two-minute block leaves about twelve steps to check rather than a hundred
%%% and twenty. Each block then re-anchors the timeline on its own output, so
%%% the shortfall is bounded per block rather than accumulated.
%%%
%%% Nothing here can make a block valid that was not. A stored step is a
%%% recomputation this node performed earlier from an anchor it had already
%%% validated, so accepting a block's step because it equals the stored one is
%%% exactly accepting it because it recomputed -- only sooner. Every other case
%%% falls back: an unknown step, a mismatch, a seed or difficulty this timeline
%%% did not compute under, a timeline that is not running, or one that fails to
%%% answer in time all yield `#{}', and the caller verifies the whole range as
%%% it always did. The timeline is a way to have done the work already, never a
%%% way to skip it.
%%%
%%% It computes only within one entropy epoch. At a reset line the seed the next
%%% steps mix in comes from a block this node has not seen, so the timeline
%%% stops at the line and waits to be re-anchored rather than guessing.
-module(lib_arweave_vdf_timeline).
-export([snapshot/5, advance/6, head/1, subscribe/2]).
-include("include/hb.hrl").
-include("include/ar_vdf.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%% How far past its anchor the timeline will run before it stops and waits for
%%% a block. A node whose peers have gone quiet should not compute forever.
-define(MAX_AHEAD, 4096).
%%% How long a caller waits for the timeline's answer.
%%%
%%% This has to exceed one step, and by a margin. The timeline handles messages
%%% between steps but cannot interrupt one: a step is a single NIF call, about
%%% 1.1 s at the difficulty mainnet runs today, so a request arriving just after
%%% a step began waits out the rest of it. A second is therefore exactly the
%%% wrong value -- it expires just short of the answer, and the timeline reads
%%% as absent on almost every block while working perfectly.
%%%
%%% The cost of waiting is half a step on average, once per block, against the
%%% tens of seconds the answer saves. The cost of expiring early is the whole
%%% benefit.
-define(ANSWER_TIMEOUT, 4000).

%% @doc Return the computed outputs and checkpoint buffers for a range. Each
%% value is `{Output, CheckpointBuffer}', in the form returned by
%% `ar_vdf:compute/3'. The caller uses one snapshot for both the step chain and
%% the final step's checkpoints.
snapshot(Start, End, Seed, Difficulty, Opts) ->
    Timeline = hb_name:singleton(name(Opts), fun timeline/0),
    snapshot(Timeline, Start, End, Seed, Difficulty, ?ANSWER_TIMEOUT).

%% @doc Ask one timeline process for a snapshot, bounding how long its current
%% step may delay the answer. The reply alias drops an answer that arrives after
%% the caller has fallen back to ordinary verification.
snapshot(Timeline, Start, End, Seed, Difficulty, Timeout) ->
    Ref = erlang:monitor(process, Timeline),
    Reply = alias([reply]),
    Timeline ! {known, Reply, Ref, Start, End, Seed, Difficulty},
    receive
        {known, Ref, Steps} ->
            unalias(Reply),
            erlang:demonitor(Ref, [flush]),
            Steps;
        {'DOWN', Ref, process, Timeline, _Reason} ->
            unalias(Reply),
            #{}
    after Timeout ->
        unalias(Reply),
        erlang:demonitor(Ref, [flush]),
        #{}
    end.

%% @doc Re-anchor the timeline on a validated block's own output and let it run
%% forward from there.
%%
%% Called only after the block has passed `verify-chain' in full, so the anchor
%% is a step this node has checked rather than one a peer asserted. `Reset' is
%% the step at which this epoch ends, or `none'.
advance(Seed, Difficulty, StepNumber, Output, Reset, Opts) ->
    Timeline = hb_name:singleton(name(Opts), fun timeline/0),
    Timeline ! {advance, Seed, Difficulty, StepNumber, Output, Reset},
    ok.

%% @doc The newest step this timeline holds, with the epoch it computed it
%% under. A miner searches what the timeline has already run, so this is the
%% question it asks when it starts and after any gap.
%%
%% `not_running' is a timeline with no anchor -- one this node has not yet
%% validated a block into -- and is a state of the node rather than a failure.
head(Opts) ->
    Timeline = hb_name:singleton(name(Opts), fun timeline/0),
    Ref = erlang:monitor(process, Timeline),
    Reply = alias([reply]),
    Timeline ! {head, Reply, Ref},
    receive
        {head, Ref, Head} ->
            unalias(Reply),
            erlang:demonitor(Ref, [flush]),
            Head;
        {'DOWN', Ref, process, Timeline, _Reason} ->
            unalias(Reply),
            not_running
    after ?ANSWER_TIMEOUT ->
        unalias(Reply),
        erlang:demonitor(Ref, [flush]),
        not_running
    end.

%% @doc Send `{vdf_step, StepNumber, Output, Seed, Difficulty}' to `Pid' as each
%% step is computed, until it dies.
%%
%% A miner cannot poll for this. The timeline handles messages only between
%% steps and a step is one uninterruptible NIF call, so a poll either waits out
%% the remainder of a step or misses one; and the interval it would have to poll
%% at is the interval it is waiting for. Pushing costs the timeline one send per
%% step and delivers each step the moment it exists.
%%
%% The epoch travels with the step because a miner's session key is `{seed,
%% difficulty}': a subscriber that read those separately could pair a step with
%% the epoch that succeeded it.
subscribe(Pid, Opts) ->
    Timeline = hb_name:singleton(name(Opts), fun timeline/0),
    Timeline ! {subscribe, Pid},
    ok.

%%% Internal functions.

%% @doc The timeline's own name. `hb_name' is BEAM-global, so an HTTP node gets
%% its own timeline. Direct resolutions without one are isolated by store.
name(Opts) ->
    {
        arweave_vdf_timeline,
        case hb_opts:get(<<"http-server">>, not_found, Opts) of
            not_found -> hb_opts:get(store, [], Opts);
            Server -> hb_util:bin(Server)
        end
    }.

%% @doc The process loop. With no anchor there is nothing to compute, so it
%% waits; with one it computes the next step whenever no message is pending.
timeline() ->
    timeline(
        #{
            anchor => none,
            computing => false,
            steps => #{},
            subscribers => #{}
        }
    ).

timeline(State = #{ computing := false }) ->
    receive
        Message -> timeline(handle(Message, State))
    end;
timeline(State) ->
    receive
        Message -> timeline(handle(Message, State))
    after 0 ->
        timeline(compute_next(State))
    end.

%% @doc Answer what is held, take a new anchor, or add a subscriber.
handle({known, From, Ref, Start, End, Seed, Difficulty}, State) ->
    From ! {known, Ref, held(Start, End, Seed, Difficulty, State)},
    State;
handle({head, From, Ref}, State) ->
    From ! {head, Ref, newest(State)},
    State;
handle({subscribe, Pid}, State = #{ subscribers := Subscribers }) ->
    State#{ subscribers => Subscribers#{ Pid => erlang:monitor(process, Pid) } };
handle({'DOWN', _Ref, process, Pid, _Reason},
        State = #{ subscribers := Subscribers }) ->
    State#{ subscribers => maps:remove(Pid, Subscribers) };
handle({advance, Seed, Difficulty, StepNumber, Output, Reset}, State) ->
    anchored(Seed, Difficulty, StepNumber, Output, Reset, State);
handle(_Message, State) ->
    State.

%% @doc The newest step held, with the epoch it was computed under.
newest(#{ anchor := none }) ->
    not_running;
newest(State = #{ at := At, head := Head, seed := Seed,
        difficulty := Difficulty }) ->
    #{
        <<"global-step-number">> => Head,
        <<"nonce-limiter-output">> => previous_output(State),
        <<"seed">> => Seed,
        <<"difficulty">> => Difficulty,
        <<"anchored-at">> => At
    }.

%% @doc Take a new anchor, keeping every step the anchor does not invalidate.
%%
%% A block re-anchors the timeline on its own output, and the steps above that
%% output are the ones this process just spent real seconds computing. Dropping
%% them costs the whole run again at one step a second, on every block, forever
%% -- and for a miner following the timeline it is worse than a cost, because
%% the steps it would have searched next stop existing. They are kept when the
%% anchor lands inside the run this timeline is already on: same epoch, and the
%% step it names is the one already computed there.
%%
%% Anything else -- a different seed or difficulty, an output this timeline did
%% not compute, an anchor above the head -- is a different chain, and the steps
%% held do not belong to it.
anchored(Seed, Difficulty, StepNumber, Output, Reset,
        State = #{ seed := Seed, difficulty := Difficulty, head := Head,
            steps := Steps })
        when Head > StepNumber ->
    case maps:get(StepNumber, Steps, not_found) of
        {Output, _Checkpoints} ->
            State#{
                anchor => Output,
                at => StepNumber,
                reset => Reset,
                computing => true,
                steps =>
                    maps:filter(fun(Step, _) -> Step > StepNumber end, Steps)
            };
        _Other ->
            fresh(Seed, Difficulty, StepNumber, Output, Reset, State)
    end;
anchored(Seed, Difficulty, StepNumber, Output, Reset, State) ->
    fresh(Seed, Difficulty, StepNumber, Output, Reset, State).

%% @doc Start a new run at an anchor, holding nothing.
fresh(Seed, Difficulty, StepNumber, Output, Reset,
        #{ subscribers := Subscribers }) ->
    #{
        anchor => Output,
        at => StepNumber,
        head => StepNumber,
        seed => Seed,
        difficulty => Difficulty,
        kernel => kernel(Difficulty, #{}),
        reset => bounded_reset(Reset, StepNumber),
        computing => true,
        steps => #{},
        subscribers => Subscribers
    }.

%% @doc The step a run must stop at, whatever window its caller looked over.
%%
%% A caller passes the reset line it found between the block's step and some
%% horizon of its own. If that horizon is shorter than `?MAX_AHEAD' the caller
%% answers `none' for a line this run would reach, and the timeline would
%% compute straight through it without mixing the seed -- producing outputs
%% that are not the chain's, silently, for as long as the run lasts. Deriving
%% the line here as well makes that impossible to arrange from outside.
bounded_reset(none, At) ->
    ar_nonce_limiter:get_entropy_reset_point(At, At + ?MAX_AHEAD);
bounded_reset(Reset, _At) ->
    Reset.

%% @doc The subset of the computed steps a caller may use: those inside the
%% range asked for, and only when the epoch matches.
held(Start, End, Seed, Difficulty,
        #{ seed := Seed, difficulty := Difficulty, steps := Steps }) ->
    maps:filter(fun(Step, _) -> Step > Start andalso Step =< End end, Steps);
held(_Start, _End, _Seed, _Difficulty, _State) ->
    #{}.

%% @doc Compute one step past the newest held, tell the subscribers, and stop
%% at the epoch's end or once far enough ahead of the anchor.
compute_next(State = #{ head := Head, steps := Steps }) ->
    Next = Head + 1,
    case stop_at(Next, State) of
        true ->
            % The run is over, not the timeline. What it computed is still the
            % newest this node has, and a miner is still entitled to search it
            % -- so the state stays and only the computing stops. Conflating
            % the two would make a timeline that had reached an entropy reset
            % line indistinguishable from one that had never seen a block.
            State#{ computing => false };
        false ->
            #{ seed := Seed, difficulty := Difficulty, kernel := Kernel } =
                State,
            {ok, Output, Checkpoints} =
                Kernel(Next, previous_output(State), Difficulty),
            told(
                {vdf_step, Next, Output, Seed, Difficulty},
                State#{
                    head => Next,
                    steps => Steps#{ Next => {Output, Checkpoints} }
                }
            )
    end.

%% @doc Hand a step to every subscriber. A dead one is dropped when its monitor
%% fires, so a send here is never to a pid this process has not seen die.
told(Message, State = #{ subscribers := Subscribers }) ->
    maps:foreach(fun(Pid, _Ref) -> Pid ! Message end, Subscribers),
    State.

%% @doc The output the next step follows: the newest one computed, or the
%% anchor when none has been.
previous_output(#{ anchor := Anchor, head := Head, steps := Steps }) ->
    case maps:get(Head, Steps, not_found) of
        {Output, _Checkpoints} -> Output;
        not_found -> Anchor
    end.

%% @doc Whether the timeline should stop rather than compute `Step'.
stop_at(Step, #{ at := At, reset := Reset }) ->
    (Step - At) > ?MAX_AHEAD orelse (is_integer(Reset) andalso Step >= Reset).

%% @doc The fastest step function that reproduces `ar_vdf:compute/3' exactly
%% **at this difficulty**, reusing the one already chosen when the difficulty
%% has not moved.
%%
%% The NIF carries three SHA-2 implementations and `ar_vdf' is compiled against
%% the portable one, which on hardware with SHA-256 instructions is several
%% times slower -- the difference between a timeline that keeps pace and one
%% that does not. Picking a faster one is only safe if it agrees bit for bit.
%%
%% The difficulty has to be part of that test rather than a detail of it. The
%% `hiopt' kernel agrees with the reference at every difficulty mainnet has
%% used and disagrees at one iteration, so a self-test run at a convenient
%% small difficulty would reject a kernel that is correct in production, and --
%% far worse -- a self-test run only at a large one would accept a kernel that
%% is wrong on a testnet. Neither is a claim worth making from the other's
%% evidence, so the check is made at the difficulty about to be used, on the
%% machine about to use it. It costs one step of each, once per difficulty, in
%% a process nothing is waiting on.
kernel(Difficulty, #{ difficulty := Difficulty, kernel := Kernel }) ->
    Kernel;
kernel(Difficulty, _State) ->
    Candidates = [vdf_sha2_hiopt_nif, vdf_sha2_fused_nif],
    case first_agreeing(Candidates, Difficulty) of
        {ok, Name} ->
            report_kernel(Name, Difficulty),
            fun(Step, Output, D) -> nif(Name, Step, Output, D) end;
        not_found ->
            report_kernel(reference, Difficulty),
            fun ar_vdf:compute/3
    end.

%% @doc Return the first entry point that matches the reference. This stops at
%% the selected candidate, avoiding a second reference step during each
%% difficulty change.
first_agreeing([], _Difficulty) ->
    not_found;
first_agreeing([Name | Names], Difficulty) ->
    case agrees(Name, Difficulty) of
        true -> {ok, Name};
        false -> first_agreeing(Names, Difficulty)
    end.

%% @doc Report both the logical NIF entry point and its physical SHA-256 kernel.
report_kernel(Name, Difficulty) ->
    Backends = ar_vdf_nif:vdf_backend_info_nif(),
    Backend =
        case Name of
            vdf_sha2_hiopt_nif -> maps:get(hiopt, Backends);
            vdf_sha2_fused_nif -> maps:get(fused, Backends);
            reference -> openssl
        end,
    ?event(arweave_sync_short,
        {vdf_timeline_kernel,
            {entrypoint, Name},
            {backend, Backend},
            {verification_backend, maps:get(verify, Backends)},
            {arch, maps:get(arch, Backends)},
            {difficulty, Difficulty}
        }
    ).

%% @doc Whether a candidate reproduces the reference at one difficulty.
agrees(Name, Difficulty) ->
    Output = crypto:hash(sha256, <<"vdf-timeline-self-test">>),
    (catch nif(Name, 2, Output, Difficulty))
        == ar_vdf:compute(2, Output, Difficulty).

%% @doc One step, through the named implementation. The salt is the one
%% `ar_vdf:compute/3' derives, and the checkpoint count is a step's worth less
%% the output itself.
nif(Name, StepNumber, Output, Difficulty) ->
    Salt = ar_vdf:step_number_to_salt_number(StepNumber - 1),
    ar_vdf_nif:Name(
        << Salt:256 >>,
        Output,
        ?VDF_CHECKPOINT_COUNT_IN_STEP - 1,
        0,
        Difficulty
    ).

-ifdef(TEST).

%% @doc A reply that arrives after the snapshot timeout does not remain in the
%% caller's mailbox.
late_snapshot_reply_is_dropped_test() ->
    Caller = self(),
    Timeline =
        spawn(
            fun() ->
                receive
                    {known, Reply, Ref, _Start, _End, _Seed, _Difficulty} ->
                        timer:sleep(20),
                        Reply ! {known, Ref, #{ 1 => {<<1>>, <<>>} }},
                        Caller ! snapshot_reply_sent
                end
            end
        ),
    ?assertEqual(#{}, snapshot(Timeline, 0, 1, <<>>, 1, 0)),
    receive
        snapshot_reply_sent -> ok
    after 1000 ->
        error('snapshot-reply-missing')
    end,
    receive
        {known, _Ref, _Steps} -> error('late-snapshot-reply-received')
    after 0 ->
        ok
    end.

-endif.
