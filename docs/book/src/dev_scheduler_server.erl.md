# dev_scheduler_server

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_scheduler_server.erl)

A long-lived server that schedules messages for a process.
It acts as a deliberate 'bottleneck' to prevent the server accidentally
assigning multiple messages to the same slot.

---

## Exported Functions

- `info/1`
- `schedule/2`
- `start/3`
- `stop/1`

---

### start

A long-lived server that schedules messages for a process.
Start a scheduling server for a given computation.

```erlang
start(ProcID, Proc, Opts) ->
    ?event(scheduling, {starting_scheduling_server, {proc_id, ProcID}}),
    spawn_link(
        fun() ->
            % Before we start, register the scheduler name.
```

### commitment_wallets

Determine the appropriate list of keys to use to commit assignments for

```erlang
commitment_wallets(ProcMsg, Opts) ->
    SchedulerVal =
        hb_ao:get_first(
            [
                {ProcMsg, <<"scheduler">>},
                {ProcMsg, <<"scheduler-location">>}
            ],
            [],
            Opts
        ),
    lists:filtermap(
        fun(Scheduler) ->
            case hb_opts:as(Scheduler, Opts) of
                {ok, #{ priv_wallet := Wallet }} -> {true, Wallet};
                _ -> false
            end
        end,
        dev_scheduler:parse_schedulers(SchedulerVal)
    ).
```

### schedule

Call the appropriate scheduling server to assign a message.

```erlang
schedule(AOProcID, Message) when is_binary(AOProcID) ->
    schedule(dev_scheduler_registry:find(AOProcID), Message);
```

### schedule

Call the appropriate scheduling server to assign a message.

```erlang
schedule(ErlangProcID, Message) ->
    ?event(
        {scheduling_message,
            {proc_id, ErlangProcID},
            {message, Message},
            {is_alive, is_process_alive(ErlangProcID)}
        }
    ),
    AbortTime = scheduler_time() + ?DEFAULT_TIMEOUT,
    ErlangProcID ! {schedule, Message, self(), AbortTime},
    receive
        {scheduled, Message, Assignment} ->
            Assignment
    after ?DEFAULT_TIMEOUT ->
        throw({scheduler_timeout, {proc_id, ErlangProcID}, {message, Message}})
    end.
```

### info

Get the current slot from the scheduling server.

```erlang
info(ProcID) ->
    ?event({getting_info, {proc_id, ProcID}}),
    ProcID ! {info, self()},
    receive {info, Info} -> Info end.
```

### stop

```erlang
stop(ProcID) ->
    ?event({stopping_scheduling_server, {proc_id, ProcID}}),
    ProcID ! stop.
```

### server

The main loop of the server. Simply waits for messages to assign and

```erlang
server(State) ->
    receive
        {schedule, Message, Reply, AbortTime} ->
            case SchedTime = scheduler_time() > AbortTime of
                true ->
                    % Ignore scheduling requests if they are too old. The
                    % `abort-time' signals to us that the client has already
                    % given up on the request, so in order to maintain
                    % predictability we ignore it.
```

### assign

Assign a message to the next slot.

```erlang
assign(State, Message, ReplyPID) ->
    try
        do_assign(State, Message, ReplyPID)
    catch
        _Class:Reason:Stack ->
            ?event({error_scheduling, Reason, Stack}),
            State
    end.
```

### do_assign

Generate and store the actual assignment message.

```erlang
do_assign(State, Message, ReplyPID) ->
    % Ensure that only committed keys from the message are included in the
    % assignment.
```

### commit_assignment

Commit to the assignment using all of our appropriate wallets.

```erlang
commit_assignment(BaseAssignment, State) ->
    Wallets = maps:get(wallets, State),
    Opts = maps:get(opts, State),
    lists:foldr(
        fun(Wallet, Assignment) ->
            hb_message:commit(Assignment, Opts#{ priv_wallet => Wallet })
        end,
        BaseAssignment,
        Wallets
    ).
```

### maybe_inform_recipient

Potentially inform the caller that the assignment has been scheduled.

```erlang
maybe_inform_recipient(Mode, ReplyPID, Message, Assignment, State) ->
    case maps:get(mode, State) of
        Mode -> ReplyPID ! {scheduled, Message, Assignment};
        _ -> ok
    end.
```

### next_hashchain

Create the next element in a chain of hashes that links this and prior

```erlang
next_hashchain(HashChain, Message, Opts) ->
    ?event({creating_next_hashchain, {hash_chain, HashChain}, {message, Message}}),
    ID = hb_message:id(Message, all, Opts),
    crypto:hash(
        sha256,
        << HashChain/binary, ID/binary >>
    ).
```

### scheduler_time

Return the current time in milliseconds.

```erlang
scheduler_time() ->
    erlang:system_time(millisecond).
```

### new_proc_test

Test the basic functionality of the server.

```erlang
new_proc_test() ->
    Wallet = ar_wallet:new(),
    SignedItem = hb_message:commit(
        #{ <<"data">> => <<"test">>, <<"random-key">> => rand:uniform(10000) },
        #{ priv_wallet => Wallet }
    ),
    SignedItem2 = hb_message:commit(
        #{ <<"data">> => <<"test2">> },
        #{ priv_wallet => Wallet }
    ),
    SignedItem3 = hb_message:commit(
        #{
            <<"data">> => <<"test2">>,
            <<"deep-key">> =>
                #{ <<"data">> => <<"test3">> }
        },
        #{ priv_wallet => Wallet }
    ),
    dev_scheduler_registry:find(hb_message:id(SignedItem, all), SignedItem),
    schedule(ID = hb_message:id(SignedItem, all), SignedItem),
    schedule(ID, SignedItem2),
    schedule(ID, SignedItem3),
    ?assertMatch(
        #{ current := 2 },
        dev_scheduler_server:info(dev_scheduler_registry:find(ID))
    ).
```

---

*Generated from [dev_scheduler_server.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_scheduler_server.erl)*
