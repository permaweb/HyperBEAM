# dev_scheduler_registry

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_scheduler_registry.erl)

A simple registry for local services in AO, using pg. Currently,
only SU processes are supported.

---

## Exported Functions

- `find/1`
- `find/2`
- `find/3`
- `get_processes/0`
- `get_wallet/0`
- `start/0`

---

### start

```erlang
start() ->
    hb_name:start(),
    ok.
```

### get_wallet

```erlang
get_wallet() ->
    % TODO: We might want to use a different wallet per SU later.
```

### find

Find a process associated with the processor ID in the local registry
Find a process associated with the processor ID in the local registry

```erlang
find(ProcID) -> find(ProcID, false).
```

### find

Find a process associated with the processor ID in the local registry
Find a process associated with the processor ID in the local registry

```erlang
find(ProcID, ProcMsgOrFalse) ->
    find(ProcID, ProcMsgOrFalse, #{ priv_wallet => hb:wallet() }).
```

### find

Same as `find/2` but with additional options passed when spawning a 

```erlang
find(ProcID, ProcMsgOrFalse, Opts) ->
    case hb_name:lookup({<<"scheduler@1.0">>, ProcID}) of
        undefined -> maybe_new_proc(ProcID, ProcMsgOrFalse, Opts);
        Pid -> Pid
    end.
```

### get_processes

Return a list of all currently registered ProcID.

```erlang
get_processes() ->
    ?event({getting_processes, hb_name:all()}),
    [ ProcID || {{<<"scheduler@1.0">>, ProcID}, _} <- hb_name:all() ].
```

### maybe_new_proc

Return a list of all currently registered ProcID.

```erlang
maybe_new_proc(_ProcID, false, _Opts) -> not_found;
```

### maybe_new_proc

Return a list of all currently registered ProcID.

```erlang
maybe_new_proc(ProcID, ProcMsg, Opts) -> 
    dev_scheduler_server:start(ProcID, ProcMsg, Opts).
```

### test_opts

```erlang
test_opts() ->
    #{
        store => hb_test_utils:test_store(),
        priv_wallet => hb:wallet()
    }.
```

### generate_test_procs

```erlang
generate_test_procs(Opts) ->
    [
        hb_message:commit(
            #{
                <<"type">> => <<"Process">>,
                <<"image">> => <<0:(1024*32)>>
            },
            Opts
        ),
        hb_message:commit(
            #{
                <<"type">> => <<"Process">>,
                <<"image">> => <<0:(1024*32)>>
            },
            Opts
        )
    ].
```

### find_non_existent_process_test

```erlang
find_non_existent_process_test() ->
    Opts = test_opts(),
    [Proc1, _Proc2] = generate_test_procs(Opts),
    start(),
    ?assertEqual(not_found, ?MODULE:find(hb_message:id(Proc1, all))).
```

### create_and_find_process_test

```erlang
create_and_find_process_test() ->
    Opts = test_opts(),
    [Proc1, _Proc2] = generate_test_procs(Opts),
    ID = hb_message:id(Proc1, all, Opts),
    start(),
    Pid1 = ?MODULE:find(ID, Proc1),
    ?assert(is_pid(Pid1)),
    ?assertEqual(Pid1, ?MODULE:find(ID, Proc1)).
```

### create_multiple_processes_test

```erlang
create_multiple_processes_test() ->
    Opts = test_opts(),
    [Proc1, Proc2] = generate_test_procs(Opts),
    start(),
    ID1 = hb_message:id(Proc1, all, Opts),
    ID2 = hb_message:id(Proc2, all, Opts),
    Pid1 = ?MODULE:find(ID1, Proc1),
    Pid2 = ?MODULE:find(ID2, Proc2),
    ?assert(is_pid(Pid1)),
    ?assert(is_pid(Pid2)),
    ?assertNotEqual(Pid1, Pid2),
    ?assertEqual(Pid1, ?MODULE:find(ID1, Proc1)),
    ?assertEqual(Pid2, ?MODULE:find(ID2, Proc2)).
```

### get_all_processes_test

```erlang
get_all_processes_test() ->
    Opts = test_opts(),
    [Proc1, Proc2] = generate_test_procs(Opts),
    start(),
    ID1 = hb_message:id(Proc1, all, Opts),
    ID2 = hb_message:id(Proc2, all, Opts),
    ?MODULE:find(ID1, Proc1),
    ?MODULE:find(ID2, Proc2),
    Processes = ?MODULE:get_processes(),
    ?assert(length(Processes) >= 2),
    ?event({processes, Processes}),
    ?assert(lists:member(ID1, Processes)),
```

---

*Generated from [dev_scheduler_registry.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_scheduler_registry.erl)*
