# hb_debugger

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_debugger.erl)

A module that provides bootstrapping interfaces for external debuggers
to connect to HyperBEAM.
The simplest way to utilize an external graphical debugger is to use the 
`erlang-ls` extension for VS Code, Emacs, or other Language Server Protocol
(LSP) compatible editors. This repository contains a `launch.json`
configuration file for VS Code that can be used to spawn a new HyperBEAM,
attach the debugger to it, and execute the specified `Module:Function(Args)`.
Additionally, the node can be started with `rebar3 debugging` in order to
allow access to the console while also allowing the debugger to attach.
Boot time is approximately 10 seconds.

---

## Exported Functions

- `await_breakpoint/0`
- `profile_and_stop/1`
- `start_and_break/2`
- `start_and_break/3`
- `start_and_break/4`
- `start/0`

---

### profile_and_stop

A module that provides bootstrapping interfaces for external debuggers
Profile a function with eflame and stop the node.

```erlang
profile_and_stop(Fun) ->
    {ok, F} = file:open("profiling-output", [write]),
    group_leader(F, self()),
    io:format("profiling-output: started.~n"),
    io:format("Profiling function: ~p.~n", [Fun]),
    Res =
        dev_profile:eval(
            Fun,
            #{ <<"return-mode">> => <<"open">>, <<"engine">> => <<"eflame">> },
            #{}
        ),
    io:format("Profiling complete. Res: ~p~n", [Res]),
    init:stop(),
    erlang:halt().
```

### start

```erlang
start() ->
    io:format("Starting debugger...~n", []),
    DebuggerRes = application:ensure_all_started(debugger),
    io:format("Started debugger server. Result: ~p.~n", [DebuggerRes]),
    io:format(
        "Waiting for debugger. Node is: ~p. Cookie is: ~p.~n",
        [node(), erlang:get_cookie()]
    ),
    await_debugger().
```

### interpret

Attempt to interpret a specified module to load it into the debugger.

```erlang
interpret(Module) ->
    Parent = self(),
    spawn(fun() ->
        case int:interpretable(Module) of
            true ->
                try Parent ! {interpreted, Module, int:i(Module) == ok}
                catch _:_ ->
                    io:format("Could not load module: ~p.~n", [Module]),
                    false
                end;
            Error ->
                io:format(
                    "Could not interpret module: ~p. Error: ~p.~n",
                    [Module, Error]
                ),
                false
        end
    end),
    receive {interpreted, Module, Res} -> Res
    after 250 -> false
    end.
```

### interpret_modules

Interpret modules from a list of atom prefixes.

```erlang
interpret_modules(Prefixes) when is_binary(Prefixes) ->
    interpret_modules(binary:split(Prefixes, <<",">>, [global, trim_all]));
```

### interpret_modules

Interpret modules from a list of atom prefixes.

```erlang
interpret_modules(Prefixes) when is_list(Prefixes) ->
    RelevantModules =
        lists:filter(
            fun(Mod) ->
                ModBin = hb_util:bin(Mod),
                lists:any(
                    fun(Prefix) ->
                        PrefixBin = hb_util:bin(Prefix),
                        binary:longest_common_prefix([ModBin, PrefixBin]) ==
                            byte_size(PrefixBin)
                    end,
                    Prefixes
                )
            end,
            hb_util:all_hb_modules()
        ),
    io:format("Relevant modules: ~p.~n", [RelevantModules]),
    lists:foreach(
        fun(Mod) ->
            io:format("Interpreting module: ~p.~n", [Mod]),
            interpret(Mod)
        end,
        RelevantModules
    ),
    RelevantModules.
```

### start_and_break

A bootstrapping function to wait for an external debugger to be attached,

```erlang
start_and_break(Module, Function) ->
    start_and_break(Module, Function, [], []).
```

### start_and_break

```erlang
start_and_break(Module, Function, Args) ->
    start_and_break(Module, Function, Args, []).
```

### start_and_break

```erlang
start_and_break(Module, Function, Args, DebuggerScope) ->
    timer:sleep(1000),
    spawn(fun() ->
        start(),
        interpret(Module),
        interpret_modules(DebuggerScope),
        SetRes = int:break_in(Module, Function, length(Args)),
        io:format(
            "Breakpoint set. Result from `int:break_in/3`: ~p.~n",
            [SetRes]
        ),
        io:format("Invoking function...~n", []),
        apply(Module, Function, Args),
        io:format("Function invoked. Terminating.~n", []),
        init:stop(),
        erlang:halt()
    end).
```

### await_debugger

Await a debugger to be attached to the node.

```erlang
await_debugger() -> await_debugger(0).
```

### await_debugger

Await a debugger to be attached to the node.

```erlang
await_debugger(N) ->
    case is_debugging_node_connected() of
        false ->
            timer:sleep(1000),
            io:format("Still waiting for debugger after ~p seconds...~n", [N]),
            await_debugger(N + 1);
        Node ->
            io:format(
                "External node connection detected. Peer: ~p.~n",
                [Node]
            ),
            N
    end.
```

### is_debugging_node_connected

Is another Distributed Erlang node connected to us?

```erlang
is_debugging_node_connected() ->
    case nodes() ++ nodes(hidden) of
        [] -> false;
        [Node | _] -> Node
    end.
```

### await_breakpoint

Await a new breakpoint being set by the debugger.

```erlang
await_breakpoint() ->
    case is_debugging_node_connected() of
        false -> start();
        _ -> do_nothing
    end,
    await_breakpoint(0).
```

### await_breakpoint

```erlang
await_breakpoint(N) ->
    io:format("Waiting for breakpoint to be set in function...~n", []),
    case int:all_breaks() of
        [] ->
            timer:sleep(1000),
            io:format("Still waiting for breakpoint after ~p seconds...~n", [N]),
            await_breakpoint(N + 1);
        [Breakpoint | _] ->
            io:format("Breakpoint set. Info: ~p.~n", [Breakpoint]),
            Breakpoint
```

---

*Generated from [hb_debugger.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_debugger.erl)*
