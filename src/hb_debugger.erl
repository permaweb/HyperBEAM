%%% @doc A module that provides bootstrapping interfaces for external debuggers
%%% to connect to HyperBEAM.
%%%
%%% The simplest way to utilize an external graphical debugger is to use the
%%% `erlang-ls' extension for VS Code, Emacs, or other Language Server Protocol
%%% (LSP) compatible editors. This repository contains a `launch.json'
%%% configuration file for VS Code that can be used to spawn a new HyperBEAM,
%%% attach the debugger to it, and execute the specified `Module:Function(Args)'.
%%% Additionally, the node can be started with `rebar3 debugging' in order to
%%% allow access to the console while also allowing the debugger to attach.
%%%
%%% Boot time is approximately 10 seconds.
-module(hb_debugger).
-export([start/0, start_and_break/2, start_and_break/3, start_and_break/4]).
-export([await_breakpoint/0]).


%% Wait for another node (which we assume to be the debugger) to be attached,
%% then return to the caller.
start() ->
    io:format("Starting debugger...~n", []),
    DebuggerRes = application:ensure_all_started(debugger),
    io:format("Started debugger server. Result: ~p.~n", [DebuggerRes]),
    io:format(
      "Waiting for debugger. Node is: ~p. Cookie is: ~p.~n",
      [node(), erlang:get_cookie()]),
    await_debugger().


%% @doc Attempt to interpret a specified module to load it into the debugger.
%% `int:i/1' seems to have an issue that will cause it to fail sporadically
%% with `error:undef' on some modules. This error appears not to be catchable
%% through the normal means. Subsequently, we attempt the load in a separate
%% process and wait for it to complete. If we do not receive a response in a
%% reasonable amount of time, we assume that the module failed to load and
%% return `false'.
interpret(Module) ->
    Parent = self(),
    spawn(fun() ->
                  case int:interpretable(Module) of
                      true ->
                          try
                              Parent ! {interpreted, Module, int:i(Module) == ok}
                          catch
                              _:_ ->
                                  io:format("Could not load module: ~p.~n", [Module]),
                                  false
                          end;
                      Error ->
                          io:format(
                            "Could not interpret module: ~p. Error: ~p.~n",
                            [Module, Error]),
                          false
                  end
          end),
    receive
        {interpreted, Module, Res} -> Res
    after
        250 ->
            false
    end.


%% @doc Interpret modules from a list of atom prefixes.
interpret_modules(Prefixes) when is_binary(Prefixes) ->
    interpret_modules(binary:split(Prefixes, <<",">>, [global, trim_all]));
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
                    Prefixes)
          end,
          hb_util:all_hb_modules()),
    io:format("Relevant modules: ~p.~n", [RelevantModules]),
    lists:foreach(
      fun(Mod) ->
              io:format("Interpreting module: ~p.~n", [Mod]),
              interpret(Mod)
      end,
      RelevantModules),
    RelevantModules.


%% @doc A bootstrapping function to wait for an external debugger to be attached,
%% then add a breakpoint on the specified `Module:Function(Args)', then call it.
start_and_break(Module, Function) ->
    start_and_break(Module, Function, [], []).


start_and_break(Module, Function, Args) ->
    start_and_break(Module, Function, Args, []).


start_and_break(Module, Function, Args, DebuggerScope) ->
    timer:sleep(1000),
    spawn(fun() ->
                  start(),
                  interpret(Module),
                  interpret_modules(DebuggerScope),
                  SetRes = int:break_in(Module, Function, length(Args)),
                  io:format(
                    "Breakpoint set. Result from `int:break_in/3`: ~p.~n",
                    [SetRes]),
                  io:format("Invoking function...~n", []),
                  apply(Module, Function, Args),
                  io:format("Function invoked. Terminating.~n", []),
                  init:stop(),
                  erlang:halt()
          end).


%% @doc Await a debugger to be attached to the node.
await_debugger() -> await_debugger(0).


await_debugger(N) ->
    case is_debugging_node_connected() of
        false ->
            timer:sleep(1000),
            io:format("Still waiting for debugger after ~p seconds...~n", [N]),
            await_debugger(N + 1);
        Node ->
            io:format(
              "External node connection detected. Peer: ~p.~n",
              [Node]),
            N
    end.


%% @doc Is another Distributed Erlang node connected to us?
is_debugging_node_connected() ->
    case nodes() ++ nodes(hidden) of
        [] -> false;
        [Node | _] -> Node
    end.


%% @doc Await a new breakpoint being set by the debugger.
await_breakpoint() ->
    case is_debugging_node_connected() of
        false -> start();
        _ -> do_nothing
    end,
    await_breakpoint(0).


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
    end.
