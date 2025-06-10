%%% @doc Hyperbeam is a decentralized node implementing the AO-Core protocol
%%% on top of Arweave.
%%% 
%%% This protocol offers a computation layer for executing arbitrary logic on 
%%% top of the network's data.
%%% 
%%% Arweave is built to offer a robust, permanent storage layer for static data
%%% over time. It can be seen as a globally distributed key-value store that
%%% allows users to lookup IDs to retrieve data at any point in time:
%%% 
%%% 	`Arweave(ID) => Message'
%%% 
%%% Hyperbeam adds another layer of functionality on top of Arweave's protocol:
%%% Allowing users to store and retrieve not only arbitrary bytes, but also to
%%% perform execution of computation upon that data:
%%% 
%%% 	`Hyperbeam(Message1, Message2) => Message3'
%%% 
%%% When Hyperbeam executes a message, it will return a new message containing
%%% the result of that execution, as well as signed commitments of its
%%% correctness. If the computation that is executed is deterministic, recipients
%%% of the new message are able to verify that the computation was performed
%%% correctly. The new message may be stored back to Arweave if desired,
%%% forming a permanent, verifiable, and decentralized log of computation.
%%% 
%%% The mechanisms described above form the basis of a decentralized and
%%% verifiable compute engine without any relevant protocol-enforced
%%% scalability limits. It is an implementation of a global, shared
%%% supercomputer.
%%% 
%%% Hyperbeam can be used for an extremely large variety of applications, from
%%% serving static Arweave data with signed commitments of correctness, to
%%% executing smart contracts that have _built-in_ HTTP APIs. The Hyperbeam
%%% node implementation implements AO, an Actor-Oriented process-based
%%% environment for orchestrating computation over Arweave messages in order to
%%% facilitate the execution of more traditional, consensus-based smart
%%% contracts.
%%% 
%%% The core abstractions of the Hyperbeam node are broadly as follows:
%%% 
%%% 1. The `hb' and `hb_opts' modules manage the node's configuration, 
%%%    environment variables, and debugging tools.
%%% 
%%% 2. The `hb_http' and `hb_http_server' modules manage all HTTP-related
%%%    functionality. `hb_http_server' handles turning received HTTP requests
%%%    into messages and applying those messages with the appropriate devices.
%%%    `hb_http' handles making requests and responding with messages. `cowboy'
%%%    is used to implement the underlying HTTP server.
%%% 
%%% 3. `hb_ao' implements the computation logic of the node: A mechanism
%%%    for resolving messages to other messages, via the application of logic
%%%    implemented in `devices'. `hb_ao' also manages the loading of Erlang
%%%    modules for each device into the node's environment. There are many
%%%    different default devices implemented in the hyperbeam node, using the
%%%    namespace `dev_*'. Some of the critical components are:
%%% 
%%%     - `dev_message': The default handler for all messages that do not 
%%%      specify their own device. The message device is also used to resolve
%%%      keys that are not implemented by the device specified in a message,
%%%      unless otherwise signalled.
%%% 
%%%    - `dev_stack': The device responsible for creating and executing stacks
%%%      of other devices on messages that request it. There are many uses for
%%%      this device, one of which is the resolution of AO processes.
%%% 
%%%    - `dev_p4': The device responsible for managing payments for the services
%%%      provided by the node.
%%% 
%%% 4. `hb_store', `hb_cache' and the store implementations forms a layered
%%%    system for managing the node's access to persistent storage. `hb_cache'
%%%    is used as a resolution mechanism for reading and writing messages, while
%%%    `hb_store' provides an abstraction over the underlying persistent key-value
%%%    byte storage mechanisms. Example `hb_store' mechanisms can be found in
%%%    `hb_store_fs' and `hb_store_remote_node'.
%%% 
%%% 5. `ar_*' modules implement functionality related to the base-layer Arweave
%%%    protocol and are largely unchanged from their counterparts in the Arweave
%%%    node codebase presently maintained by the Digital History Association
%%%    (@dha-team/Arweave).
%%% 
%%% You can find documentation of a similar form to this note in each of the core
%%% modules of the hyperbeam node.
-module(hb).
%%% Configuration and environment:
-export([init/0, now/0, build/0]).
%%% Base start configurations:
-export([start_simple_pay/0, start_simple_pay/1, start_simple_pay/2]).
-export([topup/3, topup/4]).
-export([start_mainnet/0, start_mainnet/1]).
%%% Debugging tools:
-export([no_prod/3]).
-export([read/1, read/2, debug_wait/4, profile/1, benchmark/2, benchmark/3]).
%%% Node wallet and address management:
-export([address/0, wallet/0, wallet/1]).
-include("include/hb.hrl").

%% @doc Initialize system-wide settings for the hyperbeam node.
init() ->
    hb_name:start(),
    ?event({setting_debug_stack_depth, hb_opts:get(debug_stack_depth)}),
    Old = erlang:system_flag(backtrace_depth, hb_opts:get(debug_stack_depth)),
    ?event({old_system_stack_depth, Old}),
    ok.

%% @doc Start a mainnet server without payments.
start_mainnet() ->
    start_mainnet(hb_opts:get(port)).
start_mainnet(Port) when is_integer(Port) ->
    start_mainnet(#{ port => Port });
start_mainnet(Opts) ->
    application:ensure_all_started([
        kernel,
        stdlib,
        inets,
        ssl,
        ranch,
        cowboy,
        gun,
        os_mon
    ]),
    Wallet = hb:wallet(hb_opts:get(priv_key_location, no_viable_wallet_path, Opts)),
    BaseOpts = hb_http_server:set_default_opts(Opts),
    hb_http_server:start_node(
        FinalOpts =
            BaseOpts#{
                store => #{ <<"store-module">> => hb_store_fs, <<"prefix">> => <<"cache-mainnet">> },
                priv_wallet => Wallet
            }
    ),
    Address =
        case hb_opts:get(address, no_address, FinalOpts) of
            no_address -> <<"[ !!! no-address !!! ]">>;
            Addr -> Addr
        end,
    io:format(
        "Started mainnet node at http://localhost:~p~n"
        "Operator: ~s~n",
        [maps:get(port, Opts), Address]
    ),
    <<"http://localhost:", (integer_to_binary(maps:get(port, Opts)))/binary>>.

%%% @doc Start a server with a `simple-pay@1.0' pre-processor.
start_simple_pay() ->
    start_simple_pay(address()).
start_simple_pay(Addr) ->
    rand:seed(default),
    start_simple_pay(Addr, 10000 + rand:uniform(50000)).
start_simple_pay(Addr, Port) ->
    do_start_simple_pay(#{ port => Port, operator => Addr }).

do_start_simple_pay(Opts) ->
    application:ensure_all_started([
        kernel,
        stdlib,
        inets,
        ssl,
        ranch,
        cowboy,
        gun,
        os_mon
    ]),
    Port = maps:get(port, Opts),
    Processor =
        #{
            <<"device">> => <<"p4@1.0">>,
            <<"ledger-device">> => <<"simple-pay@1.0">>,
            <<"pricing-device">> => <<"simple-pay@1.0">>
        },
    hb_http_server:start_node(
        Opts#{
            on => #{
                <<"request">> => Processor,
                <<"response">> => Processor
            }
        }
    ),
    io:format(
        "Started simple-pay node at http://localhost:~p~n"
        "Operator: ~s~n",
        [Port, address()]
    ),
    <<"http://localhost:", (integer_to_binary(Port))/binary>>.

%% @doc Helper for topping up a user's balance on a simple-pay node.
topup(Node, Amount, Recipient) ->
    topup(Node, Amount, Recipient, wallet()).
topup(Node, Amount, Recipient, Wallet) ->
    Message = hb_message:commit(
        #{
            <<"path">> => <<"/~simple-pay@1.0/topup">>,
            <<"amount">> => Amount,
            <<"recipient">> => Recipient
        },
        Wallet
    ),
    hb_http:get(Node, Message, #{}).

wallet() ->
    wallet(hb_opts:get(priv_key_location)).
wallet(Location) ->
    case file:read_file_info(Location) of
        {ok, _} ->
            ar_wallet:load_keyfile(Location);
        {error, _} -> 
            Res = ar_wallet:new_keyfile(?DEFAULT_KEY_TYPE, Location),
            ?event({created_new_keyfile, Location, address(Res)}),
            Res
    end.

%% @doc Get the address of a wallet. Defaults to the address of the wallet
%% specified by the `priv_key_location' configuration key. It can also take a
%% wallet tuple as an argument.
address() -> address(wallet()).
address(Wallet) when is_tuple(Wallet) ->
    hb_util:encode(ar_wallet:to_address(Wallet));
address(Location) -> address(wallet(Location)).

%% @doc Debugging function to read a message from the cache.
%% Specify either a scope atom (local or remote) or a store tuple
%% as the second argument.
read(ID) -> read(ID, local).
read(ID, ScopeAtom) when is_atom(ScopeAtom) ->
    read(ID, hb_store:scope(hb_opts:get(store), ScopeAtom));
read(ID, Store) ->
    hb_cache:read(Store, hb_util:id(ID)).

%% @doc Utility function to throw an error if the current mode is prod and
%% non-prod ready code is being executed. You can find these in the codebase
%% by looking for ?NO_PROD calls.
no_prod(X, Mod, Line) ->
    case hb_opts:get(mode) of
        prod ->
            io:format(standard_error,
                "=== DANGER: NON-PROD READY CODE INVOKED IN PROD ===~n", []),
            io:format(standard_error, "~w:~w:       ~p~n", [Mod, Line, X]),
            case hb_opts:get(exit_on_no_prod) of
                true -> init:stop();
                false -> throw(X)
            end;
        _ -> X
    end.

%% @doc Utility function to get the current time in milliseconds.
now() ->
    erlang:system_time(millisecond).

%% @doc Utility function to hot-recompile and load the hyperbeam environment.
build() ->
    r3:do(compile, [{dir, "src"}]).

%% @doc Utility function to start a profiling session and run a function,
%% then analyze the results. Obviously -- do not use in production.
profile(Fun) ->
    eprof:start_profiling([self()]),
    try
        Fun()
    after
        eprof:stop_profiling()
    end,
    eprof:analyze(total).

%% @doc Utility function to wait for a given amount of time, printing a debug
%% message to the console first.
debug_wait(T, Mod, Func, Line) ->
    ?event(wait, {debug_wait, {T, Mod, Func, Line}}),
    receive after T -> ok end.

%% @doc Run a function as many times as possible in a given amount of time.
benchmark(Fun, TLen) ->
    T0 = erlang:system_time(millisecond),
    hb_util:until(
        fun() -> erlang:system_time(millisecond) - T0 > (TLen * 1000) end,
        Fun,
        0
    ).

%% @doc Run multiple instances of a function in parallel for a given amount of time.
benchmark(Fun, TLen, Procs) ->
    Parent = self(),
    receive X -> ?event(benchmark, {start_benchmark_worker, X}) end,
    StartWorker =
        fun(_) ->
            Ref = make_ref(),
            ?event(benchmark, {start_benchmark_worker, Ref}),
            spawn_link(fun() ->
                Count = benchmark(Fun, TLen),
                Parent ! {work_complete, Ref, Count}
            end),
            Ref
        end,
    CollectRes =
        fun(R) ->
            receive
                {work_complete, R, Count} ->
                    ?event(benchmark, {work_complete, R, Count}),
                    Count
            end
        end,
    Refs = lists:map(StartWorker, lists:seq(1, Procs)),
    lists:sum(lists:map(CollectRes, Refs)).