%%% @doc `rebar3 eunit-all' -- run the HyperBEAM EUnit suite against a
%%% freshly built preloaded-store.
%%%
%%% The suite covers the complete test set: core HyperBEAM modules, every
%%% packaged device, and the shared preloaded test vectors. With no
%%% arguments the entire suite runs. `--module'/`-m' and `--test'/`-t'
%%% narrow the run to specific modules or test functions, regardless of
%%% whether they live in a core module or a device:
%%%
%%% <pre>
%%%   rebar3 eunit-all                           % the whole suite
%%%   rebar3 eunit-all -m some_module            % one module
%%%   rebar3 eunit-all -m some_module -t a_test  % one test
%%%   rebar3 eunit-all -t a_test                 % a test in any module
%%% </pre>
%%%
%%% `rebar3 device test' remains the device-only runner; this command is
%%% the location-agnostic one, and shares its build-and-run engine.
-module(hb_forge_eunit).
-export([init/1, do/1, format_error/1]).

-define(PROVIDER, 'eunit-all').

%% @doc Register the bare, top-level `eunit-all' command.
init(State) ->
    hb_forge_args:provider(State, ?MODULE, #{
        name => ?PROVIDER,
        namespace => default,
        example => "rebar3 eunit-all -m my_module -t my_test",
        opts => hb_forge_args:filter_opts(),
        short_desc =>
            "Run the full HyperBEAM EUnit suite, optionally filtered.",
        desc =>
            "Build a preloaded-store with all devices and core modules, "
            "then run the EUnit suite. With --module/--test, runs only "
            "the selected modules or test functions."
    }).

%% @doc Build the full preloaded test environment and run the suite,
%% narrowed by any `--module'/`--test' filters. `with-core' is forced on so
%% core modules are always part of the run.
do(State) ->
    Args = hb_forge_args:parse(State, <<"_build/eunit-all-store">>),
    hb_forge_test:run(Args#{ <<"with-core">> => true }, State).

%% @doc Render provider failures for rebar3.
format_error(Reason) ->
    hb_forge_test:format_error(Reason).
