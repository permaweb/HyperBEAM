%%% @doc A testing framework for AO-Core devices and HyperBEAM components built
%%% upon the principles of property-based testing. Rather than testing specific
%%% input and output pairs, `hb_invariant' allows us to instead focus on 
%%% defining invariant properties that should hold true for all valid inputs.
%%% `hb_invariant' gives us tools to quickly and easily generate random inputs
%%% (states, requests, node messages, etc.) to our components and then test that
%%% the stated properties hold true for each of them.
%%% 
%%% ## Execution Types.
%%% 
%%% Executions can come in a variety of forms:
%%% 
%%% - AO-Core device key relationships: Allowing us to define properties 
%%%   that should hold true for all `Base`, `Request`, node messages, and their
%%%   corresponding `Result` messages.
%%% - AO-Core device state machines: Allowing us to generate random initial
%%%   states and sequences of requests, ensuring that a set of properties hold
%%%   true at all times.
%%% - Comparisons between two AO-Core device state machines: As above, except
%%%   allowing us to define two generators for initial states, such that the 
%%%   functionality of one device can easily be compared to another. Properties
%%%   in such tests receive not only the 'pre' and 'post' states for the primary
%%%   state machine, but also the corresponding values for the reference machine.
%%% - Direct Erlang function executions: Possible in each of the above cases,
%%%   `hb_invariant' allows us to compute Erlang functions rather than AO-Core
%%%   (`ao(Base, Req, Opts)') invocations, if preferred. This allows us to utilize
%%%   `hb_invariant' to test HyperBEAM itself, as well as devices resident
%%%   inside it.
%%% 
%%% ## Execution Flow.
%%% 
%%% There are two primary invocation methods for `hb_invariant': `forall/1' and
%%% `state_machine/1'. Because the state machine is sufficiently general to cover
%%% all cases, under-the-hood `forall' is simply a wrapper around `state_machine'
%%% that sets the length of the request sequence to `1'. A consequence of this
%%% is that all invocations are able to utilize the full set of parameters to
%%% control the execution.
%%% 
%%% The state machine executor always takes a `Specification' message as an
%%% argument, and operates in a series of stages:
%%% 
%%% ```
%%% 1. Specification normalization: All non-mandatory fields are filled in with
%%%    default values, internal state keys are initialized in the `Spec', and
%%%    initial seeding of the PRNG (`rand' module) is performed.
%%% 2. Repeat for each of the `Spec/runs' of the state machine:
%%% 2.1* Generate a node message (`Opts').
%%% 2.2* Generate an initial state (`Base' message) for the execution.
%%% 2.3* Generate an initial model state (`Model' message) for the execution, if
%%%      applicable.
%%% 2.4. For each element of request sequence `Spec/length`:
%%% 2.4.1* Generate a request message (`Request' message) for the execution.
%%% 2.4.2* Execute the request message against the current state (and model state,
%%%        if applicable), resulting in a `Result' message.
%%% 2.4.3. For each of the `Spec/properties':
%%% 2.4.3.1. Attempt to invoke the property function with the prior state(s), request,
%%%        result(s), and options.
%%% 2.4.3.2. If the property function returns `true', continue to the next property.
%%% 2.4.3.3. If the property function returns `false', fail and return details of
%%%          the executed sequence and error encountered.
%%% 2.4.3.4. If the property function lacks a function clause matching the call
%%%          the failure is ignored. This allows callers to easily define which
%%%          states are relevant for a given property simply with patterns and 
%%%          guards in the function head.
%%% 2.4.4. Apply `Spec/next' to the state and model state, if applicable, resulting
%%%        in a new state and model state. If no `next' function is provided, the
%%%        result of the request stage is used in the next iteration of the loop.
%%% 3. Return `ok' if all properties were enforced successfully, otherwise return
%%%    details of the executed sequence and the error encountered.
%%% '''
%%% `*' markers above indicate that prior to the execution of a stage, the `rand'
%%% module's PRNG is seeded with a value derived from the global seed (either
%%% provided or generated at start time), the run number, the current request
%%% count, and the current stage. This allows for reproducibility of the execution
%%% sequences. See `Controlled Randomness' below for more details.
%%% 
%%% ## Generators.
%%% 
%%% `hb_invariant' supports a number of different types of `generators', utilized
%%% to derive each input in execution sequences. Supported generator forms are
%%% as follows:
%%% 
%%% - Lists: Lists of generators of other forms, from which one one member is
%%%   randomly selected and executed as if it was provided directly.
%%% - Functions: Arbitrary Erlang functions, invoked with a specific set of
%%%   arguments depending on the type of generator and the context.
%%% - Explicit values: A simple constant value or message, used without execution.
%%% 
%%% Generators of these forms may be provided by the caller for each of the 
%%% keys listed below. Their names and function signatures are as follows:
%%% - `opts(Spec)': A generator for the node message to use for a `run' of the
%%%   state machine.
%%% - `Spec/states': A generator for initial (`Base') states, executed per `run'.
%%% - `Spec/models': A generator for initial _model_ states, executed per `run'.
%%% - `Spec/requests': Generator of `Request's in the state transformation sequence.
%%% 
%%% In all cases aside `Spec/requests', the generator is optional, using a 
%%% default value if not provided. Without a `requests' generator, no sensible
%%% state transformation sequence can be generated. Subsequently, execution is
%%% aborted with an error.
%%% 
%%% ## Controlled Randomness.
%%% 
%%% In order to assist in the creation of generators and properties for 
%%% `hb_invariant', a number of helper functions are provided to quickly and
%%% easily generate random inputs of a given type. `hb_invariant' seeds Erlang's
%%% `rand' module with a value derived from a provided global seed, or a unique
%%% value per invocation of the state machine executor. In event of errors, the
%%% initial global seed is provided to the user such that issues that arose may
%%% be reproduced.
%%% 
%%% Value generators for the following types are provided:
%%% - `int/0': Generate a random integer between 0 and the maximum 'small'
%%%   (non-bignum) integer value.
%%% - `int/1': Generate a random integer between 0 and the given maximum value.
%%% - `int/2': Generate a random integer between the given values.
%%% - `float/0': Generate a random float between 0 and the maximum float value.
%%% - `float/1': Generate a random float between 0 and the given maximum value.
%%% - `string/0': Generate a random string of a given length.
%%% - `string/1': Generate a random string of a given length.
%%% - `string/4': Generate a random string of a given length, with a give
%%%   minimum and maximum character values, and a list of forbidden characters.
-module(hb_invariant).
-export([forall/1, state_machine/1]).
-export([any/0, any/1, pick/1]).
-export([int/0, int/1, int/2, float/0, float/1]).
-export([string/0, string/1, string/4, key/0, key/1]).
-include("include/hb.hrl").

%%% Default values.
-define(DEFAULT_RUNS, 10).
-define(DEFAULT_LENGTH, 10).

%% @doc Wrap a `state_machine/1' invocation, defaulting the length of each run to
%% be `1'. This results in the generation of a unique initial (`Base') state,
%% node message, and request for each `run' of the state machine.
forall(Spec) ->
    state_machine(Spec#{ length => hb_opts:get(length, 1, Spec) }).

%% @doc Execute a state machine with a given `Specification'. Supported keys are
%% as follows:
%% - `seed': The global seed to use for the execution. If not provided, a random
%%   value is generated using the operating system's entropy pool via the 
%%   `crypto' module.
%% - `runs': The number of times to regenerate the full state and request sequence
%%   for the machine. If not provided, the default value of `10' is used.
%% - `length': The number of requests to generate for each `run' of the state
%%   machine. If not provided, the default value of `10' is used.
%% - `states': A generator for initial (`Base') states.
%% - `models': A generator for initial model (comparator) states.
%% - `properties': A list of optional properties to enforce after each request
%%   in the sequence.
%% - `opts': A generator for node messages (`Opts') to use for each `run' of the
%%   state machine. If not provided, an empty node message is used.
%% - `next': A function to apply to the state and model state, if applicable,
%%   after a request has been executed and the properties have been enforced.
%%   This allows callers to manipulate the state of the machine if necessary
%%   between requests. If not provided, the result of the request stage is used
%%   directly in the next iteration of the loop.
%% 
%% See the moduledoc for more details on orchestrating state machine executions.
state_machine(Spec = #{ requests := _ }) ->
    Runs = hb_opts:get(runs, ?DEFAULT_RUNS, Spec),
    Length = hb_opts:get(length, ?DEFAULT_LENGTH, Spec),
    run_state_machines(
        Spec#{
            seed =>
                hb_opts:get(
                    seed,
                    crypto:bytes_to_integer(crypto:strong_rand_bytes(4)),
                    Spec
                ),
            states => hb_opts:get(states, undefined, Spec),
            models => hb_opts:get(models, undefined, Spec),
            properties => hb_opts:get(properties, [], Spec),
            opts => hb_opts:get(opts, #{}, Spec),
            next => hb_opts:get(next, undefined, Spec),
            runs => Runs,
            runs_remaining => Runs,
            length => Length,
            requests_remaining => Length
        }
    );
state_machine(_Spec) ->
    throw({invalid_spec, missing_request_generator}).

run_state_machines(#{ runs_remaining := 0 }) ->
    ok;
run_state_machines(
    Spec = #{
        runs_remaining := RunsRemaining,
        length := Length
    }
) ->
    seed(Spec#{ stage => init }),
    Opts = generate_opts(Spec),
    SpecWithOpts = Spec#{ opts => Opts },
    InitialState = generate_initial_state(SpecWithOpts),
    ?event({generated_initial_state, InitialState}),
    InitialModelState = generate_initial_model_state(SpecWithOpts),
    ResSequence =
        run_state_machine(
            SpecWithOpts#{
                requests_remaining => Length,
                state => InitialState,
                model_state => InitialModelState
            }
        ),
    ?event({run_result, ResSequence}),
    case lists:last(ResSequence) of
        {error, Type, Reason} ->
            ?event(
                error,
                {state_machine_execution_failure,
                    {seed, hb_opts:get(seed, undefined, Spec)},
                    {type, Type},
                    {reason, Reason},
                    {initial_state, InitialState},
                    {sequence, ResSequence}
                }
            ),
            {failure, InitialState, ResSequence};
        {ok, EndState} ->
            ?event(
                properties,
                {success,
                    {final_state, EndState},
                    {sequence, [InitialState | ResSequence]}
                },
                Opts
            ),
            run_state_machines(Spec#{ runs_remaining => RunsRemaining - 1 })
    end.

%% @doc Invoke the execution of a single state machine run.
run_state_machine(#{ requests_remaining := 0, state := State }) -> [{ok, State}];
run_state_machine(Spec = #{ requests_remaining := RequestsRemaining }) ->
    Req = generate_request(Spec),
    ?event({evaluating_request, {request, Req}}),
    case execute_request(Spec, Req) of
        {error, Type, Reason} ->
            [Req, {error, Type, Reason}];
        Result ->
            case enforce_properties(Spec, Req, Result) of
                ok ->
                    NextSpec = apply_next(Spec, Req, Result),
                    [
                        Req
                    |
                        run_state_machine(
                            NextSpec#{
                                requests_remaining => RequestsRemaining - 1
                            }
                        )
                    ];
                {error, Type, Reason} ->
                    [Req, {error, Type, Reason}]
            end
    end.

%% @doc Seed the PRNG with a value derived from the `Specification's global seed,
%% the run number, the request count, and the current execution stage.
seed(#{ seed := undefined }) ->
    ok;
seed(
        #{
            seed := Seed,
            runs_remaining := Runs,
            requests_remaining := Reqs,
            stage := Stage
        }
    ) ->
    rand:seed(exsplus, Seed + Runs + Reqs + stage_to_int(Stage)).

%% @doc Returns an integer corresponding to the stage of execution presented in
%% tuple/atom form.
stage_to_int(init) -> 0;
stage_to_int({generate, opts}) -> 1;
stage_to_int({generate, state}) -> 2;
stage_to_int({generate, request}) -> 3;
stage_to_int({execute, request}) -> 4.

%% @doc Generate a node message (`Opts') for a `run' of the state machine.
generate_opts(Spec = #{ opts := Opts }) ->
    seed(Spec#{ stage => {generate, opts} }),
    execute_generator(Opts, [Spec]).

%% @doc Generate an initial (`Base') state for a `run' of the state machine.
generate_initial_state(Spec = #{ states := Gen, opts := Opts }) ->
    seed(Spec#{ stage => {generate, state} }),
    execute_generator(Gen, [Opts]).

%% @doc Generate an initial model (comparator) state for a `run' of the state
%% machine.
generate_initial_model_state(#{ models := undefined }) ->
    undefined;
generate_initial_model_state(Spec = #{ models := Gen, opts := Opts }) ->
    seed(Spec#{ stage => {generate, state} }),
    execute_generator(Gen, [Opts]).

%% @doc Generate a request for an element of the `sequence' of requests for a
%% `run' of the state machine. If no model state is provided, a single request
%% is generated. If a model state is provided, a tuple of two requests is
%% generated, one for the primary state and one for the model state. Note: The
%% PRNG used by `hb_invariant' is re-seeded with the same value for each request
%% generation, such that random numbers used during the generation of each will
%% be shared. This allows callers to more easily compare the resulting model
%% states against the primary execution states.
generate_request(
        Spec = #{
            requests := Gen,
            state := State,
            model_state := undefined,
            opts := Opts
        }
) ->
    seed(Spec#{ stage => {generate, request} }),
    execute_generator(Gen, [State, Opts]);
generate_request(
        Spec = #{
            requests := Gen,
            state := State,
            model_state := ModelState,
            opts := Opts
        }
) ->
    seed(Spec#{ stage => {generate, request} }),
    StateReq = execute_generator(Gen, [State, Opts]),
    seed(Spec#{ stage => {generate, request} }),
    ModelReq = execute_generator(Gen, [ModelState, Opts]),
    {StateReq, ModelReq}.

%% @doc Execute a generator with a given set of arguments. If a list of generators
%% is provided, a random one is selected and executed. If a single generator is
%% provided, it is executed. If an explicit value is provided, it is returned
%% as-is.
execute_generator(Generators, Args) when is_list(Generators) ->
    execute_generator(pick(Generators), Args);
execute_generator(Generator, Args) when is_function(Generator) ->
    apply(Generator, Args);
execute_generator(ExplicitResult, _) ->
    ExplicitResult.

%% @doc Marshall execution of a request against a given state and node message.
%% If no model state is provided, the request is executed against the primary
%% state. If a model state is provided, the request is executed against both the
%% primary and model states, and the results are returned as a tuple.
execute_request(
        Spec = #{ model_state := undefined, state := State, opts := Opts },
        Req
    ) ->
    seed(Spec#{ stage => {execute, request} }),
    do_request(State, Req, Opts);
execute_request(
        Spec = #{ model_state := ModelState, state := State, opts := Opts },
        {Req, ModelReq}
    ) ->
    seed(Spec#{ stage => {execute, request} }),
    StateRes = do_request(State, Req, Opts),
    seed(Spec#{ stage => {execute, request} }),
    ModelRes = do_request(ModelState, ModelReq, Opts),
    case {StateRes, ModelRes} of
        {{ok, NewState}, {ok, NewModelState}} ->
            {ok, NewState, NewModelState};
        {{error, Reason}, _} ->
            {error, request_error, Reason};
      {_, {error, Reason}} ->
            {error, model_request_error, Reason}
    end.

%% @doc The core request executor. If the request is an AO-Core message (an
%% Erlang map), it is invoked using `hb_ao:resolve/3'. If the request is an
%% Erlang function, it is invoked with the given state and node message. If a
%% direct result is provided, it is returned as-is.
do_request(State, Req, Opts) when is_map(Req) ->
    hb_ao:resolve(State, Req, Opts);
do_request(State, Req, Opts) when is_function(Req) ->
    Req(State, Opts);
do_request(_, DirectResult, _Opts) ->
    DirectResult.

%% @doc Enforce a set of properties against a given request and result. See the
%% moduledoc for more details on the structure of properties.
enforce_properties(Spec = #{ properties := Properties }, Req, Result) ->
    enforce_properties(Properties, Req, Result, Spec).
enforce_properties([], _Req, _Result, _Spec) -> ok;
enforce_properties([Property | Properties], Req, Result, Spec) ->
    case {enforce_property(Property, Req, Result, Spec), Result} of
        {downgrade, {ok, NewState, _NewModelState}} ->
            ?event(
                {falling_back_to_primary_state_enforcement, Property}
            ),
            case enforce_property(Property, Req, {ok, NewState}, Spec) of
                X when X =:= ok orelse X =:= skip ->
                    ?event(
                        {downgraded_property_enforced,
                            {status, X},
                            {property, Property}
                        }
                    ),
                    enforce_properties(Properties, Req, Result, Spec);
                {error, Reason} -> {error, {property_error, Property}, Reason}
            end;
        {X, _} when X =:= ok orelse X =:= skip ->
            ?event(
                {property_enforced,
                    {status, X},
                    {property, Property}
                }
            ),
            enforce_properties(Properties, Req, Result, Spec);
        {{error, Reason}, _} -> {error, {property_error, Property}, Reason}
    end.

%% @doc Enforce a single property against a given request and result.
enforce_property(
        Property,
        Req,
        {ok, New1, New2},
        #{
            state := Old1,
            model_state := Old2,
            opts := Opts
        }) ->
    try Property(Old1, Old2, Req, New1, New2, Opts) of
        true -> ok;
        false -> {error, property_returned_false};
        Else -> Else
    catch
        error:{badarity, _} -> downgrade;
        error:function_clause -> skip;
        error:Reason -> {error, Reason}
    end;
enforce_property(
        Property,
        Req,
        {ok, New},
        #{
            state := Old,
            opts := Opts
        }) ->
    try Property(Old, Req, New, Opts) of
        true -> ok;
        false -> {error, property_returned_false};
        {error, Reason} -> {error, Reason};
        Else -> Else
    catch
        error:{badarity, _} -> skip;
        error:function_clause -> skip;
        error:Reason -> {error, Reason}
    end.

%% @doc Apply the `next' function to the state and model state, if applicable.
%% If no model state is provided, the `next' function is applied to the primary
%% state. If a model state is provided, the `next' function is applied to both
%% the primary and model states.
apply_next(Spec = #{ next := undefined, model_state := undefined }, _, {ok, NewState}) ->
    Spec#{ state => NewState };
apply_next(Spec = #{ next := undefined }, _, {ok, NewState, NewModelState}) ->
    Spec#{ model_state => NewModelState, state => NewState };
apply_next(
        Spec = #{
            next := Next,
            state := OldState,
            model_state := OldModelState,
            opts := Opts
        },
        Req,
        {ok, NewState, NewModelState}) ->
    Spec#{
        state => Next(OldState, Req, NewState, Opts),
        model_state => Next(OldModelState, Req, NewModelState, Opts)
    };
apply_next(
        Spec = #{
            next := Next,
            state := OldState,
            opts := Opts
        },
        Req,
        {ok, NewState}) ->
    Spec#{
        state => Next(OldState, Req, NewState, Opts)
    }.

%%% Pseudorandom Value Generators.

%% Size constants.
-define(BUILTIN_TYPES, [int, float, string, key]).
-define(INT_MAX, 1 bsl 32).
-define(INT_TINY_MAX, 32).
-define(SMALL_INT_MAX, 256).
-define(BIG_INT_MAX, 1 bsl 256).
-define(STRING_MAX_LENGTH, small).

%% @doc Generate a random value of a given type.
any() -> any(?BUILTIN_TYPES).
any(Types) -> (pick([ fun ?MODULE:Type/0 || Type <- Types ]))().

%% @doc Pick a random value from a list, map, or integer range.
pick(Int) when is_integer(Int) ->
    rand:uniform(Int);
pick([]) ->
    error(cannot_pick_from_empty_list);
pick(List) when is_list(List) ->
    lists:nth(int(length(List)), List);
pick(Map) when is_map(Map) andalso map_size(Map) == 0 ->
    error(cannot_pick_from_empty_map);
pick(Map) when is_map(Map) ->
    pick(maps:values(Map)).
pick(Min, Max, Forbidden) when is_list(Forbidden) ->
    case lists:member(X = int(Min, Max), Forbidden) of
      true -> pick(Min, Max, Forbidden);
      false -> X
    end.

%% @doc Generate a random integer.
int() -> int(?INT_MAX).
%% @doc Generate a random integer between 0 and the given maximum value --
%% expressed either explicitly or as a named size constant.
int(Spec) when not is_integer(Spec) -> int(num(Spec));
int(Max) -> rand:uniform(Max).

%% @doc Generate a random integer between the given minimum and maximum values --
%% expressed either explicitly or as a named size constant.
int(Min, Max) -> num(Min) + rand:uniform(num(Max) - num(Min)).

%% @doc Convert a named size constant to an integer.
num(Int) when is_integer(Int) -> Int;
num(tiny) -> ?INT_TINY_MAX;
num(small) -> ?SMALL_INT_MAX;
num(big) -> ?BIG_INT_MAX;
num(Max) -> Max.

%% @doc Generate a random float.
float() -> ?MODULE:float(?INT_MAX).
%% @doc Generate a random float between 0 and the given maximum value --
%% expressed either explicitly or as a named size constant.
float(small) -> rand:uniform_real() * (2 * ?SMALL_INT_MAX);
float(big) -> rand:uniform_real() * (2 * ?BIG_INT_MAX);
float(Max) -> rand:uniform_real() * (2 * Max).

%% @doc Generate a random string.
string() -> string(?STRING_MAX_LENGTH).
%% @doc Generate a random lowercase ASCII string of a given length.
string(MaxLen) -> string(MaxLen, 97, 122, [$/]).
%% @doc Generate a random lowercase ASCII string of a given length, with a given
%% minimum and maximum character value, and a list of forbidden characters.
string(MaxLen, MinChar, MaxChar, Forbidden) ->
    <<
        <<(pick(MinChar, MaxChar, Forbidden)):8>>
    ||
        _ <- lists:seq(1, int(1, MaxLen))
    >>.

%% @doc Generate a random AO-Core key.
key() -> key(tiny).
%% @doc Generate a random AO-Core key of a given length.
key(Len) -> hb_ao:normalize_key(string(Len)).