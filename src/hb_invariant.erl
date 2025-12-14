-module(hb_invariant).
-export([forall/1, state_machine/1]).
-export([any/0, any/1, pick/1]).
-export([int/0, int/1, int/2, float/0, float/1]).
-export([string/0, string/1, string/4, key/0, key/1]).
-include("include/hb.hrl").

%%% Test workflows.

-define(DEFAULT_RUNS, 10).
-define(DEFAULT_LENGTH, 10).

forall(Spec) ->
    state_machine(Spec#{ length => hb_opts:get(length, 1, Spec) }).

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
    normalize_rand(Spec#{ stage => init }),
    Opts = generate_opts(Spec),
    SpecWithOpts = Spec#{ opts => Opts },
    InitialState = generate_initial_state(SpecWithOpts),
    ?event({generated_initial_state, InitialState}),
    InitialModelState = generate_initial_model_state(SpecWithOpts),
    ResSequence =
        state_machine_loop(
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

state_machine_loop(#{ requests_remaining := 0, state := State }) -> [{ok, State}];
state_machine_loop(Spec = #{ requests_remaining := RequestsRemaining }) ->
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
                        state_machine_loop(
                            NextSpec#{
                                requests_remaining => RequestsRemaining - 1
                            }
                        )
                    ];
                {error, Type, Reason} ->
                    [Req, {error, Type, Reason}]
            end
    end.

normalize_rand(#{ seed := undefined }) ->
    ok;
normalize_rand(
        #{
            seed := Seed,
            runs_remaining := Runs,
            requests_remaining := Reqs,
            stage := Stage
        }
    ) ->
    rand:seed(exsplus, Seed + Runs + Reqs + stage_to_int(Stage)).

stage_to_int(init) -> 0;
stage_to_int({generate, opts}) -> 1;
stage_to_int({generate, state}) -> 2;
stage_to_int({generate, request}) -> 3;
stage_to_int({execute, request}) -> 4.

generate_opts(Spec = #{ opts := Opts }) ->
    normalize_rand(Spec#{ stage => {generate, opts} }),
    execute_generator(Opts, [Spec]).

generate_initial_state(Spec = #{ states := Gen, opts := Opts }) ->
    normalize_rand(Spec#{ stage => {generate, state} }),
    execute_generator(Gen, [Opts]).

generate_initial_model_state(#{ models := undefined }) ->
    undefined;
generate_initial_model_state(Spec = #{ models := Gen, opts := Opts }) ->
    normalize_rand(Spec#{ stage => {generate, state} }),
    execute_generator(Gen, [Opts]).

generate_request(
        Spec = #{
            requests := Gen,
            state := State,
            model_state := undefined,
            opts := Opts
        }
) ->
    normalize_rand(Spec#{ stage => {generate, request} }),
    execute_generator(Gen, [State, Opts]);
generate_request(
        Spec = #{
            requests := Gen,
            state := State,
            model_state := ModelState,
            opts := Opts
        }
) ->
    normalize_rand(Spec#{ stage => {generate, request} }),
    StateReq = execute_generator(Gen, [State, Opts]),
    normalize_rand(Spec#{ stage => {generate, request} }),
    ModelReq = execute_generator(Gen, [ModelState, Opts]),
    {StateReq, ModelReq}.

execute_generator(Generators, Args) when is_list(Generators) ->
    execute_generator(pick(Generators), Args);
execute_generator(Generator, Args) when is_function(Generator) ->
    apply(Generator, Args);
execute_generator(ExplicitResult, _) ->
    ExplicitResult.

execute_request(
        Spec = #{ model_state := undefined, state := State, opts := Opts },
        Req
    ) ->
    normalize_rand(Spec#{ stage => {execute, request} }),
    do_request(State, Req, Opts);
execute_request(
        Spec = #{ model_state := ModelState, state := State, opts := Opts },
        {Req, ModelReq}
    ) ->
    normalize_rand(Spec#{ stage => {execute, request} }),
    StateRes = do_request(State, Req, Opts),
    normalize_rand(Spec#{ stage => {execute, request} }),
    ModelRes = do_request(ModelState, ModelReq, Opts),
    case {StateRes, ModelRes} of
        {{ok, NewState}, {ok, NewModelState}} ->
            {ok, NewState, NewModelState};
        {{error, Reason}, _} ->
            {error, request_error, Reason};
      {_, {error, Reason}} ->
            {error, model_request_error, Reason}
    end.

do_request(State, Req, Opts) when is_function(Req) ->
    Req(State, Opts);
do_request(State, Req, Opts) when is_map(Req) ->
    hb_ao:resolve(State, Req, Opts);
do_request(_, DirectResult, _Opts) ->
    DirectResult.

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

%%% Type generators.

%% Size constants.
-define(BUILTIN_TYPES, [int, float, string, key]).
-define(INT_MAX, 1 bsl 32).
-define(INT_TINY_MAX, 32).
-define(SMALL_INT_MAX, 256).
-define(BIG_INT_MAX, 1 bsl 256).
-define(STRING_MAX_LENGTH, small).

any() -> any(?BUILTIN_TYPES).
any(Types) -> (pick([ fun ?MODULE:Type/0 || Type <- Types ]))().

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

int() -> int(?INT_MAX).
int(Spec) when not is_integer(Spec) -> int(num(Spec));
int(Max) -> rand:uniform(Max).
int(Min, Max) -> num(Min) + rand:uniform(num(Max) - num(Min)).

num(Int) when is_integer(Int) -> Int;
num(tiny) -> ?INT_TINY_MAX;
num(small) -> ?SMALL_INT_MAX;
num(big) -> ?BIG_INT_MAX;
num(Max) -> Max.

float() -> ?MODULE:float(?INT_MAX).
float(small) -> rand:uniform_real() * (2 * ?SMALL_INT_MAX);
float(big) -> rand:uniform_real() * (2 * ?BIG_INT_MAX);
float(Max) -> rand:uniform_real() * (2 * Max).

string() -> string(?STRING_MAX_LENGTH).
string(MaxLen) -> string(MaxLen, 97, 122, [$/]).
string(MaxLen, MinChar, MaxChar, Forbidden) ->
    <<
        <<(pick(MinChar, MaxChar, Forbidden)):8>>
    ||
        _ <- lists:seq(1, int(1, MaxLen))
    >>.

key() -> key(tiny).
key(Len) -> string(Len).