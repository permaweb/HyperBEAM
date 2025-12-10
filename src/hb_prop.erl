-module(hb_prop).
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
    run_state_machines(
        Spec#{
            states => hb_opts:get(states, undefined, Spec),
            models => hb_opts:get(models, undefined, Spec),
            properties => hb_opts:get(properties, [], Spec),
            next => hb_opts:get(next, undefined, Spec),
            runs => hb_opts:get(runs, ?DEFAULT_RUNS, Spec),
            length => hb_opts:get(length, ?DEFAULT_LENGTH, Spec),
            opts => hb_opts:get(opts, #{}, Spec)
        }
    );
state_machine(_Spec) ->
    throw({invalid_spec, missing_request_generator}).

run_state_machines(#{ runs := 0 }) ->
    ok;
run_state_machines(Spec = #{ runs := Runs }) ->
    InitialState = generate_initial_state(Spec),
    ?event({generated_initial_state, InitialState}),
    InitialModelState = generate_initial_model_state(Spec),
    ResSequence =
        state_machine_loop(
            Spec#{
                state => InitialState,
                model_state => InitialModelState
            }
        ),
    ?event({run_result, ResSequence}),
    case lists:last(ResSequence) of
        {error, _Type, _Reason} ->
            {failure, InitialState, ResSequence};
        ok ->
            ?event(properties,
                {successful_sequence, [InitialState | ResSequence]}
            ),
            run_state_machines(Spec#{ runs => Runs - 1 })
    end.

state_machine_loop(#{ length := 0 }) -> [ok];
state_machine_loop(Spec = #{ length := SeqLen }) ->
    Req = generate_request(Spec),
    ?event(
        {evaluating_request,
            {request, Req}
        }
    ),
    case execute_request(Spec, Req) of
        {error, Type, Reason} ->
            [Req, {error, Type, Reason}];
        Result ->
            case enforce_properties(Spec, Req, Result) of
                ok ->
                    NextSpec = apply_next(Spec, Req, Result),
                    [Req|state_machine_loop(NextSpec#{ length => SeqLen - 1 })];
                {error, Type, Reason} ->
                    [Req, {error, Type, Reason}]
            end
    end.

generate_initial_state(#{ states := Gen, opts := Opts }) ->
    execute_generator(Gen, [Opts]).

generate_initial_model_state(#{ models := undefined }) ->
    undefined;
generate_initial_model_state(#{ models := Gen, opts := Opts }) ->
    execute_generator(Gen, [Opts]).

generate_request(#{ requests := Gen, state := State, opts := Opts }) ->
    execute_generator(Gen, [State, Opts]).

execute_generator(Generators, Args) when is_list(Generators) ->
    execute_generator(pick(Generators), Args);
execute_generator(Generator, Args) when is_function(Generator) ->
    apply(Generator, Args);
execute_generator(ExplicitResult, _) ->
    ExplicitResult.

execute_request(#{ model_state := undefined, state := State, opts := Opts }, Req) ->
    do_request(State, Req, Opts);
execute_request(#{ model_state := ModelState, state := State, opts := Opts }, Req) ->
    case {do_request(State, Req, Opts), do_request(ModelState, Req, Opts)} of
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
        false -> {error, Property};
        skip -> skip
    catch
        error:{badarity, _} -> downgrade;
        error:function_clause -> skip
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
        false -> {error, Property};
        skip -> skip
    catch
        error:{badarity, _} -> skip;
        error:function_clause -> skip
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
pick([]) -> error(cannot_pick_from_empty_list);
pick(List) when is_list(List) ->
    lists:nth(int(length(List)), List).
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