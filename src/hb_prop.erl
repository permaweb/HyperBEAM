-module(hb_prop).
-export([state_machine/3, state_machine/4, state_machine_with_model/4]).
-export([any/0, any/1, pick/1]).
-export([int/0, int/1, int/2, float/0, float/1]).
-export([string/0, string/1, string/4, key/0, key/1]).
-include("include/hb.hrl").

%%% Test workflows.

-define(DEFAULT_PROPERTY_RUNS, 10).
-define(DEFAULT_PROPERTY_SEQ_LEN, 10).

state_machine(InitialState, RequestGen, Properties) ->
    state_machine(InitialState, RequestGen, Properties, #{}).
state_machine(InitialState, RequestGen, Properties, Opts) ->
    Runs = hb_opts:get(property_runs, ?DEFAULT_PROPERTY_RUNS, Opts),
    SeqLen = hb_opts:get(property_seq_len, ?DEFAULT_PROPERTY_SEQ_LEN, Opts),
    state_machine(
        #{
            runs => Runs,
            seq_len => SeqLen,
            state => InitialState,
            model_state => undefined,
            request_gen => RequestGen,
            properties => Properties,
            opts => Opts
        }
    ).

state_machine_with_model(InitState, ModelInitState, RequestGen, Properties) ->
    state_machine_with_model(InitState, ModelInitState, RequestGen, Properties, #{}).
state_machine_with_model(InitState, ModelInitState, RequestGen, Properties, Opts) ->
    Runs = hb_opts:get(property_runs, ?DEFAULT_PROPERTY_RUNS, Opts),
    SeqLen = hb_opts:get(property_seq_len, ?DEFAULT_PROPERTY_SEQ_LEN, Opts),
    state_machine(
        #{
            runs => Runs,
            seq_len => SeqLen,
            state => InitState,
            model_state => ModelInitState,
            request_gen => RequestGen,
            properties => Properties,
            opts => Opts
        }
    ).

state_machine(#{ runs := 0 }) ->
    ok;
state_machine(Spec = #{ runs := Runs }) ->
    case state_machine_loop(Spec) of
        ok -> state_machine(Spec#{ runs => Runs - 1 });
        {error, Reason} -> {error, Reason}
    end.

state_machine_loop(#{ seq_len := 0 }) -> ok;
state_machine_loop(Spec) ->
    case execute_request(Spec, Req = generate_request(Spec)) of
        {error, Reason} ->
            {error, Reason};
        Result ->
            case enforce_properties(Spec, Req, Result) of
                ok ->
                    [
                        Req
                    |
                        state_machine_loop(apply_result(Spec, Result))
                    ];
                {error, Reason} ->
                    [Req, {error, Reason}]
            end
    end.

generate_request(#{ request_gen := RequestGen, state := State, opts := Opts }) ->
    generate_request(State, RequestGen, Opts).
generate_request(State, RequestGen, Opts) when is_list(RequestGen) ->
    generate_request(State, pick(RequestGen), Opts);
generate_request(State, RequestGen, Opts) when is_function(RequestGen) ->
    RequestGen(State, Opts).

execute_request(#{ model_state := undefined, state := State, opts := Opts }, Req) ->
    hb_ao:resolve(State, Req, Opts);
execute_request(#{ model_state := ModelState, state := State, opts := Opts }, Req) ->
    case {hb_ao:resolve(State, Req, Opts), hb_ao:resolve(ModelState, Req, Opts)} of
        {{ok, NewState}, {ok, NewModelState}} ->
            {ok, NewState, NewModelState};
        {{error, Reason}, _} ->
            {primary_error, Reason};
      {_, {error, Reason}} ->
            {model_error, Reason}
    end.

enforce_properties(Spec = #{ properties := Properties }, Req, Result) ->
    enforce_properties(Properties, Req, Result, Spec).
enforce_properties([], _Req, _Result, _Spec) -> ok;
enforce_properties([Property | Properties], Req, Result, Spec) ->
    case {enforce_property(Property, Req, Result, Spec), Result} of
        {skip, {ok, NewState, _NewModelState}} ->
            case enforce_property(Property, Req, {ok, NewState}, Spec) of
                ok ->
                    enforce_properties(Properties, Req, Result, Spec);
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, _} ->
            enforce_properties(Properties, Req, Result, Spec);
        {error, Reason} ->
            {error, Reason}
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
        error:function_clause -> skip
    end.

apply_result(Spec = #{ model_state := undefined }, {ok, NewState}) ->
    Spec#{ state => NewState };
apply_result(Spec, {{ok, NewState}, {ok, NewModelState}}) ->
    Spec#{ model_state => NewModelState, state => NewState }.

%%% Type generators.

%% Size constants.
-define(BUILTIN_TYPES, [int, float, atom, string, key]).
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
string(MaxLen) -> string(MaxLen, 32, 126, [$/]).
string(MaxLen, MinChar, MaxChar, Forbidden) ->
    <<
        <<(pick(MinChar, MaxChar, Forbidden)):8>>
    ||
        _ <- lists:seq(1, int(1, MaxLen))
    >>.

key() -> key(tiny).
key(Len) -> string(Len).