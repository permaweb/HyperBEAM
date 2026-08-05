%%% @doc A finite-order Markov model over ASCII symbols.
-module(dev_markov).
-implements(<<"markov@1.0">>).
-export([
    info/0,
    train/3,
    likelihood/3,
    surprisal/3,
    mean_surprisal/3,
    perplexity/3,
    generate/3
]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_ORDER, 2).
-define(START, <<"|start|">>).
-define(END, <<"|end|">>).
-define(RNG_RANGE, (1 bsl 256)).

%% @doc Use `~message@1.0' for keys this device does not implement.
info() -> #{}.

%% @doc Add the requested samples to the model.
train(Base, Req, Opts) ->
    maybe
        {ok, Samples} ?= input(Req, false, Opts),
        {ok, Model, Order} ?= model_for_training(Base, Req, Opts),
        {ok, Base#{ <<"model">> => train_samples(Samples, Model, Order, Opts) }}
    end.

%% @doc Return the probability of the requested samples.
likelihood(Base, Req, Opts) ->
    maybe
        {ok, Exact} ?= exact_likelihood(Base, Req, Opts),
        result(Exact, result_mode(Base, Req, Opts))
    end.

%% @doc Return the samples' total self-information in bits.
surprisal(Base, Req, Opts) ->
    maybe
        {ok, Exact} ?= exact_likelihood(Base, Req, Opts),
        {ok, surprise(Exact)}
    end.

%% @doc Return the mean self-information per scored transition.
mean_surprisal(Base, Req, Opts) ->
    maybe
        {ok, Exact} ?= exact_likelihood(Base, Req, Opts),
        {ok, mean_surprise(Exact)}
    end.

%% @doc Return two to the mean self-information.
perplexity(Base, Req, Opts) ->
    maybe
        {ok, Exact} ?= exact_likelihood(Base, Req, Opts),
        {ok, perplexity(Exact)}
    end.

%% @doc Generate bytes from the model, optionally resuming a prior result.
generate(Base, Req, Opts) ->
    maybe
        {ok, Model, Order} ?= model(Base, Opts),
        {ok, Body} ?= generation_input(Req, Opts),
        {ok, Limit} ?= limit(Base, Req, Opts),
        {ok, Seed, Counter, Active} ?= generator(Req, Opts),
        generate(Body, context(Body, Order), Seed, Counter, Active, Limit,
            Model, Order, Opts)
    end.

%%% Model construction

%% @doc Load a model for training, or create one at the requested order.
model_for_training(Base, Req, Opts) ->
    case state(<<"model">>, Base, not_found, Opts) of
        not_found ->
            maybe
                {ok, Order} ?= requested_order(Base, Req, ?DEFAULT_ORDER, Opts),
                {ok, new_model(Order), Order}
            end;
        Existing ->
            maybe
                {ok, Model, ModelOrder} ?= validate_model(Existing, Opts),
                {ok, Order} ?= requested_order(Base, Req, ModelOrder, Opts),
                case Order of
                    ModelOrder -> {ok, Model, Order};
                    _ -> {error, 'order-mismatch'}
                end
            end
    end.

%% @doc Load and validate the model in the base message.
model(Base, Opts) ->
    case state(<<"model">>, Base, not_found, Opts) of
        not_found -> {error, 'model-not-found'};
        Existing -> validate_model(Existing, Opts)
    end.

%% @doc Construct an empty model at `Order'.
new_model(Order) ->
    #{
        <<"order">> => Order,
        <<"samples">> => 0,
        <<"transitions">> => #{}
    }.

%% @doc Validate the model's top-level shape and return its order.
validate_model(Model, Opts) when is_map(Model) ->
    Order = hb_maps:get(<<"order">>, Model, not_found, Opts),
    Samples = hb_maps:get(<<"samples">>, Model, not_found, Opts),
    Transitions = hb_maps:get(<<"transitions">>, Model, not_found, Opts),
    case is_integer(Order) andalso Order >= 0 andalso
            is_integer(Samples) andalso Samples >= 0 andalso
            is_map(Transitions) of
        true -> {ok, Model, Order};
        false -> {error, 'invalid-model'}
    end;
validate_model(_Model, _Opts) -> {error, 'invalid-model'}.

%% @doc Read an order from the request, then the base, then `Default'.
requested_order(Base, Req, Default, Opts) ->
    Value =
        case field(<<"order">>, Req, not_found, Opts) of
            not_found -> state(<<"order">>, Base, Default, Opts);
            RequestOrder -> RequestOrder
        end,
    non_negative(Value, 'invalid-order').

%% @doc Add every independent sample to `Model'.
train_samples(Samples, Model, Order, Opts) ->
    Transitions = hb_maps:get(<<"transitions">>, Model, #{}, Opts),
    NewTransitions = lists:foldl(
        fun(Sample, Acc) -> train_sample(Sample, Order, Acc, Opts) end,
        Transitions,
        Samples
    ),
    Model#{
        <<"samples">> => hb_maps:get(<<"samples">>, Model, 0, Opts) + length(Samples),
        <<"transitions">> => NewTransitions
    }.

%% @doc Add one bounded sample to the transition table.
train_sample(Sample, Order, Transitions, Opts) ->
    train_symbols(symbols(Sample) ++ [?END], lists:duplicate(Order, ?START),
        Order, Transitions, Opts).

%% @doc Increment each transition in a tokenized sample.
train_symbols([], _Context, _Order, Transitions, _Opts) -> Transitions;
train_symbols([Symbol | Rest], Context, Order, Transitions, Opts) ->
    ContextID = context_id(Context),
    SymbolID = symbol_id(Symbol),
    Outcomes = hb_maps:get(ContextID, Transitions, #{}, Opts),
    Count = hb_maps:get(SymbolID, Outcomes, 0, Opts),
    train_symbols(
        Rest,
        next_context(Context, Symbol, Order),
        Order,
        Transitions#{ ContextID => Outcomes#{ SymbolID => Count + 1 } },
        Opts
    ).

%%% Scoring

%% @doc Calculate the exact likelihood tuple for the requested samples.
exact_likelihood(Base, Req, Opts) ->
    maybe
        {ok, Model, Order} ?= model(Base, Opts),
        {ok, Samples} ?= input(Req, false, Opts),
        Transitions = hb_maps:get(<<"transitions">>, Model, #{}, Opts),
        score_samples(Samples, Order, Transitions, Opts)
    end.

%% @doc Multiply the likelihoods of independent samples.
score_samples(Samples, Order, Transitions, Opts) ->
    lists:foldl(
        fun(Sample, {ok, Acc}) ->
            score_symbols(symbols(Sample) ++ [?END],
                lists:duplicate(Order, ?START), Order, Transitions, Acc, Opts);
           (_Sample, Error) -> Error
        end,
        {ok, {1, 1, 0}},
        Samples
    ).

%% @doc Score every transition in one bounded sample.
score_symbols([], _Context, _Order, _Transitions, Exact, _Opts) -> {ok, Exact};
score_symbols([Symbol | Rest], Context, Order, Transitions, Exact, Opts) ->
    case ratio(Context, Symbol, Transitions, Opts) of
        {ok, Numerator, Denominator} ->
            score_symbols(Rest, next_context(Context, Symbol, Order), Order,
                Transitions, multiply(Exact, Numerator, Denominator), Opts);
        Error -> Error
    end.

%% @doc Return the observed count and total for one transition.
ratio(Context, Symbol, Transitions, Opts) ->
    case hb_maps:get(context_id(Context), Transitions, not_found, Opts) of
        not_found -> {ok, 0, 1};
        Outcomes when is_map(Outcomes) ->
            case counts(Outcomes, Opts) of
                {ok, Total} ->
                    {ok, hb_maps:get(symbol_id(Symbol), Outcomes, 0, Opts), Total};
                Error -> Error
            end;
        _ -> {error, 'invalid-model'}
    end.

%% @doc Validate and total an outcome-count message.
counts(Outcomes, Opts) ->
    case lists:foldl(
        fun(Key, {ok, Total}) ->
            case hb_maps:get(Key, Outcomes, not_found, Opts) of
                Count when is_integer(Count), Count > 0 -> {ok, Total + Count};
                _ -> {error, 'invalid-model'}
            end;
           (_Key, Error) -> Error
        end,
        {ok, 0},
        hb_maps:keys(Outcomes, Opts)
    ) of
        {ok, 0} -> {error, 'invalid-model'};
        Result -> Result
    end.

%% @doc Multiply a reduced likelihood by one transition ratio.
multiply({_Numerator, _Denominator, Events}, 0, _FactorDenominator) ->
    {0, 1, Events + 1};
multiply({0, _Denominator, Events}, _FactorNumerator, _FactorDenominator) ->
    {0, 1, Events + 1};
multiply({Numerator, Denominator, Events}, FactorNumerator, FactorDenominator) ->
    Left = gcd(Numerator, FactorDenominator),
    Right = gcd(FactorNumerator, Denominator),
    {
        (Numerator div Left) * (FactorNumerator div Right),
        (Denominator div Right) * (FactorDenominator div Left),
        Events + 1
    }.

%% @doc Return the greatest common divisor of two integers.
gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).

%% @doc Render an exact likelihood in the selected result mode.
result(_Exact, {error, _} = Error) -> Error;
result({Numerator, Denominator, Events}, integer) ->
    {ok, #{
        <<"numerator">> => Numerator,
        <<"denominator">> => Denominator,
        <<"events">> => Events
    }};
result(Exact, float) -> {ok, probability(Exact)}.

%% @doc Read and validate the requested likelihood result mode.
result_mode(Base, Req, Opts) ->
    case request_or_state(<<"result-mode">>, Base, Req, <<"float">>, Opts) of
        <<"float">> -> float;
        <<"integer">> -> integer;
        _ -> {error, 'invalid-result-mode'}
    end.

%% @doc Convert an exact likelihood to a float without first coercing its
%% potentially large integers.
probability({0, _Denominator, _Events}) -> 0.0;
probability({Numerator, Denominator, _Events}) ->
    math:pow(2.0, log2(Numerator) - log2(Denominator)).

%% @doc Return the total self-information of an exact likelihood.
surprise({0, _Denominator, _Events}) -> <<"infinity">>;
surprise({Numerator, Denominator, _Events}) -> log2(Denominator) - log2(Numerator).

%% @doc Return self-information per scored event.
mean_surprise({_Numerator, _Denominator, 0}) -> 0.0;
mean_surprise(Exact = {_Numerator, _Denominator, Events}) ->
    case surprise(Exact) of
        <<"infinity">> = Infinity -> Infinity;
        Surprise -> Surprise / Events
    end.

%% @doc Return two to the mean self-information.
perplexity({_Numerator, _Denominator, 0}) -> 1.0;
perplexity(Exact) ->
    case mean_surprise(Exact) of
        <<"infinity">> = Infinity -> Infinity;
        Mean when Mean > 1023.0 -> <<"infinity">>;
        Mean -> math:pow(2.0, Mean)
    end.

%% @doc Approximate the binary logarithm of an arbitrary positive integer.
log2(Integer) ->
    Bits = bit_length(Integer),
    Shift = max(0, Bits - 53),
    math:log2(float(Integer bsr Shift)) + Shift.

%% @doc Return the number of significant bits in a positive integer.
bit_length(Integer) ->
    Binary = binary:encode_unsigned(Integer),
    (byte_size(Binary) - 1) * 8 + byte_bits(binary:first(Binary)).

%% @doc Return the number of significant bits in a positive byte.
byte_bits(Byte) when Byte >= 128 -> 8;
byte_bits(Byte) when Byte >= 64 -> 7;
byte_bits(Byte) when Byte >= 32 -> 6;
byte_bits(Byte) when Byte >= 16 -> 5;
byte_bits(Byte) when Byte >= 8 -> 4;
byte_bits(Byte) when Byte >= 4 -> 3;
byte_bits(Byte) when Byte >= 2 -> 2;
byte_bits(_Byte) -> 1.

%%% Generation

%% @doc Continue generation until the model terminates or the limit is met.
generate(Body, _Context, Seed, Counter, false, _Limit, _Model, _Order, _Opts) ->
    {ok, generated(Body, Seed, Counter, false)};
generate(Body, Context, Seed, Counter, true, Limit, Model, Order, Opts) ->
    case Limit =/= false andalso byte_size(Body) >= Limit of
        true -> {ok, generated(Body, Seed, Counter, true)};
        false ->
            Transitions = hb_maps:get(<<"transitions">>, Model, #{}, Opts),
            case draw(Context, Seed, Counter, Transitions, Opts) of
                {ok, ?END, NextCounter} ->
                    {ok, generated(Body, Seed, NextCounter, false)};
                {ok, SymbolID, NextCounter} ->
                    case symbol(SymbolID) of
                        {ok, Symbol} ->
                            generate(<<Body/binary, Symbol/binary>>,
                                next_context(Context, Symbol, Order), Seed,
                                NextCounter, true, Limit, Model, Order, Opts);
                        Error -> Error
                    end;
                Error -> Error
            end
    end.

%% @doc Construct a terminated result or resumable continuation.
generated(Body, Seed, Counter, true) ->
    #{
        <<"body">> => Body,
        <<"seed">> => Seed,
        <<"continues">> => #{ <<"seed">> => Seed, <<"counter">> => Counter }
    };
generated(Body, Seed, _Counter, false) ->
    #{ <<"body">> => Body, <<"seed">> => Seed, <<"continues">> => false }.

%% @doc Draw the next symbol from a context's outcome counts.
draw(Context, Seed, Counter, Transitions, Opts) ->
    case hb_maps:get(context_id(Context), Transitions, not_found, Opts) of
        not_found -> {error, 'context-not-found'};
        Outcomes when is_map(Outcomes) ->
            maybe
                {ok, Total} ?= counts(Outcomes, Opts),
                case Total =< ?RNG_RANGE of
                    true -> draw_outcome(Seed, Counter, Outcomes, Total, Opts);
                    false -> {error, 'invalid-model'}
                end
            end;
        _ -> {error, 'invalid-model'}
    end.

%% @doc Draw an unbiased position from an outcome-count total.
draw_outcome(Seed, Counter, Outcomes, Total, Opts) when Counter < (1 bsl 64) ->
    Random = binary:decode_unsigned(
        crypto:hash(sha256, <<Seed/binary, Counter:64/unsigned-big>>)
    ),
    NextCounter = Counter + 1,
    Ceiling = ?RNG_RANGE - (?RNG_RANGE rem Total),
    case Random < Ceiling of
        true -> choose(lists:sort(hb_maps:keys(Outcomes, Opts)),
            Random rem Total, Outcomes, NextCounter, Opts);
        false -> draw_outcome(Seed, NextCounter, Outcomes, Total, Opts)
    end;
draw_outcome(_Seed, _Counter, _Outcomes, _Total, _Opts) ->
    {error, 'invalid-continuation'}.

%% @doc Select the symbol at a weighted position.
choose([Symbol | Rest], Position, Outcomes, Counter, Opts) ->
    Count = hb_maps:get(Symbol, Outcomes, 0, Opts),
    case Position < Count of
        true -> {ok, Symbol, Counter};
        false -> choose(Rest, Position - Count, Outcomes, Counter, Opts)
    end;
choose([], _Position, _Outcomes, _Counter, _Opts) -> {error, 'invalid-model'}.

%% @doc Read a new generator seed or a prior continuation.
generator(Req, Opts) ->
    case field(<<"continues">>, Req, not_found, Opts) of
        not_found -> seed(field(<<"seed">>, Req, not_found, Opts));
        false -> terminated_seed(field(<<"seed">>, Req, not_found, Opts));
        Continuation when is_map(Continuation) -> continuation(Continuation, Opts);
        _ -> {error, 'invalid-continuation'}
    end.

%% @doc Validate a supplied seed, or create one when absent.
seed(not_found) -> {ok, crypto:strong_rand_bytes(32), 0, true};
seed(Seed) when is_binary(Seed) -> {ok, Seed, 0, true};
seed(_Seed) -> {error, 'invalid-seed'}.

%% @doc Validate the seed carried by a terminated result.
terminated_seed(Seed) when is_binary(Seed) -> {ok, Seed, 0, false};
terminated_seed(_Seed) -> {error, 'invalid-seed'}.

%% @doc Validate a resumable generator state.
continuation(Continuation, Opts) ->
    Seed = hb_maps:get(<<"seed">>, Continuation, not_found, Opts),
    Counter = hb_maps:get(<<"counter">>, Continuation, not_found, Opts),
    case is_binary(Seed) andalso is_integer(Counter) andalso
            Counter >= 0 andalso Counter < (1 bsl 64) of
        true -> {ok, Seed, Counter, true};
        false -> {error, 'invalid-continuation'}
    end.

%% @doc Read a false or non-negative total output limit.
limit(Base, Req, Opts) ->
    Value = request_or_state(<<"limit">>, Base, Req, false, Opts),
    case Value of
        false -> {ok, false};
        _ -> non_negative(Value, 'invalid-limit')
    end.

%%% Input and symbol representation

%% @doc Read and validate one sample or a list of independent samples.
input(Req, AllowMissing, Opts) ->
    case targeted(Req, AllowMissing, Opts) of
        {ok, Value} when is_binary(Value) -> valid_samples([Value]);
        {ok, Values} when is_list(Values); is_map(Values) ->
            try valid_samples(hb_util:message_to_ordered_list(Values, Opts))
            catch _:_ -> {error, 'invalid-input'}
            end;
        Error -> Error
    end.

%% @doc Read the ASCII prefix from which generation should continue.
generation_input(Req, Opts) ->
    case targeted(Req, true, Opts) of
        {ok, Body} when is_binary(Body) ->
            case ascii(Body) of
                true -> {ok, Body};
                false -> {error, 'invalid-input'}
            end;
        {ok, _Other} -> {error, 'invalid-input'};
        Error -> Error
    end.

%% @doc Resolve the request path named by `target'.
targeted(Req, AllowMissing, Opts) ->
    Target = field(<<"target">>, Req, <<"body">>, Opts),
    case hb_ao:get(Target, {as, <<"message@1.0">>, Req}, not_found, Opts) of
        not_found when AllowMissing, Target =:= <<"body">> -> {ok, <<>>};
        not_found -> {error, 'target-not-found'};
        Value -> {ok, Value}
    end.

%% @doc Validate a list of ASCII binary samples.
valid_samples(Samples) ->
    case lists:all(fun(Sample) -> is_binary(Sample) andalso ascii(Sample) end,
            Samples) of
        true -> {ok, Samples};
        false -> {error, 'invalid-input'}
    end.

%% @doc Return whether every byte is ASCII.
ascii(Binary) -> lists:all(fun(Byte) -> Byte < 128 end, binary_to_list(Binary)).

%% @doc Split an ASCII binary into one-byte symbols.
symbols(Binary) -> [<<Byte>> || <<Byte>> <= Binary].

%% @doc Encode a symbol as a case-insensitive AO-Core map key.
symbol_id(?START) -> ?START;
symbol_id(?END) -> ?END;
symbol_id(<<Byte>>) ->
    <<"t", ($0 + Byte div 100), ($0 + (Byte div 10) rem 10), ($0 + Byte rem 10)>>.

%% @doc Decode a stored ASCII symbol ID.
symbol(<<"t", A, B, C>>)
        when A >= $0, A =< $1, B >= $0, B =< $9, C >= $0, C =< $9 ->
    Byte = (A - $0) * 100 + (B - $0) * 10 + C - $0,
    case Byte < 128 of
        true -> {ok, <<Byte>>};
        false -> {error, 'invalid-model'}
    end;
symbol(_Symbol) -> {error, 'invalid-model'}.

%% @doc Reconstruct the current context from generated output.
context(Body, Order) ->
    BodySymbols = symbols(Body),
    lists:sublist(
        lists:duplicate(Order, ?START) ++ BodySymbols,
        length(BodySymbols) + 1,
        Order
    ).

%% @doc Encode an ordered context as a case-insensitive AO-Core map key.
context_id(Symbols) ->
    iolist_to_binary([<<"c">>, [[<<"-">>, symbol_id(Symbol)] || Symbol <- Symbols]]).

%% @doc Advance an ordered context by one symbol.
next_context(_Context, _Symbol, 0) -> [];
next_context([_ | Rest], Symbol, _Order) -> Rest ++ [Symbol].

%% @doc Parse a non-negative integer or return the requested error.
non_negative(Value, Error) ->
    case hb_util:safe_int(Value) of
        {ok, Integer} when Integer >= 0 -> {ok, Integer};
        _ -> {error, Error}
    end.

%% @doc Read a key from device state as a plain message.
state(Key, Base, Default, Opts) ->
    hb_ao:get(Key, {as, <<"message@1.0">>, Base}, Default, Opts).

%% @doc Read an untrusted request field without invoking its device.
field(Key, Message, Default, Opts) ->
    hb_maps:get(Key, Message, Default, Opts).

%% @doc Read a parameter from the request, then device state.
request_or_state(Key, Base, Req, Default, Opts) ->
    case field(Key, Req, not_found, Opts) of
        not_found -> state(Key, Base, Default, Opts);
        Value -> Value
    end.

%%% Tests

%% @doc Train, update, and calculate every score form.
training_and_scores_test() ->
    {ok, State} = train(#{}, #{ <<"body">> => [<<"aba">>, <<"abb">>] }, #{}),
    Request = #{ <<"body">> => <<"aba">>, <<"result-mode">> => <<"integer">> },
    ?assertEqual(
        {ok, #{ <<"numerator">> => 1, <<"denominator">> => 2, <<"events">> => 4 }},
        likelihood(State, Request, #{})
    ),
    ?assertEqual(
        {ok, 0.5},
        likelihood(State, #{ <<"body">> => <<"aba">> }, #{})
    ),
    ?assertEqual(
        {ok, 1.0},
        surprisal(State, Request, #{})
    ),
    ?assertEqual(
        {ok, 0.25},
        mean_surprisal(State, Request, #{})
    ),
    {ok, Perplexity} = perplexity(State, Request, #{}),
    ?assert(abs(Perplexity - math:pow(2.0, 0.25)) < 1.0e-12),
    {ok, Updated} = train(State, #{ <<"body">> => <<"aba">> }, #{}),
    ?assertEqual(
        3,
        hb_ao:get(<<"model/samples">>, Updated, #{})
    ).

%% @doc Ensure separate training samples do not create cross-sample edges.
samples_are_independent_test() ->
    {ok, State} = train(
        #{},
        #{ <<"body">> => [<<"a">>, <<"b">>], <<"order">> => 1 },
        #{}
    ),
    ?assertEqual(
        {ok, #{ <<"numerator">> => 0, <<"denominator">> => 1, <<"events">> => 3 }},
        likelihood(
            State,
            #{ <<"body">> => <<"ab">>, <<"result-mode">> => <<"integer">> },
            #{}
        )
    ).

%% @doc Resolve a custom target and reject an order change after training.
target_and_order_test() ->
    {ok, State} = train(
        #{ <<"order">> => 1 },
        #{ <<"target">> => <<"word">>, <<"word">> => <<"abc">> },
        #{}
    ),
    ?assertEqual(
        1,
        hb_ao:get(<<"model/order">>, State, #{})
    ),
    ?assertEqual(
        {error, 'order-mismatch'},
        train(State, #{ <<"body">> => <<"abc">>, <<"order">> => 2 }, #{})
    ).

%% @doc Ensure limited generation resumes exactly and seeded draws are stable.
generation_continuation_test() ->
    Text = <<"abcdefghijklmnopqrstuvwxyz">>,
    {ok, State} = train(#{}, #{ <<"body">> => Text }, #{}),
    Seed = <<"repeatable">>,
    {ok, First} = generate(
        State,
        #{ <<"seed">> => Seed, <<"limit">> => 10 },
        #{}
    ),
    ?assertEqual(
        10,
        byte_size(hb_maps:get(<<"body">>, First, #{}, #{}))
    ),
    {ok, Resumed} = generate(State, First#{ <<"limit">> => 20 }, #{}),
    {ok, Direct} = generate(
        State,
        #{ <<"seed">> => Seed, <<"limit">> => 20 },
        #{}
    ),
    ?assertEqual(
        Direct,
        Resumed
    ),
    {ok, Complete} = generate(State, Resumed#{ <<"limit">> => false }, #{}),
    ?assertEqual(
        Text,
        hb_maps:get(<<"body">>, Complete, #{}, #{})
    ),
    ?assertEqual(
        false,
        hb_maps:get(<<"continues">>, Complete, #{}, #{})
    ),
    ?assertEqual(
        {ok, Complete},
        generate(State, Complete#{ <<"limit">> => 100 }, #{})
    ),
    {ok, RandomState} = train(
        #{},
        #{ <<"body">> => [<<"a">>, <<"b">>], <<"order">> => 0 },
        #{}
    ),
    ?assertMatch(
        {ok, #{ <<"body">> := <<"b">>, <<"continues">> := false }},
        generate(RandomState, #{ <<"seed">> => <<"seed">> }, #{})
    ).

%% @doc Exercise the device through AO-Core and a real HTTP/cache round-trip.
resolution_and_http_test() ->
    {ok, RoutedState} = hb_ao:resolve(
        #{ <<"device">> => <<"markov@1.0">> },
        #{ <<"path">> => <<"train">>, <<"body">> => [<<"aba">>, <<"abb">>] },
        #{}
    ),
    ?assertEqual(
        {ok, 0.5},
        hb_ao:resolve(
            RoutedState,
            #{ <<"path">> => <<"likelihood">>, <<"body">> => <<"aba">> },
            #{}
        )
    ),
    State = hb_maps:remove(<<"device">>, RoutedState, #{}),
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    {ok, ID} = hb_cache:write(State, Opts),
    Node = hb_http_server:start_node(Opts#{ <<"port">> => 0 }),
    ?assertEqual(
        {ok, 0.5},
        hb_http:get(
            Node,
            <<"/", ID/binary, "~markov@1.0/likelihood&body=aba">>,
            Opts
        )
    ).

%% @doc Reject malformed requests at each public boundary.
validation_test() ->
    ?assertEqual(
        {error, 'model-not-found'},
        likelihood(#{}, #{ <<"body">> => <<"a">> }, #{})
    ),
    ?assertEqual(
        {error, 'invalid-input'},
        train(#{}, #{ <<"body">> => <<255>> }, #{})
    ),
    ?assertEqual(
        {error, 'invalid-order'},
        train(#{}, #{ <<"body">> => <<"a">>, <<"order">> => -1 }, #{})
    ),
    {ok, State} = train(#{}, #{ <<"body">> => <<"a">> }, #{}),
    ?assertEqual(
        {error, 'invalid-result-mode'},
        likelihood(
            State,
            #{ <<"body">> => <<"a">>, <<"result-mode">> => <<"json">> },
            #{}
        )
    ),
    ?assertEqual(
        {error, 'invalid-limit'},
        generate(State, #{ <<"limit">> => -1 }, #{})
    ).
