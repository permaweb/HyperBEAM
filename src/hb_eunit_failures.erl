-module(hb_eunit_failures).

-behaviour(eunit_listener).

-export([start/0, start/1]).
-export([init/1, handle_begin/3, handle_end/3, handle_cancel/3, terminate/2]).

-record(state, {
    failed = [],
    skipped = [],
    cancelled = [],
    delay_ms = 100
}).

start() ->
    start([]).

start(Options) ->
    eunit_listener:start(?MODULE, Options).

init(Options) ->
    DelayMs = proplists:get_value(delay_ms, Options, 100),
    receive
        {start, _Reference} ->
            #state{delay_ms = DelayMs}
    end.

handle_begin(_Kind, _Data, St) ->
    St.

handle_end(_Kind, Data, St) ->
    case proplists:get_value(status, Data) of
        {error, _} ->
            Name = entity_name(Data),
            St#state{failed = [Name | St#state.failed]};
        {skipped, _Reason} ->
            Name = entity_name(Data),
            St#state{skipped = [Name | St#state.skipped]};
        _ ->
            St
    end.

handle_cancel(_Kind, Data, St) ->
    case entity_name(Data, false) of
        undefined ->
            St;
        Name ->
            St#state{cancelled = [Name | St#state.cancelled]}
    end.

terminate(_Result, #state{failed = [], skipped = [], cancelled = []}) ->
    ok;
terminate(_Result, #state{
    failed = FailedReversed,
    skipped = SkippedReversed,
    cancelled = CancelledReversed,
    delay_ms = DelayMs
}) ->
    %% EUnit always runs the tty listener plus report listeners in parallel.
    %% Delay slightly to let tty finish printing the detailed failure block,
    %% so this summary appears cleanly at the end of the run.
    timer:sleep(DelayMs),
    Failed = unique_names(lists:reverse(FailedReversed)),
    Skipped = unique_names(lists:reverse(SkippedReversed)),
    Cancelled = unique_names(lists:reverse(CancelledReversed)),
    FailedSkippedOrCancelled = unique_names(Failed ++ Skipped ++ Cancelled),
    FailedSkippedOrCancelledTests = unique_names(
        runnable_test_selectors(FailedSkippedOrCancelled)
    ),
    FailedOrCancelledModules = unique_names(
        lists:filtermap(
            fun module_from_test_selector/1,
            FailedSkippedOrCancelledTests
        )
    ),
    print_summary_block("Failed tests summary", FailedSkippedOrCancelled),
    print_csv_block("Failed tests (--test=...)", FailedSkippedOrCancelledTests),
    print_csv_block("Unique failing modules (--module=...)", FailedOrCancelledModules),
    io:format("~n", []),
    ok.

entity_name(Data) ->
    entity_name(Data, true).

entity_name(Data, IncludeIdFallback) ->
    case proplists:get_value(source, Data) of
        {Module, Function, _Arity} when is_atom(Module), is_atom(Function) ->
            lists:concat([atom_to_list(Module), ":", atom_to_list(Function)]);
        {Module, Function} when is_atom(Module), is_atom(Function) ->
            lists:concat([atom_to_list(Module), ":", atom_to_list(Function)]);
        _ ->
            case desc_or_name(Data) of
                undefined when IncludeIdFallback ->
                    io_lib:format("~tp", [proplists:get_value(id, Data)]);
                undefined ->
                    undefined;
                Desc ->
                    Desc
            end
    end.

desc_or_name(Data) ->
    case proplists:get_value(desc, Data) of
        Desc when is_binary(Desc), Desc =/= <<>> ->
            binary_to_list(Desc);
        Desc when is_list(Desc), Desc =/= [] ->
            Desc;
        _ ->
            case proplists:get_value(name, Data) of
                Name when is_binary(Name), Name =/= <<>> ->
                    binary_to_list(Name);
                Name when is_list(Name), Name =/= [] ->
                    Name;
                _ ->
                    undefined
            end
    end.

unique_names(Names) ->
    lists:reverse(
        lists:foldl(
            fun(Name, Acc) ->
                case lists:member(Name, Acc) of
                    true -> Acc;
                    false -> [Name | Acc]
                end
            end,
            [],
            Names
        )
    ).

runnable_test_selectors(Names) ->
    lists:filtermap(
        fun(Name) ->
            case is_test_selector(Name) of
                true -> {true, Name};
                false -> false
            end
        end,
        Names
    ).

is_test_selector(Name) when is_list(Name) ->
    case lists:member($:, Name) of
        true ->
            true;
        false ->
            false
    end;
is_test_selector(_) ->
    false.

module_from_test_selector(Name) ->
    case lists:splitwith(fun(Char) -> Char =/= $: end, Name) of
        {[], _} ->
            false;
        {Module, [$: | _Rest]} ->
            {true, Module};
        {_Module, []} ->
            false
    end.

print_summary_block(_Title, []) ->
    ok;
print_summary_block(Title, Names) ->
    io:format("~n~ts (~B):~n", [Title, length(Names)]),
    lists:foreach(
        fun(Name) ->
            io:format(" - ~ts~n", [Name])
        end,
        Names
    ).

print_csv_block(_Title, []) ->
    ok;
print_csv_block(Title, Names) ->
    io:format("~n~ts (~B):~n", [Title, length(Names)]),
    io:format("~ts~n", [string:join(Names, ",")]).
