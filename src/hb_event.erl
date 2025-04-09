-module(hb_event).
-moduledoc """
Wrapper for incrementing prometheus counters.
""".
-export([log/1, log/2, log/3, log/4, log/5, log/6, increment/3]).
-include("include/hb.hrl").

-define(OVERLOAD_QUEUE_LENGTH, 10000).

-doc """
Debugging log logging function. For now, it just prints to standard
error.
""".
log(X) -> log(global, X).
log(Topic, X) -> log(Topic, X, "").
log(Topic, X, Mod) -> log(Topic, X, Mod, undefined).
log(Topic, X, Mod, Func) -> log(Topic, X, Mod, Func, undefined).
log(Topic, X, Mod, Func, Line) -> log(Topic, X, Mod, Func, Line, #{}).
log(Topic, X, Mod, undefined, Line, Opts) -> log(Topic, X, Mod, "", Line, Opts);
log(Topic, X, Mod, Func, undefined, Opts) -> log(Topic, X, Mod, Func, "", Opts);
log(Topic, X, ModAtom, Func, Line, Opts) when is_atom(ModAtom) ->
    % Increment by message adding Topic as label
    increment(Topic, X, Opts),
    % Check if the module has the `hb_debug` attribute set to `print`.
    case lists:member({hb_debug, [print]}, ModAtom:module_info(attributes)) of
        true -> hb_util:debug_print(X, atom_to_list(ModAtom), Func, Line);
        false -> 
            % Check if the module has the `hb_debug` attribute set to `no_print`.
            case lists:keyfind(hb_debug, 1, ModAtom:module_info(attributes)) of
                {hb_debug, [no_print]} -> X;
                _ -> log(Topic, X, atom_to_list(ModAtom), Func, Line, Opts)
            end
    end;
log(Topic, X, ModStr, Func, Line, Opts) ->
    % Check if the debug_print option has the topic in it if set.
    case hb_opts:get(debug_print, false, Opts) of
        ModList when is_list(ModList) ->
            case lists:member(ModStr, ModList)
                orelse lists:member(atom_to_list(Topic), ModList)
            of
                true -> hb_util:debug_print(X, ModStr, Func, Line);
                false -> X
            end;
        true -> hb_util:debug_print(X, ModStr, Func, Line);
        false -> X
    end.

-doc """
Increment the counter for the given topic and message. Registers the
counter if it doesn't exist. If the topic is `global`, the message is ignored.
This means that events must specify a topic if they want to be counted,
filtering debug messages. Similarly, events with a topic that begins with
`debug` are ignored.
""".
increment(global, _Message, _Opts) -> ignored;
increment(ao_core, _Message, _Opts) -> ignored;
increment(ao_internal, _Message, _Opts) -> ignored;
increment(ao_devices, _Message, _Opts) -> ignored;
increment(ao_subresolution, _Message, _Opts) -> ignored;
increment(signature_base, _Message, _Opts) -> ignored;
increment(id_base, _Message, _Opts) -> ignored;
increment(parsing, _Message, _Opts) -> ignored;
increment(Topic, Message, _Opts) ->
    case parse_name(Message) of
        <<"debug", _/binary>> -> ignored;
        EventName ->
            TopicBin = parse_name(Topic),
            case hb_name:lookup(?MODULE) of
                Pid when is_pid(Pid) ->
                    Pid ! {increment, TopicBin, EventName};
                undefined ->
                    PID = spawn(fun() -> server() end),
                    hb_name:register(?MODULE, PID),
                    PID ! {increment, TopicBin, EventName}
            end
    end.

server() ->
    await_prometheus_started(),
    prometheus_counter:declare(
        [
            {name, <<"event">>},
            {help, <<"AO-Core execution events">>},
            {labels, [topic, event]}
        ]),
    handle_events().
handle_events() ->
    receive
        {increment, TopicBin, EventName} ->
            case erlang:process_info(self(), message_queue_len) of
                {message_queue_len, Len} when Len > ?OVERLOAD_QUEUE_LENGTH ->
                    ?debug_print(
                        {warning,
                            prometheus_event_queue_overloading,
                            {queue, Len},
                            {current_message, EventName}
                        }
                    );
                _ -> ignored
            end,
            prometheus_counter:inc(<<"event">>, [TopicBin, EventName]),
            handle_events()
    end.

-doc """
Delay the event server until prometheus is started.
""".
await_prometheus_started() ->
    receive
        Msg ->
            case application:get_application(prometheus) of
                undefined -> await_prometheus_started();
                _ -> self() ! Msg, ok
            end
    end.

parse_name(Name) when is_tuple(Name) ->
    parse_name(element(1, Name));
parse_name(Name) when is_atom(Name) ->
    atom_to_binary(Name, utf8);
parse_name(Name) when is_binary(Name) ->
    Name;
parse_name(Name) when is_list(Name) ->
    iolist_to_binary(Name).
