# hb_format

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_format.erl)

Formatting and debugging utilities for HyperBEAM.
This module provides text formatting capabilities for debugging output,
message pretty-printing, stack trace formatting, and human-readable
representations of binary data and cryptographic identifiers.
The functions in this module are primarily used for development and
debugging purposes, supporting the logging and diagnostic infrastructure
throughout the HyperBEAM system.

---

## Exported Functions

- `binary/1`
- `error/2`
- `escape_format/1`
- `eunit_print/2`
- `get_trace/1`
- `indent_lines/2`
- `indent/2`
- `indent/3`
- `indent/4`
- `maybe_multiline/3`
- `message/1`
- `message/2`
- `message/3`
- `print_trace_short/4`
- `print_trace/4`
- `print/1`
- `print/3`
- `print/4`
- `print/5`
- `remove_leading_noise/1`
- `remove_noise/1`
- `remove_trailing_noise/1`
- `short_id/1`
- `term/1`
- `term/2`
- `term/3`
- `trace_macro_helper/5`
- `trace_short/0`
- `trace_short/1`
- `trace_to_list/1`
- `trace/1`

---

### print

Formatting and debugging utilities for HyperBEAM.
Print a message to the standard error stream, prefixed by the amount

```erlang
print(X) ->
    print(X, <<>>, #{}).
```

### print

```erlang
print(X, Info, Opts) ->
    io:format(
        standard_error,
        "=== HB DEBUG ===~s==>~n~s~n",
        [Info, term(X, Opts, 0)]
    ),
    X.
```

### print

```erlang
print(X, Mod, Func, LineNum) ->
    print(X, format_debug_trace(Mod, Func, LineNum, #{}), #{}).
```

### print

```erlang
print(X, Mod, Func, LineNum, Opts) ->
    Now = erlang:system_time(millisecond),
    Last = erlang:put(last_debug_print, Now),
    TSDiff = case Last of undefined -> 0; _ -> Now - Last end,
    Info =
        hb_util:bin(
            io_lib:format(
                "[~pms in ~s @ ~s]",
                [
                    TSDiff,
                    case server_id() of
                        undefined -> hb_util:bin(io_lib:format("~p", [self()]));
                        ServerID ->
                            hb_util:bin(
                                io_lib:format(
                                    "~s (~p)",
                                    [short_id(ServerID), self()]
                                )
                            )
                    end,
                    format_debug_trace(Mod, Func, LineNum, Opts)
                ]
            )
        ),
    print(X, Info, Opts).
```

### server_id

Retreive the server ID of the calling process, if known.

```erlang
server_id() ->
    server_id(#{ server_id => undefined }).
```

### server_id

```erlang
server_id(Opts) ->
    case hb_opts:get(server_id, undefined, Opts) of
        undefined -> get(server_id);
        ServerID -> ServerID
    end.
```

### format_debug_trace

Generate the appropriate level of trace for a given call.

```erlang
format_debug_trace(Mod, Func, Line, Opts) ->
    case hb_opts:get(debug_print_trace, false, #{}) of
        short ->
            Trace =
                case hb_opts:get(debug_trace_type, erlang, Opts) of
                    erlang -> get_trace(erlang);
                    ao ->
                        % If we are printing AO-Core traces, we add the module
                        % and line number to the end to show exactly where in
                        % the handler-flow the event arose.
```

### term

Convert a term to a string for debugging print purposes.

```erlang
term(X) -> term(X, #{}).
```

### term

Convert a term to a string for debugging print purposes.

```erlang
term(X, Opts) -> term(X, Opts, 0).
```

### term

Convert a term to a string for debugging print purposes.

```erlang
term(X, Opts, Indent) ->
    try do_debug_fmt(X, Opts, Indent)
    catch A:B:C ->
        Mode = hb_opts:get(mode, prod, Opts),
        PrintFailPreference = hb_opts:get(debug_print_fail_mode, quiet, Opts),
        case {Mode, PrintFailPreference} of
            {debug, quiet} ->
                indent("[!Format failed!] ~p", [X], Opts, Indent);
            {debug, _} ->
                indent(
                    "[PRINT FAIL:] ~80p~n===== PRINT ERROR WAS ~p:~p =====~n~s",
                    [
                        X,
                        A,
                        B,
                        hb_util:bin(
                            format_trace(
                                C,
                                hb_opts:get(stack_print_prefixes, [], #{})
                            )
                        )
                    ],
                    Opts,
                    Indent
                );
            _ ->
                indent("[!Format failed!]", [], Opts, Indent)
        end
    end.
```

### do_debug_fmt

```erlang
do_debug_fmt(
    { { {rsa, _PublicExpnt1}, _Priv1, _Priv2 },
      { {rsa, _PublicExpnt2}, Pub }
    },
    Opts, Indent
) ->
    format_address(Pub, Opts, Indent);
```

### do_debug_fmt

```erlang
do_debug_fmt(
    { AtomValue,
      {
        { {rsa, _PublicExpnt1}, _Priv1, _Priv2 },
        { {rsa, _PublicExpnt2}, Pub }
      }
    },
    Opts, Indent
) ->
    AddressString = format_address(Pub, Opts, Indent),
    indent("~p: ~s", [AtomValue, AddressString], Opts, Indent);
```

### do_debug_fmt

```erlang
do_debug_fmt({explicit, X}, Opts, Indent) ->
    indent("[Explicit:] ~p", [X], Opts, Indent);
```

### do_debug_fmt

```erlang
do_debug_fmt({string, X}, Opts, Indent) ->
    indent("~s", [X], Opts, Indent);
```

### do_debug_fmt

```erlang
do_debug_fmt({trace, Trace}, Opts, Indent) ->
    indent("~n~s", [trace(Trace)], Opts, Indent);
```

### do_debug_fmt

```erlang
do_debug_fmt({as, undefined, Msg}, Opts, Indent) ->
    "\n" ++ indent("Subresolve => ", [], Opts, Indent) ++
        maybe_multiline(Msg, Opts, Indent + 1);
```

### do_debug_fmt

```erlang
do_debug_fmt({as, DevID, Msg}, Opts, Indent) ->
    "\n" ++ indent("Subresolve as ~s => ", [DevID], Opts, Indent) ++
        maybe_multiline(Msg, Opts, Indent + 1);
```

### do_debug_fmt

```erlang
do_debug_fmt({X, Y}, Opts, Indent) when is_atom(X) and is_atom(Y) ->
    indent("~p: ~p", [X, Y], Opts, Indent);
```

### do_debug_fmt

```erlang
do_debug_fmt({X, Y}, Opts, Indent) when is_record(Y, tx) ->
    indent("~p: [TX item]~n~s",
        [X, ar_bundles:format(Y, Indent + 1, Opts)],
        Opts,
        Indent
    );
```

### do_debug_fmt

```erlang
do_debug_fmt({X, Y}, Opts, Indent) when is_map(Y); is_list(Y) ->
    Formatted = maybe_multiline(Y, Opts, Indent + 1),
    indent(
        case is_binary(X) of
            true -> "~s";
            false -> "~p"
        end ++ "~s",
        [
            X,
            case is_multiline(Formatted) of
                true -> " ==>" ++ Formatted;
                false -> ": " ++ Formatted
            end
        ],
        Opts,
        Indent
    );
```

### do_debug_fmt

```erlang
do_debug_fmt({X, Y}, Opts, Indent) ->
    indent(
        "~s: ~s",
        [
            remove_leading_noise(term(X, Opts, Indent)),
            remove_leading_noise(term(Y, Opts, Indent))
        ],
        Opts,
        Indent
    );
```

### do_debug_fmt

```erlang
do_debug_fmt(TX, Opts, Indent) when is_record(TX, tx) ->
    indent("[TX item]~n~s",
        [ar_bundles:format(TX, Indent, Opts)],
        Opts,
        Indent
    );
```

### do_debug_fmt

```erlang
do_debug_fmt(MaybePrivMap, Opts, Indent) when is_map(MaybePrivMap) ->
    Map = hb_private:reset(MaybePrivMap),
    case maybe_format_short(Map, Opts, Indent) of
        {ok, SimpleFmt} -> SimpleFmt;
        error ->
            "\n" ++ lists:flatten(message(Map, Opts, Indent))
    end;
```

### do_debug_fmt

```erlang
do_debug_fmt(Tuple, Opts, Indent) when is_tuple(Tuple) ->
    format_tuple(Tuple, Opts, Indent);
```

### do_debug_fmt

```erlang
do_debug_fmt(X, Opts, Indent) when is_binary(X) ->
    indent("~s", [binary(X)], Opts, Indent);
```

### do_debug_fmt

```erlang
do_debug_fmt(Str = [X | _], Opts, Indent) when is_integer(X) andalso X >= 32 andalso X < 127 ->
    indent("~s", [Str], Opts, Indent);
```

### do_debug_fmt

```erlang
do_debug_fmt(MsgList, Opts, Indent) when is_list(MsgList) ->
    format_list(MsgList, Opts, Indent);
```

### do_debug_fmt

If the user attempts to print a wallet, format it as an address.

```erlang
do_debug_fmt(X, Opts, Indent) ->
    indent("~80p", [X], Opts, Indent).
```

### format_address

If the user attempts to print a wallet, format it as an address.

```erlang
format_address(Wallet, Opts, Indent) ->
    indent("Wallet [Addr: ~s]",
        [short_id(hb_util:human_id(ar_wallet:to_address(Wallet)))], 
        Opts, 
        Indent
    ).
```

### format_tuple

Helper function to format tuples with arity greater than 2.

```erlang
format_tuple(Tuple, Opts, Indent) ->
    to_lines(lists:map(
        fun(Elem) ->
            term(Elem, Opts, Indent)
        end,
        tuple_to_list(Tuple)
    )).
```

### format_list

Format a list. Comes in three forms: all on one line, individual items

```erlang
format_list(MsgList, Opts, Indent) ->
    case maybe_format_short(MsgList, Opts, Indent) of
        {ok, SimpleFmt} -> SimpleFmt;
        error ->
            "\n" ++
                indent("List [~w] {", [length(MsgList)], Opts, Indent) ++
                format_list_lines(MsgList, Opts, Indent)
    end.
```

### format_list_lines

Format a list as a multi-line string.

```erlang
format_list_lines(MsgList, Opts, Indent) ->
    Numbered = hb_util:number(MsgList),
    Lines =
        lists:map(
            fun({N, Msg}) ->
                format_list_item(N, Msg, Opts, Indent)
            end,
            Numbered
        ),
    AnyLong =
        lists:any(
            fun({Mode, _}) -> Mode == multiline end,
            Lines
        ),
    case AnyLong of
        false ->
            "\n" ++
                remove_trailing_noise(
                    lists:flatten(
                        lists:map(
                            fun({_, Line}) ->
                                Line
                            end,
                            Lines
                        )
                    )
                ) ++
                "\n" ++
                indent("}", [], Opts, Indent);
        true ->
            "\n" ++
            lists:flatten(lists:map(
                fun({N, Msg}) ->
                    {_, Line} = format_list_item(multiline, N, Msg, Opts, Indent),
                    Line
                end,
                Numbered
            )) ++ indent("}", [], Opts, Indent)
    end.
```

### format_list_item

Format a single element of a list.

```erlang
format_list_item(N, Msg, Opts, Indent) ->
    case format_list_item(short, N, Msg, Opts, Indent) of
        {short, String} -> {short, String};
        error -> format_list_item(multiline, N, Msg, Opts, Indent)
    end.
```

### format_list_item

```erlang
format_list_item(short, N, Msg, Opts, Indent) ->
    case maybe_format_short(Msg, Opts, Indent) of
        {ok, SimpleFmt} ->
            {short, indent("~s => ~s~n", [N, SimpleFmt], Opts, Indent + 1)};
        error -> error
    end;
```

### format_list_item

```erlang
format_list_item(multiline, N, Msg, Opts, Indent) ->
    Formatted =
        case is_multiline(Base = term(Msg, Opts, Indent + 2)) of
            true -> Base;
            false -> remove_leading_noise(Base)
        end,
    {
        multiline,
        indent(
            "~s => ~s~n",
            [N, Formatted], 
            Opts,
            Indent + 1
        )
    }.
```

### to_lines

Join a list of strings and remove trailing noise.

```erlang
to_lines(Elems) ->
    remove_trailing_noise(do_to_lines(Elems)).
```

### do_to_lines

```erlang
do_to_lines([]) -> [];
```

### do_to_lines

```erlang
do_to_lines(In =[RawElem | Rest]) ->
    Elem = lists:flatten(RawElem),
    case lists:member($\n, Elem) of
        true -> lists:flatten(lists:join("\n", In));
        false -> Elem ++ ", " ++ do_to_lines(Rest)
    end.
```

### remove_noise

Remove any leading or trailing noise from a string.

```erlang
remove_noise(Str) ->
    remove_leading_noise(remove_trailing_noise(Str)).
```

### remove_leading_noise

Remove any leading whitespace from a string.

```erlang
remove_leading_noise(Str) ->
    remove_leading_noise(Str, ?NOISE_CHARS).
```

### remove_leading_noise

```erlang
remove_leading_noise(Bin, Noise) when is_binary(Bin) ->
    hb_util:bin(remove_leading_noise(hb_util:list(Bin), Noise));
```

### remove_leading_noise

```erlang
remove_leading_noise([], _) -> [];
```

### remove_leading_noise

```erlang
remove_leading_noise([Char|Str], Noise) ->
    case lists:member(Char, Noise) of
        true ->
            remove_leading_noise(Str, Noise);
        false -> [Char|Str]
    end.
```

### remove_trailing_noise

Remove trailing noise characters from a string. By default, this is

```erlang
remove_trailing_noise(Str) ->
    removing_trailing_noise(Str, ?NOISE_CHARS).
```

### removing_trailing_noise

```erlang
removing_trailing_noise(Bin, Noise) when is_binary(Bin) ->
    removing_trailing_noise(binary:bin_to_list(Bin), Noise);
```

### removing_trailing_noise

```erlang
removing_trailing_noise(BinList, Noise) when is_list(BinList) ->
    case lists:member(lists:last(BinList), Noise) of
        true ->
            removing_trailing_noise(lists:droplast(BinList), Noise);
        false -> BinList
    end.
```

### indent

Format a string with an indentation level.

```erlang
indent(Str, Indent) -> indent(Str, #{}, Indent).
```

### indent

Format a string with an indentation level.

```erlang
indent(Str, Opts, Indent) -> indent(Str, [], Opts, Indent).
```

### indent

Format a string with an indentation level.

```erlang
indent(FmtStr, Terms, Opts, Ind) ->
    IndentSpaces = hb_opts:get(debug_print_indent, Opts),
    EscapedFmt = escape_format(FmtStr),
    lists:droplast(
        lists:flatten(
            io_lib:format(
                [$\s || _ <- lists:seq(1, Ind * IndentSpaces)] ++
                    lists:flatten(EscapedFmt) ++ "\n",
                Terms
            )
        )
    ).
```

### escape_format

Escape a string for use as an io_lib:format specifier.

```erlang
escape_format(Str) when is_list(Str) ->
    re:replace(
        Str,
        "~([a-z\\-_]+@[0-9]+\\.[0-9]+)", "~~\\1",
        [global, {return, list}]
    );
```

### escape_format

Escape a string for use as an io_lib:format specifier.
Format an error message as a string.

```erlang
escape_format(Else) -> Else.
```

### error

Escape a string for use as an io_lib:format specifier.
Format an error message as a string.

```erlang
error(ErrorMsg, Opts) ->
    Type = hb_ao:get(<<"type">>, ErrorMsg, <<"">>, Opts),
    Details = hb_ao:get(<<"details">>, ErrorMsg, <<"">>, Opts),
    Stacktrace = hb_ao:get(<<"stacktrace">>, ErrorMsg, <<"">>, Opts),
    hb_util:bin(
        [
            <<"Termination type: '">>, Type,
            <<"'\n\nStacktrace:\n\n">>, Stacktrace,
            <<"\n\nError details:\n\n">>, Details
        ]
    ).
```

### indent_lines

Take a series of strings or a combined string and format as a

```erlang
indent_lines(Strings, Indent) when is_binary(Strings) ->
    indent_lines(binary:split(Strings, <<"\n">>, [global]), Indent);
```

### indent_lines

Take a series of strings or a combined string and format as a

```erlang
indent_lines(Strings, Indent) when is_list(Strings) ->
    hb_util:bin(lists:join(
        "\n",
        [
            indent(hb_util:list(String), #{}, Indent)
        ||
            String <- Strings
        ]
    )).
```

### binary

Format a binary as a short string suitable for printing.

```erlang
binary(Bin) ->
    case short_id(Bin) of
        undefined ->
            MaxBinPrint = hb_opts:get(debug_print_binary_max),
            Printable =
                binary:part(
                    Bin,
                    0,
                    case byte_size(Bin) of
                        X when X < MaxBinPrint -> X;
                        _ -> MaxBinPrint
                    end
                ),
            PrintSegment =
                case is_human_binary(Printable) of
                    true -> Printable;
                    false -> hb_util:encode(Printable)
                end,
            lists:flatten(
                [
                    "\"",
                    [PrintSegment],
                    case Printable == Bin of
                        true -> "\"";
                        false ->
                            io_lib:format(
                                "...\" <~s bytes>",
                                [hb_util:human_int(byte_size(Bin))]
                            )
                    end
                ]
            );
        ShortID ->
            lists:flatten(io_lib:format("~s", [ShortID]))
    end.
```

### maybe_multiline

Format a map as either a single line or a multi-line string depending

```erlang
maybe_multiline(X, Opts, Indent) ->
    case maybe_format_short(X, Opts, Indent) of
        {ok, SimpleFmt} -> SimpleFmt;
        error ->
            "\n" ++ lists:flatten(message(X, Opts, Indent))
    end.
```

### maybe_format_short

Attempt to generate a short formatting of a message, using the given

```erlang
maybe_format_short(X, Opts, _Indent) ->
    MaxLen = hb_opts:get(debug_print_map_line_threshold, 100, Opts),
    SimpleFmt =
        case is_binary(X) of
            true -> binary(X);
            false -> io_lib:format("~p", [X])
        end,
    case is_multiline(SimpleFmt) orelse (lists:flatlength(SimpleFmt) > MaxLen) of
        true -> error;
        false -> {ok, SimpleFmt}
    end.
```

### is_multiline

Is the given string a multi-line string?

```erlang
is_multiline(Str) ->
    lists:member($\n, Str).
```

### eunit_print

Format and print an indented string to standard error.

```erlang
eunit_print(FmtStr, FmtArgs) ->
    io:format(
        standard_error,
        "~n~s ",
        [indent(FmtStr ++ "...", FmtArgs, #{}, 4)]
    ).
```

### print_trace

Print the trace of the current stack, up to the first non-hyperbeam

```erlang
print_trace(Stack, CallMod, CallFunc, CallLine) ->
    print_trace(Stack, "HB TRACE",
        lists:flatten(io_lib:format("[~s:~w ~p]",
            [CallMod, CallLine, CallFunc])
    )).
```

### print_trace

```erlang
print_trace(Stack, Label, CallerInfo) ->
    io:format(standard_error, "=== ~s ===~s==>~n~s",
        [
            Label, CallerInfo,
            lists:flatten(trace(Stack))
        ]).
```

### trace

Format a stack trace as a list of strings, one for each stack frame.

```erlang
trace(Stack) ->
    format_trace(Stack, hb_opts:get(stack_print_prefixes, [], #{})).
```

### format_trace

```erlang
format_trace([], _) -> [];
```

### format_trace

```erlang
format_trace([Item|Rest], Prefixes) ->
    case element(1, Item) of
        Atom when is_atom(Atom) ->
            case true of %is_hb_module(Atom, Prefixes) of
                true ->
                    [
                        format_trace(Item, Prefixes) |
                        format_trace(Rest, Prefixes)
                    ];
                false -> []
            end;
        _ -> []
    end;
```

### format_trace

```erlang
format_trace({Func, ArityOrTerm, Extras}, Prefixes) ->
    format_trace({no_module, Func, ArityOrTerm, Extras}, Prefixes);
```

### format_trace

```erlang
format_trace({Mod, Func, ArityOrTerm, Extras}, _Prefixes) ->
    ExtraMap = hb_maps:from_list(Extras),
    indent(
        "~p:~p/~p [~s]~n",
        [
            Mod, Func, ArityOrTerm,
            case hb_maps:get(line, ExtraMap, undefined) of
                undefined -> "No details";
                Line ->
                    hb_maps:get(file, ExtraMap)
                        ++ ":" ++ integer_to_list(Line)
            end
        ],
        #{},
        1
    ).
```

### print_trace_short

Print a trace to the standard error stream.

```erlang
print_trace_short(Trace, Mod, Func, Line) ->
    io:format(standard_error, "=== [ HB SHORT TRACE ~p:~w ~p ] ==> ~s~n",
        [
            Mod, Line, Func,
            trace_short(Trace)
        ]
    ).
```

### trace_to_list

Return a list of calling modules and lines from a trace, removing all

```erlang
trace_to_list(Trace) ->
    Prefixes = hb_opts:get(stack_print_prefixes, [], #{}),
    lists:filtermap(
        fun(TraceItem) when is_binary(TraceItem) ->
            {true, TraceItem};
           (TraceItem) ->
            Formatted = format_trace_element(TraceItem),
            case hb_util:is_hb_module(Formatted, Prefixes) of
                true -> {true, Formatted};
                false -> false
            end
        end,
        Trace
    ).
```

### trace_short

Format a trace to a short string.

```erlang
trace_short() -> trace_short(get_trace(erlang)).
```

### trace_short

Format a trace to a short string.

```erlang
trace_short(Type) when is_atom(Type) -> trace_short(get_trace(Type));
```

### trace_short

Format a trace to a short string.
Format a trace element in form `mod:line` or `mod:func` for Erlang

```erlang
trace_short(Trace) when is_list(Trace) ->
    lists:join(" / ", lists:reverse(trace_to_list(Trace))).
```

### format_trace_element

Format a trace to a short string.
Format a trace element in form `mod:line` or `mod:func` for Erlang

```erlang
format_trace_element(Bin) when is_binary(Bin) -> Bin;
```

### format_trace_element

Format a trace to a short string.
Format a trace element in form `mod:line` or `mod:func` for Erlang

```erlang
format_trace_element({Mod, Line}) ->
    lists:flatten(io_lib:format("~p:~p", [Mod, Line]));
```

### format_trace_element

Format a trace to a short string.
Format a trace element in form `mod:line` or `mod:func` for Erlang

```erlang
format_trace_element({Mod, _, _, [{file, _}, {line, Line}|_]}) ->
    lists:flatten(io_lib:format("~p:~p", [Mod, Line]));
```

### format_trace_element

Format a trace to a short string.
Format a trace element in form `mod:line` or `mod:func` for Erlang
Utility function to help macro `?trace/0` remove the first frame of the

```erlang
format_trace_element({Mod, Func, _ArityOrTerm, _Extras}) ->
    lists:flatten(io_lib:format("~p:~p", [Mod, Func])).
```

### trace_macro_helper

Format a trace to a short string.
Format a trace element in form `mod:line` or `mod:func` for Erlang
Utility function to help macro `?trace/0` remove the first frame of the

```erlang
trace_macro_helper(Fun, {_, {_, Stack}}, Mod, Func, Line) ->
    Fun(Stack, Mod, Func, Line).
```

### get_trace

Get the trace of the current execution. If the argument is `erlang`,

```erlang
get_trace(erlang) ->
    case catch error(debugging_print) of
        {_, {_, Stack}} -> normalize_trace(Stack);
        _ -> []
    end;
```

### get_trace

Get the trace of the current execution. If the argument is `erlang`,

```erlang
get_trace(ao) ->
    case get(ao_stack) of
        undefined -> [];
        Stack -> Stack
    end.
```

### normalize_trace

Remove all calls from this module from the top of a trace.

```erlang
normalize_trace([]) -> [];
```

### normalize_trace

Remove all calls from this module from the top of a trace.

```erlang
normalize_trace([{Mod, _, _, _}|Rest]) when Mod == ?MODULE ->
    normalize_trace(Rest);
```

### normalize_trace

Remove all calls from this module from the top of a trace.
Format a message for printing, optionally taking an indentation level

```erlang
normalize_trace(Trace) -> Trace.
```

### message

Remove all calls from this module from the top of a trace.
Format a message for printing, optionally taking an indentation level

```erlang
message(Item) -> message(Item, #{}).
```

### message

Remove all calls from this module from the top of a trace.
Format a message for printing, optionally taking an indentation level

```erlang
message(Item, Opts) -> message(Item, Opts, 0).
```

### message

Remove all calls from this module from the top of a trace.
Format a message for printing, optionally taking an indentation level

```erlang
message(Bin, Opts, Indent) when is_binary(Bin) ->
    indent(
        binary(Bin),
        Opts,
        Indent
    );
```

### message

Remove all calls from this module from the top of a trace.
Format a message for printing, optionally taking an indentation level

```erlang
message(List, Opts, Indent) when is_list(List) ->
    % Remove the leading newline from the formatted list, if it exists.
```

### message

```erlang
message(RawMap, Opts, Indent) when is_map(RawMap) ->
    % Should we filter out the priv key?
    FilterPriv = hb_opts:get(debug_show_priv, false, Opts),
    MainPriv = hb_maps:get(<<"priv">>, RawMap, #{}, Opts),
    % Add private keys to the output if they are not hidden. Opt takes 3 forms:
    % 1. `false' -- never show priv
    % 2. `if_present' -- show priv only if there are keys inside
    % 2. `always' -- always show priv
    FooterKeys =
        case {FilterPriv, MainPriv} of
            {false, _} -> [];
            {if_present, #{}} -> [];
            {_, Priv} -> [{<<"!Private!">>, Priv}]
        end,
    Map =
        case FilterPriv of
            false -> RawMap;
            _ -> hb_private:reset(RawMap)
        end,
    % Define helper functions for formatting elements of the map.
```

### message

```erlang
message(Item, Opts, Indent) ->
    % Whatever we have is not a message map.
```

### short_id

Return a short ID for the different types of IDs used in AO-Core.

```erlang
short_id(Bin) when is_binary(Bin) andalso byte_size(Bin) == 32 ->
    short_id(hb_util:human_id(Bin));
```

### short_id

Return a short ID for the different types of IDs used in AO-Core.

```erlang
short_id(Bin) when is_binary(Bin) andalso byte_size(Bin) == 43 ->
    << FirstTag:5/binary, _:33/binary, LastTag:5/binary >> = Bin,
    << FirstTag/binary, "..", LastTag/binary >>;
```

### short_id

Return a short ID for the different types of IDs used in AO-Core.

```erlang
short_id(Bin) when byte_size(Bin) > 43 andalso byte_size(Bin) < 100 ->
    case binary:split(Bin, <<"/">>, [trim_all, global]) of
        [First, Second] when byte_size(Second) == 43 ->
            FirstEnc = short_id(First),
            SecondEnc = short_id(Second),
            << FirstEnc/binary, "/", SecondEnc/binary >>;
        [First, Key] ->
            FirstEnc = short_id(First),
            << FirstEnc/binary, "/", Key/binary >>;
        _ ->
            Bin
    end;
```

### short_id

Return a short ID for the different types of IDs used in AO-Core.

```erlang
short_id(<< "/", SingleElemHashpath/binary >>) ->
    Enc = short_id(SingleElemHashpath),
    if is_binary(Enc) -> << "/", Enc/binary >>;
    true -> undefined
    end;
```

### short_id

Return a short ID for the different types of IDs used in AO-Core.

```erlang
short_id(Key) when byte_size(Key) < 43 -> Key;
```

### short_id

Return a short ID for the different types of IDs used in AO-Core.
Determine whether a binary is human-readable.

```erlang
short_id(_) -> undefined.
```

### is_human_binary

Return a short ID for the different types of IDs used in AO-Core.
Determine whether a binary is human-readable.

```erlang
is_human_binary(Bin) when is_binary(Bin) ->
    case unicode:characters_to_binary(Bin) of
        {error, _, _} -> false;
        _ -> true
```

---

*Generated from [hb_format.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_format.erl)*
