%%% @doc Formatting and debugging utilities for HyperBEAM.
%%%
%%% This module provides text formatting capabilities for debugging output,
%%% message pretty-printing, stack trace formatting, and human-readable
%%% representations of binary data and cryptographic identifiers.
%%% 
%%% The functions in this module are primarily used for development and
%%% debugging purposes, supporting the logging and diagnostic infrastructure
%%% throughout the HyperBEAM system.
-module(hb_format).
%%% Public API.
-export([term/1, term/2, term/3]).
-export([format_debug/5]).
-export([print/1, print/3, print/4, print/5, eunit_print/2]).
-export([message/1, message/2, message/3]).
-export([binary/2, error/2, trace/1, trace_short/0, trace_short/1]).
-export([indent/2, indent/3, indent/4, indent_lines/2, maybe_multiline/3]).
-export([remove_leading_noise/1, remove_trailing_noise/1, remove_noise/1]).
-export([truncate/2]).
%%% Public Utility Functions.
-export([escape_format/1, short_id/1, trace_to_list/1]).
-export([get_trace/1, print_trace/4, trace_macro_helper/5, print_trace_short/4]).
-export([process_from_trace/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% Characters that are considered noise and should be removed from strings
%%% with the `remove_noise_[leading|trailing]' functions.
-define(NOISE_CHARS, " \t\n,").

%% @doc Print a message to the standard error stream, prefixed by the amount
%% of time that has elapsed since the last call to this function.
print(X) ->
    print(X, <<>>, #{}).
print(X, Info, Opts) ->
    io:format(standard_error, "~s~n", [render_debug(X, Info, Opts)]),
    X.
print(X, Mod, Func, LineNum) ->
    print(X, debug_trace(Mod, Func, LineNum, #{}), #{}).
print(X, Mod, Func, LineNum, Opts) ->
    io:format(standard_error, "~s~n", [format_debug(X, Mod, Func, LineNum, Opts)]),
    X.

%% @doc Format a debug message without writing it, preserving the standard layout.
format_debug(X, Mod, Func, LineNum, Opts) ->
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
                    debug_trace(Mod, Func, LineNum, Opts)
                ]
            )
        ),
    render_debug(X, Info, Opts).

%% @doc Render a debug message using the standard HyperBEAM layout.
render_debug(X, Info, Opts) ->
    hb_util:bin(
        io_lib:format(
            "=== HB DEBUG ===~s==>~n~s",
            [Info, term(X, Opts, 0)]
        )
    ).

%% @doc Retreive the server ID of the calling process, if known.
server_id() ->
    server_id(#{ server_id => undefined }).
server_id(Opts) ->
    case hb_opts:get(server_id, undefined, Opts) of
        undefined -> get(server_id);
        ServerID -> ServerID
    end.

%% @doc Generate the appropriate level of trace for a given call.
debug_trace(Mod, Func, Line, Opts) ->
    case hb_opts:get(debug_print_trace, false, #{}) of
        short ->
            Trace =
                case hb_opts:get(debug_trace_type, erlang, Opts) of
                    erlang -> get_trace(erlang);
                    ao ->
                        % If we are printing AO-Core traces, we add the module
                        % and line number to the end to show exactly where in
                        % the handler-flow the event arose.
                        [
                            hb_util:bin(trace_element({Mod, Line}))
                        |
                            get_trace(ao)
                        ]
                end,
            trace_short(Trace);
        false ->
            io_lib:format("~p:~w ~p", [Mod, Line, Func])
    end.

%% @doc Convert a term to a string for debugging print purposes.
term(X) -> term(X, #{}).
term(X, Opts) -> term(X, Opts, 0).
term(X, Opts, Indent) ->
    try do_term(X, Opts, Indent)
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
                            trace(
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

do_term(
    { { {rsa, _PublicExpnt1}, _Priv1, _Priv2 },
      { {rsa, _PublicExpnt2}, Pub }
    },
    Opts, Indent
) ->
    address(Pub, Opts, Indent);
do_term(
    { AtomValue,
      {
        { {rsa, _PublicExpnt1}, _Priv1, _Priv2 },
        { {rsa, _PublicExpnt2}, Pub }
      }
    },
    Opts, Indent
) ->
    AddressString = address(Pub, Opts, Indent),
    indent("~p: ~s", [AtomValue, AddressString], Opts, Indent);
do_term({explicit, X}, Opts, Indent) ->
    indent("[Explicit:] ~p", [X], Opts, Indent);
do_term({string, X}, Opts, Indent) ->
    indent("~s", [X], Opts, Indent);
do_term({trace, Trace}, Opts, Indent) ->
    indent("~n~s", [trace(Trace)], Opts, Indent);
do_term({as, undefined, Msg}, Opts, Indent) ->
    "\n" ++ indent("Subresolve => ", [], Opts, Indent) ++
        maybe_multiline(Msg, Opts, Indent + 1);
do_term({as, DevID, Msg}, Opts, Indent) ->
    "\n" ++ indent("Subresolve as ~s => ", [DevID], Opts, Indent) ++
        maybe_multiline(Msg, Opts, Indent + 1);
do_term({X, Y}, Opts, Indent) when is_atom(X) and is_atom(Y) ->
    indent("~p: ~p", [X, Y], Opts, Indent);
do_term({X, Y}, Opts, Indent) when is_record(Y, tx) ->
    indent("~p: [TX item]~n~s",
        [X, ar_format:format(Y, Indent + 1, Opts)],
        Opts,
        Indent
    );
do_term({X, Y}, Opts, Indent) when is_map(Y); is_list(Y) ->
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
do_term({X, Y}, Opts, Indent) ->
    indent(
        "~s: ~s",
        [
            remove_leading_noise(term(X, Opts, Indent)),
            remove_leading_noise(term(Y, Opts, Indent))
        ],
        Opts,
        Indent
    );
do_term(TX, Opts, Indent) when is_record(TX, tx) ->
    indent("[TX item]~n~s",
        [ar_format:format(TX, Indent, Opts)],
        Opts,
        Indent
    );
do_term(MaybePrivMap, Opts, Indent) when is_map(MaybePrivMap) ->
    Map = hb_private:reset(MaybePrivMap),
    case maybe_short(Map, Opts, Indent) of
        {ok, SimpleFmt} -> SimpleFmt;
        error ->
            "\n" ++ lists:flatten(message(Map, Opts, Indent))
    end;
do_term(Tuple, Opts, Indent) when is_tuple(Tuple) ->
    tuple(Tuple, Opts, Indent);
do_term(X, Opts, Indent) when is_binary(X) ->
    indent("~s", [binary(X, Opts)], Opts, Indent);
do_term(Str = [X | _], Opts, Indent) when is_integer(X) andalso X >= 32 andalso X < 127 ->
    indent("~s", [Str], Opts, Indent);
do_term(MsgList, Opts, Indent) when is_list(MsgList) ->
    list(MsgList, Opts, Indent);
do_term(X, Opts, Indent) ->
    indent("~80p", [X], Opts, Indent).

%% @doc If the user attempts to print a wallet, format it as an address.
address(Wallet, Opts, Indent) ->
    indent("Wallet [Addr: ~s]",
        [short_id(hb_util:human_id(ar_wallet:to_address(Wallet)))], 
        Opts, 
        Indent
    ).

%% @doc Helper function to format tuples with arity greater than 2.
tuple(Tuple, Opts, Indent) ->
    to_lines(lists:map(
        fun(Elem) ->
            term(Elem, Opts, Indent)
        end,
        tuple_to_list(Tuple)
    )).

%% @doc Format a list. Comes in three forms: all on one line, individual items
%% on their own line, or each item a multi-line string.
list(MsgList, Opts, Indent) ->
    case maybe_short(MsgList, Opts, Indent) of
        {ok, SimpleFmt} -> SimpleFmt;
        error ->
            {ToPrint, Footer} =
                case max_keys(Opts) of
                    Max when length(MsgList) > Max ->
                        {
                            lists:sublist(MsgList, Max),
                            hb_util:bin(
                                io_lib:format(
                                    "[+ ~p additional list elements]",
                                    [length(MsgList) - Max]
                                )
                            )
                        };
                    _ -> {MsgList, <<>>}
                end,
            "\n" ++
                indent("List [~w] {", [length(MsgList)], Opts, Indent) ++
                list_lines(ToPrint, Footer, Opts, Indent)
    end.

%% @doc Format a list as a multi-line string.
list_lines(MsgList, Footer, Opts, Indent) ->
    Numbered = hb_util:number(MsgList),
    Lines =
        lists:map(
            fun({N, Msg}) ->
                list_item(N, Msg, Opts, Indent)
            end,
            Numbered
        ),
    AnyLong =
        lists:any(
            fun({Mode, _}) -> Mode == multiline end,
            Lines
        ),
    IndentedFooterList =
        if Footer == <<>> -> "";
        true -> hb_util:list(indent(Footer, Indent + 1)) ++ "\n"
        end,
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
                ) ++ "\n" ++
                IndentedFooterList ++
                indent("}", [], Opts, Indent);
        true ->
            "\n" ++
            lists:flatten(lists:map(
                fun({N, Msg}) ->
                    {_, Line} = list_item(multiline, N, Msg, Opts, Indent),
                    Line
                end,
                Numbered
            )) ++
            IndentedFooterList ++
            indent("}", [], Opts, Indent)
    end.

%% @doc Format a single element of a list.
list_item(N, Msg, Opts, Indent) ->
    case list_item(short, N, Msg, Opts, Indent) of
        {short, String} -> {short, String};
        error -> list_item(multiline, N, Msg, Opts, Indent)
    end.
list_item(short, N, Msg, Opts, Indent) ->
    case maybe_short(Msg, Opts, Indent) of
        {ok, SimpleFmt} ->
            {short, indent("~s => ~s~n", [N, SimpleFmt], Opts, Indent + 1)};
        error -> error
    end;
list_item(multiline, N, Msg, Opts, Indent) ->
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

%% @doc Join a list of strings and remove trailing noise.
to_lines(Elems) ->
    remove_trailing_noise(do_to_lines(Elems)).
do_to_lines([]) -> [];
do_to_lines(In =[RawElem | Rest]) ->
    Elem = lists:flatten(RawElem),
    case lists:member($\n, Elem) of
        true -> lists:flatten(lists:join("\n", In));
        false -> Elem ++ ", " ++ do_to_lines(Rest)
    end.

truncate(Bin, MaxSize) when is_binary(Bin) ->
    BinLen = byte_size(Bin),
    BinEnd = case BinLen > MaxSize of 
        true -> <<"...">>;
        false -> <<>>
    end,
    TruncatedBin = binary:part(Bin, 0, min(BinLen, MaxSize)),
    <<TruncatedBin/binary, BinEnd/binary>>.

%% @doc Remove any leading or trailing noise from a string.
remove_noise(Str) ->
    remove_leading_noise(remove_trailing_noise(Str)).

%% @doc Remove any leading whitespace from a string.
remove_leading_noise(Str) ->
    remove_leading_noise(Str, ?NOISE_CHARS).
remove_leading_noise(Bin, Noise) when is_binary(Bin) ->
    hb_util:bin(remove_leading_noise(hb_util:list(Bin), Noise));
remove_leading_noise([], _) -> [];
remove_leading_noise([Char|Str], Noise) ->
    case lists:member(Char, Noise) of
        true ->
            remove_leading_noise(Str, Noise);
        false -> [Char|Str]
    end.

%% @doc Remove trailing noise characters from a string. By default, this is
%% whitespace, newlines, and `,'.
remove_trailing_noise(Str) ->
    removing_trailing_noise(Str, ?NOISE_CHARS).
removing_trailing_noise(Bin, Noise) when is_binary(Bin) ->
    removing_trailing_noise(binary:bin_to_list(Bin), Noise);
removing_trailing_noise(BinList, Noise) when is_list(BinList) ->
    case lists:member(lists:last(BinList), Noise) of
        true ->
            removing_trailing_noise(lists:droplast(BinList), Noise);
        false -> BinList
    end.

%% @doc Format a string with an indentation level.
indent(Str, Indent) -> indent(Str, #{}, Indent).
indent(Str, Opts, Indent) -> indent(Str, [], Opts, Indent).
indent(FmtStr, Terms, Opts, Ind) ->
    IndentSpaces = hb_opts:get(debug_print_indent, Opts),
    EscapedFmt = escape_format(FmtStr),
    lists:droplast(
        lists:flatten(
            io_lib:format(
                [$\s || _ <- lists:seq(1, Ind * IndentSpaces)] ++
                    lists:flatten(hb_util:list(EscapedFmt)) ++ "\n",
                Terms
            )
        )
    ).

%% @doc Escape a string for use as an io_lib:format specifier.
escape_format(Str) when is_list(Str) ->
    re:replace(
        Str,
        "~([a-z\\-_]+@[0-9]+\\.[0-9]+)", "~~\\1",
        [global, {return, list}]
    );
escape_format(Else) -> Else.

%% @doc Format an error message as a string.
error(ErrorMsg, Opts) ->
    Type = hb_maps:get(<<"type">>, ErrorMsg, <<"[No type]">>, Opts),
    Details = hb_maps:get(<<"details">>, ErrorMsg, <<"[No details]">>, Opts),
    Stacktrace = hb_maps:get(<<"stacktrace">>, ErrorMsg, <<"[No trace]">>, Opts),
    hb_util:bin(
        [
            <<"Termination type: '">>, Type,
            <<"'\n\nStacktrace:\n\n">>, Stacktrace,
            <<"\n\nError details:\n\n">>, Details
        ]
    ).

%% @doc Take a series of strings or a combined string and format as a
%% single string with newlines and indentation to the given level. Note: This
%% function returns a binary.
indent_lines(Strings, Indent) when is_binary(Strings) ->
    indent_lines(binary:split(Strings, <<"\n">>, [global]), Indent);
indent_lines(Strings, Indent) when is_list(Strings) ->
    hb_util:bin(lists:join(
        "\n",
        [
            indent(hb_util:list(String), #{}, Indent)
        ||
            String <- Strings
        ]
    )).

%% @doc Format a binary as a short string suitable for printing.
binary(Bin, Opts) ->
    case short_id(Bin) of
        undefined ->
            MaxBinPrint = hb_opts:get(debug_print_binary_max, 60, Opts),
            Truncated =
                binary:part(
                    Bin,
                    0,
                    min(
                        case binary:match(Bin, <<"\n">>) of
                            {NewlinePos, _} -> NewlinePos;
                            nomatch -> MaxBinPrint
                        end,
                        MaxBinPrint
                    )
                ),
            PrintSegment =
                case hb_util:is_printable_string(Truncated) of
                    true -> Truncated;
                    false -> hb_util:encode(Truncated)
                end,
            lists:flatten(
                [
                    "\"",
                    [PrintSegment],
                    case Truncated == Bin of
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

%% @doc Format a map as either a single line or a multi-line string depending
%% on the value of the `debug_print_map_line_threshold' runtime option.
maybe_multiline(X, Opts, Indent) ->
    case maybe_short(X, Opts, Indent) of
        {ok, SimpleFmt} -> SimpleFmt;
        error ->
            "\n" ++ lists:flatten(message(X, Opts, Indent))
    end.

%% @doc Attempt to generate a short formatting of a message, using the given
%% node options.
maybe_short(X, Opts, _Indent) ->
    MaxLen = hb_opts:get(debug_print_map_line_threshold, 100, Opts),
    SimpleFmt =
        case is_binary(X) of
            true -> binary(X, Opts);
            false -> io_lib:format("~p", [X])
        end,
    case is_multiline(SimpleFmt) orelse (lists:flatlength(SimpleFmt) > MaxLen) of
        true -> error;
        false -> {ok, SimpleFmt}
    end.

%% @doc Is the given string a multi-line string?
is_multiline(Str) ->
    lists:member($\n, Str).

-ifndef(QUIET).
%% @doc Format and print an indented string to standard error.
eunit_print(FmtStr, FmtArgs) ->
    io:format(
        standard_error,
        "~n~s ",
        [indent(FmtStr ++ "...", FmtArgs, #{}, 4)]
    ).
-else.
eunit_print(_FmtStr, _FmtArgs) -> skipped_print.
-endif.

%% @doc Print the trace of the current stack, up to the first non-hyperbeam
%% module. Prints each stack frame on a new line, until it finds a frame that
%% does not start with a prefix in the `stack_print_prefixes' hb_opts.
%% Optionally, you may call this function with a custom label and caller info,
%% which will be used instead of the default.
print_trace(Stack, CallMod, CallFunc, CallLine) ->
    print_trace(Stack, "HB TRACE",
        lists:flatten(io_lib:format("[~s:~w ~p]",
            [CallMod, CallLine, CallFunc])
    )).

print_trace(Stack, Label, CallerInfo) ->
    io:format(standard_error, "=== ~s ===~s==>~n~s",
        [
            Label, CallerInfo,
            lists:flatten(trace(Stack))
        ]).

%% @doc Format a stack trace as a list of strings, one for each stack frame.
%% Each stack frame is formatted if it matches the `stack_print_prefixes'
%% option. At the first frame that does not match a prefix in the
%% `stack_print_prefixes' option, the rest of the stack is not formatted.
trace(Stack) ->
    trace(Stack, hb_opts:get(stack_print_prefixes, [], #{})).
trace([], _) -> [];
trace([Item|Rest], Prefixes) ->
    case element(1, Item) of
        Atom when is_atom(Atom) ->
            case true of %is_hb_module(Atom, Prefixes) of
                true ->
                    [
                        trace(Item, Prefixes) |
                        trace(Rest, Prefixes)
                    ];
                false -> []
            end;
        _ -> []
    end;
trace({Func, ArityOrTerm, Extras}, Prefixes) ->
    trace({no_module, Func, ArityOrTerm, Extras}, Prefixes);
trace({Mod, Func, ArityOrTerm, Extras}, _Prefixes) ->
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

%% @doc Print a trace to the standard error stream.
print_trace_short(Trace, Mod, Func, Line) ->
    io:format(standard_error, "=== [ HB SHORT TRACE ~p:~w ~p ] ==> ~s~n",
        [
            Mod, Line, Func,
            trace_short(Trace)
        ]
    ).

%% @doc Return a list of calling modules and lines from a trace, removing all
%% frames that do not match the `stack_print_prefixes' option.
trace_to_list(Trace) ->
    Prefixes = hb_opts:get(stack_print_prefixes, [], #{}),
    lists:filtermap(
        fun(TraceItem) when is_binary(TraceItem) ->
            {true, TraceItem};
           (TraceItem) ->
            Formatted = trace_element(TraceItem),
            case hb_util:is_hb_module(Formatted, Prefixes) of
                true -> {true, Formatted};
                false -> false
            end
        end,
        Trace
    ).

%% @doc Format a trace to a short string.
trace_short() -> trace_short(get_trace(erlang)).
trace_short(Type) when is_atom(Type) -> trace_short(get_trace(Type));
trace_short(Trace) when is_list(Trace) ->
    lists:join(" / ", lists:reverse(trace_to_list(Trace))).

process_from_trace([]) ->
    <<"unknown">>;
process_from_trace(Trace) ->
    % Prefer the outermost non-glue MFA (walk from trace bottom /
    % process entry). That matches a caller above pmap/proc_lib glue and
    % stays stable when the innermost slot is generic (e.g. timer:sleep) while
    % a user job remains deeper in the chain.
    case process_from_trace(lists:reverse(Trace), false) of
        none ->
            <<"unknown">>;
        Found ->
            Found
    end.

%% @doc First non-glue TraceElement scanning `Trace` from its head.
process_from_trace([], _) ->
    none;
process_from_trace([TraceElement | Rest], Spawner) ->
    case {trace_element_is_glue(TraceElement), Spawner} of
        {true, _} ->
            % Flag whether or not this is an anonymous process spawned
            % by hb_pmap.
            NextSpawner = case TraceElement of
                {hb_pmap, _, _, _} ->
                    hb_pmap;
                _ ->
                    Spawner
            end,
            process_from_trace(Rest, NextSpawner);
        {false, false} ->
            hb_util:bin(trace_element(TraceElement));
        {false, Spawner} ->
            <<
                (hb_util:bin(Spawner))/binary,
                "->",
                (hb_util:bin(trace_element(TraceElement)))/binary
            >>
        end.

trace_element_is_glue({proc_lib, init_p_do_apply, _, _}) ->
    true;
trace_element_is_glue({hb_pmap, F, _, _}) ->
    is_erlang_generated_fun_name(F);
trace_element_is_glue(_) ->
    false.

%% @doc True for compiler-generated fun atoms like `'-foo/1-fun-0-'`.
is_erlang_generated_fun_name(Func) when is_atom(Func) ->
    case atom_to_binary(Func, utf8) of
        <<"-", Rest/binary>> ->
            binary:match(Rest, <<"-fun-">>) =/= nomatch;
        _ ->
            false
    end;
is_erlang_generated_fun_name(_) ->
    false.

%% @doc Format a trace element in form `mod:line' or `mod:func' for Erlang
%% traces, or their raw form for others.
trace_element(Bin) when is_binary(Bin) -> Bin;
trace_element({Mod, Line}) ->
    lists:flatten(io_lib:format("~p:~p", [Mod, Line]));
trace_element({Mod, _, _, [{file, _}, {line, Line}|_]}) ->
    lists:flatten(io_lib:format("~p:~p", [Mod, Line]));
trace_element({Mod, Func, _ArityOrTerm, _Extras}) ->
    lists:flatten(io_lib:format("~p:~p", [Mod, Func])).

%% @doc Utility function to help macro `?trace/0' remove the first frame of the
%% stack trace.
trace_macro_helper(Fun, {_, {_, Stack}}, Mod, Func, Line) ->
    Fun(Stack, Mod, Func, Line).

%% @doc Get the trace of the current execution. If the argument is `erlang',
%% we return the Erlang stack trace. If the argument is `ao', we return the
%% AO-Core execution stack.
get_trace(erlang) ->
    case catch error(debugging_print) of
        {_, {_, Stack}} -> normalize_trace(Stack);
        _ -> []
    end;
get_trace(ao) ->
    case get(ao_stack) of
        undefined -> [];
        Stack -> Stack
    end.

%% @doc Remove all calls from this module from the top of a trace.
normalize_trace([]) -> [];
normalize_trace([{Mod, _, _, _}|Rest]) when Mod == ?MODULE ->
    normalize_trace(Rest);
normalize_trace(Trace) -> Trace.

%% @doc Format a message for printing, optionally taking an indentation level
%% to start from.
message(Item) -> message(Item, #{}).
message(Item, Opts) -> message(Item, Opts, 0).
message(Bin, Opts, Indent) when is_binary(Bin) ->
    indent(
        binary(Bin, Opts),
        Opts,
        Indent
    );
message(List, Opts, Indent) when is_list(List) ->
    % Remove the leading newline from the formatted list, if it exists.
    case term(List, Opts, Indent) of
        [$\n | String] -> String;
        String -> String
    end;
message(RawMsg, Opts, Indent) when is_map(RawMsg) ->
    % Load relevant options.
    FilterPriv = hb_opts:get(debug_show_priv, false, Opts),
    PrintCommDevice = hb_opts:get(debug_print_comm_device, true, Opts),
    PrintCommType = hb_opts:get(debug_print_comm_type, true, Opts),
    PrintCommitted = hb_opts:get(debug_print_committed, true, Opts),
    MustVerifyAllIDs = hb_opts:get(debug_print_verify, true, Opts),
    GenerateIDs = hb_opts:get(debug_print_gen_id, false, Opts),
    MainPriv = hb_maps:get(<<"priv">>, RawMsg, #{}, Opts),
    % Add private keys to the output if they are not hidden. Opt takes 3 forms:
    % 1. `false' -- never show priv
    % 2. `if_present' -- show priv only if there are keys inside
    % 2. `always' -- always show priv
    PrivKeys =
        case {FilterPriv, MainPriv} of
            {false, _} -> [];
            {if_present, #{}} -> [];
            {_, Priv} -> [{<<"!Private!">>, Priv}]
        end,
    Msg =
        case FilterPriv of
            false -> RawMsg;
            _ -> hb_private:reset(RawMsg)
        end,
    % Define helper functions for formatting elements of the map.
    ValOrUndef =
        fun(<<"hashpath">>) ->
            case Msg of
                #{ <<"priv">> := #{ <<"hashpath">> := HashPath } } ->
                    short_id(HashPath);
                _ ->
                    undefined
            end;
        (Key) ->
            case dev_message:get(Key, Msg, Opts) of
                {ok, Val} ->
                    case short_id(Val) of
                        undefined -> Val;
                        ShortID -> ShortID
                    end;
                {error, _} -> undefined
            end
        end,
    FilterUndef =
        fun(List) ->
            lists:filter(
                fun({_, undefined}) -> false;
                   (undefined) -> false;
                   (false) -> false;
                   (_) -> true
                end,
                List
            )
        end,
    % Note: We try to get the IDs _if_ they are *already* in the map. We do not
    % force calculation of the IDs here because that may cause significant
    % overhead unless the `debug_ids' option is set.
    KnownComms =
        hb_maps:without(
            [<<"commitments">>, <<"priv">>],
            hb_maps:get(<<"commitments">>, Msg, #{}, Opts),
            Opts
        ),
    MsgWithNormComms = #{ <<"commitments">> := Comms } =
        case map_size(KnownComms) == 0 andalso GenerateIDs of
            false -> Msg#{ <<"commitments">> => KnownComms };
            true ->
                case dev_message:commit(Msg, #{ <<"type">> => <<"unsigned">> }, Opts) of
                    {ok, XMsg} -> XMsg;
                    {error, _} -> Msg#{ <<"commitments">> => #{} }
                end
        end,
    {ok, CommittedKeys} =
        dev_message:committed(
            MsgWithNormComms,
            #{ <<"commitment-ids">> => <<"all">> },
            Opts
        ),
    CommIDs = hb_maps:keys(Comms, Opts),
    {_ValidIDs, InvalidIDs} =
        lists:partition(
            fun(_) when not MustVerifyAllIDs -> true;
               (ID) ->
                try
                    hb_message:verify(
                        MsgWithNormComms,
                        #{ <<"commitment-ids">> => ID },
                        Opts
                    )
                catch _:_ -> false
                end
            end,
            CommIDs
        ),
    % Prepare the metadata row for formatting.
    DevicePathMetadata =
        case {ValOrUndef(<<"device">>), ValOrUndef(<<"path">>)} of
            {undefined, undefined} -> [<<"Message ">>];
            {Device, undefined} ->
                DeviceValue =
                    format_key(
                        PrintCommDevice,
                        CommittedKeys,
                        <<"device">>,
                        <<"~", Device/binary>>,
                        Opts
                    ),
                [DeviceValue, <<" ">>];
            {undefined, Path} ->
                PathValue =
                    format_key(
                        PrintCommitted,
                        CommittedKeys,
                        <<"path">>,
                        Path,
                        Opts
                    ),
                [<<"Message < Path: ">>, PathValue, <<" > ">>];
            {Device, Path} ->
                DeviceValue =
                    format_key(
                        PrintCommitted,
                        CommittedKeys,
                        <<"device">>,
                        <<"~", Device/binary>>,
                        Opts
                    ),
                PathValue =
                    format_key(
                        PrintCommitted,
                        CommittedKeys,
                        <<"path">>,
                        Path,
                        Opts
                    ),
                [DeviceValue, <<"/">>, PathValue, <<" ">>]
        end,
    IDMetadata =
        format_ids(
            lists:map(
                fun({ID, Comm}) ->
                    hb_util:bin(io_lib:format(
                        "~s~s~s~s~s",
                        [
                            case lists:member(ID, InvalidIDs) of
                                true -> <<"!INVALID! ">>;
                                false -> <<>>
                            end,
                            short_id(ID),
                            if PrintCommDevice ->
                                [
                                    "~",
                                    hb_util:bin(
                                        hb_maps:get(
                                            <<"commitment-device">>,
                                            Comm,
                                            <<"!NO DEVICE!">>,
                                            Opts
                                        )
                                    )
                                ];
                               true -> <<>>
                            end,
                            case PrintCommType andalso hb_maps:find(<<"type">>, Comm, Opts) of
                                {ok, Type} -> <<"/", Type/binary>>;
                               _ -> <<>>
                            end,
                            case hb_maps:get(<<"committer">>, Comm, undefined, Opts) of
                                undefined -> <<>>;
                                Committer ->
                                    [<<" (Sig: ">>, short_id(Committer), <<")">>]
                            end
                        ]
                    ))
                end,
                hb_maps:to_list(Comms, Opts)
            ),
            Opts
        ),
    % Format the metadata row.
    Header =
        indent("~s[ ~s~s ] {",
            [
                hb_util:bin(FilterUndef(DevicePathMetadata)),
                case ValOrUndef(<<"hashpath">>) of
                    undefined -> <<>>;
                    HashPath -> [<<"#p: ">>, short_id(HashPath), <<" ">>]
                end,
                IDMetadata
            ],
            Opts,
            Indent
        ),
    % Put the path and device rows into the output at the _top_ of the map.
    PriorityKeys =
        [
            case hb_opts:get(debug_print_metadata, true, Opts) of
                true ->
                    {<<"commitments">>, ValOrUndef(<<"commitments">>)};
                false ->
                    {<<"commitments">>, undefined}
            end
        ],
    % Concatenate the path and device rows with the rest of the key values.
    UnsortedGeneralKVs =
        maps:to_list(
            maps:without(
                [ PriorityKey || {PriorityKey, _} <- PriorityKeys ] ++
                    [<<"device">>, <<"path">>, <<"method">>],
                Msg
            )
        ),
    % Truncate the keys to print if there are too many. The `truncate' option
    % may be an integer representing the maximum number of keys that should be
    % printed, or the atom `infinity' to print all keys.
    {TruncatedKeys, FooterKeys} =
        case max_keys(Opts) of
            Max when length(UnsortedGeneralKVs) > Max ->
                {
                    lists:sublist(UnsortedGeneralKVs, Max),
                    [
                        {
                            <<"...">>,
                            hb_util:bin(
                                io_lib:format(
                                    "[+ ~p additional keys]",
                                    [length(UnsortedGeneralKVs) - Max]
                                )
                            )
                        }
                    |
                        PrivKeys
                    ]
                };
            _ -> {UnsortedGeneralKVs, PrivKeys}
        end,
    FormattedKeys =
        lists:map(
            fun({Key, Val}) ->
                {format_key(PrintCommitted, CommittedKeys, Key, Opts), Val}
            end,
            TruncatedKeys
        ),
    KeyValsToPrint =
        FilterUndef(PriorityKeys) ++
        lists:sort(
            fun({K1, _}, {K2, _}) -> K1 < K2 end,
            FormattedKeys
        ) ++
        FooterKeys,
    % Format the remaining 'normal' keys and values.
    Res = lists:map(
        fun({KeyStr, Val}) ->
            indent(
                "~s => ~s~n",
                [
                    lists:flatten([KeyStr]),
                    case Val of
                        NextMap when is_map(NextMap) ->
                            maybe_multiline(NextMap, Opts, Indent + 2);
                        Next when is_list(Next); is_record(Next, tx) ->
                            remove_leading_noise(term(Next, Opts, Indent + 2));
                        _ when (byte_size(Val) == 32) ->
                            Short = short_id(Val),
                            io_lib:format("~s [*]", [Short]);
                        _ when byte_size(Val) == 43 ->
                            short_id(Val);
                        _ when byte_size(Val) == 87 ->
                            io_lib:format("~s [#p]", [short_id(Val)]);
                        Bin when is_binary(Bin) ->
                            binary(Bin, Opts);
                        Link when ?IS_LINK(Link) ->
                            remove_leading_noise(
                                hb_util:bin(
                                    hb_link:format(Link, Opts, Indent + 2)
                                )
                            );
                        Other ->
                            io_lib:format("~p", [Other])
                    end
                ],
                Opts,
                Indent + 1
            )
        end,
        KeyValsToPrint
    ),
    case Res of
        [] -> lists:flatten(Header ++ " [Empty] }");
        _ ->
            lists:flatten(
                Header ++ ["\n"] ++ Res ++ indent("}", Indent)
            )
    end;
message(Item, Opts, Indent) ->
    % Whatever we have is not a message map.
    indent("~p", [Item], Opts, Indent).

%%% Utility functions.

%% @doc Format a key for printing, optionally adding the appropriate `committed'
%% key specifier character. This function may be called with just a key, or a
%% value to print in place of the key, for use in producing `* ~dev-name@1.0`-style
%% results.
format_key(PrintCommitted, Committed, Key, Opts) ->
    format_key(PrintCommitted, Committed, Key, undefined, Opts).
format_key(false, _, Key, undefined, Opts) -> hb_ao:normalize_key(Key, Opts);
format_key(false, _, _, ToPrint, _) -> ToPrint;
format_key(true, Committed, Key, ToPrint, Opts) ->
    case lists:member(NormKey = hb_ao:normalize_key(Key, Opts), Committed) of
        true when ToPrint == undefined -> <<"* ", NormKey/binary>>;
        true -> <<"* ", ToPrint/binary>>;
        false -> format_key(false, Committed, Key, ToPrint, Opts)
    end.

%% @doc Return a formatted list of short IDs, given a raw list of IDs.
format_ids([], _Opts) -> undefined;
format_ids(IDs, _Opts) ->
    string:join(
        lists:map(
            fun(XID) -> hb_util:list(short_id(XID)) end,
            IDs
        ),
        ", "
    ).

%% @doc Return a short ID for the different types of IDs used in AO-Core.
short_id(<<"http://", _/binary>> = Bin) ->
    Bin;
short_id(<<"https://", _/binary>> = Bin) ->
    Bin;
short_id(Bin) when is_binary(Bin) andalso byte_size(Bin) == 32 ->
    short_id(hb_util:human_id(Bin));
short_id(Bin) when is_binary(Bin) andalso byte_size(Bin) == 43 ->
    << FirstTag:5/binary, _:33/binary, LastTag:5/binary >> = Bin,
    << FirstTag/binary, "..", LastTag/binary >>;
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
short_id(<< "/", SingleElemHashpath/binary >>) ->
    Enc = short_id(SingleElemHashpath),
    if is_binary(Enc) -> << "/", Enc/binary >>;
    true -> undefined
    end;
short_id(Key) when byte_size(Key) < 43 -> Key;
short_id(_) -> undefined.

%% Determine the maximum number of keys to print for messages, given a node
%% `Opts`.
max_keys(Opts) ->
    case hb_opts:get(debug_print_truncate, 30, Opts) of
        Max when is_integer(Max) -> Max;
        infinity -> infinity;
        Term -> hb_util:int(Term)
    end.

%%% Tests

truncate_no_truncation_test() ->
    ?assertEqual(<<"hello">>, truncate(<<"hello">>, 10)).

truncate_exact_size_test() ->
    ?assertEqual(<<"hello">>, truncate(<<"hello">>, 5)).

truncate_with_truncation_test() ->
    ?assertEqual(<<"he...">>, truncate(<<"hello">>, 2)).

truncate_empty_test() ->
    ?assertEqual(<<>>, truncate(<<>>, 5)).