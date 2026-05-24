%%% @doc Process-local AO-Core event recorder and viewer.
%%%
%%% `~events@1.0/log=start' enables event recording in the current Erlang
%%% process and returns the base message so the rest of the path can continue.
%%% `~events@1.0/new' is intended to be installed as an `on/event' hook
%%% handler. It appends the hook request to the process-local recording if
%%% recording is active. A later `log~events@1.0' call returns the captured
%%% events as an AO-Core message, JSON, text, or embedded HTML.
-module(dev_events).
-export([info/1, new/3, log/3, record/3, index/3]).
-include_lib("hb/include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEVICE, <<"events@1.0">>).
-define(NO_CACHE, [<<"no-cache">>, <<"no-store">>]).
-define(RECORDING_KEY, {?DEVICE, recording}).
-define(RECORDING_EVENTS_KEY, {?DEVICE, events}).
-define(RECORDING_SEQUENCE_KEY, {?DEVICE, sequence}).
-define(RECORDING_START_KEY, {?DEVICE, start_time}).
-define(RECORDING_LAST_KEY, {?DEVICE, last_time}).
-define(RECORDING_KEYS, [
    ?RECORDING_KEY,
    ?RECORDING_EVENTS_KEY,
    ?RECORDING_SEQUENCE_KEY,
    ?RECORDING_START_KEY,
    ?RECORDING_LAST_KEY
]).
-define(RECORDING_OPT_KEYS, [
    <<"stack">>,
    <<"trace">>
]).

%% @doc Export the public event recording and viewer endpoints.
info(_) ->
    #{
        exports => [
            <<"new">>,
            <<"log">>,
            <<"record">>,
            <<"index">>
        ]
    }.

%% @doc Append one event hook request if recording is active.
new(_Base, Req, Opts) ->
    maybe_record(Req, Opts),
    {ok, Req}.

%% @doc Start, clear, stop, or read process-local events.
log(Base, Req, Opts) ->
    case action(Req, Opts) of
        <<"start">> ->
            start_recording(recording_opts(Req, Opts)),
            {ok, pass_through(Base, Opts)};
        <<"clear">> ->
            clear_recording(),
            {ok, report()};
        <<"stop">> ->
            Report = report(),
            clear_recording(),
            response(Report#{ <<"recording">> => false }, Req, Opts);
        _ ->
            response(report(), Req, Opts)
    end.

%% @doc Resolve a target request with process-local event recording enabled.
%% Returns the recorded event report in the requested render format.
record(_Base, Req, Opts) ->
    case record_target(Req, Opts) of
        {ok, Target} ->
            RecOpts = recording_opts(Req, Opts),
            start_recording(RecOpts),
            Res = resolve_recorded(Target, with_event_hook(Opts)),
            Report0 = report(),
            clear_recording(),
            Report1 =
                Report0#{
                    <<"result">> => Res,
                    <<"target">> => Target
                },
            response(Report1, Req, Opts, <<"html">>);
        {error, Reason} ->
            response(
                #{
                    <<"recording">> => false,
                    <<"count">> => 0,
                    <<"events">> => [],
                    <<"error">> => Reason
                },
                Req,
                Opts,
                <<"html">>
            )
    end.

%% @doc Return the browser UI with the current process log embedded.
index(_Base, Req, Opts) ->
    html_response(report(), Opts).

action(Req, Opts) ->
    hb_ao:normalize_key(hb_maps:get(<<"log">>, Req, <<"read">>, Opts)).

response(Report, Req, Opts) ->
    response(Report, Req, Opts, <<"raw">>).
response(Report, Req, Opts, Default) ->
    case hb_ao:normalize_key(hb_maps:get(<<"format">>, Req, Default, Opts)) of
        <<"html">> -> html_response(Report, Opts);
        <<"json">> -> json_response(Report, Opts);
        <<"text">> -> text_response(Report, Opts);
        <<"raw">> -> raw_response(Report, Opts);
        _ -> raw_response(Report, Opts)
    end.

with_event_hook(Opts) ->
    On = maps:get(<<"on">>, Opts, #{}),
    Opts#{
        <<"on">> =>
            On#{
                <<"event">> =>
                    #{
                        <<"device">> => ?DEVICE,
                        <<"path">> => <<"new">>,
                        <<"hook/result">> => <<"ignore">>
                    }
            },
        <<"process-dictionary-transfer">> => ?RECORDING_KEYS
    }.

resolve_recorded(Target, Opts) ->
    try hb_ao:resolve(Target, Opts) of
        Res -> Res
    catch Class:Reason:Stack ->
        {
            error,
            #{
                <<"class">> => hb_util:bin(Class),
                <<"reason">> => Reason,
                <<"stack">> => Stack
            }
        }
    end.

record_target(Req, Opts) ->
    case record_target_value(Req, Opts) of
        not_found ->
            case hb_path:tl(Req, Opts) of
                undefined ->
                    {error, <<"No request supplied.">>};
                Tail ->
                    {ok,
                        record_target_from_tail(
                            hb_maps:get(<<"path">>, Tail, Tail, Opts),
                            Req,
                            Opts
                        )
                    }
            end;
        Target ->
            {ok, record_target_from_value(Target, Req, Opts)}
    end.

record_target_value(Req, Opts) ->
    hb_ao:get_first(
        [
            {Req, <<"request">>},
            {Req, <<"target">>},
            {Req, <<"record-path">>},
            {Req, <<"record">>}
        ],
        not_found,
        Opts
    ).

record_target_from_tail(Path, Req, Opts) ->
    (record_request_base(Req, Opts))#{
        <<"path">> => hb_path:to_binary(Path)
    }.

record_target_from_value(Target, _Req, _Opts) when is_map(Target) ->
    Target;
record_target_from_value(Target, Req, Opts) ->
    Path = hb_util:bin(Target),
    case external_url(Path) of
        true ->
            #{
                <<"path">> =>
                    <<
                        "/~relay@1.0/call?relay-method=",
                        (record_method(Req, Opts))/binary,
                        "&relay-path=",
                        (hb_escape:encode(Path))/binary
                    >>
            };
        false ->
            #{ <<"path">> => Path }
    end.

record_request_base(Req, Opts) ->
    hb_maps:without(
        [
            <<"format">>,
            <<"log">>,
            <<"request">>,
            <<"target">>,
            <<"record-path">>,
            <<"record">>
            | ?RECORDING_OPT_KEYS
        ],
        Req,
        Opts
    ).

record_method(Req, Opts) ->
    hb_maps:get(<<"target-method">>, Req, <<"GET">>, Opts).

external_url(<<"http://", _/binary>>) -> true;
external_url(<<"https://", _/binary>>) -> true;
external_url(_) -> false.

with_no_cache({ok, Msg}) ->
    {ok, Msg#{ <<"cache-control">> => ?NO_CACHE }};
with_no_cache(Other) ->
    Other.

html_response(Report, Opts) ->
    with_no_cache(
        case static(<<"index.html">>, Opts) of
            {ok, Msg = #{ <<"body">> := Body }} ->
                Body1 =
                    binary:replace(
                        Body,
                        <<"{{EVENT_LOG_JSON_BASE64}}">>,
                        base64:encode(json_report_body(Report, Opts)),
                        [global]
                    ),
                Body2 =
                    inline_asset(
                        Body1,
                        <<"styles.css">>,
                        <<"{{EVENTS_CSS}}">>,
                        fun escape_style/1,
                        Opts
                    ),
                {ok,
                    Msg#{
                        <<"body">> =>
                            inline_asset(
                                Body2,
                                <<"events.js">>,
                                <<"{{EVENTS_JS}}">>,
                                fun escape_script/1,
                                Opts
                            )
                    }
                };
            Other ->
                Other
        end
    ).

inline_asset(Body, Name, Placeholder, Escape, Opts) ->
    Asset =
        case static(Name, Opts) of
            {ok, #{ <<"body">> := AssetBody }} -> Escape(AssetBody);
            _ -> <<>>
        end,
    binary:replace(Body, Placeholder, Asset, [global]).

escape_style(Bin) ->
    binary:replace(Bin, <<"</style">>, <<"<\\/style">>, [global]).

escape_script(Bin) ->
    binary:replace(Bin, <<"</script">>, <<"<\\/script">>, [global]).

json_response(Report, Opts) ->
    with_no_cache(
        {ok,
            #{
                <<"content-type">> => <<"application/json">>,
                <<"body">> => json_report_body(Report, Opts)
            }
        }
    ).

text_response(Report, Opts) ->
    with_no_cache(
        {ok,
            #{
                <<"content-type">> => <<"text/plain">>,
                <<"body">> =>
                    hb_util:bin(
                        hb_format:message(
                            json_value(Report, Opts),
                            Opts#{ <<"linkify-mode">> => discard }
                        )
                    )
            }
        }
    ).

raw_response(Report, Opts) ->
    {ok, hb_maps:get(<<"events">>, Report, Report, Opts)}.

static(Name, _Opts) ->
    Filename =
        filename:join(
            hb_device_archive:implementation_dir(?MODULE),
            hb_util:list(Name)
        ),
    case file:read_file(Filename) of
        {ok, Body} ->
            {ok,
                #{
                    <<"body">> => Body,
                    <<"content-type">> => content_type(Name)
                }
            };
        {error, _} ->
            {error, not_found}
    end.

content_type(<<"index.html">>) -> <<"text/html">>;
content_type(<<"events.js">>) -> <<"text/javascript">>;
content_type(<<"styles.css">>) -> <<"text/css">>;
content_type(_) -> <<"application/octet-stream">>.

json_report_body(Report, Opts) ->
    hb_json:encode(json_value(Report, Opts)).

pass_through(Base, Opts) when is_map(Base) ->
    hb_maps:without([<<"device">>], Base, Opts);
pass_through(Base, _Opts) ->
    Base.

start_recording(Opts) ->
    Now = erlang:monotonic_time(microsecond),
    erlang:put(?RECORDING_KEY, normalize_recording_opts(Opts)),
    erlang:put(?RECORDING_EVENTS_KEY, []),
    erlang:put(?RECORDING_SEQUENCE_KEY, 0),
    erlang:put(?RECORDING_START_KEY, Now),
    erlang:put(?RECORDING_LAST_KEY, Now),
    ok.

clear_recording() ->
    lists:foreach(fun erlang:erase/1, ?RECORDING_KEYS),
    ok.

recording() ->
    erlang:get(?RECORDING_KEY) =/= undefined.

recorded() ->
    case erlang:get(?RECORDING_EVENTS_KEY) of
        undefined -> [];
        Events -> lists:reverse(Events)
    end.

report() ->
    Events = recorded(),
    #{
        <<"recording">> => recording(),
        <<"count">> => length(Events),
        <<"events">> => Events
    }.

maybe_record(HookReq, _Opts) ->
    case erlang:get(?RECORDING_KEY) of
        undefined ->
            ok;
        RecOpts ->
            Seq = next_recording_sequence(),
            Event = event_record(Seq, HookReq, RecOpts),
            erlang:put(
                ?RECORDING_EVENTS_KEY,
                [Event | recorded_reversed()]
            ),
            ok
    end.

recorded_reversed() ->
    case erlang:get(?RECORDING_EVENTS_KEY) of
        undefined -> [];
        Events -> Events
    end.

next_recording_sequence() ->
    Seq =
        case erlang:get(?RECORDING_SEQUENCE_KEY) of
            undefined -> 1;
            Prev -> Prev + 1
        end,
    erlang:put(?RECORDING_SEQUENCE_KEY, Seq),
    Seq.

event_record(Seq, HookReq, RecOpts) ->
    Event = hb_maps:get(<<"event">>, HookReq, undefined, #{}),
    Topic = hb_maps:get(<<"topic">>, HookReq, global, #{}),
    Mod = hb_maps:get(<<"module">>, HookReq, "", #{}),
    Func = hb_maps:get(<<"function">>, HookReq, "", #{}),
    Line = hb_maps:get(<<"line">>, HookReq, "", #{}),
    Now = erlang:monotonic_time(microsecond),
    Last =
        case erlang:get(?RECORDING_LAST_KEY) of
            undefined -> Now;
            Prev -> Prev
        end,
    Start =
        case erlang:get(?RECORDING_START_KEY) of
            undefined -> Now;
            Started -> Started
        end,
    erlang:put(?RECORDING_LAST_KEY, Now),
    Base = #{
        <<"sequence">> => Seq,
        <<"time">> => Now - Start,
        <<"delta">> => Now - Last,
        <<"topic">> => record_name(Topic),
        <<"name">> => record_name(Event),
        <<"module">> => record_name(Mod),
        <<"function">> => record_name(Func),
        <<"line">> => Line,
        <<"event">> => Event
    },
    maybe_add_stack(Base, RecOpts).

maybe_add_stack(Event, #{ stack := true }) ->
    case erlang:process_info(self(), current_stacktrace) of
        {current_stacktrace, Stack} ->
            Event#{
                <<"stack">> => trim_stack(Stack)
            };
        _ ->
            Event
    end;
maybe_add_stack(Event, _RecOpts) ->
    Event.

trim_stack(Stack) ->
    case trim_stack(Stack, not_found) of
        not_found -> Stack;
        Trimmed -> Trimmed
    end.

trim_stack([{hb_event, log, 6, _} | Rest], _NotFound) ->
    Rest;
trim_stack([_ | Rest], NotFound) ->
    trim_stack(Rest, NotFound);
trim_stack([], NotFound) ->
    NotFound.

normalize_recording_opts(Opts) ->
    #{
        stack => bool_opt(Opts, <<"stack">>, false)
    }.

recording_opts(Req, Opts) ->
    Raw =
        lists:foldl(
            fun(Key, Acc) ->
                case hb_maps:find(Key, Req, Opts) of
                    {ok, Val} -> Acc#{ Key => Val };
                    _ -> Acc
                end
            end,
            #{},
            ?RECORDING_OPT_KEYS
        ),
    case {maps:find(<<"stack">>, Raw), maps:find(<<"trace">>, Raw)} of
        {error, {ok, Trace}} -> Raw#{ <<"stack">> => Trace };
        _ -> Raw
    end.

bool_opt(Opts, Key, Default) ->
    case maps:get(Key, Opts, Default) of
        true -> true;
        <<"true">> -> true;
        <<"1">> -> true;
        1 -> true;
        _ -> false
    end.

record_name(Term) when is_tuple(Term), tuple_size(Term) > 0 ->
    record_name(element(1, Term));
record_name(Term) when is_atom(Term) ->
    atom_to_binary(Term, utf8);
record_name(Term) when is_binary(Term), byte_size(Term) =< 100 ->
    text_name(Term);
record_name(Term) ->
    format_term(Term, 100).

json_value(Term, Opts) when ?IS_LINK(Term) ->
    try json_value(hb_cache:ensure_loaded(Term, Opts), Opts)
    catch _:_ ->
        #{
            <<"type">> => <<"link">>,
            <<"unresolved">> => hb_link:format_unresolved(Term, Opts)
        }
    end;
json_value(Term, _Opts) when is_binary(Term) ->
    json_binary(Term);
json_value(Term, _Opts) when is_integer(Term) ->
    Term;
json_value(Term, _Opts) when is_float(Term) ->
    Term;
json_value(true, _Opts) ->
    true;
json_value(false, _Opts) ->
    false;
json_value(undefined, _Opts) ->
    null;
json_value(Term, _Opts) when is_atom(Term) ->
    atom_to_binary(Term, utf8);
json_value(Term, Opts) when is_tuple(Term) ->
    [json_value(Item, Opts) || Item <- tuple_to_list(Term)];
json_value(Term, Opts) when is_list(Term) ->
    case io_lib:printable_unicode_list(Term) of
        true ->
            json_binary(unicode_list_to_binary(Term));
        false ->
            [json_value(Item, Opts) || Item <- Term]
    end;
json_value(Term, Opts) when is_map(Term) ->
    maps:from_list(
        [
            {json_key(Key), json_value(Value, Opts)}
        ||
            {Key, Value} <- maps:to_list(Term)
        ]
    );
json_value(Term, _Opts) ->
    term_text(Term).

json_key(Key) when is_binary(Key) ->
    json_key_binary(Key);
json_key(Key) when is_atom(Key) ->
    atom_to_binary(Key, utf8);
json_key(Key) when is_integer(Key) ->
    integer_to_binary(Key);
json_key(Key) ->
    format_term(Key, 100).

unicode_list_to_binary(List) ->
    try unicode:characters_to_binary(List) of
        Bin when is_binary(Bin) -> Bin;
        _ -> term_text(List)
    catch _:_ ->
        term_text(List)
    end.

json_binary(Bin) ->
    case safe_text_binary(Bin) of
        true ->
            Bin;
        false ->
            #{
                <<"type">> => <<"binary">>,
                <<"encoding">> => <<"base64url">>,
                <<"value">> => hb_util:encode(Bin)
            }
    end.

text_name(Bin) ->
    case safe_text_binary(Bin) of
        true -> Bin;
        false -> format_term(Bin, 100)
    end.

json_key_binary(Bin) ->
    case safe_text_binary(Bin) of
        true -> Bin;
        false -> <<"base64url:", (hb_util:encode(Bin))/binary>>
    end.

safe_text_binary(Bin) ->
    case unicode:characters_to_binary(Bin, utf8, utf8) of
        Bin when is_binary(Bin) -> no_header_control_bytes(Bin);
        _ -> false
    end.

no_header_control_bytes(<<>>) ->
    true;
no_header_control_bytes(<<Byte, Rest/binary>>)
        when Byte >= 32 orelse Byte =:= $\t orelse Byte =:= $\n orelse Byte =:= $\r ->
    no_header_control_bytes(Rest);
no_header_control_bytes(_) ->
    false.

format_term(Term, MaxBytes) ->
    truncate_binary(term_text(Term), MaxBytes).

term_text(Term) ->
    try unicode:characters_to_binary(io_lib:format("~0tp", [Term])) of
        Bin when is_binary(Bin) -> Bin;
        _ -> fallback_term_text(Term)
    catch _:_ ->
        fallback_term_text(Term)
    end.

fallback_term_text(Term) ->
    try safe_term_binary(iolist_to_binary(io_lib:format("~0p", [Term])))
    catch _:_ -> <<"#unprintable">>
    end.

safe_term_binary(Bin) ->
    case unicode:characters_to_binary(Bin, utf8, utf8) of
        Bin when is_binary(Bin) -> Bin;
        _ -> hb_util:encode(Bin)
    end.

truncate_binary(Bin, MaxBytes) when byte_size(Bin) =< MaxBytes ->
    Bin;
truncate_binary(Bin, MaxBytes) ->
    Marker = truncation_marker(MaxBytes),
    Keep = max(0, MaxBytes - byte_size(Marker)),
    Prefix = utf8_prefix(Bin, Keep),
    <<Prefix/binary, Marker/binary>>.

truncation_marker(MaxBytes) when MaxBytes =< 0 ->
    <<>>;
truncation_marker(1) ->
    <<".">>;
truncation_marker(2) ->
    <<"..">>;
truncation_marker(_) ->
    <<"...">>.

utf8_prefix(_Bin, 0) ->
    <<>>;
utf8_prefix(Bin, Keep) ->
    <<Prefix:Keep/binary, _/binary>> = Bin,
    case safe_text_binary(Prefix) of
        true -> Prefix;
        false -> utf8_prefix(Bin, Keep - 1)
    end.

%%% Tests

log_start_passes_through_without_device_test() ->
    clear_recording(),
    {ok, Out} =
        log(
            #{ <<"device">> => ?DEVICE, <<"keep">> => <<"yes">> },
            #{ <<"log">> => <<"start">> },
            #{}
        ),
    ?assertEqual(<<"yes">>, maps:get(<<"keep">>, Out)),
    ?assertNot(maps:is_key(<<"device">>, Out)),
    ?assertEqual(true, recording()),
    clear_recording().

new_records_started_event_test() ->
    clear_recording(),
    start_recording(#{ <<"stack">> => true }),
    {ok, _} =
        new(
            #{},
            #{
                <<"event">> => {generating_id, #{ <<"a">> => 1 }},
                <<"topic">> => debug_id,
                <<"module">> => hb_message,
                <<"function">> => id,
                <<"line">> => 123
            },
            #{}
        ),
    Report = report(),
    ?assertEqual(1, maps:get(<<"count">>, Report)),
    [Event] = maps:get(<<"events">>, Report),
    ?assertEqual(<<"debug_id">>, maps:get(<<"topic">>, Event)),
    ?assertEqual(<<"generating_id">>, maps:get(<<"name">>, Event)),
    ?assert(maps:is_key(<<"stack">>, Event)),
    clear_recording().

ao_start_hook_read_test() ->
    clear_recording(),
    Opts =
        #{
            <<"on">> =>
                #{
                    <<"event">> =>
                        #{
                            <<"device">> => ?DEVICE,
                            <<"path">> => <<"new">>,
                            <<"hook/result">> => <<"ignore">>
                        }
                }
        },
    {ok, _} =
        hb_ao:resolve(
            #{
                <<"device">> => ?DEVICE,
                <<"path">> => <<"log">>,
                <<"log">> => <<"start">>
            },
            Opts
        ),
    hb_event:log(test_events, {generating_id, ok}, ?MODULE, ?FUNCTION_NAME, ?LINE, Opts),
    {ok, Events} =
        hb_ao:resolve(
            #{ <<"device">> => ?DEVICE, <<"path">> => <<"log">> },
            Opts
        ),
    ?assert(length(Events) > 0),
    ?assert(
        lists:any(
            fun(Event) ->
                maps:get(<<"topic">>, Event, <<>>) =:= <<"test_events">>
                    andalso maps:get(<<"name">>, Event, <<>>) =:= <<"generating_id">>
            end,
            Events
        )
    ),
    clear_recording().
