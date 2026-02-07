%%% @doc Shared helpers for Mysticeti-C test modules and examples.
%%%
%%% These utilities use the canonical `process@1.0` HTTP surface and the
%%% default scheduler response shape (a schedule map containing `assignments`).
%%% Assignment maps are normalized to slot keys (ints); non-slot metadata
%%% (e.g. commitments) is dropped.
-module(dev_mysticeti_test_utils).
-export([
    post_process_schedule/4,
    fetch_assignments_http/5,
    assignment_message/2,
    assignment_body/2,
    assignment_message_id/2
]).
-include("include/hb.hrl").

%% @doc POST /<process>/schedule with a signed message.
post_process_schedule(Node, ProcID, Msg, Opts) ->
    MsgLoaded = hb_cache:ensure_all_loaded(Msg, Opts),
    _ = hb_cache:write(MsgLoaded, Opts),
    Req = #{
        <<"path">> => << ProcID/binary, "/schedule" >>,
        <<"method">> => <<"POST">>,
        <<"body">> => MsgLoaded
    },
    hb_http:post(Node, Req, Opts).

%% @doc Fetch assignments over HTTP (GET /<process>/schedule).
fetch_assignments_http(Node, ProcID, From, To, Opts) ->
    Req0 = #{ <<"path">> => << ProcID/binary, "/schedule" >> },
    Req1 = maybe_put(<<"from">>, From, Req0, Opts),
    Req = maybe_put(<<"to">>, To, Req1, Opts),
    case hb_http:get(Node, Req, Opts) of
        {ok, Schedule} ->
            slot_only_assignments(assignments_from_schedule(Schedule, Opts), Opts);
        _ -> #{}
    end.

%% @doc Extract the scheduled message from an assignment.
assignment_message(Assignment, Opts) ->
    Loaded = load_message(Assignment, Opts),
    case hb_maps:get(<<"body">>, Loaded, not_found, Opts) of
        not_found ->
            case hb_maps:get(<<"message">>, Loaded, not_found, Opts) of
                not_found ->
                    case hb_maps:get(<<"assignment">>, Loaded, not_found, Opts) of
                        not_found -> not_found;
                        Sub -> assignment_message(Sub, Opts)
                    end;
                Msg ->
                    load_message(Msg, Opts)
            end;
        Msg ->
            load_message(Msg, Opts)
    end.

%% @doc Extract the message body from an assignment.
assignment_body(Assignment, Opts) ->
    case assignment_message(Assignment, Opts) of
        not_found -> not_found;
        Msg when is_map(Msg) -> hb_maps:get(<<"body">>, Msg, not_found, Opts);
        Msg -> Msg
    end.

%% @doc Extract the message id from an assignment.
assignment_message_id(Assignment, Opts) ->
    case assignment_message(Assignment, Opts) of
        not_found -> {error, missing_message};
        Msg when ?IS_ID(Msg) -> {ok, Msg};
        Msg when is_map(Msg) ->
            try {ok, hb_message:id(Msg, all, Opts)}
            catch Class:Reason -> {error, {invalid_message, Class, Reason}}
            end;
        _ -> {error, not_a_message}
    end.

assignments_from_schedule(Schedule, Opts) when is_map(Schedule) ->
    case hb_maps:get(<<"assignments">>, Schedule, not_found, Opts) of
        not_found -> #{};
        Assignments -> assignments_to_map(Assignments, Opts)
    end;
assignments_from_schedule(_, _Opts) ->
    #{}.

assignments_to_map(Assignments, _Opts) when is_map(Assignments) ->
    Assignments;
assignments_to_map(Assignments, Opts) when is_list(Assignments) ->
    lists:foldl(
        fun(Item, Acc) ->
            case hb_maps:get(<<"slot">>, Item, not_found, Opts) of
                not_found -> Acc;
                Slot -> hb_maps:put(Slot, Item, Acc, Opts)
            end
        end,
        #{},
        Assignments
    );
assignments_to_map(_, _Opts) ->
    #{}.

slot_only_assignments(Assignments, Opts) when is_map(Assignments) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            case hb_util:safe_int(Key) of
                {ok, IntKey} -> hb_maps:put(IntKey, Value, Acc, Opts);
                {error, _} -> Acc
            end
        end,
        #{},
        hb_maps:to_list(Assignments, Opts)
    );
slot_only_assignments(_, _Opts) ->
    #{}.

maybe_put(_Key, undefined, Map, _Opts) ->
    Map;
maybe_put(_Key, not_found, Map, _Opts) ->
    Map;
maybe_put(Key, Value, Map, Opts) ->
    hb_maps:put(Key, Value, Map, Opts).

load_message(not_found, _Opts) ->
    not_found;
load_message(Msg, Opts) when is_map(Msg) ->
    hb_cache:ensure_all_loaded(Msg, Opts);
load_message(Msg, Opts) when ?IS_LINK(Msg) ->
    hb_cache:ensure_loaded(Msg, Opts);
load_message(Msg, Opts) when ?IS_ID(Msg) ->
    case hb_cache:read(Msg, Opts) of
        {ok, Loaded} -> Loaded;
        _ -> Msg
    end;
load_message(Msg, _Opts) ->
    Msg.
