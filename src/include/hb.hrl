-include("include/ar.hrl").

-define(HYPERBEAM_VERSION, 0.9).

%% @doc Macro for checking if a message is empty, ignoring its hashpath.
-define(IS_EMPTY_MESSAGE(Msg), (map_size(Msg) == 0) orelse (map_size(Msg) == 1 andalso (is_map_key(priv, Msg) orelse is_map_key(<<"priv">>, Msg)))).
%% @doc Macro usable in guards that validates whether a term is a
%% human-readable ID encoding.
-define(IS_ID(X), (is_binary(X) andalso (byte_size(X) == 42 orelse byte_size(X) == 43 orelse byte_size(X) == 32))).
%% @doc Macro for checking a term is a link.
-define(IS_LINK(X), (is_tuple(X) andalso element(1, X) == link)).
%% @doc List of special keys that are used in the AO-Core protocol.
-define(AO_CORE_KEYS, [<<"path">>, <<"hashpath">>, <<"priv">>]).
%% @doc Keys that can be regenerated losslessly.
-define(REGEN_KEYS, [<<"unsigned_id">>, <<"content-digest">>]).

%% @doc Record used for parsing relevant components of a cursor-browsable
%% response.
-record(result, {
          messages = [],
          assignments = [],
          spawns = [],
          output = [],
          cursor = undefined
         }).

%%% Functional macros that pass the current module and line number to the
%%% underlying function.
-define(event(X), hb_event:log(global, X, ?MODULE, ?FUNCTION_NAME, ?LINE)).
-define(event(Topic, X), hb_event:log(Topic, X, ?MODULE, ?FUNCTION_NAME, ?LINE)).
-define(event(Topic, X, Opts), hb_event:log(maps:get(topic, Opts, Topic), X, ?MODULE, ?FUNCTION_NAME, ?LINE, Opts)).
-define(debug_wait(T), hb:debug_wait(T, ?MODULE, ?FUNCTION_NAME, ?LINE)).
-define(debug_print(X), hb_util:debug_print(X, ?MODULE, ?FUNCTION_NAME, ?LINE)).
-define(no_prod(X), hb:no_prod(X, ?MODULE, ?LINE)).

%%% Macro shortcuts for debugging.
%% @doc A macro for marking that you got 'here'.
-define(h(), hb_event:log("[Debug point reached.]", ?MODULE, ?FUNCTION_NAME, ?LINE)).
%% @doc Quickly print a value in the logs. Currently uses the event
%% function, but should be moved to a debug-specific function once we
%% build out better logging infrastructure.
-define(p(X), hb_event:log(X, ?MODULE, ?FUNCTION_NAME, ?LINE)).
%% @doc Print the trace of the current stack, up to the first non-hyperbeam
%% module.
-define(trace(), hb_util:trace_macro_helper(fun hb_util:print_trace/4, catch error(test), ?MODULE, ?FUNCTION_NAME, ?LINE)).
-define(trace_short(), hb_util:trace_macro_helper(fun hb_util:print_trace_short/4, catch error(test), ?MODULE, ?FUNCTION_NAME, ?LINE)).
%% @doc Draw a horizontal line in the logs.
-define(hr(), io:format(standard_error, "--------------------------------------------------------------------------------~n", [])).
-define(hr(Str), io:format(standard_error, iolist_to_binary(["---------------------------------------- ", Str, " ----------------------------------------~n"]), [])).
