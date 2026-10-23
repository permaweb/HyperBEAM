-include("ar.hrl").

-define(HYPERBEAM_VERSION, <<"0.11">>).
%% @doc The version of the AO runtime interface that this node implements. A
%% device archive requires it as `requires-ao-runtime-version'.
-define(AO_RUNTIME_VERSION, <<"0.11">>).
-define(PRELOADED_INDEX_KEY, <<"~meta@1.0/preloaded-devices-index">>).

%% @doc Macro for checking if a message is empty, ignoring its hashpath.
-define(IS_EMPTY_MESSAGE(Msg), (map_size(Msg) == 0) orelse (map_size(Msg) == 1 andalso (is_map_key(priv, Msg) orelse is_map_key(<<"priv">>, Msg)))).
%% @doc Macro usable in guards that validates whether a term is a
%% human-readable ID encoding.
-define(IS_ID(X), (is_binary(X) andalso (byte_size(X) == 42 orelse byte_size(X) == 43 orelse byte_size(X) == 32))).
%% @doc Macro usable in guards that recognizes the hashpath URI scheme.
-define(IS_HASHPATH(X), (is_binary(X) andalso byte_size(X) >= 3 andalso binary_part(X, 0, 3) == <<"ao:">>)).
%% @doc Builds the `ni:' name of a binary from its base64url SHA-256 (RFC 6920
%% s3, with no authority). `hb_cache' stores a binary under this name.
-define(NI_URI(ID), <<"ni:///sha-256;", (ID)/binary>>).
%% @doc The committer of unsigned commitments: an HMAC key known to everyone.
-define(UNSIGNED_COMMITTER, <<"hmac:constant:ao">>).
%% @doc Macro for checking a term is a link.
-define(IS_LINK(X), (is_tuple(X) andalso element(1, X) == link)).
%% @doc List of special keys that are used in the AO-Core protocol.
-define(AO_CORE_KEYS, [<<"path">>, <<"hashpath">>, <<"priv">>]).
%% @doc Keys that can be regenerated losslessly.
-define(REGEN_KEYS, [<<"unsigned_id">>]).
%% @doc Node options that contain the node's key material.
-define(NODE_KEY_OPTS, [<<"priv-wallet">>, <<"identities">>]).
%% @doc Prefix for ans104 and tx fields that are committed.
-define(FIELD_PREFIX, <<"field-">>).

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
-define(event(Topic, X, Opts), hb_event:log(maps:get(<<"topic">>, Opts, Topic), X, ?MODULE, ?FUNCTION_NAME, ?LINE, Opts)).
%%% Debug-only trace events. Under the `HB_VERBOSE' build flag (the `verbose'
%%% rebar3 profile, or `HB_VERBOSE=1' in the build environment) these are
%%% identical to `?event'; otherwise they compile to a literal no-op -- the
%%% payload term is never even constructed. Use these for hot-path tracing that
%%% is not consumed as metering information, so it costs nothing in a production
%%% build.
-ifdef(HB_VERBOSE).
-define(event_debug(X), ?event(X)).
-define(event_debug(Topic, X), ?event(Topic, X)).
-define(event_debug(Topic, X, Opts), ?event(Topic, X, Opts)).
-else.
-define(event_debug(X), ok).
-define(event_debug(Topic, X), ok).
-define(event_debug(Topic, X, Opts), ok).
-endif.
-define(debug_wait(T), hb:debug_wait(T, ?MODULE, ?FUNCTION_NAME, ?LINE)).
-define(debug_print(X), hb_event:debug_print(X, ?MODULE, ?FUNCTION_NAME, ?LINE)).

%%% Macro shortcuts for debugging.
%% @doc A macro for marking that you got 'here'.
-define(h(), hb_event:log("[Debug point reached.]", ?MODULE, ?FUNCTION_NAME, ?LINE)).
%% @doc Quickly print a value in the logs. Currently uses the event
%% function, but should be moved to a debug-specific function once we
%% build out better logging infrastructure.
-define(p(X), hb_event:log(X, ?MODULE, ?FUNCTION_NAME, ?LINE)).
%% @doc Print the trace of the current stack, up to the first non-hyperbeam
%% module.
-define(trace(), hb_format:trace_macro_helper(fun hb_format:print_trace/4, catch error(test), ?MODULE, ?FUNCTION_NAME, ?LINE)).
-define(trace_short(), hb_format:trace_macro_helper(fun hb_format:print_trace_short/4, catch error(test), ?MODULE, ?FUNCTION_NAME, ?LINE)).
%% @doc Draw a horizontal line in the logs.
-define(hr(), io:format(standard_error, "--------------------------------------------------------------------------------~n", [])).
-define(hr(Str), io:format(standard_error, iolist_to_binary(["---------------------------------------- ", Str, " ----------------------------------------~n"]), [])).

%%% Parallel-test auto-wiring. In a test build, every module that includes
%%% `hb.hrl' has `hb_test_parallel' activated: 0-arity functions whose
%%% names end in `_test_parallel' or `_test_parallel_' are exported and
%%% collected into an injected `all_parallel_test_/0' EUnit generator.
%%% See `hb_test_parallel' for the contract.
-ifdef(TEST).
-compile({parse_transform, hb_test_parallel}).
-endif.
