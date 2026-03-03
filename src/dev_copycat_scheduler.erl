%%% @doc A `~copycat@1.0' engine that fetches assignments from a legacy AO
%%% scheduler (`ao.TN.1' variant) and replicates them into the local scheduler
%%% cache, making them available via the local scheduler device and GQL
%%% querying interface.
%%%
%%% Parameters accepted in the request:
%%%   `process': Target process ID (required).
%%%   `from':    First slot to download, inclusive (default: 0).
%%%   `to':      Last slot to download, inclusive (default: fetch all pages).
%%%   `node':    Base URL of the remote SU (default: ?DEFAULT_SU_NODE).
-module(dev_copycat_scheduler).
-export([scheduler/3]).
-include_lib("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% Default legacy AO testnet scheduler-unit router.
-define(DEFAULT_SU_NODE, <<"https://su-router.ao-testnet.xyz">>).
%%% Maximum assignments requested per HTTP call.
-define(PAGE_LIMIT, 1000).

%% @doc Entry point for the copycat scheduler engine. Fetches assignments for
%% a process from a remote legacy SU and writes them into the local scheduler
%% cache so that they are accessible through `~scheduler@1.0/schedule' and
%% the local GraphQL query interface.
scheduler(_Base, Request, Opts) ->
    case hb_maps:find(<<"process">>, Request, Opts) of
        {ok, RawProcID} ->
            ProcID = hb_util:human_id(RawProcID),
            {From, To} = parse_range(Request, Opts),
            Node = hb_maps:get(<<"node">>, Request, ?DEFAULT_SU_NODE, Opts),
            ?event(copycat_short,
                {scheduler_replication_started,
                    {proc_id, ProcID},
                    {from, From},
                    {to, To},
                    {node, {string, Node}}
                }
            ),
            fetch_schedule(ProcID, Node, From, To, 0, Opts);
        error ->
            {error, <<"Missing required parameter: process">>}
    end.

%% @doc Parse the from/to slot range from the request.
parse_range(Request, Opts) ->
    From = hb_util:int(hb_maps:get(<<"from">>, Request, 0, Opts)),
    To =
        case hb_maps:find(<<"to">>, Request, Opts) of
            {ok, ToVal} -> hb_util:int(ToVal);
            error -> undefined
        end,
    {From, To}.

%% @doc Stop when we have fetched up to (or past) the requested `to' slot.
fetch_schedule(_ProcID, _Node, From, To, Total, _Opts)
        when To =/= undefined andalso From > To ->
    {ok, Total};
%% @doc Fetch one page of assignments, write them to the cache, then
%% continue until the remote SU reports no further pages.
fetch_schedule(ProcID, Node, From, To, Total, Opts) ->
    Path = build_path(ProcID, From, To),
    ?event(copycat_scheduler,
        {fetching_page,
            {node, {explicit, Node}},
            {path, {string, Path}},
            {total_so_far, Total}
        }
    ),
    case hb_http:get(Node, Path, Opts#{ http_client => httpc, protocol => http2 }) of
        {ok, Res} ->
            handle_response(ProcID, Node, From, To, Total, Res, Opts);
        {error, Reason} ->
            ?event(copycat_scheduler,
                {http_error, {reason, Reason}, {proc_id, ProcID}}
            ),
            {error, Reason}
    end.

%% @doc Dispatch on the HTTP status of a page response.
handle_response(ProcID, Node, _From, To, Total, Res, Opts) ->
    Status = hb_util:int(hb_ao:get(<<"status">>, Res, 200, Opts)),
    case Status of
        200 ->
            RawBody =
                hb_ao:get(<<"body">>, Res, <<"{}">>,
                    Opts#{ hashpath => ignore }),
            JSONRes = hb_json:decode(RawBody),
            Edges = hb_maps:get(<<"edges">>, JSONRes, [], Opts),
            Written = write_assignments(ProcID, Edges, Opts),
            NewTotal = Total + Written,
            ?event(copycat_short,
                {scheduler_page_written,
                    {proc_id, ProcID},
                    {written, Written},
                    {total, NewTotal}
                }
            ),
            maybe_continue(ProcID, Node, To, NewTotal, Edges, JSONRes, Opts);
        Other ->
            ?event(copycat_scheduler,
                {unexpected_status, Other, {proc_id, ProcID}}
            ),
            {ok, Total}
    end.

%% @doc Decide whether to fetch the next page based on `page_info.has_next_page'
%% and the cursor of the last edge returned.
maybe_continue(_ProcID, _Node, _To, Total, [], _JSONRes, _Opts) ->
    {ok, Total};
maybe_continue(ProcID, Node, To, Total, Edges, JSONRes, Opts) ->
    HasNextPage =
        hb_util:deep_get(<<"page_info/has_next_page">>, JSONRes, false, Opts),
    case HasNextPage of
        true ->
            LastCursor =
                hb_maps:get(<<"cursor">>, lists:last(Edges), 0, Opts),
            NextFrom = hb_util:int(LastCursor) + 1,
            fetch_schedule(ProcID, Node, NextFrom, To, Total, Opts);
        _ ->
            {ok, Total}
    end.

%% @doc Build the URL path for a single page of legacy-SU schedule data.
%%
%% The legacy SU `from-nonce' parameter is exclusive — it returns slots
%% *strictly after* the given nonce.  We therefore subtract 1 from `From'
%% so that the page starts at slot `From'.
build_path(ProcID, From, To) ->
    ToParam =
        case To of
            undefined -> <<>>;
            ToVal ->
                <<"&to-nonce=", (integer_to_binary(ToVal))/binary>>
        end,
    ModFrom = max(0, From - 1),
    <<
        ProcID/binary,
        "?process-id=", ProcID/binary,
        "&from-nonce=", (integer_to_binary(ModFrom))/binary,
        ToParam/binary,
        "&limit=", (integer_to_binary(?PAGE_LIMIT))/binary
    >>.

%% @doc Convert and write a list of AOS2-style assignment edges to the local
%% scheduler cache.  Each edge is fully reverse-indexed using
%% `dev_scheduler_formats:aos2_to_assignment/2', which calls
%% `hb_gateway_client:result_to_message' on both the assignment TX and the
%% message TX.  That chain runs `dev_codec_ans104:from' then
%% `dev_codec_structured:to', bringing every ANS-104 tag (Action, Reference,
%% Process, Nonce, etc.) up to the top level of the stored message — making
%% the data directly queryable via the local GQL interface.
%%
%% After conversion we set `variant = ao.N.1' so that
%% `dev_scheduler_cache:read/3' returns the message as-is instead of
%% attempting a second AOS2 conversion.
%%
%% Returns the count of successfully cached assignments.
write_assignments(ProcID, Edges, Opts) ->
    lists:foldl(
        fun(Edge, Count) ->
            try
                % Fully convert: tags in node.assignment and node.message
                % become top-level fields in the returned structured message.
                Assignment =
                    dev_scheduler_formats:aos2_to_assignment(Edge, Opts),
                ?event(copycat_scheduler, {writing_assignment, {assignment, Assignment}}),
                % Ensure process matches the target and mark the stored
                % format as ao.N.1 so dev_scheduler_cache:read treats it
                % as already fully resolved (no double-conversion).
                WithMeta = Assignment#{
                    <<"process">> => ProcID,
                    % TODO: ao.TN.1
                    <<"variant">> => <<"ao.N.1">>
                },
                case dev_scheduler_cache:write(WithMeta, Opts) of
                    ok ->
                        Count + 1;
                    {error, WriteErr} ->
                        ?event(
                            copycat_scheduler,
                            {write_failed,
                                {slot, maps:get(<<"slot">>, WithMeta, undefined)},
                                {reason, WriteErr}
                            }
                        ),
                        Count
                end
            catch
                Class:CatchReason:Stack ->
                    ?event(
                        copycat_scheduler,
                        {assignment_write_failed,
                            {class, Class},
                            {reason, CatchReason},
                            {stacktrace, Stack}
                        }
                    ),
                    Count
            end
        end,
        0,
        Edges
    ).

%%% Tests

%% @doc Basic unit test: parse_range defaults.
parse_range_defaults_test() ->
    {From, To} = parse_range(#{}, #{}),
    ?assertEqual(0, From),
    ?assertEqual(undefined, To).

%% @doc Unit test: parse_range with explicit values.
parse_range_explicit_test() ->
    Req = #{<<"from">> => <<"5">>, <<"to">> => <<"50">>},
    {From, To} = parse_range(Req, #{}),
    ?assertEqual(5, From),
    ?assertEqual(50, To).

%% @doc Unit test: build_path with no upper bound.
build_path_no_to_test() ->
    ProcID = <<"abc123">>,
    Path = build_path(ProcID, 0, undefined),
    ?assert(binary:match(Path, <<"?process-id=abc123">>) =/= nomatch),
    ?assert(binary:match(Path, <<"&from-nonce=0">>) =/= nomatch),
    ?assert(binary:match(Path, <<"&to-nonce=">>) =:= nomatch).

%% @doc Unit test: build_path with an upper bound.
build_path_with_to_test() ->
    ProcID = <<"abc123">>,
    Path = build_path(ProcID, 10, 20),
    ?assert(binary:match(Path, <<"&from-nonce=9">>) =/= nomatch),
    ?assert(binary:match(Path, <<"&to-nonce=20">>) =/= nomatch).

%% @doc Unit test: missing process parameter returns error.
missing_process_test() ->
    ?assertMatch(
        {error, <<"Missing required parameter: process">>},
        scheduler(#{}, #{}, #{})
    ).
