%%% @doc An interface for resolving requests across multiple HTTP servers, either
%%% concurrently or sequentially, and processing the results in a configurable
%%% manner.
%%% 
%%% The `Config' message for a call to `request/5' may contain the following
%%% fields:
%%% 
%%% - `multirequest-nodes': A list of nodes to request from.
%%% - `multirequest-responses': The number of responses to gather.
%%% - `multirequest-stop-after': Whether to stop after the required number of
%%%   responses, or if the caller is killed.
%%% - `multirequest-parallel': Whether to run the requests in parallel.
%%% - `multirequest-admissible': A message to resolve against the response.
%%% - `multirequest-admissible-status': The statuses that are admissible.
%%% 
%%% The `admissible' message is executed as a `base' message, with its `path'
%%% field moved to the request (or set to `is-admissible' if not present):
%%% ```
%%%     resolve(Base, Response#{ <<"path">> => Base/path OR /is-admissible }, Opts)
%%% '''
%%%
%%% Each selected response invokes `on/http-client-admissible-receipt'
%%% asynchronously, with a signed receipt in `body'. It contains `request',
%%% `response', the original `request-method' and `request-path', and the peer's
%%% `reference', if set. Acceptance means the configured checks passed; without
%%% a content predicate it does not establish that the returned content is valid.
-module(hb_http_multi).
-export([request/5, multirequest_keys/2]).
-include("include/hb.hrl").

%% @doc Dispatch the same HTTP request to many nodes. Can be configured to
%% await responses from all nodes or just one, and to halt all requests after
%% after it has received the required number of responses, or to leave all
%% requests running until they have all completed. Additionally, filters can
%% be applied to the responses to determine if they are admissible -- both on
%% `status' only, or as an AO-Core resolution on the response message.
%% 
%% Default: Race for first response.
%%
%% Expects a config message of the following form:
%%      /Nodes/1..n: Hostname | #{ hostname => Hostname, address => Address }
%%      /Responses: Number of responses to gather
%%      /Stop-After: Should we stop after the required number of responses?
%%      /Parallel: Should we run the requests in parallel?
request(Config, Method, Path, Message, Opts) ->
    #{
        nodes := Nodes,
        responses := Responses,
        stop_after := StopAfter,
        admissible := Admissible,
        admissible_status := Statuses,
        parallel := Parallel
    } = multirequest_opts(Config, Message, Opts),
    ?event(debug_multi,
        {multirequest_opts_parsed,
            {config, Config},
            {method, Method},
            {path, Path},
            {raw_message, Message}
        }),
    {AdmissibleResults, AllResponses} =
        if Parallel =/= false ->
            parallel_multirequest(
                Parallel,
                Nodes,
                Responses,
                StopAfter,
                Method,
                Path,
                Message,
                Admissible,
                Statuses,
                Opts
            );
        true ->
            serial_multirequest(
                Nodes,
                Responses,
                Method,
                Path,
                Message,
                Admissible,
                Statuses,
                Opts
            )
        end,
    ?event(debug_http, {multirequest_results, {admissible_results, AdmissibleResults}, {all_responses, AllResponses}}),
    case [hb_http:only_result(Res, Opts) || Res <- AdmissibleResults] of
        [] -> {error, {no_viable_responses, AllResponses}};
        Results -> if Responses == 1 -> hd(Results); true -> Results end
    end.

%% @doc Get the multirequest options from the config or message. The options in 
%% the message take precidence over the options in the config.
multirequest_opts(Config, Message, Opts) ->
    Opts#{
        nodes =>
            multirequest_opt(<<"nodes">>, Config, Message, #{}, Opts),
        responses =>
            multirequest_opt(<<"responses">>, Config, Message, 1, Opts),
        stop_after =>
            multirequest_opt(<<"stop-after">>, Config, Message, true, Opts),
        admissible =>
            multirequest_opt(<<"admissible">>, Config, Message, undefined, Opts),
        admissible_status =>
            multirequest_opt(<<"admissible-status">>, Config, Message, <<"All">>, Opts),
        parallel =>
            multirequest_opt(<<"parallel">>, Config, Message, false, Opts)
    }.

%% @doc Get a value for a multirequest option from the config or message.
multirequest_opt(Key, Config, Message, Default, Opts) ->
    hb_maps:get(
        <<"multirequest-", Key/binary>>,
        Message,
        hb_maps:get(Key, Config, Default, Opts),
        Opts
    ).

%% @doc Return the `multirequest-*' keys of a message, which configure a
%% multirequest on this node.
multirequest_keys(Message, Opts) ->
    lists:filter(
        fun(<<"multirequest-", _/binary>>) -> true; (_) -> false end,
        hb_maps:keys(Message, Opts)
    ).

%% @doc Check if a response is admissible, according to the configuration. A
%% reply is admissible if its status is one of the configured statuses and it
%% satisfies the `admissible' message. With the statuses `All', a reply that
%% is not `ok' is not admissible. A request that fails without a reply is not
%% admissible.
is_admissible(ErlStatus, Res, Admissible, Statuses, Opts)
        when ErlStatus == ok; is_map(Res), Statuses =/= <<"All">> ->
    ?event(debug_multi,
        {is_admissible,
            {response, Res},
            {admissible, Admissible},
            {statuses, Statuses}
        }
    ),
    AdmissibleStatus = admissible_status(Res, Statuses, Opts),
    ?event(debug_multi, {admissible_status, {result, AdmissibleStatus}}),
    AdmissibleStatus andalso admissible_response(Res, Admissible, Opts);
is_admissible(_, _, _, _, _) -> false.

%% @doc Serially request a message, collecting responses until the required
%% number of responses have been gathered. Ensure that the statuses are
%% allowed, according to the configuration.
%% Returns {AdmissibleList, AllList} where AdmissibleList contains only
%% admissible responses and AllList contains all responses.
serial_multirequest(_Nodes, 0, _Method, _Path, _Message, _Admissible, _Statuses, _Opts) -> {[], []};
serial_multirequest([], _, _Method, _Path, _Message, _Admissible, _Statuses, _Opts) -> {[], []};
serial_multirequest([Node|Nodes], Remaining, Method, Path, Message, Admissible, Statuses, Opts) ->
    {ErlStatus, Res} = node_request(Method, Node, Path, Message, Opts),
    case is_admissible(ErlStatus, Res, Admissible, Statuses, Opts) of
        true ->
            receipt(Node, Method, Path, Message, Res, Opts),
            ?event(debug_http, {admissible_status, {response, Res}}),
            {AdmissibleAcc, AllAcc} = serial_multirequest(
                Nodes,
                Remaining - 1,
                Method,
                Path,
                Message,
                Admissible,
                Statuses,
                Opts
            ),
            {[{ErlStatus, Res} | AdmissibleAcc], [{ErlStatus, Res} | AllAcc]};
        false ->
            ?event(debug_http, {inadmissible_status, {response, Res}}),
            {AdmissibleAcc, AllAcc} = serial_multirequest(
                Nodes,
                Remaining,
                Method,
                Path,
                Message,
                Admissible,
                Statuses,
                Opts
            ),
            {AdmissibleAcc, [{ErlStatus, Res} | AllAcc]}
    end.

%% @doc Dispatch the same HTTP request to many nodes in parallel.
parallel_multirequest(true, Nodes, Responses, StopAfter, Method, Path, Message, Admissible, Statuses, Opts) ->
    parallel_multirequest(length(Nodes), Nodes, Responses, StopAfter, Method, Path, Message, Admissible, Statuses, Opts);
parallel_multirequest(MaxWorkers, Nodes, Responses, StopAfter, Method, Path, Message, Admissible, Statuses, Opts) ->
    Ref = make_ref(),
    {Workers, Queue} = start_workers(MaxWorkers, Ref, Nodes, StopAfter, Method, Path, Message, Opts),
    parallel_responses([], [], Workers, Queue, {Method, Path, Message}, Ref, Responses, StopAfter, Admissible, Statuses, Opts).

%% @doc Start a new fleet of workers, returning the list of worker PIDs. If
%% `StopAfter' is `true', link each worker to the calling process, so that the
%% worker stops if the caller is killed.
start_workers(Count, Ref, Nodes, StopAfter, Method, Path, Message, Opts) ->
    Parent = self(),
    {NewWorkerNodes, NewRemainingNodes} =
        lists:split(min(Count, length(Nodes)), Nodes),
    {
        lists:map(
            fun(Node) ->
                spawn_opt(
                    fun() ->
                        Res = node_request(Method, Node, Path, Message, Opts),
                        receive no_reply -> stopping
                        after 0 -> Parent ! {Ref, self(), {Node, Res}}
                        end
                    end,
                    [link || StopAfter == true]
                )
            end,
            NewWorkerNodes
        ),
        NewRemainingNodes
    }.

%% @doc Request a message from one node. A request that raises is a failure,
%% which is not admissible. The reply is whole: the admissibility checks read
%% its `status', and `request/5' takes the value that its `ao-result' selects.
node_request(Method, Node, Path, Message, Opts) ->
    ReplyOpts = Opts#{ <<"http-only-result">> => false },
    try hb_http:request(Method, Node, Path, Message, ReplyOpts)
    catch Class:Exception:Stacktrace ->
        {failure,
            #{
                <<"node">> => Node,
                <<"class">> => Class,
                <<"exception">> => Exception,
                <<"trace">> => Stacktrace
            }
        }
    end.

%% @doc Observe a selected response without changing it. Use the same peer
%% options as the HTTP request, so its reference and hooks remain attributable.
receipt(Node, Method, Path, Message, Response, Opts) ->
    PeerOpts =
        case Node of
            #{ <<"uri">> := _, <<"opts">> := Extra } ->
                hb_maps:merge(
                    Opts,
                    hb_opts:mimic_default_types(Extra, new_atoms, Opts),
                    Opts
                );
            _ -> Opts
        end,
    spawn(
        fun() ->
            hb_http_client:receipt(
                <<"http-client-admissible-receipt">>,
                #{
                    <<"request-method">> => Method,
                    <<"request-path">> => Path,
                    <<"request">> => Message,
                    <<"response">> => Response
                },
                PeerOpts
            )
        end
    ).

%% @doc Check if a status is allowed, according to the configuration. Statuses
%% can be a single integer, a list of integers, a comma-separated list of
%% integers, or the string `All'. Integers may be given as binaries.
admissible_status(_, <<"All">>, _Opts) -> true;
admissible_status(ResponseMsg = #{ <<"status">> := _ }, Statuses, Opts) ->
    admissible_status(
        hb_http:status_code({ok, ResponseMsg}, Opts),
        Statuses,
        Opts
    );
admissible_status(Status, Statuses, Opts) when is_integer(Statuses) ->
    admissible_status(Status, [Statuses], Opts);
admissible_status(Status, Statuses, Opts) when is_binary(Status) ->
    admissible_status(binary_to_integer(Status), Statuses, Opts);
admissible_status(Status, Statuses, Opts) when is_binary(Statuses) ->
    admissible_status(Status, hb_util:binary_to_strings(Statuses), Opts);
admissible_status(Status, Statuses, _Opts) when is_list(Statuses) ->
    lists:member(Status, lists:map(fun hb_util:int/1, Statuses)).

%% @doc If an `admissable` message is set for the request, check if the response
%% adheres to it. Else, return `true'.
admissible_response(_Response, undefined, _Opts) -> true;
admissible_response(Response, Msg, Opts) ->
    Path = hb_maps:get(<<"path">>, Msg, <<"is-admissible">>, Opts),
    Req = Response#{ <<"path">> => Path },
    Base = hb_message:without_unless_signed([<<"path">>], Msg, Opts),
    ?event(debug_multi,
        {executing_admissible_message, {message, Base}, {req, Req}}
    ),
    try hb_ao:resolve(Base, Req, Opts) of
        {ok, Res} when is_atom(Res) or is_binary(Res) ->
            ?event(debug_multi, {admissible_result, {result, Res}}),
            hb_util:atom(Res) == true;
        {error, Reason} ->
            ?event(debug_multi, {admissible_error, {reason, Reason}}),
            false
    catch 
        Class:Reason:Stacktrace ->
            ?event(error, 
                {admissible_response, 
                    {class, Class}, 
                    {reason, Reason}, 
                    {stacktrace, Stacktrace}
                }
            ),
            false
    end.

%% @doc Collect the necessary number of responses, and stop workers if
%% configured to do so.
%% Returns {AdmissibleList, AllList} where AdmissibleList contains only
%% admissible responses and AllList contains all responses.
parallel_responses(AdmissibleRes, AllRes, [], _, _, Ref, _Awaiting, _StopAfter, _Admissible, _Statuses, _Opts) ->
    empty_inbox(Ref),
    {AdmissibleRes, AllRes};
parallel_responses(AdmissibleRes, AllRes, Procs, _, _, Ref, 0, false, _Admissible, _Statuses, _Opts) ->
    lists:foreach(fun(P) -> P ! no_reply end, Procs),
    empty_inbox(Ref),
    {AdmissibleRes, AllRes};
parallel_responses(AdmissibleRes, AllRes, Procs, _, _, Ref, 0, true, _Admissible, _Statuses, _Opts) ->
    lists:foreach(fun(P) -> unlink(P), exit(P, kill) end, Procs),
    empty_inbox(Ref),
    {AdmissibleRes, AllRes};
parallel_responses(AdmissibleRes, AllRes, Procs, Queue, {Method, Path, Message}, Ref, Awaiting, StopAfter, Admissible, Statuses, Opts) ->
    receive
        {Ref, Pid, {Node, {Status, NewRes}}} ->
            WorkersWithoutPid = lists:delete(Pid, Procs),
            NewAllRes = [{Status, NewRes} | AllRes],
            case is_admissible(Status, NewRes, Admissible, Statuses, Opts) of
                true ->
                    receipt(Node, Method, Path, Message, NewRes, Opts),
                    NewAwaiting = Awaiting - 1,
                    {NewProcs, NewQueue} =
                        case NewAwaiting of
                            0 -> {WorkersWithoutPid, Queue};
                            _ ->
                                {RefilledWorkers, RemainingQueue} =
                                    start_workers(1, Ref, Queue, StopAfter, Method, Path, Message, Opts),
                                {RefilledWorkers ++ WorkersWithoutPid, RemainingQueue}
                        end,
                    parallel_responses(
                        [{Status, NewRes} | AdmissibleRes],
                        NewAllRes,
                        NewProcs,
                        NewQueue,
                        {Method, Path, Message},
                        Ref,
                        NewAwaiting,
                        StopAfter,
                        Admissible,
                        Statuses,
                        Opts
                );
            false ->
                {RefilledWorkers, NewQueue} =
                    start_workers(1, Ref, Queue, StopAfter, Method, Path, Message, Opts),
                parallel_responses(
                    AdmissibleRes,
                    NewAllRes,
                    RefilledWorkers ++ WorkersWithoutPid,
                    NewQueue,
                    {Method, Path, Message},
                    Ref,
                    Awaiting,
                    StopAfter,
                    Admissible,
                    Statuses,
                    Opts
                )
        end
end.

%% @doc Empty the inbox of the current process for all messages with the given
%% reference.
empty_inbox(Ref) ->
    receive
        {Ref, _, _} -> empty_inbox(Ref);
        {Ref, _} -> empty_inbox(Ref)
    after 0 ->
        ok
    end.

%%% Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

good()   -> ao_node(hb_http_server:start_node(#{})).
slow(Ms) -> ao_node(hb_http_server:start_node(slow_node_opts(Ms))).
crash()  -> #{<<"opts">> => #{ <<"http-client">> => httpc }}.

ao_node(URL) ->
    #{<<"uri">> => <<URL/binary, "~meta@1.0/info">>,
      <<"opts">> => #{ <<"http-client">> => httpc }}.

dead_node() ->
    {ok, S} = gen_tcp:listen(0, []),
    {ok, Port} = inet:port(S),
    ok = gen_tcp:close(S),
    #{<<"uri">> => iolist_to_binary(["http://localhost:", integer_to_list(Port)]),
      <<"opts">> => #{ <<"http-client">> => httpc }}.

slow_node_opts(Ms) ->
    #{<<"test-delay">> => Ms,
      <<"on">> => #{<<"request">> =>
        #{<<"device">> => <<"test-device@1.0">>, <<"path">> => <<"delay">>}}}.

multi(Nodes, Extra) ->
    Config = Extra#{<<"nodes">> => Nodes, <<"admissible-status">> => 200},
    hb_http_multi:request(Config, <<"GET">>, <<"/">>, #{}, #{}).

multirequest_test_() ->
    {setup,
        fun() ->
            #{fast => good(), slow1 => slow(750), slow2 => slow(750),
              good1 => good(), good2 => good(), good3 => good()}
        end,
        fun(N) -> {timeout, 30, [
            {"serial fallback", fun() ->
                ?assertMatch({ok, _},
                    multi([dead_node(), maps:get(fast, N)], #{}))
            end},
            {"parallel race", fun() ->
                assert_first_response(
                    #{ <<"parallel">> => true, <<"stop-after">> => true })
            end},
            {"parallel broadcast", fun() ->
                ?assertMatch([_, _, _],
                    multi([dead_node(),
                           maps:get(good1, N), maps:get(good2, N), maps:get(good3, N),
                           maps:get(slow1, N)],
                        #{<<"parallel">> => true, <<"responses">> => 3,
                          <<"stop-after">> => false}))
            end},
            {"parallel crash", fun() ->
                Self = self(),
                Admissible = #{ <<"device">> => #{ <<"is-admissible">> =>
                    fun(_, _, _) -> Self ! admissible_ran, {ok, true} end } },
                ?assertMatch({error, {no_viable_responses, _}},
                    multi([crash(), crash()],
                        #{<<"parallel">> => true, <<"stop-after">> => true,
                          <<"admissible">> => Admissible})),
                % The `admissible' message does not run on a request that fails.
                ?assertNot(receive admissible_ran -> true after 0 -> false end)
            end},
            {"parallel caller killed", fun() ->
                Caller = spawn(fun() ->
                    receive go -> ok end,
                    multi([maps:get(slow2, N)],
                        #{<<"parallel">> => true, <<"stop-after">> => true})
                end),
                erlang:trace(Caller, true, [procs]),
                Caller ! go,
                Worker = receive {trace, Caller, spawn, W, _} -> W end,
                Monitor = erlang:monitor(process, Worker),
                exit(Caller, kill),
                % Killing the caller stops its worker before the peer replies.
                ?assertEqual(killed,
                    receive {'DOWN', Monitor, process, Worker, Reason} -> Reason
                    after 500 -> running
                    end)
            end}
        ]} end}.

%% @doc Parallel race using the actual /arweave route config from hb_opts:
%% one fast node returns 200 immediately, two slow nodes are still processing.
%% The call must return before the slow nodes finish.
parallel_race_stops_at_first_admissible_test_() ->
    {timeout, 30, fun parallel_race_stops_at_first_admissible/0}.
parallel_race_stops_at_first_admissible() ->
    Routes = maps:get(<<"routes">>, hb_opts:default_message()),
    [ArweaveRoute] =
        [R || R <- Routes,
            maps:get(<<"template">>, R, undefined) =:= <<"^/arweave">>,
            maps:is_key(<<"nodes">>, R)],
    assert_first_response(ArweaveRoute).

%% @doc Release one real HTTP response while the other requests stay blocked.
assert_first_response(Config) ->
    Parent = self(),
    Ref = make_ref(),
    Gate = #{ <<"on">> => #{ <<"request">> => #{ <<"device">> => #{
        <<"request">> => fun(_, #{ <<"body">> := Msgs }, _) ->
            Parent ! {Ref, self()},
            receive {Ref, release} -> {ok, #{ <<"body">> => Msgs }}
            after 10000 -> error(response_not_released)
            end
        end } } } },
    Nodes = [ao_node(hb_http_server:start_node(Gate)) || _ <- lists:seq(1, 3)],
    Caller = spawn(fun() -> Parent ! {Ref, result, multi(Nodes, Config)} end),
    Requests = [receive {Ref, Pid} -> Pid
        after 5000 -> error(request_not_started)
        end || _ <- Nodes],
    try
        hd(Requests) ! {Ref, release},
        receive {Ref, result, Result} -> ?assertMatch({ok, _}, Result)
        after 5000 -> error(waiting_for_other_responses)
        end
    after
        [Pid ! {Ref, release} || Pid <- Requests],
        exit(Caller, kill)
    end.

%% @doc Serial fallback: unreachable nodes are skipped until a live one
%% responds with 200.
serial_fallback_skips_non_admissible_test_() ->
    {timeout, 30, fun serial_fallback_skips_non_admissible/0}.
serial_fallback_skips_non_admissible() ->
    GoodURL = hb_http_server:start_node(#{}),
    Config = #{
        <<"nodes">> => [dead_node(), dead_node(), ao_node(GoodURL)],
        <<"parallel">> => 1,
        <<"stop-after">> => true,
        <<"admissible-status">> => 200
    },
    Result = hb_http_multi:request(Config, <<"GET">>, <<"/">>, #{}, #{}),
    ?assertMatch({ok, _}, Result).

%% @doc No admissible node: all unreachable, error tuple returned.
no_admissible_node_returns_error_test_() ->
    {timeout, 30, fun no_admissible_node_returns_error/0}.
no_admissible_node_returns_error() ->
    Config = #{
        <<"nodes">> => [dead_node(), dead_node()],
        <<"parallel">> => 1,
        <<"stop-after">> => true,
        <<"admissible-status">> => 200
    },
    Result = hb_http_multi:request(Config, <<"GET">>, <<"/">>, #{}, #{}),
    ?assertMatch({error, {no_viable_responses, _}}, Result).

-endif.
