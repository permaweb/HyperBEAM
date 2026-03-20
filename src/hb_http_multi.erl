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
%%%   responses.
%%% - `multirequest-parallel': Whether to run the requests in parallel.
%%% - `multirequest-admissible': A message to resolve against the response.
%%% - `multirequest-admissible-status': The statuses that are admissible.
%%% 
%%% The `admissible' message is executed as a `base' message, with its `path'
%%% field moved to the request (or set to `is-admissible' if not present):
%%% ```
%%%     resolve(Base, Response#{ <<"path">> => Base/path OR /is-admissible }, Opts)
%%% '''
-module(hb_http_multi).
-export([request/5]).
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
    MultirequestMsg =
        hb_message:without_unless_signed(
            lists:filter(
                fun(<<"multirequest-", _/binary>>) -> true; (_) -> false end,
                hb_maps:keys(Message)
            ),
            Message,
            Opts
        ),
    ?event(debug_multi,
        {multirequest_opts_parsed,
            {config, Config},
            {method, Method},
            {path, Path},
            {raw_message, Message},
            {message_to_send, MultirequestMsg}
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
                MultirequestMsg,
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
                MultirequestMsg,
                Admissible,
                Statuses,
                Opts
            )
        end,
    ?event(debug_http, {multirequest_results, {admissible_results, AdmissibleResults}, {all_responses, AllResponses}}),
    case AdmissibleResults of
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
    hb_ao:get_first(
        [
            {Message, <<"multirequest-", Key/binary>>},
            {Config, Key}
        ],
        Default,
        Opts#{ hashpath => ignore }
    ).

%% @doc Check if a response is admissible, according to the configuration. First,
%% we check the Erlang response status to check for `ok'. If the response is
%% not `ok', it is not admissible.
%% 
%% If the response is `ok', we check the status and the response message against
%% the configuration.
is_admissible(ok, Res, Admissible, Statuses, Opts) ->
    ?event(debug_multi,
        {is_admissible,
            {response, Res},
            {admissible, Admissible},
            {statuses, Statuses}
        }
    ),
    AdmissibleStatus = admissible_status(Res, Statuses),
    ?event(debug_multi, {admissible_status, {result, AdmissibleStatus}}),
    AdmissibleResponse = admissible_response(Res, Admissible, Opts),
    ?event(debug_multi, {admissible_response, {result, AdmissibleResponse}}),
    AdmissibleStatus andalso AdmissibleResponse;
is_admissible(_, _, _, _, _) -> false.

%% @doc Serially request a message, collecting responses until the required
%% number of responses have been gathered. Ensure that the statuses are
%% allowed, according to the configuration.
%% Returns {AdmissibleList, AllList} where AdmissibleList contains only
%% admissible responses and AllList contains all responses.
serial_multirequest(_Nodes, 0, _Method, _Path, _Message, _Admissible, _Statuses, _Opts) -> {[], []};
serial_multirequest([], _, _Method, _Path, _Message, _Admissible, _Statuses, _Opts) -> {[], []};
serial_multirequest([Node|Nodes], Remaining, Method, Path, Message, Admissible, Statuses, Opts) ->
    {ErlStatus, Res} = hb_http:request(Method, Node, Path, Message, Opts),
    case is_admissible(ErlStatus, Res, Admissible, Statuses, Opts) of
        true ->
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
    {Workers, Queue} = start_workers(MaxWorkers, Ref, Nodes, Method, Path, Message, Opts),
    parallel_responses([], [], Workers, Queue, {Method, Path, Message}, Ref, Responses, StopAfter, Admissible, Statuses, Opts).

%% @doc Start a new fleet of workers, returning the list of worker PIDs.
start_workers(Count, Ref, Nodes, Method, Path, Message, Opts) ->
    Parent = self(),
    {NewWorkerNodes, NewRemainingNodes} =
        lists:split(min(Count, length(Nodes)), Nodes),
    {
        lists:map(
            fun(Node) ->
                spawn(
                    fun() ->
                        Res =
                            try hb_http:request(Method, Node, Path, Message, Opts)
                            catch C:R -> {error, {worker_crash, C, R}}
                            end,
                        receive no_reply -> stopping
                        after 0 -> Parent ! {Ref, self(), Res}
                        end
                    end
                )
            end,
            NewWorkerNodes
        ),
        NewRemainingNodes
    }.

%% @doc Check if a status is allowed, according to the configuration. Statuses
%% can be a single integer, a comma-separated list of integers, or the string
%% `All'.
admissible_status(_, <<"All">>) -> true;
admissible_status(_ResponseMsg = #{ <<"status">> := Status }, Statuses) ->
    admissible_status(Status, Statuses);
admissible_status(Status, Statuses) when is_integer(Statuses) ->
    admissible_status(Status, [Statuses]);
admissible_status(Status, Statuses) when is_binary(Status) ->
    admissible_status(binary_to_integer(Status), Statuses);
admissible_status(Status, Statuses) when is_binary(Statuses) ->
    % Convert the statuses to a list of integers.
    admissible_status(
        Status,
        lists:map(fun binary_to_integer/1, binary:split(Statuses, <<",">>))
    );
admissible_status(Status, Statuses) when is_list(Statuses) ->
    lists:member(Status, Statuses).

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
    lists:foreach(fun(P) -> exit(P, kill) end, Procs),
    empty_inbox(Ref),
    {AdmissibleRes, AllRes};
parallel_responses(AdmissibleRes, AllRes, Procs, Queue, {Method, Path, Message}, Ref, Awaiting, StopAfter, Admissible, Statuses, Opts) ->
    receive
        {Ref, Pid, {Status, NewRes}} ->
            WorkersWithoutPid = lists:delete(Pid, Procs),
            {RefilledWorkers, NewQueue} =
                start_workers(1, Ref, Queue, Method, Path, Message, Opts),
            NewProcs = RefilledWorkers ++ WorkersWithoutPid,
            NewAllRes = [{Status, NewRes} | AllRes],
            case is_admissible(Status, NewRes, Admissible, Statuses, Opts) of
                true ->
                    parallel_responses(
                        [{Status, NewRes} | AdmissibleRes],
                        NewAllRes,
                        NewProcs,
                        NewQueue,
                        {Method, Path, Message},
                        Ref,
                        Awaiting - 1,
                        StopAfter,
                        Admissible,
                        Statuses,
                        Opts
                );
            false ->
                parallel_responses(
                    AdmissibleRes,
                    NewAllRes,
                    NewProcs,
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
crash()  -> #{<<"opts">> => #{http_client => httpc}}.

ao_node(URL) ->
    #{<<"uri">> => <<URL/binary, "~meta@1.0/info">>,
      <<"opts">> => #{http_client => httpc}}.

dead_node() ->
    {ok, S} = gen_tcp:listen(0, []),
    {ok, Port} = inet:port(S),
    ok = gen_tcp:close(S),
    #{<<"uri">> => iolist_to_binary(["http://localhost:", integer_to_list(Port)]),
      <<"opts">> => #{http_client => httpc}}.

slow_node_opts(Ms) ->
    #{test_delay => Ms,
      on => #{<<"request">> =>
        #{<<"device">> => <<"test-device@1.0">>, <<"path">> => <<"delay">>}}}.

multi(Nodes, Extra) ->
    Config = Extra#{<<"nodes">> => Nodes, <<"admissible-status">> => 200},
    hb_http_multi:request(Config, <<"GET">>, <<"/">>, #{}, #{}).

multirequest_test_() ->
    {setup,
        fun() ->
            #{fast => good(), slow1 => slow(500), slow2 => slow(500),
              good1 => good(), good2 => good(), good3 => good()}
        end,
        fun(N) -> {timeout, 30, [
            {"serial fallback", fun() ->
                ?assertMatch({ok, _},
                    multi([dead_node(), maps:get(fast, N)], #{}))
            end},
            {"parallel race", fun() ->
                T0 = erlang:monotonic_time(millisecond),
                ?assertMatch({ok, _},
                    multi([maps:get(fast, N), maps:get(slow1, N), maps:get(slow2, N)],
                        #{<<"parallel">> => true, <<"stop-after">> => true})),
                ?assert(erlang:monotonic_time(millisecond) - T0 < 500)
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
                ?assertMatch({error, {no_viable_responses, _}},
                    multi([crash(), crash()],
                        #{<<"parallel">> => true, <<"stop-after">> => true}))
            end}
        ]} end}.

-endif.
