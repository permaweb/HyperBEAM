-module(http_proc_compare).
-export([match_process_responses/2, match_process_list_parallel/2]).

%% Default ports for the two services
-define(RESULT_PORT, "6363").
-define(PROCESS_PORT, "8734").


%% Compare responses for a list of processes with slot
match_process_responses(ProcIdList, Slot) when is_list(ProcIdList) ->
    application:start(inets),
    application:start(ssl),
    
    io:format("~n=== COMPARING ~p PROCESSES (SLOT ~p) ===~n", [length(ProcIdList), Slot]),
    Results = lists:map(fun(ProcId) ->
        io:format("~n--- Processing: ~p ---~n", [ProcId]),
        Result = match_responses_internal(ProcId, Slot),
        {ProcId, Result}
    end, ProcIdList),
    
    display_summary(Results),
    Results;

%% Main comparison function with transform
match_process_responses(ProcId, Slot) ->
    application:start(inets),
    application:start(ssl),
    
    ResultUrl = build_result_url(ProcId, Slot),
    ProcessUrl = build_process_url(ProcId, Slot),
    
    io:format("Making request to result endpoint: ~s~n", [ResultUrl]),
    io:format("Making request to process endpoint: ~s~n", [ProcessUrl]),
    
    ResultResponse = make_request(ResultUrl),
    ProcessResponse = make_request(ProcessUrl),
    
    tranform_and_compare(ResultResponse, ProcessResponse).

%% Build result URL (first endpoint)
build_result_url(ProcId, Slot) when is_binary(ProcId), is_integer(Slot) ->
    build_result_url(binary_to_list(ProcId), Slot);
build_result_url(ProcId, Slot) when is_list(ProcId), is_integer(Slot) ->
    "http://localhost:" ++ ?RESULT_PORT ++ "/result/" ++ 
    integer_to_list(Slot) ++ "?process-id=" ++ ProcId.

%% Build process URL (second endpoint)  
build_process_url(ProcId, Slot) when is_binary(ProcId), is_integer(Slot) ->
    build_process_url(binary_to_list(ProcId), Slot);
build_process_url(ProcId, Slot) when is_list(ProcId), is_integer(Slot) ->
    "http://localhost:" ++ ?PROCESS_PORT ++ "/" ++ ProcId ++ 
    "~process@1.0/compute&slot=" ++ integer_to_list(Slot) ++ 
    "?require-codec=application/json&accept-bundle=true".

%% Make HTTP request and return {ok, Body} or {error, Reason}
make_request(Url) ->
    RequestOptions = [
        {timeout, 10000},
        {connect_timeout, 5000}
    ],
    HttpOptions = [],
    
    case httpc:request(get, {Url, []}, RequestOptions, HttpOptions) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
            {ok, Body};
        {ok, {{_Version, StatusCode, ReasonPhrase}, _Headers, Body}} ->
            {error, {http_error, StatusCode, ReasonPhrase, Body}};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

%% Compare results with transformation
tranform_and_compare({ok, ResultBody}, {ok, ProcessBody}) ->
    try
        %% Parse JSON responses
        ResultJson = hb_json:decode(list_to_binary(ResultBody)),
        ProcessJson = hb_json:decode(list_to_binary(ProcessBody)),
        
        %% Transform the process endpoint response using transform_outbox
        TransformedJson = transform_to_messages(ProcessJson),
        
        io:format("~n=== TRANSFORMED PROCESS RESPONSE ===~n"),
        TransformedBody = hb_json:encode(TransformedJson),
        io:format("~s~n", [TransformedBody]),
        
        %% Compare original result with transformed process response
        io:format("~n=== COMPARISON (Result vs Transformed) ===~n"),
        case hb_json:encode(ResultJson) =:= TransformedBody of
            true ->
                io:format("✓ Result and transformed process responses are identical~n");
            false ->
                io:format("✗ Result and transformed process responses differ~n"),
                compare_json_structures(ResultJson, TransformedJson)
        end,
        
        {ok, {ResultJson, TransformedJson}}
        
    catch
        error:Error ->
            io:format("~n=== JSON PARSING ERROR ===~n"),
            io:format("Error: ~p~n", [Error]),
            {error, {json_parse_error, Error}}
    end;

tranform_and_compare({ok, _Body}, {error, Error}) ->
    io:format("~n=== PROCESS ENDPOINT ERROR ===~n"),
    io:format("Error: ~p~n", [Error]),
    {error, {process_endpoint_failed, Error}};

tranform_and_compare({error, Error}, {ok, _Body}) ->
    io:format("~n=== RESULT ENDPOINT ERROR ===~n"),
    io:format("Error: ~p~n", [Error]),
    {error, {result_endpoint_failed, Error}};

tranform_and_compare({error, Error1}, {error, Error2}) ->
    io:format("~n=== BOTH REQUESTS FAILED ===~n"),
    io:format("Result endpoint error: ~p~n", [Error1]),
    io:format("Process endpoint error: ~p~n", [Error2]),
    {error, {both_failed, Error1, Error2}}.

%% Compare JSON structures at a deeper level
compare_json_structures(Json1, Json2) ->
    Messages1 = maps:get(<<"Messages">>, Json1, []),
    Messages2 = maps:get(<<"Messages">>, Json2, []),
    
    io:format("Result Messages count: ~p~n", [length(Messages1)]),
    io:format("Transformed Messages count: ~p~n", [length(Messages2)]),
    
    case Messages1 =:= Messages2 of
        true ->
            io:format("✓ Messages arrays are identical~n");
        false ->
            io:format("✗ Messages arrays differ~n"),
            compare_messages_detail(Messages1, Messages2)
    end.

%% Compare messages in detail
compare_messages_detail(Messages1, Messages2) ->
    MaxLen = max(length(Messages1), length(Messages2)),
    lists:foreach(fun(Index) ->
        Msg1 = case Index =< length(Messages1) of
            true -> lists:nth(Index, Messages1);
            false -> undefined
        end,
        Msg2 = case Index =< length(Messages2) of
            true -> lists:nth(Index, Messages2);
            false -> undefined
        end,
        
        case {Msg1, Msg2} of
            {undefined, Msg} ->
                io:format("Message ~p: Only in transformed (Target: ~s)~n", 
                         [Index, maps:get(<<"Target">>, Msg, <<"unknown">>)]);
            {Msg, undefined} ->
                io:format("Message ~p: Only in result (Target: ~s)~n", 
                         [Index, maps:get(<<"Target">>, Msg, <<"unknown">>)]);
            {Msg, Msg} ->
                io:format("Message ~p: ✓ Identical~n", [Index]);
            {Msg1, Msg2} ->
                io:format("Message ~p: ✗ Different~n", [Index]),
                Target1 = maps:get(<<"Target">>, Msg1, <<"unknown">>),
                Target2 = maps:get(<<"Target">>, Msg2, <<"unknown">>),
                io:format("  Result Target: ~s~n", [Target1]),
                io:format("  Transformed Target: ~s~n", [Target2])
        end
    end, lists:seq(1, MaxLen)).


%% Compare responses for a list of processes in parallel with slot
match_process_list_parallel(ProcIdList, Slot) ->
    application:start(inets),
    application:start(ssl),
    
    io:format("~n=== COMPARING ~p PROCESSES (PARALLEL, SLOT ~p) ===~n", [length(ProcIdList), Slot]),
    
    Parent = self(),
    lists:foreach(fun(ProcId) ->
        spawn_link(fun() ->
            Result = match_responses_internal(ProcId, Slot),
            Parent ! {result, ProcId, Result}
        end)
    end, ProcIdList),
    
    Results = collect_results(ProcIdList, []),
    
    lists:foreach(fun({ProcId, Result}) ->
        io:format("~n--- Results for: ~p ---~n", [ProcId]),
        display_result(Result)
    end, Results),
    
    display_summary(Results),
    Results.

%% Internal structured compare with slot
match_responses_internal(ProcId, Slot) ->
    ResultUrl = build_result_url(ProcId, Slot),
    ProcessUrl = build_process_url(ProcId, Slot),
    
    ResultResponse = make_request(ResultUrl),
    ProcessResponse = make_request(ProcessUrl),
    
    case {ResultResponse, ProcessResponse} of
        {{ok, ResultBody}, {ok, ProcessBody}} ->
            try
                ResultJson = hb_json:decode(list_to_binary(ResultBody)),
                ProcessJson = hb_json:decode(list_to_binary(ProcessBody)),
                TransformedJson = transform_to_messages(ProcessJson),
                
                case ResultJson =:= TransformedJson of
                    true -> {identical, {ResultJson, TransformedJson}};
                    false -> {different, {ResultJson, TransformedJson}}
                end
            catch
                error:Error -> {error, {json_parse_error, Error}}
            end;
        {Result1, Result2} ->
            {error, {Result1, Result2}}
    end.

%% Display summary of results
display_summary(Results) ->
    io:format("~n=== SUMMARY ===~n"),
    lists:foreach(fun({ProcId, Result}) ->
        Status = case Result of
            {identical, _} -> "✓ IDENTICAL";
            {different, _} -> "✗ DIFFERENT";
            {error, _}     -> "⚠ ERROR"
        end,
        io:format("~p: ~ts~n", [ProcId, Status])
    end, Results).

%% Collect results from parallel comparisons
collect_results([], Acc) ->
    lists:reverse(Acc);
collect_results(Remaining, Acc) ->
    receive
        {result, ProcId, Result} ->
            NewRemaining = lists:delete(ProcId, Remaining),
            collect_results(NewRemaining, [{ProcId, Result} | Acc])
    after 30000 ->
        io:format("Timeout waiting for results from: ~p~n", [Remaining]),
        lists:reverse(Acc)
    end.

%% Display a single result
display_result({identical, {Json1, _}}) ->
    io:format("✓ Responses are identical after transformation~n"),
    Messages = maps:get(<<"Messages">>, Json1, []),
    io:format("Messages count: ~p~n", [length(Messages)]);

display_result({different, {Json1, Json2}}) ->
    io:format("✗ Responses differ after transformation~n"),
    Messages1 = maps:get(<<"Messages">>, Json1, []),
    Messages2 = maps:get(<<"Messages">>, Json2, []),
    io:format("Result Messages count: ~p~n", [length(Messages1)]),
    io:format("Transformed Messages count: ~p~n", [length(Messages2)]);

display_result({error, Error}) ->
    io:format("⚠ Comparison failed~n"),
    io:format("Error: ~p~n", [Error]).

%% Transform outbox format to messages format
transform_to_messages(JsonMap) ->
    Results = maps:get(<<"results">>, JsonMap),
    Outbox = maps:get(<<"outbox">>, Results),
    
    %% Convert outbox map to list and sort by keys for consistent ordering
    OutboxList = maps:to_list(Outbox),
    SortedOutbox = lists:sort(fun({K1, _}, {K2, _}) -> 
        binary_to_integer(K1) =< binary_to_integer(K2) 
    end, OutboxList),
    
    %% Transform each outbox entry to message format
    Messages = lists:map(fun({_Key, Entry}) ->
        transform_entry_to_message(Entry)
    end, SortedOutbox),
    
    %% Create the final structure matching the first document
    #{
        <<"Messages">> => Messages,
        <<"Assignments">> => [],
        <<"Spawns">> => [],
        <<"Output">> => maps:get(<<"Output">>, Results, #{}),
        <<"Patches">> => [],
        <<"GasUsed">> => maps:get(<<"GasUsed">>, Results, 0)
    }.

%% Transform individual outbox entry to message format
transform_entry_to_message(Entry) ->
    Target = maps:get(<<"Target">>, Entry),
    Anchor = maps:get(<<"Anchor">>, Entry),
    Tags = maps:get(<<"Tags">>, Entry, []),
    
    BaseMessage = #{
        <<"Target">> => Target,
        <<"Anchor">> => Anchor,
        <<"Tags">> => Tags
    },
    
    case maps:get(<<"Data">>, Entry, undefined) of
        undefined -> BaseMessage;
        Data -> maps:put(<<"Data">>, Data, BaseMessage)
    end.