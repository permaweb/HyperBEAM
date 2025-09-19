-module(transform_outbox).
-export([transform_to_messages/1]).

%% Transform outbox format to messages format for exact match
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
    %% Extract required fields
    Target = maps:get(<<"Target">>, Entry),
    Anchor = maps:get(<<"Anchor">>, Entry),
    Tags = maps:get(<<"Tags">>, Entry, []),
    
    %% Build base message
    BaseMessage = #{
        <<"Target">> => Target,
        <<"Anchor">> => Anchor,
        <<"Tags">> => Tags
    },
    
    %% Add Data field if present
    case maps:get(<<"Data">>, Entry, undefined) of
        undefined -> BaseMessage;
        Data -> maps:put(<<"Data">>, Data, BaseMessage)
    end.