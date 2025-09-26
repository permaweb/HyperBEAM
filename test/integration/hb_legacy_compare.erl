-module(hb_legacy_compare).
-export([compare_testnet/1, compare_with_network/2, compare_legacy_at_nonce_hb/3, fetch_latest_nonce/1, get_message_id/2]).

-define(NONCE_OVERRIDE, 0).
-define(LOCAL_LEGACY, false).
-define(LOCAL_MAINNET, true).

compare_testnet(ProcessId) ->
    compare_with_network(ProcessId, testnet).

compare_with_network(ProcessId, Network) ->
    LatestNonce = case fetch_latest_nonce(ProcessId) of
        {ok, Nonce} -> Nonce;
        {error, _} ->
            Msg = "No latest nonce found for " ++ ProcessId, 
            throw({error, Msg})
    end,
    
    FinalNonce = case ?NONCE_OVERRIDE of
        0 -> LatestNonce;
        Override -> Override
    end,
    io:format("Comparing process ~s until nonce ~p~n", [ProcessId, FinalNonce]),
    maybe 
        true ?= Network == testnet,
        false ?= filelib:is_dir("hb_mismatches/" ++ ProcessId ++ "/testnet"),
        TestnetMismatches = compare_nonces(ProcessId, FinalNonce, testnet),
        save_mismatches(ProcessId, FinalNonce, TestnetMismatches, testnet)
    end,
    maybe
        true ?= Network =/= testnet,
        false ?= filelib:is_dir("hb_mismatches/" ++ ProcessId ++ "/whitezone"),
        WhitezoneMismatches = compare_nonces(ProcessId, FinalNonce, Network), 
        save_mismatches(ProcessId, FinalNonce, WhitezoneMismatches, whitezone)
    end.

compare_nonces(ProcessId, LatestNonce, Target) ->
    Nonces = lists:reverse(lists:seq(1, LatestNonce)),
    Result = 
        lists:foldl(fun(Nonce, Res) ->
            maybe undefined ?= Res,
                case compare_legacy_at_nonce_hb(ProcessId, Nonce, Target) of
                    #{mismatches := Map} = Res when map_size(Map) > 0 ->
                        #{Nonce => Res};
                    _Matches ->
                        undefined
                end
            end
        end, undefined, Nonces),
    maybe 
        undefined ?= Result,
        #{}
    end.

compare_legacy_at_nonce_hb(ProcessId, Nonce, testnet) ->
    io:format("Comparing legacy and hyperbeam results on process ~s at nonce ~p~n", 
                     [ProcessId, Nonce]),
    MessageId = get_message_id(ProcessId, Nonce),
    io:format("Got message id ~s~n", [MessageId]),
    
    TestnetResult = get_testnet_result(MessageId, ProcessId, 1),
    MainnetResult = get_mainnet_result(ProcessId, Nonce, 1),
    
    io:format("Fetched testnet result~n"),
    io:format("Fetched mainnet result~n"),
    io:format("Comparing results...~n"),
    
    Mismatches = deep_compare(TestnetResult, MainnetResult, ""),
    #{mismatches => Mismatches, testnet_result => TestnetResult,
    mainnet_result => MainnetResult, message_id => MessageId};

compare_legacy_at_nonce_hb(ProcessId, Nonce, WhiteZone) ->
    io:format("Whitezone detected, getting whitezone result...~n"),
    MessageId = get_message_id(ProcessId, Nonce),
    WhitezoneResult = get_whitezone_result(MessageId, ProcessId, WhiteZone),
    MainnetResult = get_mainnet_result_production(ProcessId, Nonce),
    
    io:format("Fetched whitezone result~n"),
    io:format("Fetched mainnet result~n"),
    io:format("Comparing results...~n"),
    
    Mismatches = deep_compare(WhitezoneResult, MainnetResult, ""),
    #{mismatches => Mismatches, whitezone_result => WhitezoneResult,
        mainnet_result => MainnetResult, message_id => MessageId}.

fetch_latest_nonce(ProcessId) ->
    Url = "https://su-router.ao-testnet.xyz/" ++ ProcessId ++ "/latest",
    io:format("Fetching latest nonce... ~s~n", [Url]),
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            try
                Json = hb_json:decode(list_to_binary(Body)),
                Assignment = maps:get(<<"assignment">>, Json),
                Tags = maps:get(<<"tags">>, Assignment),
                NonceTag = lists:foldl(fun(Tag, Acc) ->
                    case maps:get(<<"name">>, Tag) of
                        <<"Nonce">> -> maps:get(<<"value">>, Tag);
                        _ -> Acc
                    end
                end, null, Tags),
                case NonceTag of
                    null -> {error, no_nonce};
                    Nonce -> {ok, binary_to_integer(Nonce)}
                end
            catch
                _:_ -> {error, parse_error}
            end;
        _ -> {error, request_failed}
    end.

get_message_id(ProcessId, Nonce) ->
    io:format("Getting message id at nonce ~p...~n", [Nonce]),
    SuUrl = "https://su-router.ao-testnet.xyz/" ++ ProcessId ++ 
            "?from-nonce=" ++ integer_to_list(Nonce - 1) ++ "&limit=1",
    io:format("Getting message id at nonce ~p... ~s~n", [Nonce, SuUrl]),
    
    case httpc:request(get, {SuUrl, []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            Json = hb_json:decode(list_to_binary(Body)),
            Edges = maps:get(<<"edges">>, Json),
            [FirstEdge | _] = Edges,
            Node = maps:get(<<"node">>, FirstEdge),
            Message = maps:get(<<"message">>, Node),
            binary_to_list(maps:get(<<"id">>, Message));
        _ -> throw({error, "Failed to get message id"})
    end.

get_testnet_result(MessageId, ProcessId, Attempt) ->
    TestnetBase = case ?LOCAL_LEGACY of
        true -> "http://localhost:6364";
        false -> "https://cu.ao-testnet.xyz"
    end,
    TestnetUrl = TestnetBase ++ "/result/" ++ MessageId ++ "?process-id=" ++ ProcessId,
    io:format("Fetching testnet result... ~s~n", [TestnetUrl]),
    
    case httpc:request(get, {TestnetUrl, []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            hb_json:decode(list_to_binary(Body));
        {ok, {{_, Status, _}, _, Body}} when Status == 425 orelse Status == 503->
            hb_json:decode(list_to_binary(Body));
        _Error when Attempt < 3 ->
            get_testnet_result(MessageId, ProcessId, Attempt + 1);
        _ -> 
            throw({error, "Failed to fetch testnet result"})
    end.

get_mainnet_result(ProcessId, Nonce, Attempt) ->
    MainnetBase = case ?LOCAL_MAINNET of
        true -> "http://localhost:8734";
        false -> "https://tee-1.forward.computer"
    end,
    MainnetUrl = MainnetBase ++ "/" ++ ProcessId ++ "~process@1.0/compute&slot=" ++ 
                 integer_to_list(Nonce) ++ "/results/json",
    io:format("Fetching mainnet result... ~s~n", [MainnetUrl]),
    
    case httpc:request(get, {MainnetUrl, []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            hb_json:decode(list_to_binary(Body));
        _Error when Attempt < 3 ->
            get_mainnet_result(ProcessId, Nonce, Attempt + 1);
        _ -> 
            throw({error, "Failed to fetch mainnet result"})
    end.

get_whitezone_result(MessageId, ProcessId, WhiteZone) ->
    WhitezoneUrl = WhiteZone ++ "/result/" ++ MessageId ++ "?process-id=" ++ ProcessId,
    case httpc:request(get, {WhitezoneUrl, []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            hb_json:decode(list_to_binary(Body));
        _ -> throw({error, "Failed to fetch whitezone result"})
    end.

get_mainnet_result_production(ProcessId, Nonce) ->
    MainnetUrl = "https://tee-1.forward.computer/" ++ ProcessId ++ 
                 "~process@1.0/compute&slot=" ++ integer_to_list(Nonce) ++ "/results/json",
    io:format("Fetching mainnet result... ~s~n", [MainnetUrl]),
    
    case httpc:request(get, {MainnetUrl, []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            hb_json:decode(list_to_binary(Body));
        _ -> throw({error, "Failed to fetch mainnet result"})
    end.

deep_compare(A, B, _Path) when A =:= B -> 
    #{};

deep_compare(A, B, Path) ->
    case {A, B} of
        {null, null} -> #{};
        {null, _} -> #{Path => "one is null, the other is not"};
        {_, null} -> #{Path => "one is null, the other is not"};
        _ when is_list(A) andalso is_list(B) ->
            compare_arrays(A, B, Path);
        _ when is_list(A) andalso not is_list(B) ->
            #{Path => "one is array, other is not"};
        _ when not is_list(A) andalso is_list(B) ->
            #{Path => "one is array, other is not"};
        _ when is_map(A) andalso is_map(B) ->
            compare_maps(A, B, Path);
        _ when is_map(A) andalso not is_map(B) ->
            #{Path => "type mismatch (map vs " ++ atom_to_list(type_of(B)) ++ ")"};
        _ when not is_map(A) andalso is_map(B) ->
            #{Path => "type mismatch (" ++ atom_to_list(type_of(A)) ++ " vs map)"};
        _ ->
            compare_primitives(A, B, Path)
    end.

compare_arrays(A, B, Path) ->
    Mismatches = case length(A) =/= length(B) of
        true -> #{Path => io_lib:format("array length mismatch (~p vs ~p)", 
                                       [length(A), length(B)])};
        false -> #{}
    end,
    
    case string:str(Path, "Tags") > 0 of
        true -> maps:merge(Mismatches, compare_tags(A, B, Path));
        false -> maps:merge(Mismatches, compare_array_elements(A, B, Path, 0))
    end.

compare_tags(A, B, Path) ->
    lists:foldl(fun({I, ATag}, Acc) ->
        ATagName = maps:get(<<"name">>, ATag),
        ATagValue = maps:get(<<"value">>, ATag),
        
        BTagValues = [maps:get(<<"value">>, BTag) 
                     || BTag <- B, maps:get(<<"name">>, BTag) =:= ATagName],
        
        ParsedBTagValues = [case is_json_string(V) of
            true -> hb_json:decode(V);
            false -> V
        end || V <- BTagValues],
        
        case is_json_string(ATagValue) of
            true ->
                ParsedATagValue = hb_json:decode(ATagValue),
                Match = lists:any(fun(BTagValue) ->
                    case {is_map(BTagValue), is_map(ParsedATagValue)} of
                        {true, true} -> 
                            maps:size(deep_compare(ParsedATagValue, BTagValue, "")) =:= 0;
                        _ -> false
                    end
                end, ParsedBTagValues),
                case Match of
                    false -> 
                        Key = Path ++ "[" ++ integer_to_list(I) ++ "]",
                        Value = io_lib:format("value mismatch (~s vs ~s)", 
                                            [binary_to_list(ATagValue), 
                                             string:join([binary_to_list(V) || V <- BTagValues], ", ")]),
                        maps:put(Key, Value, Acc);
                    true -> Acc
                end;
            false ->
                case lists:member(ATagValue, BTagValues) of
                    false ->
                        Key = Path ++ "[" ++ integer_to_list(I) ++ "]",
                        Value = io_lib:format("value mismatch (~s vs ~s)", 
                                            [binary_to_list(ATagValue),
                                             string:join([binary_to_list(V) || V <- BTagValues], ", ")]),
                        maps:put(Key, Value, Acc);
                    true -> Acc
                end
        end
    end, #{}, lists:zip(lists:seq(0, length(A) - 1), A)).

compare_array_elements([], [], _, _) -> #{};
compare_array_elements([H1|T1], [H2|T2], Path, Index) ->
    NewPath = Path ++ "[" ++ integer_to_list(Index) ++ "]",
    HeadMismatches = deep_compare(H1, H2, NewPath),
    TailMismatches = compare_array_elements(T1, T2, Path, Index + 1),
    case maps:size(HeadMismatches) > 0 of
        true -> maps:merge(#{NewPath => HeadMismatches}, TailMismatches);
        false -> TailMismatches
    end;
compare_array_elements([_H1|T1], [], Path, Index) ->
    NewPath = Path ++ "[" ++ integer_to_list(Index) ++ "]",
    maps:put(NewPath, "missing in second array", 
             compare_array_elements(T1, [], Path, Index + 1));
compare_array_elements([], [_H2|T2], Path, Index) ->
    NewPath = Path ++ "[" ++ integer_to_list(Index) ++ "]",
    maps:put(NewPath, "missing in first array",
             compare_array_elements([], T2, Path, Index + 1)).

compare_maps(A, B, Path) ->
    AKeys = maps:keys(A),
    BKeys = maps:keys(B),
    AllKeys = lists:usort(AKeys ++ BKeys),
    
    lists:foldl(fun(Key, Acc) ->
        NewPath = case Path of
            "" -> binary_to_list(Key);
            _ -> Path ++ "." ++ binary_to_list(Key)
        end,
        
        case {maps:is_key(Key, A), maps:is_key(Key, B)} of
            {false, true} ->
                case Key of
                    <<"Patches">> -> Acc;  % Skip Patches key
                    _ -> maps:put(NewPath, "missing in first object", Acc)
                end;
            {true, false} ->
                maps:put(NewPath, "missing in second object", Acc);
            {true, true} ->
                AValue = maps:get(Key, A),
                BValue = maps:get(Key, B),
                Comp = deep_compare(AValue, BValue, NewPath),
                case maps:size(Comp) > 0 of
                    true -> maps:put(NewPath, Comp, Acc);
                    false -> Acc
                end;
            {false, false} -> Acc
        end
    end, #{}, AllKeys).

compare_primitives(A, B, Path) ->
    case A =:= B of
        true -> #{};
        false ->
            try
                AJson = hb_json:decode(ensure_binary(A)),
                BJson = hb_json:decode(ensure_binary(B)),
                deep_compare(AJson, BJson, Path)
            catch
                _:_ -> #{Path => "value mismatch"}
            end
    end.

save_mismatches(ProcessId, LatestNonce, Mismatches, Type) ->
    Timestamp = erlang:system_time(millisecond),
    TypeStr = atom_to_list(Type),
    Dir = "hb_mismatches/" ++ ProcessId ++ "/" ++ TypeStr,
    filelib:ensure_dir(Dir ++ "/"),
    case map_size(Mismatches) > 0 of
        true ->
            Filename = Dir ++ "/nonce-" ++ integer_to_list(LatestNonce) ++ 
                    "-" ++ integer_to_list(Timestamp) ++ ".json",
            JsonData = hb_json:encode(Mismatches),
            file:write_file(Filename, JsonData);
        false ->
            Filename = Dir ++ "/no_mismatch",
            file:write_file(Filename, <<"ok">>)
    end.

is_json_string(Str) when is_binary(Str) ->
    case catch binary_to_integer(Str) of
        Int when is_integer(Int) -> false;
        _ ->
            try
                hb_json:decode(Str),
                true
            catch
                _:_ -> false
            end
    end;
is_json_string(_) -> false.

ensure_binary(Val) when is_binary(Val) -> Val;
ensure_binary(Val) when is_list(Val) -> list_to_binary(Val);
ensure_binary(Val) -> list_to_binary(io_lib:format("~p", [Val])).

type_of(Val) when is_integer(Val) -> integer;
type_of(Val) when is_float(Val) -> float;
type_of(Val) when is_binary(Val) -> binary;
type_of(Val) when is_list(Val) -> list;
type_of(Val) when is_atom(Val) -> atom;
type_of(Val) when is_boolean(Val) -> boolean;
type_of(_) -> unknown.