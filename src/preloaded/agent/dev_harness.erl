%%% @doc Generic agent harness for HyperBEAM — `harness@1.0`.
%%%
%%% Orchestrates `llm@1.0` + `relay@1.0` + `store` (HB cache FS) + `query@1.0`
%%% + `lua@5.3a` bash-like env. Generic: fetch any URL via `relay@1.0`,
%%% parse (RSS/JSON), store under `<collection>-<id>` + `<collection>-index`,
%%% then query. `dan-feed` is just one test dataset (collection=`dan`).
-module(dev_harness).
-implements(<<"harness@1.0">>).
-export([info/1, fetch/3, parse/3, store/3, ingest/3, query/3, list/3]).
-export([info/3, handle/3, run/3, chat/3, execute/3]).
-export([fetch_feed/1, parse_feed/1, fetch_via_relay/2]).

-include_lib("eunit/include/eunit.hrl").

info(_) ->
    #{
        <<"fetch">> => dev,
        <<"parse">> => dev,
        <<"store">> => dev,
        <<"ingest">> => dev,
        <<"query">> => dev,
        <<"list">> => dev,
        <<"handle">> => dev,
        <<"run">> => dev,
        <<"chat">> => dev,
        <<"execute">> => dev
    }.
info(_, _, _) -> {ok, info(#{})}.

%% Fetch any URL via relay@1.0 (public) — dan-feed is just default test dataset
fetch(Base, Req, Opts) ->
    URL = hb_ao:get(<<"url">>, Req, hb_ao:get(<<"url">>, Base, <<"https://hyperio-mc.github.io/dan-feed/feed.xml">>, Opts), Opts),
    fetch_via_relay(URL, Opts).
fetch_feed(URL) -> fetch_via_relay(URL, #{}).
fetch_feed(URL, Opts) -> fetch_via_relay(URL, Opts).

fetch_via_relay(URL, Opts) ->
    % Use relay@1.0/call so external tool/service fetches go through relay device
    % (hb_http direct would also work for public hosts, but relay is the intended tool gateway)
    case hb_ao:resolve(#{<<"device">> => <<"relay@1.0">>, <<"path">> => <<"call">>, <<"relay-path">> => URL, <<"relay-method">> => <<"GET">>}, Opts) of
        {ok, Res} when is_map(Res) -> {ok, maps:get(<<"body">>, Res, maps:get(<<"Body">>, Res, <<>>))};
        {ok, Bin} when is_binary(Bin) -> {ok, Bin};
        Err -> Err
    end.

%% Parse RSS XML → list of post maps
parse(Base, Req, Opts) ->
    Body = hb_ao:get(<<"body">>, Req, hb_ao:get(<<"body">>, Base, <<>>, Opts), Opts),
    case Body of
        <<>> -> fetch_and_parse(Opts);
        _ -> {ok, parse_feed(Body)}
    end.
fetch_and_parse(Opts) ->
    case fetch_feed(<<"https://hyperio-mc.github.io/dan-feed/feed.xml">>, Opts) of
        {ok, Body} -> {ok, parse_feed(Body)};
        Err -> Err
    end.

parse_feed(Body) when is_binary(Body) ->
    % Very simple RSS item extraction via regex — no xmerl dep
    Items = case re:run(Body, <<"<item>(.*?)</item>">>, [global, dotall, {capture, [1], binary}]) of
        {match, Matches} -> [ hd(M) || M <- Matches ];
        nomatch -> []
    end,
    [parse_item(Item) || Item <- Items].

parse_item(Item) ->
    G = extract_tag(Item, <<"guid">>),
    #{
        <<"guid">> => G,
        <<"id">> => G,
        <<"title">> => extract_tag(Item, <<"title">>),
        <<"link">> => extract_tag(Item, <<"link">>),
        <<"description">> => extract_tag(Item, <<"description">>),
        <<"pubDate">> => extract_tag(Item, <<"pubDate">>),
        <<"source">> => extract_source(Item)
    }.

extract_tag(Bin, Tag) ->
    Pat = <<"<", Tag/binary, "[^>]*>(.*?)</", Tag/binary, ">">>,
    case re:run(Bin, Pat, [dotall, {capture, [1], binary}]) of
        {match, [V]} -> unescape(V);
        nomatch -> <<>>
    end.

extract_source(Item) ->
    case re:run(Item, <<"<source[^>]*>(.*?)</source>">>, [dotall, {capture, [1], binary}]) of
        {match, [V]} -> unescape(V);
        nomatch -> <<>>
    end.

unescape(Bin) ->
    B1 = binary:replace(Bin, <<"&amp;">>, <<"&">>, [global]),
    B2 = binary:replace(B1, <<"&lt;">>, <<"<">>, [global]),
    B3 = binary:replace(B2, <<"&gt;">>, <<">">>, [global]),
    B4 = binary:replace(B3, <<"&apos;">>, <<"'">>, [global]),
    binary:replace(B4, <<"&quot;">>, <<"\"">>, [global]).

%% Generic store: collection-<id> + collection-index (default collection=dan for demo)
store(Base, Req, Opts) ->
    Posts = case hb_ao:get(<<"posts">>, Req, not_found, Opts) of
        not_found -> case hb_ao:get(<<"posts">>, Base, not_found, Opts) of
            not_found -> case hb_ao:get(<<"items">>, Req, not_found, Opts) of
                not_found -> parse_feed(hb_ao:get(<<"body">>, Req, <<>>, Opts));
                I -> I
            end;
            P -> P
        end;
        P -> P
    end,
    PostsList = case is_list(Posts) of true -> Posts; false -> [Posts] end,
    Collection = hb_ao:get(<<"collection">>, Req, hb_ao:get(<<"collection">>, Base, <<"dan">>, Opts), Opts),
    Store = hb_opts:get(store, no_viable, Opts),
    lists:foreach(fun(P) ->
        Id = maps:get(<<"guid">>, P, maps:get(<<"id">>, P, hb_util:bin(rand:uniform(1000000)))),
        Key = <<Collection/binary, "-", Id/binary>>,
        hb_store:write(Store, #{Key => hb_json:encode(P)}, Opts)
    end, PostsList),
    IdxKey = <<Collection/binary, "-index">>,
    ExistingIdx = case hb_store:read(Store, IdxKey, Opts) of {ok, Bin} -> try hb_json:decode(Bin) catch _:_ -> [] end; _ -> [] end,
    NewIds = [maps:get(<<"guid">>, P, maps:get(<<"id">>, P, <<>>)) || P <- PostsList],
    MergedIdx = lists:usort(ExistingIdx ++ NewIds),
    hb_store:write(Store, #{IdxKey => hb_json:encode(MergedIdx)}, Opts),
    {ok, #{<<"stored">> => length(PostsList), <<"keys">> => NewIds, <<"total">> => length(MergedIdx), <<"collection">> => Collection}}.

%% Generic ingest: fetch any URL (via relay) + parse + store to collection
ingest(Base, Req, Opts) ->
    URL = hb_ao:get(<<"url">>, Req, hb_ao:get(<<"url">>, Base, <<"https://hyperio-mc.github.io/dan-feed/feed.xml">>, Opts), Opts),
    Collection = hb_ao:get(<<"collection">>, Req, hb_ao:get(<<"collection">>, Base, <<"dan">>, Opts), Opts),
    case fetch_via_relay(URL, Opts) of
        {ok, Body} ->
            Posts = parse_feed(Body),
            % If parse yields no RSS items, try JSON array fallback
            Items = case Posts of [] -> try hb_json:decode(Body) catch _:_ -> [] end; _ -> Posts end,
            ItemsList = case is_list(Items) of true -> Items; false -> [Items] end,
            Store = hb_opts:get(store, no_viable, Opts),
            lists:foreach(fun(P) ->
                M = case is_map(P) of true -> P; false -> #{<<"data">> => P} end,
                Id = maps:get(<<"guid">>, M, maps:get(<<"id">>, M, hb_util:bin(rand:uniform(1000000)))),
                Key = <<Collection/binary, "-", Id/binary>>,
                hb_store:write(Store, #{Key => hb_json:encode(M)}, Opts)
            end, ItemsList),
            IdxKey = <<Collection/binary, "-index">>,
            ExistingIdx = case hb_store:read(Store, IdxKey, Opts) of {ok, Bin} -> try hb_json:decode(Bin) catch _:_ -> [] end; _ -> [] end,
            NewIds = [case is_map(P) of true -> maps:get(<<"guid">>, P, maps:get(<<"id">>, P, <<>>)); false -> <<>> end || P <- ItemsList],
            MergedIdx = lists:usort(ExistingIdx ++ NewIds),
            hb_store:write(Store, #{IdxKey => hb_json:encode(MergedIdx)}, Opts),
            {ok, #{<<"ingested">> => length(ItemsList), <<"total">> => length(MergedIdx), <<"collection">> => Collection, <<"posts">> => ItemsList}};
        Err -> Err
    end.

%% List: generic collection-index
list(Base, Req, Opts) ->
    Collection = hb_ao:get(<<"collection">>, Req, hb_ao:get(<<"collection">>, Base, <<"dan">>, Opts), Opts),
    IdxKey = <<Collection/binary, "-index">>,
    Store = hb_opts:get(store, no_viable, Opts),
    case hb_store:read(Store, IdxKey, Opts) of
        {ok, Bin} ->
            Keys = try hb_json:decode(Bin) catch _:_ -> [] end,
            {ok, #{<<"keys">> => Keys, <<"count">> => length(Keys), <<"collection">> => Collection}};
        Err -> Err
    end.

%% Query: generic collection-index + per-item reads + q filter
query(Base, Req, Opts) ->
    Q = hb_ao:get(<<"q">>, Req, hb_ao:get(<<"query">>, Base, <<>>, Opts), Opts),
    Collection = hb_ao:get(<<"collection">>, Req, hb_ao:get(<<"collection">>, Base, <<"dan">>, Opts), Opts),
    IdxKey = <<Collection/binary, "-index">>,
    Store = hb_opts:get(store, no_viable, Opts),
    case hb_store:read(Store, IdxKey, Opts) of
        {ok, Bin} ->
            Keys = try hb_json:decode(Bin) catch _:_ -> [] end,
            Posts = [case hb_store:read(Store, <<Collection/binary, "-", K/binary>>, Opts) of {ok, B} -> try hb_json:decode(B) catch _:_ -> #{} end; _ -> #{} end || K <- Keys],
            Filtered = case Q of
                <<>> -> Posts;
                _ -> [P || P <- Posts, matches_query(P, Q)]
            end,
            {ok, #{<<"results">> => Filtered, <<"count">> => length(Filtered), <<"q">> => Q, <<"collection">> => Collection}};
        Err -> Err
    end.

matches_query(Post, Q) ->
    QLow = string:lowercase(Q),
    Fields = [maps:get(<<"title">>, Post, <<>>), maps:get(<<"description">>, Post, <<>>), maps:get(<<"link">>, Post, <<>>)],
    lists:any(fun(F) -> binary:match(string:lowercase(F), QLow) =/= nomatch end, Fields).

%% ===================================================================
%% Agent harness: message + history + tools -> llm loop -> tool dispatch
%% Implements: take message, build context (last-turn history + tools),
%% call llm@1.0, if tool_calls then execute via relay@1.0 (or requested
%% device) and append to context, repeat until no tools, return output
%% and persist updated context to hb_store.
%% ===================================================================

handle(Base, Req, Opts) -> do_harness(Base, Req, Opts).
run(Base, Req, Opts) -> do_harness(Base, Req, Opts).
chat(Base, Req, Opts) -> do_harness(Base, Req, Opts).
execute(Base, Req, Opts) -> do_harness(Base, Req, Opts).

do_harness(Base, Req, Opts) ->
    Message = get_harness_message(Base, Req, Opts),
    ToolsRaw0 = hb_ao:get(<<"tools">>, Req, hb_ao:get(<<"tools">>, Base, not_found, Opts), Opts),
    ToolsRaw = maybe_deref(ToolsRaw0, Opts),
    Tools = normalize_tools(ToolsRaw),
    Collection = hb_ao:get(<<"collection">>, Req, hb_ao:get(<<"collection">>, Base, <<"default">>, Opts), Opts),
    CollectionBin = hb_util:bin(Collection),
    HistoryKey = hb_ao:get(<<"history-key">>, Req, hb_ao:get(<<"history-key">>, Base, <<CollectionBin/binary, "-harness-history">>, Opts), Opts),
    Store = hb_opts:get(store, no_viable, Opts),
    HistoryRaw = load_harness_history(Base, Req, Opts, HistoryKey, Store),
    % Harness manages window limit: truncate previous history up to limit
    HistoryLimit = get_history_limit(Base, Req, Opts),
    History0 = truncate_history(HistoryRaw, HistoryLimit, Opts),
    % Build system instructions (identity.md + soul.md + user.md) as essay flow: system + identity/tools/history/current
    SystemMsg = build_system_message(Base, Req, Opts),
    Messages0WithoutSystem = case Message of
        <<>> -> History0;
        undefined -> History0;
        not_found -> History0;
        _ -> History0 ++ [#{<<"role">> => <<"user">>, <<"content">> => hb_util:bin(Message)}]
    end,
    % If caller supplied explicit messages list, prefer it (already contains history)
    ExplicitMessages0 = hb_ao:get(<<"messages">>, Req, hb_ao:get(<<"messages">>, Base, not_found, Opts), Opts),
    ExplicitMessages = maybe_deref(ExplicitMessages0, Opts),
    MessagesInitWithoutSystem = case ExplicitMessages of
        not_found -> Messages0WithoutSystem;
        M when is_list(M) -> [maybe_deref(X, Opts) || X <- M];
        M when is_binary(M) -> try hb_json:decode(M) catch _:_ -> Messages0WithoutSystem end;
        _ -> Messages0WithoutSystem
    end,
    MaxIters = get_max_iters(Base, Req, Opts),
    Model = hb_ao:get(<<"model">>, Req, hb_ao:get(<<"model">>, Base, <<"qwen3.6">>, Opts), Opts),
    Endpoint = hb_ao:get(<<"endpoint">>, Req, hb_ao:get(<<"endpoint">>, Base, hb_ao:get(<<"llm-endpoint">>, Base, not_found, Opts), Opts), Opts),
    EndpointNorm = case Endpoint of not_found -> undefined; _ -> Endpoint end,
    case MessagesInitWithoutSystem of
        [] when Tools =/= undefined ->
            harness_loop(MessagesInitWithoutSystem, SystemMsg, Tools, Model, EndpointNorm, MaxIters, Opts, HistoryKey, Store, 0, History0);
        [] ->
            {ok, #{<<"output">> => <<>>, <<"history">> => History0, <<"messages">> => History0, <<"iterations">> => 0}};
        _ ->
            harness_loop(MessagesInitWithoutSystem, SystemMsg, Tools, Model, EndpointNorm, MaxIters, Opts, HistoryKey, Store, 0, History0)
    end.

get_harness_message(Base, Req, Opts) ->
    Keys = [<<"message">>, <<"prompt">>, <<"data">>, <<"input">>, <<"content">>],
    Raw = get_first_key(Keys, [Req, Base], Opts),
    maybe_deref(Raw, Opts).

get_first_key([], _, _) -> not_found;
get_first_key([K|Rest], Maps, Opts) ->
    case hb_ao:get(K, hd(Maps), not_found, Opts) of
        not_found ->
            case length(Maps) of
                1 -> get_first_key(Rest, Maps, Opts);
                _ -> case hb_ao:get(K, lists:nth(2, Maps), not_found, Opts) of
                        not_found -> get_first_key(Rest, Maps, Opts);
                        V -> V
                     end
            end;
        V -> V
    end.

maybe_deref(not_found, _) -> not_found;
maybe_deref(undefined, _) -> undefined;
maybe_deref(V, Opts) ->
    try hb_cache:ensure_loaded(V, Opts) catch _:_ -> V end.

normalize_tools(not_found) -> undefined;
normalize_tools(undefined) -> undefined;
normalize_tools(T) when is_binary(T) ->
    try hb_json:decode(T) catch _:_ -> undefined end;
normalize_tools(T) when is_list(T) ->
    % Deref each tool if it's a link
    [case maybe_deref(Tool, #{}) of M when is_map(M) -> M; Other -> Other end || Tool <- T];
normalize_tools(T) when is_map(T) -> [maybe_deref(T, #{})];
normalize_tools(V) ->
    % Try to deref if it's a link
    Deref = maybe_deref(V, #{}),
    case Deref of
        T when is_list(T) -> T;
        T when is_map(T) -> [T];
        _ -> undefined
    end.

get_max_iters(Base, Req, Opts) ->
    V = hb_ao:get(<<"max-iterations">>, Req, hb_ao:get(<<"max_iterations">>, Req, hb_ao:get(<<"max-iterations">>, Base, 10, Opts), Opts), Opts),
    case V of
        N when is_integer(N) -> N;
        B when is_binary(B) -> try binary_to_integer(B) catch _:_ -> 10 end;
        _ -> 10
    end.

get_history_limit(Base, Req, Opts) ->
    V0 = hb_ao:get(<<"history_limit">>, Req, hb_ao:get(<<"history-limit">>, Req, hb_ao:get(<<"max_history">>, Req, hb_ao:get(<<"max-history">>, Req, hb_ao:get(<<"history_limit">>, Base, not_found, Opts), Opts), Opts), Opts), Opts),
    V = maybe_deref(V0, Opts),
    case V of
        not_found -> 20;
        undefined -> 20;
        N when is_integer(N) -> N;
        B when is_binary(B) -> try binary_to_integer(B) catch _:_ -> 20 end;
        _ -> 20
    end.

truncate_history(History, Limit, _Opts) when not is_list(History) -> History;
truncate_history(History, Limit, _Opts) when Limit =:= undefined; Limit =:= 0; Limit =:= not_found -> History;
truncate_history(History, Limit, _Opts) ->
    Len = length(History),
    if Len =< Limit -> History;
       true -> lists:nthtail(Len - Limit, History)
    end.

build_system_message(Base, Req, Opts) ->
    % Essay flow: System instructions + Agent identity (identity.md + soul.md + user.md) + tools already separate
    System0 = hb_ao:get(<<"system">>, Req, hb_ao:get(<<"system">>, Base, not_found, Opts), Opts),
    SystemDeref = maybe_deref(System0, Opts),
    SystemBin = case SystemDeref of not_found -> undefined; undefined -> undefined; B when is_binary(B) -> B; M when is_map(M) -> hb_json:encode(M); L when is_list(L) -> hb_util:bin(L); _ -> undefined end,
    Identity0 = hb_ao:get(<<"identity">>, Req, hb_ao:get(<<"identity">>, Base, not_found, Opts), Opts),
    Soul0 = hb_ao:get(<<"soul">>, Req, hb_ao:get(<<"soul">>, Base, not_found, Opts), Opts),
    User0 = hb_ao:get(<<"user">>, Req, hb_ao:get(<<"user">>, Base, not_found, Opts), Opts),
    Instructions0 = hb_ao:get(<<"instructions">>, Req, hb_ao:get(<<"instructions">>, Base, not_found, Opts), Opts),
    Parts = [
        case maybe_deref(Identity0, Opts) of not_found -> undefined; undefined -> undefined; V -> hb_util:bin(V) end,
        case maybe_deref(Soul0, Opts) of not_found -> undefined; undefined -> undefined; V2 -> hb_util:bin(V2) end,
        case maybe_deref(User0, Opts) of not_found -> undefined; undefined -> undefined; V3 -> hb_util:bin(V3) end,
        case maybe_deref(Instructions0, Opts) of not_found -> undefined; undefined -> undefined; V4 -> hb_util:bin(V4) end,
        SystemBin
    ],
    Filtered = [P || P <- Parts, P =/= undefined, P =/= <<>>, P =/= not_found],
    case Filtered of
        [] -> undefined;
        _ ->
            Combined = iolist_to_binary(lists:join(<<"\n\n">>, Filtered)),
            #{<<"role">> => <<"system">>, <<"content">> => Combined}
    end.

load_harness_history(Base, Req, Opts, HistoryKey, Store) ->
    % 1) explicit history param
    Explicit0 = hb_ao:get(<<"history">>, Req, hb_ao:get(<<"history">>, Base, not_found, Opts), Opts),
    Explicit = maybe_deref(Explicit0, Opts),
    case Explicit of
        not_found ->
            % 2) load from store
            case Store of
                no_viable -> [];
                _ ->
                    case hb_store:read(Store, HistoryKey, Opts) of
                        {ok, Bin} when is_binary(Bin) ->
                            try
                                Dec = hb_json:decode(Bin),
                                case Dec of L when is_list(L) -> [maybe_deref(X, Opts) || X <- L]; M when is_map(M) -> [maybe_deref(M, Opts)]; _ -> [] end
                            catch _:_ -> []
                            end;
                        {ok, L} when is_list(L) -> [maybe_deref(X, Opts) || X <- L];
                        _ -> []
                    end
            end;
        H when is_list(H) -> [maybe_deref(X, Opts) || X <- H];
        H when is_binary(H) ->
            try hb_json:decode(H) catch _:_ -> [] end;
        H when is_map(H) -> [maybe_deref(H, Opts)];
        _ -> []
    end.

save_harness_history(HistoryKey, History, Store, Opts) ->
    case Store of
        no_viable -> ok;
        _ ->
            try hb_store:write(Store, #{HistoryKey => hb_json:encode(History)}, Opts) catch _:_ -> ok end
    end,
    ok.

harness_loop(Messages, SystemMsg, Tools, Model, Endpoint, MaxIters, Opts, HistoryKey, Store, Iter, OriginalHistory) when Iter >= MaxIters ->
    save_harness_history(HistoryKey, Messages, Store, Opts),
    {error, #{<<"error">> => <<"max iterations reached">>, <<"history">> => Messages, <<"iterations">> => Iter, <<"original_history">> => OriginalHistory}};
harness_loop(Messages, SystemMsg, Tools, Model, Endpoint, MaxIters, Opts, HistoryKey, Store, Iter, OriginalHistory) ->
    LLMMessages = case SystemMsg of undefined -> Messages; _ -> [SystemMsg | Messages] end,
    LLMOpts = case Endpoint of
        undefined -> #{<<"model">> => Model, <<"messages">> => LLMMessages};
        _ -> #{<<"model">> => Model, <<"messages">> => LLMMessages, <<"endpoint">> => Endpoint}
    end,
    LLMReq = case Tools of
        undefined -> LLMOpts;
        _ -> LLMOpts#{<<"tools">> => Tools, <<"tool_choice">> => <<"auto">>}
    end,
    LLMMsg = maps:merge(#{<<"device">> => <<"llm@1.0">>, <<"path">> => <<"chat">>}, LLMReq),
    case hb_ao:resolve(LLMMsg, Opts) of
        {ok, Res} when is_map(Res) ->
            Body = maps:get(<<"body">>, Res, maps:get(<<"Body">>, Res, <<>>)),
            Parsed = try hb_json:decode(Body) catch _:_ -> Res end,
            {AssistantMsg, ToolCalls, Content} = extract_assistant(Parsed, Res),
            case ToolCalls of
                [] ->
                    FinalHistory = Messages ++ [AssistantMsg],
                    save_harness_history(HistoryKey, FinalHistory, Store, Opts),
                    {ok, #{<<"output">> => Content, <<"content">> => Content, <<"history">> => FinalHistory, <<"messages">> => FinalHistory, <<"iterations">> => Iter + 1, <<"raw">> => Parsed, <<"system">> => SystemMsg}};
                _ ->
                    ToolResults = [execute_harness_tool(TC, Opts) || TC <- ToolCalls],
                    NextMessages = Messages ++ [AssistantMsg] ++ ToolResults,
                    harness_loop(NextMessages, SystemMsg, Tools, Model, Endpoint, MaxIters, Opts, HistoryKey, Store, Iter + 1, OriginalHistory)
            end;
        {error, _} = Err -> Err;
        Other -> {error, #{<<"error">> => hb_util:bin(Other), <<"history">> => Messages}}
    end.

extract_assistant(Parsed, _Res) when is_map(Parsed) ->
    Choices = maps:get(<<"choices">>, Parsed, []),
    case Choices of
        [First|_] when is_map(First) ->
            Msg = maps:get(<<"message">>, First, maps:get(<<"delta">>, First, #{})),
            Content = maps:get(<<"content">>, Msg, <<>>),
            ToolCalls = maps:get(<<"tool_calls">>, Msg, maps:get(<<"toolCalls">>, Msg, [])),
            ToolCallsNorm = case ToolCalls of null -> []; undefined -> []; L when is_list(L) -> L; _ -> [] end,
            ContentBin = case Content of null -> <<>>; undefined -> <<>>; C when is_binary(C) -> C; C -> hb_util:bin(C) end,
            AssistantMsg = case ToolCallsNorm of
                [] -> #{<<"role">> => <<"assistant">>, <<"content">> => ContentBin};
                _ -> #{<<"role">> => <<"assistant">>, <<"content">> => ContentBin, <<"tool_calls">> => ToolCallsNorm}
            end,
            {AssistantMsg, ToolCallsNorm, ContentBin};
        _ ->
            % Fallback: try direct content field
            Content = maps:get(<<"content">>, Parsed, maps:get(<<"Content">>, Parsed, <<>>)),
            ContentBin = hb_util:bin(Content),
            AssistantMsg = #{<<"role">> => <<"assistant">>, <<"content">> => ContentBin},
            {AssistantMsg, [], ContentBin}
    end;
extract_assistant(_, Res) ->
    Content = maps:get(<<"body">>, Res, <<>>),
    Bin = hb_util:bin(Content),
    {#{<<"role">> => <<"assistant">>, <<"content">> => Bin}, [], Bin}.

execute_harness_tool(ToolCall, Opts) when is_map(ToolCall) ->
    Id = maps:get(<<"id">>, ToolCall, maps:get(<<"tool_call_id">>, ToolCall, hb_util:bin(rand:uniform(1000000)))),
    Fun = maps:get(<<"function">>, ToolCall, ToolCall),
    Name = maps:get(<<"name">>, Fun, maps:get(<<"tool">>, Fun, <<>>)),
    ArgsRaw = maps:get(<<"arguments">>, Fun, maps:get(<<"args">>, Fun, <<"{}">>)),
    Args = case ArgsRaw of
        A when is_map(A) -> A;
        A when is_binary(A) -> try hb_json:decode(A) catch _:_ -> #{<<"raw">> => A} end;
        _ -> #{}
    end,
    ResultContent = dispatch_tool(Name, Args, Opts),
    #{<<"role">> => <<"tool">>, <<"tool_call_id">> => hb_util:bin(Id), <<"content">> => hb_util:bin(ResultContent), <<"name">> => hb_util:bin(Name)}.

dispatch_tool(Name, Args, Opts) when is_map(Args) ->
    % If args already specifies a device/path, honor it
    case maps:get(<<"device">>, Args, not_found) of
        not_found ->
            % Check for relay-style tool
            RelayPath = case maps:get(<<"relay-path">>, Args, not_found) of
                not_found -> case maps:get(<<"url">>, Args, not_found) of
                    not_found -> case maps:get(<<"path">>, Args, not_found) of
                        not_found when Name =/= <<>> ->
                            % Name might be URL or relay target; try empty
                            not_found;
                        P -> P
                    end;
                    U -> U
                end;
                P -> P
            end,
            case RelayPath of
                not_found ->
                    % No path: try generic relay with args as body, or direct error
                    case hb_ao:resolve(maps:merge(#{<<"device">> => <<"relay@1.0">>, <<"path">> => <<"call">>}, Args), Opts) of
                        {ok, R} when is_map(R) -> maps:get(<<"body">>, R, maps:get(<<"Body">>, R, hb_json:encode(R)));
                        {ok, R} when is_binary(R) -> R;
                        {error, E} -> hb_util:bin(E);
                        Other -> hb_util:bin(Other)
                    end;
                _ ->
                    RelayMethod = maps:get(<<"relay-method">>, Args, maps:get(<<"method">>, Args, <<"GET">>)),
                    RelayBody = maps:get(<<"relay-body">>, Args, maps:get(<<"body">>, Args, not_found)),
                    BaseReq = #{<<"device">> => <<"relay@1.0">>, <<"path">> => <<"call">>, <<"relay-path">> => hb_util:bin(RelayPath), <<"relay-method">> => hb_util:bin(RelayMethod)},
                    ReqWithBody = case RelayBody of not_found -> BaseReq; _ -> BaseReq#{<<"relay-body">> => RelayBody} end,
                    Merged = maps:merge(ReqWithBody, maps:without([<<"relay-path">>, <<"relay-method">>, <<"relay-body">>, <<"url">>, <<"path">>, <<"method">>, <<"body">>], Args)),
                    case hb_ao:resolve(Merged, Opts) of
                        {ok, R} when is_map(R) -> maps:get(<<"body">>, R, maps:get(<<"Body">>, R, hb_json:encode(R)));
                        {ok, R} when is_binary(R) -> R;
                        {error, E} -> hb_util:bin(E);
                        Other -> hb_util:bin(Other)
                    end
            end;
        Device ->
            Path = maps:get(<<"path">>, Args, <<"call">>),
            CleanArgs = maps:without([<<"device">>, <<"path">>], Args),
            case hb_ao:resolve(maps:merge(#{<<"device">> => hb_util:bin(Device), <<"path">> => hb_util:bin(Path)}, CleanArgs), Opts) of
                {ok, R} when is_map(R) -> maps:get(<<"body">>, R, maps:get(<<"Body">>, R, hb_json:encode(R)));
                {ok, R} when is_binary(R) -> R;
                {error, E} -> hb_util:bin(E);
                Other -> hb_util:bin(Other)
            end
    end.

-ifdef(TEST).
parse_feed_test() ->
    Body = <<"<rss><channel><item><guid>1</guid><title>T</title><link>http://x</link><description>D</description></item></channel></rss>">>,
    [Post] = parse_feed(Body),
    ?assertEqual(<<"1">>, maps:get(<<"guid">>, Post)),
    ?assertEqual(<<"T">>, maps:get(<<"title">>, Post)).
-endif.
