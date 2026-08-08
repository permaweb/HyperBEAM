%%% @doc Skills device for HyperBEAM — `skills@1.0`.
%%%
%%% Implements Tom Wilson's model: Tools are atomic (get_gmail_messages),
%%% Skills are composable procedures that use 1..N tools generically.
%%% Harness enforces: does agent have tools skill requires?
%%%
%%% Skill shape (stored as JSON under `skill-<name>` + `skill-index`):
%%%   #{ name => binary, description => binary,
%%%      requires_tools => [binary],            % e.g. [<<"gmail_read">>]
%%%      instructions => binary,                % markdown procedure
%%%      steps => [map],                        % optional explicit steps
%%%      version => binary }
%%%
%%% Agent shape (per essay):
%%%   agents/<id>/identity.md, user.md, soul.md, tools.json, memory/*.md
%%%   Agent = LLM + Harness + Tools + Instructions, memory via FS.
-module(dev_skills).
-implements(<<"skills@1.0">>).
-export([info/1, info/3, register/3, get/3, list/3, run/3, compose/3, check/3]).
-export([skill_key/1, index_key/0]).

-include_lib("eunit/include/eunit.hrl").

info(_) ->
    #{
        <<"register">> => dev,
        <<"get">> => dev,
        <<"list">> => dev,
        <<"run">> => dev,
        <<"compose">> => dev,
        <<"check">> => dev
    }.
info(_, _, _) -> {ok, info(#{})}.

skill_key(Name) when is_binary(Name) -> <<"skill-", Name/binary>>;
skill_key(Name) -> skill_key(hb_util:bin(Name)).

index_key() -> <<"skill-index">>.

%% Register (or update) a skill.
%% Req keys: name | skill | id, description, requires_tools | tools, instructions | steps | procedure
register(Base, Req, Opts) ->
    Name0 = hb_ao:get(<<"name">>, Req, hb_ao:get(<<"skill">>, Req, hb_ao:get(<<"id">>, Req, hb_ao:get(<<"name">>, Base, not_found, Opts), Opts), Opts), Opts),
    Name = case maybe_deref(Name0, Opts) of
        not_found -> hb_util:bin(rand:uniform(1000000));
        N -> hb_util:bin(N)
    end,
    Desc0 = hb_ao:get(<<"description">>, Req, hb_ao:get(<<"description">>, Base, <<>>, Opts), Opts),
    Desc = hb_util:bin(maybe_deref(Desc0, Opts)),
    Requires0 = hb_ao:get(<<"requires_tools">>, Req,
                    hb_ao:get(<<"requires-tools">>, Req,
                        hb_ao:get(<<"tools">>, Req,
                            hb_ao:get(<<"requires_tools">>, Base, [], Opts), Opts), Opts), Opts),
    RequiresDeref = maybe_deref(Requires0, Opts),
    Requires = normalize_requires(RequiresDeref),
    Instructions0 = hb_ao:get(<<"instructions">>, Req,
                        hb_ao:get(<<"procedure">>, Req,
                            hb_ao:get(<<"steps">>, Req,
                                hb_ao:get(<<"instructions">>, Base, <<>>, Opts), Opts), Opts), Opts),
    Instructions = maybe_deref(Instructions0, Opts),
    InstructionsBin = case Instructions of
        B when is_binary(B) -> B;
        L when is_list(L) -> hb_json:encode(L);
        M when is_map(M) -> hb_json:encode(M);
        _ -> <<>>
    end,
    Steps0 = hb_ao:get(<<"steps">>, Req, hb_ao:get(<<"steps">>, Base, [], Opts), Opts),
    Steps = deep_deref(maybe_deref(Steps0, Opts), Opts),
    StepsList = case Steps of SL when is_list(SL) -> SL; SM when is_map(SM) -> [SM]; _ -> [] end,
    Version0 = hb_ao:get(<<"version">>, Req, hb_ao:get(<<"version">>, Base, <<"1.0">>, Opts), Opts),
    Version = hb_util:bin(maybe_deref(Version0, Opts)),
    Skill = #{
        <<"name">> => Name,
        <<"description">> => Desc,
        <<"requires_tools">> => Requires,
        <<"instructions">> => InstructionsBin,
        <<"steps">> => StepsList,
        <<"version">> => Version
    },
    Store = hb_opts:get(store, no_viable, Opts),
    Key = skill_key(Name),
    case Store of
        no_viable -> {error, no_store};
        _ ->
            ok = hb_store:write(Store, #{Key => hb_json:encode(Skill)}, Opts),
            % Update index
            IdxKey = index_key(),
            Existing = case hb_store:read(Store, IdxKey, Opts) of
                {ok, Bin} when is_binary(Bin) -> try hb_json:decode(Bin) catch _:_ -> [] end;
                {ok, ExistingList} when is_list(ExistingList) -> ExistingList;
                _ -> []
            end,
            Merged = lists:usort([Name | Existing]),
            ok = hb_store:write(Store, #{IdxKey => hb_json:encode(Merged)}, Opts),
            {ok, Skill#{ <<"stored_as">> => Key, <<"total">> => length(Merged) }}
    end.

%% Get a skill by name
get(Base, Req, Opts) ->
    Name0 = hb_ao:get(<<"name">>, Req, hb_ao:get(<<"skill">>, Req, hb_ao:get(<<"id">>, Req, hb_ao:get(<<"name">>, Base, not_found, Opts), Opts), Opts), Opts),
    Name = hb_util:bin(maybe_deref(Name0, Opts)),
    case Name of
        <<>> -> {error, missing_name};
        _ ->
            Store = hb_opts:get(store, no_viable, Opts),
            Key = skill_key(Name),
            case hb_store:read(Store, Key, Opts) of
                {ok, Bin} when is_binary(Bin) ->
                    Skill = try hb_json:decode(Bin) catch _:_ -> #{} end,
                    {ok, deep_deref(Skill, Opts)};
                {ok, M} when is_map(M) -> {ok, deep_deref(M, Opts)};
                Err -> Err
            end
    end.

%% List all skill names
list(_Base, _Req, Opts) ->
    Store = hb_opts:get(store, no_viable, Opts),
    IdxKey = index_key(),
    case hb_store:read(Store, IdxKey, Opts) of
        {ok, Bin} when is_binary(Bin) ->
            Keys = try hb_json:decode(Bin) catch _:_ -> [] end,
            {ok, #{<<"skills">> => Keys, <<"count">> => length(Keys)}};
        {ok, L} when is_list(L) -> {ok, #{<<"skills">> => L, <<"count">> => length(L)}};
        _ -> {ok, #{<<"skills">> => [], <<"count">> => 0}}
    end.

%% Check if agent's tools satisfy skill's requires_tools
check(Base, Req, Opts) ->
    SkillName0 = hb_ao:get(<<"skill">>, Req, hb_ao:get(<<"name">>, Req, hb_ao:get(<<"skill">>, Base, not_found, Opts), Opts), Opts),
    SkillName = hb_util:bin(maybe_deref(SkillName0, Opts)),
    AgentTools0 = hb_ao:get(<<"agent_tools">>, Req,
                    hb_ao:get(<<"tools">>, Req,
                        hb_ao:get(<<"agent_tools">>, Base, not_found, Opts), Opts), Opts),
    AgentToolsDeref = maybe_deref(AgentTools0, Opts),
    AgentTools = normalize_requires(AgentToolsDeref),
    case get(Base, #{<<"name">> => SkillName}, Opts) of
        {ok, Skill} ->
            Requires = maps:get(<<"requires_tools">>, Skill, []),
            Missing = [T || T <- Requires, not lists:member(T, AgentTools)],
            case Missing of
                [] -> {ok, #{<<"can_run">> => true, <<"skill">> => SkillName, <<"requires_tools">> => Requires, <<"agent_tools">> => AgentTools}};
                _ -> {ok, #{<<"can_run">> => false, <<"skill">> => SkillName, <<"requires_tools">> => Requires, <<"agent_tools">> => AgentTools, <<"missing">> => Missing}}
            end;
        Err -> Err
    end.

%% Run a skill: check permission, then delegate to harness@1.0/handle
%% Req: skill (name), message|prompt, agent_tools|tools, collection|agent, model, history, etc.
run(Base, Req, Opts) ->
    SkillName0 = hb_ao:get(<<"skill">>, Req, hb_ao:get(<<"name">>, Req, hb_ao:get(<<"skill">>, Base, not_found, Opts), Opts), Opts),
    SkillName = hb_util:bin(maybe_deref(SkillName0, Opts)),
    Message0 = hb_ao:get(<<"message">>, Req, hb_ao:get(<<"prompt">>, Req, hb_ao:get(<<"data">>, Req, hb_ao:get(<<"message">>, Base, <<>>, Opts), Opts), Opts), Opts),
    MessageDeref = maybe_deref(Message0, Opts),
    Message = case MessageDeref of not_found -> <<>>; undefined -> <<>>; M -> hb_util:bin(M) end,
    AgentTools0 = hb_ao:get(<<"agent_tools">>, Req,
                    hb_ao:get(<<"tools">>, Req,
                        hb_ao:get(<<"agent_tools">>, Base, [], Opts), Opts), Opts),
    AgentTools = normalize_requires(maybe_deref(AgentTools0, Opts)),
    % Load skill
    case get(Base, #{<<"name">> => SkillName}, Opts) of
        {ok, Skill} ->
            Requires = maps:get(<<"requires_tools">>, Skill, []),
            Missing = [T || T <- Requires, not lists:member(T, AgentTools)],
            case Missing of
                [] ->
                    Instructions = maps:get(<<"instructions">>, Skill, <<>>),
                    % Map requires_tools to relay tool specs for harness
                    HarnessTools = tools_to_harness_specs(Requires),
                    Collection0 = hb_ao:get(<<"collection">>, Req, hb_ao:get(<<"agent">>, Req, hb_ao:get(<<"collection">>, Base, SkillName, Opts), Opts), Opts),
                    Collection = hb_util:bin(maybe_deref(Collection0, Opts)),
                    Model0 = hb_ao:get(<<"model">>, Req, hb_ao:get(<<"model">>, Base, <<"qwen3.6">>, Opts), Opts),
                    Model = hb_util:bin(maybe_deref(Model0, Opts)),
                    % History handling delegated to harness — essay flow: system (instructions) + history + tools + current
                    HarnessReq0 = #{
                        <<"device">> => <<"harness@1.0">>,
                        <<"path">> => <<"handle">>,
                        <<"message">> => Message,
                        <<"tools">> => HarnessTools,
                        <<"collection">> => Collection,
                        <<"model">> => Model
                    },
                    HarnessReq = case Instructions of
                        <<>> -> HarnessReq0;
                        _ -> HarnessReq0#{<<"system">> => Instructions}
                    end,
                    % Pass through endpoint/history/identity/soul/user if supplied (essay: system = identity+soul+user+instructions)
                    HarnessReq2 = maybe_add(<<"endpoint">>, hb_ao:get(<<"endpoint">>, Req, not_found, Opts), HarnessReq, Opts),
                    HarnessReq3 = maybe_add(<<"history">>, hb_ao:get(<<"history">>, Req, not_found, Opts), HarnessReq2, Opts),
                    HarnessReq4 = maybe_add(<<"identity">>, hb_ao:get(<<"identity">>, Req, hb_ao:get(<<"identity">>, Base, not_found, Opts), Opts), HarnessReq3, Opts),
                    HarnessReq5 = maybe_add(<<"soul">>, hb_ao:get(<<"soul">>, Req, hb_ao:get(<<"soul">>, Base, not_found, Opts), Opts), HarnessReq4, Opts),
                    HarnessReq6 = maybe_add(<<"user">>, hb_ao:get(<<"user">>, Req, hb_ao:get(<<"user">>, Base, not_found, Opts), Opts), HarnessReq5, Opts),
                    case hb_ao:resolve(HarnessReq6, Opts) of
                        {ok, Res} when is_map(Res) ->
                            Output = maps:get(<<"output">>, Res, maps:get(<<"content">>, Res, <<>>)),
                            % Optionally append to skill-specific memory (store output)
                            {ok, Res#{ <<"skill">> => SkillName, <<"output">> => Output, <<"can_run">> => true }};
                        Err -> Err
                    end;
                _ ->
                    {error, #{<<"can_run">> => false, <<"skill">> => SkillName, <<"missing">> => Missing, <<"requires_tools">> => Requires, <<"agent_tools">> => AgentTools}}
            end;
        Err -> Err
    end.

%% Compose two skills into a new skill
compose(Base, Req, Opts) ->
    NameA0 = hb_ao:get(<<"skill_a">>, Req, hb_ao:get(<<"a">>, Req, not_found, Opts), Opts),
    NameB0 = hb_ao:get(<<"skill_b">>, Req, hb_ao:get(<<"b">>, Req, not_found, Opts), Opts),
    NewName0 = hb_ao:get(<<"name">>, Req, hb_ao:get(<<"new_name">>, Req, not_found, Opts), Opts),
    NameA = hb_util:bin(maybe_deref(NameA0, Opts)),
    NameB = hb_util:bin(maybe_deref(NameB0, Opts)),
    NewName = case maybe_deref(NewName0, Opts) of not_found -> <<NameA/binary, "+", NameB/binary>>; N -> hb_util:bin(N) end,
    case {get(Base, #{<<"name">> => NameA}, Opts), get(Base, #{<<"name">> => NameB}, Opts)} of
        {{ok, SkillA}, {ok, SkillB}} ->
            RequiresA = maps:get(<<"requires_tools">>, SkillA, []),
            RequiresB = maps:get(<<"requires_tools">>, SkillB, []),
            Requires = lists:usort(RequiresA ++ RequiresB),
            Instructions = <<(maps:get(<<"instructions">>, SkillA, <<>>))/binary, "\n\nThen:\n", (maps:get(<<"instructions">>, SkillB, <<>>))/binary>>,
            Steps = maps:get(<<"steps">>, SkillA, []) ++ maps:get(<<"steps">>, SkillB, []),
            Desc = <<"Composed ", NameA/binary, " + ", NameB/binary>>,
            % Register new skill
            register(Base, #{<<"name">> => NewName, <<"description">> => Desc, <<"requires_tools">> => Requires, <<"instructions">> => Instructions, <<"steps">> => Steps}, Opts);
        {Err, _} when element(1, Err) == error -> Err;
        {_, Err} -> Err
    end.

%% Helpers
normalize_requires(not_found) -> [];
normalize_requires(undefined) -> [];
normalize_requires(L) when is_list(L) -> [hb_util:bin(maybe_deref(X, #{})) || X <- L];
normalize_requires(B) when is_binary(B) ->
    try hb_json:decode(B) of
        L when is_list(L) -> [hb_util:bin(X) || X <- L];
        _ -> [B]
    catch _:_ -> [B]
    end;
normalize_requires(M) when is_map(M) -> [hb_util:bin(K) || K <- maps:keys(M)];
normalize_requires(V) -> [hb_util:bin(V)].

tools_to_harness_specs(Requires) ->
    [tool_to_spec(T) || T <- Requires].

tool_to_spec(Tool) when is_binary(Tool) ->
    % Map well-known tools to relay specs
    case Tool of
        <<"gmail_read">> -> gmail_spec();
        <<"gmail_send">> -> gmail_send_spec();
        <<"calendar_read">> -> calendar_spec();
        <<"drive_read">> -> drive_spec();
        <<"github_read">> -> github_spec();
        _ ->
            % Generic: expose as function that takes relay-path
            #{
                <<"type">> => <<"function">>,
                <<"function">> => #{
                    <<"name">> => Tool,
                    <<"description">> => <<Tool/binary, " tool">>,
                    <<"parameters">> => #{
                        <<"type">> => <<"object">>,
                        <<"properties">> => #{
                            <<"relay-path">> => #{<<"type">> => <<"string">>},
                            <<"relay-method">> => #{<<"type">> => <<"string">>}
                        }
                    }
                }
            }
    end.

gmail_spec() ->
    #{
        <<"type">> => <<"function">>,
        <<"function">> => #{
            <<"name">> => <<"get_gmail_messages">>,
            <<"description">> => <<"Read Gmail messages">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"q">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Search query">>},
                    <<"max">> => #{<<"type">> => <<"integer">>}
                }
            }
        }
    }.

gmail_send_spec() ->
    #{
        <<"type">> => <<"function">>,
        <<"function">> => #{
            <<"name">> => <<"send_gmail">>,
            <<"description">> => <<"Send Gmail">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"to">> => #{<<"type">> => <<"string">>},
                    <<"subject">> => #{<<"type">> => <<"string">>},
                    <<"body">> => #{<<"type">> => <<"string">>}
                }
            }
        }
    }.

calendar_spec() ->
    #{
        <<"type">> => <<"function">>,
        <<"function">> => #{
            <<"name">> => <<"get_calendar_events">>,
            <<"description">> => <<"Read calendar events">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"timeMin">> => #{<<"type">> => <<"string">>},
                    <<"timeMax">> => #{<<"type">> => <<"string">>}
                }
            }
        }
    }.

drive_spec() ->
    #{
        <<"type">> => <<"function">>,
        <<"function">> => #{
            <<"name">> => <<"read_drive">>,
            <<"description">> => <<"Read Drive file">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"fileId">> => #{<<"type">> => <<"string">>}
                }
            }
        }
    }.

github_spec() ->
    #{
        <<"type">> => <<"function">>,
        <<"function">> => #{
            <<"name">> => <<"github_search">>,
            <<"description">> => <<"Search GitHub">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"q">> => #{<<"type">> => <<"string">>}
                }
            }
        }
    }.

maybe_add(_Key, not_found, Map, _Opts) -> Map;
maybe_add(_Key, undefined, Map, _Opts) -> Map;
maybe_add(Key, Val0, Map, Opts) ->
    Val = maybe_deref(Val0, Opts),
    Map#{Key => Val}.

maybe_deref(not_found, _) -> not_found;
maybe_deref(undefined, _) -> undefined;
maybe_deref(V, Opts) ->
    try hb_cache:ensure_loaded(V, Opts) catch _:_ -> V end.

deep_deref(V, Opts) when is_map(V) ->
    case (catch hb_cache:ensure_loaded(V, Opts)) of
        V2 when is_map(V2), V2 =/= V -> deep_deref(V2, Opts);
        _ -> maps:from_list([{K, deep_deref(Val, Opts)} || {K, Val} <- maps:to_list(V)])
    end;
deep_deref(V, Opts) when is_list(V) -> [deep_deref(X, Opts) || X <- V];
deep_deref(V, Opts) ->
    case (catch hb_cache:ensure_loaded(V, Opts)) of
        V2 when V2 =/= V -> deep_deref(V2, Opts);
        _ -> V
    end.

-ifdef(TEST).
register_get_list_test() ->
    {ok,_}=application:ensure_all_started(hb),
    Store = hb_opts:get(store, no_viable, #{}),
    % Clean
    hb_store:write(Store, #{index_key() => hb_json:encode([])}, #{}),
    {ok, _} = register(#{}, #{<<"name">> => <<"summarize">>, <<"requires_tools">> => [<<"gmail_read">>], <<"instructions">> => <<"Summarize docs">>}, #{}),
    {ok, #{<<"skills">> := Keys}} = list(#{}, #{}, #{}),
    ?assert(lists:member(<<"summarize">>, Keys)),
    {ok, Skill} = get(#{}, #{<<"name">> => <<"summarize">>}, #{}),
    ?assertEqual(<<"summarize">>, maps:get(<<"name">>, Skill)).

check_can_run_test() ->
    {ok,_}=application:ensure_all_started(hb),
    register(#{}, #{<<"name">> => <<"inbox-zero">>, <<"requires_tools">> => [<<"gmail_read">>]}, #{}),
    {ok, #{<<"can_run">> := true}} = check(#{}, #{<<"skill">> => <<"inbox-zero">>, <<"tools">> => [<<"gmail_read">>, <<"gmail_send">>]}, #{}),
    {ok, #{<<"can_run">> := false, <<"missing">> := [<<"gmail_read">>]}} = check(#{}, #{<<"skill">> => <<"inbox-zero">>, <<"tools">> => [<<"calendar_read">>]}, #{}).
-endif.
