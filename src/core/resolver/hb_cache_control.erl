%%% @doc Cache control logic for the AO-Core resolver. It derives cache settings
%%% from request, response, execution-local node Opts, as well as the global
%%% node Opts. It applies these settings when asked to maybe store/lookup in 
%%% response to a request.
-module(hb_cache_control).
-export([maybe_store/5, maybe_lookup/5]).
-export([derive_cache_settings/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% When other cache control settings are not specified, we default to the
%%% following settings.
-define(DEFAULT_STORE_OPT, false).
-define(DEFAULT_LOOKUP_OPT,  true).

%%% Public API

%% @doc Write a resulting M3 message to the cache if requested. The precedence
%% order of cache control sources is as follows:
%% 1. The `Opts' map (letting the node operator have the final say).
%% 2. The `Res' results message (granted by Base's device).
%% 3. The `OriginalReq' message (the user's request).
%% Base is not used, such that it can specify cache control information about 
%% itself, without affecting its outputs. Cache identity uses the varied inputs.
maybe_store(Base, Req, Res, OriginalReq, Opts) ->
    case derive_cache_settings([Res, OriginalReq], Opts) of
        #{ <<"store">> := true } ->
            ?event(caching, {caching_result, {base, Base}, {req, Req}, {res, Res}}),
            dispatch_cache_write(Base, Req, Res, Opts);
        _ -> 
            not_caching
    end.

%% @doc Handles cache lookup, modulated by the caching options requested by
%% the user. Honors the following `Opts' cache keys: 
%%      `only_if_cached': If set and we do not find a result in the cache,
%%                        return an error with a `Cache-Status' of `miss' and
%%                        a 504 `Status'.
%%      `no_cache':       If set, the cached values are never used. Returns
%%                        `continue' to the caller.
maybe_lookup(Base, Req, _OriginalBase, OriginalReq, Opts) ->
    % A base's own `cache-control' describes itself, not this lookup.
    case derive_cache_settings([OriginalReq], Opts) of
        #{ <<"lookup">> := false } ->
            ?event({skip_cache_check, lookup_disabled}),
            {continue, Base, Req};
        Settings = #{ <<"lookup">> := true } ->
            OutputScopedOpts =
                hb_store:scope(
                    Opts,
                    hb_opts:get(store_scope_resolved, local, Opts)
                ),
            case hb_cache:read_resolved(Base, Req, OutputScopedOpts) of
                {hit, not_found} ->
                    {error, not_found};
                {hit, {ok, Res}} ->
                    ?event(caching,
                        {cache_hit,
                            {base, Base},
                            {req, Req},
                            {res, Res}
                        }
                    ),
                    {ok, Res};
                _ ->
                    ?event(caching, {result_cache_miss, Base, Req}),
                    case Settings of
                        #{ <<"only-if-cached">> := true } ->
                            only_if_cached_not_found_error(Base, Req, Opts);
                        _ ->
                            {continue, Base, Req}
                        end
            end
    end.

%%% Internal functions

%% @doc Dispatch the cache write to a worker process if requested.
%% Invoke the appropriate cache write function based on the type of the message.
dispatch_cache_write(Base, Req, Res, Opts) ->
    case hb_opts:get(async_cache, false, Opts) of
        true ->
            spawn(fun() -> perform_cache_write(Base, Req, Res, Opts) end),
            ok;
        false ->
            perform_cache_write(Base, Req, Res, Opts)
    end.

%% @doc Internal function to write a compute result to the cache.
perform_cache_write(Base, Req, Res, Opts) when is_map(Res); is_binary(Res) ->
    case cacheable(Base, false, Opts) andalso cacheable(Req, false, Opts)
            andalso cacheable(Res, true, Opts) of
        true ->
            BaseID = hb_message:id(Base, all, Opts),
            ReqID = hb_message:id(Req, all, Opts),
            {ok, BaseID} = hb_cache:write(Base, Opts),
            {ok, ReqID} = hb_cache:write(Req, Opts),
            {ok, Path} = hb_cache:write(Res, Opts),
            ResultID = case is_map(Res) of
                true -> hb_message:id(Res, all, Opts);
                false -> Path
            end,
            hb_cache:link(
                ResultID,
                <<BaseID/binary, "/", ReqID/binary>>,
                Opts#{ <<"store">> => hb_opts:get(attested_store, [], Opts) }
            );
        false ->
            ?event(caching, {skip_caching, uncacheable_computation}),
            skip_caching
    end;
perform_cache_write(_Base, _Req, _Res, _Opts) -> skip_caching.

%% @doc Inputs cannot contain signatures. Results may, provided every field
%% survives storage and every commitment verifies, including linked children.
cacheable(Link, AllowSigned, Opts) when ?IS_LINK(Link) ->
    try
        Loaded = hb_cache:ensure_loaded(Link, Opts),
        % A link must expose the identity used to name it in its parent.
        Identified =
            case hb_link:normalize(#{ <<"child">> => Link }, discard, Opts) of
                #{ <<"child">> := <<"ao:", ID/binary>> }
                        when is_map(Loaded); is_list(Loaded) ->
                    hb_message:id(Loaded, all, Opts) == ID;
                _ -> true
            end,
        Identified andalso cacheable(Loaded, AllowSigned, Opts)
    catch throw:{necessary_message_not_found, _, _} -> false
    end;
cacheable(Msg, AllowSigned, Opts) when is_map(Msg) ->
    (case AllowSigned of
        true -> complete(Msg, Opts);
        false -> hb_message:signers(Msg, Opts) == []
    end) andalso
        lists:all(
            fun({Key, Value}) ->
                hb_private:is_private(Key) orelse cacheable(Value, AllowSigned, Opts)
            end,
            maps:to_list(hb_message:uncommitted(Msg, Opts))
        );
cacheable(Values, AllowSigned, Opts) when is_list(Values) ->
    lists:all(fun(Value) -> cacheable(Value, AllowSigned, Opts) end, Values);
cacheable(_Value, _AllowSigned, _Opts) -> true.

%% @doc No field observed by the computation may disappear when it is stored.
complete(Msg, Opts) ->
    {ok, Committed} = hb_message:with_only_committed(Msg, Opts),
    lists:all(fun hb_private:is_private/1, maps:keys(Msg) -- maps:keys(Committed))
        andalso hb_message:verify(Msg, #{ <<"ids">> => <<"all">> }, Opts).

%% @doc Generate a message to return when `only_if_cached' was specified, and
%% we don't have a cached result.
only_if_cached_not_found_error(Base, Req, Opts) ->
    ?event(
        caching,
        {only_if_cached_execution_failed, {base, Base}, {req, Req}},
        Opts
    ),
    {error,
        #{
            <<"status">> => 504,
            <<"cache-status">> => <<"miss">>,
            <<"body">> =>
                <<"Computed result not available in cache.">>
        }
    }.

%% @doc Derive cache settings from a series of option sources and the opts,
%% honoring precidence order. The Opts is used as the first source. Returns a
%% map with `store' and `lookup' keys, each of which is a boolean.
%% 
%% For example, if the last source has a `no_store', the first expresses no
%% preference, but the Opts has `<<"cache-control">> => [always]', then the result 
%% will contain a `<<"store">> => true' entry.
derive_cache_settings(SourceList, Opts) ->
    lists:foldr(
        fun(Source, Acc) ->
            maybe_set(Acc, cache_source_to_cache_settings(Source, Opts), Opts)
        end,
        #{ <<"store">> => ?DEFAULT_STORE_OPT, <<"lookup">> => ?DEFAULT_LOOKUP_OPT },
        [{opts, Opts}|lists:filter(fun erlang:is_map/1, SourceList)]
    ).

%% @doc Takes a key and two maps, returning the first map with the key set to
%% the value of the second map _if_ the value is not undefined.
maybe_set(Map1, Map2, Opts) ->
    lists:foldl(
        fun(Key, AccMap) ->
            case hb_maps:get(Key, Map2, undefined, Opts) of
                undefined -> AccMap;
                Value -> hb_maps:put(Key, Value, AccMap, Opts)
            end
        end,
        Map1,
        hb_maps:keys(Map2, Opts)
    ).

%% @doc Convert a cache source to a cache setting. The setting _must_ always be
%% directly in the source, not an AO-Core-derivable value. The 
%% `to_cache_control_map' function is used as the source of settings in all
%% cases, except where an `Opts' specifies that hashpaths should not be updated,
%% which leads to the result not being cached (as it may be stored with an 
%% incorrect hashpath).
cache_source_to_cache_settings({opts, Opts}, _) ->
    CCMap = specifiers_to_cache_settings(hb_opts:get(cache_control, [], Opts)),
    case hb_opts:get(hashpath, update, Opts) of
        ignore -> CCMap#{ <<"store">> => false };
        _ -> CCMap
    end;
cache_source_to_cache_settings(Msg, Opts) ->
    case hb_maps:find(<<"cache-control">>, Msg, Opts) of
        {ok, CC} -> specifiers_to_cache_settings(CC);
        _ -> #{}
    end.

%% @doc Convert a cache control list as received via HTTP headers into a 
%% normalized map of simply whether we should store and/or lookup the result.
%% A header separates its directives with commas. Directives are ASCII tokens,
%% compared without regard to case. A value that cannot be read as a list (its
%% bytes are not UTF-8) gives no directive.
%% Only binaries and atoms are directives: any other value, such as a message,
%% is ignored.
specifiers_to_cache_settings(CCSpecifier) when is_binary(CCSpecifier) ->
    specifiers_to_cache_settings(
        try hb_util:binary_to_strings(CCSpecifier)
        catch error:{cannot_parse_list, _} -> []
        end
    );
specifiers_to_cache_settings(CCSpecifier) when not is_list(CCSpecifier) ->
    specifiers_to_cache_settings([CCSpecifier]);
specifiers_to_cache_settings(RawCCList) ->
    CCList =
        lists:map(
            fun(CC) -> hb_util_string:lowercase(hb_ao:normalize_key(CC)) end,
            [ CC || CC <- RawCCList, is_binary(CC) orelse is_atom(CC) ]
        ),
    #{
        <<"store">> =>
            case lists:member(<<"always">>, CCList) of
                true -> true;
                false ->
                    case lists:member(<<"no-store">>, CCList) of
                        true -> false;
                        false ->
                            case lists:member(<<"store">>, CCList) of
                                true -> true;
                                false -> undefined
                            end
                    end
            end,
        <<"lookup">> =>
            case lists:member(<<"always">>, CCList) of
                true -> true;
                false ->
                    case lists:member(<<"no-cache">>, CCList) of
                        true -> false;
                    false ->
                        case lists:member(<<"cache">>, CCList) of
                            true -> true;
                            false -> undefined
                        end
                    end
            end,
        <<"only-if-cached">> =>
            case lists:member(<<"only-if-cached">>, CCList) of
                true -> true;
                false -> undefined
            end
    }.

%%% Tests

%% Helpers to create a message with Cache-Control header
msg_with_cc(CC) -> #{ <<"cache-control">> => CC }.
opts_with_cc(CC) -> #{ <<"cache-control">> => CC }.

%% Test precedence order (Opts > Res > Req)
opts_override_message_settings_test() ->
    Req = msg_with_cc([<<"no-store">>]),
    Res = msg_with_cc([<<"no-cache">>]),
    Opts = opts_with_cc([<<"always">>]),
    Result = derive_cache_settings([Res, Req], Opts),
    ?assertEqual(#{<<"store">> => true, <<"lookup">> => true}, Result).

msg_precidence_overrides_test() ->
    Req = msg_with_cc([<<"always">>]),
    Res = msg_with_cc([<<"no-store">>]),  % No restrictions
    Result = derive_cache_settings([Res, Req], opts_with_cc([])),
    ?assertEqual(#{<<"store">> => false, <<"lookup">> => true}, Result).

%% Test specific directives
no_store_directive_test() ->
    Msg = msg_with_cc([<<"no-store">>]),
    Result = derive_cache_settings([Msg], opts_with_cc([])),
    % A message in the list is ignored: `no-store' still applies.
    WithMsg = msg_with_cc([<<"no-store">>, #{ <<"x">> => 1 }]),
    ?assertEqual(Result, derive_cache_settings([WithMsg], opts_with_cc([]))),
    ?assertEqual(#{<<"store">> => false, <<"lookup">> => ?DEFAULT_LOOKUP_OPT}, Result).

no_cache_directive_test() ->
    Msg = msg_with_cc([<<"no-cache">>]),
    Result = derive_cache_settings([Msg], opts_with_cc([])),
    ?assertEqual(#{<<"store">> => ?DEFAULT_STORE_OPT, <<"lookup">> => false}, Result).

only_if_cached_directive_test() ->
    Msg = msg_with_cc([<<"only-if-cached">>]),
    Result = derive_cache_settings([Msg], opts_with_cc([])),
    ?assertEqual(
        #{
            <<"store">> => ?DEFAULT_STORE_OPT,
            <<"lookup">> => ?DEFAULT_LOOKUP_OPT,
            <<"only-if-cached">> => true
        },
        Result
    ).

%% Test hashpath settings
hashpath_ignore_prevents_storage_test() ->
    Opts = (opts_with_cc([]))#{<<"hashpath">> => ignore},
    Result = derive_cache_settings([], Opts),
    ?assertEqual(#{<<"store">> => ?DEFAULT_STORE_OPT, <<"lookup">> => ?DEFAULT_LOOKUP_OPT}, Result).

%% Test multiple directives
multiple_directives_test() ->
    Msg = msg_with_cc([<<"no-store">>, <<"no-cache">>, <<"only-if-cached">>]),
    Result = derive_cache_settings([Msg], opts_with_cc([])),
    ?assertEqual(
        #{
            <<"store">> => false,
            <<"lookup">> => false,
            <<"only-if-cached">> => true
        },
        Result
    ).

%% Test empty/missing cases
empty_message_list_test() ->
    Result = derive_cache_settings([], opts_with_cc([])),
    ?assertEqual(#{<<"store">> => ?DEFAULT_STORE_OPT, <<"lookup">> => ?DEFAULT_LOOKUP_OPT}, Result).

message_without_cache_control_test() ->
    Result = derive_cache_settings([#{}], opts_with_cc([])),
    ?assertEqual(#{<<"store">> => ?DEFAULT_STORE_OPT, <<"lookup">> => ?DEFAULT_LOOKUP_OPT}, Result).

%% Test the cache_source_to_cache_setting function directly
opts_source_cache_control_test() ->
    Result =
        cache_source_to_cache_settings(
            {opts, opts_with_cc([<<"no-store">>])},
            #{}
        ),
    ?assertEqual(#{
        <<"store">> => false,
        <<"lookup">> => undefined,
        <<"only-if-cached">> => undefined
    }, Result).

message_source_cache_control_test() ->
    Msg = msg_with_cc([<<"no-cache">>]),
    Result = cache_source_to_cache_settings(Msg, #{}),
    ?assertEqual(#{
        <<"store">> => undefined,
        <<"lookup">> => false,
        <<"only-if-cached">> => undefined
    }, Result).

%%% Basic cached AO-Core resolution tests

cache_binary_result_test() ->
    CachedMsg = <<"GOOD FUNCTION">>,
    Base =
        #{
            <<"device">> => <<"test-device@1.0">>,
            <<"test-func">> => <<"literal">>
        },
    Req = <<"test-func">>,
    {ok, Res} = hb_ao:resolve(Base, Req, #{ <<"cache-control">> => [<<"always">>] }),
    ?assertEqual(CachedMsg, Res),
    {ok, Res2} = hb_ao:resolve(Base, Req, #{ <<"cache-control">> => [<<"only-if-cached">>] }),
    {ok, Res3} = hb_ao:resolve(Base, Req, #{ <<"cache-control">> => [<<"only-if-cached">>] }),
    ?assertEqual(CachedMsg, Res2),
    ?assertEqual(Res2, Res3).

cache_message_result_test() ->
    CachedMsg =
        #{
            <<"purpose">> => <<"Test-Message">>,
            <<"aux">> => #{ <<"aux-message">> => <<"Aux-Message-Value">> },
            <<"test-key">> => rand:uniform(1000000)
        },
    Base = #{ <<"test-key">> => CachedMsg, <<"local">> => <<"Binary">> },
    Req = <<"test-key">>,
    {ok, Res} =
        hb_ao:resolve(
            Base,
            Req,
            #{
                <<"cache-control">> => [<<"always">>]
            }
        ),
    ?event({res1, Res}),
    ?event(reading_from_cache),
    {ok, Res2} = hb_ao:resolve(Base, Req, #{ <<"cache-control">> => [<<"only-if-cached">>] }),
    ?event(reading_from_cache_again),
    {ok, Res3} = hb_ao:resolve(Base, Req, #{ <<"cache-control">> => [<<"only-if-cached">>] }),
    ?event({res2, Res2}),
    ?event({res3, Res3}),
    ?assertEqual(Res2, Res3).

%% @doc Changed results do not retain the input's commitments. Signed inputs
%% exercise a device that mutates them directly; unsigned inputs are stripped
%% before invocation. The cache contains each message under its own ID.
mangled_result_does_not_poison_cache_test() ->
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"cache-control">> => [<<"always">>],
        <<"priv-wallet">> => ar_wallet:new()
    },
    Base = #{ <<"device">> => <<"test-device@1.0">>, <<"counter">> => <<"1">> },
    lists:foreach(
        fun({Input, Req, Stored}) ->
            ID = hb_message:id(Input, all, Opts),
            {ok, Res} = hb_ao:resolve(Input, Req, Opts),
            Content = hb_message:uncommitted(hb_private:reset(Res), Opts),
            ?assertNotEqual(Base, Content),
            ?assertEqual([], hb_message:signers(Res, Opts)),
            ?assertEqual(not_found, hb_message:commitment(ID, Res, Opts)),
            ResID = hb_message:id(Res, all, Opts),
            ?assertNotEqual(ID, ResID),
            ?assertEqual(Base, read_content(ID, Opts)),
            case Stored of
                true -> ?assertEqual(Content, read_content(ResID, Opts));
                false ->
                    ?assertEqual({error, not_found}, hb_cache:read(ResID, Opts))
            end
        end,
        [
            % `mangle' gets a signed base, so its result is not stored.
            {hb_message:commit(Base, Opts), <<"mangle">>, false},
            {hb_message:normalize_commitments(Base, Opts),
                #{ <<"path">> => <<"set">>, <<"counter">> => <<"2">> }, true}
        ]
    ).

read_content(ID, Opts) ->
    {ok, Msg} = hb_cache:read(ID, Opts),
    hb_message:uncommitted(
        hb_private:reset(hb_cache:ensure_all_loaded(Msg, Opts)),
        Opts
    ).

%% @doc A signed request is split into steps that change its `path', and its
%% `cache-control' has the node store their results. The cache keeps the
%% signed content under the request's ID: a step that changes a signed key is
%% written under its own ID.
split_signed_request_does_not_poison_cache_test() ->
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"priv-wallet">> => ar_wallet:new()
    },
    Node = hb_http_server:start_node(Opts),
    Path = <<"/~meta@1.0/info/address">>,
    Signed =
        hb_message:commit(
            #{
                <<"path">> => Path,
                <<"x">> => <<"1">>,
                <<"cache-control">> => [<<"always">>]
            },
            Opts
        ),
    ID = hb_message:id(Signed, all, Opts),
    {ok, _} = hb_cache:write(Signed, Opts),
    {ok, Address} = hb_http:get(Node, Signed, Opts),
    ?assertEqual(
        hb_util:human_id(
            ar_wallet:to_address(hb_opts:get(priv_wallet, none, Opts))
        ),
        Address
    ),
    ?assertEqual(Path, hb_maps:get(<<"path">>, read_content(ID, Opts), Opts)).

partial_unsigned_request_cache_isolation_test() ->
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"attested-store">> => hb_test_utils:test_store(),
        <<"async-cache">> => false,
        <<"paranoid-verify">> => [cache_write],
        <<"spawn-worker">> => false
    },
    Base = #{ <<"a">> => 0 },
    CommittedPath =
        hb_message:commit(
            #{ <<"path">> => <<"set">> },
            Opts,
            #{ <<"type">> => <<"unsigned">> }
        ),
    FirstReq = CommittedPath#{
        <<"x">> => <<"first">>,
        <<"cache-control">> => [<<"always">>]
    },
    SecondReq = CommittedPath#{
        <<"x">> => <<"second">>,
        <<"cache-control">> => [<<"always">>]
    },
    ?assert(hb_message:verify(FirstReq, all, Opts)),
    ?assert(hb_message:verify(SecondReq, all, Opts)),
    {ok, First} = hb_ao:resolve(Base, FirstReq, Opts),
    {ok, Second} = hb_ao:resolve(Base, SecondReq, Opts),
    ?assertEqual(<<"first">>, hb_maps:get(<<"x">>, First, Opts)),
    ?assertEqual(<<"second">>, hb_maps:get(<<"x">>, Second, Opts)).

%% @doc Every computation address identifies the complete invocation inputs and
%% result. Partial commitments at any loaded depth cannot alias another call.
varied_input_identity_test_() ->
    [
        {setup,
            fun() -> #{
                <<"store">> => hb_test_utils:test_store(Store),
                <<"attested-store">> => hb_test_utils:test_store(Store),
                <<"cache-control">> => [<<"always">>],
                <<"return-context">> => true,
                <<"priv-wallet">> => ar_wallet:new()
            } end,
            fun(Opts) ->
                hb_store:reset([maps:get(<<"store">>, Opts),
                    maps:get(<<"attested-store">>, Opts)])
            end,
            fun(Opts) ->
                {atom_to_list(Store) ++ " " ++ atom_to_list(Type) ++ " " ++
                    binary_to_list(Which) ++ " " ++ atom_to_list(Depth),
                    fun() -> varied_input_identity(Type, Which, Depth, Opts) end}
            end
        }
    || Type <- [unsigned, stale, signed, multiple], Which <- [<<"base">>, <<"request">>],
        Depth <- [root, child, list],
        Store <- [hb_store_volatile, hb_store_fs, hb_store_lmdb]
    ].

varied_input_identity(Type, Which, Depth, Opts) ->
    Inputs = #{
        <<"base">> => #{ <<"device">> => <<"test-device@1.0">> },
        <<"request">> => #{ <<"path">> => <<"vary-inspect">> }
    },
    Plain = maps:get(Which, Inputs),
    Signed = Type == signed orelse Type == multiple,
    CommitType = case Signed of true -> <<"signed">>; false -> <<"unsigned">> end,
    Original = case Depth of root -> Plain; _ -> #{ <<"value">> => 1 } end,
    Initial = hb_message:commit(
        case Type of stale -> Original#{ <<"x">> => <<"original">> }; _ -> Original end,
        Opts, #{ <<"type">> => CommitType }),
    Part = case Type of
        multiple -> hb_message:normalize_commitments(
            hb_message:commit(Initial, Opts#{ <<"priv-wallet">> => ar_wallet:new() }), Opts);
        _ -> Initial
    end,
    Addresses = lists:map(fun(X) ->
        Extended = Part#{ <<"x">> => X },
        Input = case Depth of
            root -> Extended;
            child -> Plain#{ <<"child">> => Extended };
            list -> Plain#{ <<"child">> => [Extended] }
        end,
        Pair = Inputs#{ Which => Input },
        {ok, Ctx} = hb_ao:resolve(maps:get(<<"base">>, Pair),
            maps:get(<<"request">>, Pair), Opts),
        VB = maps:get(<<"varied-base">>, Ctx, maps:get(<<"base">>, Ctx)),
        VQ = maps:get(<<"varied-request">>, Ctx, maps:get(<<"request">>, Ctx)),
        VR = maps:get(<<"varied-result">>, Ctx),
        Echo = hb_maps:get(Which, VR, undefined, Opts),
        ?assertEqual(Signed andalso Depth == root,
            hb_maps:get(<<Which/binary, "-committed">>, VR, Opts)),
        case Signed andalso Depth == root of
            true -> ?assertEqual(hb_maps:get(<<"commitments">>, Input, Opts),
                hb_maps:get(<<"commitments">>, Echo, Opts));
            false -> ok
        end,
        ?assertEqual(cache_content(Input, Opts), cache_content(Echo, Opts)),
        BaseID = hb_message:id(VB, all, Opts),
        ReqID = hb_message:id(VQ, all, Opts),
        Path = <<BaseID/binary, "/", ReqID/binary>>,
        Link = hb_store:read(maps:get(<<"attested-store">>, Opts), Path, Opts),
        case Signed of
            true ->
                ?assertEqual({error, not_found}, Link);
            false ->
                {ok, <<"ao:", Target/binary>>} = Link,
                lists:foreach(fun({ID, Expected}) ->
                    {ok, Stored} = hb_cache:read(ID, Opts),
                    ?assertEqual(cache_content(Expected, Opts), cache_content(Stored, Opts))
                end, [{BaseID, VB}, {ReqID, VQ}, {Target, VR}]),
                ?assert(hb_hashpath:verify_all(hb_hashpath:format(Ctx, Opts), Opts)),
                {ok, Hit} = hb_ao:resolve(VB, VQ,
                    Opts#{ <<"cache-control">> => [<<"only-if-cached">>] }),
                ?assertEqual(cache_content(VR, Opts),
                    cache_content(maps:get(<<"varied-result">>, Hit), Opts))
        end,
        Path
    end, [<<"first">>, <<"second">>]),
    case Signed of
        false -> ?assertEqual(2, length(lists:usort(Addresses)));
        true -> ?assertEqual(1, length(lists:usort(Addresses)))
    end.

%% @doc Compare public content independently of lazy loading and commitments.
cache_content(Msg, Opts) ->
    hb_message:uncommitted_deep(
        hb_private:reset(hb_cache:ensure_all_loaded(Msg, Opts)), Opts).

%% @doc Adding unsigned child metadata cannot change a computation under the
%% same ID. JSON makes commitment presence observable as ordinary output bytes.
unsigned_child_metadata_test() ->
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"attested-store">> => hb_test_utils:test_store(),
        <<"cache-control">> => [<<"always">>]
    },
    Child = #{ <<"a">> => <<"1">> },
    Committed = hb_message:commit(Child, Opts, #{ <<"type">> => <<"unsigned">> }),
    {ok, ID} = hb_cache:write(Committed, Opts),
    Link = {link, <<"ao:", ID/binary>>, #{}},
    lists:foreach(fun(Wrap) ->
        Base = #{ <<"device">> => <<"json@1.0">>, <<"child">> => Wrap(Child) },
        Req = #{ <<"path">> => <<"serialize">>, <<"bundle">> => true },
        {ok, First} = hb_ao:resolve(Base, Req, Opts),
        lists:foreach(fun(Variant) ->
            Changed = Base#{ <<"child">> => Wrap(Variant) },
            {ok, Hit} = hb_ao:resolve(Changed, Req,
                Opts#{ <<"cache-control">> => [<<"only-if-cached">>] }),
            {ok, Fresh} = hb_ao:resolve(Changed, Req,
                Opts#{ <<"cache-control">> => [<<"no-cache">>, <<"no-store">>] }),
            Body = hb_maps:get(<<"body">>, First, Opts),
            ?assertEqual(Body, hb_maps:get(<<"body">>, Hit, Opts)),
            ?assertEqual(Body, hb_maps:get(<<"body">>, Fresh, Opts))
        end, [Committed, Link])
    end, [fun(X) -> X end, fun(X) -> #{ <<"nested">> => [X] } end]).

%% @doc A linked signed child must exclude a computation just as an inline
%% one does: its signature ID does not name its uncommitted extensions.
linked_signed_child_test_() ->
    [ {integer_to_list(Count) ++ " signatures",
        fun() -> linked_signed_child(Count) end} || Count <- [1, 2] ].

linked_signed_child(Count) ->
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"attested-store">> => hb_test_utils:test_store(),
        <<"cache-control">> => [<<"always">>],
        <<"priv-wallet">> => ar_wallet:new()
    },
    Signed = lists:foldl(fun(_, Acc) ->
        hb_message:commit(Acc, Opts#{ <<"priv-wallet">> => ar_wallet:new() })
    end, #{ <<"a">> => <<"1">> }, lists:seq(1, Count)),
    hb_cache:write(Signed, Opts),
    ID = hb_message:id(Signed, all, Opts),
    Base = #{ <<"device">> => <<"json@1.0">>,
        <<"child">> => {link, <<"ao:", ID/binary>>, #{}} },
    Req = #{ <<"path">> => <<"serialize">>, <<"bundle">> => true },
    {ok, _} = hb_ao:resolve(Base, Req, Opts),
    Extended = Base#{ <<"child">> => Signed#{ <<"extra">> => <<"2">> } },
    ?assertEqual(hb_message:id(Base, all, Opts),
        hb_message:id(Extended, all, Opts)),
    ?assertMatch({error, #{ <<"status">> := 504 }}, hb_ao:resolve(Extended, Req,
        Opts#{ <<"cache-control">> => [<<"only-if-cached">>] })).

%% @doc A result with two signatures is linked, and reloads with both of them.
multiple_signed_result_test() ->
    Opts = #{ <<"store">> => hb_test_utils:test_store(),
        <<"attested-store">> => hb_test_utils:test_store(),
        <<"cache-control">> => [<<"always">>] },
    Base = hb_message:commit(#{ <<"a">> => 1 }, Opts,
        #{ <<"type">> => <<"unsigned">> }),
    Req = hb_message:commit(#{ <<"path">> => <<"set">>, <<"b">> => 2 }, Opts,
        #{ <<"type">> => <<"unsigned">> }),
    Res = lists:foldl(fun(_, Acc) ->
        hb_message:commit(Acc, Opts#{ <<"priv-wallet">> => ar_wallet:new() })
    end, #{ <<"a">> => 1, <<"b">> => 2 }, [1, 2]),
    maybe_store(Base, Req, Res, Req, Opts),
    {hit, {ok, Hit}} = hb_cache:read_resolved(Base, Req, Opts),
    ?assertEqual(
        hb_message:id(Res, all, Opts),
        hb_message:id(hb_cache:ensure_all_loaded(Hit, Opts), all, Opts)
    ).

%% @doc A device's fresh partial unsigned result must retain every returned
%% field on the next cache hit, even when its commitment was not an input's.
partial_unsigned_result_test() ->
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"attested-store">> => hb_test_utils:test_store(),
        <<"cache-control">> => [<<"always">>]
    },
    Base = #{ <<"a">> => <<"1">>, <<"extra">> => <<"2">> },
    Req = #{ <<"path">> => <<"commit">>, <<"type">> => <<"unsigned">>,
        <<"committed">> => [<<"a">>] },
    {ok, First} = hb_ao:resolve(Base, Req, Opts),
    {ok, Hit} = hb_ao:resolve(Base, Req,
        Opts#{ <<"cache-control">> => [<<"only-if-cached">>] }),
    ?assertEqual(Base, cache_content(First, Opts)),
    ?assertEqual(Base, cache_content(Hit, Opts)),
    ?assert(hb_hashpath:verify_all(hb_path:hashpath(First, Opts), Opts)).

%% @doc A signature removed by Vary does not exclude the unsigned computation.
projected_signed_input_test() ->
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"attested-store">> => hb_test_utils:test_store(),
        <<"cache-control">> => [<<"always">>],
        <<"return-context">> => true,
        <<"priv-wallet">> => ar_wallet:new()
    },
    Base = hb_message:commit(#{ <<"device">> => <<"test-device@1.0">>,
        <<"required">> => <<"7">>, <<"omitted">> => 1,
        <<"deep">> => #{ <<"slot">> => <<"8">> } }, Opts),
    {ok, Ctx} = hb_ao:resolve(Base,
        #{ <<"path">> => <<"vary-projection">>,
            <<"deep-request">> => #{ <<"slot">> => <<"9">> } }, Opts),
    VB = maps:get(<<"varied-base">>, Ctx),
    VQ = maps:get(<<"varied-request">>, Ctx),
    ?assertEqual([], hb_message:signers(VB, Opts)),
    ?assertEqual(7, hb_maps:get(<<"required">>, VB, Opts)),
    BaseID = hb_message:id(VB, all, Opts),
    ReqID = hb_message:id(VQ, all, Opts),
    {ok, <<"ao:", ID/binary>>} =
        hb_store:read(maps:get(<<"attested-store">>, Opts),
            <<BaseID/binary, "/", ReqID/binary>>, Opts),
    ?assertEqual(hb_message:id(maps:get(<<"varied-result">>, Ctx), all, Opts), ID),
    ?assert(hb_hashpath:verify_all(hb_hashpath:format(Ctx, Opts), Opts)).

%% @doc A new signed result remains intact; only complete results are reusable.
signed_result_storage_test() ->
    lists:foreach(fun(Keys) ->
        Opts = #{
            <<"store">> => hb_test_utils:test_store(),
            <<"attested-store">> => hb_test_utils:test_store(),
            <<"cache-control">> => [<<"always">>],
            <<"priv-wallet">> => ar_wallet:new()
        },
        Base = #{ <<"a">> => <<"1">>, <<"extra">> => <<"2">> },
        Req = #{ <<"path">> => <<"commit">>, <<"committed">> => Keys },
        {ok, Res} = hb_ao:resolve(Base, Req, Opts),
        ?assertEqual(Base, cache_content(Res, Opts)),
        ?assertEqual(1, length(hb_message:signers(Res, Opts))),
        ?assert(hb_message:verify(Res, all, Opts)),
        Cached = hb_ao:resolve(Base, Req,
            Opts#{ <<"cache-control">> => [<<"only-if-cached">>] }),
        case Keys of
            [<<"a">>] -> ?assertMatch({error, #{ <<"status">> := 504 }}, Cached);
            _ ->
                {ok, Hit} = Cached,
                ?assertEqual(Base, cache_content(Hit, Opts)),
                ?assertEqual(hb_message:id(Res, all, Opts), hb_message:id(Hit, all, Opts))
        end
    end, [[<<"a">>], [<<"a">>, <<"extra">>]]).

%% @doc Repeated asynchronous writes from one caller must all reach the store.
async_computation_writes_test() ->
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"attested-store">> => hb_test_utils:test_store(),
        <<"cache-control">> => [<<"always">>],
        <<"async-cache">> => true
    },
    Base = #{ <<"a">> => 1 },
    lists:foreach(fun(X) ->
        Req = #{ <<"path">> => <<"set">>, <<"x">> => X },
        {ok, Res} = hb_ao:resolve(Base, Req, Opts),
        CachedOpts = Opts#{ <<"cache-control">> => [<<"only-if-cached">>] },
        ?assert(hb_util:wait_until(fun() ->
            case hb_ao:resolve(Base, Req, CachedOpts) of
                {ok, Hit} -> cache_content(Res, Opts) == cache_content(Hit, Opts);
                _ -> false
            end
        end, 2000))
    end, [<<"first">>, <<"second">>]).
