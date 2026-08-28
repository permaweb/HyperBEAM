%%% @doc Cache control logic for the AO-Core resolver. It derives cache settings
%%% from request, response, execution-local node Opts, as well as the global
%%% node Opts. It applies these settings when asked to maybe store/lookup in 
%%% response to a request.
-module(hb_cache_control).
-export([maybe_store/4, maybe_lookup/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% When other cache control settings are not specified, we default to the
%%% following settings.
-define(DEFAULT_STORE_OPT, false).
-define(DEFAULT_LOOKUP_OPT,  true).
-define(MAX_DELTA_SECONDS, 2147483647).

%%% Public API

%% @doc Write a resulting M3 message to the cache if requested. The precedence
%% order of cache control sources is as follows:
%% 1. The `Opts' map (letting the node operator have the final say).
%% 2. The `Res' results message (granted by Base's device).
%% 3. The `Req' message (the user's request).
%% Base is not used, such that it can specify cache control information about 
%% itself, without affecting its outputs.
maybe_store(Base, Req, Res, Opts) ->
    case derive_cache_settings([Res, Req], Opts) of
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
%%      `max-age':        If set, cached results must have a sufficiently
%%                        recent `priv-created-at' timestamp.
maybe_lookup(Base, Req, Opts) ->
    case exec_likely_faster_heuristic(Base, Req, Opts) of
        true ->
            ?event(caching, {skip_cache_check, exec_likely_faster_heuristic}),
            {continue, Base, Req};
        false -> lookup(Base, Req, Opts)
    end.

lookup(Base, Req, Opts) ->
    case derive_cache_settings([Base, Req], Opts) of
        Settings = #{ <<"lookup">> := false } ->
            ?event({skip_cache_check, lookup_disabled}),
            case derive_cache_settings(
                [Base, Req], Opts#{ <<"only">> => local }
            ) of
                #{ <<"lookup">> := false, <<"only-if-cached">> := true } ->
                    cache_miss(Base, Req, Settings, Opts);
                _ -> maybe_load_base(Base, Req, Opts)
            end;
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
                    ReqPolicy = request_policy(Req, Opts),
                    ResPolicy = response_policy(Res, OutputScopedOpts),
                    case classify_cached(
                        Res,
                        ReqPolicy,
                        ResPolicy,
                        os:system_time(second),
                        OutputScopedOpts
                    ) of
                        Decision when element(1, Decision) =:= fresh;
                                      element(1, Decision) =:= stale_allowed ->
                            CachedRes = cached_result(Res, OutputScopedOpts),
                            ?event(caching,
                                {cache_hit,
                                    {base, Base},
                                    {req, Req},
                                    {res, CachedRes}
                                }
                            ),
                            {ok, CachedRes};
                        {unacceptable, _Reason} ->
                            ?event(caching, {stale_cache_result, Base, Req}),
                            cache_miss(Base, Req, Settings, Opts)
                    end;
                _ ->
                    ?event(caching, {result_cache_miss, Base, Req}),
                    cache_miss(Base, Req, Settings, Opts)
            end
    end.

%%% Internal functions

%% @doc Classify a loaded result using request and response age policy.
classify_cached(Res, ReqPolicy, ResPolicy, Now, Opts) ->
    case {
        maps:get(<<"max-age">>, ReqPolicy),
        maps:get(<<"max-stale">>, ReqPolicy),
        maps:get(<<"max-age">>, ResPolicy),
        maps:get(<<"no-cache">>, ResPolicy)
    } of
        {_, _, _, true} -> {unacceptable, response_no_cache};
        {invalid, _, _, _} -> {unacceptable, invalid_request_max_age};
        {_, invalid, _, _} -> {unacceptable, invalid_request_max_stale};
        {_, _, invalid, _} -> {unacceptable, invalid_response_max_age};
        {undefined, absent, undefined, false} -> {fresh, undefined};
        _ -> classify_cached_age(Res, ReqPolicy, ResPolicy, Now, Opts)
    end.

%% @doc Apply the request ceiling and response freshness lifetime.
classify_cached_age(Res, ReqPolicy, ResPolicy, Now, Opts) ->
    case cached_age(Res, Now, Opts) of
        error -> {unacceptable, invalid_created_at};
        {ok, Age} ->
            case maps:get(<<"max-age">>, ReqPolicy) of
                MaxAge when is_integer(MaxAge), Age > MaxAge ->
                    {unacceptable, request_max_age};
                _ ->
                    classify_response_age(Age, ReqPolicy, ResPolicy)
            end
    end.

%% @doc Classify freshness and any request-permitted staleness.
classify_response_age(Age, ReqPolicy, ResPolicy) ->
    case maps:get(<<"max-age">>, ResPolicy) of
        undefined ->
            case maps:get(<<"max-stale">>, ReqPolicy) of
                absent -> {fresh, Age};
                _ -> {unacceptable, response_lifetime_missing}
            end;
        Lifetime when Age < Lifetime ->
            {fresh, Age};
        Lifetime ->
            case maps:get(<<"must-revalidate">>, ResPolicy) of
                true -> {unacceptable, must_revalidate};
                false -> classify_stale(Age, Lifetime, ReqPolicy)
            end
    end.

%% @doc Permit stale reuse only when max-stale allows the excess age.
classify_stale(Age, Lifetime, ReqPolicy) ->
    StaleBy = Age - Lifetime,
    case maps:get(<<"max-stale">>, ReqPolicy) of
        any -> {stale_allowed, Age, StaleBy};
        MaxStale when is_integer(MaxStale), StaleBy =< MaxStale ->
            {stale_allowed, Age, StaleBy};
        _ -> {unacceptable, stale}
    end.

%% @doc Calculate age from a cache-owned timestamp.
cached_age(Res, Now, Opts) when is_map(Res), is_integer(Now), Now >= 0 ->
    try hb_util:int(hb_maps:get(<<"priv-created-at">>, Res, Opts)) of
        CreatedAt when CreatedAt >= 0, CreatedAt =< Now -> {ok, Now - CreatedAt};
        _ -> error
    catch
        _:_ -> error
    end;
cached_age(_, _, _) -> error.

%% @doc Return the request max-age policy.
request_max_age(Req, Opts) ->
    case hb_maps:get(<<"max-age">>, Req, undefined, Opts) of
        undefined -> undefined;
        infinity -> undefined;
        <<"infinity">> -> undefined;
        Value -> parse_delta_seconds(Value)
    end.

%% @doc Return the request max-stale policy.
request_max_stale(Req, Opts) ->
    case hb_maps:get(<<"max-stale">>, Req, undefined, Opts) of
        undefined -> absent;
        true -> any;
        Value -> parse_delta_seconds(Value)
    end.

%% @doc Read freshness policy directly from the request.
request_policy(Req, Opts) ->
    #{
        <<"max-age">> => request_max_age(Req, Opts),
        <<"max-stale">> => request_max_stale(Req, Opts)
    }.

%% @doc Read the response lifetime and rules that prohibit reuse.
response_policy(Res, Opts) ->
    Parsed = message_cache_control(Res, Opts),
    #{
        <<"max-age">> => numeric_directive(<<"max-age">>, Parsed),
        <<"no-cache">> => has_directive(<<"no-cache">>, Parsed),
        <<"must-revalidate">> =>
            has_directive(<<"must-revalidate">>, Parsed)
    }.

cache_miss(Base, Req, #{ <<"only-if-cached">> := true }, Opts) ->
    only_if_cached_not_found_error(Base, Req, Opts);
cache_miss(Base, Req, _Settings, Opts) ->
    maybe_load_base(Base, Req, Opts).

%% @doc Remove private freshness and restore separately stored commitments.
cached_result(Res, Opts) when is_map(Res) ->
    PublicRes = maps:remove(<<"priv-created-at">>, Res),
    WithCommitments = hb_cache:read_all_commitments(PublicRes, Opts),
    case maps:get(<<"commitments">>, WithCommitments, #{}) of
        Commitments when map_size(Commitments) > 0 -> WithCommitments;
        _ -> PublicRes
    end;
cached_result(Res, _Opts) ->
    Res.

%% @doc Load an ID base required to execute the request.
maybe_load_base(Base, Req, _Opts) when not ?IS_ID(Base) ->
    {continue, Base, Req};
maybe_load_base(Base, Req, Opts) ->
    case hb_cache:read(Base, Opts) of
        {ok, FullBase} ->
            ?event(load_message,
                {cache_hit_base_message_load,
                    {base_id, Base},
                    {base_loaded, FullBase}
                }
            ),
            {continue, FullBase, Req};
        {error, not_found} ->
            necessary_messages_not_found_error(Base, Req, Opts)
    end.

%% @doc Dispatch the cache write to a worker process if requested.
%% Invoke the appropriate cache write function based on the type of the message.
dispatch_cache_write(Base, Req, Res, Opts) ->
    case hb_opts:get(async_cache, false, Opts) of
        true ->
            find_or_spawn_async_writer(Opts) ! {write, Base, Req, Res, Opts},
            ok;
        false ->
            perform_cache_write(Base, Req, Res, Opts)
    end.

%% @doc Find our async cacher process, or spawn one if none exists.
find_or_spawn_async_writer(_Opts) ->
    case erlang:get({hb_cache_control, async_writer}) of
        undefined ->
            PID = spawn(fun() -> async_writer() end),
            erlang:put({hb_cache_control, async_writer}, PID),
            PID;
        PID ->
            PID
    end.

%% @doc Optional worker process to write messages to the cache.
async_writer() ->
    receive
        {write, Base, Req, Res, Opts} ->
            perform_cache_write(Base, Req, Res, Opts);
        stop -> ok
    end.

%% @doc Internal function to write a compute result to the cache.
perform_cache_write(Base, Req, Res, Opts) ->
    hb_cache:write(Base, Opts),
    hb_cache:write(Req, Opts),
    StorableRes = case Res of
        Candidate when is_map(Candidate) ->
            {ok, StorableMap} = hb_message:with_only_committed(Candidate, Opts),
            StorableMap;
        _ -> Res
    end,
    TracksFreshness = tracks_freshness(
        request_policy(Req, Opts),
        response_policy(StorableRes, Opts)
    ),
    CacheAddress = hb_cache:resolved_address(Base, Req, Opts),
    WriteResult = case {Res, TracksFreshness} of
        {<<_/binary>>, _} ->
            hb_cache:write_binary(
                CacheAddress,
                Res,
                Opts
            );
        {Map, true} when is_map(Map) ->
            hb_cache:write_resolved(
                CacheAddress,
                Map,
                os:system_time(second),
                Opts
            );
        {Map, false} when is_map(Map) ->
            hb_cache:write(
                maps:remove(<<"priv-created-at">>, Map),
                Opts
            );
        _ ->
            ?event({cannot_write_result, Res}),
            skip_caching
    end,
    mirror_base_id_address(
        Base,
        Req,
        CacheAddress,
        WriteResult,
        TracksFreshness,
        Opts
    ).

%% @doc Mirror freshness-tracked results only; untracked maps stay canonical.
mirror_base_id_address(
        Base, Req, CacheAddress, {ok, Path} = Result, true, Opts
    ) when is_map(Base) ->
    IDAddress = hb_cache:resolved_address(
        hb_message:id(Base, all, Opts),
        Req,
        Opts
    ),
    case IDAddress =:= CacheAddress of
        true -> Result;
        false ->
            hb_cache:link(Path, IDAddress, Opts),
            Result
    end;
mirror_base_id_address(_Base, _Req, _CacheAddress, Result, _Tracked, _Opts) ->
    Result.

%% @doc Return whether a cached map needs co-located age metadata.
tracks_freshness(ReqPolicy, ResPolicy) ->
    is_integer(maps:get(<<"max-age">>, ReqPolicy)) orelse
        is_integer(maps:get(<<"max-age">>, ResPolicy)).

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

%% @doc Generate a message to return when the necessary messages to execute a 
%% cache lookup are not found in the cache.
necessary_messages_not_found_error(Base, Req, Opts) ->
    ?event(
        load_message,
        {necessary_messages_not_found, {base, Base}, {req, Req}},
        Opts
    ),
    {error,
        #{
            <<"status">> => 404,
            <<"body">> =>
                <<"Necessary messages not found in cache.">>
        }
    }.

%% @doc Determine whether we are likely to be faster looking up the result in
%% our cache (hoping we have it), or executing it directly.
exec_likely_faster_heuristic(_M1, _M2, _) ->
    false;
exec_likely_faster_heuristic({as, _, Base}, Req, Opts) ->
    exec_likely_faster_heuristic(Base, Req, Opts);
exec_likely_faster_heuristic(Base, Req, Opts) ->
    case hb_opts:get(cache_lookup_hueristics, true, Opts) of
        false -> false;
        true ->
            case ?IS_ID(Base) of
                true -> false;
                false -> is_explicit_lookup(Base, Req, Opts)
            end
    end.
is_explicit_lookup(Base, #{ <<"path">> := Key }, Opts) ->
    % For now, just check whether the key is explicitly in the map. That is 
    % a good signal that we will likely be asked by the device to grab it.
    % If we have `only-if-cached' in the opts, we always force lookup, too.
    case specifiers_to_cache_settings(hb_opts:get(cache_control, [], Opts)) of
        #{ <<"only-if-cached">> := true } -> false;
        _ -> is_map(Base) andalso hb_maps:is_key(Key, Base, Opts)
    end.

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

%% @doc Parse an AO list or a raw Cache-Control field.
parse_cache_control(RawList) when is_list(RawList) ->
    Parsed = [parse_cache_control(Raw) || Raw <- RawList],
    case lists:member(invalid, Parsed) of
        true -> invalid;
        false -> lists:append(Parsed)
    end;
parse_cache_control(Raw) ->
    try cow_http_hd:parse_cache_control(hb_util:bin(Raw))
    catch _:_ -> invalid
    end.

%% @doc Parse one bounded non-negative delta-seconds value.
parse_delta_seconds(Value)
        when is_integer(Value), Value >= 0, Value =< ?MAX_DELTA_SECONDS ->
    Value;
parse_delta_seconds(Value) when is_binary(Value) ->
    try parse_delta_seconds(binary_to_integer(string:trim(Value)))
    catch _:_ -> invalid end;
parse_delta_seconds(_) -> invalid.

%% @doc Return one numeric directive, rejecting malformed duplicates.
numeric_directive(_Name, invalid) -> invalid;
numeric_directive(Name, Parsed) ->
    case {lists:member(Name, Parsed), [Value || {Key, Value} <- Parsed, Key =:= Name]} of
        {false, []} -> undefined;
        {false, [Value]} -> parse_delta_seconds(Value);
        _ -> invalid
    end.

%% @doc Return whether one argumentless directive is present.
has_directive(_Name, invalid) -> false;
has_directive(Name, Parsed) -> lists:member(Name, Parsed).

%% @doc Parse Cache-Control directly from a map-shaped message.
message_cache_control(Msg, Opts) when is_map(Msg) ->
    parse_cache_control(hb_maps:get(<<"cache-control">>, Msg, [], Opts));
message_cache_control(_, _) -> [].

%% @doc Convert a cache control list as received via HTTP headers into a
%% normalized map of simply whether we should store and/or lookup the result.
specifiers_to_cache_settings(CCSpecifier) ->
    CCList = parse_cache_control(CCSpecifier),
    #{
        <<"store">> =>
            case has_directive(<<"always">>, CCList) of
                true -> true;
                false ->
                    case has_directive(<<"no-store">>, CCList) of
                        true -> false;
                        false ->
                            case has_directive(<<"store">>, CCList) of
                                true -> true;
                                false -> undefined
                            end
                    end
            end,
        <<"lookup">> =>
            case has_directive(<<"always">>, CCList) of
                true -> true;
                false ->
                    case has_directive(<<"no-cache">>, CCList) of
                        true -> false;
                    false ->
                        case has_directive(<<"cache">>, CCList) of
                            true -> true;
                            false -> undefined
                        end
                    end
            end,
        <<"only-if-cached">> =>
            case has_directive(<<"only-if-cached">>, CCList) of
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
    CachedMsg = <<"test-message">>,
    Base = #{ <<"test-key">> => CachedMsg },
    Req = <<"test-key">>,
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

cache_result_max_age_test() ->
    Opts = #{
        <<"store">> => [hb_test_utils:test_store()],
        <<"cache-control">> => [<<"always">>]
    },
    Base = #{ <<"device">> => <<"test-device@1.0">>, <<"name">> => <<"HB">> },
    Req = #{ <<"path">> => <<"index">>, <<"max-age">> => 60 },
    Result = #{
        <<"value">> => <<"cached">>,
        <<"priv-created-at">> =>
            integer_to_binary(os:system_time(second) + 3600)
    },
    {ok, _} = perform_cache_write(Base, Req, Result, Opts),
    {hit, {ok, Cached}} = hb_cache:read_resolved(Base, Req, Opts),
    CreatedAt = hb_util:int(hb_maps:get(<<"priv-created-at">>, Cached, Opts)),
    ?assert(is_integer(CreatedAt)),
    {ok, Served} = hb_ao:resolve(Base, Req, Opts),
    ?assertEqual(
        not_found,
        hb_maps:get(<<"priv-created-at">>, Served, not_found, Opts)
    ),
    CacheAddress = hb_cache:resolved_address(Base, Req, Opts),
    StaleCreatedAt = os:system_time(second) - 61,
    {ok, _} = hb_cache:write_resolved(
        CacheAddress,
        Result,
        StaleCreatedAt,
        Opts
    ),
    ?assertMatch(
        {error, #{ <<"status">> := 504 }},
        hb_ao:resolve(
            Base,
            Req,
            Opts#{ <<"cache-control">> => [<<"only-if-cached">>] }
        )
    ),
    {ok, Refreshed} = hb_ao:resolve(Base, Req, Opts),
    ?assertEqual(
        not_found,
        hb_maps:get(<<"priv-created-at">>, Refreshed, not_found, Opts)
    ),
    {hit, {ok, RefreshedEntry}} = hb_cache:read_resolved(Base, Req, Opts),
    ?assert(
        hb_util:int(hb_maps:get(
            <<"priv-created-at">>,
            RefreshedEntry,
            Opts
        )) > StaleCreatedAt
    ).

cache_result_priv_created_at_regression_test() ->
    Wallet = ar_wallet:new(),
    Opts = #{
        <<"store">> => [hb_test_utils:test_store()],
        <<"priv-wallet">> => Wallet,
        <<"cache-control">> => [<<"always">>]
    },
    Base = #{ <<"device">> => <<"test-device@1.0">>, <<"name">> => <<"HB">> },
    Req = #{ <<"path">> => <<"index">>, <<"max-age">> => 3600 },
    Result = hb_message:commit(
        #{
            <<"body">> => <<"cached">>,
            <<"content-type">> => <<"text/plain">>,
            <<"created-at">> => <<"application-value">>,
            <<"status">> => 200
        },
        Opts
    ),
    {ok, _} = perform_cache_write(Base, Req, Result, Opts),
    CacheAddress = hb_cache:resolved_address(Base, Req, Opts),
    {ok, _} = hb_cache:write_resolved(
        CacheAddress,
        Result,
        os:system_time(second) - 1000,
        Opts
    ),
    {hit, {ok, CachedEntry}} = hb_cache:read_resolved(Base, Req, Opts),
    CachedTimestamp = hb_maps:get(<<"priv-created-at">>, CachedEntry, Opts),
    ?assert(is_integer(hb_util:int(CachedTimestamp))),
    Parent = self(),
    EventHandler = #{
        <<"device">> => #{
            event =>
                fun(_, EventReq, _) ->
                    case {
                        maps:get(<<"module">>, EventReq),
                        maps:get(<<"body">>, EventReq)
                    } of
                        {?MODULE, {cache_hit, _, _, _} = Event} ->
                            Parent ! {cache_hit_event, Event};
                        _ -> ok
                    end,
                    {ok, EventReq}
                end
        }
    },
    CacheOnlyOpts = Opts#{
        <<"cache-control">> => [<<"only-if-cached">>],
        <<"on">> => #{ <<"event">> => EventHandler }
    },
    OldEventOpts = erlang:get({hb_event, event_opts}),
    erlang:put({hb_event, event_opts}, CacheOnlyOpts),
    Served =
        try
            {ok, CachedResult} = maybe_lookup(Base, Req, CacheOnlyOpts),
            CachedResult
        after
            case OldEventOpts of
                undefined -> erlang:erase({hb_event, event_opts});
                _ -> erlang:put({hb_event, event_opts}, OldEventOpts)
            end
        end,
    CacheHitEvent =
        receive
            {cache_hit_event, Event} -> Event
        after 1000 ->
            error(cache_hit_event_not_observed)
        end,
    EncodedEvent = term_to_binary(CacheHitEvent),
    ?assertEqual(nomatch, binary:match(EncodedEvent, <<"priv-created-at">>)),
    ?assertEqual(nomatch, binary:match(EncodedEvent, CachedTimestamp)),
    ?assertEqual(false, maps:is_key(<<"priv-created-at">>, Served)),
    ?assertEqual(
        <<"application-value">>,
        hb_maps:get(<<"created-at">>, Served, Opts)
    ),
    ?assertEqual(
        hb_message:id(Result, none, Opts),
        hb_message:id(Served, none, Opts)
    ),
    ?assertEqual(
        hb_message:id(Result, all, Opts),
        hb_message:id(Served, all, Opts)
    ),
    ?assertEqual(
        hb_maps:get(<<"commitments">>, Result, Opts),
        hb_maps:get(<<"commitments">>, Served, Opts)
    ),
    ?assert(hb_message:verify(Served, all, Opts)),
    {ok, _} = hb_cache:write_resolved(
        CacheAddress,
        Result,
        os:system_time(second) - 3601,
        Opts
    ),
    ?assertMatch(
        {error, #{ <<"status">> := 504, <<"cache-status">> := <<"miss">> }},
        maybe_lookup(Base, Req, CacheOnlyOpts)
    ),
    ?assertEqual(
        {error, not_found},
        hb_cache:read(<<"created-at/", CacheAddress/binary>>, Opts)
    ).

cache_binary_max_age_remains_available_without_age_test() ->
    Opts = #{
        <<"store">> => [hb_test_utils:test_store()],
        <<"cache-control">> => [<<"always">>]
    },
    Base = #{ <<"device">> => <<"test-device@1.0">>, <<"name">> => <<"HB">> },
    Req = #{ <<"path">> => <<"index">>, <<"max-age">> => 60 },
    ReqWithoutMaxAge = maps:remove(<<"max-age">>, Req),
    ?assertEqual(
        hb_cache:resolved_address(Base, ReqWithoutMaxAge, Opts),
        hb_cache:resolved_address(Base, Req, Opts)
    ),
    {ok, _} = perform_cache_write(Base, Req, <<"cached">>, Opts),
    CacheOnlyOpts = Opts#{ <<"cache-control">> => [<<"only-if-cached">>] },
    ?assertMatch(
        {error, #{ <<"status">> := 504, <<"cache-status">> := <<"miss">> }},
        maybe_lookup(Base, Req, CacheOnlyOpts)
    ),
    ?assertEqual(
        {ok, <<"cached">>},
        maybe_lookup(Base, ReqWithoutMaxAge, CacheOnlyOpts)
    ).

cache_map_tracked_then_reusable_without_max_age_test() ->
    Opts = #{
        <<"store">> => [hb_test_utils:test_store()],
        <<"cache-control">> => [<<"always">>]
    },
    Base = #{ <<"device">> => <<"test-device@1.0">>, <<"name">> => <<"HB">> },
    Req = #{ <<"path">> => <<"index">> },
    ReqWithMaxAge = Req#{ <<"max-age">> => 60 },
    Result = #{
        <<"value">> => <<"cached">>,
        <<"priv-created-at">> =>
            integer_to_binary(os:system_time(second) + 3600)
    },
    ?assertEqual(
        hb_cache:resolved_address(Base, Req, Opts),
        hb_cache:resolved_address(Base, ReqWithMaxAge, Opts)
    ),
    {ok, _} = perform_cache_write(Base, Req, Result, Opts),
    ResultID = hb_message:id(
        maps:remove(<<"priv-created-at">>, Result),
        all,
        Opts
    ),
    {ok, Canonical} = hb_cache:read(ResultID, Opts),
    ?assertEqual(false, maps:is_key(<<"priv-created-at">>, Canonical)),
    ?assertEqual(miss, hb_cache:read_resolved(Base, Req, Opts)),
    {ok, _} = perform_cache_write(Base, ReqWithMaxAge, Result, Opts),
    {hit, {ok, Cached}} = hb_cache:read_resolved(Base, Req, Opts),
    ?assert(maps:is_key(<<"priv-created-at">>, Cached)),
    CacheOnlyOpts = Opts#{ <<"cache-control">> => [<<"only-if-cached">>] },
    {ok, Served} = maybe_lookup(Base, Req, CacheOnlyOpts),
    ?assertEqual(<<"cached">>, hb_maps:get(<<"value">>, Served, Opts)),
    ?assertEqual(false, maps:is_key(<<"priv-created-at">>, Served)).

%% @doc Cover the exact freshness, stale, and prohibition boundaries.
minimal_freshness_policy_test() ->
    Req0 = #{ <<"max-age">> => undefined, <<"max-stale">> => absent },
    Res0 = #{ <<"max-age">> => undefined, <<"no-cache">> => false,
        <<"must-revalidate">> => false },
    Res60 = Res0#{ <<"max-age">> => 60 },
    Cases = [
        {{fresh, 59}, cached_at(41), Req0, Res60},
        {{unacceptable, stale}, cached_at(40), Req0, Res60},
        {{fresh, 60}, cached_at(40), Req0#{ <<"max-age">> => 60 }, Res0},
        {{unacceptable, request_max_age}, cached_at(39), Req0#{ <<"max-age">> => 60 }, Res0},
        {{stale_allowed, 70, 10}, cached_at(30), Req0#{ <<"max-stale">> => 10 }, Res60},
        {{unacceptable, stale}, cached_at(29), Req0#{ <<"max-stale">> => 10 }, Res60},
        {{stale_allowed, 90, 30}, cached_at(10), Req0#{ <<"max-stale">> => any }, Res60},
        {{unacceptable, response_lifetime_missing}, cached_at(30), Req0#{ <<"max-stale">> => 10 }, Res0},
        {{unacceptable, response_no_cache}, cached_at(90), Req0, Res60#{ <<"no-cache">> => true }},
        {{unacceptable, must_revalidate}, cached_at(30), Req0#{ <<"max-stale">> => any },
            Res60#{ <<"must-revalidate">> => true }},
        {{unacceptable, invalid_created_at}, #{}, Req0, Res60},
        {{unacceptable, invalid_created_at}, #{ <<"priv-created-at">> => <<"invalid">> }, Req0, Res60},
        {{unacceptable, invalid_created_at}, cached_at(-1), Req0, Res60},
        {{unacceptable, invalid_created_at}, cached_at(101), Req0, Res60}
    ],
    lists:foreach(fun({Expected, Res, Req, Policy}) ->
        ?assertEqual(Expected, classify_cached(Res, Req, Policy, 100, #{}))
    end, Cases),
    ?assertEqual(
        {fresh, undefined},
        classify_cached(#{}, Req0, Res0, 100, #{})
    ).

%% @doc Parse request policy and keep only uncommitted policy out of identity.
minimal_policy_parsing_and_identity_test() ->
    Req = #{ <<"path">> => <<"index">>, <<"max-age">> => 60,
        <<"max-stale">> => true },
    ?assertEqual(#{ <<"max-age">> => 60, <<"max-stale">> => any },
        request_policy(Req, #{})),
    ?assertEqual(response_policy(#{ <<"cache-control">> => <<"max-age=60">> }, #{}),
        response_policy(#{ <<"cache-control">> => [<<"max-age=60">>] }, #{})),
    lists:foreach(fun(CC) ->
        Policy = response_policy(#{ <<"cache-control">> => CC }, #{}),
        ?assertEqual(invalid, maps:get(<<"max-age">>, Policy))
    end, [<<"max-age">>, <<"max-age=-1">>, <<"max-age=nope">>,
        <<"max-age=999999999999999999999">>, <<"max-age=60, max-age=30">>]),
    WalletOpts = #{ <<"priv-wallet">> => ar_wallet:new(),
        <<"commitment-device">> => <<"httpsig@1.0">> },
    Commit = fun(MaxAge) ->
        hb_message:commit(Req#{ <<"max-age">> => MaxAge }, WalletOpts,
            #{ <<"committed">> => [<<"path">>, <<"max-age">>, <<"max-stale">>] })
    end,
    Committed60 = Commit(60),
    Committed30 = Commit(30),
    ?assert(hb_message:verify(Committed60, all, WalletOpts)),
    Base = #{ <<"device">> => <<"test-device@1.0">> },
    ?assertEqual(
        hb_cache:resolved_address(Base, Req, #{}),
        hb_cache:resolved_address(Base, Req#{ <<"max-age">> => 30 }, #{})
    ),
    ?assertEqual(60, maps:get(<<"max-age">>,
        request_policy(Committed60, WalletOpts))),
    ?assertNotEqual(hb_cache:resolved_address(Base, Committed60, WalletOpts),
        hb_cache:resolved_address(Base, Committed30, WalletOpts)).

%% @doc Request freshness policy remains visible to the executing device.
request_policy_remains_in_device_input_test() ->
    Handler = fun(_, Req, _) -> {ok, Req} end,
    Base = #{ <<"device">> => #{ index => Handler } },
    Req = #{ <<"path">> => <<"index">>, <<"max-age">> => 60,
        <<"max-stale">> => true },
    {ok, Result} = hb_ao:resolve(Base, Req,
        #{ <<"cache-control">> => [<<"no-cache">>, <<"no-store">>] }),
    ?assertEqual(60, hb_maps:get(<<"max-age">>, Result, #{})),
    ?assertEqual(true, hb_maps:get(<<"max-stale">>, Result, #{})).

%% @doc An explicit cache-only request never executes with lookup disabled.
no_cache_only_if_cached_test() ->
    Base = #{ <<"device">> => <<"test-device@1.0">> },
    CacheOnlyReq = #{ <<"path">> => <<"index">>, <<"cache-control">> =>
        [<<"no-cache">>, <<"only-if-cached">>] },
    ?assertMatch({error, #{ <<"status">> := 504 }},
        hb_ao:resolve(Base, CacheOnlyReq, #{})).

inherited_no_cache_does_not_preempt_device_cache_only_test() ->
    Base = #{ <<"device">> => <<"test-device@1.0">>, <<"name">> => <<"HB">> },
    Req = #{ <<"path">> => <<"index">>, <<"cache-control">> => [<<"only-if-cached">>] },
    ?assertMatch({ok, _}, hb_ao:resolve(Base, Req, #{})).

%% @doc Response max-age stores time and max-stale controls live cache reuse.
cache_response_max_age_and_request_max_stale_test() ->
    Base = #{ <<"device">> => <<"test-device@1.0">>, <<"name">> => <<"HB">> },
    Req = #{ <<"path">> => <<"index">> },
    Result = #{ <<"body">> => <<"cached">>,
        <<"cache-control">> => <<"max-age=60">> },
    Opts = #{ <<"store">> => [hb_test_utils:test_store()],
        <<"cache-control">> => [<<"always">>] },
    {ok, _} = perform_cache_write(Base, Req, Result, Opts),
    Address = hb_cache:resolved_address(Base, Req, Opts),
    {hit, {ok, Stored}} = hb_cache:read_resolved(Base, Req, Opts),
    ?assert(is_integer(hb_util:int(
        hb_maps:get(<<"priv-created-at">>, Stored, Opts)))),
    {ok, _} = hb_cache:write_resolved(
        Address, Result, os:system_time(second) - 70, Opts),
    CacheOnly = Opts#{ <<"cache-control">> => [<<"only-if-cached">>] },
    AllowedReq = Req#{ <<"max-stale">> => 100 },
    {ok, _Stale} = maybe_lookup(Base, AllowedReq, CacheOnly),
    DeniedReq = Req#{ <<"max-stale">> => 0 },
    ?assertMatch(
        {error, #{ <<"status">> := 504 }},
        maybe_lookup(Base, DeniedReq, CacheOnly)
    ),
    ?assertMatch({continue, _, _}, maybe_lookup(Base, Req, Opts)).

%% @doc Uncommitted response policy cannot outlive signed cache storage.
signed_uncommitted_response_policy_is_not_tracked_test() ->
    Opts = #{ <<"priv-wallet">> => ar_wallet:new(),
        <<"commitment-device">> => <<"httpsig@1.0">>,
        <<"store">> => [hb_test_utils:test_store()] },
    Base = #{ <<"device">> => <<"test-device@1.0">> },
    Req = #{ <<"path">> => <<"index">> },
    Result = hb_message:commit(
        #{ <<"body">> => <<"cached">>, <<"cache-control">> => <<"max-age=60">> },
        Opts, #{ <<"committed">> => [<<"body">>] }),
    {ok, ResultID} = perform_cache_write(Base, Req, Result, Opts),
    {ok, Stored} = hb_cache:read(ResultID, Opts),
    ?assertNot(maps:is_key(<<"cache-control">>, Stored)),
    ?assertNot(maps:is_key(<<"priv-created-at">>, Stored)),
    ?assertEqual(miss, hb_cache:read_resolved(Base, Req, Opts)).

%% @doc Build an already-loaded cache record with a private creation time.
cached_at(CreatedAt) ->
    #{ <<"priv-created-at">> => integer_to_binary(CreatedAt) }.
