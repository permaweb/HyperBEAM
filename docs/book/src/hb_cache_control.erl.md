# hb_cache_control

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_cache_control.erl)

Cache control logic for the AO-Core resolver. It derives cache settings
from request, response, execution-local node Opts, as well as the global
node Opts. It applies these settings when asked to maybe store/lookup in 
response to a request.

---

## Exported Functions

- `maybe_lookup/3`
- `maybe_store/4`

---

### maybe_store

Cache control logic for the AO-Core resolver. It derives cache settings
Write a resulting M3 message to the cache if requested. The precedence

```erlang
maybe_store(Msg1, Msg2, Msg3, Opts) ->
    case derive_cache_settings([Msg3, Msg2], Opts) of
        #{ <<"store">> := true } ->
            ?event(caching, {caching_result, {msg1, Msg1}, {msg2, Msg2}, {msg3, Msg3}}),
            dispatch_cache_write(Msg1, Msg2, Msg3, Opts);
        _ -> 
            not_caching
    end.
```

### maybe_lookup

Handles cache lookup, modulated by the caching options requested by

```erlang
maybe_lookup(Msg1, Msg2, Opts) ->
    case exec_likely_faster_heuristic(Msg1, Msg2, Opts) of
        true ->
            ?event(caching, {skip_cache_check, exec_likely_faster_heuristic}),
            {continue, Msg1, Msg2};
        false -> lookup(Msg1, Msg2, Opts)
    end.
```

### lookup

```erlang
lookup(Msg1, Msg2, Opts) ->
    case derive_cache_settings([Msg1, Msg2], Opts) of
        #{ <<"lookup">> := false } ->
            ?event({skip_cache_check, lookup_disabled}),
            {continue, Msg1, Msg2};
        Settings = #{ <<"lookup">> := true } ->
            OutputScopedOpts = 
                hb_store:scope(
                    Opts,
                    hb_opts:get(store_scope_resolved, local, Opts)
                ),
            case hb_cache:read_resolved(Msg1, Msg2, OutputScopedOpts) of
                {ok, Msg3} ->
                    ?event(caching,
                        {cache_hit,
                            case is_binary(Msg3) of
                                true -> hb_path:hashpath(Msg1, Msg2, Opts);
                                false -> hb_path:hashpath(Msg3, Opts)
                            end,
                            {msg1, Msg1},
                            {msg2, Msg2},
                            {msg3, Msg3}
                        }
                    ),
                    {ok, Msg3};
                not_found ->
                    ?event(caching, {result_cache_miss, Msg1, Msg2}),
                    case Settings of
                        #{ <<"only-if-cached">> := true } ->
                            only_if_cached_not_found_error(Msg1, Msg2, Opts);
                        _ ->
                            case ?IS_ID(Msg1) of
                                    false -> {continue, Msg1, Msg2};
                                    true ->
                                        case hb_cache:read(Msg1, Opts) of
                                            {ok, FullMsg1} ->
                                                ?event(load_message,
                                                    {cache_hit_base_message_load,
                                                        {base_id, Msg1},
                                                        {base_loaded, FullMsg1}
                                                    }
                                                ),
                                                {continue, FullMsg1, Msg2};
                                            not_found ->
                                                necessary_messages_not_found_error(
                                                    Msg1,
                                                    Msg2,
                                                    Opts
                                                )
                                        end
                                end
                        end
            end
    end.
```

### dispatch_cache_write

Dispatch the cache write to a worker process if requested.

```erlang
dispatch_cache_write(Msg1, Msg2, Msg3, Opts) ->
    case hb_opts:get(async_cache, false, Opts) of
        true ->
            find_or_spawn_async_writer(Opts) ! {write, Msg1, Msg2, Msg3, Opts},
            ok;
        false ->
            perform_cache_write(Msg1, Msg2, Msg3, Opts)
    end.
```

### find_or_spawn_async_writer

Find our async cacher process, or spawn one if none exists.

```erlang
find_or_spawn_async_writer(_Opts) ->
    case erlang:get({hb_cache_control, async_writer}) of
        undefined ->
            PID = spawn(fun() -> async_writer() end),
            erlang:put({hb_cache_control, async_writer}, PID),
            PID;
        PID ->
            PID
    end.
```

### async_writer

Optional worker process to write messages to the cache.

```erlang
async_writer() ->
    receive
        {write, Msg1, Msg2, Msg3, Opts} ->
            perform_cache_write(Msg1, Msg2, Msg3, Opts);
        stop -> ok
    end.
```

### perform_cache_write

Internal function to write a compute result to the cache.

```erlang
perform_cache_write(Msg1, Msg2, Msg3, Opts) ->
    hb_cache:write(Msg1, Opts),
    hb_cache:write(Msg2, Opts),
    case Msg3 of
        <<_/binary>> ->
            hb_cache:write_binary(
                hb_path:hashpath(Msg1, Msg2, Opts),
                Msg3,
                Opts
            );
        Map when is_map(Map) ->
            hb_cache:write(Msg3, Opts);
        _ ->
            ?event({cannot_write_result, Msg3}),
            skip_caching
    end.
```

### only_if_cached_not_found_error

Generate a message to return when `only_if_cached` was specified, and

```erlang
only_if_cached_not_found_error(Msg1, Msg2, Opts) ->
    ?event(
        caching,
        {only_if_cached_execution_failed, {msg1, Msg1}, {msg2, Msg2}},
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
```

### necessary_messages_not_found_error

Generate a message to return when the necessary messages to execute a 

```erlang
necessary_messages_not_found_error(Msg1, Msg2, Opts) ->
    ?event(
        load_message,
        {necessary_messages_not_found, {msg1, Msg1}, {msg2, Msg2}},
        Opts
    ),
    {error,
        #{
            <<"status">> => 404,
            <<"body">> =>
                <<"Necessary messages not found in cache.">>
        }
    }.
```

### exec_likely_faster_heuristic

Determine whether we are likely to be faster looking up the result in

```erlang
exec_likely_faster_heuristic(M1, _M2, _) when (not ?IS_ID(M1)) ->
    true;
```

### exec_likely_faster_heuristic

Determine whether we are likely to be faster looking up the result in

```erlang
exec_likely_faster_heuristic({as, _, Msg1}, Msg2, Opts) ->
    exec_likely_faster_heuristic(Msg1, Msg2, Opts);
```

### exec_likely_faster_heuristic

Determine whether we are likely to be faster looking up the result in

```erlang
exec_likely_faster_heuristic(Msg1, Msg2, Opts) ->
    case hb_opts:get(cache_lookup_hueristics, true, Opts) of
        false -> false;
        true ->
            case ?IS_ID(Msg1) of
                true -> false;
                false -> is_explicit_lookup(Msg1, Msg2, Opts)
            end
    end.
```

### is_explicit_lookup

```erlang
is_explicit_lookup(Msg1, #{ <<"path">> := Key }, Opts) ->
    % For now, just check whether the key is explicitly in the map. That is 
    % a good signal that we will likely be asked by the device to grab it.
```

### derive_cache_settings

Derive cache settings from a series of option sources and the opts,

```erlang
derive_cache_settings(SourceList, Opts) ->
    lists:foldr(
        fun(Source, Acc) ->
            maybe_set(Acc, cache_source_to_cache_settings(Source, Opts), Opts)
        end,
        #{ <<"store">> => ?DEFAULT_STORE_OPT, <<"lookup">> => ?DEFAULT_LOOKUP_OPT },
        [{opts, Opts}|lists:filter(fun erlang:is_map/1, SourceList)]
    ).
```

### maybe_set

Takes a key and two maps, returning the first map with the key set to

```erlang
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
```

### cache_source_to_cache_settings

Convert a cache source to a cache setting. The setting _must_ always be

```erlang
cache_source_to_cache_settings({opts, Opts}, _) ->
    CCMap = specifiers_to_cache_settings(hb_opts:get(cache_control, [], Opts)),
    case hb_opts:get(hashpath, update, Opts) of
        ignore -> CCMap#{ <<"store">> => false };
        _ -> CCMap
    end;
```

### cache_source_to_cache_settings

Convert a cache source to a cache setting. The setting _must_ always be

```erlang
cache_source_to_cache_settings(Msg, Opts) ->
    case dev_message:get(<<"cache-control">>, Msg, Opts) of
        {ok, CC} -> specifiers_to_cache_settings(CC);
        {error, not_found} -> #{}
    end.
```

### specifiers_to_cache_settings

Convert a cache control list as received via HTTP headers into a 

```erlang
specifiers_to_cache_settings(CCSpecifier) when not is_list(CCSpecifier) ->
    specifiers_to_cache_settings([CCSpecifier]);
```

### specifiers_to_cache_settings

Convert a cache control list as received via HTTP headers into a 

```erlang
specifiers_to_cache_settings(RawCCList) ->
    CCList = lists:map(fun hb_ao:normalize_key/1, RawCCList),
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
```

### msg_with_cc

```erlang
msg_with_cc(CC) -> #{ <<"cache-control">> => CC }.
```

### opts_with_cc

```erlang
opts_with_cc(CC) -> #{ cache_control => CC }.
%% Test precedence order (Opts > Msg3 > Msg2)
```

### opts_override_message_settings_test

```erlang
opts_override_message_settings_test() ->
    Msg2 = msg_with_cc([<<"no-store">>]),
    Msg3 = msg_with_cc([<<"no-cache">>]),
    Opts = opts_with_cc([<<"always">>]),
    Result = derive_cache_settings([Msg3, Msg2], Opts),
    ?assertEqual(#{<<"store">> => true, <<"lookup">> => true}, Result).
```

### msg_precidence_overrides_test

```erlang
msg_precidence_overrides_test() ->
    Msg2 = msg_with_cc([<<"always">>]),
    Msg3 = msg_with_cc([<<"no-store">>]),  % No restrictions
    Result = derive_cache_settings([Msg3, Msg2], opts_with_cc([])),
    ?assertEqual(#{<<"store">> => false, <<"lookup">> => true}, Result).
%% Test specific directives
```

### no_store_directive_test

```erlang
no_store_directive_test() ->
    Msg = msg_with_cc([<<"no-store">>]),
    Result = derive_cache_settings([Msg], opts_with_cc([])),
    ?assertEqual(#{<<"store">> => false, <<"lookup">> => ?DEFAULT_LOOKUP_OPT}, Result).
```

### no_cache_directive_test

```erlang
no_cache_directive_test() ->
    Msg = msg_with_cc([<<"no-cache">>]),
    Result = derive_cache_settings([Msg], opts_with_cc([])),
    ?assertEqual(#{<<"store">> => ?DEFAULT_STORE_OPT, <<"lookup">> => false}, Result).
```

### only_if_cached_directive_test

```erlang
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
```

### hashpath_ignore_prevents_storage_test

```erlang
hashpath_ignore_prevents_storage_test() ->
    Opts = (opts_with_cc([]))#{hashpath => ignore},
    Result = derive_cache_settings([], Opts),
    ?assertEqual(#{<<"store">> => ?DEFAULT_STORE_OPT, <<"lookup">> => ?DEFAULT_LOOKUP_OPT}, Result).
%% Test multiple directives
```

### multiple_directives_test

```erlang
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
```

### empty_message_list_test

```erlang
empty_message_list_test() ->
    Result = derive_cache_settings([], opts_with_cc([])),
    ?assertEqual(#{<<"store">> => ?DEFAULT_STORE_OPT, <<"lookup">> => ?DEFAULT_LOOKUP_OPT}, Result).
```

### message_without_cache_control_test

```erlang
message_without_cache_control_test() ->
    Result = derive_cache_settings([#{}], opts_with_cc([])),
    ?assertEqual(#{<<"store">> => ?DEFAULT_STORE_OPT, <<"lookup">> => ?DEFAULT_LOOKUP_OPT}, Result).
%% Test the cache_source_to_cache_setting function directly
```

### opts_source_cache_control_test

```erlang
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
```

### message_source_cache_control_test

```erlang
message_source_cache_control_test() ->
    Msg = msg_with_cc([<<"no-cache">>]),
    Result = cache_source_to_cache_settings(Msg, #{}),
    ?assertEqual(#{
        <<"store">> => undefined,
        <<"lookup">> => false,
        <<"only-if-cached">> => undefined
    }, Result).
```

### cache_binary_result_test

```erlang
cache_binary_result_test() ->
    CachedMsg = <<"test-message">>,
    Msg1 = #{ <<"test-key">> => CachedMsg },
    Msg2 = <<"test-key">>,
    {ok, Res} = hb_ao:resolve(Msg1, Msg2, #{ cache_control => [<<"always">>] }),
    ?assertEqual(CachedMsg, Res),
    {ok, Res2} = hb_ao:resolve(Msg1, Msg2, #{ cache_control => [<<"only-if-cached">>] }),
    {ok, Res3} = hb_ao:resolve(Msg1, Msg2, #{ cache_control => [<<"only-if-cached">>] }),
    ?assertEqual(CachedMsg, Res2),
    ?assertEqual(Res2, Res3).
```

### cache_message_result_test

```erlang
cache_message_result_test() ->
    CachedMsg =
        #{
            <<"purpose">> => <<"Test-Message">>,
            <<"aux">> => #{ <<"aux-message">> => <<"Aux-Message-Value">> },
            <<"test-key">> => rand:uniform(1000000)
        },
    Msg1 = #{ <<"test-key">> => CachedMsg, <<"local">> => <<"Binary">> },
    Msg2 = <<"test-key">>,
    {ok, Res} =
        hb_ao:resolve(
            Msg1,
            Msg2,
            #{
                cache_control => [<<"always">>]
            }
        ),
    ?event({res1, Res}),
    ?event(reading_from_cache),
    {ok, Res2} = hb_ao:resolve(Msg1, Msg2, #{ cache_control => [<<"only-if-cached">>] }),
    ?event(reading_from_cache_again),
    {ok, Res3} = hb_ao:resolve(Msg1, Msg2, #{ cache_control => [<<"only-if-cached">>] }),
    ?event({res2, Res2}),
    ?event({res3, Res3}),
```

---

*Generated from [hb_cache_control.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_cache_control.erl)*
