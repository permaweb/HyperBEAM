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
        #{ <<"lookup">> := false } ->
            ?event({skip_cache_check, lookup_disabled}),
            maybe_load_base(Base, Req, Opts);
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
                    case fresh(Res, Req, Opts) of
                        true ->
                            CachedRes = cached_result(Res, OutputScopedOpts),
                            ?event(caching,
                                {cache_hit,
                                    {base, Base},
                                    {req, Req},
                                    {res, CachedRes}
                                }
                            ),
                            {ok, CachedRes};
                        false ->
                            ?event(caching, {stale_cache_result, Base, Req}),
                            cache_miss(Base, Req, Settings, Opts)
                    end;
                _ ->
                    ?event(caching, {result_cache_miss, Base, Req}),
                    cache_miss(Base, Req, Settings, Opts)
            end
    end.

%%% Internal functions

%% @doc Return whether a cached result satisfies the requested `max-age'.
fresh(Res, Req, Opts) ->
    case hb_maps:get(<<"max-age">>, Req, infinity, Opts) of
        infinity -> true;
        <<"infinity">> -> true;
        RawMaxAge ->
            try
                CreatedAt = hb_util:int(
                    hb_maps:get(<<"priv-created-at">>, Res, Opts)
                ),
                MaxAge = hb_util:int(RawMaxAge),
                Now = os:system_time(second),
                true = CreatedAt >= 0,
                true = CreatedAt =< Now,
                true = MaxAge >= 0,
                Now - CreatedAt < MaxAge
            catch
                _:_ -> false
            end
    end.

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
    TracksFreshness = hb_maps:is_key(<<"max-age">>, Req, Opts),
    CacheAddress = hb_cache:resolved_address(Base, Req, Opts),
    case {Res, TracksFreshness} of
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
            hb_cache:write_hashpath(
                CacheAddress,
                maps:remove(<<"priv-created-at">>, Map),
                Opts
            );
        _ ->
            ?event({cannot_write_result, Res}),
            skip_caching
    end.

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

%% @doc Parse Cache-Control values while preserving each original directive.
parse_cache_control(undefined) -> [];
parse_cache_control(RawList) when is_list(RawList) ->
    lists:append(lists:map(fun parse_cache_control/1, RawList));
parse_cache_control(Raw) ->
    try
        Bin = hb_util:bin(Raw),
        lists:filtermap(
            fun parse_cache_directive/1,
            hb_util:split_depth_string_aware($,, Bin)
        )
    catch
        _:_ -> []
    end.

%% @doc Parse one Cache-Control directive with Cowlib and a tolerant fallback.
parse_cache_directive(RawDirective) ->
    Directive = string:trim(RawDirective),
    case Directive of
        <<>> -> false;
        _ ->
            try cow_http_hd:parse_cache_control(Directive) of
                [Parsed] -> {true, normalize_cache_directive(Parsed, Directive)};
                _ -> parse_legacy_cache_directive(Directive)
            catch
                _:_ -> parse_legacy_cache_directive(Directive)
            end
    end.

%% @doc Normalize a Cowlib directive without creating input-derived atoms.
normalize_cache_directive({RawName, RawValue}, Raw) ->
    Name = normalize_cache_directive_name(RawName),
    {
        Name,
        normalize_cache_directive_value(Name, RawValue),
        Raw
    };
normalize_cache_directive(RawName, Raw) ->
    {normalize_cache_directive_name(RawName), true, Raw}.

%% @doc Normalize only the case-insensitive directive name.
normalize_cache_directive_name(Name) ->
    hb_ao:normalize_key(string:lowercase(Name)).

%% @doc Bound numeric parser output while preserving extension values.
normalize_cache_directive_value(_Name, Value) when is_integer(Value) ->
    min(max(0, Value), ?MAX_DELTA_SECONDS);
normalize_cache_directive_value(Name, Value) ->
    Unquoted = hb_util:unquote(Value),
    case lists:member(
        Name,
        [
            <<"max-age">>,
            <<"max-stale">>,
            <<"min-fresh">>,
            <<"stale-while-revalidate">>,
            <<"stale-if-error">>
        ]
    ) of
        false -> Unquoted;
        true ->
            case parse_delta_seconds(Unquoted) of
                invalid -> Unquoted;
                Delta -> Delta
            end
    end.

%% @doc Parse OWS around `=' and quoted commas rejected by strict Cowlib.
parse_legacy_cache_directive(Directive) ->
    case hb_util:split_depth_string_aware_single($=, Directive) of
        {no_match, RawName, <<>>} ->
            {
                true,
                {
                    normalize_cache_directive_name(string:trim(RawName)),
                    true,
                    Directive
                }
            };
        {_Match, RawName, RawValue} ->
            Name = normalize_cache_directive_name(string:trim(RawName)),
            Value = normalize_cache_directive_value(
                Name,
                string:trim(RawValue)
            ),
            {true, {Name, Value, Directive}}
    end.

%% @doc Return all normalized values for one directive name.
directive_values(Name, Parsed) ->
    [Value || {Directive, Value, _Raw} <- Parsed, Directive =:= Name].

%% @doc Return whether a directive is present, regardless of its argument.
has_directive(Name, Parsed) ->
    directive_values(Name, Parsed) =/= [].

%% @doc Parse one non-negative delta-seconds value conservatively.
parse_delta_seconds(Value) when is_integer(Value), Value >= 0 ->
    min(Value, ?MAX_DELTA_SECONDS);
parse_delta_seconds(Value) when is_binary(Value) ->
    Trimmed = string:trim(hb_util:unquote(Value)),
    case is_decimal(Trimmed) of
        false -> invalid;
        true when byte_size(Trimmed) > 10 -> ?MAX_DELTA_SECONDS;
        true -> min(binary_to_integer(Trimmed), ?MAX_DELTA_SECONDS)
    end;
parse_delta_seconds(_) ->
    invalid.

%% @doc Return whether a binary is a non-empty ASCII decimal number.
is_decimal(<<>>) -> false;
is_decimal(Bin) ->
    lists:all(
        fun(Char) -> Char >= $0 andalso Char =< $9 end,
        binary_to_list(Bin)
    ).

%% @doc Parse a directive that must occur once with a delta-seconds value.
numeric_directive(Name, Parsed) ->
    case directive_values(Name, Parsed) of
        [] -> undefined;
        [Value] -> parse_delta_seconds(Value);
        _ -> invalid
    end.

%% @doc Parse request max-stale, including its valueless form.
max_stale_directive(Parsed) ->
    case directive_values(<<"max-stale">>, Parsed) of
        [] -> absent;
        [true] -> any;
        [Value] ->
            case parse_delta_seconds(Value) of
                invalid -> absent;
                Delta -> Delta
            end;
        _ -> absent
    end.

%% @doc Combine header and compatibility max-age using the stricter value.
combine_max_age(undefined, undefined) -> undefined;
combine_max_age(invalid, _) -> invalid;
combine_max_age(_, invalid) -> invalid;
combine_max_age(undefined, Legacy) -> Legacy;
combine_max_age(Header, undefined) -> Header;
combine_max_age(Header, Legacy) -> min(Header, Legacy).

%% @doc Parse the legacy top-level request max-age compatibility field.
legacy_max_age(Msg, Opts) ->
    case hb_maps:find(<<"max-age">>, Msg, Opts) of
        error -> undefined;
        {ok, <<"infinity">>} -> undefined;
        {ok, infinity} -> undefined;
        {ok, Value} -> parse_delta_seconds(Value)
    end.

%% @doc Build the request-side cache policy without response directives.
request_policy(Msg, Opts) ->
    Parsed = message_cache_control(Msg, Opts),
    HeaderMaxAge = numeric_directive(<<"max-age">>, Parsed),
    #{
        <<"max-age">> =>
            combine_max_age(HeaderMaxAge, legacy_max_age(Msg, Opts)),
        <<"max-stale">> => max_stale_directive(Parsed),
        <<"min-fresh">> => numeric_directive(<<"min-fresh">>, Parsed),
        <<"no-cache">> => has_directive(<<"no-cache">>, Parsed),
        <<"no-store">> => has_directive(<<"no-store">>, Parsed),
        <<"only-if-cached">> =>
            has_directive(<<"only-if-cached">>, Parsed)
    }.

%% @doc Build the response-side cache policy without request directives.
response_policy(Msg, Opts) ->
    Parsed = message_cache_control(Msg, Opts),
    #{
        <<"max-age">> => numeric_directive(<<"max-age">>, Parsed),
        <<"no-cache">> => has_directive(<<"no-cache">>, Parsed),
        <<"no-store">> => has_directive(<<"no-store">>, Parsed),
        <<"private">> => has_directive(<<"private">>, Parsed),
        <<"must-revalidate">> =>
            has_directive(<<"must-revalidate">>, Parsed)
    }.

%% @doc Parse the Cache-Control field of an AO message when present.
message_cache_control(Msg, Opts) ->
    case hb_maps:find(<<"cache-control">>, Msg, Opts) of
        {ok, Raw} -> parse_cache_control(Raw);
        _ -> []
    end.

%% @doc Classify a cached result using already loaded timing metadata.
classify_cached(Res, ReqPolicy, ResPolicy, Now) ->
    case cache_policy_prohibition(ReqPolicy, ResPolicy) of
        none -> classify_cached_age(Res, ReqPolicy, ResPolicy, Now);
        Reason -> {unacceptable, Reason}
    end.

%% @doc Return the first rule that requires validation or prohibits reuse.
cache_policy_prohibition(ReqPolicy, ResPolicy) ->
    case {
        maps:get(<<"no-cache">>, ReqPolicy),
        maps:get(<<"no-store">>, ResPolicy),
        maps:get(<<"private">>, ResPolicy),
        maps:get(<<"no-cache">>, ResPolicy),
        maps:get(<<"max-age">>, ReqPolicy),
        maps:get(<<"min-fresh">>, ReqPolicy),
        maps:get(<<"max-age">>, ResPolicy)
    } of
        {true, _, _, _, _, _, _} -> request_no_cache;
        {_, true, _, _, _, _, _} -> response_no_store;
        {_, _, true, _, _, _, _} -> response_private;
        {_, _, _, true, _, _, _} -> response_no_cache;
        {_, _, _, _, invalid, _, _} -> invalid_request_max_age;
        {_, _, _, _, _, invalid, _} -> invalid_request_min_fresh;
        {_, _, _, _, _, _, invalid} -> invalid_response_max_age;
        _ -> none
    end.

%% @doc Calculate age only when some request or response rule needs it.
classify_cached_age(Res, ReqPolicy, ResPolicy, Now) ->
    case age_required(ReqPolicy, ResPolicy) of
        false -> {fresh, undefined};
        true ->
            case cached_age(Res, Now) of
                {ok, Age} -> classify_cached_lifetime(Age, ReqPolicy, ResPolicy);
                error -> {unacceptable, invalid_created_at}
            end
    end.

%% @doc Return whether the decision depends upon a stored timestamp.
age_required(ReqPolicy, ResPolicy) ->
    maps:get(<<"max-age">>, ReqPolicy) =/= undefined orelse
        maps:get(<<"min-fresh">>, ReqPolicy) =/= undefined orelse
        maps:get(<<"max-stale">>, ReqPolicy) =/= absent orelse
        maps:get(<<"max-age">>, ResPolicy) =/= undefined orelse
        maps:get(<<"must-revalidate">>, ResPolicy).

%% @doc Read a bounded immediate timestamp without invoking the cache/store.
cached_age(Res, Now) when is_map(Res), is_integer(Now), Now >= 0 ->
    try hb_util:int(maps:get(<<"priv-created-at">>, Res)) of
        CreatedAt when CreatedAt >= 0, CreatedAt =< Now -> {ok, Now - CreatedAt};
        _ -> error
    catch
        _:_ -> error
    end;
cached_age(_, _) ->
    error.

%% @doc Apply request ceilings before the response freshness lifetime.
classify_cached_lifetime(Age, ReqPolicy, ResPolicy) ->
    case maps:get(<<"max-age">>, ReqPolicy) of
        MaxAge when is_integer(MaxAge), Age > MaxAge ->
            {unacceptable, request_max_age};
        _ ->
            classify_response_lifetime(
                Age,
                ReqPolicy,
                ResPolicy,
                maps:get(<<"max-age">>, ResPolicy)
            )
    end.

%% @doc Preserve legacy hits when no response lifetime-dependent rule exists.
classify_response_lifetime(Age, ReqPolicy, ResPolicy, undefined) ->
    case {
        maps:get(<<"min-fresh">>, ReqPolicy),
        maps:get(<<"max-stale">>, ReqPolicy),
        maps:get(<<"must-revalidate">>, ResPolicy)
    } of
        {undefined, absent, false} -> {fresh, Age};
        _ -> {unacceptable, response_lifetime_missing}
    end;
classify_response_lifetime(Age, ReqPolicy, ResPolicy, Lifetime) ->
    case min_fresh_satisfied(Age, Lifetime, ReqPolicy) of
        false -> {unacceptable, min_fresh};
        true when Age < Lifetime -> {fresh, Age};
        true -> classify_stale(Age, Lifetime, ReqPolicy, ResPolicy)
    end.

%% @doc Require both freshness and the requested remaining lifetime.
min_fresh_satisfied(Age, Lifetime, ReqPolicy) ->
    case maps:get(<<"min-fresh">>, ReqPolicy) of
        undefined -> true;
        MinFresh -> Age < Lifetime andalso Lifetime - Age >= MinFresh
    end.

%% @doc Permit stale reuse only through request max-stale.
classify_stale(Age, Lifetime, ReqPolicy, ResPolicy) ->
    case maps:get(<<"must-revalidate">>, ResPolicy) of
        true -> {unacceptable, must_revalidate};
        false ->
            StaleBy = Age - Lifetime,
            case maps:get(<<"max-stale">>, ReqPolicy) of
                any -> {stale_allowed, Age, StaleBy, max_stale};
                MaxStale when is_integer(MaxStale), StaleBy =< MaxStale ->
                    {stale_allowed, Age, StaleBy, max_stale};
                _ -> {unacceptable, stale}
            end
    end.

%% @doc Convert Cache-Control input into legacy store and lookup settings.
specifiers_to_cache_settings(CCSpecifier) ->
    Parsed = parse_cache_control(CCSpecifier),
    #{
        <<"store">> =>
            case has_directive(<<"always">>, Parsed) of
                true -> true;
                false ->
                    case has_directive(<<"no-store">>, Parsed) of
                        true -> false;
                        false ->
                            case has_directive(<<"store">>, Parsed) of
                                true -> true;
                                false -> undefined
                            end
                    end
            end,
        <<"lookup">> =>
            case has_directive(<<"always">>, Parsed) of
                true -> true;
                false ->
                    case has_directive(<<"no-cache">>, Parsed) of
                        true -> false;
                    false ->
                        case has_directive(<<"cache">>, Parsed) of
                            true -> true;
                            false -> undefined
                        end
                    end
            end,
        <<"only-if-cached">> =>
            case has_directive(<<"only-if-cached">>, Parsed) of
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
    {ok, _} = hb_cache:write_resolved(
        CacheAddress,
        Result,
        os:system_time(second) - 61,
        Opts
    ),
    {ok, Refreshed} = hb_ao:resolve(Base, Req, Opts),
    ?assertEqual(
        not_found,
        hb_maps:get(<<"priv-created-at">>, Refreshed, not_found, Opts)
    ),
    ?assertNotEqual(Served, Refreshed).

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

cache_map_without_max_age_is_reusable_test() ->
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
    {hit, {ok, Cached}} = hb_cache:read_resolved(Base, Req, Opts),
    ?assertEqual(false, maps:is_key(<<"priv-created-at">>, Cached)),
    CacheOnlyOpts = Opts#{ <<"cache-control">> => [<<"only-if-cached">>] },
    ?assertMatch(
        {error, #{ <<"status">> := 504, <<"cache-status">> := <<"miss">> }},
        maybe_lookup(Base, ReqWithMaxAge, CacheOnlyOpts)
    ),
    {ok, Served} = maybe_lookup(Base, Req, CacheOnlyOpts),
    ?assertEqual(<<"cached">>, hb_maps:get(<<"value">>, Served, Opts)),
    ?assertEqual(false, maps:is_key(<<"priv-created-at">>, Served)).

fresh_uses_loaded_metadata_without_store_read_test() ->
    Parent = self(),
    Tracer = spawn(fun() -> cache_control_trace_forwarder(Parent) end),
    erlang:trace(self(), true, [call, {tracer, Tracer}]),
    erlang:trace_pattern({hb_store, read, 3}, true, []),
    IsFresh =
        try
            fresh(
                #{
                    <<"priv-created-at">> =>
                        integer_to_binary(os:system_time(second))
                },
                #{ <<"max-age">> => 60 },
                #{}
            )
        after
            stop_cache_control_trace(Tracer)
        end,
    ?assert(IsFresh),
    receive
        {trace_event, {trace, _, call, {hb_store, read, _}}} ->
            ?assert(false)
    after 0 ->
        ok
    end.

%% @doc Disable store-read tracing and stop its forwarding process.
stop_cache_control_trace(Tracer) ->
    erlang:trace_pattern({hb_store, read, 3}, false, []),
    erlang:trace(self(), false, [call]),
    TraceRef = erlang:trace_delivered(self()),
    Delivered =
        receive
            {trace_delivered, _, TraceRef} -> true
        after 1000 ->
            false
        end,
    Tracer ! {flush, self()},
    Flushed =
        receive
            trace_flushed -> true
        after 1000 ->
            false
        end,
    Tracer ! stop,
    case {Delivered, Flushed} of
        {true, true} -> ok;
        {false, _} -> error(trace_delivery_timeout);
        {_, false} -> error(trace_flush_timeout)
    end.

%% @doc Missing or invalid private timestamps fail closed when max-age applies.
freshness_private_metadata_validation_test() ->
    Now = os:system_time(second),
    Req = #{ <<"max-age">> => 60 },
    ?assert(fresh(#{}, #{}, #{})),
    ?assertNot(fresh(#{}, Req, #{})),
    ?assertNot(fresh(#{ <<"priv-created-at">> => <<"invalid">> }, Req, #{})),
    ?assertNot(fresh(#{ <<"priv-created-at">> => <<"-1">> }, Req, #{})),
    ?assertNot(
        fresh(
            #{ <<"priv-created-at">> => integer_to_binary(Now + 1) },
            Req,
            #{}
        )
    ),
    ?assert(
        fresh(
            #{ <<"priv-created-at">> => integer_to_binary(Now) },
            Req,
            #{}
        )
    ).

%% @doc Forward test trace events and provide an ordered flush barrier.
cache_control_trace_forwarder(Parent) ->
    receive
        {flush, ReplyTo} ->
            ReplyTo ! trace_flushed,
            cache_control_trace_forwarder(Parent);
        stop ->
            ok;
        TraceEvent ->
            Parent ! {trace_event, TraceEvent},
            cache_control_trace_forwarder(Parent)
    end.

%% @doc Parse wire and list Cache-Control forms without losing quoted commas.
cache_control_parser_regression_test() ->
    Parsed = parse_cache_control(
        <<"MaX-aGe = \"60\", only-if-cached, community = \"A,B\"">>
    ),
    ?assertEqual([60], directive_values(<<"max-age">>, Parsed)),
    ?assertEqual([true], directive_values(<<"only-if-cached">>, Parsed)),
    ?assertEqual([<<"A,B">>], directive_values(<<"community">>, Parsed)),
    ListParsed = parse_cache_control([<<"always">>, <<"NO-STORE">>]),
    ?assertEqual([true], directive_values(<<"always">>, ListParsed)),
    ?assertEqual([true], directive_values(<<"no-store">>, ListParsed)).

%% @doc Request policy handles numeric, bare, duplicate, and invalid forms.
request_cache_policy_regression_test() ->
    Policy = request_policy(
        #{
            <<"cache-control">> =>
                <<"max-age=60, max-stale=10, min-fresh=5, only-if-cached">>
        },
        #{}
    ),
    ?assertEqual(60, maps:get(<<"max-age">>, Policy)),
    ?assertEqual(10, maps:get(<<"max-stale">>, Policy)),
    ?assertEqual(5, maps:get(<<"min-fresh">>, Policy)),
    ?assert(maps:get(<<"only-if-cached">>, Policy)),
    Bare = request_policy(
        #{ <<"cache-control">> => <<"max-stale">> },
        #{}
    ),
    ?assertEqual(any, maps:get(<<"max-stale">>, Bare)),
    Duplicate = request_policy(
        #{ <<"cache-control">> => <<"max-age=60, max-age=30">> },
        #{}
    ),
    ?assertEqual(invalid, maps:get(<<"max-age">>, Duplicate)),
    Invalid = request_policy(
        #{ <<"cache-control">> => <<"min-fresh=-1, max-stale=nope">> },
        #{}
    ),
    ?assertEqual(invalid, maps:get(<<"min-fresh">>, Invalid)),
    ?assertEqual(absent, maps:get(<<"max-stale">>, Invalid)),
    Overflow = request_policy(
        #{ <<"cache-control">> => <<"max-age=999999999999999999999">> },
        #{}
    ),
    ?assertEqual(2147483647, maps:get(<<"max-age">>, Overflow)).

%% @doc Response policy remains independent from request-only directives.
response_cache_policy_regression_test() ->
    Policy = response_policy(
        #{
            <<"cache-control">> =>
                <<"max-age=60, no-cache=\"body\", private=\"secret\", "
                  "must-revalidate, max-stale=99">>
        },
        #{}
    ),
    ?assertEqual(60, maps:get(<<"max-age">>, Policy)),
    ?assert(maps:get(<<"no-cache">>, Policy)),
    ?assert(maps:get(<<"private">>, Policy)),
    ?assert(maps:get(<<"must-revalidate">>, Policy)),
    ?assertEqual(false, maps:is_key(<<"max-stale">>, Policy)),
    Duplicate = response_policy(
        #{ <<"cache-control">> => <<"max-age=60, max-age=30">> },
        #{}
    ),
    ?assertEqual(invalid, maps:get(<<"max-age">>, Duplicate)).

%% @doc Freshness boundaries use explicit time and the correct policy side.
cache_freshness_classifier_boundaries_test() ->
    Now = 1000,
    Response60 = response_policy(
        #{ <<"cache-control">> => <<"max-age=60">> },
        #{}
    ),
    EmptyRequest = request_policy(#{}, #{}),
    ?assertEqual(
        {fresh, 59},
        classify_cached(cache_entry(Now - 59), EmptyRequest, Response60, Now)
    ),
    ?assertEqual(
        {unacceptable, stale},
        classify_cached(cache_entry(Now - 60), EmptyRequest, Response60, Now)
    ),
    Request60 = request_policy(#{ <<"max-age">> => 60 }, #{}),
    ?assertEqual(
        {fresh, 60},
        classify_cached(
            cache_entry(Now - 60),
            Request60,
            response_policy(
                #{ <<"cache-control">> => <<"max-age=120">> },
                #{}
            ),
            Now
        )
    ),
    Request0 = request_policy(#{ <<"max-age">> => 0 }, #{}),
    ?assertEqual(
        {fresh, 0},
        classify_cached(cache_entry(Now), Request0, Response60, Now)
    ),
    MinFresh = request_policy(
        #{ <<"cache-control">> => <<"min-fresh=10">> },
        #{}
    ),
    ?assertEqual(
        {fresh, 50},
        classify_cached(cache_entry(Now - 50), MinFresh, Response60, Now)
    ).

%% @doc Max-stale is measured from the response freshness lifetime.
cache_max_stale_classifier_regression_test() ->
    Now = 1000,
    Response60 = response_policy(
        #{ <<"cache-control">> => <<"max-age=60">> },
        #{}
    ),
    MaxStale10 = request_policy(
        #{ <<"cache-control">> => <<"max-stale=10">> },
        #{}
    ),
    ?assertEqual(
        {stale_allowed, 70, 10, max_stale},
        classify_cached(cache_entry(Now - 70), MaxStale10, Response60, Now)
    ),
    ?assertEqual(
        {unacceptable, stale},
        classify_cached(cache_entry(Now - 71), MaxStale10, Response60, Now)
    ),
    Bare = request_policy(
        #{ <<"cache-control">> => <<"max-stale">> },
        #{}
    ),
    ?assertEqual(
        {stale_allowed, 500, 440, max_stale},
        classify_cached(cache_entry(Now - 500), Bare, Response60, Now)
    ),
    ?assertEqual(
        {unacceptable, response_lifetime_missing},
        classify_cached(
            cache_entry(Now - 1),
            Bare,
            response_policy(#{}, #{}),
            Now
        )
    ).

%% @doc Age-dependent decisions reject unusable co-located metadata.
cache_freshness_metadata_regression_test() ->
    Now = 1000,
    Req = request_policy(#{ <<"max-age">> => 60 }, #{}),
    Res = response_policy(#{}, #{}),
    lists:foreach(
        fun(Candidate) ->
            ?assertEqual(
                {unacceptable, invalid_created_at},
                classify_cached(Candidate, Req, Res, Now)
            )
        end,
        [
            #{},
            #{ <<"priv-created-at">> => <<"invalid">> },
            #{ <<"priv-created-at">> => <<"-1">> },
            #{ <<"priv-created-at">> => <<"1001">> },
            #{ <<"priv-created-at">> => {link, <<"missing">>} },
            <<"binary-candidate">>
        ]
    ).

%% @doc Build a deterministic cached map for classifier tests.
cache_entry(CreatedAt) ->
    #{ <<"priv-created-at">> => integer_to_binary(CreatedAt) }.
