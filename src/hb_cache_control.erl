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
-define(DEFAULT_STORE_OPT,  false).
-define(DEFAULT_LOOKUP_OPT, false).

%%% Public API


%% @doc Write a resulting M3 message to the cache if requested. The precedence
%% order of cache control sources is as follows:
%% 1. The `Opts' map (letting the node operator have the final say).
%% 2. The `Msg3' results message (granted by Msg1's device).
%% 3. The `Msg2' message (the user's request).
%% Msg1 is not used, such that it can specify cache control information about
%% itself, without affecting its outputs.
maybe_store(Msg1, Msg2, Msg3, Opts) ->
    case derive_cache_settings([Msg3, Msg2], Opts) of
        #{<<"store">> := true} ->
            ?event(caching, {caching_result, {msg1, Msg1}, {msg2, Msg2}, {msg3, Msg3}}),
            dispatch_cache_write(Msg1, Msg2, Msg3, Opts);
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
maybe_lookup(Msg1, Msg2, Opts) ->
    case exec_likely_faster_heuristic(Msg1, Msg2, Opts) of
        true ->
            ?event(caching, {skip_cache_check, exec_likely_faster_heuristic}),
            {continue, Msg1, Msg2};
        false -> lookup(Msg1, Msg2, Opts)
    end.


lookup(Msg1, Msg2, Opts) ->
    case derive_cache_settings([Msg1, Msg2], Opts) of
        #{<<"lookup">> := false} ->
            ?event({skip_cache_check, lookup_disabled}),
            {continue, Msg1, Msg2};
        Settings = #{<<"lookup">> := true} ->
            OutputScopedOpts =
                hb_store:scope(
                  Opts,
                  hb_opts:get(store_scope_resolved, local, Opts)),
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
                            {msg3, Msg3}}),
                    {ok, Msg3};
                not_found ->
                    ?event(caching, {result_cache_miss, Msg1, Msg2}),
                    case Settings of
                        #{<<"only-if-cached">> := true} ->
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
                                                    {base_loaded, FullMsg1}}),
                                            {continue, FullMsg1, Msg2};
                                        not_found ->
                                            necessary_messages_not_found_error(
                                              Msg1,
                                              Msg2,
                                              Opts)
                                    end
                            end
                    end
            end
    end.


%%% Internal functions


%% @doc Dispatch the cache write to a worker process if requested.
%% Invoke the appropriate cache write function based on the type of the message.
dispatch_cache_write(Msg1, Msg2, Msg3, Opts) ->
    Dispatch =
        fun() ->
                hb_cache:write(Msg1, Opts),
                hb_cache:write(Msg2, Opts),
                case Msg3 of
                    <<_/binary>> ->
                        hb_cache:write_binary(
                          hb_path:hashpath(Msg1, Msg2, Opts),
                          Msg3,
                          Opts);
                    Map when is_map(Map) ->
                        hb_cache:write(Msg3, Opts);
                    _ ->
                        ?event({cannot_write_result, Msg3}),
                        skip_caching
                end
        end,
    case hb_opts:get(async_cache, false, Opts) of
        true -> spawn(Dispatch);
        false -> Dispatch()
    end.


%% @doc Generate a message to return when `only_if_cached' was specified, and
%% we don't have a cached result.
only_if_cached_not_found_error(Msg1, Msg2, Opts) ->
    ?event(
      caching,
      {only_if_cached_execution_failed, {msg1, Msg1}, {msg2, Msg2}},
      Opts),
    {error,
     #{
       <<"status">> => 504,
       <<"cache-status">> => <<"miss">>,
       <<"body">> =>
           <<"Computed result not available in cache.">>
      }}.


%% @doc Generate a message to return when the necessary messages to execute a
%% cache lookup are not found in the cache.
necessary_messages_not_found_error(Msg1, Msg2, Opts) ->
    ?event(
      load_message,
      {necessary_messages_not_found, {msg1, Msg1}, {msg2, Msg2}},
      Opts),
    {error,
     #{
       <<"status">> => 404,
       <<"body">> =>
           <<"Necessary messages not found in cache.">>
      }}.


%% @doc Determine whether we are likely to be faster looking up the result in
%% our cache (hoping we have it), or executing it directly.
exec_likely_faster_heuristic({as, _, Msg1}, Msg2, Opts) ->
    exec_likely_faster_heuristic(Msg1, Msg2, Opts);
exec_likely_faster_heuristic(Msg1, Msg2, Opts) ->
    case hb_opts:get(cache_lookup_hueristics, true, Opts) of
        false -> false;
        true ->
            case ?IS_ID(Msg1) of
                true -> false;
                false -> is_explicit_lookup(Msg1, Msg2, Opts)
            end
    end.


is_explicit_lookup(Msg1, #{<<"path">> := Key}, Opts) ->
    % For now, just check whether the key is explicitly in the map. That is
    % a good signal that we will likely be asked by the device to grab it.
    % If we have `only-if-cached' in the opts, we always force lookup, too.
    case specifiers_to_cache_settings(hb_opts:get(cache_control, [], Opts)) of
        #{<<"only-if-cached">> := true} -> false;
        _ -> is_map(Msg1) andalso hb_maps:is_key(Key, Msg1, Opts)
    end.


%% @doc Derive cache settings from a series of option sources and the opts,
%% honoring precidence order. The Opts is used as the first source. Returns a
%% map with `store' and `lookup' keys, each of which is a boolean.
%%
%% For example, if the last source has a `no_store', the first expresses no
%% preference, but the Opts has `cache_control => [always]', then the result
%% will contain a `store => true' entry.
derive_cache_settings(SourceList, Opts) ->
    lists:foldr(
      fun(Source, Acc) ->
              maybe_set(Acc, cache_source_to_cache_settings(Source, Opts), Opts)
      end,
      #{<<"store">> => ?DEFAULT_STORE_OPT, <<"lookup">> => ?DEFAULT_LOOKUP_OPT},
      [{opts, Opts} | lists:filter(fun erlang:is_map/1, SourceList)]).


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
      hb_maps:keys(Map2, Opts)).


%% @doc Convert a cache source to a cache setting. The setting _must_ always be
%% directly in the source, not an AO-Core-derivable value. The
%% `to_cache_control_map' function is used as the source of settings in all
%% cases, except where an `Opts' specifies that hashpaths should not be updated,
%% which leads to the result not being cached (as it may be stored with an
%% incorrect hashpath).
cache_source_to_cache_settings({opts, Opts}, _) ->
    CCMap = specifiers_to_cache_settings(hb_opts:get(cache_control, [], Opts)),
    case hb_opts:get(hashpath, update, Opts) of
        ignore -> CCMap#{<<"store">> => false};
        _ -> CCMap
    end;
cache_source_to_cache_settings(Msg, Opts) ->
    case dev_message:get(<<"cache-control">>, Msg, Opts) of
        {ok, CC} -> specifiers_to_cache_settings(CC);
        {error, not_found} -> #{}
    end.


%% @doc Convert a cache control list as received via HTTP headers into a
%% normalized map of simply whether we should store and/or lookup the result.
specifiers_to_cache_settings(CCSpecifier) when not is_list(CCSpecifier) ->
    specifiers_to_cache_settings([CCSpecifier]);
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


%%% Tests


%% Helpers to create a message with Cache-Control header
msg_with_cc(CC) -> #{<<"cache-control">> => CC}.


opts_with_cc(CC) -> #{cache_control => CC}.


%% Test precedence order (Opts > Msg3 > Msg2)
opts_override_message_settings_test() ->
    Msg2 = msg_with_cc([<<"no-store">>]),
    Msg3 = msg_with_cc([<<"no-cache">>]),
    Opts = opts_with_cc([<<"always">>]),
    Result = derive_cache_settings([Msg3, Msg2], Opts),
    ?assertEqual(#{<<"store">> => true, <<"lookup">> => true}, Result).


msg_precidence_overrides_test() ->
    Msg2 = msg_with_cc([<<"always">>]),
    Msg3 = msg_with_cc([<<"no-store">>]),  % No restrictions
    Result = derive_cache_settings([Msg3, Msg2], opts_with_cc([])),
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
      Result).


%% Test hashpath settings
hashpath_ignore_prevents_storage_test() ->
    Opts = (opts_with_cc([]))#{hashpath => ignore},
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
      Result).


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
          #{}),
    ?assertEqual(#{
                   <<"store">> => false,
                   <<"lookup">> => undefined,
                   <<"only-if-cached">> => undefined
                  },
                 Result).


message_source_cache_control_test() ->
    Msg = msg_with_cc([<<"no-cache">>]),
    Result = cache_source_to_cache_settings(Msg, #{}),
    ?assertEqual(#{
                   <<"store">> => undefined,
                   <<"lookup">> => false,
                   <<"only-if-cached">> => undefined
                  },
                 Result).


%%% Basic cached AO-Core resolution tests


cache_binary_result_test() ->
    CachedMsg = <<"test-message">>,
    Msg1 = #{<<"test-key">> => CachedMsg},
    Msg2 = <<"test-key">>,
    {ok, Res} = hb_ao:resolve(Msg1, Msg2, #{cache_control => [<<"always">>]}),
    ?assertEqual(CachedMsg, Res),
    {ok, Res2} = hb_ao:resolve(Msg1, Msg2, #{cache_control => [<<"only-if-cached">>]}),
    {ok, Res3} = hb_ao:resolve(Msg1, Msg2, #{cache_control => [<<"only-if-cached">>]}),
    ?assertEqual(CachedMsg, Res2),
    ?assertEqual(Res2, Res3).


cache_message_result_test() ->
    CachedMsg =
        #{
          <<"purpose">> => <<"Test-Message">>,
          <<"aux">> => #{<<"aux-message">> => <<"Aux-Message-Value">>},
          <<"test-key">> => rand:uniform(1000000)
         },
    Msg1 = #{<<"test-key">> => CachedMsg, <<"local">> => <<"Binary">>},
    Msg2 = <<"test-key">>,
    {ok, Res} =
        hb_ao:resolve(
          Msg1,
          Msg2,
          #{
            cache_control => [<<"always">>]
           }),
    ?event({res1, Res}),
    ?event(reading_from_cache),
    {ok, Res2} = hb_ao:resolve(Msg1, Msg2, #{cache_control => [<<"only-if-cached">>]}),
    ?event(reading_from_cache_again),
    {ok, Res3} = hb_ao:resolve(Msg1, Msg2, #{cache_control => [<<"only-if-cached">>]}),
    ?event({res2, Res2}),
    ?event({res3, Res3}),
    ?assertEqual(Res2, Res3).
