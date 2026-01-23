%%% @doc A minimal implementation of the AO-Core 1.5 resolution system.
-module(hb_ao_micro).
-export([get/3, resolve/2, resolve/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Shortcut to resolve a key and return the result, unwrapping `resolve's
%% `{ok, ...}' return.
get(Key, Msg, Opts) ->
    hb_util:ok(resolve(Msg, Key, Opts)).

%% @doc Take a path or sequence of messages and resolve them sequentially. The
%% result of the resolution of the first element of the path/list against the 
%% second is used as the base for the third element, and so on. The resulting
%% message after the final pair has been evaluated is returned.
-spec resolve(binary() | [binary() | map()], map()) -> {ok, any()}.
resolve(Path, Opts) when is_binary(Path) ->
    resolve(hb_path:term_to_path_parts(Path, Opts), Opts);
resolve([Base, Req|Rest], Opts) ->
    case resolve(Base, Req, Opts) of
        {ok, Result} when length(Rest) == 0 ->
            ?event(ao_core, {resolved_final_result, {result, Result}}, Opts),
            {ok, Result};
        {ok, Result} ->
            ?event(ao_core, {resolved_intermediate_result, {result, Result}}, Opts),
            resolve([Result|Rest], Opts);
        {error, Reason} -> {error, Reason}
    end.

%% @doc Resolve a hashpath via its `Base` and `Req` (`Prefix/Suffix`) components.
%% Each of the stages is a separate function with its own arguments, building up
%% the state of the computation as it progresses.
-spec resolve(binary() | map(), binary() | map(), map()) -> {ok, any()}.
resolve(Base, Req, Opts) -> stage_1(Base, Req, Opts).

%% @doc Stage 1: Write the `Base' component to the cache. Refer to it in the 
%% future via its ID.
stage_1(Base, Req, Opts) when is_map(Base) ->
    ?event(ao_core, {normalize_offloading_base, Base}, Opts),
    {ok, BaseID} = hb_cache_micro:write(Base, Opts),
    stage_1(BaseID, Req, Opts);
stage_1(Base, #{ <<"path">> := Key }, Opts) ->
    stage_1(Base, Key, Opts);
stage_1(Base, Req, Opts) when is_map(Req) ->
    ?event(ao_core, {normalize_offloading_req, Req}, Opts),
    {ok, ReqID} = hb_cache_micro:write(Req, Opts),
    stage_1(Base, ReqID, Opts);
stage_1(Base, Req, Opts) ->
    ?event(ao_core, {stage_1, {base_id, Base}, {req_id, Req}}),
    stage_2(Base, Req, Opts).

%% @doc Stage 2: Try to read the key directly. If it is not found, try to locate
%% a `device' key. Search each layer of a potentially extended message in turn.
%% The desire semantics are a breadth-first (explicit key then device lookup
%% per layer), rather than depth-first (search recursively for the key, then for
%% the device).
stage_2(Base, ReqID, Opts) ->
    case value_or_device(Base, ReqID, Opts) of
        {value, Result} ->
            ?event(
                ao_core,
                {cache_hit, {base, Base}, {req, ReqID}, {result, Result}},
                Opts
            ),
            {ok, Result};
        {device, Device} ->
            ?event(
                ao_core,
                {device_found, {base, Base}, {device, Device}},
                Opts
            ),
            stage_3(Base, ReqID, Device, Opts);
        not_found ->
            ?event(ao_core, {lookup_failure, {base, Base}, {req, ReqID}}, Opts),
            {error, not_found}
    end.

%% @doc Search through each layer of a message in turn to find the first instance
%% of a value for the given key or a `device' key.
value_or_device(BaseMsg, Req, Opts) when is_map(BaseMsg) ->
    value_or_device_from_message(BaseMsg, Req, Opts);
value_or_device(Link, Req, Opts) when ?IS_LINK(Link) ->
    case hb_cache_micro:resolve(Link, Opts) of
        not_found -> not_found;
        {ok, {link, ID, _}} -> value_or_device(ID, Req, Opts)
    end;
value_or_device(BaseID, Req, Opts) ->
    case hb_cache_micro:read(<<BaseID/binary, "/", Req/binary>>, Opts) of
        {ok, Result} -> {value, Result};
        not_found ->
            case hb_cache_micro:read(<<BaseID/binary, "/device">>, Opts) of
                {ok, Device} -> {device, Device};
                not_found ->
                    value_or_device(
                        {link, <<BaseID/binary, "/...">>, #{}},
                        Req,
                        Opts
                    )
            end
    end.

%% @doc As with `value_or_device`, but searching a (potentially partially) 
%% loaded message instead of upon an ID.
value_or_device_from_message(LoadedBase, Req, Opts) ->
    ?event(ao_core, {finding_value_or_device_from_loaded, LoadedBase}),
    case LoadedBase of
        #{ Req := Link } when ?IS_LINK(Link) ->
            {value, hb_util:ok(hb_cache_micro:read(Link, Opts))};
        #{ Req := Value } -> {value, Value};
        #{ <<"device">> := Link } when ?IS_LINK(Link) ->
            {device, hb_util:ok(hb_cache_micro:read(Link, Opts))};
        #{ <<"device">> := Device } ->
            {device, Device};
        #{ <<"...">> := Inner } -> value_or_device(Inner, Req, Opts);
        _ -> not_found
    end.

%% @doc Stage 3: Try to read the `device' of the `BaseID' and the `path' of the
%% `ReqID'. The default device is `message@1.0', and absence of a `path' results
%% in a `throw'.
stage_3(Base, ReqID, DeviceID, Opts) when ?IS_ID(ReqID) ->
    case hb_cache_micro:read(<<ReqID/binary, "/path">>, Opts) of
        {ok, Key} -> stage_4(Base, ReqID, DeviceID, Key, Opts);
        not_found -> throw({no_path_in_request, {base, Base}, {req, ReqID}})
    end;
stage_3(Base, ReqKey, DeviceID, Opts) when is_binary(ReqKey) ->
    stage_4(Base, ReqKey, DeviceID, ReqKey, Opts);
stage_3(Base, Req = #{ <<"path">> := Path }, DeviceID, Opts) when ?IS_LINK(Path) ->
    case hb_cache_micro:read(Path, Opts) of
        {ok, ReqKey} -> stage_4(Base, Req, DeviceID, ReqKey, Opts);
        _ -> throw({no_path_in_request, {base, Base}, {req, Req}})
    end;
stage_3(Base, Req = #{ <<"path">> := ReqKey }, DeviceID, Opts) ->
    stage_4(Base, Req, DeviceID, ReqKey, Opts);
stage_3(Base, Req, DeviceID, Opts) ->
    case value_or_device(Req, <<"path">>, Opts) of
        {value, ReqKey} -> stage_4(Base, Req, DeviceID, ReqKey, Opts);
        _ -> throw({no_path_in_request, {base, Base}, {req, Req}})
    end.

%% @doc Stage 4: Read the device and key from the cache. We expect to find a
%% `resolver' function and a `vary' function in return.
stage_4(BaseID, ReqID, DeviceID, Key, Opts) ->
    case hb_cache_micro:read(<<DeviceID/binary, "/", Key/binary>>, Opts) of
        {ok, #{ <<"resolver">> := Func, <<"vary">> := Vary }} ->
            ?event(ao_core,
                {found_resolver_and_vary,
                    {device, DeviceID},
                    {key, Key},
                    {resolver, Func},
                    {vary, Vary}
                },
                Opts
            ),
            stage_5(BaseID, ReqID, {Vary, Func}, Opts);
        not_found ->
            ?event(
                warning,
                {key_resolver_not_found,
                    {device, DeviceID},
                    {key, Key}
                },
                Opts
            ),
            {error, not_found}
    end.

%% @doc Stage 5: Apply the `vary' function to the `BaseID' and `ReqID' to 
%% load the arguments and validate their types for the `resolver' function.
stage_5(BaseID, ReqID, {Vary, Func}, Opts) ->
    {ok, VariedBase, VariedReq} = Vary(BaseID, ReqID, Opts),
    stage_6(BaseID, Func, VariedBase, VariedReq, Opts).

%% @doc Stage 6: Try to read the `VariedBase/VariedReq' from the cache. Return
%% if found. If not found, we move on to the next stage. This deduplicates all
%% prior computations for `Base` and `Req' messages that reduce to the same
%% `Vary'ed versions.
stage_6(BaseID, Func, VariedBase, VariedReq, Opts) ->
    {ok, VariedBaseID} = hb_cache_micro:write(VariedBase, Opts),
    {ok, VariedReqID} = hb_cache_micro:write(VariedReq, Opts),
    case hb_cache_micro:read(HP = <<VariedBaseID/binary, "/", VariedReqID/binary>>, Opts) of
        not_found -> stage_7(BaseID, Func, VariedBase, VariedReq, Opts);
        {ok, VariedResult} ->
            % If the generic result upon the `VariedBase/VariedReq' key is found,
            % we skip execution and jump to the final stage: normalizing the
            % generic result to the specific `BaseID', if appropriate.
            ?event(ao_core, {varied_cache_hit, {path, HP}}, Opts),
            stage_9(BaseID, VariedResult, Opts)
    end.

%% @doc Stage 7: Execute the `resolver' function with the given arguments.
stage_7(BaseID, Func, VariedBase, VariedReq, Opts) ->
    Args = hb_ao_device:truncate_args(Func, [VariedBase, VariedReq, Opts]),
    ?event(ao_core, {executing_resolver, {func, Func}}, Opts),
    case apply(Func, Args) of
        {ok, RawResult} ->
            ?event(
                ao_core,
                {resolver_execution_succeeded,
                    {func, Func},
                    {result, RawResult}},
                Opts
            ),
            stage_8(BaseID, VariedBase, VariedReq, RawResult, Opts);
        {error, Reason} ->
            ?event(
                ao_core,
                {resolver_execution_failed,
                    {func, Func},
                    {args, Args},
                    {reason, Reason}
                },
                Opts
            ),
            {error, Reason}
    end.

%% @doc Stage 8: Write the raw result to the cache and link it to the 
%% `VariedBase/VariedReq' key. Future callers whose varied `Base' and `Req'
%% reduce to the same `VariedBase/VariedReq' key will be able to read this
%% result from the cache.
stage_8(BaseID, VariedBase, VariedReq, RawResult, Opts) ->
    {ok, VariedBaseID} = hb_cache_micro:write(VariedBase, Opts),
    {ok, VariedReqID} = hb_cache_micro:write(VariedReq, Opts),
    {ok, ResultID} = hb_cache_micro:write(RawResult, Opts),
    VariedHP = <<VariedBaseID/binary, "/", VariedReqID/binary>>,
    ok = hb_cache_micro:link(ResultID, VariedHP, Opts),
    ?event(
        ao_core,
        {wrote_result_to_cache, {varied_path, VariedHP}, {result, ResultID}},
        Opts
    ),
    stage_9(BaseID, RawResult, Opts).

%% @doc Stage 9: Replace `... : base` in the result with `... : OriginalBaseID'
%% if present. This ensures that keys the resolver would like to passthrough
%% are preserved in the result.
stage_9(BaseID, Result = #{ <<"...">> := base }, Opts) ->
    ?event(
        ao_core,
        {returning_extended_result, {result, Result}, {base, BaseID}},
        Opts
    ),
    {ok, Result#{ <<"...">> => BaseID }};
stage_9(_BaseID, Result, Opts) ->
    ?event(
        ao_core,
        {returning_unmodified_result, {result, Result}},
        Opts
    ),
    {ok, Result}.

%%% AO-Core 1.5 micro-tests.

opts() ->
    #{
        store => [hb_test_utils:test_store(hb_store_lmdb), hd(tl(hb_opts:get(store)))]
    }.

lookup_test() ->
    ?assertEqual(
        {ok, <<"value">>},
        resolve(#{ <<"key">> => <<"value">> }, <<"key">>, opts())
    ).

lookup_with_req_msg_test() ->
    ?assertEqual(
        {ok, <<"value">>},
        resolve(
            #{ <<"key">> => <<"value">> },
            #{ <<"path">> => <<"key">> },
            opts()
        )
    ).

deep_lookup_test() ->
    ?assertEqual(
        {ok, <<"value">>},
        resolve(
            [
                #{ <<"deep">> => #{ <<"key">> => <<"value">> } },
                <<"deep">>,
                <<"key">>
            ],
            opts()
        )
    ).

message_device_extension_lookup_test() ->
    Opts = opts(),
    ?assertEqual(
        {ok, <<"value">>},
        resolve(
            #{
                <<"ignored">> => <<"value">>,
                <<"...">> => #{ <<"test-key">> => <<"value">> }
            },
            <<"test-key">>,
            Opts
        )
    ).

device_key_resolution_test() ->
    dev_test:info(),
    ?assertEqual(
        {ok, <<"GOOD FUNCTION">>},
        resolve(
            #{ <<"device">> => <<"test-device@1.0">> },
            <<"example">>,
            opts()
        )
    ).

varied_result_test() ->
    ResolveResult = 
        resolve(
            #{ <<"x">> => 1, <<"device">> => <<"test-device@1.0">> },
            <<"varied">>,
            opts()
        ),
    {ok, ExpectedBaseId} = 
        hb_cache_micro:write(
            #{ <<"x">> => 1, <<"device">> => <<"test-device@1.0">> },    
            opts()
        ),
    ?assertEqual(
        {
            ok,
            #{ 
                <<"x">> => 2, 
                <<"...">> => ExpectedBaseId
            }
        },
        ResolveResult  
    ).

device_or_key_precedence_test() ->
    Root = #{ <<"i-like">> => <<"dogs">> },
    Middle =
        #{
            <<"i-like">> => <<"cows">>,
            <<"device">> => <<"test-device@1.0">>,
            <<"...">> => Root
        },
    Middle2 =
        #{
            <<"device">> => <<"test-device@1.0">>,
            <<"...">> => Root
        },
    Top = #{ <<"i-like">> => <<"cats">>, <<"...">> => Middle },
    ?assertEqual({ok, <<"dogs">>}, resolve(Root, <<"i-like">>, opts())),
    ?assertEqual({ok, <<"cows">>}, resolve(Middle, <<"i-like">>, opts())),
    ?assertEqual({ok, <<"cats">>}, resolve(Top, <<"i-like">>, opts())),
    ?assertEqual({ok, <<"turtles">>}, resolve(Middle2, <<"i-like">>, opts())).

device_param_precedence_test() ->
    ?assertEqual(
        {ok, 2},
        resolve(
            [
                #{
                    <<"device">> => <<"test-device@1.0">>,
                    <<"x">> => 1,
                    <<"...">> => #{ <<"inc">> => #{ <<"x">> => 0 } }
                },
                <<"inc">>,
                <<"x">>
            ],
            opts()
        )
    ),
    ?assertEqual(
        {ok, 2},
        resolve(
            [
                #{
                    <<"x">> => 1,
                    <<"...">> =>
                        #{
                            <<"device">> => <<"test-device@1.0">>,
                            <<"...">> => #{ <<"inc">> => #{ <<"x">> => 0 } }
                        }
                },
                <<"inc">>,
                <<"x">>
            ],
            opts()
        )
    ).