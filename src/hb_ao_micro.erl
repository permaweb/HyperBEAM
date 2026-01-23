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
            ?event(ao_core, {resolved_intermediate_result, {result, {explicit, Result}}}, Opts),
            resolve([Result|Rest], Opts);
        {error, Reason} -> {error, Reason}
    end.

%% @doc Resolve a hashpath via its `Base` and `Req` (`Prefix/Suffix`) components.
%% Each of the stages is a separate function with its own arguments, building up
%% the state of the computation as it progresses.
-spec resolve(binary() | map(), binary() | map(), map()) -> {ok, any()}.
resolve(Base, Req, Opts) -> stage_1(Base, Req, Opts).

%% @doc Stage 1: Normalize the `Base' and `Req' components.
%% If either of the components is not a binary, we write them to the cache and 
%% use the resulting IDs as the `Base' and `Req' for the next stage.
stage_1({link, ID, _Opts}, Req, Opts) ->
    stage_1(ID, Req, Opts);
stage_1(Base, {link, ID, _Opts}, Opts) ->
    stage_1(Base, ID, Opts);
stage_1(Base, Req, Opts) when is_map(Base) ->
    ?event(ao_core, {normalize_offloading_base, {explicit, Base}}, Opts),
    {ok, BaseID} = hb_cache_micro:write(Base, Opts),
    stage_1(BaseID, Req, Opts);
stage_1(BaseID, Req, Opts) when is_map(Req) ->
    ?event(ao_core, {normalize_offloading_req, Req}, Opts),
    {ok, ReqID} = hb_cache_micro:write(Req, Opts),
    stage_1(BaseID, ReqID, Opts);
stage_1(BaseID, ReqID, Opts) when is_binary(BaseID) andalso is_binary(ReqID) ->
    stage_2(BaseID, ReqID, Opts).

%% @doc Stage 2: Try to read the key directly. Return if found.
%% If not found, we move on to the next stage.
stage_2(BaseID, ReqID, Opts) ->
    case hb_cache_micro:read(HP = <<BaseID/binary, "/", ReqID/binary>>, Opts) of
        {ok, Result} ->
            ?event(ao_core, {cache_hit, {path, HP}, {result, Result}}, Opts),
            {ok, Result};
        not_found ->
            stage_3(BaseID, ReqID, Opts)
    end.

%% @doc Stage 3: Try to read the `device' of the `BaseID' and the `path' of the
%% `ReqID'. The default device is `message@1.0', and absence of a `path' results
%% in a `throw'.
stage_3(BaseID, Req, Opts) ->
    Store = hb_opts:get(store, no_store, Opts),
    DeviceID =
        case hb_store:read(Store, <<BaseID/binary, "/device">>) of
            {ok, Device} -> Device;
            not_found -> <<"message@1.0">>
        end,
    case not ?IS_ID(Req) of
        true -> stage_4(BaseID, Req, DeviceID, Req, Opts);
        false ->
            case hb_store:read(Store, <<Req/binary, "/path">>) of
                {ok, Key} -> stage_4(BaseID, Req, DeviceID, Key, Opts);
                not_found -> throw({no_path_in_request, {base, BaseID}, {req, Req}})
            end
    end.

%% @doc Stage 4: Read the device and key from the cache. We expect to find a
%% `resolver' function and a `vary' function in return.
stage_4(BaseID, ReqID, DeviceID, Key, Opts) ->
    Store = hb_opts:get(store, no_store, Opts),
    case hb_store:read(Store, <<DeviceID/binary, "/", Key/binary>>) of
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
    ?event(ao_core, {executing_resolver, {func, Func}, {args, Args}}, Opts),
    case apply(Func, Args) of
        {ok, RawResult} ->
            ?event(
                ao_core,
                {resolver_execution_succeeded,
                    {func, Func},
                    {args, Args},
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
stage_9(BaseID, Result = #{ '...' := base }, Opts) ->
    ?event(
        ao_core,
        {returning_extended_result, {result, Result}, {base, BaseID}},
        Opts
    ),
    {ok, Result#{ '...' => BaseID }};
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
        store => [hb_test_utils:test_store(hb_store_fs), hd(tl(hb_opts:get(store)))]
    }.

lookup_test() ->
    ?assertEqual(
        {ok, <<"value">>},
        resolve(#{ <<"key">> => <<"value">> }, <<"key">>, opts())
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
    ?assertEqual(
        {ok, <<"value">>},
        resolve(
            #{
                <<"ignored">> => <<"value">>,
                <<"...">> => #{ <<"test-key">> => <<"value">> }
            },
            <<"test-key">>,
            opts()
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
        dev_message:id(
            #{ <<"x">> => 1, <<"device">> => <<"test-device@1.0">> },    
            #{ <<"committers">> => <<"none">> },
            opts()
        ),
    ?assertEqual(
        {
            ok,
            #{ 
                <<"x">> => 2, 
                '...' => ExpectedBaseId
            }
        },
        ResolveResult  
    ).

device_precedence_test() ->
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
    ?assertEqual({ok, <<"test-device@1.0">>}, resolve(Root, <<"device">>, opts())),
    ?assertEqual({ok, <<"dogs">>}, resolve(Root, <<"i-like">>, opts())),
    ?assertEqual({ok, <<"cows">>}, resolve(Middle, <<"i-like">>, opts())),
    ?assertEqual({ok, <<"cats">>}, resolve(Top, <<"i-like">>, opts())),
    ?assertEqual({ok, <<"turtles">>}, resolve(Middle2, <<"i-like">>, opts())).

device_param_precedence_test() ->
    ?assertEqual(
        {ok, 1},
        resolve(
            #{
                <<"device">> => <<"test-device@1.0">>,
                <<"x">> => 1,
                <<"...">> => #{ <<"inc">> => 0 }
            },
            <<"inc">>,
            opts()
        )
    ).