%%% @doc A device that executes a WASM image on messages using the Memory-64 
%%% preview standard. In the backend, this device uses `beamr': An Erlang wrapper 
%%% for WAMR, the WebAssembly Micro Runtime.
%%% 
%%% The device has the following requirements and interface:
%%% <pre>
%%%     M1/Init ->
%%%         Assumes:
%%%             M1/process
%%%             M1/[Prefix]/image
%%%         Generates:
%%%             /priv/[Prefix]/instance
%%%             /priv/[Prefix]/import-resolver
%%%         Side-effects:
%%%             Creates a WASM executor loaded in memory of the HyperBEAM node.
%%% 
%%%     M1/Compute ->
%%%         Assumes:
%%%             M1/priv/[Prefix]/instance
%%%             M1/priv/[Prefix]/import-resolver
%%%             M1/process
%%%             M2/message
%%%             M2/message/function OR M1/function
%%%             M2/message/parameters OR M1/parameters
%%%         Generates:
%%%             /results/[Prefix]/type
%%%             /results/[Prefix]/output
%%%         Side-effects:
%%%             Calls the WASM executor with the message and process.
%%%     M1/[Prefix]/state ->
%%%         Assumes:
%%%             M1/priv/[Prefix]/instance
%%%         Generates:
%%%             Raw binary WASM state
%%% </pre>
-module(dev_wasm).
-export([info/2, init/3, compute/3, import/3, terminate/3, snapshot/3, normalize/3]).
%%% API for other devices:
-export([instance/3]).
%%% Test API:
-export([cache_wasm_image/1, cache_wasm_image/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Export all functions aside the `instance/3' function.
info(_Base, _Opts) ->
    #{
        excludes => [instance]
    }.

%% @doc Boot a WASM image on the image stated in the `process/image' field of
%% the message.
init(M1, M2, Opts) ->
    ?event(running_init),
    % Where we should read initial parameters from.
    InPrefix = dev_stack:input_prefix(M1, M2, Opts),
    % Where we should read/write our own state to.
    Prefix = dev_stack:prefix(M1, M2, Opts),
    ?event({in_prefix, InPrefix}),
    ImageBin =
        case hb_ao:get(<<InPrefix/binary, "/image">>, M1, Opts) of
            not_found ->
                case hb_ao:get(<<"body">>, M1, Opts) of
                    not_found ->
                        throw(
                            {
                                wasm_init_error,
                                <<
                                    "No viable image found in ",
                                    InPrefix/binary,
                                    "/image."
                                >>,
                                {base, M1}
                            }
                        );
                    Bin when is_binary(Bin) -> Bin
                end;
            ImageID when ?IS_ID(ImageID) ->
                ?event({getting_wasm_image, ImageID}),
                {ok, ImageMsg} = hb_cache:read(ImageID, Opts),
                hb_ao:get(<<"body">>, ImageMsg, Opts);
            ImageMsg when is_map(ImageMsg) ->
                ?event(wasm_image_message_directly_provided),
                hb_ao:get(<<"body">>, ImageMsg, Opts);
            Image when is_binary(Image) ->
                ?event(wasm_image_binary_directly_provided),
                Image
        end,
    Mode =
        case hb_ao:get(<<InPrefix/binary, "/Mode">>, M1, Opts) of
            not_found -> wasm;
            <<"WASM">> -> wasm;
            <<"AOT">> ->
                case hb_opts:get(wasm_allow_aot, false, Opts) of
                    true -> aot;
                    false -> wasm
                end
        end,
    % Start the WASM executor.
    {ok, Instance, _Imports, _Exports} = hb_beamr:start(ImageBin, Mode),
    % Set the WASM Instance, handler, and standard library invokation function.
    ?event({setting_wasm_instance, Instance, {prefix, Prefix}}),
    {ok,
        hb_private:set(M1,
            #{
                <<Prefix/binary, "/write">> =>
                    fun(Binary) ->
                        {ok, Ptr} = hb_beamr_io:write_string(Instance, Binary),
                        {ok, Ptr}
                    end,
                <<Prefix/binary, "/read">> =>
                    fun Reader([Ptr]) -> Reader(Ptr);
                        Reader(Ptr) ->
                            {ok, Binary} = hb_beamr_io:read_string(Instance, Ptr),
                            {ok, Binary}
                    end,
                <<Prefix/binary, "/instance">> => Instance,
                <<Prefix/binary, "/import-resolver">> =>
                    fun default_import_resolver/3
            },
            Opts
        )
    }.

%% @doc Take a BEAMR import call and resolve it using `hb_ao'.
default_import_resolver(Base, Req, Opts) ->
    #{
        instance := WASM,
        module := Module,
        func := Func,
        args := Args,
        func_sig := Signature
    } = Req,
    Prefix = dev_stack:prefix(Base, Req, Opts),
    {ok, Res} =
        hb_ao:resolve(
            hb_private:set(
                Base,
                #{ <<Prefix/binary, "/instance">> => WASM },
                Opts
            ),
            #{
                <<"path">> => <<"import">>,
                <<"module">> => list_to_binary(Module),
                <<"func">> => list_to_binary(Func),
                <<"args">> => Args,
                <<"func-sig">> => list_to_binary(Signature)
            },
            Opts
        ),
    NextState = hb_ao:get(state, Res, Opts),
    Response = hb_ao:get(results, Res, Opts),
    {ok, Response, NextState}.

%% @doc Call the WASM executor with a message that has been prepared by a prior
%% pass.
compute(RawM1, M2, Opts) ->
    % Normalize the message to have an open WASM instance, but no literal `State'.
    % The hashpath is not updated during this process. This allows us to take
    % two different messages and get the same result:
    % - A message with a `State' key but no WASM instance in `priv/'.
    % - A message with a WASM instance in `priv/' but no `State' key.
    {ok, M1} = normalize(RawM1, M2, Opts),
    ?event(running_compute),
    Prefix = dev_stack:prefix(M1, M2, Opts),
    case hb_ao:get(pass, M1, Opts) of
        X when X == 1 orelse X == not_found ->
            % Extract the WASM Instance, func, params, and standard library
            % invokation from the message and apply them with the WASM executor.
            WASMFunction =
                hb_ao:get_first(
                    [
                        {M2, <<"body/function">>},
                        {M2, <<"function">>},
                        {M1, <<"function">>}
                    ],
                    Opts
                ),
            WASMParams =
                hb_ao:get_first(
                    [
                        {M2, <<"body/parameters">>},
                        {M2, <<"parameters">>},
                        {M1, <<"parameters">>}
                    ],
                    Opts
                ),
            case WASMFunction of
                not_found ->
                    ?event(
                        {
                            skipping_wasm_exec,
                            {reason, wasm_function_not_provided},
                            {prefix, Prefix},
                            {m1, M1},
                            {m2, M2}
                        }
                    ),
                    {ok, M1};
                _ ->
                    ?event(
                        {
                            calling_wasm_executor,
                            {prefix, Prefix},
                            {wasm_function, {explicit, WASMFunction}},
                            {wasm_params, WASMParams},
                            {m1, M1},
                            {m2, M2},
                            {priv, hb_private:from_message(M1)}
                        }
                    ),
                    {ResType, Res, MsgAfterExecution} =
                        hb_beamr:call(
                            instance(M1, M2, Opts),
                            WASMFunction,
                            case WASMParams of
                                not_found -> [];
                                Params -> Params
                            end,
                            hb_private:get(<<Prefix/binary, "/import-resolver">>, M1, Opts),
                            M1,
                            Opts
                        ),
                    {ok,
                        hb_ao:set(MsgAfterExecution,
                            #{
                                <<"results/", Prefix/binary, "/type">> => ResType,
                                <<"results/", Prefix/binary, "/output">> => Res
                            },
                            Opts
                        )
                    }
            end;
        _ -> {ok, M1}
    end.

%% @doc Normalize the message to have an open WASM instance, but no literal
%% `State' key. Ensure that we do not change the hashpath during this process.
normalize(RawM1, M2, Opts) ->
    ?event({normalize_raw_m1, RawM1}),
    M3 = 
        case instance(RawM1, M2, Opts) of
            not_found ->
                DeviceKey =
                    case hb_ao:get(<<"device-key">>, RawM1, Opts) of
                        not_found -> [];
                        Key -> [Key]
                    end,
                ?event(
                    {no_instance_attempting_to_get_snapshot,
                        {base, RawM1}, {device_key, DeviceKey}
                    }
                ),
                Memory = 
                    hb_ao:get(
                        [<<"snapshot">>] ++ DeviceKey ++ [<<"body">>],
                        {as, dev_message, RawM1},
                        Opts
                    ),
                case Memory of
                    not_found -> throw({error, no_wasm_instance_or_snapshot});
                    State ->
                        {ok, M1} = init(RawM1, State, Opts),
                        Res = hb_beamr:deserialize(instance(M1, M2, Opts), State),
                        ?event(snapshot, {wasm_deserialized, {result, Res}}),
                        M1
                end;
            _ ->
                ?event(wasm_instance_found_not_deserializing),
                RawM1
        end,
    dev_message:set(M3, #{ <<"snapshot">> => unset }, Opts).

%% @doc Serialize the WASM state to a binary.
snapshot(M1, M2, Opts) ->
    ?event(snapshot, generating_snapshot),
    Instance = instance(M1, M2, Opts),
    {ok, Serialized} = hb_beamr:serialize(Instance),
    {ok,
        #{
            <<"body">> => Serialized
        }
    }.

%% @doc Tear down the WASM executor.
terminate(M1, M2, Opts) ->
    ?event(terminate_called_on_dev_wasm),
    Prefix = dev_stack:prefix(M1, M2, Opts),
    Instance = instance(M1, M2, Opts),
    hb_beamr:stop(Instance),
    {ok, hb_private:set(M1,
        #{
            <<Prefix/binary, "/instance">> => unset
        },
        Opts
    )}.

%% @doc Get the WASM instance from the message. Note that this function is exported
%% such that other devices can use it, but it is excluded from calls from AO-Core
%% resolution directly.
instance(M1, M2, Opts) ->
    Prefix = dev_stack:prefix(M1, M2, Opts),
    Path = <<Prefix/binary, "/instance">>,
    ?event({searching_for_instance, Path, M1}),
    hb_private:get(Path, M1, Opts#{ hashpath => ignore }).

%% @doc Handle standard library calls by:
%% 1. Adding the right prefix to the path from BEAMR.
%% 2. Adding the state to the message at the stdlib path.
%% 3. Resolving the adjusted-path-Req against the added-state-Base.
%% 4. If it succeeds, return the new state from the message.
%% 5. If it fails with `not_found', call the stub handler.
import(Base, Req, Opts) ->
    % 1. Adjust the path to the stdlib.
    ModName = hb_ao:get(<<"module">>, Req, Opts),
    FuncName = hb_ao:get(<<"func">>, Req, Opts),
    Prefix = dev_stack:prefix(Base, Req, Opts),
    AdjustedPath =
        <<
            Prefix/binary,
            "/stdlib/",
            ModName/binary,
            "/",
            FuncName/binary
        >>,
    StatePath = << Prefix/binary, "/stdlib/", ModName/binary, "/state" >>,
    AdjustedReq = Req#{ <<"path">> => AdjustedPath },
    % 2. Add the current state to the message at the stdlib path.
    AdjustedBase =
        hb_ao:set(
            Base,
            #{ StatePath => Base },
            Opts#{ hashpath => ignore }
        ),
    ?event({state_added_base, AdjustedBase, AdjustedReq}),
    % 3. Resolve the adjusted path against the added state.
    case hb_ao:resolve(AdjustedBase, AdjustedReq, Opts) of
        {ok, Res} ->
            % 4. Success. Return.
            {ok, Res};
        {error, not_found} ->
            ?event(stdlib_not_found),
            % 5. Failure. Call the stub handler.
            undefined_import_stub(Base, Req, Opts)
    end.

%% @doc Log the call to the standard library as an event, and write the
%% call details into the message.
undefined_import_stub(Base, Req, Opts) ->
    ?event({unimplemented_dev_wasm_call, {base, Base}, {req, Req}}),
    Prefix = dev_stack:prefix(Base, Req, Opts),
    UndefinedCallsPath =
        <<"state/results/", Prefix/binary, "/undefined-calls">>,
    Res = hb_ao:set(
        Base,
        #{
            UndefinedCallsPath =>
                [
                    Req
                |
                    case hb_ao:get(UndefinedCallsPath, Base, Opts) of
                        not_found -> [];
                        X -> X
                    end
                ]
        },
        Opts
    ),
    {ok, #{ state => Res, results => [0] }}.

%%% Tests

init() ->
    application:ensure_all_started(hb),
    hb:init().

% Pass
input_prefix_test() ->
    init(),
    #{ <<"image">> := ImageID } = cache_wasm_image("test/test.wasm"),
    Base =
        #{
            <<"device">> => <<"wasm-64@1.0">>,
            <<"input-prefix">> => <<"test-in">>,
            <<"test-in">> => #{ <<"image">> => ImageID }
        },
    {ok, Req} = hb_ao:resolve(Base, <<"init">>, #{}),
    ?event({after_init, Req}),
    Priv = hb_private:from_message(Req),
    ?assertMatch(
        {ok, Instance} when is_pid(Instance),
        hb_ao:resolve(Priv, <<"instance">>, #{})
    ),
    ?assertMatch(
        {ok, Fun} when is_function(Fun),
        hb_ao:resolve(Priv, <<"import-resolver">>, #{})
    ).

%% @doc Test that realistic prefixing for a `dev_process' works --
%% including both inputs (from `Process/') and outputs (to the 
%% Device-Key) work
process_prefixes_test() ->
    init(),
    Base =
        #{
            <<"device">> => <<"wasm-64@1.0">>,
            <<"output-prefix">> => <<"wasm">>,
            <<"input-prefix">> => <<"process">>,
            <<"process">> => cache_wasm_image("test/test.wasm")
        },
    {ok, Res} = hb_ao:resolve(Base, <<"init">>, #{}),
    ?event({after_init, Res}),
    Priv = hb_private:from_message(Res),
    ?assertMatch(
        {ok, Instance} when is_pid(Instance),
        hb_ao:resolve(Priv, <<"wasm/instance">>, #{})
    ),
    ?assertMatch(
        {ok, Fun} when is_function(Fun),
        hb_ao:resolve(Priv, <<"wasm/import-resolver">>, #{})
    ).


init_test() ->
    init(),
    Msg = cache_wasm_image("test/test.wasm"),
    {ok, Base} = hb_ao:resolve(Msg, <<"init">>, #{}),
    ?event({after_init, Base}),
    Priv = hb_private:from_message(Base),
    ?assertMatch(
        {ok, Instance} when is_pid(Instance),
        hb_ao:resolve(Priv, <<"instance">>, #{})
    ),
    ?assertMatch(
        {ok, Fun} when is_function(Fun),
        hb_ao:resolve(Priv, <<"import-resolver">>, #{})
    ).

basic_execution_test() ->
    ?assertEqual(
        {ok, [120.0]},
        test_run_wasm("test/test.wasm", <<"fac">>, [5.0], #{})
    ).

basic_execution_64_test() ->
    ?assertEqual(
        {ok, [120.0]},
        test_run_wasm("test/test-64.wasm", <<"fac">>, [5.0], #{})
    ).

imported_function_test() ->
    ?assertEqual(
        {ok, [32]},
        test_run_wasm(
            "test/pow_calculator.wasm",
            <<"pow">>,
            [2, 5],
            #{
                <<"stdlib/my_lib">> =>
                    #{ <<"device">> => <<"test-device@1.0">> }
            }
        )
    ).

benchmark_test() ->
    BenchTime = 0.5,
    init(),
    Msg0 = cache_wasm_image("test/test-64.wasm"),
    {ok, Base} = hb_ao:resolve(Msg0, <<"init">>, #{}),
    Req =
        hb_maps:merge(
            Base,
            #{
                <<"function">> => <<"fac">>,
                <<"parameters">> => [5.0]
            },
			#{}
        ),
    Iterations =
        hb_test_utils:benchmark(
            fun() ->
                hb_ao:resolve(Req, <<"compute">>, #{})
            end,
            BenchTime
        ),
    ?event(benchmark, {scheduled, Iterations}),
    hb_test_utils:benchmark_print(
        <<"Through AO-Core:">>,
        <<"resolutions">>,
        Iterations,
        BenchTime
    ),
    ?assert(Iterations > 5),
    ok.

state_export_and_restore_test() ->
    init(),
    % Generate a WASM message. We use the pow_calculator because it has a 
    % reasonable amount of memory to work with.
    Msg0 = cache_wasm_image("test/pow_calculator.wasm"),
    {ok, Base} = hb_ao:resolve(Msg0, <<"init">>, #{}),
    Req =
        hb_maps:merge(
            Base,
            Extras = #{
                <<"function">> => <<"pow">>,
                <<"parameters">> => [2, 2],
                <<"stdlib">> =>
                    #{
                        <<"my_lib">> =>
                            #{ <<"device">> => <<"test-device@1.0">> }
                    }
            },
			#{}
        ),
    ?event({after_setup, Req}),
    % Compute a computation and export the state.
    {ok, Resa} = hb_ao:resolve(Req, <<"compute">>, #{}),
    ?assertEqual([4], hb_ao:get(<<"results/output">>, Resa, #{})),
    {ok, State} = hb_ao:resolve(Resa, <<"snapshot">>, #{}),
    ?event({state_res, State}),
    % Restore the state without calling Init.
    NewBase = hb_maps:merge(Msg0, Extras#{ <<"snapshot">> => State }, #{}),
    ?assertEqual(
        {ok, [4]},
        hb_ao:resolve(NewBase, <<"compute/results/output">>, #{})
    ).

%%% Test helpers

cache_wasm_image(Image) ->
    cache_wasm_image(Image, #{}).
cache_wasm_image(Image, Opts) ->
    {ok, Bin} = file:read_file(Image),
    Msg = #{ <<"body">> => Bin },
    {ok, ID} = hb_cache:write(Msg, Opts),
    #{
        <<"device">> => <<"wasm-64@1.0">>,
        <<"image">> => ID
    }.

test_run_wasm(File, Func, Params, AdditionalMsg) ->
    init(),
    Msg0 = cache_wasm_image(File),
    {ok, Base} = hb_ao:resolve(Msg0, <<"init">>, #{}),
    ?event({after_init, Base}),
    Req =
        hb_maps:merge(
            Base,
            hb_ao:set(
                #{
                    <<"function">> => Func,
                    <<"parameters">> => Params
                },
                AdditionalMsg,
                #{ hashpath => ignore }
            ),
			#{}
        ),
    ?event({after_setup, Req}),
    {ok, StateRes} = hb_ao:resolve(Req, <<"compute">>, #{}),
    ?event({after_resolve, StateRes}),
    hb_ao:resolve(StateRes, <<"results/output">>, #{}).
