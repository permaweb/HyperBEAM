%%% @doc Implements the legacy/classic exception handling pattern of the 
%%% Emscripten WASM execution environment.
%%% 
%%% Emscripten has many subtly different ways of handling exceptions.
%%% For the avoidance of doubt, the pattern in question works as follows:
%%% 
%%% ```
%%% function invoke_vjj(index, a1, a2) {
%%%   var sp = stackSave();
%%%   try {
%%%     getWasmTableEntry(Number(index))(a1, a2);
%%%   } catch (e) {
%%%     stackRestore(sp);
%%%     if (e !== e + 0) throw e;
%%%     _setThrew(1, 0);
%%%   }
%%% }
%%% '''
%%% 
%%% Where '_vjj' represents the type spec of the function.
-module(dev_emscripten).
-export([info/1, init/3, router/4, invoke_ii/3, invoke_vjj/3]).


-include("src/include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-hb_debug(print).

info(_) ->
    #{
        default_handler => fun router/4,
        excludes => [keys, id, unsigned, hashpath]
    }.

%% @doc On-boot, initialize the virtual file system with:
%% - Empty stdio files
init(M1, _M2, Opts) ->
    ?event(init_emscripten),
    MsgWithLib =
        hb_ao:set(
            M1,
            #{
                <<"wasm/stdlib/env">> =>
                    #{ device => <<"emscripten@1.0">>}
            },
            Opts
        ),
    {ok, MsgWithLib}.


invoke_ii(Msg1, Msg2, Opts) ->
	?event(invoke_emscripten_vjj),
	router(<<"invoke_ii">>, Msg1, Msg2, Opts).

invoke_vjj(Msg1, Msg2, Opts) ->
	?event(invoke_emscripten_vjj),
	router(<<"invoke_vjj">>, Msg1, Msg2, Opts).

router(<<"invoke_", _/binary>>, Msg1, Msg2, Opts) ->
    ?event(invoke_emscripten),
    State = hb_ao:get(<<"State">>, Msg1, #{ hashpath => ignore }),
    WASM = dev_wasm:instance(State, Msg2, Opts),
    [Index|Args] = hb_ao:get(args, Msg2, #{ hashpath => ignore }),
    %?event(debug, invoke_emscripten_stack_get_current),
    % {ok, SP, _} = hb_beamr:call(WASM, <<"emscripten_stack_get_current">>, []),
    % ?event(debug, invoke_emscripten_stack_get_current_done),
    ImportResolver = hb_private:get(<<"WASM/import-resolver">>, State, Opts),
    try 
        ?event(trying_indirect_call),
        Res = hb_beamr:call(WASM, Index, Args, ImportResolver, State, Opts),
        ?event(debug, try_indirect_call_succeeded),
        Res
    catch
        _:Error ->
            ?event(debug, calling_emscripten_stack_restore),
            % hb_beamr:call(WASM, <<"_emscripten_stack_restore">>, [SP]),
            % ?event(debug, calling_set_threw),
            % hb_beamr:call(WASM, <<"setThrew">>, [1, 0]),
            % ?event(debug, calling_set_threw_done),
            {error, Error}
    end.

%%% Tests
init() ->
    application:ensure_all_started(hb).

generate_emscripten_stack(File, Func, Params) ->
    init(),
    Msg0 = dev_wasm:cache_wasm_image(File),
    Msg1 = Msg0#{
        <<"device">> => <<"stack@1.0">>,
        <<"device-stack">> => [<<"WASI@1.0">>, <<"WASM-64@1.0">>, <<"emscripten@1.0">>],
        <<"output-prefixes">> => [<<"wasm">>, <<"wasm">>],
        <<"stack-keys">> => [<<"init">>, <<"compute">>],
        <<"function">> => Func,
        <<"params">> => Params
    },
    {ok, Msg2} = hb_ao:resolve(Msg1, <<"init">>, #{}),
    Msg2.

%% @doc Ensure that an AOS Emscripten-style WASM AOT module can be invoked
%% with a function reference.
emscripten_aot_test() ->
    Init = generate_emscripten_stack("test/try.aot", <<"handle">>, [0, 0]),
    Instance = hb_private:get(<<"wasm/instance">>, Init, #{}),
    Msg = <<"msg">>,
    Env = <<"env">>,
    {ok, Ptr1} = hb_beamr_io:malloc(Instance, byte_size(Msg)),
    ?assertNotEqual(0, Ptr1),
    hb_beamr_io:write(Instance, Ptr1, Msg),
    {ok, Ptr2} = hb_beamr_io:malloc(Instance, byte_size(Env)),
    ?assertNotEqual(0, Ptr2),
    hb_beamr_io:write(Instance, Ptr2, Env),
    Ready = Init#{ <<"parameters">> => [Ptr1, Ptr2] },
    {ok, StateRes} = hb_ao:resolve(Ready, <<"compute">>, #{}),
    [Ptr] = hb_ao:get(<<"results/wasm/output">>, StateRes),
    {ok, Output} = hb_beamr_io:read_string(Instance, Ptr),
    ?assertEqual(<<"Initial">>, Output).
