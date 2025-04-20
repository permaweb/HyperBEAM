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
-export([info/1, init/3, router/4, invoke_vjj/3]).


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
    MsgWithLib =
        hb_converge:set(
            M1,
            #{
                <<"WASM/stdlib/env">> =>
                    #{ device => <<"Emscripten/1.0">>}
            },
            Opts
        ),
    {ok, MsgWithLib}.

invoke_vjj(Msg1, Msg2, Opts) ->
	?event(debug, invoke_emscripten_vjj),
	router(<<"invoke_vjj">>, Msg1, Msg2, Opts).

router(<<"invoke_", _/binary>>, Msg1, Msg2, Opts) ->
    ?event(debug, invoke_emscripten),
    State = hb_ao:get(<<"State">>, Msg1, #{ hashpath => ignore }),
    WASM = dev_wasm:instance(State, Msg2, Opts),
    [Index|Args] = hb_ao:get(args, Msg2, #{ hashpath => ignore }),
    %?event(debug, invoke_emscripten_stack_get_current),
    % {ok, SP, _} = hb_beamr:call(WASM, <<"emscripten_stack_get_current">>, []),
    % ?event(debug, invoke_emscripten_stack_get_current_done),
    ImportResolver = hb_private:get(<<"WASM/Import-Resolver">>, State, Opts),
    try 
        ?event(debug, trying_indirect_call),
        Res = hb_beamr:call(WASM, Index, Args, ImportResolver, State, Opts),
        ?event(debug, try_succeeded),
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
generate_stack(File) ->
    Wallet = hb:wallet(),
    Msg0 = dev_wasm:cache_wasm_image(File),
    Image = hb_converge:get(<<"Image">>, Msg0, #{}),
    Msg1 = Msg0#{
        device => <<"Stack/1.0">>,
        <<"Device-Stack">> =>
            [
                <<"WASI/1.0">>,
                <<"JSON-Iface/1.0">>,
                <<"Emscripten/1.0">>,
                <<"WASM-64/1.0">>,
                <<"Multipass/1.0">>
            ],
        <<"Input-Prefix">> => <<"Process">>,
        <<"Output-Prefix">> => <<"WASM">>,
        <<"Passes">> => 2,
        <<"Stack-Keys">> => [<<"Init">>, <<"Compute">>],
        <<"Process">> =>
            hb_message:sign(#{
                <<"Type">> => <<"Process">>,
                <<"Image">> => Image,
                <<"Mode">> => <<"AOT">>,
                <<"Scheduler">> => hb:address(),
                <<"Authority">> => hb:address()
            }, Wallet)
    },
    {ok, Msg2} = hb_converge:resolve(Msg1, <<"Init">>, #{}),
    Msg2.

%% @doc Ensure that an AOS Emscripten-style WASM AOT module can be invoked
%% with a function reference.
emscripten_aot_test() ->
    Msg = generate_stack("test/process.aot"),
    Proc = hb_converge:get(<<"Process">>, Msg, #{ hashpath => ignore }),
    ProcID = hb_converge:get(id, Proc, #{}),
    {ok, Msg3} =
        hb_converge:resolve(
            Msg,
            dev_json_iface:generate_aos_msg(ProcID, <<"return 1+1">>),
            #{}
        ),
    Data = hb_converge:get(<<"Results/Data">>, Msg3, #{}),
    ?assertEqual(<<"2">>, Data).
