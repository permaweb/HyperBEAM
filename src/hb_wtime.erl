-module(hb_wtime).

-export([
    create/1,
    meta/1,
    call_begin/3,
    call_continue/4,
    call_cancel/3,
    mem_size/1,
    mem_read/3,
    mem_write/3,
    call/6
]).

-include("cargo.hrl").
-on_load(init/0).
-define(NOT_LOADED, not_loaded(?LINE)).

-include("src/include/hb.hrl").
% -hb_debug(print).

%%%===================================================================
%%% API
%%%===================================================================

create(_Bin) ->
    ?NOT_LOADED.

meta(_Context) ->
    ?NOT_LOADED.

call_begin(_Context, _Func, _Args) ->
    ?NOT_LOADED.

call_continue(_Context, _Module, _Field, _Results) ->
    ?NOT_LOADED.

call_cancel(_Context, _Module, _Field) ->
    ?NOT_LOADED.

mem_size(_Context) ->
    ?NOT_LOADED.

mem_read(_Context, _Offset, _Length) ->
    ?NOT_LOADED.

mem_write(_Context, _Offset, _Data) ->
    ?NOT_LOADED.

%%%===================================================================
%%% NIF
%%%===================================================================

init() ->
    ?load_nif_from_crate(hb_wtime, 0).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

%%%===================================================================
%%% High-level call API
%%%===================================================================

find_import_in_list([], _FunctionToCompare) ->
    false;
find_import_in_list([{func, ImportModule, ImportField, ParamTypes, ResultTypes} | RestImports], {TestImportModule, TestImportField})
  when is_binary(ImportModule) and is_binary(ImportField) ->
    if
        ImportModule == TestImportModule andalso ImportField == TestImportField ->
            {true, {matched_import_results, ParamTypes, ResultTypes}};
        true ->
            find_import_in_list(RestImports, {TestImportModule, TestImportField})
    end;
find_import_in_list([_ | RestImports], {TestImportModule, TestImportField}) -> 
    find_import_in_list(RestImports, {TestImportModule, TestImportField}).

get_import_func_type(InstMeta, {ImportModule, ImportField}) ->
    ?event(get_import_func_type),
    {{imports, Imports}, _} = InstMeta,
    % ?event({get_import_res_type_inputs, #{imports => Imports, import_module => ImportModule, import_field => ImportField}}),
    FoundImport = find_import_in_list(Imports, {ImportModule, ImportField}),
    ?event({get_import_res_type_found_import, FoundImport}),
    ResType = case FoundImport of
        {true, {matched_import_results, ParamTypes, ResultTypes}} ->
            {ok, {params, ParamTypes}, {results, ResultTypes}};
        false -> {error, no_import_found}
    end,
    ?event({get_import_res_type_final_res_type, ResType}),
    ResType.

type_list_to_string_parts([]) ->
    [];
type_list_to_string_parts([Atom]) when is_atom(Atom) ->
    [atom_to_binary(Atom, utf8)];
type_list_to_string_parts([Atom | Rest]) when is_atom(Atom) ->
    [atom_to_binary(Atom, utf8), <<", ">> | type_list_to_string_parts(Rest)].

func_type_atoms_to_sig(ParamTypes, ResultTypes) ->
    ?event({func_type_atoms_to_sig, {params, ParamTypes}, {results, ResultTypes}}),
    ParamsString = iolist_to_binary(type_list_to_string_parts(ParamTypes)),
    ResultsString = iolist_to_binary(type_list_to_string_parts(ResultTypes)),
    Sig = <<"(", ParamsString/binary, ") -> (", ResultsString/binary, ")">>,
    ?event({func_type_atoms_to_sig_final, Sig}),
    Sig.

%% Internal helper function to handle the host import loop.
call_loop(Instance, InstMeta, ImportModule, ImportField, ImportParams, ImportResolver, CurrentState, Opts) ->
    ?event({call_loop_begin, {{module, ImportModule}, {field, ImportField}, {params, ImportParams}, {import_resolver, ImportResolver}}}),
    {ok, {params, ParamTypes}, {results, ResultTypes}} = get_import_func_type(InstMeta, {ImportModule, ImportField}),
    ?event({call_loop_params, {param_types, ParamTypes}, {result_types, ResultTypes}}),
    Signature = func_type_atoms_to_sig(ParamTypes, ResultTypes),
    ?event({call_loop_signature, Signature}),
    ImportRes = ImportResolver(CurrentState, #{
        instance => Instance,
        module => ImportModule,
        func => ImportField,
        args => ImportParams,
        func_sig => Signature
    }, Opts),
    ?event({
        import_resolver_result,
        {in,  ImportModule, ImportField, ImportParams},
        {out, ImportRes}
    }),
    case ImportRes of
        {ok, HostResultList, NextState} ->
            % Resume Wasm execution with the host result
            case hb_wtime:call_continue(Instance, ImportModule, ImportField, HostResultList) of
                {ok, complete, FinalResultList} ->
                    {ok, FinalResultList, ok, NextState}; % Final completion
                {ok, import, [NextImportModule, NextImportField, NextImportParams]} ->
                    % Another import was needed, continue the loop
                    call_loop(Instance, InstMeta, NextImportModule, NextImportField, NextImportParams, ImportResolver, NextState, Opts);
                {error, Reason} ->
                    {error, {call_continue_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {import_resolver_failed, Reason}}
    end.

call(Instance, Function, Params, ImportResolver, State1, Opts) ->
    ?event({call_begin, Instance, Function, Params}),
    {ok, InstMeta} = hb_wtime:meta(Instance),
    case hb_wtime:call_begin(Instance, Function, Params) of
        {ok, complete, ResultList} ->
            ?event({call_complete, Instance, Function, Params, ResultList}),
            {ok, ResultList, ok, State1};
        {ok, import, [ImportModule, ImportField, ImportParams]} ->
            % Start the import resolution loop
            call_loop(Instance, InstMeta, ImportModule, ImportField, ImportParams, ImportResolver, State1, Opts);
        {error, Reason} ->
            ?event({call_begin_failed, Instance, Function, Params, Reason}),
            {error, {call_begin_failed, Reason}}
    end.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

create_test() ->
    % Minimal valid WASM binary
    Bin = <<0, 97, 115, 109, 1, 0, 0, 0>>,
    {ok, _Inst} = create(Bin),
    ok.

create_error_test() ->
    % Invalid WASM binary
    Bin = <<0, 0, 0, 0, 0, 0, 0, 0>>,
    {error, Error} = create(Bin),
    ?event({invalid_wasm_error_raw, {explicit, Error}}),
    ok.

meta_test() ->
    Bin =
        <<
            "(module\n"
            "  (import \"env\" \"some_import_func\" (func $some_import_func (param i32) (result i32)))\n"
            "  (memory (export \"memory\") 1)\n"
            "  (func (export \"some_export_func\") (result f64)\n"
            "    f64.const 1.0\n"
            "  )\n"
            ")"
        >>,
    {ok, Inst} = create(Bin),
    {ok, Meta} = meta(Inst),
    {{imports, Imports}, {exports, Exports}} = Meta,
    ?assertEqual([
        {func, <<"env">>, <<"some_import_func">>, [i32], [i32]}
    ], Imports),
    ?assertEqual([
        {memory, <<"memory">>},
        {func, <<"some_export_func">>, [], [f64]}
    ], Exports),
    ok.

call_start_test() ->
    {ok, Bin} = file:read_file("test/test.wasm"),
    {ok, Inst} = create(Bin),
    {ok, complete, Res} = call_begin(Inst, <<"fac">>, [5.0]),
    ?assertEqual([120.0], Res),
    ok.

call_resume_test() ->
    {ok, Bin} = file:read_file("test/pow_calculator.wasm"),
    {ok, Inst} = create(Bin),
    Mod = <<"my_lib">>,
    Field = <<"mul">>,
    {ok, import, [Mod, Field, Res1]} = call_begin(Inst, <<"pow">>, [2, 2]),
    ?assertEqual([1, 2], Res1),
    {ok, import, [Mod, Field, [Res2A, Res2B]]} = call_continue(Inst, Mod, Field, [1 * 2]),
    {ok, complete, Res3} = call_continue(Inst, Mod, Field, [Res2A * Res2B]),
    ?assertEqual([4], Res3),
    ok.

nested_call_test() ->
    WAT =
        <<
            "(module\n"
            "  (import \"env\" \"host_A\" (func $host_A (result i32)))\n"
            "  (func (export \"outer_func\") (result i32)\n"
            "    call $host_A\n"
            "  )\n"
            "  (func (export \"inner_func\") (result i32)\n"
            "    i32.const 99\n"
            "  )\n"
            ")"
        >>,
    {ok, Inst} = create(WAT),
    % 1. Start outer_func, it should call host_A
    {ok, import, [EnvModuleA, FuncNameA, _ParamsA]} = call_begin(Inst, <<"outer_func">>, []),
    ?assertEqual(<<"env">>, EnvModuleA),
    ?assertEqual(<<"host_A">>, FuncNameA),
    % 2. While outer_func is awaiting host_A, call inner_func (nesting)
    %    call_begin should allow this because the FSM is in AwaitingHost for the outer call.
    {ok, complete, [InnerResult]} = call_begin(Inst, <<"inner_func">>, []),
    ?assertEqual(99, InnerResult),
    % 3. Provide response for host_A to resume outer_func
    HostAResponse = 42,
    {ok, complete, [OuterResult]} = call_continue(Inst, EnvModuleA, FuncNameA, [HostAResponse]),
    ?assertEqual(HostAResponse, OuterResult),
    ok.

mem_size_read_write_test() ->
    WAT = <<"(module (memory (export \"memory\") 1))">>,
    {ok, Inst} = create(WAT),
    % Memory size for 1 page
    {ok, Size} = mem_size(Inst),
    ?assertEqual(65536, Size), % 64KiB
    % Write data to memory
    InitialOffset = 5,
    WriteData = <<"Hello Wasmtime!\x00\x99">>,
    ?assertEqual(ok, mem_write(Inst, InitialOffset, WriteData)),
    % Read subset of data back from memory
    AdditionalOffset = 10,
    ReadOffset = InitialOffset + AdditionalOffset,
    ReadLength = size(WriteData) - AdditionalOffset,
    {ok, ReadData} = mem_read(Inst, ReadOffset, ReadLength),
    ?assertEqual(<<"time!\x00\x99">>, ReadData),
    ok.

mem_size_zero_test() ->
    WAT = <<"(module (memory (export \"memory\") 0))">>,
    {ok, Inst} = create(WAT),
    {ok, 0} = mem_size(Inst),
    ok.

mem_size_not_found_test() ->
    WAT = <<"(module)">>,
    {ok, Inst} = create(WAT),
    {ok, not_found} = mem_size(Inst),
    ok.

-endif.
