%%% A wrapper module for generating and executing EUnit tests for all Lua modules.
%%% When executed with `rebar3 lua-test`, this module will be invoked and scan the
%%% `scripts' directory for all Lua files, and generate an EUnit test suite for
%%% each one. By default, an individual test is generated for each function in
%%% the global `_G' table that ends in `_test'.
%%% 
%%% In order to specify other tests to run instead, the user may employ the 
%%% `LUA_TESTS' and `LUA_SCRIPTS' environment variables. The syntax for these
%%% variables is described in the function documentation for `parse_spec'.
%%% 
-module(dev_lua_test).
-export([parse_spec/1]).
-include_lib("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Parse a string representation of test descriptions received from the 
%% command line via the `LUA_TESTS' environment variable.
%% 
%% Supported syntax in loose BNF/RegEx:
%% 
%%      Definitions := (ModDef,)+
%%      ModDef      := ModName(TestDefs)?
%%      ModName     := ModuleInLUA_SCRIPTS|(FileName[.lua])?
%%      TestDefs    := (:TestDef)+
%%      TestDef     := TestName
%% 
%% File names ending in `.lua' are assumed to be relative paths from the current
%% working directory. Module names lacking the `.lua' extension are assumed to
%% be modules found in the `LUA_SCRIPTS' environment variable (defaulting to
%% `scripts/').
%% 
%% For example, to run a single test one could call the following:
%% 
%%      LUA_TESTS=~/src/LuaScripts/test.yourTest rebar3 lua-tests
%% 
%% To specify that one would like to run all of the tests in the 
%% `scripts/test.lua' file and two tests from the `scripts/test2.lua' file, the
%% user could provide the following test definition:
%% 
%%      LUA_TESTS="test,scripts/test2.userTest1|userTest2" rebar3 lua-tests
%% 
parse_spec(Str) when is_list(Str) ->
    parse_spec(hb_util:bin(Str));
parse_spec(tests) ->
    % The user has not given a test spec, so we default to running all tests in
    % the `LUA_SCRIPTS' directory (defaulting to `scripts/').
    {ok, Files} = file:list_dir(ScriptDir = hb_opts:get(lua_scripts)),
    RelevantFiles = 
        lists:filter(
            fun(File) ->
                terminates_with(File, <<"lua">>)
            end,
            Files
        ),
    ?event({loading_scripts, RelevantFiles}),
    [
        {
            <<
                (hb_util:bin(ScriptDir))/binary,
                "/",
                (hb_util:bin(File))/binary
            >>,
            tests
        }
    ||
        File <- RelevantFiles
    ];
parse_spec(Str) ->
    lists:map(
        fun(ModDef) ->
            [ModName|TestDefs] = binary:split(ModDef, <<":">>, [global, trim_all]),
            ScriptDir = hb_util:bin(hb_opts:get(lua_scripts)),
            File =
                case terminates_with(ModName, <<".lua">>) of
                    true -> ModName;
                    false -> << ScriptDir/binary, "/", ModName/binary, ".lua" >>
                end,
            Tests =
                case TestDefs of
                    [] -> tests;
                    TestDefs -> TestDefs
                end,
            {File, Tests}
        end,
        binary:split(Str, <<",">>, [global, trim_all])
    ).

%% @doc Main entrypoint for Lua tests.
exec_test_() ->
    ScriptDefs = hb_opts:get(lua_tests),
    lists:map(
        fun({File, Funcs}) -> suite(File, Funcs) end,
        ScriptDefs
    ).

%% @doc Generate an EUnit test suite for a given Lua script. If the `Funcs' is
%% the atom `tests' we find all of the global functions in the script, then 
%% filter for those ending in `_test' in a similar fashion to Eunit.
suite(File, Funcs) ->
    {ok, State} = new_state(File),
    {foreach,
        fun() -> ok end,
        fun(_) -> ok end,
        lists:map(
            fun(FuncName) ->
                {
                    hb_util:list(File) ++ ":" ++ hb_util:list(FuncName),
                    fun() -> exec_test(State, FuncName) end
                }
            end,
            case Funcs of
                tests ->
                    lists:filter(
                        fun(FuncName) ->
                            terminates_with(FuncName, <<"_test">>)
                        end,
                        hb_ao:get(<<"functions">>, State, #{})
                    );
                FuncNames -> FuncNames
            end
        )
    }.

%% @doc Create a new Lua environment for a given script.
new_state(File) ->
    ?event(debug_lua_test, {generating_state_for, File}),
    {ok, Module} = file:read_file(hb_util:list(File)),
    {ok, _} =
        hb_ao:resolve(
            #{
                <<"device">> => <<"lua@5.3a">>,
                <<"module">> => #{
                    <<"content-type">> => <<"application/lua">>,
                    <<"name">> => File,
                    <<"body">> => Module
                }
            },
            <<"init">>,
            #{}
        ).

%% @doc Generate an EUnit test for a given function.
exec_test(State, Function) ->
    {Status, Result} =
        hb_ao:resolve(
            State,
            #{ <<"path">> => Function, <<"parameters">> => [] },
            #{}
        ),
    case Status of
        ok -> ok;
        error ->
            hb_util:debug_print(Result, <<"Lua">>, Function, 1),
            ?assertEqual(
                ok,
                Status
            )
    end.

%%% Utility functions.

%% @doc Check if a string terminates with a given suffix.
terminates_with(String, Suffix) ->
    binary:longest_common_suffix(lists:map(fun hb_util:bin/1, [String, Suffix]))
        == byte_size(Suffix).