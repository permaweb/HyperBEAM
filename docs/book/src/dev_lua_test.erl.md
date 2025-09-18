# dev_lua_test

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_lua_test.erl)

A wrapper module for generating and executing EUnit tests for all Lua modules.
When executed with `rebar3 lua-test`, this module will be invoked and scan the
`scripts` directory for all Lua files, and generate an EUnit test suite for
each one. By default, an individual test is generated for each function in
the global `_G` table that ends in `_test`.
In order to specify other tests to run instead, the user may employ the 
`LUA_TESTS` and `LUA_SCRIPTS` environment variables. The syntax for these
variables is described in the function documentation for `parse_spec`.

---

## Exported Functions

- `parse_spec/1`

---

### parse_spec

Parse a string representation of test descriptions received from the 

```erlang
parse_spec(Str) when is_list(Str) ->
    parse_spec(hb_util:bin(Str));
```

### parse_spec

Parse a string representation of test descriptions received from the 

```erlang
parse_spec(tests) ->
    % The user has not given a test spec, so we default to running all tests in
    % the `LUA_SCRIPTS' directory (defaulting to `scripts/').
```

### parse_spec

```erlang
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
```

### exec_test_

Main entrypoint for Lua tests.

```erlang
exec_test_() ->
    ScriptDefs = hb_opts:get(lua_tests),
    lists:map(
        fun({File, Funcs}) -> suite(File, Funcs) end,
        ScriptDefs
    ).
```

### suite

Generate an EUnit test suite for a given Lua script. If the `Funcs` is

```erlang
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
```

### new_state

Create a new Lua environment for a given script.

```erlang
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
```

### exec_test

Generate an EUnit test for a given function.

```erlang
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
            hb_format:print(Result, <<"Lua">>, Function, 1),
            ?assertEqual(
                ok,
                Status
            )
    end.
```

### terminates_with

Check if a string terminates with a given suffix.

```erlang
terminates_with(String, Suffix) ->
    binary:longest_common_suffix(lists:map(fun hb_util:bin/1, [String, Suffix]))
```

---

*Generated from [dev_lua_test.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_lua_test.erl)*
