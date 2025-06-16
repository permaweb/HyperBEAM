# [Module dev_lua_test.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_lua_test.erl)




<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#exec_test-2">exec_test/2*</a></td><td>Generate an EUnit test for a given function.</td></tr><tr><td valign="top"><a href="#exec_test_-0">exec_test_/0*</a></td><td>Main entrypoint for Lua tests.</td></tr><tr><td valign="top"><a href="#new_state-1">new_state/1*</a></td><td>Create a new Lua environment for a given script.</td></tr><tr><td valign="top"><a href="#parse_spec-1">parse_spec/1</a></td><td>Parse a string representation of test descriptions received from the
command line via the <code>LUA_TESTS</code> environment variable.</td></tr><tr><td valign="top"><a href="#suite-2">suite/2*</a></td><td>Generate an EUnit test suite for a given Lua script.</td></tr><tr><td valign="top"><a href="#terminates_with-2">terminates_with/2*</a></td><td>Check if a string terminates with a given suffix.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="exec_test-2"></a>

### exec_test/2 * ###

`exec_test(State, Function) -> any()`

Generate an EUnit test for a given function.

<a name="exec_test_-0"></a>

### exec_test_/0 * ###

`exec_test_() -> any()`

Main entrypoint for Lua tests.

<a name="new_state-1"></a>

### new_state/1 * ###

`new_state(File) -> any()`

Create a new Lua environment for a given script.

<a name="parse_spec-1"></a>

### parse_spec/1 ###

`parse_spec(Str) -> any()`

Parse a string representation of test descriptions received from the
command line via the `LUA_TESTS` environment variable.

Supported syntax in loose BNF/RegEx:

Definitions := (ModDef,)+
ModDef      := ModName(TestDefs)?
ModName     := ModuleInLUA_SCRIPTS|(FileName[.lua])?
TestDefs    := (:TestDef)+
TestDef     := TestName

File names ending in `.lua` are assumed to be relative paths from the current
working directory. Module names lacking the `.lua` extension are assumed to
be modules found in the `LUA_SCRIPTS` environment variable (defaulting to
`scripts/`).

For example, to run a single test one could call the following:

LUA_TESTS=~/src/LuaScripts/test.yourTest rebar3 lua-tests

To specify that one would like to run all of the tests in the
`scripts/test.lua` file and two tests from the `scripts/test2.lua` file, the
user could provide the following test definition:

LUA_TESTS="test,scripts/test2.userTest1|userTest2" rebar3 lua-tests

<a name="suite-2"></a>

### suite/2 * ###

`suite(File, Funcs) -> any()`

Generate an EUnit test suite for a given Lua script. If the `Funcs` is
the atom `tests` we find all of the global functions in the script, then
filter for those ending in `_test` in a similar fashion to Eunit.

<a name="terminates_with-2"></a>

### terminates_with/2 * ###

`terminates_with(String, Suffix) -> any()`

Check if a string terminates with a given suffix.

