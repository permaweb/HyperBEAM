%%% @doc Helpers for Lua test discovery.
-module(hb_lua_test_utils).
-export([parse_spec/1]).
-include("include/hb.hrl").

%% @doc Parse a string representation of Lua test descriptions.
parse_spec(Str) when is_list(Str) ->
    parse_spec(hb_util:bin(Str));
parse_spec(tests) ->
    Files =
        case file:list_dir(ScriptDir = hb_opts:get(lua_scripts)) of
            {ok, FileList} -> FileList;
            {error, enoent} -> []
        end,
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

%% @doc Check if a string terminates with a given suffix.
terminates_with(String, Suffix) ->
    binary:longest_common_suffix(lists:map(fun hb_util:bin/1, [String, Suffix]))
        == byte_size(Suffix).
