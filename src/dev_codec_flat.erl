%%% @doc A codec for turning TABMs into/from flat Erlang maps that have 
%%% (potentially multi-layer) paths as their keys, and a normal TABM binary as 
%%% their value.
-module(dev_codec_flat).
-export([from/1, to/1, commit/3, verify/3, committed/3]).
%%% Testing utilities
-export([serialize/1, deserialize/1]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%%% Route signature functions to the `dev_codec_httpsig' module
commit(Msg, Req, Opts) -> dev_codec_httpsig:commit(Msg, Req, Opts).
verify(Msg, Req, Opts) -> dev_codec_httpsig:verify(Msg, Req, Opts).
committed(Msg, Req, Opts) -> dev_codec_httpsig:committed(Msg, Req, Opts).

%% @doc Convert a flat map to a TABM.
from(Bin) when is_binary(Bin) ->
    hb_util:ok(deserialize(Bin));
from(Map) when is_map(Map) ->
    maps:fold(
        fun(Path, Value, Acc) ->
            inject_at_path(hb_path:term_to_path_parts(Path), Value, Acc)
        end,
        #{},
        Map
    ).

%% Helper function to inject a value at a specific path in a nested map
inject_at_path([Key], Value, Map) ->
    case maps:get(Key, Map, not_found) of
        not_found ->
            Map#{ Key => Value };
        ExistingMap when is_map(ExistingMap) andalso is_map(Value) ->
            % If both are maps, merge them
            Map#{ Key => maps:merge(ExistingMap, Value) };
        OldValue ->
            % Otherwise, alert the user and fail
            throw({path_collision,
                {key, Key},
                {existing, OldValue},
                {value, Value}
            })
    end;
inject_at_path([Key|Rest], Value, Map) ->
    SubMap = maps:get(Key, Map, #{}),
    maps:put(Key, inject_at_path(Rest, Value, SubMap), Map).

%% @doc Convert a TABM to a flat map.
to(Bin) when is_binary(Bin) -> Bin;
to(Map) when is_map(Map) ->
    maps:fold(
        fun(Key, Value, Acc) ->
            case to(Value) of
                SubMap when is_map(SubMap) ->
                    maps:fold(
                        fun(SubKey, SubValue, InnerAcc) ->
                            maps:put(
                                hb_path:to_binary([Key, SubKey]),
                                SubValue,
                                InnerAcc
                            )
                        end,
                        Acc,
                        SubMap
                    );
                SimpleValue ->
                    maps:put(hb_path:to_binary([Key]), SimpleValue, Acc)
            end
        end,
        #{},
        Map
    ).

serialize(Map) when is_map(Map) ->
    Flattened = hb_message:convert(Map, <<"flat@1.0">>, #{}),
    {ok,
        iolist_to_binary(lists:foldl(
                fun(Key, Acc) ->
                    [
                        Acc,
                        hb_path:to_binary(Key),
                        <<": ">>,
                        maps:get(Key, Flattened), <<"\n">>
                    ]
                end,
                <<>>,
                maps:keys(Flattened)
            )
        )
    }.

deserialize(Bin) when is_binary(Bin) ->
    Flat = lists:foldl(
        fun(Line, Acc) ->
            case binary:split(Line, <<": ">>, [global]) of
                [Key, Value] ->
                    Acc#{ Key => Value };
                _ ->
                    Acc
            end
        end,
        #{},
        binary:split(Bin, <<"\n">>, [global])
    ),
    {ok, hb_message:convert(Flat, <<"structured@1.0">>, <<"flat@1.0">>, #{})}.

%%% Tests

simple_conversion_test() ->
    Flat = #{[<<"a">>] => <<"value">>},
    Nested = #{<<"a">> => <<"value">>},
    ?assert(hb_message:match(Nested, dev_codec_flat:from(Flat))),
    ?assert(hb_message:match(Flat, dev_codec_flat:to(Nested))).

nested_conversion_test() ->
    Flat = #{<<"a/b">> => <<"value">>},
    Nested = #{<<"a">> => #{<<"b">> => <<"value">>}},
    Unflattened = dev_codec_flat:from(Flat),
    Flattened = dev_codec_flat:to(Nested),
    ?assert(hb_message:match(Nested, Unflattened)),
    ?assert(hb_message:match(Flat, Flattened)).

multiple_paths_test() ->
    Flat = #{
        <<"x/y">> => <<"1">>,
        <<"x/z">> => <<"2">>,
        <<"a">> => <<"3">>
    },
    Nested = #{
        <<"x">> => #{
            <<"y">> => <<"1">>,
            <<"z">> => <<"2">>
        },
        <<"a">> => <<"3">>
    },
    ?assert(hb_message:match(Nested, dev_codec_flat:from(Flat))),
    ?assert(hb_message:match(Flat, dev_codec_flat:to(Nested))).

path_list_test() ->
    Nested = #{
        <<"x">> => #{
            [<<"y">>, <<"z">>] => #{
                <<"a">> => <<"2">>
            },
            <<"a">> => <<"2">>
        }
    },
    Flat = dev_codec_flat:to(Nested),
    lists:foreach(
        fun(Key) ->
            ?assert(not lists:member($\n, binary_to_list(Key)))
        end,
        maps:keys(Flat)
    ).

binary_passthrough_test() ->
	% Note: Modified for changes to the `from/1' function.
	Bin = <<"raw: binary">>,
	?assertEqual(#{<<"raw">> => <<"binary">>}, dev_codec_flat:from(Bin)),
	?assertEqual(Bin, dev_codec_flat:to(Bin)).

deep_nesting_test() ->
    Flat = #{<<"a/b/c/d">> => <<"deep">>},
    Nested = #{<<"a">> => #{<<"b">> => #{<<"c">> => #{<<"d">> => <<"deep">>}}}},
    Unflattened = dev_codec_flat:from(Flat),
    Flattened = dev_codec_flat:to(Nested),
    ?assert(hb_message:match(Nested, Unflattened)),
    ?assert(hb_message:match(Flat, Flattened)).

empty_map_test() ->
    ?assertEqual(#{}, dev_codec_flat:from(#{})),
    ?assertEqual(#{}, dev_codec_flat:to(#{})).