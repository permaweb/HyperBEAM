%%% @doc A codec for turning TABMs into/from flat Erlang maps that have 
%%% (potentially multi-layer) paths as their keys, and a normal TABM binary as 
%%% their value.
-module(dev_flat).
-specification("../../../specs/flat@1.0.md").
-export([from/3, to/3, commit/3, verify/3, deserialize/3]).
%%% Testing utilities
-export([serialize/1, serialize/2, deserialize/1]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Route commitments through `httpsig@1.0'.
-spec commit(#{ _ => _ }, #{ _ => _ }, map()) -> term().
commit(Msg, Req, Opts) ->
    % `hb_message:commit/3' without its first line, which takes the node key:
    % a client can call this.
    {ok, Structured} =
        hb_ao:raw(<<"structured@1.0">>, <<"to">>, Msg, #{}, Opts),
    hb_ao:raw(
        <<"message@1.0">>,
        <<"commit">>,
        Structured,
        Req#{ <<"commitment-device">> => <<"httpsig@1.0">> },
        Opts
    ).

%% @doc Route verification through `httpsig@1.0'.
-spec verify(#{ _ => _ }, #{ _ => _ }, map()) -> term().
verify(Msg, Req, Opts) ->
    hb_ao:raw(<<"httpsig@1.0">>, <<"verify">>, Msg, Req, Opts).

%% @doc Convert a flat map to a TABM.
-spec from(binary() | #{ _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, binary() | #{ _ => _ }}.
from(Bin, _, _Opts) when is_binary(Bin) -> {ok, Bin};
from(Map, Req, Opts) when is_map(Map) ->
    {ok,
        maps:fold(
            fun(Path, Value, Acc) ->
                case Value of
                    [] ->
                        ?event(error,
                            {empty_list_value,
                                {path, Path},
                                {value, Value},
                                {map, Map}
                            }
                        );
                    _ ->
                        ok
                end,
                hb_util:deep_set(
                    hb_path:term_to_path_parts(Path, Opts),
                    hb_util:ok(from(Value, Req, Opts)),
                    Acc,
                    Opts
                )
            end,
            #{},
            Map
        )
    }.

%% @doc Convert a TABM to a flat map.
-spec to(binary() | [_] | #{ _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, binary() | #{ _ => _ }}.
to(Bin, _, _Opts) when is_binary(Bin) -> {ok, Bin};
to(List, Req, Opts) when is_list(List) ->
    % `structured@1.0' gives the list as a numbered message, marked as a list.
    to(
        hb_message:convert(
            List,
            tabm,
            #{
                <<"device">> => <<"structured@1.0">>,
                <<"encode-types">> => [<<"list">>],
                <<"bundle">> => true
            },
            Opts
        ),
        Req,
        Opts
    );
to(Map, Req, Opts) when is_map(Map) ->
    Res = 
        maps:fold(
            fun(Key, Value, Acc) ->
                case to(Value, Req, Opts) of
                    {ok, SubMap} when is_map(SubMap) ->
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
                    {ok, SimpleValue} ->
                        maps:put(hb_path:to_binary([Key]), SimpleValue, Acc)
                end
            end,
            #{},
            Map
        ),
    {ok, Res}.

serialize(Map) when is_map(Map) ->
    serialize(Map, #{}).

serialize(Map, Opts) when is_map(Map) ->
    Flattened = hb_message:convert(hb_private:reset(Map), <<"flat@1.0">>, #{}),
    {ok,
        iolist_to_binary(lists:foldl(
                fun(Key, Acc) ->
                    [
                        Acc,
                        hb_path:to_binary(Key),
                        <<": ">>,
                        hb_maps:get(Key, Flattened, not_found, Opts), <<"\n">>
                    ]
                end,
                <<>>,
                hb_util:to_sorted_keys(Flattened, Opts)
            )
        )
    }.

deserialize(Bin) when is_binary(Bin) ->
    Flat = lists:foldl(
        fun({_, <<>>}, Acc) -> Acc;
           ({N, Line}, Acc) ->
            case binary:split(Line, <<": ">>, [global]) of
                [Key, Value] ->
                    Acc#{ Key => Value };
                _ ->
                    error({invalid_line, N})
            end
        end,
        #{},
        lists:enumerate(binary:split(Bin, <<"\n">>, [global]))
    ),
    {ok, hb_message:convert(Flat, <<"structured@1.0">>, <<"flat@1.0">>, #{})}.

%% @doc Parse flat text into a TABM.
deserialize(Bin, _Req, _Opts) ->
    deserialize(Bin).

%%% Tests

simple_conversion_test() ->
    Flat = #{[<<"a">>] => <<"value">>},
    Nested = #{<<"a">> => <<"value">>},
    ?assert(hb_message:match(Nested, hb_util:ok(dev_flat:from(Flat, #{}, #{})))),
    ?assert(hb_message:match(Flat, hb_util:ok(dev_flat:to(Nested, #{}, #{})))).

nested_conversion_test() ->
    Flat = #{<<"a/b">> => <<"value">>},
    Nested = #{<<"a">> => #{<<"b">> => <<"value">>}},
    Unflattened = hb_util:ok(dev_flat:from(Flat, #{}, #{})),
    Flattened = hb_util:ok(dev_flat:to(Nested, #{}, #{})),
    ?assert(hb_message:match(Nested, Unflattened)),
    ?assert(hb_message:match(Flat, Flattened)),
    % Native TABM lists round-trip through flat encoding.
    ?assertEqual([<<"description">>],
        hb_message:convert(
            hb_message:convert([<<"description">>], <<"flat@1.0">>, tabm, #{}),
            <<"structured@1.0">>,
            <<"flat@1.0">>,
            #{}
        )
    ).

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
    ?assert(hb_message:match(Nested, hb_util:ok(dev_flat:from(Flat, #{}, #{})))),
    ?assert(hb_message:match(Flat, hb_util:ok(dev_flat:to(Nested, #{}, #{})))).

path_list_test() ->
    Nested = #{
        <<"x">> => #{
            [<<"y">>, <<"z">>] => #{
                <<"a">> => <<"2">>
            },
            <<"a">> => <<"2">>
        }
    },
    Flat = hb_util:ok(dev_flat:to(Nested, #{}, #{})),
    lists:foreach(
        fun(Key) ->
            ?assert(not lists:member($\n, binary_to_list(Key)))
        end,
        hb_maps:keys(Flat, #{})
    ).

binary_passthrough_test() ->
    Bin = <<"raw binary">>,
    ?assertEqual(Bin, hb_util:ok(dev_flat:from(Bin, #{}, #{}))),
    ?assertEqual(Bin, hb_util:ok(dev_flat:to(Bin, #{}, #{}))).

deep_nesting_test() ->
    Flat = #{<<"a/b/c/d">> => <<"deep">>},
    Nested = #{<<"a">> => #{<<"b">> => #{<<"c">> => #{<<"d">> => <<"deep">>}}}},
    Unflattened = hb_util:ok(dev_flat:from(Flat, #{}, #{})),
    Flattened = hb_util:ok(dev_flat:to(Nested, #{}, #{})),
    ?assert(hb_message:match(Nested, Unflattened)),
    ?assert(hb_message:match(Flat, Flattened)).

empty_map_test() ->
    ?assertEqual(#{}, hb_util:ok(dev_flat:from(#{}, #{}, #{}))),
    ?assertEqual(#{}, hb_util:ok(dev_flat:to(#{}, #{}, #{}))).
