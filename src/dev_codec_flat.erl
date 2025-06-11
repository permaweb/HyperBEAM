%%% @doc A codec for turning TABMs into/from flat Erlang maps that have
%%% (potentially multi-layer) paths as their keys, and a normal TABM binary as
%%% their value.
-module(dev_codec_flat).
-export([from/3, to/3, commit/3, verify/3]).
%%% Testing utilities
-export([serialize/1, serialize/2, deserialize/1]).
-include_lib("eunit/include/eunit.hrl").

-include("include/hb.hrl").


%%% Route signature functions to the `dev_codec_httpsig' module
commit(Msg, Req, Opts) -> dev_codec_httpsig:commit(Msg, Req, Opts).


verify(Msg, Req, Opts) -> dev_codec_httpsig:verify(Msg, Req, Opts).


%% @doc Convert a flat map to a TABM.
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
                               {map, Map}});
                   _ ->
                       ok
               end,
               inject_at_path(
                 hb_path:term_to_path_parts(Path, Opts),
                 hb_util:ok(from(Value, Req, Opts)),
                 Acc,
                 Opts)
       end,
       #{},
       Map)}.


%% Helper function to inject a value at a specific path in a nested map
inject_at_path([Key], Value, Map) ->
    inject_at_path([Key], Value, Map, #{});

inject_at_path([Key | Rest], Value, Map) ->
    inject_at_path([Key | Rest], Value, Map, #{}).


inject_at_path([Key], Value, Map, Opts) ->
    case maps:get(Key, Map, not_found) of
        not_found ->
            Map#{Key => Value};
        ExistingMap when is_map(ExistingMap) andalso is_map(Value) ->
            % If both are maps, merge them
            Map#{Key => hb_maps:merge(ExistingMap, Value, Opts)};
        OldValue ->
            % Otherwise, alert the user and fail
            throw({path_collision,
                   {key, Key},
                   {existing, OldValue},
                   {value, Value}})
    end;

inject_at_path([Key | Rest], Value, Map, Opts) ->
    SubMap = hb_maps:get(Key, Map, #{}, Opts),
    hb_maps:put(Key, inject_at_path(Rest, Value, SubMap, Opts), Map, Opts).


%% @doc Convert a TABM to a flat map.
to(Bin, _, _Opts) when is_binary(Bin) -> {ok, Bin};
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
                                      InnerAcc)
                            end,
                            Acc,
                            SubMap);
                      {ok, SimpleValue} ->
                          maps:put(hb_path:to_binary([Key]), SimpleValue, Acc)
                  end
          end,
          #{},
          Map),
    {ok, Res}.


serialize(Map) when is_map(Map) ->
    serialize(Map, #{}).


serialize(Map, Opts) when is_map(Map) ->
    Flattened = hb_message:convert(Map, <<"flat@1.0">>, #{}),
    {ok,
     iolist_to_binary(lists:foldl(
                        fun(Key, Acc) ->
                                [Acc,
                                 hb_path:to_binary(Key),
                                 <<": ">>,
                                 hb_maps:get(Key, Flattened, Opts),
                                 <<"\n">>]
                        end,
                        <<>>,
                        hb_util:to_sorted_keys(Flattened, Opts)))}.


deserialize(Bin) when is_binary(Bin) ->
    Flat = lists:foldl(
             fun(Line, Acc) ->
                     case binary:split(Line, <<": ">>, [global]) of
                         [Key, Value] ->
                             Acc#{Key => Value};
                         _ ->
                             Acc
                     end
             end,
             #{},
             binary:split(Bin, <<"\n">>, [global])),
    {ok, hb_message:convert(Flat, <<"structured@1.0">>, <<"flat@1.0">>, #{})}.


%%% Tests


simple_conversion_test() ->
    Flat = #{[<<"a">>] => <<"value">>},
    Nested = #{<<"a">> => <<"value">>},
    ?assert(hb_message:match(Nested, hb_util:ok(dev_codec_flat:from(Flat, #{}, #{})))),
    ?assert(hb_message:match(Flat, hb_util:ok(dev_codec_flat:to(Nested, #{}, #{})))).


nested_conversion_test() ->
    Flat = #{<<"a/b">> => <<"value">>},
    Nested = #{<<"a">> => #{<<"b">> => <<"value">>}},
    Unflattened = hb_util:ok(dev_codec_flat:from(Flat, #{}, #{})),
    Flattened = hb_util:ok(dev_codec_flat:to(Nested, #{}, #{})),
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
    ?assert(hb_message:match(Nested, hb_util:ok(dev_codec_flat:from(Flat, #{}, #{})))),
    ?assert(hb_message:match(Flat, hb_util:ok(dev_codec_flat:to(Nested, #{}, #{})))).


path_list_test() ->
    Nested = #{
               <<"x">> => #{
                            [<<"y">>, <<"z">>] => #{
                                                    <<"a">> => <<"2">>
                                                   },
                            <<"a">> => <<"2">>
                           }
              },
    Flat = hb_util:ok(dev_codec_flat:to(Nested, #{}, #{})),
    lists:foreach(
      fun(Key) ->
              ?assert(not lists:member($\n, binary_to_list(Key)))
      end,
      hb_maps:keys(Flat, #{})).


binary_passthrough_test() ->
    Bin = <<"raw binary">>,
    ?assertEqual(Bin, hb_util:ok(dev_codec_flat:from(Bin, #{}, #{}))),
    ?assertEqual(Bin, hb_util:ok(dev_codec_flat:to(Bin, #{}, #{}))).


deep_nesting_test() ->
    Flat = #{<<"a/b/c/d">> => <<"deep">>},
    Nested = #{<<"a">> => #{<<"b">> => #{<<"c">> => #{<<"d">> => <<"deep">>}}}},
    Unflattened = hb_util:ok(dev_codec_flat:from(Flat, #{}, #{})),
    Flattened = hb_util:ok(dev_codec_flat:to(Nested, #{}, #{})),
    ?assert(hb_message:match(Nested, Unflattened)),
    ?assert(hb_message:match(Flat, Flattened)).


empty_map_test() ->
    ?assertEqual(#{}, hb_util:ok(dev_codec_flat:from(#{}, #{}, #{}))),
    ?assertEqual(#{}, hb_util:ok(dev_codec_flat:to(#{}, #{}, #{}))).
