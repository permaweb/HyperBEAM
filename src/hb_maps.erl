%%% @doc An abstraction for working with maps in HyperBEAM, matching the
%%% generic `maps' module, but additionally supporting the resolution of
%%% links as they are encountered. These functions must be used extremely
%%% carefully. In virtually all circumstances, the `hb_ao:resolve/3' or
%%% `hb_ao:get/3' functions should be used instead, as they will execute the
%%% full AO-Core protocol upon requests (normalizing keys, applying the
%%% appropriate device's functions, as well as resolving links). By using this
%%% module's functions, you are implicitly making the assumption that the message
%%% in question is of the `~message@1.0' form, ignoring any other keys that its
%%% actual device may present. This module is intended for the extremely rare
%%% circumstances in which the additional overhead of the full AO-Core
%%% execution cycle is not acceptable, and the data in question is known to
%%% conform to the `~message@1.0' form.
%%%
%%% If you do not understand any/all of the above, you are in the wrong place!
%%% Utilise the `hb_ao' module and read the documentation therein, saving
%%% yourself from the inevitable issues that will arise from using this
%%% module without understanding the full implications. You have been warned.
-module(hb_maps).
-export([get/2, get/3, get/4, put/3, put/4, find/2, find/3]).
-export([is_key/2, is_key/3, keys/1, keys/2, values/1, values/2]).
-export([map/2, map/3, filter/2, filter/3, filtermap/2, filtermap/3]).
-export([fold/3, fold/4, take/2, take/3, size/1, size/2]).
-export([merge/2, merge/3, remove/2, remove/3]).
-export([with/2, with/3, without/2, without/3, update_with/3, update_with/4]).
-export([from_list/1, to_list/1, to_list/2]).
-include_lib("eunit/include/eunit.hrl").


-spec get(Key :: term(), Map :: map()) -> term().
get(Key, Map) ->
    get(Key, Map, undefined).


-spec get(Key :: term(), Map :: map(), Default :: term()) -> term().
get(Key, Map, Default) ->
    get(Key, Map, Default, #{}).


%% @doc Get a value from a map, resolving links as they are encountered in both
%% the TABM encoded link format, as well as the structured type.
-spec get(Key :: term(),
          Map :: map(),
          Default :: term(),
          Opts :: map()) -> term().
get(Key, Map, Default, Opts) ->
    hb_cache:ensure_loaded(
      maps:get(
        Key,
        hb_cache:ensure_loaded(Map, Opts),
        Default),
      Opts).


-spec find(Key :: term(), Map :: map()) -> {ok, term()} | error.
find(Key, Map) ->
    find(Key, Map, #{}).


-spec find(Key :: term(), Map :: map(), Opts :: map()) -> {ok, term()} | error.
find(Key, Map, Opts) ->
    hb_cache:ensure_loaded(maps:find(Key, hb_cache:ensure_loaded(Map, Opts)), Opts).


-spec put(Key :: term(), Value :: term(), Map :: map()) -> map().
put(Key, Value, Map) ->
    put(Key, Value, Map, #{}).


-spec put(Key :: term(),
          Value :: term(),
          Map :: map(),
          Opts :: map()) -> map().
put(Key, Value, Map, Opts) ->
    maps:put(Key, Value, hb_cache:ensure_loaded(Map, Opts)).


-spec is_key(Key :: term(), Map :: map()) -> boolean().
is_key(Key, Map) ->
    is_key(Key, Map, #{}).


-spec is_key(Key :: term(), Map :: map(), Opts :: map()) -> boolean().
is_key(Key, Map, Opts) ->
    maps:is_key(Key, hb_cache:ensure_loaded(Map, Opts)).


-spec keys(Map :: map()) -> [term()].
keys(Map) ->
    keys(Map, #{}).


-spec keys(Map :: map(), Opts :: map()) -> [term()].
keys(Map, Opts) ->
    maps:keys(hb_cache:ensure_loaded(Map, Opts)).


-spec values(Map :: map()) -> [term()].
values(Map) -> values(Map, #{}).


-spec values(Map :: map(), Opts :: map()) -> [term()].
values(Map, Opts) ->
    maps:values(hb_cache:ensure_loaded(Map, Opts)).


-spec size(Map :: map()) -> non_neg_integer().
size(Map) ->
    size(Map, #{}).


-spec size(Map :: map(), Opts :: map()) -> non_neg_integer().
size(Map, Opts) ->
    maps:size(hb_cache:ensure_loaded(Map, Opts)).


-spec map(Fun :: fun((Key :: term(), Value :: term()) -> term()),
          Map :: map()) -> map().
map(Fun, Map) ->
    map(Fun, Map, #{}).


-spec map(Fun :: fun((Key :: term(), Value :: term()) -> term()),
          Map :: map(),
          Opts :: map()) -> map().
map(Fun, Map, Opts) ->
    maps:map(
      fun(K, V) -> Fun(K, hb_cache:ensure_loaded(V, Opts)) end,
      hb_cache:ensure_loaded(Map, Opts)).


-spec merge(Map1 :: map(), Map2 :: map()) -> map().
merge(Map1, Map2) ->
    merge(Map1, Map2, #{}).


-spec merge(Map1 :: map(), Map2 :: map(), Opts :: map()) -> map().
merge(Map1, Map2, Opts) ->
    maps:merge(hb_cache:ensure_loaded(Map1, Opts), hb_cache:ensure_loaded(Map2, Opts)).


-spec remove(Key :: term(), Map :: map()) -> map().
remove(Key, Map) ->
    remove(Key, Map, #{}).


-spec remove(Key :: term(), Map :: map(), Opts :: map()) -> map().
remove(Key, Map, Opts) ->
    maps:remove(Key, hb_cache:ensure_loaded(Map, Opts)).


-spec with(Keys :: [term()], Map :: map()) -> map().
with(Keys, Map) ->
    with(Keys, Map, #{}).


-spec with(Keys :: [term()], Map :: map(), Opts :: map()) -> map().
with(Keys, Map, Opts) ->
    maps:with(Keys, hb_cache:ensure_loaded(Map, Opts)).


-spec without(Keys :: [term()], Map :: map()) -> map().
without(Keys, Map) ->
    without(Keys, Map, #{}).


-spec without(Keys :: [term()], Map :: map(), Opts :: map()) -> map().
without(Keys, Map, Opts) ->
    maps:without(Keys, hb_cache:ensure_loaded(Map, Opts)).


-spec filter(Fun :: fun((Key :: term(), Value :: term()) -> boolean()),
             Map :: map()) -> map().
filter(Fun, Map) ->
    filter(Fun, Map, #{}).


-spec filter(Fun :: fun((Key :: term(), Value :: term()) -> boolean()),
             Map :: map(),
             Opts :: map()) -> map().
filter(Fun, Map, Opts) ->
    maps:filtermap(
      fun(K, V) ->
              case Fun(K, Loaded = hb_cache:ensure_loaded(V, Opts)) of
                  true -> {true, Loaded};
                  false -> false
              end
      end,
      hb_cache:ensure_loaded(Map, Opts)).


-spec filtermap(Fun :: fun((Key :: term(), Value :: term()) -> {boolean(), term()}),
                Map :: map()) -> map().
filtermap(Fun, Map) ->
    filtermap(Fun, Map, #{}).


-spec filtermap(Fun :: fun((Key :: term(), Value :: term()) -> {boolean(), term()}),
                Map :: map(),
                Opts :: map()) -> map().
filtermap(Fun, Map, Opts) ->
    maps:filtermap(
      fun(K, V) -> Fun(K, hb_cache:ensure_loaded(V, Opts)) end,
      hb_cache:ensure_loaded(Map, Opts)).


-spec fold(Fun :: fun((Key :: term(), Value :: term(), Acc :: term()) -> term()),
           Acc :: term(),
           Map :: map()) -> term().
fold(Fun, Acc, Map) ->
    fold(Fun, Acc, Map, #{}).


-spec fold(Fun :: fun((Key :: term(), Value :: term(), Acc :: term()) -> term()),
           Acc :: term(),
           Map :: map(),
           Opts :: map()) -> term().
fold(Fun, Acc, Map, Opts) ->
    maps:fold(
      fun(K, V, CurrAcc) -> Fun(K, hb_cache:ensure_loaded(V, Opts), CurrAcc) end,
      Acc,
      hb_cache:ensure_loaded(Map, Opts)).


-spec take(N :: non_neg_integer(), Map :: map()) -> map().
take(N, Map) ->
    take(N, Map, #{}).


-spec take(N :: non_neg_integer(), Map :: map(), Opts :: map()) -> map().
take(N, Map, Opts) ->
    maps:take(N, hb_cache:ensure_loaded(Map, Opts)).


-spec update_with(Key :: term(),
                  Fun :: fun((Value :: term()) -> term()),
                  Map :: map()) -> map().
update_with(Key, Fun, Map) ->
    update_with(Key, Fun, Map, #{}).


-spec update_with(Key :: term(),
                  Fun :: fun((Value :: term()) -> term()),
                  Map :: map(),
                  Opts :: map()) -> map().
update_with(Key, Fun, Map, Opts) ->
    maps:update_with(Key, Fun, hb_cache:ensure_loaded(Map, Opts), Opts).


-spec from_list(List :: [{Key :: term(), Value :: term()}]) -> map().
from_list(List) ->
    maps:from_list(List).


-spec to_list(Map :: map()) -> [{Key :: term(), Value :: term()}].
to_list(Map) ->
    to_list(Map, #{}).


-spec to_list(Map :: map(), Opts :: map()) -> [{Key :: term(), Value :: term()}].
to_list(Map, Opts) ->
    maps:to_list(hb_cache:ensure_loaded(Map, Opts)).


%%% Tests


get_with_link_test() ->
    Bin = <<"TEST DATA">>,
    Opts = #{},
    {ok, Location} = hb_cache:write(Bin, Opts),
    Map = #{1 => 1, 2 => {link, Location, #{}}, 3 => 3},
    ?assertEqual(Bin, get(2, Map)).


map_with_link_test() ->
    Bin = <<"TEST DATA">>,
    Opts = #{},
    {ok, Location} = hb_cache:write(Bin, Opts),
    Map = #{1 => 1, 2 => {link, Location, #{}}, 3 => 3},
    ?assertEqual(#{1 => 1, 2 => Bin, 3 => 3}, map(fun(_K, V) -> V end, Map, #{})).


get_with_typed_link_test() ->
    Bin = <<"123">>,
    Opts = #{},
    {ok, Location} = hb_cache:write(Bin, Opts),
    Map = #{1 => 1, 2 => {link, Location, #{<<"type">> => integer}}, 3 => 3},
    ?assertEqual(123, get(2, Map, undefined)).


resolve_on_link_test() ->
    Msg = #{<<"test-key">> => <<"test-value">>},
    Opts = #{},
    {ok, ID} = hb_cache:write(Msg, Opts),
    ?assertEqual(
      {ok, <<"test-value">>},
      hb_ao:resolve({link, ID, #{}}, <<"test-key">>, #{})).


filter_with_link_test() ->
    Bin = <<"TEST DATA">>,
    Opts = #{},
    {ok, Location} = hb_cache:write(Bin, Opts),
    Map = #{1 => 1, 2 => {link, Location, #{}}, 3 => 3},
    ?assertEqual(#{1 => 1, 3 => 3}, filter(fun(_, V) -> V =/= Bin end, Map)).


filtermap_with_link_test() ->
    Bin = <<"TEST DATA">>,
    Opts = #{},
    {ok, Location} = hb_cache:write(Bin, Opts),
    Map = #{1 => 1, 2 => {link, Location, #{}}, 3 => 3},
    ?assertEqual(
      #{2 => <<"FOUND">>},
      filtermap(
        fun(_, <<"TEST DATA">>) -> {true, <<"FOUND">>};
           (_K, _V) -> false
        end,
        Map)).


fold_with_typed_link_test() ->
    Bin = <<"123">>,
    Opts = #{},
    {ok, Location} = hb_cache:write(Bin, Opts),
    Map = #{1 => 1, 2 => {link, Location, #{<<"type">> => integer}}, 3 => 3},
    ?assertEqual(127, fold(fun(_, V, Acc) -> V + Acc end, 0, Map)).


filter_passively_loads_test() ->
    Bin = <<"TEST DATA">>,
    Opts = #{},
    {ok, Location} = hb_cache:write(Bin, Opts),
    Map = #{1 => 1, 2 => {link, Location, #{}}, 3 => 3},
    ?assertEqual(
      #{1 => 1, 2 => <<"TEST DATA">>, 3 => 3},
      filter(fun(_, _) -> true end, Map)).


filtermap_passively_loads_test() ->
    Bin = <<"TEST DATA">>,
    Opts = #{},
    {ok, Location} = hb_cache:write(Bin, Opts),
    Map = #{1 => 1, 2 => {link, Location, #{}}, 3 => 3},
    ?assertEqual(
      #{1 => 1, 2 => <<"TEST DATA">>, 3 => 3},
      filtermap(fun(_, V) -> {true, V} end, Map)).
