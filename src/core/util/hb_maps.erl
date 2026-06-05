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
-export([get/2, get/3, get/4, get_first/2, get_first/3, put/3, put/4, find/2, find/3]).
-export([is_key/2, is_key/3, keys/1, keys/2, values/1, values/2]).
-export([map/2, map/3, filter/2, filter/3, filtermap/2, filtermap/3]).
-export([fold/3, fold/4, take/2, take/3, size/1, size/2, flatten/2]).
-export([merge/2, merge/3, remove/2, remove/3]).
-export([with/2, with/3, without/2, without/3, update_with/3, update_with/4]).
-export([from_list/1, to_list/1, to_list/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% HyperBEAM-specific functions

-spec get_first(
    Paths :: [{Base :: map() | binary(), Path :: binary()}],
    Opts :: map()
) -> term().
get_first(Paths, Opts) ->
    get_first(Paths, not_found, Opts).

-spec get_first(
    Paths :: [{Base :: map() | binary(), Path :: binary()}],
    Default :: term(),
    Opts :: map()
) -> term().
get_first([], Default, _Opts) -> Default;
get_first([{Base, Path}|Paths], Default, Opts) ->
    case find(Path, Base, Opts) of
        {ok, Value} -> Value;
        error -> get_first(Paths, Default, Opts)
    end.

-spec get(Key :: term(), Map :: map()) -> term().
get(Key, Map) ->
    get(Key, Map, undefined).

-spec get(Key :: term(), Map :: map(), Default :: term()) -> term().
get(Key, Map, Default) ->
    get(Key, Map, Default, #{}).

%% @doc Get a value from a map, resolving links as they are encountered in both
%% the TABM encoded link format, as well as the structured type. When the map
%% is a plain map and the looked-up value is not itself a link, we skip the
%% `hb_cache:ensure_loaded' round-trip entirely -- this is the overwhelming
%% majority case on the resolve hot path.
-spec get(
    Key :: term(),
    Map :: map(),
    Default :: term(),
    Opts :: map()
) -> term().
get(Key, Map, Default, Opts) when is_map(Map) ->
    case maps:find(Key, Map) of
        {ok, V} when not ?IS_LINK(V) -> V;
        {ok, Link} -> hb_cache:ensure_loaded(Link, Opts);
        error ->
            % The key is absent from this layer. If the message extends another
            % via the reserved `...' key, fall through to the parent so that
            % inherited keys resolve. The nearest layer always wins.
            case loaded_extension(Map, Opts) of
                error -> Default;
                {ok, Ext} -> get(Key, Ext, Default, Opts)
            end
    end;
get(Key, Map, Default, Opts) ->
    hb_cache:ensure_loaded(
        maps:get(
            Key,
            hb_cache:ensure_loaded(Map, Opts),
            Default
        ),
        Opts
    ).

-spec find(Key :: term(), Map :: map()) -> {ok, term()} | error.
find(Key, Map) ->
    find(Key, Map, #{}).

-spec find(Key :: term(), Map :: map(), Opts :: map()) -> {ok, term()} | error.
find(Key, Map, Opts) when is_map(Map) ->
    case maps:find(Key, Map) of
        {ok, V} when not ?IS_LINK(V) -> {ok, V};
        error ->
            % Fall through a message extension (`...') to the parent, if present.
            case loaded_extension(Map, Opts) of
                error -> error;
                {ok, Ext} -> find(Key, Ext, Opts)
            end;
        Result -> hb_cache:ensure_loaded(Result, Opts)
    end;
find(Key, Map, Opts) ->
    hb_cache:ensure_loaded(maps:find(Key, hb_cache:ensure_loaded(Map, Opts)), Opts).

%% @doc Collapse a message extension chain into a single concrete map: the
%% reserved `...' key is followed to the parent (loading it if it is a link) and
%% merged underneath this layer, with the nearer layer winning on conflict. The
%% `...' pointer itself is dropped. A map without `...' (the common case) is
%% returned unchanged, so this is a no-op for non-extended messages. Value-level
%% links are left intact for callers to resolve as they iterate, matching the
%% other iteration helpers.
-spec flatten(Map :: map(), Opts :: map()) -> map().
flatten(Map, Opts) ->
    case hb_cache:ensure_loaded(Map, Opts) of
        Loaded when is_map(Loaded) ->
            case loaded_extension(Loaded, Opts) of
                error -> Loaded;
                {ok, Ext} ->
                    FlatExt = flatten(Ext, Opts),
                    Own = without_extension(Loaded),
                    ChangedKeys = changed_extension_keys(FlatExt, Own, Opts),
                    OwnChangedKeys = lists:usort([<<"...">> | ChangedKeys]),
                    merge_flattened_layers(
                        drop_commitments_for_keys(FlatExt, ChangedKeys, Opts),
                        drop_commitments_for_keys(Own, OwnChangedKeys, Opts)
                    )
            end;
        Loaded -> Loaded
    end.

merge_flattened_layers(Base, Own) ->
    Merged = maps:merge(Base, Own),
    case {maps:find(<<"commitments">>, Base), maps:find(<<"commitments">>, Own)} of
        {{ok, BaseCommitments}, {ok, OwnCommitments}} ->
            Merged#{
                <<"commitments">> => maps:merge(BaseCommitments, OwnCommitments)
            };
        _ ->
            Merged
    end.

changed_extension_keys(FlatBase, Own, Opts) ->
    lists:filter(
        fun(Key) ->
            case maps:find(Key, FlatBase) of
                error ->
                    false;
                {ok, BaseValue} ->
                    OwnValue = maps:get(Key, Own),
                    materialized_message_link(BaseValue, OwnValue)
                        orelse not equivalent_value(BaseValue, OwnValue, Opts)
            end
        end,
        maps:keys(maps:without([<<"commitments">>, <<"priv">>], Own))
    ).

materialized_message_link(
    {link, _ID, #{ <<"type">> := <<"link">> }},
    Value
) when is_map(Value); is_list(Value) ->
    true;
materialized_message_link(_BaseValue, _OwnValue) ->
    false.

equivalent_value(BaseValue, OwnValue, Opts) ->
    try
        hb_cache:ensure_loaded(BaseValue, Opts)
            =:= hb_cache:ensure_loaded(OwnValue, Opts)
    catch
        _:_ -> BaseValue =:= OwnValue
    end.

drop_commitments_for_keys(Msg, [], _Opts) ->
    Msg;
drop_commitments_for_keys(Msg, Keys, Opts) ->
    case maps:find(<<"commitments">>, Msg) of
        error ->
            Msg;
        {ok, Commitments} ->
            Filtered =
                maps:filter(
                    fun(_ID, Commitment) ->
                        not intersects(committed_keys(Commitment, Opts), Keys)
                    end,
                    Commitments
                ),
            maybe_update_commitments(Msg, Filtered)
    end.

maybe_update_commitments(Msg, Commitments) when map_size(Commitments) == 0 ->
    maps:remove(<<"commitments">>, Msg);
maybe_update_commitments(Msg, Commitments) ->
    Msg#{ <<"commitments">> => Commitments }.

committed_keys(Commitment, Opts) ->
    lists:map(
        fun hb_link:remove_link_specifier/1,
        hb_util:message_to_ordered_list(
            maps:get(<<"committed">>, Commitment, []),
            Opts
        )
    ).

intersects(A, B) ->
    lists:any(fun(Item) -> lists:member(Item, B) end, A).

extension(Map) ->
    case maps:find(<<"...">>, Map) of
        {ok, Ext} -> {ok, Ext};
        error -> maps:find(<<"...+link">>, Map)
    end.

loaded_extension(Map, Opts) ->
    case extension(Map) of
        error -> error;
        {ok, Ext} -> load_extension(Ext, Opts)
    end.

load_extension(Ext, Opts) ->
    case hb_cache:ensure_loaded(Ext, Opts) of
        Loaded when is_map(Loaded) -> {ok, Loaded};
        ID when is_binary(ID) ->
            case hb_cache:read(ID, hb_store:scope(Opts, local)) of
                {ok, Loaded} when is_map(Loaded) -> {ok, Loaded};
                _ -> error
            end;
        _ -> error
    end.

without_extension(Map) ->
    maps:without([<<"...">>, <<"...+link">>], Map).

-spec put(Key :: term(), Value :: term(), Map :: map()) -> map().
put(Key, Value, Map) ->
	put(Key, Value, Map, #{}).

-spec put(
	Key :: term(),
	Value :: term(),
	Map :: map(),
	Opts :: map()
) -> map().
put(Key, Value, Map, Opts) ->
    maps:put(Key, Value, hb_cache:ensure_loaded(Map, Opts)).

-spec is_key(Key :: term(), Map :: map()) -> boolean().
is_key(Key, Map) ->
    is_key(Key, Map, #{}).

-spec is_key(Key :: term(), Map :: map(), Opts :: map()) -> boolean().
is_key(Key, Map, Opts) when is_map(Map) ->
    case maps:is_key(Key, Map) of
        true -> true;
        false ->
            % Fall through a message extension (`...') to the parent, if present.
            case loaded_extension(Map, Opts) of
                error -> false;
                {ok, Ext} -> is_key(Key, Ext, Opts)
            end
    end;
is_key(Key, Map, Opts) ->
    maps:is_key(Key, hb_cache:ensure_loaded(Map, Opts)).

-spec keys(Map :: map()) -> [term()].
keys(Map) ->
	keys(Map, #{}).

-spec keys(Map :: map(), Opts :: map()) -> [term()].
keys(Map, Opts) when is_map(Map) ->
    case loaded_extension(Map, Opts) of
        error -> maps:keys(Map);
        {ok, Ext} ->
            % A message extension: the visible keys are this layer's own keys
            % (excluding the `...' pointer) unioned with the parent's keys, with
            % the nearer layer winning on conflict.
            OwnKeys = maps:keys(without_extension(Map)),
            ParentKeys = keys(Ext, Opts),
            OwnKeys ++ [ K || K <- ParentKeys, not lists:member(K, OwnKeys) ]
    end;
keys(Map, Opts) ->
    maps:keys(hb_cache:ensure_loaded(Map, Opts)).

-spec values(Map :: map()) -> [term()].
values(Map) -> values(Map, #{}).

-spec values(Map :: map(), Opts :: map()) -> [term()].
values(Map, Opts) ->
    maps:values(flatten(Map, Opts)).

-spec size(Map :: map()) -> non_neg_integer().
size(Map) ->
	size(Map, #{}).

-spec size(Map :: map(), Opts :: map()) -> non_neg_integer().
size(Map, Opts) ->
    maps:size(flatten(Map, Opts)).

-spec map(
    Fun :: fun((Key :: term(), Value :: term()) -> term()),
    Map :: map()
) -> map().
map(Fun, Map) ->
    map(Fun, Map, #{}).

-spec map(
    Fun :: fun((Key :: term(), Value :: term()) -> term()),
    Map :: map(),
    Opts :: map()
) -> map().
map(Fun, Map, Opts) ->
    maps:map(
        fun(K, V) -> Fun(K, hb_cache:ensure_loaded(V, Opts)) end,
        flatten(Map, Opts)
    ).

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

-spec filter(
    Fun :: fun((Key :: term(), Value :: term()) -> boolean()),
    Map :: map()
) -> map().
filter(Fun, Map) ->
    filter(Fun, Map, #{}).

-spec filter(
    Fun :: fun((Key :: term(), Value :: term()) -> boolean()),
    Map :: map(),
    Opts :: map()
) -> map().
filter(Fun, Map, Opts) ->
    maps:filtermap(
        fun(K, V) ->
            case Fun(K, Loaded = hb_cache:ensure_loaded(V, Opts)) of
                true -> {true, Loaded};
                false -> false
            end
        end,
        flatten(Map, Opts)
    ).

-spec filtermap(
    Fun :: fun((Key :: term(), Value :: term()) -> {boolean(), term()}),
    Map :: map()
) -> map().
filtermap(Fun, Map) ->
    filtermap(Fun, Map, #{}).

-spec filtermap(
    Fun :: fun((Key :: term(), Value :: term()) -> {boolean(), term()}),
    Map :: map(),
    Opts :: map()
) -> map().
filtermap(Fun, Map, Opts) ->
    maps:filtermap(
        fun(K, V) -> Fun(K, hb_cache:ensure_loaded(V, Opts)) end,
        flatten(Map, Opts)
    ).

-spec fold(
    Fun :: fun((Key :: term(), Value :: term(), Acc :: term()) -> term()),
    Acc :: term(),
    Map :: map()
) -> term().
fold(Fun, Acc, Map) ->
    fold(Fun, Acc, Map, #{}).

-spec fold(
    Fun :: fun((Key :: term(), Value :: term(), Acc :: term()) -> term()),
    Acc :: term(),
    Map :: map(),
    Opts :: map()
) -> term().
fold(Fun, Acc, Map, Opts) ->
    maps:fold(
        fun(K, V, CurrAcc) -> Fun(K, hb_cache:ensure_loaded(V, Opts), CurrAcc) end,
        Acc,
        flatten(Map, Opts)
    ).

-spec take(N :: non_neg_integer(), Map :: map()) -> map().
take(N, Map) ->
	take(N, Map, #{}).

-spec take(N :: non_neg_integer(), Map :: map(), Opts :: map()) -> map().
take(N, Map, Opts) ->
    maps:take(N, hb_cache:ensure_loaded(Map, Opts)).

-spec update_with(
    Key :: term(),
    Fun :: fun((Value :: term()) -> term()),
    Map :: map()
) -> map().
update_with(Key, Fun, Map) ->
    update_with(Key, Fun, Map, #{}).

-spec update_with(
    Key :: term(),
    Fun :: fun((Value :: term()) -> term()),
    Map :: map(),
    Opts :: map()
) -> map().
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
    maps:to_list(flatten(Map, Opts)).

%%% Tests

get_with_link_test() ->
    Bin = <<"TEST DATA">>,
    Opts = #{},
    {ok, Location} = hb_cache:write(Bin, Opts),
    Map = #{ 1 => 1, 2 => {link, Location, #{}}, 3 => 3 },
    ?assertEqual(Bin, get(2, Map)).

map_with_link_test() ->
    Bin = <<"TEST DATA">>,
    Opts = #{},
    {ok, Location} = hb_cache:write(Bin, Opts),
    Map = #{ 1 => 1, 2 => {link, Location, #{}}, 3 => 3 },
    ?assertEqual(#{1 => 1, 2 => Bin, 3 => 3}, map(fun(_K, V) -> V end, Map, #{})).

get_with_typed_link_test() ->
    Bin = <<"123">>,
    Opts = #{},
    {ok, Location} = hb_cache:write(Bin, Opts),
    Map = #{ 1 => 1, 2 => {link, Location, #{ <<"type">> => integer }}, 3 => 3 },
    ?assertEqual(123, get(2, Map, undefined)).

resolve_on_link_test() ->
    Msg = #{ <<"test-key">> => <<"test-value">> },
    Opts = #{},
    {ok, ID} = hb_cache:write(Msg, Opts),
    ?assertEqual(
        {ok, <<"test-value">>},
        hb_ao:resolve({link, ID, #{}}, <<"test-key">>, #{})
    ).

filter_with_link_test() ->
    Bin = <<"TEST DATA">>,
    Opts = #{},
    {ok, Location} = hb_cache:write(Bin, Opts),
    Map = #{ 1 => 1, 2 => {link, Location, #{}}, 3 => 3 },
    ?assertEqual(#{1 => 1, 3 => 3}, filter(fun(_, V) -> V =/= Bin end, Map)).

filtermap_with_link_test() ->
    Bin = <<"TEST DATA">>,
    Opts = #{},
    {ok, Location} = hb_cache:write(Bin, Opts),
    Map = #{ 1 => 1, 2 => {link, Location, #{}}, 3 => 3 },
    ?assertEqual(
        #{2 => <<"FOUND">>},
        filtermap(
            fun(_, <<"TEST DATA">>) -> {true, <<"FOUND">>};
               (_K, _V) -> false
            end,
            Map
        )
    ).

fold_with_typed_link_test() ->
    Bin = <<"123">>,
    Opts = #{},
    {ok, Location} = hb_cache:write(Bin, Opts),
    Map = #{ 1 => 1, 2 => {link, Location, #{ <<"type">> => integer }}, 3 => 3 },
    ?assertEqual(127, fold(fun(_, V, Acc) -> V + Acc end, 0, Map)).

filter_passively_loads_test() ->
    Bin = <<"TEST DATA">>,
    Opts = #{},
    {ok, Location} = hb_cache:write(Bin, Opts),
    Map = #{ 1 => 1, 2 => {link, Location, #{}}, 3 => 3 },
    ?assertEqual(
        #{1 => 1, 2 => <<"TEST DATA">>, 3 => 3},
        filter(fun(_, _) -> true end, Map)
    ).

filtermap_passively_loads_test() ->
    Bin = <<"TEST DATA">>,
    Opts = #{},
    {ok, Location} = hb_cache:write(Bin, Opts),
    Map = #{ 1 => 1, 2 => {link, Location, #{}}, 3 => 3 },
    ?assertEqual(
        #{ 1 => 1, 2 => <<"TEST DATA">>, 3 => 3 },
        filtermap(fun(_, V) -> {true, V} end, Map)
    ).

%%% Message extension (`...') tests. A message may extend another by holding it
%%% under the reserved `...' key; key lookups fall through to the parent, with
%%% the nearest layer winning.

get_with_extension_test() ->
    Base = #{ <<"a">> => 1, <<"b">> => 2 },
    Ext = #{ <<"b">> => 20, <<"...">> => Base },
    ?assertEqual(20, get(<<"b">>, Ext, undefined)),
    ?assertEqual(1, get(<<"a">>, Ext, undefined)),
    ?assertEqual(default, get(<<"c">>, Ext, default)).

find_with_extension_test() ->
    Base = #{ <<"a">> => 1 },
    Ext = #{ <<"b">> => 2, <<"...">> => Base },
    ?assertEqual({ok, 1}, find(<<"a">>, Ext, #{})),
    ?assertEqual({ok, 2}, find(<<"b">>, Ext, #{})),
    ?assertEqual(error, find(<<"c">>, Ext, #{})).

is_key_with_extension_test() ->
    Base = #{ <<"a">> => 1 },
    Ext = #{ <<"b">> => 2, <<"...">> => Base },
    ?assert(is_key(<<"a">>, Ext, #{})),
    ?assert(is_key(<<"b">>, Ext, #{})),
    ?assertNot(is_key(<<"c">>, Ext, #{})).

nested_extension_test() ->
    L0 = #{ <<"a">> => 1 },
    L1 = #{ <<"b">> => 2, <<"...">> => L0 },
    L2 = #{ <<"c">> => 3, <<"...">> => L1 },
    ?assertEqual(1, get(<<"a">>, L2, undefined)),
    ?assertEqual(2, get(<<"b">>, L2, undefined)),
    ?assertEqual(3, get(<<"c">>, L2, undefined)).

extension_through_link_test() ->
    Opts = #{},
    {ok, Location} = hb_cache:write(#{ <<"a">> => 1 }, Opts),
    Ext = #{ <<"b">> => 2, <<"...">> => {link, Location, #{}} },
    ?assertEqual(1, get(<<"a">>, Ext, undefined, Opts)),
    ?assertEqual(2, get(<<"b">>, Ext, undefined, Opts)),
    ?assertEqual({ok, 1}, find(<<"a">>, Ext, Opts)).

keys_with_extension_test() ->
    Base = #{ <<"a">> => 1, <<"b">> => 2 },
    Ext = #{ <<"b">> => 20, <<"c">> => 3, <<"...">> => Base },
    % Union of own and inherited keys; the `...' pointer is not itself a key.
    ?assertEqual(
        lists:sort([<<"a">>, <<"b">>, <<"c">>]),
        lists:sort(keys(Ext, #{}))
    ).

flatten_extension_test() ->
    L0 = #{ <<"a">> => 1, <<"b">> => 2 },
    L1 = #{ <<"b">> => 20, <<"c">> => 3, <<"...">> => L0 },
    % Nearer layer wins; the `...' pointer is dropped.
    ?assertEqual(
        #{ <<"a">> => 1, <<"b">> => 20, <<"c">> => 3 },
        flatten(L1, #{})
    ),
    % No-op for a map without `...'.
    ?assertEqual(L0, flatten(L0, #{})),
    % Iteration ops see the flattened view.
    ?assertEqual(lists:sort([1, 20, 3]), lists:sort(values(L1, #{}))),
    ?assertEqual(24, fold(fun(_, V, Acc) -> V + Acc end, 0, L1, #{})),
    ?assertEqual(3, size(L1, #{})).
