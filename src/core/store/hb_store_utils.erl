%%% @doc Helper tools for `hb_store` implementations, common across
%%% implementations
-module(hb_store_utils).
-export([list_request_bounds/1, apply_list_bounds/2]).
-include_lib("eunit/include/eunit.hrl").

%% @doc The bounds of a list request as a message: the child it starts
%% from, or `none'; the most children it returns, `batch', or `all'; and
%% its direction, `asc' or `desc'. A request naming none of them bounds
%% nothing: every child, in the store's own order.
list_request_bounds(Req) ->
    #{
        <<"from">> => maps:get(<<"from">>, Req, none),
        <<"limit">> =>
            case maps:get(<<"limit">>, Req, all) of
                all -> all;
                batch -> batch;
                Limit -> hb_util:int(Limit)
            end,
        <<"direction">> => hb_util:atom(maps:get(<<"direction">>, Req, asc))
    }.

%% @doc The children a list request names, from every child a store holds:
%% all of them, in the store's own order, unless the request bounds the
%% list -- from a child, in a direction, or up to a limit -- when they are
%% the sorted children within its bounds. A batch is every child.
apply_list_bounds(Children, Req) ->
    case list_request_bounds(Req) of
        #{ <<"from">> := none, <<"limit">> := all, <<"direction">> := asc } ->
            Children;
        #{
            <<"from">> := From,
            <<"limit">> := Limit,
            <<"direction">> := Direction
        } ->
            Ordered =
                case Direction of
                    asc -> lists:sort(Children);
                    desc -> lists:reverse(lists:sort(Children))
                end,
            Ahead =
                lists:dropwhile(
                    fun(Child) -> behind(Direction, Child, From) end,
                    Ordered
                ),
            case Limit of
                Count when is_integer(Count) -> lists:sublist(Ahead, Count);
                _ -> Ahead
            end
    end.

%% @doc Whether a child lies before the start of a bounded list.
behind(_Direction, _Child, none) -> false;
behind(asc, Child, From) -> Child < From;
behind(desc, Child, From) -> Child > From.

%%% Tests

%% @doc A list without bounds is every child, however many; from a named
%% child it walks the group's sorted children from it, inclusive, in either
%% direction, up to its limit or as a batch.
list_request_bounds_test_() ->
    hb_store:generate_test_suite([{"list bounds", fun list_bounds/1}]).

list_bounds(Store) ->
    ok = hb_store:group(Store, <<"set">>, #{}),
    lists:foreach(
        fun(Name) ->
            Key = hb_path:to_binary([<<"set">>, Name]),
            ok = hb_store:write(Store, #{ Key => <<>> }, #{})
        end,
        [<<"b">>, <<"d">>, <<"a">>, <<"c">>]
    ),
    List =
        fun(Req) ->
            hb_store:list(Store, Req#{ <<"list">> => <<"set">> }, #{})
        end,
    {ok, All} = List(#{}),
    ?assertEqual([<<"a">>, <<"b">>, <<"c">>, <<"d">>], lists:sort(All)),
    ok = hb_store:group(Store, <<"many">>, #{}),
    ok =
        hb_store:write(
            Store,
            maps:from_list(
                [
                    {hb_path:to_binary([<<"many">>, hb_util:bin(N)]), <<>>}
                ||
                    N <- lists:seq(1, 1500)
                ]
            ),
            #{}
        ),
    {ok, Many} = hb_store:list(Store, <<"many">>, #{}),
    ?assertEqual(1500, length(Many)),
    ?assertEqual(
        {ok, [<<"b">>, <<"c">>]},
        List(#{ <<"from">> => <<"b">>, <<"limit">> => 2 })
    ),
    ?assertEqual(
        {ok, [<<"c">>, <<"d">>]},
        List(#{ <<"from">> => <<"bb">>, <<"limit">> => 5 })
    ),
    ?assertEqual(
        {ok, [<<"c">>, <<"b">>, <<"a">>]},
        List(#{ <<"from">> => <<"c">>, <<"direction">> => <<"desc">> })
    ),
    ?assertEqual(
        {ok, [<<"d">>, <<"c">>]},
        List(#{ <<"direction">> => desc, <<"limit">> => 2 })
    ),
    ?assertEqual(
        {ok, [<<"c">>, <<"d">>]},
        List(#{ <<"from">> => <<"c">>, <<"limit">> => batch })
    ),
    ?assertEqual({ok, []}, List(#{ <<"from">> => <<"e">> })).
