%%% @doc Helper tools for `hb_store` implementations, common across
%%% implementations
-module(hb_store_utils).
-export([list_request_bounds/1, apply_list_bounds/2]).
-export([is_link/1, to_path/1, is_data_path/1, child_prefix/1]).
-export([immediate_children/2, resolve_path_links/2]).
-include_lib("eunit/include/eunit.hrl").
-define(MAX_REDIRECTS, 1000). % Only resolve 1000 links to data

%% @doc The bounds of a list request as a message: the child it starts
%% from, or `none'; the most children it returns, `batch', or `all'; and
%% its direction, `asc' or `desc'. A request with none of them bounds
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

%% @doc The children a list request answers with, from every child a store
%% holds: all of them, in the store's own order, unless the request bounds
%% the list -- from a child, in a direction, or up to a limit -- when they
%% are the sorted children within its bounds. A batch is every child.
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

%%% Paths laid out as keys, as `hb_store_lmdb' lays them out: a value is
%%% a `group' marker, a `link:' to another path, or the value itself.

%% @doc Whether a value is a link, and the path it links to.
is_link(<<"link:", Link/binary>>) when byte_size(Link) > 0 ->
    {true, Link};
is_link(_) ->
    false.

%% @doc Path parts joined into a path.
to_path(PathParts) ->
    hb_util:bin(lists:join(<<"/">>, PathParts)).

%% @doc Whether a path is content-addressed data, which never holds links.
is_data_path(<<"data">>) -> true;
is_data_path(<<"data/", _/binary>>) -> true;
is_data_path(_) -> false.

%% @doc Resolve links in a path, checking each segment except the last,
%% reading each accumulated path through `Read', which answers
%% `{ok, Value}' or `not_found'. Returns the resolved path where any
%% intermediate links have been followed.
resolve_path_links(Read, Path) ->
    resolve_path_links(Read, Path, 0).

%% @doc Resolve a path's links at a depth, refusing a chain of links past
%% `MAX_REDIRECTS'.
resolve_path_links(_Read, _Path, Depth) when Depth > ?MAX_REDIRECTS ->
    % Prevent infinite loops with depth limit
    {error, too_many_redirects};
resolve_path_links(_Read, [LastSegment], _Depth) ->
    % Base case: only one segment left, no link resolution needed
    {ok, [LastSegment]};
resolve_path_links(Read, Path, Depth) ->
    resolve_path_links_acc(Read, Path, [], Depth).

%% @doc Resolve a path's links segment by segment, accumulating the
%% resolved path in reverse.
resolve_path_links_acc(_Read, [], AccPath, _Depth) ->
    % No more segments to process
    {ok, lists:reverse(AccPath)};
resolve_path_links_acc(_, FullPath = [<<"data">>|_], [], _Depth) ->
    {ok, FullPath};
resolve_path_links_acc(Read, [Head | Tail], AccPath, Depth) ->
    % Build the accumulated path so far
    CurrentPath = lists:reverse([Head | AccPath]),
    CurrentPathBin = to_path(CurrentPath),
    % Check if the accumulated path (not just the segment) is a link
    case Read(CurrentPathBin) of
        {ok, Value} ->
            case is_link(Value) of
                {true, Link} ->
                    % The accumulated path is a link! Resolve it
                    LinkSegments = binary:split(Link, <<"/">>, [global]),
                    % Replace the accumulated path with the link target and
                    % continue with remaining segments
                    NewPath = LinkSegments ++ Tail,
                    resolve_path_links(Read, NewPath, Depth + 1);
                false ->
                    % Not a link, continue accumulating
                    resolve_path_links_acc(Read, Tail, [Head | AccPath], Depth)
            end;
        not_found ->
            % Path doesn't exist as a complete link, continue accumulating
            resolve_path_links_acc(Read, Tail, [Head | AccPath], Depth)
    end.

%% @doc The immediate children among the rows under a prefix, as
%% `{Child, Value}' pairs: each row's key with the prefix stripped, dropping
%% any that still contains a `/' -- a grandchild reached only through a
%% subgroup, whose own `group' marker is an immediate child in its own right.
immediate_children(Prefix, Rows) ->
    PrefixSize = byte_size(Prefix),
    lists:filtermap(
        fun({Key, Value}) ->
            case Key of
                <<Prefix:PrefixSize/binary, Child/binary>>
                        when Child =/= <<>> ->
                    case binary:match(Child, <<"/">>) of
                        nomatch -> {true, {Child, Value}};
                        _ -> false
                    end;
                _ ->
                    false
            end
        end,
        Rows
    ).

%% @doc The prefix of a path's children: the path and a `/', with none at
%% the root.
child_prefix(<<>>) -> <<>>;
child_prefix(<<"/">>) -> <<>>;
child_prefix(Path) ->
    case binary:last(Path) of
        $/ -> Path;
        _ -> <<Path/binary, "/">>
    end.

%%% Tests

%% @doc Every store lists a group's children whole by default, however
%% many; from a child it lists the group's sorted children from that child,
%% inclusive, in either direction, up to its limit or as a batch.
list_request_bounds_test_() ->
    hb_store:generate_test_suite([{"list bounds", fun list_bounds/1}]).

%% @doc The list bounds test, run against one store.
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
