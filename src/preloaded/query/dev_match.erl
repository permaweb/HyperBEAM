%%% @doc A reverse index for finding all message IDs with a given key-value pair.
%%%
%%% Two indexes serve two different questions:
%%%
%%% - The path-row index (the `match-index' option): rows are store paths under
%%%   a `key=value' address, listing the IDs of cached messages that carry the
%%%   pair. It answers "which cached messages match this template", serving
%%%   `all' and `hb_cache:match'.
%%% - The published sorted-set index (the `match-store' option): rows are
%%%   packed 17-byte items -- the leading ten bytes of the SHA-256 of the
%%%   predicate `~match@1.0/key=value', then the absolute weave offset of an
%%%   item carrying the pair. It answers "which weave offsets carry this
%%%   predicate", serving `locate' and the `~query@1.0' GraphQL interface. The
%%%   format is specified in `docs/misc/published-arweave-indexes.md'.
%%%
%%% Row construction and store selection for both indexes live in `hb_cache',
%%% which writes the rows: this module resolves through the same functions, so
%%% the write and read sides cannot drift apart.
-module(dev_match).
-export([info/0, all/3, locate/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_LOCATE_LIMIT, 1000).
%% The largest weave offset a packed 17-byte row can carry: a 49-bit uint.
-define(MAX_OFFSET, (1 bsl 49) - 1).

%% @doc Default all non-message@1.0 and device keys to match a single key in the
%% index.
info() ->
    #{
        excludes => [<<"set">>, <<"remove">>, <<"id">>, <<"verify">>],
        default => fun match/4
    }.

%% @doc Match a single key-value pair in the index, returning all message IDs that
%% contain the key-value pair.
match(Key, Base, _Req, Opts) -> match(Key, Base, Opts).
match(Key, Base, Opts) ->
    Store = hb_cache:match_store(Opts),
    {ok, Value} = hb_maps:find(Key, Base, Opts),
    case hb_store:list(
        Store,
        hb_cache:match_address(
            hb_ao:normalize_key(Key),
            hb_cache:match_value_path(Value, Opts)
        ),
        Opts
    ) of
        {ok, Messages} -> {ok, Messages};
        _ -> {error, not_found}
    end.

%% @doc Match the full base message against the index, returning the intersection
%% of all matches for each key.
all(Base, _Req, Opts) ->
    IndexBase = hb_message:uncommitted(hb_private:reset(Base)),
    Keys =
        hb_maps:keys(
            IndexBase
        ),
    case Keys of
        [] -> {ok, []};
        [FirstKey | Rest] ->
            case match(FirstKey, IndexBase, Opts) of
                {ok, FirstMatches} ->
                    lists:foldl(
                        fun(Key, {ok, Acc}) ->
                            case match(Key, IndexBase, Opts) of
                                {ok, Matches} ->
                                    {ok, hb_util:list_with(Acc, Matches)};
                                _ ->
                                    {error, not_found}
                            end;
                           (_Key, Error) ->
                                Error
                        end,
                        {ok, FirstMatches},
                        Rest
                    );
                _ ->
                    {error, not_found}
            end
    end.

%%% Reading the published sorted-set index.

%% @doc Find the weave offsets that carry every predicate of the base template,
%% by leapfrog intersection over the sorted-set stores: each predicate is asked
%% for its row nearest the cursor, an answer past the cursor moves the cursor
%% there, and an offset that every predicate answers with is emitted. The walk
%% runs ascending by default and descending when the request's `direction' is
%% `desc'. The request's `from' (inclusive) is where the walk starts -- its
%% low end ascending, its high end descending -- `to' (exclusive) is where it
%% stops, and `limit' caps the page. The result is the run of matching offsets
%% in walk order: an empty list is a completed page, not a missing predicate.
locate(Base, Req, Opts) ->
    case hb_cache:match_item_stores(Opts) of
        [] -> {error, not_found};
        Stores ->
            Template = locate_template(Base, Opts),
            Direction =
                case hb_maps:get(<<"direction">>, Req, <<"asc">>, Opts) of
                    <<"desc">> -> desc;
                    _ -> asc
                end,
            From =
                case {Direction, hb_maps:get(<<"from">>, Req, none, Opts)} of
                    {asc, none} -> 0;
                    {desc, none} -> ?MAX_OFFSET;
                    {_, Given} -> hb_util:int(Given)
                end,
            To =
                case hb_maps:get(<<"to">>, Req, none, Opts) of
                    none -> none;
                    Bound -> hb_util:int(Bound)
                end,
            Limit =
                hb_util:int(
                    hb_maps:get(<<"limit">>, Req, ?DEFAULT_LOCATE_LIMIT, Opts)
                ),
            case hb_maps:to_list(Template, Opts) of
                [] -> {error, not_found};
                [{Key, Value}] ->
                    scan(
                        Direction,
                        Stores,
                        hb_cache:match_item_prefix(Key, Value),
                        From, To, Limit, Opts
                    );
                Pairs ->
                    Prefixes =
                        [
                            hb_cache:match_item_prefix(Key, Value)
                        ||
                            {Key, Value} <- Pairs
                        ],
                    intersect(
                        Direction, Stores, Prefixes, From, To, Limit, [], Opts
                    )
            end
    end.

%% @doc The predicates a template message names. The template is compared in
%% TABM form -- the encoding rows are written from -- so typed values match
%% their wire representation, and the type annotations themselves are not
%% predicates.
locate_template(Base, Opts) ->
    Spec =
        hb_message:convert(
            hb_message:uncommitted(hb_private:reset(Base)),
            tabm,
            <<"structured@1.0">>,
            Opts
        ),
    hb_maps:without([<<"ao-types">>, <<"device">>], Spec, Opts).

%% @doc Walk the leapfrog intersection of a set of predicates from the
%% cursor, emitting each offset that all of them carry.
intersect(_Direction, _Stores, _Prefixes, _Cursor, _To, Limit, Acc, _Opts)
        when Limit =< 0 ->
    {ok, lists:reverse(Acc)};
intersect(Direction, _Stores, _Prefixes, Cursor, _To, _Limit, Acc, _Opts)
        when
        (Direction =:= asc andalso Cursor > ?MAX_OFFSET) orelse
            (Direction =:= desc andalso Cursor < 0)
    ->
    {ok, lists:reverse(Acc)};
intersect(Direction, Stores, Prefixes, Cursor, To, Limit, Acc, Opts) ->
    case past_bound(Direction, Cursor, To) of
        true -> {ok, lists:reverse(Acc)};
        false ->
            case probe(Direction, Stores, Prefixes, Cursor, Opts) of
                {ok, Cursor} ->
                    intersect(
                        Direction, Stores, Prefixes, step(Direction, Cursor),
                        To, Limit - 1, [Cursor | Acc], Opts
                    );
                {ok, Next} ->
                    intersect(
                        Direction, Stores, Prefixes, Next, To, Limit, Acc, Opts
                    );
                exhausted ->
                    {ok, lists:reverse(Acc)};
                {error, _} = Error ->
                    Error
            end
    end.

step(asc, Cursor) -> Cursor + 1;
step(desc, Cursor) -> Cursor - 1.

%% @doc Whether the cursor has crossed the walk's exclusive stop bound.
past_bound(_Direction, _Cursor, none) -> false;
past_bound(asc, Cursor, To) -> Cursor >= To;
past_bound(desc, Cursor, To) -> Cursor =< To.

%% @doc Ask each predicate in turn for its offset nearest the cursor. An
%% answer past the cursor restarts the walk there; agreement from every
%% predicate is a match; a predicate with nothing left ends the walk.
probe(_Direction, _Stores, [], Cursor, _Opts) -> {ok, Cursor};
probe(Direction, Stores, [Prefix | Rest], Cursor, Opts) ->
    case nearest_offset(Direction, Stores, Prefix, Cursor, Opts) of
        none -> exhausted;
        {ok, Cursor} -> probe(Direction, Stores, Rest, Cursor, Opts);
        {ok, Next} -> {ok, Next};
        {error, _} = Error -> Error
    end.

%% @doc The offset nearest the cursor among a predicate's rows, across the
%% store list: the first at or after it ascending, the last at or before it
%% descending. Each layer answers with its own nearest row and the nearest
%% wins: the layers are deltas of one logical set.
nearest_offset(Direction, Stores, Prefix, From, Opts) ->
    nearest_offset(Direction, Stores, Prefix, From, none, Opts).
nearest_offset(_Direction, [], _Prefix, _From, none, _Opts) -> none;
nearest_offset(_Direction, [], _Prefix, _From, Best, _Opts) -> {ok, Best};
nearest_offset(Direction, [Store | Rest], Prefix, From, Best, Opts) ->
    case list_items(Direction, Store, Prefix, From, 1, Opts) of
        {ok, []} ->
            nearest_offset(Direction, Rest, Prefix, From, Best, Opts);
        {ok, [Item | _]} ->
            {_Prefix, Offset} = hb_cache:decode_match_item(Item),
            nearest_offset(
                Direction, Rest, Prefix, From,
                nearer(Direction, Best, Offset), Opts
            );
        not_found ->
            nearest_offset(Direction, Rest, Prefix, From, Best, Opts);
        {error, not_found} ->
            nearest_offset(Direction, Rest, Prefix, From, Best, Opts);
        {error, _} = Error ->
            Error
    end.

nearer(_Direction, none, Offset) -> Offset;
nearer(asc, Best, Offset) -> min(Best, Offset);
nearer(desc, Best, Offset) -> max(Best, Offset).

%% @doc The run of a single predicate's offsets in walk order. Each layer
%% contributes its own bounded run and the merged result is deduplicated: the
%% same row may sit in several layers.
scan(Direction, Stores, Prefix, From, To, Limit, Opts) ->
    case runs(Direction, Stores, Prefix, From, Limit, Opts) of
        {ok, Runs} ->
            Merged = merge(Direction, Runs),
            {ok, lists:sublist(bound(Direction, Merged, To), Limit)};
        {error, _} = Error ->
            Error
    end.

merge(asc, Runs) ->
    lists:umerge(Runs);
merge(desc, Runs) ->
    lists:reverse(lists:umerge([ lists:reverse(Run) || Run <- Runs ])).

bound(_Direction, Offsets, none) ->
    Offsets;
bound(asc, Offsets, To) ->
    lists:takewhile(fun(Offset) -> Offset < To end, Offsets);
bound(desc, Offsets, To) ->
    lists:takewhile(fun(Offset) -> Offset > To end, Offsets).

%% @doc One bounded run of offsets per store that answers for a predicate.
runs(_Direction, [], _Prefix, _From, _Limit, _Opts) -> {ok, []};
runs(Direction, [Store | Rest], Prefix, From, Limit, Opts) ->
    case list_items(Direction, Store, Prefix, From, Limit, Opts) of
        {ok, Items} ->
            case runs(Direction, Rest, Prefix, From, Limit, Opts) of
                {ok, Runs} ->
                    {ok,
                        [
                            [
                                element(2, hb_cache:decode_match_item(Item))
                            ||
                                Item <- Items
                            ]
                        |
                            Runs
                        ]
                    };
                {error, _} = Error ->
                    Error
            end;
        not_found ->
            runs(Direction, Rest, Prefix, From, Limit, Opts);
        {error, not_found} ->
            runs(Direction, Rest, Prefix, From, Limit, Opts);
        {error, _} = Error ->
            Error
    end.

%% @doc List a predicate's rows from a single store, from an offset cursor
%% (inclusive). The cursor rides as a whole packed item, so the store's seek
%% lands exactly where the caller's last page ended.
list_items(Direction, Store, Prefix, From, Limit, Opts) ->
    Request =
        #{
            <<"list">> => Prefix,
            <<"from">> => hb_cache:encode_match_item(Prefix, From),
            <<"limit">> => Limit
        },
    hb_store:list(
        [Store],
        case Direction of
            asc -> Request;
            desc -> Request#{ <<"direction">> => <<"backward">> }
        end,
        Opts
    ).

%%% Tests

%% @doc A sorted-set store seeded with the given packed rows.
test_set_store(Tag, Rows) ->
    Store = hb_test_utils:test_store(hb_store_lmdb_set, Tag),
    hb_store:reset(Store, #{}, #{}),
    ok = hb_store:write([Store], maps:from_keys(Rows, <<>>), #{}),
    Store.

%% @doc Packed rows for a list of `{Key, Value, Offsets}' predicates.
test_rows(Predicates) ->
    [
        hb_cache:encode_match_item(
            hb_cache:match_item_prefix(Key, Value),
            Offset
        )
    ||
        {Key, Value, Offsets} <- Predicates,
        Offset <- Offsets
    ].

test_locate(Template, Req, Opts) ->
    hb_ao:raw(<<"match@1.0">>, <<"locate">>, Template, Req, Opts).

%% @doc A single predicate's run pages with `from' (inclusive), `to'
%% (exclusive) and `limit', and a typed template value matches the wire form
%% it was indexed under.
locate_single_predicate_test() ->
    Store =
        test_set_store(
            <<"locate-single">>,
            test_rows([
                {<<"type">>, <<"Message">>, [10, 20, 30]},
                {<<"slot">>, <<"2382">>, [20]}
            ])
        ),
    Opts = #{ <<"match-store">> => [Store] },
    Template = #{ <<"type">> => <<"Message">> },
    ?assertEqual({ok, [10, 20, 30]}, test_locate(Template, #{}, Opts)),
    ?assertEqual(
        {ok, [20, 30]},
        test_locate(Template, #{ <<"from">> => 20 }, Opts)
    ),
    ?assertEqual(
        {ok, [30]},
        test_locate(Template, #{ <<"from">> => 21 }, Opts)
    ),
    ?assertEqual(
        {ok, [10, 20]},
        test_locate(Template, #{ <<"limit">> => 2 }, Opts)
    ),
    ?assertEqual(
        {ok, [20]},
        test_locate(Template, #{ <<"from">> => 11, <<"to">> => 30 }, Opts)
    ),
    ?assertEqual({ok, [20]}, test_locate(#{ <<"slot">> => 2382 }, #{}, Opts)),
    % Descending: from the run's end by default, resumed by an inclusive
    % upper cursor, stopped by an exclusive lower bound.
    Desc = #{ <<"direction">> => <<"desc">> },
    ?assertEqual({ok, [30, 20, 10]}, test_locate(Template, Desc, Opts)),
    ?assertEqual(
        {ok, [20, 10]},
        test_locate(Template, Desc#{ <<"from">> => 20 }, Opts)
    ),
    ?assertEqual(
        {ok, [30, 20]},
        test_locate(Template, Desc#{ <<"limit">> => 2 }, Opts)
    ),
    ?assertEqual(
        {ok, [30, 20]},
        test_locate(Template, Desc#{ <<"to">> => 10 }, Opts)
    ),
    ok = hb_store:stop(Store).

%% @doc Leapfrog intersection over two and three predicates, paged by the
%% same cursor bounds as a single-predicate scan.
locate_intersection_test() ->
    Store =
        test_set_store(
            <<"locate-intersect">>,
            test_rows([
                {<<"type">>, <<"Message">>, [10, 20, 30, 40]},
                {<<"data-protocol">>, <<"ao">>, [20, 40, 50]},
                {<<"variant">>, <<"ao.TN.1">>, [5, 40]}
            ])
        ),
    Opts = #{ <<"match-store">> => [Store] },
    Two = #{ <<"type">> => <<"Message">>, <<"data-protocol">> => <<"ao">> },
    Three = Two#{ <<"variant">> => <<"ao.TN.1">> },
    ?assertEqual({ok, [20, 40]}, test_locate(Two, #{}, Opts)),
    ?assertEqual({ok, [40]}, test_locate(Three, #{}, Opts)),
    ?assertEqual({ok, [40]}, test_locate(Two, #{ <<"from">> => 21 }, Opts)),
    ?assertEqual({ok, [20]}, test_locate(Two, #{ <<"limit">> => 1 }, Opts)),
    ?assertEqual({ok, [20]}, test_locate(Two, #{ <<"to">> => 40 }, Opts)),
    % The same intersections walk descending.
    Desc = #{ <<"direction">> => <<"desc">> },
    ?assertEqual({ok, [40, 20]}, test_locate(Two, Desc, Opts)),
    ?assertEqual({ok, [40]}, test_locate(Three, Desc, Opts)),
    ?assertEqual(
        {ok, [20]},
        test_locate(Two, Desc#{ <<"from">> => 39 }, Opts)
    ),
    ?assertEqual({ok, [40]}, test_locate(Two, Desc#{ <<"limit">> => 1 }, Opts)),
    ?assertEqual({ok, [40]}, test_locate(Two, Desc#{ <<"to">> => 20 }, Opts)),
    ok = hb_store:stop(Store).

%% @doc Layered stores answer as one logical set: a predicate's rows merge
%% across the store list, duplicate offsets collapse, and intersections see
%% the union of every layer.
locate_layering_test() ->
    First =
        test_set_store(
            <<"locate-layer-1">>,
            test_rows([
                {<<"type">>, <<"Message">>, [10, 30]},
                {<<"data-protocol">>, <<"ao">>, [30]}
            ])
        ),
    Second =
        test_set_store(
            <<"locate-layer-2">>,
            test_rows([
                {<<"type">>, <<"Message">>, [10, 20]},
                {<<"data-protocol">>, <<"ao">>, [10]}
            ])
        ),
    Opts = #{ <<"match-store">> => [First, Second] },
    ?assertEqual(
        {ok, [10, 20, 30]},
        test_locate(#{ <<"type">> => <<"Message">> }, #{}, Opts)
    ),
    ?assertEqual(
        {ok, [10, 30]},
        test_locate(
            #{ <<"type">> => <<"Message">>, <<"data-protocol">> => <<"ao">> },
            #{},
            Opts
        )
    ),
    % The layered walks mirror descending.
    ?assertEqual(
        {ok, [30, 20, 10]},
        test_locate(
            #{ <<"type">> => <<"Message">> },
            #{ <<"direction">> => <<"desc">> },
            Opts
        )
    ),
    ?assertEqual(
        {ok, [30, 10]},
        test_locate(
            #{ <<"type">> => <<"Message">>, <<"data-protocol">> => <<"ao">> },
            #{ <<"direction">> => <<"desc">> },
            Opts
        )
    ),
    ok = hb_store:stop(First),
    ok = hb_store:stop(Second).

%% @doc An empty page and a predicate with no rows are both completed, empty
%% answers. Only a node with no configured match-store cannot answer at all
%% -- the distinction a paging caller needs to fall back on the right cases.
locate_empty_page_test() ->
    Store =
        test_set_store(
            <<"locate-empty">>,
            test_rows([{<<"type">>, <<"Message">>, [10]}])
        ),
    Opts = #{ <<"match-store">> => [Store] },
    ?assertEqual(
        {ok, []},
        test_locate(#{ <<"type">> => <<"Message">> }, #{ <<"from">> => 11 }, Opts)
    ),
    ?assertEqual({ok, []}, test_locate(#{ <<"type">> => <<"Other">> }, #{}, Opts)),
    ?assertEqual(
        {error, not_found},
        test_locate(#{ <<"type">> => <<"Message">> }, #{}, #{})
    ),
    ok = hb_store:stop(Store).
