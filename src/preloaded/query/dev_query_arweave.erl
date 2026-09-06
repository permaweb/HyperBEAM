%%% @doc An implementation of the Arweave GraphQL API, inside the `~query@1.0'
%%% device.
%%%
%%% When an `hb_store_arweave' index is available, transaction results are
%%% sorted by block height via the monotonically increasing Arweave data
%%% offsets stored in `hb_store_arweave_offset'.  The `sort' argument on the
%%% `transactions' query selects the order (`HEIGHT_DESC' by default,
%%% `HEIGHT_ASC' for ascending).  A `block' range filter narrows results to
%%% transactions whose offsets fall within the requested block heights.
%%%
%%% A node with stores of the `~match@1.0' index -- its own, a published
%%% index's, its mempool's, or all of them as one -- answers the
%%% `transactions' query from them whenever the query's filters are the
%%% index's pairs: single-valued `tags', one owner, one recipient, and no
%%% explicit IDs. The page is then read from the index in weave order between
%%% bounds, the mempool leading it descending and ending it ascending, and
%%% each cursor names its match's key in the index, so paging is stateless.
%%% A match the node's stores hold is read through `hb_cache'; one only a
%%% published index knows is read from the weave by one read of the item's
%%% header, carrying the header's fields alone. An item the weave holds at
%%% two offsets is two matches, and so two edges with one ID. The page's
%%% `count' is read on demand, up to `query_arweave_max_index_count'
%%% matches. Every other query, and every node without the index, is
%%% answered through `hb_cache' -- where several values for one tag are
%%% refused, as they are here.
-module(dev_query_arweave).
%%% AO-Core API:
-export([query/4]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%%% Default returned page size and maximum allowed page size.
-define(DEFAULT_PAGE_SIZE, 10).
-define(DEFAULT_MAX_PAGE_SIZE, 100).
%% The cursor of an index-served edge: its match's key in the index.
-define(MEMBER_CURSOR, "member=").
%% The most matches a page's `count' reads by default.
-define(DEFAULT_MAX_INDEX_COUNT, 1000).
%% The bytes read past an offset when the item's header runs beyond its
%% chunk: signature, owner, target, anchor and tags, which ANS-104 caps at
%% 4 KiB.
-define(ITEM_PROBE_LENGTH, 8192).
%% @doc The arguments that are supported by the Arweave GraphQL API.
-define(SUPPORTED_QUERY_ARGS,
    [
        <<"height">>,
        <<"id">>,
        <<"ids">>,
        <<"tags">>,
        <<"owners">>,
        <<"recipients">>
    ]
).

%% @doc Handle an Arweave GraphQL query for either transactions or blocks.
query(List, <<"edges">>, _Args, _Opts) when is_list(List) ->
    {ok, [{ok, Msg} || Msg <- List]};
query(#{ <<"edges">> := Edges }, <<"edges">>, _Args, _Opts) ->
    {ok, [{ok, Edge} || Edge <- Edges]};
query(#{ <<"node">> := Node }, <<"node">>, _Args, _Opts) ->
    {ok, Node};
query(Msg, <<"node">>, _Args, _Opts) ->
    {ok, Msg};
query(#{ <<"pageInfo">> := PageInfo }, <<"pageInfo">>, _Args, _Opts) ->
    {ok, PageInfo};
query(#{ <<"hasNextPage">> := HasNextPage }, <<"hasNextPage">>, _Args, _Opts) ->
    {ok, HasNextPage};
query(#{ <<"count">> := Count }, <<"count">>, _Args, _Opts) ->
    {ok, Count};
query(#{ <<"matches">> := Matches, <<"terminal">> := Terminal },
        <<"edges">>, _Args, Opts) ->
    % The edges of an index-served page, read only when asked for. The
    % terminal page of a forced-next-page read marks its last cursor.
    Edges = match_edges(Matches, Opts),
    Marked =
        case Terminal of
            true -> force_terminal_cursor(Edges);
            false -> Edges
        end,
    {ok, [{ok, Edge} || Edge <- Marked]};
query(#{ <<"template">> := Template, <<"ranges">> := Ranges },
        <<"count">>, _Args, Opts) ->
    % The count of an index-served page, read over its ranges on demand,
    % up to the node's maximum.
    Cap =
        hb_opts:get(
            query_arweave_max_index_count,
            ?DEFAULT_MAX_INDEX_COUNT,
            Opts
        ),
    case index_matches(Template, Ranges, none, Cap, Opts) of
        {ok, Matches} -> {ok, hb_util:bin(length(Matches))};
        Error -> Error
    end;
query(Obj, <<"transaction">>, Args, Opts) ->
    case query(Obj, <<"transactions">>, Args, Opts) of
        {ok, #{ <<"edges">> := [] }} -> {ok, null};
        {ok, #{ <<"edges">> := [#{ <<"node">> := Msg } | _] }} -> {ok, Msg}
    end;
query(Obj, <<"transactions">>, Args, Opts) ->
    ?event({transactions_query,
        {object, Obj},
        {field, <<"transactions">>},
        {args, Args}
    }),
    case index_connection(Args, Opts) of
        unservable -> cached_transactions(Args, Opts);
        Result -> Result
    end;
query(Obj, <<"block">>, Args, Opts) ->
    case query(Obj, <<"blocks">>, Args, Opts) of
        {ok, []} -> {ok, null};
        {ok, [Msg|_]} -> {ok, Msg}
    end;
query(Obj, <<"blocks">>, Args, Opts) ->
    ?event({blocks, 
            {object, Obj}, 
            {field, <<"blocks">>}, 
            {args, Args}
        }),
    Matches = match_args(Args, Opts),
    ?event({blocks_matches, Matches}),
    Blocks =
        lists:filtermap(
            fun(Match) ->
                case hb_cache:read(Match, Opts) of
                    {ok, Msg} -> {true, Msg};
                    _ -> false
                end
            end,
            Matches
        ),
    % Return the blocks as a list of messages.
    % Individual access methods are defined below.
    {ok, Blocks};
query(Block, <<"previous">>, _Args, Opts) ->
    {ok, hb_maps:get(<<"previous_block">>, Block, null, Opts)};
query(Block, <<"height">>, _Args, Opts) ->
    {ok, hb_maps:get(<<"height">>, Block, null, Opts)};
query(Block, <<"timestamp">>, _Args, Opts) ->
    {ok, hb_maps:get(<<"timestamp">>, Block, null, Opts)};
query(Msg, <<"signature">>, _Args, Opts) ->
    % Return the signature of the transaction.
    % Other TX access methods are defined below.
    case hb_message:commitments(#{ <<"committer">> => '_' }, Msg, Opts) of
        not_found -> {ok, null};
        Commitments ->
            case hb_maps:keys(Commitments) of
                [] -> {ok, null};
                [CommID | _] ->
                    {ok, Commitment} = hb_maps:find(CommID, Commitments, Opts),
                    hb_maps:find(<<"signature">>, Commitment, Opts)
            end
    end;
query(Msg, <<"owner">>, _Args, Opts) ->
    ?event({query_owner, Msg}),
    case hb_message:commitments(#{ <<"committer">> => '_' }, Msg, Opts) of
        not_found -> {ok, null};
        Commitments ->
            case hb_maps:keys(Commitments) of
                [] -> {ok, null};
                [CommID | _] ->
                    {ok, Commitment} = hb_maps:find(CommID, Commitments, Opts),
                    {ok, Address} = hb_maps:find(<<"committer">>, Commitment, Opts),
                    {ok, KeyID} = hb_maps:find(<<"keyid">>, Commitment, Opts),
                    Key = hb_util:remove_scheme_prefix(KeyID),
                    {ok, #{
                        <<"address">> => Address,
                        <<"key">> => Key
                    }}
            end
    end;
query(#{ <<"key">> := Key }, <<"key">>, _Args, _Opts) ->
    {ok, Key};
query(#{ <<"address">> := Address }, <<"address">>, _Args, _Opts) ->
    {ok, Address};
query(Msg, <<"fee">>, _Args, Opts) ->
    {ok, hb_maps:get(<<"fee">>, Msg, 0, Opts)};
query(Msg, <<"quantity">>, _Args, Opts) ->
    {ok, hb_maps:get(<<"quantity">>, Msg, 0, Opts)};
query(Number, <<"winston">>, _Args, _Opts) when is_number(Number) ->
    {ok, Number};
query(Msg, <<"recipient">>, _Args, Opts) ->
    case find_field_key(<<"field-target">>, Msg, Opts) of
        {ok, null} -> {ok, <<"">>};
        OkRes -> OkRes
    end;
query(Msg, <<"anchor">>, _Args, Opts) ->
    case find_field_key(<<"field-anchor">>, Msg, Opts) of
        {ok, null} -> {ok, <<"">>};
        {ok, Anchor} -> encode_anchor(Anchor)
    end;
query(Msg, <<"data">>, _Args, Opts) ->
    Data =
        hb_ao:get_first(
            [
                {{as, <<"message@1.0">>, Msg}, <<"data">>},
                {{as, <<"message@1.0">>, Msg}, <<"body">>}
            ],
            <<>>,
            Opts
        ),
    Type = hb_maps:get(<<"content-type">>, Msg, null, Opts),
    {ok, #{ <<"data">> => Data, <<"type">> => Type }};
query(#{ <<"data">> := Data }, <<"size">>, _Args, _Opts) ->
    {ok, byte_size(Data)};
query(#{ <<"type">> := Type }, <<"type">>, _Args, _Opts) ->
    {ok, Type};
query(Obj, Field, Args, _Opts) ->
    ?event({unimplemented_transactions_query,
        {object, Obj},
        {field, Field},
        {args, Args}
    }),
    {ok, <<"Not implemented.">>}.

%% @doc Serve a transactions page through `hb_cache': the IDs the query's
%% arguments match, annotated with their offsets, filtered to the block
%% range and sorted.
cached_transactions(Args, Opts) ->
    case valid_after_cursor(Args, Opts) of
        true ->
            Matches = match_args(Args, Opts),
            WithExplicit =
                case explicit_ids(Args, Opts) of
                    [] -> Matches;
                    ExplicitIDs -> hb_util:list_with(Matches, ExplicitIDs)
                end,
            Ordered =
                case annotate_ids(WithExplicit, Opts) of
                    unavailable -> [#{ <<"id">> => ID } || ID <- Matches];
                    Annotated ->
                        Order = maps:get(<<"sort">>, Args, <<"HEIGHT_DESC">>),
                        sort_offset_annotated(
                            filter_offset_annotated(
                                Annotated,
                                maps:get(<<"block">>, Args, undefined),
                                Opts
                            ),
                            Order,
                            Opts
                        )
                end,
            ?event({transactions_matches, Matches}),
            {ok, connection(Ordered, Args, Opts)};
        false ->
            ?event(
                {invalid_after_cursor,
                    hb_maps:get(<<"after">>, Args, not_found, Opts)
                }
            ),
            {ok, connection([], Args, Opts)}
    end.

%% @doc Encode a transaction anchor (`last_tx`) for the GraphQL response.
%% Per the Arweave spec, an anchor is one of:
%%   - empty (first TX from a wallet),
%%   - a 32-byte raw TX ID (the wallet's last outgoing TX), or
%%   - a 48-byte raw block hash (any of the last 50 blocks).
%% The cached value may already be base64url-encoded (43 / 64 chars). Other
%% sizes are not valid per the spec.
encode_anchor(<<>>) -> {ok, <<>>};
encode_anchor(Bin) when is_binary(Bin), byte_size(Bin) == 32 -> {ok, hb_util:encode(Bin)};
encode_anchor(Bin) when is_binary(Bin), byte_size(Bin) == 48 -> {ok, hb_util:encode(Bin)};
encode_anchor(Bin) when is_binary(Bin), byte_size(Bin) == 43 -> {ok, Bin};
encode_anchor(Bin) when is_binary(Bin), byte_size(Bin) == 64 -> {ok, Bin};
encode_anchor(Other) -> {error, <<"invalid_anchor: ", Other/binary>>}.

%% @doc Find and return a value from the fields of a message (from its
%% commitments).
find_field_key(Field, Msg, Opts) ->
    case hb_message:commitments(#{ Field => '_' }, Msg, Opts) of
        not_found -> {ok, null};
        Commitments ->
            case hb_maps:keys(Commitments) of
                [] -> {ok, null};
                [CommID | _] ->
                    {ok, Commitment} = hb_maps:find(CommID, Commitments, Opts),
                    case hb_maps:find(Field, Commitment, Opts) of
                        {ok, Value} -> {ok, Value};
                        error -> {ok, null}
                    end
            end
    end.

%% @doc Generate the connection response for a ordered, annotated list of 
%% results.
connection(Ordered, Args, Opts) ->
    ResultsCount = length(Ordered),
    Remaining = drop_to_cursor(Args, Ordered, Opts),
    CountToReturn = page_size(Args, Opts),
    ResultsPagePlusOne = read_ids(Remaining, CountToReturn + 1, Opts),
    ResultsPage = lists:sublist(ResultsPagePlusOne, CountToReturn),
    HasNextPage = length(ResultsPagePlusOne) > CountToReturn,
    ForceNextPage = force_next_page(Args, Opts),
    Edges =
        case ForceNextPage andalso (not HasNextPage) of
            true -> force_terminal_cursor(ResultsPage);
            false -> ResultsPage
        end,
    #{
        <<"count">> => hb_util:bin(ResultsCount),
        <<"edges">> => Edges,
        <<"pageInfo">> =>
            #{
                <<"hasNextPage">> => HasNextPage orelse ForceNextPage
            }
    }.

force_next_page(Args, Opts) ->
    hb_util:bool(hb_maps:get(<<"force-next-page">>, Args, false, Opts)).

force_terminal_cursor([]) -> [];
force_terminal_cursor(Edges) ->
    [Last = #{ <<"cursor">> := Cursor } | RestRev] = lists:reverse(Edges),
    lists:reverse([Last#{ <<"cursor">> => << Cursor/binary, "&remaining=0" >> } | RestRev]).

%% @doc Read IDs into their Arweave GraphQL-compliant object form, from a list
%% of offset-annotated messages.
read_ids([], _Count, _Opts) -> [];
read_ids(_, 0, _Opts) -> [];
read_ids([AnnotatedID = #{ <<"id">> := ID } | Rest], Count, Opts) ->
    case hb_cache:read(ID, Opts) of
        {ok, Msg} ->
            [AnnotatedID#{ <<"node">> => Msg } | read_ids(Rest, Count - 1, Opts)];
        _ ->
            read_ids(Rest, Count, Opts)
    end.

%% @doc Drop to the cursor position, returning the list of items after the cursor.
drop_to_cursor(Args, Ordered, Opts) ->
    drop_to_cursor(
        hb_maps:get(<<"after">>, Args, null, Opts),
        Ordered
    ).
drop_to_cursor(null, Ordered) ->
    Ordered;
drop_to_cursor(undefined, Ordered) ->
    Ordered;
drop_to_cursor(<<>>, Ordered) ->
    Ordered;
drop_to_cursor(_After, []) ->
    [];
drop_to_cursor(After, [#{ <<"cursor">> := After } | Rest]) ->
    Rest;
drop_to_cursor(After, [_ | Rest]) ->
    drop_to_cursor(After, Rest).

valid_after_cursor(Args, Opts) ->
    valid_cursor(hb_maps:get(<<"after">>, Args, null, Opts)).

valid_cursor(null) ->
    true;
valid_cursor(undefined) ->
    true;
valid_cursor(<<>>) ->
    true;
valid_cursor(<<"offset=", Cursor/binary>>) ->
    valid_offset_cursor(Cursor);
valid_cursor(<<"pending=", ID/binary>>) when ?IS_ID(ID) ->
    true;
valid_cursor(<<"ephemeral=", ID/binary>>) when ?IS_ID(ID) ->
    true;
valid_cursor(_) ->
    false.

valid_offset_cursor(Cursor) ->
    case binary:split(Cursor, <<"-">>) of
        [Offset] ->
            valid_integer_cursor_part(Offset);
        [Offset, Ordinate] ->
            valid_integer_cursor_part(Offset)
                andalso valid_integer_cursor_part(Ordinate);
        _ ->
            false
    end.

valid_integer_cursor_part(<<>>) -> false;
valid_integer_cursor_part(Bin) ->
    try binary_to_integer(Bin) >= 0
    catch _:_ -> false
    end.

%% @doc Return the page size, clamped to the maximum allowed.
page_size(Args, Opts) ->
    DefaultPageSize = hb_opts:get(default_page_size, ?DEFAULT_PAGE_SIZE, Opts),
    MaxPageSize = hb_opts:get(max_page_size, ?DEFAULT_MAX_PAGE_SIZE, Opts),
    max(
        0,
        min(
            hb_maps:get(<<"first">>, Args, DefaultPageSize, Opts),
            MaxPageSize
        )
    ).

%% @doc Sort messages by their block height, if Arweave index store is available.
%% Takes a list of IDs and returns the same list sorted by block height. IDs that
%% do not have an offset are always placed at the end of the list -- regardless
%% of the sort order.
sort_offset_annotated(AnnotatedIDs, SortOrder, _Opts) ->
    {WithOffset, WithoutOffset} =
        lists:partition(
            fun(AnnotatedID) -> maps:is_key(<<"offset">>, AnnotatedID) end,
            AnnotatedIDs
        ),
    {Pending, Confirmed} =
        lists:partition(fun(#{ <<"offset">> := Offset }) -> pending_offset(Offset) end, WithOffset),
    ByID = fun(#{ <<"id">> := A }, #{ <<"id">> := B }) -> A < B end,
    ByOffset = fun(#{ <<"offset">> := A }, #{ <<"offset">> := B }) -> A < B end,
    UserOrderSorted =
        case SortOrder of
            <<"HEIGHT_ASC">> ->
                lists:sort(ByOffset, Confirmed) ++
                    lists:sort(ByID, Pending) ++
                    lists:sort(ByID, WithoutOffset);
            _ ->
                lists:reverse(lists:sort(ByID, Pending)) ++
                    lists:reverse(lists:sort(ByOffset, Confirmed)) ++
                    lists:reverse(lists:sort(ByID, WithoutOffset))
        end,
    ?event(
        {order_by_block,
            {sort_order, SortOrder},
            {with_offset, length(WithOffset)},
            {without_offset, length(WithoutOffset)}
        }
    ),
    UserOrderSorted.

%% @doc Convert a block height range (`#{<<"min">> => Min, <<"max">> => Max}')
%% into weave byte offset boundaries `{StartOffset, EndOffset}'. Notably, the
%% highest offset is not the max block height. It is 'infinity', such that TXs
%% that are indexed but are not yet confirmed are included.
block_range_to_offset_range(Heights, Opts) ->
    StartOffset =
        case hb_maps:get(<<"min">>, Heights, 0, Opts) of
            0 -> 0;
            RawMin ->
                case read_block(hb_util:int(RawMin), Opts) of
                    {ok, MinBlock} ->
                        % The `weave_size` is the size at the _end_ of the block,
                        % so we must subtract the start from it to find the 
                        % starting byte of the block.
                        WeaveSize = hb_util:int(
                            hb_maps:get(<<"weave_size">>, MinBlock, 0, Opts)),
                        BlockSize = hb_util:int(
                            hb_maps:get(<<"block_size">>, MinBlock, 0, Opts)),
                        WeaveSize - BlockSize;
                    {error, not_found} -> 0
                end
        end,
    EndOffset =
        case hb_maps:get(<<"max">>, Heights, infinity, Opts) of
            infinity -> infinity;
            RawMax ->
                case read_block(hb_util:int(RawMax), Opts) of
                    {ok, MaxBlock} ->
                        hb_util:int(
                            hb_maps:get(<<"weave_size">>, MaxBlock, 0, Opts)
                        );
                    {error, not_found} -> infinity
                end
        end,
    ?event(
        {calculated_offsets_from_block_range,
            {block_range, Heights},
            {start_offset, StartOffset},
            {end_offset, EndOffset}
        }
    ),
    {StartOffset, EndOffset}.

%% @doc Read block metadata by height.  Tries the local block cache first;
%% when `query_arweave_remote_block_ranges' is `true' (the default) and the
%% block is not cached locally, falls back to `arweave@2.9/block'.
read_block(Height, Opts) ->
    case read_cached_block(Height, Opts) of
        {ok, Block} -> {ok, Block};
        {error, not_found} ->
            case hb_opts:get(query_arweave_remote_block_ranges, true, Opts) of
                true ->
                    ?event({read_block_remote, {height, Height}}),
                    hb_ao:resolve(
                        #{ <<"device">> => <<"arweave@2.9">> },
                        #{ <<"path">> => <<"block">>, <<"block">> => Height },
                        Opts
                    );
                _ -> {error, not_found}
            end;
        not_found ->
            case hb_opts:get(query_arweave_remote_block_ranges, true, Opts) of
                true ->
                    ?event({read_block_remote, {height, Height}}),
                    hb_ao:resolve(
                        #{ <<"device">> => <<"arweave@2.9">> },
                        #{ <<"path">> => <<"block">>, <<"block">> => Height },
                        Opts
                    );
                _ -> {error, not_found}
            end
    end.

%% @doc Read a block from the Arweave pseudo-path cache.
read_cached_block(Height, Opts) ->
    hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"block">>,
            <<"block">> => Height,
            <<"cache-control">> => [<<"only-if-cached">>]
        },
        Opts
    ).

%% @doc Return the latest block height indexed in the Arweave pseudo-path cache.
latest_cached_block(Opts) ->
    Blocks =
        hb_cache:list_numbered(
            hb_path:to_binary([
                <<"~arweave@2.9">>,
                <<"block">>,
                <<"height">>
            ]),
            Opts
        ),
    case Blocks of
        [] -> not_found;
        _ -> {ok, lists:max(Blocks)}
    end.

%%% Index-served pages

%% @doc Serve a transactions page from the `~match@1.0' index: the matches
%% of the query's pairs over its block range from its cursor, in the sort's
%% direction. Every query the index cannot serve is `unservable':
%% `cached_transactions' answers it.
index_connection(Args, Opts) ->
    maybe
        {ok, Template} ?= index_template(Args, Opts),
        {ok, After} ?= index_cursor(Args, Opts),
        Direction =
            case hb_maps:get(<<"sort">>, Args, <<"HEIGHT_DESC">>, Opts) of
                <<"HEIGHT_ASC">> -> asc;
                _ -> desc
            end,
        Ranges = index_ranges(Direction, Args, Opts),
        PageSize = page_size(Args, Opts),
        {ok, Matches} ?=
            index_matches(Template, Ranges, After, PageSize + 1, Opts),
        More = length(Matches) > PageSize,
        ForceNextPage = force_next_page(Args, Opts),
        {ok,
            #{
                <<"matches">> => lists:sublist(Matches, PageSize),
                <<"terminal">> => ForceNextPage andalso not More,
                <<"template">> => Template,
                <<"ranges">> => Ranges,
                <<"pageInfo">> =>
                    #{ <<"hasNextPage">> => More orelse ForceNextPage }
            }}
    end.

%% @doc The query's filters as the index's pairs: each tag's one value, and
%% the `committer' and `field-target' one owner and one recipient are
%% indexed under. Explicit IDs, a height or bundle filter, a filter given
%% several values, and a query naming no pair are `unservable'.
index_template(Args, Opts) ->
    Get = fun(Filter) -> hb_maps:get(Filter, Args, null, Opts) end,
    Fields =
        [
            {Pair, Get(Filter)}
        ||
            {Pair, Filter} <-
                [
                    {<<"committer">>, <<"owners">>},
                    {<<"field-target">>, <<"recipients">>}
                ]
        ],
    maybe
        true ?= explicit_ids(Args, Opts) =:= [] orelse unservable,
        true ?=
            Get(<<"height">>) =:= null andalso Get(<<"bundledIn">>) =:= null
                orelse unservable,
        true ?=
            lists:all(
                fun({_Pair, Values}) ->
                    Values =:= null orelse length(Values) =< 1
                end,
                Fields
            ) orelse unservable,
        Tags =
            case Get(<<"tags">>) of
                null -> #{};
                Filters -> dev_query_graphql:keys_to_template(Filters)
            end,
        Template =
            maps:merge(
                Tags,
                maps:from_list([ {Pair, Value} || {Pair, [Value]} <- Fields ])
            ),
        true ?= map_size(Template) > 0 orelse unservable,
        {ok, Template}
    end.

%% @doc The match the page resumes after, from the cursor of an
%% index-served edge with its terminal marker dropped; a cursor of another
%% form names a cached item, from which only `cached_transactions' resumes.
index_cursor(Args, Opts) ->
    case hb_maps:get(<<"after">>, Args, null, Opts) of
        Unset when Unset =:= null; Unset =:= undefined; Unset =:= <<>> ->
            {ok, none};
        <<?MEMBER_CURSOR, Cursor/binary>> ->
            {ok, hd(binary:split(Cursor, <<"&">>))};
        _ ->
            unservable
    end.

%% @doc The ranges a page reads in order, as `~match@1.0' bounds: the
%% offsets of the query's block range, from its near end in the page's
%% direction to its far end; or, with no range, the whole weave -- the
%% mempool leading it descending and ending it ascending -- followed by the
%% messages the weave never held, last in either order, as
%% `cached_transactions' orders them.
index_ranges(Direction, Args, Opts) ->
    Heights = hb_maps:get(<<"block">>, Args, null, Opts),
    Ignored = hb_opts:get(query_arweave_ignore_block_ranges, false, Opts),
    Window =
        case Heights =:= null orelse Ignored of
            true -> open;
            false -> block_range_to_offset_range(Heights, Opts)
        end,
    Bounds =
        case {Direction, Window} of
            {asc, open} ->
                [#{ <<"from">> => 0 }, #{ <<"from">> => -1, <<"to">> => 0 }];
            {asc, {Start, infinity}} ->
                [#{ <<"from">> => Start }];
            {asc, {Start, End}} ->
                [#{ <<"from">> => Start, <<"to">> => End }];
            {desc, open} ->
                [#{ <<"from">> => infinity }];
            {desc, {Start, infinity}} ->
                [#{ <<"from">> => infinity, <<"to">> => Start - 1 }];
            {desc, {Start, End}} ->
                [#{ <<"from">> => End - 1, <<"to">> => Start - 1 }]
        end,
    [ Range#{ <<"direction">> => Direction } || Range <- Bounds ].

%% @doc The matches of a page: the ranges read in order from the cursor,
%% which lies in the range holding its key.
index_matches(Template, Ranges, After, Limit, Opts) ->
    % A cursor among the messages the weave never held resumes their range
    % alone: the second of the two an open ascending page reads.
    Ahead =
        case {After, Ranges} of
            {<<"-1", _/binary>>, [_Weave, Unmined]} -> [Unmined];
            _ -> Ranges
        end,
    locate_ranges(Template, Ahead, After, Limit, Opts).

%% @doc The matches of the ranges in order from the cursor, as far as the
%% page has room.
locate_ranges(_Template, Ranges, _After, Limit, _Opts)
        when Ranges =:= []; Limit =:= 0 ->
    {ok, []};
locate_ranges(Template, [Range | Rest], After, Limit, Opts) ->
    Bounds =
        case After of
            none -> Range;
            _ -> (maps:remove(<<"from">>, Range))#{ <<"after">> => After }
        end,
    maybe
        {ok, Matches} ?=
            locate(Template, Bounds#{ <<"limit">> => Limit }, Opts),
        {ok, More} ?=
            locate_ranges(Template, Rest, none, Limit - length(Matches), Opts),
        {ok, Matches ++ More}
    end.

%% @doc The matches of a template through `~match@1.0'. A node without
%% stores of the index is `unservable'; a failing store is an error, as
%% `cached_transactions' answers from different data.
locate(Template, Req, Opts) ->
    case hb_ao:raw(
            <<"match@1.0">>,
            Template,
            Req#{ <<"path">> => <<"locate">> },
            Opts
        ) of
        {error, not_found} -> unservable;
        Result -> Result
    end.

%% @doc The edges of the page's matches in its order, under cursors naming
%% their keys, read together. A match carrying an ID reads its cached
%% message; one without reads the item at its offset from the weave. A
%% match neither can read is dropped and reported.
match_edges(Matches, Opts) ->
    Read =
        hb_pmap:parallel_map(
            Matches,
            fun(Match) -> {Match, match_message(Match, Opts)} end,
            hb_opts:get(arweave_chunk_fetch_concurrency, 10, Opts)
        ),
    lists:filtermap(
        fun({#{ <<"member">> := Member }, {ok, Node}}) ->
                {true,
                    #{
                        <<"cursor">> => <<?MEMBER_CURSOR, Member/binary>>,
                        <<"node">> => Node
                    }};
            ({Match, Error}) ->
                ?event(warning,
                    {match_unreadable, {match, Match}, {error, Error}}
                ),
                false
        end,
        Read
    ).

%% @doc A match's message: through `hb_cache' by its ID, or from the weave
%% by its offset.
match_message(#{ <<"id">> := ID }, Opts) -> hb_cache:read(ID, Opts);
match_message(#{ <<"offset">> := Offset }, Opts) -> header(Offset, Opts).

%% @doc The message of the item at a weave offset, from its header alone:
%% parsed from the bytes between the offset and the end of its chunk -- one
%% fetch, and every byte a gateway serves when the chunk closes the bundle,
%% as the weave's padding follows it -- or from a longer read when the
%% header runs into the next chunk. The data is left out: neither the index
%% nor the header holds its length.
header(Offset, Opts) ->
    Tail =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave@2.9">> },
            #{ <<"path">> => <<"chunk">>, <<"offset">> => Offset + 1 },
            Opts
        ),
    case header_message(Tail, Opts) of
        {ok, Node} ->
            {ok, Node};
        {error, _} ->
            header_message(
                hb_store_arweave:read_chunks(Offset, ?ITEM_PROBE_LENGTH, Opts),
                Opts
            )
    end.

%% @doc The message of the item whose header opens the read bytes.
header_message({ok, Bytes}, Opts) ->
    try
        {ok, _HeaderSize, TX} = ar_bundles:deserialize_header(Bytes),
        {ok,
            hb_message:convert(
                TX#tx{ data = <<>>, data_size = 0 },
                <<"structured@1.0">>,
                <<"ans104@1.0">>,
                Opts
            )}
    catch _:Reason ->
        {error, {'invalid-item', Reason}}
    end;
header_message(Error, _Opts) ->
    Error.

%%% Match argument processing

%% @doc Progressively generate matches from each argument for a transaction
%% query.  The `block' range is applied as a post-filter over the candidate
%% set rather than as a set-producing index lookup.
match_args(Args, Opts) when is_map(Args) ->
    match_args(
        maps:to_list(
            maps:with(
                ?SUPPORTED_QUERY_ARGS,
                Args
            )
        ),
        [],
        Opts
    ).
match_args([], [], _Opts) -> [];
match_args([], Results, _Opts) ->
    ?event({match_args_results, Results}),
    hb_util:unique(
        lists:foldl(
            fun(Result, Acc) -> hb_util:list_with(Result, Acc) end,
            hd(Results),
            tl(Results)
        )
    );
match_args([{Field, X} | Rest], Acc, Opts) ->
    ?event({match, {field, Field}, {arg, X}}),
    case match(Field, X, Opts) of
        {ok, Result} -> match_args(Rest, [Result | Acc], Opts);
        _Error -> match_args(Rest, Acc, Opts)
    end.

%% @doc Generate a match upon `tags' in the arguments, if given.
match(_, null, _) -> ignore;
match(<<"height">>, Heights, Opts) ->
    Min = hb_maps:get(<<"min">>, Heights, 0, Opts),
    Max =
        case hb_maps:find(<<"max">>, Heights, Opts) of
            {ok, GivenMax} -> GivenMax;
            error ->
                hb_util:ok(latest_cached_block(Opts))
        end,
    {ok,
        lists:filtermap(
            fun(Height) ->
                case read_cached_block(Height, Opts) of
                    {ok, Block} ->
                        {true, hb_message:id(Block, none, Opts)};
                    _ ->
                        false
                end
            end,
            lists:seq(Min, Max)
        )
    };
match(<<"id">>, ID, _Opts) ->
    {ok, [ID]};
match(<<"ids">>, IDs, _Opts) ->
    {ok, IDs};
match(<<"tags">>, Tags, Opts) ->
    hb_cache:match(dev_query_graphql:keys_to_template(Tags), Opts);
match(<<"owners">>, Owners, Opts) ->
    {ok, matching_commitments(<<"committer">>, Owners, Opts)};
match(<<"owner">>, Owner, Opts) ->
    Res =  matching_commitments(<<"committer">>, Owner, Opts),
    ?event({match_owner, Owner, Res}),
    {ok, Res};
match(<<"recipients">>, Recipients, Opts) ->
    {ok, matching_commitments(<<"field-target">>, Recipients, Opts)};
match(UnsupportedFilter, _, _) ->
    throw({unsupported_query_filter, UnsupportedFilter}).

%%% Block range post-filter

%% @doc Offset-annotate a list of IDs, returning {StartOffset, ID} pairs.
annotate_ids(IDs, Opts) ->
    case hb_store_arweave:store_from_opts(Opts) of
        no_store -> unavailable;
        StoreOpts -> annotate_offsets(IDs, StoreOpts, undefined, 0, Opts)
    end.
annotate_offsets([], _StoreOpts, _LastOffset, _Ordinate, _Opts) -> [];
annotate_offsets([ID|IDs], StoreOpts, LastOffset, Ordinate, Opts) ->
    {Offset, Annotated} =
        case hb_store_arweave:read_offset(StoreOpts, ID, Opts) of
            {ok, #{ <<"start">> := StartOffset, <<"length">> := Length }} ->
                {
                    StartOffset,
                    #{
                        <<"id">> => ID,
                        <<"offset">> => StartOffset,
                        <<"length">> => Length
                    }
                };
            _ ->
                {undefined, #{ <<"id">> => ID }}
        end,
    {NewOrdinate, Postfix} =
        case Offset =/= undefined andalso not pending_offset(Offset) andalso Offset =:= LastOffset of
            true -> {Ordinate + 1, <<"-", (hb_util:bin(Ordinate + 1))/binary>>};
            false -> {0, <<>>}
        end,
    WithCursor =
        Annotated#{
            <<"cursor">> => << (offset_cursor(ID, Offset))/binary, Postfix/binary >>
        },
    [WithCursor | annotate_offsets(IDs, StoreOpts, Offset, NewOrdinate, Opts)].

offset_cursor(ID, undefined) when is_binary(ID) -> <<"ephemeral=", ID/binary>>;
offset_cursor(ID, Offset) when is_binary(ID) ->
    case pending_offset(Offset) of
        true -> <<"pending=", ID/binary>>;
        false -> <<"offset=", (hb_util:bin(Offset))/binary>>
    end.

pending_offset(relative) -> true;
pending_offset(#{ <<"relative">> := _, <<"offset">> := _ }) -> true;
pending_offset(_) -> false.

%% @doc Apply the `block' height range as a post-filter over candidate IDs.
%% Each candidate's offset is checked against the block range boundaries,
%% avoiding materialisation of the full store.
filter_offset_annotated(AnnotatedIDs, HeightRange, _Opts)
        when HeightRange =:= undefined orelse HeightRange =:= null ->
    AnnotatedIDs;
filter_offset_annotated(AnnotatedIDs, Heights, Opts) ->
    case hb_opts:get(query_arweave_ignore_block_ranges, false, Opts) of
        true ->
            AnnotatedIDs;
        false ->
            do_filter_offset_annotated(AnnotatedIDs, Heights, Opts)
    end.
do_filter_offset_annotated(AnnotatedIDs, Heights, Opts) ->
    {StartOffset, EndOffset} =
        block_range_to_offset_range(Heights, Opts),
    Filtered =
        lists:filter(
            fun(#{ <<"offset">> := Offset }) when Offset =:= relative ->
                    EndOffset =:= infinity;
                (#{ <<"offset">> := Offset }) when is_map(Offset) ->
                    EndOffset =:= infinity;
                (#{ <<"offset">> := IDOffset, <<"length">> := Length })
                        when is_integer(IDOffset) ->
                    ((StartOffset =:= 0) orelse (IDOffset >= StartOffset)) andalso
                        (
                            (EndOffset =:= infinity) orelse
                                (IDOffset + Length =< EndOffset)
                        );
                (_) -> false
            end,
            AnnotatedIDs
        ),
    ?event({filtered_out_of_range, length(AnnotatedIDs) - length(Filtered)}),
    Filtered.

%% @doc Return the IDs of the messages whose commitments carry a field:
%% their committer, or their target.
matching_commitments(Field, Values, Opts) when is_list(Values) ->
    hb_util:unique(lists:flatten(
        lists:filtermap(
            fun(Value) ->
                case matching_commitments(Field, Value, Opts) of
                    not_found -> false;
                    IDs -> {true, IDs}
                end
            end,
            Values
        )
    ));
matching_commitments(Field, Value, Opts) when is_binary(Value) ->
    case hb_cache:match(#{ Field => Value }, Opts) of
        {ok, IDs} ->
            ?event(
                {found_matching_commitments,
                    {field, Field},
                    {value, Value},
                    {ids, IDs}
                }
            ),
            IDs;
        _ -> not_found
    end.

%% @doc Return the explicit IDs from the arguments, if given. Searches for
%% both `ids' and `id' keys.
explicit_ids(Args, Opts) ->
    hb_util:unique(
        case hb_maps:get(<<"ids">>, Args, null, Opts) of
            IDs when is_list(IDs) -> IDs;
            _ -> []
        end ++
        case hb_maps:get(<<"id">>, Args, null, Opts) of
            ID when is_binary(ID) -> [ID];
            _ -> []
        end
    ).

pending_offsets_page_by_cursor_test() ->
    Store = hb_test_utils:test_store(),
    ArweaveStore = #{ <<"store-module">> => hb_store_arweave, <<"index-store">> => [Store] },
    Opts = #{ <<"store">> => [Store], <<"arweave-index-store">> => ArweaveStore },
    {ok, NumericID} =
        hb_cache:write(#{ <<"type">> => <<"Message">>, <<"data">> => <<"numeric">> }, Opts),
    {ok, PendingA} =
        hb_cache:write(#{ <<"type">> => <<"Message">>, <<"data">> => <<"pending-a">> }, Opts),
    ok = hb_store_arweave:write_offset(
        ArweaveStore, NumericID, <<"tx@1.0">>, 10, 1),
    ok = hb_store_arweave:write_offset(
        ArweaveStore, PendingA, <<"tx@1.0">>, relative, 0),
    BaseArgs =
        #{
            <<"ids">> => [PendingA, NumericID],
            <<"block">> => #{ <<"min">> => 0 },
            <<"first">> => 1
        },
    Page =
        fun(Args) ->
            {ok, #{ <<"edges">> := [Edge] }} =
                query(#{}, <<"transactions">>, Args, Opts),
            Edge
        end,
    {ok, BlockMsgID} =
        hb_cache:write(
            #{ <<"height">> => 1, <<"weave_size">> => 100, <<"block_size">> => 100 },
            Opts
        ),
    hb_cache:link(
        BlockMsgID,
        [<<"~arweave@2.9">>, <<"block">>, <<"height">>, <<"1">>],
        Opts
    ),
    #{ <<"id">> := NumericID } = Page(BaseArgs#{ <<"block">> => #{ <<"max">> => 1 } }),
    #{ <<"id">> := NumericID } = Page(BaseArgs#{ <<"sort">> => <<"HEIGHT_ASC">> }),
    #{ <<"id">> := PendingA, <<"cursor">> := FirstCursor } = Page(BaseArgs),
    #{ <<"id">> := NumericID } = Page(BaseArgs#{ <<"after">> => FirstCursor }),
    ok.

%% @doc The messages the weave never held page out last in either order,
%% by cursor, from a node's own stores.
unmined_pages_test() ->
    Opts = #{ <<"store">> => [hb_test_utils:test_store()] },
    Node = hb_http_server:start_node(Opts),
    lists:foreach(
        fun(N) ->
            {ok, _} =
                hb_cache:write(
                    #{ <<"type">> => <<"Unmined">>, <<"n">> => hb_util:bin(N) },
                    Opts
                )
        end,
        lists:seq(1, 3)
    ),
    Query =
        <<"""
            query($after: String, $sort: SortOrder) {
                transactions(
                    tags: [{ name: "type", values: ["Unmined"] }],
                    first: 1,
                    after: $after,
                    sort: $sort
                ) {
                    pageInfo { hasNextPage }
                    edges { cursor node { id } }
                }
            }
        """>>,
    Pages =
        fun Pages(Sort, After, Acc) ->
            #{
                <<"edges">> := [Edge = #{ <<"cursor">> := Cursor }],
                <<"pageInfo">> := #{ <<"hasNextPage">> := More }
            } =
                hb_util:deep_get(
                    <<"data/transactions">>,
                    dev_query_graphql:test_query(
                        Node,
                        Query,
                        #{ <<"after">> => After, <<"sort">> => Sort },
                        Opts
                    ),
                    #{},
                    Opts
                ),
            Item = maps:get(<<"node">>, Edge),
            case More of
                true -> Pages(Sort, Cursor, [Item | Acc]);
                false -> lists:reverse([Item | Acc])
            end
        end,
    Descending = Pages(<<"HEIGHT_DESC">>, null, []),
    ?assertEqual(3, length(lists:usort(Descending))),
    ?assertEqual(lists:reverse(Descending), Pages(<<"HEIGHT_ASC">>, null, [])).

%% @doc A page served from the published index on Arweave, its items
%% read from the weave: the twenty-four items of `action=Battle.Begin'
%% page out in each order under cursors naming their keys, every item
%% carrying the tag it was found by, the count is the whole set's, and the
%% last page closes.
published_pages_test_() ->
    {timeout, 300, fun published_pages/0}.
published_pages() ->
    Sizes = <<"&key-hash-size=39&value-hash-size=40&offset-size=49">>,
    Opts =
        #{
            <<"store">> =>
                [
                    (hb_test_utils:test_store(hb_store_lmdb))#{
                        <<"capacity">> => 1024 * 1024 * 1024
                    }
                ],
            <<"match-index">> =>
                [
                    #{
                        <<"store-module">> => hb_store_arlmdb,
                        <<"name">> => <<"query-published-index">>,
                        <<"root">> =>
                            <<"oWRzBr3KHhULAL-s5ULeXac1mb_WQOX5uFBRea16iRI">>,
                        <<"return-row">> => true,
                        <<"prefix">> => <<"~match@1.0/">>,
                        <<"to-key">> => <<"~match@1.0/row", Sizes/binary>>,
                        <<"from-key">> => <<"~match@1.0/member", Sizes/binary>>
                    }
                ]
        },
    Node = hb_http_server:start_node(Opts),
    Query =
        <<"""
            query($after: String, $sort: SortOrder) {
                transactions(
                    tags: [{ name: "action", values: ["Battle.Begin"] }],
                    first: 10,
                    after: $after,
                    sort: $sort
                ) {
                    count
                    pageInfo { hasNextPage }
                    edges { cursor node { id tags { name value } } }
                }
            }
        """>>,
    Page =
        fun(Sort, After) ->
            hb_util:deep_get(
                <<"data/transactions">>,
                dev_query_graphql:test_query(
                    Node,
                    Query,
                    #{ <<"after">> => After, <<"sort">> => Sort },
                    Opts
                ),
                #{},
                Opts
            )
        end,
    Pages =
        fun Pages(Sort, After, Acc) ->
            #{
                <<"edges">> := Edges,
                <<"pageInfo">> := #{ <<"hasNextPage">> := More }
            } = Page(Sort, After),
            Last = maps:get(<<"cursor">>, lists:last(Edges)),
            case More of
                true -> Pages(Sort, Last, Acc ++ Edges);
                false -> Acc ++ Edges
            end
        end,
    Descending = Pages(<<"HEIGHT_DESC">>, null, []),
    ?assertEqual(24, length(Descending)),
    ?assertEqual(
        lists:reverse(Descending),
        Pages(<<"HEIGHT_ASC">>, null, [])
    ),
    IDs = [ ID || #{ <<"node">> := #{ <<"id">> := ID } } <- Descending ],
    ?assertEqual(24, length(lists:usort(IDs))),
    ?assert(
        lists:all(
            fun(#{ <<"node">> := #{ <<"tags">> := Tags } }) ->
                lists:member(
                    #{
                        <<"name">> => <<"action">>,
                        <<"value">> => <<"Battle.Begin">>
                    },
                    Tags
                )
            end,
            Descending
        )
    ),
    ?assertMatch(#{ <<"count">> := <<"24">> }, Page(<<"HEIGHT_DESC">>, null)).
