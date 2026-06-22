%%% @doc An implementation of the Arweave GraphQL API, inside the `~query@1.0'
%%% device.
%%%
%%% When an `hb_store_arweave' index is available, transaction results are
%%% sorted by block height via the monotonically increasing Arweave data
%%% offsets stored in `hb_store_arweave_offset'.  The `sort' argument on the
%%% `transactions' query selects the order (`HEIGHT_DESC' by default,
%%% `HEIGHT_ASC' for ascending).  A `block' range filter narrows results to
%%% transactions whose offsets fall within the requested block heights.
-module(dev_query_arweave).
%%% AO-Core API:
-export([query/4]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%%% Default returned page size and maximum allowed page size.
-define(DEFAULT_PAGE_SIZE, 10).
-define(DEFAULT_MAX_PAGE_SIZE, 100).
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
            {ok, #{ <<"start-offset">> := StartOffset, <<"length">> := Length }} ->
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

%% @doc Return the base IDs for messages that have a matching commitment.
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
            lists:map(fun(ID) -> commitment_id_to_base_id(ID, Opts) end, IDs);
        _ -> not_found
    end.

%% @doc Convert a commitment message's ID to a base ID.
commitment_id_to_base_id(ID, Opts) ->
    Store = hb_opts:get(store, no_store, Opts),
    ?event({commitment_id_to_base_id, ID}),
    case hb_store:read(Store, << ID/binary, "/signature">>, Opts) of
        {ok, EncSig} ->
            Sig = hb_util:decode(EncSig),
            ?event({commitment_id_to_base_id_sig, Sig}),
            hb_util:encode(hb_crypto:sha256(Sig));
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
