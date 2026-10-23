%%% @doc Arweave-shaped GraphQL queries over AO-Core messages, called by
%%% `dev_query_graphql' within `query@1.0'. This is a subset of the API declared
%%% in `scripts/schema.gql', not a separate device or a transaction codec.
%%%
%%% `transaction(id: ...)' reads one message by ID. `transactions' returns
%%% `edges' containing `node' and `cursor', `pageInfo.hasNextPage', and an
%%% optional string `count'. Supported filters are `ids', `tags', `owners',
%%% `recipients' and `block'. Tag names address message keys. Values within a
%%% tag are ORed; separate tags and other filters are ANDed. Owners are
%%% Arweave addresses, matched by the URIs of their committers; indexed
%%% recipients use the commitment's `field-target'.
%%% Without these selecting filters,
%%% queries enumerate confirmed base-layer TXs from block headers, optionally
%%% within inclusive `block.min/max' heights. This excludes bundled data items
%%% and pending messages. Heights start at zero and end at the compact index's
%%% tip, or the network tip if the index is empty and remote reads are enabled.
%%% Within a height, TX IDs sort lexically; descending reverses both orders.
%%% `block=...&tx=...' cursors resume exclusively within that height. Missing
%%% headers return errors; empty blocks are skipped. `count' has the same cap
%%% as indexed queries and ignores `after'. Selected nodes read by ID from the
%%% local stores, then `~arweave@2.9/tx' with `exclude-data=true'.
%%% Their block fields reuse the containing header.
%%%
%%% `first' limits the page, clamped between zero and node option
%%% `max-page-size' (default 100). The schema defaults `first' to 10; calls
%%% without that argument use `default-page-size' (default 10). `sort' defaults
%%% to `HEIGHT_DESC'; `HEIGHT_ASC' reverses weave order. Pending positions lead
%%% descending and follow confirmed positions ascending; messages with no
%%% position come last in either direction. Pass a returned `cursor' unchanged
%%% as `after' with the same filters and sort; cursors should be treated as
%%% opaque. Pagination reads the current index, not a saved snapshot.
%%%
%%% Queries with at least one indexed predicate and no explicit IDs or bundle
%%% filter can use `~match@1.0/locate' with a compatible cursor. Its store
%%% pipelines supply entries with `offset', `id' and `commitment-device'.
%%% A known L1 ID reads locally, then requests its native header; other IDs
%%% read through `hb_cache', using the node's configured stores. An entry without
%%% an ID is read at its weave offset and deserialized by its commitment device
%%% with `exclude-data=true', then converted to a structured message. This
%%% requires a byte-addressable item and a device supporting header decoding;
%%% it cannot identify a base-layer transaction by offset alone. Header reads
%%% may span chunks. The payload is omitted, so `data.size' is null for these
%%% header-only results: its original length is unknown.
%%%
%%% Indexed pagination orders by offset and ID and returns `member=' cursors.
%%% One ID at two offsets can produce two edges. Unreadable entries are omitted
%%% from the edges, and the next matches take their place until the page has
%%% `first' edges, the index has no more, or the page has read
%%% `query-arweave-max-index-count' matches. `hasNextPage' reflects remaining
%%% index matches, not their readability. `count' ignores `after' and counts
%%% matches across the query's ranges, capped by `query-arweave-max-index-count'
%%% (default 1000); it is neither an uncapped total nor a count of readable
%%% edges. Cursor-only indexed and block-TX pages do not read transaction
%%% messages; their cursors identify candidates regardless of readability.
%%% Explicit IDs select candidates directly. With predicates, each readable
%%% candidate is checked by `match@1.0/check', without scanning the index; count
%%% includes only matching readable candidates. Missing positions do not prevent
%%% predicate checks. Without predicates, count precedes message read failures;
%%% cursor-only ID pages with known positions omit message reads. IDs without
%%% positions must be readable before they can supply an edge.
%%% Without an Arweave offset store, that path cannot order or filter by weave
%%% position. If reading an entry gives an error other than `not_found', the
%%% page fails with that error.
%%%
%%% With selecting filters, `block' bounds are inclusive heights translated
%%% to weave byte ranges.
%%% Indexed matching tests entry offsets; cache matching tests each item's
%%% start and end. Block metadata is read locally, then remotely unless
%%% `query-arweave-remote-block-ranges=false'. A missing lower or upper bound's
%%% block metadata falls back to zero or infinity respectively.
%%% `query-arweave-ignore-block-ranges=true' disables this filtering.
%%% The AO-Core request's `force-next-page=true' forces `hasNextPage=true' and
%%% appends `&remaining=0' to the last cursor when no further match is found.
%%%
%%% `id' uses the message projection in `dev_query_graphql'. Native transaction
%%% tags use the requested commitment's original tags when present, otherwise
%%% normalized base-message keys excluding its native field keys.
%%% Signature and owner fields come from
%%% a signed commitment; recipient and anchor use commitment field mappings.
%%% `fee' and `quantity' use `field-reward'
%%% and `field-quantity' on the requested commitment, defaulting to zero.
%%% Amounts project as winston and exact AR strings.
%%% `data.size' prefers an L1 transaction's declared or indexed payload size,
%%% then measures binary `data', falling back to `body', then an empty binary.
%%% Structured bodies and omitted payloads have unknown size (null).
%%% `data.type' reads `content-type'. These projections
%%% do not reconstruct an Arweave transaction or its original tag list.
%%% Transaction `block' seeks the compact block index at the matched weave
%%% position, then reads the selected header. Index misses fall back to cached
%%% block ranges, sharing the height catalogue within the page, then remote
%%% heights when enabled. L1 IDs resolve shared boundaries; pending positions
%%% return null. Block metadata is prepared only for selected fields or L1
%%% boundary checks; empty pages do not enumerate cached heights.
%%%
%%% `blocks' pages by height, descending by default or `HEIGHT_ASC', within
%%% inclusive `height.min/max' bounds. Height enumeration defaults to zero and
%%% the network tip and reads at most `first + 1' heights. Supplied `ids' are
%%% resolved, filtered by the given bounds, sorted and paged instead.
%%% `height=' cursors resume exclusively; failed reads
%%% return errors. `block(id: ...)' reads one block. Reads use the same local
%%% and remote policy as transaction block bounds. Bundle and ingestion-time
%%% filters and ingestion-time ordering are not implemented. `networkInfo.height'
%%% reads the configured Arweave node's status. `parent { id }' and `bundledIn { id }'
%%% return null for an L1 transaction and an empty ID for a bundled item.
%%% Other unsupported fields may return a placeholder or a GraphQL type error.
%%% Schema acceptance does not imply filter support.
-module(dev_query_arweave).
-device_libraries([lib_arweave_common]).
%%% AO-Core API:
-export([query/4]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%%% Default returned page size and maximum allowed page size.
-define(DEFAULT_PAGE_SIZE, 10).
-define(DEFAULT_MAX_PAGE_SIZE, 100).
%% The cursor of an index-served edge: its match's key in the index.
-define(MEMBER_CURSOR, "member=").
%% The most matches a page reads by default, for its `count' or its edges.
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
    % The edges of a block-TX page, read only when asked for. The
    % terminal page of a forced-next-page read marks its last cursor.
    Edges = match_edges(Matches, Opts),
    Marked =
        case Terminal of
            true -> force_terminal_cursor(Edges);
            false -> Edges
        end,
    {ok, [{ok, Edge} || Edge <- Marked]};
query(#{ <<"predicates">> := Predicates, <<"ranges">> := Ranges },
        <<"count">>, _Args, Opts) ->
    % The count of an index-served page, read over its ranges on demand,
    % up to the node's maximum.
    Cap =
        hb_opts:get(
            query_arweave_max_index_count,
            ?DEFAULT_MAX_INDEX_COUNT,
            Opts
        ),
    case index_matches(Predicates, Ranges, none, Cap, Opts) of
        {ok, Matches} -> {ok, hb_util:bin(length(Matches))};
        Error -> Error
    end;
query(#{ <<"block-range">> := Range }, <<"count">>, _Args, Opts) ->
    Cap = hb_opts:get(query_arweave_max_index_count, ?DEFAULT_MAX_INDEX_COUNT, Opts),
    maybe
        {ok, Matches} ?= block_transactions(Range, none, Cap, Opts),
        {ok, hb_util:bin(length(Matches))}
    end;
query(Obj, <<"transaction">>, Args, Opts) ->
    case query(Obj, <<"transactions">>, Args,
            Opts#{ <<"query-arweave-nodes">> => true }) of
        {ok, #{ <<"edges">> := [] }} -> {ok, null};
        {ok, #{ <<"edges">> := [#{ <<"node">> := Msg } | _] }} -> {ok, Msg}
    end;
query(Obj, <<"transactions">>, RawArgs, Opts) ->
    Args = maps:map(
        fun(<<"tags">>, Tags) when is_list(Tags) ->
            [Tag#{ <<"name">> := hb_util:lowercase_key(Name) }
                || Tag = #{ <<"name">> := Name } <- Tags];
           (_, Value) -> Value
        end,
        RawArgs
    ),
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
    case hb_maps:get(<<"id">>, Args, null, Opts) of
        null -> transaction_block(Obj, Opts);
        ID -> read_block(ID, Opts)
    end;
query(_Obj, <<"networkInfo">>, _Args, Opts) ->
    hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        <<"status">>,
        Opts
    );
query(_Obj, <<"blocks">>, Args, Opts) ->
    block_connection(Args, Opts);
query(Block, <<"previous">>, _Args, Opts) ->
    {ok, hb_maps:get(<<"previous-block">>, Block, null, Opts)};
query(Block, <<"height">>, _Args, Opts) ->
    {ok, hb_maps:get(<<"height">>, Block, null, Opts)};
query(Block, <<"timestamp">>, _Args, Opts) ->
    {ok, hb_maps:get(<<"timestamp">>, Block, null, Opts)};
query(Msg, <<"tags">>, Args, Opts) ->
    Selected =
        case hb_private:get(<<"query-match/id">>, Msg, <<>>, Opts) of
            <<>> -> Msg;
            ID -> hb_message:with_commitments(ID, Msg, Opts)
        end,
    Native = [Commitment
        || Commitment <- hb_maps:values(
            hb_message:commitments(#{}, Selected, Opts), Opts),
        lists:member(hb_maps:get(<<"commitment-device">>, Commitment, undefined, Opts),
            [<<"tx@1.0">>, <<"ans104@1.0">>])],
    maybe
        [Commitment | _] ?= Native,
        {ok, Tags} ?= hb_maps:find(<<"original-tags">>, Commitment, Opts),
        {ok, [{ok, Tag} || Tag <- hb_util:message_to_ordered_list(Tags, Opts)]}
    else
        _ ->
            Comm = case Native of [C | _] -> C; [] -> #{} end,
            Fields = [Key || <<"field-", Key/binary>> <- hb_maps:keys(Comm, Opts)],
            BundleTags = hb_maps:with(?BUNDLE_KEYS, Comm, Opts),
            {ok, Keys} =
                dev_query_graphql:execute(
                    #{opts => Opts},
                    hb_maps:merge(
                        hb_maps:without(
                            Fields, hb_message:uncommitted(Msg, Opts), Opts),
                        BundleTags, Opts),
                    <<"keys">>,
                    Args
                ),
            % We give the tags in the order of the commitment's `committed'
            % keys, because the item's signature covers them in that order.
            Ordered =
                lib_arweave_common:order_tags(
                    [{Name, Tag} || Tag = {ok, #{ <<"name">> := Name }} <- Keys],
                    Comm,
                    Opts
                ),
            {ok, [Tag || {_, Tag} <- Ordered]}
    end;
query(Msg, <<"signature">>, _Args, Opts) ->
    % Return the signature of the transaction.
    % Other TX access methods are defined below.
    case hb_message:commitments(#{ <<"committer">> => '_' }, Msg, Opts) of
        not_found -> {ok, <<>>};
        Commitments ->
            case hb_maps:keys(Commitments) of
                [] -> {ok, <<>>};
                [CommID | _] ->
                    {ok, Commitment} = hb_maps:find(CommID, Commitments, Opts),
                    hb_maps:find(<<"signature">>, Commitment, Opts)
            end
    end;
query(Msg, <<"owner">>, _Args, Opts) ->
    ?event({query_owner, Msg}),
    case hb_message:commitments(#{ <<"committer">> => '_' }, Msg, Opts) of
        not_found -> {ok, #{ <<"address">> => <<>>, <<"key">> => <<>> }};
        Commitments ->
            case hb_maps:keys(Commitments) of
                [] -> {ok, #{ <<"address">> => <<>>, <<"key">> => <<>> }};
                [CommID | _] ->
                    {ok, Commitment} = hb_maps:find(CommID, Commitments, Opts),
                    {ok, Committer} =
                        hb_maps:find(<<"committer">>, Commitment, Opts),
                    {ok, #{
                        <<"address">> => hb_util:committer_address(Committer),
                        <<"key">> =>
                            hb_maps:get(
                                <<"public-key">>,
                                Commitment,
                                hb_maps:get(<<"owner">>, Commitment, null, Opts),
                                Opts
                            )
                    }}
            end
    end;
query(#{ <<"key">> := Key }, <<"key">>, _Args, _Opts) ->
    {ok, Key};
query(#{ <<"address">> := Address }, <<"address">>, _Args, _Opts) ->
    {ok, Address};
query(Msg, <<"fee">>, _Args, Opts) ->
    transaction_amount(Msg, <<"field-reward">>, Opts);
query(Msg, <<"quantity">>, _Args, Opts) ->
    transaction_amount(Msg, <<"field-quantity">>, Opts);
query(Number, <<"winston">>, _Args, _Opts) ->
    {ok, hb_util:bin(Number)};
query(Number, <<"ar">>, _Args, _Opts) ->
    Winston = hb_util:int(Number),
    {ok,
        iolist_to_binary(
            io_lib:format(
                "~B.~12..0B",
                [Winston div ?WINSTON_PER_AR, Winston rem ?WINSTON_PER_AR]
            )
        )
    };
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
query(Msg, <<"data">>, _Args, _Opts) ->
    {ok, Msg};
query(Msg, <<"size">>, _Args, Opts) ->
    Size =
        case find_field_key(<<"field-data-size">>, Msg, Opts) of
            {ok, null} -> indexed_data_size(Msg, Opts);
            {ok, DeclaredSize} -> DeclaredSize
        end,
    case Size of
        null ->
            Data =
                case hb_private:get(<<"query-data-omitted">>, Msg, false, Opts) of
                    true -> null;
                    false ->
                        hb_ao:get_first(
                            [
                                {{as, <<"message@1.0">>, Msg}, <<"data">>},
                                {{as, <<"message@1.0">>, Msg}, <<"body">>}
                            ],
                            <<>>,
                            Opts
                        )
                end,
            {ok,
                case Data of
                    Bin when is_binary(Bin) -> byte_size(Bin);
                    _ -> null
                end
            };
        _ -> {ok, Size}
    end;
query(Msg, <<"type">>, _Args, Opts) ->
    {ok, hb_maps:get(<<"content-type">>, Msg, null, Opts)};
query(Msg, Field, _Args, Opts)
        when Field =:= <<"bundledIn">>; Field =:= <<"parent">> ->
    L1 = #{ <<"commitment-device">> => <<"tx@1.0">> },
    case hb_maps:values(hb_message:commitments(L1, Msg, Opts), Opts) of
        [] -> {ok, #{}};
        _ -> {ok, null}
    end;
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
            Ordered =
                case annotate_ids(Matches, Opts) of
                    unavailable -> [#{ <<"id">> => ID,
                        <<"cursor">> => offset_cursor(ID, undefined) } || ID <- Matches];
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
            {ok, connection(check_explicit_ids(Ordered, Args, Opts), Args, Opts)};
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

%% @doc Native amounts come only from the requested commitment's fields.
transaction_amount(Msg, Field, Opts) ->
    Selected =
        case hb_private:get(<<"query-match/id">>, Msg, <<>>, Opts) of
            <<>> -> Msg;
            ID -> hb_message:with_commitments(ID, Msg, Opts)
        end,
    Commitments = hb_message:commitments(#{ Field => '_' }, Selected, Opts),
    case hb_maps:values(Commitments, Opts) of
        [] -> {ok, 0};
        [Commitment | _] -> {ok, hb_maps:get(Field, Commitment, 0, Opts)}
    end.

%% @doc Find a field preserved by a message's commitment.
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

%% @doc The offset index length is the payload size for an L1 transaction.
indexed_data_size(Msg, Opts) ->
    case hb_private:get(<<"query-match">>, Msg, #{}, Opts) of
        #{ <<"commitment-device">> := <<"tx@1.0">>,
            <<"length">> := Length } -> Length;
        #{ <<"commitment-device">> := Device }
                when Device =/= <<>>, Device =/= <<"tx@1.0">> -> null;
        #{ <<"id">> := ID } when ID =/= <<>> -> indexed_l1_size(ID, Opts);
        _ -> null
    end.

indexed_l1_size(ID, Opts) ->
    case hb_store_arweave:store_from_opts(Opts) of
        no_store -> null;
        Store ->
            case hb_store_arweave:read_offset(Store, ID, Opts) of
                {ok, #{ <<"codec">> := <<"tx@1.0">>,
                    <<"length">> := Length }} -> Length;
                _ -> null
            end
    end.

%% @doc Generate the connection response for a ordered, annotated list of 
%% results.
connection(Ordered, Args, Opts) ->
    ResultsCount = length(Ordered),
    Remaining = drop_to_cursor(Args, Ordered, Opts),
    CountToReturn = page_size(Args, Opts),
    ResultsPagePlusOne = read_ids(Remaining, CountToReturn + 1, Opts),
    ResultsPage = block_edges(lists:sublist(ResultsPagePlusOne, CountToReturn), Opts),
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
read_ids([ID], _Count, Opts) -> read_id(ID, Opts);
read_ids(IDs, Count, Opts) ->
    {Page, Rest} = lists:split(min(Count, length(IDs)), IDs),
    Read = lists:append(hb_pmap:parallel_map(
        Page,
        fun(Annotated) -> read_id(Annotated, Opts) end,
        hb_opts:get(arweave_chunk_fetch_concurrency, 10, Opts)
    )),
    Read ++ read_ids(Rest, Count - length(Read), Opts).

%% @doc Known positions supply cursors without reading unselected nodes.
%% Otherwise omit messages the stores cannot supply.
read_id(#{ <<"offset">> := _, <<"cursor">> := Cursor },
        #{ <<"query-arweave-nodes">> := false }) ->
    [#{ <<"cursor">> => Cursor }];
read_id(#{ <<"node">> := _ } = Edge, _Opts) ->
    [Edge];
read_id(AnnotatedID, Opts) ->
    case read_match(AnnotatedID, Opts) of
        {ok, Msg} ->
            [AnnotatedID#{ <<"node">> =>
                hb_private:set(Msg, <<"query-match">>, AnnotatedID, Opts)
            }];
        _ -> []
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
    ByOffset = fun(#{ <<"offset">> := A, <<"id">> := AID },
        #{ <<"offset">> := B, <<"id">> := BID }) -> {A, AID} =< {B, BID} end,
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

%%% Block pages.

%% @doc Find a transaction's block, trying cached headers before remote ones.
%% Use the matched position, so pending entries cannot inherit a confirmed block.
transaction_block(Msg, Opts) ->
    Match = hb_private:get(<<"query-match">>, Msg, #{}, Opts),
    match_block(Match, Opts).

%% @doc Resolve the containing block from a match's position and signed ID.
match_block(Match, Opts) ->
    {Result, _} = resolve_match_block(Match, Opts),
    Result.

%% @doc Reuse prepared results and lazily share fallback heights within a page.
resolve_match_block(#{ <<"block-result">> := Result }, Opts) -> {Result, Opts};
resolve_match_block(Match = #{ <<"offset">> := Offset },
        Opts = #{ <<"query-block">> := Block }) when is_integer(Offset), Offset >= 0 ->
    End = hb_util:int(hb_maps:get(<<"weave-size">>, Block, 0, Opts)),
    Start = End - hb_util:int(hb_maps:get(<<"block-size">>, Block, 0, Opts)),
    case Start =< Offset andalso Offset =< End
            andalso block_contains(Match, Block, End, Opts) of
        true -> {{ok, Block}, Opts};
        false -> resolve_match_block(Match, maps:remove(<<"query-block">>, Opts))
    end;
resolve_match_block(Match = #{ <<"offset">> := Offset }, Opts)
        when is_integer(Offset), Offset >= 0 ->
    From = case hb_maps:get(<<"commitment-device">>, Match, <<>>, Opts) of
        <<"tx@1.0">> -> Offset;
        _ -> Offset + 1
    end,
    case indexed_block(Match, From, 0, Opts) of
        {ok, null} ->
            QueryOpts = case maps:is_key(<<"query-block-heights">>, Opts) of
                true -> Opts;
                false -> Opts#{ <<"query-block-heights">> =>
                    list_to_tuple(lists:sort(cached_block_heights(Opts))) }
            end,
            Sorted = maps:get(<<"query-block-heights">>, QueryOpts),
            Result = case block_at_offset(Match, Sorted, 1, tuple_size(Sorted), QueryOpts) of
                {ok, null} -> remote_transaction_block(Match, QueryOpts);
                Found -> Found
            end,
            {Result, QueryOpts};
        Result -> {Result, Opts}
    end;
resolve_match_block(_Match, Opts) -> {{ok, null}, Opts}.

%% @doc Prepare only requested block metadata, listing fallback heights at
%% most once for the batch, reusing the preceding block when it contains the
%% next match. Results stay with the match in request-private data.
prepare_blocks(Matches, Needed, Opts) ->
    lists:mapfoldl(
        fun(Match, QueryOpts) ->
            case Needed(Match) of
                false -> {Match, QueryOpts};
                true ->
                    {Result, NextOpts} = resolve_match_block(Match, QueryOpts),
                    Reuse = case Result of
                        {ok, Block} when Block =/= null -> NextOpts#{ <<"query-block">> => Block };
                        _ -> NextOpts
                    end,
                    {Match#{ <<"block-result">> => Result }, Reuse}
            end
        end,
        Opts,
        Matches
    ).

%% @doc Project selected block fields after reading a nonempty transaction page.
block_edges(Edges, Opts) ->
    case hb_opts:get(query_arweave_blocks, false, Opts) of
        false -> Edges;
        true ->
            Matches = [hb_private:get(<<"query-match">>, maps:get(<<"node">>, Edge), #{}, Opts)
                || Edge <- Edges],
            {Prepared, _} = prepare_blocks(Matches, fun(_) -> true end, Opts),
            [Edge#{ <<"node">> := hb_private:set(maps:get(<<"node">>, Edge),
                <<"query-match">>, Match, Opts) }
                || {Edge, Match} <- lists:zip(Edges, Prepared)]
    end.

%% @doc L1s at either byte boundary need membership checks for height filters.
block_boundary(#{ <<"commitment-device">> := <<"tx@1.0">>, <<"offset">> := Offset },
        #{ <<"block-start">> := Start, <<"block-end">> := End }) ->
    is_integer(Offset) andalso (Offset =:= Start orelse Offset =:= End);
block_boundary(_Match, _Bounds) -> false.

%% @doc Seek the first candidate by end offset, then read only its header.
%% L1s also inspect every same-end block for zero-data transaction membership.
indexed_block(Match = #{ <<"offset">> := Offset }, From, Height, Opts) ->
    maybe
        {ok, [Entry]} ?= hb_ao:resolve(
            #{ <<"path">> => <<"~arweave@2.9/blocks">>,
                <<"weave-size">> => From, <<"height">> => Height }, Opts),
        End = hb_maps:get(<<"weave-size">>, Entry, 0, Opts),
        Result = read_block(hb_maps:get(<<"hash">>, Entry, not_found, Opts), Opts),
        case Result of
            {ok, Block} ->
                BlockEnd = hb_util:int(hb_maps:get(<<"weave-size">>, Block, 0, Opts)),
                Start = BlockEnd - hb_util:int(hb_maps:get(<<"block-size">>, Block, 0, Opts)),
                case Start =< Offset andalso BlockEnd =:= End
                        andalso block_contains(Match, Block, End, Opts) of
                    true -> {ok, Block};
                    false when End =:= Offset ->
                        indexed_block(Match, End,
                            hb_maps:get(<<"height">>, Entry, 0, Opts) + 1, Opts);
                    false -> {ok, null}
                end;
            {error, not_found} when End =:= Offset ->
                indexed_block(Match, End,
                    hb_maps:get(<<"height">>, Entry, 0, Opts) + 1, Opts);
            {error, not_found} -> {ok, null};
            ReadError -> ReadError
        end
    else
        {ok, []} -> {ok, null};
        Error -> Error
    end.

%% @doc Search all heights only when the node allows remote block reads.
remote_transaction_block(Match, Opts) ->
    case hb_opts:get(query_arweave_remote_block_ranges, true, Opts) of
        true ->
            maybe
                {ok, Status} ?= query(undefined, <<"networkInfo">>, #{}, Opts),
                Height = hb_util:int(hb_maps:get(<<"height">>, Status, 0, Opts)),
                block_at_offset(Match, remote, 0, Height, Opts)
            end;
        _ -> {ok, null}
    end.

%% @doc Binary-search blocks by their weave ranges. L1 membership
%% disambiguates zero-data transactions at shared block boundaries.
block_at_offset(_Match, _Heights, Low, High, _Opts) when Low > High ->
    {ok, null};
block_at_offset(Match = #{ <<"offset">> := Offset }, Heights, Low, High, Opts) ->
    Mid = (Low + High) div 2,
    maybe
        {ok, Block} ?=
            case Heights of
                remote -> read_block(Mid, Opts);
                _ -> read_cached_block(element(Mid, Heights), Opts)
            end,
        End = hb_util:int(hb_maps:get(<<"weave-size">>, Block, 0, Opts)),
        Start = End - hb_util:int(hb_maps:get(<<"block-size">>, Block, 0, Opts)),
        case {Offset < Start, Offset > End} of
            {true, _} -> block_at_offset(Match, Heights, Low, Mid - 1, Opts);
            {_, true} -> block_at_offset(Match, Heights, Mid + 1, High, Opts);
            _ ->
                case block_contains(Match, Block, End, Opts) of
                    true -> {ok, Block};
                    false ->
                        maybe
                            {ok, null} ?=
                                case Offset =:= Start of
                                    true -> block_at_offset(
                                        Match, Heights, Low, Mid - 1, Opts);
                                    false -> {ok, null}
                                end,
                            case Offset =:= End of
                                true -> block_at_offset(
                                    Match, Heights, Mid + 1, High, Opts);
                                false -> {ok, null}
                            end
                        end
                end
        end
    else
        {error, not_found} -> {ok, null};
        Error -> Error
    end.

%% @doc L1s must occur in the block's TX list; bundled items occupy bytes
%% before its end. Zero-data L1s can also sit exactly at the end.
block_contains(Match = #{ <<"commitment-device">> := <<"tx@1.0">> },
        Block, _End, Opts) ->
    lists:member(hb_maps:get(<<"id">>, Match, <<>>, Opts),
        hb_maps:get(<<"txs">>, Block, [], Opts));
block_contains(#{ <<"offset">> := Offset }, _Block, End, _Opts) ->
    Offset < End.

%% @doc Read a bounded page of blocks by height, or from explicit block IDs.
block_connection(RawArgs, Opts) ->
    Present = fun(_Key, Value) -> Value =/= null end,
    Args = hb_maps:filter(Present, RawArgs, Opts),
    Range =
        hb_maps:filter(Present, hb_maps:get(<<"height">>, Args, #{}, Opts), Opts),
    Min = max(0, hb_maps:get(<<"min">>, Range, 0, Opts)),
    maybe
        {ok, After} ?= block_cursor(hb_maps:get(<<"after">>, Args, none, Opts)),
        Direction =
            case hb_maps:get(<<"sort">>, Args, <<"HEIGHT_DESC">>, Opts) of
                <<"HEIGHT_ASC">> -> 1;
                <<"INGESTED_AT_ASC">> -> unsupported;
                <<"INGESTED_AT_DESC">> -> unsupported;
                _ -> -1
            end,
        true ?= Direction =/= unsupported orelse
            {error, <<"Unsupported block sort.">>},
        IDs = hb_maps:get(<<"ids">>, Args, all, Opts),
        Max =
            case {IDs, hb_maps:get(<<"max">>, Range, infinity, Opts)} of
                {all, infinity} ->
                    Status =
                        hb_util:ok(query(undefined, <<"networkInfo">>, #{}, Opts)),
                    hb_maps:get(<<"height">>, Status, not_found, Opts);
                {_, Bound} -> Bound
            end,
        {From, To} =
            case {Direction, After} of
                {1, H} when is_integer(H) -> {max(Min, H + 1), Max};
                {-1, H} when is_integer(H) -> {Min, min(Max, H - 1)};
                _ -> {Min, Max}
            end,
        Limit = page_size(Args, Opts),
        Blocks = block_page(IDs, From, To, Direction, Limit + 1, Opts),
        {ok,
            #{
                <<"edges">> =>
                    [
                        #{ <<"node">> => Block,
                            <<"cursor">> => <<"height=", Height/binary>> }
                    ||
                        Block <- lists:sublist(Blocks, Limit),
                        Height <- [hb_util:bin(
                            hb_maps:get(<<"height">>, Block, not_found, Opts)
                        )]
                    ],
                <<"pageInfo">> => #{ <<"hasNextPage">> => length(Blocks) > Limit }
            }
        }
    end.

%% @doc Parse an exclusive block-height cursor.
block_cursor(After) when After =:= none; After =:= <<>> -> {ok, none};
block_cursor(After) ->
    try
        <<"height=", Bin/binary>> = After,
        Height = binary_to_integer(Bin),
        true = Height >= 0,
        {ok, Height}
    catch _:_ -> {error, <<"Invalid cursor.">>}
    end.

%% @doc Read at most the requested height window; ID filters read their own set.
block_page(all, Min, Max, Direction, Limit, Opts) ->
    Heights =
        if
            Min > Max -> [];
            Direction =:= 1 -> lists:seq(Min, min(Max, Min + Limit - 1));
            true -> lists:seq(Max, max(Min, Max - Limit + 1), -1)
        end,
    [hb_util:ok(read_block(Height, Opts)) || Height <- Heights];
block_page(IDs, Min, Max, Direction, Limit, Opts) ->
    Blocks = [hb_util:ok(read_block(ID, Opts)) || ID <- lists:usort(IDs)],
    Ordered =
        lists:keysort(1,
            [{Height, Block} || Block <- Blocks,
                Height <- [hb_maps:get(<<"height">>, Block, not_found, Opts)],
                Height >= Min, Height =< Max]
        ),
    Page = case Direction of 1 -> Ordered; -1 -> lists:reverse(Ordered) end,
    [Block || {_Height, Block} <- lists:sublist(Page, Limit)].

%% @doc Convert a block height range (`#{<<"min">> => Min, <<"max">> => Max}')
%% into weave byte offset boundaries `{StartOffset, EndOffset}'. Notably, the
%% highest offset is not the max block height. It is 'infinity', such that TXs
%% that are indexed but are not yet confirmed are included.
block_range_to_offset_range(Heights, Opts) ->
    StartOffset =
        case hb_maps:get(<<"min">>, Heights, 0, Opts) of
            null -> 0;
            0 -> 0;
            RawMin ->
                case read_block(hb_util:int(RawMin), Opts) of
                    {ok, MinBlock} ->
                        % The `weave-size` is the size at the _end_ of the block,
                        % so we must subtract the start from it to find the 
                        % starting byte of the block.
                        WeaveSize = hb_util:int(
                            hb_maps:get(<<"weave-size">>, MinBlock, 0, Opts)),
                        BlockSize = hb_util:int(
                            hb_maps:get(<<"block-size">>, MinBlock, 0, Opts)),
                        WeaveSize - BlockSize;
                    {error, not_found} -> 0
                end
        end,
    EndOffset =
        case hb_maps:get(<<"max">>, Heights, infinity, Opts) of
            null -> infinity;
            infinity -> infinity;
            RawMax ->
                case read_block(hb_util:int(RawMax), Opts) of
                    {ok, MaxBlock} ->
                        hb_util:int(
                            hb_maps:get(<<"weave-size">>, MaxBlock, 0, Opts)
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

%% @doc Read block metadata by height or ID. Tries the local block cache first;
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
                        #{ <<"path">> => <<"block">>, <<"block">> => Height,
                            <<"include-proofs">> => false },
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
                        #{ <<"path">> => <<"block">>, <<"block">> => Height,
                            <<"include-proofs">> => false },
                        Opts
                    );
                _ -> {error, not_found}
            end
    end.

%% @doc Read a block from the Arweave pseudo-path cache.
read_cached_block(Height, Opts) ->
    hb_ao:raw(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"block">>,
            <<"block">> => Height,
            <<"include-proofs">> => false,
            <<"cache-control">> => [<<"only-if-cached">>]
        },
        Opts
    ).

%% @doc Return the latest block height indexed in the Arweave pseudo-path cache.
latest_cached_block(Opts) ->
    case cached_block_heights(Opts) of
        [] -> not_found;
        Blocks -> {ok, lists:max(Blocks)}
    end.

%% @doc List block heights already available in the Arweave pseudo-path cache.
cached_block_heights(Opts) ->
    hb_util:ok(hb_ao:resolve(<<"~arweave@2.9/block-heights">>, Opts)).

%%% Index-served pages

%% @doc Select block enumeration for unfiltered queries, indexed matching for
%% predicates, or `unservable' for the cache-matching path.
index_connection(Args, Opts) ->
    case index_predicates(Args, Opts) of
        {ok, []} -> block_transaction_connection(Args, Opts);
        {ok, Predicates} -> index_connection(Predicates, Args, Opts);
        Result -> Result
    end.

%% @doc Read a page selected by indexed predicates.
index_connection(Predicates, Args, Opts) ->
    maybe
        {ok, After} ?= index_cursor(Args, Opts),
        Direction =
            case hb_maps:get(<<"sort">>, Args, <<"HEIGHT_DESC">>, Opts) of
                <<"HEIGHT_ASC">> -> asc;
                _ -> desc
            end,
        Ranges = index_ranges(Direction, Args, Opts),
        PageSize = page_size(Args, Opts),
        {ok, Read, Edges, More} ?=
            index_edges(Predicates, Ranges, After, PageSize, 0, Opts),
        ForceNextPage = force_next_page(Args, Opts),
        Page =
            #{
                <<"edges">> =>
                    case ForceNextPage andalso not More of
                        true -> force_terminal_cursor(Edges);
                        false -> Edges
                    end,
                <<"predicates">> => Predicates,
                <<"ranges">> => Ranges,
                <<"pageInfo">> =>
                    #{ <<"hasNextPage">> => More orelse ForceNextPage }
            },
        {ok,
            case After =:= none andalso not More of
                true ->
                    Cap = hb_opts:get(query_arweave_max_index_count,
                        ?DEFAULT_MAX_INDEX_COUNT, Opts),
                    Page#{ <<"count">> => hb_util:bin(min(Read, Cap)) };
                false -> Page
            end}
    end.

%% @doc Read the edges of a page from the index, with the number of matches
%% read and whether the index has more past them. The next matches take the
%% place of those whose items cannot be read, until the page has `Size'
%% edges, the index has no more, or the page has read
%% `query-arweave-max-index-count' matches.
index_edges(Predicates, Ranges, After, Size, Read, Opts) ->
    maybe
        {ok, Matches} ?=
            index_matches(Predicates, Ranges, After, Size + 1, Opts),
        Page = lists:sublist(Matches, Size),
        Edges = match_edges(Page, Opts),
        More = length(Matches) > Size,
        Total = Read + length(Page),
        Cap =
            hb_opts:get(
                query_arweave_max_index_count,
                ?DEFAULT_MAX_INDEX_COUNT,
                Opts
            ),
        Missing = min(Size - length(Edges), Cap - Total),
        case More andalso Missing > 0 of
            false -> {ok, Total, Edges, More};
            true ->
                maybe
                    {ok, AllRead, Rest, RestMore} ?=
                        index_edges(
                            Predicates,
                            Ranges,
                            maps:get(<<"member">>, lists:last(Page)),
                            Missing,
                            Total,
                            Opts
                        ),
                    {ok, AllRead, Edges ++ Rest, RestMore}
                end
        end
    end.

%% @doc The query's AND predicates, each with alternative values. Owners use
%% `committer', and recipients `field-target'. Explicit IDs, a height or bundle
%% filter are `unservable'. An empty predicate list selects block enumeration.
index_predicates(Args, Opts) ->
    Get = fun(Filter) -> hb_maps:get(Filter, Args, null, Opts) end,
    Fields =
        [
            {<<"committer">>, owner_committers(Get(<<"owners">>))},
            {<<"field-target">>, Get(<<"recipients">>)}
        ],
    maybe
        true ?= Get(<<"ids">>) =/= [] orelse unservable,
        true ?= explicit_ids(Args, Opts) =:= [] orelse unservable,
        true ?=
            Get(<<"height">>) =:= null andalso Get(<<"bundledIn">>) =:= null
                orelse unservable,
        Tags =
            case Get(<<"tags">>) of
                null -> [];
                Filters -> Filters
            end,
        Predicates = Tags ++
            [ #{ <<"name">> => Pair, <<"values">> => Values }
            || {Pair, Values} <- Fields, Values =/= null ],
        {ok, Predicates}
    end.

%% @doc Page base-layer TX IDs directly from their containing block headers.
block_transaction_connection(RawArgs, Opts) ->
    Present = fun(_Key, Value) -> Value =/= null end,
    Args = hb_maps:filter(Present, RawArgs, Opts),
    Range = case hb_opts:get(query_arweave_ignore_block_ranges, false, Opts) of
        true -> #{};
        false -> hb_maps:filter(Present, hb_maps:get(<<"block">>, Args, #{}, Opts), Opts)
    end,
    maybe
        {ok, After} ?= block_transaction_cursor(hb_maps:get(<<"after">>, Args, null, Opts)),
        {ok, Tip} ?= block_index_tip(Opts),
        Bounds = #{
            <<"min">> => max(0, hb_maps:get(<<"min">>, Range, 0, Opts)),
            <<"max">> => min(Tip, hb_maps:get(<<"max">>, Range, Tip, Opts)),
            <<"direction">> => case hb_maps:get(<<"sort">>, Args, <<"HEIGHT_DESC">>, Opts) of
                <<"HEIGHT_ASC">> -> 1;
                _ -> -1
            end
        },
        Limit = page_size(Args, Opts),
        {ok, Matches} ?= block_transactions(Bounds, After, Limit + 1, Opts),
        More = length(Matches) > Limit,
        Force = force_next_page(Args, Opts),
        {ok, #{
            <<"matches">> => lists:sublist(Matches, Limit),
            <<"block-range">> => Bounds,
            <<"terminal">> => Force andalso not More,
            <<"pageInfo">> => #{ <<"hasNextPage">> => More orelse Force }
        }}
    end.

%% @doc Find the indexed tip with one ordered seek, without listing headers.
block_index_tip(Opts) ->
    case hb_ao:resolve(<<"~arweave@2.9/blocks&direction=desc">>, Opts) of
        {ok, [Entry]} -> {ok, hb_maps:get(<<"height">>, Entry, Opts)};
        {ok, []} ->
            case hb_opts:get(query_arweave_remote_block_ranges, true, Opts) of
                false -> {ok, -1};
                true ->
                    maybe
                        {ok, Status} ?= query(undefined, <<"networkInfo">>, #{}, Opts),
                        {ok, hb_util:int(hb_maps:get(<<"height">>, Status, Opts))}
                    end
            end;
        Error -> Error
    end.

%% @doc Decode a height and TX ID, accepting the forced-page terminal marker.
block_transaction_cursor(After) when After =:= null; After =:= <<>> -> {ok, none};
block_transaction_cursor(After) ->
    try
        [<<"block=", H/binary>>, <<"tx=", ID:43/binary>> | Tail] =
            binary:split(After, <<"&">>, [global]),
        true = Tail =:= [] orelse Tail =:= [<<"remaining=0">>],
        Height = binary_to_integer(H),
        true = Height >= 0,
        <<_:32/binary>> = hb_util:native_id(ID),
        {ok, #{ <<"height">> => Height, <<"id">> => ID }}
    catch _:_ -> {error, <<"Invalid cursor.">>}
    end.

%% @doc Start at the cursor's height, clamped to the near end of the range.
block_transactions(Range = #{ <<"min">> := Min, <<"max">> := Max,
        <<"direction">> := Direction }, After, Limit, Opts) ->
    Height = case {Direction, After} of
        {1, #{ <<"height">> := H }} -> max(Min, H);
        {-1, #{ <<"height">> := H }} -> min(Max, H);
        {1, none} -> Min;
        {-1, none} -> Max
    end,
    block_transactions(Height, Range, After, Limit, Opts).

%% @doc Read enough headers for the page; reuse each header for block fields.
block_transactions(Height, #{ <<"min">> := Min, <<"max">> := Max }, _After,
        Limit, _Opts) when Height < Min; Height > Max; Limit =:= 0 -> {ok, []};
block_transactions(Height, Range = #{ <<"direction">> := Direction }, After, Limit, Opts) ->
    maybe
        {ok, Block} ?= read_block(Height, Opts),
        IDs = lists:sort(hb_maps:get(<<"txs">>, Block, [], Opts)),
        Ordered = case Direction of 1 -> IDs; -1 -> lists:reverse(IDs) end,
        Remaining = case After of
            #{ <<"height">> := Height, <<"id">> := Last } ->
                [ID || ID <- Ordered, (Direction =:= 1 andalso ID > Last)
                    orelse (Direction =:= -1 andalso ID < Last)];
            _ -> Ordered
        end,
        Matches = [#{
            <<"id">> => ID, <<"commitment-device">> => <<"tx@1.0">>,
            <<"block-result">> => {ok, Block},
            <<"cursor">> => <<"block=", (hb_util:bin(Height))/binary, "&tx=", ID/binary>>
        } || ID <- lists:sublist(Remaining, Limit)],
        {ok, Rest} ?= block_transactions(Height + Direction, Range, none,
            Limit - length(Matches), Opts),
        {ok, Matches ++ Rest}
    end.

%% @doc The match the page resumes after, from the cursor of an
%% index-served edge with its terminal marker dropped; a cursor of another
%% form gives a cached item, from which only `cached_transactions' resumes.
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
                [#{ <<"from">> => Start, <<"to">> => End + 1 }];
            {desc, open} ->
                [#{ <<"from">> => infinity }];
            {desc, {Start, infinity}} ->
                [#{ <<"from">> => infinity, <<"to">> => Start - 1 }];
            {desc, {Start, End}} ->
                [#{ <<"from">> => End, <<"to">> => Start - 1 }]
        end,
    Filter =
        case Window of
            open -> #{};
            {Low, High} -> #{ <<"block">> => Heights,
                <<"block-start">> => Low, <<"block-end">> => High }
        end,
    [ (maps:merge(Range, Filter))#{ <<"direction">> => Direction }
        || Range <- Bounds ].

%% @doc The matches of a page: the ranges read in order from the cursor,
%% which lies in the range that contains its key.
index_matches(Predicates, Ranges, After, Limit, Opts) ->
    % A cursor among the messages the weave never held resumes their range
    % alone: the second of the two an open ascending page reads.
    Ahead =
        case {After, Ranges} of
            {<<"-1", _/binary>>, [_Weave, Unmined]} -> [Unmined];
            _ -> Ranges
        end,
    locate_ranges(Predicates, Ahead, After, Limit, Opts).

%% @doc The matches of the ranges in order from the cursor, as far as the
%% page has room.
locate_ranges(_Predicates, Ranges, _After, Limit, _Opts)
        when Ranges =:= []; Limit =:= 0 ->
    {ok, []};
locate_ranges(Predicates, [Range | Rest], After, Limit, Opts) ->
    maybe
        {ok, Bounds} ?= range_cursor(Range, After, Opts),
        {ok, Matches} ?=
            locate_range(Predicates, Bounds, Limit, Opts),
        {ok, More} ?=
            locate_ranges(Predicates, Rest, none, Limit - length(Matches), Opts),
        {ok, Matches ++ More}
    end.

%% @doc A cursor cannot start a range before its inclusive near boundary.
range_cursor(Range, none, _Opts) -> {ok, Range};
range_cursor(Range = #{ <<"from">> := From, <<"direction">> := Direction },
        After, Opts) ->
    try
        {ok, #{ <<"offset">> := Offset }} = hb_ao:raw(
            <<"match@1.0">>, <<"entry">>, #{ <<"body">> => After }, #{}, Opts),
        case (Direction =:= asc andalso Offset < From) orelse
                (Direction =:= desc andalso Offset > From) of
            true -> {ok, Range};
            false ->
                {ok, (maps:remove(<<"from">>, Range))#{ <<"after">> => After }}
        end
    catch error:badarg ->
        {error, <<"Invalid cursor.">>}
    end.

%% @doc Filter boundary candidates before counting the page, refilling it
%% from the last examined member with bounded reads until full or exhausted.
locate_range(Predicates, Bounds, Limit, Opts) ->
    maybe
        {ok, Matches} ?= locate(Predicates, Bounds#{ <<"limit">> => Limit }, Opts),
        {Prepared, QueryOpts} = prepare_blocks(Matches,
            fun(Match) -> block_boundary(Match, Bounds) end, Opts),
        Accepted = [Match || Match <- Prepared, in_block_range(Match, Bounds, QueryOpts)],
        case length(Matches) =:= Limit andalso length(Accepted) < Limit of
            false -> {ok, Accepted};
            true ->
                Next = (maps:remove(<<"from">>, Bounds))#{
                    <<"after">> => maps:get(<<"member">>, lists:last(Matches)) },
                maybe
                    {ok, More} ?= locate_range(
                        Predicates, Next, Limit - length(Accepted), QueryOpts),
                    {ok, Accepted ++ More}
                end
        end
    end.

%% @doc Byte ranges identify bundled items, but L1s at shared boundaries
%% require block membership to distinguish zero-data transactions.
in_block_range(Match, #{ <<"block">> := Heights, <<"block-start">> := Start,
        <<"block-end">> := End }, Opts) ->
    Offset = maps:get(<<"offset">>, Match, undefined),
    Min = case maps:get(<<"min">>, Heights, 0) of null -> 0; Low -> Low end,
    Max = case maps:get(<<"max">>, Heights, infinity) of
        null -> infinity; High -> High end,
    case pending_offset(Offset) of
        true -> End =:= infinity;
        false when is_integer(Offset), Offset >= Start, Offset =< End ->
            case maps:get(<<"commitment-device">>, Match, <<>>) of
                <<"tx@1.0">> when Offset =:= End;
                        Offset =:= Start, Min > 0 ->
                    case hb_util:ok(match_block(Match, Opts)) of
                        null -> false;
                        Block ->
                            Height = hb_maps:get(<<"height">>, Block, -1, Opts),
                            Height >= Min andalso Height =< Max
                    end;
                _ -> Offset < End
            end;
        false -> false
    end;
in_block_range(_Match, _Range, _Opts) -> true.

%% @doc The matches of the predicates through `~match@1.0'. A node without
%% stores of the index is `unservable'; a failing store is an error, as
%% `cached_transactions' answers from different data.
locate(Predicates, Req, Opts) ->
    try hb_ao:raw(
            <<"match@1.0">>,
            #{},
            Req#{ <<"path">> => <<"locate">>, <<"predicates">> => Predicates },
            Opts
        ) of
        {error, not_found} -> unservable;
        Result -> Result
    catch error:badarg ->
        {error, <<"Invalid cursor.">>}
    end.

%% @doc The edges of the page's matches in its order, under cursors naming
%% their keys, read together. A match carrying an ID reads its cached
%% message; one without reads the item at its offset from the weave. A
%% match neither can read is dropped and reported.
match_edges(Matches, #{ <<"query-arweave-nodes">> := false }) ->
    [#{ <<"cursor">> => match_cursor(Match) } || Match <- Matches];
match_edges(Matches, Opts) ->
    Read =
        hb_pmap:parallel_map(
            [maps:remove(<<"block-result">>, Match) || Match <- Matches],
            fun(Match) -> read_match(Match, Opts) end,
            hb_opts:get(arweave_chunk_fetch_concurrency, 10, Opts)
        ),
    block_edges(lists:filtermap(
        fun({Match, {ok, Node}}) ->
                {true,
                    #{
                        <<"cursor">> => match_cursor(Match),
                        <<"node">> =>
                            hb_private:set(Node, <<"query-match">>, Match, Opts)
                    }};
            ({Match, Error}) ->
                ?event(warning,
                    {match_unreadable, {match, Match}, {error, Error}}
                ),
                false
        end,
        lists:zip(Matches, Read)
    ), Opts).

%% @doc Preserve block-page cursors, or encode a match index member.
match_cursor(#{ <<"cursor">> := Cursor }) -> Cursor;
match_cursor(#{ <<"member">> := Member }) -> <<?MEMBER_CURSOR, Member/binary>>.

%% @doc Read the message of a match. If the read gives an error other than
%% `not_found', throw the error, so that the page fails.
read_match(Match, Opts) ->
    case match_message(Match, Opts) of
        {error, not_found} -> {error, not_found};
        {error, _} = Error -> throw(Error);
        Read -> Read
    end.

%% @doc L1 metadata needs only its native header, not an expanded bundle.
%% Other messages read by ID, or decode their header at a weave offset.
match_message(#{ <<"id">> := ID, <<"commitment-device">> := <<"tx@1.0">>,
        <<"offset">> := Offset }, Opts) when is_integer(Offset), Offset >= 0 ->
    l1_message(ID, Opts);
match_message(#{ <<"id">> := ID, <<"cursor">> := <<"block=", _/binary>> }, Opts) ->
    l1_message(ID, Opts);
match_message(#{ <<"id">> := ID }, Opts) when ID =/= <<>> ->
    hb_cache:read(ID, Opts);
match_message(#{ <<"offset">> := Offset, <<"commitment-device">> := Device }, Opts) ->
    header(Offset, Device, Opts).

%% @doc Keep header-only L1 reads separate from complete-message cache reads.
l1_message(ID, Opts) ->
    case hb_cache:read(ID, hb_store:scope(Opts, local)) of
        {ok, Msg} -> {ok, Msg};
        {error, not_found} -> hb_ao:resolve(#{ <<"path">> => <<"~arweave@2.9/tx">>,
            <<"tx">> => ID, <<"exclude-data">> => true }, Opts);
        Error -> Error
    end.

%% @doc The message of the item at a weave offset, from its header alone:
%% parsed from the bytes between the offset and the end of its chunk -- one
%% fetch, and every byte a gateway serves when the chunk closes the bundle,
%% as the weave's padding follows it -- or from a longer read when the
%% header runs into the next chunk. The data is left out: neither the index
%% nor the header gives its length.
header(Offset, Device, Opts) ->
    Tail =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave@2.9">> },
            #{ <<"path">> => <<"chunk">>, <<"offset">> => Offset + 1 },
            Opts
        ),
    case header_message(Tail, Device, Opts) of
        {ok, Node} ->
            {ok, Node};
        {error, _} ->
            header_message(
                hb_store_arweave:read_chunks(Offset, ?ITEM_PROBE_LENGTH, Opts),
                Device,
                Opts
            )
    end.

%% @doc The message of the item whose header opens the read bytes.
header_message({ok, Bytes}, Device, Opts) ->
    try
        {ok, TABM} =
            hb_ao:raw(
                Device, <<"deserialize">>, #{ <<"body">> => Bytes },
                #{ <<"exclude-data">> => true }, Opts
            ),
        Msg = hb_message:convert(TABM, <<"structured@1.0">>, tabm, Opts),
        {ok, hb_private:set(Msg, <<"query-data-omitted">>, true, Opts)}
    catch _:Reason ->
        {error, {'invalid-item', Reason}}
    end;
header_message(Error, _Device, _Opts) ->
    Error.

%%% Match argument processing

%% @doc Progressively generate matches from each argument for a transaction
%% query.  The `block' range is applied as a post-filter over the candidate
%% set rather than as a set-producing index lookup.
match_args(#{ <<"ids">> := [] }, _Opts) -> [];
match_args(Args, Opts) when is_map(Args) ->
    case explicit_ids(Args, Opts) of
        [_ | _] = IDs -> IDs;
        [] ->
            match_args(
                maps:to_list(maps:with(?SUPPORTED_QUERY_ARGS, Args)),
                [],
                Opts
            )
    end.
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
        ignore -> match_args(Rest, Acc, Opts);
        {error, _} = Error -> throw(Error)
    end.

%% @doc Explicit IDs bound the work to candidate reads and predicate checks.
%% Keep loaded messages for projection; offsets are only ordering metadata.
check_explicit_ids(Ordered, Args, Opts) ->
    {ok, Predicates} = index_predicates(
        maps:with([<<"tags">>, <<"owners">>, <<"recipients">>], Args), Opts),
    case explicit_ids(Args, Opts) =:= [] orelse Predicates =:= [] of
        true -> Ordered;
        false -> lists:append(hb_pmap:parallel_map(
            Ordered,
            fun(Annotated = #{ <<"id">> := ID }) ->
                maybe
                    [Edge = #{ <<"node">> := Msg }] ?=
                        read_id(Annotated, Opts#{ <<"query-arweave-nodes">> => true }),
                    {ok, true} ?= hb_ao:raw(
                        <<"match@1.0">>, hb_message:with_commitments([ID], Msg, Opts),
                        #{ <<"path">> => <<"check">>, <<"predicates">> => Predicates },
                        Opts
                    ),
                    [Edge]
                else
                    [] -> [];
                    {ok, false} -> [];
                    Error -> throw(Error)
                end
            end,
            hb_opts:get(arweave_chunk_fetch_concurrency, 10, Opts)
        ))
    end.

%% @doc Generate a match upon `tags' in the arguments, if given.
match(_, null, _) -> ignore;
match(<<"tags">>, [], _) -> ignore;
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
    case hb_opts:get(match_index, false, Opts) =:= false orelse
            hb_opts:get(cache_read_mode, normal, Opts) =:= raw of
        true -> native_tags(Tags, Opts);
        false ->
            hb_ao:raw(
                <<"match@1.0">>, #{},
                #{ <<"path">> => <<"all">>, <<"predicates">> => Tags },
                Opts
            )
    end;
match(<<"owners">>, Owners, Opts) ->
    {ok, matching_commitments(<<"committer">>, owner_committers(Owners), Opts)};
match(<<"owner">>, Owner, Opts) ->
    Res =
        matching_commitments(<<"committer">>, hb_committer:uri(Owner), Opts),
    ?event({match_owner, Owner, Res}),
    {ok, Res};
match(<<"recipients">>, Recipients, Opts) ->
    {ok, matching_commitments(<<"target">>, Recipients, Opts)};
match(UnsupportedFilter, _, _) ->
    throw({unsupported_query_filter, UnsupportedFilter}).

%% @doc OR each tag's native cache matches, then AND the tags. Used when the
%% node disables indexed matching or requests raw cache matching.
native_tags(Tags, Opts) ->
    Results =
        [
            lists:append([
                case hb_cache:match(#{ Name => Value }, Opts) of
                    {ok, IDs} -> IDs;
                    {error, not_found} -> [];
                    {error, _} = Error -> throw(Error)
                end
            || Value <- hb_maps:get(<<"values">>, Tag, not_found, Opts) ])
        ||
            Tag <- Tags,
            Name <- [hb_maps:get(<<"name">>, Tag, not_found, Opts)]
        ],
    {ok, lists:foldl(fun hb_util:list_with/2, hd(Results), tl(Results))}.

%%% Block range post-filter

%% @doc Offset-annotate a list of IDs, returning {StartOffset, ID} pairs.
annotate_ids(IDs, Opts) ->
    case hb_store_arweave:store_from_opts(Opts) of
        no_store -> unavailable;
        StoreOpts -> annotate_offsets(lists:sort(IDs), StoreOpts, #{}, Opts)
    end.
annotate_offsets([], _StoreOpts, _Ordinals, _Opts) -> [];
annotate_offsets([ID|IDs], StoreOpts, Ordinals, Opts) ->
    {Offset, Annotated} =
        case hb_store_arweave:read_offset(StoreOpts, ID, Opts) of
            {ok, Location = #{ <<"start">> := StartOffset, <<"length">> := Length }} ->
                {
                    StartOffset,
                    #{
                        <<"id">> => ID,
                        <<"offset">> => StartOffset,
                        <<"commitment-device">> =>
                            hb_maps:get(<<"codec">>, Location, <<>>, Opts),
                        <<"length">> => Length
                    }
                };
            _ ->
                {undefined, #{ <<"id">> => ID }}
        end,
    Ordinate = maps:get(Offset, Ordinals, 0),
    Postfix =
        case is_integer(Offset) andalso Ordinate > 0 of
            true -> <<"-", (hb_util:bin(Ordinate))/binary>>;
            false -> <<>>
        end,
    WithCursor =
        Annotated#{
            <<"cursor">> => << (offset_cursor(ID, Offset))/binary, Postfix/binary >>
        },
    [WithCursor | annotate_offsets(IDs, StoreOpts,
        Ordinals#{ Offset => Ordinate + 1 }, Opts)].

offset_cursor(ID, undefined) when is_binary(ID) -> <<"ephemeral=", ID/binary>>;
offset_cursor(ID, Offset) when is_binary(ID) ->
    case pending_offset(Offset) of
        true -> <<"pending=", ID/binary>>;
        false -> <<"offset=", (hb_util:bin(Offset))/binary>>
    end.

pending_offset(infinity) -> true;
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
    Range = #{ <<"block">> => Heights, <<"block-start">> => StartOffset,
        <<"block-end">> => EndOffset },
    {Prepared, QueryOpts} = prepare_blocks(AnnotatedIDs,
        fun(Match) -> block_boundary(Match, Range) end, Opts),
    Filtered =
        lists:filter(
            fun(Match) ->
                in_block_range(Match, Range, QueryOpts) andalso
                    case Match of
                        #{ <<"offset">> := Offset, <<"length">> := Length }
                                when is_integer(Offset), is_integer(EndOffset) ->
                            Offset + Length =< EndOffset;
                        _ -> true
                    end
            end,
            Prepared
        ),
    ?event({filtered_out_of_range, length(AnnotatedIDs) - length(Filtered)}),
    Filtered.

%% @doc Convert the owners of a GraphQL query, which are Arweave addresses, to
%% the URIs of their committers.
owner_committers(null) -> null;
owner_committers(Owners) -> [hb_committer:uri(Owner) || Owner <- Owners].

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

%% @doc Clamp only cursors before the near bound, preserving exclusive IDs.
range_cursor_test() ->
    lists:foreach(
        fun({Direction, From, Before, Ahead}) ->
            Range = #{ <<"direction">> => Direction, <<"from">> => From },
            ?assertEqual({ok, Range}, range_cursor(Range, none, #{})),
            ?assertEqual({ok, Range}, range_cursor(Range, Before, #{})),
            lists:foreach(
                fun(Cursor) ->
                    ?assertEqual({ok, #{ <<"direction">> => Direction,
                        <<"after">> => Cursor }}, range_cursor(Range, Cursor, #{}))
                end,
                [hb_util:bin(From),
                    <<"00000000000000000010",
                        "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAans104@1.0">>,
                    Ahead]
            ),
            ?assertEqual({error, <<"Invalid cursor.">>},
                range_cursor(Range, <<"nonsense">>, #{}))
        end,
        [{asc, 10, <<"9">>, <<"infinity">>}, {desc, 10, <<"11">>, <<"-1">>}]
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
    Missing = hb_util:encode(crypto:hash(sha256, <<"missing">>)),
    ok = hb_store_arweave:write_offset(
        ArweaveStore, Missing, <<"tx@1.0">>, 11, 0),
    BaseArgs =
        #{
            <<"ids">> => [PendingA, Missing, NumericID],
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
            #{ <<"height">> => 1, <<"weave-size">> => 100, <<"block-size">> => 100 },
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
    % A cursor-only page reports indexed candidates even without their nodes.
    ?assertMatch(#{ <<"data">> := #{ <<"transactions">> := #{
        <<"edges">> := [#{ <<"cursor">> := <<"offset=11">> }],
        <<"pageInfo">> := #{ <<"hasNextPage">> := true }
    }}}, dev_query_graphql:test_query(hb_http_server:start_node(Opts),
        <<"query($ids: [ID!], $after: String) { transactions(ids: $ids,",
            " after: $after, first: 1) { edges { cursor }",
            " pageInfo { hasNextPage } } }">>,
        #{ <<"ids">> => [PendingA, Missing, NumericID],
            <<"after">> => FirstCursor }, Opts)).

%% @doc Block bounds and exclusive member cursors intersect in either order.
bounded_member_pages_test() ->
    Store = (hb_test_utils:test_store(hb_store_lmdb))#{
        <<"capacity">> => 1024 * 1024 * 1024, <<"list-batch-size">> => 2 },
    Opts = #{ <<"store">> => [Store], <<"priv-wallet">> => ar_wallet:new(),
        <<"match-index">> => [Store#{ <<"from-list">> => <<"~match@1.0/entries">> }] },
    lists:foreach(
        fun({N, Offset}) ->
            Msg = hb_message:commit(#{ <<"type">> => <<"Window">>,
                <<"n">> => N }, Opts),
            hb_cache:write(hb_private:set(Msg, <<"offset">>, Offset, Opts), Opts)
        end,
        lists:enumerate([9, 10, 10, 19, 20])
    ),
    {ok, BlockID} = hb_cache:write(#{ <<"height">> => 1,
        <<"weave-size">> => 20, <<"block-size">> => 10 }, Opts),
    hb_cache:link(BlockID, <<"~arweave@2.9/block/height/1">>, Opts),
    Node = hb_http_server:start_node(Opts),
    Query = <<"query($after:String,$sort:SortOrder){transactions(first:1,",
        "block:{min:1,max:1},tags:[{name:\"type\",values:[\"Window\"]}],",
        "after:$after,sort:$sort){edges{cursor}pageInfo{hasNextPage}}}">>,
    Page = fun(Sort, After) -> hb_util:deep_get(<<"data/transactions">>,
        dev_query_graphql:test_query(Node, Query,
            #{ <<"sort">> => Sort, <<"after">> => After }, Opts), #{}, Opts) end,
    Pages = fun Pages(Sort, After) ->
        #{ <<"edges">> := Edges, <<"pageInfo">> := #{ <<"hasNextPage">> := More }} =
            Page(Sort, After),
        case More of
            false -> Edges;
            true -> Edges ++ Pages(Sort, maps:get(<<"cursor">>, lists:last(Edges)))
        end
    end,
    Asc = Pages(<<"HEIGHT_ASC">>, null),
    ?assertEqual(3, length(lists:usort(Asc))),
    ?assertEqual(Asc, Pages(<<"HEIGHT_ASC">>, <<"member=9">>)),
    ?assertEqual(lists:reverse(Asc), Pages(<<"HEIGHT_DESC">>, <<"member=21">>)),
    ?assertEqual([], Pages(<<"HEIGHT_ASC">>, <<"member=21">>)),
    ?assertEqual([], Pages(<<"HEIGHT_DESC">>, <<"member=9">>)).

%% @doc Signed messages the weave never held page out last in either order,
%% by cursor, from a node's own stores.
unmined_pages_test() ->
    Opts = #{
        <<"store">> => [hb_test_utils:test_store()],
        <<"priv-wallet">> => ar_wallet:new(),
        <<"query-arweave-max-index-count">> => 2
    },
    Node = hb_http_server:start_node(Opts),
    lists:foreach(
        fun(N) ->
            {ok, _} =
                hb_cache:write(
                    hb_message:commit(
                        #{ <<"type">> => <<"Unmined">>, <<"n">> => hb_util:bin(N) },
                        Opts
                    ),
                    Opts
                )
        end,
        lists:seq(1, 3)
    ),
    Query =
        <<"""
            query($after: String, $sort: SortOrder, $first: Int = 1) {
                transactions(
                    tags: [{ name: "type", values: ["Unmined"] }],
                    first: $first,
                    after: $after,
                    sort: $sort
                ) {
                    count
                    pageInfo { hasNextPage }
                    edges { cursor node { id } }
                }
            }
        """>>,
    Pages =
        fun Pages(Sort, After, Acc) ->
            #{
                <<"count">> := <<"2">>,
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
    ?assertEqual(lists:reverse(Descending), Pages(<<"HEIGHT_ASC">>, null, [])),
    % The node cannot read this item; a page takes the next match in its place.
    {ok, _} =
        hb_cache:write(
            hb_private:set(
                hb_message:commit(#{ <<"type">> => <<"Unmined">> }, Opts),
                <<"offset">>,
                10,
                Opts
            ),
            Opts#{
                <<"store">> => [hb_test_utils:test_store()],
                <<"match-index">> => maps:get(<<"store">>, Opts)
            }
        ),
    ?assertEqual(Descending, Pages(<<"HEIGHT_DESC">>, null, [])),
    ?assertMatch(
        #{ <<"data">> := #{ <<"transactions">> := #{
            <<"count">> := <<"2">>,
            <<"pageInfo">> := #{ <<"hasNextPage">> := false }
        }}},
        dev_query_graphql:test_query(Node, Query, #{ <<"first">> => 10 }, Opts)
    ),
    #{ <<"errors">> := Errors } =
        dev_query_graphql:test_query(
            Node, Query, #{ <<"after">> => <<"member=nonsense">> }, Opts
        ),
    ?assertMatch(
        [_ | _],
        [ Error || #{ <<"message">> := Error } <- Errors,
            binary:match(Error, <<"Invalid cursor.">>) =/= nomatch ]
    ).

%% @doc A page served from the published index on Arweave, its items
%% read from the weave: the twenty-four items of `action=Battle.Begin'
%% page out in each order under cursors with their keys, every item
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
                        <<"from-key">> =>
                            <<"~match@1.0/member", Sizes/binary,
                                "/set&commitment-device=ans104@1.0">>,
                        <<"from-list">> =>
                            <<"~match@1.0/members", Sizes/binary,
                                "&commitment-device=ans104@1.0">>
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
                    edges { cursor node { id data { size } tags { name value } } }
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
            fun(#{ <<"node">> := #{ <<"tags">> := Tags,
                    <<"data">> := #{ <<"size">> := null } } }) ->
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
