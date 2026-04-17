%%% @doc A `~copycat@1.0' engine that fetches block data from an Arweave node for
%%% replication. This engine works in _reverse_ chronological order by default.
%%% If `to' is omitted, it keeps moving downward from `from' until it reaches a
%%% block that is already indexed at the requested depth (checked via block
%%% markers first, then legacy per-TX fallback for pre-marker indexes). If `to'
%%% is provided, every block in the range is processed.
-module(dev_copycat_arweave).
-device_libraries([lib_arweave_common]).
-export([arweave/3]).
-export([set_depth_recursion_cap/2, get_depth_recursion_cap/1]).
-include_lib("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(ARWEAVE_DEVICE, <<"~arweave@2.9">>).
-define(CUTOVER_KEY, <<"block/marker-cutover-height">>).
-define(DEPTH_SENTINEL, 99999).
% By default we'll index blocks to depth 2 which is:
% - depth 1: L1 TXs
% - depth 2: L2 bundles and dataitems
% Note: this means that the children of L2 bundles are not indexed at
% depth 2.
-define(DEFAULT_BLOCK_DEPTH, 2).
-define(DEFAULT_COPYCAT_MEMORY_BUDGET, 6 * 1024 * 1024 * 1024).

% GET /~cron@1.0/once&cron-path=~copycat@1.0/arweave

%% @doc Fetch blocks from an Arweave node between a given range, or from the
%% latest known block towards the Genesis block. If no range is provided, we
%% fetch blocks from the latest known block towards the Genesis block.
arweave(_Base, Request, Opts) ->
    case hb_maps:get(<<"mode">>, Request, <<"write">>, Opts) of
        <<"mempool">> ->
            index_mempool(Request, Opts);
        <<"write">> ->
            case hb_maps:find(<<"id">>, Request, Opts) of
                {ok, TXID} ->
                    case process_l1_request(TXID, Request, Opts) of
                        {ok, Stats} when is_map(Stats) ->
                            ?event(
                                copycat_short,
                                {arweave_tx_indexed,
                                    {id, {explicit, TXID}},
                                    {items_indexed, maps:get(items_count, Stats, 0)},
                                    {bundle_txs, maps:get(bundle_count, Stats, 0)},
                                    {skipped_txs, maps:get(skipped_count, Stats, 0)}
                                }
                            ),
                            {ok, Stats#{
                                <<"body">> => maps:get(items_count, Stats, 0)
                            }};
                        _ -> 
                            {ok, #{
                                items_count => 0,
                                bundle_count => 0,
                                skipped_count => 0,
                                <<"body">> => 0
                            }}                         
                    end;
                error ->
                    case parse_range(Request, Opts) of
                        {error, unavailable} ->
                            {error, unavailable};
                        {ok, {From, To}} ->
                            TargetDepth = request_depth(
                                Request, ?DEFAULT_BLOCK_DEPTH, Opts),
                            ?event(copycat_short,
                                {indexing_blocks,
                                    {from, From}, {to, To},
                                    {depth, TargetDepth}}
                            ),
                            fetch_blocks(From, To, TargetDepth, Opts)
                    end
            end;
        <<"list">> ->
            case parse_range(Request, Opts) of
                {error, unavailable} -> {error, unavailable};
                {ok, {From, To}} -> list_index(From, To, Opts)
            end;
        <<"inventory">> ->
            case parse_range(Request, Opts) of
                {error, unavailable} -> {error, unavailable};
                {ok, {From, To}} -> inventory_index(From, To, Opts)
            end;
        Mode ->
            {error, <<"Unsupported mode `", (hb_util:bin(Mode))/binary,
                "`. Supported modes are: write, list, inventory">>}
    end.
%% @doc Set bundles descendant recursion cap, avoids recursion
%% in very nested bundles (very rare).
set_depth_recursion_cap(Cap, Opts) when is_integer(Cap), Cap > 0 ->
    Opts#{copycat_depth_recursion_cap => Cap}.
%% @doc Get the set depth recursion cap from hb_opts.
get_depth_recursion_cap(Opts) ->
    hb_opts:get(copycat_depth_recursion_cap, undefined, Opts).

%% @doc Return the effective per-TX memory cap, clamped to the global budget.
%% Lazily initializes the budget pool on first call.
effective_memory_cap(Opts) ->
    Budget = hb_opts:get(
        copycat_memory_budget, ?DEFAULT_COPYCAT_MEMORY_BUDGET, Opts),
    hb_copycat_budget:ensure_started(Budget),
    hb_copycat_budget:get_budget().

%% @doc Return the store path for a block completion marker.
block_indexed_path(Height) ->
    <<"block/", (hb_util:bin(Height))/binary, "/depth">>.

%% @doc Return the store path for a per-block item index at a given depth.
block_items_path(Height, Depth) ->
    <<"block/", (hb_util:bin(Height))/binary,
        "/items/", (hb_util:bin(Depth))/binary>>.

%% @doc Return the store path for a parent index entry.
parent_path(ItemID) when byte_size(ItemID) =:= 32 ->
    <<"parent/", ItemID/binary>>.

%% @doc Encode a parent entry for storage.
encode_parent_entry(Height, block) when is_integer(Height) ->
    <<0, Height:64/big-unsigned>>;
encode_parent_entry(ParentID, bundle) when byte_size(ParentID) =:= 32 ->
    <<1, ParentID:32/binary>>.

%% @doc Write a parent entry for an item to the index store.
write_parent(ItemID, ParentData, Type, Store) ->
    Entry = encode_parent_entry(ParentData, Type),
    hb_store:write(Store, parent_path(ItemID), Entry).

%% @doc Encode a list of 32-byte raw IDs into a single binary.
encode_item_ids(IDs) ->
    << <<ID:32/binary>> || ID <- IDs >>.

%% @doc Decode a binary of concatenated 32-byte IDs into a list.
%% Rejects binaries whose size is not a multiple of 32.
decode_item_ids(<<>>) -> [];
decode_item_ids(Bin) when byte_size(Bin) rem 32 =/= 0 ->
    {error, invalid_item_ids_binary};
decode_item_ids(Bin) ->
    decode_item_ids_acc(Bin, []).

decode_item_ids_acc(<<>>, Acc) -> lists:reverse(Acc);
decode_item_ids_acc(<<ID:32/binary, Rest/binary>>, Acc) ->
    decode_item_ids_acc(Rest, [ID | Acc]).

%% @doc Shift all depth keys in an item ID map by Offset.
shift_item_ids(Map, Offset) ->
    maps:fold(
        fun(Depth, IDs, Acc) -> Acc#{Depth + Offset => IDs} end,
        #{},
        Map
    ).

%% @doc Merge a list of depth→ID-list maps in one pass per depth key.
merge_all_item_ids(Maps) ->
    AllKeys = lists:usort(lists:flatmap(fun maps:keys/1, Maps)),
    maps:from_list([
        {K, lists:append([maps:get(K, M, []) || M <- Maps])}
    || K <- AllKeys]).

%% @doc Merge two depth→ID-list maps by concatenating lists at each depth.
merge_item_ids(A, B) ->
    maps:fold(
        fun(Depth, IDs, Acc) ->
            Existing = maps:get(Depth, Acc, []),
            Acc#{Depth => Existing ++ IDs}
        end,
        A,
        B
    ).

%% @doc Read the stored marker depth for a block, or undefined if none.
read_block_marker_depth(Height, Opts) ->
    case hb_store_arweave:store_from_opts(Opts) of
        no_store -> undefined;
        #{ <<"index-store">> := Store } ->
            case hb_store:read(Store, block_indexed_path(Height), Opts) of
                {ok, Bin} ->
                    try binary_to_integer(Bin)
                    catch _:_ -> undefined
                    end;
                {error, not_found} -> undefined
            end
    end.

%% @doc Check if a block has been indexed at the given depth or deeper.
is_block_indexed(undefined, _TargetDepth, _Opts) ->
    false;
is_block_indexed(Height, TargetDepth, Opts) ->
    case read_block_marker_depth(Height, Opts) of
        undefined -> false;
        StoredDepth -> StoredDepth >= TargetDepth
    end.

%% @doc Write per-depth item ID lists for a block.
%% Writes an entry for every depth from 1 through AchievedDepth (empty if
%% no items at that level), plus any partial depths beyond AchievedDepth
%% that were collected during indexing.
write_block_item_ids(Height, AchievedDepth, ItemIDs, Opts) ->
    Store = get_index_store(Opts),
    MaxStoredDepth = case maps:keys(ItemIDs) of
        [] -> AchievedDepth;
        Keys -> max(AchievedDepth, lists:max(Keys))
    end,
    Results = lists:map(
        fun(D) ->
            IDs = maps:get(D, ItemIDs, []),
            Bin = encode_item_ids(IDs),
            hb_store:write(
                Store,
                block_items_path(Height, D),
                Bin
            )
        end,
        lists:seq(1, MaxStoredDepth)
    ),
    case lists:all(fun(R) -> R =:= ok end, Results) of
        true -> ok;
        false ->
            ?event(copycat_short,
                {block_item_ids_write_failed,
                    {height, Height}}),
            {error, item_ids_write_failed}
    end.

%% @doc Write a block completion marker with the achieved depth.
mark_block_indexed(Height, Depth, Opts) ->
    Store = get_index_store(Opts),
    hb_store:write(
        Store,
        block_indexed_path(Height),
        integer_to_binary(Depth)
    ).

%% @doc Read the persisted cutover height from the index store.
read_cutover_height(Opts) ->
    Store = get_index_store(Opts),
    case hb_store:read(Store, ?CUTOVER_KEY, Opts) of
        {ok, Bin} -> hb_util:int(Bin);
        {error, not_found} -> undefined
    end.

%% @doc Write the cutover height if not already set.
ensure_cutover_height(Height, Opts) ->
    case read_cutover_height(Opts) of
        undefined ->
            Store = get_index_store(Opts),
            hb_store:write(Store, ?CUTOVER_KEY, hb_util:bin(Height)),
            ?event(copycat_short, {marker_cutover_initialized, {height, Height}});
        _ -> ok
    end.

%% @doc Normalize an owner address into the native ID form used for comparisons.
normalize_owner_id(Addr) ->
    hb_util:native_id(hb_util:bin(Addr)).

%% @doc Adds an address to the owners aliases cache in Opts, mapping
%% Alias -> native address for fast lookup and once per address computation.
add_owner_alias(Addr, Alias, Opts) when is_binary(Alias) -> 
    ExistingAliases = hb_opts:get(owner_aliases, #{}, Opts),
    Opts#{ owner_aliases => ExistingAliases#{ Alias => normalize_owner_id(Addr) }};
add_owner_alias(_Addr, Alias, _Opts) ->
    throw({invalid_owner_alias, Alias}).

%% @doc Retrieve the address of a given alias.
resolve_owner_alias(Alias, Opts) when is_binary(Alias) ->
    Aliases = hb_opts:get(owner_aliases, #{}, Opts),
    case hb_maps:find(Alias, Aliases) of
        {ok, Addr} -> {ok, Addr};
        error -> {error, {owner_alias_not_found, Alias}}
    end;
resolve_owner_alias(Alias, _Opts) ->
    {error, {invalid_owner_alias, Alias}}.
%% @doc Parse include/exclude owner filters from the request.
%% Supports direct owner values and owner aliases.
parse_owner_filter(Request, Opts) ->
    maybe
        {ok, IncludeOwner} ?=
            resolve_owner_filter_value(
                <<"include-owner">>,
                <<"include-owner-alias">>,
                Request,
                Opts
            ),
        {ok, ExcludeOwner} ?=
            resolve_owner_filter_value(
                <<"exclude-owner">>,
                <<"exclude-owner-alias">>,
                Request,
                Opts
            ),
        {ok, #{
            include_owner => IncludeOwner,
            exclude_owner => ExcludeOwner
        }}
    else
        {error, _} = Error ->
            Error
    end.
%% @doc Resolve one owner filter value from either a direct owner param or
%% a comma-separated owner alias param. Alias takes precedence.
resolve_owner_filter_value(OwnerKey, AliasKey, Request, Opts) ->
    case hb_maps:find(AliasKey, Request, Opts) of
        {ok, Alias} ->
            resolve_owner_aliases(Alias, Opts);
        error ->
            case hb_maps:find(OwnerKey, Request, Opts) of
                {ok, Owner} ->
                    {ok, normalize_owner_id(Owner)};
                error ->
                    {ok, undefined}
            end
    end.
%% @doc Resolve one or more comma-separated owner aliases into normalized owner IDs.
resolve_owner_aliases(Alias, Opts) ->
    case
        lists:filter(
            fun(Part) -> byte_size(Part) > 0 end,
            binary:split(hb_util:bin(Alias), <<",">>, [global])
        )
    of
        [SingleAlias] ->
            case resolve_owner_alias(SingleAlias, Opts) of
                {ok, Addr} -> {ok, normalize_owner_id(Addr)};
                {error, _} = Error -> Error
            end;
        Aliases ->
            resolve_owner_aliases(Aliases, Opts, [])
    end.
%% @doc Resolve a list of owner aliases into normalized owner IDs.
resolve_owner_aliases([], _Opts, Acc) ->
    {ok, lists:reverse(Acc)};
resolve_owner_aliases([Alias | Rest], Opts, Acc) ->
    case resolve_owner_alias(Alias, Opts) of
        {ok, Addr} ->
            resolve_owner_aliases(Rest, Opts, [normalize_owner_id(Addr) | Acc]);
        {error, _} = Error ->
            Error
    end.
%% @doc Parse an L1 tag filter from `Name:Value` form.
parse_tag_filter(Key, Request, Opts) ->
    case hb_maps:find(Key, Request, Opts) of
        {ok, Tag} ->
            case binary:split(hb_util:bin(Tag), <<":">>, [global]) of
                [Name, Value]
                        when byte_size(Name) > 0 andalso byte_size(Value) > 0 ->
                    {ok, #{name => Name, value => Value}};
                _ ->
                    {error, invalid_tag_filter}
            end;
        error ->
            {ok, undefined}
    end.
%% @doc Process the `id=...` copycat path for an already indexed L1 TX.
%% applies L1-level owner/tag filters on the lightweight TX header first, then,
%% if the TX passes and is a bundle, loads the full L1 payload once and indexes
%% descendants in-memory up to the requested safe depth (defaults to full recursion 
%% till the set copycat_depth_recursion_cap).
process_l1_request(TXID, Request, Opts) ->
    Depth = request_depth(Request, <<"safe_max">>, Opts),
    QueryL1Offset =
        hb_util:bool(
            hb_maps:get(<<"query-l1-offset">>, Request, false, Opts)
        ),
    observe_copycat_l1_stage(
        <<"l1_request_total">>,
        fun() ->
            try
                maybe
                    {ok, OwnerFilters} ?= parse_owner_filter(Request, Opts),
                    {ok, IncludeTag} ?= parse_tag_filter(<<"include-tag">>, Request, Opts),
                    {ok, ExcludeTag} ?= parse_tag_filter(<<"exclude-tag">>, Request, Opts),
                    {ok,
                        maybe_process_l1_tx(
                            TXID,
                            OwnerFilters#{
                                include_tag => IncludeTag,
                                exclude_tag => ExcludeTag
                            },
                            Depth,
                            QueryL1Offset,
                            Opts
                        )}
                else
                    {error, _} = Error ->
                        Error
                end
            catch
                _:Reason:Stacktrace ->
                    ?event(copycat_short,
                        {error,
                            {reason, Reason},
                            {stacktrace, Stacktrace}}),
                    {error, Reason}
            end
        end
    ).
%% @doc Parse the requested recursion depth and clamp it to the configured
%% safe cap. Depth is relative so depth 1 is always one level below the
%% root specified in the request (either a block or an L1 TX ID).
%% 
%% `safe_max` resolves to the current copycat depth recursion cap.
request_depth(Request, Default, Opts) ->
    MaxRecursionCap = get_depth_recursion_cap(Opts),
    RequestedDepth =
        case hb_maps:get(<<"depth">>, Request, Default, Opts) of
            <<"safe_max">> -> MaxRecursionCap;
            Value -> hb_util:int(Value)
        end,
    erlang:min(
        MaxRecursionCap,
        erlang:max(1, RequestedDepth)
    ).
%% @doc Return the first matching L1 filter reason for a TX header, or `pass`.
l1_filter_reason(TX, Filters) ->
    IncludeOwner = maps:get(include_owner, Filters, undefined),
    ExcludeOwner = maps:get(exclude_owner, Filters, undefined),
    IncludeTag = maps:get(include_tag, Filters, undefined),
    ExcludeTag = maps:get(exclude_tag, Filters, undefined),
    Owner = ar_tx:get_owner_address(TX),
    maybe
        pass ?= maybe_include_owner(Owner, IncludeOwner),
        pass ?= maybe_exclude_owner(Owner, ExcludeOwner),
        pass ?= maybe_include_tag(TX, IncludeTag),
        pass ?= maybe_exclude_tag(TX, ExcludeTag),
        pass
    else
        Reason -> Reason
    end.
%% @doc Match an owner against an undefined, single-owner, or multi-owner filter.
owner_matches_filter(_Owner, undefined) ->
    false;
owner_matches_filter(Owner, Owners) when is_list(Owners) ->
    lists:member(Owner, Owners);
owner_matches_filter(Owner, FilterOwner) ->
    Owner =:= FilterOwner.

maybe_include_owner(_Owner, undefined) ->
    pass;
maybe_include_owner(Owner, IncludeOwner) ->
    case owner_matches_filter(Owner, IncludeOwner) of
        true -> pass;
        false -> include_owner_mismatch
    end.

maybe_exclude_owner(_Owner, undefined) ->
    pass;
maybe_exclude_owner(Owner, ExcludeOwner) ->
    case owner_matches_filter(Owner, ExcludeOwner) of
        true -> exclude_owner_match;
        false -> pass
    end.

maybe_include_tag(_TX, undefined) ->
    pass;
maybe_include_tag(TX, IncludeTag) ->
    case has_tag_pair(TX, IncludeTag) of
        true -> pass;
        false -> include_tag_mismatch
    end.

maybe_exclude_tag(_TX, undefined) ->
    pass;
maybe_exclude_tag(TX, ExcludeTag) ->
    case has_tag_pair(TX, ExcludeTag) of
        true -> exclude_tag_match;
        false -> pass
    end.

has_tag_pair(#tx{tags = Tags}, #{name := Name, value := Value}) ->
    TagValue = dev_arweave_common:tagfind(Name, Tags, not_found),
    case TagValue of
        not_found ->
            false;
        _ ->
            LowerTagValue = hb_util:to_lower(TagValue),
            LowerValue = hb_util:to_lower(Value),
            case LowerTagValue of
                LowerValue -> true;
                _ -> false
            end
    end;
has_tag_pair(_, _) ->
    false.
%% @doc Parse the range from the request.
parse_range(Request, Opts) ->
    maybe
        {ok, From} ?=
            case hb_maps:find(<<"from">>, Request, Opts) of
                {ok, FromHeight} -> normalize_height(FromHeight, Opts);
                error -> latest_height(Opts)
            end,
        {ok, To} ?=
            case hb_maps:find(<<"to">>, Request, Opts) of
                {ok, ToHeight} -> normalize_height(ToHeight, Opts);
                error -> {ok, undefined}
            end,
        case From < 0 orelse (is_integer(To) andalso To < 0) of
            true ->
                ?event(copycat_short,
                    {height_resolved_negative,
                        {from, From}, {to, To}}),
                {error, unavailable};
            false ->
                {ok, {From, To}}
        end
    else
        {error, Reason} ->
            ?event(copycat_short,
                {latest_height_failed, {reason, Reason}}),
            {error, unavailable}
    end.

normalize_height(Height, Opts) ->
    RequestedHeight = hb_util:int(Height),
    case RequestedHeight < 0 of
        true ->
            case latest_height(Opts) of
                {ok, Tip} -> {ok, Tip + RequestedHeight};
                {error, _} = Err -> Err
            end;
        false ->
            {ok, RequestedHeight}
    end.

latest_height(Opts) ->
    case hb_ao:resolve(
        <<?ARWEAVE_DEVICE/binary, "/current/height">>,
        Opts
    ) of
        {ok, ResolvedHeight} -> {ok, hb_util:int(ResolvedHeight)};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Check if a transaction ID is indexed in the arweave index store.
is_tx_indexed(TXID, Opts) ->
    Store = get_index_store(Opts),
    case hb_store:read(Store, hb_store_arweave_offset:path(TXID), Opts) of
        {ok, _} -> true;
        {error, not_found} -> false
    end.

%% @doc List indexed blocks and transactions in the given range.
%% Returns JSON with block heights as keys, each containing indexed and not-indexed lists.
list_index(From, undefined, Opts) ->
    list_index(From, 0, Opts);
list_index(From, To, _Opts) when From < To ->
    {ok, #{
        <<"content-type">> => <<"application/json">>,
        <<"body">> => hb_json:encode(#{})
    }};
list_index(From, To, Opts) ->
    Result = list_index_blocks(From, To, Opts, #{}),
    JSON = hb_json:encode(Result),
    {ok, #{
        <<"content-type">> => <<"application/json">>,
        <<"body">> => JSON
    }}.

%% @doc Iterate through blocks and check index status for each transaction.
list_index_blocks(Current, To, _Opts, Acc) when Current < To ->
    Acc;
list_index_blocks(Current, To, Opts, Acc) ->
    case fetch_block_header(Current, Opts) of
        {ok, Block} ->
            TXIDs = hb_maps:get(<<"txs">>, Block, [], Opts),
            case TXIDs of
                [] ->
                    list_index_blocks(Current - 1, To, Opts, Acc);
                _ ->
                    {IndexedTXs, _NotIndexedTXs} = classify_txs(TXIDs, Opts),
                    case IndexedTXs of
                        [] ->
                            % Do not include blocks with no locally indexed TXs.
                            list_index_blocks(Current - 1, To, Opts, Acc);
                        _ ->
                            BlockKey = hb_util:bin(Current),
                            BlockInfo = assemble_block_info(
                                Current, Block, Opts),
                            WithItems = case maps:get(
                                <<"depth">>, BlockInfo, undefined)
                            of
                                undefined -> BlockInfo;
                                _ ->
                                    BlockInfo#{
                                        <<"items">> =>
                                            read_block_item_counts(
                                                Current, Opts)}
                            end,
                            NewAcc = Acc#{BlockKey => WithItems},
                            list_index_blocks(Current - 1, To, Opts, NewAcc)
                    end
            end;
        {error, _} ->
            list_index_blocks(Current - 1, To, Opts, Acc)
    end.

%% @doc Build base block info with indexed/not-indexed TXs and optional depth.
assemble_block_info(Height, Block, Opts) ->
    TXIDs = hb_maps:get(<<"txs">>, Block, [], Opts),
    {IndexedTXs, NotIndexedTXs} = classify_txs(TXIDs, Opts),
    Base = #{
        <<"indexed">> => IndexedTXs,
        <<"not-indexed">> => NotIndexedTXs
    },
    case read_block_marker_depth(Height, Opts) of
        undefined -> Base;
        Depth -> Base#{<<"depth">> => Depth}
    end.

%% @doc Probe item entries upward from depth 1, applying TransformFun to each.
probe_block_items(Height, Opts, TransformFun) ->
    case hb_store_arweave:store_from_opts(Opts) of
        no_store -> #{};
        #{ <<"index-store">> := Store } ->
            probe_block_items(Height, Store, 1, #{}, TransformFun, Opts)
    end.

probe_block_items(Height, Store, Depth, Acc, TransformFun, Opts) ->
    case hb_store:read(Store, block_items_path(Height, Depth), Opts) of
        {ok, Bin} ->
            Key = hb_util:bin(Depth),
            probe_block_items(
                Height, Store, Depth + 1,
                Acc#{Key => TransformFun(Bin)}, TransformFun, Opts);
        {error, not_found} ->
            Acc
    end.

count_ids(Bin) when byte_size(Bin) rem 32 =:= 0 ->
    byte_size(Bin) div 32;
count_ids(_) -> <<"corrupt">>.

decode_and_encode_ids(Bin) ->
    case decode_item_ids(Bin) of
        {error, _} -> <<"corrupt">>;
        List -> [hb_util:encode(ID) || ID <- List]
    end.

read_block_item_counts(Height, Opts) ->
    probe_block_items(Height, Opts, fun count_ids/1).

read_block_item_ids(Height, Opts) ->
    probe_block_items(Height, Opts, fun decode_and_encode_ids/1).

%% @doc mode=inventory: return per-depth item ID lists from the local index store.
%% Supports range queries. The inventory read itself is local-only (no network).
%% Note: range parsing may call latest_height/1 if from/to are omitted or negative.
inventory_index(From, undefined, Opts) ->
    inventory_index(From, 0, Opts);
inventory_index(From, To, _Opts) when From < To ->
    {ok, #{
        <<"content-type">> => <<"application/json">>,
        <<"body">> => hb_json:encode(#{})
    }};
inventory_index(From, To, Opts) ->
    Result = inventory_local(From, To, Opts, #{}),
    JSON = hb_json:encode(Result),
    {ok, #{
        <<"content-type">> => <<"application/json">>,
        <<"body">> => JSON
    }}.

inventory_local(Current, To, _Opts, Acc) when Current < To -> Acc;
inventory_local(Current, To, Opts, Acc) ->
    case read_block_marker_depth(Current, Opts) of
        undefined ->
            inventory_local(Current - 1, To, Opts, Acc);
        Depth ->
            ItemIDs = read_block_item_ids(Current, Opts),
            BlockKey = hb_util:bin(Current),
            BlockInfo = #{<<"depth">> => Depth, <<"items">> => ItemIDs},
            inventory_local(Current - 1, To, Opts,
                Acc#{BlockKey => BlockInfo})
    end.

fetch_block_header(Height, Opts) ->
    ?event(debug_copycat, {fetching_block, Height}),
    observe_event(<<"block_header">>, fun() ->
        hb_ao:resolve(
            <<
                ?ARWEAVE_DEVICE/binary,
                "/block=",
                (hb_util:bin(Height))/binary
            >>,
            Opts
        )
    end).

%% @doc Classify transactions as indexed or not-indexed.
classify_txs(TXIDs, Opts) ->
    lists:foldl(
        fun(TXID, {IndexedAcc, NotIndexedAcc}) ->
            case is_tx_indexed(TXID, Opts) of
                true -> {[TXID | IndexedAcc], NotIndexedAcc};
                false -> {IndexedAcc, [TXID | NotIndexedAcc]}
            end
        end,
        {[], []},
        TXIDs
    ).

%% @doc Fetch blocks from an Arweave node while moving downward from `Current'.
%% If `To' is provided, every block in [`To', `Current'] is processed. If `To'
%% is omitted, stop at the first block already indexed at the requested depth
%% (via block markers above cutover, or legacy per-TX check below cutover).
fetch_blocks(Current, To, TargetDepth, _Opts)
        when is_integer(To), Current < To ->
    ?event(copycat_short,
        {arweave_block_indexing_completed,
            {reached_target, To},
            {target_depth, TargetDepth}
        }
    ),
    {ok, To};
fetch_blocks(Current, undefined, _TargetDepth, _Opts) when Current < 0 ->
    {ok, 0};
fetch_blocks(Current, undefined, TargetDepth, Opts) ->
    BlockWorkers = block_workers(Opts),
    fetch_blocks_open_ended(Current, TargetDepth, BlockWorkers, Opts);
fetch_blocks(Current, To, TargetDepth, Opts) ->
    BlockWorkers = block_workers(Opts),
    fetch_blocks_ranged(Current, To, TargetDepth, BlockWorkers, Opts).

block_workers(Opts) ->
    max(1, hb_opts:get(arweave_block_workers, 3, Opts)).

%% @doc Process a known range of blocks in parallel batches.
fetch_blocks_ranged(Current, To, TargetDepth, _Workers, _Opts)
        when Current < To ->
    ?event(copycat_short,
        {arweave_block_indexing_completed,
            {reached_target, To},
            {target_depth, TargetDepth}
        }
    ),
    {ok, To};
fetch_blocks_ranged(Current, To, TargetDepth, Workers, Opts) ->
    BatchEnd = max(To, Current - Workers + 1),
    Heights = lists:seq(Current, BatchEnd, -1),
    hb_pmap:parallel_map(
        Heights,
        fun(H) ->
            case is_block_indexed(H, TargetDepth, Opts) of
                true -> ok;
                false ->
                    observe_event(<<"block_indexed">>, fun() ->
                        fetch_and_process_block(H, To, TargetDepth, Opts)
                    end)
            end
        end,
        Workers
    ),
    fetch_blocks_ranged(BatchEnd - 1, To, TargetDepth, Workers, Opts).

%% @doc Process blocks until an already-indexed block is found.
%% Fetches headers in parallel, stops at the first indexed block,
%% then processes the unindexed prefix in parallel.
fetch_blocks_open_ended(Current, _TargetDepth, _Workers, _Opts)
        when Current < 0 ->
    {ok, 0};
fetch_blocks_open_ended(Current, TargetDepth, Workers, Opts) ->
    BatchEnd = max(0, Current - Workers + 1),
    Heights = lists:seq(Current, BatchEnd, -1),
    HeaderResults = hb_pmap:parallel_map(
        Heights,
        fun(H) -> {H, fetch_block_header(H, Opts)} end,
        Workers
    ),
    case find_indexed_prefix(HeaderResults, TargetDepth, Opts) of
        {all_unindexed, ToProcess} ->
            process_prefetched_blocks(
                ToProcess, TargetDepth, Workers, Opts),
            fetch_blocks_open_ended(
                BatchEnd - 1, TargetDepth, Workers, Opts);
        {stop_at, StopHeight, ToProcess} ->
            process_prefetched_blocks(
                ToProcess, TargetDepth, Workers, Opts),
            ?event(copycat_short,
                {arweave_block_indexing_completed,
                    {stop_at_indexed_block, StopHeight}
                }
            ),
            {ok, StopHeight}
    end.

%% @doc Walk header results in order, return the unindexed prefix and
%% either the stop height or all_unindexed.
find_indexed_prefix(HeaderResults, TargetDepth, Opts) ->
    find_indexed_prefix(HeaderResults, TargetDepth, Opts, []).

find_indexed_prefix([], _TargetDepth, _Opts, Acc) ->
    {all_unindexed, lists:reverse(Acc)};
find_indexed_prefix([{H, BlockRes} | Rest], TargetDepth, Opts, Acc) ->
    case is_already_indexed(BlockRes, TargetDepth, Opts) of
        true ->
            {stop_at, H, lists:reverse(Acc)};
        false ->
            find_indexed_prefix(
                Rest, TargetDepth, Opts, [{H, BlockRes} | Acc])
    end.

%% @doc Process a list of {Height, BlockRes} tuples in parallel.
process_prefetched_blocks(Blocks, TargetDepth, Workers, Opts) ->
    hb_pmap:parallel_map(
        Blocks,
        fun({H, BlockRes}) ->
            observe_event(<<"block_indexed">>, fun() ->
                process_block(BlockRes, H, undefined, TargetDepth, Opts)
            end)
        end,
        Workers
    ).

%% @doc Determine whether a fetched block is considered indexed at the
%% requested depth. Checks block markers first. For blocks at or above
%% the cutover height, the marker is authoritative. For blocks below
%% the cutover, falls back to legacy per-TX check.
is_already_indexed({ok, Block}, TargetDepth, Opts) ->
    Height = hb_maps:get(<<"height">>, Block, undefined, Opts),
    case is_block_indexed(Height, TargetDepth, Opts) of
        true ->
            true;
        false ->
            case is_post_cutover(Height, Opts) of
                true ->
                    false;
                false ->
                    TXIDs = hb_maps:get(<<"txs">>, Block, [], Opts),
                    lists:any(
                        fun(TXID) -> is_tx_indexed(TXID, Opts) end,
                        TXIDs
                    )
            end
    end;
is_already_indexed({error, _}, _TargetDepth, _Opts) ->
    false.

is_post_cutover(undefined, _Opts) -> false;
is_post_cutover(Height, Opts) ->
    case read_cutover_height(Opts) of
        undefined -> false;
        Cutover -> Height >= Cutover
    end.

fetch_and_process_block(Current, To, TargetDepth, Opts) ->
    BlockRes = fetch_block_header(Current, Opts),
    process_block(BlockRes, Current, To, TargetDepth, Opts).

%% @doc Process a block.
process_block(BlockRes, Current, To, TargetDepth, Opts) ->
    case BlockRes of
        {ok, Block} ->
            ?event(debug_copycat, {{processing_block, Current},
                {indep_hash, hb_maps:get(<<"indep_hash">>, Block, <<>>)}}),
            case maybe_index_block(Block, TargetDepth, Opts) of
                {block_skipped, Results} ->
                    TotalTXs = maps:get(total_txs, Results, 0),
                    ?event(
                        copycat_short,
                        {arweave_block_skipped,
                            {height, Current},
                            {total_txs, TotalTXs},
                            {target, To}
                        }
                    );
                {block_cached, Results} ->
                    ItemsIndexed = maps:get(items_count, Results, 0),
                    TotalTXs = maps:get(total_txs, Results, 0),
                    BundleTXs = maps:get(bundle_count, Results, 0),
                    SkippedTXs = maps:get(skipped_count, Results, 0),
                    AchievedDepth = maps:get(
                        achieved_depth, Results,
                        max(2, TargetDepth)),
                    ItemIDs = maps:get(item_ids, Results, #{}),
                    maybe
                        ok ?= write_block_item_ids(
                            Current, AchievedDepth, ItemIDs, Opts),
                        ok ?= mark_block_indexed(
                            Current, AchievedDepth, Opts),
                        ensure_cutover_height(Current, Opts),
                        ?event(
                            copycat_short,
                            {arweave_block_indexed,
                                {height, Current},
                                {items_indexed, ItemsIndexed},
                                {total_txs, TotalTXs},
                                {bundle_txs, BundleTXs},
                                {skipped_txs, SkippedTXs},
                                {achieved_depth, AchievedDepth},
                                {target, To}
                            }
                        )
                    else
                        {error, item_ids_write_failed} ->
                            ?event(
                                copycat_short,
                                {arweave_block_metadata_failed,
                                    {height, Current},
                                    {target, To}
                                }
                            ),
                            throw(item_ids_write_failed);
                        Error ->
                            ?event(
                                copycat_short,
                                {arweave_block_marker_failed,
                                    {height, Current},
                                    {target, To},
                                    {error, Error}
                                }
                            ),
                            throw({writing_to_index_store, Error})
                    end
            end;
        {error, _} = Error ->
            ?event(
                copycat_short,
                {arweave_block_not_found,
                    {height, Current},
                    {target, To},
                    {reason, Error}} 
            )
    end.

%% @doc Index the IDs of all transactions in the block if configured to do so.
maybe_index_block(Block, TargetDepth, Opts) ->
    TotalTXs = length(hb_maps:get(<<"txs">>, Block, [], Opts)),
    case hb_opts:get(arweave_index_ids, true, Opts) of
        false -> 
            {block_skipped, #{
                items_count => 0,
                total_txs => TotalTXs,
                bundle_count => 0,
                skipped_count => 0
            }};
        true ->
            BlockEndOffset = hb_util:int(
                hb_maps:get(<<"weave_size">>, Block, 0, Opts)),
            BlockSize = hb_util:int(
                hb_maps:get(<<"block_size">>, Block, 0, Opts)),
            BlockStartOffset = BlockEndOffset - BlockSize,
            case resolve_tx_headers(hb_maps:get(<<"txs">>, Block, [], Opts), Opts) of
                error ->
                    % Skip entire block if any transaction errors
                    {block_skipped, #{
                        skipped_count => TotalTXs,
                        total_txs => TotalTXs
                    }};
                {ok, TXs} ->
                    Height = hb_maps:get(<<"height">>, Block, 0, Opts),
                    L1IDs = [TX#tx.id || TX <- TXs],
                    TXsWithData = ar_block:generate_size_tagged_list_from_txs(TXs, Height),
                    ValidTXs = lists:filter(
                        fun({{padding, _}, _}) -> false; (_) -> true end,
                        TXsWithData
                    ),
                    TXResults = process_block_txs(
                        ValidTXs, BlockStartOffset, TargetDepth, Height, Opts),
                    ExistingIDs = maps:get(item_ids, TXResults, #{}),
                    {block_cached, TXResults#{
                        total_txs => TotalTXs,
                        item_ids => ExistingIDs#{1 => L1IDs}
                    }}
            end
    end.

%% @doc Apply Fun to each item in Items with parallel workers.
%% Fun takes an item and returns a result.
%% Returns a list of results in the same order as the input items.
%% Uses arweave_index_workers from Opts to determine max concurrency (default 1 = sequential).
parallel_map(Items, Fun, Opts) ->
    MaxWorkers = max(1, hb_opts:get(arweave_index_workers, 1, Opts)),
    hb_pmap:parallel_map(Items, Fun, MaxWorkers).

%% @doc Process a single transaction and return its contribution to the counters.
%% Returns a map with keys: items_count, bundle_count, skipped_count
process_block_tx({{padding, _PaddingRoot}, _EndOffset}, _BlockStartOffset, TargetDepth, _BlockHeight, _Opts) ->
    #{items_count => 0, bundle_count => 0, skipped_count => 0,
        achieved_depth => max(2, TargetDepth)};
process_block_tx({{TX, _TXDataRoot}, EndOffset}, BlockStartOffset, TargetDepth, BlockHeight, Opts) ->
    ArweaveStore = hb_store_arweave:store_from_opts(Opts),
    TXID = hb_util:encode(TX#tx.id),
    TXEndOffset = BlockStartOffset + EndOffset,
    TXStartOffset = TXEndOffset - TX#tx.data_size,
    ?event(debug_copycat, {writing_index,
        {id, {explicit, TXID}},
        {offset, TXStartOffset},
        {size, TX#tx.data_size}
    }),
    ok = observe_event(<<"item_indexed">>, fun() ->
        hb_store_arweave:write_offset(
            ArweaveStore,
            TXID,
            <<"tx@1.0">>,
            TXStartOffset,
            TX#tx.data_size
        )
    end),
    #{ <<"index-store">> := IndexStore } = ArweaveStore,
    ok = write_parent(TX#tx.id, BlockHeight, block, IndexStore),
    try is_bundle_tx(TX, Opts) of
        false ->
            #{items_count => 0, bundle_count => 0, skipped_count => 0,
                achieved_depth => max(2, TargetDepth)};
        true when TargetDepth > 2 ->
            %% Retry to preserve bundle count
            try 
                L1Result = process_l1_tx_direct(
                    TXStartOffset, TX#tx.data_size,
                    TargetDepth - 1, ArweaveStore, TXID, TX#tx.id, Opts),
                L1Result#{
                    achieved_depth =>
                        max(2, maps:get(achieved_depth, L1Result, 0))
                }
            catch 
                _:Reason:Stacktrace ->
                    ?event(copycat_short,
                        {arweave_bundle_skipped,
                            {tx, {explicit, TX#tx.id}},
                            {reason, Reason},
                            {stacktrace, Stacktrace}}),
                    #{items_count => 0, bundle_count => 1,
                        skipped_count => 1, achieved_depth => 0}
            end;
        true ->
            % Lightweight processing of block transactions to depth 2. We
            % can avoid loading the full L1 TX data into memory, and instead
            % only load the bundle header. But as a result we're unable to
            % recurse any deeper than L2 dataitems.
            ?event(debug_copycat, {fetching_bundle_header, 
                {tx_id, {string, TXID}},
                {tx_end_offset, TXEndOffset},
                {tx_data_size, TX#tx.data_size}
            }),
            BundleRes = download_bundle_header(
                TXEndOffset, TX#tx.data_size, Opts
            ),
            case BundleRes of
                {ok, HeaderSize, BundleIndex} ->
                    {TotalTime, {_, ItemsCount}} = timer:tc(fun() ->
                        lists:foldl(
                            fun({ItemID, Size}, {ItemStartOffset, ItemsCountAcc}) ->
                                ok = hb_store_arweave:write_offset(
                                    ArweaveStore,
                                    hb_util:encode(ItemID),
                                    <<"ans104@1.0">>,
                                    ItemStartOffset,
                                    Size
                                ),
                                ok = write_parent(ItemID, TX#tx.id, bundle, IndexStore),
                                {ItemStartOffset + Size, ItemsCountAcc + 1}
                            end,
                            {TXStartOffset + HeaderSize, 0},
                            BundleIndex
                        )
                    end),
                    L2IDs = [ItemID || {ItemID, _Size} <- BundleIndex],
                    ?event(debug_copycat,
                        {bundle_items_indexed,
                            {tx_id, {string, TXID}},
                            {items_count, ItemsCount}
                        }),
                    % Single event record for the batch
                    record_event_metrics(<<"item_indexed">>, ItemsCount, TotalTime),
                    #{items_count => ItemsCount, bundle_count => 1,
                        skipped_count => 0, achieved_depth => 2,
                        item_ids => #{2 => L2IDs}};
                {error, Reason} ->
                    ?event(
                        copycat_short,
                        {arweave_bundle_skipped,
                            {tx_id, {explicit, TXID}},
                            {reason, Reason}
                        }
                    ),
                    #{items_count => 0, bundle_count => 1,
                        skipped_count => 1, achieved_depth => 0}
            end
    catch
        _:Reason:Stacktrace ->
            ?event(copycat_short,
                {arweave_bundle_skipped,
                    {tx, {explicit, TX#tx.id}},
                    {reason, Reason},
                    {stacktrace, Stacktrace}}),
            #{items_count => 0, bundle_count => 0, skipped_count => 1, achieved_depth => 0}
    end.

download_bundle_header(EndOffset, Size, Opts) ->
    observe_event(<<"bundle_header">>, fun() ->
        dev_arweave:bundle_header(EndOffset - Size, Opts)
    end).

%% @doc Process transactions: spawn workers and manage the worker pool.
%% This function processes transactions in parallel using parallel_map.
%% When arweave_index_workers <= 1, processes sequentially (one worker at a time).
%% When arweave_index_workers > 1, processes in parallel with the specified concurrency limit.
%% Returns a map with keys: items_count, bundle_count, skipped_count.
process_block_txs(ValidTXs, BlockStartOffset, TargetDepth, BlockHeight, Opts) ->
    Results = parallel_map(
        ValidTXs,
        fun(TXWithData) -> process_block_tx(
            TXWithData, BlockStartOffset, TargetDepth, BlockHeight, Opts) end,
        Opts
    ),
    Folded = lists:foldl(
        fun(Result, Acc) ->
            #{
                items_count =>
                    maps:get(items_count, Result, 0)
                        + maps:get(items_count, Acc, 0),
                bundle_count =>
                    maps:get(bundle_count, Result, 0)
                        + maps:get(bundle_count, Acc, 0),
                skipped_count =>
                    maps:get(skipped_count, Result, 0)
                        + maps:get(skipped_count, Acc, 0),
                achieved_depth =>
                    min(
                        maps:get(achieved_depth, Result, ?DEPTH_SENTINEL),
                        maps:get(achieved_depth, Acc, ?DEPTH_SENTINEL)
                    )
            }
        end,
        #{items_count => 0, bundle_count => 0, skipped_count => 0,
            achieved_depth => ?DEPTH_SENTINEL},
        Results
    ),
    MergedIDs = merge_all_item_ids(
        [maps:get(item_ids, R, #{}) || R <- Results]),
    Folded2 = Folded#{item_ids => MergedIDs},
    case maps:get(achieved_depth, Folded2) of
        ?DEPTH_SENTINEL ->
            Folded2#{achieved_depth => max(2, TargetDepth)};
        _ ->
            Folded2
    end.

%% @doc Process a single indexed L1 TX candidate after lightweight filter checks.
maybe_process_l1_tx(TXID, Filters, Depth, QueryL1Offset, Opts) ->
    Skipped = #{items_count => 0, bundle_count => 0, skipped_count => 1,
        achieved_depth => 0},
    NormalizedTXID = hb_util:native_id(TXID),
    EncodedTXID = hb_util:encode(NormalizedTXID),
    IndexStore = hb_store_arweave:store_from_opts(Opts),
    ?event(copycat_short,
        {indexing_l1_tx, {tx_id, {explicit, EncodedTXID}},
        {depth, Depth},
        {query_l1_offset, QueryL1Offset}
    }),
    maybe
        {ok,
            #{
                <<"codec-device">> := <<"tx@1.0">>,
                <<"start-offset">> := StartOffset,
                <<"length">> := Length
            }} ?=
            observe_copycat_l1_stage(
                <<"l1_offset_lookup">>,
                fun() ->
                    ensure_l1_tx_offset(
                        NormalizedTXID,
                        EncodedTXID,
                        IndexStore,
                        QueryL1Offset,
                        Opts
                    )
                end
            ),
        {ok, TX} ?= resolve_tx_header(EncodedTXID, Opts),
        pass ?= l1_filter_reason(TX, Filters),
        bundle ?=
            case is_bundle_tx(TX, Opts) of
                true -> bundle;
                false -> not_bundle
            end,
        within_effective_cap ?=
            case Length =< effective_memory_cap(Opts) of
                true -> within_effective_cap;
                false -> effective_cap_exceeded
            end,
        ok ?= hb_copycat_budget:lease(Length),
        try process_l1_tx(
            StartOffset,
            Length,
            Depth,
            IndexStore,
            EncodedTXID,
            hb_util:decode(EncodedTXID),
            Opts
        )
        after
            hb_copycat_budget:release(Length)
        end
    else
        {error, Reason} ->
            ?event(
                copycat_short,
                {arweave_tx_skipped,
                    {tx_id, {explicit, EncodedTXID}},
                    {reason, Reason}
                }
            ),
            Skipped;
        error ->
            % event already logged in resolve_tx_header
            Skipped;
        not_bundle ->
            ?event(
                copycat_short,
                {arweave_tx_skipped,
                    {tx_id, {explicit, EncodedTXID}},
                    {reason, not_bundle}
                }
            ),
            Skipped;
        effective_cap_exceeded ->
            ?event(
                copycat_short,
                {arweave_bundle_skipped,
                    {tx_id, {explicit, EncodedTXID}},
                    {reason, effective_cap_exceeded}
                }
            ),
            #{
                items_count => 0,
                bundle_count => 1,
                skipped_count => 1,
                achieved_depth => 0
            };
        FilterReason ->
            ?event(
                copycat_short,
                {arweave_tx_skipped,
                    {tx_id, {explicit, EncodedTXID}},
                    {reason, FilterReason}
                }
            ),
            Skipped
    end.

%% @doc Fast path for depth>2 block indexing. Skips offset lookup and
%% header re-fetch since the caller already has both.
process_l1_tx_direct(StartOffset, Length, Depth, IndexStore, EncodedTXID, ParentID, Opts) ->
    EffectiveCap = effective_memory_cap(Opts),
    case Length > EffectiveCap of
        true ->
            ?event(copycat_short,
                {arweave_bundle_skipped,
                    {tx_id, {explicit, EncodedTXID}},
                    {reason, effective_cap_exceeded}
                }
            ),
            #{items_count => 0, bundle_count => 1,
                skipped_count => 1, achieved_depth => 0};
        false ->
            ok = hb_copycat_budget:lease(Length),
            try
                process_l1_tx(
                    StartOffset, Length, Depth,
                    IndexStore, EncodedTXID, ParentID, Opts)
            after
                hb_copycat_budget:release(Length)
            end
    end.

%% @doc Load the L1 TX data into memory and index it.
process_l1_tx(
        StartOffset, Length, Depth, IndexStore, EncodedTXID, ParentID, Opts) ->
    case observe_copycat_l1_stage(
        <<"l1_read_chunks">>,
        fun() -> hb_store_arweave:read_chunks(StartOffset, Length, Opts) end
    ) of
        {ok, BundleData} ->
            {TotalTime, IndexRes} = timer:tc(
                fun() ->
                    observe_copycat_l1_stage(
                        <<"l1_full_bundle_index">>,
                        fun() ->
                            index_full_bundle_bytes(
                                BundleData,
                                StartOffset,
                                Depth,
                                IndexStore,
                                ParentID,
                                Opts
                            )
                        end
                    )
                end
            ),
            case IndexRes of
                {ok, ItemsCount, AchievedDepth, BundleIDs} ->
                    record_event_metrics(
                        <<"item_indexed">>,
                        ItemsCount,
                        TotalTime
                    ),
                    #{
                        items_count => ItemsCount,
                        bundle_count => 1,
                        skipped_count => 0,
                        achieved_depth => 1 + AchievedDepth,
                        item_ids => shift_item_ids(BundleIDs, 1)
                    };
                {error, Reason} ->
                    ?event(
                        copycat_short,
                        {arweave_bundle_skipped,
                            {tx_id, {explicit, EncodedTXID}},
                            {reason, Reason}
                        }
                    ),
                    #{
                        items_count => 0,
                        bundle_count => 1,
                        skipped_count => 1,
                        achieved_depth => 0
                    }
            end;
        {error, Reason} ->
            ?event(
                copycat_short,
                {arweave_bundle_skipped,
                    {tx_id, {explicit, EncodedTXID}},
                    {reason, Reason}
                }
            ),
            #{
                items_count => 0,
                bundle_count => 1,
                skipped_count => 1,
                achieved_depth => 0
            };
        not_found ->
            ?event(
                copycat_short,
                {arweave_bundle_skipped,
                    {tx_id, {explicit, EncodedTXID}},
                    {reason, not_found}
                }
            ),
            #{
                items_count => 0,
                bundle_count => 1,
                skipped_count => 1,
                achieved_depth => 0
            }
    end.
%% @doc Ensure the root L1 TX offset exists locally before `id=...` indexing.
%% if the offset is missing and `query_l1_offset` is enabled, fetches the TX
%% offset metadata from Arweave, writes it to the local offset store, and
%% retries the local lookup.
ensure_l1_tx_offset(_TXID, _EncodedTXID, IndexStore, _LoadL1Offset, _Opts)
        when is_map(IndexStore) =:= false ->
    {error, missing_offset};
ensure_l1_tx_offset(TXID, EncodedTXID, IndexStore, QueryL1Offset, Opts) ->
    case hb_store_arweave:read_offset(IndexStore, TXID) of
        {ok, _} = OffsetRes ->
            OffsetRes;
        not_found when QueryL1Offset ->
            ?event(
                copycat_short,
                {arweave_tx_querying_offset,
                    {tx_id, {explicit, EncodedTXID}},
                    {source, network}
                }
            ),
            case query_l1_tx_offset(EncodedTXID, IndexStore, Opts) of
                ok ->
                    case hb_store_arweave:read_offset(IndexStore, TXID) of
                        {ok, _} = OffsetRes ->
                            OffsetRes;
                        not_found ->
                            {error, missing_offset}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        not_found ->
            {error, missing_offset}
    end.

query_l1_tx_offset(TXID, IndexStore, Opts) ->
    % TODO: move this into dev_arweave - I think? Unless it's possible to
    % query this already via one of the existing ~arweave@2.9 paths?
    case observe_copycat_l1_stage(
        <<"l1_offset_query_http">>,
        fun() ->
            hb_http:request(
                #{
                    <<"path">> => <<"/arweave/tx/", TXID/binary, "/offset">>,
                    <<"method">> => <<"GET">>
                },
                Opts
            )
        end
    ) of
        {ok, #{ <<"body">> := OffsetBody }} ->
            OffsetMsg = hb_json:decode(OffsetBody),
            EndOffset = hb_util:int(maps:get(<<"offset">>, OffsetMsg)),
            Size = hb_util:int(maps:get(<<"size">>, OffsetMsg)),
            StartOffset = EndOffset - Size,
            ok = observe_copycat_l1_stage(
                <<"l1_offset_query_store_write">>,
                fun() ->
                    hb_store_arweave:write_offset(
                        IndexStore,
                        TXID,
                        <<"tx@1.0">>,
                        StartOffset,
                        Size
                    )
                end
            ),
            ok;
        {error, Reason} ->
            {error, Reason};
        not_found ->
            {error, not_found}
    end.

index_full_bundle_bytes(_BundleData, _BundleStartOffset, Depth, _Store, _ParentID, _Opts)
        when Depth =< 0 ->
    {ok, 0, 0, #{}};
index_full_bundle_bytes(BundleData, BundleStartOffset, Depth, Store, ParentID, Opts) ->
    case ar_bundles:decode_bundle_header(BundleData) of
        invalid_bundle_header ->
            {error, invalid_bundle_header};
        {ItemsBin, BundleIndex} ->
            HeaderSize = byte_size(BundleData) - byte_size(ItemsBin),
            index_full_bundle_items(
                BundleIndex,
                ItemsBin,
                BundleStartOffset + HeaderSize,
                Depth,
                Store,
                ParentID,
                Opts,
                0,
                ?DEPTH_SENTINEL,
                [],
                #{}
            )
    end.

%% @doc Index bundle children from decoded bundle bytes and recurse descendants in-memory.
%% Returns {ok, Count, MinAchievedDepth, ItemIDs} or {error, Reason}.
%% ItemIDs is a map of relative-depth => list of raw 32-byte IDs.
index_full_bundle_items(
        [], _ItemsBin, _ItemStartOffset, Depth, _Store, _ParentID, _Opts,
        Count, MinDepth, ThisLevelIDs, DescIDs) ->
    FinalDepth = case MinDepth of
        ?DEPTH_SENTINEL -> Depth;
        _ -> 1 + MinDepth
    end,
    AllIDs = DescIDs#{1 => lists:reverse(ThisLevelIDs)},
    {ok, Count, FinalDepth, AllIDs};
index_full_bundle_items(
    [{ItemID, Size} | Rest],
    ItemsBin,
    ItemStartOffset,
    Depth,
    #{ <<"index-store">> := IndexStore } = Store,
    ParentID,
    Opts,
    Count,
    MinDepth,
    ThisLevelIDs,
    DescIDs
) when byte_size(ItemsBin) >= Size ->
    ItemBinary = binary:part(ItemsBin, 0, Size),
    EncodedItemID = hb_util:encode(ItemID),
    ParseResult = validate_and_flag_item_id(
        ItemBinary, ItemID, EncodedItemID, IndexStore),
    ok = hb_store_arweave:write_offset(
        Store,
        EncodedItemID,
        <<"ans104@1.0">>,
        ItemStartOffset,
        Size
    ),
    ok = write_parent(ItemID, ParentID, bundle, IndexStore),
    {DescendantCount, ItemAchievedDepth, ChildIDs} =
        case {Depth > 1, ParseResult} of
            {true, {ok, HeaderSize, ParsedItem}} ->
                index_full_bundle_descendants_parsed(
                    ParsedItem, HeaderSize,
                    ItemStartOffset, Depth - 1, Store, ItemID, Opts);
            _ ->
                {0, Depth - 1, #{}}
        end,
    ShiftedChildIDs = shift_item_ids(ChildIDs, 1),
    index_full_bundle_items(
        Rest,
        binary:part(ItemsBin, Size, byte_size(ItemsBin) - Size),
        ItemStartOffset + Size,
        Depth,
        Store,
        ParentID,
        Opts,
        Count + 1 + DescendantCount,
        min(MinDepth, ItemAchievedDepth),
        [ItemID | ThisLevelIDs],
        merge_item_ids(DescIDs, ShiftedChildIDs)
    );
index_full_bundle_items(
        _BundleIndex, _ItemsBin, _ItemStartOffset, _Depth,
        _Store, _ParentID, _Opts, _Count, _MinDepth, _ThisLevelIDs, _DescIDs) ->
    {error, invalid_bundle_header}.

%% @doc Recurse into a nested data item using an already-parsed header.
%% Returns {Count, AchievedDepth, ItemIDs}.
index_full_bundle_descendants_parsed(
        _ParsedItem, _HeaderSize, _ItemStartOffset, Depth, _Store, _ParentID, _Opts)
        when Depth =< 0 ->
    {0, 0, #{}};
index_full_bundle_descendants_parsed(
        ParsedItem, HeaderSize, ItemStartOffset, Depth, Store, ParentID, Opts) ->
    case is_bundle_tx(ParsedItem, Opts) of
        true ->
            case index_full_bundle_bytes(
                ParsedItem#tx.data,
                ItemStartOffset + HeaderSize,
                Depth,
                Store,
                ParentID,
                Opts
            ) of
                {ok, Count, ChildDepth, ChildIDs} ->
                    {Count, ChildDepth, ChildIDs};
                _ ->
                    {0, 0, #{}}
            end;
        false ->
            {0, Depth, #{}}
    end.

%% @doc Validate an item ID by hashing the signature from the deserialized
%% header. Returns {ok, HeaderSize, ParsedItem} on successful parse, or
%% error if deserialization fails. Mismatch flags are written but don't
%% prevent the item from being indexed.
validate_and_flag_item_id(ItemBinary, DeclaredID, EncodedDeclaredID, IndexStore) ->
    try ar_bundles:deserialize_header(ItemBinary) of
        {ok, HeaderSize, ParsedItem} ->
            ComputedID = crypto:hash(sha256, ParsedItem#tx.signature),
            case ComputedID =:= DeclaredID of
                true ->
                    ok;
                false ->
                    ok = hb_store:write(
                        IndexStore,
                        hb_store_arweave_offset:mismatch_path(
                            DeclaredID),
                        ComputedID
                    ),
                    ?event(copycat_short,
                        {item_id_mismatch,
                            {declared_id, {explicit, EncodedDeclaredID}},
                            {computed_id,
                                {explicit, hb_util:encode(ComputedID)}}
                        }
                    )
            end,
            {ok, HeaderSize, ParsedItem};
        _ ->
            error
    catch
        _:_ ->
            error
    end.

%% @doc Check whether a TX header indicates bundle content.
%% NOTE: This function can throw if transaction tags aren't properly formated
is_bundle_tx(TX, _Opts) ->
    ar_tx:type(TX) =/= binary.

resolve_tx_headers(TXIDs, Opts) ->
    Results = parallel_map(
        TXIDs,
        fun(TXID) -> resolve_tx_header(TXID, Opts) end,
        Opts
    ),
    case lists:any(fun(Res) -> Res =:= error end, Results) of
        true -> error;
        false ->
            TXs = lists:foldr(
                fun({ok, TX}, Acc) -> [TX | Acc] end,
                [],
                Results
            ),
            {ok, TXs}
    end.

resolve_tx_header(TXID, Opts) ->
    try
        ?event(debug_copycat, {fetching_tx, {explicit, TXID}}),
        ResolveRes = observe_event(<<"tx_header">>, fun() ->
            hb_ao:resolve(
                <<
                    ?ARWEAVE_DEVICE/binary,
                    "/tx&tx=",
                    TXID/binary,
                    "&exclude-data=true"
                >>,
                Opts
            )
        end),
        case ResolveRes of
            {ok, StructuredTXHeader} ->
                {ok,
                    hb_message:convert(
                        StructuredTXHeader,
                        <<"tx@1.0">>,
                        <<"structured@1.0">>,
                        Opts)};
            {error, ResolveError} ->
                ?event(
                    copycat_short,
                    {arweave_tx_skipped,
                        {tx_id, {explicit, TXID}},
                        {reason, ResolveError}
                    }
                ),
                error
        end
    catch
        Class:Reason:_ ->
            ?event(
                copycat_short,
                {arweave_tx_skipped,
                    {tx_id, {explicit, TXID}},
                    {class, Class},
                    {reason, Reason}
                }
            ),
            error
    end.

%% @doc Record event metrics (count and duration) using hb_event:record.
record_event_metrics(MetricName, Count, Duration) ->
    hb_event:record(<<"arweave_block_count">>, MetricName, #{}, Count),
    hb_event:record(<<"arweave_block_duration">>, MetricName, #{}, Duration).

record_copycat_l1_metrics(MetricName, Count, Duration) ->
    hb_event:record(copycat_l1_count, MetricName, #{}, Count),
    hb_event:record(copycat_l1_duration, MetricName, #{}, Duration).

%% @doc Track an operation's execution time and count using hb_event:record.
%% Always tracks both count and duration, regardless of success/failure.
observe_event(MetricName, Fun) ->
    {Time, Result} = timer:tc(Fun),
    record_event_metrics(MetricName, 1, Time),
    Result.

observe_copycat_l1_stage(MetricName, Fun) ->
    {Time, Result} = timer:tc(Fun),
    record_copycat_l1_metrics(MetricName, 1, Time),
    Result.

get_index_store(Opts) ->
    case hb_store_arweave:store_from_opts(Opts) of
        #{ <<"index-store">> := Store } -> Store;
        _ -> throw(no_index_store_available)
    end.

%% @doc Scan the mempool and index any accessible unconfirmed TXs.
index_mempool(_Request, Opts) ->
    case dev_arweave:pending(#{}, #{}, Opts) of
        {ok, TXIDs} when is_list(TXIDs) ->
            Results = parallel_map(TXIDs,
                fun(TXID) -> index_mempool_tx(TXID, Opts) end, Opts),
            Summary = lists:foldl(fun(R, Acc) ->
                K = case R of
                    ok -> indexed; existing -> existing;
                    missing_data -> missing_data; _ -> failed
                end,
                Acc#{ K => maps:get(K, Acc) + 1 }
            end, #{ indexed => 0, existing => 0,
                    missing_data => 0, failed => 0 }, Results),
            ?event(copycat_short, {mempool_scan_completed, Summary}),
            {ok, Summary};
        Error -> Error
    end.

index_mempool_tx(TXID, Opts) ->
    case is_tx_indexed(TXID, Opts) of
        true -> existing;
        false ->
            case dev_arweave:pending(#{}, #{ <<"pending">> => TXID }, Opts) of
                {ok, StructuredTX} ->
                    TX = hb_message:convert(StructuredTX,
                        <<"tx@1.0">>, <<"structured@1.0">>, Opts),
                    case has_mempool_data(TX) of
                        true -> write_mempool_offsets(TXID, TX, Opts);
                        false -> missing_data
                    end;
                _ -> failed
            end
    end.

has_mempool_data(#tx{ data_size = 0 }) -> true;
has_mempool_data(#tx{ data = D, data_size = S })
        when is_binary(D) -> byte_size(D) =:= S;
has_mempool_data(_) -> false.

write_mempool_offsets(TXID, TX, Opts) ->
    Store = hb_store_arweave:store_from_opts(Opts),
    ok = hb_store_arweave:write_offset(
        Store, TXID, <<"tx@1.0">>, relative, TX#tx.data_size),
    case load_mempool_data(TXID, TX, Opts) of
        {ok, Data} ->
            write_mempool_children(Store, TXID, TX, Data, Opts);
        _ ->
            ok
    end,
    ok.

write_mempool_children(Store, TXID, TX, Data, Opts) ->
    case is_bundle_tx(TX, Opts) of
        true ->
            case load_mempool_bundle_index(TXID, Data, Opts) of
                {ok, HeaderSize, BundleIndex} ->
                    write_mempool_items(Store, TXID, BundleIndex, HeaderSize);
                _ -> ok
            end;
        false ->
            case standalone_item_id(Data) of
                {ok, ItemID} ->
                    Ref = #{ <<"relative">> => TXID, <<"offset">> => 0 },
                    hb_store_arweave:write_offset(
                        Store, ItemID, <<"ans104@1.0">>,
                        Ref, TX#tx.data_size);
                not_found -> ok
            end
    end.

write_mempool_items(_Store, _TXID, [], _Offset) -> ok;
write_mempool_items(Store, TXID, [{ItemID, Size} | Rest], Offset) ->
    Ref = #{ <<"relative">> => TXID, <<"offset">> => Offset },
    hb_store_arweave:write_offset(
        Store, hb_util:encode(ItemID), <<"ans104@1.0">>, Ref, Size),
    write_mempool_items(Store, TXID, Rest, Offset + Size).

load_mempool_data(_TXID, #tx{ data_size = 0 }, _Opts) ->
    {ok, <<>>};
load_mempool_data(TXID, #tx{ data_size = Size }, Opts) when Size > 0 ->
    hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"chunk">>,
            <<"offset">> => #{
                <<"relative">> => TXID,
                <<"offset">> => 0
            },
            <<"length">> => Size
        },
        Opts
    ).

load_mempool_bundle_index(_TXID, Data, _Opts) when is_binary(Data), Data =/= <<>> ->
    try ar_bundles:decode_bundle_header(Data) of
        {ItemsBin, BundleIndex} ->
            {ok, byte_size(Data) - byte_size(ItemsBin), BundleIndex};
        invalid_bundle_header ->
            {error, invalid_bundle_header}
    catch _:_ ->
        {error, invalid_bundle_header}
    end;
load_mempool_bundle_index(TXID, <<>>, Opts) ->
    try
        {ok, FirstChunk} =
            hb_ao:resolve(
                #{ <<"device">> => <<"arweave@2.9">> },
                #{
                    <<"path">> => <<"chunk">>,
                    <<"offset">> => #{
                        <<"relative">> => TXID,
                        <<"offset">> => 0
                    }
                },
                Opts
            ),
        case ar_bundles:bundle_header_size(FirstChunk) of
            invalid_bundle_header ->
                {error, invalid_bundle_header};
            HeaderSize when HeaderSize =< byte_size(FirstChunk) ->
                {_ItemsBin, BundleIndex} =
                    ar_bundles:decode_bundle_header(
                        binary:part(FirstChunk, 0, HeaderSize)
                    ),
                {ok, HeaderSize, BundleIndex};
            HeaderSize ->
                RemainingSize = HeaderSize - byte_size(FirstChunk),
                {ok, RemainingChunk} =
                    hb_ao:resolve(
                        #{ <<"device">> => <<"arweave@2.9">> },
                        #{
                            <<"path">> => <<"chunk">>,
                            <<"offset">> => #{
                                <<"relative">> => TXID,
                                <<"offset">> => byte_size(FirstChunk)
                            },
                            <<"length">> => RemainingSize
                        },
                        Opts
                    ),
                HeaderBin = <<FirstChunk/binary, RemainingChunk/binary>>,
                {_ItemsBin, BundleIndex} =
                    ar_bundles:decode_bundle_header(HeaderBin),
                {ok, HeaderSize, BundleIndex}
        end
    catch _:_ ->
        {error, invalid_bundle_header}
    end.

standalone_item_id(Data) when is_binary(Data), Data =/= <<>> ->
    try
        Item = ar_bundles:deserialize(Data),
        case ar_bundles:verify_item(Item) of
            true -> {ok, hb_util:encode(Item#tx.id)};
            false -> not_found
        end
    catch _:_ -> not_found
    end;
standalone_item_id(_) -> not_found.

%%% Tests

index_ids_test_parallel() ->
    %% Test block: https://viewblock.io/arweave/block/1827942
    %% Note: this block includes a data item with an Ethereum signature. This
    %% signature type is not yet (as of Jan 2026) supported by ar_bundles.erl,
    %% however we should still be able to index it (we just can't deserialize
    %% it).
    {_TestStore, StoreOpts, Opts} = setup_index_opts(),
    {ok, 1827942} =
        hb_ao:resolve(
            <<"~copycat@1.0/arweave&from=1827942&to=1827942">>,
            Opts
        ),
    ?assertMatch(
        {ok, _},
        hb_store_arweave:read(
            StoreOpts,
            #{ <<"read">> => <<"WbRAQbeyjPHgopBKyi0PLeKWvYZr3rgZvQ7QY3ASJS4">> },
            Opts
        )
    ),
    assert_item_read(
        <<"0vy2Ey8bWkSDcRIvWQJjxDeVGYOrTSmYIIhBILJntY8">>,
        Opts),
    assert_item_read(
        <<"2lmrYydmDweX2MgGH39ZEB9hKm2JqGOYmRiG3n_xh8A">>,
        Opts),
    assert_item_read(
        <<"ATi9pQF_eqb99UK84R5rq8lGfRGpilVQOYyth7rXxh8">>,
        Opts),
    assert_item_read(
        <<"4VSfUbhMVZQHW5VfVwQZOmC5fR3W21DZgFCyz8CA-cE">>,
        Opts),
    assert_item_read(
        <<"ZQRHZhktk6dAtX9BlhO1teOtVlGHoyaWP25kAlhxrM4">>,
        Opts),
    % The T2pluNnaavL7-S2GkO_m3pASLUqMH_XQ9IiIhZKfySs can be deserialized so
    % we'll verify that some of its items were index and match the version
    % in the deserialized bundle.
    assert_bundle_read(
        <<"T2pluNnaavL7-S2GkO_m3pASLUqMH_XQ9IiIhZKfySs">>,
        [
            {<<"54K1ehEIKZxGSusgZzgbGYaHfllwWQ09-S9-eRUJg5Y">>, <<"1">>},
            {<<"MgatoEjlO_YtdbxFi9Q7Hxbs0YQVcChddhSS7FsdeIg">>, <<"19">>},
            {<<"z-oKJfhMq5qoVFrljEfiBKgumaJmCWVxNJaavR5aPE8">>, <<"26">>}
        ],
        Opts
    ),
    % Non-ans104 data transaction 
    assert_item_read(
        <<"bXEgFm4K2b5VD64skBNAlS3I__4qxlM3Sm4Z5IXj3h8">>,
        Opts),
    % This bundle previously triggered the ANS-104 tag-section boundary bug:
    % the decoder ran past the declared tag bytes into the JSON body and
    % crashed with a badmatch on the body content (the `"address":"0x..."'
    % string). With the strict tag-section boundary enforced, the item is
    % decoded and indexed correctly.
    ?assertMatch(
        {ok, _},
        hb_store_arweave:read(
            StoreOpts,
            #{ <<"read">> => <<"kK67S13W_8jM9JUw2umVamo0zh9v1DeVxWrru2evNco">> },
            Opts)
    ),
    assert_bundle_read(
        <<"c2ATDuTgwKCcHpAFZqSt13NC-tA4hdA7Aa2xBPuOzoE">>,
        [
            {<<"OBKr-7UrmjxFD-h-qP-XLuvCgtyuO_IDpBMgIytvusA">>, <<"1">>}
        ],
        Opts
    ),
    % L3 item not read when doing L1 depth=1
    assert_item_not_read(<<"8aJrRWtHcJvJ61qsH6agGkemzrtLw3W22xFrpCGAnTM">>, Opts),
    ok.

block_depth_3_test() ->
    %% Test block: https://viewblock.io/arweave/block/1827942
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    {ok, 1827942} =
        hb_ao:resolve(
            <<"~copycat@1.0/arweave&from=1827942&to=1827942&depth=3">>,
            Opts
        ),
    % L3 item read when doing depth=3
    assert_item_read(
        <<"8aJrRWtHcJvJ61qsH6agGkemzrtLw3W22xFrpCGAnTM">>,
        Opts),
    ok.

%% @doc Test a bundle header that fits in a single chunk.
small_bundle_header_test_parallel() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    TXID = <<"29TsnbqPQ_7rQ_r4KF5qRr995W1wBw_mTy6WEMy40aw">>,
    {ok, #{ <<"body">> := OffsetBody }} =
        hb_http:request(
            #{
                <<"path">> => <<"/arweave/tx/", TXID/binary, "/offset">>,
                <<"method">> => <<"GET">>
            },
            Opts
        ),
    OffsetMsg = hb_json:decode(OffsetBody),
    EndOffset = hb_util:int(maps:get(<<"offset">>, OffsetMsg)),
    Size = hb_util:int(maps:get(<<"size">>, OffsetMsg)),
    {ok, HeaderSize, BundleIndex} =
        download_bundle_header(EndOffset, Size, Opts),
    ?assertEqual(1704, length(BundleIndex)),
    ?assertEqual(109088, HeaderSize),
    ok.

%% @doc Test a bundle header that doesn't fit in a single chunk.
large_bundle_header_test_parallel() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    TXID = <<"bnMTI7LglBGSaK5EdV_juh6GNtXLm0cd5lkd2q4nlT0">>,
    {ok, #{ <<"body">> := OffsetBody }} =
        hb_http:request(
            #{
                <<"path">> => <<"/arweave/tx/", TXID/binary, "/offset">>,
                <<"method">> => <<"GET">>
            },
            Opts
        ),
    OffsetMsg = hb_json:decode(OffsetBody),
    EndOffset = hb_util:int(maps:get(<<"offset">>, OffsetMsg)),
    Size = hb_util:int(maps:get(<<"size">>, OffsetMsg)),
    {ok, HeaderSize, BundleIndex} =
        download_bundle_header(EndOffset, Size, Opts),
    ?assertEqual(15000, length(BundleIndex)),
    ?assertEqual(960032, HeaderSize),
    ok.

invalid_bundle_header_test_parallel() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    TXID = <<"cGNURX2IUt98VKVIeXSfYe6eulNwPEqijaQfvatzd_o">>,
    {ok, #{ <<"body">> := OffsetBody }} =
        hb_http:request(
            #{
                <<"path">> => <<"/arweave/tx/", TXID/binary, "/offset">>,
                <<"method">> => <<"GET">>
            },
            Opts
        ),
    OffsetMsg = hb_json:decode(OffsetBody),
    EndOffset = hb_util:int(maps:get(<<"offset">>, OffsetMsg)),
    Size = hb_util:int(maps:get(<<"size">>, OffsetMsg)),
    ?assertEqual({error, invalid_bundle_header},
        download_bundle_header(EndOffset, Size, Opts)),
    ok.

invalid_bundle_test_parallel() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    Block = 1307606,
    {ok, Block} =
        hb_ao:resolve(
            <<"~copycat@1.0/arweave&from=", (hb_util:bin(Block))/binary, "&to=", (hb_util:bin(Block))/binary>>,
            Opts
        ),
    assert_bundle_read(
        <<"8S12ZqO6-_icGkeuH8mFq6x9q7OIoXOqFRGH5k-wshg">>,
        [
            {<<"gintz-t6q_kdeP_IBQVGnp9fgFzs-pPGGehXW-V7ZRk">>, <<"1">>}
        ],
        Opts
    ),
    % L1 TX with bundle tags, but data is not a valid bundle. The L1 TX
    % should still be indexed.
    assert_item_read(<<"cGNURX2IUt98VKVIeXSfYe6eulNwPEqijaQfvatzd_o">>, Opts),
    ok.

block_with_large_integer_test_parallel() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    Block = 633719,
    {ok, Block} =
        hb_ao:resolve(
            <<"~copycat@1.0/arweave&from=", (hb_util:bin(Block))/binary, "&to=", (hb_util:bin(Block))/binary>>,
            Opts
        ),
    % This is bundle signed with a solana signature, so only the L1 TX can
    % actually be loaded.
    assert_item_read(<<"UXpcKTl6Mh34eTFSgny4NcIqoUjBcgYIcMqromcS6_Q">>, Opts),
    ok.

empty_block_test_parallel() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    Block = 1865858,
    {ok, Block} =
        hb_ao:resolve(
            <<"~copycat@1.0/arweave&from=", (hb_util:bin(Block))/binary, "&to=", (hb_util:bin(Block))/binary>>,
            Opts
        ),
    ok.

% ecdsa_no_data_test() ->
%     {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
%     {ok, 1827904} =
%         hb_ao:resolve(
%             <<"~copycat@1.0/arweave&from=1827904&to=1827904">>,
%             Opts
%         ),
%     assert_bundle_read(
%         Opts,
%         <<"VNhX_pSANk_8j0jZBR5bh_5jr-lkfbHDjtHd8FKqx7U">>,
%         [
%             {<<"3xDKhrCQcPuBtcm1ipZS5C9gAfFYClgHuHOHAXGfchM">>, <<"1">>},
%             {<<"JantC8f89VE-RidArHnU9589gY5T37NDXnWpI7H_psc">>, <<"7">>}
%         ]
%     ),
%     ok.

% ecdsa_with_data_test() ->
%     {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
%     Block = 1720431,
%     fetch_and_process_block(Block, Block, Opts),
%     {ok, Block} =
%         hb_ao:resolve(
%             <<"~copycat@1.0/arweave&from=", (hb_util:bin(Block))/binary, "&to=", (hb_util:bin(Block))/binary>>,
%             Opts
%         ),
%     ok.

%% @doc Disabled because the test takes ~30 seconds to run.
%% dev_arweave:get_tx_data_tag_exclude_data_test has some test coverage for
%% handling an L1 TX with a data tag. 
tx_with_data_tag_test_disabled() ->
    {_TestStore, StoreOpts, Opts} = setup_index_opts(),
    Block = 1289677,
    {ok, Block} =
        hb_ao:resolve(
            <<"~copycat@1.0/arweave&from=", (hb_util:bin(Block))/binary, "&to=", (hb_util:bin(Block))/binary>>,
            Opts
        ),
    ?assertException(
        error,
        {badmatch, unsupported_tx_format},
        hb_store_arweave:read(
            StoreOpts,
            #{ <<"read">> => <<"ZwsFMXcwuakDuIhskokVHYiOPVcywDUAUTMLAJ72fgw">> },
            Opts)
    ),
    ?assertException(
        error,
        {badmatch, unsupported_tx_format},
        hb_store_arweave:read(
            StoreOpts,
            #{ <<"read">> => <<"-8ikoQo3KZkp9Hz_7kNdiUw3Vmn7J2DFslL_rBz0OBY">> },
            Opts)
    ),
    assert_bundle_read(
        <<"0vvttUgGqSsMul8RKIPvBjlwTU5_0x68sZr4uJxgNF8">>,
        [
            {<<"7U7GRZ8cXtKezSQmQmGpJar6haz-uink46i6evxzDCI">>, <<"1">>}
        ],
        Opts
    ),
    assert_item_read(<<"jI0A4BASHaUdCCsdv249BxDX6IlE0Ko391TuI6REATw">>, Opts),
    ok.

tx_with_no_data_test_parallel() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    Block = 1826700,
    BlockBin = hb_util:bin(Block),
    {ok, Block} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", BlockBin/binary, "&"
                "to=", BlockBin/binary, "&"
                "mode=write"
            >>,
            Opts
        ),
    % Value transfer
    Resolved = hb_ao:resolve(<<"XSQIgyDY1XUJNz79OeRHFaNpJZyaJSBd7XFsjWlZpNU">>, Opts),
    ?assertMatch({ok, _}, Resolved),
    {ok, StructuredTX} = Resolved,
    ?assert(hb_message:verify(StructuredTX, all, Opts)),
    ?assertEqual(
        <<"XSQIgyDY1XUJNz79OeRHFaNpJZyaJSBd7XFsjWlZpNU">>,
        hb_message:id(StructuredTX, signed, Opts)
    ),
    TX = hb_message:convert(
        StructuredTX,
        <<"tx@1.0">>,
        <<"structured@1.0">>,
        Opts),
    ?assertEqual(0, TX#tx.data_size),
    ?assertEqual(538493200840000, TX#tx.quantity),
    % TX with non-ans104 data
    assert_item_read(
        <<"bpd0CzsoTr9-X83sPCx08uNzZC_EgFwb-P8lnHXSeRo">>,
        Opts),
    %% Now list the index using list mode
    {ok, Response} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", BlockBin/binary, "&"
                "to=", BlockBin/binary, "&"
                "mode=list"
            >>,
            Opts
        ),
    JSONBody = maps:get(<<"body">>, Response),
    IndexData = hb_json:decode(JSONBody),
    BlockInfo = maps:get(BlockBin, IndexData),
    %% Verify indexed and not-indexed keys exist
    ?assert(maps:is_key(<<"indexed">>, BlockInfo)),
    ?assert(maps:is_key(<<"not-indexed">>, BlockInfo)),
    ?assertEqual([
            <<"XSQIgyDY1XUJNz79OeRHFaNpJZyaJSBd7XFsjWlZpNU">>,
            <<"bpd0CzsoTr9-X83sPCx08uNzZC_EgFwb-P8lnHXSeRo">>,
            <<"n5rT8Y9Jet7SCnl_M77UrPNUFeud5iKazsn9Sr9gsWA">>,
            <<"hvZlThf1B1tY4wMm4cETSsk8vIkOY3QZRmaBnQSzlVo">>,
            <<"3urwRfVyWN35HE5RHGwOUk6CxkJ_lZOaMY7HZbeJyRs">>
        ], maps:get(<<"indexed">>, BlockInfo)),
    ?assertEqual([ ], maps:get(<<"not-indexed">>, BlockInfo)),
    ok.

non_string_tags_test_parallel() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    Res = resolve_tx_header(<<"752P6t4cOjMabYHqzC6hyLhxyo4YKZLblg7va_J21YE">>, Opts),
    ?assertEqual(error, Res),
    ok.

list_index_test_parallel() ->
    %% Test block: https://viewblock.io/arweave/block/1827942
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    %% First index the block using write mode
    Block = 1827942,
    BlockBin = hb_util:bin(Block),
    {ok, Block} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", BlockBin/binary, "&"
                "to=", BlockBin/binary, "&"
                "mode=write"
            >>,
            Opts
        ),
    %% Now list the index using list mode
    {ok, Response} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", BlockBin/binary, "&"
                "to=", BlockBin/binary, "&"
                "mode=list"
            >>,
            Opts
        ),
    %% Verify content-type is application/json
    ?assertEqual(<<"application/json">>, maps:get(<<"content-type">>, Response)),
    ?event(debug_test, {response, Response}),
    %% Decode the JSON body
    JSONBody = maps:get(<<"body">>, Response),
    IndexData = hb_json:decode(JSONBody),
    %% Verify the block height is present as a key
    ?assert(maps:is_key(BlockBin, IndexData)),
    BlockInfo = maps:get(BlockBin, IndexData),
    %% Verify indexed and not-indexed keys exist
    ?assert(maps:is_key(<<"indexed">>, BlockInfo)),
    ?assert(maps:is_key(<<"not-indexed">>, BlockInfo)),
    ?assertEqual([
            <<"c2ATDuTgwKCcHpAFZqSt13NC-tA4hdA7Aa2xBPuOzoE">>,
            <<"kK67S13W_8jM9JUw2umVamo0zh9v1DeVxWrru2evNco">>,
            <<"bXEgFm4K2b5VD64skBNAlS3I__4qxlM3Sm4Z5IXj3h8">>,
            <<"T2pluNnaavL7-S2GkO_m3pASLUqMH_XQ9IiIhZKfySs">>,
            <<"WbRAQbeyjPHgopBKyi0PLeKWvYZr3rgZvQ7QY3ASJS4">>
        ], maps:get(<<"indexed">>, BlockInfo)),
    ?assertEqual([ ], maps:get(<<"not-indexed">>, BlockInfo)),
    ok.

auto_stop_on_indexed_block_test_parallel() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    IndexedBlock = 1827941,
    Higher1 = IndexedBlock + 1,
    Higher2 = IndexedBlock + 2,
    {ok, IndexedBlock} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", (hb_util:bin(IndexedBlock))/binary, "&"
                "to=", (hb_util:bin(IndexedBlock))/binary, "&"
                "mode=write"
            >>,
            Opts
        ),
    {ok, IndexedBlock} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", (hb_util:bin(Higher2))/binary, "&"
                "mode=write"
            >>,
            Opts
        ),
    ?assert(has_any_indexed_tx(Higher2, Opts)),
    ?assert(has_any_indexed_tx(Higher1, Opts)),
    ?assert(has_any_indexed_tx(IndexedBlock, Opts)),
    ?assertNot(has_any_indexed_tx(IndexedBlock-1, Opts)),
    ?assert(is_block_indexed(IndexedBlock, 2, Opts)),
    ?assert(is_block_indexed(Higher1, 2, Opts)),
    ?assert(is_block_indexed(Higher2, 2, Opts)),
    ok.

explicit_to_reindexes_all_test_parallel() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    IndexedBlock = 1827942,
    LowerBlock = IndexedBlock - 1,
    {ok, IndexedBlock} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", (hb_util:bin(IndexedBlock))/binary, "&"
                "to=", (hb_util:bin(IndexedBlock))/binary, "&"
                "mode=write"
            >>,
            Opts
        ),
    ?assertNot(has_any_indexed_tx(LowerBlock, Opts)),
    {ok, LowerBlock} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", (hb_util:bin(IndexedBlock+1))/binary, "&"
                "to=", (hb_util:bin(LowerBlock))/binary, "&"
                "mode=write"
            >>,
            Opts
        ),
    ?assert(has_any_indexed_tx(LowerBlock, Opts)),
    ok.

%% @doc Manually write to the index to simulate a partially indexed block.
%% This should also trigger a stop when the `to` option is omitted.
auto_stop_partial_index_test_parallel() ->
    {_TestStore, StoreOpts, Opts} = setup_index_opts(),
    Block = 1826700,
    HigherBlock = Block + 1,
    NoIndexOpts = Opts#{
        <<"arweave-index-ids">> => false,
        <<"arweave-index-blocks">> => true
    },
    {ok, Block} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", (hb_util:bin(Block))/binary, "&"
                "to=", (hb_util:bin(Block))/binary, "&"
                "mode=write"
            >>,
            NoIndexOpts
        ),
    {ok, BlockData} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave@2.9">> },
            #{
                <<"path">> => <<"block">>,
                <<"block">> => Block,
                <<"cache-control">> => [<<"only-if-cached">>]
            },
            Opts
        ),
    TXIDs = hb_maps:get(<<"txs">>, BlockData, [], Opts),
    ?assert(length(TXIDs) > 0),
    [OneTXID | _] = TXIDs,
    hb_store_arweave:write_offset(StoreOpts, OneTXID, <<"tx@1.0">>, 0, 0),
    {ok, Block} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", (hb_util:bin(HigherBlock))/binary, "&"
                "mode=write"
            >>,
            Opts
        ),
    ?assert(has_any_indexed_tx(HigherBlock, Opts)),
    ?assert(has_any_indexed_tx(Block, Opts)),
    ?assertNot(has_any_indexed_tx(Block-1, Opts)),
    ?assert(is_block_indexed(HigherBlock, 2, Opts)),
    ?assertNot(is_block_indexed(Block, 2, Opts)),
    ok.

negative_parse_range_test_parallel() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    {ok, Tip} =
        hb_ao:resolve(
            <<?ARWEAVE_DEVICE/binary, "/current/height">>,
            Opts
        ),
    {ok, {NegativeFrom, UndefinedTo}} =
        parse_range(#{ <<"from">> => <<"-3">> }, Opts),
    ?assertEqual(hb_util:int(Tip) - 3, NegativeFrom),
    ?assertEqual(undefined, UndefinedTo),
    {ok, {PositiveFrom, NegativeTo}} =
        parse_range(#{ <<"from">> => <<"10">>, <<"to">> => <<"-3">> }, Opts),
    ?assertEqual(10, PositiveFrom),
    ?assertEqual(hb_util:int(Tip) - 3, NegativeTo),
    ok.

latest_height_failure_test_parallel() ->
    {ok, MockURL, MockHandle} = hb_mock_server:start([
        {"/block/current", block_current, {500, <<"Internal Server Error">>}}
    ]),
    TestStore = hb_test_utils:test_store(),
    Opts = #{
        <<"store">> => [TestStore],
        <<"routes">> => [
            #{
                <<"template">> => <<"^/arweave">>,
                <<"nodes">> => [
                    #{
                        <<"match">> => <<"^/arweave">>,
                        <<"with">> => MockURL,
                        <<"opts">> => #{ <<"http-client">> => httpc }
                    }
                ],
                <<"parallel">> => true,
                <<"stop-after">> => true,
                <<"admissible-status">> => 200
            }
        ]
    },
    try
        ?assertMatch(
            {error, unavailable},
            parse_range(#{}, Opts)
        ),
        ?assertMatch(
            {error, unavailable},
            hb_ao:resolve(
                <<"~copycat@1.0/arweave&mode=write">>, Opts)
        )
    after
        hb_mock_server:stop(MockHandle)
    end.

negative_resolved_height_test_parallel() ->
    {ok, MockURL, MockHandle} = hb_mock_server:start([
        {"/block/current", block_current,
            {200, <<"{\"height\": 5}">>}}
    ]),
    TestStore = hb_test_utils:test_store(),
    Opts = #{
        <<"store">> => [TestStore],
        <<"arweave-index-blocks">> => false,
        <<"routes">> => [
            #{
                <<"template">> => <<"^/arweave">>,
                <<"nodes">> => [
                    #{
                        <<"match">> => <<"^/arweave">>,
                        <<"with">> => MockURL,
                        <<"opts">> => #{ <<"http-client">> => httpc }
                    }
                ],
                <<"parallel">> => true,
                <<"stop-after">> => true,
                <<"admissible-status">> => 200
            }
        ]
    },
    try
        ?assertMatch(
            {error, unavailable},
            parse_range(#{ <<"from">> => <<"-10">> }, Opts)
        )
    after
        hb_mock_server:stop(MockHandle)
    end.

negative_from_index_test_parallel() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    {ok, Tip} = latest_height(Opts),
    StopBlock = 1827942,
    StartBlock = 1827943,
    OffsetFromTip = Tip - StartBlock,
    ?assert(OffsetFromTip > 0),
    NegativeFrom = <<"-", (hb_util:bin(OffsetFromTip))/binary>>,
    {ok, StopBlock} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", (hb_util:bin(StopBlock))/binary, "&"
                "to=", (hb_util:bin(StopBlock))/binary, "&"
                "mode=write"
            >>,
            Opts
        ),
    {ok, StopBlock} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", NegativeFrom/binary, "&"
                "mode=write"
            >>,
            Opts
        ),
    ?assert(has_any_indexed_tx(StartBlock, Opts)),
    NextBlock = highest_contiguous_indexed_block(StopBlock, 50, Opts),
    ?assertEqual(StartBlock, NextBlock),
    assert_indexed_range(NextBlock, StopBlock, Opts),
    ?assertNot(has_any_indexed_tx(StopBlock - 1, Opts)),
    ?assertNot(has_any_indexed_tx(NextBlock + 1, Opts)),
    ok.

owner_alias_roundtrip_test() ->
    Opts1 =
        add_owner_alias(
            <<"FPjbN7EVwP3XwQJx8qnKqJDYa4TLJ0Y8gu4AaiUuW1c">>,
            <<"turbo">>,
            #{}
        ),
    Opts2 =
        add_owner_alias(
            <<"JNC6vBhU4sAK5T49VL4k79vNer0tZjM8fI1gpqUQK5g">>,
            <<"redstone">>,
            Opts1
        ),
    ?assertEqual(
        {ok, normalize_owner_id(<<"FPjbN7EVwP3XwQJx8qnKqJDYa4TLJ0Y8gu4AaiUuW1c">>)},
        resolve_owner_alias(<<"turbo">>, Opts2)
    ),
    ?assertEqual(
        {ok, normalize_owner_id(<<"JNC6vBhU4sAK5T49VL4k79vNer0tZjM8fI1gpqUQK5g">>)},
        resolve_owner_alias(<<"redstone">>, Opts2)
    ),
    ?assertEqual(
        {error, {owner_alias_not_found, <<"unknown">>}},
        resolve_owner_alias(<<"unknown">>, Opts2)
    ),
    ok.

parse_tag_filter_test() ->
    ?assertEqual(
        {ok, #{name => <<"App-Name">>, value => <<"ao">>}},
        parse_tag_filter(<<"include-tag">>, #{<<"include-tag">> => <<"App-Name:ao">>}, #{})
    ),
    ?assertEqual(
        {ok, undefined},
        parse_tag_filter(<<"include-tag">>, #{}, #{})
    ),
    ?assertEqual(
        {error, invalid_tag_filter},
        parse_tag_filter(<<"include-tag">>, #{<<"include-tag">> => <<"App-Name">>}, #{})
    ),
    ?assertEqual(
        {error, invalid_tag_filter},
        parse_tag_filter(<<"include-tag">>, #{<<"include-tag">> => <<":ao">>}, #{})
    ),
    ?assertEqual(
        {error, invalid_tag_filter},
        parse_tag_filter(<<"include-tag">>, #{<<"include-tag">> => <<"App-Name:">>}, #{})
    ),
    ok.

l1_filter_reason_test() ->
    Owner = <<"owner-1">>,
    OtherOwner = <<"owner-2">>,
    TX = #tx{
        owner = <<"non-default-owner">>,
        owner_address = Owner,
        tags = [
            {<<"App-Name">>, <<"ao">>},
            {<<"Bundler-App-Name">>, <<"Redstone">>}
        ]
    },
    IncludeTag = #{name => <<"App-Name">>, value => <<"ao">>},
    ExcludeTag = #{name => <<"Bundler-App-Name">>, value => <<"Redstone">>},
    ?assertEqual(pass, l1_filter_reason(TX, #{})),
    ?assertEqual(pass, l1_filter_reason(TX, #{include_owner => Owner})),
    ?assertEqual(
        include_owner_mismatch,
        l1_filter_reason(TX, #{include_owner => OtherOwner})
    ),
    ?assertEqual(
        exclude_owner_match,
        l1_filter_reason(TX, #{exclude_owner => Owner})
    ),
    ?assertEqual(
        pass,
        l1_filter_reason(TX, #{exclude_owner => OtherOwner})
    ),
    ?assertEqual(pass, l1_filter_reason(TX, #{include_tag => IncludeTag})),
    ?assertEqual(
        include_tag_mismatch,
        l1_filter_reason(
            TX,
            #{include_tag => #{name => <<"Content-Type">>, value => <<"text/plain">>}}
        )
    ),
    ?assertEqual(
        exclude_tag_match,
        l1_filter_reason(TX, #{exclude_tag => ExcludeTag})
    ),
    ?assertEqual(
        pass,
        l1_filter_reason(
            TX,
            #{exclude_tag => #{name => <<"Content-Type">>, value => <<"text/plain">>}}
        )
    ),
    ?assertEqual(
        exclude_tag_match,
        l1_filter_reason(
            TX,
            #{include_tag => IncludeTag, exclude_tag => ExcludeTag}
        )
    ),
    ?assertEqual(
        pass,
        l1_filter_reason(TX, #{include_owner => [OtherOwner, Owner]})
    ),
    ok.

request_depth_clamping_test() ->
    {_TestStore, _StoreOpts, Opts0} = setup_index_opts(),
    ?assertEqual(6, request_depth(#{}, <<"safe_max">>, Opts0)),
    ?assertEqual(
        2,
        request_depth(#{<<"depth">> => <<"2">>}, <<"safe_max">>, Opts0)
    ),
    ?assertEqual(
        1,
        request_depth(#{<<"depth">> => <<"0">>}, <<"safe_max">>, Opts0)
    ),
    ?assertEqual(
        6,
        request_depth(#{<<"depth">> => <<"999">>}, <<"safe_max">>, Opts0)
    ),
    Opts1 = set_depth_recursion_cap(2, Opts0),
    ?assertEqual(2, request_depth(#{}, <<"safe_max">>, Opts1)),
    % no recursion cap set, use default from hb_opts
    ?assertEqual(6, request_depth(#{}, <<"safe_max">>, #{})),
    ok.

id_depth_1_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    {Block, TXID} = {1827942, <<"T2pluNnaavL7-S2GkO_m3pASLUqMH_XQ9IiIhZKfySs">>},
    ok = index_l1_offsets(Block, Opts),
    {ok, Result} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "id=", TXID/binary, "&"
                "mode=write&"
                "depth=1"
            >>,
            Opts
        ),
    ?assertEqual(26, maps:get(items_count, Result)),
    ?assertEqual(1, maps:get(bundle_count, Result)),
    ?assertEqual(0, maps:get(skipped_count, Result)),
    assert_bundle_read(
        <<"T2pluNnaavL7-S2GkO_m3pASLUqMH_XQ9IiIhZKfySs">>,
        [
            {<<"54K1ehEIKZxGSusgZzgbGYaHfllwWQ09-S9-eRUJg5Y">>, <<"1">>},
            {<<"MgatoEjlO_YtdbxFi9Q7Hxbs0YQVcChddhSS7FsdeIg">>, <<"19">>},
            {<<"z-oKJfhMq5qoVFrljEfiBKgumaJmCWVxNJaavR5aPE8">>, <<"26">>}
        ],
        Opts
    ),
    % L3 item not read when doing L1 depth=1
    assert_item_not_read(<<"8aJrRWtHcJvJ61qsH6agGkemzrtLw3W22xFrpCGAnTM">>, Opts),
    ok.

id_depth_2_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    {Block, TXID} = {1827942, <<"T2pluNnaavL7-S2GkO_m3pASLUqMH_XQ9IiIhZKfySs">>},
    ok = index_l1_offsets(Block, Opts),
    {ok, Result} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "id=", TXID/binary, "&"
                "mode=write&"
                "depth=2"
            >>,
            Opts
        ),
    ?assertEqual(52, maps:get(items_count, Result)),
    ?assertEqual(1, maps:get(bundle_count, Result)),
    ?assertEqual(0, maps:get(skipped_count, Result)),
    assert_bundle_read(
        <<"T2pluNnaavL7-S2GkO_m3pASLUqMH_XQ9IiIhZKfySs">>,
        [
            {<<"54K1ehEIKZxGSusgZzgbGYaHfllwWQ09-S9-eRUJg5Y">>, <<"1">>},
            {<<"MgatoEjlO_YtdbxFi9Q7Hxbs0YQVcChddhSS7FsdeIg">>, <<"19">>},
            {<<"z-oKJfhMq5qoVFrljEfiBKgumaJmCWVxNJaavR5aPE8">>, <<"26">>}
        ],
        Opts
    ),
    % L2 bundle and L3 children should be read when doing L1 with depth=2
    assert_bundle_read(
        <<"54K1ehEIKZxGSusgZzgbGYaHfllwWQ09-S9-eRUJg5Y">>,
        [
            {<<"iS5R3iSKaCdcXG2nlKWsbdT1_uhQe54nMsgYK-ivEcE">>, <<"1">>},
            {<<"8aJrRWtHcJvJ61qsH6agGkemzrtLw3W22xFrpCGAnTM">>, <<"2">>}
        ],
        Opts
    ),
    ok.

id_exclude_tag_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    {Block, TXID} = {1827942, <<"T2pluNnaavL7-S2GkO_m3pASLUqMH_XQ9IiIhZKfySs">>},
    ok = index_l1_offsets(Block, Opts),
    {ok, Result} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "id=", TXID/binary, "&"
                "mode=write&"
                "exclude-tag=App-Name:ArDrive%20Turbo&"
                "depth=2"
            >>,
            Opts
        ),
    ?assertEqual(0, maps:get(items_count, Result)),
    ?assertEqual(0, maps:get(bundle_count, Result)),
    ?assertEqual(1, maps:get(skipped_count, Result)),
    assert_item_not_read(<<"iS5R3iSKaCdcXG2nlKWsbdT1_uhQe54nMsgYK-ivEcE">>, Opts),
    ok.

id_include_owner_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    {Block, TXID} = {1827942, <<"T2pluNnaavL7-S2GkO_m3pASLUqMH_XQ9IiIhZKfySs">>},
    ok = index_l1_offsets(Block, Opts),
    {ok, Included} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "id=", TXID/binary, "&"
                "mode=write&"
                "include-owner=JNC6vBhjHY1EPwV3pEeNmrsgFMxH5d38_LHsZ7jful8"
            >>,
            Opts
        ),
    ?assertEqual(52, maps:get(items_count, Included)),
    ?assertEqual(1, maps:get(bundle_count, Included)),
    ?assertEqual(0, maps:get(skipped_count, Included)),
    {ok, Skipped} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "id=", TXID/binary, "&"
                "mode=write&"
                "include-owner=FPjbN7EVwP3XwQJx8qnKqJDYa4TLJ0Y8gu4AaiUuW1c"
            >>,
            Opts
        ),
    ?assertEqual(0, maps:get(items_count, Skipped)),
    ?assertEqual(0, maps:get(bundle_count, Skipped)),
    ?assertEqual(1, maps:get(skipped_count, Skipped)).

id_missing_offset_without_load_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    {_Block, TXID} = {1827942, <<"T2pluNnaavL7-S2GkO_m3pASLUqMH_XQ9IiIhZKfySs">>},
    {ok, Result} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "id=", TXID/binary, "&"
                "mode=write"
            >>,
            Opts
        ),
    ?assertEqual(0, maps:get(items_count, Result)),
    ?assertEqual(0, maps:get(bundle_count, Result)),
    ?assertEqual(1, maps:get(skipped_count, Result)),
    assert_item_not_read(<<"T2pluNnaavL7-S2GkO_m3pASLUqMH_XQ9IiIhZKfySs">>, Opts),
    ok.

id_missing_offset_with_load_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    {_Block, TXID} = {1827942, <<"T2pluNnaavL7-S2GkO_m3pASLUqMH_XQ9IiIhZKfySs">>},
    {ok, Result} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "id=", TXID/binary, "&"
                "mode=write&"
                "query-l1-offset=true&"
                "depth=2"
            >>,
            Opts
        ),
    ?assertEqual(52, maps:get(items_count, Result)),
    ?assertEqual(1, maps:get(bundle_count, Result)),
    ?assertEqual(0, maps:get(skipped_count, Result)),
    assert_bundle_read(
        <<"T2pluNnaavL7-S2GkO_m3pASLUqMH_XQ9IiIhZKfySs">>,
        [
            {<<"54K1ehEIKZxGSusgZzgbGYaHfllwWQ09-S9-eRUJg5Y">>, <<"1">>},
            {<<"MgatoEjlO_YtdbxFi9Q7Hxbs0YQVcChddhSS7FsdeIg">>, <<"19">>},
            {<<"z-oKJfhMq5qoVFrljEfiBKgumaJmCWVxNJaavR5aPE8">>, <<"26">>}
        ],
        Opts
    ),
    % L2 bundle and L3 children should be read when doing L1 with depth=2
    assert_bundle_read(
        <<"54K1ehEIKZxGSusgZzgbGYaHfllwWQ09-S9-eRUJg5Y">>,
        [
            {<<"iS5R3iSKaCdcXG2nlKWsbdT1_uhQe54nMsgYK-ivEcE">>, <<"1">>},
            {<<"8aJrRWtHcJvJ61qsH6agGkemzrtLw3W22xFrpCGAnTM">>, <<"2">>}
        ],
        Opts
    ),
    ok.

parse_owner_filter_unknown_alias_test() ->
    ?assertEqual(
        {error, {owner_alias_not_found, <<"nonexistent">>}},
        parse_owner_filter(
            #{<<"include-owner-alias">> => <<"nonexistent">>},
            #{}
        )
    ),
    ok.

index_l1_offsets(Block, Opts) ->
    BlockBin = hb_util:bin(Block),
    {ok, Block} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", BlockBin/binary, "&"
                "to=", BlockBin/binary, "&"
                "mode=write&"
                "depth=1"
            >>,
            Opts
        ),
    ok.

setup_index_opts() ->
    TestStore = hb_test_utils:test_store(),
    StoreOpts = #{ <<"index-store">> => [TestStore] },
    Store = [
        TestStore,
        #{
            <<"store-module">> => hb_store_fs,
            <<"name">> => <<"cache-mainnet">>
        },
        #{
            <<"store-module">> => hb_store_arweave,
            <<"name">> => <<"cache-arweave">>,
            <<"index-store">> => [TestStore],
            <<"arweave-node">> => <<"https://arweave.net">>
        },
        #{
            <<"store-module">> => hb_store_gateway,
            <<"subindex">> => [
                #{
                    <<"name">> => <<"Data-Protocol">>,
                    <<"value">> => <<"ao">>
                }
            ],
            <<"local-store">> => [TestStore]
        },
        #{
            <<"store-module">> => hb_store_gateway,
            <<"local-store">> => [TestStore]
        }
    ],
    Opts = #{
        <<"store">> => Store,
        <<"arweave-index-ids">> => true,
        <<"arweave-index-store">> => StoreOpts
    },
    {TestStore, StoreOpts, Opts}.

assert_bundle_read(BundleID, ExpectedItems, Opts) ->
    ReadItems =
        lists:map(
            fun({ItemID, _Index}) ->
                assert_item_read(ItemID, Opts)
            end,
            ExpectedItems
        ),
    Bundle = assert_item_read(BundleID, Opts),
    lists:foreach(
        fun({{_ItemID, Index}, Item}) ->
            QueriedItem = hb_ao:get(Index, Bundle, Opts),
            ?assertEqual(
                hb_maps:without(?AO_CORE_KEYS, Item),
                hb_maps:without(?AO_CORE_KEYS, QueriedItem))
        end,
        lists:zip(ExpectedItems, ReadItems)
    ),
    ok.

assert_item_read(ItemID, Opts) ->
    ?event(debug_test, {resolving, {explicit, ItemID}}),
    ReadResult = hb_store_arweave:read(
        hb_store_arweave:store_from_opts(Opts), ItemID),
    ?assertMatch({ok, _}, ReadResult, ItemID),
    {ok, Item} = ReadResult,
    ?event(debug_test, {item, Item}),
    ?assert(hb_message:verify(Item, all, Opts)),
    ?assertEqual(ItemID, hb_message:id(Item, signed)),
    Item.

assert_item_not_read(ItemID, Opts) ->
    ReadResult = hb_store_arweave:read(
        hb_store_arweave:store_from_opts(Opts), ItemID),
    ?assertEqual(not_found, ReadResult),
    ok.

has_any_indexed_tx(Height, Opts) ->
    case fetch_block_header(Height, Opts) of
        {ok, Block} ->
            TXIDs = hb_maps:get(<<"txs">>, Block, [], Opts),
            lists:any(fun(TXID) -> is_tx_indexed(TXID, Opts) end, TXIDs);
        {error, _} ->
            false
    end.

highest_contiguous_indexed_block(StartBlock, MaxLookahead, Opts) ->
    highest_contiguous_indexed_block(
        StartBlock + 1,
        StartBlock + MaxLookahead,
        StartBlock,
        Opts
    ).

highest_contiguous_indexed_block(Current, Max, LastIndexed, _Opts)
        when Current > Max ->
    LastIndexed;
highest_contiguous_indexed_block(Current, Max, LastIndexed, Opts) ->
    case has_any_indexed_tx(Current, Opts) of
        true ->
            highest_contiguous_indexed_block(Current + 1, Max, Current, Opts);
        false ->
            LastIndexed
    end.

assert_indexed_range(From, To, _Opts) when From < To ->
    ok;
assert_indexed_range(From, To, Opts) ->
    ?assert(has_any_indexed_tx(From, Opts)),
    assert_indexed_range(From - 1, To, Opts).

block_marker_depth_2_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    Block = 1827942,
    {ok, Block} =
        hb_ao:resolve(
            <<"~copycat@1.0/arweave&from=",
                (hb_util:bin(Block))/binary, "&to=",
                (hb_util:bin(Block))/binary>>,
            Opts
        ),
    ?assert(is_block_indexed(Block, 2, Opts)),
    ?assertNot(is_block_indexed(Block, 3, Opts)),
    ok.

depth_1_normalizes_to_2_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    TX1 = #tx{
        format = 2,
        id = crypto:strong_rand_bytes(32),
        data_size = 100,
        tags = []
    },
    TX2 = #tx{
        format = 2,
        id = crypto:strong_rand_bytes(32),
        data_size = 200,
        tags = []
    },
    Tuples = [
        {{TX1, <<>>}, 100},
        {{TX2, <<>>}, 300}
    ],
    Result = process_block_txs(Tuples, 0, 1, 88888888, Opts),
    ?assertEqual(2, maps:get(achieved_depth, Result)),
    Height = 88888888,
    mark_block_indexed(Height, maps:get(achieved_depth, Result), Opts),
    ?assert(is_block_indexed(Height, 1, Opts)),
    ?assert(is_block_indexed(Height, 2, Opts)),
    ?assertNot(is_block_indexed(Height, 3, Opts)),
    ok.

block_marker_cutover_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    LowerBlock = 1827941,
    UpperBlock = 1827942,
    {ok, UpperBlock} =
        hb_ao:resolve(
            <<"~copycat@1.0/arweave&from=",
                (hb_util:bin(UpperBlock))/binary, "&to=",
                (hb_util:bin(UpperBlock))/binary>>,
            Opts
        ),
    Cutover = read_cutover_height(Opts),
    ?assertNotEqual(undefined, Cutover),
    ?assert(is_block_indexed(UpperBlock, 2, Opts)),
    ?assertNot(is_block_indexed(LowerBlock, 2, Opts)),
    ok.

achieved_depth_block_depth_3_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    Block = 1827942,
    {ok, Block} =
        hb_ao:resolve(
            <<"~copycat@1.0/arweave&from=",
                (hb_util:bin(Block))/binary, "&to=",
                (hb_util:bin(Block))/binary, "&depth=3">>,
            Opts
        ),
    ?assert(is_block_indexed(Block, 3, Opts)),
    ok.

invalid_bundle_bytes_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    StoreOpts = hb_store_arweave:store_from_opts(Opts),
    ?assertEqual(
        {error, invalid_bundle_header},
        index_full_bundle_bytes(<<"not a bundle">>, 0, 2, StoreOpts, <<0:256>>, Opts)
    ),
    ok.

small_block_depth_3_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    Block = 1889322,
    {ok, Block} =
        hb_ao:resolve(
            <<"~copycat@1.0/arweave&from=",
                (hb_util:bin(Block))/binary, "&to=",
                (hb_util:bin(Block))/binary, "&depth=3">>,
            Opts
        ),
    ?assert(is_block_indexed(Block, 3, Opts)),
    #{ <<"index-store">> := Store } = hb_store_arweave:store_from_opts(Opts),
    {ok, L1Bin} = hb_store:read(Store, block_items_path(Block, 1), Opts),
    ?assert(length(decode_item_ids(L1Bin)) > 0),
    {ok, L2Bin} = hb_store:read(Store, block_items_path(Block, 2), Opts),
    ?assert(length(decode_item_ids(L2Bin)) > 0),
    {ok, L3Bin} = hb_store:read(Store, block_items_path(Block, 3), Opts),
    L3IDs = decode_item_ids(L3Bin),
    ?assertEqual(3, length(L3IDs)),
    assert_item_read(
        <<"npAzk_BomjWBQQr_xnmlhdxjyl97EJnNv_MAaXffs1s">>,
        Opts),
    ok.

no_mismatch_flags_on_valid_bundles_test() ->
    {_TestStore, StoreOpts, Opts} = setup_index_opts(),
    Block = 1827942,
    {ok, Block} =
        hb_ao:resolve(
            <<"~copycat@1.0/arweave&from=",
                (hb_util:bin(Block))/binary, "&to=",
                (hb_util:bin(Block))/binary, "&depth=3">>,
            Opts
        ),
    #{ <<"index-store">> := IndexStore } = StoreOpts,
    ItemID = hb_util:native_id(
        <<"54K1ehEIKZxGSusgZzgbGYaHfllwWQ09-S9-eRUJg5Y">>),
    ?assertEqual(
        not_found,
        hb_store:read(
            IndexStore,
            hb_store_arweave_offset:mismatch_path(ItemID),
            Opts
        )
    ),
    ok.

mismatch_path_encoding_test() ->
    ID = crypto:strong_rand_bytes(32),
    Path = hb_store_arweave_offset:mismatch_path(ID),
    ?assert(binary:match(Path, <<"mismatch/">>) =/= nomatch),
    ok.

exact_marker_depth_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    Block = 1827942,
    {ok, Block} =
        hb_ao:resolve(
            <<"~copycat@1.0/arweave&from=",
                (hb_util:bin(Block))/binary, "&to=",
                (hb_util:bin(Block))/binary, "&depth=3">>,
            Opts
        ),
    #{ <<"index-store">> := Store } =
        hb_store_arweave:store_from_opts(Opts),
    {ok, StoredBin} =
        hb_store:read(Store, block_indexed_path(Block), Opts),
    StoredDepth = binary_to_integer(StoredBin),
    ?assertEqual(3, StoredDepth),
    ok.

fabricated_mismatch_test() ->
    {_TestStore, StoreOpts, Opts} = setup_index_opts(),
    {Priv, Pub} = ar_wallet:new(),
    Target = crypto:strong_rand_bytes(32),
    Anchor = crypto:strong_rand_bytes(32),
    Item = ar_bundles:sign_item(
        ar_bundles:new_item(Target, Anchor, [], <<"test data">>),
        {Priv, Pub}
    ),
    ItemBinary = ar_bundles:serialize(Item),
    RealID = crypto:hash(sha256, Item#tx.signature),
    FakeID = crypto:strong_rand_bytes(32),
    EncodedFakeID = hb_util:encode(FakeID),
    #{ <<"index-store">> := IndexStore } = StoreOpts,
    validate_and_flag_item_id(ItemBinary, FakeID, EncodedFakeID, IndexStore),
    {ok, StoredActualID} =
        hb_store:read(
            IndexStore,
            hb_store_arweave_offset:mismatch_path(FakeID),
            Opts
        ),
    ?assertEqual(RealID, StoredActualID),
    ?assertEqual(
        not_found,
        hb_store:read(
            IndexStore,
            hb_store_arweave_offset:mismatch_path(RealID),
            Opts
        )
    ),
    ok.

block_item_ids_depth_2_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    {ok, 1827942} =
        hb_ao:resolve(
            <<"~copycat@1.0/arweave&from=1827942&to=1827942">>,
            Opts
        ),
    #{ <<"index-store">> := Store } = hb_store_arweave:store_from_opts(Opts),
    {ok, L1Bin} = hb_store:read(Store, block_items_path(1827942, 1), Opts),
    L1IDs = decode_item_ids(L1Bin),
    ?assert(length(L1IDs) > 0),
    {ok, L2Bin} = hb_store:read(Store, block_items_path(1827942, 2), Opts),
    L2IDs = decode_item_ids(L2Bin),
    ?assert(length(L2IDs) > 0),
    L2Encoded = [hb_util:encode(ID) || ID <- L2IDs],
    Pos54K = index_of(<<"54K1ehEIKZxGSusgZzgbGYaHfllwWQ09-S9-eRUJg5Y">>, L2Encoded),
    PosOBK = index_of(<<"OBKr-7UrmjxFD-h-qP-XLuvCgtyuO_IDpBMgIytvusA">>, L2Encoded),
    ?assert(is_integer(Pos54K)),
    ?assert(is_integer(PosOBK)),
    ?assert(Pos54K < PosOBK),
    ?assertEqual(not_found, hb_store:read(Store, block_items_path(1827942, 3), Opts)),
    ok.

block_item_ids_depth_3_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    {ok, 1827942} =
        hb_ao:resolve(
            <<"~copycat@1.0/arweave&from=1827942&to=1827942&depth=3">>,
            Opts
        ),
    #{ <<"index-store">> := Store } = hb_store_arweave:store_from_opts(Opts),
    {ok, L1Bin} = hb_store:read(Store, block_items_path(1827942, 1), Opts),
    L1Count = length(decode_item_ids(L1Bin)),
    ?assertEqual(5, L1Count),
    {ok, L2Bin} = hb_store:read(Store, block_items_path(1827942, 2), Opts),
    L2Count = length(decode_item_ids(L2Bin)),
    ?assert(L2Count > 0),
    {ok, L3Bin} = hb_store:read(Store, block_items_path(1827942, 3), Opts),
    L3Count = length(decode_item_ids(L3Bin)),
    ?assert(L3Count >= 1),
    L3IDs = decode_item_ids(L3Bin),
    L3Encoded = [hb_util:encode(ID) || ID <- L3IDs],
    ?assert(lists:member(
        <<"8aJrRWtHcJvJ61qsH6agGkemzrtLw3W22xFrpCGAnTM">>, L3Encoded)),
    ok.

list_index_with_items_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    {ok, 1827942} =
        hb_ao:resolve(
            <<"~copycat@1.0/arweave&from=1827942&to=1827942">>,
            Opts
        ),
    {ok, ListResult} =
        hb_ao:resolve(
            <<"~copycat@1.0/arweave&from=1827942&to=1827942&mode=list">>,
            Opts
        ),
    Body = hb_json:decode(hb_maps:get(<<"body">>, ListResult)),
    BlockInfo = maps:get(<<"1827942">>, Body),
    ?assert(is_integer(maps:get(<<"depth">>, BlockInfo))),
    Items = maps:get(<<"items">>, BlockInfo),
    ?assert(maps:get(<<"1">>, Items) > 0),
    ?assert(maps:get(<<"2">>, Items) > 0),
    ok.

inventory_single_block_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    {ok, 1827942} =
        hb_ao:resolve(
            <<"~copycat@1.0/arweave&from=1827942&to=1827942">>,
            Opts
        ),
    {ok, InvResult} =
        hb_ao:resolve(
            <<"~copycat@1.0/arweave&from=1827942&to=1827942&mode=inventory">>,
            Opts
        ),
    Body = hb_json:decode(hb_maps:get(<<"body">>, InvResult)),
    BlockInfo = maps:get(<<"1827942">>, Body),
    ?assert(is_integer(maps:get(<<"depth">>, BlockInfo))),
    Items = maps:get(<<"items">>, BlockInfo),
    L1Items = maps:get(<<"1">>, Items),
    ?assert(is_list(L1Items)),
    ?assert(length(L1Items) > 0),
    L2Items = maps:get(<<"2">>, Items),
    ?assert(is_list(L2Items)),
    ?assert(length(L2Items) > 0),
    ?assertEqual(5, length(L1Items)),
    ?assert(lists:member(
        <<"54K1ehEIKZxGSusgZzgbGYaHfllwWQ09-S9-eRUJg5Y">>, L2Items)),
    ok.

inventory_range_test() ->
    {_TestStore, StoreOpts, Opts} = setup_index_opts(),
    #{ <<"index-store">> := Store } = StoreOpts,
    hb_store:write(Store, block_indexed_path(77777777), <<"2">>),
    hb_store:write(Store, block_items_path(77777777, 1), <<0:256>>),
    hb_store:write(Store, block_items_path(77777777, 2), <<>>),
    hb_store:write(Store, block_indexed_path(77777778), <<"2">>),
    hb_store:write(Store, block_items_path(77777778, 1), <<1:256>>),
    hb_store:write(Store, block_items_path(77777778, 2), <<>>),
    {ok, InvResult} = inventory_index(77777778, 77777777, Opts),
    Body = hb_json:decode(hb_maps:get(<<"body">>, InvResult)),
    ?assert(maps:is_key(<<"77777777">>, Body)),
    ?assert(maps:is_key(<<"77777778">>, Body)),
    ?assertEqual(2, maps:get(<<"depth">>, maps:get(<<"77777777">>, Body))),
    ?assertEqual(2, maps:get(<<"depth">>, maps:get(<<"77777778">>, Body))),
    ok.

decode_item_ids_validation_test() ->
    ?assertEqual([], decode_item_ids(<<>>)),
    GoodBin = <<0:256, 1:256>>,
    ?assertEqual(2, length(decode_item_ids(GoodBin))),
    BadBin = <<0:240>>,
    ?assertEqual({error, invalid_item_ids_binary}, decode_item_ids(BadBin)),
    ok.

corrupt_item_ids_read_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    #{ <<"index-store">> := Store } = hb_store_arweave:store_from_opts(Opts),
    Height = 99999999,
    hb_store:write(Store, block_indexed_path(Height), <<"2">>),
    hb_store:write(Store, block_items_path(Height, 1), <<0:256>>),
    hb_store:write(Store, block_items_path(Height, 2), <<0:240>>),
    Counts = read_block_item_counts(Height, Opts),
    ?assertEqual(1, maps:get(<<"1">>, Counts)),
    ?assertEqual(<<"corrupt">>, maps:get(<<"2">>, Counts)),
    IDs = read_block_item_ids(Height, Opts),
    ?assertEqual(1, length(maps:get(<<"1">>, IDs))),
    ?assertEqual(<<"corrupt">>, maps:get(<<"2">>, IDs)),
    ok.

parent_encode_decode_test() ->
    BlockEntry = encode_parent_entry(12345, block),
    ?assertEqual(<<0, 12345:64/big-unsigned>>, BlockEntry),
    BundleID = crypto:strong_rand_bytes(32),
    BundleEntry = encode_parent_entry(BundleID, bundle),
    ?assertEqual(<<1, BundleID:32/binary>>, BundleEntry),
    Combined = <<BlockEntry/binary, BundleEntry/binary>>,
    Decoded = hb_store_arweave:decode_parent_entries(Combined),
    ?assertEqual([{12345, block}, {BundleID, bundle}], Decoded),
    ok.

parent_not_found_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    StoreOpts2 = hb_store_arweave:store_from_opts(Opts),
    UnknownID = crypto:strong_rand_bytes(32),
    ?assertEqual(not_found, hb_store_arweave:read_parent(StoreOpts2, UnknownID)),
    ok.

parent_depth_2_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    Block = 1827942,
    {ok, Block} =
        hb_ao:resolve(
            <<"~copycat@1.0/arweave&from=",
                (hb_util:bin(Block))/binary, "&to=",
                (hb_util:bin(Block))/binary, "&depth=2">>,
            Opts
        ),
    StoreOpts2 = hb_store_arweave:store_from_opts(Opts),
    {ok, InvResult} =
        hb_ao:resolve(
            <<"~copycat@1.0/arweave&from=",
                (hb_util:bin(Block))/binary, "&to=",
                (hb_util:bin(Block))/binary, "&mode=inventory">>,
            Opts
        ),
    Body = hb_json:decode(hb_maps:get(<<"body">>, InvResult)),
    BlockInfo = maps:get(hb_util:bin(Block), Body),
    L1Items = maps:get(<<"1">>, maps:get(<<"items">>, BlockInfo)),
    L1ID = hb_util:decode(hd(L1Items)),
    {ok, [{Block, block}]} = hb_store_arweave:read_parent(StoreOpts2, L1ID),
    L2Items = maps:get(<<"2">>, maps:get(<<"items">>, BlockInfo)),
    case L2Items of
        [] -> ok;
        [FirstL2 | _] ->
            L2ID = hb_util:decode(FirstL2),
            {ok, [{L2Parent, bundle}]} =
                hb_store_arweave:read_parent(StoreOpts2, L2ID),
            ?assert(lists:member(
                hb_util:encode(L2Parent), L1Items))
    end,
    ok.

parent_depth_3_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    Block = 1889322,
    {ok, Block} =
        hb_ao:resolve(
            <<"~copycat@1.0/arweave&from=",
                (hb_util:bin(Block))/binary, "&to=",
                (hb_util:bin(Block))/binary, "&depth=3">>,
            Opts
        ),
    StoreOpts2 = hb_store_arweave:store_from_opts(Opts),
    {ok, InvResult} =
        hb_ao:resolve(
            <<"~copycat@1.0/arweave&from=",
                (hb_util:bin(Block))/binary, "&to=",
                (hb_util:bin(Block))/binary, "&mode=inventory">>,
            Opts
        ),
    Body = hb_json:decode(hb_maps:get(<<"body">>, InvResult)),
    BlockInfo = maps:get(hb_util:bin(Block), Body),
    L3Items = maps:get(<<"3">>, maps:get(<<"items">>, BlockInfo)),
    ?assert(length(L3Items) > 0),
    L2Items = maps:get(<<"2">>, maps:get(<<"items">>, BlockInfo)),
    L3ID = hb_util:decode(hd(L3Items)),
    {ok, [{L3Parent, bundle}]} =
        hb_store_arweave:read_parent(StoreOpts2, L3ID),
    ?assert(lists:member(hb_util:encode(L3Parent), L2Items)),
    ok.

parent_corrupt_data_test() ->
    ?assertEqual([], hb_store_arweave:decode_parent_entries(<<>>)),
    ?assertEqual(
        {error, corrupt_parent_data},
        hb_store_arweave:decode_parent_entries(<<5, 1, 2, 3>>)),
    Truncated = <<0, 1, 2, 3>>,
    ?assertEqual(
        {error, corrupt_parent_data},
        hb_store_arweave:decode_parent_entries(Truncated)),
    ValidThenCorrupt = <<0, 100:64/big-unsigned, 99>>,
    ?assertEqual(
        {error, corrupt_parent_data},
        hb_store_arweave:decode_parent_entries(ValidThenCorrupt)),
    ok.

parent_endpoint_block_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    Block = 1827942,
    {ok, Block} =
        hb_ao:resolve(
            <<"~copycat@1.0/arweave&from=",
                (hb_util:bin(Block))/binary, "&to=",
                (hb_util:bin(Block))/binary, "&depth=2">>,
            Opts
        ),
    {ok, InvResult} =
        hb_ao:resolve(
            <<"~copycat@1.0/arweave&from=",
                (hb_util:bin(Block))/binary, "&to=",
                (hb_util:bin(Block))/binary, "&mode=inventory">>,
            Opts
        ),
    InvBody = hb_json:decode(hb_maps:get(<<"body">>, InvResult)),
    BlockInfo = maps:get(hb_util:bin(Block), InvBody),
    L1Items = maps:get(<<"1">>, maps:get(<<"items">>, BlockInfo)),
    L1EncodedID = hd(L1Items),
    {ok, ParentResult} =
        hb_ao:resolve(
            <<"~arweave@2.9/parent=", L1EncodedID/binary>>,
            Opts
        ),
    ?assertEqual(
        <<"application/json">>,
        hb_maps:get(<<"content-type">>, ParentResult)),
    Body = hb_json:decode(hb_maps:get(<<"body">>, ParentResult)),
    Parents = maps:get(<<"parents">>, Body),
    ?assertEqual(1, length(Parents)),
    [Entry] = Parents,
    ?assertEqual(<<"block">>, maps:get(<<"type">>, Entry)),
    ?assertEqual(Block, maps:get(<<"height">>, Entry)),
    ok.

parent_endpoint_bundle_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    Block = 1827942,
    {ok, Block} =
        hb_ao:resolve(
            <<"~copycat@1.0/arweave&from=",
                (hb_util:bin(Block))/binary, "&to=",
                (hb_util:bin(Block))/binary, "&depth=2">>,
            Opts
        ),
    {ok, InvResult} =
        hb_ao:resolve(
            <<"~copycat@1.0/arweave&from=",
                (hb_util:bin(Block))/binary, "&to=",
                (hb_util:bin(Block))/binary, "&mode=inventory">>,
            Opts
        ),
    InvBody = hb_json:decode(hb_maps:get(<<"body">>, InvResult)),
    BlockInfo = maps:get(hb_util:bin(Block), InvBody),
    L1Items = maps:get(<<"1">>, maps:get(<<"items">>, BlockInfo)),
    L2Items = maps:get(<<"2">>, maps:get(<<"items">>, BlockInfo)),
    ?assert(length(L2Items) > 0),
    L2EncodedID = hd(L2Items),
    {ok, ParentResult} =
        hb_ao:resolve(
            <<"~arweave@2.9/parent=", L2EncodedID/binary>>,
            Opts
        ),
    ?assertEqual(
        <<"application/json">>,
        hb_maps:get(<<"content-type">>, ParentResult)),
    Body = hb_json:decode(hb_maps:get(<<"body">>, ParentResult)),
    [Entry] = maps:get(<<"parents">>, Body),
    ?assertEqual(<<"bundle">>, maps:get(<<"type">>, Entry)),
    ParentID = maps:get(<<"id">>, Entry),
    ?assert(lists:member(ParentID, L1Items)),
    ok.

parent_endpoint_not_found_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    FakeID = <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,
    ?assertEqual(
        {error, not_found},
        hb_ao:resolve(
            <<"~arweave@2.9/parent=", FakeID/binary>>,
            Opts
        )
    ),
    ok.

index_of(Elem, List) -> index_of(Elem, List, 1).

index_of(_Elem, [], _N) -> not_found;
index_of(Elem, [Elem | _], N) -> N;
index_of(Elem, [_ | Rest], N) -> index_of(Elem, Rest, N + 1).