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
query(List, <<"edges">>, _Args, _Opts) ->
    {ok, [{ok, Msg} || Msg <- List]};
query(Msg, <<"node">>, _Args, _Opts) ->
    {ok, Msg};
query(Obj, <<"transaction">>, Args, Opts) ->
    case query(Obj, <<"transactions">>, Args, Opts) of
        {ok, []} -> {ok, null};
        {ok, [Msg|_]} -> {ok, Msg}
    end;
query(Obj, <<"transactions">>, Args, Opts) ->
    ?event({transactions_query,
        {object, Obj},
        {field, <<"transactions">>},
        {args, Args}
    }),
    Matches = match_args(Args, Opts),
    Ordered =
        case annotate_offsets(Matches, Opts) of
            unavailable -> Matches;
            Annotated ->
                Order = maps:get(<<"sort">>, Args, <<"HEIGHT_DESC">>),
                remove_annotations(
                    sort_offset_annotated(
                        filter_offset_annotated(
                            Annotated,
                            maps:get(<<"block">>, Args, undefined),
                            Opts
                        ),
                        Order,
                        Opts
                    )
                )
        end,
    ?event({transactions_matches, Matches}),
    Messages =
        lists:filtermap(
            fun(Match) ->
                case hb_cache:read(Match, Opts) of
                    {ok, Msg} -> {true, Msg};
                    not_found -> false
                end
            end,
            Ordered
        ),
    {ok, Messages};
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
                    not_found -> false
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
                    Key = dev_codec_httpsig_keyid:remove_scheme_prefix(KeyID),
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
        {ok, Anchor} -> {ok, hb_util:human_id(Anchor)}
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

%% @doc Sort messages by their block height, if Arweave index store is available.
%% Takes a list of IDs and returns the same list sorted by block height. IDs that
%% do not have an offset are always placed at the end of the list -- regardless
%% of the sort order.
sort_offset_annotated(IDs, SortOrder, _Opts) ->
    {WithOffset, WithoutOffset} =
        lists:partition(
            fun({Offset, _, _}) -> Offset =/= undefined end,
            IDs
        ),
    Sorted =
        case SortOrder of
            <<"HEIGHT_ASC">> -> lists:keysort(1, WithOffset);
            _ -> lists:reverse(lists:keysort(1, WithOffset))
        end,
    ?event(
        {order_by_block,
            {sort_order, SortOrder},
            {with_offset, length(WithOffset)},
            {without_offset, length(WithoutOffset)}
        }
    ),
    Sorted ++ WithoutOffset.

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
                    not_found -> 0
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
                    not_found -> infinity
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
%% block is not cached locally, falls back to `dev_arweave:block/2'.
read_block(Height, Opts) ->
    case dev_arweave_block_cache:read(Height, Opts) of
        {ok, Block} -> {ok, Block};
        not_found ->
            case hb_opts:get(query_arweave_remote_block_ranges, true, Opts) of
                true ->
                    ?event({read_block_remote, {height, Height}}),
                    dev_arweave:block(#{}, #{ <<"block">> => Height }, Opts);
                _ -> not_found
            end
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
match_args([], Results, Opts) ->
    ?event({match_args_results, Results}),
    Matches =
        lists:foldl(
            fun(Result, Acc) ->
                hb_util:list_with(resolve_ids(Result, Opts), Acc)
            end,
            resolve_ids(hd(Results), Opts),
            tl(Results)
        ),
    hb_util:unique(
        lists:flatten(
            [
                all_ids(ID, Opts)
            ||
                ID <- Matches
            ]
        )
    );
match_args([{Field, X} | Rest], Acc, Opts) ->
    MatchRes = match(Field, X, Opts),
    ?event({match, {field, Field}, {arg, X}, {match_res, MatchRes}}),
    case MatchRes of
        {ok, Result} ->
            match_args(Rest, [Result | Acc], Opts);
        _Error ->
            match_args(Rest, Acc, Opts)
    end.

%% @doc Generate a match upon `tags' in the arguments, if given.
match(_, null, _) -> ignore;
match(<<"height">>, Heights, Opts) ->
    Min = hb_maps:get(<<"min">>, Heights, 0, Opts),
    Max =
        case hb_maps:find(<<"max">>, Heights, Opts) of
            {ok, GivenMax} -> GivenMax;
            error ->
                {ok, Latest} = dev_arweave_block_cache:latest(Opts),
                Latest
        end,
    #{ store := ScopedStores } = scope(Opts),
    {ok,
        lists:filtermap(
            fun(Height) ->
                Path = dev_arweave_block_cache:path(Height, Opts),
                case hb_store:type(ScopedStores, Path) of
                    not_found -> false;
                    _ -> {true, hb_store:resolve(ScopedStores, Path)}
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
annotate_offsets(IDs, Opts) ->
    case hb_store_arweave:store_from_opts(Opts) of
        no_store -> unavailable;
        StoreOpts -> annotate_offsets(IDs, StoreOpts, Opts)
    end.
annotate_offsets(IDs, StoreOpts, _Opts) ->
    lists:map(
        fun(ID) ->
            case hb_store_arweave:read_offset(StoreOpts, ID) of
                {ok, #{ <<"start-offset">> := Offset, <<"length">> := Length }} ->
                    {Offset, Length, ID};
                _ -> {undefined, undefined, ID}
            end
        end,
        IDs
    ).

%% @doc Remove all annotations of start offset and length from a list of IDs.
remove_annotations(IDs) -> lists:map(fun({_, _, ID}) -> ID end, IDs).

%% @doc Apply the `block' height range as a post-filter over candidate IDs.
%% Each candidate's offset is checked against the block range boundaries,
%% avoiding materialisation of the full store.
filter_offset_annotated(IDs, HeightRange, _Opts)
        when HeightRange =:= undefined orelse HeightRange =:= null ->
    IDs;
filter_offset_annotated(IDs, Heights, Opts) ->
    {StartOffset, EndOffset} =
        block_range_to_offset_range(Heights, Opts),
    Filtered =
        lists:filter(
            fun({IDOffset, Length, _}) ->
                ((StartOffset =:= 0) orelse (IDOffset >= StartOffset)) andalso
                    ((EndOffset =:= infinity) orelse (IDOffset + Length =< EndOffset))
            end,
            IDs
        ),
    ?event({filtered_out_of_range, length(IDs) - length(Filtered)}),
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
        not_found -> not_found
    end.

%% @doc Convert a commitment message's ID to a base ID.
commitment_id_to_base_id(ID, Opts) ->
    Store = hb_opts:get(store, no_store, Opts),
    ?event({commitment_id_to_base_id, ID}),
    case hb_store:read(Store, << ID/binary, "/signature">>) of
        {ok, EncSig} ->
            Sig = hb_util:decode(EncSig),
            ?event({commitment_id_to_base_id_sig, Sig}),
            hb_util:encode(hb_crypto:sha256(Sig));
        not_found -> not_found
    end.

%% @doc Find all IDs for a message, by any of its other IDs.
all_ids(ID, Opts) ->
    Store = hb_opts:get(store, no_store, Opts),
    case hb_store:list(Store, << ID/binary, "/commitments">>) of
        {ok, []} -> [ID];
        {ok, CommitmentIDs} -> CommitmentIDs;
        _ -> [ID]
    end.

%% @doc Scope the stores used for block matching. The searched stores can be
%% scoped by setting the `query_arweave_scope' option.
scope(Opts) ->
    Scope = hb_opts:get(query_arweave_scope, [local], Opts),
    hb_store:scope(Opts, Scope).

%% @doc Resolve a list of IDs to their store paths, using the stores provided.
resolve_ids(IDs, Opts) ->
    Scoped = scope(Opts),
    lists:map(
        fun(ID) ->
            case hb_cache:read(ID, Opts) of
                {ok, Msg} -> hb_message:id(Msg, uncommitted, Scoped);
                not_found -> ID
            end
        end,
        IDs
    ).