# dev_query_arweave

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_query_arweave.erl)

An implementation of the Arweave GraphQL API, inside the `~query@1.0`
device.

---

## Exported Functions

- `query/4`

---

### query

An implementation of the Arweave GraphQL API, inside the `~query@1.0`
The arguments that are supported by the Arweave GraphQL API.
Handle an Arweave GraphQL query for either transactions or blocks.

```erlang
query(List, <<"edges">>, _Args, _Opts) ->
    {ok, [{ok, Msg} || Msg <- List]};
```

### query

An implementation of the Arweave GraphQL API, inside the `~query@1.0`
The arguments that are supported by the Arweave GraphQL API.
Handle an Arweave GraphQL query for either transactions or blocks.

```erlang
query(Msg, <<"node">>, _Args, _Opts) ->
    {ok, Msg};
```

### query

An implementation of the Arweave GraphQL API, inside the `~query@1.0`
The arguments that are supported by the Arweave GraphQL API.
Handle an Arweave GraphQL query for either transactions or blocks.

```erlang
query(Obj, <<"transaction">>, Args, Opts) ->
    case query(Obj, <<"transactions">>, Args, Opts) of
        {ok, []} -> {ok, null};
        {ok, [Msg|_]} -> {ok, Msg}
    end;
```

### query

An implementation of the Arweave GraphQL API, inside the `~query@1.0`
The arguments that are supported by the Arweave GraphQL API.
Handle an Arweave GraphQL query for either transactions or blocks.

```erlang
query(Obj, <<"transactions">>, Args, Opts) ->
    ?event({transactions_query,
        {object, Obj},
        {field, <<"transactions">>},
        {args, Args}
    }),
    Matches = match_args(Args, Opts),
    ?event({transactions_matches, Matches}),
    Messages =
        lists:filtermap(
            fun(Match) ->
                case hb_cache:read(Match, Opts) of
                    {ok, Msg} -> {true, Msg};
                    not_found -> false
                end
            end,
            Matches
        ),
    {ok, Messages};
```

### query

An implementation of the Arweave GraphQL API, inside the `~query@1.0`
The arguments that are supported by the Arweave GraphQL API.
Handle an Arweave GraphQL query for either transactions or blocks.

```erlang
query(Obj, <<"block">>, Args, Opts) ->
    case query(Obj, <<"blocks">>, Args, Opts) of
        {ok, []} -> {ok, null};
        {ok, [Msg|_]} -> {ok, Msg}
    end;
```

### query

An implementation of the Arweave GraphQL API, inside the `~query@1.0`
The arguments that are supported by the Arweave GraphQL API.
Handle an Arweave GraphQL query for either transactions or blocks.

```erlang
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
```

### query

```erlang
query(Block, <<"previous">>, _Args, Opts) ->
    {ok, hb_maps:get(<<"previous_block">>, Block, null, Opts)};
```

### query

```erlang
query(Block, <<"height">>, _Args, Opts) ->
    {ok, hb_maps:get(<<"height">>, Block, null, Opts)};
```

### query

```erlang
query(Block, <<"timestamp">>, _Args, Opts) ->
    {ok, hb_maps:get(<<"timestamp">>, Block, null, Opts)};
```

### query

```erlang
query(Msg, <<"signature">>, _Args, Opts) ->
    % Return the signature of the transaction.
```

### query

```erlang
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
```

### query

```erlang
query(#{ <<"key">> := Key }, <<"key">>, _Args, _Opts) ->
    {ok, Key};
```

### query

```erlang
query(#{ <<"address">> := Address }, <<"address">>, _Args, _Opts) ->
    {ok, Address};
```

### query

```erlang
query(Msg, <<"fee">>, _Args, Opts) ->
    {ok, hb_maps:get(<<"fee">>, Msg, 0, Opts)};
```

### query

```erlang
query(Msg, <<"quantity">>, _Args, Opts) ->
    {ok, hb_maps:get(<<"quantity">>, Msg, 0, Opts)};
```

### query

```erlang
query(Number, <<"winston">>, _Args, _Opts) when is_number(Number) ->
    {ok, Number};
```

### query

```erlang
query(Msg, <<"recipient">>, _Args, Opts) ->
    case find_field_key(<<"field-target">>, Msg, Opts) of
        {ok, null} -> {ok, <<"">>};
        OkRes -> OkRes
    end;
```

### query

```erlang
query(Msg, <<"anchor">>, _Args, Opts) ->
    case find_field_key(<<"field-anchor">>, Msg, Opts) of
        {ok, null} -> {ok, <<"">>};
        {ok, Anchor} -> {ok, hb_util:human_id(Anchor)}
    end;
```

### query

```erlang
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
```

### query

```erlang
query(#{ <<"data">> := Data }, <<"size">>, _Args, _Opts) ->
    {ok, byte_size(Data)};
```

### query

```erlang
query(#{ <<"type">> := Type }, <<"type">>, _Args, _Opts) ->
    {ok, Type};
```

### query

Find and return a value from the fields of a message (from its

```erlang
query(Obj, Field, Args, _Opts) ->
    ?event({unimplemented_transactions_query,
        {object, Obj},
        {field, Field},
        {args, Args}
    }),
    {ok, <<"Not implemented.">>}.
```

### find_field_key

Find and return a value from the fields of a message (from its

```erlang
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
```

### match_args

Progressively generate matches from each argument for a transaction

```erlang
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
```

### match_args

```erlang
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
```

### match_args

```erlang
match_args([{Field, X} | Rest], Acc, Opts) ->
    MatchRes = match(Field, X, Opts),
    ?event({match, {field, Field}, {arg, X}, {match_res, MatchRes}}),
    case MatchRes of
        {ok, Result} ->
            match_args(Rest, [Result | Acc], Opts);
        _Error ->
            match_args(Rest, Acc, Opts)
    end.
```

### match

Generate a match upon `tags` in the arguments, if given.

```erlang
match(_, null, _) -> ignore;
```

### match

Generate a match upon `tags` in the arguments, if given.

```erlang
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
```

### match

Generate a match upon `tags` in the arguments, if given.

```erlang
match(<<"id">>, ID, _Opts) ->
    {ok, [ID]};
```

### match

Generate a match upon `tags` in the arguments, if given.

```erlang
match(<<"ids">>, IDs, _Opts) ->
    {ok, IDs};
```

### match

Generate a match upon `tags` in the arguments, if given.

```erlang
match(<<"tags">>, Tags, Opts) ->
    hb_cache:match(dev_query_graphql:keys_to_template(Tags), Opts);
```

### match

Generate a match upon `tags` in the arguments, if given.

```erlang
match(<<"owners">>, Owners, Opts) ->
    {ok, matching_commitments(<<"committer">>, Owners, Opts)};
```

### match

Generate a match upon `tags` in the arguments, if given.

```erlang
match(<<"owner">>, Owner, Opts) ->
    Res =  matching_commitments(<<"committer">>, Owner, Opts),
    ?event({match_owner, Owner, Res}),
    {ok, Res};
```

### match

Generate a match upon `tags` in the arguments, if given.

```erlang
match(<<"recipients">>, Recipients, Opts) ->
    {ok, matching_commitments(<<"field-target">>, Recipients, Opts)};
```

### match

Generate a match upon `tags` in the arguments, if given.

```erlang
match(UnsupportedFilter, _, _) ->
    throw({unsupported_query_filter, UnsupportedFilter}).
```

### matching_commitments

Return the base IDs for messages that have a matching commitment.

```erlang
matching_commitments(Field, Values, Opts) when is_list(Values) ->
    hb_util:unique(lists:flatten(
        lists:map(
            fun(Value) -> matching_commitments(Field, Value, Opts) end,
            Values
        )
    ));
```

### matching_commitments

Return the base IDs for messages that have a matching commitment.

```erlang
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
```

### commitment_id_to_base_id

Convert a commitment message's ID to a base ID.

```erlang
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
```

### all_ids

Find all IDs for a message, by any of its other IDs.

```erlang
all_ids(ID, Opts) ->
    Store = hb_opts:get(store, no_store, Opts),
    case hb_store:list(Store, << ID/binary, "/commitments">>) of
        {ok, []} -> [ID];
        {ok, CommitmentIDs} -> CommitmentIDs;
        _ -> []
    end.
```

### scope

Scope the stores used for block matching. The searched stores can be

```erlang
scope(Opts) ->
    Scope = hb_opts:get(query_arweave_scope, [local], Opts),
    hb_store:scope(Opts, Scope).
```

### resolve_ids

Resolve a list of IDs to their store paths, using the stores provided.

```erlang
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
```

---

*Generated from [dev_query_arweave.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_query_arweave.erl)*
