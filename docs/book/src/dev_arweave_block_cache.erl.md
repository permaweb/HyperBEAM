# dev_arweave_block_cache

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_arweave_block_cache.erl)

A module that performs caching operations for the Arweave device, 
focused on ensuring that block metadata is queriable via pseudo-paths.

---

## Exported Functions

- `heights/1`
- `latest/1`
- `path/2`
- `read/2`
- `write/2`

---

### latest

A module that performs caching operations for the Arweave device, 
The pseudo-path prefix which the Arweave block cache should use.
Get the latest block from the cache.

```erlang
latest(Opts) ->
    case heights(Opts) of
        {ok, []} ->
            ?event(arweave_cache, no_blocks_in_cache),
            not_found;
        {ok, Blocks} ->
            Latest = lists:max(Blocks),
            ?event(arweave_cache, {latest_block_from_cache, {latest, Latest}}),
            {ok, Latest}
    end.
```

### heights

Get the list of blocks from the cache.

```erlang
heights(Opts) ->
    AllBlocks =
        hb_cache:list_numbered(
            hb_store:path(hb_opts:get(store, no_viable_store, Opts), [
                ?ARWEAVE_BLOCK_CACHE_PREFIX,
                <<"block">>,
                <<"height">>
            ]),
            Opts
        ),
    ?event(arweave_cache, {listed_blocks, length(AllBlocks)}),
    {ok, AllBlocks}.
```

### read

Read a block from the cache.

```erlang
read(Block, Opts) ->
    Res = hb_cache:read(path(Block, Opts), Opts),
    ?event(arweave_cache, {read_block, {reference, Block}, {result, Res}}),
    Res.
```

### path

Return the path of a block that will be used in the cache.

```erlang
path(Block, Opts) when is_integer(Block) ->
    hb_store:path(hb_opts:get(store, no_viable_store, Opts), [
        ?ARWEAVE_BLOCK_CACHE_PREFIX,
        <<"block">>,
        <<"height">>,
        hb_util:bin(Block)
    ]).
```

### write

Write a block to the cache and create pseudo-paths for it.

```erlang
write(Block, Opts) ->
    {ok, Height} = hb_maps:find(<<"height">>, Block, Opts),
    {ok, BlockID} = hb_maps:find(<<"indep_hash">>, Block, Opts),
    {ok, BlockHash} = hb_maps:find(<<"hash">>, Block, Opts),
    {ok, MsgID} = hb_cache:write(Block, Opts),
    % Link the independent hash and the dependent hash to the written AO-Core
    % message ID.
```

---

*Generated from [dev_arweave_block_cache.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_arweave_block_cache.erl)*
