# dev_copycat_arweave

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_copycat_arweave.erl)

A `~copycat@1.0` engine that fetches block data from an Arweave node for
replication. This engine works in _reverse_ chronological order by default,
fetching blocks from the latest known block towards the Genesis block. The
node avoids retrieving blocks that are already present in the cache using
`~arweave@2.9-pre`'s built-in caching mechanism.

---

## Exported Functions

- `arweave/3`

---

### arweave

A `~copycat@1.0` engine that fetches block data from an Arweave node for
Fetch blocks from an Arweave node between a given range, or from the

```erlang
arweave(_Base, Request, Opts) ->
    {From, To} = parse_range(Request, Opts),
    fetch_blocks(Request, From, To, Opts).
```

### parse_range

Parse the range from the request.

```erlang
parse_range(Request, Opts) ->
    From =
        case hb_maps:find(<<"from">>, Request, Opts) of
            {ok, Height} -> Height;
            error ->
                {ok, LatestHeight} =
                    hb_ao:resolve(
                        <<?ARWEAVE_DEVICE/binary, "/current/height">>,
                        Opts
                    ),
                LatestHeight
        end,
    To = hb_maps:get(<<"to">>, Request, 0, Opts),
    {From, To}.
```

### fetch_blocks

Fetch blocks from an Arweave node between a given range.

```erlang
fetch_blocks(Req, Current, Current, _Opts) ->
    ?event(copycat_arweave,
        {arweave_block_indexing_completed,
            {reached_target, Current},
            {initial_request, Req}
        }
    ),
    {ok, Current};
```

### fetch_blocks

Fetch blocks from an Arweave node between a given range.

```erlang
fetch_blocks(Req, Current, To, Opts) ->
    BlockRes =
        hb_ao:resolve(
            <<
                ?ARWEAVE_DEVICE/binary,
                "/block=",
                (hb_util:bin(Current))/binary
            >>,
            Opts
        ),
    process_block(BlockRes, Req, Current, To, Opts),
    fetch_blocks(Req, Current - 1, To, Opts).
```

### process_block

Process a block.

```erlang
process_block(BlockRes, _Req, Current, To, _Opts) ->
    case BlockRes of
        {ok, _} ->
            ?event(
                copycat_short,
                {arweave_block_cached,
                    {height, Current},
                    {target, To}
                }
            );
        {error, not_found} ->
            ?event(
                copycat_short,
                {arweave_block_not_found,
                    {height, Current},
                    {target, To}
                }
            )
```

---

*Generated from [dev_copycat_arweave.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_copycat_arweave.erl)*
