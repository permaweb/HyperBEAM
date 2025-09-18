# dev_process_cache

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_process_cache.erl)

A wrapper around the hb_cache module that provides a more
convenient interface for reading the result of a process at a given slot or
message ID.

---

## Exported Functions

- `latest/2`
- `latest/3`
- `latest/4`
- `read/2`
- `read/3`
- `write/4`

---

### read

A wrapper around the hb_cache module that provides a more
Read the result of a process at a given slot.

```erlang
read(ProcID, Opts) ->
    hb_util:ok(latest(ProcID, Opts)).
```

### read

```erlang
read(ProcID, SlotRef, Opts) ->
    ?event({reading_computed_result, ProcID, SlotRef}),
    Path = path(ProcID, SlotRef, Opts),
    hb_cache:read(Path, Opts).
```

### write

Write a process computation result to the cache.

```erlang
write(ProcID, Slot, Msg, Opts) ->
    % Write the item to the cache in the root of the store.
```

### path

Calculate the path of a result, given a process ID and a slot.

```erlang
path(ProcID, Ref, Opts) ->
    path(ProcID, Ref, [], Opts).
```

### path

```erlang
path(ProcID, Ref, PathSuffix, Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    hb_store:path(
        Store,
        [
            <<"computed">>,
            hb_util:human_id(ProcID)
        ] ++
        case Ref of
            Int when is_integer(Int) -> ["slot", integer_to_binary(Int)];
            root -> [];
            slot_root -> ["slot"];
            _ -> [Ref]
        end ++ PathSuffix
    ).
```

### latest

Retrieve the latest slot for a given process. Optionally state a limit

```erlang
latest(ProcID, Opts) -> latest(ProcID, [], Opts).
```

### latest

Retrieve the latest slot for a given process. Optionally state a limit

```erlang
latest(ProcID, RequiredPath, Opts) ->
    latest(ProcID, RequiredPath, undefined, Opts).
```

### latest

```erlang
latest(ProcID, RawRequiredPath, Limit, Opts) ->
    ?event(
        {latest_called,
            {proc_id, ProcID},
            {required_path, RawRequiredPath},
            {limit, Limit}
        }
    ),
    % Convert the required path to a list of _binary_ keys.
```

### first_with_path

Find the latest assignment with the requested path suffix.

```erlang
first_with_path(ProcID, RequiredPath, Slots, Opts) ->
    first_with_path(
        ProcID,
        RequiredPath,
        Slots,
        Opts,
        hb_opts:get(store, no_viable_store, Opts)
    ).
```

### first_with_path

```erlang
first_with_path(_ProcID, _Required, [], _Opts, _Store) ->
    not_found;
```

### first_with_path

```erlang
first_with_path(ProcID, RequiredPath, [Slot | Rest], Opts, Store) ->
    RawPath = path(ProcID, Slot, RequiredPath, Opts),
    ResolvedPath = hb_store:resolve(Store, RawPath),
    ?event({trying_slot, {slot, Slot}, {path, RawPath}, {resolved_path, ResolvedPath}}),
    case hb_store:type(Store, ResolvedPath) of
        not_found ->
            first_with_path(ProcID, RequiredPath, Rest, Opts, Store);
        _ ->
            Slot
    end.
```

### process_cache_suite_test_

```erlang
process_cache_suite_test_() ->
    hb_store:generate_test_suite(
        [
            {"write and read process outputs", fun test_write_and_read_output/1},
            {"find latest output (with path)", fun find_latest_outputs/1}
        ],
        [
            {Name, Opts}
        ||
            {Name, Opts} <- hb_store:test_stores()
        ]
    ).
```

### test_write_and_read_output

Test for writing multiple computed outputs, then getting them by

```erlang
test_write_and_read_output(Opts) ->
    Proc = hb_cache:test_signed(
        #{ <<"test-item">> => hb_cache:test_unsigned(<<"test-body-data">>) }),
    ProcID = hb_util:human_id(hb_ao:get(id, Proc)),
    Item1 = hb_cache:test_signed(<<"Simple signed output #1">>),
    Item2 = hb_cache:test_unsigned(<<"Simple unsigned output #2">>),
    {ok, Path0} = write(ProcID, 0, Item1, Opts),
    {ok, Path1} = write(ProcID, 1, Item2, Opts),
    {ok, DirectReadItem1} = hb_cache:read(Path0, Opts),
    ?assert(hb_message:match(Item1, DirectReadItem1)),
    {ok, DirectReadItem2} = hb_cache:read(Path1, Opts),
    ?assert(hb_message:match(Item2, DirectReadItem2)),
    {ok, ReadItem1BySlotNum} = read(ProcID, 0, Opts),
    ?assert(hb_message:match(Item1, ReadItem1BySlotNum)),
    {ok, ReadItem2BySlotNum} = read(ProcID, 1, Opts),
    ?assert(hb_message:match(Item2, ReadItem2BySlotNum)),
    {ok, ReadItem1ByID} =
        read(ProcID, hb_util:human_id(hb_ao:get(id, Item1)), Opts),
    ?assert(hb_message:match(Item1, ReadItem1ByID)),
    {ok, ReadItem2ByID} =
        read(ProcID, hb_util:human_id(hb_message:id(Item2, all)), Opts),
    ?assert(hb_message:match(Item2, ReadItem2ByID)).
```

### find_latest_outputs

Test for retrieving the latest computed output for a process.

```erlang
find_latest_outputs(Opts) ->
    % Create test environment.
```

---

*Generated from [dev_process_cache.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_process_cache.erl)*
