# dev_dedup

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_dedup.erl)

A device that deduplicates messages in an evaluation stream, returning
status `skip` if the message has already been seen.
This device is typically used to ensure that a message is only executed
once, even if assigned multiple times, upon a `~process@1.0` evaluation.
It can, however, be used in many other contexts.
This device honors the `pass` key if it is present in the message. If so,
it will only run on the first pass. Additionally, the device supports
a `subject-key` key that allows the caller to specify the key whose ID
should be used for deduplication. If the `subject-key` key is not present,
the device will use the `body` of the request as the subject. If the key is
set to `request`, the device will use the entire request itself as the
subject.
This device runs on the first pass of the `compute` key call if executed
in a stack, and not in subsequent passes. Currently the device stores its
list of already seen items in memory, but at some point it will likely make
sense to drop them in the cache.

---

## Exported Functions

- `info/1`

---

### info

A device that deduplicates messages in an evaluation stream, returning

```erlang
info(_M1) ->
    #{
        default => fun handle/4,
        exclude => [keys, set, id, commit]
    }.
```

### handle

Forward the keys and `set` functions to the message device, handle all

```erlang
handle(<<"keys">>, M1, _M2, _Opts) ->
    dev_message:keys(M1);
```

### handle

Forward the keys and `set` functions to the message device, handle all

```erlang
handle(<<"set">>, M1, M2, Opts) ->
    dev_message:set(M1, M2, Opts);
```

### handle

Forward the keys and `set` functions to the message device, handle all

```erlang
handle(Key, M1, M2, Opts) ->
    ?event({dedup_handle, {key, Key}, {msg1, M1}, {msg2, M2}}),
    % Find the relevant parameters from the messages. We search for the
    % `dedup-key' key in the first message, and use that value as the key to
    % look for in the second message.
```

### dedup_test

```erlang
dedup_test() ->
    hb:init(),
    % Create a stack with a dedup device and 2 devices that will append to a
    % `Result' key.
```

### dedup_with_multipass_test

```erlang
dedup_with_multipass_test() ->
    % Create a stack with a dedup device and 2 devices that will append to a
    % `Result' key and a `Multipass' device that will repeat the message for 
    % an additional pass. We want to ensure that Multipass is not hindered by
    % the dedup device.
```

---

*Generated from [dev_dedup.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_dedup.erl)*
