# dev_name

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_name.erl)

A device for resolving names to their corresponding values, through the
use of a `resolver` interface. Each `resolver` is a message that can be
given a `key` and returns an associated value. The device will attempt to
match the key against each resolver in turn, and return the value of the
first resolver that matches.

---

## Exported Functions

- `info/1`

---

### info

A device for resolving names to their corresponding values, through the
Configure the `default` key to proxy to the `resolver/4` function.

```erlang
info(_) ->
    #{
        default => fun resolve/4,
        excludes => [<<"keys">>, <<"set">>]
    }.
```

### resolve

Resolve a name to its corresponding value. The name is given by the key

```erlang
resolve(Key, _, Req, Opts) ->
    Resolvers = hb_opts:get(name_resolvers, [], Opts),
    ?event({resolvers, Resolvers}),
    case match_resolver(Key, Resolvers, Opts) of
        {ok, Resolved} ->
            case hb_util:atom(hb_ao:get(<<"load">>, Req, true, Opts)) of
                false ->
                    {ok, Resolved};
                true ->
                    hb_cache:read(Resolved, Opts)
            end;
        not_found ->
            not_found
    end.
```

### match_resolver

Find the first resolver that matches the key and return its value.

```erlang
match_resolver(_Key, [], _Opts) -> 
    not_found;
```

### match_resolver

Find the first resolver that matches the key and return its value.

```erlang
match_resolver(Key, [Resolver | Resolvers], Opts) ->
    case execute_resolver(Key, Resolver, Opts) of
        {ok, Value} ->
            ?event({resolver_found, {key, Key}, {value, Value}}),
            {ok, Value};
        _ ->
            match_resolver(Key, Resolvers, Opts)
    end.
```

### execute_resolver

Execute a resolver with the given key and return its value.

```erlang
execute_resolver(Key, Resolver, Opts) ->
    ?event({executing, {key, Key}, {resolver, Resolver}}),
    hb_ao:resolve(
        Resolver,
        #{ <<"path">> => <<"lookup">>, <<"key">> => Key },
        Opts
    ).
```

### no_resolvers_test

```erlang
no_resolvers_test() ->
    ?assertEqual(
        not_found,
        resolve(<<"hello">>, #{}, #{}, #{ only => local })
    ).
```

### message_lookup_device_resolver

```erlang
message_lookup_device_resolver(Msg) ->
    #{
        <<"device">> => #{
            <<"lookup">> => fun(_, Req, Opts) ->
                Key = hb_ao:get(<<"key">>, Req, Opts),
                ?event({test_resolver_executing, {key, Key}, {req, Req}, {msg, Msg}}),
                case maps:get(Key, Msg, not_found) of
                    not_found ->
                        ?event({test_resolver_not_found, {key, Key}, {msg, Msg}}),
                        {error, not_found};
                    Value ->
                        ?event({test_resolver_found, {key, Key}, {value, Value}}),
                        {ok, Value}
                end
            end
        }
    }.
```

### single_resolver_test

```erlang
single_resolver_test() ->
    ?assertEqual(
        {ok, <<"world">>},
        resolve(
            <<"hello">>,
            #{},
            #{ <<"load">> => false },
            #{
                name_resolvers => [
                    message_lookup_device_resolver(
                        #{<<"hello">> => <<"world">>}
                    )
                ]
            }
        )
    ).
```

### multiple_resolvers_test

```erlang
multiple_resolvers_test() ->
    ?assertEqual(
        {ok, <<"bigger-world">>},
        resolve(
            <<"hello">>,
            #{},
            #{ <<"load">> => false },
            #{
                name_resolvers => [
                    message_lookup_device_resolver(
                        #{<<"irrelevant">> => <<"world">>}
                    ),
                    message_lookup_device_resolver(
                        #{<<"hello">> => <<"bigger-world">>}
                    )
                ]
            }
        )
    ).
```

### load_and_execute_test

Test that we can resolve messages from a name loaded with the device.

```erlang
load_and_execute_test() ->
    TestKey = <<"test-key", (hb_util:bin(erlang:system_time(millisecond)))/binary>>,
    {ok, ID} = hb_cache:write(
        #{
            <<"deep">> => <<"PING">>
        },
        #{}
    ),
    ?assertEqual(
        {ok, <<"PING">>},
        hb_ao:resolve_many(
            [
                #{ <<"device">> => <<"name@1.0">> },
                #{ <<"path">> => TestKey },
                #{ <<"path">> => <<"deep">> }
            ],
            #{
                name_resolvers => [
                    message_lookup_device_resolver(#{ <<"irrelevant">> => ID }),
                    message_lookup_device_resolver(#{ TestKey => ID })
                ]
            }
        )
```

---

*Generated from [dev_name.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_name.erl)*
