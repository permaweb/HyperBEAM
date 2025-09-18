# dev_patch

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_patch.erl)

A device that can be used to reorganize a message: Moving data from
one path inside it to another. This device's function runs in two modes:
1. When using `all` to move all data at the path given in `from` to the
   path given in `to`.
2. When using `patches` to move all submessages in the source to the target,
   _if_ they have a `method` key of `PATCH` or a `device` key of `patch@1.0`.
Source and destination paths may be prepended by `base:` or `req:` keys to
indicate that they are relative to either of the message's that the
computation is being performed on.
The search order for finding the source and destination keys is as follows,
where `X` is either `from` or `to`:
1. The `patch-X` key of the execution message.
2. The `X` key of the execution message.
3. The `patch-X` key of the request message.
4. The `X` key of the request message.
Additionally, this device implements the standard computation device keys,
allowing it to be used as an element of an execution stack pipeline, etc.

---

## Exported Functions

- `all/3`
- `compute/3`
- `init/3`
- `normalize/3`
- `patches/3`
- `snapshot/3`

---

### init

A device that can be used to reorganize a message: Moving data from
Necessary hooks for compliance with the `execution-device` standard.

```erlang
init(Msg1, _Msg2, _Opts) -> {ok, Msg1}.
```

### normalize

A device that can be used to reorganize a message: Moving data from
Necessary hooks for compliance with the `execution-device` standard.

```erlang
normalize(Msg1, _Msg2, _Opts) -> {ok, Msg1}.
```

### snapshot

A device that can be used to reorganize a message: Moving data from
Necessary hooks for compliance with the `execution-device` standard.

```erlang
snapshot(Msg1, _Msg2, _Opts) -> {ok, Msg1}.
```

### compute

A device that can be used to reorganize a message: Moving data from
Necessary hooks for compliance with the `execution-device` standard.
Get the value found at the `patch-from` key of the message, or the

```erlang
compute(Msg1, Msg2, Opts) -> patches(Msg1, Msg2, Opts).
```

### all

A device that can be used to reorganize a message: Moving data from
Necessary hooks for compliance with the `execution-device` standard.
Get the value found at the `patch-from` key of the message, or the

```erlang
all(Msg1, Msg2, Opts) ->
    move(all, Msg1, Msg2, Opts).
```

### patches

Find relevant `PATCH` messages in the given source key of the execution

```erlang
patches(Msg1, Msg2, Opts) ->
    move(patches, Msg1, Msg2, Opts).
```

### move

Unified executor for the `all` and `patches` modes.

```erlang
move(Mode, Msg1, Msg2, Opts) ->
    maybe
        % Find the input paths.
```

### uninitialized_patch_test

```erlang
uninitialized_patch_test() ->
    InitState = #{
        <<"device">> => <<"patch@1.0">>,
        <<"results">> => #{
            <<"outbox">> => #{
                <<"1">> => #{
                    <<"method">> => <<"PATCH">>,
                    <<"prices">> => #{
                        <<"apple">> => 100,
                        <<"banana">> => 200
                    }
                },
                <<"2">> => #{
                    <<"method">> => <<"GET">>,
                    <<"prices">> => #{
                        <<"apple">> => 1000
                    }
                }
            }
        },
        <<"other-message">> => <<"other-value">>,
        <<"patch-to">> => <<"/">>,
        <<"patch-from">> => <<"/results/outbox">>
    },
    {ok, ResolvedState} =
        hb_ao:resolve(
            InitState,
            <<"compute">>,
            #{}
        ),
    ?event({resolved_state, ResolvedState}),
    ?assertEqual(
        100,
        hb_ao:get(<<"prices/apple">>, ResolvedState, #{})
    ),
    ?assertMatch(
        not_found,
        hb_ao:get(<<"results/outbox/1">>, ResolvedState, #{})
    ).
```

### patch_to_submessage_test

```erlang
patch_to_submessage_test() ->
    InitState = #{
        <<"device">> => <<"patch@1.0">>,
        <<"results">> => #{
            <<"outbox">> => #{
                <<"1">> =>
                    hb_message:commit(#{
                        <<"method">> => <<"PATCH">>,
                        <<"prices">> => #{
                            <<"apple">> => 100,
                            <<"banana">> => 200
                        }
                    },
                    hb:wallet()
                )
            }
        },
        <<"state">> => #{
            <<"prices">> => #{
                <<"apple">> => 1000
            }
        },
        <<"other-message">> => <<"other-value">>,
        <<"patch-to">> => <<"/state">>,
        <<"patch-from">> => <<"/results/outbox">>
    },
    {ok, ResolvedState} =
        hb_ao:resolve(
            InitState,
            <<"compute">>,
            #{}
        ),
    ?event({resolved_state, ResolvedState}),
    ?assertEqual(
        100,
        hb_ao:get(<<"state/prices/apple">>, ResolvedState, #{})
    ).
```

### all_mode_test

```erlang
all_mode_test() ->
    InitState = #{
        <<"device">> => <<"patch@1.0">>,
        <<"input">> => #{
            <<"zones">> => #{
                <<"1">> => #{
                    <<"method">> => <<"PATCH">>,
                    <<"prices">> => #{
                        <<"apple">> => 100,
                        <<"banana">> => 200
                    }
                },
                <<"2">> => #{
                    <<"method">> => <<"GET">>,
                    <<"prices">> => #{
                        <<"orange">> => 300
                    }
                }
            }
        },
        <<"state">> => #{
            <<"prices">> => #{
                <<"apple">> => 1000
            }
        }
    },
    {ok, ResolvedState} =
        hb_ao:resolve(
            InitState,
            #{
                <<"path">> => <<"all">>,
                <<"patch-to">> => <<"/state">>,
                <<"patch-from">> => <<"/input/zones">>
            },
            #{}
        ),
    ?event({resolved_state, ResolvedState}),
    ?assertEqual(
        100,
        hb_ao:get(<<"state/1/prices/apple">>, ResolvedState, #{})
    ),
    ?assertEqual(
        300,
        hb_ao:get(<<"state/2/prices/orange">>, ResolvedState, #{})
    ),
    ?assertEqual(
        not_found,
        hb_ao:get(<<"input/zones">>, ResolvedState, #{})
    ).
```

### req_prefix_test

```erlang
req_prefix_test() ->
    BaseMsg = #{
        <<"device">> => <<"patch@1.0">>,
        <<"state">> => #{
            <<"prices">> => #{
                <<"apple">> => 1000
            }
        }
    },
    ReqMsg = #{
        <<"path">> => <<"all">>,
        <<"patch-from">> => <<"req:/results/outbox/1">>,
        <<"patch-to">> => <<"/state">>,
        <<"results">> => #{
            <<"outbox">> => #{
                <<"1">> => #{
                    <<"method">> => <<"PATCH">>,
                    <<"prices">> => #{
                        <<"apple">> => 100,
                        <<"banana">> => 200
                    }
                }
            }
        }
    },
    {ok, ResolvedState} = hb_ao:resolve(BaseMsg, ReqMsg, #{}),
    ?event({resolved_state, ResolvedState}),
    ?assertEqual(
        100,
        hb_ao:get(<<"state/prices/apple">>, ResolvedState, #{})
    ),
    ?assertEqual(
        200,
        hb_ao:get(<<"state/prices/banana">>, ResolvedState, #{})
    ),
    ?assertEqual(
        not_found,
        hb_ao:get(<<"results/outbox/1">>, ResolvedState, #{})
```

---

*Generated from [dev_patch.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_patch.erl)*
