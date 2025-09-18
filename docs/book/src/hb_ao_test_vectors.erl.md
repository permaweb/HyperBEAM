# hb_ao_test_vectors

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_ao_test_vectors.erl)

Uses a series of different `Opts` values to test the resolution engine's 
execution under different circumstances.

---

### run_test

Uses a series of different `Opts` values to test the resolution engine's 
Easy hook to make a test executable via the command line:

```erlang
run_test() ->
    multiple_as_subresolutions_test(#{}).
```

### suite_test_

Run each test in the file with each set of options. Start and reset

```erlang
suite_test_() ->
    hb_test_utils:suite_with_opts(test_suite(), test_opts()).
```

### benchmark_test_

```erlang
benchmark_test_() ->
    hb_test_utils:suite_with_opts(benchmark_suite(), test_opts()).
```

### test_suite

```erlang
test_suite() ->
    [
        {resolve_simple, "resolve simple",
            fun resolve_simple_test/1},
        {resolve_id, "resolve id",
            fun resolve_id_test/1},
        {start_as, "start as",
            fun start_as_test/1},
        {start_as_with_parameters, "start as with parameters",
            fun start_as_with_parameters_test/1},
        {load_as, "load as",
            fun load_as_test/1},
        {as_path, "as path",
            fun as_path_test/1},
        {continue_as, "continue as",
            fun continue_as_test/1},
        {multiple_as_subresolutions, "multiple as subresolutions",
            fun multiple_as_subresolutions_test/1},
        {resolve_key_twice, "resolve key twice",
            fun resolve_key_twice_test/1},
        {resolve_from_multiple_keys, "resolve from multiple keys",
            fun resolve_from_multiple_keys_test/1},
        {resolve_path_element, "resolve path element",
            fun resolve_path_element_test/1},
        {resolve_binary_key, "resolve binary key",
            fun resolve_binary_key_test/1},
        {key_to_binary, "key to binary",
            fun key_to_binary_test/1},
        {key_from_id_device_with_args, "key from id device with args",
            fun key_from_id_device_with_args_test/1},
        {device_with_handler_function, "device with handler function",
            fun device_with_handler_function_test/1},
        {device_with_default_handler_function,
            "device with default handler function",
            fun device_with_default_handler_function_test/1},
        {basic_get, "basic get",
            fun basic_get_test/1},
        {recursive_get, "recursive get",
            fun recursive_get_test/1},
        {deep_recursive_get, "deep recursive get",
            fun deep_recursive_get_test/1},
        {basic_set, "basic set",
            fun basic_set_test/1},
        {get_with_device, "get with device",
            fun get_with_device_test/1},
        {get_as_with_device, "get as with device",
            fun get_as_with_device_test/1},
        {set_with_device, "set with device",
            fun set_with_device_test/1},
        {deep_set, "deep set",
            fun deep_set_test/1},
        {deep_set_with_device, "deep set with device",
            fun deep_set_with_device_test/1},
        {device_exports, "device exports",
            fun device_exports_test/1},
        {device_excludes, "device excludes",
            fun device_excludes_test/1},
        {denormalized_device_key, "denormalized device key",
            fun denormalized_device_key_test/1},
        {list_transform, "list transform",
            fun list_transform_test/1},
        {step_hook, "step hook",
            fun step_hook_test/1}
    ].
```

### benchmark_suite

```erlang
benchmark_suite() ->
    [
        {benchmark_simple, "simple resolution benchmark",
            fun benchmark_simple_test/1},
        {benchmark_multistep, "multistep resolution benchmark",
            fun benchmark_multistep_test/1},
        {benchmark_get, "get benchmark",
            fun benchmark_get_test/1},
        {benchmark_set, "single value set benchmark",
            fun benchmark_set_test/1},
        {benchmark_set_multiple, "set two keys benchmark",
            fun benchmark_set_multiple_test/1},
        {benchmark_set_multiple_deep, "set two keys deep benchmark",
            fun benchmark_set_multiple_deep_test/1}
    ].
```

### test_opts

```erlang
test_opts() ->
    [
        #{
            name => normal,
            desc => "Default opts",
            opts => #{},
            skip => []
        },
        #{
            name => without_hashpath,
            desc => "Default without hashpath",
            opts => #{
                hashpath => ignore
            },
            skip => []
        },
        #{
            name => no_cache,
            desc => "No cache read or write",
            opts => #{
                hashpath => ignore,
                cache_control => [<<"no-cache">>, <<"no-store">>],
                spawn_worker => false,
                store => #{
                    <<"store-module">> => hb_store_fs,
                    <<"name">> => <<"cache-TEST/fs">>
                }
            },
            skip => [load_as]
        },
        #{
            name => only_store,
            desc => "Store, don't read",
            opts => #{
                hashpath => update,
                cache_control => [<<"no-cache">>],
                spawn_worker => false,
                store => #{
                    <<"store-module">> => hb_store_fs,
                    <<"name">> => <<"cache-TEST/fs">>
                }
            },
            skip => [
                denormalized_device_key,
                deep_set_with_device,
                load_as
            ],
            reset => false
        },
        #{
            name => only_if_cached,
            desc => "Only read, don't exec",
            opts => #{
                hashpath => ignore,
                cache_control => [<<"only-if-cached">>],
                spawn_worker => false,
                store => #{
                    <<"store-module">> => hb_store_fs,
                    <<"name">> => <<"cache-TEST/fs">>
                }
            },
            skip => [
                % Exclude tests that return a list on its own for now, as raw 
                % lists cannot be cached yet.
```

### exec_dummy_device

Ensure that we can read a device from the cache then execute it. By 

```erlang
exec_dummy_device(SigningWallet, Opts) ->
    % Compile the test device and store it in an accessible cache to the execution
    % environment.
```

### load_device_test

```erlang
load_device_test() ->
    % Establish an execution environment which trusts the device author.
```

### untrusted_load_device_test

```erlang
untrusted_load_device_test() ->
    % Establish an execution environment which does not trust the device author.
```

### resolve_simple_test

```erlang
resolve_simple_test(Opts) ->
    Res = hb_ao:resolve(#{ <<"a">> => <<"RESULT">> }, <<"a">>, Opts),
    ?assertEqual({ok, <<"RESULT">>}, Res).
```

### resolve_id_test

```erlang
resolve_id_test(Opts) ->
    ?assertMatch(
        ID when byte_size(ID) == 43,
        hb_ao:get(id, #{ test_key => <<"1">> }, Opts)
    ).
```

### resolve_key_twice_test

```erlang
resolve_key_twice_test(Opts) ->
    % Ensure that the same message can be resolved again.
```

### resolve_from_multiple_keys_test

```erlang
resolve_from_multiple_keys_test(Opts) ->
    ?assertEqual(
        {ok, [<<"a">>]},
        hb_ao:resolve(#{ <<"a">> => <<"1">>, <<"priv_a">> => <<"2">> }, <<"keys">>, Opts)
    ).
```

### resolve_path_element_test

```erlang
resolve_path_element_test(Opts) ->
    ?assertEqual(
        {ok, [<<"test_path">>]},
        hb_ao:resolve(#{ <<"path">> => [<<"test_path">>] }, <<"path">>, Opts)
    ),
    ?assertEqual(
        {ok, [<<"a">>]},
        hb_ao:resolve(#{ <<"Path">> => [<<"a">>] }, <<"Path">>, Opts)
    ).
```

### key_to_binary_test

```erlang
key_to_binary_test(Opts) ->
    ?assertEqual(<<"a">>, hb_ao:normalize_key(a, Opts)),
    ?assertEqual(<<"a">>, hb_ao:normalize_key(<<"a">>, Opts)),
    ?assertEqual(<<"a">>, hb_ao:normalize_key("a", Opts)).
```

### resolve_binary_key_test

```erlang
resolve_binary_key_test(Opts) ->
    ?assertEqual(
        {ok, <<"RESULT">>},
        hb_ao:resolve(#{ a => <<"RESULT">> }, <<"a">>, Opts)
    ),
    ?assertEqual(
        {ok, <<"1">>},
        hb_ao:resolve(
            #{
                <<"Test-Header">> => <<"1">>
            },
            <<"Test-Header">>,
            Opts
        )
    ).
```

### generate_device_with_keys_using_args

Generates a test device with three keys, each of which uses

```erlang
generate_device_with_keys_using_args() ->
    #{
        key_using_only_state =>
            fun(State) ->
                {ok,
                    <<(hb_maps:get(<<"state_key">>, State))/binary>>
                }
            end,
        key_using_state_and_msg =>
            fun(State, Msg) ->
                {ok,
                    <<
                        (hb_maps:get(<<"state_key">>, State))/binary,
                        (hb_maps:get(<<"msg_key">>, Msg))/binary
                    >>
                }
            end,
        key_using_all =>
            fun(State, Msg, Opts) ->
                {ok,
                    <<
                        (hb_maps:get(<<"state_key">>, State, undefined, Opts))/binary,
                        (hb_maps:get(<<"msg_key">>, Msg, undefined, Opts))/binary,
                        (hb_maps:get(<<"opts_key">>, Opts, undefined, Opts))/binary
                    >>
                }
            end
    }.
```

### gen_default_device

Create a simple test device that implements the default handler.

```erlang
gen_default_device() ->
    #{
        info =>
            fun() ->
                #{
                    default =>
                        fun(_, _State) ->
                            {ok, <<"DEFAULT">>}
                        end
                }
            end,
        <<"state_key">> =>
            fun(_) ->
                {ok, <<"STATE">>}
            end
    }.
```

### gen_handler_device

Create a simple test device that implements the handler key.

```erlang
gen_handler_device() ->
    #{
        info =>
            fun() ->
                #{
                    handler =>
                        fun(<<"set">>, M1, M2, Opts) ->
                            dev_message:set(M1, M2, Opts);
                        (_, _, _, _) ->
                            {ok, <<"HANDLER VALUE">>}
                        end
                }
            end
    }.
```

### key_from_id_device_with_args_test

Test that arguments are passed to a device key as expected.

```erlang
key_from_id_device_with_args_test(Opts) ->
    Msg =
        #{
            device => generate_device_with_keys_using_args(),
            state_key => <<"1">>
        },
    ?assertEqual(
        {ok, <<"1">>},
        hb_ao:resolve(
            Msg,
            #{
                <<"path">> => <<"key_using_only_state">>,
                <<"msg_key">> => <<"2">> % Param message, which is ignored
            },
            Opts
        )
    ),
    ?assertEqual(
        {ok, <<"13">>},
        hb_ao:resolve(
            Msg,
            #{
                <<"path">> => <<"key_using_state_and_msg">>,
                <<"msg_key">> => <<"3">> % Param message, with value to add
            },
            Opts
        )
    ),
    ?assertEqual(
        {ok, <<"1337">>},
        hb_ao:resolve(
            Msg,
            #{
                <<"path">> => <<"key_using_all">>,
                <<"msg_key">> => <<"3">> % Param message
            },
            Opts#{
                <<"opts_key">> => <<"37">>,
                <<"cache_control">> => [<<"no-cache">>, <<"no-store">>]
            }
        )
    ).
```

### device_with_handler_function_test

```erlang
device_with_handler_function_test(Opts) ->
    Msg =
        #{
            device => gen_handler_device(),
            test_key => <<"BAD">>
        },
    ?assertEqual(
        {ok, <<"HANDLER VALUE">>},
        hb_ao:resolve(Msg, <<"test_key">>, Opts)
    ).
```

### device_with_default_handler_function_test

```erlang
device_with_default_handler_function_test(Opts) ->
    Msg =
        #{
            device => gen_default_device()
        },
    ?assertEqual(
        {ok, <<"STATE">>},
        hb_ao:resolve(Msg, <<"state_key">>, Opts)
    ),
    ?assertEqual(
        {ok, <<"DEFAULT">>},
        hb_ao:resolve(Msg, <<"any_random_key">>, Opts)
    ).
```

### basic_get_test

```erlang
basic_get_test(Opts) ->
    Msg = #{ <<"key1">> => <<"value1">>, <<"key2">> => <<"value2">> },
    ?assertEqual(<<"value1">>, hb_ao:get(<<"key1">>, Msg, Opts)),
    ?assertEqual(<<"value2">>, hb_ao:get(<<"key2">>, Msg, Opts)),
    ?assertEqual(<<"value2">>, hb_ao:get(<<"key2">>, Msg, Opts)),
    ?assertEqual(<<"value2">>, hb_ao:get([<<"key2">>], Msg, Opts)).
```

### recursive_get_test

```erlang
recursive_get_test(Opts) ->
    Msg = #{
        <<"key1">> => <<"value1">>,
        <<"key2">> => #{
            <<"key3">> => <<"value3">>,
            <<"key4">> => #{
                <<"key5">> => <<"value5">>,
                <<"key6">> => #{
                    <<"key7">> => <<"value7">>
                }
            }
        }
    },
    ?assertEqual(
        {ok, <<"value1">>},
        hb_ao:resolve(Msg, #{ <<"path">> => <<"key1">> }, Opts)
    ),
    ?assertEqual(<<"value1">>, hb_ao:get(<<"key1">>, Msg, Opts)),
    ?assertEqual(
        {ok, <<"value3">>},
        hb_ao:resolve(Msg, #{ <<"path">> => [<<"key2">>, <<"key3">>] }, Opts)
    ),
    ?assertEqual(<<"value3">>, hb_ao:get([<<"key2">>, <<"key3">>], Msg, Opts)),
    ?assertEqual(<<"value3">>, hb_ao:get(<<"key2/key3">>, Msg, Opts)).
```

### deep_recursive_get_test

```erlang
deep_recursive_get_test(Opts) ->
    Msg = #{
        <<"key1">> => <<"value1">>,
        <<"key2">> => #{
            <<"key3">> => <<"value3">>,
            <<"key4">> => #{
                <<"key5">> => <<"value5">>,
                <<"key6">> => #{
                    <<"key7">> => <<"value7">>
                }
            }
        }
    },
    ?assertEqual(<<"value7">>, hb_ao:get(<<"key2/key4/key6/key7">>, Msg, Opts)).
```

### basic_set_test

```erlang
basic_set_test(Opts) ->
    Msg = #{ <<"key1">> => <<"value1">>, <<"key2">> => <<"value2">> },
    UpdatedMsg = hb_ao:set(Msg, #{ <<"key1">> => <<"new_value1">> }, Opts),
    ?event({set_key_complete, {key, <<"key1">>}, {value, <<"new_value1">>}}),
    ?assertEqual(<<"new_value1">>, hb_ao:get(<<"key1">>, UpdatedMsg, Opts)),
    ?assertEqual(<<"value2">>, hb_ao:get(<<"key2">>, UpdatedMsg, Opts)).
```

### get_with_device_test

```erlang
get_with_device_test(Opts) ->
    Msg =
        #{
            <<"device">> => generate_device_with_keys_using_args(),
            <<"state_key">> => <<"STATE">>
        },
    ?assertEqual(<<"STATE">>, hb_ao:get(<<"state_key">>, Msg, Opts)),
    ?assertEqual(<<"STATE">>, hb_ao:get(<<"key_using_only_state">>, Msg, Opts)).
```

### get_as_with_device_test

```erlang
get_as_with_device_test(Opts) ->
    Msg =
        #{
            <<"device">> => gen_handler_device(),
            <<"test_key">> => <<"ACTUAL VALUE">>
        },
    ?assertEqual(
        <<"HANDLER VALUE">>,
        hb_ao:get(test_key, Msg, Opts)
    ),
    ?assertEqual(
        <<"ACTUAL VALUE">>,
        hb_ao:get(test_key, {as, dev_message, Msg}, Opts)
    ).
```

### set_with_device_test

```erlang
set_with_device_test(Opts) ->
    Msg =
        #{
            <<"device">> =>
                #{
                    <<"set">> =>
                        fun(State, _Msg) ->
                            Acc = hb_maps:get(<<"set_count">>, State, <<"">>, Opts),
                            {ok,
                                State#{
                                    <<"set_count">> => << Acc/binary, "." >>
                                }
                            }
                        end
                },
            <<"state_key">> => <<"STATE">>
        },
    ?assertEqual(<<"STATE">>, hb_ao:get(<<"state_key">>, Msg, Opts)),
    SetOnce = hb_ao:set(Msg, #{ <<"state_key">> => <<"SET_ONCE">> }, Opts),
    ?assertEqual(<<".">>, hb_ao:get(<<"set_count">>, SetOnce, Opts)),
    SetTwice = hb_ao:set(SetOnce, #{ <<"state_key">> => <<"SET_TWICE">> }, Opts),
    ?assertEqual(<<"..">>, hb_ao:get(<<"set_count">>, SetTwice, Opts)),
    ?assertEqual(<<"STATE">>, hb_ao:get(<<"state_key">>, SetTwice, Opts)).
```

### deep_set_test

```erlang
deep_set_test(Opts) ->
    % First validate second layer changes are handled correctly.
```

### deep_set_new_messages_test

```erlang
deep_set_new_messages_test() ->
    Opts = hb_maps:get(opts, hd(test_opts())),
    % Test that new messages are created when the path does not exist.
```

### deep_set_with_device_test

```erlang
deep_set_with_device_test(Opts) ->
    Device = #{
        set =>
            fun(Msg1, Msg2) ->
                % A device where the set function modifies the key
                % and adds a modified flag.
```

### device_exports_test

```erlang
device_exports_test(Opts) ->
	Msg = #{ <<"device">> => dev_message },
	?assert(hb_ao:is_exported(Msg, dev_message, info, Opts)),
	?assert(hb_ao:is_exported(Msg, dev_message, set, Opts)),
	?assert(
        hb_ao:is_exported(
            Msg,
            dev_message,
            not_explicitly_exported,
            Opts
        )
    ),
	Dev = #{
		info => fun() -> #{ exports => [set] } end,
		set => fun(_, _) -> {ok, <<"SET">>} end
	},
	Msg2 = #{ <<"device">> => Dev },
	?assert(hb_ao:is_exported(Msg2, Dev, info, Opts)),
	?assert(hb_ao:is_exported(Msg2, Dev, set, Opts)),
	?assert(not hb_ao:is_exported(Msg2, Dev, not_exported, Opts)),
    Dev2 = #{
        info =>
            fun() ->
                #{
                    exports => [test1, <<"test2">>],
                    handler =>
                        fun() ->
                            {ok, <<"Handler-Value">>}
                        end
                }
            end
    },
    Msg3 = #{ <<"device">> => Dev2, <<"test1">> => <<"BAD1">>, <<"test3">> => <<"GOOD3">> },
    ?assertEqual(<<"Handler-Value">>, hb_ao:get(<<"test1">>, Msg3, Opts)),
    ?assertEqual(<<"Handler-Value">>, hb_ao:get(<<"test2">>, Msg3, Opts)),
    ?assertEqual(<<"GOOD3">>, hb_ao:get(<<"test3">>, Msg3, Opts)),
    ?assertEqual(<<"GOOD4">>,
        hb_ao:get(
            <<"test4">>,
            hb_ao:set(Msg3, <<"test4">>, <<"GOOD4">>, Opts)
        )
    ),
    ?assertEqual(not_found, hb_ao:get(<<"test5">>, Msg3, Opts)).
```

### device_excludes_test

```erlang
device_excludes_test(Opts) ->
    % Create a device that returns an identifiable message for any key, but also
    % sets excludes to [set], such that the message can be modified using the 
    % default handler.
```

### denormalized_device_key_test

```erlang
denormalized_device_key_test(Opts) ->
	Msg = #{ <<"device">> => dev_test },
	?assertEqual(dev_test, hb_ao:get(device, Msg, Opts)),
	?assertEqual(dev_test, hb_ao:get(<<"device">>, Msg, Opts)),
	?assertEqual({module, dev_test},
		erlang:fun_info(
            element(3, hb_ao:message_to_fun(Msg, test_func, Opts)),
            module
        )
    ).
```

### list_transform_test

```erlang
list_transform_test(Opts) ->
    Msg = [<<"A">>, <<"B">>, <<"C">>, <<"D">>, <<"E">>],
    ?assertEqual(<<"A">>, hb_ao:get(1, Msg, Opts)),
    ?assertEqual(<<"B">>, hb_ao:get(2, Msg, Opts)),
    ?assertEqual(<<"C">>, hb_ao:get(3, Msg, Opts)),
    ?assertEqual(<<"D">>, hb_ao:get(4, Msg, Opts)),
    ?assertEqual(<<"E">>, hb_ao:get(5, Msg, Opts)).
```

### start_as_test

```erlang
start_as_test(Opts) ->
    ?assertEqual(
        {ok, <<"GOOD_FUNCTION">>},
        hb_ao:resolve_many(
            [
                {as, <<"test-device@1.0">>, #{ <<"path">> => <<>> }},
                #{ <<"path">> => <<"test_func">> }
            ],
            Opts
        )
    ).
```

### start_as_with_parameters_test

```erlang
start_as_with_parameters_test(Opts) ->
    % Resolve a key on a message that has its device set with `as'.
```

### load_as_test

```erlang
load_as_test(Opts) ->
    % Load a message as a device with the `as' keyword.
```

### as_path_test

```erlang
as_path_test(Opts) ->
    % Create a message with the test device, which implements the test_func
    % function. It normally returns `GOOD_FUNCTION'.
```

### continue_as_test

```erlang
continue_as_test(Opts) ->
    % Resolve a list of messages in sequence, swapping the device in the middle.
```

### multiple_as_subresolutions_test

```erlang
multiple_as_subresolutions_test(Opts) ->
    % Test that multiple as subresolutions in a sequence are handled correctly.
```

### step_hook_test

```erlang
step_hook_test(InitOpts) ->
    % Test that the step hook is called correctly. We do this by sending ourselves
    % a message each time the hook is called. We also send a `reference', such 
    % that this test is uniquely identified and further/prior tests do not affect
    % it.
```

### benchmark_simple_test

```erlang
benchmark_simple_test(Opts) ->
    Time =
        hb_test_utils:benchmark_iterations(
            fun(I) -> hb_ao:resolve(#{ <<"a">> => I }, <<"a">>, Opts) end,
            ?BENCHMARK_ITERATIONS
        ),
    hb_test_utils:benchmark_print(
        <<"Single-step resolutions:">>,
        ?BENCHMARK_ITERATIONS,
        Time
    ).
```

### benchmark_multistep_test

```erlang
benchmark_multistep_test(Opts) ->
    Time =
        hb_test_utils:benchmark_iterations(
            fun(I) ->
                hb_ao:resolve(
                    #{
                        <<"iteration">> => I,
                        <<"a">> => #{
                            <<"b">> => #{ <<"return">> => I }
                        }
                    },
                    <<"a/b/return">>,
                    Opts
                )
            end,
            ?BENCHMARK_ITERATIONS
        ),
    hb_test_utils:benchmark_print(
        <<"Multistep resolutions:">>,
        ?BENCHMARK_ITERATIONS,
        Time
    ).
```

### benchmark_get_test

```erlang
benchmark_get_test(Opts) ->
    Time =
        hb_test_utils:benchmark_iterations(
            fun(I) ->
                hb_ao:get(
                    <<"a">>,
                    #{ <<"a">> => <<"1">>, <<"iteration">> => I },
                    Opts
                )
            end,
            ?BENCHMARK_ITERATIONS
        ),
    hb_test_utils:benchmark_print(
        <<"Get operations:">>,
        ?BENCHMARK_ITERATIONS,
        Time
    ).
```

### benchmark_set_test

```erlang
benchmark_set_test(Opts) ->
    Time =
        hb_test_utils:benchmark_iterations(
            fun(I) ->
                hb_ao:set(
                    #{ <<"a">> => <<"1">>, <<"iteration">> => I },
                    <<"a">>,
                    <<"2">>,
                    Opts
                )
            end,
            ?BENCHMARK_ITERATIONS
        ),
    hb_test_utils:benchmark_print(
        <<"Single value set operations:">>,
        ?BENCHMARK_ITERATIONS,
        Time
    ).
```

### benchmark_set_multiple_test

```erlang
benchmark_set_multiple_test(Opts) ->
    Time =
        hb_test_utils:benchmark_iterations(
            fun(I) ->
                hb_ao:set(
                    #{ <<"a">> => <<"1">>, <<"iteration">> => I },
                    #{ <<"a">> => <<"1a">>, <<"b">> => <<"2">> },
                    Opts
                )
            end,
            ?BENCHMARK_ITERATIONS
        ),
    hb_test_utils:benchmark_print(
        <<"Set two keys operations:">>,
        ?BENCHMARK_ITERATIONS,
        Time
    ).
```

### benchmark_set_multiple_deep_test

```erlang
benchmark_set_multiple_deep_test(Opts) ->
    Time =
        hb_test_utils:benchmark_iterations(
            fun(I) ->
                hb_ao:set(
                    #{ <<"a">> => #{ <<"b">> => <<"1">> } },
                    #{ <<"a">> => #{ <<"b">> => <<"2">>, <<"c">> => I } },
                    Opts
                )
            end,
            ?BENCHMARK_ITERATIONS
        ),
    hb_test_utils:benchmark_print(
        <<"Set two keys operations:">>,
        ?BENCHMARK_ITERATIONS,
        Time
```

---

*Generated from [hb_ao_test_vectors.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_ao_test_vectors.erl)*
