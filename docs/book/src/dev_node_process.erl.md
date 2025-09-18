# dev_node_process

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_node_process.erl)

A device that implements the singleton pattern for processes specific
to an individual node. This device uses the `local-name@1.0` device to
register processes with names locally, persistenting them across reboots.
Definitions of singleton processes are expected to be found with their 
names in the `node_processes` section of the node message.

---

## Exported Functions

- `info/1`

---

### info

A device that implements the singleton pattern for processes specific
Register a default handler for the device. Inherits `keys` and `set`

```erlang
info(_Opts) ->
    #{
        default => fun lookup/4,
        excludes => [<<"set">>, <<"keys">>]
    }.
```

### lookup

Lookup a process by name.

```erlang
lookup(Name, _Base, Req, Opts) ->
    ?event(node_process, {lookup, {name, Name}}),
    LookupRes =
        hb_ao:resolve(
            #{ <<"device">> => <<"local-name@1.0">> },
            #{ <<"path">> => <<"lookup">>, <<"key">> => Name, <<"load">> => true },
            Opts
        ),
    case LookupRes of
        {ok, ProcessID} ->
            hb_cache:read(ProcessID, Opts);
        {error, not_found} ->
            case hb_ao:get(<<"spawn">>, Req, true, Opts) of
                true ->
                    spawn_register(Name, Opts);
                false ->
                    {error, not_found}
            end
    end.
```

### spawn_register

Spawn a new process according to the process definition found in the 

```erlang
spawn_register(Name, Opts) ->
    case hb_opts:get(node_processes, #{}, Opts) of
        #{ Name := BaseDef } ->
            % We have found the base process definition. Augment it with the 
            % node's address as necessary, then commit to the result.
```

### augment_definition

Augment the given process definition with the node's address.

```erlang
augment_definition(BaseDef, Opts) ->
    Address =
        hb_util:human_id(
            ar_wallet:to_address(
                hb_opts:get(priv_wallet, no_viable_wallet, Opts)
            )
        ),
    SchedulersFromBase =
        hb_util:binary_to_addresses(
            hb_ao:get(<<"scheduler">>, BaseDef, <<>>, Opts)
        ),
    AuthoritiesFromBase =
        hb_util:binary_to_addresses(
            hb_ao:get(<<"authority">>, BaseDef, <<>>, Opts)
        ),
    Schedulers = (SchedulersFromBase -- [Address]) ++ [Address],
    Authorities = (AuthoritiesFromBase -- [Address]) ++ [Address],
    % Normalize the scheduler and authority lists to binary strings.
```

### generate_test_opts

Helper function to generate a test environment and its options.

```erlang
generate_test_opts() ->
    {ok, Module} = file:read_file(<<"test/test.lua">>),
    generate_test_opts(#{
        ?TEST_NAME => #{
            <<"device">> => <<"process@1.0">>,
            <<"execution-device">> => <<"lua@5.3a">>,
            <<"scheduler-device">> => <<"scheduler@1.0">>,
            <<"module">> => #{
                <<"content-type">> => <<"text/x-lua">>,
                <<"body">> => Module
            }
        }
    }).
```

### generate_test_opts

```erlang
generate_test_opts(Defs) ->
    #{
        node_processes => Defs,
        priv_wallet => ar_wallet:new()
    }.
```

### lookup_no_spawn_test

```erlang
lookup_no_spawn_test() ->
    Opts = generate_test_opts(),
    ?assertEqual(
        {error, not_found},
        lookup(<<"name1">>, #{}, #{}, Opts)
    ).
```

### lookup_spawn_test

```erlang
lookup_spawn_test() ->
    Opts = generate_test_opts(),
    Res1 = {_, Process1} =
        hb_ao:resolve(
            #{ <<"device">> => <<"node-process@1.0">> },
            ?TEST_NAME,
            Opts
        ),
    ?assertMatch(
        {ok, #{ <<"device">> := <<"process@1.0">> }},
        Res1
    ),
    {ok, Process2} = hb_ao:resolve(
        #{ <<"device">> => <<"node-process@1.0">> },
        ?TEST_NAME,
        Opts
    ),
    ?assertEqual(
        hb_cache:ensure_all_loaded(Process1, Opts),
        hb_cache:ensure_all_loaded(Process2, Opts)
    ).
```

### lookup_execute_test

Test that a process can be spawned, executed upon, and its result retrieved.

```erlang
lookup_execute_test() ->
    Opts = generate_test_opts(),
    Res1 =
        hb_ao:resolve_many(
            [
                #{ <<"device">> => <<"node-process@1.0">> },
                ?TEST_NAME,
                #{
                    <<"path">> => <<"schedule">>,
                    <<"method">> => <<"POST">>,
                    <<"body">> =>
                        hb_message:commit(
                            #{
                                <<"path">> => <<"compute">>,
                                <<"test-key">> => <<"test-value">>
                            },
                            Opts
                        )
                }
            ],
            Opts
        ),
    ?assertMatch(
        {ok, #{ <<"slot">> := 1 }},
        Res1
    ),
    ?assertMatch(
        42,
        hb_ao:get(
            << ?TEST_NAME/binary, "/now/results/output/body" >>,
            #{ <<"device">> => <<"node-process@1.0">> },
            Opts
        )
```

---

*Generated from [dev_node_process.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_node_process.erl)*
