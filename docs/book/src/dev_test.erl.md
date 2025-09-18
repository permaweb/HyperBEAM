# dev_test

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_test.erl)

A simple test device for AO-Core, so that we can test the functionality that
depends on using Erlang's module system.
NOTE: This device is labelled `test-device/1.0` to avoid conflicts with
other testing functionality -- care should equally be taken to avoid
using the `test` key in other settings.

---

## Exported Functions

- `compute/3`
- `delay/3`
- `increment_counter/3`
- `index/3`
- `info/1`
- `info/3`
- `init/3`
- `load/3`
- `mul/2`
- `postprocess/3`
- `restore/3`
- `snapshot/3`
- `test_func/1`
- `update_state/3`

---

### info

Exports a default_handler function that can be used to test the

```erlang
info(_) ->
	#{
        <<"default">> => dev_message,
		handlers => #{
			<<"info">> => fun info/3,
			<<"update_state">> => fun update_state/3,
			<<"increment_counter">> => fun increment_counter/3
		}
	}.
```

### info

Exports a default_handler function that can be used to test the
Example index handler.

```erlang
info(_Msg1, _Msg2, _Opts) ->
	InfoBody = #{
		<<"description">> => <<"Test device for testing the AO-Core framework">>,
		<<"version">> => <<"1.0">>,
		<<"paths">> => #{
			<<"info">> => <<"Get device info">>,
			<<"test_func">> => <<"Test function">>,
			<<"compute">> => <<"Compute function">>,
			<<"init">> => <<"Initialize function">>,
			<<"restore">> => <<"Restore function">>,
			<<"mul">> => <<"Multiply function">>,
			<<"snapshot">> => <<"Snapshot function">>,
			<<"response">> => <<"Response function">>,
			<<"update_state">> => <<"Update state function">>
		}
	},
	{ok, #{<<"status">> => 200, <<"body">> => InfoBody}}.
```

### index

Exports a default_handler function that can be used to test the
Example index handler.

```erlang
index(Msg, _Req, Opts) ->
    Name = hb_ao:get(<<"name">>, Msg, <<"turtles">>, Opts),
    {ok,
        #{
            <<"content-type">> => <<"text/html">>,
            <<"body">> => <<"i like ", Name/binary, "!">>
        }
    }.
```

### load

Return a message with the device set to this module.

```erlang
load(Base, _, _Opts) ->
    {ok, Base#{ <<"device">> => <<"test-device@1.0">> }}.
```

### test_func

Return a message with the device set to this module.
Example implementation of a `compute` handler. Makes a running list of

```erlang
test_func(_) ->
	{ok, <<"GOOD_FUNCTION">>}.
```

### compute

Return a message with the device set to this module.
Example implementation of a `compute` handler. Makes a running list of

```erlang
compute(Msg1, Msg2, Opts) ->
    AssignmentSlot = hb_ao:get(<<"slot">>, Msg2, Opts),
    Seen = hb_ao:get(<<"already-seen">>, Msg1, Opts),
    ?event({compute_called, {msg1, Msg1}, {msg2, Msg2}, {opts, Opts}}),
    {ok,
        hb_ao:set(
            Msg1,
            #{
                <<"random-key">> => <<"random-value">>,
                <<"results">> =>
                    #{ <<"assignment-slot">> => AssignmentSlot },
                <<"already-seen">> => [AssignmentSlot | Seen]
            },
            Opts
        )
    }.
```

### init

Example `init/3` handler. Sets the `Already-Seen` key to an empty list.
Example `restore/3` handler. Sets the hidden key `Test/Started` to the

```erlang
init(Msg, _Msg2, Opts) ->
    ?event({init_called_on_dev_test, Msg}),
    {ok, hb_ao:set(Msg, #{ <<"already-seen">> => [] }, Opts)}.
```

### restore

Example `init/3` handler. Sets the `Already-Seen` key to an empty list.
Example `restore/3` handler. Sets the hidden key `Test/Started` to the

```erlang
restore(Msg, _Msg2, Opts) ->
    ?event({restore_called_on_dev_test, Msg}),
    case hb_ao:get(<<"already-seen">>, Msg, Opts) of
        not_found ->
            ?event({restore_not_found, Msg}),
            {error, <<"No viable state to restore.">>};
        AlreadySeen ->
            ?event({restore_found, AlreadySeen}),
            {ok,
                hb_private:set(
                    Msg,
                    #{ <<"test-key/started-state">> => AlreadySeen },
                    Opts
                )
            }
    end.
```

### mul

Example implementation of an `imported` function for a WASM
Do nothing when asked to snapshot.

```erlang
mul(Msg1, Msg2) ->
    ?event(mul_called),
    State = hb_ao:get(<<"state">>, Msg1, #{ hashpath => ignore }),
    [Arg1, Arg2] = hb_ao:get(<<"args">>, Msg2, #{ hashpath => ignore }),
    ?event({mul_called, {state, State}, {args, [Arg1, Arg2]}}),
    {ok, #{ <<"state">> => State, <<"results">> => [Arg1 * Arg2] }}.
```

### snapshot

Example implementation of an `imported` function for a WASM
Do nothing when asked to snapshot.

```erlang
snapshot(Msg1, Msg2, _Opts) ->
    ?event({snapshot_called, {msg1, Msg1}, {msg2, Msg2}}),
    {ok, #{}}.
```

### postprocess

Set the `postprocessor-called` key to true in the HTTP server.

```erlang
postprocess(_Msg, #{ <<"body">> := Msgs }, Opts) ->
    ?event({postprocess_called, Opts}),
    hb_http_server:set_opts(Opts#{ <<"postprocessor-called">> => true }),
    {ok, Msgs}.
```

### update_state

Find a test worker's PID and send it an update message.

```erlang
update_state(_Msg, Msg2, _Opts) ->
    case hb_ao:get(<<"test-id">>, Msg2) of
        not_found ->
            {error, <<"No test ID found in message.">>};
        ID ->
            LookupResult = hb_name:lookup({<<"test">>, ID}),
            case LookupResult of
                undefined ->
                    {error, <<"No test worker found.">>};
                Pid ->
                    Pid ! {update, Msg2},
                    {ok, Pid}
            end
    end.
```

### increment_counter

Find a test worker's PID and send it an increment message.

```erlang
increment_counter(_Msg1, Msg2, _Opts) ->
    case hb_ao:get(<<"test-id">>, Msg2) of
        not_found ->
            {error, <<"No test ID found in message.">>};
        ID ->
            LookupResult = hb_name:lookup({<<"test">>, ID}),
            case LookupResult of
                undefined ->
                    {error, <<"No test worker found for increment.">>};
                Pid when is_pid(Pid) ->
                    Pid ! {increment},
				    {ok, Pid};
                _ -> % Handle case where registered value isn't a PID
                    {error, <<"Invalid registration found for test worker.">>}
            end
    end.
```

### delay

Does nothing, just sleeps `Req/duration or 750` ms and returns the 

```erlang
delay(Msg1, Req, Opts) ->
    Duration =
        hb_ao:get_first(
            [
                {Msg1, <<"duration">>},
                {Req, <<"duration">>}
            ],
            750,
            Opts
        ),
    ?event(delay, {delay, {sleeping, Duration}}),
    timer:sleep(Duration),
    ?event({delay, waking}),
    Return =
        case hb_ao:get(<<"return">>, Msg1, Opts) of
            not_found ->
                hb_ao:get(<<"body">>, Req, #{ <<"result">> => <<"slept">> }, Opts);
            ReturnMsgs ->
                ReturnMsgs
        end,
    ?event(delay, {returning, Return}),
    {ok, Return}.
```

### device_with_function_key_module_test

Tests the resolution of a default function.

```erlang
device_with_function_key_module_test() ->
	Msg =
		#{
			<<"device">> => <<"test-device@1.0">>
		},
	?assertEqual(
		{ok, <<"GOOD_FUNCTION">>},
		hb_ao:resolve(Msg, test_func, #{})
	).
```

### compute_test

```erlang
compute_test() ->
    Msg0 = #{ <<"device">> => <<"test-device@1.0">> },
    {ok, Msg1} = hb_ao:resolve(Msg0, init, #{}),
    Msg2 =
        hb_ao:set(
            #{ <<"path">> => <<"compute">> },
            #{
                <<"slot">> => 1,
                <<"body/number">> => 1337
            },
            #{}
        ),
    {ok, Msg3} = hb_ao:resolve(Msg1, Msg2, #{}),
    ?assertEqual(1, hb_ao:get(<<"results/assignment-slot">>, Msg3, #{})),
    Msg4 =
        hb_ao:set(
            #{ <<"path">> => <<"compute">> },
            #{
                <<"slot">> => 2,
                <<"body/number">> => 9001
            },
            #{}
        ),
    {ok, Msg5} = hb_ao:resolve(Msg3, Msg4, #{}),
    ?assertEqual(2, hb_ao:get(<<"results/assignment-slot">>, Msg5, #{})),
    ?assertEqual([2, 1], hb_ao:get(<<"already-seen">>, Msg5, #{})).
```

### restore_test

```erlang
restore_test() ->
    Msg1 = #{ <<"device">> => <<"test-device@1.0">>, <<"already-seen">> => [1] },
    {ok, Msg3} = hb_ao:resolve(Msg1, <<"restore">>, #{}),
```

---

*Generated from [dev_test.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_test.erl)*
