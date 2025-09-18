# hb_opts

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_opts.erl)

A module for interacting with local and global options inside
HyperBEAM. Options are set globally, but can also be overridden using an
an optional local `Opts` map argument. Many functions across the HyperBEAM 
environment accept an `Opts` argument, which can be used to customize 
behavior.
Options set in an `Opts` map must _never_ change the behavior of a function
that should otherwise be deterministic. Doing so may lead to loss of funds
by the HyperBEAM node operator, as the results of their executions will be
different than those of other node operators. If they are economically 
staked on the correctness of these results, they may experience punishments
for non-verifiable behavior. Instead, if a local node setting makes 
deterministic behavior impossible, the caller should fail the execution 
with a refusal to execute.

---

## Exported Functions

- `as/2`
- `check_required_opts/2`
- `default_message_with_env/0`
- `default_message/0`
- `ensure_node_history/2`
- `get/1`
- `get/2`
- `get/3`
- `identities/1`
- `load_bin/2`
- `load/1`
- `load/2`
- `mimic_default_types/3`

---

### default_message_with_env

A module for interacting with local and global options inside
Return the default message with all environment variables set.

```erlang
default_message_with_env() ->
    maps:fold(
        fun(Key, _Spec, NodeMsg) ->
            case global_get(Key, undefined, #{}) of
                undefined -> NodeMsg;
                Value -> NodeMsg#{ Key => Value }
            end
        end,
        default_message(),
        ?ENV_KEYS
    ).
```

### default_message

The default configuration options of the hyperbeam node.

```erlang
default_message() ->
    #{
        %%%%%%%% Functional options %%%%%%%%
        hb_config_location => <<"config.flat">>,
        initialized => true,
        %% What HTTP client should the node use?
        %% Options: gun, httpc
        http_client => gun,
        %% Scheduling mode: Determines when the SU should inform the recipient
        %% that an assignment has been scheduled for a message.
```

### get

Get an option from the global options, optionally overriding with a

```erlang
get(Key) -> ?MODULE:get(Key, undefined).
```

### get

Get an option from the global options, optionally overriding with a

```erlang
get(Key, Default) -> ?MODULE:get(Key, Default, #{}).
```

### get

Get an option from the global options, optionally overriding with a

```erlang
get(Key, Default, Opts) when is_binary(Key) ->
    try binary_to_existing_atom(Key, utf8) of
        AtomKey -> do_get(AtomKey, Default, Opts)
    catch
        error:badarg -> do_get(Key, Default, Opts)
    end;
```

### get

Get an option from the global options, optionally overriding with a

```erlang
get(Key, Default, Opts) ->
    do_get(Key, Default, Opts).
```

### do_get

```erlang
do_get(Key, Default, Opts = #{ <<"only">> := Only }) ->
    do_get(Key, Default, maps:remove(<<"only">>, Opts#{ only => Only }));
```

### do_get

```erlang
do_get(Key, Default, Opts = #{ <<"prefer">> := Prefer }) ->
    do_get(Key, Default, maps:remove(<<"prefer">>, Opts#{ prefer => Prefer }));
```

### do_get

```erlang
do_get(Key, Default, Opts = #{ only := local }) ->
    case maps:find(Key, Opts) of
        {ok, Value} -> Value;
        error -> 
            Default
    end;
```

### do_get

```erlang
do_get(Key, Default, Opts = #{ only := global }) ->
    case global_get(Key, hb_opts_not_found, Opts) of
        hb_opts_not_found -> Default;
        Value -> Value
    end;
```

### do_get

```erlang
do_get(Key, Default, Opts = #{ prefer := global }) ->
    case do_get(Key, hb_opts_not_found, #{ only => global }) of
        hb_opts_not_found -> do_get(Key, Default, Opts#{ only => local });
        Value -> Value
    end;
```

### do_get

```erlang
do_get(Key, Default, Opts = #{ prefer := local }) ->
    case do_get(Key, hb_opts_not_found, Opts#{ only => local }) of
        hb_opts_not_found ->
            do_get(Key, Default, Opts#{ only => global });
        Value -> Value
    end;
```

### do_get

```erlang
do_get(Key, Default, Opts) ->
    % No preference was set in Opts, so we default to local.
```

### global_get

Get an environment variable or configuration key. Depending on whether

```erlang
global_get(Key, Default, Opts) ->
    case erlang:get({processed_env, Key}) of
        {cached, Value} -> Value;
        undefined ->
            % Thee value is not cached, so we need to process it.
```

### cached_os_env

Cache the result of os:getenv/1 in the process dictionary, as it never

```erlang
cached_os_env(Key, DefaultValue) ->
    case erlang:get({os_env, Key}) of
        {cached, false} -> DefaultValue;
        {cached, Value} -> Value;
        undefined ->
            % The process dictionary returns `undefined' for a key that is not
            % set, so we need to check the environment and store the result.
```

### normalize_default

Get an option from environment variables, optionally consulting the

```erlang
normalize_default({conditional, Feature, IfTest, Else}) ->
    case hb_features:enabled(Feature) of
        true -> IfTest;
        false -> Else
    end;
```

### normalize_default

Get an option from environment variables, optionally consulting the
An abstraction for looking up configuration variables. In the future,

```erlang
normalize_default(Default) -> Default.
```

### config_lookup

Get an option from environment variables, optionally consulting the
An abstraction for looking up configuration variables. In the future,
Parse a `flat@1.0` encoded file into a map, matching the types of the 

```erlang
config_lookup(Key, Default, _Opts) -> maps:get(Key, default_message(), Default).
```

### load

Get an option from environment variables, optionally consulting the
An abstraction for looking up configuration variables. In the future,
Parse a `flat@1.0` encoded file into a map, matching the types of the 

```erlang
load(Path) -> load(Path, #{}).
```

### load

Get an option from environment variables, optionally consulting the
An abstraction for looking up configuration variables. In the future,
Parse a `flat@1.0` encoded file into a map, matching the types of the 

```erlang
load(Path, Opts) ->
    {ok, Device} = path_to_device(Path),
    case file:read_file(Path) of
        {ok, Bin} ->
            load_bin(Device, Bin, Opts);
        _ -> {error, not_found}
    end.
```

### path_to_device

Convert a path to a device from its file extension. If no extension is

```erlang
path_to_device(Path) ->
    case binary:split(hb_util:bin(Path), <<".">>, []) of
        [_, Extension] ->
            ?event(debug_node_msg,
                {path_to_device,
                    {path, Path},
                    {extension, Extension}
                }
            ),
            extension_to_device(Extension);
        _ -> {ok, <<"flat@1.0">>}
    end.
```

### extension_to_device

Convert a file extension to a device name.

```erlang
extension_to_device(Ext) ->
    extension_to_device(Ext, maps:get(preloaded_devices, default_message())).
```

### extension_to_device

```erlang
extension_to_device(_, []) -> {error, not_found};
```

### extension_to_device

```erlang
extension_to_device(Ext, [#{ <<"name">> := Name }|Rest]) ->
    case binary:match(Name, Ext) of
        nomatch -> extension_to_device(Ext, Rest);
        {0, _} -> {ok, Name}
    end.
```

### load_bin

Parse a given binary with a device (defaulting to `flat@1.0`) into a

```erlang
load_bin(Bin, Opts) ->
    load_bin(<<"flat@1.0">>, Bin, Opts).
```

### load_bin

Parse a given binary with a device (defaulting to `flat@1.0`) into a

```erlang
load_bin(<<"flat@1.0">>, Bin, Opts) ->
    % Trim trailing whitespace from each line in the file.
```

### load_bin

```erlang
load_bin(Device, Bin, Opts) ->
    try
        {
            ok,
            mimic_default_types(
                hb_cache:ensure_all_loaded(
                    hb_message:convert(Bin, <<"structured@1.0">>, Device, Opts),
                    Opts
                ),
                new_atoms,
                Opts
            )
        }
    catch error:B -> {error, B}
    end.
```

### mimic_default_types

Mimic the types of the default message for a given map.

```erlang
mimic_default_types(Map, Mode, Opts) ->
    Default = default_message_with_env(),
    hb_maps:from_list(lists:map(
        fun({Key, Value}) ->
            NewKey = try hb_util:key_to_atom(Key, Mode) catch _:_ -> Key end,
            NewValue = 
                case hb_maps:get(NewKey, Default, not_found, Opts) of
                    not_found -> Value;
                    DefaultValue when is_atom(DefaultValue) ->
                        hb_util:atom(Value);
                    DefaultValue when is_integer(DefaultValue) ->
                        hb_util:int(Value);
                    DefaultValue when is_float(DefaultValue) ->
                        hb_util:float(Value);
                    DefaultValue when is_binary(DefaultValue) ->
                        Value;
                    _ -> Value
                end,
            {NewKey, NewValue}
        end,
        hb_maps:to_list(Map, Opts)
    )).
```

### as

Find a given identity from the `identities` map, and return the options

```erlang
as(Identity, Opts) ->
    case identities(Opts) of
        #{ Identity := SubOpts } ->
            ?event({found_identity_sub_opts_are, SubOpts}),
            {ok, maps:merge(Opts, mimic_default_types(SubOpts, new_atoms, Opts))};
        _ ->
            {error, not_found}
    end.
```

### identities

Find all known IDs and their sub-options from the `priv_ids` map. Allows

```erlang
identities(Opts) ->
    identities(hb:wallet(), Opts).
```

### identities

```erlang
identities(Default, Opts) ->
    Named = ?MODULE:get(identities, #{}, Opts),
    % Generate an address-based map of identities.
```

### check_required_opts

Utility function to check for required options in a list.

```erlang
-spec check_required_opts(list({binary(), term()}), map()) -> 
    {ok, map()} | {error, binary()}.
```

```erlang
check_required_opts(KeyValuePairs, Opts) ->
    MissingOpts = lists:filtermap(
        fun({Name, Value}) ->
            case Value of
                not_found -> {true, Name};
                _ -> false
            end
        end,
        KeyValuePairs
    ),
    case MissingOpts of
        [] -> 
            {ok, Opts};
        _ ->
            MissingOptsStr = binary:list_to_bin(
                lists:join(<<", ">>, MissingOpts)
            ),
            ErrorMsg = <<"Missing required opts: ", MissingOptsStr/binary>>,
            {error, ErrorMsg}
    end.
```

### ensure_node_history

Ensures all items in a node history meet required configuration options.

```erlang
-spec ensure_node_history(NodeHistory :: list() | term(), RequiredOpts :: map()) -> 
    {ok, binary()} | {error, binary()}.
```

```erlang
ensure_node_history(Opts, RequiredOpts) ->
    ?event(validate_history_items, {required_opts, RequiredOpts}),
    maybe
        % Get the node history from the options
        NodeHistory = hb_opts:get(node_history, [], Opts),
        % Add the Opts to the node history to validate all items
        NodeHistoryWithOpts = [ Opts | NodeHistory ],
        % Normalize required options
        NormalizedRequiredOpts ?= hb_ao:normalize_keys(RequiredOpts),
        % Normalize all node history items once
        NormalizedNodeHistory ?= lists:map(
            fun(Item) -> 
                hb_ao:normalize_keys(Item)
            end,
            NodeHistoryWithOpts
        ),
        % Get the first item (complete opts) and remaining items (differences)
        [FirstItem | RemainingItems] = NormalizedNodeHistory,
        % Step 2: Validate first item values match requirements
        FirstItemValuesMatch = hb_message:match(NormalizedRequiredOpts, FirstItem, primary),
        true ?= (FirstItemValuesMatch == true) orelse {error, values_invalid},
        % Step 3: Check that remaining items don't modify required keys
        NoRequiredKeysModified = lists:all(
            fun(HistoryItem) ->
                % For each required key, if it exists in this history item,
                % it must match the value from the first item
                hb_message:match(RequiredOpts, HistoryItem, only_present)
            end,
            RemainingItems
        ),
        true ?= NoRequiredKeysModified orelse {error, required_key_modified},
        % If we've made it this far, everything is valid
        ?event({validate_node_history_items, all_items_valid}),
        {ok, valid}
    else
        {error, values_invalid} ->
            ?event({validate_node_history_items, validation_failed, invalid_values}),
            {error, invalid_values};
        {error, required_key_modified} ->
            ?event({validate_node_history_items, validation_failed, required_key_modified}),
            {error, modified_required_key};
        _ ->
            ?event({validate_node_history_items, validation_failed, unknown}),
            {error, validation_failed}
    end.
```

### global_get_test

```erlang
global_get_test() ->
    ?assertEqual(debug, ?MODULE:get(mode)),
    ?assertEqual(debug, ?MODULE:get(mode, production)),
    ?assertEqual(undefined, ?MODULE:get(unset_global_key)),
    ?assertEqual(1234, ?MODULE:get(unset_global_key, 1234)).
```

### local_get_test

```erlang
local_get_test() ->
    Local = #{ only => local },
    ?assertEqual(undefined, 
        ?MODULE:get(test_key, undefined, Local)),
    ?assertEqual(correct,
        ?MODULE:get(test_key, undefined, Local#{ test_key => correct })).
```

### local_preference_test

```erlang
local_preference_test() ->
    Local = #{ prefer => local },
    ?assertEqual(correct,
        ?MODULE:get(test_key, undefined, Local#{ test_key => correct })),
    ?assertEqual(correct,
        ?MODULE:get(mode, undefined, Local#{ mode => correct })),
    ?assertNotEqual(undefined,
        ?MODULE:get(mode, undefined, Local)).
```

### global_preference_test

```erlang
global_preference_test() ->
    Global = #{ prefer => global },
    ?assertEqual(undefined, ?MODULE:get(test_key, undefined, Global)),
    ?assertNotEqual(incorrect,
        ?MODULE:get(mode, undefined, Global#{ mode => incorrect })),
    ?assertNotEqual(undefined, ?MODULE:get(mode, undefined, Global)).
```

### load_flat_test

```erlang
load_flat_test() ->
    % File contents:
    % port: 1234
    % host: https://ao.computer
    % await-inprogress: false
    {ok, Conf} = load("test/config.flat", #{}),
    ?event({loaded, {explicit, Conf}}),
    % Ensure we convert types as expected.
```

### load_json_test

```erlang
load_json_test() ->
    {ok, Conf} = load("test/config.json", #{}),
    ?event(debug_node_msg, {loaded, Conf}),
    ?assertEqual(1234, hb_maps:get(port, Conf)),
    ?assertEqual(9001, hb_maps:get(example, Conf)),
    % A binary
    ?assertEqual(<<"https://ao.computer">>, hb_maps:get(host, Conf)),
    % An atom, where the key contained a header-key `-' rather than a `_'.
```

### as_identity_test

```erlang
as_identity_test() ->
    DefaultWallet = ar_wallet:new(),
    TestWallet1 = ar_wallet:new(),
    TestWallet2 = ar_wallet:new(),
    TestID2 = hb_util:human_id(TestWallet2),
    Opts = #{
        test_key => 0,
        priv_wallet => DefaultWallet,
        identities => #{
            <<"testname-1">> => #{
                priv_wallet => TestWallet1,
                test_key => 1
            },
            TestID2 => #{
                priv_wallet => TestWallet2,
                test_key => 2
            }
        }
    },
    ?event({base_opts, Opts}),
    Identities = identities(Opts),
    ?event({identities, Identities}),
    % The number of identities should be 5: `default`, its ID, `testname-1`,
    % and its ID, and just the ID of `TestWallet2`.
```

### ensure_node_history_test

```erlang
ensure_node_history_test() ->
    % Define some test data
    RequiredOpts = #{
        key1 => 
            #{
                <<"type">> => <<"string">>,
                <<"value">> => <<"value1">>
            },
        key2 => <<"value2">>
    },
    % Test case: All items have required options
    ValidOpts =
    #{
        <<"key1">> => 
            #{
                <<"type">> => <<"string">>,
                <<"value">> => <<"value1">>
            }, 
        <<"key2">> => <<"value2">>, 
        <<"extra">> => <<"value">>,
        node_history => [
            #{
                <<"key1">> => 
                    #{
                        <<"type">> => <<"string">>,
                        <<"value">> => <<"value1">>
                    }, 
                <<"key2">> => <<"value2">>, 
                <<"extra">> => <<"value">>
            },
            #{
                <<"key1">> => 
                    #{
                        <<"type">> => <<"string">>,
                        <<"value">> => <<"value1">>
                    }, 
                <<"key2">> => <<"value2">>
            }
        ]
    },
    ?assertEqual({ok, valid}, ensure_node_history(ValidOpts, RequiredOpts)),
    ?event({valid_items, ValidOpts}),
    % Test Missing items
    MissingItems = 
    #{
        <<"key1">> => 
            #{
                <<"type">> => <<"string">>,
                <<"value">> => <<"value1">>
            }, 
        node_history => [
            #{
                <<"key1">> => 
                    #{
                        <<"type">> => <<"string">>,
                        <<"value">> => <<"value1">>
                    }
                % missing key2
            }
        ]
    },
    ?assertEqual({error, invalid_values}, ensure_node_history(MissingItems, RequiredOpts)),
    ?event({missing_items, MissingItems}),
    % Test Invalid items
    InvalidItems =
        #{
            <<"key1">> => 
                #{
                    <<"type">> => <<"string">>,
                    <<"value">> => <<"value">>
                }, 
            <<"key2">> => <<"value2">>,
            node_history =>
                [
                    #{
                        <<"key1">> => 
                            #{
                                <<"type">> => <<"string">>,
                                <<"value">> => <<"value2">>
                            },
                        <<"key2">> => <<"value3">>
                    }
                ]
        },
    ?assertEqual({error, invalid_values}, ensure_node_history(InvalidItems, RequiredOpts)).
```

---

*Generated from [hb_opts.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_opts.erl)*
