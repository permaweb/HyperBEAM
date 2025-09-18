# dev_lua_lib

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_lua_lib.erl)

A module for providing AO library functions to the Lua environment.
This module contains the implementation of the functions, each by the name
that should be used in the `ao` table in the Lua environment. Every export
is imported into the Lua environment.
Each function adheres closely to the Luerl calling convention, adding the 
appropriate node message as a third argument:
    fun(Args, State, NodeMsg) -> {ResultTerms, NewState}
As Lua allows for multiple return values, each function returns a list of
terms to grant to the caller. Matching the tuple convention used by AO-Core,
the first term is typically the status, and the second term is the result.

---

## Exported Functions

- `event/3`
- `get/3`
- `install/3`
- `resolve/3`
- `set/3`

---

### install

A module for providing AO library functions to the Lua environment.
Install the library into the given Lua environment.

```erlang
install(Base, State, Opts) ->
    % Calculate and set the new `preloaded_devices' option.
```

### return

Helper function for returning a result from a Lua function.

```erlang
return(Result, ExecState, Opts) ->
    ?event(lua_import, {import_returning, {result, Result}}),
    TableEncoded = dev_lua:encode(hb_cache:ensure_all_loaded(Result, Opts), Opts),
    {ReturnParams, ResultingState} =
        lists:foldr(
            fun(LuaEncoded, {Params, StateIn}) ->
                {NewParam, NewState} = luerl:encode(LuaEncoded, StateIn),
                {[NewParam | Params], NewState}
            end,
            {[], ExecState},
            TableEncoded
        ),
    ?event({lua_encoded, ReturnParams}),
    {ReturnParams, ResultingState}.
```

### resolve

A wrapper function for performing AO-Core resolutions. Offers both the 

```erlang
resolve([SingletonMsg], ExecState, ExecOpts) ->
    ?event({ao_core_resolver, {msg, SingletonMsg}}),
    ParsedMsgs = hb_singleton:from(SingletonMsg, ExecOpts),
    ?event({parsed_msgs_to_resolve, ParsedMsgs}),
    resolve({many, ParsedMsgs}, ExecState, ExecOpts);
```

### resolve

A wrapper function for performing AO-Core resolutions. Offers both the 

```erlang
resolve([Base, Path], ExecState, ExecOpts) when is_binary(Path) ->
    PathParts = hb_path:term_to_path_parts(Path, ExecOpts),
    resolve({many, [Base] ++ PathParts}, ExecState, ExecOpts);
```

### resolve

A wrapper function for performing AO-Core resolutions. Offers both the 

```erlang
resolve(Msgs, ExecState, ExecOpts) when is_list(Msgs) ->
    resolve({many, Msgs}, ExecState, ExecOpts);
```

### resolve

A wrapper function for performing AO-Core resolutions. Offers both the 

```erlang
resolve({many, Msgs}, ExecState, ExecOpts) ->
    MaybeAsMsgs = lists:map(fun convert_as/1, Msgs),
    try hb_ao:resolve_many(MaybeAsMsgs, ExecOpts) of
        {Status, Res} ->
            ?event({resolved_msgs, {status, Status}, {res, Res}, {exec_opts, ExecOpts}}),
            {[Status, Res], ExecState}
    catch
        Error ->
            ?event(lua_error, {ao_core_resolver_error, Error}),
            {[<<"error">>, Error], ExecState}
    end.
```

### get

A wrapper for `hb_ao`'s `get` functionality.

```erlang
get([Key, Base], ExecState, ExecOpts) ->
    ?event({ao_core_get, {base, Base}, {key, Key}}),
    NewRes = hb_ao:get(convert_as(Key), convert_as(Base), ExecOpts),
    ?event({ao_core_get_result, {result, NewRes}}),
    {[NewRes], ExecState}.
```

### convert_as

Converts any `as` terms from Lua to their HyperBEAM equivalents.

```erlang
convert_as([<<"as">>, Device, RawMsg]) ->
    {as, Device, RawMsg};
```

### convert_as

Converts any `as` terms from Lua to their HyperBEAM equivalents.

```erlang
convert_as(Other) ->
    Other.
```

### set

Wrapper for `hb_ao`'s `set` functionality.

```erlang
set([Base, Key, Value], ExecState, ExecOpts) ->
    ?event({ao_core_set, {base, Base}, {key, Key}, {value, Value}}),
    NewRes = hb_ao:set(Base, Key, Value, ExecOpts),
    ?event({ao_core_set_result, {result, NewRes}}),
    {[NewRes], ExecState};
```

### set

Wrapper for `hb_ao`'s `set` functionality.

```erlang
set([Base, NewValues], ExecState, ExecOpts) ->
    ?event({ao_core_set, {base, Base}, {new_values, NewValues}}),
    NewRes = hb_ao:set(Base, NewValues, ExecOpts),
    ?event({ao_core_set_result, {result, NewRes}}),
    {[NewRes], ExecState}.
```

### event

Allows Lua scripts to signal events using the HyperBEAM hosts internal

```erlang
event([Event], ExecState, Opts) ->
    ?event({recalling_event, Event}),
    event([global, Event], ExecState, Opts);
```

### event

Allows Lua scripts to signal events using the HyperBEAM hosts internal

```erlang
event([Group, Event], State, Opts) when is_list(Event) ->
    event([Group, list_to_tuple(Event)], State, Opts);
```

### event

Allows Lua scripts to signal events using the HyperBEAM hosts internal

```erlang
event([Group, Event], ExecState, Opts) ->
    ?event(
        lua_event,
        {event,
            {group, Group},
            {event, Event}
        }
    ),
    ?event(Group, Event),
```

---

*Generated from [dev_lua_lib.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_lua_lib.erl)*
