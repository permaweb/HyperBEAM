# dev_cu

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_cu.erl)

## Exported Functions

- `execute/2`
- `push/2`

---

### push

```erlang
push(Msg, S = #{ assignment := Assignment, logger := _Logger }) ->
    ?event(
        {pushing_message,
            {assignment, hb_util:id(Assignment, unsigned)},
            {message, hb_util:id(Msg, unsigned)}
        }
    ),
    case hb_client:compute(Assignment, Msg) of
        {ok, Results} ->
            ?event(computed_results),
            {ok, S#{ results => Results }};
        Error ->
            throw({cu_error, Error})
    end.
```

### execute

```erlang
execute(CarrierMsg, S) ->
    MaybeBundle = ar_bundles:hd(CarrierMsg),
    Store = hb_opts:get(store),
    Wallet = hb:wallet(),
    {ok, Results} =
        case MaybeBundle of
            #tx{data = #{ <<"body">> := _Msg, <<"assignment">> := Assignment }} ->
                % TODO: Execute without needing to call the SU unnecessarily.
```

---

*Generated from [dev_cu.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_cu.erl)*
