# dev_faff

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_faff.erl)

A module that implements a 'friends and family' pricing policy.
It will allow users to process requests only if their addresses are
in the allow-list for the node.
Fundamentally against the spirit of permissionlessness, but it is useful if
you are running a node for your own purposes and would not like to allow 
others to make use of it -- even for a fee. It also serves as a useful
example of how to implement a custom pricing policy, as it implements stubs
for both the pricing and ledger P4 APIs.

---

## Exported Functions

- `charge/3`
- `estimate/3`

---

### estimate

A module that implements a 'friends and family' pricing policy.
Decide whether or not to service a request from a given address.

```erlang
estimate(_, Msg, NodeMsg) ->
    ?event(payment, {estimate, {msg, Msg}}),
    % Check if the address is in the allow-list.
```

### is_admissible

Check whether all of the signers of the request are in the allow-list.

```erlang
is_admissible(Msg, NodeMsg) ->
    AllowList = hb_opts:get(faff_allow_list, [], NodeMsg),
    Req = hb_ao:get(<<"request">>, Msg, NodeMsg),
    Signers = hb_message:signers(Req, NodeMsg),
    ?event(payment, {is_admissible, {signers, Signers}, {allow_list, AllowList}}),
    lists:all(
        fun(Signer) -> lists:member(Signer, AllowList) end,
        Signers
    ).
```

### charge

Charge the user's account if the request is allowed.

```erlang
charge(_, Req, _NodeMsg) ->
    ?event(payment, {charge, Req}),
    {ok, true}.
```

---

*Generated from [dev_faff.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_faff.erl)*
