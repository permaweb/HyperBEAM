# dev_whois

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_whois.erl)

A device for returning the IP/host information of a requester or
itself.

---

## Exported Functions

- `echo/3`
- `ensure_host/1`
- `node/3`

---

### echo

A device for returning the IP/host information of a requester or
Return the calculated host information for the requester.
Return the host information for the node. Sets the `host` key in the

```erlang
echo(_, Req, Opts) ->
    {ok, hb_maps:get(<<"ao-peer">>, Req, <<"unknown">>, Opts)}.
```

### node

A device for returning the IP/host information of a requester or
Return the calculated host information for the requester.
Return the host information for the node. Sets the `host` key in the

```erlang
node(_, _, Opts) ->
    case ensure_host(Opts) of
        {ok, NewOpts} ->
            {ok, hb_opts:get(host, <<"unknown">>, NewOpts)};
        Error ->
            Error
    end.
```

### ensure_host

Return the node message ensuring that the host is set. If it is not, we

```erlang
ensure_host(Opts) ->
    case hb_opts:get(host, <<"unknown">>, Opts) of
        <<"unknown">> ->
            case bootstrap_node_echo(Opts) of
                {ok, Host} ->
                    % Set the host information in the persisted node message.
```

### bootstrap_node_echo

Find the local host information from the specified bootstrap node.

```erlang
bootstrap_node_echo(Opts) ->
    case hb_opts:get(host_bootstrap_node, false, Opts) of
        false ->
            {error, <<"No bootstrap node configured.">>};
        BootstrapNode ->
            hb_http:get(BootstrapNode, <<"/~whois@1.0/echo">>, Opts)
    end.
```

### find_self_test

```erlang
find_self_test() ->
    BoostrapNode =
        hb_http_server:start_node(#{
            priv_wallet => ar_wallet:new()
        }),
    PeerNode =
        hb_http_server:start_node(#{
            port => Port = rand:uniform(40000) + 10000,
            priv_wallet => ar_wallet:new(),
            host_bootstrap_node => BoostrapNode,
            http_client => httpc
        }),
    ?event({nodes, {peer, PeerNode}, {bootstrap, BoostrapNode}}),
    {ok, ReceivedPeerHost} = hb_http:get(PeerNode, <<"/~whois@1.0/node">>, #{}),
    ?event({find_self_test, ReceivedPeerHost}),
```

---

*Generated from [dev_whois.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_whois.erl)*
