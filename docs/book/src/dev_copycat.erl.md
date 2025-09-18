# dev_copycat

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_copycat.erl)

A device for orchestrating indexing of messages from foreign sources
into a HyperBEAM node's caches.
Supported sources of messages are as follows:
- A remote Arweave GraphQL endpoint.
- A remote Arweave node.
Each source is implemented as a separate engine, with `dev_copycat_[ENGINE]`
as the module name.

---

## Exported Functions

- `arweave/3`
- `graphql/3`

---

### graphql

A device for orchestrating indexing of messages from foreign sources
Fetch data from a GraphQL endpoint for replication. See 

```erlang
graphql(Base, Request, Opts) ->
    dev_copycat_graphql:graphql(Base, Request, Opts).
```

### arweave

Fetch data from an Arweave node for replication. See `dev_copycat_arweave`

```erlang
arweave(Base, Request, Opts) ->
```

---

*Generated from [dev_copycat.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_copycat.erl)*
