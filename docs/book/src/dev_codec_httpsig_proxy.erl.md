# dev_codec_httpsig_proxy

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_codec_httpsig_proxy.erl)

A utility module that contains proxy functions for calling the
`~httpsig@1.0` codec's HMAC commitment functions with secret keys.
These tools are helpful for implementing a standardized pattern:
1. A device verifies a user's request/derives a secret key for them.
2. The device then wants to commit a message with the user's secret key
   using the `secret:[h(secret)]` commitment scheme.
3. The commitment must then be modified to reference a different device
   as the `commitment-device` key.
4. When `/verify` is called, the `~httpsig@1.0` codec is used under-the-hood
   to validate the commitment on the re-derived secret key.
This module is currently used by the `~cookie@1.0` and `~http-auth@1.0`
devices.

---

## Exported Functions

- `commit/5`
- `verify/4`

---

### commit

A utility module that contains proxy functions for calling the
Commit to a given `Base` message with a given `Secret`, setting the 

```erlang
commit(Device, Secret, Base, Req, Opts) ->
    % If there are no existing commitments, we use the unmodified base message.
```

### verify

Verify a given `Base` message with a given `Secret` using the `~httpsig@1.0`

```erlang
verify(Secret, Base, RawReq, Opts) ->
    ProxyRequest =
        RawReq#{
            <<"commitment-device">> => <<"httpsig@1.0">>,
            <<"path">> => <<"verify">>,
            <<"secret">> => Secret
        },
    ?event({proxy_request, ProxyRequest}),
```

---

*Generated from [dev_codec_httpsig_proxy.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_codec_httpsig_proxy.erl)*
