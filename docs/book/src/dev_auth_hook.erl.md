# dev_auth_hook

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_auth_hook.erl)

A device offering an on-request hook that signs incoming messages with
node-hosted wallets, in accordance with the node operator's configuration.
It is intended for deployment in environments where a node's users have
intrinsic reasons for trusting the node outside of the scope of this device.
For example, if executed on a node running in a Trusted Execution Environment
with `~snp@1.0`, or a node they operate or is operated by a trusted
third-party.
This device utilizes the `generator` interface type which other devices may
implement. The generator is used to find/create a secret based on a user's
request, which is then passed to the `~proxy-wallet@1.0` device and matched
with a wallet which is used to sign the request. The `generator` interface
may implement the following keys:
<pre>
    `generate` (optional): A key that generates a secret based on a
                           user's request. May return either the secret
                           directly, or a message with a `secret` key. If 
                           a message is returned, it is assumed to be a
                           modified version of the user's request and is
                           used for further processing.
    `finalize` (optional): A key that takes the message sequence after this
                           device has processed it and returns it in a
                           modified form.
</pre>
At present, the `~cookie-secret@1.0` and `~http-auth@1.0` devices implement
the `generator` interface. For example, the following hook definition will
use the `~cookie-secret@1.0` device to generate and manage wallets for
users, with authentication details stored in cookies:
<pre>
  "on": {
    "request": {
      "device": "auth-hook@1.0",
      "secret-provider": {
        "device": "cookie-secret@1.0"
      }
    }
  }
</pre>
`~auth-hook@1.0` expects to receive a `secret-provider` key in the hook
base message. It may optionally also take a `generate-path` and
`finalize-path`, which are used to generate the secret and post-process the
response. If either `X-path` keys are not present, the `generate` and
`finalize` paths are used upon the `secret-provider` message. If the secret
provider's device does not implement these keys, the operations are skipped.
Node operators may also specify a `when` message inside their hook definition
which is used to determine when messages should be signed. The supported keys
are:
<pre>
    `committers`: always | uncommitted | [committer1, or committer2, or ...]
    `keys`: always | [key1, or key2, or ...]
</pre>
Both keys are optional and can be combined to form 'and' conditions. For
example, the following hook definition will sign all uncommitted requests
that have the `Authorization` header:
<pre>
  "on": {
    "request": {
      "device": "auth-hook@1.0",
      "when": {
            "keys": ["authorization"],
            "committers": "uncommitted"
        }
      }
    }
</pre>

---

## Exported Functions

- `request/3`

---

### request

A device offering an on-request hook that signs incoming messages with
Process an incoming request through a key provider. The key provider

```erlang
request(Base, HookReq, Opts) ->
    ?event({auth_hook_request, {base, Base}, {hook_req, HookReq}}),
    maybe
        % Get the key provider from options and short-circuit if none is
        % provided.
```

### is_relevant

Check if the request is relevant to the hook base. Node operators may

```erlang
is_relevant(Base, Request, MessageSequence, Opts) ->
    Committers = is_relevant_from_committers(Base, Request, Opts),
    Keys =
        lists:any(
            fun(Msg) -> is_relevant_from_keys(Base, Msg, Opts) end,
            [Request | MessageSequence]
        ),
    ?event({auth_hook_is_relevant, {committers, Committers}, {keys, Keys}}),
    if Committers andalso Keys -> true;
        true -> {skip, {committers, Committers}, {keys, Keys}}
    end.
```

### is_relevant_from_committers

Check if the request is relevant to the hook base based on the committers

```erlang
is_relevant_from_committers(Base, Request, Opts) ->
    Config =
        hb_util:deep_get(
            [<<"when">>, <<"committers">>],
            Base,
            <<"uncommitted">>,
            Opts
        ),
    ?event({auth_hook_is_relevant_from_committers, {config, Config}, {base, Base}}),
    case Config of
        <<"always">> -> true;
        <<"uncommitted">> -> hb_message:signers(Request, Opts) == [];
        RelevantCommitters ->
            lists:any(
                fun(Signer) ->
                    lists:member(Signer, RelevantCommitters)
                end,
                hb_message:signers(Request, Opts)
            )
    end.
```

### is_relevant_from_keys

Check if the request is relevant to the hook base based on the presence

```erlang
is_relevant_from_keys(_Base, ID, _Opts) when is_binary(ID) ->
    false;
```

### is_relevant_from_keys

Check if the request is relevant to the hook base based on the presence

```erlang
is_relevant_from_keys(Base, {as, _, Msg}, Opts) ->
    is_relevant_from_keys(Base, Msg, Opts);
```

### is_relevant_from_keys

Check if the request is relevant to the hook base based on the presence

```erlang
is_relevant_from_keys(Base, {resolve, Msg}, Opts) ->
    is_relevant_from_keys(Base, Msg, Opts);
```

### is_relevant_from_keys

Check if the request is relevant to the hook base based on the presence

```erlang
is_relevant_from_keys(Base, Request, Opts) ->
    Config = hb_util:deep_get([<<"when">>, <<"keys">>], Base, <<"always">>, Opts),
    ?event(
        {
            auth_hook_is_relevant_from_keys,
            {config, Config},
            {base, Base},
            {request, Request}
        }
    ),
    case Config of
        <<"always">> -> true;
        RelevantKeys ->
            lists:any(
                fun(Key) ->
                    case hb_maps:find(Key, Request, Opts) of
                        {ok, _} -> true;
                        error -> false
                    end
                end,
                RelevantKeys
            )
    end.
```

### generate_secret

Normalize authentication credentials, generating new ones if needed.

```erlang
generate_secret(Provider, Request, Opts) ->
    case call_provider(<<"generate">>, Provider, Request, Opts) of
        {error, not_found} ->
            ?event({no_generate_handler, Provider}),
            {ok, Provider, strip_sensitive(Request, Opts)};
        {error, Err} ->
            % Forward the error. The main handler will fail to match this and
            % return the error to the user.
```

### strip_sensitive

Strip the `secret` field from a request.
Generate a wallet with the key if the `wallet` field is not present in

```erlang
strip_sensitive(Request, Opts) ->
    hb_maps:without([<<"secret">>], Request, Opts).
```

### generate_wallet

Strip the `secret` field from a request.
Generate a wallet with the key if the `wallet` field is not present in

```erlang
generate_wallet(Provider, Request, Opts) ->
    {ok, #{ <<"body">> := WalletID }} =
        dev_secret:generate(Provider, Request, Opts),
    ?event({generated_wallet, WalletID}),
    {ok, Provider, refresh_opts(Opts)}.
```

### sign_request

Sign a request using the configured key provider

```erlang
sign_request(Provider, Msg, Opts) ->
    case hb_maps:get(<<"skip-commit">>, Provider, true, Opts) of
        false ->
            % Skip signing and return the normalized message.
```

### maybe_sign_messages

Process a sequence of messages, signing those marked for signing

```erlang
maybe_sign_messages(Provider, SignedReq, Opts) ->
    Parsed = hb_singleton:from(SignedReq, Opts),
    ?event({auth_hook_parsed_messages, {sequence_length, length(Parsed)}}),
    SignKey = hb_opts:get(auth_hook_commit_key, ?DEFAULT_COMMIT_KEY, Opts),
    Processed = maybe_sign_messages(Provider, SignKey, Parsed, Opts),
    {ok, Processed}.
```

### maybe_sign_messages

```erlang
maybe_sign_messages(_Provider, _Key, [], _Opts) -> [];
```

### maybe_sign_messages

```erlang
maybe_sign_messages(Provider, Key, [Msg | Rest], Opts) when is_map(Msg) ->
    case hb_util:atom(hb_maps:get(Key, Msg, false, Opts)) of
        true ->
            Uncommitted = hb_message:uncommitted(Msg, Opts),
            ?event({auth_hook_signing_message, {uncommitted, Msg}}),
            case sign_request(Provider, Uncommitted, Opts) of
                {ok, Signed} ->
                    [
                        Signed
                    |
                        maybe_sign_messages(Provider, Key, Rest, Opts)
                    ];
                {error, Err} ->
                    ?event({auth_hook_sign_error, Err}),
                    [{error, Err}]
            end;
        _ ->
            [Msg | maybe_sign_messages(Provider, Key, Rest, Opts)]
    end;
```

### maybe_sign_messages

```erlang
maybe_sign_messages(Provider, Key, [Msg | Rest], Opts) ->
    [Msg | maybe_sign_messages(Provider, Key, Rest, Opts)].
```

### finalize

Finalize the response by adding authentication state

```erlang
finalize(KeyProvider, SignedReq, MessageSequence, Opts) ->
    % Add the signed request and message sequence to the response, mirroring the
    % structure of a normal `~hook@1.0' on-request hook.
```

### refresh_opts

Refresh the options and log an event if they have changed.

```erlang
refresh_opts(Opts) ->
    NewOpts = hb_http_server:get_opts(Opts),
    case NewOpts of
        Opts -> ?event(auth_hook_no_opts_change);
        _ ->
            ?event(
                {auth_hook_opts_changed,
                    {size_diff,
                        erlang:external_size(NewOpts) -
                            erlang:external_size(Opts)
                    }
                }
            )
    end,
    NewOpts.
```

### find_provider

Get the key provider from the base message or the defaults.

```erlang
find_provider(Base, Opts) ->
    case hb_maps:get(<<"secret-provider">>, Base, no_key_provider, Opts) of
        no_key_provider ->
            case hb_opts:get(hook_secret_provider, no_key_provider, Opts) of
                no_key_provider -> {error, no_key_provider};
                SecretProvider -> SecretProvider
            end;
        SecretProvider when is_binary(SecretProvider) ->
            {ok, #{ <<"device">> => SecretProvider }};
        SecretProvider when is_map(SecretProvider) ->
            {ok, SecretProvider};
        _ ->
            {error, invalid_auth_provider}
    end.
```

### call_provider

Find the appropriate handler for a key in the key provider.

```erlang
call_provider(Key, Provider, Request, Opts) ->
    ?event({call_provider, {key, Key}, {provider, Provider}, {req, Request}}),
    ExecKey = hb_maps:get(<< Key/binary, "-path">>, Provider, Key, Opts),
    ?event({call_provider, {exec_key, ExecKey}}),
    case hb_ao:resolve(Provider, Request#{ <<"path">> => ExecKey }, Opts) of
        {ok, Msg} when is_map(Msg) ->
            % The result is a message. We revert the path to its original value.
```

### ignored_keys

Default keys to ignore when signing

```erlang
ignored_keys(Msg, Opts) ->
    hb_maps:get(
        <<"ignored-keys">>,
        Msg,
        hb_opts:get(
            hook_auth_ignored_keys,
            ?DEFAULT_IGNORED_KEYS,
            Opts
        )
    ).
```

### cookie_test

```erlang
cookie_test() ->
    % Start a node with a secret-provider that uses the cookie device.
```

### http_auth_test

```erlang
http_auth_test() ->
    % Start a node with the `~http-auth@1.0' device as the secret-provider.
```

### chained_preprocess_test

```erlang
chained_preprocess_test() ->
    % Start a node with the `~http-auth@1.0' device as the secret-provider, with
    % a router chained afterwards in the request hook.
```

### when_test

```erlang
when_test() ->
    % Start a node with the `~http-auth@1.0' device as the secret-provider. Only
    % request commitment with the hook if the `Authorization' header is present.
```

### signers_from_commitments_response

The cookie hook test(s) call `GET /commitments`, which returns the 

```erlang
signers_from_commitments_response(Response, ServerWallet) ->
    ServerAddress = ar_wallet:to_address(ServerWallet),
    hb_maps:values(hb_maps:filtermap(
        fun(Key, Value) when ?IS_ID(Key) ->
            Type = hb_maps:get(<<"type">>, Value, not_found, #{}),
            Committer = hb_maps:get(<<"committer">>, Value, not_found, #{}),
            case {Type, Committer} of
                {<<"rsa-pss-sha512">>, ServerAddress} -> false;
                {<<"rsa-pss-sha512">>, _} -> {true, Committer};
                _ -> false
            end;
           (_Key, _Value) ->
            false
        end,
        Response,
        #{}
```

---

*Generated from [dev_auth_hook.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_auth_hook.erl)*
