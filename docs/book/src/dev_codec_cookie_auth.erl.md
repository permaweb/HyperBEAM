# dev_codec_cookie_auth

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_codec_cookie_auth.erl)

Implements the `message@1.0` commitment interface for the `~cookie@1.0`,
as well as the `generator` interface type for the `~auth-hook@1.0` device.
See the [cookie codec](dev_codec_cookie.html) documentation for more details.

---

## Exported Functions

- `commit/3`
- `finalize/3`
- `generate/3`
- `verify/3`

---

### generate

Implements the `message@1.0` commitment interface for the `~cookie@1.0`,
Generate a new secret (if no `committer` specified), and use it as the

```erlang
generate(Base, Request, Opts) ->
    {WithCookie, Secrets} =
        case find_secrets(Request, Opts) of
            [] ->
                {ok, GeneratedSecret} = generate_secret(Base, Request, Opts),
                {ok, Updated} = store_secret(GeneratedSecret, Request, Opts),
                {Updated, [GeneratedSecret]};
            FoundSecrets ->
                {Request, FoundSecrets}
        end,
    ?event({normalized_cookies_found, {secrets, Secrets}}),
    {
        ok,
        WithCookie#{
            <<"secret">> => Secrets
        }
    }.
```

### finalize

Finalize an `on-request` hook by adding the cookie to the chain of 

```erlang
finalize(Base, Request, Opts) ->
    ?event(debug_auth, {finalize, {base, Base}, {request, Request}}),
    maybe
        {ok, SignedMsg} ?= hb_maps:find(<<"request">>, Request, Opts),
        {ok, MessageSequence} ?= hb_maps:find(<<"body">>, Request, Opts),
        % Cookie auth adds set-cookie to response
        {ok, #{ <<"set-cookie">> := SetCookie }} =
            dev_codec_cookie:to(
                SignedMsg,
                #{ <<"format">> => <<"set-cookie">> },
                Opts
            ),
        {
            ok,
            MessageSequence ++
                [#{ <<"path">> => <<"set">>, <<"set-cookie">> => SetCookie }]
        }
    else error ->
        {error, no_request}
    end.
```

### commit

Generate a new secret (if no `committer` specified), and use it as the

```erlang
commit(Base, Request, RawOpts) when ?IS_LINK(Request) ->
    Opts = dev_codec_cookie:opts(RawOpts),
    commit(Base, hb_cache:ensure_loaded(Request, Opts), Opts);
```

### commit

Generate a new secret (if no `committer` specified), and use it as the

```erlang
commit(Base, Req = #{ <<"secret">> := Secret }, RawOpts) ->
    Opts = dev_codec_cookie:opts(RawOpts),
    commit(hb_cache:ensure_loaded(Secret, Opts), Base, Req, Opts);
```

### commit

Generate a new secret (if no `committer` specified), and use it as the

```erlang
commit(Base, Request, RawOpts) ->
    Opts = dev_codec_cookie:opts(RawOpts),
    % Calculate the key to use for the commitment.
```

### commit

Given the secret key, commit the message and set the cookie. This 

```erlang
commit(Secret, Base, Request, Opts) ->
    {ok, CommittedMsg} =
        dev_codec_httpsig_proxy:commit(
            <<"cookie@1.0">>,
            Secret,
            Base,
            Request,
            Opts
        ),
    store_secret(Secret, CommittedMsg, Opts).
```

### store_secret

Update the nonces for a given secret.

```erlang
store_secret(Secret, Msg, Opts) ->
    CookieAddr = dev_codec_httpsig_keyid:secret_key_to_committer(Secret),
    % Create the cookie parameters, using the name as the key and the secret as
    % the value.
```

### verify

Verify the HMAC commitment with the key being the secret from the 

```erlang
verify(Base, ReqLink, RawOpts) when ?IS_LINK(ReqLink) ->
    Opts = dev_codec_cookie:opts(RawOpts),
    verify(Base, hb_cache:ensure_loaded(ReqLink, Opts), Opts);
```

### verify

Verify the HMAC commitment with the key being the secret from the 

```erlang
verify(Base, Req = #{ <<"secret">> := Secret }, RawOpts) ->
    Opts = dev_codec_cookie:opts(RawOpts),
    ?event({verify_with_explicit_key, {base, Base}, {request, Req}}),
    dev_codec_httpsig_proxy:verify(
        hb_util:decode(Secret),
        Base,
        Req,
        Opts
    );
```

### verify

Verify the HMAC commitment with the key being the secret from the 

```erlang
verify(Base, Request, RawOpts) ->
    Opts = dev_codec_cookie:opts(RawOpts),
    ?event({verify_finding_key, {base, Base}, {request, Request}}),
    case find_secret(Request, Opts) of
        {ok, Secret} ->
            dev_codec_httpsig_proxy:verify(
                hb_util:decode(Secret),
                Base,
                Request,
                Opts
            );
        {error, Err} ->
            {error, Err}
    end.
```

### generate_secret

Generate a new secret key for the given request. The user may specify

```erlang
generate_secret(_Base, Request, Opts) ->
    case hb_maps:get(<<"generator">>, Request, undefined, Opts) of
        undefined ->
            % If no generator is specified, use the default generator.
```

### default_generator

Generate a new secret key using the default generator.

```erlang
default_generator(_Opts) ->
    {ok, hb_util:encode(crypto:strong_rand_bytes(64))}.
```

### execute_generator

Execute a generator function. See `generate_secret/3` for more details.

```erlang
execute_generator(GeneratorPath, Opts) when is_binary(GeneratorPath) ->
    hb_ao:resolve(GeneratorPath, Opts);
```

### execute_generator

Execute a generator function. See `generate_secret/3` for more details.
Find all secrets in the cookie of a message.

```erlang
execute_generator(Generator, Opts) ->
    Path = hb_maps:get(<<"path">>, Generator, <<"generate">>, Opts),
    hb_ao:resolve(Generator#{ <<"path">> => Path }, Opts).
```

### find_secrets

Execute a generator function. See `generate_secret/3` for more details.
Find all secrets in the cookie of a message.

```erlang
find_secrets(Request, Opts) ->
    maybe
        {ok, Cookie} ?= dev_codec_cookie:extract(Request, #{}, Opts),
        [
            hb_maps:get(SecretRef, Cookie, secret_unavailable, Opts)
        ||
            SecretRef = <<"secret-", _/binary>> <- hb_maps:keys(Cookie)
        ]
    else error -> []
    end.
```

### find_secret

Find the secret key for the given committer, if it exists in the cookie.

```erlang
find_secret(Request, Opts) ->
    maybe
        {ok, Committer} ?= hb_maps:find(<<"committer">>, Request, Opts),
        find_secret(Committer, Request, Opts)
    else error -> {error, no_secret}
    end.
```

### find_secret

```erlang
find_secret(Committer, Request, Opts) ->
    maybe
        {ok, Cookie} ?= dev_codec_cookie:extract(Request, #{}, Opts),
        {ok, _Secret} ?= hb_maps:find(<<"secret-", Committer/binary>>, Cookie, Opts)
    else error -> {error, not_found}
    end.
```

### directly_invoke_commit_verify_test

Call the cookie codec's `commit` and `verify` functions directly.

```erlang
directly_invoke_commit_verify_test() ->
    Base = #{ <<"test-key">> => <<"test-value">> },
    CommittedMsg =
        hb_message:commit(
            Base,
            #{},
            #{
                <<"commitment-device">> => <<"cookie@1.0">>
            }
        ),
    ?event({committed_msg, CommittedMsg}),
    ?assertEqual(1, length(hb_message:signers(CommittedMsg, #{}))),
    VerifyReq =
        apply_cookie(
            CommittedMsg#{
                <<"committers">> => hb_message:signers(CommittedMsg, #{})
            },
            CommittedMsg,
            #{}
        ),
    VerifyReqWithoutComms = hb_maps:without([<<"commitments">>], VerifyReq, #{}),
    ?event({verify_req_without_comms, VerifyReqWithoutComms}),
    ?assert(hb_message:verify(CommittedMsg, VerifyReqWithoutComms, #{})),
    ok.
```

### http_set_get_cookies_test

Set keys in a cookie and verify that they can be parsed into a message.

```erlang
http_set_get_cookies_test() ->
    Node = hb_http_server:start_node(#{}),
    {ok, SetRes} =
        hb_http:get(
            Node,
            <<"/~cookie@1.0/store?k1=v1&k2=v2">>,
            #{}
        ),
    ?event(debug_cookie, {set_cookie_test, {set_res, SetRes}}),
    ?assertMatch(#{ <<"set-cookie">> := _ }, SetRes),
    Req = apply_cookie(#{ <<"path">> => <<"/~cookie@1.0/extract">> }, SetRes, #{}),
    {ok, Res} = hb_http:get(Node, Req, #{}),
    ?assertMatch(#{ <<"k1">> := <<"v1">>, <<"k2">> := <<"v2">> }, Res),
    ok.
```

### apply_cookie

Takes the cookies from the `GenerateResponse` and applies them to the

```erlang
apply_cookie(NextReq, GenerateResponse, Opts) ->
    {ok, Cookie} = dev_codec_cookie:extract(GenerateResponse, #{}, Opts),
    {ok, NextWithParsedCookie} = dev_codec_cookie:store(NextReq, Cookie, Opts),
    {ok, NextWithCookie} =
        dev_codec_cookie:to(
            NextWithParsedCookie,
            #{ <<"format">> => <<"cookie">> },
            Opts
        ),
```

---

*Generated from [dev_codec_cookie_auth.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_codec_cookie_auth.erl)*
