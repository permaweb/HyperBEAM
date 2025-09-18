# dev_codec_httpsig_keyid

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_codec_httpsig_keyid.erl)

A library for extracting and validating key material for `httpsig@1.0`
requests. Offers support for the following keyid schemes:
- `publickey`: The keyid is an encoded public key with the `publickey:` prefix.
- `constant`: The key is simply the keyid itself, including the `public:`
  prefix if given.
- `secret`: The key is hashed and the `secret:` prefix is added to the
  result in order to generate a keyid.
These functions are abstracted in order to allow for the addition of new
schemes in the future.

---

## Exported Functions

- `keyid_to_committer/1`
- `keyid_to_committer/2`
- `remove_scheme_prefix/1`
- `req_to_key_material/2`
- `secret_key_to_committer/1`

---

### req_to_key_material

A library for extracting and validating key material for `httpsig@1.0`
Extract the key and keyid from a request, returning

```erlang
req_to_key_material(Req, Opts) ->
    ?event({req_to_key_material, {req, Req}}),
    KeyID = maps:get(<<"keyid">>, Req, undefined),
    ?event({keyid_to_key_material, {keyid, KeyID}}),
    case find_scheme(KeyID, Req, Opts) of
        {ok, Scheme} ->
            ?event({scheme_found, {scheme, Scheme}}),
            ApplyRes = apply_scheme(Scheme, KeyID, Req),
            ?event({apply_scheme_result, {apply_res, ApplyRes}}),
            case ApplyRes of
                {ok, _, CalcKeyID} when KeyID /= undefined, CalcKeyID /= KeyID ->
                    {error, key_mismatch};
                {ok, Key, CalcKeyID} ->
                    {ok, Scheme, Key, CalcKeyID};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, undefined_scheme} ->
            {ok, DefaultScheme} = req_to_default_scheme(Req, Opts),
            req_to_key_material(Req#{ <<"scheme">> => DefaultScheme }, Opts);
        {error, Reason} ->
            {error, Reason}
    end.
```

### find_scheme

Find the scheme from a keyid or request. Returns `{ok, Scheme}` or

```erlang
find_scheme(KeyID, Req = #{ <<"scheme">> := RawScheme }, Opts) ->
    Scheme = hb_util:atom(RawScheme),
    % Validate that the scheme in the request matches the scheme in the keyid.
```

### find_scheme

```erlang
find_scheme(undefined, _Req, _Opts) ->
    {error, undefined_scheme};
```

### find_scheme

```erlang
find_scheme(KeyID, Req, Opts) ->
    SchemeRes =
        case binary:split(KeyID, <<":">>) of
            [SchemeBin, _KeyID] -> {ok, SchemeBin};
            [_NoSchemeKeyID] ->
                % Determine the default scheme based on the `type' of the request.
```

### req_to_default_scheme

Determine the default scheme based on the `type` of the request.

```erlang
req_to_default_scheme(Req, _Opts) ->
    case maps:find(<<"type">>, Req) of
        {ok, Type} ->
            case maps:find(Type, ?DEFAULT_SCHEMES_BY_TYPE) of
                {ok, Scheme} -> {ok, Scheme};
                error -> {error, unsupported_scheme}
            end;
        error ->
            {error, no_request_type}
    end.
```

### apply_scheme

Apply the requested scheme to generate the key material (key and keyid).

```erlang
apply_scheme(publickey, KeyID, _Req) ->
    % Remove the `publickey:' prefix from the keyid and return the key.
```

### apply_scheme

```erlang
apply_scheme(constant, RawKeyID, _Req) ->
    % In the `constant' scheme, the key is simply the key itself, including the
    % `constant:' prefix if given.
```

### apply_scheme

```erlang
apply_scheme(secret, _KeyID, Req) ->
    % In the `secret' scheme, the key is hashed to generate a keyid.
```

### apply_scheme

```erlang
apply_scheme(_Scheme, _Key, _KeyID) ->
    {error, unsupported_scheme}.
```

### keyid_to_committer

Given a keyid and a scheme, generate the committer value for a commitment.

```erlang
keyid_to_committer(KeyID) ->
    case find_scheme(KeyID, #{}, #{}) of
        {ok, Scheme} -> keyid_to_committer(Scheme, KeyID);
        {error, _} -> undefined
    end.
```

### keyid_to_committer

```erlang
keyid_to_committer(publickey, KeyID) ->
    % Note: There is a subtlety here. The `KeyID' is decoded with the 
    % `hb_util:decode' function rather than `base64:decode'. The reason for this
    % is that certain codecs (e.g. `ans104@1.0') encode the public key with
    % `base64url' encoding, rather than the standard `base64' encoding in 
    % HTTPSig. Our `hb_util:decode' function handles both cases returning the
    % same raw bytes, and is subsequently safe.
```

### keyid_to_committer

```erlang
keyid_to_committer(secret, KeyID) ->
    remove_scheme_prefix(KeyID);
```

### keyid_to_committer

```erlang
keyid_to_committer(constant, _KeyID) ->
    undefined.
```

### secret_key_to_committer

Given a secret key, generate the committer value for a commitment.

```erlang
secret_key_to_committer(Key) ->
    hb_util:human_id(hb_crypto:sha256(Key)).
```

### remove_scheme_prefix

Remove the `scheme:` prefix from a keyid.

```erlang
remove_scheme_prefix(KeyID) ->
    case binary:split(KeyID, <<":">>) of
        [_Scheme, Key] -> Key;
        [Key] -> Key
    end.
```

---

*Generated from [dev_codec_httpsig_keyid.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_codec_httpsig_keyid.erl)*
