# dev_codec_cookie

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_codec_cookie.erl)

A utility device that manages setting and encoding/decoding the cookies
found in requests from a caller. This device implements the `~cookie@1.0`
codec, inline with the `~message@1.0` schema for conversion.
Additionally, a `commit` to a message using a secret generated and stored 
in the cookies of the caller, and a `verify` key that validates said
commitments. In addition, a `generate` key is provided to perform only the
generation side of the commitment process. The `finalize` key may be 
employed to add a `set` operation to the end of a message sequence, which
is used in hooks that need to ensure a caller always receives cookies
generated outside of the normal AO-Core execution flow. In totality, these
keys implement the `generator` interface type, and may be employed in
various contexts. For example, `~auth-hook@1.0` may be configured to use
this device to generate and store secrets in the cookies of the caller,
which are then used with the `~proxy-wallet@1.0` device to sign requests.
The `commit` and `verify` keys utilize the `~httpsig@1.0`'s HMAC `secret`
commitment scheme, which uses a secret key to commit to a message, with the
`committer` being listed as a hash of the secret.
This device supports the following paths:
`/commit`: Sets a `secret` key in the cookies of the caller. The name of 
the cookie is calculated as the hash of the secret. 
`/verify`: Verifies the caller's request by checking the committer in the
request matches the secret in the cookies of the base message.
`/store`: Sets the keys in the request message in the cookies of the caller.
`/extract`: Extracts the cookies from a base message.
`/reset`: Removes all cookie keys from the base message.
`/to`: Converts a message containing cookie sources (`cookie`, `set-cookie`,
or `priv/cookie`) into the format specified in the request message (e.g.
`set-cookie`, `cookie`).
`/from`: Converts a message containing encoded cookies into a message
containing the cookies parsed and normalized.

---

## Exported Functions

- `commit/3`
- `extract/3`
- `finalize/3`
- `from/3`
- `generate/3`
- `get_cookie/3`
- `opts/1`
- `reset/2`
- `store/3`
- `to/3`
- `verify/3`

---

### opts

A utility device that manages setting and encoding/decoding the cookies
Get the private store options to use for functions in the cookie device.

```erlang
opts(Opts) -> hb_private:opts(Opts).
%%% ~message@1.0 Commitments API keys.
```

### commit

```erlang
commit(Base, Req, RawOpts) -> dev_codec_cookie_auth:commit(Base, Req, RawOpts).
```

### verify

Preprocessor keys that utilize cookies and the `~secret@1.0` device to

```erlang
verify(Base, Req, RawOpts) -> dev_codec_cookie_auth:verify(Base, Req, RawOpts).
```

### generate

Preprocessor keys that utilize cookies and the `~secret@1.0` device to

```erlang
generate(Base, Req, Opts) ->
    dev_codec_cookie_auth:generate(Base, Req, Opts).
```

### finalize

Finalize an `on-request` hook by adding the `set-cookie` header to the

```erlang
finalize(Base, Request, Opts) ->
    dev_codec_cookie_auth:finalize(Base, Request, Opts).
```

### get_cookie

Get the cookie with the given key from the base message. The format of

```erlang
get_cookie(Base, Req, RawOpts) ->
    Opts = opts(RawOpts),
    {ok, Cookies} = extract(Base, Req, Opts),
    Key = hb_maps:get(<<"key">>, Req, undefined, Opts),
    case hb_maps:get(Key, Cookies, undefined, Opts) of
        undefined -> {error, not_found};
        Cookie ->
            Format = hb_maps:get(<<"format">>, Req, <<"default">>, Opts),
            case Format of
                <<"default">> -> {ok, Cookie};
                <<"set-cookie">> -> {ok, normalize_cookie_value(Cookie)};
                <<"cookie">> -> {ok, value(Cookie)}
            end
    end.
```

### extract

Return the parsed and normalized cookies from a message.

```erlang
extract(Msg, Req, Opts) ->
    {ok, MsgWithCookie} = from(Msg, Req, Opts),
    Cookies = hb_private:get(<<"cookie">>, MsgWithCookie, #{}, Opts),
    {ok, Cookies}.
```

### store

Set the keys in the request message in the cookies of the caller. Removes

```erlang
store(Base, Req, RawOpts) ->
    Opts = opts(RawOpts),
    ?event({store, {base, Base}, {req, Req}}),
    {ok, ExistingCookies} = extract(Base, Req, Opts),
    ?event({store, {existing_cookies, ExistingCookies}}),
    {ok, ResetBase} = reset(Base, Opts),
    ?event({store, {reset_base, ResetBase}}),
    MsgToSet =
        hb_maps:without(
            [
                <<"path">>,
                <<"accept-bundle">>,
                <<"ao-peer">>,
                <<"host">>,
                <<"method">>,
                <<"body">>
            ],
            hb_private:reset(Req),
            Opts
        ),
    ?event({store, {msg_to_set, MsgToSet}}),
    NewCookies = hb_maps:merge(ExistingCookies, MsgToSet, Opts),
    NewBase = hb_private:set(ResetBase, <<"cookie">>, NewCookies, Opts),
    {ok, NewBase}.
```

### reset

Remove all cookie keys from the given message (including `cookie` and

```erlang
reset(Base, RawOpts) ->
    Opts = opts(RawOpts),
    WithoutBaseCookieKeys =
        hb_maps:without(
            [<<"cookie">>, <<"set-cookie">>],
            Base,
            Opts
        ),
    WithoutPrivCookie =
        hb_private:set(
            WithoutBaseCookieKeys,
            <<"cookie">>,
            unset,
            Opts
        ),
    {ok, WithoutPrivCookie}.
```

### to

Convert a message containing cookie sources (`cookie`, `set-cookie`,

```erlang
to(Msg, Req, Opts) ->
    ?event({to, {msg, Msg}, {req, Req}}),
    CookieOpts = opts(Opts),
    LoadedMsg = hb_cache:ensure_all_loaded(Msg, CookieOpts),
    ?event({to, {loaded_msg, LoadedMsg}}),
    do_to(LoadedMsg, Req, CookieOpts).
```

### do_to

```erlang
do_to(Msg, Req = #{ <<"format">> := <<"set-cookie">> }, Opts) when is_map(Msg) ->
    ?event({to_set_cookie, {msg, Msg}, {req, Req}}),
    {ok, ExtractedParsedCookies} = extract(Msg, Req, Opts),
    {ok, ResetBase} = reset(Msg, Opts),
    SetCookieLines =
        maps:values(
            maps:map(
                fun to_set_cookie_line/2,
                ExtractedParsedCookies
            )
        ),
    MsgWithSetCookie =
        ResetBase#{
            <<"set-cookie">> => SetCookieLines
        },
    {ok, MsgWithSetCookie};
```

### do_to

```erlang
do_to(Msg, Req = #{ <<"format">> := <<"cookie">> }, Opts) when is_map(Msg) ->
    ?event({to_cookie, {msg, Msg}, {req, Req}}),
    {ok, ExtractedParsedCookies} = extract(Msg, Req, Opts),
    {ok, ResetBase} = reset(Msg, Opts),
    CookieLines =
        hb_maps:values(
            hb_maps:map(
            fun to_cookie_line/2,
                ExtractedParsedCookies,
                Opts
            ),
            Opts
        ),
    ?event({to_cookie, {cookie_lines, CookieLines}}),
    CookieLine = join(CookieLines, <<"; ">>),
    {ok, ResetBase#{ <<"cookie">> => CookieLine }};
```

### do_to

```erlang
do_to(Msg, _Req, _Opts) when is_map(Msg) ->
    error({cookie_to_error, {no_format_specified, Msg}});
```

### do_to

```erlang
do_to(Msg, _Req, _Opts) ->
    error({cookie_to_error, {unexpected_message_format, Msg}}).
```

### to_set_cookie_line

Convert a single cookie into a `set-cookie` header line. The cookie 

```erlang
to_set_cookie_line(Key, RawCookie) ->
    Cookie = normalize_cookie_value(RawCookie),
    % Encode the cookie key-value pair as a string to use as the base.
```

### to_cookie_line

Convert a single cookie into a `cookie` header component. These

```erlang
to_cookie_line(Key, Cookie) ->
    to_set_cookie_line(Key, value(Cookie)).
```

### from

Normalize a message containing a `cookie`, `set-cookie`, and potentially

```erlang
from(Msg, Req, Opts) ->
    CookieOpts = opts(Opts),
    LoadedMsg = hb_cache:ensure_all_loaded(Msg, Opts),
    do_from(LoadedMsg, Req, CookieOpts).
```

### do_from

```erlang
do_from(Msg, Req, Opts) when is_map(Msg) ->
    {ok, ResetBase} = reset(Msg, Opts),
    % Get the cookies, parsed, from each available source.
```

### do_from

```erlang
do_from(CookiesMsg, _Req, _Opts) ->
    error({cookie_from_error, {unexpected_message_format, CookiesMsg}}).
```

### from_cookie

Convert the `cookie` key into a parsed cookie message. `cookie` headers

```erlang
from_cookie(#{ <<"cookie">> := Cookie }, Req, Opts) ->
    from_cookie(Cookie, Req, Opts);
```

### from_cookie

Convert the `cookie` key into a parsed cookie message. `cookie` headers

```erlang
from_cookie(Cookies, Req, Opts) when is_list(Cookies) ->
    MergedParsed =
        lists:foldl(
            fun(Cookie, Acc) ->
                {ok, Parsed} = from_cookie(Cookie, Req, Opts),
                hb_maps:merge(Acc, Parsed, Opts)
            end,
            #{},
            Cookies
        ),
    {ok, MergedParsed};
```

### from_cookie

Convert the `cookie` key into a parsed cookie message. `cookie` headers

```erlang
from_cookie(Cookie, _Req, _Opts) when is_binary(Cookie) ->
    BinaryCookiePairs = split(semicolon, Cookie),
    KeyValList =
        lists:map(
            fun(BinaryCookiePair) ->
                {[Key, Value], _Rest} = split(pair, BinaryCookiePair),
                {Key, hb_escape:decode(Value)}
            end,
            BinaryCookiePairs
        ),
    NormalizedMessage = maps:from_list(KeyValList),
    {ok, NormalizedMessage};
```

### from_cookie

Convert the `cookie` key into a parsed cookie message. `cookie` headers

```erlang
from_cookie(_MsgWithoutCookie, _Req, _Opts) ->
    % The cookie key is not present in the message, so we return an empty map.
```

### from_set_cookie

Convert a `set-cookie` header line into a cookie message. The `set-cookie`

```erlang
from_set_cookie(#{ <<"set-cookie">> := Cookie }, Req, Opts) ->
    ?event({from_set_cookie, {cookie, Cookie}}),
    from_set_cookie(Cookie, Req, Opts);
```

### from_set_cookie

Convert a `set-cookie` header line into a cookie message. The `set-cookie`

```erlang
from_set_cookie(MsgWithoutSet, _Req, _Opts) when is_map(MsgWithoutSet) ->
    % The set-cookie key is not present in the message, so we return an empty map.
```

### from_set_cookie

```erlang
from_set_cookie(Lines, Req, Opts) when is_list(Lines) ->
    MergedParsed =
        lists:foldl(
            fun(Line, Acc) ->
                {ok, Parsed} = from_set_cookie(Line, Req, Opts),
                hb_maps:merge(Acc, Parsed)
            end,
            #{},
            Lines
        ),
    {ok, MergedParsed};
```

### from_set_cookie

```erlang
from_set_cookie(Line, _Req, Opts) when is_binary(Line) ->
    {[Key, Value], Rest} = split(pair, Line),
    ValueDecoded = hb_escape:decode(Value),
    % If there is no remaining binary after the pair, we have a simple key-value
    % pair, returning just the binary as the value. Otherwise, we split the
    % remaining binary into attributes and flags and return a message with the
    % value and those parsed elements.
```

### to_sorted_list

Takes a message or list of binaries and returns a sorted list of key-

```erlang
to_sorted_list(Msg) when is_map(Msg) ->
    lists:keysort(
        1,
        [
            {trim_bin(hb_util:bin(K)), trim_bin(V)}
            || {K, V} <- maps:to_list(Msg)
        ]
    );
```

### to_sorted_list

Takes a message or list of binaries and returns a sorted list of key-

```erlang
to_sorted_list(Binaries) when is_list(Binaries) ->
    lists:sort(
        lists:map(
            fun(Bin) -> trim_bin(hb_util:bin(Bin)) end,
            Binaries
        )
    ).
```

### value

Take a single parse cookie and return only the value (ignoring attributes

```erlang
value(Msg) when is_map(Msg) ->
    maps:get(<<"value">>, Msg, Msg);
```

### value

Take a single parse cookie and return only the value (ignoring attributes

```erlang
value(Bin) when is_binary(Bin) ->
    Bin.
```

### normalize_cookie_value

Normalize a cookie value to a map with the following keys:

```erlang
normalize_cookie_value(Msg) when is_map(Msg) ->
    Msg#{
        <<"value">> => maps:get(<<"value">>, Msg, Msg),
        <<"attributes">> => maps:get(<<"attributes">>, Msg, #{}),
        <<"flags">> => maps:get(<<"flags">>, Msg, [])
    };
```

### normalize_cookie_value

Normalize a cookie value to a map with the following keys:

```erlang
normalize_cookie_value(Bin) when is_binary(Bin) ->
    #{
        <<"value">> => Bin,
        <<"attributes">> => #{},
        <<"flags">> => []
    }.
```

### trim_bin

Trim a binary of leading and trailing whitespace.

```erlang
trim_bin(Bin) when is_binary(Bin) ->
    list_to_binary(string:trim(binary_to_list(Bin))).
```

### join

Join a list of binaries into a `separator`-separated string. Abstracts

```erlang
join(Binaries, Separator) ->
    hb_util:bin(
        string:join(
            lists:map(fun hb_util:list/1, Binaries),
            hb_util:list(Separator)
        )
    ).
```

### split

Split a binary by a separator type (`pair`, `lines`, or `attributes`).

```erlang
split(pair, Bin) ->
    [Key, ValueRest] = binary:split(Bin, <<"=">>),
    {_, Value, Rest} = hb_util:split_depth_string_aware_single($;, ValueRest),
    {[Key, unquote(Value)], trim_leading(Rest)};
```

### split

Split a binary by a separator type (`pair`, `lines`, or `attributes`).

```erlang
split(lines, Bin) ->
    lists:map(fun trim_leading/1, hb_util:split_depth_string_aware($,, Bin));
```

### split

Split a binary by a separator type (`pair`, `lines`, or `attributes`).

```erlang
split(semicolon, Bin) ->
    lists:map(fun trim_leading/1, hb_util:split_depth_string_aware($;, Bin)).
```

### trim_leading

Remove leading whitespace from a binary, if present.

```erlang
trim_leading(Line) when not is_binary(Line) ->
    trim_leading(hb_util:bin(Line));
```

### trim_leading

Remove leading whitespace from a binary, if present.

```erlang
trim_leading(<<>>) -> <<>>;
```

### trim_leading

Remove leading whitespace from a binary, if present.

```erlang
trim_leading(<<" ", Rest/binary>>) -> trim_leading(Rest);
```

### trim_leading

Remove leading whitespace from a binary, if present.
Unquote a binary if it is quoted. If it is not quoted, we return the

```erlang
trim_leading(Line) -> Line.
```

### unquote

Remove leading whitespace from a binary, if present.
Unquote a binary if it is quoted. If it is not quoted, we return the

```erlang
unquote(<< $\", Rest/binary>>) ->
    {Unquoted, _} = hb_util:split_escaped_single($\", Rest),
    Unquoted;
```

---

*Generated from [dev_codec_cookie.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_codec_cookie.erl)*
