# hb_link

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_link.erl)

Utility functions for working with links.

---

## Exported Functions

- `decode_all_links/1`
- `format_unresolved/1`
- `format_unresolved/2`
- `format_unresolved/3`
- `format/1`
- `format/2`
- `format/3`
- `is_link_key/1`
- `normalize/2`
- `normalize/3`
- `remove_link_specifier/1`

---

### normalize

Utility functions for working with links.
Takes a message and ensures that it is normalized:

```erlang
normalize(Msg, Opts) when is_map(Opts) ->
    normalize(Msg, hb_opts:get(linkify_mode, offload, Opts), Opts).
```

### normalize

```erlang
normalize(Msg, false, _Opts) ->
    Msg;
```

### normalize

```erlang
normalize(Msg, Mode, Opts) when is_map(Msg) ->
    maps:merge(
        maps:with([<<"commitments">>, <<"priv">>], Msg),
            maps:from_list(
                lists:map(
                    fun({Key, {link, ID, LinkOpts = #{ <<"type">> := <<"link">> }}}) ->
                        % The value is a link. Deconstruct it and ensure it is
                        % normalized (lazy links are made greedy, and both are
                        % returned in binary TABM form).
```

### normalize

```erlang
normalize(OtherVal, Mode, Opts) when is_list(OtherVal) ->
    lists:map(fun(X) -> normalize(X, Mode, Opts) end, OtherVal);
```

### normalize

```erlang
normalize(OtherVal, _Mode, _Opts) ->
    OtherVal.
```

### decode_all_links

Decode links embedded in the headers of a message.

```erlang
decode_all_links(Msg) when is_map(Msg) ->
    maps:from_list(
        lists:map(
            fun({Key, MaybeID}) ->
                case is_link_key(Key) of
                    true ->
                        NewKey = binary:part(Key, 0, byte_size(Key) - 5),
                        {NewKey, 
                            {
                                link,
                                MaybeID,
                                #{
                                    <<"type">> => <<"link">>,
                                    <<"lazy">> => false
                                }
                            }
                        };
                    _ -> {Key, MaybeID}
                end
            end,
            maps:to_list(Msg)
        )
    );
```

### decode_all_links

Decode links embedded in the headers of a message.

```erlang
decode_all_links(List) when is_list(List) ->
    lists:map(fun(X) -> decode_all_links(X) end, List);
```

### decode_all_links

Decode links embedded in the headers of a message.

```erlang
decode_all_links(OtherVal) ->
    OtherVal.
```

### is_link_key

Determine if a key is an encoded link.

```erlang
is_link_key(Key) when byte_size(Key) >= 5 ->
    binary:part(Key, byte_size(Key) - 5, 5) =:= <<"+link">>;
```

### is_link_key

Determine if a key is an encoded link.
Remove any `+link` suffixes from a key.

```erlang
is_link_key(_) -> false.
```

### remove_link_specifier

Determine if a key is an encoded link.
Remove any `+link` suffixes from a key.

```erlang
remove_link_specifier(Key) ->
    case is_link_key(Key) of
        true -> binary:part(Key, 0, byte_size(Key) - 5);
        false -> Key
    end.
```

### format

Format a link as a short string suitable for printing. Checks the node

```erlang
format(Link) -> format(Link, #{}).
```

### format

Format a link as a short string suitable for printing. Checks the node

```erlang
format(Link, Opts) ->
    format(Link, Opts, 0).
```

### format

```erlang
format(Link, Opts, Indent) ->
    case hb_opts:get(debug_resolve_links, false, Opts) of
        true ->
            try
                hb_format:message(
                    hb_cache:ensure_all_loaded(Link, Opts),
                    Opts,
                    Indent
                )
            catch
                _:_ -> << "!UNRESOLVABLE! ", (format_unresolved(Link, Opts))/binary >>
            end;
        false -> format_unresolved(Link, Opts, Indent)
    end.
```

### format_unresolved

Format a link without resolving it.

```erlang
format_unresolved(Link) ->
    format_unresolved(Link, #{}).
```

### format_unresolved

```erlang
format_unresolved({link, ID, Opts}, BaseOpts) ->
    format_unresolved({link, ID, Opts}, BaseOpts, 0).
```

### format_unresolved

```erlang
format_unresolved({link, ID, Opts}, BaseOpts, Indent) ->
    hb_util:bin(
        hb_format:indent(
            "~s~s: ~s",
            [
                case maps:get(<<"lazy">>, Opts, false) of
                    true -> <<"Lazy link">>;
                    false -> <<"Link">>
                end,
                case maps:get(<<"type">>, Opts, no_type) of
                    no_type -> <<>>;
                    Type -> <<" (to ", (hb_util:bin(Type))/binary, ")" >>
                end,
                ID
            ],
            BaseOpts,
            Indent
        )
    ).
```

### offload_linked_message_test

```erlang
offload_linked_message_test() ->
    Opts = #{},
    Msg = #{
        <<"immediate-key">> => <<"immediate-value">>,
        <<"link-key">> => #{
            <<"immediate-key-2">> => <<"link-value">>,
            <<"link-key-2">> => #{
                <<"immediate-key-3">> => <<"link-value-2">>
            }
        }
    },
    Offloaded = normalize(Msg, offload, Opts),
    Structured = hb_message:convert(Offloaded, <<"structured@1.0">>, tabm, Opts),
    ?event(linkify, {test_recvd_linkified, {msg, Structured}}),
    Loaded = hb_cache:ensure_all_loaded(Structured, Opts),
    ?event(linkify, {test_recvd_loaded, {msg, Loaded}}),
    ?assertEqual(Msg, Loaded).
```

### offload_list_test

```erlang
offload_list_test() ->
    Opts = #{},
    Msg = #{
        <<"list-key">> => [1.0, 2.0, 3.0]
    },
    TABM = hb_message:convert(Msg, tabm, <<"structured@1.0">>, Opts),
    Linkified = normalize(TABM, offload, Opts),
    Msg2 = hb_message:convert(Linkified, <<"structured@1.0">>, tabm, Opts),
    Res = hb_cache:ensure_all_loaded(Msg2, Opts),
    ?assertEqual(Msg, Res).
```

---

*Generated from [hb_link.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_link.erl)*
