# dev_codec_httpsig_conv

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_codec_httpsig_conv.erl)

A codec that marshals TABM encoded messages to and from the "HTTP"
message structure.
Every HTTP message is an HTTP multipart message.
See https://datatracker.ietf.org/doc/html/rfc7578
For each TABM Key:
The Key/Value Pair will be encoded according to the following rules:
    "signatures" -> {SignatureInput, Signature} header Tuples, each encoded
					as a Structured Field Dictionary
    "body" ->
        - if a map, then recursively encode as its own HyperBEAM message
        - otherwise encode as a normal field
    _ -> encode as a normal field
Each field will be mapped to the HTTP Message according to the following 
rules:
    "body" -> always encoded part of the body as with Content-Disposition
			  type of "inline"
    _ ->
        - If the byte size of the value is less than the ?MAX_TAG_VALUE,
		  then encode as a header, also attempting to encode as a
		  structured field.
        - Otherwise encode the value as a part in the multipart response

---

## Exported Functions

- `encode_http_msg/2`
- `from/3`
- `to/3`

---

### from

A codec that marshals TABM encoded messages to and from the "HTTP"
Convert a HTTP Message into a TABM.

```erlang
from(Bin, _Req, _Opts) when is_binary(Bin) -> {ok, Bin};
```

### from

A codec that marshals TABM encoded messages to and from the "HTTP"
Convert a HTTP Message into a TABM.

```erlang
from(Link, _Req, _Opts) when ?IS_LINK(Link) -> {ok, Link};
```

### from

A codec that marshals TABM encoded messages to and from the "HTTP"
Convert a HTTP Message into a TABM.

```erlang
from(HTTP, _Req, Opts) ->
    % First, parse all headers excluding the signature-related headers, as they
    % are handled separately.
```

### body_to_tabm

Generate the body TABM from the `body` key of the encoded message.

```erlang
body_to_tabm(HTTP, Opts) ->
    % Extract the body and content-type from the HTTP message.
```

### body_to_parts

Split the body into parts, if it is a multipart.

```erlang
body_to_parts(_ContentType, no_body, _Opts) -> no_body;
```

### body_to_parts

Split the body into parts, if it is a multipart.

```erlang
body_to_parts(ContentType, Body, _Opts) ->
    ?event(
        {from_body,
            {content_type, {explicit, ContentType}},
            {body, Body}
        }
    ),
    Params =
        case ContentType of
            undefined -> [];
            _ ->
                {item, {_, _XT}, XParams} =
                    hb_structured_fields:parse_item(ContentType),
                XParams
        end,
    case lists:keyfind(<<"boundary">>, 1, Params) of
        false ->
            % The body is not a multipart, so just set as is to the Inlined key on
            % the TABM.
```

### from_body_part

Parse a single part of a multipart body into a TABM.

```erlang
from_body_part(InlinedKey, Part, Opts) ->
    % Extract the Headers block and Body. Only split on the FIRST double CRLF
    {RawHeadersBlock, RawBody} =
        case binary:split(Part, [?DOUBLE_CRLF], []) of
            [XRawHeadersBlock] ->
                % The message has no body.
```

### to

Convert a TABM into an HTTP Message. The HTTP Message is a simple Erlang Map

```erlang
to(TABM, Req, Opts) -> to(TABM, Req, [], Opts).
```

### to

Convert a TABM into an HTTP Message. The HTTP Message is a simple Erlang Map

```erlang
to(Bin, _Req, _FormatOpts, _Opts) when is_binary(Bin) -> {ok, Bin};
```

### to

Convert a TABM into an HTTP Message. The HTTP Message is a simple Erlang Map

```erlang
to(Link, _Req, _FormatOpts, _Opts) when ?IS_LINK(Link) -> {ok, Link};
```

### to

Convert a TABM into an HTTP Message. The HTTP Message is a simple Erlang Map

```erlang
to(TABM, Req = #{ <<"index">> := true }, _FormatOpts, Opts) ->
    % If the caller has specified that an `index` page is requested, we:
    % 1. Convert the message to HTTPSig as usual.
```

### to

```erlang
to(TABM, Req, FormatOpts, Opts) when is_map(TABM) ->
    % Ensure that the material for the message is loaded, if the request is
    % asking for a bundle.
```

### do_to

```erlang
do_to(Binary, _FormatOpts, _Opts) when is_binary(Binary) -> Binary;
```

### do_to

```erlang
do_to(TABM, FormatOpts, Opts) when is_map(TABM) ->
    InlineKey =
        case lists:keyfind(inline, 1, FormatOpts) of
            {inline, _InlineFieldHdrs, Key} -> Key;
            _ -> not_set
        end,
    % Calculate the initial encoding from the TABM
    Enc0 =
        maps:fold(
            fun(<<"body">>, Value, AccMap) ->
                    OldBody = maps:get(<<"body">>, AccMap, #{}),
                    AccMap#{ <<"body">> => OldBody#{ <<"body">> => Value } };
               (Key, Value, AccMap) when Key =:= InlineKey andalso InlineKey =/= not_set ->
                    OldBody = maps:get(<<"body">>, AccMap, #{}),
                    AccMap#{ <<"body">> => OldBody#{ InlineKey => Value } };
               (Key, Value, AccMap) ->
                    field_to_http(AccMap, {Key, Value}, #{})
            end,
            % Add any inline field denotations to the HTTP message
            case lists:keyfind(inline, 1, FormatOpts) of
                {inline, InlineFieldHdrs, _InlineKey} -> InlineFieldHdrs;
                _ -> #{}
            end,
            maps:without([<<"priv">>], TABM)
        ),
    ?event({prepared_body_map, {msg, Enc0}}),
    BodyMap = maps:get(<<"body">>, Enc0, #{}),
    GroupedBodyMap = group_maps(BodyMap, <<>>, #{}, Opts),
    Enc1 =
        case GroupedBodyMap of
            EmptyBody when map_size(EmptyBody) =:= 0 ->
                % If the body map is empty, then simply set the body to be a 
                % corresponding empty binary.
```

### group_ids

Group all elements with:

```erlang
group_ids(Map) ->
    % Find all keys that are IDs.
```

### ungroup_ids

Decode the `ao-ids` key into a map.

```erlang
ungroup_ids(Msg = #{ <<"ao-ids">> := IDBin }, Opts) ->
    % Extract the ID binary from the Map
    EncodedIDsMap = hb_structured_fields:parse_dictionary(IDBin),
    % Convert the value back into a raw binary
    IDsMap =
        lists:map(
            fun({K, {item, {string, Bin}, _}}) -> {K, Bin} end,
            EncodedIDsMap
        ),
    % Add the decoded IDs to the Map and remove the `ao-ids' key
    hb_maps:merge(hb_maps:without([<<"ao-ids">>], Msg, Opts), hb_maps:from_list(IDsMap), Opts);
```

### ungroup_ids

Decode the `ao-ids` key into a map.
Merge maps at the same level, if possible.

```erlang
ungroup_ids(Msg, _Opts) -> Msg.
```

### group_maps

Decode the `ao-ids` key into a map.
Merge maps at the same level, if possible.

```erlang
group_maps(Map) ->
    group_maps(Map, <<>>, #{}, #{}).
```

### group_maps

```erlang
group_maps(Map, Parent, Top, Opts) when is_map(Map) ->
    ?event({group_maps, {map, Map}, {parent, Parent}, {top, Top}}),
    {Flattened, NewTop} = hb_maps:fold(
        fun(Key, Value, {CurMap, CurTop}) ->
            ?event({group_maps, {key, Key}, {value, Value}}),
            NormKey = hb_ao:normalize_key(Key),
            FlatK =
                case Parent of
                    <<>> -> NormKey;
                    _ -> <<Parent/binary, "/", NormKey/binary>>
                end,
            case Value of
                _ when is_map(Value) orelse is_list(Value) ->
                    NormMsg =
                        if is_list(Value) ->
                            hb_message:convert(
                                Value,
                                tabm,
                                <<"structured@1.0">>,
                                Opts
                            );
                        true ->
                            Value
                        end,
                    case hb_maps:size(NormMsg, Opts) of
                        0 ->
                            {
                                CurMap,
                                hb_maps:put(
                                    FlatK,
                                    #{ <<"ao-types">> => <<"empty-message">> },
                                    CurTop,
                                    Opts
                                )
                            };
                        _ ->
                            NewTop = group_maps(NormMsg, FlatK, CurTop, Opts),
                            {CurMap, NewTop}
                    end;
                _ ->
                    ?event({group_maps, {norm_key, NormKey}, {value, Value}}),
                    case byte_size(Value) > ?MAX_HEADER_LENGTH of
                        % the value is too large to be encoded as a header
                        % within a part, so instead lift it to be a top level
                        % part
                        true ->
                            NewTop = hb_maps:put(FlatK, Value, CurTop, Opts),
                            {CurMap, NewTop};
                        % Encode the value in the current part
                        false ->
                            NewCurMap = hb_maps:put(NormKey, Value, CurMap, Opts),
                            {NewCurMap, CurTop}
                    end
            end
        end,
        {#{}, Top},
        Map,
        Opts
    ),
    case hb_maps:size(Flattened, Opts) of
        0 -> NewTop;
        _ -> case Parent of
            <<>> -> hb_maps:merge(NewTop, Flattened, Opts);
            _ ->
                Res = NewTop#{ Parent => Flattened },
                ?event({returning_res, {res, Res}}),
                Res
        end
    end.
```

### boundary_from_parts

Generate a unique, reproducible boundary for the

```erlang
boundary_from_parts(PartList) ->
    BodyBin =
        iolist_to_binary(
            lists:join(?CRLF,
                lists:map(
                    fun ({_PartName, PartBin}) -> PartBin end,
                    PartList
                )
            )
        ),
    RawBoundary = crypto:hash(sha256, BodyBin),
    hb_util:encode(RawBoundary).
```

### encode_body_part

Encode a multipart body part to a flat binary.

```erlang
encode_body_part(PartName, BodyPart, InlineKey, Opts) ->
    % We'll need to prepend a Content-Disposition header
    % to the part, using the field name as the form part
    % name.
```

### inline_key

given a message, returns a binary tuple:

```erlang
inline_key(Msg) ->
    inline_key(Msg, #{}).
```

### inline_key

```erlang
inline_key(Msg, Opts) ->
    % The message can name a key whose value will be placed in the body as the
    % inline part. Otherwise, the Msg <<"body">> is used. If not present, the
    % Msg <<"data">> is used.
    InlineBodyKey = hb_maps:get(<<"ao-body-key">>, Msg, false, Opts),
    ?event({inlined, InlineBodyKey}),
    case {
        InlineBodyKey,
        hb_maps:is_key(<<"body">>, Msg, Opts)
            andalso not ?IS_LINK(maps:get(<<"body">>, Msg, Opts)),
        hb_maps:is_key(<<"data">>, Msg, Opts)
            andalso not ?IS_LINK(maps:get(<<"data">>, Msg, Opts))
    } of
        % ao-body-key already exists, so no need to add one
        {Explicit, _, _} when Explicit =/= false -> {#{}, InlineBodyKey};
        % ao-body-key defaults to <<"body">> (see below)
        % So no need to add one
        {_, true, _} -> {#{}, <<"body">>};
        % We need to preserve the ao-body-key, as the <<"data">> field,
        % so that it is preserved during encoding and decoding
        {_, _, true} -> {#{<<"ao-body-key">> => <<"data">>}, <<"data">>};
        % default to body being the inlined part.
```

### encode_http_msg

Encode a HTTP message into a binary, converting it to `httpsig@1.0`

```erlang
encode_http_msg(Msg, Opts) ->
    % Convert the message to a HTTP-Sig encoded output.
```

### encode_http_flat_msg

Encode a HTTP message into a binary. The input *must* be a raw map of 

```erlang
encode_http_flat_msg(Httpsig, Opts) ->
    % Serialize the headers, to be included in the part of the multipart response
    HeaderList =
        lists:foldl(
            fun ({HeaderName, RawHeaderVal}, Acc) ->
                HVal = hb_cache:ensure_loaded(RawHeaderVal, Opts),
                ?event({encoding_http_header, {header, HeaderName}, {value, HVal}}),
                [<<HeaderName/binary, ": ", HVal/binary>> | Acc]
            end,
            [],
            hb_maps:to_list(hb_maps:without([<<"body">>, <<"priv">>], Httpsig, Opts), Opts)
        ),
    EncodedHeaders = iolist_to_binary(lists:join(?CRLF, lists:reverse(HeaderList))),
    case hb_maps:get(<<"body">>, Httpsig, <<>>, Opts) of
        <<>> -> EncodedHeaders;
        % Some-Headers: some-value
        % content-type: image/png
        % 
        % <body>
        SubBody -> <<EncodedHeaders/binary, ?DOUBLE_CRLF/binary, SubBody/binary>>
    end.
```

### field_to_http

All maps are encoded into the body of the HTTP message

```erlang
field_to_http(Httpsig, {Name, Value}, Opts) when is_map(Value) ->
    NormalizedName = hb_ao:normalize_key(Name),
    OldBody = hb_maps:get(<<"body">>, Httpsig, #{}, Opts),
    Httpsig#{ <<"body">> => OldBody#{ NormalizedName => Value } };
```

### field_to_http

All maps are encoded into the body of the HTTP message

```erlang
field_to_http(Httpsig, {Name, Value}, Opts) when is_binary(Value) ->
    NormalizedName = hb_ao:normalize_key(Name),
    % The default location where the value is encoded within the HTTP
    % message depends on its size.
```

### group_maps_test

```erlang
group_maps_test() ->
   Map = #{
        <<"a">> => <<"1">>,
        <<"b">> => #{
            <<"x">> => <<"10">>,
            <<"y">> => #{
                <<"z">> => <<"20">>
            },
            <<"foo">> => #{
                <<"bar">> => #{
                    <<"fizz">> => <<"buzz">>
                }
            } 
        },
        <<"c">> => #{
            <<"d">> => <<"30">>
        },
        <<"e">> => <<"2">>,
        <<"buf">> => <<"hello">>,
        <<"nested">> => #{
            <<"foo">> => <<"iiiiii">>,
            <<"here">> => #{
                <<"bar">> => <<"baz">>,
                <<"fizz">> => <<"buzz">>,
                <<"pop">> => #{
                    <<"very-fizzy">> => <<"very-buzzy">>
                }
            }
        }
    },
    Lifted = group_maps(Map),
    ?assertEqual(
        Lifted,
        #{
            <<"a">> => <<"1">>,
            <<"b">> => #{<<"x">> => <<"10">>},
            <<"b/foo/bar">> => #{<<"fizz">> => <<"buzz">>},
            <<"b/y">> => #{<<"z">> => <<"20">>},
            <<"buf">> => <<"hello">>,
            <<"c">> => #{<<"d">> => <<"30">>},
            <<"e">> => <<"2">>,
            <<"nested">> => #{<<"foo">> => <<"iiiiii">>},
            <<"nested/here">> => #{<<"bar">> => <<"baz">>, <<"fizz">> => <<"buzz">>},
            <<"nested/here/pop">> => #{<<"very-fizzy">> => <<"very-buzzy">>}
        }
    ),
    ok.
```

### group_maps_flat_compatible_test

The grouped maps encoding is a subset of the flat encoding,

```erlang
group_maps_flat_compatible_test() ->
    Map = #{
        <<"a">> => <<"1">>,
        <<"b">> => #{
            <<"x">> => <<"10">>,
            <<"y">> => #{
                <<"z">> => <<"20">>
            },
            <<"foo">> => #{
                <<"bar">> => #{
                    <<"fizz">> => <<"buzz">>
                }
            } 
        },
        <<"c">> => #{
            <<"d">> => <<"30">>
        },
        <<"e">> => <<"2">>,
        <<"buf">> => <<"hello">>,
        <<"nested">> => #{
            <<"foo">> => <<"iiiiii">>,
            <<"here">> => #{
                <<"bar">> => <<"baz">>,
                <<"fizz">> => <<"buzz">>
            }
        }
    },
    Lifted = group_maps(Map),
    ?assertEqual(dev_codec_flat:from(Lifted, #{}, #{}), {ok, Map}),
    ok.
```

### encode_message_with_links_test

```erlang
encode_message_with_links_test() ->
    Msg = #{
        <<"immediate-key">> => <<"immediate-value">>,
        <<"typed-key">> => 4
    },
    {ok, Path} = hb_cache:write(Msg, #{}),
    {ok, Read} = hb_cache:read(Path, #{}),
    % Ensure that the message now has a lazy link
    ?assertMatch({link, _, _}, maps:get(<<"typed-key">>, Read, #{})),
    % Encode and decode the message as `httpsig@1.0`
    Enc = hb_message:convert(Msg, <<"httpsig@1.0">>, #{}),
    ?event({encoded, Enc}),
    Dec = hb_message:convert(Enc, <<"structured@1.0">>, <<"httpsig@1.0">>, #{}),
    % Ensure that the result is the same as the original message
    ?event({decoded, Dec}),
```

---

*Generated from [dev_codec_httpsig_conv.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_codec_httpsig_conv.erl)*
