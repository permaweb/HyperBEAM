# dev_scheduler_formats

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_scheduler_formats.erl)

This module is used by dev_scheduler in order to produce outputs that
are compatible with various forms of AO clients. It features two main formats:
- `application/json`
- `application/http`
The `application/json` format is a legacy format that is not recommended for
new integrations of the AO protocol.

---

## Exported Functions

- `aos2_normalize_types/1`
- `aos2_to_assignment/2`
- `aos2_to_assignments/3`
- `assignments_to_aos2/4`
- `assignments_to_bundle/4`

---

### assignments_to_bundle

This module is used by dev_scheduler in order to produce outputs that
Generate a `GET /schedule` response for a process as HTTP-sig bundles.

```erlang
assignments_to_bundle(ProcID, Assignments, More, Opts) ->
    TimeInfo = ar_timestamp:get(),
    assignments_to_bundle(ProcID, Assignments, More, TimeInfo, Opts).
```

### assignments_to_bundle

```erlang
assignments_to_bundle(ProcID, Assignments, More, TimeInfo, RawOpts) ->
    Opts = format_opts(RawOpts),
    {Timestamp, Height, Hash} = TimeInfo,
    {ok, #{
        <<"type">> => <<"schedule">>,
        <<"process">> => hb_util:human_id(ProcID),
        <<"continues">> => hb_util:atom(More),
        <<"timestamp">> => hb_util:int(Timestamp),
        <<"block-height">> => hb_util:int(Height),
        <<"block-hash">> => hb_util:human_id(Hash),
        <<"assignments">> =>
            hb_maps:from_list(
                lists:map(
                    fun(Assignment) ->
                        {
                            hb_ao:get(
                                <<"slot">>,
                                Assignment,
                                Opts#{ hashpath => ignore }
                            ),
                            Assignment
                        }
                    end,
                    Assignments
                )
            )
    }}.
```

### assignments_to_aos2

```erlang
assignments_to_aos2(ProcID, Assignments, More, RawOpts) when is_map(Assignments) ->
    assignments_to_aos2(
        ProcID,
        hb_util:message_to_ordered_list(Assignments),
        More,
        format_opts(RawOpts)
    );
```

### assignments_to_aos2

```erlang
assignments_to_aos2(ProcID, Assignments, More, RawOpts) ->
    Opts = format_opts(RawOpts),
    {Timestamp, Height, Hash} = ar_timestamp:get(),
    BodyStruct = 
        #{
            <<"page_info">> =>
                #{
                    <<"process">> => hb_util:human_id(ProcID),
                    <<"has_next_page">> => More,
                    <<"timestamp">> => list_to_binary(integer_to_list(Timestamp)),
                    <<"block-height">> => list_to_binary(integer_to_list(Height)),
                    <<"block-hash">> => hb_util:human_id(Hash)
                },
            <<"edges">> =>
                lists:map(
                    fun(Assignment) ->
                        #{
                            <<"cursor">> => cursor(Assignment, Opts),
                            <<"node">> => assignment_to_aos2(Assignment, Opts)
                        }
                    end,
                    Assignments
                )
        },
    Encoded = hb_json:encode(BodyStruct),
    ?event({body_struct, BodyStruct}),
    ?event({encoded, {explicit, Encoded}}),
    {ok, 
        #{
            <<"content-type">> => <<"application/json">>,
            <<"body">> => Encoded
        }
    }.
```

### cursor

Generate a cursor for an assignment. This should be the slot number, at
Convert an assignment to an AOS2-compatible JSON structure.

```erlang
cursor(Assignment, RawOpts) ->
    Opts = format_opts(RawOpts),
    hb_ao:get(<<"slot">>, Assignment, Opts).
```

### assignment_to_aos2

Generate a cursor for an assignment. This should be the slot number, at
Convert an assignment to an AOS2-compatible JSON structure.

```erlang
assignment_to_aos2(Assignment, RawOpts) ->
    Opts = format_opts(RawOpts),
    Message = hb_ao:get(<<"body">>, Assignment, Opts),
    AssignmentWithoutBody = hb_maps:without([<<"body">>], Assignment, Opts),
    #{
        <<"message">> =>
            dev_json_iface:message_to_json_struct(Message, Opts),
        <<"assignment">> =>
            dev_json_iface:message_to_json_struct(AssignmentWithoutBody, Opts)
    }.
```

### aos2_to_assignments

Convert an AOS2-style JSON structure to a normalized HyperBEAM

```erlang
aos2_to_assignments(ProcID, Body, RawOpts) ->
    Opts = format_opts(RawOpts),
    Assignments = hb_maps:get(<<"edges">>, Body, Opts, Opts),
    ?event({raw_assignments, Assignments}),
    ParsedAssignments =
        lists:map(
            fun(A) -> aos2_to_assignment(A, Opts) end,
            Assignments
        ),
    ?event({parsed_assignments, ParsedAssignments}),
    TimeInfo =
        case ParsedAssignments of
            [] -> {0, 0, hb_util:encode(<<0:256>>)};
            _ ->
                Last = lists:last(ParsedAssignments),
                {
                    hb_ao:get(<<"timestamp">>, Last, Opts),
                    hb_ao:get(<<"block-height">>, Last, Opts),
                    hb_ao:get(<<"block-hash">>, Last, Opts)
                }
        end,
    assignments_to_bundle(ProcID, ParsedAssignments, false, TimeInfo, Opts).
```

### aos2_to_assignment

Create and normalize an assignment from an AOS2-style JSON structure.
The `hb_gateway_client` module expects all JSON structures to at least

```erlang
aos2_to_assignment(A, RawOpts) ->
    Opts = format_opts(RawOpts),
    % Unwrap the node if it is provided
    Node = hb_maps:get(<<"node">>, A, A, Opts),
    ?event({node, Node}),
    {ok, Assignment} =
        hb_gateway_client:result_to_message(
            aos2_normalize_data(hb_maps:get(<<"assignment">>, Node, undefined, Opts)),
            Opts
        ),
    NormalizedAssignment = aos2_normalize_types(Assignment),
    {ok, Message} =
        case hb_maps:get(<<"message">>, Node, undefined, Opts) of
            null ->
                MessageID = hb_maps:get(<<"message">>, Assignment, undefined, Opts),
                ?event(error, {scheduler_did_not_provide_message, MessageID}),
                case hb_cache:read(MessageID, Opts) of
                    {ok, Msg} -> {ok, Msg};
                    {error, _} ->
                        throw({error,
                            {message_not_given_by_scheduler_or_cache,
                                MessageID}
                            }
                        )
                end;
            Body ->
                hb_gateway_client:result_to_message(
                    aos2_normalize_data(Body),
                    Opts
                )
        end,
    NormalizedMessage = aos2_normalize_types(Message),
    ?event({message, Message}),
    NormalizedAssignment#{ <<"body">> => NormalizedMessage }.
```

### aos2_normalize_data

Create and normalize an assignment from an AOS2-style JSON structure.
The `hb_gateway_client` module expects all JSON structures to at least

```erlang
aos2_normalize_data(JSONStruct) ->
    case JSONStruct of
        #{<<"data">> := _} -> JSONStruct;
        _ -> JSONStruct#{ <<"data">> => <<>> }
    end.
```

### aos2_normalize_types

Normalize an AOS2 formatted message to ensure that all field NAMES and

```erlang
aos2_normalize_types(Msg = #{ <<"timestamp">> := TS }) when is_binary(TS) ->
    aos2_normalize_types(Msg#{ <<"timestamp">> => hb_util:int(TS) });
```

### aos2_normalize_types

Normalize an AOS2 formatted message to ensure that all field NAMES and

```erlang
aos2_normalize_types(Msg = #{ <<"nonce">> := Nonce })
        when is_binary(Nonce) and not is_map_key(<<"slot">>, Msg) ->
    aos2_normalize_types(
        Msg#{ <<"slot">> => hb_util:int(Nonce) }
    );
```

### aos2_normalize_types

Normalize an AOS2 formatted message to ensure that all field NAMES and

```erlang
aos2_normalize_types(Msg = #{ <<"epoch">> := DS }) when is_binary(DS) ->
    aos2_normalize_types(Msg#{ <<"epoch">> => hb_util:int(DS) });
```

### aos2_normalize_types

Normalize an AOS2 formatted message to ensure that all field NAMES and

```erlang
aos2_normalize_types(Msg = #{ <<"slot">> := Slot }) when is_binary(Slot) ->
    aos2_normalize_types(Msg#{ <<"slot">> => hb_util:int(Slot) });
```

### aos2_normalize_types

Normalize an AOS2 formatted message to ensure that all field NAMES and

```erlang
aos2_normalize_types(Msg) when not is_map_key(<<"block-hash">>, Msg) ->
    ?event({missing_block_hash, Msg}),
    aos2_normalize_types(Msg#{ <<"block-hash">> => hb_util:encode(<<0:256>>) });
```

### aos2_normalize_types

Normalize an AOS2 formatted message to ensure that all field NAMES and

```erlang
aos2_normalize_types(Msg) ->
    ?event(
        {
            aos2_normalized_types,
            {msg, Msg},
            {anchor, hb_ao:get(<<"anchor">>, Msg, <<>>, #{})}
        }
    ),
    Msg.
```

### format_opts

For all scheduler format operations, we do not calculate hashpaths,

```erlang
format_opts(Opts) ->
    Opts#{
        hashpath => ignore,
        cache_control => [<<"no-cache">>, <<"no-store">>],
        await_inprogress => false
```

---

*Generated from [dev_scheduler_formats.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_scheduler_formats.erl)*
