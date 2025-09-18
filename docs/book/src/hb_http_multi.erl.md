# hb_http_multi

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_http_multi.erl)

An interface for resolving requests across multiple HTTP servers, either
concurrently or sequentially, and processing the results in a configurable
manner.
The `Config` message for a call to `request/5` may contain the following
fields:
- `multirequest-nodes`: A list of nodes to request from.
- `multirequest-responses`: The number of responses to gather.
- `multirequest-stop-after`: Whether to stop after the required number of
  responses.
- `multirequest-parallel`: Whether to run the requests in parallel.
- `multirequest-admissible`: A message to resolve against the response.
- `multirequest-admissible-status`: The statuses that are admissible.
The `admissible` message is executed as a `base` message, with its `path`
field moved to the request (or set to `is-admissible` if not present):
```
    resolve(Base, Response#{ <<"path">> => Base/path OR /is-admissible }, Opts)
'''

---

## Exported Functions

- `request/5`

---

### request

An interface for resolving requests across multiple HTTP servers, either
Dispatch the same HTTP request to many nodes. Can be configured to

```erlang
request(Config, Method, Path, Message, Opts) ->
    #{
        nodes := Nodes,
        responses := Responses,
        stop_after := StopAfter,
        admissible := Admissible,
        admissible_status := Statuses,
        parallel := Parallel
    } = multirequest_opts(Config, Message, Opts),
    MultirequestMsg =
        hb_message:without_unless_signed(
            lists:filter(
                fun(<<"multirequest-", _/binary>>) -> true; (_) -> false end,
                hb_maps:keys(Message)
            ),
            Message,
            Opts
        ),
    ?event(debug_multi,
        {multirequest_opts_parsed,
            {config, Config},
            {method, Method},
            {path, Path},
            {raw_message, Message},
            {message_to_send, MultirequestMsg}
        }),
    AllResults =
        if Parallel ->
            parallel_multirequest(
                Nodes,
                Responses,
                StopAfter,
                Method,
                Path,
                MultirequestMsg,
                Admissible,
                Statuses,
                Opts
            );
        true ->
            serial_multirequest(
                Nodes,
                Responses,
                Method,
                Path,
                MultirequestMsg,
                Admissible,
                Statuses,
                Opts
            )
        end,
    ?event(http, {multirequest_results, {results, AllResults}}),
    case AllResults of
        [] -> {error, no_viable_responses};
        Results -> if Responses == 1 -> hd(Results); true -> Results end
    end.
```

### multirequest_opts

Get the multirequest options from the config or message. The options in 

```erlang
multirequest_opts(Config, Message, Opts) ->
    Opts#{
        nodes =>
            multirequest_opt(<<"nodes">>, Config, Message, #{}, Opts),
        responses =>
            multirequest_opt(<<"responses">>, Config, Message, 1, Opts),
        stop_after =>
            multirequest_opt(<<"stop-after">>, Config, Message, true, Opts),
        admissible =>
            multirequest_opt(<<"admissible">>, Config, Message, undefined, Opts),
        admissible_status =>
            multirequest_opt(<<"admissible-status">>, Config, Message, <<"All">>, Opts),
        parallel =>
            multirequest_opt(<<"parallel">>, Config, Message, false, Opts)
    }.
```

### multirequest_opt

Get a value for a multirequest option from the config or message.

```erlang
multirequest_opt(Key, Config, Message, Default, Opts) ->
    hb_ao:get_first(
        [
            {Message, <<"multirequest-", Key/binary>>},
            {Config, Key}
        ],
        Default,
        Opts#{ hashpath => ignore }
    ).
```

### is_admissible

Check if a response is admissible, according to the configuration. First,

```erlang
is_admissible(ok, Res, Admissible, Statuses, Opts) ->
    ?event(debug_multi,
        {is_admissible,
            {response, Res},
            {admissible, Admissible},
            {statuses, Statuses}
        }
    ),
    AdmissibleStatus = admissible_status(Res, Statuses),
    ?event(debug_multi, {admissible_status, {result, AdmissibleStatus}}),
    AdmissibleResponse = admissible_response(Res, Admissible, Opts),
    ?event(debug_multi, {admissible_response, {result, AdmissibleResponse}}),
    AdmissibleStatus andalso AdmissibleResponse;
```

### is_admissible

Check if a response is admissible, according to the configuration. First,
Serially request a message, collecting responses until the required

```erlang
is_admissible(_, _, _, _, _) -> false.
```

### serial_multirequest

Check if a response is admissible, according to the configuration. First,
Serially request a message, collecting responses until the required

```erlang
serial_multirequest(_Nodes, 0, _Method, _Path, _Message, _Admissible, _Statuses, _Opts) -> [];
```

### serial_multirequest

Check if a response is admissible, according to the configuration. First,
Serially request a message, collecting responses until the required

```erlang
serial_multirequest([], _, _Method, _Path, _Message, _Admissible, _Statuses, _Opts) -> [];
```

### serial_multirequest

Check if a response is admissible, according to the configuration. First,
Serially request a message, collecting responses until the required

```erlang
serial_multirequest([Node|Nodes], Remaining, Method, Path, Message, Admissible, Statuses, Opts) ->
    {ErlStatus, Res} = hb_http:request(Method, Node, Path, Message, Opts),
    case is_admissible(ErlStatus, Res, Admissible, Statuses, Opts) of
        true ->
            ?event(http, {admissible_status, {response, Res}}),
            [
                {ErlStatus, Res}
            |
                serial_multirequest(
                    Nodes,
                    Remaining - 1,
                    Method,
                    Path,
                    Message,
                    Admissible,
                    Statuses,
                    Opts
                )
            ];
        false ->
            ?event(http, {inadmissible_status, {response, Res}}),
            serial_multirequest(
                Nodes,
                Remaining,
                Method,
                Path,
                Message,
                Admissible,
                Statuses,
                Opts
            )
    end.
```

### parallel_multirequest

Dispatch the same HTTP request to many nodes in parallel.

```erlang
parallel_multirequest(Nodes, Responses, StopAfter, Method, Path, Message, Admissible, Statuses, Opts) ->
    Ref = make_ref(),
    Parent = self(),
    Procs =
        lists:map(
            fun(Node) ->
                spawn(
                    fun() ->
                        Res = hb_http:request(Method, Node, Path, Message, Opts),
                        receive no_reply -> stopping
                        after 0 -> Parent ! {Ref, self(), Res}
                        end
                    end
                )
            end,
            Nodes
        ),
    parallel_responses([], Procs, Ref, Responses, StopAfter, Admissible, Statuses, Opts).
```

### admissible_status

Check if a status is allowed, according to the configuration. Statuses

```erlang
admissible_status(_, <<"All">>) -> true;
```

### admissible_status

Check if a status is allowed, according to the configuration. Statuses

```erlang
admissible_status(_ResponseMsg = #{ <<"status">> := Status }, Statuses) ->
    admissible_status(Status, Statuses);
```

### admissible_status

Check if a status is allowed, according to the configuration. Statuses

```erlang
admissible_status(Status, Statuses) when is_integer(Statuses) ->
    admissible_status(Status, [Statuses]);
```

### admissible_status

Check if a status is allowed, according to the configuration. Statuses

```erlang
admissible_status(Status, Statuses) when is_binary(Status) ->
    admissible_status(binary_to_integer(Status), Statuses);
```

### admissible_status

Check if a status is allowed, according to the configuration. Statuses

```erlang
admissible_status(Status, Statuses) when is_binary(Statuses) ->
    % Convert the statuses to a list of integers.
```

### admissible_status

```erlang
admissible_status(Status, Statuses) when is_list(Statuses) ->
    lists:member(Status, Statuses).
```

### admissible_response

If an `admissable` message is set for the request, check if the response

```erlang
admissible_response(_Response, undefined, _Opts) -> true;
```

### admissible_response

If an `admissable` message is set for the request, check if the response

```erlang
admissible_response(Response, Msg, Opts) ->
    Path = hb_maps:get(<<"path">>, Msg, <<"is-admissible">>, Opts),
    Req = Response#{ <<"path">> => Path },
    Base = hb_message:without_unless_signed([<<"path">>], Msg, Opts),
    ?event(debug_multi,
        {executing_admissible_message, {message, Base}, {req, Req}}
    ),
    case hb_ao:resolve(Base, Req, Opts) of
        {ok, Res} when is_atom(Res) or is_binary(Res) ->
            ?event(debug_multi, {admissible_result, {result, Res}}),
            hb_util:atom(Res) == true;
        {error, Reason} ->
            ?event(debug_multi, {admissible_error, {reason, Reason}}),
            false
    end.
```

### parallel_responses

Collect the necessary number of responses, and stop workers if

```erlang
parallel_responses(Res, Procs, Ref, 0, false, _Admissible, _Statuses, _Opts) ->
    lists:foreach(fun(P) -> P ! no_reply end, Procs),
    empty_inbox(Ref),
    {ok, Res};
```

### parallel_responses

Collect the necessary number of responses, and stop workers if

```erlang
parallel_responses(Res, Procs, Ref, 0, true, _Admissible, _Statuses, _Opts) ->
    lists:foreach(fun(P) -> exit(P, kill) end, Procs),
    empty_inbox(Ref),
    Res;
```

### parallel_responses

Collect the necessary number of responses, and stop workers if

```erlang
parallel_responses(Res, Procs, Ref, Awaiting, StopAfter, Admissible, Statuses, Opts) ->
    receive
        {Ref, Pid, {Status, NewRes}} ->
            case is_admissible(Status, NewRes, Admissible, Statuses, Opts) of
                true ->
                    parallel_responses(
                        [NewRes | Res],
                        lists:delete(Pid, Procs),
                        Ref,
                        Awaiting - 1,
                        StopAfter,
                        Admissible,
                        Statuses,
                        Opts
                );
            false ->
                parallel_responses(
                    Res,
                    lists:delete(Pid, Procs),
                    Ref,
                    Awaiting,
                    StopAfter,
                    Admissible,
                    Statuses,
                    Opts
                )
        end
end.
```

### empty_inbox

Empty the inbox of the current process for all messages with the given

```erlang
empty_inbox(Ref) ->
```

---

*Generated from [hb_http_multi.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_http_multi.erl)*
