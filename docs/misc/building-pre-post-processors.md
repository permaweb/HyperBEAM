# Building Pre/Post-Processors in AO

Pre/post-processors in AO allow you to intercept and potentially modify incoming requests before they are executed by the target device or process. This guide explains how to build a preprocessor, focusing on a common pattern: exempting certain request paths from being relayed or modified.

## Core Concepts

1.  **Exempt Routes (`exempt-routes`):** A list of route templates defined in the node's configuration (`Msg1`). If an incoming request (`Msg2`) matches any of these templates, it should bypass the main preprocessor logic (e.g., relaying).
2.  **Exemption Check (`is_exempt/3`):** A function that determines if a request should be exempt. It checks two things:
    *   An optional `is-exempt` message defined in the node's configuration (`Msg1`). If present, this message is resolved, and its result determines exemption.
    *   If `is-exempt` is not found, it matches the incoming request's path against the `exempt-routes` list using `dev_router:match/3`.
3.  **Preprocessing Logic (`preprocess/3`):** The main function that receives the node configuration (`Msg1`), the incoming request (`Msg2`), and options (`Opts`). Based on the result of `is_exempt/3`, it either:
    *   **If Exempt:** Returns the original, parsed list of messages to be executed. This list is found in the `body` key of `Msg2`.
    *   **If Not Exempt:** Performs the main preprocessing action, such as rewriting the request to be relayed to another node. This often involves using the raw, unparsed request singleton found in the `request` key of `Msg2`.

## Implementation Example (Pseudo-code)

This pseudo-code illustrates the flow:

```erlang
-define(DEFAULT_EXEMPT_ROUTES, [
  % Default routes that should bypass preprocessing
  #{ <<"template">> => <<"/~meta@1.0/.*">> },
  #{ <<"template">> => <<"/~greenzone@1.0/.*">> }
  % ... other default exempt routes
]).

%% @doc Check if a request is exempt from preprocessing.
is_exempt(Msg1, Msg2, Opts) ->
    case ao.get(<<"is-exempt">>, Msg1, Opts) of
        not_found ->
            % No explicit is-exempt message, check against exempt-routes
            ExemptRoutes =
                hb_opts:get(
                    exempt_routes,
                    ?DEFAULT_EXEMPT_ROUTES,
                    Msg1
                ),
            Req = hb_ao:get(<<"request">>, Msg2, Opts),
            {_, Matches} =
                dev_router:match(
                    #{ <<"routes">> => ExemptRoutes },
                    Req,
                    Msg1 % Use NodeMsg (Msg1) for Opts context if needed
                ),
            case Matches of
                 no_matching_route -> {ok, false}; % Not exempt
                 _ -> {ok, true}                 % Exempt
             end;
        IsExemptMsg ->
            % Resolve the custom is-exempt message
            ao.resolve(IsExemptMsg, Msg2, Opts)
    end.

%% @doc Preprocess an incoming request.
preprocess(Msg1, Msg2, Opts) ->
    case is_exempt(Msg1, Msg2, Opts) of
        {ok, true} ->
             % Request is exempt. Return the original parsed message list.
             % IMPORTANT: Use the 'body' key from Msg2.
             {ok, hb_ao:get(<<"body">>, Msg2, Opts)};
        {ok, false} ->
             % Request is not exempt. Perform preprocessing (e.g., relay).
             % IMPORTANT: Use the 'request' key from Msg2 for the raw singleton.
            {ok,
                [
                    #{ <<"device">> => <<"relay@1.0">> }, % Example: Relay device
                    #{
                        <<"path">> => <<"call">>,
                        <<"target">> => <<"body">>, % Target the 'body' of the relay message
                        <<"body">> =>
                            % Get the raw request singleton
                            hb_ao:get(<<"request">>, Msg2, Opts#{ hashpath => ignore })
                    }
                ]
            };
        {error, Reason} ->
            % Handle errors from is_exempt resolution
            {error, Reason}
    end.
```

## Key Considerations

*   **`body` vs. `request`:** The preprocessor receives the incoming request in two forms within `Msg2`:
    *   `body`: A **parsed list** of AO messages that represent the steps to be executed. Use this when you want to return the original execution plan (i.e., when exempting).
    *   `request`: The **raw, unparsed TABM singleton** message sent by the user. Use this when you need the original message structure, for example, to forward it unmodified in a relay request.
*   **`dev_router:match/3`:** This function is used to match a request (`Req`) against a list of route templates (`Routes`). It's borrowed from the routing logic but is useful here for checking path-based exemptions.
*   **Configuration:** The `exempt-routes` and the optional `is-exempt` message should be configured in the node's options (accessible via `Msg1` or `Opts`).
*   **Error Handling:** Ensure proper error handling, especially when resolving the `is-exempt` message.

By following this pattern, you can create flexible preprocessors that selectively apply logic based on configurable rules and request paths. 