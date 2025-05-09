# Extending HyperBEAM

HyperBEAM's modular design, built on AO-Core principles and Erlang/OTP, makes it highly extensible. You can add new functionalities or modify existing behaviors primarily by creating new **Devices** or implementing **Pre/Post-Processors**.

!!! warning "Advanced Topic"
    Extending HyperBEAM requires a good understanding of Erlang/OTP, the AO-Core protocol, and HyperBEAM's internal architecture. This guide provides a high-level overview; detailed implementation requires deeper exploration of the source code.

## Approach 1: Creating New Devices

This is the most common way to add significant new capabilities.
A Device is essentially an Erlang module (typically named `dev_*.erl`) that processes AO-Core messages.

**Steps:**

1.  **Define Purpose:** Clearly define what your device will do. What kind of messages will it process? What state will it manage (if any)? What functions (keys) will it expose?
2.  **Create Module:** Create a new Erlang module (e.g., `src/dev_my_new_device.erl`).
3.  **Implement `info/0..2` (Optional but Recommended):** Define an `info` function to signal capabilities and requirements to HyperBEAM (e.g., exported keys, variant/version ID).
    ```erlang
    info() ->
        #{
          variant => <<"MyNewDevice/1.0">>,
          exports => [<<"do_something">>, <<"get_status">>]
        }.
    ```
4.  **Implement Key Functions:** Create Erlang functions corresponding to the keys your device exposes. These functions typically take `StateMessage`, `InputMessage`, and `Environment` as arguments and return `{ok, NewMessage}` or `{error, Reason}`.
    ```erlang
    do_something(StateMsg, InputMsg, Env) ->
        % ... perform action based on InputMsg ...
        NewState = ..., % Calculate new state
        {ok, NewState}.

    get_status(StateMsg, _InputMsg, _Env) ->
        % ... read status from StateMsg ...
        StatusData = ..., 
        {ok, StatusData}.
    ```
5.  **Handle State (If Applicable):** Devices can be stateless or stateful. Stateful devices manage their state within the `StateMessage` passed between function calls.
6.  **Register Device:** Ensure HyperBEAM knows about your device. This might involve adding it to build configurations or potentially a dynamic registration mechanism if available.
7.  **Testing:** Write EUnit tests for your device's functions.

**Example Idea:** A device that bridges to another blockchain network, allowing AO processes to read data or trigger transactions on that chain.

## Approach 2: Building Pre/Post-Processors

Pre/post-processors allow you to intercept incoming requests *before* they reach the target device/process (`preprocess`) or modify the response *after* execution (`postprocess`). These are often implemented using the `dev_stack` device or specific hooks within the request handling pipeline.

**Use Cases:**

*   **Authentication/Authorization:** Checking signatures or permissions before allowing execution.
*   **Request Modification:** Rewriting requests, adding metadata, or routing based on specific criteria.
*   **Response Formatting:** Changing the structure or content type of the response.
*   **Metering/Logging:** Recording request details or charging for usage before or after execution.

**Implementation:**

Processors often involve checking specific conditions (like request path or headers) and then either:

a.  Passing the request through unchanged.
b.  Modifying the request/response message structure.
c.  Returning an error or redirect.
<!-- d. The guide on [Building Pre/Post-Processors](TODO:link-to-pre-post-processor-guide-once-available) provides a detailed example pattern, particularly focusing on exempting certain routes. -->

**Example Idea:** A preprocessor that automatically adds a timestamp tag to all incoming messages for a specific process.
<!-- 
## Approach 3: Modifying Existing Devices (Use with Caution)

You could directly modify the source code of existing `dev_*.erl` modules. However, this is generally discouraged as it makes future updates harder and can break compatibility. -->

## Approach 3: Custom Routing Strategies

While `dev_router` provides basic strategies (round-robin, etc.), you could potentially implement a custom load balancing or routing strategy module that `dev_router` could be configured to use. This would involve understanding the interfaces expected by `dev_router`.

**Example Idea:** A routing strategy that queries worker nodes for their specific capabilities before forwarding a request.

## Getting Started

1.  **Familiarize Yourself:** Deeply understand Erlang/OTP and the HyperBEAM codebase (`src/` directory), especially [`hb_ao.erl`](../resources/source-code/hb_ao.md), [`hb_message.erl`](../resources/source-code/hb_message.md), and existing `dev_*.erl` modules relevant to your idea.
2.  **Study Examples:** Look at simple devices like `dev_patch.erl` or more complex ones like `dev_process.erl` to understand patterns.
3.  **Start Small:** Implement a minimal version of your idea first.
4.  **Test Rigorously:** Use `rebar3 eunit` extensively.
5.  **Engage Community:** Ask questions in developer channels if you get stuck.

Extending HyperBEAM allows you to tailor the AO network's capabilities to specific needs, contributing to its rich and evolving ecosystem.
