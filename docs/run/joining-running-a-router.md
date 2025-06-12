# Joining or Running a Router Node

Router nodes play a crucial role in the HyperBEAM network by directing incoming HTTP requests to appropriate worker nodes capable of handling the requested computation or data retrieval. They act as intelligent load balancers and entry points into the AO ecosystem.

!!! info "Advanced Topic"
    Configuring and running a production-grade router involves considerations beyond the scope of this introductory guide, including network topology, security, high availability, and performance tuning.

## What is a Router?

In HyperBEAM, the `dev_router` module (and associated logic) implements routing functionality. A node configured as a router typically:

1.  Receives external HTTP requests.
2.  Parses the request path to determine the target process, device, and desired operation.
3.  Consults its routing table or logic to select an appropriate downstream worker node (which might be itself or another node).
4.  Forwards the request to the selected worker.
5.  Receives the response from the worker.
6.  Returns the response to the original client.

Routers often maintain information about the capabilities and load of worker nodes they know about.

!!! note "Using Routers as a Client"
    To use a router as a client, simply make HTTP requests to the router's URL: `https://dev-router.forward.computer/<process_id>~<device>/<key>...`. The router will automatically route your request to an appropriate worker node.

## Node Registration Process
*Coming soon...*
<!-- HyperBEAM nodes can register themselves with router networks to offer computational services. The registration process involves configuring route parameters and submitting signed registration requests to router nodes.

### Registration Configuration

Before registering with a router, your node needs to be configured with the following parameters:

```erlang
% Core registration parameters
{router_node, "https://dev-router.forward.computer"},  % Target router endpoint
{router_prefix, "/my-process-id~"},                 % Route prefix to register
{router_price, 100},                                % Price for computation (units)
{router_template, "/my-process-id~process@1.0/.*"}, % Route template pattern

% Optional parameters
{as, "SIGNING_PROCESS_ID"},                         % Process ID to sign as (optional)
{trusted_nodes, #{                                  % Trusted node identifiers
    "NODE_ID_1" => true,
    "NODE_ID_2" => true
}}
```

### Registration Methods

#### 1. Automatic Registration via AO Message

The primary registration method uses AO messages sent to the router process:

```lua
-- Send registration message to router
ao.send({
    Target = "ROUTER_PROCESS_ID",
    Action = "register",
    Data = json.encode({
        subject = "self",
        action = "register",
        route = {
            prefix = "/my-process-id~",
            template = "/my-process-id~process@1.0/.*",
            price = 100
        }
    })
})
```

#### 2. Direct HTTP Registration

Nodes can also register directly via HTTP POST to the router's schedule endpoint:

```bash
curl -X POST https://dev-router.forward.computer/router~node-process@1.0/schedule \
  -H "Content-Type: application/json" \
  -d '{
    "subject": "self",
    "action": "register",
    "route": {
      "prefix": "/my-process-id~",
      "template": "/my-process-id~process@1.0/.*",
      "price": 100
    }
  }'
```

#### 3. Programmatic Registration

Using the HyperBEAM device system:

```erlang
% Call the router device's register function
hb_ao:call(RouterProcessID, <<"register">>, #{
    <<"router_node">> => <<"https://dev-router.forward.computer">>,
    <<"router_prefix">> => <<"/my-process-id~">>,
    <<"router_template">> => <<"/my-process-id~process@1.0/.*">>,
    <<"router_price">> => 100
}, Opts).
```

### Trust System and Registration Validation

The registration process includes a two-tier validation system:

#### Trusted Node Registration
Nodes that are pre-configured as trusted can register immediately without additional validation:

```erlang
% Configure trusted nodes in router
{trusted_nodes, #{
    "TRUSTED_NODE_ID_1" => true,
    "TRUSTED_NODE_ID_2" => true
}}
```

Trusted nodes bypass the admissibility check and are immediately added to the routing table.

#### Standard Node Registration
Non-trusted nodes go through an admissibility validation process:

1. **Signature Verification**: The router verifies the registration message is properly signed
2. **Admissibility Check**: The router runs custom logic to determine if the node should be accepted
3. **Route Addition**: If approved, the node is added to the dynamic routing table
4. **Recalculation**: The router recalculates optimal routes including the new node

### Dynamic Router Configuration

The dynamic router system (implemented in `scripts/dynamic-router.lua`) handles registrations with the following logic:

```lua
-- Example router state configuration
local router_state = {
    ["trusted"] = "TRUSTED_SIGNER_ID",  -- Optional trusted signer
    ["is-admissible"] = {               -- Admissibility validation process
        path = "is-admissible",
        -- Custom validation logic
    },
    ["routes"] = {},                    -- Dynamic routing table
    ["performance"] = {}                -- Performance metrics
}
```

## Configuring Routing Behavior

Routing logic is primarily configured through node options, often managed via `hb_opts` or environment variables when starting the node. Key aspects include:

*   **Route Definitions:** Defining patterns (templates) and corresponding downstream targets (worker node URLs or internal handlers). Routes are typically ordered by precedence.
*   **Load Balancing Strategy:** How the router chooses among multiple potential workers for a given route (e.g., round-robin, least connections, latency-based).
*   **Worker Discovery/Management:** How the router learns about available worker nodes and their status.

**Example Configuration Snippet (Conceptual - from `hb_opts` or config file):**

```erlang
{
  routes,
  [
    #{ template => "/~meta@1.0/.*", target => self }, % Handle meta locally
    #{ template => "/PROCESS_ID1~process@1.0/.*", target => "http://worker1.example.com" },
    #{ template => "/PROCESS_ID2~process@1.0/.*", target => "http://worker2.example.com" },
    #{ template => "/.*~wasm64@1.0/.*", target => ["http://wasm_worker1", "http://wasm_worker2"], strategy => round_robin }, % Route WASM requests
    #{ template => "/.*", target => "http://default_worker.example.com" } % Default fallback
  ]
},
{ router_load_balancing_strategy, latency_aware }
```

*(Note: The actual configuration format and options should be verified in the `hb_opts.erl` and `dev_router.erl` source code.)*

## Running a Node with Router Registration

To run a HyperBEAM node that automatically registers with a router network:

### 1. Configure Registration Parameters

Create or update your node configuration file:

```erlang
% config/sys.config or similar
[
    {hyperbeam, [
        {router_node, "https://dev-router.forward.computer"},
        {router_prefix, "/my-unique-prefix~"},
        {router_template, "/my-unique-prefix~process@1.0/.*"},
        {router_price, 100},
        % ... other node configuration
    ]}
].
```

### 2. Start the Node

```bash
# Start HyperBEAM with router registration enabled
DEBUG=HB_PRINT rebar3 shell --config config/sys.config

# Or use environment variables
ROUTER_NODE="https://dev-router.forward.computer" \
ROUTER_PREFIX="/my-prefix~" \
ROUTER_TEMPLATE="/my-prefix~process@1.0/.*" \
ROUTER_PRICE=100 \
rebar3 shell
```

### 3. Manual Registration

If not configured for automatic registration, you can manually trigger registration:

```erlang
% In the Erlang shell
hb_ao:call(router_process_id, <<"register">>, #{}, Opts).
```

## Running a Simple Router

While a dedicated router setup is complex, any HyperBEAM node implicitly performs some level of routing, especially if it needs to interact with other nodes (e.g., via the `~relay@1.0` device). The default configuration might route certain requests internally or have basic forwarding capabilities.

To run a node that explicitly acts *more* like a router, you would typically configure it with specific `routes` pointing to other worker nodes, potentially disabling local execution for certain devices it intends to forward.

## Joining an Existing Router Network

To join an existing router network means to register your HyperBEAM node as a compute provider that can handle requests routed by the network. This makes your node part of the distributed computation infrastructure.

### Prerequisites for Joining

Before joining a router network, ensure your node:

1. **Has computational capabilities** you want to offer (specific devices, WASM execution, etc.)
2. **Is properly configured** with unique process IDs and route templates
3. **Has network connectivity** to communicate with router nodes
4. **Meets any trust requirements** of the target router network

### Joining Process

The joining process is essentially the node registration process detailed above:

1. **Configure your node** with appropriate route parameters
2. **Submit a registration request** to the target router
3. **Pass validation checks** (signature verification, admissibility, etc.)
4. **Get added to the routing table** and start receiving routed requests

Once successfully joined, your node will:
- Receive computational requests from the router
- Execute the requested operations
- Return results to clients via the router
- Participate in the distributed AO network

### Example: Joining dev-router.forward.computer

```erlang
% Configure your node to join the public router
{router_node, "https://dev-router.forward.computer"},
{router_prefix, "/my-unique-node-id~"},
{router_template, "/my-unique-node-id~process@1.0/.*"},
{router_price, 50}  % Competitive pricing for services
```



## Monitoring Registration Status

You can monitor your node's registration status through:

### Log Messages
```bash
# Enable debug logging
DEBUG=HB_PRINT rebar3 shell

# Look for registration-related log messages
# [info] Router registration successful
# [debug] Route added to routing table
```

### Router State Queries
```erlang
% Query router for current routing table
hb_ao:call(RouterProcessID, <<"routes">>, #{}, Opts).

% Check if node is registered
hb_ao:call(RouterProcessID, <<"status">>, #{}, Opts).
```

<!-- ## Troubleshooting Registration

Common registration issues and solutions:

### Registration Rejected
- Verify your node's signature is valid
- Check if the router requires trusted node status
- Ensure your route prefix doesn't conflict with existing routes

### Network Connectivity
- Verify the router endpoint is accessible
- Check firewall and network configuration
- Test HTTP connectivity to the router

### Configuration Errors
- Validate all required parameters are set
- Check route template syntax
- Verify price is within acceptable range -->

## Further Exploration

*   Examine the `dev_router.erl` source code for detailed implementation.
*   Review the `scripts/dynamic-router.lua` for router-side logic.
*   Review the available configuration options in `hb_opts.erl` related to routing (`routes`, strategies, etc.).
*   Consult community channels for best practices on deploying production routers. -->
