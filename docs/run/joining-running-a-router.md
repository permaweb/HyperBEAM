# Joining or Running a Router Node

Router nodes play a crucial role in the HyperBEAM network by directing incoming HTTP requests to appropriate worker nodes capable of handling the requested computation or data retrieval. They act as intelligent load balancers and entry points into the AO ecosystem.

!!! info "Advanced Topic"
    Configuring and running a production-grade router involves considerations beyond the scope of this introductory guide, including network topology, security, high availability, and performance tuning.

## What is a Router?

In HyperBEAM, the `dev_router` module (and associated logic) implements routing functionality. A node configured as a router typically:

1.  Receives external HTTP requests (HyperPATH calls).
2.  Parses the request path to determine the target process, device, and desired operation.
3.  Consults its routing table or logic to select an appropriate downstream worker node (which might be itself or another node).
4.  Forwards the request to the selected worker.
5.  Receives the response from the worker.
6.  Returns the response to the original client.

Routers often maintain information about the capabilities and load of worker nodes they know about.

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

## Running a Simple Router

While a dedicated router setup is complex, any HyperBEAM node implicitly performs some level of routing, especially if it needs to interact with other nodes (e.g., via the `~relay@1.0` device). The default configuration might route certain requests internally or have basic forwarding capabilities.

To run a node that explicitly acts *more* like a router, you would typically configure it with specific `routes` pointing to other worker nodes, potentially disabling local execution for certain devices it intends to forward.

## Joining an Existing Router Network

As a user or developer, you typically don't *run* the main public routers (like `router-1.forward.computer`). Instead, you configure your client applications (or your own local node if it needs to relay requests) to *use* these public routers as entry points.

When making HyperPATH calls, you simply target the public router's URL:

```
https://<router_url>/<process_id>~<device>/<key>...
```
The router handles directing your request to an appropriate compute node.

## Further Exploration

*   Examine the `dev_router.erl` source code for detailed implementation.
*   Review the available configuration options in `hb_opts.erl` related to routing (`routes`, strategies, etc.).
*   Consult community channels or advanced documentation for best practices on deploying production routers.
