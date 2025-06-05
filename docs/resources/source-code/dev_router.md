# [Module dev_router.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_router.erl)




A device that routes outbound messages from the node to their
appropriate network recipients via HTTP.

<a name="description"></a>

## Description ##

All messages are initially
routed to a single process per node, which then load-balances them
between downstream workers that perform the actual requests.

The routes for the router are defined in the `routes` key of the `Opts`,
as a precidence-ordered list of maps. The first map that matches the
message will be used to determine the route.

Multiple nodes can be specified as viable for a single route, with the
`Choose` key determining how many nodes to choose from the list (defaulting
to 1). The `Strategy` key determines the load distribution strategy,
which can be one of `Random`, `By-Base`, or `Nearest`. The route may also
define additional parallel execution parameters, which are used by the
`hb_http` module to manage control of requests.

The structure of the routes should be as follows:

```

       Node?: The node to route the message to.
       Nodes?: A list of nodes to route the message to.
       Strategy?: The load distribution strategy to use.
       Choose?: The number of nodes to choose from the list.
       Template?: A message template to match the message against, either as a
                  map or a path regex.
```
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_route_test-0">add_route_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#apply_route-2">apply_route/2*</a></td><td>Apply a node map's rules for transforming the path of the message.</td></tr><tr><td valign="top"><a href="#apply_routes-3">apply_routes/3*</a></td><td>Generate a <code>uri</code> key for each node in a route.</td></tr><tr><td valign="top"><a href="#binary_to_bignum-1">binary_to_bignum/1*</a></td><td>Cast a human-readable or native-encoded ID to a big integer.</td></tr><tr><td valign="top"><a href="#by_base_determinism_test-0">by_base_determinism_test/0*</a></td><td>Ensure that <code>By-Base</code> always chooses the same node for the same
hashpath.</td></tr><tr><td valign="top"><a href="#choose-5">choose/5*</a></td><td>Implements the load distribution strategies if given a cluster.</td></tr><tr><td valign="top"><a href="#choose_1_test-1">choose_1_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#choose_n_test-1">choose_n_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#device_call_from_singleton_test-0">device_call_from_singleton_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#dynamic_route_provider_test-0">dynamic_route_provider_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#dynamic_router_test-0">dynamic_router_test/0*</a></td><td>Example of a Lua module being used as the <code>route_provider</code> for a
HyperBEAM node.</td></tr><tr><td valign="top"><a href="#dynamic_routing_by_performance-0">dynamic_routing_by_performance/0*</a></td><td></td></tr><tr><td valign="top"><a href="#dynamic_routing_by_performance_test_-0">dynamic_routing_by_performance_test_/0*</a></td><td>Demonstrates routing tables being dynamically created and adjusted
according to the real-time performance of nodes.</td></tr><tr><td valign="top"><a href="#explicit_route_test-0">explicit_route_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#extract_base-2">extract_base/2*</a></td><td>Extract the base message ID from a request message.</td></tr><tr><td valign="top"><a href="#field_distance-2">field_distance/2*</a></td><td>Calculate the minimum distance between two numbers
(either progressing backwards or forwards), assuming a
256-bit field.</td></tr><tr><td valign="top"><a href="#find_target_path-2">find_target_path/2*</a></td><td>Find the target path to route for a request message.</td></tr><tr><td valign="top"><a href="#generate_hashpaths-1">generate_hashpaths/1*</a></td><td></td></tr><tr><td valign="top"><a href="#generate_nodes-1">generate_nodes/1*</a></td><td></td></tr><tr><td valign="top"><a href="#get_routes_test-0">get_routes_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>Exported function for getting device info, controls which functions are
exposed via the device API.</td></tr><tr><td valign="top"><a href="#info-3">info/3</a></td><td>HTTP info response providing information about this device.</td></tr><tr><td valign="top"><a href="#load_routes-1">load_routes/1*</a></td><td>Load the current routes for the node.</td></tr><tr><td valign="top"><a href="#local_dynamic_router_test-0">local_dynamic_router_test/0*</a></td><td>Example of a Lua module being used as the <code>route_provider</code> for a
HyperBEAM node.</td></tr><tr><td valign="top"><a href="#local_process_route_provider_test-0">local_process_route_provider_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#lowest_distance-1">lowest_distance/1*</a></td><td>Find the node with the lowest distance to the given hashpath.</td></tr><tr><td valign="top"><a href="#lowest_distance-2">lowest_distance/2*</a></td><td></td></tr><tr><td valign="top"><a href="#match-3">match/3</a></td><td>Find the first matching template in a list of known routes.</td></tr><tr><td valign="top"><a href="#match_routes-3">match_routes/3*</a></td><td></td></tr><tr><td valign="top"><a href="#match_routes-4">match_routes/4*</a></td><td></td></tr><tr><td valign="top"><a href="#preprocess-3">preprocess/3</a></td><td>Preprocess a request to check if it should be relayed to a different node.</td></tr><tr><td valign="top"><a href="#register-3">register/3</a></td><td></td></tr><tr><td valign="top"><a href="#relay_nearest_test-0">relay_nearest_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#route-2">route/2</a></td><td>Find the appropriate route for the given message.</td></tr><tr><td valign="top"><a href="#route-3">route/3</a></td><td></td></tr><tr><td valign="top"><a href="#route_provider_test-0">route_provider_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#route_regex_matches_test-0">route_regex_matches_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#route_template_message_matches_test-0">route_template_message_matches_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#routes-3">routes/3</a></td><td>Device function that returns all known routes.</td></tr><tr><td valign="top"><a href="#simulate-4">simulate/4*</a></td><td></td></tr><tr><td valign="top"><a href="#simulation_distribution-2">simulation_distribution/2*</a></td><td></td></tr><tr><td valign="top"><a href="#simulation_occurences-2">simulation_occurences/2*</a></td><td></td></tr><tr><td valign="top"><a href="#strategy_suite_test_-0">strategy_suite_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#template_matches-3">template_matches/3*</a></td><td>Check if a message matches a message template or path regex.</td></tr><tr><td valign="top"><a href="#unique_nodes-1">unique_nodes/1*</a></td><td></td></tr><tr><td valign="top"><a href="#unique_test-1">unique_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#weighted_random_strategy_test-0">weighted_random_strategy_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#within_norms-3">within_norms/3*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_route_test-0"></a>

### add_route_test/0 * ###

`add_route_test() -> any()`

<a name="apply_route-2"></a>

### apply_route/2 * ###

`apply_route(Msg, Route) -> any()`

Apply a node map's rules for transforming the path of the message.
Supports the following keys:
- `opts`: A map of options to pass to the request.
- `prefix`: The prefix to add to the path.
- `suffix`: The suffix to add to the path.
- `replace`: A regex to replace in the path.

<a name="apply_routes-3"></a>

### apply_routes/3 * ###

`apply_routes(Msg, R, Opts) -> any()`

Generate a `uri` key for each node in a route.

<a name="binary_to_bignum-1"></a>

### binary_to_bignum/1 * ###

`binary_to_bignum(Bin) -> any()`

Cast a human-readable or native-encoded ID to a big integer.

<a name="by_base_determinism_test-0"></a>

### by_base_determinism_test/0 * ###

`by_base_determinism_test() -> any()`

Ensure that `By-Base` always chooses the same node for the same
hashpath.

<a name="choose-5"></a>

### choose/5 * ###

`choose(N, X2, Hashpath, Nodes, Opts) -> any()`

Implements the load distribution strategies if given a cluster.

<a name="choose_1_test-1"></a>

### choose_1_test/1 * ###

`choose_1_test(Strategy) -> any()`

<a name="choose_n_test-1"></a>

### choose_n_test/1 * ###

`choose_n_test(Strategy) -> any()`

<a name="device_call_from_singleton_test-0"></a>

### device_call_from_singleton_test/0 * ###

`device_call_from_singleton_test() -> any()`

<a name="dynamic_route_provider_test-0"></a>

### dynamic_route_provider_test/0 * ###

`dynamic_route_provider_test() -> any()`

<a name="dynamic_router_test-0"></a>

### dynamic_router_test/0 * ###

`dynamic_router_test() -> any()`

Example of a Lua module being used as the `route_provider` for a
HyperBEAM node. The module utilized in this example dynamically adjusts the
likelihood of routing to a given node, depending upon price and performance.
also include preprocessing support for routing

<a name="dynamic_routing_by_performance-0"></a>

### dynamic_routing_by_performance/0 * ###

`dynamic_routing_by_performance() -> any()`

<a name="dynamic_routing_by_performance_test_-0"></a>

### dynamic_routing_by_performance_test_/0 * ###

`dynamic_routing_by_performance_test_() -> any()`

Demonstrates routing tables being dynamically created and adjusted
according to the real-time performance of nodes. This test utilizes the
`dynamic-router` script to manage routes and recalculate weights based on the
reported performance.

<a name="explicit_route_test-0"></a>

### explicit_route_test/0 * ###

`explicit_route_test() -> any()`

<a name="extract_base-2"></a>

### extract_base/2 * ###

`extract_base(RawPath, Opts) -> any()`

Extract the base message ID from a request message. Produces a single
binary ID that can be used for routing decisions.

<a name="field_distance-2"></a>

### field_distance/2 * ###

`field_distance(A, B) -> any()`

Calculate the minimum distance between two numbers
(either progressing backwards or forwards), assuming a
256-bit field.

<a name="find_target_path-2"></a>

### find_target_path/2 * ###

`find_target_path(Msg, Opts) -> any()`

Find the target path to route for a request message.

<a name="generate_hashpaths-1"></a>

### generate_hashpaths/1 * ###

`generate_hashpaths(Runs) -> any()`

<a name="generate_nodes-1"></a>

### generate_nodes/1 * ###

`generate_nodes(N) -> any()`

<a name="get_routes_test-0"></a>

### get_routes_test/0 * ###

`get_routes_test() -> any()`

<a name="info-1"></a>

### info/1 ###

`info(X1) -> any()`

Exported function for getting device info, controls which functions are
exposed via the device API.

<a name="info-3"></a>

### info/3 ###

`info(Msg1, Msg2, Opts) -> any()`

HTTP info response providing information about this device

<a name="load_routes-1"></a>

### load_routes/1 * ###

`load_routes(Opts) -> any()`

Load the current routes for the node. Allows either explicit routes from
the node message's `routes` key, or dynamic routes generated by resolving the
`route_provider` message.

<a name="local_dynamic_router_test-0"></a>

### local_dynamic_router_test/0 * ###

`local_dynamic_router_test() -> any()`

Example of a Lua module being used as the `route_provider` for a
HyperBEAM node. The module utilized in this example dynamically adjusts the
likelihood of routing to a given node, depending upon price and performance.

<a name="local_process_route_provider_test-0"></a>

### local_process_route_provider_test/0 * ###

`local_process_route_provider_test() -> any()`

<a name="lowest_distance-1"></a>

### lowest_distance/1 * ###

`lowest_distance(Nodes) -> any()`

Find the node with the lowest distance to the given hashpath.

<a name="lowest_distance-2"></a>

### lowest_distance/2 * ###

`lowest_distance(Nodes, X) -> any()`

<a name="match-3"></a>

### match/3 ###

`match(Base, Req, Opts) -> any()`

Find the first matching template in a list of known routes. Allows the
path to be specified by either the explicit `path` (for internal use by this
module), or `route-path` for use by external devices and users.

<a name="match_routes-3"></a>

### match_routes/3 * ###

`match_routes(ToMatch, Routes, Opts) -> any()`

<a name="match_routes-4"></a>

### match_routes/4 * ###

`match_routes(ToMatch, Routes, Keys, Opts) -> any()`

<a name="preprocess-3"></a>

### preprocess/3 ###

`preprocess(Msg1, Msg2, Opts) -> any()`

Preprocess a request to check if it should be relayed to a different node.

<a name="register-3"></a>

### register/3 ###

`register(M1, M2, Opts) -> any()`

<a name="relay_nearest_test-0"></a>

### relay_nearest_test/0 * ###

`relay_nearest_test() -> any()`

<a name="route-2"></a>

### route/2 ###

`route(Msg, Opts) -> any()`

Find the appropriate route for the given message. If we are able to
resolve to a single host+path, we return that directly. Otherwise, we return
the matching route (including a list of nodes under `nodes`) from the list of
routes.

If we have a route that has multiple resolving nodes, check
the load distribution strategy and choose a node. Supported strategies:

```

            All: Return all nodes (default).
         Random: Distribute load evenly across all nodes, non-deterministically.
        By-Base: According to the base message's hashpath.
      By-Weight: According to the node's <code>weight</code> key.
        Nearest: According to the distance of the node's wallet address to the
                 base message's hashpath.
```

`By-Base` will ensure that all traffic for the same hashpath is routed to the
same node, minimizing work duplication, while `Random` ensures a more even
distribution of the requests.

Can operate as a `~router@1.0` device, which will ignore the base message,
routing based on the Opts and request message provided, or as a standalone
function, taking only the request message and the `Opts` map.

<a name="route-3"></a>

### route/3 ###

`route(X1, Msg, Opts) -> any()`

<a name="route_provider_test-0"></a>

### route_provider_test/0 * ###

`route_provider_test() -> any()`

<a name="route_regex_matches_test-0"></a>

### route_regex_matches_test/0 * ###

`route_regex_matches_test() -> any()`

<a name="route_template_message_matches_test-0"></a>

### route_template_message_matches_test/0 * ###

`route_template_message_matches_test() -> any()`

<a name="routes-3"></a>

### routes/3 ###

`routes(M1, M2, Opts) -> any()`

Device function that returns all known routes.

<a name="simulate-4"></a>

### simulate/4 * ###

`simulate(Runs, ChooseN, Nodes, Strategy) -> any()`

<a name="simulation_distribution-2"></a>

### simulation_distribution/2 * ###

`simulation_distribution(SimRes, Nodes) -> any()`

<a name="simulation_occurences-2"></a>

### simulation_occurences/2 * ###

`simulation_occurences(SimRes, Nodes) -> any()`

<a name="strategy_suite_test_-0"></a>

### strategy_suite_test_/0 * ###

`strategy_suite_test_() -> any()`

<a name="template_matches-3"></a>

### template_matches/3 * ###

`template_matches(ToMatch, Template, Opts) -> any()`

Check if a message matches a message template or path regex.

<a name="unique_nodes-1"></a>

### unique_nodes/1 * ###

`unique_nodes(Simulation) -> any()`

<a name="unique_test-1"></a>

### unique_test/1 * ###

`unique_test(Strategy) -> any()`

<a name="weighted_random_strategy_test-0"></a>

### weighted_random_strategy_test/0 * ###

`weighted_random_strategy_test() -> any()`

<a name="within_norms-3"></a>

### within_norms/3 * ###

`within_norms(SimRes, Nodes, TestSize) -> any()`

