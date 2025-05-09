# [Module hb_cache_render.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_cache_render.erl)




A module that helps to render given Key graphs into the .dot files.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_arc-4">add_arc/4*</a></td><td>Add an arc to the graph.</td></tr><tr><td valign="top"><a href="#add_node-3">add_node/3*</a></td><td>Add a node to the graph.</td></tr><tr><td valign="top"><a href="#cache_path_to_dot-2">cache_path_to_dot/2</a></td><td>Generate a dot file from a cache path and options/store.</td></tr><tr><td valign="top"><a href="#cache_path_to_dot-3">cache_path_to_dot/3</a></td><td></td></tr><tr><td valign="top"><a href="#cache_path_to_graph-3">cache_path_to_graph/3</a></td><td>Main function to collect graph elements.</td></tr><tr><td valign="top"><a href="#collect_output-2">collect_output/2*</a></td><td>Helper function to collect output from port.</td></tr><tr><td valign="top"><a href="#dot_to_svg-1">dot_to_svg/1</a></td><td>Convert a dot graph to SVG format.</td></tr><tr><td valign="top"><a href="#extract_label-1">extract_label/1*</a></td><td>Extract a label from a path.</td></tr><tr><td valign="top"><a href="#get_graph_data-1">get_graph_data/1</a></td><td>Get graph data for the Three.js visualization.</td></tr><tr><td valign="top"><a href="#get_label-1">get_label/1*</a></td><td>Extract a readable label from a path.</td></tr><tr><td valign="top"><a href="#get_node_type-1">get_node_type/1*</a></td><td>Convert node color from hb_cache_render to node type for visualization.</td></tr><tr><td valign="top"><a href="#graph_to_dot-1">graph_to_dot/1*</a></td><td>Generate the DOT file from the graph.</td></tr><tr><td valign="top"><a href="#prepare_deeply_nested_complex_message-0">prepare_deeply_nested_complex_message/0</a></td><td></td></tr><tr><td valign="top"><a href="#prepare_signed_data-0">prepare_signed_data/0</a></td><td></td></tr><tr><td valign="top"><a href="#prepare_unsigned_data-0">prepare_unsigned_data/0</a></td><td></td></tr><tr><td valign="top"><a href="#process_composite_node-6">process_composite_node/6*</a></td><td>Process a composite (directory) node.</td></tr><tr><td valign="top"><a href="#process_simple_node-6">process_simple_node/6*</a></td><td>Process a simple (leaf) node.</td></tr><tr><td valign="top"><a href="#render-1">render/1</a></td><td>Render the given Key into svg.</td></tr><tr><td valign="top"><a href="#render-2">render/2</a></td><td></td></tr><tr><td valign="top"><a href="#test_signed-2">test_signed/2*</a></td><td></td></tr><tr><td valign="top"><a href="#test_unsigned-1">test_unsigned/1*</a></td><td></td></tr><tr><td valign="top"><a href="#traverse_store-4">traverse_store/4*</a></td><td>Traverse the store recursively to build the graph.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_arc-4"></a>

### add_arc/4 * ###

`add_arc(Graph, From, To, Label) -> any()`

Add an arc to the graph

<a name="add_node-3"></a>

### add_node/3 * ###

`add_node(Graph, ID, Color) -> any()`

Add a node to the graph

<a name="cache_path_to_dot-2"></a>

### cache_path_to_dot/2 ###

`cache_path_to_dot(ToRender, StoreOrOpts) -> any()`

Generate a dot file from a cache path and options/store

<a name="cache_path_to_dot-3"></a>

### cache_path_to_dot/3 ###

`cache_path_to_dot(ToRender, RenderOpts, StoreOrOpts) -> any()`

<a name="cache_path_to_graph-3"></a>

### cache_path_to_graph/3 ###

`cache_path_to_graph(ToRender, GraphOpts, StoreOrOpts) -> any()`

Main function to collect graph elements

<a name="collect_output-2"></a>

### collect_output/2 * ###

`collect_output(Port, Acc) -> any()`

Helper function to collect output from port

<a name="dot_to_svg-1"></a>

### dot_to_svg/1 ###

`dot_to_svg(DotInput) -> any()`

Convert a dot graph to SVG format

<a name="extract_label-1"></a>

### extract_label/1 * ###

`extract_label(Path) -> any()`

Extract a label from a path

<a name="get_graph_data-1"></a>

### get_graph_data/1 ###

`get_graph_data(Opts) -> any()`

Get graph data for the Three.js visualization

<a name="get_label-1"></a>

### get_label/1 * ###

`get_label(Path) -> any()`

Extract a readable label from a path

<a name="get_node_type-1"></a>

### get_node_type/1 * ###

`get_node_type(Color) -> any()`

Convert node color from hb_cache_render to node type for visualization

<a name="graph_to_dot-1"></a>

### graph_to_dot/1 * ###

`graph_to_dot(Graph) -> any()`

Generate the DOT file from the graph

<a name="prepare_deeply_nested_complex_message-0"></a>

### prepare_deeply_nested_complex_message/0 ###

`prepare_deeply_nested_complex_message() -> any()`

<a name="prepare_signed_data-0"></a>

### prepare_signed_data/0 ###

`prepare_signed_data() -> any()`

<a name="prepare_unsigned_data-0"></a>

### prepare_unsigned_data/0 ###

`prepare_unsigned_data() -> any()`

<a name="process_composite_node-6"></a>

### process_composite_node/6 * ###

`process_composite_node(Store, Key, Parent, ResolvedPath, JoinedPath, Graph) -> any()`

Process a composite (directory) node

<a name="process_simple_node-6"></a>

### process_simple_node/6 * ###

`process_simple_node(Store, Key, Parent, ResolvedPath, JoinedPath, Graph) -> any()`

Process a simple (leaf) node

<a name="render-1"></a>

### render/1 ###

`render(StoreOrOpts) -> any()`

Render the given Key into svg

<a name="render-2"></a>

### render/2 ###

`render(ToRender, StoreOrOpts) -> any()`

<a name="test_signed-2"></a>

### test_signed/2 * ###

`test_signed(Data, Wallet) -> any()`

<a name="test_unsigned-1"></a>

### test_unsigned/1 * ###

`test_unsigned(Data) -> any()`

<a name="traverse_store-4"></a>

### traverse_store/4 * ###

`traverse_store(Store, Key, Parent, Graph) -> any()`

Traverse the store recursively to build the graph

