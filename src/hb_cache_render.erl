%%% @doc A module that helps to render given Key graphs into the .dot files
-module(hb_cache_render).
-export([render/1, render/2, cache_path_to_dot/2, cache_path_to_dot/3, dot_to_svg/1]).
-export([get_graph_data/3, cache_path_to_graph/3]).
% Preparing data for testing
-export([prepare_unsigned_data/0,
         prepare_signed_data/0,
         prepare_deeply_nested_complex_message/0]).
-include("include/hb.hrl").


%% @doc Render the given Key into svg
render(StoreOrOpts) ->
    render(all, StoreOrOpts).


render(ToRender, StoreOrOpts) ->
    % Collect graph elements (nodes and arcs) by traversing the store
    % Generate and view the graph visualization
    % Write SVG to file and open it
    file:write_file("new_render_diagram.svg",
                    dot_to_svg(cache_path_to_dot(ToRender, StoreOrOpts))),
    os:cmd("open new_render_diagram.svg"),
    ok.


%% @doc Generate a dot file from a cache path and options/store
cache_path_to_dot(ToRender, StoreOrOpts) ->
    cache_path_to_dot(ToRender, #{}, StoreOrOpts).


cache_path_to_dot(ToRender, RenderOpts, StoreOrOpts) ->
    graph_to_dot(cache_path_to_graph(ToRender, RenderOpts, StoreOrOpts), StoreOrOpts).


%% @doc Main function to collect graph elements
cache_path_to_graph(ToRender, GraphOpts, StoreOrOpts) when is_map(StoreOrOpts) ->
    Store = hb_opts:get(store, no_viable_store, StoreOrOpts),
    ?event({store, Store}),
    cache_path_to_graph(ToRender, GraphOpts, Store, StoreOrOpts).


cache_path_to_graph(all, GraphOpts, Store, Opts) ->
    {ok, Keys} = hb_store:list(Store, <<"/">>),
    ?event({all_keys, Keys}),
    cache_path_to_graph(Store, GraphOpts, Keys, Opts);
cache_path_to_graph(InitPath, GraphOpts, Store, Opts) when is_binary(InitPath) ->
    cache_path_to_graph(Store, GraphOpts, [InitPath], Opts);
cache_path_to_graph(Store, GraphOpts, RootKeys, Opts) ->
    % Use a map to track nodes, arcs and visited paths (to avoid cycles)
    EmptyGraph = GraphOpts#{nodes => #{}, arcs => #{}, visited => #{}},
    % Process all root keys and get the final graph
    lists:foldl(
      fun(Key, Acc) -> traverse_store(Store, Key, undefined, Acc, Opts) end,
      EmptyGraph,
      RootKeys).


%% @doc Traverse the store recursively to build the graph
traverse_store(Store, Path, Parent, Graph, Opts) ->
    % Get the path and check if we've already visited it
    JoinedPath = hb_store:join(Path),
    ResolvedPath =
        case hb_link:is_link_key(JoinedPath) of
            true ->
                ?event({is_link_key, {path, Path}, {res_path, JoinedPath}}),
                {ok, Link} = hb_store:read(Store, hb_store:resolve(Store, JoinedPath)),
                ?event({resolved_link, {read, Link}}),
                hb_store:resolve(Store, Link);
            false -> hb_store:resolve(Store, Path)
        end,
    ?event({traverse_store, {path, Path}, {joined_path, JoinedPath}, {resolved_path, ResolvedPath}, {parent, Parent}}),
    % Skip if we've already processed this node
    case hb_maps:get(visited, Graph, #{}, Opts) of
        #{JoinedPath := _} -> Graph;
        _ ->
            % Mark as visited to avoid cycles
            Graph1 = Graph#{visited => hb_maps:put(JoinedPath, true, hb_maps:get(visited, Graph, #{}, Opts), Opts)},
            % ?event({traverse_store, {key, Key}, {graph1, Graph1}}),
            % Process node based on its type
            case hb_store:type(Store, ResolvedPath) of
                simple ->
                    process_simple_node(Store, Path, Parent, ResolvedPath, JoinedPath, Graph1, Opts);
                composite ->
                    process_composite_node(Store, Path, Parent, ResolvedPath, JoinedPath, Graph1, Opts);
                _ ->
                    ?event({unknown_node_type, {path, Path}, {type, hb_store:type(Store, Path)}}),
                    Graph1
            end
    end.


%% @doc Process a simple (leaf) node
process_simple_node(_Store, Key, Parent, ResolvedPath, JoinedPath, Graph, Opts) ->
    % ?event({process_simple_node, {key, Key}, {resolved_path, ResolvedPath}}),
    % Add the node to the graph
    case hb_maps:get(render_data, Graph, true, Opts) of
        false -> Graph;
        true ->
            Graph1 = add_node(Graph, ResolvedPath, "lightblue", Opts),
            % If we have a parent, add an arc from parent to this node
            case Parent of
                undefined -> Graph1;
                ParentPath ->
                    Label = extract_label(JoinedPath),
                    add_arc(Graph1, ParentPath, ResolvedPath, Label, Opts)
            end
    end.


%% @doc Process a composite (directory) node
process_composite_node(_Store, <<"data">>, _Parent, _ResolvedPath, _JoinedPath, Graph, _Opts) ->
    % Data is a special case: It contains every binary item in the store.
    % We don't need to render it.
    Graph;
process_composite_node(Store, _Key, Parent, ResolvedPath, JoinedPath, Graph, Opts) ->
    % Add the node to the graph
    Graph1 = add_node(Graph, ResolvedPath, "lightcoral", Opts),
    % If we have a parent, add an arc from parent to this node
    Graph2 = case Parent of
                 undefined -> Graph1;
                 ParentPath ->
                     Label = extract_label(JoinedPath),
                     add_arc(Graph1, ParentPath, ResolvedPath, Label, Opts)
             end,
    % Process children recursively
    case hb_store:list(Store, ResolvedPath) of
        {ok, SubItems} ->
            lists:foldl(
              fun(SubItem, Acc) ->
                      ChildKey = [ResolvedPath, SubItem],
                      traverse_store(Store, ChildKey, ResolvedPath, Acc, Opts)
              end,
              Graph2,
              SubItems);
        _ -> Graph2
    end.


%% @doc Add a node to the graph
add_node(Graph, ID, Color, Opts) ->
    Nodes = hb_maps:get(nodes, Graph, #{}, Opts),
    Graph#{nodes => hb_maps:put(ID, {ID, Color}, Nodes, Opts)}.


%% @doc Add an arc to the graph
add_arc(Graph, From, To, Label, Opts) ->
    ?event({insert_arc, {id1, From}, {id2, To}, {label, Label}}),
    Arcs = hb_maps:get(arcs, Graph, #{}, Opts),
    Graph#{arcs => hb_maps:put({From, To, Label}, true, Arcs, Opts)}.


%% @doc Extract a label from a path
extract_label(Path) ->
    case binary:split(Path, <<"/">>, [global]) of
        [] -> Path;
        Parts ->
            FilteredParts = [ P || P <- Parts, P /= <<>> ],
            case FilteredParts of
                [] -> Path;
                _ -> lists:last(FilteredParts)
            end
    end.


%% @doc Generate the DOT file from the graph
graph_to_dot(Graph, Opts) ->
    % Create graph header
    Header = [<<"digraph filesystem {\n">>,
              <<"  node [shape=circle];\n">>],
    % Create nodes section
    Nodes = hb_maps:fold(
              fun(ID, {Label, Color}, Acc) ->
                      [Acc,
                       io_lib:format(
                         <<"  \"~s\" [label=\"~s\", color=~s, style=filled];~n">>,
                         [ID, hb_util:short_id(hb_util:bin(Label)), Color])]
              end,
              [],
              hb_maps:get(nodes, Graph, #{}, Opts),
              Opts),
    % Create arcs section
    Arcs = hb_maps:fold(
             fun({From, To, Label}, _, Acc) ->
                     [Acc,
                      io_lib:format(
                        <<"  \"~s\" -> \"~s\" [label=\"~s\"];~n">>,
                        [From, To, hb_util:short_id(hb_util:bin(Label))])]
             end,
             [],
             hb_maps:get(arcs, Graph, #{}, Opts),
             Opts),
    % Create graph footer
    Footer = <<"}\n">>,
    % Combine all parts and convert to binary
    iolist_to_binary([Header, Nodes, Arcs, Footer]).


%% @doc Convert a dot graph to SVG format
dot_to_svg(DotInput) ->
    % Create a port to the dot command
    Port = open_port({spawn, "dot -Tsvg"}, [binary, use_stdio, stderr_to_stdout]),
    % Send the dot content to the process
    true = port_command(Port, iolist_to_binary(DotInput)),
    % Get the SVG output
    collect_output(Port, []).


%% @doc Helper function to collect output from port
collect_output(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            case binary:part(Data, byte_size(Data) - 7, 7) of
                <<"</svg>\n">> ->
                    port_close(Port),
                    iolist_to_binary(lists:reverse([Data | Acc]));
                _ -> collect_output(Port, [Data | Acc])
            end;
        {Port, eof} ->
            port_close(Port),
            iolist_to_binary(lists:reverse(Acc))
    after
        10000 ->
            {error, timeout}
    end.


%% @doc Get graph data for the Three.js visualization
get_graph_data(Base, MaxSize, Opts) ->
    % Try to generate graph using hb_cache_render
    Graph =
        try
            % Use hb_cache_render to build the graph
            cache_path_to_graph(Base, #{}, Opts)
        catch
            Error:Reason:Stack ->
                ?event({hyperbuddy_graph_error, Error, Reason, Stack}),
                #{nodes => #{}, arcs => #{}, visited => #{}}
        end,
    % Extract nodes and links for the visualization
    NodesMap = maps:get(nodes, Graph, #{}),
    ArcsMap = maps:get(arcs, Graph, #{}),
    % Limit to top `MaxSize` nodes if there are too many
    NodesList =
        case maps:size(NodesMap) > MaxSize of
            true ->
                % Take a subset of nodes
                {ReducedNodes, _} = lists:split(
                                      MaxSize,
                                      maps:to_list(NodesMap)),
                ReducedNodes;
            false ->
                maps:to_list(NodesMap)
        end,
    % Get node IDs for filtering links
    NodeIds = [ ID || {ID, _} <- NodesList ],
    % Convert to JSON format for web visualization
    Nodes =
        [ #{
            <<"id">> => ID,
            <<"label">> => get_label(hb_util:bin(ID)),
            <<"type">> => get_node_type(Color)
           }
          || {ID, {_, Color}} <- NodesList ],
    % Filter links to only include those between nodes we're showing
    FilteredLinks =
        [ {From, To, Label}
          || {From, To, Label} <- maps:keys(ArcsMap),
             lists:member(From, NodeIds) andalso
             lists:member(To, NodeIds) ],
    Links =
        [ #{
            <<"source">> => From,
            <<"target">> => To,
            <<"label">> => Label
           }
          || {From, To, Label} <- FilteredLinks ],
    % Return the JSON data
    JsonData = hb_json:encode(#{<<"nodes">> => Nodes, <<"links">> => Links}),
    {ok, #{
           <<"body">> => JsonData,
           <<"content-type">> => <<"application/json">>
          }}.


%% @doc Convert node color from hb_cache_render to node type for visualization
get_node_type(Color) ->
    case Color of
        "lightblue" -> <<"simple">>;
        "lightcoral" -> <<"composite">>;
        _ -> <<"unknown">>
    end.


%% @doc Extract a readable label from a path
get_label(Path) ->
    case binary:split(Path, <<"/">>, [global]) of
        [] -> Path;
        Parts ->
            FilteredParts = [ P || P <- Parts, P /= <<>> ],
            case FilteredParts of
                [] -> Path;
                _ -> lists:last(FilteredParts)
            end
    end.


% Test data preparation functions
prepare_unsigned_data() ->
    Opts = #{
             store => #{
                        <<"store-module">> => hb_store_fs,
                        <<"name">> => <<"cache-TEST/render-fs">>
                       }
            },
    Item = test_unsigned(#{<<"key">> => <<"Simple unsigned data item">>}),
    {ok, _Path} = hb_cache:write(Item, Opts).


prepare_signed_data() ->
    Opts = #{
             store => #{
                        <<"store-module">> => hb_store_fs,
                        <<"name">> => <<"cache-TEST/render-fs">>
                       }
            },
    Wallet = ar_wallet:new(),
    Item = test_signed(#{<<"l2-test-key">> => <<"l2-test-value">>}, Wallet),
    %% Write the simple unsigned item
    {ok, _Path} = hb_cache:write(Item, Opts).


prepare_deeply_nested_complex_message() ->
    Opts = #{
             store => #{
                        <<"store-module">> => hb_store_fs,
                        <<"name">> => <<"cache-TEST/render-fs">>
                       }
            },
    Wallet = ar_wallet:new(),
    %% Create nested data
    Level3SignedSubmessage = test_signed([1, 2, 3], Wallet),
    Outer =
        #{
          <<"level1">> =>
              hb_message:commit(
                #{
                  <<"level2">> =>
                      #{
                        <<"level3">> => Level3SignedSubmessage,
                        <<"e">> => <<"f">>,
                        <<"z">> => [1, 2, 3]
                       },
                  <<"c">> => <<"d">>,
                  <<"g">> => [<<"h">>, <<"i">>],
                  <<"j">> => 1337
                 },
                ar_wallet:new()),
          <<"a">> => <<"b">>
         },
    %% Write the nested item
    {ok, _} = hb_cache:write(Outer, Opts).


test_unsigned(Data) ->
    #{
      <<"base-test-key">> => <<"base-test-value">>,
      <<"data">> => Data
     }.


test_signed(Data, Wallet) ->
    hb_message:commit(test_unsigned(Data), Wallet).
