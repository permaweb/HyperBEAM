%%% @doc A device that renders a REPL-like interface for AO-Core via HTML.
-module(dev_hyperbuddy).
-export([info/0, format/3, metrics/3]).
-include_lib("include/hb.hrl").

%% @doc Export an explicit list of files via http.
info() ->
    #{
        default => fun serve/4,
        routes => #{
            <<"index">> => <<"index.html">>,
            <<"console">> => <<"console.html">>,
            <<"graph">> => <<"graph.html">>,
			<<"styles.css">> => <<"styles.css">>,
			<<"metrics.js">> => <<"metrics.js">>,
			<<"devices.js">> => <<"devices.js">>,
			<<"utils.js">> => <<"utils.js">>,
			<<"main.js">> => <<"main.js">>,
			<<"graph.js">> => <<"graph.js">>
        }
    }.

%% @doc The main HTML page for the REPL device.
metrics(_, Req, Opts) ->
    case hb_opts:get(prometheus, not hb_features:test(), Opts) of
        true ->
            {_, HeaderList, Body} =
            prometheus_http_impl:reply(
                #{path => true,
                headers => 
                    fun(Name, Default) ->
                        hb_ao:get(Name, Req, Default, Opts)
                    end,
                registry => prometheus_registry:exists(<<"default">>),
                standalone => false}
            ),
            RawHeaderMap = maps:from_list(prometheus_cowboy:to_cowboy_headers(HeaderList)),
            Headers = maps:map(fun(_, Value) -> hb_util:bin(Value) end, RawHeaderMap),
            {ok, Headers#{ <<"body">> => Body }};
        false ->
            {ok, #{ <<"body">> => <<"Prometheus metrics disabled.">> }}
    end.

%% @doc Employ HyperBEAM's internal pretty printer to format a message.
format(Base, _, _) ->
    {ok, #{ <<"body">> => hb_util:bin(hb_message:format(Base)) }}.

%% @doc Serve a file from the priv directory. Only serves files that are explicitly
%% listed in the `routes' field of the `info/0' return value.
serve(<<"keys">>, M1, _M2, _Opts) -> dev_message:keys(M1);
serve(<<"set">>, M1, M2, Opts) -> dev_message:set(M1, M2, Opts);
serve(<<"graph-data">>, _, _, Opts) -> get_graph_data(Opts);
serve(Key, _, _, _) ->
    ?event({hyperbuddy_serving, Key}),
    case maps:get(Key, maps:get(routes, info(), no_routes), undefined) of
        undefined -> {error, not_found};
        Filename -> return_file(Filename)
    end.

%% @doc Get graph data for the Three.js visualization
get_graph_data(Opts) ->
    % Get the store from options
    Store = hb_opts:get(store, no_viable_store, Opts),
    
    % Create empty graph structure in case of errors
    Graph = #{nodes => #{}, arcs => #{}, visited => #{}},
    
    % Try to generate a graph similar to hb_cache_render
    Graph1 = try
        % Use existing render function to get keys in the store
        {ok, Keys} = hb_store:list(Store, "/"),
        build_graph(Store, Keys, Graph)
    catch
        Error:Reason:Stack -> 
            ?event({hyperbuddy_graph_error, Error, Reason, Stack}),
            Graph
    end,
    
    % Extract nodes and links for the visualization
    NodesMap = maps:get(nodes, Graph1, #{}),
    ArcsMap = maps:get(arcs, Graph1, #{}),
    
    % Limit to top 500 nodes if there are too many
    NodesList = 
        case maps:size(NodesMap) > 500 of
            true ->
                % Take a subset of nodes
                {ReducedNodes, _} = lists:split(
                    500,
                    maps:to_list(NodesMap)
                ),
                ReducedNodes;
            false ->
                maps:to_list(NodesMap)
        end,
    
    % Get node IDs for filtering links
    NodeIds = [ID || {ID, _} <- NodesList],
    
    % Convert to JSON format
    Nodes = [
        #{
            <<"id">> => ID,
            <<"label">> => get_label(ID),
            <<"type">> => Type
        }
        || {ID, Type} <- NodesList
    ],
    
    % Filter links to only include those between nodes we're showing
    FilteredLinks = [
        {From, To, Label}
        || {From, To, Label} <- maps:keys(ArcsMap),
           lists:member(From, NodeIds) andalso lists:member(To, NodeIds)
    ],
    
    Links = [
        #{
            <<"source">> => From,
            <<"target">> => To,
            <<"label">> => Label
        }
        || {From, To, Label} <- FilteredLinks
    ],
    
    % Return the JSON data
    JsonData = hb_json:encode(#{
        <<"nodes">> => Nodes,
        <<"links">> => Links
    }),
    
    {ok, #{
        <<"body">> => JsonData,
        <<"content-type">> => <<"application/json">>
    }}.

%% @doc Build a graph structure by traversing the store recursively with cycle detection
build_graph(Store, RootKeys, Graph) ->
    % Process all root keys and get the final graph
    lists:foldl(
        fun(Key, Acc) -> traverse_store(Store, Key, undefined, Acc) end,
        Graph,
        RootKeys
    ).

%% @doc Traverse the store recursively to build the graph with cycle detection
traverse_store(Store, Key, Parent, Graph) ->
    % Get the path and check if we've already visited it
    JoinedPath = case is_binary(Key) of
        true -> Key;
        false -> hb_store:join(Key)
    end,
    
    % Resolve the path
    ResolvedPath = hb_store:resolve(Store, Key),
    
    % Skip if we've already processed this node
    case maps:get(visited, Graph, #{}) of
        #{JoinedPath := _} -> Graph;
        _ ->
            % Mark as visited to avoid cycles
            Graph1 = Graph#{visited => maps:put(JoinedPath, true, maps:get(visited, Graph, #{}))},
            
            % Skip special paths that would lead to too many nodes
            case JoinedPath of
                <<"data">> -> Graph1; % Skip the data path
                _ ->
                    % Process node based on its type
                    case hb_store:type(Store, Key) of
                        simple -> 
                            process_simple_node(Store, Key, Parent, ResolvedPath, JoinedPath, Graph1);
                        composite -> 
                            process_composite_node(Store, Key, Parent, ResolvedPath, JoinedPath, Graph1);
                        _ -> 
                            Graph1
                    end
            end
    end.

%% @doc Process a simple (leaf) node
process_simple_node(Store, _Key, Parent, ResolvedPath, JoinedPath, Graph) ->
    % Add the node to the graph
    Graph1 = add_node_to_graph(Graph, ResolvedPath, <<"simple">>),
    
    % If we have a parent, add an arc from parent to this node
    case Parent of
        undefined -> Graph1;
        ParentPath -> 
            Label = get_label(JoinedPath),
            add_arc_to_graph(Graph1, ParentPath, ResolvedPath, Label)
    end.

%% @doc Process a composite (directory) node
process_composite_node(Store, Key, Parent, ResolvedPath, JoinedPath, Graph) ->
    % Add the node to the graph
    Graph1 = add_node_to_graph(Graph, ResolvedPath, <<"composite">>),
    
    % If we have a parent, add an arc from parent to this node
    Graph2 = case Parent of
        undefined -> Graph1;
        ParentPath -> 
            Label = get_label(JoinedPath),
            add_arc_to_graph(Graph1, ParentPath, ResolvedPath, Label)
    end,
    
    % Process children recursively
    case hb_store:list(Store, ResolvedPath) of
        {ok, SubItems} ->
            % Only process a reasonable number of subitems to avoid explosions
            SafeSubItems = case length(SubItems) > 100 of
                true -> lists:sublist(SubItems, 100);
                false -> SubItems
            end,
            
            lists:foldl(
                fun(SubItem, Acc) ->
                    ChildKey = [ResolvedPath, SubItem],
                    traverse_store(Store, ChildKey, ResolvedPath, Acc)
                end,
                Graph2,
                SafeSubItems
            );
        _ -> Graph2
    end.

%% @doc Add a node to the graph with type information
add_node_to_graph(Graph, ID, Type) ->
    Nodes = maps:get(nodes, Graph, #{}),
    Graph#{nodes => maps:put(ID, Type, Nodes)}.

%% @doc Add an arc to the graph
add_arc_to_graph(Graph, From, To, Label) ->
    Arcs = maps:get(arcs, Graph, #{}),
    Graph#{arcs => maps:put({From, To, Label}, true, Arcs)}.

%% @doc Extract a readable label from a path
get_label(Path) ->
    case binary:split(hb_util:bin(Path), <<"/">>, [global]) of
        [] -> Path;
        Parts -> 
            FilteredParts = [P || P <- Parts, P /= <<>>],
            case FilteredParts of
                [] -> Path;
                _ -> lists:last(FilteredParts)
            end
    end.

%% @doc Read a file from disk and serve it as a static HTML page.
return_file(Name) ->
    Base = hb_util:bin(code:priv_dir(hb)),
    Filename = <<Base/binary, "/html/hyperbuddy@1.0/", Name/binary >>,
    ?event({hyperbuddy_serving, Filename}),
    {ok, Body} = file:read_file(Filename),
    {ok, #{
        <<"body">> => Body,
        <<"content-type">> =>
            case filename:extension(Filename) of
                <<".html">> -> <<"text/html">>;
                <<".js">> -> <<"text/javascript">>;
                <<".css">> -> <<"text/css">>;
                <<".png">> -> <<"image/png">>;
                <<".ico">> -> <<"image/x-icon">>
            end
    }}.