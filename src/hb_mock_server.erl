%%% @doc Mock HTTP server for testing. Collects request bodies and returns
%%% configurable responses. 
-module(hb_mock_server).
-export([start/1, stop/1, get_requests/2, get_requests/3, get_requests/4]).
%% Cowboy handler callback
-export([init/2]).
-include("include/hb.hrl").

%%%===================================================================
%%% Public API
%%%===================================================================

%% @doc Start a generic mock HTTP server that collects request bodies.
%% Usage: start([{"/endpoint", endpoint_tag, {status, body}}, ...])
%%        start([{"/endpoint", endpoint_tag, fun(Req) -> {Status, Body} end}, ...])
%%        start([{"/endpoint", endpoint_tag}, ...]) for default {200, <<"OK">>}
%%
%% Response formats:
%%   {Status, Body}     - Static response
%%   fun(Req) -> ...    - Function called with request map, returns {Status, Body}
%%
%% Paths support Cowboy route patterns:
%%   "/price/:amount"  - Matches /price/123, /price/abc, etc.
%%   "/user/:id/post/:post_id" - Multiple parameters
%%   "/files/[...]"    - Catch-all (matches /files/anything/here)
%%
%% Automatically generates unique listener ID and dynamic port.
%% Returns: {ok, ServerURL, ServerHandle}
start(Endpoints) ->
    %% Ensure cowboy/ranch are started
    application:ensure_all_started(cowboy),
    CollectorPID = spawn(fun() -> collect_loop(#{}) end),
    ListenerID = make_ref(),
    NormalizedEndpoints = lists:map(
        fun
            ({Path, Tag, Response}) when is_function(Response) -> 
                {Path, Tag, Response};
            ({Path, Tag, {Status, Body}}) -> 
                {Path, Tag, {Status, Body}};
            ({Path, Tag}) -> 
                {Path, Tag, {200, <<>>}}
        end,
        Endpoints
    ),
    Routes = [
        {Path, ?MODULE, {Tag, Response, CollectorPID}}
        || {Path, Tag, Response} <- NormalizedEndpoints
    ],
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    {ok, _Listener} = cowboy:start_clear(
        ListenerID,
        [{port, 0}], %% dynamic port allocation
        #{env => #{dispatch => Dispatch}}
    ),
    %% Get the port that was assigned
    Port = ranch:get_port(ListenerID),
    ServerURL = iolist_to_binary(io_lib:format("http://localhost:~p", [Port])),
    {ok, ServerURL, {CollectorPID, ListenerID}}.

stop({CollectorPID, ListenerID}) ->
    cowboy:stop_listener(ListenerID),
    CollectorPID ! stop.

%% @doc Get all requests collected for a given endpoint tag.
%% Returns the accumulated requests without clearing them.
%% Takes the ServerHandle returned from start/1.
get_requests({CollectorPID, _ListenerID}, Tag) ->
    CollectorPID ! {get_requests, Tag, self()},
    receive
        {requests, Requests} -> Requests
    after 1000 -> []
    end.

get_requests(Type, Count, ServerHandle) ->
    get_requests(Type, Count, ServerHandle, 10000).

get_requests(Type, Count, ServerHandle, Timeout) ->
    %% Wait for expected transaction
    hb_util:wait_until(
        fun() ->
            Requests = get_requests(ServerHandle, Type),
            length(Requests) >= Count
        end,
        Timeout
    ),
    get_requests(ServerHandle, Type).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Collector process loop for mock server.
collect_loop(State) ->
    receive
        {request, Tag, Body} ->
            ?event({request, Tag, Body}),
            Requests = maps:get(Tag, State, []),
            collect_loop(State#{Tag => [Body | Requests]});
        {get_requests, Tag, From} ->
            Requests = maps:get(Tag, State, []),
            From ! {requests, lists:reverse(Requests)},
            %% Keep the requests in state (don't clear them)
            collect_loop(State);
        stop -> ok
    end.

%% @doc Convert a cowboy request to a message (i.e. just convert the atom
%% keys to binaries and add the body)
request_to_message(Req, Body) ->
    maps:fold(
        fun(Key, Value, Acc) ->
            maps:put(hb_util:bin(Key), Value, Acc)
        end,
        #{<<"body">> => Body},
        Req
    ).

%%%===================================================================
%%% Cowboy Handler Callback
%%%===================================================================

%% @doc Cowboy handler callback - DO NOT CALL DIRECTLY.
%% This is invoked automatically by Cowboy when requests arrive at the 
%% mock server. See start/1 for usage.
init(Req0, {Tag, Response, CollectorPID} = State) ->
    {ok, Body, Req} = cowboy_req:read_body(Req0),
    Msg = request_to_message(Req, Body),
    CollectorPID ! {request, Tag, Msg},
    %% Determine the response - either call the function or use the static value
    {StatusCode, ResponseBody} = case is_function(Response) of
        true -> Response(Msg);
        false -> Response
    end,
    {ok, cowboy_req:reply(StatusCode, #{}, ResponseBody, Req), State}.

