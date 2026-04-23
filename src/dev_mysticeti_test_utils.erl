%%% @doc Shared helpers for Mysticeti-C test modules and examples.
%%%
%%% These utilities use the canonical `process@1.0` HTTP surface and the
%%% default scheduler response shape (a schedule map containing `assignments`).
%%% Assignment maps are normalized to slot keys (ints); non-slot metadata
%%% (e.g. commitments) is dropped.
%%%
%%% Network-setup helpers (scheduler-location registration, readiness probes)
%%% are shared across `dev_mysticeti_test_networks`, `dev_mysticeti_props`,
%%% and `hb_examples`.
-module(dev_mysticeti_test_utils).
-export([
    post_process_schedule/4,
    fetch_assignments_http/5,
    assignment_message/2,
    assignment_body/2,
    assignment_message_id/2
]).
%%% Network-setup helpers
-export([
    start_mysticeti_nodes/1,
    register_scheduler_location/2,
    post_scheduler_location/4,
    wait_for_scheduler_locations/3,
    wait_for_nodes_ready/2,
    scheduler_location_from_response/2,
    trim_trailing_slash/1,
    http_opts/1
]).
-include("include/hb.hrl").

%% @doc POST /<process>/schedule with a signed message.
post_process_schedule(Node, ProcID, Msg, Opts) ->
    MsgLoaded = hb_cache:ensure_all_loaded(Msg, Opts),
    _ = hb_cache:write(MsgLoaded, Opts),
    Req = #{
        <<"path">> => << ProcID/binary, "/schedule" >>,
        <<"method">> => <<"POST">>,
        <<"body">> => MsgLoaded
    },
    hb_http:post(Node, Req, Opts).

%% @doc Fetch assignments over HTTP (GET /<process>/schedule).
fetch_assignments_http(Node, ProcID, From, To, Opts) ->
    Req0 = #{ <<"path">> => << ProcID/binary, "/schedule" >> },
    Req1 = maybe_put(<<"from">>, From, Req0, Opts),
    Req = maybe_put(<<"to">>, To, Req1, Opts),
    case hb_http:get(Node, Req, Opts) of
        {ok, Schedule} ->
            slot_only_assignments(assignments_from_schedule(Schedule, Opts), Opts);
        _ -> #{}
    end.

%% @doc Extract the scheduled message from an assignment.
assignment_message(Assignment, Opts) ->
    Loaded = load_message(Assignment, Opts),
    case hb_maps:get(<<"body">>, Loaded, not_found, Opts) of
        not_found ->
            case hb_maps:get(<<"message">>, Loaded, not_found, Opts) of
                not_found ->
                    case hb_maps:get(<<"assignment">>, Loaded, not_found, Opts) of
                        not_found -> not_found;
                        Sub -> assignment_message(Sub, Opts)
                    end;
                Msg ->
                    load_message(Msg, Opts)
            end;
        Msg ->
            load_message(Msg, Opts)
    end.

%% @doc Extract the message body from an assignment.
assignment_body(Assignment, Opts) ->
    case assignment_message(Assignment, Opts) of
        not_found -> not_found;
        Msg when is_map(Msg) -> hb_maps:get(<<"body">>, Msg, not_found, Opts);
        Msg -> Msg
    end.

%% @doc Extract the message id from an assignment.
assignment_message_id(Assignment, Opts) ->
    case assignment_message(Assignment, Opts) of
        not_found -> {error, missing_message};
        Msg when ?IS_ID(Msg) -> {ok, Msg};
        Msg when is_map(Msg) ->
            try {ok, hb_message:id(Msg, all, Opts)}
            catch Class:Reason -> {error, {invalid_message, Class, Reason}}
            end;
        _ -> {error, not_a_message}
    end.

assignments_from_schedule(Schedule, Opts) when is_map(Schedule) ->
    case hb_maps:get(<<"assignments">>, Schedule, not_found, Opts) of
        not_found -> #{};
        Assignments -> assignments_to_map(Assignments, Opts)
    end;
assignments_from_schedule(_, _Opts) ->
    #{}.

assignments_to_map(Assignments, _Opts) when is_map(Assignments) ->
    Assignments;
assignments_to_map(Assignments, Opts) when is_list(Assignments) ->
    lists:foldl(
        fun(Item, Acc) ->
            case hb_maps:get(<<"slot">>, Item, not_found, Opts) of
                not_found -> Acc;
                Slot -> hb_maps:put(Slot, Item, Acc, Opts)
            end
        end,
        #{},
        Assignments
    );
assignments_to_map(_, _Opts) ->
    #{}.

slot_only_assignments(Assignments, Opts) when is_map(Assignments) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            case hb_util:safe_int(Key) of
                {ok, IntKey} -> hb_maps:put(IntKey, Value, Acc, Opts);
                {error, _} -> Acc
            end
        end,
        #{},
        hb_maps:to_list(Assignments, Opts)
    );
slot_only_assignments(_, _Opts) ->
    #{}.

maybe_put(_Key, undefined, Map, _Opts) ->
    Map;
maybe_put(_Key, not_found, Map, _Opts) ->
    Map;
maybe_put(Key, Value, Map, Opts) ->
    hb_maps:put(Key, Value, Map, Opts).

load_message(not_found, _Opts) ->
    not_found;
load_message(Msg, Opts) when is_map(Msg) ->
    hb_cache:ensure_all_loaded(Msg, Opts);
load_message(Msg, Opts) when ?IS_LINK(Msg) ->
    hb_cache:ensure_loaded(Msg, Opts);
load_message(Msg, Opts) when ?IS_ID(Msg) ->
    case hb_cache:read(Msg, Opts) of
        {ok, Loaded} -> Loaded;
        _ -> Msg
    end;
load_message(Msg, _Opts) ->
    Msg.

%%% Network-setup helpers

%% @doc Start N isolated Mysticeti nodes with separate stores.
start_mysticeti_nodes(NodeCount) ->
    Wallets = [ar_wallet:new() || _ <- lists:seq(1, NodeCount)],
    Validators = [hb_util:human_id(ar_wallet:to_address(W)) || W <- Wallets],
    Nodes =
        lists:map(
            fun({Wallet, Author, Index}) ->
                LocalStore = hb_test_utils:test_store(),
                ok = hb_store:reset(LocalStore),
                ok = hb_store:start(LocalStore),
                Port = random_port(),
                Opts =
                    #{
                        store => [LocalStore],
                        priv_wallet => Wallet,
                        mysticeti_author => Author,
                        mysticeti_registry_namespace => Author,
                        cache_writers => Validators,
                        port => Port,
                        host => <<"localhost">>,
                        gateway => <<"http://localhost:1">>,
                        http_connect_timeout => 2000,
                        http_request_send_timeout => 2000
                    },
                {Url0, FinalPort} = start_node_with_retry(Opts, 10),
                Url = trim_trailing_slash(Url0),
                FinalOpts = Opts#{ port => FinalPort },
                #{ url => Url, addr => Author, opts => FinalOpts, index => Index }
            end,
            lists:zip3(Wallets, Validators, lists:seq(1, NodeCount))
        ),
    {Nodes, Validators}.

random_port() ->
    20000 + rand:uniform(40000).

start_node_with_retry(_Opts, 0) ->
    erlang:error({start_node_failed, retries_exhausted});
start_node_with_retry(Opts, Attempts) ->
    Port = hb_opts:get(port, undefined, Opts),
    case catch hb_http_server:start_node(Opts) of
        Url when is_binary(Url) ->
            {trim_trailing_slash(Url), Port};
        {'EXIT', Reason} ->
            case contains_eaddrinuse(Reason) of
                true ->
                    start_node_with_retry(Opts#{ port => random_port() }, Attempts - 1);
                false ->
                    erlang:error({start_node_failed, Reason})
            end
    end.

contains_eaddrinuse(eaddrinuse) -> true;
contains_eaddrinuse(Term) when is_tuple(Term) ->
    contains_eaddrinuse(tuple_to_list(Term));
contains_eaddrinuse(Term) when is_list(Term) ->
    lists:any(fun contains_eaddrinuse/1, Term);
contains_eaddrinuse(_Term) ->
    false.

%% @doc Trim a trailing slash from a URL.
trim_trailing_slash(<<>>) -> <<>>;
trim_trailing_slash(Url) when is_binary(Url) ->
    case binary:last(Url) of
        $/ -> binary:part(Url, 0, byte_size(Url) - 1);
        _ -> Url
    end.

%% @doc Return HTTP opts with connect and send timeouts.
http_opts(Opts) ->
    Opts#{
        http_connect_timeout => 2000,
        http_request_send_timeout => 10000
    }.

%% @doc Wait until all nodes respond to scheduler status.
wait_for_nodes_ready(Nodes, Timeout) ->
    _Ready =
        hb_util:wait_until(
            fun() ->
                missing_ready_nodes(Nodes) == []
            end,
            Timeout
        ),
    case missing_ready_nodes(Nodes) of
        [] -> true;
        Missing -> {error, Missing}
    end.

%% @doc Return any nodes that do not report ready status.
missing_ready_nodes(Nodes) ->
    lists:foldl(
        fun(#{ url := Node, opts := Opts }, Acc) ->
            ReqOpts = http_opts(Opts),
            case catch hb_http:get(Node, <<"/~scheduler@1.0/status">>, ReqOpts) of
                {ok, Res} ->
                    Status = hb_maps:get(<<"status">>, Res, 200, ReqOpts),
                    case Status < 400 of
                        true -> Acc;
                        false -> [{Node, {status, Status}} | Acc]
                    end;
                Error ->
                    [{Node, {request_error, Error}} | Acc]
            end
        end,
        [],
        Nodes
    ).

%% @doc Register the local scheduler location via HTTP.
register_scheduler_location(Node, Opts) ->
    Req0 = #{
        <<"path">> => <<"/~scheduler@1.0/location">>,
        <<"method">> => <<"POST">>
    },
    Req = hb_message:commit(Req0, Opts),
    ReqOpts = http_opts(Opts),
    case hb_http:post(Node, Req, ReqOpts) of
        {ok, Response} ->
            case scheduler_location_from_response(Response, Opts) of
                {ok, Location} ->
                    {ok, hb_cache:ensure_all_loaded(Location, Opts)};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% @doc Post a scheduler-location record to a peer node.
post_scheduler_location(Node, Location, SenderOpts, Timeout) ->
    Deadline = erlang:system_time(millisecond) + Timeout,
    post_scheduler_location(Node, Location, SenderOpts, Deadline, none).

%% @doc Retry posting a scheduler-location until success or timeout.
post_scheduler_location(Node, Location, SenderOpts, Deadline, LastError) ->
    ReqOpts = http_opts(SenderOpts),
    Attempt =
        case catch hb_http:post(
            Node,
            <<"/~scheduler@1.0/location">>,
            Location,
            ReqOpts
        ) of
            {ok, Res} ->
                Status = hb_maps:get(<<"status">>, Res, 200, ReqOpts),
                case Status < 400 of
                    true -> {ok, Res};
                    false -> {error, {status, Status, Res}}
                end;
            Error ->
                {error, {request_error, Error}}
        end,
    case Attempt of
        {ok, _} = Ok ->
            Ok;
        {error, Reason} ->
            Now = erlang:system_time(millisecond),
            case Now >= Deadline of
                true -> {error, {timeout, Reason, LastError}};
                false ->
                    timer:sleep(100),
                    post_scheduler_location(
                        Node,
                        Location,
                        SenderOpts,
                        Deadline,
                        Reason
                    )
            end
    end.

%% @doc Wait until all nodes have locations for all addresses.
wait_for_scheduler_locations(Nodes, Addresses, Timeout) ->
    _Ready =
        hb_util:wait_until(
            fun() ->
                missing_scheduler_locations(Nodes, Addresses) == []
            end,
            Timeout
        ),
    case missing_scheduler_locations(Nodes, Addresses) of
        [] -> true;
        Missing -> {error, Missing}
    end.

%% @doc Return any missing scheduler locations across nodes.
missing_scheduler_locations(Nodes, Addresses) ->
    lists:foldl(
        fun(#{ url := Node, opts := Opts }, Acc0) ->
            lists:foldl(
                fun(Address, Acc1) ->
                    ReqOpts =
                        http_opts(Opts),
                    case catch hb_http:get(
                        Node,
                        <<"/~scheduler@1.0/location?address=", Address/binary>>,
                        ReqOpts
                    ) of
                        {ok, Response} ->
                            Status = hb_maps:get(<<"status">>, Response, 200, ReqOpts),
                            case Status >= 400 of
                                true ->
                                    [{Node, Address, {status, Status}} | Acc1];
                                false ->
                                    case scheduler_location_from_response(Response, ReqOpts) of
                                        {ok, _} -> Acc1;
                                        {error, Reason} ->
                                            [{Node, Address, {invalid, Reason}} | Acc1]
                                    end
                            end;
                        Error ->
                            [{Node, Address, {request_error, Error}} | Acc1]
                    end
                end,
                Acc0,
                Addresses
            )
        end,
        [],
        Nodes
    ).

%% @doc Extract a scheduler-location from a response message.
scheduler_location_from_response(Response, Opts) ->
    case scheduler_location_candidate(Response, Opts) of
        {ok, Location0} ->
            case hb_message:with_only_committed(Location0, Opts) of
                {ok, Location} -> {ok, Location};
                {error, _} -> {ok, Location0}
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Find a scheduler-location candidate in a response.
scheduler_location_candidate(Response, Opts) ->
    case direct_scheduler_location(Response, Opts) of
        {ok, _} = Ok -> Ok;
        {error, _} ->
            decode_scheduler_location(Response, Opts)
    end.

%% @doc Decode a structured scheduler-location response if needed.
decode_scheduler_location(Response, Opts) ->
    try hb_message:convert(Response, <<"structured@1.0">>, <<"httpsig@1.0">>, Opts) of
        Decoded ->
            case direct_scheduler_location(Decoded, Opts) of
                {ok, _} = Ok -> Ok;
                {error, _} -> {error, {invalid_scheduler_location_response, Response}}
            end
    catch
        _:_ ->
            {error, {invalid_scheduler_location_response, Response}}
    end.

%% @doc Extract a scheduler-location when it is already embedded.
direct_scheduler_location(Response, Opts) ->
    case hb_maps:get(<<"type">>, Response, undefined, Opts) of
        <<"scheduler-location">> ->
            {ok, Response};
        _ ->
            case hb_maps:get(<<"body">>, Response, not_found, Opts) of
                Body when is_map(Body) ->
                    case hb_maps:get(<<"type">>, Body, undefined, Opts) of
                        <<"scheduler-location">> -> {ok, Body};
                        _ -> {error, not_found}
                    end;
                _ ->
                    {error, not_found}
            end
    end.
