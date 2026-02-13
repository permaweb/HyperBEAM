%%% @doc A wrapper library for gun. This module originates from the Arweave
%%% project, and has been modified for use in HyperBEAM.
-module(hb_http_client).
-behaviour(gen_server).
-include("include/hb.hrl").
-export([start_link/1, response_status_to_atom/1, request/2]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2]).
-export([ok/0]).

-record(state, {
	opts = #{}
}).

-define(DEFAULT_RETRIES, 0).
-define(DEFAULT_RETRY_TIME, 1000).
-define(DEFAULT_KEEPALIVE_TIMEOUT, 60_000).
-define(DEFAULT_CONNECT_TIMEOUT, 60_000).
-define(DEFAULT_429_BACKOFF_MS, 5000).
-define(RATE_LIMIT_ETS, hb_http_client_rate_limits).
-define(CONNECTIONS_ETS, hb_http_client_connections).
-define(CONN_STATUS_ETS, hb_http_client_conn_status).
-define(CONN_COUNTER_ETS, hb_http_client_conn_counter).

%% Connection pool sizes per type (easily configurable)
-define(READ_POOL_SIZE, 20).
-define(WRITE_POOL_SIZE, 10).

%%% ==================================================================
%%% Public interface.
%%% ==================================================================

ok() ->
    gen_server:call(?MODULE, ok).

start_link(Opts) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Convert a HTTP status code to a status atom.
response_status_to_atom(Status) ->
    case Status of
        201 -> created;
        X when X < 400 -> ok;
        X when X < 500 -> error;
        _ -> failure
    end.

request(Args, Opts) ->
    request(Args, hb_opts:get(http_retry, ?DEFAULT_RETRIES, Opts), Opts).
request(Args, RemainingRetries, Opts) ->
    Response = do_request(Args, Opts),
    case Response of
        {error, _Details} -> maybe_retry(RemainingRetries, Args, Response, Opts);
        {ok, Status, _Headers, _Body} ->
            StatusAtom = response_status_to_atom(Status),
            RetryResponses = hb_opts:get(http_retry_response, [], Opts),
            case lists:member(StatusAtom, RetryResponses) of
                true -> maybe_retry(RemainingRetries, Args, Response, Opts);
                false -> Response
            end
    end.

do_request(Args, Opts) ->
    case hb_opts:get(http_client, gun, Opts) of
        gun -> gun_req(Args, Opts);
        httpc -> httpc_req(Args, Opts)
    end.

maybe_retry(0, _, OriginalResponse, _) -> OriginalResponse;
maybe_retry(Remaining, Args, OriginalResponse, Opts) ->
    RetryBaseTime = hb_opts:get(http_retry_time, ?DEFAULT_RETRY_TIME, Opts),
    RetryTime =
        case hb_opts:get(http_retry_mode, backoff, Opts) of
            constant -> RetryBaseTime;
            backoff ->
                BaseRetries = hb_opts:get(http_retry, ?DEFAULT_RETRIES, Opts),
                RetryBaseTime * (1 + (BaseRetries - Remaining))
        end,
    ErrDetails = case OriginalResponse of
        {error, Details} -> Details;
        {ok, Status, _, _} -> Status
    end,
    ?event(
        warning,
        {retrying_http_request,
            {after_ms, RetryTime},
            {error, ErrDetails},
            {request, Args}
        }
    ),
    timer:sleep(RetryTime),
    request(Args, Remaining - 1, Opts).

httpc_req(Args, Opts) ->
    #{
        peer := Peer,
        path := Path,
        method := RawMethod,
        headers := Headers,
        body := Body
    } = Args,
    ?event({httpc_req, Args}),
    {Host, Port} = parse_peer(Peer, Opts),
    Scheme = case Port of
        443 -> "https";
        _ -> "http"
    end,
    ?event(http_client, {httpc_req, {explicit, Args}}),
    URL = binary_to_list(iolist_to_binary([Scheme, "://", Host, ":", integer_to_binary(Port), Path])),
    FilteredHeaders = hb_maps:without([<<"content-type">>, <<"cookie">>], Headers, Opts),
    HeaderKV =
        [
            {binary_to_list(Key), binary_to_list(Value)}
        ||
            {Key, Value} <- hb_maps:to_list(FilteredHeaders, Opts)
        ] ++
        [
            {<<"cookie">>, CookieLine}
        ||
            CookieLine <-
                case hb_maps:get(<<"cookie">>, Headers, [], Opts) of
                    Binary when is_binary(Binary) ->
                        [Binary];
                    List when is_list(List) ->
                        List
                end
        ],
    Method = binary_to_existing_atom(hb_util:to_lower(RawMethod)),
    ContentType = hb_maps:get(<<"content-type">>, Headers, <<"application/octet-stream">>, Opts),
    Request =
        case Method of
            get ->
                {
                    URL,
                    HeaderKV
                };
            _ ->
                upload_metric(Body),
                {
                    URL,
                    HeaderKV,
                    binary_to_list(ContentType),
                    Body
                }
        end,
    ?event({http_client_outbound, Method, URL, Request}),
    HTTPCOpts = [{full_result, true}, {body_format, binary}],
	StartTime = os:system_time(native),
    case httpc:request(Method, Request, [], HTTPCOpts) of
        {ok, {{_, Status, _}, RawRespHeaders, RespBody}} ->
            download_metric(RespBody),
	        EndTime = os:system_time(native),
            RespHeaders =
                [
                    {list_to_binary(Key), list_to_binary(Value)}
                ||
                    {Key, Value} <- RawRespHeaders
                ],
            ?event(http_client, {httpc_resp, Status, RespHeaders, RespBody}),
            record_duration(#{
                    <<"request-method">> => method_to_bin(Method),
                    <<"request-path">> => hb_util:bin(Path),
                    <<"status-class">> => get_status_class(Status),
                    <<"duration">> => EndTime - StartTime
                },
                Opts
            ),
            {ok, Status, RespHeaders, RespBody};
        {error, Reason} ->
            ?event(http_client, {httpc_error, Reason}),
            {error, Reason}
    end.

gun_req(Args, Opts) ->
    gun_req(Args, false, Opts).
gun_req(Args, ReestablishedConnection, Opts) ->
	StartTime = os:system_time(native),
	#{ peer := Peer, path := Path, method := Method } = Args,
	ConnType = get_connection_type(Method),
	Response =
        case get_connection(Peer, ConnType, Args, Opts) of
            {ok, PID} ->
                ar_rate_limiter:throttle(Peer, Path, Opts),
                case do_gun_request(PID, Args, Opts) of
                    {error, Error} when Error == {shutdown, normal};
                            Error == noproc ->
                        case ReestablishedConnection of
                            true -> {error, client_error};
                            false -> gun_req(Args, true, Opts)
                        end;
                    Reply ->
                        Reply
                end;
            {'EXIT', _} ->
                {error, client_error};
            Error ->
                ?event(http_client, {gun_error, Error}),
                Error
	    end,
	EndTime = os:system_time(native),
	%% Only log the metric for the top-level call to req/2 - not the recursive call
	%% that happens when the connection is reestablished.
	case ReestablishedConnection of
		true ->
			ok;
		false ->
            record_duration(#{
                    <<"request-method">> => method_to_bin(Method),
                    <<"request-path">> => hb_util:bin(Path),
                    <<"status-class">> => get_status_class(Response),
                    <<"duration">> => EndTime - StartTime
                },
                Opts
            )
	end,
	Response.

%% @doc Determine the connection type based on the HTTP method.
%% Read operations (GET, HEAD) use the 'read' connection.
%% Write operations (POST, PUT, DELETE, etc.) use the 'write' connection.
get_connection_type(<<"GET">>) -> read;
get_connection_type(<<"get">>) -> read;
get_connection_type(<<"HEAD">>) -> read;
get_connection_type(<<"head">>) -> read;
get_connection_type(get) -> read;
get_connection_type(head) -> read;
get_connection_type(_) -> write.

%% @doc Get the pool size for a connection type.
get_pool_size(read) -> ?READ_POOL_SIZE;
get_pool_size(write) -> ?WRITE_POOL_SIZE.

%% @doc Get the next connection index using round-robin selection.
%% Uses ets:update_counter for atomic increment.
get_next_conn_index(Peer, ConnType) ->
    PoolSize = get_pool_size(ConnType),
    CounterKey = {Peer, ConnType},
    %% Atomically increment and wrap around using update_counter
    %% If key doesn't exist, it will be created with default 0
    try
        Index = ets:update_counter(?CONN_COUNTER_ETS, CounterKey, {2, 1, PoolSize, 1}),
        Index
    catch
        error:badarg ->
            %% Key doesn't exist, initialize it
            ets:insert_new(?CONN_COUNTER_ETS, {CounterKey, 1}),
            1
    end.

%% @doc Get a connection for a peer+type, using ETS for fast lookup.
%% If no connection exists, it will be created via the gen_server.
%% Uses round-robin to distribute requests across the connection pool.
get_connection(Peer, ConnType, Args, Opts) ->
    PoolSize = get_pool_size(ConnType),
    ConnIndex = get_next_conn_index(Peer, ConnType),
    ConnKey = {Peer, ConnType, ConnIndex},
    get_connection_by_key(ConnKey, PoolSize, Args, Opts, 0).

%% @doc Try to get a connection by key, with fallback to other pool connections.
get_connection_by_key(ConnKey, PoolSize, Args, Opts, Attempts) when Attempts < PoolSize ->
    case ets:lookup(?CONNECTIONS_ETS, ConnKey) of
        [{ConnKey, PID}] ->
            %% Found a connection, check if it's still alive and connected
            case ets:lookup(?CONN_STATUS_ETS, PID) of
                [{PID, connected, _MonitorRef, _ConnKey}] ->
                    {ok, PID};
                [{PID, {connecting, _}, _MonitorRef, _ConnKey}] ->
                    %% Connection is being established, wait for it via gen_server
                    catch gen_server:call(?MODULE, {get_connection, ConnKey, Args, Opts}, infinity);
                [] ->
                    %% Status not found, connection might be dead, create new one
                    catch gen_server:call(?MODULE, {get_connection, ConnKey, Args, Opts}, infinity)
            end;
        [] ->
            %% No connection, create one via gen_server
            catch gen_server:call(?MODULE, {get_connection, ConnKey, Args, Opts}, infinity)
    end;
get_connection_by_key(_ConnKey, _PoolSize, _Args, _Opts, _Attempts) ->
    {error, no_available_connection}.

%% @doc Record the duration of the request in an async process. We write the 
%% data to prometheus if the application is enabled, as well as invoking the
%% `http_monitor' if appropriate.
record_duration(Details, Opts) ->
    spawn(
        fun() ->
            % First, write to prometheus if it is enabled. Prometheus works
            % only with strings as lists, so we encode the data before granting
            % it.
            GetFormat = fun 
                            (<<"request-category">>) ->
                                case maps:get(<<"request-path">>, Details) of
                                    %% TODO: Make it configurable for S3 bucket defined
                                    <<"/hb-s3", _/binary>> -> <<"S3">>;
                                    <<"/hyperbeam", _/binary>> -> <<"S3">>;
                                    <<"/graphql">> -> <<"GraphQL">>;
                                    <<"/raw", _/binary>> -> <<"RAW">>;
                                    <<"/tx", _/binary>> -> <<"TX">>;
                                    <<"/chunk", _/binary>> -> <<"Chunk">>;
                                    <<"/block/height/", _/binary>> -> <<"Block Height">>;
                                    _Path -> 
                                        <<"unknown">>
                                end;
                            (Key) -> 
                                hb_util:list(maps:get(Key, Details)) 
                        end,
            case application:get_application(prometheus) of
                undefined -> ok;
                _ ->
                    prometheus_histogram:observe(
                        http_request_duration_seconds,
                        lists:map(
                            GetFormat,
                            [
                                <<"request-method">>,
                                <<"status-class">>,
                                <<"request-category">>
                            ]
                        ),
                        maps:get(<<"duration">>, Details)
                    )
            end,
            maybe_invoke_monitor(
                Details#{ <<"path">> => <<"duration">> },
                Opts
            )
        end
    ).

%% @doc Invoke the HTTP monitor message with AO-Core, if it is set in the 
%% node message key. We invoke the given message with the `body' set to a signed
%% version of the details. This allows node operators to configure their machine
%% to record duration statistics into customized data stores, computations, or
%% processes etc. Additionally, we include the `http_reference' value, if set in
%% the given `opts'.
%% 
%% We use `hb_ao:get' rather than `hb_opts:get', as settings configured
%% by the `~router@1.0' route `opts' key are unable to generate atoms.
maybe_invoke_monitor(Details, Opts) ->
    case hb_ao:get(<<"http_monitor">>, Opts, Opts) of
        not_found -> ok;
        Monitor ->
            % We have a monitor message. Place the `details' into the body, set
            % the `method' to "POST", add the `http_reference' (if applicable)
            % and sign the request. We use the node message's wallet as the
            % source of the key.
            MaybeWithReference =
                case hb_ao:get(<<"http_reference">>, Opts, Opts) of
                    not_found -> Details;
                    Ref -> Details#{ <<"reference">> => Ref }
                end,
            Req =
                Monitor#{
                    <<"body">> =>
                        hb_message:commit(
                            MaybeWithReference#{
                                <<"method">> => <<"POST">>
                            },
                            Opts
                        )
                },
            % Use the singleton parse to generate the message sequence to 
            % execute.
            ReqMsgs = hb_singleton:from(Req, Opts),
            Res = hb_ao:resolve_many(ReqMsgs, Opts),
            ?event(http_monitor, {resolved_monitor, Res})
    end.

%%% ==================================================================
%%% gen_server callbacks.
%%% ==================================================================

init(Opts) ->
    init_ets_tables(),
    case hb_opts:get(prometheus, not hb_features:test(), Opts) of
        true ->
            ?event({starting_prometheus_application,
                    {test_mode, hb_features:test()}
                }
            ),
            try
                application:ensure_all_started([prometheus, prometheus_cowboy]),
                init_prometheus(),
	            {ok, #state{ opts = Opts }}
            catch
                Type:Reason:Stack ->
                    ?event(warning,
                        {prometheus_not_started,
                            {type, Type},
                            {reason, Reason},
                            {stack, Stack}
                        }
                    ),
                    {ok, #state{ opts = Opts }}
            end;
        false -> {ok, #state{ opts = Opts }}
    end.

init_ets_tables() ->
    init_ets_table(?RATE_LIMIT_ETS),
    init_ets_table(?CONNECTIONS_ETS),
    init_ets_table(?CONN_STATUS_ETS),
    init_counter_ets_table(?CONN_COUNTER_ETS).

init_ets_table(Table) ->
    case ets:whereis(Table) of
        undefined ->
            ets:new(Table, [
                named_table,
                public,
                set,
                {read_concurrency, true},
                {write_concurrency, true}
            ]);
        _ ->
            ok
    end.

init_counter_ets_table(Table) ->
    case ets:whereis(Table) of
        undefined ->
            ets:new(Table, [
                named_table,
                public,
                set,
                {write_concurrency, true}
            ]);
        _ ->
            ok
    end.

init_prometheus() ->
    application:ensure_all_started([prometheus, prometheus_cowboy]),
	prometheus_counter:new([
		{name, gun_requests_total},
		{labels, [http_method, status_class]},
		{
			help,
			"The total number of GUN requests."
		}
	]),
	prometheus_gauge:new([{name, outbound_connections},
		{help, "The current number of the open outbound network connections"}]),
	prometheus_histogram:new([
		{name, http_request_duration_seconds},
		{buckets, [0.01, 0.1, 0.5, 1, 5, 10, 30, 60]},
        {labels, [http_method, status_class, category]},
		{
			help,
			"The total duration of an hb_http_client:req call. This includes more than"
            " just the GUN request itself (e.g. establishing a connection, "
            "throttling, etc...)"
		}
	]),
	prometheus_histogram:new([
		{name, http_client_get_chunk_duration_seconds},
		{buckets, [0.1, 1, 10, 60]},
        {labels, [status_class, peer]},
		{
			help,
			"The total duration of an HTTP GET chunk request made to a peer."
		}
	]),
	prometheus_counter:new([
		{name, http_client_downloaded_bytes_total},
		{help, "The total amount of bytes requested via HTTP, per remote endpoint"}
	]),
	prometheus_counter:new([
		{name, http_client_uploaded_bytes_total},
		{help, "The total amount of bytes posted via HTTP, per remote endpoint"}
	]),
    ?event(started),
    ok.

handle_call(ok, _From, State) ->
    {reply, ok, State};
handle_call({get_connection, ConnKey, Args, _Opts}, From, State) ->
    %% ConnKey = {Peer, ConnType, Index} where ConnType is 'read' or 'write'
    %% and Index is 1..PoolSize for round-robin distribution
    %% Double-check ETS to handle race conditions
    case ets:lookup(?CONNECTIONS_ETS, ConnKey) of
        [{ConnKey, PID}] ->
            %% Connection exists, check status
            case ets:lookup(?CONN_STATUS_ETS, PID) of
                [{PID, connected, _MonitorRef, _ConnKey}] ->
                    {reply, {ok, PID}, State};
                [{PID, {connecting, PendingRequests}, MonitorRef, ConnKey}] ->
                    %% Add to pending requests list
                    ets:insert(?CONN_STATUS_ETS, {PID, {connecting, [{From, Args} | PendingRequests]}, MonitorRef, ConnKey}),
                    {noreply, State};
                [] ->
                    %% Status not found, PID is stale - remove and create new
                    ets:delete(?CONNECTIONS_ETS, ConnKey),
                    create_new_connection(ConnKey, Args, From, State)
            end;
        [] ->
            %% No connection exists, create one
            create_new_connection(ConnKey, Args, From, State)
    end;

handle_call(Request, _From, State) ->
	?event(warning, {unhandled_call, {module, ?MODULE}, {request, Request}}),
	{reply, ok, State}.

%% @doc Create a new connection and store it in ETS.
create_new_connection(ConnKey, Args, From, State) ->
    MergedOpts = hb_maps:merge(State#state.opts, hb_maps:get(opts, Args, #{}), #{}),
    {ok, PID} = open_connection(Args, MergedOpts),
    MonitorRef = monitor(process, PID),
    %% Store connection in ETS
    ets:insert(?CONNECTIONS_ETS, {ConnKey, PID}),
    %% Store status with monitor ref and conn key
    ets:insert(?CONN_STATUS_ETS, {PID, {connecting, [{From, Args}]}, MonitorRef, ConnKey}),
    {reply, {ok, PID}, State}.

handle_cast(Cast, State) ->
	?event(warning, {unhandled_cast, {module, ?MODULE}, {cast, Cast}}),
	{noreply, State}.

handle_info({gun_up, PID, Protocol}, State) ->
	case ets:lookup(?CONN_STATUS_ETS, PID) of
		[] ->
			%% A connection timeout should have occurred.
			{noreply, State};
		[{PID, {connecting, PendingRequests}, MonitorRef, ConnKey}] ->
            ?event(http_client, {gun_up, {protocol, Protocol}, {conn_key, ConnKey}}),
			[gen_server:reply(ReplyTo, {ok, PID}) || {ReplyTo, _} <- PendingRequests],
			ets:insert(?CONN_STATUS_ETS, {PID, connected, MonitorRef, ConnKey}),
			inc_prometheus_gauge(outbound_connections),
			{noreply, State};
		[{PID, connected, _MonitorRef, ConnKey}] ->
			?event(warning,
                {gun_up_pid_already_exists, {conn_key, ConnKey}}),
			{noreply, State}
	end;

handle_info({gun_error, PID, Reason}, State) ->
	case ets:lookup(?CONN_STATUS_ETS, PID) of
		[] ->
			?event(warning, {gun_connection_error_with_unknown_pid}),
			{noreply, State};
		[{PID, Status, MonitorRef, ConnKey}] ->
			ets:delete(?CONNECTIONS_ETS, ConnKey),
			ets:delete(?CONN_STATUS_ETS, PID),
			demonitor(MonitorRef, [flush]),
			Reason2 =
				case Reason of
					timeout ->
						connect_timeout;
					{Type, _} ->
						Type;
					_ ->
						Reason
				end,
			case Status of
				{connecting, PendingRequests} ->
					reply_error(PendingRequests, Reason2);
				connected ->
					dec_prometheus_gauge(outbound_connections),
					ok
			end,
			gun:shutdown(PID),
            ?event(http_outbound, {gun_shutdown, {conn_key, ConnKey}}),
			?event(http_client, {connection_error, {conn_key, ConnKey}, {reason, Reason}}),
			{noreply, State}
	end;

handle_info({gun_down, PID, Protocol, Reason, KilledStreams}, State) ->
    ?event(http_client, 
        {gun_down, 
            {protocol, Protocol}, 
            {reason, Reason}, 
            {killed_streams, KilledStreams}}),
	case ets:lookup(?CONN_STATUS_ETS, PID) of
		[] ->
			?event(warning,
                {gun_connection_down_with_unknown_pid, {protocol, Protocol}}),
			{noreply, State};
		[{PID, Status, MonitorRef, ConnKey}] ->
			ets:delete(?CONNECTIONS_ETS, ConnKey),
			ets:delete(?CONN_STATUS_ETS, PID),
			demonitor(MonitorRef, [flush]),
			Reason2 =
				case Reason of
					{Type, _} ->
						Type;
					_ ->
						Reason
				end,
			case Status of
				{connecting, PendingRequests} ->
					reply_error(PendingRequests, Reason2);
				_ ->
					dec_prometheus_gauge(outbound_connections),
					ok
			end,
			gun:shutdown(PID),
			?event(http_outbound, {gun_shutdown_after_down, {conn_key, ConnKey}}),
			{noreply, State}
	end;

handle_info({'DOWN', _Ref, process, PID, Reason}, State) ->
	case ets:lookup(?CONN_STATUS_ETS, PID) of
		[] ->
			{noreply, State};
		[{PID, Status, _MonitorRef, ConnKey}] ->
			ets:delete(?CONNECTIONS_ETS, ConnKey),
			ets:delete(?CONN_STATUS_ETS, PID),
			case Status of
				{connecting, PendingRequests} ->
					reply_error(PendingRequests, Reason);
				_ ->
					dec_prometheus_gauge(outbound_connections),
					ok
			end,
			{noreply, State}
	end;

handle_info({clear_rate_limit, Peer, Path}, State) ->
	clear_rate_limit(Peer, Path),
	{noreply, State};

handle_info(Message, State) ->
	?event(warning, {unhandled_info, {module, ?MODULE}, {message, Message}}),
	{noreply, State}.

terminate(Reason, _State) ->
	?event(info,{http_client_terminating, {reason, Reason}}),
	ets:foldl(
		fun({PID, _Status, _MonitorRef, _ConnKey}, Acc) ->
			gun:shutdown(PID),
			Acc
		end,
		ok,
		?CONN_STATUS_ETS
	),
	ok.

%%% ==================================================================
%%% Private functions.
%%% ==================================================================

%% @doc Safe wrapper for prometheus_gauge:inc/2.
inc_prometheus_gauge(Name) ->
    case application:get_application(prometheus) of
        undefined -> ok;
        _ ->
            try prometheus_gauge:inc(Name)
            catch _:_ ->
                init_prometheus(),
                prometheus_gauge:inc(Name)
            end
    end.

%% @doc Safe wrapper for prometheus_gauge:dec/2.
dec_prometheus_gauge(Name) ->
    case application:get_application(prometheus) of
        undefined -> ok;
        _ -> prometheus_gauge:dec(Name)
    end.

inc_prometheus_counter(Name, Labels, Value) ->
    case application:get_application(prometheus) of
        undefined -> ok;
        _ -> prometheus_counter:inc(Name, Labels, Value)
    end.

open_connection(#{ peer := Peer }, Opts) ->
    {Host, Port} = parse_peer(Peer, Opts),
    ?event(http_outbound, {parsed_peer, {peer, Peer}, {host, Host}, {port, Port}}),
    BaseGunOpts =
        #{
            http_opts =>
                #{
                    keepalive =>
                        hb_opts:get(
                            http_keepalive,
                            ?DEFAULT_KEEPALIVE_TIMEOUT,
                            Opts
                        )
                },
            retry => 3,
            connect_timeout =>
                hb_opts:get(
                    http_connect_timeout,
                    ?DEFAULT_CONNECT_TIMEOUT,
                    Opts
                )
        },
    Transport =
        case Port of
            443 -> tls;
            _ -> tcp
        end,
    DefaultProto =
        case hb_features:http3() of
            true -> http3;
            false -> http2
        end,
    % Fallback through earlier HTTP versions if the protocol is not supported.
    GunOpts =
        case Proto = hb_opts:get(protocol, DefaultProto, Opts) of
            http3 -> 
                BaseGunOpts#{protocols => [http3], transport => quic};
            http1 ->
                %% In some cases we might need HTTP1 for better reliability
                BaseGunOpts#{protocols => [http]};
            _ -> 
                BaseGunOpts
        end,
    ?event(http_outbound,
        {gun_open,
            {host, Host},
            {port, Port},
            {protocol, Proto},
            {transport, Transport}
        }
    ),
	gun:open(Host, Port, GunOpts).

parse_peer(Peer, Opts) ->
    Parsed = uri_string:parse(Peer),
    case Parsed of
        #{ host := Host, port := Port } ->
            {hb_util:list(Host), Port};
        URI = #{ host := Host } ->
            {
                hb_util:list(Host),
                case hb_maps:get(scheme, URI, undefined, Opts) of
                    <<"https">> -> 443;
                    _ -> hb_opts:get(port, 8734, Opts)
                end
            }
    end.

reply_error([], _Reason) ->
	ok;
reply_error([PendingRequest | PendingRequests], Reason) ->
	ReplyTo = element(1, PendingRequest),
	Args = element(2, PendingRequest),
	Method = hb_maps:get(method, Args),
	record_response_status(Method, {error, Reason}),
	gen_server:reply(ReplyTo, {error, Reason}),
	reply_error(PendingRequests, Reason).

record_response_status(Method, Response) ->
	inc_prometheus_counter(gun_requests_total,
        [
            hb_util:list(method_to_bin(Method)),
			hb_util:list(get_status_class(Response))
        ],
        1
    ).

method_to_bin(get) ->
	<<"GET">>;
method_to_bin(post) ->
	<<"POST">>;
method_to_bin(put) ->
	<<"PUT">>;
method_to_bin(head) ->
	<<"HEAD">>;
method_to_bin(delete) ->
	<<"DELETE">>;
method_to_bin(connect) ->
	<<"CONNECT">>;
method_to_bin(options) ->
	<<"OPTIONS">>;
method_to_bin(trace) ->
	<<"TRACE">>;
method_to_bin(patch) ->
	<<"PATCH">>;
method_to_bin(Method) when is_binary(Method) ->
    Method;
method_to_bin(_) ->
	<<"unknown">>.

do_gun_request(PID, Args, Opts) ->
	Peer = hb_maps:get(peer, Args, undefined, Opts),
	Path = hb_maps:get(path, Args, undefined, Opts),
	case check_rate_limit(Peer, Path) of
		ok ->
            % Temporary Opts here to avoid conflict with <<"store-module">> converted to store_module.
			do_gun_request_inner(PID, Args,             hb_opts:mimic_default_types(Opts, existing, Opts));
		{rate_limited, RetryAfterMs} ->
			?event(http_client, {rate_limited_fast_fail, {peer, Peer}, {path, Path}, {retry_after_ms, RetryAfterMs}}),
			{error, {rate_limited, RetryAfterMs}}
	end.

do_gun_request_inner(PID, Args, Opts) ->
	Timer =
        inet:start_timer(
            hb_opts:get(http_request_send_timeout, no_request_send_timeout, Opts)
        ),
	Method = hb_maps:get(method, Args, undefined, Opts),
	Path = hb_maps:get(path, Args, undefined, Opts),
    HeaderMap = hb_maps:get(headers, Args, #{}, Opts),
    % Normalize cookie header lines from the header map. We support both
    % lists of cookie lines and a single cookie line.
	HeadersWithoutCookie =
        hb_maps:to_list(
            hb_maps:without([<<"cookie">>], HeaderMap, Opts),
            Opts
        ),
    CookieLines =
        case hb_maps:get(<<"cookie">>, HeaderMap, [], Opts) of
            BinCookieLine when is_binary(BinCookieLine) -> [BinCookieLine];
            CookieLinesList -> CookieLinesList
        end,
    CookieHeaders = [ {<<"cookie">>, CookieLine} || CookieLine <- CookieLines ],
    Headers = HeadersWithoutCookie ++ CookieHeaders,
	Body = hb_maps:get(body, Args, <<>>, Opts),
    ?event(
        http_client,
        {gun_request,
            {method, Method},
            {path, Path},
            {headers, {explicit, Headers}},
            {body, {explicit, {body, Body}}}
        },
        Opts
    ),
	Ref = gun:request(PID, Method, Path, Headers, Body),
	ResponseArgs =
        #{
            pid => PID, 
            stream_ref => Ref,
			timer => Timer, 
            limit => hb_maps:get(limit, Args, infinity, Opts),
			counter => 0, 
            acc => [], 
            start => os:system_time(microsecond),
			is_peer_request => hb_maps:get(is_peer_request, Args, true, Opts)
        },
	Response = await_response(hb_maps:merge(Args, ResponseArgs, Opts), Opts),
	record_response_status(Method, Response),
	inet:stop_timer(Timer),
	Response.

await_response(Args, Opts) ->
	#{ pid := PID, stream_ref := Ref, timer := Timer, limit := Limit,
			counter := Counter, acc := Acc, method := Method, path := Path } = Args,
	case gun:await(PID, Ref, inet:timeout(Timer)) of
		{response, fin, 429, Headers} ->
			upload_metric(Args),
			Peer = hb_maps:get(peer, Args, undefined, Opts),
			handle_429_response(Peer, Path, Headers),
			?event(http, {gun_response, {status, 429}, {headers, Headers}, {body, none}}),
			{ok, 429, Headers, <<>>};
		{response, fin, Status, Headers} ->
			upload_metric(Args),
			?event(http, {gun_response, {status, Status}, {headers, Headers}, {body, none}}),
			{ok, Status, Headers, <<>>};
		{response, nofin, 429, Headers} ->
			Peer = hb_maps:get(peer, Args, undefined, Opts),
			handle_429_response(Peer, Path, Headers),
			await_response(Args#{ status => 429, headers => Headers }, Opts);
		{response, nofin, Status, Headers} ->
			await_response(Args#{ status => Status, headers => Headers }, Opts);
		{data, nofin, Data} ->
			case Limit of
				infinity ->
					await_response(Args#{ acc := [Acc | Data] }, Opts);
				Limit ->
					Counter2 = size(Data) + Counter,
					case Limit >= Counter2 of
						true ->
							await_response(
                                Args#{
                                    counter := Counter2,
                                    acc := [Acc | Data]
                                },
                                Opts
                            );
						false ->
							?event(error, {http_fetched_too_much_data, Args,
									<<"Fetched too much data">>, Opts}),
							{error, too_much_data}
					end
			end;
		{data, fin, Data} ->
			FinData = iolist_to_binary([Acc | Data]),
			download_metric(FinData),
			upload_metric(Args),
			{ok,
                hb_maps:get(status, Args, undefined, Opts),
                hb_maps:get(headers, Args, undefined, Opts),
                FinData
            };
		{error, timeout} = Response ->
            ?event(http_outbound, {gun_cancel, {path, Path}}),
			gun:cancel(PID, Ref),
			log(warn, gun_await_process_down, Args, Response, Opts),
			Response;
        {error,{connection_error,{stream_closed, Message}}} = Response ->
            ?event(http_outbound, {gun_cancel, {path, Path}, {message, Message}}),
            gun:cancel(PID, Ref),
            Response;
		{error, Reason} = Response when is_tuple(Reason) ->
			record_response_status(Method, Response),
			log(warn, gun_await_process_down, Args, Reason, Opts),
			Response;
		Response ->
			record_response_status(Method, Response),
			log(warn, gun_await_unknown, Args, Response, Opts),
			Response
	end.

log(Type, Event, #{method := Method, peer := Peer, path := Path}, Reason, Opts) ->
    ?event(
        http,
        {gun_log,
            {type, Type},
            {event, Event},
            {method, Method},
            {peer, Peer},
            {path, Path},
            {reason, Reason}
        },
        Opts
    ),
    ok.

download_metric(Data) ->
	inc_prometheus_counter(
		http_client_downloaded_bytes_total,
        [],
		byte_size(Data)
	).

upload_metric(Body) when is_binary(Body) ->
	inc_prometheus_counter(
		http_client_uploaded_bytes_total,
		[],
		byte_size(Body)
	);
upload_metric(#{method := <<"POST">>, body := Body}) ->
	inc_prometheus_counter(
		http_client_uploaded_bytes_total,
		[],
		byte_size(Body)
	);
upload_metric(#{method := post, body := Body}) ->
	inc_prometheus_counter(
		http_client_uploaded_bytes_total,
		[],
		byte_size(Body)
	);
upload_metric(_) ->
	ok.

% @doc Return the HTTP status class label for cowboy_requests_total and
% gun_requests_total metrics.
get_status_class({ok, {{Status, _}, _, _, _, _}}) ->
	get_status_class(Status);
get_status_class({ok, Status, _RespondeHeaders, _Body}) ->
    get_status_class(Status);
get_status_class({error, connection_closed}) ->
	<<"connection_closed">>;
get_status_class({error, connect_timeout}) ->
	<<"connect_timeout">>;
get_status_class({error, timeout}) ->
	<<"timeout">>;
get_status_class({error,{shutdown,timeout}}) ->
	<<"shutdown_timeout">>;
get_status_class({error, econnrefused}) ->
	<<"econnrefused">>;
get_status_class({error, {shutdown,econnrefused}}) ->
	<<"shutdown_econnrefused">>;
get_status_class({error, {shutdown,ehostunreach}}) ->
	<<"shutdown_ehostunreach">>;
get_status_class({error, {shutdown,normal}}) ->
	<<"shutdown_normal">>;
get_status_class({error, {closed,_}}) ->
	<<"closed">>;
get_status_class({error, noproc}) ->
	<<"noproc">>;
get_status_class({error, {rate_limited, _}}) ->
	<<"rate_limited_fast_fail">>;
get_status_class({error, {connection_error, {stream_closed, _Message}}}) -> 
    <<"stream_closed">>;
get_status_class({error, {stream_error, {stream_error, too_many_streams, _Message}}}) ->
    <<"too_many_streams">>;
get_status_class({error, {stream_error, {stream_error, refused_stream, _Message}}}) ->
    <<"refused_stream">>;
get_status_class({error, {stream_error, {goaway, no_error, _Message}}}) ->
    <<"goaway">>;
get_status_class({error, {stream_error, {closed, {error, einval}}}}) ->
    <<"closed_einval">>;
get_status_class(208) ->
	<<"already_processed">>;
get_status_class(404) ->
	<<"not_found">>;
get_status_class(429) ->
	<<"too_many_requests">>;
get_status_class(Data) when is_integer(Data), Data > 0 ->
	hb_util:bin(prometheus_http:status_class(Data));
get_status_class(Data) when is_binary(Data) ->
	case catch binary_to_integer(Data) of
		{_, _} ->
			<<"unknown">>;
		Status ->
			get_status_class(Status)
	end;
get_status_class(Data) when is_atom(Data) ->
	atom_to_binary(Data);
get_status_class(StatusClass) ->
    ?event(error, {unknown_status_class, {status_class, StatusClass}}),
	<<"unknown">>.

%% ==================================================================
%% Rate limiting (429) fail-fast functions
%% ==================================================================

%% @doc Check if the peer+path is currently rate-limited.
%% Returns `ok` if not rate-limited, or `{rate_limited, RemainingMs}` if it is.
check_rate_limit(Peer, Path) ->
	case ets:lookup(?RATE_LIMIT_ETS, {Peer, Path}) of
		[] ->
			ok;
		[{{Peer, Path}, ExpiresAt}] ->
			Now = erlang:system_time(millisecond),
			case ExpiresAt > Now of
				true ->
					{rate_limited, ExpiresAt - Now};
				false ->
					%% Entry expired, clean it up
					ets:delete(?RATE_LIMIT_ETS, {Peer, Path}),
					ok
			end
	end.

%% @doc Handle a 429 response by parsing Retry-After header and storing rate limit.
handle_429_response(Peer, Path, Headers) ->
	RetryAfterMs = parse_retry_after_header(Headers),
	set_rate_limit(Peer, Path, RetryAfterMs).

%% @doc Store the rate limit in ETS and schedule cleanup.
set_rate_limit(Peer, Path, RetryAfterMs) ->
	ExpiresAt = erlang:system_time(millisecond) + RetryAfterMs,
	ets:insert(?RATE_LIMIT_ETS, {{Peer, Path}, ExpiresAt}),
	?event(http_client, {rate_limit_set, {peer, Peer}, {path, Path}, {expires_at, ExpiresAt}}),
	%% Schedule cleanup after the backoff period
	erlang:send_after(RetryAfterMs, self(), {clear_rate_limit, Peer, Path}),
	ok.

%% @doc Parse the Retry-After header from response headers.
%% Returns the backoff time in milliseconds.
%% If no Retry-After header is present, returns the default backoff.
parse_retry_after_header(Headers) ->
	case find_header(<<"retry-after">>, Headers) of
		undefined ->
			?DEFAULT_429_BACKOFF_MS;
		Value ->
			parse_retry_after_value(Value)
	end.

%% @doc Find a header value by name (case-insensitive).
find_header(Name, Headers) ->
	LowerName = hb_util:to_lower(Name),
	case lists:search(
		fun({Key, _}) -> hb_util:to_lower(Key) == LowerName end,
		Headers
	) of
		{value, {_, Value}} -> Value;
		false -> undefined
	end.

%% @doc Parse Retry-After value which can be:
%% - An integer (delay in seconds)
%% - An HTTP-date (absolute time)
%% Returns delay in milliseconds.
parse_retry_after_value(Value) when is_binary(Value) ->
	case catch binary_to_integer(Value) of
		Seconds when is_integer(Seconds), Seconds > 0 ->
			Seconds * 1000;
		_ ->
			%% Could be an HTTP-date, but for simplicity use default
			?DEFAULT_429_BACKOFF_MS
	end;
parse_retry_after_value(Value) when is_list(Value) ->
	parse_retry_after_value(list_to_binary(Value));
parse_retry_after_value(_) ->
	?DEFAULT_429_BACKOFF_MS.

%% @doc Clear a rate limit entry from ETS.
clear_rate_limit(Peer, Path) ->
	ets:delete(?RATE_LIMIT_ETS, {Peer, Path}),
	?event(http_client, {rate_limit_cleared, {peer, Peer}, {path, Path}}),
	ok.