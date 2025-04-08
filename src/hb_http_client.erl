-module(hb_http_client).
-moduledoc """
A wrapper library for gun. This module originates from the Arweave
project, and has been modified for use in HyperBEAM.
""".
-behaviour(gen_server).
-include("include/hb.hrl").
-export([start_link/1, req/2]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2]).

-record(state, {
	pid_by_peer = #{},
	status_by_pid = #{},
	opts = #{}
}).

%%% ==================================================================
%%% Public interface.
%%% ==================================================================

start_link(Opts) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

req(Args, Opts) -> req(Args, false, Opts).
req(Args, ReestablishedConnection, Opts) ->
    case hb_opts:get(http_client, gun, Opts) of
        gun -> gun_req(Args, ReestablishedConnection, Opts);
        httpc -> httpc_req(Args, ReestablishedConnection, Opts)
    end.

httpc_req(Args, _, Opts) ->
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
    ?event(http, {httpc_req, Args}),
    URL = binary_to_list(iolist_to_binary([Scheme, "://", Host, ":", integer_to_binary(Port), Path])),
    FilteredHeaders = maps:remove(<<"content-type">>, Headers),
    HeaderKV =
        [ {binary_to_list(Key), binary_to_list(Value)} || {Key, Value} <- maps:to_list(FilteredHeaders) ],
    Method = binary_to_existing_atom(hb_util:to_lower(RawMethod)),
    ContentType = maps:get(<<"content-type">>, Headers, <<"application/octet-stream">>),
    Request =
        case Method of
            get ->
                {
                    URL,
                    HeaderKV
                };
            _ ->
                {
                    URL,
                    HeaderKV,
                    binary_to_list(ContentType),
                    Body
                }
        end,
    ?event(http, {httpc_req, Method, URL, Request}),
    HTTPCOpts = [{full_result, true}, {body_format, binary}],
    case httpc:request(Method, Request, [], HTTPCOpts) of
        {ok, {{_, Status, _}, RawRespHeaders, RespBody}} ->
            RespHeaders =
                [
                    {list_to_binary(Key), list_to_binary(Value)}
                ||
                    {Key, Value} <- RawRespHeaders
                ],
            ?event(http, {httpc_resp, Status, RespHeaders, RespBody}),
            {ok, Status, RespHeaders, RespBody};
        {error, Reason} ->
            ?event(http, {httpc_error, Reason}),
            {error, Reason}
    end.

gun_req(Args, ReestablishedConnection, Opts) ->
	StartTime = erlang:monotonic_time(),
	#{ peer := Peer, path := Path, method := Method } = Args,
	Response =
        case catch gen_server:call(?MODULE, {get_connection, Args, Opts}, infinity) of
            {ok, PID} ->
                ar_rate_limiter:throttle(Peer, Path, Opts),
                case request(PID, Args, Opts) of
                    {error, Error} when Error == {shutdown, normal};
                            Error == noproc ->
                        case ReestablishedConnection of
                            true ->
                                {error, client_error};
                            false ->
                                req(Args, true, Opts)
                        end;
                    Reply ->
                        Reply
                end;
            {'EXIT', _} ->
                {error, client_error};
            Error ->
                Error
	    end,
	EndTime = erlang:monotonic_time(),
	%% Only log the metric for the top-level call to req/2 - not the recursive call
	%% that happens when the connection is reestablished.
	case ReestablishedConnection of
		true ->
			ok;
		false ->
            case application:get_application(prometheus) of
                undefined -> ok;
                _ -> prometheus_histogram:observe(http_request_duration_seconds, [
                        method_to_list(Method),
                        Path,
                        get_status_class(Response)
                    ], EndTime - StartTime)
            end
	end,
	Response.
%%% ==================================================================
%%% gen_server callbacks.
%%% ==================================================================

init(Opts) ->
    case hb_opts:get(prometheus, not hb_features:test(), Opts) of
        true ->
            ?event({starting_prometheus_application,
                    {test_mode, hb_features:test()}
                }
            ),
            try
                application:ensure_all_started([prometheus, prometheus_cowboy]),
                init_prometheus(Opts)
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

init_prometheus(Opts) ->
	prometheus_counter:new([
		{name, gun_requests_total},
		{labels, [http_method, route, status_class]},
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
        {labels, [http_method, route, status_class]},
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
		{help, "The total amount of bytes requested via HTTP, per remote endpoint"},
		{labels, [route]}
	]),
	prometheus_counter:new([
		{name, http_client_uploaded_bytes_total},
		{help, "The total amount of bytes posted via HTTP, per remote endpoint"},
		{labels, [route]}
	]),
    ?event(started),
	{ok, #state{ opts = Opts }}.

handle_call({get_connection, Args, Opts}, From,
		#state{ pid_by_peer = PIDPeer, status_by_pid = StatusByPID } = State) ->
	Peer = maps:get(peer, Args),
	case maps:get(Peer, PIDPeer, not_found) of
		not_found ->
			{ok, PID} = open_connection(Args, maps:merge(State#state.opts, Opts)),
			MonitorRef = monitor(process, PID),
			PIDPeer2 = maps:put(Peer, PID, PIDPeer),
			StatusByPID2 =
                maps:put(
                    PID,
                    {{connecting, [{From, Args}]}, MonitorRef, Peer},
					StatusByPID
                ),
			{
                reply,
                {ok, PID},
                State#state{
                    pid_by_peer = PIDPeer2,
                    status_by_pid = StatusByPID2
                }
            };
		PID ->
			case maps:get(PID, StatusByPID) of
				{{connecting, PendingRequests}, MonitorRef, Peer} ->
					StatusByPID2 =
                        maps:put(PID,
                            {
                                {connecting, [{From, Args} | PendingRequests]},
                                MonitorRef,
                                Peer
                            },
                            StatusByPID
                        ),
					{noreply, State#state{ status_by_pid = StatusByPID2 }};
				{connected, _MonitorRef, Peer} ->
					{reply, {ok, PID}, State}
			end
	end;

handle_call(Request, _From, State) ->
	?event(warning, {unhandled_call, {module, ?MODULE}, {request, Request}}),
	{reply, ok, State}.

handle_cast(Cast, State) ->
	?event(warning, {unhandled_cast, {module, ?MODULE}, {cast, Cast}}),
	{noreply, State}.

handle_info({gun_up, PID, _Protocol}, #state{ status_by_pid = StatusByPID } = State) ->
	case maps:get(PID, StatusByPID, not_found) of
		not_found ->
			%% A connection timeout should have occurred.
			{noreply, State};
		{{connecting, PendingRequests}, MonitorRef, Peer} ->
			[gen_server:reply(ReplyTo, {ok, PID}) || {ReplyTo, _} <- PendingRequests],
			StatusByPID2 = maps:put(PID, {connected, MonitorRef, Peer}, StatusByPID),
			inc_prometheus_gauge(outbound_connections),
			{noreply, State#state{ status_by_pid = StatusByPID2 }};
		{connected, _MonitorRef, Peer} ->
			?event(warning,
                {gun_up_pid_already_exists, {peer, ar_util:format_peer(Peer)}}),
			{noreply, State}
	end;

handle_info({gun_error, PID, Reason},
		#state{ pid_by_peer = PIDByPeer, status_by_pid = StatusByPID } = State) ->
	case maps:get(PID, StatusByPID, not_found) of
		not_found ->
			?event(warning, {gun_connection_error_with_unknown_pid}),
			{noreply, State};
		{Status, _MonitorRef, Peer} ->
			PIDByPeer2 = maps:remove(Peer, PIDByPeer),
			StatusByPID2 = maps:remove(PID, StatusByPID),
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
			?event({connection_error, {reason, Reason}}),
			{noreply, State#state{ status_by_pid = StatusByPID2, pid_by_peer = PIDByPeer2 }}
	end;

handle_info({gun_down, PID, Protocol, Reason, _KilledStreams, _UnprocessedStreams},
			#state{ pid_by_peer = PIDByPeer, status_by_pid = StatusByPID } = State) ->
	case maps:get(PID, StatusByPID, not_found) of
		not_found ->
			?event(warning,
                {gun_connection_down_with_unknown_pid, {protocol, Protocol}}),
			{noreply, State};
		{Status, _MonitorRef, Peer} ->
			PIDByPeer2 = maps:remove(Peer, PIDByPeer),
			StatusByPID2 = maps:remove(PID, StatusByPID),
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
			{noreply,
                State#state{
                    status_by_pid = StatusByPID2,
                    pid_by_peer = PIDByPeer2
                }
            }
	end;

handle_info({'DOWN', _Ref, process, PID, Reason},
		#state{ pid_by_peer = PIDByPeer, status_by_pid = StatusByPID } = State) ->
	case maps:get(PID, StatusByPID, not_found) of
		not_found ->
			{noreply, State};
		{Status, _MonitorRef, Peer} ->
			PIDByPeer2 = maps:remove(Peer, PIDByPeer),
			StatusByPID2 = maps:remove(PID, StatusByPID),
			case Status of
				{connecting, PendingRequests} ->
					reply_error(PendingRequests, Reason);
				_ ->
					dec_prometheus_gauge(outbound_connections),
					ok
			end,
			{noreply,
                State#state{
                    status_by_pid = StatusByPID2,
                    pid_by_peer = PIDByPeer2
                }
            }
	end;

handle_info(Message, State) ->
	?event(warning, {unhandled_info, {module, ?MODULE}, {message, Message}}),
	{noreply, State}.

terminate(Reason, #state{ status_by_pid = StatusByPID }) ->
	?event(info,{http_client_terminating, {reason, Reason}}),
	maps:map(fun(PID, _Status) -> gun:shutdown(PID) end, StatusByPID),
	ok.

%%% ==================================================================
%%% Private functions.
%%% ==================================================================

-doc """
Safe wrapper for prometheus_gauge:inc/2.
""".
inc_prometheus_gauge(Name) ->
    case application:get_application(prometheus) of
        undefined -> ok;
        _ -> prometheus_gauge:inc(Name)
    end.

-doc """
Safe wrapper for prometheus_gauge:dec/2.
""".
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
	ConnectTimeout =
		hb_opts:get(http_connect_timeout, no_connect_timeout, Opts),
    BaseGunOpts =
        #{
            http_opts =>
                #{
                    keepalive =>
                        hb_opts:get(
                            http_keepalive,
                            no_keepalive_timeout,
                            Opts
                        )
                },
            retry => 0,
            connect_timeout => ConnectTimeout
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
            http3 -> BaseGunOpts#{protocols => [http3], transport => quic};
            _ -> BaseGunOpts
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
                case maps:get(scheme, URI, undefined) of
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
	Method = maps:get(method, Args),
	Path = maps:get(path, Args),
	record_response_status(Method, Path, {error, Reason}),
	gen_server:reply(ReplyTo, {error, Reason}),
	reply_error(PendingRequests, Reason).

record_response_status(Method, Path, Response) ->
	inc_prometheus_counter(gun_requests_total,
        [
            method_to_list(Method),
			Path,
			get_status_class(Response)
        ],
        1
    ).

method_to_list(get) ->
	"GET";
method_to_list(post) ->
	"POST";
method_to_list(put) ->
	"PUT";
method_to_list(head) ->
	"HEAD";
method_to_list(delete) ->
	"DELETE";
method_to_list(connect) ->
	"CONNECT";
method_to_list(options) ->
	"OPTIONS";
method_to_list(trace) ->
	"TRACE";
method_to_list(patch) ->
	"PATCH";
method_to_list(_) ->
	"unknown".

request(PID, Args, Opts) ->
	Timer =
        inet:start_timer(
            hb_opts:get(http_request_send_timeout, no_request_send_timeout, Opts)
        ),
	Method = maps:get(method, Args),
	Path = maps:get(path, Args),
	Headers = maps:get(headers, Args, []),
	Body = maps:get(body, Args, <<>>),
    ?event(http, {gun_request, {method, Method}, {path, Path}, {headers, Headers}, {body, Body}}),
	Ref = gun:request(PID, Method, Path, Headers, Body),
	ResponseArgs =
        #{
            pid => PID, stream_ref => Ref,
			timer => Timer, limit => maps:get(limit, Args, infinity),
			counter => 0, acc => [], start => os:system_time(microsecond),
			is_peer_request => maps:get(is_peer_request, Args, true)
        },
	Response = await_response(maps:merge(Args, ResponseArgs), Opts),
	record_response_status(Method, Path, Response),
	inet:stop_timer(Timer),
	Response.

await_response(Args, Opts) ->
	#{ pid := PID, stream_ref := Ref, timer := Timer, limit := Limit,
			counter := Counter, acc := Acc, method := Method, path := Path } = Args,
	case gun:await(PID, Ref, inet:timeout(Timer)) of
		{response, fin, Status, Headers} ->
			upload_metric(Args),
			?event(http, {gun_response, {status, Status}, {headers, Headers}, {body, none}}),
			{ok, Status, Headers, <<>>};
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
			download_metric(FinData, Args),
			upload_metric(Args),
			{ok,
                maps:get(status, Args),
                maps:get(headers, Args),
                FinData
            };
		{error, timeout} = Response ->
			record_response_status(Method, Path, Response),
			gun:cancel(PID, Ref),
			log(warn, gun_await_process_down, Args, Response, Opts),
			Response;
		{error, Reason} = Response when is_tuple(Reason) ->
			record_response_status(Method, Path, Response),
			log(warn, gun_await_process_down, Args, Reason, Opts),
			Response;
		Response ->
			record_response_status(Method, Path, Response),
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

download_metric(Data, #{path := Path}) ->
	inc_prometheus_counter(
		http_client_downloaded_bytes_total,
		[Path],
		byte_size(Data)
	).

upload_metric(#{method := post, path := Path, body := Body}) ->
	inc_prometheus_counter(
		http_client_uploaded_bytes_total,
		[Path],
		byte_size(Body)
	);
upload_metric(_) ->
	ok.

% @doc Return the HTTP status class label for cowboy_requests_total and
% gun_requests_total metrics.
get_status_class({ok, {{Status, _}, _, _, _, _}}) ->
	get_status_class(Status);
get_status_class({error, connection_closed}) ->
	"connection_closed";
get_status_class({error, connect_timeout}) ->
	"connect_timeout";
get_status_class({error, timeout}) ->
	"timeout";
get_status_class({error,{shutdown,timeout}}) ->
	"shutdown_timeout";
get_status_class({error, econnrefused}) ->
	"econnrefused";
get_status_class({error, {shutdown,econnrefused}}) ->
	"shutdown_econnrefused";
get_status_class({error, {shutdown,ehostunreach}}) ->
	"shutdown_ehostunreach";
get_status_class({error, {shutdown,normal}}) ->
	"shutdown_normal";
get_status_class({error, {closed,_}}) ->
	"closed";
get_status_class({error, noproc}) ->
	"noproc";
get_status_class(208) ->
	"already_processed";
get_status_class(Data) when is_integer(Data), Data > 0 ->
	prometheus_http:status_class(Data);
get_status_class(Data) when is_binary(Data) ->
	case catch binary_to_integer(Data) of
		{_, _} ->
			"unknown";
		Status ->
			get_status_class(Status)
	end;
get_status_class(Data) when is_atom(Data) ->
	atom_to_list(Data);
get_status_class(_) ->
	"unknown".