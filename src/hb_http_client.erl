%%% @doc A wrapper library for gun. This module originates from the Arweave
%%% project, and has been modified for use in HyperBEAM.
-module(hb_http_client).
-behaviour(gen_server).
-include("include/hb.hrl").
-include("include/hb_opts.hrl").
%% Public API
-export([request/2, init_prometheus/0, response_status_to_atom/1]).
%% GenServer
-export([start_link/1, init/1]).
-export([handle_cast/2, handle_call/3, handle_info/2, terminate/2]).

-record(state, {
    opts = #{},
    pid_by_peer = #{},
    peer_by_pid = #{},
    monitor_by_pid = #{}
}).

-define(DEFAULT_RETRIES, 0).
-define(DEFAULT_RETRY_TIME, 1000).
-define(DEFAULT_KEEPALIVE_TIMEOUT, 60_000).
-define(DEFAULT_CONNECT_TIMEOUT, 60_000).
-define(HTTP_CLIENT_POOL, hb_http_client_pool).

%%% ==================================================================
%%% Public interface.
%%% ==================================================================

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

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
    case hb_opts:get(http_client, ?DEFAULT_HTTP_CLIENT, Opts) of
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
    ?event(debug_http_client, {httpc_req, {explicit, Args}}),
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
            ?event(debug_http_client, {httpc_resp, Status, RespHeaders, RespBody}),
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
	StartTime = os:system_time(native),
    #{ path := Path, method := Method } = Args,
    Response =
        case
            catch poolboy:transaction(
                ?HTTP_CLIENT_POOL,
                fun(Worker) ->
                    gen_server:call(Worker, {request, Args, Opts}, infinity)
                end,
                infinity
            )
        of
            {'EXIT', _} ->
                {error, client_error};
            Reply ->
                Reply
        end,
	EndTime = os:system_time(native),
    record_duration(#{
            <<"request-method">> => method_to_bin(Method),
            <<"request-path">> => hb_util:bin(Path),
            <<"status-class">> => get_status_class(Response),
            <<"duration">> => EndTime - StartTime
        },
        Opts
    ),
	Response.

%% @doc Execute a full request cycle inside a poolboy worker: throttle, ensure
%% a gun connection, send the request, and retry once on a stale connection.
request_on_worker(Args, Opts, State) ->
    request_on_worker(Args, false, Opts, State).
request_on_worker(Args, ReestablishedConnection, Opts, State) ->
    #{ peer := Peer, path := Path } = Args,
    ar_rate_limiter:throttle(Peer, Path, Opts),
    case ensure_connection(Peer, Args, Opts, State) of
        {ok, PID, NextState} ->
            case
                do_gun_request(
                    PID,
                    Args,
                    hb_opts:mimic_default_types(Opts, existing, Opts)
                )
            of
                {error, Error} when Error == {shutdown, normal};
                        Error == noproc ->
                    ClearedState = remove_connection_by_pid(PID, NextState),
                    case ReestablishedConnection of
                        true -> {{error, client_error}, ClearedState};
                        false ->
                            request_on_worker(
                                Args,
                                true,
                                Opts,
                                ClearedState
                            )
                    end;
                Reply ->
                    {Reply, NextState}
            end;
        {error, Reason, NextState} ->
            {{error, Reason}, NextState}
    end.

%% @doc Return an existing live gun connection for Peer, or open a new one.
ensure_connection(Peer, Args, Opts, State = #state{pid_by_peer = PIDByPeer}) ->
    case maps:find(Peer, PIDByPeer) of
        {ok, PID} ->
            case is_process_alive(PID) of
                true ->
                    {ok, PID, State};
                false ->
                    State2 = remove_connection(Peer, PID, State),
                    open_and_track_connection(Peer, Args, Opts, State2)
            end;
        error ->
            open_and_track_connection(Peer, Args, Opts, State)
    end.

%% @doc Open a new gun connection to Peer, await h2 negotiation, and track it.
open_and_track_connection(Peer, Args, Opts, State) ->
    MergedOpts = hb_maps:merge(State#state.opts, hb_maps:get(opts, Args, #{}), #{}),
    case open_connection(Args, MergedOpts) of
        {ok, PID} ->
            MonitorRef = monitor(process, PID),
            case
                gun:await_up(
                    PID,
                    hb_opts:get(
                        http_connect_timeout,
                        ?DEFAULT_CONNECT_TIMEOUT,
                        Opts
                    )
                )
            of
                {ok, _Protocol} ->
                    {ok, PID, add_connection(Peer, PID, MonitorRef, State)};
                {error, Reason} ->
                    demonitor(MonitorRef, [flush]),
                    gun:shutdown(PID),
                    {error, normalize_connection_error(Reason), State}
            end;
        {error, Reason} ->
            {error, normalize_connection_error(Reason), State}
    end.

%% @doc Store a gun PID, its peer, and monitor ref in the worker state.
add_connection(
    Peer,
    PID,
    MonitorRef,
    State = #state{
        pid_by_peer = PIDByPeer,
        peer_by_pid = PeerByPID,
        monitor_by_pid = MonitorByPID
    }
) ->
    inc_prometheus_gauge(outbound_connections),
    State#state{
        pid_by_peer = maps:put(Peer, PID, PIDByPeer),
        peer_by_pid = maps:put(PID, Peer, PeerByPID),
        monitor_by_pid = maps:put(PID, MonitorRef, MonitorByPID)
    }.

%% @doc Remove a gun connection from the worker state and demonitor it.
remove_connection(
    Peer,
    PID,
    State = #state{
        pid_by_peer = PIDByPeer,
        peer_by_pid = PeerByPID,
        monitor_by_pid = MonitorByPID
    }
) ->
    PIDByPeer2 =
        case maps:get(Peer, PIDByPeer, undefined) of
            PID -> maps:remove(Peer, PIDByPeer);
            _ -> PIDByPeer
        end,
    case maps:find(PID, MonitorByPID) of
        {ok, MonitorRef} ->
            demonitor(MonitorRef, [flush]);
        error ->
            ok
    end,
    case maps:is_key(PID, PeerByPID) of
        true -> dec_prometheus_gauge(outbound_connections);
        false -> ok
    end,
    State#state{
        pid_by_peer = PIDByPeer2,
        peer_by_pid = maps:remove(PID, PeerByPID),
        monitor_by_pid = maps:remove(PID, MonitorByPID)
    }.

%% @doc Remove a gun connection by PID (reverse-lookup the peer first).
remove_connection_by_pid(PID, State = #state{peer_by_pid = PeerByPID}) ->
    case maps:find(PID, PeerByPID) of
        {ok, Peer} ->
            remove_connection(Peer, PID, State);
        error ->
            State
    end.

%% @doc Flatten nested gun error tuples into a single readable atom.
normalize_connection_error(timeout) ->
    connect_timeout;
normalize_connection_error({error, Reason}) ->
    normalize_connection_error(Reason);
normalize_connection_error({shutdown, Reason}) ->
    normalize_connection_error(Reason);
normalize_connection_error({Type, _}) ->
    Type;
normalize_connection_error(Reason) ->
    Reason.

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
            ?event(debug_http_monitor, {resolved_monitor, Res})
    end.

%%% ==================================================================
%%% gen_server callbacks.
%%% ==================================================================

init(Opts) ->
    case hb_opts:get(prometheus, not hb_features:test(), Opts) of
        true ->
            try init_prometheus()
            catch Type:Reason ->
                ?event(warning,
                    {prometheus_not_started, {type, Type}, {reason, Reason}})
            end;
        false -> ok
    end,
    {ok, #state{ opts = Opts }}.

handle_call({request, Args, Opts}, _From, State) ->
    ArgsOpts = maps:get(opts, Args, #{}),
    HttpOpts =
        maps:get(
            opts,
            hb_opts:mimic_default_types(Opts, existing, #{deep => true}),
            #{}
        ),
    MergedHttpOpts = maps:merge(ArgsOpts, HttpOpts),
    MergedArgs = Args#{opts => MergedHttpOpts},
    {Response, NextState} = request_on_worker(MergedArgs, Opts, State),
    {reply, Response, NextState};

handle_call(Request, _From, State) ->
	?event(warning, {unhandled_call, {module, ?MODULE}, {request, Request}}),
	{reply, ok, State}.

handle_cast(Cast, State) ->
	?event(warning, {unhandled_cast, {module, ?MODULE}, {cast, Cast}}),
	{noreply, State}.

handle_info({gun_up, PID, Protocol}, State) ->
    ?event(http_client, {gun_up, {pid, PID}, {protocol, Protocol}}),
    {noreply, State};

handle_info({gun_error, PID, Reason}, State) ->
    NextState = remove_connection_by_pid(PID, State),
    ?event(http_client, {connection_error, {pid, PID}, {reason, Reason}}),
    gun:shutdown(PID),
    {noreply, NextState};

handle_info({gun_down, PID, Protocol, Reason, _KilledStreams}, State) ->
    NextState = remove_connection_by_pid(PID, State),
    ?event(http_outbound, {gun_shutdown_after_down,
            {pid, PID},
            {protocol, Protocol},
            {reason, Reason}
        }
    ),
    gun:shutdown(PID),
    {noreply, NextState};

handle_info({'DOWN', _Ref, process, PID, Reason}, State) ->
    ?event(debug_http_client, {gun_connection_down, {pid, PID}, {reason, Reason}}),
    {noreply, remove_connection_by_pid(PID, State)};

handle_info(Message, State) ->
	?event(warning, {unhandled_info, {module, ?MODULE}, {message, Message}}),
	{noreply, State}.

terminate(Reason, #state{monitor_by_pid = MonitorByPID}) ->
	?event(info,{http_client_terminating, {reason, Reason}}),
    maps:foreach(
        fun(PID, MonitorRef) ->
            gun:shutdown(PID),
            demonitor(MonitorRef, [flush])
        end,
        MonitorByPID
    ),
	ok.

%%% ==================================================================
%%% Private functions.
%%% ==================================================================

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
            retry => 0,
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
    GunOpts =
        case Proto = hb_opts:get(protocol, DefaultProto, Opts) of
            http3 -> BaseGunOpts#{protocols => [http3], transport => quic};
            http2 -> BaseGunOpts#{protocols => [http2]};
            http1 -> BaseGunOpts#{protocols => [http]}
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

do_gun_request(PID, Args, Opts) ->
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
	record_response_status(Method, Response, Path),
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
			download_metric(FinData),
			upload_metric(Args),
			{ok,
                hb_maps:get(status, Args, undefined, Opts),
                hb_maps:get(headers, Args, undefined, Opts),
                FinData
            };
		{error, timeout} = Response ->
			record_response_status(Method, Response, Path),
            ?event(http_outbound, {gun_cancel, {path, Path}}),
			gun:cancel(PID, Ref),
			log(warning, gun_await_process_down, Args, timeout, Opts),
			Response;
        {error,{connection_error,{stream_closed, Message}}} = Response ->
            ?event(http_outbound, {gun_cancel, {path, Path}, {message, Message}}),
            gun:cancel(PID, Ref),
            Response;
		{error, Reason} = Response when is_tuple(Reason) ->
			record_response_status(Method, Response, Path),
			log(warning, gun_await_process_down, Args, Reason, Opts),
			Response;
		Response ->
			record_response_status(Method, Response, Path),
			log(warning, gun_await_unknown, Args, Response, Opts),
			Response
	end.

%% @doc Debug `http` state logging.
log(Type, Event, #{method := Method, peer := Peer, path := Path}, Reason, Opts) ->
    ?event(
        Type,
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

%% Metrics

init_prometheus() ->
    hb_prometheus:declare(counter, [
        {name, gun_requests_total},
        {labels, [http_method, status_class, category]},
        {help, "The total number of GUN requests."}
    ]),
    hb_prometheus:declare(gauge, [
        {name, outbound_connections},
        {help, "The current number of the open outbound network connections"}
    ]),
    hb_prometheus:declare(histogram, [
        {name, http_request_duration_seconds},
        {buckets, [0.01, 0.1, 0.5, 1, 5, 10, 30, 60]},
        {labels, [http_method, status_class, category]},
        {help,
            "The total duration of an hb_http_client:req call. This includes more than"
            " just the GUN request itself (e.g. establishing a connection, "
            "throttling, etc...)"}
    ]),
    hb_prometheus:declare(histogram, [
        {name, http_client_get_chunk_duration_seconds},
        {buckets, [0.1, 1, 10, 60]},
        {labels, [status_class, peer]},
        {help, "The total duration of an HTTP GET chunk request made to a peer."}
    ]),
    hb_prometheus:declare(counter, [
        {name, http_client_downloaded_bytes_total},
        {help, "The total amount of bytes requested via HTTP, per remote endpoint"}
    ]),
    hb_prometheus:declare(counter, [
        {name, http_client_uploaded_bytes_total},
        {help, "The total amount of bytes posted via HTTP, per remote endpoint"}
    ]),
    ok.

%% @doc Record the duration of the request in an async process. We write the
%% data to prometheus if the application is enabled, as well as invoking the
%% `http_monitor' if appropriate.
record_duration(Details, Opts) ->
    spawn(
        fun() ->
            GetFormat = fun
                (<<"request-category">>) ->
                    path_to_category(maps:get(<<"request-path">>, Details));

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

record_response_status(Method, Response, Path) ->
	inc_prometheus_counter(gun_requests_total,
        [
            hb_util:list(method_to_bin(Method)),
			hb_util:list(get_status_class(Response)),
            hb_util:list(path_to_category(Path))
        ],
        1
    ).

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
        _ ->
            try
                prometheus_counter:inc(Name, Labels, Value)
            catch
                _:_ ->
                    init_prometheus(),
                    prometheus_counter:inc(Name, Labels, Value)
            end
    end.

download_metric(Data) ->
	inc_prometheus_counter(
		http_client_downloaded_bytes_total,
        [],
		byte_size(Data)
	).

%% @doc Record instances of uploaded bytes to the remote server.
upload_metric(#{method := Method, body := Body}) when is_atom(Method) ->
    upload_metric(#{ method => hb_util:bin(Method), body => Body });
upload_metric(#{ method := <<"POST">>, body := Body}) -> upload_metric(Body);
upload_metric(#{ method := <<"PUT">>, body := Body}) -> upload_metric(Body);
upload_metric(Body) when is_binary(Body) ->
	inc_prometheus_counter(
		http_client_uploaded_bytes_total,
		[],
		byte_size(Body)
	);
upload_metric(_) ->
	ok.

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

% @doc Return the HTTP status class label for cowboy_requests_total and
% gun_requests_total metrics.
get_status_class({ok, {{Status, _}, _, _, _, _}}) ->
	get_status_class(Status);
get_status_class({ok, Status, _RespondeHeaders, _Body}) ->
    get_status_class(Status);
get_status_class({error, connection_closed}) ->
	<<"connection-closed">>;
get_status_class({error, connect_timeout}) ->
	<<"connect-timeout">>;
get_status_class({error, timeout}) ->
	<<"timeout">>;
get_status_class({error,{shutdown,timeout}}) ->
	<<"shutdown-timeout">>;
get_status_class({error, econnrefused}) ->
	<<"econnrefused">>;
get_status_class({error, {shutdown,econnrefused}}) ->
	<<"shutdown-econnrefused">>;
get_status_class({error, {down, {shutdown, econnrefused}}}) ->
    <<"shutdown-econnrefused">>;
get_status_class({error, {shutdown,ehostunreach}}) ->
	<<"shutdown-ehostunreach">>;
get_status_class({error, {shutdown,normal}}) ->
	<<"shutdown-normal">>;
get_status_class({error, {closed,_}}) ->
	<<"closed">>;
get_status_class({error, noproc}) ->
	<<"noproc">>;
get_status_class({error, {connection_error, {stream_closed, _Message}}}) ->
    <<"stream-closed">>;
get_status_class({error, {stream_error, {stream_error, too_many_streams, _Message}}}) ->
    <<"too-many-streams">>;
get_status_class({error, {stream_error, {stream_error, refused_stream, _Message}}}) ->
    <<"refused-stream">>;
get_status_class({error, {stream_error, {goaway, no_error, _Message}}}) ->
    <<"go-away">>;
get_status_class({error, {stream_error, {closed, {error, einval}}}}) ->
    <<"closed-einval">>;
get_status_class({error, {down, shutdown}}) ->
    <<"down-shutdown">>;
get_status_class({error, {stream_error, closed}}) ->
    <<"stream-closed">>;
get_status_class({error, {stream_error, {closed, {error, closed}}}}) ->
    <<"stream-closed">>;
get_status_class({error, {stream_error, closing}}) ->
    <<"stream-closing">>;
get_status_class({error, {down, noproc}}) ->
    <<"noproc">>;
get_status_class({error, {stream_error, {closed, normal}}}) ->
    <<"stream-closed">>;
get_status_class(208) ->
	<<"already-processed">>;
get_status_class(404) ->
	<<"not-found">>;
get_status_class(429) ->
	<<"too-many-requests">>;
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
    ?event(warning, {unknown_status_class, {status_class, StatusClass}}),
	<<"unknown">>.

%% @doc Convert path to category for grafana labels.
path_to_category(Path) ->
    case Path of
        <<"/graphql">> -> <<"GraphQL">>;
        <<"/raw", _/binary>> -> <<"Raw">>;
        <<"/tx/", _/binary>> -> <<"TX">>;
        <<"/tx_anchor", _/binary>> -> <<"TX Anchor">>;
        <<"/chunk", _/binary>> -> <<"Chunk">>;
        <<"/price/", _/binary>> -> <<"Price">>;
        <<"/block/height/", _/binary>> -> <<"Block Height">>;
        <<"/block/current", _/binary>> -> <<"Current Block">>;
        <<"/price", _/binary>> -> <<"Price">>;
        <<"/~cache@1.0/read", _/binary>> -> <<"Remote Read">>;
        undefined -> <<"unknown">>;
        _ -> <<"unknown">>
    end.
