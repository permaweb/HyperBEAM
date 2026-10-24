%%% @doc Reverse-tunnel contracts exercised across isolated HTTP nodes.
-module(dev_tunnel_test_vectors).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

routing_test_() -> vector(fun routing/1).
registration_test_() -> vector(fun registration/1).
transport_codec_test_() -> vector(fun transport_codec/1).
scoped_host_test_() -> vector(fun scoped_host/1).
host_fallthrough_test_() -> vector(fun host_fallthrough/1).
call_wait_bound_test_() -> vector(fun call_wait_bound/1).
request_lifecycle_test_() -> vector(fun request_lifecycle/1).
request_errors_test_() -> vector(fun request_errors/1).
linked_direct_test_() -> vector(fun linked_direct/1).
public_call_test_() -> vector(fun public_call/1).
payloads_test_() -> vector(fun payloads/1).
linked_values_test_() -> vector(fun linked_values/1).
negotiation_test_() -> vector(fun negotiation/1).
http_semantics_test_() -> vector(fun http_semantics/1).
authorization_test_() -> vector(fun authorization/1).
response_auth_test_() -> vector(fun response_auth/1).
unavailable_test_() -> vector(fun unavailable/1).
concurrent_test_() -> vector(fun concurrent/1).
long_running_test_() -> vector(fun long_running/1).
rejected_response_test_() -> vector(fun rejected_response/1).
disconnect_test_() -> vector(fun disconnect/1).
disconnected_busy_host_test_() -> vector(fun disconnected_busy_host/1).
restart_test_() -> vector(fun restart/1).
broker_isolation_test_() -> vector(fun broker_isolation/1).
pool_resize_test_() -> vector(fun pool_resize/1).
worker_loss_test_() -> vector(fun worker_loss/1).
broker_restart_test_() -> vector(fun broker_restart/1).
persisted_restart_test_() -> vector(fun persisted_restart/1).
mixed_load_test_() -> vector(fun mixed_load/1).
scheduler_httpsig_test_() -> vector(fun scheduler_httpsig/1).
scheduler_ans104_test_() -> vector(fun scheduler_ans104/1).

%% @doc Run each scenario with its own nodes and cleanup. The description of
%% its test is the name of the scenario's function.
vector(Test) ->
    {name, Name} = erlang:fun_info(Test, name),
    {
        setup, fun setup/0, fun cleanup/1,
        fun(Ctx) ->
            {hb_util:list(Name), {timeout, 120, fun() -> Test(Ctx) end}}
        end
    }.

%% @doc Give each HTTP node its own wallet, primary and auxiliary stores.
setup() ->
    Reject = atomics:new(1, []),
    Broker =
        start_node(
            #{
                <<"priv-test-reject-response">> => Reject,
                <<"tunnel-response-timeout">> => 12_000,
                <<"on">> => #{
                    <<"request">> => [
                        #{
                            <<"device">> => #{
                                <<"request">> => fun reject_response/3
                            }
                        },
                        #{
                            <<"device">> => <<"tunnel@1.0">>,
                            <<"path">> => <<"request">>
                        }
                    ]
                }
            }
        ),
    First = origin(),
    Second = origin(),
    #{
        broker => Broker, first => First, second => Second,
        caller => opts(), reject => Reject
    }.

%% @doc Start an origin that runs the test operations from its request hook.
origin() ->
    start_node(
        #{
            <<"priv-test-writes">> => atomics:new(1, []),
            <<"on">> => #{
                <<"request">> => #{
                    <<"device">> => #{ <<"request">> => fun fixture_request/3 }
                }
            }
        }
    ).

%% @doc Return the actual listener options, including the node's server ID.
start_node(Extra) ->
    Opts = maps:merge(opts(), Extra),
    URL = hb_http_server:start_node(Opts),
    ID =
        hb_util:human_id(
            ar_wallet:to_address(maps:get(<<"priv-wallet">>, Opts))
        ),
    #{
        url => URL, id => ID,
        opts => hb_http_server:get_opts(#{ <<"http-server">> => ID })
    }.

%% @doc Isolate node storage and disable result reuse for the transport tests.
opts() ->
    Store = [hb_test_utils:test_store()],
    #{
        <<"port">> => 0,
        <<"priv-wallet">> => ar_wallet:new(),
        <<"store">> => Store,
        <<"match-index">> => Store,
        <<"scheduler-store">> => Store,
        <<"priv-store">> => Store,
        <<"attested-store">> => Store,
        <<"loaded-device-store">> => [hb_test_utils:test_store()],
        <<"http-client">> => gun,
        <<"protocol">> => http2,
        <<"http-only-result">> => false,
        <<"http-client-send-timeout">> => 20_000,
        <<"tunnel-register-timeout">> => 1_000,
        <<"cache-control">> => [<<"no-store">>, <<"no-cache">>]
    }.

%% @doc Stop only this vector's listeners, workers and stores.
cleanup(#{
    broker := Broker, first := First, second := Second, caller := Caller
}) ->
    lists:foreach(fun stop_node/1, [First, Second, Broker]),
    stop_stores(Caller).

%% @doc Stop a node's broker and workers, then its listener and stores.
stop_node(#{id := ID, opts := Opts}) ->
    dev_tunnel_server:stop(ID),
    cowboy:stop_listener(ID),
    stop_stores(Opts).

%% @doc Stop a node's primary and device stores.
stop_stores(Opts) ->
    hb_store:stop(maps:get(<<"store">>, Opts)),
    hb_store:stop(maps:get(<<"loaded-device-store">>, Opts)).

%% @doc Connect through AO-Core using the origin's actual HTTP node message.
connect(#{url := Broker}, #{id := ID, opts := Opts}, Workers) ->
    {ok, Res} =
        hb_ao:resolve(
            #{ <<"device">> => <<"tunnel@1.0">> },
            #{
                <<"path">> => <<"connect">>, <<"peer">> => Broker,
                <<"workers">> => Workers
            },
            Opts
        ),
    ?assertEqual(Workers, hb_maps:get(<<"workers">>, Res, not_found, Opts)),
    await(fun() -> registered(Broker, ID, Opts) =:= Workers end).

%% @doc Return the number of registrations that a broker has for an address.
registered(Broker, ID, Opts) ->
    hb_ao:get(<<"registered/", ID/binary>>, status(Broker, Opts), 0, Opts).

%% @doc Read a node's tunnel status over HTTP.
status(Node, Opts) ->
    success(hb_http:get(Node, <<"/~tunnel@1.0/status">>, Opts), Opts).

%% @doc Wait for observable state instead of guessing a network delay.
await(Fun) ->
    Deadline = erlang:monotonic_time(millisecond) + 5_000,
    hb_util:until(
        fun() ->
            case Fun() of
                true -> true;
                false ->
                    ?assert(erlang:monotonic_time(millisecond) < Deadline),
                    false
            end
        end
    ).

%% @doc Wait until the broker has no queued or dispatched requests.
drained(#{broker := #{url := Broker}, caller := Opts}) ->
    await(
        fun() ->
            S = status(Broker, Opts),
            hb_maps:get(<<"open-requests">>, S, 0, Opts) =:= 0
        end
    ).

%% @doc Check that a reply succeeded and that all of its commitments verify,
%% and return it loaded.
success(Result, Opts) ->
    ?assertMatch({ok, _}, Result),
    {ok, Msg} = Result,
    Loaded = hb_cache:ensure_all_loaded(Msg, Opts),
    ?assertEqual(true, hb_message:deep_verify(Loaded, Opts)),
    Loaded.

%% @doc Check that all of the commitments of an error reply verify, and return
%% it loaded.
response({_, Msg}, Opts) when is_map(Msg) ->
    Loaded = hb_cache:ensure_all_loaded(Msg, Opts),
    ?assertEqual(true, hb_message:deep_verify(Loaded, Opts)),
    Loaded.

%% @doc Sign the whole test message, including nested payloads.
signed(Msg, Opts) ->
    hb_message:commit(
        Msg, Opts,
        #{
            <<"commitment-device">> => <<"httpsig@1.0">>,
            <<"bundle">> => true,
            <<"committed">> => maps:keys(hb_message:uncommitted(Msg, Opts))
        }
    ).

%% @doc Append the transport target without disturbing application parameters.
path(Path, #{id := ID}) ->
    Separator =
        case binary:match(Path, <<"?">>) of
            nomatch -> <<"?">>;
            _ -> <<"&">>
        end,
    <<Path/binary, Separator/binary, "tunnel-target=", ID/binary>>.

%% @doc Give the host whose first label is the DNS URI of a node's wallet.
host(ID) ->
    Label = hb_committer:uri(ID),
    <<Label/binary, ".example">>.

%% @doc Send a GET request to an origin through the broker.
get(Ctx, Origin, Path) ->
    #{broker := #{url := Broker}, caller := Opts} = Ctx,
    hb_http:get(Broker, path(Path, Origin), Opts).

%% @doc Send a POST request to an origin through the broker.
post(Ctx, Origin, Path, Msg) ->
    #{broker := #{url := Broker}, caller := Opts} = Ctx,
    hb_http:post(Broker, path(Path, Origin), Msg, Opts).

%% @doc Route the test namespace to an ordinary AO-Core message device.
fixture_request(_, Hook = #{<<"request">> := Raw}, _Opts) ->
    case hb_maps:get(<<"path">>, Raw, <<>>) of
        <<"/~tunnel-test@1.0/", Tail/binary>> ->
            [Operation | _] = binary:split(Tail, <<"?">>),
            URI = uri_string:parse(hb_maps:get(<<"path">>, Raw)),
            Params =
                maps:from_list(
                    uri_string:dissect_query(maps:get(query, URI, <<>>))
                ),
            {ok, #{
                <<"body">> => [
                    #{
                        <<"device">> => #{
                            <<"echo">> => fun echo/3,
                            <<"linked">> => fun linked/3,
                            <<"list">> => fun list/3,
                            <<"missing">> => fun missing/3,
                            <<"reply">> => fun reply/3,
                            <<"asset">> => fun asset/3,
                            <<"context">> => fun context/3,
                            <<"write">> => fun write/3
                        },
                        <<"fixture-request">> => Raw,
                        <<"fixture-params">> => Params
                    },
                    #{ <<"path">> => Operation }
                ]
            }};
        _ -> {ok, Hook}
    end.

%% @doc Check the received payload's signatures before returning it unchanged.
echo(Base, _, Opts) ->
    Raw = hb_maps:get(<<"fixture-request">>, Base, not_found, Opts),
    ?assertEqual(true, hb_message:deep_verify(Raw, Opts)),
    {ok, #{
        <<"status">> => 200,
        <<"body">> => hb_maps:get(<<"payload">>, Raw, <<>>, Opts)
    }}.

%% @doc Return a value that exists only in the origin's store.
linked(_, _, Opts) ->
    {ok, ID} =
        hb_cache:write(
            #{ <<"nested">> => #{ <<"value">> => <<"origin-only">> } }, Opts
        ),
    {ok, #{
        <<"status">> => 200,
        <<"body">> => hb_link:decode(<<"ao:", ID/binary>>)
    }}.

%% @doc Return a list as the result.
list(_, _, _) -> {ok, [1, <<"two">>, #{ <<"three">> => 3 }]}.

%% @doc Give the result of the `list' fixture as a list, rather than as the
%% numbered message that resolution gives.
list_response(_, Hook = #{ <<"request">> := Raw }, Opts) ->
    case hb_maps:get(<<"path">>, Raw, <<>>, Opts) of
        <<"/~tunnel-test@1.0/list">> ->
            Body = hb_maps:get(<<"body">>, Hook, #{}, Opts),
            {ok,
                Hook#{
                    <<"body">> => hb_util:message_to_ordered_list(Body, Opts)
                }
            };
        _ -> {ok, Hook}
    end.

%% @doc Return a valid link to a message that the origin does not have.
missing(_, _, _) ->
    {ok, #{
        <<"body">> =>
            hb_link:decode(<<"ao:IYkkrqlZNW_J-4T-5eFApZOMRl5P4VjvrcOXWvIqB1Q">>)
    }}.

%% @doc Check that the options of an execution omit the node's keys, and
%% return their `test-http-option', which `http-extra-opts' sets.
context(_, _, Opts) ->
    ?assertEqual(false, hb_maps:is_key(<<"priv-wallet">>, Opts)),
    ?assertEqual(false, hb_maps:is_key(<<"identities">>, Opts)),
    {ok, #{ <<"body">> => hb_maps:get(<<"test-http-option">>, Opts) }}.

%% @doc Prove that the response hook sees the origin's execution result.
fixture_response(_, Hook, Opts) ->
    Body = hb_message:uncommitted(hb_maps:get(<<"body">>, Hook, Opts), Opts),
    {ok, Hook#{ <<"body">> => Body#{ <<"response-hook">> => true } }}.

%% @doc Exercise status, redirect and cookie fields through the real HTTP codec.
reply(Base, _, Opts) ->
    Code = hb_util:int(parameter(<<"code">>, Base, 200, Opts)),
    Body =
        case Code of
            204 -> #{};
            _ -> #{ <<"body">> => <<"fixture response">> }
        end,
    RawRes = Body#{
        <<"status">> => Code,
        <<"location">> => <<"/next?value=one">>,
        <<"content-type">> => <<"text/plain">>,
        <<"set-cookie">> => [<<"a=one; Path=/">>, <<"b=two; Path=/">>]
    },
    Res =
        case Code of
            204 -> maps:remove(<<"set-cookie">>, RawRes);
            _ -> RawRes
        end,
    case Code >= 400 of true -> {error, Res}; false -> {ok, Res} end.

%% @doc Return an HTML page with the content type that `media' gives.
asset(Base, _, Opts) ->
    {ok, #{
        <<"status">> => 200,
        <<"content-type">> =>
            parameter(<<"media">>, Base, <<"text/html">>, Opts),
        <<"body">> => <<"<!doctype html><title>Tunnel</title><p>ready</p>">>
    }}.

%% @doc Count execution before delaying its response, so retries are visible.
write(Base, _, Opts) ->
    Count = atomics:add_get(hb_maps:get(<<"priv-test-writes">>, Opts), 1, 1),
    timer:sleep(hb_util:int(parameter(<<"duration">>, Base, 0, Opts))),
    {ok, #{<<"status">> => 200, <<"body">> => Count}}.

%% @doc Read fixture controls from signed fields or query parameters.
parameter(Key, Base, Default, Opts) ->
    Raw = hb_maps:get(<<"fixture-request">>, Base, not_found, Opts),
    Params = hb_maps:get(<<"fixture-params">>, Base, #{}, Opts),
    hb_maps:get(Key, Raw, hb_maps:get(Key, Params, Default, Opts), Opts).

%% @doc Reject each posted response while the test's `reject' flag is 1, and
%% fail the next one with 503 when it is 2.
reject_response(_, Hook = #{<<"request">> := Raw}, Opts) ->
    Reject = hb_maps:get(<<"priv-test-reject-response">>, Opts),
    case {hb_maps:get(<<"path">>, Raw, <<>>, Opts), atomics:get(Reject, 1)} of
        {<<"/~tunnel@1.0/response", _/binary>>, 1} ->
            {error, #{
                <<"status">> => 403,
                <<"body">> => <<"Response policy rejected.">>
            }};
        {<<"/~tunnel@1.0/response", _/binary>>, 2} ->
            atomics:put(Reject, 1, 0),
            {failure, #{
                <<"status">> => 503,
                <<"body">> => <<"Response policy failed once.">>
            }};
        _ -> {ok, Hook}
    end.

%% @doc Check that an expired registration keeps its host present while its
%% node connects again, and that the requests of an absent host expire.
registration(Ctx = #{
    broker := Broker, first := First, second := Second, caller := Opts
}) ->
    URL = maps:get(url, Broker),
    lists:foreach(
        fun({Node, Expected}) ->
            ID = maps:get(id, Node),
            NodeOpts = maps:get(opts, Node),
            Registration =
                signed(
                    #{ <<"address">> => ID, <<"tunnel-timeout">> => 10 },
                    NodeOpts
                ),
            Result =
                response(
                    hb_http:post(
                        URL,
                        <<"/~tunnel@1.0/register">>,
                        Registration,
                        Opts
                    ),
                    Opts
                ),
            ?assertEqual(204, hb_maps:get(<<"status">>, Result, Opts)),
            ?assertEqual(0, registered(URL, ID, Opts)),
            Request = #{
                <<"path">> => <<"/~meta@1.0/info/address">>,
                <<"host">> => host(ID)
            },
            Deadline = erlang:monotonic_time(millisecond) + 5_000,
            Pending = async(fun() -> hb_http:get(URL, Request, Opts) end),
            await(
                fun() ->
                    hb_maps:get(
                        <<"open-requests">>,
                        status(URL, Opts),
                        Opts
                    ) =:= 1
                end
            ),
            case Expected of
                200 -> connect(Broker, Node, 2);
                404 -> ok
            end,
            Reply = response(collect(Pending), Opts),
            ?assertEqual(Expected, hb_maps:get(<<"status">>, Reply, Opts)),
            ?assert(erlang:monotonic_time(millisecond) < Deadline),
            case Expected of
                200 ->
                    ?assertEqual(ID, hb_maps:get(<<"body">>, Reply, Opts));
                404 ->
                    Absent = success(hb_http:get(URL, Request, Opts), Opts),
                    ?assertEqual(maps:get(id, Broker),
                        hb_maps:get(<<"body">>, Absent, Opts))
            end,
            drained(Ctx)
        end,
        [{First, 200}, {Second, 404}]
    ).

%% @doc Check that registrations and envelopes are signed, and give their
%% codec, over HTTP.
transport_codec(Ctx = #{broker := Broker, first := First, caller := Opts}) ->
    Owner = self(),
    Ref = make_ref(),
    BrokerOpts = maps:get(opts, Broker),
    On = hb_maps:get(<<"on">>, BrokerOpts, BrokerOpts),
    Observe = fun(_, Hook = #{<<"request">> := Raw}, NodeOpts) ->
        Path = hb_maps:get(<<"path">>, Raw, <<>>, NodeOpts),
        case Path of
            <<"/~tunnel@1.0/register">> ->
                Owner ! {Ref, Path, Raw};
            <<"/~tunnel@1.0/response">> ->
                Owner ! {Ref, Path, Raw};
            _ -> ok
        end,
        {ok, Hook}
    end,
    hb_http_server:set_opts(BrokerOpts#{
        <<"on">> => On#{
            <<"request">> => [
                #{<<"device">> => #{<<"request">> => Observe}}
                | hb_maps:get(<<"request">>, On, BrokerOpts)
            ]
        }
    }),
    connect(Broker, First, 1),
    Reply = success(get(Ctx, First, <<"/~meta@1.0/info/address">>), Opts),
    ?assertEqual(maps:get(id, First), hb_maps:get(<<"body">>, Reply, Opts)),
    Envelopes =
        lists:map(
            fun(Path) ->
                receive
                    {Ref, Path, Raw} ->
                        ?assert(hb_message:deep_verify(Raw, BrokerOpts)),
                        {
                            Path,
                            hb_maps:get(
                                <<"codec-device">>,
                                Raw,
                                none,
                                BrokerOpts
                            )
                        }
                after 5_000 -> error({missing_transport_envelope, Path})
                end
            end,
            [<<"/~tunnel@1.0/register">>, <<"/~tunnel@1.0/response">>]
        ),
    ?assertEqual([
        {<<"/~tunnel@1.0/register">>, <<"httpsig@1.0">>},
        {<<"/~tunnel@1.0/response">>, <<"httpsig@1.0">>}
    ], Envelopes),
    drained(Ctx).

%% @doc Check that a `tunnel-host' routes only the hosts that are its direct
%% children.
scoped_host(Ctx = #{broker := Broker, first := First, caller := Opts}) ->
    hb_http_server:set_opts(
        (maps:get(opts, Broker))#{ <<"tunnel-host">> => <<"example">> }
    ),
    connect(Broker, First, 2),
    Label = hb_committer:uri(maps:get(id, First)),
    lists:foreach(
        fun({Suffix, Expected}) ->
            Result =
                success(
                    hb_http:get(
                        maps:get(url, Broker),
                        #{
                            <<"path">> => <<"/~meta@1.0/info/address">>,
                            <<"host">> => <<Label/binary, Suffix/binary>>
                        },
                        Opts
                    ),
                    Opts
                ),
            ?assertEqual(Expected, hb_maps:get(<<"body">>, Result, Opts))
        end,
        [
            {<<".example">>, maps:get(id, First)},
            {<<".EXAMPLE.:1234">>, maps:get(id, First)},
            {<<".www.example">>, maps:get(id, Broker)},
            {<<".example.other">>, maps:get(id, Broker)}
        ]
    ),
    Result = success(get(Ctx, First, <<"/~meta@1.0/info/address">>), Opts),
    ?assertEqual(maps:get(id, First), hb_maps:get(<<"body">>, Result, Opts)),
    drained(Ctx).

%% @doc Check that a signed request for an absent host reaches the next hook
%% unchanged, and that a tunneled request runs no later hook.
host_fallthrough(Ctx = #{broker := Broker, first := First, caller := Opts}) ->
    BrokerOpts = maps:get(opts, Broker),
    On = maps:get(<<"on">>, BrokerOpts),
    Calls = atomics:new(1, []),
    Count =
        fun(Base, Hook, NodeOpts) ->
            Raw = hb_maps:get(<<"request">>, Hook, NodeOpts),
            case hb_maps:get(<<"path">>, Raw, <<>>, NodeOpts) of
                <<"/~tunnel-test@1.0/", _/binary>> -> atomics:add(Calls, 1, 1);
                _ -> ok
            end,
            fixture_request(Base, Hook, NodeOpts)
        end,
    hb_http_server:set_opts(BrokerOpts#{
        <<"on">> => On#{
            <<"request">> =>
                maps:get(<<"request">>, On) ++
                    [#{ <<"device">> => #{ <<"request">> => Count } }]
        }
    }),
    Host = host(maps:get(id, First)),
    Path = <<"/~tunnel-test@1.0/echo">>,
    Request =
        signed(
            #{
                <<"host">> => Host,
                <<"path">> => Path,
                <<"method">> => <<"POST">>,
                <<"payload">> => #{ <<"value">> => <<"unchanged">> }
            },
            Opts
        ),
    Reply =
        success(
            hb_http:post(maps:get(url, Broker), Path, Request, Opts),
            Opts
        ),
    ?assertEqual(#{ <<"value">> => <<"unchanged">> },
        hb_maps:get(<<"body">>, Reply, Opts)),
    ?assertEqual(1, atomics:get(Calls, 1)),
    connect(Broker, First, 2),
    % The origin serves a signed request with its signed host, as it verifies.
    ?assertEqual(
        {#{ <<"value">> => <<"unchanged">> }, 1},
        {
            hb_maps:get(
                <<"body">>,
                success(
                    hb_http:post(maps:get(url, Broker), Path, Request, Opts),
                    Opts
                ),
                not_found,
                Opts
            ),
            atomics:get(Calls, 1)
        }
    ),
    Before = atomics:get(Calls, 1),
    lists:foreach(
        fun(Code) ->
            Result =
                response(
                    hb_http:get(
                        maps:get(url, Broker),
                        #{
                            <<"host">> => Host,
                            <<"path">> =>
                                <<
                                    "/~tunnel-test@1.0/reply?code=",
                                    (integer_to_binary(Code))/binary
                                >>
                        },
                        Opts
                    ),
                    Opts
                ),
            ?assertEqual(Code, hb_maps:get(<<"status">>, Result, Opts)),
            ?assertEqual(Before, atomics:get(Calls, 1))
        end,
        [200, 404]
    ),
    drained(Ctx).

%% @doc Check that a `call' cannot occupy an origin's worker past the origin's
%% own response limit. One call carries a nested `call' to an absent target that
%% asks to wait for `infinity'; the origin's single worker runs it and waits on
%% the origin's own broker. The wait is bounded by the origin's limit, so the
%% worker serves the next request. Before the fix the inner call waits forever
%% and the next request is never served.
call_wait_bound(Ctx = #{broker := Broker, first := First, caller := Opts}) ->
    URL = maps:get(url, Broker),
    hb_http_server:set_opts(
        (hb_http_server:get_opts(maps:get(opts, First)))#{
            <<"tunnel-response-timeout">> => 500
        }
    ),
    connect(Broker, First, 1),
    Absent = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Attack =
        async(
            fun() ->
                hb_http:post(
                    URL,
                    <<"/~tunnel@1.0/call">>,
                    #{
                        <<"tunnel-target">> => maps:get(id, First),
                        <<"request">> => #{
                            <<"path">> => <<"/~tunnel@1.0/call">>,
                            <<"tunnel-target">> => Absent,
                            <<"tunnel-timeout">> => <<"infinity">>,
                            <<"request">> =>
                                #{ <<"path">> => <<"/~meta@1.0/info/address">> }
                        }
                    },
                    Opts
                )
            end
        ),
    await(fun() -> registered(URL, maps:get(id, First), Opts) =:= 0 end),
    ?assertEqual(
        maps:get(id, First),
        hb_maps:get(
            <<"body">>,
            success(get(Ctx, First, <<"/~meta@1.0/info/address">>), Opts),
            not_found,
            Opts
        )
    ),
    ?assertEqual(
        504,
        hb_maps:get(
            <<"status">>, response(collect(Attack), Opts), not_found, Opts
        )
    ),
    drained(Ctx).

%% @doc Check that a path that does not parse and a missing link have the
%% same status through a tunnel as directly.
request_errors(Ctx = #{broker := Broker, first := First, caller := Opts}) ->
    connect(Broker, First, 1),
    lists:foreach(
        fun({Path, Status}) ->
            Direct =
                response(hb_http:get(maps:get(url, First), Path, Opts), Opts),
            Tunnel =
                response(
                    hb_http:post(
                        maps:get(url, Broker),
                        <<"/~tunnel@1.0/call">>,
                        #{
                            <<"tunnel-target">> => maps:get(id, First),
                            <<"request">> => #{ <<"path">> => Path }
                        },
                        Opts
                    ),
                    Opts
                ),
            ?assertEqual(Status, hb_maps:get(<<"status">>, Direct, Opts)),
            ?assertEqual(Status, hb_maps:get(<<"status">>, Tunnel, Opts)),
            % The origin replies to an exception as its HTTP server does.
            ?assertEqual(
                <<"throw">>,
                hb_maps:get(<<"type">>, Tunnel, not_found, Opts)
            )
        end,
        [{<<"/a/b~">>, 400}, {<<"/~tunnel-test@1.0/missing">>, 404}]
    ),
    drained(Ctx).

%% @doc Check that a tunneled request follows the origin's initialization,
%% signature and hook rules.
request_lifecycle(Ctx = #{
    broker := Broker, first := First, second := Second, caller := Opts
}) ->
    FirstOpts = maps:get(opts, First),
    hb_http_server:set_opts(FirstOpts#{ <<"initialized">> => false }),
    connect(Broker, First, 1),
    BlockedPath = <<"/~tunnel-test@1.0/context">>,
    Direct =
        response(hb_http:get(maps:get(url, First), BlockedPath, Opts), Opts),
    Tunnel = response(get(Ctx, First, BlockedPath), Opts),
    ?assertEqual(400, hb_maps:get(<<"status">>, Direct, Opts)),
    ?assertEqual(400, hb_maps:get(<<"status">>, Tunnel, Opts)),
    ?assertEqual(hb_maps:get(<<"body">>, Direct, Opts),
        hb_maps:get(<<"body">>, Tunnel, Opts)),
    lists:foreach(
        fun(Path) ->
            success(hb_http:get(maps:get(url, First), Path, Opts), Opts),
            success(get(Ctx, First, Path), Opts)
        end,
        [<<"/~meta@1.0/info">>, <<"/~meta@1.0/info?test-value=one">>]
    ),
    SecondOpts = maps:get(opts, Second),
    hb_http_server:set_opts(SecondOpts#{
        <<"force-signed-requests">> => false,
        <<"test-http-option">> => <<"node-default">>,
        <<"http-extra-opts">> =>
            #{ <<"test-http-option">> => <<"http-value">> },
        <<"on">> => (hb_maps:get(<<"on">>, SecondOpts))#{
            <<"response">> => #{
                <<"device">> => #{ <<"response">> => fun fixture_response/3 }
            }
        }
    }),
    connect(Broker, Second, 1),
    hb_http_server:set_opts(
        (hb_http_server:get_opts(SecondOpts))#{
            <<"force-signed-requests">> => true
        }
    ),
    lists:foreach(
        fun(Result) ->
            ?assertEqual(400,
                hb_maps:get(<<"status">>, response(Result, Opts), Opts))
        end,
        [hb_http:get(maps:get(url, Second), BlockedPath, Opts),
            get(Ctx, Second, BlockedPath)]
    ),
    Request = signed(#{ <<"probe">> => <<"signed-application">> }, Opts),
    lists:foreach(
        fun(Result) ->
            Res = success(Result, Opts),
            ?assertEqual(<<"http-value">>, hb_maps:get(<<"body">>, Res, Opts)),
            ?assertEqual(true, hb_maps:get(<<"response-hook">>, Res, Opts))
        end,
        [hb_http:post(maps:get(url, Second), BlockedPath, Request, Opts),
            post(Ctx, Second, BlockedPath, Request)]
    ),
    drained(Ctx).

%% @doc Route by host or address without forwarding unrelated broker requests.
routing(Ctx = #{
    broker := Broker, first := First, second := Second, caller := Opts
}) ->
    connect(Broker, First, 2),
    connect(Broker, Second, 2),
    lists:foreach(
        fun(Origin = #{ id := ID }) ->
            Res =
                success(get(Ctx, Origin, <<"/~meta@1.0/info/address">>), Opts),
            ?assertEqual(ID, hb_maps:get(<<"body">>, Res, not_found, Opts)),
            Hosted =
                success(
                    hb_http:get(
                        maps:get(url, Broker),
                        #{ <<"path">> => <<"/~meta@1.0/info/address">>,
                            <<"host">> => host(ID) },
                        Opts
                    ),
                    Opts
                ),
            ?assertEqual(ID, hb_maps:get(<<"body">>, Hosted, not_found, Opts))
        end,
        [First, Second]
    ),
    Res =
        success(
            hb_http:get(
                maps:get(url, Broker),
                <<"/~meta@1.0/info/address">>,
                Opts
            ),
            Opts
        ),
    ?assertEqual(
        maps:get(id, Broker),
        hb_maps:get(<<"body">>, Res, not_found, Opts)
    ),
    % A path that is not a URI is resolved by the broker as by any node.
    Pipe = <<"/~meta@1.0/info/address|x">>,
    ?assertEqual(
        element(1, hb_http:get(maps:get(url, First), Pipe, Opts)),
        element(1, hb_http:get(maps:get(url, Broker), Pipe, Opts))
    ),
    drained(Ctx).

%% @doc Carry a signed application path in an envelope without rewriting it.
public_call(Ctx = #{broker := Broker, first := First, caller := Opts}) ->
    connect(Broker, First, 2),
    URL = maps:get(url, Broker),
    Request =
        signed(
            #{
                <<"path">> => <<"/~meta@1.0/info/address">>,
                <<"method">> => <<"GET">>
            },
            Opts
        ),
    Call =
        signed(
            #{
                <<"tunnel-target">> => maps:get(id, First),
                <<"request">> => Request
            },
            Opts
        ),
    lists:foreach(
        fun(Path) ->
            Res = success(hb_http:post(URL, Path, Call, Opts), Opts),
            ?assertEqual(
                maps:get(id, First),
                hb_maps:get(<<"body">>, Res, not_found, Opts)
            )
        end,
        % A call with a query in its path is not routed as a request itself.
        [<<"/~tunnel@1.0/call">>, <<"/~tunnel@1.0/call?tunnel-timeout=5000">>]
    ),
    % A call without a request is refused.
    ?assertMatch(
        {error, #{ <<"status">> := 400 }},
        hb_http:post(
            URL,
            <<"/~tunnel@1.0/call">>,
            #{ <<"tunnel-target">> => maps:get(id, First) },
            Opts
        )
    ),
    drained(Ctx).

%% @doc Compare direct and tunneled values, device messages and commitments.
payloads(Ctx = #{broker := Broker, first := First, caller := Opts}) ->
    connect(Broker, First, 4),
    ChildOpts = Opts#{<<"priv-wallet">> => ar_wallet:new()},
    Child =
        signed(
            #{<<"status">> => 201, <<"body">> => <<"signed child">>},
            ChildOpts
        ),
    ANS =
        hb_message:commit(
            #{ <<"body">> => <<"ANS-104 child">> },
            ChildOpts,
            #{
                <<"commitment-device">> => <<"ans104@1.0">>,
                <<"bundle">> => true
            }
        ),
    Values = [
        <<>>, <<0, 1, 255, 0>>, 42, true, [1, <<"two">>, #{ <<"three">> => 3 }],
        #{
            <<"path">> => <<"application/path">>,
            <<"device">> => <<"application">>,
            <<"method">> => <<"PATCH">>, <<"host">> => <<"origin.example">>,
            <<"status">> => 207, <<"child">> => Child, <<"ans104">> => ANS
        },
        binary:copy(<<"payload">>, 600_000)
    ],
    lists:foreach(
        fun(Value) ->
            lists:foreach(
                fun(Signed) ->
                    Msg =
                        case Signed of
                            true -> signed(#{ <<"payload">> => Value }, Opts);
                            false -> #{ <<"payload">> => Value }
                        end,
                    Direct =
                        success(
                            hb_http:post(
                                maps:get(url, First),
                                <<"/~tunnel-test@1.0/echo">>, Msg, Opts
                            ),
                            Opts
                        ),
                    ?assertEqual(
                        hb_private:reset(Value),
                        hb_private:reset(hb_maps:get(<<"body">>, Direct, Opts))
                    ),
                    Res =
                        success(
                            post(Ctx, First, <<"/~tunnel-test@1.0/echo">>, Msg),
                            Opts
                        ),
                    ?assertEqual(
                        hb_private:reset(Value),
                        hb_private:reset(hb_maps:get(<<"body">>, Res, Opts))
                    ),
                    % The tunneled reply has the signers of the direct reply.
                    ?assertEqual(
                        hb_message:signers(Direct, Opts),
                        hb_message:signers(Res, Opts)
                    )
                end,
                [false, true]
            )
        end,
        Values
    ),
    % A list that a response hook gives is the `body' of the reply.
    Current = hb_http_server:get_opts(maps:get(opts, First)),
    hb_http_server:set_opts(
        Current#{
            <<"on">> => (hb_maps:get(<<"on">>, Current))#{
                <<"response">> => #{
                    <<"device">> => #{ <<"response">> => fun list_response/3 }
                }
            }
        }
    ),
    lists:foreach(
        fun(Result) ->
            ?assertEqual(
                [1, <<"two">>, #{ <<"three">> => 3 }],
                hb_maps:get(<<"body">>, success(Result, Opts), not_found, Opts)
            )
        end,
        [
            hb_http:get(
                maps:get(url, First),
                <<"/~tunnel-test@1.0/list">>,
                Opts
            ),
            get(Ctx, First, <<"/~tunnel-test@1.0/list">>)
        ]
    ),
    drained(Ctx).

%% @doc Verify the same linked fixture through the origin's direct listener.
linked_direct(#{first := First, caller := Opts}) ->
    Result =
        success(
            hb_http:get(
                maps:get(url, First),
                <<"/~tunnel-test@1.0/linked">>,
                Opts
            ),
            Opts
        ),
    ?assertEqual(
        <<"origin-only">>,
        hb_ao:get(<<"body/nested/value">>, Result, Opts)
    ).

%% @doc Materialize links at their owning node in both directions.
linked_values(Ctx = #{broker := Broker, first := First, caller := Opts}) ->
    connect(Broker, First, 2),
    {ok, ID} =
        hb_cache:write(
            #{ <<"nested">> => #{ <<"value">> => <<"caller-only">> } }, Opts
        ),
    Res =
        success(
            post(
                Ctx, First, <<"/~tunnel-test@1.0/echo">>,
                #{ <<"payload">> => hb_link:decode(<<"ao:", ID/binary>>) }
            ),
            Opts
        ),
    ?assertEqual(
        <<"caller-only">>,
        hb_ao:get(<<"body/nested/value">>, Res, Opts)
    ),
    lists:foreach(
        fun(_) ->
            Loaded =
                success(get(Ctx, First, <<"/~tunnel-test@1.0/linked">>), Opts),
            ?assertEqual(
                <<"origin-only">>,
                hb_ao:get(<<"body/nested/value">>, Loaded, Opts)
            )
        end,
        lists:seq(1, 3)
    ).

%% @doc Leave codec and bundling negotiation to HyperBEAM's HTTP layer.
negotiation(Ctx = #{broker := Broker, first := First, caller := Opts}) ->
    connect(Broker, First, 2),
    lists:foreach(
        fun(Codec) ->
            lists:foreach(
                fun(Bundle) ->
                    Res =
                        success(
                            hb_http:get(
                                maps:get(url, Broker),
                                #{
                                    <<"path">> =>
                                        path(
                                            <<"/~meta@1.0/info/address">>,
                                            First
                                        ),
                                    <<"accept-bundle">> => Bundle,
                                    <<"require-codec">> => Codec
                                },
                                Opts
                            ),
                            Opts
                        ),
                    ?assertEqual(
                        maps:get(id, First), hb_maps:get(<<"body">>, Res, Opts)
                    )
                end,
                [false, true]
            )
        end,
        [<<"httpsig@1.0">>, <<"json@1.0">>, <<"ans104@1.0">>]
    ),
    drained(Ctx).

%% @doc Compare native browser bytes as well as decoded AO-Core responses.
http_semantics(Ctx = #{broker := Broker, first := First, caller := Opts}) ->
    connect(Broker, First, 4),
    lists:foreach(
        fun(Code) ->
            Req =
                <<
                    "/~tunnel-test@1.0/reply?code=",
                    (integer_to_binary(Code))/binary
                >>,
            Direct =
                response(hb_http:get(maps:get(url, First), Req, Opts), Opts),
            ?assertEqual(
                Code,
                hb_maps:get(<<"status">>, Direct, not_found, Opts)
            ),
            Tunnel = response(get(Ctx, First, Req), Opts),
            ?assertEqual(
                Code,
                hb_maps:get(<<"status">>, Tunnel, not_found, Opts)
            ),
            lists:foreach(
                fun(Key) ->
                    ?assertEqual(
                        hb_maps:get(Key, Direct, not_found, Opts),
                        hb_maps:get(Key, Tunnel, not_found, Opts)
                    )
                end,
                [<<"body">>, <<"location">>, <<"content-type">>]
            ),
            {Code, DirectHeaders, DirectBody} = raw_get(First, Req),
            {Code, TunnelHeaders, TunnelBody} =
                raw_get(Broker, path(Req, First)),
            case Code of
                Error when Error == 403; Error == 404; Error == 500 ->
                    % Native error pages include the request and commitments.
                    Marker = case Error of
                        403 -> <<"Access denied.">>;
                        404 -> <<"Page cannot be found.">>;
                        500 -> <<"500 - Oops.">>
                    end,
                    lists:foreach(
                        fun({Headers, Body}) ->
                            ?assertMatch(
                                "text/html" ++ _,
                                proplists:get_value("content-type", Headers)
                            ),
                            ?assertNotEqual({Code, Marker, nomatch},
                                {Code, Marker, binary:match(Body, Marker)})
                        end,
                        [
                            {DirectHeaders, DirectBody},
                            {TunnelHeaders, TunnelBody}
                        ]
                    );
                _ -> ?assertEqual(DirectBody, TunnelBody)
            end,
            ?assertEqual(
                lists:sort(
                    proplists:get_all_values("set-cookie", DirectHeaders)
                ),
                lists:sort(
                    proplists:get_all_values("set-cookie", TunnelHeaders)
                )
            )
        end,
        [200, 201, 204, 307, 400, 403, 404, 500, 503]
    ),
    {200, Headers, Body} =
        raw_get(Broker, path(<<"/~tunnel-test@1.0/asset">>, First)),
    ?assertEqual(<<"<!doctype html><title>Tunnel</title><p>ready</p>">>, Body),
    ?assertEqual("text/html", proplists:get_value("content-type", Headers)),
    {200, CookieHeaders, CookieBody} =
        raw_get(Broker, path(<<"/~tunnel-test@1.0/reply">>, First)),
    ?assertEqual(
        2,
        length(proplists:get_all_values("set-cookie", CookieHeaders))
    ),
    ?assertEqual(
        "text/plain",
        proplists:get_value("content-type", CookieHeaders)
    ),
    ?assertEqual(<<"fixture response">>, CookieBody),
    drained(Ctx).

%% @doc Read raw HTTP with no HyperBEAM-specific content negotiation.
raw_get(#{url := URL}, Path) ->
    BaseURL = string:trim(URL, trailing, "/"),
    {ok, {{_, Status, _}, Headers, Body}} =
        httpc:request(
            get,
            {binary_to_list(<<BaseURL/binary, Path/binary>>), []},
            [{autoredirect, false}, {timeout, 20_000}],
            [{body_format, binary}]
        ),
    {Status, Headers, Body}.

%% @doc Reject unsigned configuration changes through direct and tunneled paths.
authorization(Ctx = #{
    broker := Broker, first := First, second := Second, caller := Opts
}) ->
    URL = maps:get(url, Broker),
    Register =
        #{ <<"address">> => maps:get(id, First), <<"tunnel-timeout">> => 10 },
    lists:foreach(
        fun(Msg) ->
            Res =
                response(
                    hb_http:post(URL, <<"/~tunnel@1.0/register">>, Msg, Opts),
                    Opts
                ),
            ?assertEqual(403, hb_maps:get(<<"status">>, Res, not_found, Opts))
        end,
        [Register, signed(Register, Opts)]
    ),
    Unsigned =
        response(
            hb_http:post(
                maps:get(url, First), <<"/~tunnel@1.0/connect">>,
                #{ <<"peer">> => URL, <<"workers">> => 1 }, Opts
            ),
            Opts
        ),
    ?assertEqual(403, hb_maps:get(<<"status">>, Unsigned, not_found, Opts)),
    OwnerOpts = maps:get(opts, First),
    % Code that runs without the node's wallet, as a request does, must sign.
    ?assertMatch(
        {error, #{ <<"status">> := 403 }},
        hb_ao:resolve(
            #{ <<"device">> => <<"tunnel@1.0">> },
            #{
                <<"path">> => <<"connect">>,
                <<"peer">> => URL,
                <<"workers">> => 3
            },
            maps:without([<<"priv-wallet">>, <<"identities">>], OwnerOpts)
        )
    ),
    Signed = signed(#{<<"peer">> => URL, <<"workers">> => 2}, OwnerOpts),
    success(
        hb_http:post(
            maps:get(url, First),
            <<"/~tunnel@1.0/connect">>,
            Signed,
            Opts
        ),
        Opts
    ),
    await(fun() -> registered(URL, maps:get(id, First), Opts) =:= 2 end),
    PeerOnly =
        (signed(#{ <<"peer">> => URL }, OwnerOpts))#{ <<"workers">> => 9 },
    Kept =
        success(
            hb_http:post(
                maps:get(url, First), <<"/~tunnel@1.0/connect">>, PeerOnly, Opts
            ),
            Opts
        ),
    ?assertEqual(1, hb_maps:get(<<"workers">>, Kept, Opts)),
    connect(Broker, First, 2),
    Control = #{<<"peer">> => maps:get(url, Second), <<"workers">> => 1},
    Explicit =
        response(post(Ctx, First, <<"/~tunnel@1.0/connect">>, Control), Opts),
    ?assertEqual(403, hb_maps:get(<<"status">>, Explicit, not_found, Opts)),
    Hosted =
        response(
            hb_http:post(
                URL,
                Control#{
                    <<"path">> => <<"/~tunnel@1.0/connect">>,
                    <<"host">> => host(maps:get(id, First))
                },
                Opts
            ),
            Opts
        ),
    ?assertEqual(403, hb_maps:get(<<"status">>, Hosted, not_found, Opts)),
    OriginStatus = status(maps:get(url, First), Opts),
    ?assertEqual(
        1, hb_maps:get(<<"open-client-pools">>, OriginStatus, not_found, Opts)
    ),
    ?assertEqual(
        2, hb_maps:get(<<"open-client-workers">>, OriginStatus, not_found, Opts)
    ).

%% @doc Check that only the origin of a dispatched request can answer it, and
%% only once.
response_auth(Ctx = #{broker := Broker, first := First, caller := Opts}) ->
    URL = maps:get(url, Broker),
    Owner = maps:get(opts, First),
    Register =
        signed(
            #{
                <<"address">> => maps:get(id, First),
                <<"tunnel-timeout">> => 5_000
            },
            Owner
        ),
    Reg =
        async(
            fun() ->
                hb_http:post(URL, <<"/~tunnel@1.0/register">>, Register, Owner)
            end
        ),
    await(fun() -> registered(URL, maps:get(id, First), Opts) =:= 1 end),
    Call = async(fun() -> get(Ctx, First, <<"/~meta@1.0/info/address">>) end),
    Work = success(collect(Reg), Owner),
    ID = hb_maps:get(<<"tunnel-request-id">>, Work, not_found, Owner),
    ?assertEqual(
        maps:get(id, First),
        hb_maps:get(<<"tunnel-target">>, Work, not_found, Owner)
    ),
    ?assert(is_map(hb_maps:get(<<"request">>, Work, not_found, Owner))),
    Inner =
        signed(
            #{<<"status">> => 200, <<"body">> => <<"authenticated">>},
            Owner
        ),
    Envelope =
        #{
            <<"tunnel-request-id">> => ID,
            <<"result-status">> => ok,
            <<"response">> => Inner
        },
    lists:foreach(
        fun(Msg) ->
            Denied =
                response(
                    hb_http:post(URL, <<"/~tunnel@1.0/response">>, Msg, Opts),
                    Opts
                ),
            ?assertEqual(
                403,
                hb_maps:get(<<"status">>, Denied, not_found, Opts)
            )
        end,
        [
            Envelope,
            signed(Envelope, Opts),
            (signed(#{ <<"tunnel-request-id">> => ID }, Owner))#{
                <<"response">> => Inner
            }
        ]
    ),
    Accepted =
        success(
            hb_http:post(
                URL, <<"/~tunnel@1.0/response">>, signed(Envelope, Owner), Owner
            ),
            Owner
        ),
    ?assertEqual(202, hb_maps:get(<<"status">>, Accepted, not_found, Owner)),
    ?assertEqual(
        <<"authenticated">>,
        hb_maps:get(<<"body">>, success(collect(Call), Opts), not_found, Opts)
    ),
    Late =
        response(
            hb_http:post(
                URL, <<"/~tunnel@1.0/response">>, signed(Envelope, Owner), Owner
            ),
            Owner
        ),
    ?assertEqual(404, hb_maps:get(<<"status">>, Late, not_found, Owner)),
    drained(Ctx).

%% @doc Check that a request for an absent host falls through, and that an
%% explicit request waits until its timeout.
unavailable(Ctx = #{broker := Broker, first := First, caller := Opts}) ->
    Missing =
        response(
            hb_http:get(
                maps:get(url, Broker),
                #{
                    <<"path">> => <<"/~meta@1.0/info/address">>,
                    <<"host">> => host(maps:get(id, First))
                },
                Opts
            ),
            Opts
        ),
    ?assertEqual(200, hb_maps:get(<<"status">>, Missing, not_found, Opts)),
    ?assertEqual(maps:get(id, Broker), hb_maps:get(<<"body">>, Missing, Opts)),
    Invalid =
        response(
            hb_http:get(
                maps:get(url, Broker),
                <<"/~meta@1.0/info/address?tunnel-target=invalid">>, Opts
            ),
            Opts
        ),
    ?assertEqual(400, hb_maps:get(<<"status">>, Invalid, not_found, Opts)),
    {Micros, TimedRaw} =
        timer:tc(
            fun() ->
                get(
                    Ctx,
                    First,
                    <<"/~meta@1.0/info/address?tunnel-timeout=50">>
                )
            end
        ),
    Timed = response(TimedRaw, Opts),
    ?assertEqual(504, hb_maps:get(<<"status">>, Timed, not_found, Opts)),
    ?assert(Micros < 2_000_000),
    drained(Ctx),
    connect(Broker, First, 1),
    success(get(Ctx, First, <<"/~meta@1.0/info/address">>), Opts).

%% @doc Keep payload, wallet and worker isolation across concurrent origins.
concurrent(Ctx = #{
    broker := Broker, first := First, second := Second, caller := Opts
}) ->
    connect(Broker, First, 4),
    connect(Broker, Second, 4),
    Jobs =
        [
            {N, async(fun() ->
                Origin = case N rem 2 of 0 -> First; 1 -> Second end,
                post(
                    Ctx,
                    Origin,
                    <<"/~tunnel-test@1.0/echo">>,
                    #{ <<"payload">> => N }
                )
            end)}
        ||
            N <- lists:seq(1, 96)
        ],
    lists:foreach(
        fun({N, Ref}) ->
            Reply = success(collect(Ref), Opts),
            ?assertEqual(N, hb_maps:get(<<"body">>, Reply, not_found, Opts))
        end,
        Jobs
    ),
    drained(Ctx),
    % A worker registers again after it posts its last response.
    await(
        fun() ->
            registered(maps:get(url, Broker), maps:get(id, First), Opts) =:= 4
                andalso
                    registered(
                        maps:get(url, Broker), maps:get(id, Second), Opts
                    ) =:= 4
        end
    ).

%% @doc Check that a six-second write finishes, and runs once.
long_running(Ctx = #{broker := Broker, first := First, caller := Opts}) ->
    connect(Broker, First, 2),
    Res =
        success(
            post(
                Ctx, First, <<"/~tunnel-test@1.0/write">>,
                #{ <<"duration">> => 6_000 }
            ),
            Opts
        ),
    ?assertEqual(1, hb_maps:get(<<"body">>, Res, not_found, Opts)),
    ?assertEqual(1, writes(First)),
    drained(Ctx).

%% @doc Check that a rejected response does not run its write again.
rejected_response(Ctx = #{
    broker := Broker, first := First, caller := Opts, reject := Reject
}) ->
    connect(Broker, First, 2),
    atomics:put(Reject, 1, 1),
    Res =
        response(
            post(
                Ctx,
                First,
                <<"/~tunnel-test@1.0/write?tunnel-timeout=6500">>,
                #{}
            ),
            Opts
        ),
    ?assertEqual(504, hb_maps:get(<<"status">>, Res, not_found, Opts)),
    ?assertEqual(1, writes(First)),
    atomics:put(Reject, 1, 0),
    drained(Ctx),
    success(get(Ctx, First, <<"/~meta@1.0/info/address">>), Opts),
    % A response that the broker fails once is posted again and delivered.
    atomics:put(Reject, 1, 2),
    ?assertEqual(
        maps:get(id, First),
        hb_maps:get(
            <<"body">>,
            success(get(Ctx, First, <<"/~meta@1.0/info/address">>), Opts),
            not_found,
            Opts
        )
    ).

%% @doc Return the number of times that an origin has run the `write' fixture.
writes(#{opts := Opts}) ->
    atomics:get(hb_maps:get(<<"priv-test-writes">>, Opts), 1).

%% @doc Check that a caller that closes its connection leaves no request once
%% its execution ends.
disconnect(Ctx = #{broker := Broker, first := First, caller := Opts}) ->
    URL = maps:get(url, Broker),
    Socket = socket(URL, path(<<"/~meta@1.0/info/address">>, First)),
    await(
        fun() ->
            hb_maps:get(
                <<"open-requests">>,
                status(URL, Opts),
                not_found,
                Opts
            ) =:= 1
        end
    ),
    gen_tcp:close(Socket),
    drained(Ctx),
    connect(Broker, First, 2),
    Started =
        socket(URL, path(<<"/~tunnel-test@1.0/write?duration=250">>, First)),
    await(fun() -> writes(First) =:= 1 end),
    gen_tcp:close(Started),
    drained(Ctx),
    await(fun() -> registered(URL, maps:get(id, First), Opts) =:= 2 end),
    success(get(Ctx, First, <<"/~meta@1.0/info/address">>), Opts),
    ?assertEqual(1, writes(First)),
    Infinite =
        socket(
            URL,
            path(
                <<
                    "/~tunnel-test@1.0/write?duration=250"
                    "&tunnel-timeout=infinity"
                >>,
                First
            )
        ),
    await(fun() -> writes(First) =:= 2 end),
    gen_tcp:close(Infinite),
    drained(Ctx).

%% @doc Check that a target stays present while it runs the request of a
%% caller that closed its connection.
disconnected_busy_host(Ctx = #{
    broker := Broker, first := First, caller := Opts
}) ->
    connect(Broker, First, 1),
    URL = maps:get(url, Broker),
    Started =
        socket(URL, path(<<"/~tunnel-test@1.0/write?duration=2000">>, First)),
    await(fun() -> writes(First) =:= 1 end),
    gen_tcp:close(Started),
    ID = maps:get(id, First),
    Reply =
        success(
            hb_http:get(
                URL,
                #{
                    <<"path">> => <<"/~meta@1.0/info/address">>,
                    <<"host">> => host(ID)
                },
                Opts
            ),
            Opts
        ),
    ?assertEqual(ID, hb_maps:get(<<"body">>, Reply, not_found, Opts)),
    ?assertEqual(1, writes(First)),
    drained(Ctx).

%% @doc Open a real HTTP caller whose lifetime the test controls.
socket(URL, Path) ->
    #{port := Port} = uri_string:parse(URL),
    {ok, Socket} =
        gen_tcp:connect("localhost", Port, [binary, {active, false}], 5_000),
    ok =
        gen_tcp:send(
            Socket,
            [
                <<"GET ">>, Path,
                <<" HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n">>
            ]
        ),
    Socket.

%% @doc Check that the registrations of a stopped pool end, and that it can
%% connect again.
restart(Ctx = #{broker := Broker, first := First, caller := Opts}) ->
    connect(Broker, First, 2),
    success(get(Ctx, First, <<"/~meta@1.0/info/address">>), Opts),
    dev_tunnel_server:stop(maps:get(id, First)),
    await(
        fun() ->
            registered(maps:get(url, Broker), maps:get(id, First), Opts) =:= 0
        end
    ),
    connect(Broker, First, 2),
    success(get(Ctx, First, <<"/~meta@1.0/info/address">>), Opts),
    drained(Ctx).

%% @doc Check that two listeners in one BEAM share no registrations or
%% requests.
broker_isolation(Ctx = #{broker := Broker, first := First, caller := Opts}) ->
    Other =
        start_node(
            #{
                <<"on">> => #{
                    <<"request">> => #{
                        <<"device">> => <<"tunnel@1.0">>,
                        <<"path">> => <<"request">>
                    }
                }
            }
        ),
    try
        connect(Broker, First, 2),
        ?assertEqual(
            0,
            registered(maps:get(url, Other), maps:get(id, First), Opts)
        ),
        success(get(Ctx, First, <<"/~meta@1.0/info/address">>), Opts),
        ?assertEqual(
            0,
            hb_maps:get(
                <<"open-requests">>, status(maps:get(url, Other), Opts),
                not_found, Opts
            )
        )
    after stop_node(Other)
    end.

%% @doc Schedule through the tunnel with `httpsig@1.0' commitments.
scheduler_httpsig(Ctx) -> scheduler(Ctx, <<"httpsig@1.0">>).

%% @doc Schedule through the tunnel with `ans104@1.0' commitments.
scheduler_ans104(Ctx) -> scheduler(Ctx, <<"ans104@1.0">>).

%% @doc Observe real persisted scheduler slots without any public upload route.
scheduler(Ctx = #{broker := Broker, first := First, caller := Opts}, Codec) ->
    OriginOpts = (maps:get(opts, First))#{
        <<"bundler-ans104">> => maps:get(url, Broker),
        <<"bundler-httpsig">> => maps:get(url, Broker),
        <<"scheduler-default-commitment-spec">> => <<"httpsig@1.0">>,
        <<"scheduling-mode">> => local_confirmation
    },
    hb_http_server:set_opts(OriginOpts),
    connect(Broker, First, 2),
    Process =
        hb_message:commit(
            #{
                <<"device">> => <<"scheduler@1.0">>,
                <<"type">> => <<"Process">>,
                <<"scheduler-location">> => maps:get(id, First),
                <<"test-seed">> =>
                    hb_util:human_id(crypto:strong_rand_bytes(32))
            },
            Opts,
            #{ <<"commitment-device">> => Codec, <<"bundle">> => true }
        ),
    ProcID = hb_message:id(Process, all, Opts),
    Scheduled =
        post(
            Ctx, First, <<"/~scheduler@1.0/schedule">>,
            signed(#{ <<"body">> => Process }, Opts)
        ),
    timer:sleep(5_200),
    Slot =
        success(
            hb_http:get(
                maps:get(url, First),
                #{
                    <<"path">> => <<"/~scheduler@1.0/slot">>,
                    <<"target">> => ProcID
                },
                Opts
            ),
            Opts
        ),
    ?assertEqual(0, hb_maps:get(<<"current">>, Slot, not_found, Opts)),
    Assignment = success(Scheduled, Opts),
    Body = hb_maps:get(<<"body">>, Assignment, not_found, Opts),
    ?assertEqual(ProcID, hb_message:id(Body, all, Opts)),
    Message =
        hb_message:commit(
            #{
                <<"type">> => <<"Message">>, <<"target">> => ProcID,
                <<"body">> => <<"one scheduled message">>
            },
            Opts,
            #{ <<"commitment-device">> => Codec, <<"bundle">> => true }
        ),
    Assigned =
        success(
            post(
                Ctx, First, <<"/~scheduler@1.0/schedule">>,
                signed(#{ <<"body">> => Message }, Opts)
            ),
            Opts
        ),
    ?assertEqual(
        hb_message:id(Message, all, Opts),
        hb_message:id(
            hb_maps:get(<<"body">>, Assigned, not_found, Opts),
            all,
            Opts
        )
    ),
    Last =
        success(
            hb_http:get(
                maps:get(url, First),
                #{
                    <<"path">> => <<"/~scheduler@1.0/slot">>,
                    <<"target">> => ProcID
                },
                Opts
            ),
            Opts
        ),
    ?assertEqual(1, hb_maps:get(<<"current">>, Last, not_found, Opts)),
    drained(Ctx).

%% @doc Check that `connect' resizes one pool, also while it runs requests,
%% and stops it with no workers.
pool_resize(Ctx = #{broker := Broker, first := First, caller := Opts}) ->
    connect(Broker, First, 1),
    connect(Broker, First, 4),
    connect(Broker, First, 4),
    Jobs = [
        async(fun() ->
            post(
                Ctx, First, <<"/~tunnel-test@1.0/write">>,
                #{ <<"duration">> => 250 }
            )
        end)
    || _ <- lists:seq(1, 4)
    ],
    await(fun() -> writes(First) =:= 4 end),
    connect(Broker, First, 2),
    lists:foreach(fun(Job) -> success(collect(Job), Opts) end, Jobs),
    await(
        fun() ->
            Current = status(maps:get(url, First), Opts),
            hb_maps:get(<<"open-client-workers">>, Current, Opts) =:= 2
                andalso
                    hb_maps:get(<<"retiring-client-workers">>, Current, Opts)
                        =:= 0
        end
    ),
    ?assertEqual(4, writes(First)),
    success(get(Ctx, First, <<"/~meta@1.0/info/address">>), Opts),
    drained(Ctx),
    % A pool of no workers stops once its workers end their registrations.
    connect(Broker, First, 0),
    await(
        fun() ->
            Current = status(maps:get(url, First), Opts),
            [0, 0, 0] =:=
                [
                    hb_maps:get(Key, Current, Opts)
                ||
                    Key <-
                        [
                            <<"open-client-pools">>,
                            <<"open-client-workers">>,
                            <<"retiring-client-workers">>
                        ]
                ]
        end
    ).

%% @doc Check that a worker killed during a write is replaced, and that the
%% write does not run again.
worker_loss(Ctx = #{broker := Broker, first := First, caller := Opts}) ->
    connect(Broker, First, 1),
    Job =
        async(
            fun() ->
                post(
                    Ctx,
                    First,
                    <<"/~tunnel-test@1.0/write?tunnel-timeout=2500">>,
                    #{ <<"duration">> => 6_000 }
                )
            end
        ),
    await(fun() -> writes(First) =:= 1 end),
    Owner = hb_name:lookup({dev_tunnel_server, maps:get(id, First)}),
    {links, [Worker]} = process_info(Owner, links),
    exit(Worker, kill),
    Failed = response(collect(Job), Opts),
    ?assertEqual(504, hb_maps:get(<<"status">>, Failed, Opts)),
    await(
        fun() ->
            registered(maps:get(url, Broker), maps:get(id, First), Opts) =:= 1
        end
    ),
    success(get(Ctx, First, <<"/~meta@1.0/info/address">>), Opts),
    ?assertEqual(1, writes(First)),
    drained(Ctx).

%% @doc Check that a broker that restarts releases its callers, and that
%% workers register again.
broker_restart(Ctx = #{broker := Broker, first := First, caller := Opts}) ->
    Pending =
        async(fun() -> get(Ctx, First, <<"/~meta@1.0/info/address">>) end),
    await(
        fun() ->
            hb_maps:get(
                <<"open-requests">>, status(maps:get(url, Broker), Opts), Opts
            ) =:= 1
        end
    ),
    dev_tunnel_server:stop(maps:get(id, Broker)),
    ?assertEqual(
        503, hb_maps:get(<<"status">>, response(collect(Pending), Opts), Opts)
    ),
    connect(Broker, First, 2),
    dev_tunnel_server:stop(maps:get(id, Broker)),
    await(
        fun() ->
            registered(maps:get(url, Broker), maps:get(id, First), Opts) =:= 2
        end
    ),
    success(get(Ctx, First, <<"/~meta@1.0/info/address">>), Opts),
    drained(Ctx).

%% @doc Restart the actual origin listener with its wallet and store intact.
persisted_restart(Ctx = #{broker := Broker, first := First, caller := Opts}) ->
    #{ id := ID, opts := OriginOpts } = First,
    Message = signed(#{ <<"body">> => <<"retained across restart">> }, Opts),
    {ok, CachedID} = hb_cache:write(Message, OriginOpts),
    connect(Broker, First, 2),
    Read = <<"/~cache@1.0/read?read=", CachedID/binary>>,
    Before = success(get(Ctx, First, Read), Opts),
    dev_tunnel_server:stop(ID),
    cowboy:stop_listener(ID),
    URL = hb_http_server:start_node(OriginOpts#{ <<"port">> => 0 }),
    Restarted = First#{
        url => URL,
        opts => hb_http_server:get_opts(#{ <<"http-server">> => ID })
    },
    await(fun() -> registered(maps:get(url, Broker), ID, Opts) =:= 0 end),
    connect(Broker, Restarted, 2),
    After = success(get(Ctx, Restarted, Read), Opts),
    ?assertEqual(
        hb_maps:get(<<"body">>, Before, Opts),
        hb_maps:get(<<"body">>, After, Opts)
    ),
    ?assertEqual(
        ID,
        hb_maps:get(
            <<"body">>,
            success(get(Ctx, Restarted, <<"/~meta@1.0/info/address">>), Opts),
            Opts
        )
    ),
    drained(Ctx).

%% @doc Check that rounds of signed and unsigned requests to two origins
%% drain, and leave the broker as they found it.
mixed_load(Ctx = #{
    broker := Broker, first := First, second := Second, caller := Opts
}) ->
    connect(Broker, First, 4),
    connect(Broker, Second, 4),
    BrokerPID = hb_name:lookup({dev_tunnel_server, maps:get(id, Broker)}),
    erlang:garbage_collect(BrokerPID),
    {memory, BeforeMemory} = process_info(BrokerPID, memory),
    lists:foreach(
        fun(Round) ->
            Jobs = [
                {N, async(fun() ->
                    Node = case N rem 2 of 0 -> First; 1 -> Second end,
                    case N rem 4 of
                        0 ->
                            get(
                                Ctx,
                                Node,
                                <<"/~tunnel-test@1.0/reply?code=503">>
                            );
                        1 -> get(Ctx, Node, <<"/~tunnel-test@1.0/linked">>);
                        _ ->
                            post(
                                Ctx, Node, <<"/~tunnel-test@1.0/echo">>,
                                signed(#{ <<"payload">> => [Round, N] }, Opts)
                            )
                    end
                end)}
            || N <- lists:seq(1, 24)
            ],
            lists:foreach(
                fun({N, Job}) ->
                    Reply = response(collect(Job), Opts),
                    case N rem 4 of
                        0 ->
                            ?assertEqual(
                                503,
                                hb_maps:get(<<"status">>, Reply, Opts)
                            );
                        1 ->
                            ?assertEqual(
                                <<"origin-only">>,
                                hb_ao:get(<<"body/nested/value">>, Reply, Opts)
                            );
                        _ ->
                            ?assertEqual(
                                [Round, N], hb_maps:get(<<"body">>, Reply, Opts)
                            )
                    end
                end,
                Jobs
            ),
            drained(Ctx)
        end,
        lists:seq(1, 10)
    ),
    success(get(Ctx, First, <<"/~meta@1.0/info/address">>), Opts),
    success(get(Ctx, Second, <<"/~meta@1.0/info/address">>), Opts),
    await(
        fun() ->
            registered(maps:get(url, Broker), maps:get(id, First), Opts) =:= 4
                andalso
                    registered(
                        maps:get(url, Broker), maps:get(id, Second), Opts
                    ) =:= 4
        end
    ),
    drained(Ctx),
    erlang:garbage_collect(BrokerPID),
    {memory, AfterMemory} = process_info(BrokerPID, memory),
    {monitors, Monitors} = process_info(BrokerPID, monitors),
    ?assertEqual(8, length(Monitors)),
    ?assert(AfterMemory < BeforeMemory + 64 * 1024),
    ?event({tunnel_soak_memory, BeforeMemory, AfterMemory}).

%% @doc Preserve exceptions from independent real HTTP callers for the
%% assertion.
async(Fun) ->
    Parent = self(),
    Ref = make_ref(),
    spawn(
        fun() ->
            Result =
                try Fun()
                catch Class:Reason:Stack -> {caught, Class, Reason, Stack}
                end,
            Parent ! {Ref, Result}
        end
    ),
    Ref.

%% @doc Re-raise a caller failure with its original stack.
collect(Ref) ->
    receive
        {Ref, {caught, Class, Reason, Stack}} ->
            erlang:raise(Class, Reason, Stack);
        {Ref, Result} -> Result
    after 20_000 -> error({'response-not-received', Ref})
    end.
