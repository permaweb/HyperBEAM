%%% @doc Signed HTTP observations across isolated HyperBEAM peers. Transport
%%% receipts describe attempts; admissible receipts describe selected replies
%%% after the caller's configured status and content checks.
-module(hb_http_receipt_examples).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Every backend reports success, HTTP failure and refused connections.
%% Retrying a refused connection produces one receipt for each attempt.
transport_test_() ->
    [
        {atom_to_list(Client), {timeout, 45, fun() -> transport(Client) end}}
    || Client <- [gun, hackney, httpc]
    ].

transport(Client) ->
    with_nodes(
        [#{}, response_opts(status)],
        fun([{Good, _}, {Bad, _}]) ->
            Ref = make_ref(),
            with_client(
                #{
                    <<"http-client">> => Client,
                    <<"http-reference">> => <<"configured-peer">>,
                    <<"on">> => observers(self(), Ref)
                },
                fun(Opts) ->
                    Path = <<"/~meta@1.0/info/address">>,
                    lists:foreach(
                        fun({Peer, ExpectedStatus, ExpectedClass}) ->
                            Start = erlang:monotonic_time(native),
                            Result = raw_get(Peer, Path, Opts),
                            Elapsed = erlang:monotonic_time(native) - Start,
                            ?assertMatch({ok, ExpectedStatus, _, _}, Result),
                            Receipt = receipt(Ref, transport, Opts),
                            check_transport(Receipt, Path, Opts),
                            ?assertEqual(
                                ExpectedClass,
                                hb_maps:get(<<"status-class">>, Receipt, Opts)
                            ),
                            ?assert(
                                hb_maps:get(<<"duration">>, Receipt, Opts)
                                    =< Elapsed
                            )
                        end,
                        [{Good, 200, <<"success">>},
                            {Bad, 503, <<"server-error">>}]
                    ),
                    ?assertMatch(
                        {error, _},
                        raw_get(
                            dead_node(),
                            Path,
                            Opts#{
                                <<"http-retry">> => 1,
                                <<"http-retry-time">> => 1
                            }
                        )
                    ),
                    lists:foreach(
                        fun(_) ->
                            Receipt = receipt(Ref, transport, Opts),
                            check_transport(Receipt, Path, Opts),
                            Class = hb_maps:get(<<"status-class">>, Receipt, Opts),
                            ?assert(is_binary(Class)),
                            ?assertNotEqual(<<"success">>, Class)
                        end,
                        [1, 2]
                    ),
                    no_receipt(Ref)
                end
            )
        end
    ).

%% @doc Both dispatch modes reject a different committed ID, a mutated signed
%% body, and valid content with a disallowed status. Only the good peer is
%% reported, even when the response itself claims another peer's reference.
admissibility_test_() ->
    [
        {atom_to_list(Mode) ++ "/" ++ atom_to_list(Bad),
            {timeout, 45, fun() -> admissibility(Mode, Bad) end}}
    || Mode <- [serial, parallel], Bad <- [wrong_id, tampered, status]
    ].

admissibility(Mode, BadKind) ->
    with_nodes(
        [#{}, response_opts(BadKind)],
        fun([{Good, GoodOpts}, {Bad, BadOpts}]) ->
            Signed =
                hb_message:commit(
                    #{
                        <<"body">> => <<"authentic content">>,
                        <<"child">> => #{ <<"body">> => <<"nested content">> },
                        <<"reference">> => <<"forged-reference">>,
                        <<"http-reference">> => <<"forged-reference">>
                    },
                    GoodOpts
                ),
            ID = hb_message:id(Signed, all, GoodOpts),
            {ok, _} = hb_cache:write(Signed, GoodOpts),
            install_bad(BadKind, Signed, ID, BadOpts),
            Ref = make_ref(),
            with_client(
                #{ <<"on">> => observers(self(), Ref) },
                fun(Opts) ->
                    Path = <<"/~cache@1.0/read=", ID/binary>>,
                    % Verify the rejected fixture over HTTP before racing it.
                    {BadStatus, BadResponse} =
                        hb_http:get(Bad, Path, Opts#{ <<"on">> => #{} }),
                    ?assertEqual(
                        case BadKind of status -> failure; _ -> ok end,
                        BadStatus
                    ),
                    ?assertEqual(
                        BadKind =/= wrong_id,
                        has_commitment(BadResponse, ID, Opts)
                    ),
                    ?assertEqual(
                        BadKind =/= tampered,
                        hb_message:verify(BadResponse, all, Opts)
                    ),
                    Request = #{
                        <<"path">> => Path,
                        <<"request-token">> =>
                            hb_util:human_id(crypto:strong_rand_bytes(32))
                    },
                    Config = #{
                        <<"nodes">> => [
                            peer(Bad, Path, <<"bad-peer">>),
                            peer(Good, Path, <<"good-peer">>)
                        ],
                        <<"parallel">> => Mode == parallel,
                        % Gather both candidates so rejection is exercised even
                        % when the good response arrives first in a parallel run.
                        <<"responses">> => 2,
                        <<"admissible-status">> => 200,
                        <<"admissible">> => admissible(ID)
                    },
                    lists:foreach(
                        fun(_) ->
                            [{ok, Result}] = hb_http:get(Config, Request, Opts),
                            ?assert(hb_message:verify(Result, all, Opts)),
                            ?assertEqual(
                                <<"authentic content">>,
                                hb_maps:get(<<"body">>, Result, Opts)
                            ),
                            Report = receipt(Ref, admissible, Opts),
                            check_signed(Report, <<"good-peer">>, Opts),
                            ?assertMatch(
                                #{
                                    <<"request-method">> := <<"GET">>,
                                    <<"request-path">> := Path
                                },
                                Report
                            ),
                            ?assertEqual(
                                Request,
                                hb_message:uncommitted(
                                    hb_maps:get(<<"request">>, Report, Opts)
                                )
                            ),
                            Response = hb_maps:get(<<"response">>, Report, Opts),
                            ?assertMatch(
                                #{
                                    <<"status">> := 200,
                                    <<"body">> := <<"authentic content">>,
                                    <<"reference">> := <<"forged-reference">>
                                },
                                Response
                            ),
                            ?assert(hb_message:verify(Response, all, Opts)),
                            ?assert(has_commitment(Response, ID, Opts)),
                            ?assertEqual(
                                <<"nested content">>,
                                hb_ao:get(<<"child/body">>, Response, Opts)
                            ),
                            Transports =
                                [receipt(Ref, transport, Opts) || _ <- [1, 2]],
                            BadClass =
                                case BadKind of
                                    status -> <<"server-error">>;
                                    _ -> <<"success">>
                                end,
                            ?assertEqual(
                                [{<<"bad-peer">>, BadClass},
                                    {<<"good-peer">>, <<"success">>}],
                                lists:sort(
                                    [
                                        {
                                            hb_maps:get(<<"reference">>, R, Opts),
                                            hb_maps:get(<<"status-class">>, R, Opts)
                                        }
                                    || R <- Transports
                                    ]
                                )
                            ),
                            lists:foreach(
                                fun(R) ->
                                    check_signed(
                                        R,
                                        hb_maps:get(<<"reference">>, R, Opts),
                                        Opts
                                    ),
                                    ?assertNot(
                                        hb_maps:is_key(<<"response">>, R, Opts)
                                    )
                                end,
                                Transports
                            ),
                            no_receipt(Ref)
                        end,
                        % Repeated requests must execute the observer each time.
                        [1, 2]
                    )
                end
            )
        end
    ).

%% @doc Chained native handlers receive Base and Req independently. A blocked,
%% then failing observer cannot delay or replace either HTTP result.
observer_pipeline_test_() ->
    {timeout, 30, fun observer_pipeline/0}.

observer_pipeline() ->
    with_nodes(
        [#{}],
        fun([{Peer, PeerOpts}]) ->
            Owner = self(),
            Ref = make_ref(),
            First = #{
                <<"path">> => <<"mark">>,
                <<"marker">> => <<"handler-base">>,
                <<"device">> => #{
                    mark =>
                        fun(Base, Req, Opts) ->
                            ?assertEqual(
                                <<"handler-base">>,
                                hb_maps:get(<<"marker">>, Base, Opts)
                            ),
                            {ok, Req#{ <<"chain-marker">> => <<"first-handler">> }}
                        end
                }
            },
            Last = #{
                <<"path">> => <<"observe">>,
                <<"device">> => #{
                    observe =>
                        fun(_Base, Req, Opts) ->
                            ?assertEqual(
                                <<"first-handler">>,
                                hb_maps:get(<<"chain-marker">>, Req, Opts)
                            ),
                            Owner !
                                {Ref, blocked, self(),
                                    hb_cache:ensure_all_loaded(
                                        hb_maps:get(<<"body">>, Req, Opts), Opts
                                    )
                                },
                            receive {Ref, release} -> error('observer-failed')
                            after 5000 -> error('observer-not-released')
                            end
                        end
                }
            },
            Pipeline = [First, Last, observer(Owner, Ref, unexpected)],
            with_client(
                #{
                    <<"http-reference">> => <<"selected-peer">>,
                    <<"on">> => #{
                        <<"http-client-receipt">> => Pipeline,
                        <<"http-client-admissible-receipt">> => Pipeline
                    }
                },
                fun(Opts) ->
                    Path = <<"/~meta@1.0/info/address">>,
                    {ok, Reply} =
                        hb_http:get(
                            #{
                                <<"nodes">> =>
                                    [peer(Peer, Path, <<"selected-peer">>)],
                                <<"admissible-status">> => 200
                            },
                            Path,
                            Opts
                        ),
                    ?assertEqual(address(PeerOpts), Reply),
                    lists:foreach(
                        fun(_) ->
                            receive
                                {Ref, blocked, PID, Report} ->
                                    MRef = erlang:monitor(process, PID),
                                    PID ! {Ref, release},
                                    check_signed(Report, <<"selected-peer">>, Opts),
                                    receive {'DOWN', MRef, process, PID, _} -> ok
                                    after 5000 -> error('observer-did-not-exit')
                                    end
                            after 5000 -> error('observer-did-not-start')
                            end
                        end,
                        [1, 2]
                    ),
                    QuietPeer = (peer(Peer, Path, <<"selected-peer">>))#{
                        <<"opts">> => #{
                            <<"on">> => #{
                                <<"http-client-receipt">> => [],
                                <<"http-client-admissible-receipt">> => []
                            }
                        }
                    },
                    ?assertEqual(
                        {ok, address(PeerOpts)},
                        hb_http:get(#{ <<"nodes">> => [QuietPeer] }, Path, Opts)
                    ),
                    no_receipt(Ref)
                end
            )
        end
    ).

%% @doc Native relay handlers can deliver the signed body to a real collector.
%% The relay handler sets `hook/recursive' to `false', so its request runs no
%% receipt hook, though the request's route has one.
collector_test_() ->
    {timeout, 30, fun collector/0}.

collector() ->
    Owner = self(),
    Ref = make_ref(),
    Collector = #{
        <<"on">> => #{
            <<"request">> => #{
                <<"device">> => #{
                    request =>
                        fun(_Base, Req, Opts) ->
                            Request = hb_maps:get(<<"request">>, Req, Opts),
                            Owner !
                                {Ref, collected,
                                    hb_cache:ensure_all_loaded(
                                        hb_maps:get(<<"body">>, Request, Opts),
                                        Opts
                                    )
                                },
                            {ok,
                                Req#{
                                    <<"body">> => [
                                        #{ <<"body">> => <<"received">> },
                                        #{ <<"path">> => <<"body">> }
                                    ]
                                }
                            }
                        end
                }
            }
        }
    },
    with_nodes(
        [#{}, Collector],
        fun([{Peer, PeerOpts}, {Sink, _}]) ->
            with_client(
                #{
                    <<"http-reference">> => <<"observed-peer">>,
                    <<"routes">> => [
                        #{
                            <<"template">> => <<"^/receipts$">>,
                            <<"node">> => #{
                                <<"prefix">> => Sink,
                                <<"opts">> => #{
                                    <<"on">> => #{
                                        <<"http-client-receipt">> =>
                                            observer(Owner, Ref, transport)
                                    }
                                }
                            }
                        }
                    ],
                    <<"on">> => #{
                        <<"http-client-receipt">> => [
                            #{
                                <<"device">> => <<"relay@1.0">>,
                                <<"path">> => <<"call">>,
                                <<"method">> => <<"POST">>,
                                <<"relay-route">> => <<"/receipts">>,
                                <<"hook">> => #{ <<"recursive">> => false }
                            },
                            observer(Owner, Ref, delivered)
                        ]
                    }
                },
                fun(Opts) ->
                    Path = <<"/~meta@1.0/info/address">>,
                    {ok, Reply} = hb_http:get(Peer, Path, Opts),
                    ?assertEqual(address(PeerOpts), Reply),
                    Report = receipt(Ref, collected, Opts),
                    check_signed(Report, <<"observed-peer">>, Opts),
                    ?assertEqual(
                        Path, hb_maps:get(<<"request-path">>, Report, Opts)
                    ),
                    ?assertEqual(
                        <<"duration">>, hb_maps:get(<<"path">>, Report, Opts)
                    ),
                    ?assertEqual(<<"received">>, receipt(Ref, delivered, Opts)),
                    % The relay's request runs no receipt hook of its route.
                    no_receipt(Ref)
                end
            )
        end
    ).

%% @doc Configure an AO predicate over the full decoded response, not its body.
admissible(ID) ->
    #{
        <<"expected-id">> => ID,
        <<"device">> => #{
            is_admissible =>
                fun(Base, Response, Opts) ->
                    Expected = hb_maps:get(<<"expected-id">>, Base, Opts),
                    {ok,
                        has_commitment(Response, Expected, Opts) andalso
                            hb_message:verify(Response, all, Opts)
                    }
                end
        }
    }.

%% @doc HTTP adds transport commitments without changing the requested one.
has_commitment(Response, ID, Opts) ->
    hb_maps:is_key(ID, hb_maps:get(<<"commitments">>, Response, #{}, Opts), Opts).

%% @doc Install real cache contents, optionally aliased under the requested ID.
install_bad(wrong_id, _Signed, ID, Opts) ->
    Wrong = hb_message:commit(#{ <<"body">> => <<"different content">> }, Opts),
    {ok, _} = hb_cache:write(Wrong, Opts),
    ok = hb_cache:link(hb_message:id(Wrong, all, Opts), ID, Opts);
install_bad(_Kind, Signed, _ID, Opts) ->
    {ok, _} = hb_cache:write(Signed, Opts).

%% @doc Exercise malformed content or a failing status on a genuine HB reply.
response_opts(wrong_id) -> #{};
response_opts(Kind) ->
    #{
        <<"on">> => #{
            <<"response">> => #{
                <<"device">> => #{
                    response =>
                        fun(_Base, Req, Opts) ->
                            Response = hb_maps:get(<<"body">>, Req, Opts),
                            {ok,
                                Req#{
                                    <<"body">> =>
                                        case Kind of
                                            tampered ->
                                                Response#{
                                                    <<"body">> =>
                                                        <<"tampered content">>
                                                };
                                            status ->
                                                Response#{ <<"status">> => 503 }
                                        end
                                }
                            }
                        end
                }
            }
        }
    }.

%% @doc Per-peer options are local attribution, independent of response fields.
peer(URL, <<"/", Path/binary>>, Reference) ->
    #{
        <<"uri">> => <<URL/binary, Path/binary>>,
        <<"opts">> => #{ <<"http-reference">> => Reference }
    }.

%% @doc Capture each hook's signed body via ordinary Base/Req hook resolution.
observers(Owner, Ref) ->
    #{
        <<"http-client-receipt">> => observer(Owner, Ref, transport),
        <<"http-client-admissible-receipt">> => observer(Owner, Ref, admissible)
    }.

observer(Owner, Ref, Kind) ->
    #{
        <<"path">> => <<"observe">>,
        <<"device">> => #{
            observe =>
                fun(_Base, Req, Opts) ->
                    Owner !
                        {Ref, Kind,
                            hb_cache:ensure_all_loaded(
                                hb_maps:get(<<"body">>, Req, Opts), Opts
                            )
                        },
                    {ok, Req}
                end
        }
    }.

%% @doc Wait for a particular asynchronous observation without shared state.
receipt(Ref, Kind, Opts) ->
    receive
        {Ref, Kind, Report} -> hb_cache:ensure_all_loaded(Report, Opts)
    after 5000 -> error({'missing-receipt', Kind})
    end.

%% @doc Rejected candidates and receipt deliveries must not add observations.
no_receipt(Ref) ->
    receive
        {Ref, Kind, Report} -> error({'unexpected-receipt', Kind, Report});
        {Ref, blocked, PID, Report} ->
            PID ! {Ref, release},
            error({'unexpected-receipt', blocked, Report})
    after 100 -> ok
    end.

%% @doc Both hook bodies are committed by the requesting client, not the peer.
check_signed(Receipt, Reference, Opts) ->
    ?assert(hb_message:verify(Receipt, all, Opts)),
    ?assertEqual(
        [hb_test_utils:committer(maps:get(<<"priv-wallet">>, Opts))],
        hb_message:signers(Receipt, Opts)
    ),
    ?assertEqual(Reference, hb_maps:get(<<"reference">>, Receipt, Opts)),
    ?assertEqual(<<"POST">>, hb_maps:get(<<"method">>, Receipt, Opts)).

%% @doc Transport receipts contain timings, never a content-acceptance claim.
check_transport(Receipt, Path, Opts) ->
    check_signed(Receipt, <<"configured-peer">>, Opts),
    ?assertEqual(<<"GET">>, hb_maps:get(<<"request-method">>, Receipt, Opts)),
    ?assertEqual(Path, hb_maps:get(<<"request-path">>, Receipt, Opts)),
    ?assertEqual(<<"duration">>, hb_maps:get(<<"path">>, Receipt, Opts)),
    Duration = hb_maps:get(<<"duration">>, Receipt, Opts),
    ?assert(is_integer(Duration) andalso Duration >= 0),
    lists:foreach(
        fun(Key) -> ?assertNot(hb_maps:is_key(Key, Receipt, Opts)) end,
        [<<"request">>, <<"response">>, <<"event">>]
    ).

%% @doc Use the real transport API, retaining HTTP status for these assertions.
raw_get(Peer, Path, Opts) ->
    hb_http_client:request(
        #{
            peer => Peer, path => Path, method => <<"GET">>,
            headers => #{}, body => <<>>
        },
        Opts
    ).

%% @doc Reserve then close an ephemeral port to obtain a refused connection.
dead_node() ->
    {ok, Socket} = gen_tcp:listen(0, []),
    {ok, Port} = inet:port(Socket),
    ok = gen_tcp:close(Socket),
    <<"http://127.0.0.1:", (integer_to_binary(Port))/binary>>.

%% @doc Isolate all stores, identities and listeners from other tests.
node_opts(Extra) ->
    maps:merge(
        #{
            <<"store">> => hb_test_utils:test_store(),
            <<"priv-wallet">> => ar_wallet:new(),
            <<"port">> => 0,
            <<"protocol">> => http1,
            <<"http-client">> => gun,
            <<"relay-http-client">> => gun,
            <<"http-retry">> => 0,
            <<"http-redirects">> => 0
        },
        Extra
    ).

address(Opts) ->
    hb_util:human_id(ar_wallet:to_address(maps:get(<<"priv-wallet">>, Opts))).

%% @doc The client owns a store and wallet, but does not need a listener.
with_client(Extra, Fun) ->
    Opts = node_opts(Extra),
    try Fun(Opts)
    after hb_store:stop(maps:get(<<"store">>, Opts))
    end.

%% @doc Nested cleanup also stops prior peers when a subsequent start fails.
with_nodes([], Fun) -> Fun([]);
with_nodes([Config | Rest], Fun) ->
    Opts = node_opts(Config),
    Node = hb_http_server:start_node(Opts),
    try
        with_nodes(Rest, fun(Nodes) -> Fun([{Node, Opts} | Nodes]) end)
    after
        cowboy:stop_listener(address(Opts)),
        hb_store:stop(maps:get(<<"store">>, Opts))
    end.
