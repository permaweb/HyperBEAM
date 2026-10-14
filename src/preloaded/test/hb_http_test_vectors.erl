%%% @doc A battery of test vectors for serving messages between nodes over
%%% HTTP. Each message is written to the cache of an isolated primary node, then
%%% read back over six paths: directly with `hb_http', through a
%%% `hb_store_remote_node' store, through a `~relay@1.0' node, from a node that
%%% received it by POST, and from a second or third node that downloaded and
%%% cached it. Each path is read with every codec, both `accept-bundle' settings
%%% and both `http-only-result' settings, from volatile, LMDB and filesystem
%%% primaries. Each read is checked against the message that was written: its
%%% values and types, its signatures and its IDs.
%%%
%%% By default, each message is read once with each transport, over a path and
%%% from a primary store that rotate between cases. Set
%%% `HB_HTTP_TEST_VECTORS=full' to read every combination. Run a single case
%%% with `eunit:test(hb_http_test_vectors:run(Name, Path, Transport, Store))'.
-module(hb_http_test_vectors).
-export([run/3, run/4]).
-include_lib("eunit/include/eunit.hrl").

%% @doc The messages that the primary node serves. A message given as a
%% function is built with the primary's options, so that it can be signed.
%% A message given as a pair is written as its first element and read back as
%% its second: a message written with explicit types or links, or with keys
%% that `structured@1.0' reads in lowercase.
messages() ->
    [
        {"basic", #{ <<"hello">> => <<"world">> }},
        {"body", #{ <<"body">> => <<"hello">> }},
        {"empty body", #{ <<"body">> => <<>> }},
        {"empty message", #{}},
        {"empty child", #{ <<"child">> => #{} }},
        {"empty list", #{ <<"items">> => [] }},
        {"empty root list", []},
        {"typed scalars", typed()},
        {"large integers",
            #{
                <<"positive">> => 9007199254740993,
                <<"negative">> => -9007199254740993
            }
        },
        {"binary body", #{ <<"body">> => <<0, 255, 1, 128>> }},
        {"escaped header", #{ <<"text">> => <<"a\nb\r\n\"c\"\\d">> }},
        {"edge whitespace", #{ <<"text">> => <<" \tvalue\t ">> }},
        {"unicode",
            #{ <<"text">> => <<"caf", 195, 169, 240, 159, 140, 141>> }
        },
        {"binary list", #{ <<"items">> => [<<"a">>, <<"b">>, <<>>] }},
        {"typed list", #{ <<"items">> => [1, 2.5, true, null, <<"x">>] }},
        {"root list", [1, <<"two">>, #{ <<"three">> => 3 }]},
        {"nested message",
            #{ <<"child">> => #{ <<"hello">> => <<"world">> } }
        },
        {"nested typed", #{ <<"child">> => typed() }},
        {"nested typed list",
            #{
                <<"items">> =>
                    [
                        [1, 2.5],
                        #{ <<"enabled">> => false },
                        [true, <<"x">>]
                    ]
            }
        },
        {"list of messages", #{ <<"items">> => [typed(), typed()] }},
        {"nested typed body", #{ <<"child">> => #{ <<"body">> => 42 } }},
        {"nested body and data",
            #{
                <<"child">> =>
                    #{
                        <<"body">> => <<"body bytes">>,
                        <<"data">> => <<"data bytes">>
                    }
            }
        },
        {"deep message",
            lists:foldl(
                fun(_, Msg) -> #{ <<"child">> => Msg } end,
                typed(),
                lists:seq(1, 6)
            )
        },
        {"wide message",
            maps:from_list([{hb_util:bin(N), N} || N <- lists:seq(1, 32)])
        },
        {"large body",
            #{ <<"body">> => binary:copy(<<"abcdefgh">>, 2048) }
        },
        {"large header", #{ <<"text">> => binary:copy(<<"x">>, 8192) }},
        {"megabyte body", #{ <<"body">> => binary:copy(<<"x">>, 1048577) }},
        {"header boundary",
            #{
                <<"at-limit">> => binary:copy(<<"x">>, 4096),
                <<"over-limit">> => binary:copy(<<"y">>, 4097)
            }
        },
        {"deep lists",
            #{
                <<"items">> =>
                    lists:foldl(
                        fun(_, Child) -> [Child, [], #{}, <<>>] end,
                        [typed()],
                        lists:seq(1, 8)
                    )
            }
        },
        {"wide lists",
            #{ <<"items">> => lists:duplicate(32, [typed(), [], #{}, <<>>]) }
        },
        {"deep empty values",
            lists:foldl(
                fun(_, Child) ->
                    #{
                        <<"child">> => Child,
                        <<"body">> => <<>>,
                        <<"map">> => #{},
                        <<"list">> => []
                    }
                end,
                #{},
                lists:seq(1, 6)
            )
        },
        {"empty commitments", #{ <<"commitments">> => #{}, <<"body">> => <<>> }},
        {"explicit types",
            {
                #{
                    <<"ao-types">> => <<"integer=(\"answer\")">>,
                    <<"answer">> => <<"42">>
                },
                #{ <<"answer">> => 42 }
            }
        },
        {"explicit link",
            fun(Opts) ->
                Child = typed(),
                {ok, ID} = hb_cache:write(Child, Opts),
                {
                    #{ <<"child">> => <<"ao:", ID/binary>> },
                    #{ <<"child">> => Child }
                }
            end
        },
        {"ID shaped binaries",
            fun(Opts) ->
                #{
                    <<"id">> => hb_message:id(typed(), all, Opts),
                    <<"raw-id">> => <<0:256>>
                }
            end
        },
        {"store markers",
            #{
                <<"group">> => <<"group">>,
                <<"link">> => <<"link:literal">>,
                <<"raw">> => <<"raw:literal">>
            }
        },
        {"reserved data keys",
            #{
                <<"signature">> => <<"data">>,
                <<"content-digest">> => <<"also data">>
            }
        },
        {"content type",
            #{
                <<"content-type">> => <<"text/plain">>,
                <<"body">> => <<"hello">>
            }
        },
        {"unsigned status",
            #{ <<"status">> => <<"pending">>, <<"value">> => 1 }
        },
        {"nested ans104",
            fun(Opts) ->
                #{ <<"child">> => signed(typed(), <<"ans104@1.0">>, Opts) }
            end
        },
        {"nested bundled ans104",
            fun(Opts) ->
                #{
                    <<"child">> =>
                        signed(nested(), bundle(<<"ans104@1.0">>), Opts)
                }
            end
        },
        {"double httpsig",
            fun(Opts) -> double(typed(), <<"httpsig@1.0">>, Opts) end
        },
        {"double bundled httpsig",
            fun(Opts) -> double(nested(), bundle(<<"httpsig@1.0">>), Opts) end
        },
        {"mixed signatures",
            fun(Opts) ->
                signed(
                    signed(typed(), <<"ans104@1.0">>, Opts),
                    <<"httpsig@1.0">>,
                    Opts
                )
            end
        },
        {"multiply signed child",
            fun(Opts) ->
                #{ <<"child">> => double(typed(), <<"httpsig@1.0">>, Opts) }
            end
        }
    ] ++
    [
        {
            Name ++ Suffix,
            {
                Wrap(#{ Key => <<"literal">> }),
                Wrap(#{ hb_util:lowercase_key(Key) => <<"literal">> })
            }
        }
    ||
        {Name, Key} <-
            [
                {"upper case key", <<"Upper">>},
                {"slash key", <<"a/b">>},
                {"percent key", <<"a%2fb">>},
                {"dot key", <<".">>},
                {"double dot key", <<"a..b">>},
                {"space key", <<"a b">>},
                {"colon key", <<"a:b">>},
                {"long key", binary:copy(<<"k">>, 200)},
                {"long escaped key", binary:copy(<<"A">>, 100)},
                {"path key", <<"path">>},
                {"method key", <<"method">>}
            ],
        {Suffix, Wrap} <-
            [
                {"", fun(Msg) -> Msg end},
                {" nested", fun(Msg) -> #{ <<"child">> => Msg } end}
            ]
    ] ++
    [
        {"device key", #{ <<"device">> => <<"message@1.0">>, <<"value">> => 1 }},
        {"escaped key collision",
            #{ <<"a/b">> => <<"slash">>, <<"a%2fb">> => <<"percent">> }
        }
    ] ++
    [
        {"shared child " ++ integer_to_list(N),
            #{ <<"parent">> => N, <<"left">> => typed(), <<"right">> => typed() }
        }
    || N <- lists:seq(1, 3)
    ] ++
    [
        {"signed typed child " ++ hb_util:list(Codec),
            fun(Opts) ->
                #{
                    <<"child">> =>
                        signed(
                            #{
                                <<"items">> => [typed(), [1, false, []]],
                                <<"body">> => 42
                            },
                            bundle(Codec),
                            Opts
                        )
                }
            end
        }
    || Codec <- [<<"httpsig@1.0">>, <<"ans104@1.0">>, <<"tx@1.0">>]
    ] ++
    [
        {"signed " ++ Name, fun(Opts) -> signed(nested(), Spec, Opts) end}
    ||
        {Name, Spec} <-
            [
                {"httpsig", <<"httpsig@1.0">>},
                {"bundled httpsig", bundle(<<"httpsig@1.0">>)},
                {"ans104", <<"ans104@1.0">>},
                {"bundled ans104", bundle(<<"ans104@1.0">>)},
                {"tx", <<"tx@1.0">>},
                {"bundled tx", bundle(<<"tx@1.0">>)}
            ]
    ] ++
    [
        {"signed " ++ Name ++ " ans104",
            fun(Opts) ->
                signed(
                    nested(),
                    #{ <<"device">> => <<"ans104@1.0">>, <<"type">> => Type },
                    Opts#{ <<"priv-wallet">> => ar_wallet:new(KeyType) }
                )
            end
        }
    ||
        {Name, Type, KeyType} <-
            [
                {"ed25519", <<"ed25519-sha512">>, {eddsa, ed25519}},
                {"ethereum", <<"ethereum">>, ethereum}
            ]
    ].

%% @doc The ways that a client reads a message from the primary node.
paths() -> [direct, remote, secondary, tertiary, relay, post].

%% @doc The stores that the primary node, and a node that receives a POST, write
%% messages to.
stores() -> [hb_store_volatile, hb_store_lmdb, hb_store_fs].

%% @doc The transports: an `accept' of the media type of every codec that
%% `hb_codec_test_vectors' tests, alone and before `*/*', with both
%% `accept-bundle' and `http-only-result' settings.
transports() ->
    [
        {
            "accept=" ++ hb_util:list(Accept) ++
                " / accept-bundle=" ++ hb_util:list(Bundle) ++
                " / http-only-result=" ++ hb_util:list(OnlyResult),
            #{
                accept => Accept,
                codec => Codec,
                bundle => Bundle,
                only_result => OnlyResult
            }
        }
    ||
        Others <- [<<>>, <<", */*;q=0.1">>],
        Codec <-
            [
                <<"httpsig@1.0">>,
                <<"json@1.0">>,
                <<"ans104@1.0">>,
                <<"flat@1.0">>,
                <<"structured@1.0">>,
                <<"tx@1.0">>
            ],
        Bundle <- [false, true],
        OnlyResult <- [true, false],
        Accept <- [<<(hb_http:media_type(Codec))/binary, Others/binary>>]
    ].

%% @doc The combinations that a codec cannot represent by design, each with
%% its reason. They are not generated as tests.
exceptions() ->
    [
        {all, <<"flat@1.0">>, [false, true],
            "Flat text does not escape newlines or ': ' in keys and values."},
        {all, <<"structured@1.0">>, [false, true],
            "Structured messages have no binary wire format."},
        {
            [
                "double httpsig",
                "double bundled httpsig",
                "mixed signatures",
                "signed httpsig",
                "signed bundled httpsig",
                "signed ans104",
                "signed bundled ans104",
                "signed ed25519 ans104",
                "signed ethereum ans104"
            ],
            <<"tx@1.0">>,
            [false, true],
            "A transaction carries one native root signature, not foreign ones."
        },
        {["empty child"], <<"flat@1.0">>, [true],
            "Flat paths cannot represent an empty nested map."},
        {
            [
                "double httpsig",
                "double bundled httpsig",
                "mixed signatures",
                "signed httpsig",
                "signed bundled httpsig",
                "signed tx",
                "signed bundled tx"
            ],
            <<"ans104@1.0">>,
            [false, true],
            "An ANS-104 item carries one native root signature, not foreign ones."
        }
    ].

%% @doc Return the vectors whose message is posted with a link. The node that
%% receives one holds the link, and not the message that it links to.
posted_links() -> ["explicit link"].

typed() ->
    #{
        <<"integer">> => 42,
        <<"float">> => 1.5,
        <<"boolean">> => true,
        <<"atom">> => null
    }.

nested() -> #{ <<"body">> => <<"payload">>, <<"child">> => typed() }.

bundle(Codec) -> #{ <<"device">> => Codec, <<"bundle">> => true }.

signed(Msg, Spec, Opts) -> hb_message:commit(Msg, Opts, Spec).

%% @doc Sign a message with two wallets.
double(Msg, Spec, Opts) ->
    Result =
        signed(
            signed(Msg, Spec, Opts),
            Spec,
            Opts#{
                <<"priv-wallet">> => maps:get(<<"priv-second-wallet">>, Opts)
            }
        ),
    ?assertEqual(2, length(hb_message:signers(Result, Opts))),
    Result.

%% @doc The suite for each primary store: the default cases, or every
%% combination with `HB_HTTP_TEST_VECTORS=full'.
suite_test_() ->
    Full = os:getenv("HB_HTTP_TEST_VECTORS") == "full",
    [suite(messages(), paths(), transports(), Store, Full) || Store <- stores()].

%% @doc Generate one named case of the suite, to reproduce it alone. The primary
%% store is `hb_store_volatile' unless given.
run(Name, Path, Transport) ->
    run(Name, Path, Transport, hb_store_volatile).
run(Name, Path, Transport, Store) ->
    {Name, _} = Message = lists:keyfind(Name, 1, messages()),
    {Transport, Spec} = Selection = lists:keyfind(Transport, 1, transports()),
    true = lists:member(Path, paths()),
    true = lists:member(Store, stores()),
    false = excluded(Name, Spec),
    suite([Message], [Path], [Selection], Store, true).

%% @doc Generate the tests: one primary node holds every message, and each test
%% reads one message over one path and transport with fresh client stores.
suite(Messages, Paths, Transports, Store, Full) ->
    {setup,
        fun() -> primary(Messages, Store) end,
        fun stop_primary/1,
        fun({Host, PrimaryOpts, Prepared}) ->
            [{foreach, Setup, Reset, Tests}] =
                hb_test_utils:suite_with_opts(
                    [
                        {
                            Name,
                            Name ++ " / " ++ hb_util:list(Store) ++ " / " ++
                                hb_util:list(Path) ++ " / " ++ Desc,
                            fun(ClientOpts) ->
                                {ok, Source, Expected, ID} = Preparation,
                                ?assertEqual(
                                    {error, not_found},
                                    hb_cache:read(ID, ClientOpts)
                                ),
                                exercise(
                                    read_path(Name, Path),
                                    {Host, PrimaryOpts},
                                    {ID, Source},
                                    Expected,
                                    Transport,
                                    ClientOpts#{
                                        <<"http-only-result">> =>
                                            maps:get(only_result, Transport)
                                    }
                                )
                            end
                        }
                    ||
                        {MessageIndex, {Name, Preparation}} <-
                            lists:enumerate(Prepared),
                        {PathIndex, Path} <- lists:enumerate(Paths),
                        {TransportIndex, {Desc, Transport}} <-
                            lists:enumerate(Transports),
                        not excluded(Name, Transport),
                        selected(
                            Full, MessageIndex, PathIndex, TransportIndex, Store
                        )
                    ],
                    [
                        #{
                            name => isolated,
                            desc => "HTTP vectors",
                            parallel => false,
                            timeout => 30,
                            opts => options()
                        }
                    ]
                ),
            % The suite helper resets each test's stores; they are also stopped,
            % so that their volatile tables are released.
            {foreach,
                Setup,
                fun(Opts) ->
                    try Reset(Opts)
                    after hb_store:stop(maps:get(<<"store">>, Opts))
                    end
                end,
                Tests
            }
        end
    }.

%% @doc Whether a case is generated. By default, each message is read once with
%% each transport, and the path and store rotate between cases.
selected(true, _, _, _, _) -> true;
selected(false, MessageIndex, PathIndex, TransportIndex, Store) ->
    PathCount = length(paths()),
    PathIndex == 1 + (MessageIndex + TransportIndex - 2) rem PathCount
        andalso Store ==
            lists:nth(
                1 + (MessageIndex - 1 + (TransportIndex - 1) div PathCount)
                    rem length(stores()),
                stores()
            ).

%% @doc Return the path that a case reads its message over. A message that is
%% posted with a link is read back from the node that holds the link.
read_path(Name, post) ->
    case lists:member(Name, posted_links()) of
        true -> post_link;
        false -> post
    end;
read_path(_Name, Path) -> Path.

excluded(Name, #{ codec := Codec, bundle := Bundle }) ->
    lists:any(
        fun({Names, ExcludedCodec, Bundles, _Reason}) ->
            ExcludedCodec == Codec
                andalso (Names == all orelse lists:member(Name, Names))
                andalso lists:member(Bundle, Bundles)
        end,
        exceptions()
    ).

%% @doc Options for an isolated node or client: a fresh store, volatile unless
%% given, no hooks or uploads, and an ephemeral port.
options() ->
    options(hb_store_volatile).
options(Store) ->
    #{
        <<"store">> => hb_test_utils:test_store(Store),
        <<"on">> => #{},
        <<"port">> => 0,
        <<"tls">> => false,
        <<"protocol">> => http2,
        <<"http-client">> => gun,
        <<"http-redirects">> => 0,
        <<"http-retry">> => 0,
        <<"http-client-send-timeout">> => 5000,
        <<"http-client-connect-timeout">> => 5000,
        <<"generate-index">> => false,
        <<"num-acceptors">> => 1,
        <<"prometheus">> => false,
        <<"process-sampler">> => false,
        <<"system-monitor">> => false
    }.

%% @doc Start the primary node and write each message to its cache. A message
%% that fails to build or write becomes a failing test, not a failed setup.
primary(Messages, Store) ->
    Opts =
        (options(Store))#{
            <<"priv-wallet">> => ar_wallet:new(),
            <<"priv-second-wallet">> => ar_wallet:new(),
            <<"priv-peer-wallets">> => [ar_wallet:new(), ar_wallet:new()]
        },
    hb_store:start(maps:get(<<"store">>, Opts)),
    Host = hb_http_server:start_node(Opts),
    Prepared =
        [
            {Name, prepare(Message, Opts)}
        ||
            {Name, Message} <- Messages
        ],
    {Host, Opts, Prepared}.

prepare(Message, Opts) ->
    try
        Built =
            case is_function(Message, 1) of
                true -> Message(Opts);
                false -> Message
            end,
        {Msg, Expected} =
            case Built of
                {Source, Canonical} -> {Source, Canonical};
                _ -> {Built, Built}
            end,
        ?assertEqual(true, hb_message:deep_verify(Msg, Opts)),
        {ok, _} = hb_cache:write(Msg, Opts),
        {ok, Msg, Expected, hb_message:id(Msg, all, Opts)}
    catch Class:Reason:Stacktrace -> {error, {Class, Reason, Stacktrace}}
    end.

stop_primary({_Host, Opts, _Prepared}) -> stop_node(Opts).

stop_node(Opts) ->
    cowboy:stop_listener(
        hb_util:human_id(
            ar_wallet:to_address(maps:get(<<"priv-wallet">>, Opts))
        )
    ),
    hb_store:reset(maps:get(<<"store">>, Opts)),
    hb_store:stop(maps:get(<<"store">>, Opts)).

%% @doc Read the message over a path and check it against the message that was
%% written, then check the codec of the reply on the wire.
exercise(direct, {Host, _}, {ID, _}, Expected, Transport, ClientOpts) ->
    Actual = download(Host, ID, Expected, Transport, ClientOpts),
    validate(Expected, Actual, ClientOpts),
    check_wire(Host, wire_request(ID, Transport), Transport);
exercise(link, {Host, _}, {ID, _}, Expected, Transport, ClientOpts) ->
    % The node holds the link, and not the message that it links to. It
    % cannot bundle that message, and its reply without a bundle holds the
    % link.
    Request = (headers(Transport))#{ <<"path">> => <<"/", ID/binary>> },
    case maps:get(bundle, Transport) of
        true ->
            ?assertMatch(
                {error, #{ <<"status">> := 404 }},
                hb_http:get(Host, Request, ClientOpts)
            );
        false ->
            {ok, Received} = hb_http:get(Host, Request, ClientOpts),
            ?assertEqual(true, hb_message:deep_verify(Received, ClientOpts)),
            identities(
                Expected,
                payload(Expected, Received, ClientOpts),
                ClientOpts
            ),
            check_wire(Host, wire_request(ID, Transport), Transport)
    end;
exercise(remote, {Host, _}, {ID, _}, Expected, Transport, ClientOpts) ->
    Store = maps:get(<<"store">>, ClientOpts),
    Remote =
        ClientOpts#{
            <<"store-module">> => hb_store_remote_node,
            <<"node">> =>
                #{
                    <<"uri">> =>
                        <<
                            Host/binary,
                            "~cache@1.0/read?",
                            (query(Transport))/binary
                        >>,
                    <<"opts">> => #{}
                },
            <<"store">> => Store,
            <<"local-store">> => Store,
            <<"access">> => [<<"read">>]
        },
    ReadOpts = ClientOpts#{ <<"store">> => [Store, Remote] },
    {ok, Received} = hb_cache:read(ID, ReadOpts),
    Actual = hb_cache:ensure_all_loaded(Received, ReadOpts),
    validate(Expected, Actual, ReadOpts),
    check_wire(
        Host,
        #{
            path => <<"/~cache@1.0/read?", (query(Transport))/binary>>,
            headers => #{ <<"read">> => ID }
        },
        Transport
    );
exercise(Path, {_, PrimaryOpts}, {ID, Source} = Message, Expected, Transport,
        ClientOpts) when Path == post; Path == post_link ->
    #{ <<"store-module">> := Store } = maps:get(<<"store">>, PrimaryOpts),
    PostOpts =
        (options(Store))#{
            <<"on">> =>
                #{
                    <<"request">> =>
                        #{
                            <<"device">> =>
                                #{ <<"request">> => fun receive_post/3 }
                        }
                }
        },
    with_node(
        PrimaryOpts,
        PostOpts,
        fun(PostHost, NodeOpts) ->
            ?assertEqual({error, not_found}, hb_cache:read(ID, NodeOpts)),
            % The upload loads the message's links from the primary's store,
            % and caches into a store of its own, not the client's.
            UploadStore = maps:get(<<"store">>, options()),
            UploadOpts =
                ClientOpts#{
                    <<"store">> =>
                        [UploadStore, maps:get(<<"store">>, PrimaryOpts)]
                },
            try
                {ok, Reply} =
                    hb_http:post(
                        PostHost,
                        <<"/">>,
                        (headers(Transport))#{ <<"body">> => Source },
                        UploadOpts
                    ),
                ?assertEqual(ID, hb_maps:get(<<"id">>, Reply, UploadOpts))
            after hb_store:stop(UploadStore)
            end,
            ?assertEqual({error, not_found}, hb_cache:read(ID, ClientOpts)),
            exercise(
                case Path of post -> direct; post_link -> link end,
                {PostHost, NodeOpts},
                Message,
                Expected,
                Transport,
                ClientOpts
            )
        end
    );
exercise(relay, {Host, PrimaryOpts}, {ID, _}, Expected, Transport, ClientOpts) ->
    with_node(
        PrimaryOpts,
        (options())#{
            <<"relay-block-internal">> => false,
            <<"relay-allowed-hosts">> => [<<"localhost">>]
        },
        fun(RelayHost, _Opts) ->
            % The relay asks the primary for the transport's codec, and the
            % client asks the relay for it.
            Request =
                (headers(Transport))#{
                    <<"path">> => <<"/~relay@1.0/call">>,
                    <<"peer">> => Host,
                    <<"relay-path">> => <<"/", ID/binary>>,
                    <<"target">> => <<"proxy-message">>,
                    <<"proxy-message">> => headers(Transport)
                },
            Actual =
                download(RelayHost, Request, Expected, Transport, ClientOpts),
            validate(Expected, Actual, ClientOpts),
            Encoded =
                hb_message:convert(
                    Request,
                    bundle(<<"httpsig@1.0">>),
                    ClientOpts
                ),
            check_wire(
                RelayHost,
                #{
                    path => maps:get(<<"path">>, Request),
                    headers => maps:remove(<<"body">>, Encoded),
                    body => maps:get(<<"body">>, Encoded, <<>>)
                },
                Transport
            )
        end
    );
exercise(Path, {Host, PrimaryOpts}, {ID, _} = Message, Expected, Transport,
        ClientOpts) when Path == secondary; Path == tertiary ->
    with_node(
        PrimaryOpts,
        options(),
        fun(SecondaryHost, SecondaryOpts) ->
            ?assertEqual({error, not_found}, hb_cache:read(ID, SecondaryOpts)),
            Downloaded = download(Host, ID, Expected, Transport, SecondaryOpts),
            validate(Expected, Downloaded, SecondaryOpts),
            {ok, _} = hb_cache:write(Downloaded, SecondaryOpts),
            {ok, Cached} = hb_cache:read(ID, SecondaryOpts),
            validate(
                Expected,
                hb_cache:ensure_all_loaded(Cached, SecondaryOpts),
                SecondaryOpts
            ),
            % The secondary has only its local store, so the client's read
            % cannot reach the primary.
            exercise(
                case Path of secondary -> direct; tertiary -> secondary end,
                {SecondaryHost, SecondaryOpts},
                Message,
                Expected,
                Transport,
                ClientOpts
            )
        end
    ).

%% @doc A request hook for the node that receives a POST: cache the posted
%% message as it was sent, without loading its links, then reply with its ID.
receive_post(_, #{ <<"request">> := #{ <<"method">> := <<"POST">> } = Request },
        Opts) ->
    Msg = hb_maps:get(<<"body">>, Request, Opts),
    ?assertEqual(true, hb_message:deep_verify(Msg, Opts)),
    {ok, _} = hb_cache:write(Msg, Opts),
    {ok, #{ <<"body">> => [#{ <<"id">> => hb_message:id(Msg, all, Opts) }] }};
receive_post(_, Request, _) -> {ok, Request}.

%% @doc Run `Fun' with a fresh node, then stop it and remove its store. The node
%% takes the first spare wallet of the node that starts it and keeps the rest,
%% so nodes that run at once have distinct wallets and no case generates keys.
with_node(PrimaryOpts, Options, Fun) ->
    [Wallet | Wallets] = maps:get(<<"priv-peer-wallets">>, PrimaryOpts),
    Opts =
        Options#{
            <<"priv-wallet">> => Wallet,
            <<"priv-peer-wallets">> => Wallets
        },
    hb_store:start(maps:get(<<"store">>, Opts)),
    Host = hb_http_server:start_node(Opts),
    try Fun(Host, Opts)
    after stop_node(Opts)
    end.

%% @doc The transport as a query string. A remote store builds its own request
%% headers, so its transport is given in the URI.
query(Transport) ->
    uri_string:compose_query(maps:to_list(headers(Transport))).

%% @doc The request headers that ask for the transport's codec. Lazy links in
%% the reply are loaded with `hb_http''s usual peer stores.
headers(#{ accept := Accept, bundle := Bundle }) ->
    #{
        <<"accept">> => Accept,
        <<"accept-bundle">> => hb_util:bin(Bundle)
    }.

%% @doc Read a message from a node, by ID or with a request, and return it
%% without the keys and commitments that the reply adds.
download(Host, ID, Expected, Transport, Opts) when is_binary(ID) ->
    download(
        Host,
        (headers(Transport))#{ <<"path">> => <<"/", ID/binary>> },
        Expected,
        Transport,
        Opts
    );
download(Host, Request, Expected, Transport, Opts) ->
    {ok, Received} =
        hb_http:get(
            Host,
            Request,
            Opts#{ <<"http-only-result">> => maps:get(only_result, Transport) }
        ),
    Loaded = hb_cache:ensure_all_loaded(Received, Opts),
    ?assertEqual(true, hb_message:deep_verify(Loaded, Opts)),
    % A full reply holds a literal or list result under its `ao-result' key.
    Result =
        case is_map(Loaded)
                andalso hb_maps:get(<<"ao-result">>, Loaded, false, Opts) of
            Key when is_binary(Key) -> hb_maps:get(Key, Loaded, Opts);
            false -> Loaded
        end,
    payload(Expected, Result, Opts).

%% @doc Remove the keys and commitments that the reply adds, keeping any that
%% the message itself holds.
payload(Expected, Received, Opts) when is_map(Expected), is_map(Received) ->
    WithoutReceipt =
        hb_message:without_commitments(
            #{ <<"committed">> => [<<"hashpath">>] },
            Received,
            Opts
        ),
    {ok, Committed} = hb_message:with_only_committed(WithoutReceipt, Opts),
    % A codec may add an unsigned commitment over the reply. It verified above;
    % keep only the signed commitments and those of the message itself.
    ExpectedCommitments = maps:get(<<"commitments">>, Expected, #{}),
    Payload =
        Committed#{
            <<"commitments">> =>
                maps:filter(
                    fun(ID, Commitment) ->
                        maps:is_key(<<"committer">>, Commitment)
                            orelse maps:is_key(ID, ExpectedCommitments)
                    end,
                    maps:get(<<"commitments">>, Committed, #{})
                )
        },
    maps:without(
        [Key || Key <- response_keys(), not maps:is_key(Key, Expected)],
        Payload
    );
payload(_Expected, Received, _Opts) -> Received.

%% @doc The keys that a reply adds, unless the message holds them itself.
response_keys() ->
    [
        <<"status">>,
        <<"hashpath">>,
        <<"date">>,
        <<"server">>,
        <<"content-length">>,
        <<"access-control-allow-origin">>,
        <<"access-control-allow-methods">>,
        <<"access-control-expose-headers">>
    ].

%% @doc Check a read message against the message that was written: exact values
%% and types, a strict match, signatures, and IDs.
validate(Expected, Actual, Opts) ->
    ?assertEqual(content(Expected), content(Actual)),
    case is_map(Expected) of
        true ->
            ?assertEqual(
                true,
                hb_message:match(Expected, Actual, strict, Opts)
            );
        false -> ok
    end,
    ?assertEqual(true, hb_message:deep_verify(Actual, Opts)),
    identities(Expected, Actual, Opts).

%% @doc A message's values, without its commitments or private keys.
content(Msg) when is_map(Msg) ->
    maps:map(
        fun(_, Value) -> content(Value) end,
        maps:without([<<"commitments">>], hb_private:reset(Msg))
    );
content(List) when is_list(List) -> [content(Value) || Value <- List];
content(Value) -> Value.

%% @doc Check the signed and unsigned IDs of a message and of each message it
%% holds, so that a message without commitments cannot pass by verifying
%% nothing.
identities(Expected, {link, URI, _}, Opts) ->
    % A link is `ao:' followed by the ID of the message.
    ?assertEqual(<<"ao:", (hb_message:id(Expected, all, Opts))/binary>>, URI);
identities(Expected, Actual, Opts) when is_map(Expected) ->
    ?assertEqual(signed_ids(Expected), signed_ids(Actual)),
    lists:foreach(
        fun(Kind) ->
            ?assertEqual(
                hb_message:id(Expected, Kind, Opts),
                hb_message:id(Actual, Kind, Opts)
            )
        end,
        [unsigned, signed]
    ),
    maps:foreach(
        fun(Key, Value) -> identities(Value, maps:get(Key, Actual), Opts) end,
        maps:without([<<"commitments">>], hb_private:reset(Expected))
    );
identities(Expected, Actual, Opts) when is_list(Expected) ->
    ?assertEqual(
        hb_message:id(Expected, unsigned, Opts),
        hb_message:id(Actual, unsigned, Opts)
    ),
    lists:foreach(
        fun({ExpectedValue, ActualValue}) ->
            identities(ExpectedValue, ActualValue, Opts)
        end,
        lists:zip(Expected, Actual)
    );
identities(_Expected, _Actual, _Opts) -> ok.

signed_ids(Msg) ->
    lists:sort(
        [
            ID
        ||
            {ID, Commitment} <-
                maps:to_list(maps:get(<<"commitments">>, Msg, #{})),
            maps:is_key(<<"committer">>, Commitment)
        ]
    ).

wire_request(ID, Transport) ->
    #{ path => <<"/", ID/binary>>, headers => headers(Transport) }.

%% @doc Check the codec of the reply on the wire, which `hb_http' does not
%% return. A request from a fresh client means that a client decoding the reply
%% in another codec cannot hide a wrong negotiation. The `signature-input' and
%% the `content-type' of the reply give its codec.
check_wire(Host, Request, #{ codec := Codec }) ->
    Opts = options(),
    try
        {ok, 200, Headers, _Body} =
            hb_http_client:request(
                maps:merge(
                    #{ peer => Host, method => <<"GET">>, body => <<>> },
                    Request
                ),
                Opts
            ),
        ?assertEqual(
            {ok, Codec},
            hb_http:message_codec(maps:from_list(Headers), Opts)
        )
    after hb_store:stop(maps:get(<<"store">>, Opts))
    end.
