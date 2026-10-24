%%% @doc A battery of test vectors for serving messages between nodes over
%%% HTTP. Each message is written to the cache of an isolated primary node, then
%%% read back over eight paths: directly with `hb_http', through a
%%% `hb_store_remote_node' store, through a `~relay@1.0' node, from a node that
%%% received it by POST, from a second or third node that downloaded and cached
%%% it, through a `~tunnel@1.0' broker to which the primary connects, and
%%% through the same broker to a node that reads the primary through its
%%% store. Each path is read with every codec, both `accept-bundle' settings
%%% and both `http-only-result' settings, from volatile, LMDB and filesystem
%%% primaries. Each read is checked against the message that was written: its
%%% values and types, its signatures and its IDs.
%%%
%%% The primary also resolves executions: requests for keys of messages, Lua
%%% functions and a Lua process, and requests that fail. Each is sent directly,
%%% where its reply must have the execution's status, and through both tunnel
%%% paths, where its reply must be the reply of the same request sent directly
%%% to the tunnel's origin: the same status, message and commitments.
%%%
%%% By default, each message and execution is sent once with each transport,
%%% over a path and from a primary store that rotate between cases. Set
%%% `HB_HTTP_TEST_VECTORS=full' to send every combination. Run a single case
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
                % 85 spaces escape to 255 bytes, the longest file name.
                {"long escaped key", binary:copy(<<" ">>, 85)},
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

%% @doc The requests that the primary node resolves, rather than reads, each
%% with the HTTP status of its reply. A request given as a function is built
%% with the primary's host and options, after it writes what the request reads
%% to the primary's store.
executions() ->
    [
        {"message key", 200,
            fun(_, Opts) -> resolve(nested(), <<"child">>, Opts) end
        },
        {"literal key", 200,
            fun(_, Opts) -> resolve(nested(), <<"body">>, Opts) end
        },
        {"list key", 200,
            fun(_, Opts) ->
                resolve(
                    #{ <<"items">> => [1, <<"two">>, typed()] },
                    <<"items">>,
                    Opts
                )
            end
        },
        {"lua message", 200,
            fun(_, Opts) -> lua(<<"assoctable">>, #{}, Opts) end
        },
        {"lua list", 200, fun(_, Opts) -> lua(<<"ListTable">>, #{}, Opts) end},
        {"lua request", 200,
            fun(_, Opts) ->
                lua(<<"hello">>, #{ <<"name">> => <<"tunnel">> }, Opts)
            end
        },
        {"signed lua request", 200,
            fun(_, Opts) ->
                hb_message:commit(
                    lua(
                        <<"hello/hello">>,
                        #{
                            <<"method">> => <<"POST">>,
                            <<"name">> => <<"signed">>
                        },
                        Opts
                    ),
                    Opts
                )
            end
        },
        {"lua error", 400,
            fun(_, Opts) -> lua(<<"error_response">>, #{}, Opts) end
        },
        {"lua failure", 500,
            fun(_, Opts) -> lua(<<"sandboxed_fail">>, #{}, Opts) end
        },
        {"lua process", 200, fun process/2},
        {"missing message", 404,
            #{ <<"path">> => <<"/", (hb_util:human_id(<<0:256>>))/binary>> }
        },
        {"invalid path", 400, #{ <<"path">> => <<"/a/b~">> }}
    ].

%% @doc Write a message to the primary's store and return a request for a key of
%% it.
resolve(Msg, Key, Opts) ->
    {ok, _} = hb_cache:write(Msg, Opts),
    #{
        <<"path">> =>
            <<"/", (hb_message:id(Msg, all, Opts))/binary, "/", Key/binary>>
    }.

%% @doc Write the test Lua module to the primary's store and return a request
%% for a function of it. The primary runs `assoctable' of the module first: the
%% first use of `lua@5.3a' in a BEAM loads the code of the Lua VM, which can
%% take longer than a client waits for a reply.
lua(Function, Request, Opts) ->
    {ok, Script} = file:read_file("test/test.lua"),
    Module =
        #{
            <<"content-type">> => <<"application/lua">>,
            <<"body">> => Script
        },
    {ok, _} = hb_cache:write(Module, Opts),
    Prefix =
        <<
            "/~lua@5.3a&module=",
            (hb_message:id(Module, all, Opts))/binary,
            "/"
        >>,
    {ok, _} =
        hb_ao:resolve(
            <<Prefix/binary, "assoctable">>,
            Opts#{ <<"cache-control">> => [<<"no-store">>, <<"no-cache">>] }
        ),
    Request#{ <<"path">> => <<Prefix/binary, Function/binary>> }.

%% @doc Write a Lua process to the primary's store, with one message in its
%% schedule, and return a request for the output of its computation.
process(_Host, Opts) ->
    {ok, Script} = file:read_file("test/test.lua"),
    Process =
        hb_message:commit(
            #{
                <<"device">> => <<"process@1.0">>,
                <<"type">> => <<"Process">>,
                <<"scheduler-device">> => <<"scheduler@1.0">>,
                <<"execution-device">> => <<"lua@5.3a">>,
                <<"module">> =>
                    #{
                        <<"content-type">> => <<"application/lua">>,
                        <<"body">> => Script
                    },
                <<"scheduler-location">> => hb_opts:address(Opts)
            },
            Opts
        ),
    {ok, _} = hb_cache:write(Process, Opts),
    ProcID = hb_message:id(Process, all, Opts),
    {ok, _} =
        hb_ao:resolve(
            Process,
            #{
                <<"path">> => <<"schedule">>,
                <<"method">> => <<"POST">>,
                <<"body">> =>
                    hb_message:commit(
                        #{
                            <<"target">> => ProcID,
                            <<"type">> => <<"Message">>
                        },
                        Opts
                    )
            },
            Opts#{ <<"hashpath">> => ignore }
        ),
    #{ <<"path">> => <<"/", ProcID/binary, "/now/results/output/body">> }.

%% @doc The ways that a client reads a message from the primary node.
paths() ->
    [direct, remote, secondary, tertiary, relay, post, tunnel, tunnel_remote].

%% @doc The ways that a client sends an execution's request.
execution_paths() -> [direct, tunnel, tunnel_remote].

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

%% @doc The messages that a path cannot read by design, each with its reason.
%% They are not generated as tests.
path_exceptions() ->
    [
        {["lua process"], [tunnel_remote],
            "The scheduler of the process is the primary, so its schedule is "
            "not in the primary's cache, which is all the origin reads."}
    ].

%% @doc Return the vectors whose message is posted with a link. The node that
%% receives one has the link, and not the message that it links to.
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

%% @doc Generate one named case of the suite or of the executions, to reproduce
%% it alone. The primary store is `hb_store_volatile' unless given.
run(Name, Path, Transport) ->
    run(Name, Path, Transport, hb_store_volatile).
run(Name, Path, Transport, Store) ->
    {Transport, Spec} = Selection = lists:keyfind(Transport, 1, transports()),
    true = lists:member(Store, stores()),
    false = excluded(Name, Path, Spec),
    case lists:keyfind(Name, 1, messages()) of
        {Name, _} = Message ->
            true = lists:member(Path, paths()),
            suite([Message], [Path], [Selection], Store, true);
        false ->
            {Name, _, _} = Execution = lists:keyfind(Name, 1, executions()),
            true = lists:member(Path, execution_paths()),
            execution_suite([Execution], [Path], [Selection], Store, true)
    end.

%% @doc Generate the tests: one primary node has every message, and each test
%% reads one message over one path and transport with fresh client stores.
suite(Messages, Paths, Transports, Store, Full) ->
    {setup,
        fun() -> primary(fun prepare/3, Messages, Store) end,
        fun stop_primary/1,
        fun({Host, PrimaryOpts, Prepared}) ->
            isolated(
                "HTTP vectors",
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
                    not excluded(Name, Path, Transport),
                    selected(
                        Full,
                        MessageIndex,
                        {PathIndex, length(Paths)},
                        TransportIndex,
                        Store
                    )
                ]
            )
        end
    }.

%% @doc The executions for each primary store: the default cases, or every
%% combination with `HB_HTTP_TEST_VECTORS=full'.
execution_test_() ->
    Full = os:getenv("HB_HTTP_TEST_VECTORS") == "full",
    [
        execution_suite(
            executions(),
            execution_paths(),
            transports(),
            Store,
            Full
        )
    ||
        Store <- stores()
    ].

%% @doc Generate the execution tests: one primary node resolves every request,
%% and each test sends one request over one path and transport with fresh
%% client stores.
execution_suite(Executions, Paths, Transports, Store, Full) ->
    {setup,
        fun() -> primary(fun prepare_execution/3, Executions, Store) end,
        fun stop_primary/1,
        fun({Host, PrimaryOpts, Prepared}) ->
            isolated(
                "HTTP executions",
                [
                    {
                        Name,
                        Name ++ " / " ++ hb_util:list(Store) ++ " / " ++
                            hb_util:list(Path) ++ " / " ++ Desc,
                        fun(ClientOpts) ->
                            {ok, Request, Status} = Preparation,
                            execute(
                                Path,
                                {Host, PrimaryOpts},
                                Request,
                                Status,
                                Transport,
                                ClientOpts#{
                                    <<"http-only-result">> =>
                                        maps:get(only_result, Transport)
                                }
                            )
                        end
                    }
                ||
                    {ExecutionIndex, {Name, Preparation}} <-
                        lists:enumerate(Prepared),
                    {PathIndex, Path} <- lists:enumerate(Paths),
                    {TransportIndex, {Desc, Transport}} <-
                        lists:enumerate(Transports),
                    not excluded(Name, Path, Transport),
                    selected(
                        Full,
                        ExecutionIndex,
                        {PathIndex, length(Paths)},
                        TransportIndex,
                        Store
                    )
                ]
            )
        end
    }.

%% @doc Run the tests one at a time, each with fresh client stores. The suite
%% helper resets each test's stores; they are also stopped, so that their
%% volatile tables are released.
isolated(Desc, Tests) ->
    [{foreach, Setup, Reset, Cases}] =
        hb_test_utils:suite_with_opts(
            Tests,
            [
                #{
                    name => isolated,
                    desc => Desc,
                    parallel => false,
                    timeout => 30,
                    opts => options()
                }
            ]
        ),
    {foreach,
        Setup,
        fun(Opts) ->
            try Reset(Opts)
            after hb_store:stop(maps:get(<<"store">>, Opts))
            end
        end,
        Cases
    }.

%% @doc Whether a case is generated. By default, each case is run once with
%% each transport, and the path and store rotate between cases.
selected(true, _, _, _, _) -> true;
selected(false, CaseIndex, {PathIndex, PathCount}, TransportIndex, Store) ->
    PathIndex == 1 + (CaseIndex + TransportIndex - 2) rem PathCount
        andalso Store ==
            lists:nth(
                1 + (CaseIndex - 1 + (TransportIndex - 1) div PathCount)
                    rem length(stores()),
                stores()
            ).

%% @doc Return the path that a case reads its message over. A message that is
%% posted with a link is read back from the node that has the link.
read_path(Name, post) ->
    case lists:member(Name, posted_links()) of
        true -> post_link;
        false -> post
    end;
read_path(_Name, Path) -> Path.

excluded(Name, Path, #{ codec := Codec, bundle := Bundle }) ->
    lists:any(
        fun({Names, ExcludedCodec, Bundles, _Reason}) ->
            ExcludedCodec == Codec
                andalso (Names == all orelse lists:member(Name, Names))
                andalso lists:member(Bundle, Bundles)
        end,
        exceptions()
    ) orelse
        lists:any(
            fun({Names, Paths, _Reason}) ->
                lists:member(Name, Names) andalso lists:member(Path, Paths)
            end,
            path_exceptions()
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

%% @doc Start the primary node and prepare each case with its host and options:
%% write each message to its cache, or what each execution reads. A case that
%% fails to prepare becomes a failing test, not a failed setup. The cases that
%% read through a tunnel share one broker, to which the primary stays
%% connected.
primary(Prepare, Cases, Store) ->
    Opts =
        (options(Store))#{
            <<"priv-wallet">> => ar_wallet:new(),
            <<"priv-second-wallet">> => ar_wallet:new(),
            <<"priv-peer-wallets">> => [ar_wallet:new(), ar_wallet:new()],
            <<"priv-test-broker">> => start_broker()
        },
    hb_store:start(maps:get(<<"store">>, Opts)),
    Host = hb_http_server:start_node(Opts),
    connect(Opts),
    Prepared =
        [
            {element(1, Case), Prepare(Case, Host, Opts)}
        ||
            Case <- Cases
        ],
    {Host, Opts, Prepared}.

prepare({_Name, Message}, _Host, Opts) ->
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

%% @doc Build an execution's request with the primary's host and options.
prepare_execution({_Name, Status, Request}, Host, Opts) ->
    try
        {
            ok,
            case is_function(Request, 2) of
                true -> Request(Host, Opts);
                false -> Request
            end,
            Status
        }
    catch Class:Reason:Stacktrace -> {error, {Class, Reason, Stacktrace}}
    end.

stop_primary({_Host, Opts, _Prepared}) ->
    {_, BrokerOpts} = broker(Opts),
    disconnect(Opts),
    stop_node(BrokerOpts),
    stop_node(Opts).

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
    % The node has the link, and not the message that it links to. It
    % cannot bundle that message, and its reply without a bundle contains the
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
    with_node(
        PrimaryOpts,
        (options(Store))#{ <<"operator">> => hb_opts:address(PrimaryOpts) },
        fun(PostHost, NodeOpts) ->
            ?assertEqual({error, not_found}, hb_cache:read(ID, NodeOpts)),
            % The upload is a `cache-write' request with the message as its
            % body, signed by the primary, which is the node's `operator'. It
            % loads the message's links from the primary's store, and caches
            % into a store of its own, not the client's.
            UploadStore = maps:get(<<"store">>, options()),
            UploadOpts =
                ClientOpts#{
                    <<"store">> =>
                        [UploadStore, maps:get(<<"store">>, PrimaryOpts)],
                    <<"priv-wallet">> =>
                        maps:get(<<"priv-wallet">>, PrimaryOpts)
                },
            try
                {ok, _} =
                    hb_http:post(
                        PostHost,
                        hb_message:commit(
                            (headers(Transport))#{
                                <<"path">> => <<"/~cache@1.0/write">>,
                                <<"type">> => <<"cache-write">>,
                                <<"body">> => Source
                            },
                            UploadOpts
                        ),
                        UploadOpts
                    )
            after hb_store:stop(UploadStore)
            end,
            ?assertMatch({ok, _}, hb_cache:read(ID, NodeOpts)),
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
            <<"hosts-block-internal">> => false,
            <<"hosts-allowed">> => [<<"127.0.0.1">>]
        },
        fun(RelayHost, _Opts) ->
            % The relay asks the primary for the transport's codec, and the
            % client asks the relay for it.
            Request =
                (headers(Transport))#{
                    <<"path">> => <<"/~relay@1.0/call">>,
                    <<"peer">> => Host,
                    <<"relay-path">> => <<"/", ID/binary>>,
                    <<"relay-subject">> => <<"proxy-message">>,
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
exercise(tunnel, {_, OriginOpts}, Message, Expected, Transport, ClientOpts) ->
    % The client reads the message from the broker, as a direct read, and the
    % broker carries the request to the origin.
    exercise(
        direct,
        broker(OriginOpts),
        Message,
        Expected,
        Transport#{
            headers => route(#{}, hb_opts:address(OriginOpts), ClientOpts)
        },
        ClientOpts
    );
exercise(tunnel_remote, {Host, PrimaryOpts}, {ID, _} = Message, Expected,
        Transport, ClientOpts) ->
    with_remote_origin(
        Host,
        PrimaryOpts,
        fun(OriginHost, OriginOpts) ->
            ?assertEqual(
                {error, not_found},
                hb_cache:read(ID, hb_store:scope(OriginOpts, local))
            ),
            exercise(
                tunnel,
                {OriginHost, OriginOpts},
                Message,
                Expected,
                Transport,
                ClientOpts
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

%% @doc Send an execution's request over a path. A direct request must have the
%% execution's status. A request through a tunnel must have the same reply as a
%% direct request to the tunnel's origin: the same status, message and
%% commitments.
execute(direct, {Host, _}, Request, Status, Transport, ClientOpts) ->
    send(Host, Request, Status, Transport, ClientOpts);
execute(tunnel, {Host, OriginOpts}, Request, Status, Transport, ClientOpts) ->
    {BrokerHost, _} = broker(OriginOpts),
    validate(
        send(Host, Request, Status, Transport, ClientOpts),
        send(
            BrokerHost,
            Request,
            Status,
            Transport#{
                headers =>
                    route(Request, hb_opts:address(OriginOpts), ClientOpts)
            },
            ClientOpts
        ),
        ClientOpts
    );
execute(tunnel_remote, {Host, PrimaryOpts}, Request, Status, Transport,
        ClientOpts) ->
    with_remote_origin(
        Host,
        PrimaryOpts,
        fun(OriginHost, OriginOpts) ->
            execute(
                tunnel,
                {OriginHost, OriginOpts},
                Request,
                Status,
                Transport,
                ClientOpts
            )
        end
    ).

%% @doc Send a request to a node with the transport's headers, check the status
%% of its reply, and return the reply without the keys and commitments that the
%% reply adds.
send(Host, Request, Status, Transport, Opts) ->
    Message = maps:merge(Request, headers(Transport)),
    {ErlStatus, Received} =
        hb_http:request(
            maps:get(<<"method">>, Message, <<"GET">>),
            Host,
            maps:get(<<"path">>, Message),
            Message,
            Opts
        ),
    ?assertEqual(hb_http_client:response_status_to_atom(Status), ErlStatus),
    Loaded = hb_cache:ensure_all_loaded(Received, Opts),
    ?assertEqual(true, hb_message:deep_verify(Loaded, Opts)),
    case is_map(Loaded) of
        true ->
            ?assertEqual(
                Status,
                hb_maps:get(<<"status">>, Loaded, not_found, Opts)
            );
        false -> ok
    end,
    payload(#{}, Loaded, Opts).

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

%% @doc Start a broker with the `tunnel@1.0' request hook, and return its host
%% and options.
start_broker() ->
    Opts =
        (options())#{
            <<"priv-wallet">> => ar_wallet:new(),
            <<"on">> =>
                #{
                    <<"request">> =>
                        #{
                            <<"device">> => <<"tunnel@1.0">>,
                            <<"path">> => <<"request">>
                        }
                }
        },
    hb_store:start(maps:get(<<"store">>, Opts)),
    {hb_http_server:start_node(Opts), Opts}.

%% @doc Return the host and options of the suite's broker.
broker(Opts) -> maps:get(<<"priv-test-broker">>, Opts).

%% @doc Connect a node to the suite's broker with one worker. A broker routes a
%% request by its `host' only while the origin is connected, so we wait for the
%% origin to register.
connect(Opts) ->
    {BrokerHost, BrokerOpts} = broker(Opts),
    Address = hb_opts:address(Opts),
    resize(BrokerHost, 1, Opts),
    hb_util:until(
        fun() ->
            {ok, Status} =
                hb_http:get(BrokerHost, <<"/~tunnel@1.0/status">>, BrokerOpts),
            hb_ao:get(<<"registered/", Address/binary>>, Status, 0, BrokerOpts)
                > 0
        end,
        fun() -> receive after 10 -> 1 end end,
        0
    ).

%% @doc Disconnect a node from the suite's broker. A worker that waits on the
%% broker with a registration stops only once the registration ends, and a
%% broker that stops waits 4 seconds for an open HTTP/2 stream. Until the worker
%% stops, we send it a request that expires at once, which ends its
%% registration.
disconnect(Opts) ->
    {BrokerHost, BrokerOpts} = broker(Opts),
    Address = hb_opts:address(Opts),
    resize(BrokerHost, 0, Opts),
    hb_util:until(
        fun() ->
            hb_http:get(
                BrokerHost,
                #{
                    <<"path">> => <<"/~meta@1.0/info/address">>,
                    <<"tunnel-target">> => Address,
                    <<"tunnel-timeout">> => 0
                },
                BrokerOpts
            ),
            {ok, Status} =
                hb_ao:resolve(
                    #{ <<"device">> => <<"tunnel@1.0">> },
                    #{ <<"path">> => <<"status">> },
                    Opts
                ),
            hb_maps:get(<<"retiring-client-workers">>, Status, 0, Opts) == 0
        end,
        fun() -> receive after 10 -> 1 end end,
        0
    ).

%% @doc Resize the pool of workers with which a node connects to a broker.
resize(BrokerHost, Workers, Opts) ->
    {ok, _} =
        hb_ao:resolve(
            #{ <<"device">> => <<"tunnel@1.0">> },
            #{
                <<"path">> => <<"connect">>,
                <<"peer">> => BrokerHost,
                <<"workers">> => Workers
            },
            Opts
        ).

%% @doc Return the headers that ask a broker for an origin. An unsigned request
%% gives the origin's address as its `tunnel-target'. A node keeps only the keys
%% of a signed request that its commitments cover, and its `host', so a signed
%% request gives a `host' whose first label is the base32 form of the address.
route(Request, Address, Opts) ->
    case hb_message:signers(Request, Opts) of
        [] -> #{ <<"tunnel-target">> => Address };
        _ ->
            Label = hb_committer:uri(Address),
            #{ <<"host">> => <<Label/binary, ".localhost">> }
    end.

%% @doc Run `Fun' with an origin that has no messages of its own: it reads them
%% from the primary through its store. The origin is connected to the suite's
%% broker while `Fun' runs.
with_remote_origin(Host, PrimaryOpts, Fun) ->
    Origin = #{ <<"store">> := Store } = options(),
    with_node(
        PrimaryOpts,
        Origin#{
            <<"store">> =>
                [
                    Store,
                    #{
                        <<"store-module">> => hb_store_remote_node,
                        <<"node">> => Host,
                        <<"access">> => [<<"read">>]
                    }
                ],
            <<"priv-test-broker">> => broker(PrimaryOpts)
        },
        fun(OriginHost, OriginOpts) ->
            connect(OriginOpts),
            try Fun(OriginHost, OriginOpts)
            after disconnect(OriginOpts)
            end
        end
    ).

%% @doc The transport as a query string. A remote store builds its own request
%% headers, so its transport is given in the URI.
query(Transport) ->
    uri_string:compose_query(maps:to_list(headers(Transport))).

%% @doc The request headers that ask for the transport's codec, with the
%% transport's other headers. Lazy links in the reply are loaded with
%% `hb_http''s usual peer stores.
headers(Transport = #{ accept := Accept, bundle := Bundle }) ->
    (maps:get(headers, Transport, #{}))#{
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
    % A full reply contains a literal or list result under its `ao-result' key.
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
    % A computed result that a codec other than `httpsig@1.0' sends may have
    % the node's signature on its hashpath as its only commitment. Without it,
    % its keys are all its own.
    {ok, Committed} =
        case hb_maps:get(<<"commitments">>, WithoutReceipt, #{}, Opts) of
            None when map_size(None) == 0 ->
                {ok, maps:remove(<<"commitments">>, WithoutReceipt)};
            _ -> hb_message:with_only_committed(WithoutReceipt, Opts)
        end,
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

%% @doc The keys that a reply adds, unless the message has them itself.
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
