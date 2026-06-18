%%% @doc A module for interacting with local and global options inside
%%% HyperBEAM. Options are set globally, but can also be overridden using an
%%% an optional local `Opts' map argument. Many functions across the HyperBEAM 
%%% environment accept an `Opts' argument, which can be used to customize 
%%% behavior.
%%% 
%%% Options set in an `Opts' map must _never_ change the behavior of a function
%%% that should otherwise be deterministic. Doing so may lead to loss of funds
%%% by the HyperBEAM node operator, as the results of their executions will be
%%% different than those of other node operators. If they are economically 
%%% staked on the correctness of these results, they may experience punishments
%%% for non-verifiable behavior. Instead, if a local node setting makes 
%%% deterministic behavior impossible, the caller should fail the execution 
%%% with a refusal to execute.
-module(hb_opts).
-export([get/1, get/2, get/3, as/2, identities/1, load/1, load/2, load_bin/2]).
-export([default_message/0, default_message_with_env/0, mimic_default_types/3]).
-export([ensure_node_history/2]).
-export([check_required_opts/2]).
-include("include/hb.hrl").
-include("include/hb_opts.hrl").
-include("include/hb_arweave_nodes.hrl").
-include("../../_build/hb_preloaded_index.hrl").

-ifndef(PRELOADED_DEVICES_INDEX_MESSAGE_ID).
-define(PRELOADED_DEVICES_INDEX_MESSAGE_ID, undefined).
-endif.

%%% @doc Returns the build-time-generated preloaded-store index ID, or
%%% `undefined' if the preloaded store has not yet been built.
preloaded_index_default() ->
    ?PRELOADED_DEVICES_INDEX_MESSAGE_ID.

%%% Environment variables that can be used to override the default message.
-ifdef(TEST).
-define(DEFAULT_PRINT_OPTS,
    [
        error, http_error, cron_error, hook_error
    ]
).
-else.
-define(DEFAULT_PRINT_OPTS,
    [
        error, http_error, cron_error, hook_error, warning,
        http_server_short, http_client_short,
        compute_short, push_short, copycat_short, bundler_short
    ]
).
-endif.

%%% Default name resolvers. In test mode, we do not use any name resolvers, but
%%% in-production mode we preload the ARNS snapshot as a baseline.
-ifndef(TEST).
-define(DEFAULT_NAME_RESOLVERS,
    [
        #{ <<"device">> => <<"arweave@2.9">> },
        #{ <<"device">> => <<"b32-name@1.0">> },
        <<
            "G_gb7SAgogHMtmqycwaHaC6uC-CZ3akACdFv5PUaEE8",
                "~json@1.0/deserialize&target=data"
        >>
    ]
).
-else.
-define(DEFAULT_NAME_RESOLVERS, []).
-endif.

-ifdef(AO_PROFILING).
-define(DEFAULT_TRACE_TYPE, ao).
-else.
-define(DEFAULT_TRACE_TYPE, erlang).
-endif.

-define(DEFAULT_PRIMARY_STORE, #{
    <<"name">> => <<"cache-mainnet/lmdb">>,
    <<"store-module">> => hb_store_lmdb
}).
-define(DEFAULT_GATEWAY, <<"https://arweave.net">>).
-define(
    DEFAULT_HTTP_OPTS,
    #{ <<"http-client">> => ?DEFAULT_HTTP_CLIENT, <<"protocol">> => http2 }
).
-define(ENV_KEYS,
    #{
        <<"priv-key-location">> => {"HB_KEY", "hyperbeam-key.json"},
        <<"hb-config-location">> => {"HB_CONFIG", "config.flat"},
        <<"preloaded-store">> =>
            {"HB_PRELOADED_STORE", fun preloaded_store_from_env/1},
        <<"preloaded-devices-index">> =>
            {"HB_PRELOADED_DEVICES_INDEX", fun hb_util:bin/1},
        <<"port">> => {"HB_PORT", fun erlang:list_to_integer/1, "8734"},
        <<"mode">> => {"HB_MODE", fun list_to_existing_atom/1},
        <<"paranoid-verify">> =>
            {"HB_PARANOID", fun topic_list_to_atoms/1, "false"},
        % Acceptable values for paranoid_verify:
        %   false - No paranoid verification
        %   true - Verify all messages in all contexts
        %   [http_request] - Verify messages in outbound HTTP requests only
        %   [cache_write] - Verify messages only when writing to cache
        %   [cache_read] - Verify messages only when reading from cache
        %   [http_request, cache_write] - Verify in both contexts
        <<"debug-print">> =>
            {
                "HB_PRINT",
                fun topic_list_to_atoms/1,
                {preparsed, ?DEFAULT_PRINT_OPTS}
            },
        <<"debug-log">> =>
            {
                "HB_LOG",
                fun topic_list_to_atoms/1,
                {preparsed, false}
            },
        <<"log-dir">> => {"HB_LOG_DIR", fun hb_util:bin/1, "logs"},
        <<"log-max-files">> => {"HB_LOG_MAX_FILES", fun hb_util:int/1, "5"},
        <<"log-max-bytes">> =>
            {"HB_LOG_MAX_BYTES", fun hb_util:int/1, "52428800"},
        <<"lua-scripts">> => {"LUA_SCRIPTS", "scripts"},
        <<"lua-tests">> =>
            {"LUA_TESTS", fun lua_tests_from_env/1, tests},
        <<"default-index">> =>
            {
                "HB_INDEX",
                fun("ui") ->
                    #{
                        <<"device">> => <<"hyperbuddy@1.0">>
                    };
                   ("text") ->
                    #{
                        <<"device">> => <<"hyperbuddy@1.0">>,
                        <<"path">> => <<"format">>
                    };
                   (Str) ->
                    case string:tokens(Str, "/") of
                        [Device, Path] ->
                            #{ <<"device">> => Device, <<"path">> => Path };
                        [Device] ->
                            #{ <<"device">> => Device }
                    end
                end,
                "ui"
            }
    }
).

%% @doc Convert a comma-separated list of topics, as occassionally used by `HB_*`
%% environment variables, to a list of atoms. Additionally, will return `true' if
%% the string is `true', `1', or `all'.
topic_list_to_atoms({preparsed, Parsed}) -> Parsed;
topic_list_to_atoms("false") -> [];
topic_list_to_atoms("1") -> true;
topic_list_to_atoms("true") -> true;
topic_list_to_atoms("all") -> true;
topic_list_to_atoms(Str) ->
    lists:map(fun(Topic) -> list_to_atom(Topic) end, string:tokens(Str, ",")).

preloaded_store_from_env(Path) ->
    #{
        <<"store-module">> => hb_store_lmdb,
        <<"name">> => hb_util:bin(Path)
    }.

%% @doc Keep the raw Lua test selector in opts; the Lua test device parses it.
lua_tests_from_env(tests) -> tests;
lua_tests_from_env(Spec) -> hb_util:bin(Spec).

%% @doc Convert an opts key to lower-case binary dash form.
canonical_key(Key) when is_atom(Key) ->
    canonical_key(atom_to_binary(Key, utf8));
canonical_key(Key) when is_list(Key) ->
    case hb_util:is_string_list(Key) of
        true -> canonical_key(list_to_binary(Key));
        false -> Key
    end;
canonical_key(Key) when is_binary(Key) ->
    hb_util:to_lower(binary:replace(Key, <<"_">>, <<"-">>, [global]));
canonical_key(Key) -> Key.

%% @doc Return the default message with all environment variables set.
default_message_with_env() ->
    maps:fold(
        fun(Key, _Spec, NodeMsg) ->
            case global_get(Key, undefined, #{}) of
                undefined -> NodeMsg;
                Value -> NodeMsg#{ Key => Value }
            end
        end,
        default_message(),
        ?ENV_KEYS
    ).

%% @doc The default configuration options of the hyperbeam node. The result is
%% memoised in the process dictionary on first call — every subsequent
%% invocation in the same process returns the cached map without rebuilding
%% it. The immutable portion of the node config is genuinely constant for the
%% lifetime of a process, so this is safe; the `cached_os_env/2' helper
%% applies the same pattern to environment-variable lookups below.
default_message() ->
    case erlang:get(default_message) of
        undefined ->
            Cached = raw_default_message(),
            erlang:put(default_message, Cached),
            Cached;
        Cached -> Cached
    end.

%% @doc The raw (uncached) default message. Internal — callers should use
%% `default_message/0' which memoises this value per-process.
raw_default_message() ->
    #{
        %%%%%%%% Functional options %%%%%%%%
        <<"hb-config-location">> => <<"config.flat">>,
        <<"initialized">> => true,
        % What HTTP client should the node use?
        % Options: gun, httpc, hackney
        <<"http-client">> => ?DEFAULT_HTTP_CLIENT,
        % Scheduling mode: Determines when the SU should inform the recipient
        % that an assignment has been scheduled for a message.
        % Options: aggressive(!), local_confirmation, remote_confirmation,
        %          disabled
        <<"scheduling-mode">> => local_confirmation,
        % Compute mode: Determines whether the process device should attempt to 
        % execute more messages on a process after it has returned a result.
        % Options: aggressive, lazy
        <<"compute-mode">> => lazy,
        % Choice of remote nodes for tasks that are not local to hyperbeam.
        <<"gateway">> => ?DEFAULT_GATEWAY,
        <<"bundler-ans104">> => <<"https://up.arweave.net:443">>,
        % Location of the wallet keyfile on disk that this node will use.
        <<"priv-key-location">> => <<"hyperbeam-key.json">>,
        % The time-to-live that should be specified when we register
        % ourselves as a scheduler on the network.
        % Default: 7 days.
        <<"scheduler-location-ttl">> => (60 * 60 * 24 * 7) * 1000,
        % Generated LMDB store of preloaded device artifacts.
        <<"preloaded-store">> =>
            #{
                <<"store-module">> => hb_store_lmdb,
                <<"name">> => <<"_build/preloaded-store">>,
                <<"capacity">> => 1024 * 1024 * 1024,
                <<"read-only">> => true
            },
        % Build-time ID of the preloaded name resolver message.
        <<"preloaded-devices-index">> => preloaded_index_default(),
        % Store for resolved device reference -> loaded module atom,
        % shared across processes so the first caller to resolve a
        % device spares the rest the index read and archive
        % extraction. Defaults to a `hb_store_volatile`.
        <<"loaded-device-store">> =>
            [
                #{
                    <<"store-module">> => hb_store_volatile,
                    <<"name">> => <<"loaded-device-volatile-store">>
                }
            ],
        % Default execution cache control options
        <<"cache-control">> => [<<"no-cache">>, <<"no-store">>],
        <<"cache-lookup-hueristics">> => false,
        % Should we await in-progress executions, rather than re-running?
        % Has three settings: false, only `named' executions, or all executions.
        <<"await-inprogress">> => named,
        % Should the node attempt to access data from remote caches for
        % client requests?
        <<"access-remote-cache-for-client">> => false,
        % Should the node attempt to load devices from remote signers?
        <<"load-remote-devices">> => false,
        % The list of device signers that the node should trust.
        <<"trusted-device-signers">> => [],
        % Map of device name/spec ID -> trusted implementation ID,
        % loaded without a signer check.
        <<"trusted-devices">> => #{},
        % What should the node do if a client error occurs?
        <<"client-error-strategy">> => throw,
        % HTTP client request options
        <<"http-client-connect-timeout">> => 5000,
        <<"http-client-keepalive">> => 120000,
        <<"http-client-send-timeout">> => 300_000,
        <<"port">> => 8734,
        <<"process-sampler">> => true,
        <<"process-sampler-interval">> => 15000,
        <<"wasm-allow-aot">> => false,
        % Options for the relay device
        <<"relay-http-client">> => httpc,
        % The default codec to use for commitment signatures.
        <<"commitment-device">> => <<"httpsig@1.0">>,
        % Dev options
        <<"mode">> => debug,
        <<"profiling">> => true,
        % Every modification to `Opts' called directly by the node operator
        % should be recorded here.
        <<"node-history">> => [],
        <<"debug-stack-depth">> => 40,
        <<"debug-print">> => false,
        <<"debug-log">> => false,
        <<"log-dir">> => <<"logs">>,
        <<"log-max-files">> => 5,
        <<"log-max-bytes">> => 52428800,
        <<"debug-print-map-line-threshold">> => 30,
        <<"debug-print-binary-max">> => 60,
        <<"debug-print-indent">> => 2,
        <<"debug-print-truncate">> => 30,
        <<"stack-print-prefixes">> => ["hb", "dev", "ar", "maps"],
        <<"debug-print-trace">> => short, % `short` | `false`. Has performance impact.
        <<"debug-print-verify">> => false,
        <<"debug-print-metadata">> => true,
        <<"debug-print-gen-id">> => false,
        <<"debug-print-committers">> => true,
        <<"debug-print-comm-device">> => true,
        <<"debug-print-comm-type">> => true,
        <<"debug-trace-type">> => ?DEFAULT_TRACE_TYPE,
        <<"short-trace-len">> => 20,
        <<"debug-show-priv">> => if_present,
        <<"debug-resolve-links">> => false,
        <<"debug-print-fail-mode">> => long,
		<<"trusted">> => #{},
        <<"name-resolvers">> => ?DEFAULT_NAME_RESOLVERS,
        <<"routes">> => [
            % Local CU routes.
            #{
                <<"template">> => <<"/result/.*">>,
                <<"node">> => #{ <<"prefix">> => <<"http://localhost:6363">> }
            },
            #{
                <<"template">> => <<"/snapshot/.*">>,
                <<"node">> => #{ <<"prefix">> => <<"http://localhost:6363">> }
            },
            #{
                <<"template">> => <<"/dry-run.*">>,
                <<"node">> => #{ <<"prefix">> => <<"http://localhost:6363">> }
            },
            #{
                <<"template">> => <<"/state.*">>,
                <<"node">> => #{ <<"prefix">> => <<"http://localhost:6363">> }
            },
            % GraphQL: race all gateways, take the first 200.
            #{
                <<"template">> => <<"/graphql">>,
                <<"nodes">> =>
                    [
                        #{
                            <<"prefix">> => <<"https://ao-search-gateway.goldsky.com">>,
                            <<"opts">> => ?DEFAULT_HTTP_OPTS
                        },
                        #{
                            <<"prefix">> => <<"https://arweave-search.goldsky.com">>,
                            <<"opts">> => ?DEFAULT_HTTP_OPTS
                        },
                        #{
                            <<"prefix">> => ?DEFAULT_GATEWAY,
                            <<"opts">> => ?DEFAULT_HTTP_OPTS
                        }
                    ]
            },
            % chunk requests: route to the nearest data nodes by
            % partition midpoint (byte offset).
            #{
                <<"template">> => 
                    #{
                        <<"path">> => <<"^/arweave/chunk">>,
                        <<"method">> => <<"GET">>
                    },
                <<"nodes">> => add_opts(?ARWEAVE_BOOTSTRAP_DATA_NODES ++ ?ARWEAVE_BOOTSTRAP_TIP_NODES),
                <<"strategy">> => <<"Shuffled-Range">>,
                <<"choose">> =>
                    length(
                        ?ARWEAVE_BOOTSTRAP_DATA_NODES
                            ++ ?ARWEAVE_BOOTSTRAP_TIP_NODES
                    ),
                <<"parallel">> => 1,
                <<"responses">> => 1,
                <<"stop-after">> => true,
                <<"admissible-status">> => 200
            },
            #{
                <<"template">> => 
                    #{
                        <<"path">> => <<"^/arweave/unconfirmed_chunk">>,
                        <<"method">> => <<"GET">>
                    },
                <<"nodes">> => add_opts(?ARWEAVE_BOOTSTRAP_TIP_NODES),
                <<"choose">> => length(?ARWEAVE_BOOTSTRAP_TIP_NODES),
                <<"parallel">> => 1,
                <<"responses">> => 1,
                <<"stop-after">> => true,
                <<"admissible-status">> => 200
            },
            #{
                <<"template">> => 
                    #{
                        <<"path">> => <<"^/arweave/chunk">>,
                        <<"method">> => <<"POST">>
                    },
                <<"nodes">> => add_opts(?ARWEAVE_BOOTSTRAP_DATA_NODES ++ ?ARWEAVE_BOOTSTRAP_TIP_NODES),
                <<"strategy">> => <<"Shuffled-Range">>,
                <<"choose">> =>
                    length(
                        ?ARWEAVE_BOOTSTRAP_DATA_NODES
                            ++ ?ARWEAVE_BOOTSTRAP_TIP_NODES
                    ),
                <<"parallel">> => 5,
                <<"responses">> => 3, %% keep going until we get 3x 200s
                <<"stop-after">> => true,
                <<"admissible-status">> => 200
            },
            #{
                <<"template">> => 
                    #{
                        <<"path">> => <<"^/arweave/tx">>,
                        <<"method">> => <<"POST">>
                    },
                <<"nodes">> => add_opts(?ARWEAVE_BOOTSTRAP_CHAIN_NODES ++ ?ARWEAVE_BOOTSTRAP_TIP_NODES),
                <<"parallel">> => true,
                <<"responses">> => 3,
                <<"stop-after">> => false,
                <<"admissible-status">> => 200
            },
            % Raw data requests via arweave.net gateway.
            #{
                <<"template">> => <<"^/arweave/raw">>,
                <<"node">> =>
                    #{
                        <<"match">> => <<"^/arweave">>,
                        <<"with">> => ?DEFAULT_GATEWAY,
                        <<"opts">> => ?DEFAULT_HTTP_OPTS
                    }
            },
            % General Arweave requests: race all chain nodes, take
            % the first 200.
            #{
                <<"template">> => <<"^/arweave">>,
                <<"nodes">> => add_opts(?ARWEAVE_BOOTSTRAP_CHAIN_NODES),
                <<"parallel">> => true,
                <<"stop-after">> => true,
                <<"admissible-status">> => 200
            },
            % Raw data requests via arweave.net gateway. TODO: Update later.
            #{
                <<"template">> => <<"/raw">>,
                <<"node">> =>
                    #{
                        <<"prefix">> => ?DEFAULT_GATEWAY,
                        <<"opts">> => ?DEFAULT_HTTP_OPTS
                    }
            }
        ],
        <<"store">> =>
            [
                ?DEFAULT_PRIMARY_STORE,
                #{
                    <<"store-module">> => hb_store_fs,
                    <<"name">> => <<"cache-mainnet">>
                },
                #{
                    <<"store-module">> => hb_store_arweave,
                    <<"name">> => <<"cache-arweave">>,
                    <<"index-store">> => [?DEFAULT_PRIMARY_STORE],
                    <<"arweave-node">> => ?DEFAULT_GATEWAY
                },
                #{
                    <<"store-module">> => hb_store_gateway,
                    <<"subindex">> => [
                        #{
                            <<"name">> => <<"Data-Protocol">>,
                            <<"value">> => <<"ao">>
                        }
                    ],
                    <<"local-store">> => [?DEFAULT_PRIMARY_STORE]
                },
                #{
                    <<"store-module">> => hb_store_gateway,
                    <<"local-store">> => [?DEFAULT_PRIMARY_STORE]
                }
            ],
        <<"match-index">> => [?DEFAULT_PRIMARY_STORE],
        <<"priv-store">> =>
            [
                #{
                    <<"store-module">> => hb_store_fs,
                    <<"name">> => <<"cache-priv">>
                }
            ],
        % default_index => #{ <<"device">> => <<"hyperbuddy@1.0">> },
        % Should we use the latest cached state of a process when computing?
        <<"process-now-from-cache">> => false,
        % Should we trust the GraphQL API when converting to ANS-104? Some GQL
        % services do not provide the `anchor' or `last_tx' fields, so their
        % responses are not verifiable.
        <<"ans104-trust-gql">> => true,
        % Number of chunks to fetch in parallel when loading a TX or dataitem.
        <<"arweave-chunk-fetch-concurrency">> => 5,
        <<"http-extra-opts">> =>
            #{
                <<"force-message">> => true,
                <<"cache-control">> => [<<"always">>]
            },
        % Should the node store all signed messages?
        <<"store-all-signed">> => true,
        % Should the node use persistent processes?
        <<"process-workers">> => true,
        % Options for the router device
        <<"router-opts">> => #{
            <<"routes">> => []
        },
        <<"on">> => #{
            <<"request">> =>
                [
                    #{
                        <<"device">> => <<"rate-limit@1.0">>
                    },
                    #{
                        <<"device">> => <<"auth-hook@1.0">>,
                        <<"path">> => <<"request">>,
                        <<"when">> => #{
                            <<"keys">> => [<<"authorization">>, <<"!">>]
                        },
                        <<"secret-provider">> =>
                            #{
                                <<"device">> => <<"http-auth@1.0">>,
                                <<"access-control">> =>
                                    #{ <<"device">> => <<"http-auth@1.0">> }
                            }
                    },
                    #{
                        <<"device">> => <<"name@1.0">>
                    },
                    #{
                        <<"device">> => <<"manifest@1.0">>
                    },
                    #{
                        <<"device">> => <<"blacklist@1.0">>
                    }
                ]
        },
        <<"scheduler-default-commitment-spec">> => <<"httpsig@1.0">>,
        <<"genesis-wasm-import-authorities">> =>
            [
                <<"WjnS-s03HWsDSdMnyTdzB1eHZB2QheUWP_FVRVYxkXk">>
            ],
        % Should the node track and expose prometheus metrics?
        % We do not set this explicitly, so that the hb_features:test() value
        % can be used to determine if we should expose metrics instead,
        % dynamically changing the configuration based on whether we are running
        % tests or not. To override this, set the `prometheus' option explicitly.
        % prometheus => false
        % Define the behaviour when accessing a file inside a manifest that 
        % doesn't exists.
        % Options:
        % - fallback: Fallback to the index page
        % - error: Return 404 Not Found
        <<"manifest-404">> => fallback
    }.

%% @doc Get an option from the global options, optionally overriding with a
%% local `Opts' map if `prefer' or `only' is set to `local'. If the `only' 
%% option is provided in the `local' map, only keys found in the corresponding
%% (`local' or `global') map will be returned. This function also offers users
%% a way to specify a default value to return if the option is not set.
%% 
%% `prefer' defaults to `local'.
get(Key) -> ?MODULE:get(Key, undefined).
get(Key, Default) -> ?MODULE:get(Key, Default, #{}).
get(Key, Default, Opts) when is_atom(Key); is_list(Key) ->
    ?MODULE:get(canonical_key(Key), Default, Opts);
get(Key, Default, Opts)
        when is_binary(Key),
             is_map(Opts),
             not is_map_key(<<"only">>, Opts),
             not is_map_key(<<"prefer">>, Opts) ->
    case maps:find(Key, Opts) of
        {ok, Value} -> Value;
        error ->
            case is_map_key(Key, ?ENV_KEYS) of
                true -> do_get(Key, Default, Opts);
                false -> maps:get(Key, default_message(), Default)
            end
    end;
get(Key, Default, Opts) when is_binary(Key) ->
    do_get(Key, Default, Opts);
get(Key, Default, Opts)
        when is_map(Opts),
             is_map_key(<<"only">>, Opts) orelse is_map_key(<<"prefer">>, Opts) ->
    do_get(canonical_key(Key), Default, Opts);
get(Key, Default, Opts) ->
    ?MODULE:get(canonical_key(Key), Default, Opts).
do_get(Key, Default, Opts = #{ <<"only">> := local }) ->
    case maps:find(Key, Opts) of
        {ok, Value} -> Value;
        error -> Default
    end;
do_get(Key, Default, Opts = #{ <<"only">> := global }) ->
    case global_get(Key, hb_opts_not_found, Opts) of
        hb_opts_not_found -> Default;
        Value -> Value
    end;
do_get(Key, Default, Opts = #{ <<"prefer">> := global }) ->
    case do_get(Key, hb_opts_not_found, #{ <<"only">> => global }) of
        hb_opts_not_found ->
            do_get(Key, Default, Opts#{ <<"only">> => local });
        Value -> Value
    end;
do_get(Key, Default, Opts = #{ <<"prefer">> := local }) ->
    case do_get(Key, hb_opts_not_found, Opts#{ <<"only">> => local }) of
        hb_opts_not_found ->
            do_get(Key, Default, Opts#{ <<"only">> => global });
        Value -> Value
    end;
do_get(Key, Default, Opts) ->
    % No preference was set in Opts, so we default to local.
    do_get(Key, Default, Opts#{ <<"prefer">> => local }).

%% @doc Get an environment variable or configuration key. Depending on whether
%% the value is derived from an environment variable, we may be able to cache
%% the result in the process dictionary.
global_get(Key, Default, Opts) ->
    NormKey = canonical_key(Key),
    case erlang:get({processed_env, NormKey}) of
        {cached, Value} -> Value;
        undefined ->
            % Thee value is not cached, so we need to process it.
            {IsCachable, Value} =
                case maps:get(NormKey, ?ENV_KEYS, Default) of
                    Default -> {false, config_lookup(NormKey, Default, Opts)};
                    {EnvKey, ValParser, DefaultValue} when is_function(ValParser) ->
                        {true, ValParser(
                            cached_os_env(
                                EnvKey,
                                normalize_default(DefaultValue)
                            )
                        )};
                    {EnvKey, ValParser} when is_function(ValParser) ->
                        case cached_os_env(EnvKey, not_found) of
                            not_found ->
                                {false, config_lookup(NormKey, Default, Opts)};
                            V -> {true, ValParser(V)}
                        end;
                    {EnvKey, DefaultValue} ->
                        {true, cached_os_env(EnvKey, DefaultValue)}
                    end,
            % Cache the result if it is immutable and return.
            if IsCachable ->
                    erlang:put({processed_env, NormKey}, {cached, Value});
            true -> ok
            end,
            Value
    end.

%% @doc Cache the result of os:getenv/1 in the process dictionary, as it never
%% changes during the lifetime of a node.
cached_os_env(Key, DefaultValue) ->
    case erlang:get({os_env, Key}) of
        {cached, false} -> DefaultValue;
        {cached, Value} -> Value;
        undefined ->
            % The process dictionary returns `undefined' for a key that is not
            % set, so we need to check the environment and store the result.
            erlang:put({os_env, Key}, {cached, os:getenv(Key)}),
            % We recurse to follow the normal path.
            cached_os_env(Key, DefaultValue)
    end.

%% @doc Get an option from environment variables, optionally consulting the
%% `hb_features' of the node if a conditional default tuple is provided.
normalize_default({conditional, Feature, IfTest, Else}) ->
    case hb_features:enabled(Feature) of
        true -> IfTest;
        false -> Else
    end;
normalize_default(Default) -> Default.

%% @doc An abstraction for looking up configuration variables. In the future,
%% this is the function that we will want to change to support a more dynamic
%% configuration system.
config_lookup(Key, Default, _Opts) ->
    maps:get(canonical_key(Key), default_message(), Default).

%% @doc Parse a configuration source into a map, matching the types of the
%% keys to those in the default message. The source may be a single path or
%% a comma-separated list of paths. When multiple paths are provided, each
%% is loaded individually (with extension-based parsing and type coercion
%% applied per source) and the results are deep-merged left-to-right, so
%% sources on the right override sources on the left.
load(Path) -> load(Path, #{}).
load(Path, Opts) ->
    case binary:split(hb_util:bin(Path), <<",">>, [global]) of
        [_] -> load_path(Path, Opts);
        Parts ->
            Trimmed =
                lists:filter(
                    fun(P) -> P =/= <<>> end,
                    [iolist_to_binary(string:trim(P)) || P <- Parts]
                ),
            load_paths(Trimmed, Opts)
    end.

%% @doc Load a single configuration source from disk.
load_path(Path, Opts) ->
    {ok, Device} = path_to_device(Path),
    case file:read_file(Path) of
        {ok, Bin} ->
            load_bin(Device, Bin, Opts);
        _ -> {error, not_found}
    end.

%% @doc Load each configuration source in turn, deep-merging the results in
%% left-to-right order. Fails fast if any individual source cannot be loaded.
load_paths(Paths, Opts) -> load_paths(Paths, Opts, #{}).
load_paths([], _Opts, Acc) -> {ok, Acc};
load_paths([P | Rest], Opts, Acc) ->
    case load_path(P, Opts) of
        {ok, Map} ->
            load_paths(Rest, Opts, hb_util:deep_merge(Acc, Map, Opts));
        {error, _} = Err -> Err
    end.

%% @doc Convert a path to a device from its file extension. If no extension is
%% provided, we default to `flat@1.0'.
path_to_device(Path) ->
    case binary:split(hb_util:bin(Path), <<".">>, []) of
        [_, Extension] ->
            ?event(debug_node_msg,
                {path_to_device,
                    {path, Path},
                    {extension, Extension}
                }
            ),
            extension_to_device(Extension);
        _ -> {ok, <<"flat@1.0">>}
    end.

%% @doc Convert a file extension to a device name. Configuration files
%% are loaded by extension, so the kernel needs to know about a small,
%% fixed set of codecs ahead of any device resolution. Adding new
%% codec extensions here is a kernel-level change, not a configuration
%% one.
extension_to_device(Ext) ->
    NormExt = hb_util:bin(Ext),
    Map = #{
        <<"flat">> => <<"flat@1.0">>,
        <<"json">> => <<"json@1.0">>,
        <<"tabm">> => <<"flat@1.0">>,
        <<"httpsig">> => <<"httpsig@1.0">>,
        <<"ans104">> => <<"ans104@1.0">>,
        <<"tx">> => <<"tx@1.0">>,
        <<"structured">> => <<"structured@1.0">>
    },
    case maps:find(NormExt, Map) of
        {ok, Name} -> {ok, Name};
        error -> {error, not_found}
    end.

%% @doc Parse a given binary with a device (defaulting to `flat@1.0') into a
%% node message. Types are converted to match those in the default message, if
%% applicable.
load_bin(Bin, Opts) ->
    load_bin(<<"flat@1.0">>, Bin, Opts).
load_bin(<<"flat@1.0">>, Bin, Opts) ->
    % Trim trailing whitespace from each line in the file.
    Ls =
        lists:map(
            fun(Line) -> string:trim(Line, trailing) end,
            binary:split(Bin, <<"\n">>, [global])
        ),
    try hb_ao:raw(
        <<"flat@1.0">>,
        <<"deserialize">>,
        iolist_to_binary(lists:join(<<"\n">>, Ls)),
        #{},
        Opts
    )
    of
        {ok, Map} ->
            {ok, mimic_default_types(Map, false, Opts)}
    catch
        error:B -> {error, B}
    end;
load_bin(Device, Bin, Opts) ->
    try
        {
            ok,
            mimic_default_types(
                hb_cache:ensure_all_loaded(
                    hb_message:convert(
                        Bin,
                        <<"structured@1.0">>,
                        Device,
                        Opts#{ <<"linkify-mode">> => false }
                    ),
                    Opts
                ),
                false,
                Opts
            )
        }
    catch error:B -> {error, B}
    end.

%% @doc Mimic the types of the default message for a given map.
mimic_default_types(Map, _Mode, Opts) ->
    Default = default_message_with_env(),
    hb_maps:from_list(
        lists:map(
            fun({Key, Value}) ->
                NewKey = canonical_key(Key),
                NewValue =
                    case hb_maps:get(NewKey, Default, not_found, Opts) of
                        not_found -> Value;
                        DefaultValue -> coerce_value(DefaultValue, Value)
                    end,
                {NewKey, NewValue}
            end,
            hb_maps:to_list(Map, Opts)
        )
    ).

coerce_value(not_found, Value) ->
    Value;
coerce_value(DefaultValue, Value) when is_atom(DefaultValue) ->
    try hb_util:atom(Value)
    catch _:_ -> Value
    end;
coerce_value(DefaultValue, Value) when is_integer(DefaultValue) ->
    hb_util:int(Value);
coerce_value(DefaultValue, Value) when is_float(DefaultValue) ->
    hb_util:float(Value);
coerce_value(_DefaultValue, Value) ->
    Value.

%% @doc Find a given identity from the `identities' map, and return the options
%% merged with the sub-options for that identity.
as(Identity, Opts) ->
    case identities(Opts) of
        #{ Identity := SubOpts } ->
            ?event({found_identity_sub_opts_are, SubOpts}),
            {ok, maps:merge(Opts, mimic_default_types(SubOpts, false, Opts))};
        _ ->
            {error, not_found}
    end.

%% @doc Find all known IDs and their sub-options from the `priv_ids' map. Allows
%% the identities to be named, or based on addresses. The results are normalized
%% such that the map returned by this function contains both mechanisms for 
%% finding an identity and its sub-options. Additionally, sub-options are also
%% normalized such that the `address' property is present and accurate for all
%% given identities.
identities(Opts) ->
    identities(hb:wallet(), Opts).
identities(Default, Opts) ->
    Named = ?MODULE:get(identities, #{}, Opts),
    % Generate an address-based map of identities.
    Addresses =
        maps:from_list(lists:filtermap(
            fun({_Name, SubOpts}) ->
                case maps:find(<<"priv-wallet">>, SubOpts) of
                    {ok, Wallet} ->
                        Addr = hb_util:human_id(ar_wallet:to_address(Wallet)),
                        {true, {Addr, SubOpts}};
                    error -> false
                end
            end,
            maps:to_list(Named)
        )),
    % Merge the named and address-based maps. Normalize each result to ensure
    % that the `address' property is present and accurate.
    Identities =
        maps:map(
            fun(_NameOrID, SubOpts) ->
                case maps:find(<<"priv-wallet">>, SubOpts) of
                    {ok, Wallet} ->
                        SubOpts#{ <<"address">> => hb_util:human_id(Wallet) };
                    error -> SubOpts
                end
            end,
            maps:merge(Named, Addresses)
        ),
    ?event({identities_without_default, Identities}),
    % Add a default identity if one is not already present.
    DefaultWallet = ?MODULE:get(priv_wallet, Default, Opts),
    case maps:find(DefaultID = hb_util:human_id(DefaultWallet), Identities) of
        {ok, _} -> Identities;
        error ->
            Identities#{
                DefaultID => #{
                    <<"priv-wallet">> => DefaultWallet
                },
                <<"default">> => #{
                    <<"priv-wallet">> => DefaultWallet
                }
            }
    end.

%% @doc Utility function to check for required options in a list.
%% Takes a list of {Name, Value} pairs and returns:
%% - {ok, Opts} when all required options are present (Value =/= not_found)
%% - {error, ErrorMsg} with a message listing all missing options when any are not_found
%% @param KeyValuePairs A list of {Name, Value} pairs to check.
%% @param Opts The original options map to return if validation succeeds.
%% @returns `{ok, Opts}' if all required options are present, or
%% `{error, <<"Missing required parameters: ", MissingOptsStr/binary>>}'
%% where `MissingOptsStr' is a comma-separated list of missing option names.
-spec check_required_opts(list({binary(), term()}), map()) -> 
    {ok, map()} | {error, binary()}.
check_required_opts(KeyValuePairs, Opts) ->
    MissingOpts = lists:filtermap(
        fun({Name, Value}) ->
            case Value of
                not_found -> {true, Name};
                _ -> false
            end
        end,
        KeyValuePairs
    ),
    case MissingOpts of
        [] -> 
            {ok, Opts};
        _ ->
            MissingOptsStr = binary:list_to_bin(
                lists:join(<<", ">>, MissingOpts)
            ),
            ErrorMsg = <<"Missing required opts: ", MissingOptsStr/binary>>,
            {error, ErrorMsg}
    end.

%% @doc Ensures all items in a node history meet required configuration options.
%%
%% This function verifies that the first item (complete opts) contains all required
%% configuration options and that their values match the expected format. Then it
%% validates that subsequent history items (which represent differences) never
%% modify any of the required keys from the first item.
%%
%% Validation is performed in two steps:
%% 1. Checks that the first item has all required keys and valid values
%% 2. Verifies that subsequent items don't modify any required keys from the first item
%%
%% @param Opts The complete options map (will become first item in history)
%% @param RequiredOpts A map of options that must be present and unchanging
%% @returns {ok, <<"valid">>} when validation passes
%% @returns {error, <<"missing_keys">>} when required keys are missing from first item
%% @returns {error, <<"invalid_values">>} when first item values don't match requirements
%% @returns {error, <<"modified_required_key">>} when history items modify required keys
%% @returns {error, <<"validation_failed">>} when other validation errors occur
-spec ensure_node_history(NodeHistory :: list() | term(), RequiredOpts :: map()) -> 
    {ok, binary()} | {error, binary()}.
ensure_node_history(Opts, RequiredOpts) ->
    ?event(validate_history_items, {required_opts, RequiredOpts}),
    maybe
        % Get the node history from the options
        NodeHistory = hb_opts:get(node_history, [], Opts),
        % Add the Opts to the node history to validate all items
        NodeHistoryWithOpts = [ Opts | NodeHistory ],
        % Normalize required options
        NormalizedRequiredOpts ?= hb_ao:normalize_keys(RequiredOpts),
        % Normalize all node history items once
        NormalizedNodeHistory ?= lists:map(
            fun(Item) ->
                hb_ao:normalize_keys(Item)
            end,
            NodeHistoryWithOpts
        ),
        % Get the first item (complete opts) and remaining items (differences)
        [FirstItem | RemainingItems] = NormalizedNodeHistory,
        % Step 2: Validate first item values match requirements
        FirstItemValuesMatch = hb_message:match(NormalizedRequiredOpts, FirstItem, primary),
        true ?= (FirstItemValuesMatch == true) orelse {error, values_invalid},
        % Step 3: Check that remaining items don't modify required keys
        NoRequiredKeysModified = lists:all(
            fun(HistoryItem) ->
                % For each required key, if it exists in this history item,
                % it must match the value from the first item
                hb_message:match(RequiredOpts, HistoryItem, only_present)
            end,
            RemainingItems
        ),
        true ?= NoRequiredKeysModified orelse {error, required_key_modified},
        % If we've made it this far, everything is valid
        ?event({validate_node_history_items, all_items_valid}),
        {ok, valid}
    else
        {error, values_invalid} ->
            ?event({validate_node_history_items, validation_failed, invalid_values}),
            {error, invalid_values};
        {error, required_key_modified} ->
            ?event({validate_node_history_items, validation_failed, required_key_modified}),
            {error, modified_required_key};
        _ ->
            ?event({validate_node_history_items, validation_failed, unknown}),
            {error, validation_failed}
    end.

%% @doc Util to add opts to nodes.
add_opts(Items) ->
    add_opts(Items, ?DEFAULT_HTTP_OPTS).
add_opts(Items, Opts) ->
    lists:map(
        fun (Item) when is_map(Item) -> 
            Item#{<<"opts">> => Opts}
        end, 
        Items
    ).

%%% Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

global_get_test() ->
    ?assertEqual(debug, ?MODULE:get(mode)),
    ?assertEqual(debug, ?MODULE:get(mode, production)),
    ?assertEqual(undefined, ?MODULE:get(unset_global_key)),
    ?assertEqual(1234, ?MODULE:get(unset_global_key, 1234)),
    ?assertEqual(false, ?MODULE:get(debug_log)),
    ?assertEqual(<<"logs">>, ?MODULE:get(log_dir)),
    ?assertEqual(5, ?MODULE:get(log_max_files)),
    ?assertEqual(52428800, ?MODULE:get(log_max_bytes)).

local_get_test() ->
    Local = #{ <<"only">> => local },
    ?assertEqual(undefined, 
        ?MODULE:get(test_key, undefined, Local)),
    ?assertEqual(correct,
        ?MODULE:get(test_key, undefined, Local#{ <<"test-key">> => correct })).

local_preference_test() ->
    Local = #{ <<"prefer">> => local },
    ?assertEqual(correct,
        ?MODULE:get(test_key, undefined, Local#{ <<"test-key">> => correct })),
    ?assertEqual(correct,
        ?MODULE:get(mode, undefined, Local#{ <<"mode">> => correct })),
    ?assertNotEqual(undefined,
        ?MODULE:get(mode, undefined, Local)).

global_preference_test() ->
    Global = #{ <<"prefer">> => global },
    ?assertEqual(undefined, ?MODULE:get(test_key, undefined, Global)),
    ?assertNotEqual(incorrect,
        ?MODULE:get(mode, undefined, Global#{ <<"mode">> => incorrect })),
    ?assertNotEqual(undefined, ?MODULE:get(mode, undefined, Global)).

load_flat_test() ->
    % File contents:
    % port: 1234
    % node-host: https://ao.computer
    % await-inprogress: false
    {ok, Conf} = load("test/config.flat", #{}),
    ?event({loaded, {explicit, Conf}}),
    % Ensure we convert types as expected.
    ?assertEqual(1234, hb_maps:get(<<"port">>, Conf)),
    % A binary
    ?assertEqual(<<"https://ao.computer">>, hb_maps:get(<<"node-host">>, Conf)),
    % An atom, where the key contained a header-key `-' rather than a `_'.
    ?assertEqual(false, hb_maps:get(<<"await-inprogress">>, Conf)).

load_json_test() ->
    {ok, Conf} = load("test/config.json", #{}),
    ?event(debug_node_msg, {loaded, Conf}),
    ?assertEqual(1234, hb_maps:get(<<"port">>, Conf)),
    ?assertEqual(9001, hb_maps:get(<<"example">>, Conf)),
    % A binary
    ?assertEqual(<<"https://ao.computer">>, hb_maps:get(<<"node-host">>, Conf)),
    % An atom, where the key contained a header-key `-' rather than a `_'.
    ?assertEqual(false, hb_maps:get(<<"await-inprogress">>, Conf)),
    % Ensure that a store with `ao-types' is loaded correctly.
    ?assertMatch(
        [#{ <<"store-module">> := hb_store_fs }|_],
        hb_maps:get(<<"store">>, Conf)
    ).

load_multi_precedence_test() ->
    %% Two sources via a comma-separated path. Each is parsed and type-coerced
    %% individually, then deep-merged left-to-right so the right-hand source
    %% overrides shared keys.
    A = "test/config-multi-a.flat",
    B = "test/config-multi-b.flat",
    ok = file:write_file(A, <<"port: 1111\nshared: left\n">>),
    ok = file:write_file(B, <<"shared: right\nextra: only-right\n">>),
    try
        {ok, Conf} = load(A ++ "," ++ B, #{}),
        ?assertEqual(1111, hb_maps:get(<<"port">>, Conf)),
        ?assertEqual(<<"right">>, hb_maps:get(<<"shared">>, Conf)),
        ?assertEqual(<<"only-right">>, hb_maps:get(<<"extra">>, Conf))
    after
        file:delete(A),
        file:delete(B)
    end.

load_multi_mixed_extensions_test() ->
    %% Comma-separated sources with different extensions: each is parsed by
    %% the device implied by its own extension before being merged.
    {ok, Conf} = load("test/config.flat,test/config.json", #{}),
    ?assertEqual(1234, hb_maps:get(<<"port">>, Conf)),
    ?assertEqual(false, hb_maps:get(<<"await-inprogress">>, Conf)),
    ?assertEqual(9001, hb_maps:get(<<"example">>, Conf)),
    ?assertMatch(
        [#{ <<"store-module">> := hb_store_fs }|_],
        hb_maps:get(<<"store">>, Conf)
    ).

preloaded_env_override_test() ->
    StorePath = "/tmp/hb-preloaded-env-test",
    Index = "abc123",
    os:putenv("HB_PRELOADED_STORE", StorePath),
    os:putenv("HB_PRELOADED_DEVICES_INDEX", Index),
    erase({os_env, "HB_PRELOADED_STORE"}),
    erase({os_env, "HB_PRELOADED_DEVICES_INDEX"}),
    erase({processed_env, <<"preloaded-store">>}),
    erase({processed_env, <<"preloaded-devices-index">>}),
    try
            ?assertEqual(
                #{
                    <<"store-module">> => hb_store_lmdb,
                    <<"name">> => hb_util:bin(StorePath)
                },
            ?MODULE:get(preloaded_store, undefined, #{})
        ),
        ?assertEqual(
            hb_util:bin(Index),
            ?MODULE:get(preloaded_devices_index, undefined, #{})
        )
    after
        os:unsetenv("HB_PRELOADED_STORE"),
        os:unsetenv("HB_PRELOADED_DEVICES_INDEX"),
        erase({os_env, "HB_PRELOADED_STORE"}),
        erase({os_env, "HB_PRELOADED_DEVICES_INDEX"}),
        erase({processed_env, <<"preloaded-store">>}),
        erase({processed_env, <<"preloaded-devices-index">>})
    end.

as_identity_test() ->
    DefaultWallet = ar_wallet:new(),
    TestWallet1 = ar_wallet:new(),
    TestWallet2 = ar_wallet:new(),
    TestID2 = hb_util:human_id(TestWallet2),
    Opts = #{
        <<"test-key">> => 0,
        <<"priv-wallet">> => DefaultWallet,
        <<"identities">> => #{
            <<"testname-1">> => #{
                <<"priv-wallet">> => TestWallet1,
                <<"test-key">> => 1
            },
            TestID2 => #{
                <<"priv-wallet">> => TestWallet2,
                <<"test-key">> => 2
            }
        }
    },
    ?event({base_opts, Opts}),
    Identities = identities(Opts),
    ?event({identities, Identities}),
    % The number of identities should be 5: `default`, its ID, `testname-1`,
    % and its ID, and just the ID of `TestWallet2`.
    ?assertEqual(5, maps:size(Identities)),
    % The wallets for each of the names should be the same as the wallets we
    % provided. We also check that the settings are applied correctly.
    ?assertMatch(
        {ok, #{ <<"priv-wallet">> := DefaultWallet, <<"test-key">> := 0 }},
        as(<<"default">>, Opts)
    ),
    ?assertMatch(
        {ok, #{ <<"priv-wallet">> := DefaultWallet, <<"test-key">> := 0 }},
        as(hb_util:human_id(DefaultWallet), Opts)
    ),
    ?assertMatch(
        {ok, #{ <<"priv-wallet">> := TestWallet1, <<"test-key">> := 1 }},
        as(<<"testname-1">>, Opts)
    ),
    ?assertMatch(
        {ok, #{ <<"priv-wallet">> := TestWallet1, <<"test-key">> := 1 }},
        as(hb_util:human_id(TestWallet1), Opts)
    ),
    ?assertMatch(
        {ok, #{ <<"priv-wallet">> := TestWallet2, <<"test-key">> := 2 }},
        as(TestID2, Opts)
    ).
    
ensure_node_history_test() ->
    % Define some test data
    RequiredOpts = #{
        key1 => 
            #{
                <<"type">> => <<"string">>,
                <<"value">> => <<"value1">>
            },
        key2 => <<"value2">>
    },
    % Test case: All items have required options
    ValidOpts =
    #{
        <<"key1">> => 
            #{
                <<"type">> => <<"string">>,
                <<"value">> => <<"value1">>
            }, 
        <<"key2">> => <<"value2">>, 
        <<"extra">> => <<"value">>,
        <<"node-history">> => [
            #{
                <<"key1">> => 
                    #{
                        <<"type">> => <<"string">>,
                        <<"value">> => <<"value1">>
                    }, 
                <<"key2">> => <<"value2">>, 
                <<"extra">> => <<"value">>
            },
            #{
                <<"key1">> => 
                    #{
                        <<"type">> => <<"string">>,
                        <<"value">> => <<"value1">>
                    }, 
                <<"key2">> => <<"value2">>
            }
        ]
    },
    ?assertEqual({ok, valid}, ensure_node_history(ValidOpts, RequiredOpts)),
    ?event({valid_items, ValidOpts}),
    % Test Missing items
    MissingItems = 
    #{
        <<"key1">> => 
            #{
                <<"type">> => <<"string">>,
                <<"value">> => <<"value1">>
            }, 
        <<"node-history">> => [
            #{
                <<"key1">> => 
                    #{
                        <<"type">> => <<"string">>,
                        <<"value">> => <<"value1">>
                    }
                % missing key2

            }
        ]
    },
    ?assertEqual({error, invalid_values}, ensure_node_history(MissingItems, RequiredOpts)),
    ?event({missing_items, MissingItems}),
    % Test Invalid items
    InvalidItems =
        #{
            <<"key1">> => 
                #{
                    <<"type">> => <<"string">>,
                    <<"value">> => <<"value">>
                }, 
            <<"key2">> => <<"value2">>,
            <<"node-history">> =>
                [
                    #{
                        <<"key1">> => 
                            #{
                                <<"type">> => <<"string">>,
                                <<"value">> => <<"value2">>
                            },
                        <<"key2">> => <<"value3">>
                    }
                ]
        },
    ?assertEqual({error, invalid_values}, ensure_node_history(InvalidItems, RequiredOpts)).
-endif.
