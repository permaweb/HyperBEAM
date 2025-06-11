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
-export([default_message/0, mimic_default_types/3]).
-export([ensure_node_history/2]).
-export([check_required_opts/2]).
-include("include/hb.hrl").


%% @doc The default configuration options of the hyperbeam node.
default_message() ->
    #{
      %%%%%%%% Functional options %%%%%%%%
      hb_config_location => <<"config.flat">>,
      initialized => true,
      %% What HTTP client should the node use?
      %% Options: gun, httpc
      http_client => gun,
      %% Scheduling mode: Determines when the SU should inform the recipient
      %% that an assignment has been scheduled for a message.
      %% Options: aggressive(!), local_confirmation, remote_confirmation,
      %%          disabled
      scheduling_mode => local_confirmation,
      %% Compute mode: Determines whether the process device should attempt to
      %% execute more messages on a process after it has returned a result.
      %% Options: aggressive, lazy
      compute_mode => lazy,
      %% Choice of remote nodes for tasks that are not local to hyperbeam.
      host => <<"localhost">>,
      gateway => <<"https://arweave.net">>,
      bundler_ans104 => <<"https://up.arweave.net:443">>,
      %% Location of the wallet keyfile on disk that this node will use.
      priv_key_location => <<"hyperbeam-key.json">>,
      %% The time-to-live that should be specified when we register
      %% ourselves as a scheduler on the network.
      %% Default: 7 days.
      scheduler_location_ttl => (60 * 60 * 24 * 7) * 1000,
      %% Preloaded devices for the node to use. These names override
      %% resolution of devices via ID to the default implementations.
      preloaded_devices => [#{<<"name">> => <<"apply@1.0">>, <<"module">> => dev_apply},
                            #{<<"name">> => <<"ans104@1.0">>, <<"module">> => dev_codec_ans104},
                            #{<<"name">> => <<"compute@1.0">>, <<"module">> => dev_cu},
                            #{<<"name">> => <<"cache@1.0">>, <<"module">> => dev_cache},
                            #{<<"name">> => <<"cacheviz@1.0">>, <<"module">> => dev_cacheviz},
                            #{<<"name">> => <<"cron@1.0">>, <<"module">> => dev_cron},
                            #{<<"name">> => <<"dedup@1.0">>, <<"module">> => dev_dedup},
                            #{<<"name">> => <<"delegated-compute@1.0">>, <<"module">> => dev_delegated_compute},
                            #{<<"name">> => <<"faff@1.0">>, <<"module">> => dev_faff},
                            #{<<"name">> => <<"flat@1.0">>, <<"module">> => dev_codec_flat},
                            #{<<"name">> => <<"genesis-wasm@1.0">>, <<"module">> => dev_genesis_wasm},
                            #{<<"name">> => <<"greenzone@1.0">>, <<"module">> => dev_green_zone},
                            #{<<"name">> => <<"httpsig@1.0">>, <<"module">> => dev_codec_httpsig},
                            #{<<"name">> => <<"hyperbuddy@1.0">>, <<"module">> => dev_hyperbuddy},
                            #{<<"name">> => <<"json@1.0">>, <<"module">> => dev_codec_json},
                            #{<<"name">> => <<"json-iface@1.0">>, <<"module">> => dev_json_iface},
                            #{<<"name">> => <<"local-name@1.0">>, <<"module">> => dev_local_name},
                            #{<<"name">> => <<"lookup@1.0">>, <<"module">> => dev_lookup},
                            #{<<"name">> => <<"lua@5.3a">>, <<"module">> => dev_lua},
                            #{<<"name">> => <<"manifest@1.0">>, <<"module">> => dev_manifest},
                            #{<<"name">> => <<"message@1.0">>, <<"module">> => dev_message},
                            #{<<"name">> => <<"meta@1.0">>, <<"module">> => dev_meta},
                            #{<<"name">> => <<"monitor@1.0">>, <<"module">> => dev_monitor},
                            #{<<"name">> => <<"multipass@1.0">>, <<"module">> => dev_multipass},
                            #{<<"name">> => <<"name@1.0">>, <<"module">> => dev_name},
                            #{<<"name">> => <<"node-process@1.0">>, <<"module">> => dev_node_process},
                            #{<<"name">> => <<"p4@1.0">>, <<"module">> => dev_p4},
                            #{<<"name">> => <<"patch@1.0">>, <<"module">> => dev_patch},
                            #{<<"name">> => <<"poda@1.0">>, <<"module">> => dev_poda},
                            #{<<"name">> => <<"process@1.0">>, <<"module">> => dev_process},
                            #{<<"name">> => <<"push@1.0">>, <<"module">> => dev_push},
                            #{<<"name">> => <<"relay@1.0">>, <<"module">> => dev_relay},
                            #{<<"name">> => <<"router@1.0">>, <<"module">> => dev_router},
                            #{<<"name">> => <<"scheduler@1.0">>, <<"module">> => dev_scheduler},
                            #{<<"name">> => <<"simple-pay@1.0">>, <<"module">> => dev_simple_pay},
                            #{<<"name">> => <<"snp@1.0">>, <<"module">> => dev_snp},
                            #{<<"name">> => <<"stack@1.0">>, <<"module">> => dev_stack},
                            #{<<"name">> => <<"structured@1.0">>, <<"module">> => dev_codec_structured},
                            #{<<"name">> => <<"test-device@1.0">>, <<"module">> => dev_test},
                            #{<<"name">> => <<"volume@1.0">>, <<"module">> => dev_volume},
                            #{<<"name">> => <<"wasi@1.0">>, <<"module">> => dev_wasi},
                            #{<<"name">> => <<"wasm-64@1.0">>, <<"module">> => dev_wasm}],
      %% Default execution cache control options
      cache_control => [<<"no-cache">>, <<"no-store">>],
      cache_lookup_hueristics => false,
      % Should we await in-progress executions, rather than re-running?
      % Has three settings: false, only `named' executions, or all executions.
      await_inprogress => named,
      %% Should the node attempt to access data from remote caches for
      %% client requests?
      access_remote_cache_for_client => false,
      %% Should the node attempt to load devices from remote signers?
      load_remote_devices => false,
      %% The list of device signers that the node should trust.
      trusted_device_signers => [],
      %% What should the node do if a client error occurs?
      client_error_strategy => throw,
      %% HTTP request options
      http_connect_timeout => 5000,
      http_keepalive => 120000,
      http_request_send_timeout => 60000,
      port => 8734,
      wasm_allow_aot => false,
      %% Options for the relay device
      relay_http_client => httpc,
      %% The default codec to use for commitment signatures.
      commitment_device => <<"httpsig@1.0">>,
      %% Dev options
      mode => debug,
      % Every modification to `Opts' called directly by the node operator
      % should be recorded here.
      node_history => [],
      debug_stack_depth => 40,
      debug_print_map_line_threshold => 30,
      debug_print_binary_max => 60,
      debug_print_indent => 2,
      debug_print => false,
      stack_print_prefixes => ["hb", "dev", "ar", "maps"],
      debug_print_trace => short,  % `short` | `false`. Has performance impact.
      short_trace_len => 20,
      debug_metadata => true,
      debug_ids => true,
      debug_committers => false,
      debug_show_priv => if_present,
      debug_resolve_links => false,
      trusted => #{},
      routes => [#{
                   % Routes for the genesis-wasm device to use a local CU, if requested.
                   <<"template">> => <<"/result/.*">>,
                   <<"node">> => #{<<"prefix">> => <<"http://localhost:6363">>}
                  },
                 #{
                   % Routes for GraphQL requests to use a remote GraphQL API.
                   <<"template">> => <<"/graphql">>,
                   <<"nodes">> =>
                       [#{
                          <<"prefix">> => <<"https://arweave-search.goldsky.com">>,
                          <<"opts">> => #{http_client => httpc, protocol => http2}
                         },
                        #{
                          <<"prefix">> => <<"https://arweave.net">>,
                          <<"opts">> => #{http_client => gun, protocol => http2}
                         }]
                  },
                 #{
                   % Routes for raw data requests to use a remote gateway.
                   <<"template">> => <<"/raw">>,
                   <<"node">> =>
                       #{
                         <<"prefix">> => <<"https://arweave.net">>,
                         <<"opts">> => #{http_client => gun, protocol => http2}
                        }
                  }],
      store =>
          [
           % #{
           %     <<"name">> => <<"cache-mainnet/lru">>,
           %     <<"capacity">> => 512 * 1024 * 1024,
           %     <<"store-module">> => hb_store_lru,
           %     <<"persistent-store">> => #{
           %         <<"store-module">> => hb_store_fs,
           %         <<"name">> => <<"cache-mainnet/lru">>
           %     }
           % },
           #{
             <<"name">> => <<"cache-mainnet/lmdb">>,
             <<"store-module">> => hb_store_lmdb
            },
           #{
             <<"store-module">> => hb_store_fs,
             <<"name">> => <<"cache-mainnet">>
            },
           #{
             <<"store-module">> => hb_store_gateway,
             <<"subindex">> => [#{
                                  <<"name">> => <<"Data-Protocol">>,
                                  <<"value">> => <<"ao">>
                                 }],
             <<"store">> =>
                 [#{
                    <<"store-module">> => hb_store_lmdb,
                    <<"name">> => <<"cache-mainnet/lmdb">>
                   }]
            },
           #{
             <<"store-module">> => hb_store_gateway,
             <<"store">> =>
                 [#{
                    <<"store-module">> => hb_store_lmdb,
                    <<"name">> => <<"cache-mainnet/lmdb">>
                   }]
            }],
      default_index => #{<<"device">> => <<"hyperbuddy@1.0">>},
      % Should we use the latest cached state of a process when computing?
      process_now_from_cache => false,
      % Should we trust the GraphQL API when converting to ANS-104? Some GQL
      % services do not provide the `anchor' or `last_tx' fields, so their
      % responses are not verifiable.
      ans104_trust_gql => true,
      http_extra_opts =>
          #{
            force_message => true,
            cache_control => [<<"always">>]
           },
      % Should the node store all signed messages?
      store_all_signed => true,
      % Should the node use persistent processes?
      process_workers => false,
      % Options for the router device
      router_registration_opts => #{}
      % Should the node track and expose prometheus metrics?
      % We do not set this explicitly, so that the hb_features:test() value
      % can be used to determine if we should expose metrics instead,
      % dynamically changing the configuration based on whether we are running
      % tests or not. To override this, set the `prometheus' option explicitly.
      % prometheus => false
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


get(Key, Default, Opts) when is_binary(Key) ->
    try binary_to_existing_atom(Key, utf8) of
        AtomKey -> get(AtomKey, Default, Opts)
    catch
        error:badarg -> Default
    end;
get(Key, Default, Opts = #{<<"only">> := Only}) ->
    get(Key, Default, maps:remove(<<"only">>, Opts#{only => Only}));
get(Key, Default, Opts = #{<<"prefer">> := Prefer}) ->
    get(Key, Default, maps:remove(<<"prefer">>, Opts#{prefer => Prefer}));
get(Key, Default, Opts = #{only := local}) ->
    case maps:find(Key, Opts) of
        {ok, Value} -> Value;
        error ->
            Default
    end;
get(Key, Default, Opts = #{only := global}) ->
    case global_get(Key, hb_opts_not_found, Opts) of
        hb_opts_not_found -> Default;
        Value -> Value
    end;
get(Key, Default, Opts = #{prefer := global}) ->
    case ?MODULE:get(Key, hb_opts_not_found, #{only => global}) of
        hb_opts_not_found -> ?MODULE:get(Key, Default, Opts#{only => local});
        Value -> Value
    end;
get(Key, Default, Opts = #{prefer := local}) ->
    case ?MODULE:get(Key, hb_opts_not_found, Opts#{only => local}) of
        hb_opts_not_found ->
            ?MODULE:get(Key, Default, Opts#{only => global});
        Value -> Value
    end;
get(Key, Default, Opts) ->
    % No preference was set in Opts, so we default to local.
    ?MODULE:get(Key, Default, Opts#{prefer => local}).


-ifdef(TEST).
-define(DEFAULT_PRINT_OPTS, "error,http_error").
-else.
-define(DEFAULT_PRINT_OPTS, "error,http_error,http_short,compute_short,push_short").
-endif.

-define(ENV_KEYS,
        #{
          priv_key_location => {"HB_KEY", "hyperbeam-key.json"},
          hb_config_location => {"HB_CONFIG", "config.flat"},
          port => {"HB_PORT", fun erlang:list_to_integer/1, "8734"},
          mode => {"HB_MODE", fun list_to_existing_atom/1},
          debug_print =>
              {"HB_PRINT",
               fun(Str) when Str == "1" -> true;
                  (Str) when Str == "true" -> true;
                  (Str) ->
                       lists:map(fun hb_util:bin/1, string:tokens(Str, ","))
               end,
               ?DEFAULT_PRINT_OPTS},
          lua_scripts => {"LUA_SCRIPTS", "scripts"},
          lua_tests => {"LUA_TESTS", fun dev_lua_test:parse_spec/1, tests}
         }).


%% @doc Get an environment variable or configuration key.
global_get(Key, Default, Opts) ->
    case maps:get(Key, ?ENV_KEYS, Default) of
        Default -> config_lookup(Key, Default, Opts);
        {EnvKey, ValParser, DefaultValue} when is_function(ValParser) ->
            ValParser(cached_os_env(EnvKey, normalize_default(DefaultValue)));
        {EnvKey, ValParser} when is_function(ValParser) ->
            case cached_os_env(EnvKey, not_found) of
                not_found -> config_lookup(Key, Default, Opts);
                Value -> ValParser(Value)
            end;
        {EnvKey, DefaultValue} ->
            cached_os_env(EnvKey, DefaultValue)
    end.


%% @doc Cache the result of os:getenv/1 in the process dictionary, as it never
%% changes during the lifetime of a node.
cached_os_env(Key, DefaultValue) ->
    case erlang:get({os_env, Key}) of
        undefined ->
            case os:getenv(Key) of
                false -> DefaultValue;
                Value ->
                    erlang:put({os_env, Key}, Value),
                    Value
            end;
        Value -> Value
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
config_lookup(Key, Default, Opts) -> hb_maps:get(Key, default_message(), Default, Opts).


%% @doc Parse a `flat@1.0' encoded file into a map, matching the types of the
%% keys to those in the default message.
load(Path) -> load(Path, #{}).


load(Path, Opts) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            load_bin(Bin, Opts);
        _ -> {error, not_found}
    end.


load_bin(Bin, Opts) ->
    % Trim trailing whitespace from each line in the file.
    Ls =
        lists:map(
          fun(Line) -> string:trim(Line, trailing) end,
          binary:split(Bin, <<"\n">>, [global])),
    try dev_codec_flat:deserialize(iolist_to_binary(lists:join(<<"\n">>, Ls))) of
        {ok, Map} ->
            {ok, mimic_default_types(Map, new_atoms, Opts)}
    catch
        error:B -> {error, B}
    end.


%% @doc Mimic the types of the default message for a given map.
mimic_default_types(Map, Mode, Opts) ->
    Default = default_message(),
    hb_maps:from_list(lists:map(
                        fun({Key, Value}) ->
                                NewKey = try hb_util:key_to_atom(Key, Mode) catch _:_ -> Key end,
                                NewValue =
                                    case hb_maps:get(NewKey, Default, not_found, Opts) of
                                        not_found -> Value;
                                        DefaultValue when is_atom(DefaultValue) ->
                                            hb_util:atom(Value);
                                        DefaultValue when is_integer(DefaultValue) ->
                                            hb_util:int(Value);
                                        DefaultValue when is_float(DefaultValue) ->
                                            hb_util:float(Value);
                                        DefaultValue when is_binary(DefaultValue) ->
                                            Value;
                                        _ -> Value
                                    end,
                                {NewKey, NewValue}
                        end,
                        hb_maps:to_list(Map, Opts))).


%% @doc Find a given identity from the `identities' map, and return the options
%% merged with the sub-options for that identity.
as(Identity, Opts) ->
    case identities(Opts) of
        #{Identity := SubOpts} ->
            ?event({found_identity_sub_opts_are, SubOpts}),
            {ok, maps:merge(Opts, mimic_default_types(SubOpts, new_atoms, Opts))};
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
                                 case maps:find(priv_wallet, SubOpts) of
                                     {ok, Wallet} ->
                                         Addr = hb_util:human_id(ar_wallet:to_address(Wallet)),
                                         {true, {Addr, SubOpts}};
                                     error -> false
                                 end
                         end,
                         maps:to_list(Named))),
    % Merge the named and address-based maps. Normalize each result to ensure
    % that the `address' property is present and accurate.
    Identities =
        maps:map(
          fun(_NameOrID, SubOpts) ->
                  case maps:find(priv_wallet, SubOpts) of
                      {ok, Wallet} ->
                          SubOpts#{<<"address">> => hb_util:human_id(Wallet)};
                      error -> SubOpts
                  end
          end,
          maps:merge(Named, Addresses)),
    ?event({identities_without_default, Identities}),
    % Add a default identity if one is not already present.
    DefaultWallet = ?MODULE:get(priv_wallet, Default, Opts),
    case maps:find(DefaultID = hb_util:human_id(DefaultWallet), Identities) of
        {ok, _} -> Identities;
        error ->
            Identities#{
              DefaultID => #{
                             priv_wallet => DefaultWallet
                            },
              <<"default">> => #{
                                 priv_wallet => DefaultWallet
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
                    KeyValuePairs),
    case MissingOpts of
        [] ->
            {ok, Opts};
        _ ->
            MissingOptsStr = binary:list_to_bin(
                               lists:join(<<", ">>, MissingOpts)),
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
        NodeHistoryWithOpts = [Opts | NodeHistory],
        % Normalize required options
        NormalizedRequiredOpts ?= hb_ao:normalize_keys(RequiredOpts),
        % Normalize all node history items once
        NormalizedNodeHistory ?= lists:map(
                                   fun(Item) ->
                                           hb_ao:normalize_keys(Item)
                                   end,
                                   NodeHistoryWithOpts),
        % Get the first item (complete opts) and remaining items (differences)
        [FirstItem | RemainingItems] = NormalizedNodeHistory,

        % Step 1: Validate first item has all required keys
        FirstItemKeysPresent = lists:all(
                                 fun(Key) ->
                                         maps:is_key(Key, FirstItem)
                                 end,
                                 maps:keys(NormalizedRequiredOpts)),
        true ?= FirstItemKeysPresent orelse {error, keys_missing},

        % Step 2: Validate first item values match requirements
        FirstItemValuesMatch = hb_message:match(FirstItem, NormalizedRequiredOpts, only_present),
        true ?= (FirstItemValuesMatch == true) orelse {error, values_invalid},
        % Step 3: Check that remaining items don't modify required keys
        NoRequiredKeysModified = lists:all(
                                   fun(HistoryItem) ->
                                           % For each required key, if it exists in this history item,
                                           % it must match the value from the first item
                                           lists:all(
                                             fun(RequiredKey) ->
                                                     case maps:find(RequiredKey, HistoryItem) of
                                                         {ok, Value} ->
                                                             % Key exists in history item, check it matches first item
                                                             case maps:find(RequiredKey, FirstItem) of
                                                                 {ok, Value} -> true;  % Values match
                                                                 {ok, _DifferentValue} -> false;  % Values differ
                                                                 error -> false  % Key missing from first item (shouldn't happen)
                                                             end;
                                                         error ->
                                                             % Key doesn't exist in this history item, which is fine
                                                             true
                                                     end
                                             end,
                                             maps:keys(NormalizedRequiredOpts))
                                   end,
                                   RemainingItems),
        true ?= NoRequiredKeysModified orelse {error, required_key_modified},

        % If we've made it this far, everything is valid
        ?event({validate_node_history_items, all_items_valid}),
        {ok, <<"valid">>}
    else
        {error, keys_missing} ->
            ?event({validate_node_history_items, validation_failed, missing_keys}),
            {error, <<"missing_keys">>};
        {error, values_invalid} ->
            ?event({validate_node_history_items, validation_failed, invalid_values}),
            {error, <<"invalid_values">>};
        {error, required_key_modified} ->
            ?event({validate_node_history_items, validation_failed, required_key_modified}),
            {error, <<"modified_required_key">>};
        _ ->
            ?event({validate_node_history_items, validation_failed, unknown}),
            {error, <<"validation_failed">>}
    end.


%%% Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


global_get_test() ->
    ?assertEqual(debug, ?MODULE:get(mode)),
    ?assertEqual(debug, ?MODULE:get(mode, production)),
    ?assertEqual(undefined, ?MODULE:get(unset_global_key)),
    ?assertEqual(1234, ?MODULE:get(unset_global_key, 1234)).


local_get_test() ->
    Local = #{only => local},
    ?assertEqual(undefined,
                 ?MODULE:get(test_key, undefined, Local)),
    ?assertEqual(correct,
                 ?MODULE:get(test_key, undefined, Local#{test_key => correct})).


local_preference_test() ->
    Local = #{prefer => local},
    ?assertEqual(correct,
                 ?MODULE:get(test_key, undefined, Local#{test_key => correct})),
    ?assertEqual(correct,
                 ?MODULE:get(mode, undefined, Local#{mode => correct})),
    ?assertNotEqual(undefined,
                    ?MODULE:get(mode, undefined, Local)).


global_preference_test() ->
    Global = #{prefer => global},
    ?assertEqual(undefined, ?MODULE:get(test_key, undefined, Global)),
    ?assertNotEqual(incorrect,
                    ?MODULE:get(mode, undefined, Global#{mode => incorrect})),
    ?assertNotEqual(undefined, ?MODULE:get(mode, undefined, Global)).


load_test() ->
    % File contents:
    % port: 1234
    % host: https://ao.computer
    % await-inprogress: false
    {ok, Conf} = load("test/config.flat", #{}),
    ?event({loaded, {explicit, Conf}}),
    % Ensure we convert types as expected.
    ?assertEqual(1234, hb_maps:get(port, Conf)),
    % A binary
    ?assertEqual(<<"https://ao.computer">>, hb_maps:get(host, Conf)),
    % An atom, where the key contained a header-key `-' rather than a `_'.
    ?assertEqual(false, hb_maps:get(await_inprogress, Conf)).


as_identity_test() ->
    DefaultWallet = ar_wallet:new(),
    TestWallet1 = ar_wallet:new(),
    TestWallet2 = ar_wallet:new(),
    TestID2 = hb_util:human_id(TestWallet2),
    Opts = #{
             test_key => 0,
             priv_wallet => DefaultWallet,
             identities => #{
                             <<"testname-1">> => #{
                                                   priv_wallet => TestWallet1,
                                                   test_key => 1
                                                  },
                             TestID2 => #{
                                          priv_wallet => TestWallet2,
                                          test_key => 2
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
      {ok, #{priv_wallet := DefaultWallet, test_key := 0}},
      as(<<"default">>, Opts)),
    ?assertMatch(
      {ok, #{priv_wallet := DefaultWallet, test_key := 0}},
      as(hb_util:human_id(DefaultWallet), Opts)),
    ?assertMatch(
      {ok, #{priv_wallet := TestWallet1, test_key := 1}},
      as(<<"testname-1">>, Opts)),
    ?assertMatch(
      {ok, #{priv_wallet := TestWallet1, test_key := 1}},
      as(hb_util:human_id(TestWallet1), Opts)),
    ?assertMatch(
      {ok, #{priv_wallet := TestWallet2, test_key := 2}},
      as(TestID2, Opts)).


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
          node_history => [#{
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
                            }]
         },
    ?assertEqual({ok, <<"valid">>}, ensure_node_history(ValidOpts, RequiredOpts)),
    ?event({valid_items, ValidOpts}),
    % Test Missing items
    MissingItems =
        #{
          <<"key1">> =>
              #{
                <<"type">> => <<"string">>,
                <<"value">> => <<"value1">>
               },
          node_history => [#{
                             <<"key1">> =>
                                 #{
                                   <<"type">> => <<"string">>,
                                   <<"value">> => <<"value1">>
                                  }
                             % missing key2

                            }]
         },

    ?assertEqual({error, <<"missing_keys">>}, ensure_node_history(MissingItems, RequiredOpts)),
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
          node_history =>
              [#{
                 <<"key1">> =>
                     #{
                       <<"type">> => <<"string">>,
                       <<"value">> => <<"value2">>
                      },
                 <<"key2">> => <<"value3">>
                }]
         },
    ?assertEqual({error, <<"invalid_values">>}, ensure_node_history(InvalidItems, RequiredOpts)).


-endif.
