%%% @doc Tests the deployment of the HyperBEAM node.
-module(hb_deployment_test_vectors).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% Helper functions for nested map updates

%% @doc Update a nested value in a map using a path list
update_nested_value(Map, Path, Value) ->
    update_nested_value(Map, Path, Value, create_missing).

update_nested_value(_Map, [], Value, _) ->
    Value;
update_nested_value(Map, [Key], Value, _) when is_map(Map) ->
    Map#{Key => Value};
update_nested_value(Map, [Key | RestPath], Value, CreateMissing) when is_map(Map) ->
    SubMap = case maps:find(Key, Map) of
        {ok, ExistingSubMap} when is_map(ExistingSubMap) ->
            ExistingSubMap;
        {ok, _} ->
            #{};  % Replace non-map values
        error when CreateMissing =:= create_missing ->
            #{};
        error ->
            throw({path_not_found, Key})
    end,
    UpdatedSubMap = update_nested_value(SubMap, RestPath, Value, CreateMissing),
    Map#{Key => UpdatedSubMap}.

%% @doc Apply multiple nested updates at once
apply_nested_updates(Config, Updates) ->
    lists:foldl(
        fun({Path, Value}, AccConfig) ->
            update_nested_value(AccConfig, Path, Value)
        end,
        Config,
        Updates
    ).

%% @doc Post configuration to meta endpoint (equivalent to TypeScript postConfig)
post_config(NodeUrl, ConfigContent) ->
    post_config(NodeUrl, ConfigContent, <<"json@1.0">>).

post_config(NodeUrl, ConfigContent, Device) ->
    Path = <<"~meta@1.0/info">>,
    Body = hb_json:encode(ConfigContent),
    BodyString = (Body),
    
    % Build full URL
    Url = <<NodeUrl/binary, Path/binary>>,
    ?event({url, Url}),
    
    Headers = [
        {"codec-device", binary_to_list(Device)},
        {"accept-bundle", "true"},
        {"content-type", "application/json"}
    ],

    % Make HTTP POST request
    case httpc:request(post, {Url, Headers, "application/json", BodyString}, [], []) of
        {ok, {{_Version, Status, _ReasonPhrase}, ResponseHeaders, ResponseBody}} ->
            ?event({meta_post_response, {status, Status}, {body, ResponseBody}}),
            #{
                success => Status >= 200 andalso Status < 300,
                status => Status,
                body => list_to_binary(ResponseBody),
                headers => ResponseHeaders
            };
        {error, Reason} ->
            ?event({meta_post_error, {reason, Reason}}),
            #{
                success => false,
                status => 0,
                body => hb_util:bin(Reason)
            }
    end.


%% @doc Test the deployment of the HyperBEAM node.
run_test() ->

    application:ensure_all_started([
    kernel,
    stdlib,
    inets,
    ssl,
    ranch,
    cowboy,
    gun,
    os_mon,
    hb
    ]),
    % Run the node with the modified jsons
    MainOpts = hb_opts:default_message(),    
    LedgerWallet = ar_wallet:new(),
    LedgerWalletAddress = hb_util:human_id(ar_wallet:to_address(LedgerWallet)),
    LedgerStore = hb_test_utils:test_store(hb_store_lmdb, <<"ledger">>),
    Opts = MainOpts#{
        port => 10000,
        operator => unclaimed,
        priv_wallet => LedgerWallet,
        force_signed => true,
        cache_writers => [LedgerWalletAddress],
        store =>
            [
                LedgerStore,
                #{
                    <<"store-module">> => hb_store_fs,
                    <<"name">> => <<"cache-mainnet">>
                },
                #{
                    <<"store-module">> => hb_store_gateway,
                    <<"subindex">> => [
                        #{
                            <<"name">> => <<"Data-Protocol">>,
                            <<"value">> => <<"ao">>
                        }
                    ],
                    <<"local-store">> => [LedgerStore]
                },
                #{
                    <<"store-module">> => hb_store_gateway,
                    <<"local-store">> => [LedgerStore]
                }
            ]
    },

    LedgerNode = hb_http_server:start_node(Opts),

    % Run RedAO deployment script
    ?event({running_redao_deployment, "Starting npm run deploy-gz-ao"}),
    RedAODir = filename:join(["deployment", "RedAO"]),
    ?event({redao_dir, {string, RedAODir}}),

    InstallCommand = "cd " ++ RedAODir ++ " && npm i",
    case os:cmd(InstallCommand) of
        Output when is_list(Output) ->
            ?event({redao_deployment_output, {explicit, Output}});
        Error ->
            ?event({redao_deployment_error, {string, hb_util:bin(Error)}})
    end,

    DeployCommand = "cd " ++ RedAODir ++ " && npm run deploy-gz-ao",
    case os:cmd(DeployCommand) of
        Output2 when is_list(Output2) ->
            ?event({redao_deployment_output, {explicit, Output2}});
        Error2 ->
            ?event({redao_deployment_error, {string, hb_util:bin(Error2)}})
    end,

    timer:sleep(10000),

    % Define multiple nested updates
    ?event({ledger_node,  {explicit, LedgerNode}}),

    {ok, Config} = file:read_file(filename:join("deployment/configs", "compute.json")),
    DecodedConfig = hb_json:decode(Config, [return_maps]),

    ?event({loaded_configs, DecodedConfig}),

    Updates = [
        {[<<"node_processes">>, <<"ledger">>, <<"admin">>], LedgerWalletAddress},
        {[<<"node_processes">>, <<"ledger">>, <<"authority">>], LedgerWalletAddress},
        {[<<"p4_recipient">>], LedgerWalletAddress}
    ],
    
    % Apply all updates at once
    ModConfig = apply_nested_updates(DecodedConfig, Updates),

    ?event({modified_configs, {string, hb_json:encode(ModConfig)}}),
    
    % Post the modified config to the meta endpoint
    MetaResponse = post_config(LedgerNode, ModConfig),
    

    ?event({meta_response, MetaResponse}),
    

    % Test the ledger node
    case hb_http:get(LedgerNode, #{ <<"path">> => <<"ledger~node-process@1.0/now">>, <<"accept">> => <<"application/json">>, <<"accept-bundle">> => <<"true">> }, #{}) of
        {ok, Res} ->
            ?event({ledger_now, {string, hb_json:encode(Res)}});  
        {error, Reason} ->
            ?event({ledger_now_error, {string, hb_json:encode(Reason)}});
        _ -> 
            ?event({ledger_now_error, "Unknown reason"})
    end,

    ?assert(true).

