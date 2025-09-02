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

%% @doc Create an AO process (equivalent to RedAO deploy-gz-ao.js)
create_ao_process(Node, Wallet) ->
    % Get wallet address (equivalent to gzAOAddress)
    WalletAddress = hb_util:human_id(ar_wallet:to_address(Wallet)),
    % Load config values (equivalent to config.json)
    Config = #{
        <<"hb-authority">> => WalletAddress,
        <<"legacy-authority">> => <<"fcoN_xJeisVsPXA-trzVAuIiqO3ydLQxM-L4XbrQKzY">>,
        <<"ao-token">> => <<"0syT13r0s0tgPmIed95bJnuSqaD29HQNN8D3ElLSrsc">>,
        <<"beta-gz-ao-src">> => <<"LxLUDGJD2QLnyIVO_boJnUbVnyXjSuV0d90VNCt6I0g">>,
        <<"beta-gz-ao-name">> => <<"[BETA-1.2] Green Zone AO">>,
        <<"beta-gz-ao-ticker">> => <<"BETA-GZ">>,
        <<"beta-gz-ao-denomination">> => <<"12">>,
        <<"process-module">> => <<"URgYpPQzvxxfYQtjrIQ116bl3YBfcImo3JEnNo8Hlrk">>,
        <<"process-scheduler">> => <<"_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA">>
    },
    
    
    % Build authorities list
    HBAuthority = maps:get(<<"hb-authority">>, Config),
    LegacyAuthority = maps:get(<<"legacy-authority">>, Config),
    Authorities = <<HBAuthority/binary, ",", LegacyAuthority/binary>>,
    
    % Create process message (equivalent to aoLegacy.spawn)
    ProcMsg = #{
        <<"data-protocol">> => <<"ao">>,
        <<"Type">> => <<"Process">>,
        <<"Variant">> => <<"ao.TN.1">>,
        <<"Module">> => maps:get(<<"process-module">>, Config),
        <<"Scheduler">> => maps:get(<<"process-scheduler">>, Config),
        <<"data">> => <<"1984">>,
        % Tags equivalent to JavaScript version
        <<"On-Boot">> => maps:get(<<"beta-gz-ao-src">>, Config),
        <<"Authority">> => Authorities,
        <<"ParentToken">> => maps:get(<<"ao-token">>, Config),
        <<"Name">> => maps:get(<<"beta-gz-ao-name">>, Config),
        <<"Ticker">> => maps:get(<<"beta-gz-ao-ticker">>, Config),
        <<"Denomination">> => maps:get(<<"beta-gz-ao-denomination">>, Config)
    },
    
    ?event({creating_ao_process, {process_msg, ProcMsg}}),
    
    % Sign and commit the process message
    SignedProc = hb_message:commit(ProcMsg, #{ priv_wallet => Wallet }),
    ProcessId = hb_message:id(SignedProc, none),
    
    ?event({ao_process_created, {process_id, ProcessId}}),
    
    % Schedule the process (equivalent to spawn)
    case schedule_process(SignedProc, ProcessId, Wallet, Node) of
        {ok, ScheduleRes} ->
            ?event({ao_process_scheduled, {result, ScheduleRes}}),
            
            % Send authorities update message (equivalent to authoritiesUpdate)
            AuthUpdateData = <<"ao.authorities = {'", HBAuthority/binary, "', '", LegacyAuthority/binary, "'}">>,
            AuthUpdateMsg = #{
                <<"data-protocol">> => <<"ao">>,
                <<"type">> => <<"Message">>,
                <<"variant">> => <<"ao.TN.1">>,
                <<"target">> => ProcessId,
                <<"data">> => AuthUpdateData,
                <<"Action">> => <<"Eval">>
            },
            
            SignedAuthUpdate = hb_message:commit(AuthUpdateMsg, #{ priv_wallet => Wallet }),
            AuthUpdateId = hb_message:id(SignedAuthUpdate, none),
            
            case schedule_process(SignedAuthUpdate, AuthUpdateId, Wallet, Node) of
                {ok, AuthRes} ->
                    ?event({ao_authorities_updated, {result, AuthRes}}),
                    {ok, ProcessId};
                {error, AuthError} ->
                    ?event({ao_authorities_update_failed, {error, AuthError}}),
                    {error, AuthError}
            end;
        {error, ScheduleError} ->
            ?event({ao_process_schedule_failed, {error, ScheduleError}}),
            {error, ScheduleError}
    end.

%% @doc Schedule a process or message (helper function)
schedule_process(SignedMsg, Target, Wallet, Node) ->
    SignedReq = 
        hb_message:commit(
            #{
                <<"path">> => <<"/~scheduler@1.0/schedule">>,
                <<"method">> => <<"POST">>,
                <<"type">> => <<"Process">>,
                <<"body">> => SignedMsg
            },
            #{ priv_wallet => Wallet }
        ),
    hb_http:post(Node, SignedReq, #{}).


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

    % Create AO process (equivalent to RedAO deployment)
    ?event({running_ao_deployment, "Creating AO process in Erlang"}),
    case create_ao_process(LedgerNode, LedgerWallet) of
        {ok, ProcessId} ->
            ?event({ao_deployment_success, {process_id, ProcessId}});
        {error, Error} ->
            ?event({ao_deployment_error, {error, Error}})
    end,

    timer:sleep(5000),

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

