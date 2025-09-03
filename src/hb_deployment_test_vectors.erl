    %%% @doc Tests the deployment of the HyperBEAM node.
    -module(hb_deployment_test_vectors).
    -include_lib("eunit/include/eunit.hrl").
    -include("include/hb.hrl").

    %% @doc Main test orchestration function
    run_test() ->
        LegacyAddress = <<"fcoN_xJeisVsPXA-trzVAuIiqO3ydLQxM-L4XbrQKzY">>,
        Amount = <<"3">>,

        % Initialize
        % init_applications(),
        
        % Setup
        {_LedgerOpts, _LedgerWallet, LedgerAddress, LedgerNode, LedgerStore} = 
            setup_node_config(<<"ledger">>),

        % Get wallet path from environment or use default
        RedAOWalletPath = case os:getenv("WALLET_PATH") of
            EnvPath -> EnvPath;
            false -> "/test/wallet.json"
        end,
        
        ?event({using_wallet_path, {path, RedAOWalletPath}}),
        {RedAOWallet, RedAOWalletAddress} = load_wallet(RedAOWalletPath),
        
        % Deploy AO process
        {ok, RedAOProcessId} = deploy_ao_process_step(RedAOWallet, LedgerAddress, LegacyAddress),
        ?event({ao_process_deployed, {explicit, RedAOProcessId}}),
        
        % Setup configuration
        ?event({setting_up_compute_config, {explicit, LedgerAddress}, {explicit, RedAOProcessId}, {explicit, RedAOWalletAddress}}),
        ModConfig = setup_compute_config(LedgerAddress, RedAOProcessId, RedAOWalletAddress, LegacyAddress),
        
        % Post configuration
        _LedgerMetaPostResponse = post_config(LedgerNode, ModConfig),

        {Status, LedgerProcessId} = hb_http:get(LedgerNode, <<"/ledger~node-process@1.0/commitments/keys/1">>, #{}),
        ?event({ledger_commitments_keys_1, {status, Status}, {result, LedgerProcessId}}),
        % Test functionality

        % Transfer AO tokens to the ledger node
        {ok, TransferId, TransferBody} = transfer_ao_tokens(RedAOProcessId, Amount, RedAOWallet),
        ?event({transfer_completed, {transfer_id, TransferId}, {body, TransferBody}}),
        
        timer:sleep(15000),

        LedgerTransferMsg = #{
            <<"type">> => <<"Message">>,
            <<"data">> => <<"">>,
            <<"anchor">> => <<"">>,
            <<"data-protocol">> => <<"ao">>,
            <<"variant">> => <<"ao.N.1">>,
            <<"accept-bundle">> => <<"true">>,
            <<"accept-codec">> => <<"httpsig@1.0">>,
            <<"action">> => <<"Transfer">>,
            <<"target">> => RedAOProcessId,
            <<"recipient">> => RedAOWalletAddress,
            <<"route">> => LedgerProcessId,
            <<"quantity">> => Amount
        },
        
        % Sign and serialize the transfer message
        {LedgerTransferId, LedgerTransferANS104} = sign_and_serialize(LedgerTransferMsg, RedAOWallet),
        ?event({ledger_transfer_created, {transfer_id, LedgerTransferId}}),
        
        % Send to HyperBEAM node instead of legacy AO
        NodeUrl = <<LedgerNode/binary>>,
        case send_to_hyperbeam_node(LedgerTransferANS104, NodeUrl, RedAOProcessId) of
            {ok, StatusCode, Body} ->
                ?event({ledger_transfer_response, {status, StatusCode}, {body, Body}});
            {error, Reason} ->
                ?event({ledger_transfer_error, {reason, Reason}})
        end,

        _TestResult = test_ledger_node(LedgerNode),

        timer:sleep(3000),    

        SignedMsg = 
        hb_message:commit(
            #{
                <<"path">> => <<"/Kv6jQCcs8GwNpioj6tkTt06zD130YgqIHX7QNnZQYQc~process@1.0/compute&slot=1">>
            },
            #{
                priv_wallet => RedAOWallet
            }
        ),

        {NowStatus, NowResult} = hb_http:get(LedgerNode, SignedMsg, #{}),
        ?event({ledger_now, {status, NowStatus}, {result, NowResult}}),

        timer:sleep(3000),    

        _TestResult2 = test_ledger_node(LedgerNode),

        % Assert success
        ?assert(true).

% clean_up(Stores) ->
%     lists:foreach(fun(Store) -> hb_store:stop(Store) end, Stores).


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

%% @doc Utility functions for AO deployment

%% @doc Create a new wallet and return wallet + address
create_wallet() ->
    Wallet = ar_wallet:new(),
    Address = hb_util:encode(ar_wallet:to_address(Wallet)),
    {Wallet, Address}.

%% @doc Load wallet from file and return wallet + address
load_wallet(FilePath) ->
    Wallet = ar_wallet:load_keyfile(FilePath),
    Address = hb_util:encode(ar_wallet:to_address(Wallet)),
    {Wallet, Address}.

%% @doc Build authority string from wallet address and fixed authorities
build_authorities(WalletAddress, LegacyAddress) ->
    WalletStr = binary_to_list(WalletAddress),
    LegacyStr = binary_to_list(LegacyAddress),
    AllAuthorities = [WalletStr, LegacyStr],
    list_to_binary(string:join(AllAuthorities, ",")).

%% @doc Create AO process message
create_ao_process_msg(Authority) ->
    #{
        <<"Type">> => <<"Process">>,
        <<"Data-Protocol">> => <<"ao">>,
        <<"Variant">> => <<"ao.TN.1">>,
        <<"Module">> => <<"URgYpPQzvxxfYQtjrIQ116bl3YBfcImo3JEnNo8Hlrk">>,
        <<"Scheduler">> => <<"_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA">>,
        <<"Name">> => <<"[BETA-1.2] Green Zone AO">>,
        <<"data">> => <<"1984">>,
        <<"On-Boot">> => <<"LxLUDGJD2QLnyIVO_boJnUbVnyXjSuV0d90VNCt6I0g">>,
        <<"Authority">> => Authority,
        <<"ParentToken">> => <<"0syT13r0s0tgPmIed95bJnuSqaD29HQNN8D3ElLSrsc">>,
        <<"Ticker">> => <<"BETA-GZ">>,
        <<"Denomination">> => <<"12">>
    }.

%% @doc Create authorities update message
create_authorities_msg(ProcessId, WalletAddress, LegacyAddress) ->
    WalletAddressStr = binary_to_list(WalletAddress),
    LegacyAddressStr = binary_to_list(LegacyAddress),
    AuthoritiesData = list_to_binary("ao.authorities = {'" ++ WalletAddressStr ++ 
        "', '" ++ LegacyAddressStr ++ "'}"),
    
    #{
        <<"Type">> => <<"Message">>,
        <<"Data-Protocol">> => <<"ao">>,
        <<"Variant">> => <<"ao.TN.1">>,
        <<"target">> => ProcessId,
        <<"data">> => AuthoritiesData,
        <<"Action">> => <<"Eval">>
    }.

%% @doc Create transfer message
create_transfer_msg(ProcessId, Quantity) ->
    #{
        <<"Type">> => <<"Message">>,
        <<"Data-Protocol">> => <<"ao">>,
        <<"Variant">> => <<"ao.TN.1">>,
        <<"target">> => <<"0syT13r0s0tgPmIed95bJnuSqaD29HQNN8D3ElLSrsc">>, % ao-token
        <<"Action">> => <<"Transfer">>,
        <<"Quantity">> => Quantity,
        <<"Recipient">> => ProcessId % beta-gz-ao-token
    }.

%% @doc Sign and serialize message to ANS104
sign_and_serialize(Msg, Wallet) ->
    {ok, TX} = dev_codec_ans104:to(Msg, #{}, #{}),
    SignedTX = ar_bundles:sign_item(TX, Wallet),
    ANS104Bytes = ar_bundles:serialize(SignedTX),
    ProcessId = hb_util:encode(SignedTX#tx.id),
    {ProcessId, ANS104Bytes}.

%% @doc Send ANS104 data to legacy AO endpoint
send_to_legacy_ao(ANS104Bytes) ->
    send_to_legacy_ao(ANS104Bytes, "https://mu.ao-testnet.xyz").

send_to_legacy_ao(ANS104Bytes, Url) ->
    Headers = [{"Content-Type", "application/octet-stream"}, {"Accept", "application/json"}],
    HTTPOptions = [{autoredirect, false}],
    Options = [],
    
    case httpc:request(post, {Url, Headers, "application/octet-stream", ANS104Bytes}, HTTPOptions, Options) of
        {ok, {{_Version, StatusCode, _ReasonPhrase}, ResponseHeaders, Body}} ->
            ?event({legacy_ao_response, {status, StatusCode}, {headers, ResponseHeaders}, {body, Body}}),
            {ok, StatusCode, Body};
        {error, Reason} ->
            ?event({legacy_ao_error, {reason, Reason}}),
            {error, Reason}
    end.

%% @doc Send ANS104 data to HyperBEAM node (similar to send_to_legacy_ao)
send_to_hyperbeam_node(ANS104Bytes, NodeUrl, ProcessId) ->
    Path = binary_to_list(ProcessId) ++ "~process@1.0/push",
    Url = binary_to_list(NodeUrl) ++ Path,
    
    Headers = [
        {"Content-Type", "application/ans104"}, 
        {"accept-bundle", "true"},
        {"codec-device", "ans104@1.0"}
    ],
    HTTPOptions = [{autoredirect, false}],
    Options = [],
    
    ?event({sending_to_hyperbeam_node, {url, Url}}),
    
    case httpc:request(post, {Url, Headers, "application/ans104", ANS104Bytes}, HTTPOptions, Options) of
        {ok, {{_Version, StatusCode, _ReasonPhrase}, ResponseHeaders, Body}} ->
            ?event({hyperbeam_node_response, {status, StatusCode}, {headers, ResponseHeaders}, {body, Body}}),
            {ok, StatusCode, Body};
        {error, Reason} ->
            ?event({hyperbeam_node_error, {reason, Reason}}),
            {error, Reason}
    end.

%% @doc Deploy AO process with authorities setup
deploy_ao_process(RedAOWallet, ProcessWalletAddress, LegacyAddress) ->
    % Get wallet address and build authority
    Authority = build_authorities(ProcessWalletAddress, LegacyAddress),
    
    ?event({deploying_ao_process, {wallet_address, ProcessWalletAddress}, {authority, Authority}}),
    
    % Create and send process
    ProcessMsg = create_ao_process_msg(Authority),
    {ProcessId, ProcessANS104} = sign_and_serialize(ProcessMsg, RedAOWallet),
    
    ?event({process_created, {process_id, ProcessId}}),
    
    case send_to_legacy_ao(ProcessANS104) of
        {ok, StatusCode, Body} when StatusCode >= 200, StatusCode < 300 ->
            ?event({process_deployed, {process_id, ProcessId}, {response, Body}}),
            
            % Send authorities update
            AuthoritiesMsg = create_authorities_msg(ProcessId, ProcessWalletAddress, LegacyAddress),
            {_AuthId, AuthANS104} = sign_and_serialize(AuthoritiesMsg, RedAOWallet),
            
            case send_to_legacy_ao(AuthANS104) of
                {ok, AuthStatusCode, AuthBody} when AuthStatusCode >= 200, AuthStatusCode < 300 ->
                    ?event({authorities_updated, {process_id, ProcessId}, {response, AuthBody}}),
                    {ok, ProcessId, RedAOWallet, ProcessWalletAddress};
                {ok, AuthStatusCode, AuthBody} ->
                    ?event({authorities_update_failed, {status, AuthStatusCode}, {body, AuthBody}}),
                    {error, {authorities_update_failed, AuthStatusCode, AuthBody}};
                {error, AuthReason} ->
                    ?event({authorities_update_error, {reason, AuthReason}}),
                    {error, {authorities_update_error, AuthReason}}
            end;
        {ok, StatusCode, Body} ->
            ?event({process_deploy_failed, {status, StatusCode}, {body, Body}}),
            {error, {process_deploy_failed, StatusCode, Body}};
        {error, Reason} ->
            ?event({process_deploy_error, {reason, Reason}}),
            {error, {process_deploy_error, Reason}}
    end.

%% @doc Transfer AO tokens to a process
transfer_ao_tokens(ProcessId, Quantity, RedAOWallet) ->
    
    ?event({transferring_ao, {process_id, ProcessId}, {quantity, Quantity}}),
    
    % Create and send transfer message
    TransferMsg = create_transfer_msg(ProcessId, Quantity),
    {TransferId, TransferANS104} = sign_and_serialize(TransferMsg, RedAOWallet),
    
    ?event({transfer_created, {transfer_id, TransferId}}),
    
    case send_to_legacy_ao(TransferANS104) of
        {ok, StatusCode, Body} when StatusCode >= 200, StatusCode < 300 ->
            ?event({transfer_completed, {transfer_id, TransferId}, {response, Body}}),
            {ok, TransferId, Body};
        {ok, StatusCode, Body} ->
            ?event({transfer_failed, {status, StatusCode}, {body, Body}}),
            {error, {transfer_failed, StatusCode, Body}};
        {error, Reason} ->
            ?event({transfer_error, {reason, Reason}}),
            {error, {transfer_error, Reason}}
    end.


%% @doc Initialize applications and dependencies
init_applications() ->
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
    ]).

%% @doc Create wallet and node configuration
setup_node_config(Name) ->
    % MainOpts = hb_opts:default_message(),    
    {Wallet, WalletAddress} = create_wallet(),
    Store = hb_test_utils:test_store(hb_store_lmdb, Name),
    hb_store:start(Store),
    hb_store:reset(Store),
    timer:sleep(3000),
    Opts = #{
        port => 10000,
        operator => unclaimed,
        priv_wallet => Wallet,
        store =>
            [
                Store,
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
                    <<"local-store">> => [Store]
                },
                #{
                    <<"store-module">> => hb_store_gateway,
                    <<"local-store">> => [Store]
                }
            ],
            on => #{
                <<"request">> => #{
                    <<"device">> => <<"p4@1.0">>,
                    <<"ledger-device">> => <<"lua@5.3a">>,
                    <<"pricing-device">> => <<"simple-pay@1.0">>,
                    <<"ledger-path">> => <<"/ledger~node-process@1.0">>,
                    <<"module">> => <<"2MVk_oOeXrQBOl27JQIeFKhwRlfGzFxs5UhrC_dSrJo">>
                },
                <<"response">> => #{
                    <<"device">> => <<"p4@1.0">>,
                    <<"ledger-device">> => <<"lua@5.3a">>,
                    <<"pricing-device">> => <<"simple-pay@1.0">>,
                    <<"ledger-path">> => <<"/ledger~node-process@1.0">>,
                    <<"module">> => <<"2MVk_oOeXrQBOl27JQIeFKhwRlfGzFxs5UhrC_dSrJo">>
                }
            },
            router_opts => #{
                <<"offered">> =>
                    [
                        #{ 
                            <<"template">> => <<"^/.*~process@1.0/.*">>,
                            <<"registration-peer">> => <<"http://localhost:10000">>,
                            <<"prefix">> => <<"http://localhost:10000">>,
                            <<"price">> => 1
                        }

                    ]
            }
    },
    Node = hb_http_server:start_node(Opts),
    ?event({hyperbeam_node_started, {node, Node}}),
    Node,
    {Opts, Wallet, WalletAddress, Node, Store}.


%% @doc Deploy AO process
deploy_ao_process_step(RedAOWallet, ProcessWalletAddress, LegacyAddress) ->
    ?event({deploying_ao_process, "Using Erlang utility functions"}),
    case deploy_ao_process(RedAOWallet, ProcessWalletAddress, LegacyAddress) of
        {ok, ProcessId, _ProcessWallet, _ProcessAddress} ->
            ?event({ao_process_deployed, {process_id, ProcessId}}),
            {ok, ProcessId};
        {error, DeployError} ->
            ?event({ao_process_deploy_failed, {error, DeployError}}),
            {error, DeployError}
    end.

%% @doc Load and modify configuration
setup_compute_config(ProcessWalletAddress, RedAOProcessId, RedAOWalletAddress, LegacyAddress) ->
    {ok, Config} = file:read_file(filename:join("deployment/configs", "compute.json")),
    DecodedConfig = hb_json:decode(Config, [return_maps]),
    
    ?event({loaded_configs, DecodedConfig}),
    
    Updates = [
        {[<<"node_processes">>, <<"ledger">>, <<"token">>], RedAOProcessId},
        {[<<"node_processes">>, <<"ledger">>, <<"admin">>], ProcessWalletAddress},
        {[<<"node_processes">>, <<"ledger">>, <<"authority">>], <<ProcessWalletAddress/binary, ",", LegacyAddress/binary>>},
        {[<<"p4_recipient">>], <<"Kv6jQCcs8GwNpioj6tkTt06zD130YgqIHX7QNnZQYQc">>}
    ],
    
    ModConfig = apply_nested_updates(DecodedConfig, Updates),
    ?event({modified_configs, {string, hb_json:encode(ModConfig)}}),
    
    ModConfig.

%% @doc Test the ledger node functionality
test_ledger_node(ProcessNode) ->
    ?event({testing_ledger_node, {node, ProcessNode}}),
    case hb_http:get(ProcessNode, #{ 
        <<"path">> => <<"/ledger~node-process@1.0/now">>, 
        <<"accept-bundle">> => <<"true">> 
    }, #{}) of
        {ok, Res} ->
            ?event({ledger_now, {string, hb_json:encode(Res)}}),
            {ok, Res};
        {error, Reason} ->
            ?event({ledger_now_error, {string, hb_json:encode(Reason)}}),
            {error, Reason};
        Other -> 
            ?event({ledger_now_error, "Unknown reason", {result, Other}}),
            {error, unknown}
    end.
