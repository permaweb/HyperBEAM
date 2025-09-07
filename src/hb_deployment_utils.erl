%%% @doc Utility functions for HyperBEAM node deployment and AO process
%%% management.
%%% This module provides comprehensive tools for deploying AO processes,
%%% managing configuration updates, wallet operations, and interacting
%%% with both legacy AO endpoints and HyperBEAM nodes. It handles the
%%% complete lifecycle of process deployment including wallet creation,
%%% authority setup, token transfers, and message serialization using
%%% the ANS104 format for Arweave network compatibility.
-module(hb_deployment_utils).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

-export([update_nested_value/3, apply_nested_updates/2]).
-export([post_config/2, post_config/3]).
-export([create_node/2]).
-export([create_wallet/0, load_wallet/1]).
-export([build_authorities/2]).
-export([create_ao_process_msg/1, create_authorities_msg/3]).
-export([create_transfer_msg/2]).
-export([create_process_to_ledger_transfer_msg/4]).
-export([sign_and_serialize/2]).
-export([send_to_legacy_ao/1, send_to_legacy_ao/2]).
-export([transfer_ao_tokens/3]).

-export([push_signed_message/3, push_signed_message/4]).
-export([trim_trailing_zero/1]).

-define(AO_MODULE,
        <<"URgYpPQzvxxfYQtjrIQ116bl3YBfcImo3JEnNo8Hlrk">>).
-define(PARENT_AO_TOKEN,
        <<"0syT13r0s0tgPmIed95bJnuSqaD29HQNN8D3ElLSrsc">>).
-define(LEGACY_SCHEDULER,
        <<"_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA">>).
-define(ON_BOOT, <<"LxLUDGJD2QLnyIVO_boJnUbVnyXjSuV0d90VNCt6I0g">>).
-define(MU_URL, <<"https://mu.ao-testnet.xyz">>).


%% @doc Convert a number to a binary string, removing trailing .0 for whole numbers.
%% This function handles both floats and integers, ensuring that whole number floats
%% like 2.0 are converted to "2" instead of "2.0", while preserving decimal values.
%%
%% Examples:
%%   trim_trailing_zero(2.0) -> <<"2">>
%%   trim_trailing_zero(2.5) -> <<"2.5">>  
%%   trim_trailing_zero(2) -> <<"2">>
%%   trim_trailing_zero(<<"2">>) -> <<"2">>
trim_trailing_zero(Float) when is_float(Float) ->
    case trunc(Float) == Float of
        true -> integer_to_binary(trunc(Float));
        false -> hb_util:bin(Float)
    end;
trim_trailing_zero(Integer) when is_integer(Integer) ->
    integer_to_binary(Integer);
trim_trailing_zero(Binary) when is_binary(Binary) ->
    Binary;
trim_trailing_zero(Other) ->
    hb_util:bin(Other).


%% @doc Update a nested value in a map using a path list for
%% configuration changes
%% This function provides a convenient way to update deeply nested map
%% structures commonly found in configuration files and deployment
%% settings without having to manually traverse each level of the nested
%% structure, automatically creating missing intermediate maps when needed
%% for robust configuration management.
update_nested_value(Map, Path, Value) ->
    update_nested_value(Map, Path, Value, create_missing).
%% Base case: empty path means we've reached the target location for
%% value update
update_nested_value(_Map, [], Value, _) ->
    Value;
%% Single key case: directly update the map with the new value at the
%% specified key
update_nested_value(Map, [Key], Value, _) when is_map(Map) ->
    Map#{Key => Value};
%% Recursive case: traverse deeper into nested map structure to reach
%% target path
update_nested_value(Map, [Key | RestPath], Value, CreateMissing)
        when is_map(Map) ->
    % Determine the submap to work with, handling various edge cases
    % gracefully
    SubMap = case maps:find(Key, Map) of
        % Key exists and contains a valid map - use it for further
        % traversal
        {ok, ExistingSubMap} when is_map(ExistingSubMap) ->
            ExistingSubMap;
        % Key exists but contains non-map value - replace with empty map
        % structure
        {ok, _} ->
            #{};  % Replace non-map values with empty map for
                  % consistent structure
        % Key doesn't exist and we're allowed to create missing
        % intermediate maps
        error when CreateMissing =:= create_missing ->
            #{};
        % Key doesn't exist and creation of missing keys is not allowed
        % - error out
        error ->
            throw({path_not_found, Key})
    end,
    % Recursively update the submap with remaining path and merge back
    % into parent
    UpdatedSubMap = update_nested_value(SubMap, RestPath, Value,
                                        CreateMissing),
    Map#{Key => UpdatedSubMap}.

%% @doc Apply multiple nested updates at once for batch configuration
%% modifications
%% This function efficiently processes a list of path-value pairs to
%% update multiple nested values in a single operation, reducing the need
%% for multiple individual calls and ensuring atomic-like behavior for
%% related configuration changes that should be applied together as a
%% cohesive unit for deployment consistency and reliability.
apply_nested_updates(Config, Updates) ->
    lists:foldl(
        fun({Path, Value}, AccConfig) ->
            update_nested_value(AccConfig, Path, Value)
        end,
        Config,
        Updates
    ).

%% @doc Post configuration to meta endpoint equivalent to TypeScript postConfig
%% This function provides a simplified interface for posting
%% configuration data to HyperBEAM node meta endpoints using the default
%% JSON codec device for standard configuration updates, automatically
%% handling the common case where no specific codec device needs to be
%% specified for typical deployment configuration scenarios.
post_config(NodeUrl, ConfigContent) ->
    post_config(NodeUrl, ConfigContent, <<"json@1.0">>).
post_config(NodeUrl, ConfigContent, Device) ->
    Path = <<"~meta@1.0/info">>,
    Body = hb_json:encode(ConfigContent),    
    case hb_http:post(
        NodeUrl,
        #{ 
            <<"path">> => Path, 
            <<"codec-device">> => Device,
            <<"body">> => Body
        },
        #{}
    ) of
        {ok, Res} ->
            ?event({res, {explicit, Res}}),
            {ok, Res};
        {error, Reason} ->
            ?event({res_error, {reason, Reason}}),
            {error, Reason}
    end.

%% @doc Creates and configures a HyperBEAM test node with wallet and storage.
%%
%% This function performs the following setup operations:
%% 1. Generates a new wallet and extracts its address
%% 2. Initializes an LMDB test store with the specified name
%% 3. Configures multiple storage layers including filesystem cache and gateway
%% 4. Sets up router options with service offerings and pricing
%% 5. Starts the HTTP server node with all configurations
%%
%% The node configuration includes:
%% - Port 10000 for HTTP communications
%% - Multi-layer storage system (LMDB, filesystem, gateway)
%% - Router with service template matching and pricing
%% - Integration with Arweave data protocol filtering
%%
%% @param Name Binary name identifier for the test store
%% @returns {Opts, Wallet, WalletAddress, Node} tuple containing:
%%   - Opts: Complete node configuration map
%%   - Wallet: Generated private wallet for the node
%%   - WalletAddress: Public address of the generated wallet
%%   - Node: Started HTTP server node process
create_node(Name, Port) ->
    {Wallet, WalletAddress} = hb_deployment_utils:create_wallet(),
    Store = hb_test_utils:test_store(hb_store_lmdb, Name),
    Opts = #{
        port => Port,
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
            ]
    },
    Node = hb_http_server:start_node(Opts),
    ?event({hyperbeam_node_started, {node, Node}}),
    Node,
    {Opts, Wallet, WalletAddress, Node}.

%% @doc Create a new wallet and return wallet + address for AO process
%% deployment
%% This function generates a fresh Arweave wallet keypair and computes
%% the associated address, returning both components needed for process
%% deployment and transaction signing operations within the AO ecosystem
%% for secure process management.
create_wallet() ->
    Wallet = ar_wallet:new(),
    Address = hb_util:encode(ar_wallet:to_address(Wallet)),
    {Wallet, Address}.

%% @doc Load wallet from file and return wallet + address for existing
%% key usage
%% This function loads an existing Arweave wallet from a keyfile and
%% computes the associated address, enabling the use of pre-existing
%% wallets for AO process deployment while maintaining compatibility with
%% standard Arweave wallet formats.
load_wallet(FilePath) ->
    Wallet = ar_wallet:load_keyfile(FilePath),
    Address = hb_util:encode(ar_wallet:to_address(Wallet)),
    {Wallet, Address}.

%% @doc Build authority string from wallet address and fixed authorities
%% for access
%% This function constructs a comma-separated authority list combining
%% the process wallet address with legacy addresses, creating the
%% authority specification needed for AO process deployment and permission
%% management within the distributed system.
build_authorities(WalletAddress, LegacyAddress) ->
    WalletStr = binary_to_list(WalletAddress),
    LegacyStr = binary_to_list(LegacyAddress),
    AllAuthorities = [WalletStr, LegacyStr],
    list_to_binary(string:join(AllAuthorities, ",")).

%% @doc Create AO process message with all required fields for process
%% deployment
%% This function constructs a complete AO process creation message
%% including all necessary metadata, module references, scheduler
%% configuration, and token details required for successful process
%% deployment within the AO network infrastructure.
create_ao_process_msg(Authority) ->
    #{
        <<"Type">> => <<"Process">>,
        <<"Data-Protocol">> => <<"ao">>,
        <<"Variant">> => <<"ao.TN.1">>,
        <<"Name">> => <<"[BETA-1.2] Green Zone AO">>,
        <<"data">> => <<"1984">>,
        <<"On-Boot">> => ?ON_BOOT,
        <<"Authority">> => Authority,
        <<"ParentToken">> => ?PARENT_AO_TOKEN,
        <<"Ticker">> => <<"BETA-GZ">>,
        <<"Denomination">> => <<"12">>,
        <<"Module">> => ?AO_MODULE,
        <<"Scheduler">> => ?LEGACY_SCHEDULER,
        <<"device">> => <<"process@1.0">>,
        <<"execution-device">> => <<"genesis-wasm@1.0">>,
        <<"scheduler-device">> => <<"scheduler@1.0">>,
        <<"push-device">> => <<"push@1.0">>
    }.

%% @doc Create authorities update message for process permission
%% configuration
%% This function generates an AO message that updates the authorities
%% list for an existing process, allowing multiple addresses to have
%% control over the process execution and management within the
%% decentralized AO computing environment.
create_authorities_msg(ProcessId, WalletAddress, LegacyAddress) ->
    WalletAddressStr = binary_to_list(WalletAddress),
    LegacyAddressStr = binary_to_list(LegacyAddress),
    AuthoritiesData = 
        list_to_binary(
            "ao.authorities = {'" ++
            WalletAddressStr ++ 
            "', '" ++ 
            LegacyAddressStr ++ 
            "'}"
        ),
    #{
        <<"type">> => <<"Message">>,
        <<"data-protocol">> => <<"ao">>,
        <<"variant">> => <<"ao.TN.1">>,
        <<"target">> => ProcessId,
        <<"recipient">> => ProcessId,
        <<"data">> => AuthoritiesData,
        <<"action">> => <<"Eval">>
    }.

%% @doc Create transfer message for moving tokens to deployed AO process
%% instances
%% This function constructs a token transfer message that moves a
%% specified quantity of tokens from the parent token contract to a newly
%% deployed process, enabling the process to have initial token balance
%% for operations within AO ecosystem.
create_transfer_msg(ProcessId, Quantity) ->
    #{
        <<"Type">> => <<"Message">>,
        <<"Data-Protocol">> => <<"ao">>,
        <<"Variant">> => <<"ao.TN.1">>,
        <<"target">> => ?PARENT_AO_TOKEN, 
        <<"Action">> => <<"Transfer">>,
        <<"Quantity">> => Quantity,
        <<"Recipient">> => ProcessId 
    }.

%% @doc Creates a message to transfer tokens from an AO process to a ledger node.
%%
%% This function constructs an AO protocol message that instructs a process
%% to transfer a specified quantity of tokens to a ledger node. The message
%% uses the AO.N.1 variant and includes routing information to direct the
%% transfer through the appropriate ledger process.
%%
%% Message structure follows AO protocol specifications:
%% - Uses ans104 bundle format for commitment and codec
%% - Specifies Transfer action for token movement
%% - Routes through the ledger process for balance tracking
%% - Maintains compatibility with httpsig@1.0 authentication
%%
%% @param RedAOProcessId Binary ID of the source AO process containing tokens
%% @param RedAOWalletAddress Binary address of the wallet receiving tokens
%% @param LedgerProcessId Binary ID of the ledger node process for routing
%% @param Quantity Binary string representing the amount to transfer
%% @returns Map containing the complete AO transfer message structure
create_process_to_ledger_transfer_msg(
    RedAOProcessId, 
    RedAOWalletAddress, 
    LedgerProcessId, 
    Quantity
) ->
    #{
        <<"type">> => <<"Message">>,
        <<"data-protocol">> => <<"ao">>,
        <<"variant">> => <<"ao.N.1">>,
        <<"accept-bundle">> => <<"true">>,
        <<"accept-codec">> => <<"httpsig@1.0">>,
        <<"action">> => <<"Transfer">>,
        <<"target">> => RedAOProcessId,
        <<"recipient">> => RedAOWalletAddress,
        <<"route">> => LedgerProcessId,
        <<"quantity">> => Quantity
    }.

%% @doc Pushes a signed message to a HyperBEAM node's general push endpoint.
%%
%% This is a convenience wrapper that sends a message to the node's default
%% push endpoint without targeting a specific process. The message will be
%% signed with the provided wallet and committed using ans104 format.
%%
%% @param Msg Map containing the message to be signed and sent
%% @param Wallet Private wallet for signing the message
%% @param Node HyperBEAM node process to send the message to
%% @returns {ok, Response, MessageId} on success or {error, Reason} on failure
push_signed_message(Msg, Wallet, Node) ->
    Path = <<"push">>,
    do_push_signed_message(Msg, Wallet, Node, Path).

%% @doc Pushes a signed message to a specific AO process on a HyperBEAM node.
%%
%% This function sends a message to a specific process's push endpoint,
%% constructed using the process ID. The message will be signed with the
%% provided wallet and committed using ans104 format for AO compatibility.
%%
%% Endpoint format: "{ProcessId}~process@1.0/push"
%%
%% @param Msg Map containing the message to be signed and sent
%% @param Wallet Private wallet for signing the message
%% @param Node HyperBEAM node process to send the message to
%% @param ProcessId Binary ID of the target AO process
%% @returns {ok, Response, MessageId} on success or {error, Reason} on failure
push_signed_message(Msg, Wallet, Node, ProcessId) ->
    Path = <<ProcessId/binary, "~process@1.0/push">>,
    do_push_signed_message(Msg, Wallet, Node, Path).

%% @doc Internal function that handles the actual message signing and HTTP posting.
%%
%% This function performs the core operations for message delivery:
%% 1. Creates wallet options for message signing
%% 2. Commits the message using ans104 format with bundle acceptance
%% 3. Generates a message ID from the signed message
%% 4. Posts the signed message to the specified endpoint
%% 5. Returns the response and message ID or error details
%%
%% The function uses ans104@1.0 codec for Arweave network compatibility
%% and accepts JSON responses from the node.
%%
%% @param Msg Map containing the message to be signed and sent
%% @param Wallet Private wallet for signing the message
%% @param Node HyperBEAM node process to send the message to
%% @param Path Binary string representing the target endpoint path
%% @returns {ok, Response, MessageId} on success or {error, Reason} on failure
do_push_signed_message(Msg, Wallet, Node, Path) ->
    Opts = #{ priv_wallet => Wallet },
    SignedMsg = hb_message:commit(
        Msg,
        Opts,
        #{ 
            <<"commitment-device">> => <<"ans104@1.0">>,
            <<"accept-bundle">> => <<"true">>,
            <<"accept">> => <<"application/json">>
        }
    ),
    Id = hb_message:id(SignedMsg, signed, Opts),
    case hb_http:post(
        Node,
        SignedMsg#{ 
            <<"path">> => Path, 
            <<"codec-device">> => <<"ans104@1.0">>
        },
        Opts
    ) of
        {ok, Res} ->
            ?event({res, {explicit, Res}}),
            {ok, Res, Id};
        {error, Reason} ->
            ?event({res_error, {reason, Reason}}),
            {error, Reason}
    end.

%% @doc Sign and serialize message to ANS104 format for Arweave network
%% transmission
%% This function converts AO messages into the ANS104 bundle format,
%% signs them with the provided wallet, and serializes them into bytes
%% ready for network transmission to AO endpoints, ensuring cryptographic
%% integrity and network compatibility.
sign_and_serialize(Msg, Wallet) ->
    % Convert message map to ANS104 transaction format using development
    % codec
    {ok, TX} = dev_codec_ans104:to(Msg, #{}, #{}),
    % Sign the transaction with the provided wallet for cryptographic
    % authentication
    SignedTX = ar_bundles:sign_item(TX, Wallet),
    % Serialize signed transaction to binary format for network
    % transmission
    ANS104Bytes = ar_bundles:serialize(SignedTX),
    % Extract and encode the transaction ID for use as process identifier
    % reference
    ProcessId = hb_util:encode(SignedTX#tx.id),
    % Return tuple containing process ID and serialized ANS104 bytes for
    % transmission
    {ProcessId, ANS104Bytes}.

%% @doc Send ANS104 data to legacy AO endpoint using default MU URL
%% configuration
%% This convenience function provides a simplified interface for sending
%% ANS104 data to the default legacy AO endpoint without requiring
%% explicit URL specification, streamlining the common case of
%% interacting with the standard AO testnet infrastructure.
send_to_legacy_ao(ANS104Bytes) ->
    send_to_legacy_ao(ANS104Bytes, ?MU_URL).
send_to_legacy_ao(ANS104Bytes, Url) ->
    Headers = [
        {"Content-Type", "application/octet-stream"},
        {"Accept", "application/json"}
    ],
    HTTPOptions = [{autoredirect, false}],
    Options = [],
    case httpc:request(
        post, 
        {Url, Headers, "application/octet-stream", ANS104Bytes}, 
        HTTPOptions, 
        Options
    ) of
        {ok, 
            {
                {_Version, StatusCode, _ReasonPhrase}, 
                ResponseHeaders,
                Body
            }
        } ->
            ?event({
                legacy_ao_response, 
                {status, StatusCode},
                {headers, ResponseHeaders}, 
                {body, Body}
            }),
            {ok, StatusCode, Body};
        {error, Reason} ->
            ?event({legacy_ao_error, {reason, Reason}}),
            {error, Reason}
    end.

%% @doc Transfer AO tokens to a process for initial balance and
%% operational funding
%% This function handles the complete token transfer workflow including
%% message creation, signing, serialization, and transmission to legacy
%% AO endpoints with comprehensive error handling and logging for
%% reliable token distribution to deployed processes.
transfer_ao_tokens(ProcessId, Quantity, RedAOWallet) ->
    TransferMsg = create_transfer_msg(ProcessId, Quantity),
    {TransferId, TransferANS104} = 
        sign_and_serialize(TransferMsg, RedAOWallet),
    case send_to_legacy_ao(TransferANS104) of
        {ok, StatusCode, Body} when StatusCode >= 200, StatusCode < 300 ->
            ?event({
                transfer_completed, 
                {transfer_id, TransferId},
                {response, Body}
            }),
            {ok, TransferId, Body};
        {ok, StatusCode, Body} ->
            ?event({transfer_failed, {status, StatusCode}, {body, Body}}),
            {error, {transfer_failed, StatusCode, Body}};
        {error, Reason} ->
            ?event({transfer_error, {reason, Reason}}),
            {error, {transfer_error, Reason}}
    end.


