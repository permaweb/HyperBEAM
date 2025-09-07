%%% @doc Tests the deployment of the HyperBEAM nodes.
-module(hb_deployment_test_vectors).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

-define(LEGACY_ADDRESS, <<"fcoN_xJeisVsPXA-trzVAuIiqO3ydLQxM-L4XbrQKzY">>).
-define(P4_RECIPIENT, <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>).
-define(LEGACY_PROCESS_ID, <<"Kv6jQCcs8GwNpioj6tkTt06zD130YgqIHX7QNnZQYQc">>).
-define(P4_CLIENT_MODULE, <<"2MVk_oOeXrQBOl27JQIeFKhwRlfGzFxs5UhrC_dSrJo">>).
-define(LEDGER_MODULE, <<"W-FoIIxJlBdhO5Z3CbQJg7UKxgawCTT0yVGkJGNWzLs">>).
-define(AMOUNT, <<"2">>).
-define(PRICE, <<"1">>).

%% @doc Tests the complete deployment and ledger balance verification workflow.
%%
%% This comprehensive test function performs the following operations:
%% 1. Sets up a HyperBEAM compute node with proper configuration
%% 2. Loads a wallet from the environment or default test location
%% 3. Deploys an AO process with appropriate authorities
%% 4. Configures the compute node with ledger and P4 client modules
%% 5. Transfers AO tokens to the ledger and verifies balances
%% 6. Executes a compute slot request to test P4 functionality
%% 7. Validates final ledger balances after transaction processing
%%
%% The test verifies:
%% - Initial wallet balance matches the transferred amount
%% - Final wallet balance is reduced by the transaction price
%% - P4 recipient receives the correct transaction fee
%%
%% Environment variables:
%% - WALLET_PATH: Optional path to wallet file (defaults to "/test/wallet.json")
%%
%% @returns ok on successful test completion
%% @throws Various assertion failures if balances don't match expected values
ledger_balance_test() ->
    %% TODO: Probably best not to use real wallets with funds... 
    %% TODO: Maybe Think of a way to mock this instead.
    % Setup test node and load wallet
    { _, _, ComputeAddress, ComputeNode } = 
        hb_deployment_utils:create_node(<<"compute">>, 10000),
    RedAOWalletPath = case os:getenv("WALLET_PATH") of
        EnvPath -> EnvPath;
        false -> "/test/wallet.json"
    end,
    {RedAOWallet, RedAOWalletAddress} = 
        hb_deployment_utils:load_wallet(RedAOWalletPath),
    ?event({red_ao, {address, RedAOWalletAddress}}),
    % Deploy AO process with authorities
    Authorities = 
        hb_deployment_utils:build_authorities(
            RedAOWalletAddress, 
            ?LEGACY_ADDRESS
        ),
    {ok, RedAOProcessRes, RedAOProcessId} = 
        hb_deployment_utils:push_signed_message(
            hb_deployment_utils:create_ao_process_msg(Authorities),
            RedAOWallet,
            ComputeNode
        ),
    ?event({
        red_ao_process, 
        {red_ao_process_id, RedAOProcessId}, 
        {red_ao_process_res, {explicit, RedAOProcessRes}}
    }),
    % Setup authorities for the deployed process
    {ok, AuthRes, AuthId} = 
        hb_deployment_utils:push_signed_message(
            hb_deployment_utils:create_authorities_msg(
                RedAOProcessId, 
                RedAOWalletAddress, 
                ?LEGACY_ADDRESS
            ),
            RedAOWallet,
            ComputeNode,
            RedAOProcessId
        ),
    ?event({
        authorities, 
        {authorities_id, AuthId},
        {authorities_res, {explicit, AuthRes}}
    }),
    % Configure and deploy ledger node process
    ?event({
        compute_config, 
        {compute_address, ComputeAddress}, 
        {red_ao_process_id, RedAOProcessId}
    }),
    ModConfig = setup_compute_config(ComputeAddress, RedAOProcessId),
    LedgerMetaPostResponse = 
        hb_deployment_utils:post_config(ComputeNode, ModConfig),
    ?event({
        ledger_meta_post_response, 
        {explicit, LedgerMetaPostResponse}
    }),
    % Get ledger process ID from commitments
    {Status, LedgerProcessId} = 
        hb_http:get(
            ComputeNode, 
            #{ <<"path">> => <<"ledger~node-process@1.0/commitments/keys/1">> },
            #{}
        ),
    ?event({
        ledger_commitments_keys_1,
        {status, Status}, 
        {ledger_process_id, {explicit, LedgerProcessId}}
    }),
    % Transfer tokens to AO process
    {ok, TransferId, TransferBody} = 
        hb_deployment_utils:transfer_ao_tokens(
            RedAOProcessId, 
            ?AMOUNT,
            RedAOWallet
        ),
    ?event({
        transfer_completed, 
        {transfer_id, TransferId}, 
        {transfer_body, {explicit, TransferBody}}
    }),
    % TODO: Instead of timeout, make requests to check balance from legacy CU.
    timer:sleep(15000),
    % Transfer tokens to ledger
    hb_deployment_utils:push_signed_message(
        hb_deployment_utils:create_process_to_ledger_transfer_msg(
            RedAOProcessId, 
            RedAOWalletAddress, 
            LedgerProcessId, 
            ?AMOUNT
        ),
        RedAOWallet,
        ComputeNode,
        RedAOProcessId
    ),
    % Verify initial balance matches transferred amount
    IntegerAmount = binary_to_integer(?AMOUNT),
    IntegerPrice = binary_to_integer(?PRICE),
    LedgerBalance = get_ledger_balance(ComputeNode),
    RedAOWalletBalance = 
        hb_deployment_utils:trim_trailing_zero(
            hb_maps:get(RedAOWalletAddress, LedgerBalance)
        ),
    ?event({ledger_balance, { explicit, LedgerBalance}}),
    ?event({red_ao_wallet_balance, { explicit, RedAOWalletBalance}}),
    ?assertEqual(
        RedAOWalletBalance,
        hb_util:bin(IntegerAmount)
    ),
    % Execute compute slot request (triggers P4 fee deduction)
    ComputeSlotResult = 
        compute_slot_request(ComputeNode, 1, ?LEGACY_PROCESS_ID, RedAOWallet),
    ?event({
        compute_slot, 
        {slot, ComputeSlotResult}
    }),
    % Verify final balances after P4 transaction
    FinalLedgerBalance = get_ledger_balance(ComputeNode),
    FinalRedAOWalletBalance = 
        hb_deployment_utils:trim_trailing_zero(
            hb_maps:get(RedAOWalletAddress, FinalLedgerBalance)
        ),
    P4RecipientBalance = 
        hb_deployment_utils:trim_trailing_zero(
            hb_maps:get(?P4_RECIPIENT, FinalLedgerBalance)
        ),
    ?event({final_ledger_balance, { explicit, FinalLedgerBalance}}),
    ?event({p4_recipient_balance, { explicit, P4RecipientBalance}}),
    % Assert wallet balance reduced by transaction price
    ?assertEqual(
        FinalRedAOWalletBalance,
        hb_util:bin(IntegerAmount - IntegerPrice)
    ),
    % Assert P4 recipient received the transaction fee
    ?assertEqual(
        P4RecipientBalance,
        hb_util:bin(IntegerPrice)
    ).

%% @doc Loads and customizes compute node configuration for AO process deployment.
%%
%% This function performs the following configuration operations:
%% 1. Reads the base compute configuration from deployment/configs/compute.json
%% 2. Decodes the JSON configuration into an Erlang map structure
%% 3. Applies a series of nested updates to customize the configuration
%% 4. Sets up ledger node process with token, admin, module, and authority
%% 5. Configures P4 client modules for request/response handling
%% 6. Sets the P4 recipient address for transaction fees
%%
%% Configuration updates include:
%% - Ledger token: Associates with the deployed AO process ID
%% - Ledger admin: Sets the process wallet as administrator
%% - Ledger module: Specifies the ledger implementation module
%% - Ledger authority: Combines process wallet and legacy addresses
%% - P4 modules: Sets request and response handling modules
%% - P4 recipient: Defines where transaction fees are sent
%%
%% @param ProcessWalletAddress Binary address of the process wallet
%% @param RedAOProcessId Binary ID of the deployed AO process
%% @returns Modified configuration map ready for node deployment
setup_compute_config(ProcessWalletAddress, RedAOProcessId) ->
    {ok, Config} = file:read_file(filename:join("deployment/configs", "compute.json")),
    DecodedConfig = hb_json:decode(Config, [return_maps]),
    Updates = [
        {
            [<<"node_processes">>, <<"ledger">>, <<"token">>], 
            RedAOProcessId
        },
        {
            [<<"node_processes">>, <<"ledger">>, <<"admin">>], 
            ProcessWalletAddress
        },
        {
            [<<"node_processes">>, <<"ledger">>, <<"module">>], 
            ?LEDGER_MODULE
        },
        {
            [<<"node_processes">>, <<"ledger">>, <<"authority">>], 
            <<ProcessWalletAddress/binary, ",", ?LEGACY_ADDRESS/binary>>
        },
        {
            [<<"on">>, <<"request">>, <<"module">>], 
            ?P4_CLIENT_MODULE
        },
        {   
            [<<"on">>, <<"response">>, <<"module">>], 
            ?P4_CLIENT_MODULE
        },
        {
            [<<"p4_recipient">>], 
            ?P4_RECIPIENT
        },
        {
            [<<"router_opts">>], 
            #{
                <<"offered">> => [
                    #{
                        <<"template">> => <<"^/.*~process@1.0/.*">>,
                        <<"registration-peer">> => <<"http://localhost:10000">>,
                        <<"prefix">> => <<"http://localhost:10000">>,
                        <<"price">> => binary_to_integer(?PRICE)
                    }
                ]
            }
        }
    ],
    hb_deployment_utils:apply_nested_updates(DecodedConfig, Updates).

%% @doc Retrieves the current balance information from the ledger node process.
%%
%% This function performs the following operations:
%% 1. Constructs the ledger balance endpoint path
%% 2. Makes an HTTP GET request to the node's ledger process
%% 3. Extracts the balance information from the response
%% 4. Returns the balance map containing wallet addresses and amounts
%%
%% The ledger balance endpoint provides real-time balance information
%% for all wallets that have interacted with the ledger process.
%%
%% @param Node The HyperBEAM node process to query
%% @returns Map containing wallet addresses as keys and balances as values,
%%          or empty map #{} if no balance information is available
get_ledger_balance(Node) ->
    Path = <<"/ledger~node-process@1.0/now">>,
    {_, GetBalanceResult} = 
        hb_http:get(Node, #{ <<"path">> => Path }, #{}),
    hb_maps:get(<<"balance">>, GetBalanceResult, #{}).

%% @doc Executes a compute slot request against a specific AO process.
%%
%% This function performs the following operations:
%% 1. Constructs the compute slot endpoint path with process ID and slot number
%% 2. Creates a signed message using the provided wallet for authentication
%% 3. Makes an authenticated HTTP GET request to the compute endpoint
%% 4. Returns the computation result from the specified slot
%%
%% The compute slot request triggers execution of the AO process at a specific
%% slot, which can involve state transitions, message processing, and fee
%% collection through the P4 protocol.
%%
%% Path format: "{ProcessId}~process@1.0/compute&slot={Slot}/at-slot"
%%
%% @param Node The HyperBEAM node process to send the request to
%% @param Slot Integer slot number to compute at
%% @param ProcessId Binary ID of the target AO process
%% @param RedAOWallet Private wallet used for signing the request
%% @returns Computation result from the AO process execution
compute_slot_request(Node, Slot, ProcessId, RedAOWallet) ->
    Path = 
        <<
            ProcessId/binary,
            "~process@1.0/compute&slot=", 
            (integer_to_binary(Slot))/binary,
            "/at-slot"
        >>,
    SignedMsg = 
        hb_message:commit(
            #{ <<"path">> => Path }, 
            #{ priv_wallet => RedAOWallet }
        ),
    {_, ComputeSlotResult} = 
        hb_http:get(Node, SignedMsg, #{ priv_wallet => RedAOWallet }),
    ComputeSlotResult.

%% TODO: In the future lets send everything through HyperBEAM.
% SignedTransferTokenMsg = hb_message:commit(
%     #{
%         <<"Type">> => <<"Message">>,
%         <<"Data-Protocol">> => <<"ao">>,
%         <<"Variant">> => <<"ao.TN.1">>,
%         % Parent AO token contract identifier
%         <<"target">> => <<"0syT13r0s0tgPmIed95bJnuSqaD29HQNN8D3ElLSrsc">>, 
%         <<"action">> => <<"Transfer">>,
%         <<"Quantity">> => ?AMOUNT,
%         % Target process receiving the token transfer
%         <<"Recipient">> => RedAOProcessId,
%         <<"device">> => <<"process@1.0">>,
%         <<"execution-device">> => <<"genesis-wasm@1.0">>,
%         <<"scheduler-device">> => <<"scheduler@1.0">>,
%         <<"push-device">> => <<"push@1.0">>
%     },
%     #{ priv_wallet => RedAOWallet },
%     #{ 
%         <<"commitment-device">> => <<"ans104@1.0">>,
%         <<"accept-bundle">> => <<"true">>,
%         <<"accept">> => <<"application/ans104">>
%     }
% ),
% case hb_http:post(
%     ComputeNode,
%     SignedTransferTokenMsg#{ 
%         <<"path">> => PushPath,
%         <<"codec-device">> => <<"ans104@1.0">>
%     },
%     #{ priv_wallet => RedAOWallet }
% ) of
%     {ok, TransferTokenRes} ->
%         ?event({transfer_token_res, {explicit, TransferTokenRes}});
%     {error, TransferTokenReason} ->
%         ?event({transfer_token_res_error, {reason, TransferTokenReason}}),
%         {error, TransferTokenReason}
% end,

% timer:sleep(15000),