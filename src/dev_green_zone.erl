%%% @doc The green zone device, which provides secure communication and identity
%%% management between trusted nodes.
%%%
%%% It handles node initialization, joining existing green zones, key exchange,
%%% and node identity cloning. All operations are protected by hardware 
%%% commitment and encryption.
-module(dev_green_zone).

%% Device API exports
-export([info/1, info/3, join/3, init/3, become/3, key/3, is_trusted/3]).
%% Encryption helper functions
-export([encrypt_data/2, decrypt_data/3]).

-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

%%% ===================================================================
%%% Type Specifications
%%% ===================================================================

%% Device API function specs
-spec info(term()) -> #{exports := [atom()]}.
-spec info(term(), term(), map()) -> {ok, map()}.
-spec init(term(), term(), map()) -> {ok, binary()} | {error, binary()}.
-spec join(term(), term(), map()) -> {ok, map()} | {error, map() | binary()}.
-spec key(term(), term(), map()) -> {ok, map()} | {error, binary()}.
-spec become(term(), term(), map()) -> {ok, map()} | {error, binary()}.

%% Helpers for init/3
-spec setup_green_zone_config(map()) -> {ok, map()}.
-spec ensure_wallet(map()) -> term().
-spec ensure_aes_key(map()) -> binary().

%% Helpers for join/3
-spec extract_peer_info(map()) -> 
    {binary() | undefined, binary() | undefined, boolean()}.
-spec should_join_peer(
    binary() | undefined, binary() | undefined, boolean()
) -> boolean().

%% Helpers for join_peer/5
-spec join_peer(binary(), binary(), term(), term(), map()) -> 
    {ok, map()} | {error, map() | binary()}.
-spec prepare_join_request(map()) -> {ok, map()} | {error, term()}.
-spec verify_peer_response(map(), binary(), map()) -> boolean().
-spec extract_and_decrypt_zone_key(map(), map()) -> 
    {ok, binary()} | {error, term()}.
-spec finalize_join_success(binary(), map()) -> {ok, map()}.

%% Helpers for validate_join/3
-spec validate_join(term(), map(), map()) -> {ok, map()} | {error, binary()}.
-spec extract_join_request_data(map(), map()) -> 
    {ok, {binary(), term()}} | {error, term()}.
-spec process_successful_join(binary(), term(), map(), map()) -> {ok, map()}.
-spec validate_peer_opts(map(), map()) -> boolean().
-spec add_trusted_node(binary(), map(), term(), map()) -> ok.

%% Helpers for key/3
-spec get_appropriate_wallet(map()) -> term().
-spec build_key_response(binary(), binary()) -> {ok, map()}.

%% Helpers for become/3
-spec validate_become_params(map()) -> 
    {ok, {binary(), binary()}} | {error, atom()}.
-spec request_and_verify_peer_key(binary(), binary(), map()) -> 
    {ok, map()} | {error, atom()}.
-spec finalize_become(map(), binary(), binary(), map()) -> {ok, map()}.
-spec update_node_identity(term(), map()) -> ok.

%% General/Shared helpers
-spec default_zone_required_opts(map()) -> map().
-spec replace_self_values(map(), map()) -> map().
-spec is_trusted(term(), map(), map()) -> {ok, binary()}.
-spec encrypt_payload(binary(), term()) -> binary().
-spec decrypt_zone_key(binary(), map()) -> {ok, binary()} | {error, binary()}.
-spec try_mount_encrypted_volume(term(), map()) -> ok.

%% Encryption helper specs
-spec encrypt_data(binary(), map()) -> 
    {ok, {binary(), binary()}} | {error, term()}.
-spec decrypt_data(binary(), binary(), map()) -> 
    {ok, binary()} | {error, term()}.

%% @doc Controls which functions are exposed via the device API.
%%
%% This function defines the security boundary for the green zone device by
%% explicitly listing which functions are available through the API.
%%
%% @param _ Ignored parameter
%% @returns A map with the `exports' key containing a list of allowed functions
info(_) -> 
    #{ 
        exports => [
            <<"info">>, 
            <<"init">>, 
            <<"join">>, 
            <<"become">>, 
            <<"key">>
        ] 
    }.

%% @doc Provides information about the green zone device and its API.
%%
%% This function returns detailed documentation about the device, including:
%% 1. A high-level description of the device's purpose
%% 2. Version information
%% 3. Available API endpoints with their parameters and descriptions
%%
%% @param _Msg1 Ignored parameter
%% @param _Msg2 Ignored parameter
%% @param _Opts A map of configuration options
%% @returns {ok, Map} containing the device information and documentation
info(_Msg1, _Msg2, _Opts) ->
    InfoBody = #{
        <<"description">> => 
            <<
                "Green Zone secure communication",
                "and identity management for trusted nodes"
            >>,
        <<"version">> => <<"1.0">>,
        <<"api">> => #{
            <<"info">> => #{
                <<"description">> => <<"Get device info">>
            },
            <<"init">> => #{
                <<"description">> => <<"Initialize the green zone">>,
                <<"details">> => 
                    <<
                        "Sets up the node's cryptographic",
                        "identity with wallet and AES key"
                    >>
            },
            <<"join">> => #{
                <<"description">> => <<"Join an existing green zone">>,
                <<"required-node-opts">> => #{
                    <<"green-zone-peer-location">> => 
                        <<"Target peer's address">>,
                    <<"green-zone-peer-id">> => 
                        <<"Target peer's unique identifier">>
                }
            },
            <<"key">> => #{
                <<"description">> => 
                    <<"Retrieve and encrypt the node's private key">>,
                <<"details">> => 
                    <<
                        "Returns the node's private key encrypted",
                        "with the shared AES key"
                    >>
            },
            <<"become">> => #{
                <<"description">> => <<"Clone the identity of a target node">>,
                <<"required-node-opts">> => #{
                    <<"green-zone-peer-location">> => 
                        <<"Target peer's address">>,
                    <<"green-zone-peer-id">> => 
                        <<"Target peer's unique identifier">>
                }
            }
        }
    },
    {ok, #{<<"status">> => 200, <<"body">> => InfoBody}}.


%% @doc Initialize the green zone for a node.
%%
%% This function performs the following operations:
%% 1. Checks if the green zone is already initialized
%% 2. Sets up and processes the required configuration for the green zone
%% 3. Ensures a wallet (keypair) exists or creates a new one
%% 4. Generates a new 256-bit AES key for secure communication
%% 5. Updates the node's configuration with these cryptographic identities
%% 6. Attempts to mount an encrypted volume using the AES key
%%
%% Config options in Opts map:
%% - green_zone_required_config: (Optional) Custom configuration requirements
%% - priv_wallet: (Optional) Existing wallet to use instead of creating 
%%   a new one
%% - priv_green_zone_aes: (Optional) Existing AES key, if already part of a zone
%%
%% @param _M1 Ignored parameter
%% @param _M2 May contain a `required-config' map for custom requirements
%% @param Opts A map of configuration options
%% @returns `{ok, Binary}' on success with confirmation message, or
%% `{error, Binary}' on failure with error message.
init(_M1, _M2, Opts) ->
    ?event(green_zone, {init, start}),
    maybe
        % Check if already initialized
        false ?= hb_opts:get(green_zone_initialized, false, Opts),
        % Setup configuration
        {ok, ProcessedRequiredConfig} ?= setup_green_zone_config(Opts),
        % Ensure wallet and AES key exist
        NodeWallet = ensure_wallet(Opts),
        GreenZoneAES = ensure_aes_key(Opts),
        % Store configuration and finalize setup
        NewOpts = Opts#{
            priv_wallet => NodeWallet,
            priv_green_zone_aes => GreenZoneAES,
            trusted_nodes => #{},
            green_zone_required_opts => ProcessedRequiredConfig,
            green_zone_initialized => true
        },
        hb_http_server:set_opts(NewOpts),
        try_mount_encrypted_volume(GreenZoneAES, NewOpts),
        ?event(green_zone, {init, complete}),
        {ok, <<"Green zone initialized successfully.">>}
    else
        true ->
            {error, <<"Green zone already initialized.">>};
        Error ->
            ?event(green_zone, {init, error, Error}),
            {error, <<"Failed to initialize green zone">>}
    end.
    

%% @doc Initiates the join process for a node to enter an existing green zone.
%%
%% This function determines the appropriate join strategy and routes to the
%% correct handler:
%% 1. Extracts peer information from configuration options
%% 2. Determines whether to join a specific peer or validate a local request
%% 3. Routes to join_peer/5 if peer details are provided and node has 
%%    no identity
%% 4. Routes to validate_join/3 for local join request processing
%%
%% Config options in Opts map:
%% - green_zone_peer_location: Target peer's address
%% - green_zone_peer_id: Target peer's unique identifier
%% - green_zone_adopt_config: 
%%     (Optional) Whether to adopt peer's configuration (default: true)
%%
%% @param M1 The join request message with target peer information
%% @param M2 Additional request details, may include adoption preferences
%% @param Opts A map of configuration options for join operations
%% @returns `{ok, Map}' on success with join response details, or
%% `{error, Binary}' on failure with error message.
join(M1, M2, Opts) ->
    ?event(green_zone, {join, start}),
    maybe
        % Extract peer information and determine join strategy
        {PeerLocation, PeerID, HasGreenZoneIdentity} = extract_peer_info(Opts),
        ?event(green_zone, 
            {join_peer, PeerLocation, PeerID, HasGreenZoneIdentity}
        ),
        % Route to appropriate join handler based on configuration
        case should_join_peer(PeerLocation, PeerID, HasGreenZoneIdentity) of
            true ->
                join_peer(PeerLocation, PeerID, M1, M2, Opts);
            false ->
                validate_join(M1, M2, hb_cache:ensure_all_loaded(Opts, Opts))
        end
    end.

%% @doc Encrypts and provides the node's private key for secure sharing.
%%
%% This function performs the following operations:
%% 1. Determines the appropriate wallet to use (green-zone identity or default)
%% 2. Extracts the private key components from the wallet
%% 3. Encrypts the private key using the green zone AES key via helper function
%% 4. Builds and returns a standardized response with encrypted key and IV
%%
%% Required configuration in Opts map:
%% - priv_green_zone_aes: The shared AES key for the green zone
%% - priv_wallet: The node's wallet containing the private key to encrypt
%%
%% @param _M1 Ignored parameter
%% @param _M2 Ignored parameter
%% @param Opts A map of configuration options
%% @returns `{ok, Map}' containing the encrypted key and IV on success, or
%% `{error, Binary}' if the node is not part of a green zone
key(_M1, _M2, Opts) ->
    ?event(green_zone, {get_key, start}),
    maybe
        % Get appropriate wallet (green-zone identity or default)
        Wallet = get_appropriate_wallet(Opts),
        {{KeyType, Priv, Pub}, _PubKey} = Wallet,
        ?event(green_zone, 
            {get_key, wallet, hb_util:human_id(ar_wallet:to_address(Pub))}),
        % Encrypt the node's private key (encode term so encrypt is binary-only)
        {ok, {EncryptedData, IV}} ?= encrypt_data(term_to_binary({KeyType, Priv, Pub}), Opts),
        ?event(green_zone, {get_key, encrypt, complete}),
        build_key_response(EncryptedData, IV)
    else
        {error, no_green_zone_aes_key} ->
            ?event(green_zone, {get_key, error, <<"no aes key">>}),
            {error, <<"Node not part of a green zone.">>};
        {error, EncryptError} ->
            ?event(green_zone, {get_key, encrypt_error, EncryptError}),
            {error, <<"Encryption failed">>};
        Error ->
            ?event(green_zone, {get_key, unexpected_error, Error}),
            {error, <<"Failed to retrieve key">>}
    end.

%% @doc Clones the identity of a target node in the green zone.
%%
%% This function performs the following operations:
%% 1. Validates required parameters and green zone membership
%% 2. Requests and verifies the target node's encrypted key
%% 3. Finalizes the identity adoption process through helper functions
%%
%% Required configuration in Opts map:
%% - green_zone_peer_location: Target node's address
%% - green_zone_peer_id: Target node's unique identifier
%% - priv_green_zone_aes: The shared AES key for the green zone
%%
%% @param _M1 Ignored parameter
%% @param _M2 Ignored parameter
%% @param Opts A map of configuration options
%% @returns `{ok, Map}' on success with confirmation details, or
%% `{error, Binary}' if the node is not part of a green zone or
%% identity adoption fails.
become(_M1, _M2, Opts) ->
    ?event(green_zone, {become, start}),
    maybe
        % Validate required parameters and green zone membership
        {ok, {NodeLocation, NodeID}} ?= validate_become_params(Opts),
        % Request and verify peer's encrypted key
        {ok, KeyResp} ?= 
            request_and_verify_peer_key(NodeLocation, NodeID, Opts),
        % Finalize identity adoption
        finalize_become(KeyResp, NodeLocation, NodeID, Opts)
    else
        {error, no_green_zone_aes_key} ->
            ?event(green_zone, {become, error, <<"no aes key">>}),
            {error, <<"Node not part of a green zone.">>};
        {error, missing_peer_location} ->
            {error, <<"green-zone-peer-location required">>};
        {error, missing_peer_id} ->
            {error, <<"green-zone-peer-id required">>};
        {error, invalid_peer_response} ->
            {error, <<"Received incorrect response from peer!">>};
        Error ->
            ?event(green_zone, {become, unexpected_error, Error}),
            {error, <<"Failed to adopt target node identity">>}
    end.


%%% ===================================================================
%%% Internal Helper Functions
%%% ===================================================================

%%% -------------------------------------------------------------------
%%% Helpers for init/3
%%% -------------------------------------------------------------------

%% @doc Setup and process green zone configuration.
%%
%% This function retrieves the required configuration, processes any
%% "self" placeholder values, and returns the processed configuration.
%%
%% @param Opts Configuration options
%% @returns {ok, ProcessedConfig} with processed configuration
setup_green_zone_config(Opts) ->
    RequiredConfig = hb_opts:get(
        <<"green-zone-required-config">>,
        default_zone_required_opts(Opts),
        Opts
    ),
    ProcessedRequiredConfig = replace_self_values(RequiredConfig, Opts),
    ?event(green_zone, {init, required_config, ProcessedRequiredConfig}),
    {ok, ProcessedRequiredConfig}.

%% @doc Ensure a wallet exists, creating one if necessary.
%%
%% This function checks if a wallet already exists in the configuration
%% and creates a new one if needed.
%%
%% @param Opts Configuration options
%% @returns Wallet (existing or newly created)
ensure_wallet(Opts) ->
    case hb_opts:get(priv_wallet, undefined, Opts) of
        undefined -> 
            ?event(green_zone, {init, wallet, missing}),
            hb:wallet();
        ExistingWallet ->
            ?event(green_zone, {init, wallet, found}),
            ExistingWallet
    end.

%% @doc Ensure an AES key exists, generating one if necessary.
%%
%% This function checks if a green zone AES key already exists and
%% generates a new 256-bit key if needed.
%%
%% @param Opts Configuration options
%% @returns AES key (existing or newly generated)
ensure_aes_key(Opts) ->
    case hb_opts:get(priv_green_zone_aes, undefined, Opts) of
        undefined ->
            ?event(green_zone, {init, aes_key, generated}),
            crypto:strong_rand_bytes(32);
        ExistingAES ->
            ?event(green_zone, {init, aes_key, found}),
            ExistingAES
    end.

%%% -------------------------------------------------------------------
%%% Helpers for join/3
%%% -------------------------------------------------------------------

%% @doc Extract peer information from configuration options.
%%
%% This function extracts the peer location, peer ID, and checks if the
%% node already has a green zone identity.
%%
%% @param Opts Configuration options
%% @returns {PeerLocation, PeerID, HasGreenZoneIdentity} tuple
extract_peer_info(Opts) ->
    PeerLocation = hb_opts:get(green_zone_peer_location, undefined, Opts),
    PeerID = hb_opts:get(green_zone_peer_id, undefined, Opts),
    Identities = hb_opts:get(identities, #{}, Opts),
    HasGreenZoneIdentity = maps:is_key(<<"green-zone">>, Identities),
    {PeerLocation, PeerID, HasGreenZoneIdentity}.

%% @doc Determine whether to join a specific peer or validate locally.
%%
%% This function implements the decision logic for join strategy:
%% - Join peer if: no existing identity AND peer location AND peer ID provided
%% - Validate locally otherwise
%%
%% @param PeerLocation Target peer location (may be undefined)
%% @param PeerID Target peer ID (may be undefined)  
%% @param HasGreenZoneIdentity Whether node already has green zone identity
%% @returns true if should join peer, false if should validate locally
should_join_peer(PeerLocation, PeerID, HasGreenZoneIdentity) ->
    (not HasGreenZoneIdentity) andalso 
    (PeerLocation =/= undefined) andalso 
    (PeerID =/= undefined).

%%% -------------------------------------------------------------------
%%% Helpers for join_peer/5
%%% -------------------------------------------------------------------

%% @doc Processes a join request to a specific peer node.
%%
%% This function handles the client-side join flow when connecting to a peer:
%% 1. Verifies the node is not already in a green zone
%% 2. Prepares a join request with commitment report and public key
%% 3. Sends the join request to the target peer
%% 4. Verifies the response is from the expected peer
%% 5. Extracts and decrypts the zone key from the response
%% 6. Finalizes the join by updating configuration with the shared key
%%
%% @param PeerLocation The target peer's address
%% @param PeerID The target peer's unique identifier
%% @param _M1 Ignored parameter
%% @param M2 May contain ShouldMount flag to enable encrypted volume mounting
%% @param InitOpts A map of initial configuration options
%% @returns `{ok, Map}' on success with confirmation message, or
%% `{error, Map|Binary}' on failure with error details
join_peer(PeerLocation, PeerID, _M1, _M2, InitOpts) ->
    maybe
        % Verify node is not already in a green zone
        undefined ?= hb_opts:get(priv_green_zone_aes, undefined, InitOpts),
        % Prepare join request
        {ok, Req} ?= prepare_join_request(InitOpts),
        % Send join request to peer
        ?event(green_zone, 
            {join, sending_commitment, PeerLocation, PeerID, Req}
        ),
        {ok, Resp} ?= 
            hb_http:post(
                PeerLocation, 
                <<"/~greenzone@1.0/join">>,
                Req, 
                InitOpts
            ),
        % Verify response from expected peer
        true ?= verify_peer_response(Resp, PeerID, InitOpts),
        % Extract and decrypt zone key
        {ok, AESKey} ?= extract_and_decrypt_zone_key(Resp, InitOpts),
        % Update configuration with shared key
        finalize_join_success(AESKey, InitOpts)
    else
        {error, already_joined} ->
            ?event(green_zone, {join, already_joined}),
            {error, <<"Node already part of green zone.">>};
        {error, Reason} ->
            {error, #{<<"status">> => 400, <<"reason">> => Reason}};
        {unavailable, Reason} ->
            ?event(green_zone, {
                join_error, peer_unavailable, PeerLocation, PeerID, Reason
            }),
            {error, #{
                <<"status">> => 503,
                <<"body">> => <<"Peer node is unreachable.">>
            }};
        false ->
            {error, <<"Received incorrect response from peer!">>};
        Error ->
            ?event(green_zone, {join, error, Error}),
            {error, Error}
    end.

%% @doc Prepare a join request with commitment report and public key.
%%
%% This function creates a hardware-backed commitment report and prepares
%% the join request message with the node's public key.
%%
%% @param InitOpts Initial configuration options
%% @returns {ok, Req} with prepared request, or {error, Reason}
prepare_join_request(InitOpts) ->
    maybe
        Wallet = hb_opts:get(priv_wallet, undefined, InitOpts),
        {ok, Report} ?= dev_snp:generate(#{}, #{}, InitOpts),
        WalletPub = element(2, Wallet),
        ?event(green_zone, {remove_uncommitted, Report}),
        MergedReq = hb_ao:set(
            Report, 
            <<"public-key">>,
            base64:encode(term_to_binary(WalletPub)),
            InitOpts
        ),
        % Create committed join request using the wallet
        Req = hb_cache:ensure_all_loaded(
            hb_message:commit(MergedReq, InitOpts),
            InitOpts
        ),
        ?event({join_req, {explicit, Req}}),
        ?event({verify_res, hb_message:verify(Req)}),
        {ok, Req}
    end.

%% @doc Verify that response is from expected peer.
%%
%% This function verifies the response signature and ensures it comes
%% from the expected peer to prevent man-in-the-middle attacks.
%%
%% @param Resp Response from peer
%% @param PeerID Expected peer identifier
%% @param InitOpts Configuration options
%% @returns true if verified, false otherwise
verify_peer_response(Resp, PeerID, InitOpts) ->
    ?event(green_zone, {join, join_response, Resp}),
    Signers = hb_message:signers(Resp, InitOpts),
    ?event(green_zone, {join, signers, Signers}),
    IsVerified = hb_message:verify(Resp, Signers, InitOpts),
    ?event(green_zone, {join, verify, IsVerified}),
    IsPeerSigner = lists:member(PeerID, Signers),
    ?event(green_zone, {join, peer_is_signer, IsPeerSigner, PeerID}),
    IsPeerSigner andalso IsVerified.

%% @doc Extract and decrypt zone key from peer response.
%%
%% This function extracts the encrypted zone key from the peer's response
%% and decrypts it using the local node's private key.
%%
%% @param Resp Response containing encrypted zone key
%% @param InitOpts Configuration options
%% @returns {ok, AESKey} with decrypted key, or {error, Reason}
extract_and_decrypt_zone_key(Resp, InitOpts) ->
    ZoneKey = hb_ao:get(<<"zone-key">>, Resp, InitOpts),
    decrypt_zone_key(ZoneKey, InitOpts).

%% @doc Finalize successful join by updating configuration.
%%
%% This function updates the node's configuration with the shared AES key
%% and returns a success response.
%%
%% @param AESKey Decrypted shared AES key
%% @param InitOpts Initial configuration options
%% @returns {ok, Map} with success response
finalize_join_success(AESKey, InitOpts) ->
    ?event(green_zone, {opts, {explicit, InitOpts}}),
    NewOpts = InitOpts#{priv_green_zone_aes => AESKey},
    hb_http_server:set_opts(NewOpts),
    {ok, #{
        <<"body">> => <<"Node joined green zone successfully.">>, 
        <<"status">> => 200
    }}.

%%% -------------------------------------------------------------------
%%% Helpers for validate_join/3
%%% -------------------------------------------------------------------

%% @doc Validates an incoming join request from another node.
%%
%% This function handles the server-side join flow when receiving a connection
%% request:
%% 1. Validates the peer's configuration meets required standards
%% 2. Extracts join request data (node address and public key)
%% 3. Verifies the hardware-backed commitment report
%% 4. Processes the successful join through helper functions
%%
%% @param M1 Ignored parameter
%% @param Req The join request containing commitment report and public key
%% @param Opts A map of configuration options
%% @returns `{ok, Map}' on success with encrypted AES key, or
%% `{error, Binary}' on failure with error message
validate_join(M1, Req, Opts) ->
    maybe
        ?event(green_zone, {join, start}),
        % Validate peer configuration
        true ?= validate_peer_opts(Req, Opts),
        % Extract join request data
        {ok, {NodeAddr, RequesterPubKey}} ?= 
            extract_join_request_data(Req, Opts),
        % Verify commitment report
        {ok, <<"true">>} ?= dev_snp:verify(M1, Req, Opts),
        ?event(green_zone, {join, commitment, verified}),
        % Process successful join
        process_successful_join(NodeAddr, RequesterPubKey, Req, Opts)
    else
        false ->
            throw(invalid_join_request);
        {ok, <<"false">>} ->
            ?event(green_zone, {join, commitment, failed}),
            {error, <<"Received invalid commitment report.">>};
        Error ->
            ?event(green_zone, {join, commitment, error, Error}),
            Error
    end.

%% @doc Extract join request data including node address and public key.
%%
%% This function extracts and processes the essential data from a join request,
%% including the node address and decoded public key.
%%
%% @param Req Join request message
%% @param Opts Configuration options
%% @returns {ok, {NodeAddr, RequesterPubKey}} or {error, Reason}
extract_join_request_data(Req, Opts) ->
    maybe
        % Extract basic request data
        NodeAddr = hb_ao:get(<<"address">>, Req, Opts),
        ?event(green_zone, {join, extract, {node_addr, NodeAddr}}),
        % Extract and decode public key
        EncodedPubKey = hb_ao:get(<<"public-key">>, Req, Opts),
        ?event(green_zone, {encoded_pub_key, {explicit, EncodedPubKey}}),
        RequesterPubKey = case EncodedPubKey of
            not_found -> not_found;
            Encoded -> binary_to_term(base64:decode(Encoded))
        end,
        ?event(green_zone, {public_key, {explicit, RequesterPubKey}}),
        {ok, {NodeAddr, RequesterPubKey}}
    end.

%% @doc Process a successful join by adding node and encrypting zone key.
%%
%% This function handles the final steps of a successful join request,
%% including adding the node to trusted list and encrypting the zone key.
%%
%% @param NodeAddr Address of joining node
%% @param RequesterPubKey Public key of joining node
%% @param Req Original join request (for Report)
%% @param Opts Configuration options
%% @returns {ok, Map} with success response
process_successful_join(NodeAddr, RequesterPubKey, Req, Opts) ->
    % Get required data
    Report = hb_ao:get(<<"report">>, Req, Opts),
    GreenZoneAES = hb_opts:get(priv_green_zone_aes, undefined, Opts),
    ?event(green_zone, {green_zone_aes, {explicit, GreenZoneAES}}),
    {WalletPubKey, _} = hb_opts:get(priv_wallet, undefined, Opts),
    % Add joining node to trusted nodes
    add_trusted_node(NodeAddr, Report, RequesterPubKey, Opts),
    ?event(green_zone, {join, update, trusted_nodes, ok}),
    % Encrypt shared AES key for the joining node
    EncryptedPayload = encrypt_payload(GreenZoneAES, RequesterPubKey),
    ?event(green_zone, {join, encrypt, aes_key, complete}),
    {ok, #{
        <<"body">>         => <<"Node joined green zone successfully.">>,
        <<"node-address">> => NodeAddr,
        <<"zone-key">>     => base64:encode(EncryptedPayload),
        <<"public-key">>   => WalletPubKey
    }}.

%% @doc Validates that a peer's configuration matches required options.
%%
%% This function ensures the peer node meets configuration requirements:
%% 1. Retrieves the local node's required configuration
%% 2. Gets the peer's options from its message
%% 3. Adds required configuration to peer's required options list
%% 4. Verifies the peer's node history is valid
%% 5. Checks that the peer's options match the required configuration
%%
%% @param Req The request message containing the peer's configuration
%% @param Opts A map of the local node's configuration options
%% @returns true if the peer's configuration is valid, false otherwise
validate_peer_opts(Req, Opts) ->
    ?event(green_zone, {validate_peer_opts, start, Req}),
    % Get the required config from the local node's configuration.
    RequiredConfig =
        hb_ao:normalize_keys(
            hb_opts:get(green_zone_required_opts, #{}, Opts)),
    ConvertedRequiredConfig = 
        hb_message:uncommitted(
            hb_cache:ensure_all_loaded(
                hb_message:commit(RequiredConfig, Opts),
                Opts
            )
        ),
    ?event(green_zone, 
        {validate_peer_opts, required_config, ConvertedRequiredConfig}
    ),
    PeerOpts =
        hb_ao:normalize_keys(
            hb_ao:get(<<"node-message">>, Req, undefined, Opts)),
    % Validate each item in node_history has required options
    Result = try
        case hb_opts:ensure_node_history(PeerOpts, ConvertedRequiredConfig) of
            {ok, _} -> 
                ?event(green_zone, 
                    {validate_peer_opts, history_items_check, valid}
                ),
                true;
            {error, ErrorMsg} ->
                ?event(green_zone, 
                    {
                        validate_peer_opts, 
                        history_items_check, 
                        {invalid, ErrorMsg}
                    }
                ),
                false
        end
            catch
        HistError:HistReason:HistStacktrace ->
            ?event(green_zone, {validate_peer_opts, history_items_error, 
                {HistError, HistReason, HistStacktrace}}),
                    false
    end,
    ?event(green_zone, {validate_peer_opts, final_result, Result}),
    Result.

%% @doc Adds a node to the trusted nodes list with its commitment report.
%%
%% This function updates the trusted nodes configuration:
%% 1. Retrieves the current trusted nodes map
%% 2. Adds the new node with its report and public key
%% 3. Updates the node configuration with the new trusted nodes list
%%
%% @param NodeAddr The joining node's address
%% @param Report The commitment report provided by the joining node
%% @param RequesterPubKey The joining node's public key
%% @param Opts A map of configuration options
%% @returns ok
add_trusted_node(NodeAddr, Report, RequesterPubKey, Opts) ->
    % Retrieve the current trusted nodes map.
    TrustedNodes = hb_opts:get(trusted_nodes, #{}, Opts),
    % Add the joining node's details to the trusted nodes.
    UpdatedTrustedNodes = maps:put(NodeAddr, #{
        report => Report,
        public_key => RequesterPubKey
    }, TrustedNodes),
    % Update configuration with the new trusted nodes and AES key.
    ok = hb_http_server:set_opts(Opts#{
        trusted_nodes => UpdatedTrustedNodes
    }).

%%% -------------------------------------------------------------------
%%% Helpers for key/3
%%% -------------------------------------------------------------------

%% @doc Get the appropriate wallet for the current context.
%%
%% This function determines which wallet to use based on whether the node
%% has a green-zone identity or should use the default wallet.
%%
%% @param Opts Configuration options containing identities and wallet info
%% @returns Wallet to use for encryption operations
get_appropriate_wallet(Opts) ->
    case hb_opts:as(<<"green-zone">>, Opts) of
        {ok, IdentityOpts} -> hb_opts:get(priv_wallet, undefined, IdentityOpts);
        {error, not_found} -> hb_opts:get(priv_wallet, undefined, Opts)
    end.

%% @doc Build successful key response with encrypted data.
%%
%% This function constructs the standard response format for successful
%% key encryption operations.
%%
%% @param EncryptedData Base64-encoded encrypted key data
%% @param IV Base64-encoded initialization vector
%% @returns {ok, Map} with standardized response format
build_key_response(EncryptedData, IV) ->
    {ok, #{
        <<"status">>        => 200,
        <<"encrypted-key">> => base64:encode(EncryptedData),
        <<"iv">>            => base64:encode(IV)
    }}.

%%% -------------------------------------------------------------------
%%% Helpers for become/3
%%% -------------------------------------------------------------------

%% @doc Validate parameters required for become operation.
%%
%% This function validates that all required parameters are present for
%% the become operation and that the node is part of a green zone.
%%
%% @param Opts Configuration options
%% @returns {ok, {NodeLocation, NodeID}} if valid, or {error, Reason}
validate_become_params(Opts) ->
    maybe
        % Check if node is part of a green zone
        GreenZoneAES = hb_opts:get(priv_green_zone_aes, undefined, Opts),
        case GreenZoneAES of
            undefined -> {error, no_green_zone_aes_key};
            _ -> ok
        end,
        % Extract and validate peer parameters
        NodeLocation = 
            hb_opts:get(green_zone_peer_location, undefined, Opts),
        NodeID = hb_opts:get(green_zone_peer_id, undefined, Opts),
        case {NodeLocation, NodeID} of
            {undefined, _} -> {error, missing_peer_location};
            {_, undefined} -> {error, missing_peer_id};
            {_, _} -> {ok, {NodeLocation, NodeID}}
        end
    end.

%% @doc Request peer's key and verify the response.
%%
%% This function handles the HTTP request to get the peer's encrypted key
%% and verifies that the response is authentic and from the expected peer.
%%
%% @param NodeLocation Target node's address
%% @param NodeID Target node's identifier
%% @param Opts Configuration options
%% @returns {ok, KeyResp} if successful, or {error, Reason}
request_and_verify_peer_key(NodeLocation, NodeID, Opts) ->
    maybe
        ?event(green_zone, {become, getting_key, NodeLocation, NodeID}),
        % Request encrypted key from target node
        {ok, KeyResp} ?= 
            hb_http:get(NodeLocation, <<"/~greenzone@1.0/key">>, Opts),
        % Verify response signature
        Signers = hb_message:signers(KeyResp, Opts),
        true ?= (hb_message:verify(KeyResp, Signers, Opts) and 
                 lists:member(NodeID, Signers)),
        {ok, KeyResp}
    else
        false ->
            {error, invalid_peer_response};
        Error ->
            Error
    end.

%% @doc Finalize the become process by decrypting and adopting target identity.
%%
%% This function completes the identity adoption process by:
%% 1. Extracting and decrypting the target node's encrypted key data
%% 2. Converting the decrypted data back into a keypair structure
%% 3. Creating a new green zone wallet with the target's identity
%% 4. Updating the node's identity configuration
%% 5. Mounting an encrypted volume with the new identity
%% 6. Returning confirmation of successful identity adoption
%%
%% @param KeyResp Response containing encrypted key data from target node
%% @param NodeLocation URL of the target node for logging
%% @param NodeID ID of the target node for logging
%% @param Opts Configuration options containing decryption keys
%% @returns {ok, Map} with success confirmation and peer details
finalize_become(KeyResp, NodeLocation, NodeID, Opts) ->
    maybe
        % Decode and decrypt the encrypted key
        Combined = base64:decode(hb_ao:get(<<"encrypted-key">>, KeyResp, Opts)),
        IV = base64:decode(hb_ao:get(<<"iv">>, KeyResp, Opts)),
        {ok, DecryptedBin} ?= decrypt_data(Combined, IV, Opts),
        % Log current wallet info
        OldWallet = hb_opts:get(priv_wallet, undefined, Opts),
        OldWalletAddr = hb_util:human_id(ar_wallet:to_address(OldWallet)),
        ?event(green_zone, {become, old_wallet, OldWalletAddr}),
        % Extract and process target node's keypair
        {KeyType, Priv, Pub} = binary_to_term(DecryptedBin),
        ?event(green_zone, {become, decrypted_bin, DecryptedBin}),
        ?event(green_zone, {become, keypair, Pub}),
        % Update node identity with target's keypair
        GreenZoneWallet = {{KeyType, Priv, Pub}, {KeyType, Pub}},
        ok ?= update_node_identity(GreenZoneWallet, Opts),
        % Mount encrypted volume and finalize
        try_mount_encrypted_volume(GreenZoneWallet, Opts),
        ?event(green_zone, {become, update_wallet, complete}),
        {ok, #{
            <<"body">> => #{
                <<"message">> => 
                    <<"Successfully adopted target node identity">>,
                <<"peer-location">> => NodeLocation,
                <<"peer-id">> => NodeID
            }
        }}
    end.

%% @doc Update node identity with new green zone wallet.
%%
%% This function updates the node's identity configuration to include
%% the new green zone wallet and commits the changes.
%%
%% @param GreenZoneWallet New wallet to use for green zone identity
%% @param Opts Current configuration options
%% @returns ok if successful
update_node_identity(GreenZoneWallet, Opts) ->
    Identities = hb_opts:get(identities, #{}, Opts),
    UpdatedIdentities = Identities#{
        <<"green-zone">> => #{
            priv_wallet => GreenZoneWallet
        }
    },
    NewOpts = Opts#{identities => UpdatedIdentities},
    hb_http_server:set_opts(NewOpts).

%%% -------------------------------------------------------------------
%%% General/Shared helpers
%%% -------------------------------------------------------------------

%% @doc Prepare a join request with commitment report and public key.
%%
%% This function creates a hardware-backed commitment report and prepares
%% the join request message with the node's public key.
%%
%% @param InitOpts Initial configuration options
%% @returns {ok, Req} with prepared request, or {error, Reason}
default_zone_required_opts(Opts) ->
    #{
        trusted_device_signers => hb_opts:get(trusted_device_signers, [], Opts),
        load_remote_devices => hb_opts:get(load_remote_devices, false, Opts),
        preload_devices => hb_opts:get(preload_devices, [], Opts),
        routes => hb_opts:get(routes, [], Opts),
        on => hb_opts:get(on, undefined, Opts),
        scheduling_mode => disabled,
        initialized => permanent
    }.

%% @doc Replace values of <<"self">> in a configuration map with 
%% corresponding values from Opts.
%%
%% This function iterates through all key-value pairs in the configuration map.
%% If a value is <<"self">>, it replaces that value with the result of
%% hb_opts:get(Key, not_found, Opts) where Key is the corresponding key.
%% The result is passed through hb_cache:ensure_all_loaded/2 so any lazy links
%% in the config or in the fetched Opts values are resolved.
%%
%% @param Config The configuration map to process
%% @param Opts The options map to fetch replacement values from
%% @returns A new map with <<"self">> values replaced and lazy links resolved
replace_self_values(Config, Opts) ->
    Replaced = maps:map(
        fun(Key, Value) ->
            case Value of
                <<"self">> ->
                    hb_opts:get(Key, not_found, Opts);
                _ ->
                    Value
            end
        end,
        Config
    ),
    hb_cache:ensure_all_loaded(Replaced, Opts).

%% @doc Returns `true' if the request is signed by a trusted node.
%%
%% This function verifies whether an incoming request is signed by a node
%% that is part of the trusted nodes list in the green zone. It extracts
%% all signers from the request and checks if any of them match the trusted
%% nodes configured for this green zone.
%%
%% @param _M1 Ignored parameter
%% @param Req The request message to verify
%% @param Opts Configuration options containing trusted_nodes map
%% @returns {ok, Binary} with "true" or "false" indicating trust status
is_trusted(_M1, Req, Opts) ->
    Signers = hb_message:signers(Req, Opts),
    {ok,
        hb_util:bin(
            lists:any(
                fun(Signer) ->
                    lists:member(
                        Signer,
                        maps:keys(hb_opts:get(trusted_nodes, #{}, Opts))
                    )
                end,
                Signers
            )
        )
    }.


%% @doc Encrypts an AES key with a node's RSA public key.
%%
%% This function securely encrypts the shared key for transmission:
%% 1. Extracts the RSA public key components
%% 2. Creates an RSA public key record
%% 3. Performs public key encryption on the AES key
%%
%% @param AESKey The shared AES key (256-bit binary)
%% @param RequesterPubKey The node's public RSA key
%% @returns The encrypted AES key
encrypt_payload(AESKey, RequesterPubKey) ->
    ?event(green_zone, {encrypt_payload, start}),
    %% Expect RequesterPubKey in the form: { {rsa, E}, Pub }
    { {rsa, E}, Pub } = RequesterPubKey,
    RSAPubKey = #'RSAPublicKey'{
        publicExponent = E,
        modulus = crypto:bytes_to_integer(Pub)
    },
    Encrypted = public_key:encrypt_public(AESKey, RSAPubKey),
    ?event(green_zone, {encrypt_payload, complete}),
    Encrypted.

%% @doc Decrypts an AES key using the node's RSA private key.
%%
%% This function handles decryption of the zone key:
%% 1. Decodes the encrypted key if it's in Base64 format
%% 2. Extracts the RSA private key components from the wallet
%% 3. Creates an RSA private key record
%% 4. Performs private key decryption on the encrypted key
%%
%% @param EncZoneKey The encrypted zone AES key (Base64 encoded or binary)
%% @param Opts A map of configuration options
%% @returns {ok, DecryptedKey} on success with the decrypted AES key
decrypt_zone_key(EncZoneKey, Opts) ->
    % Decode if necessary
    RawEncKey = case is_binary(EncZoneKey) of
        true -> base64:decode(EncZoneKey);
        false -> EncZoneKey
    end,
    % Get wallet and extract key components
    {{_KeyType = {rsa, E}, Priv, Pub}, _PubKey} = 
        hb_opts:get(priv_wallet, #{}, Opts),
    % Create RSA private key record
    RSAPrivKey = #'RSAPrivateKey'{
        publicExponent = E,
        modulus = crypto:bytes_to_integer(Pub),
        privateExponent = crypto:bytes_to_integer(Priv)
    },
    DecryptedKey = public_key:decrypt_private(RawEncKey, RSAPrivKey),
    ?event(green_zone, {decrypt_zone_key, complete}),
    {ok, DecryptedKey}.

%% @doc Attempts to mount an encrypted volume using the green zone AES key.
%%
%% This function handles the complete process of secure storage setup by
%% delegating to the dev_volume module, which provides a unified interface
%% for volume management.
%%
%% The encryption key used for the volume is the same AES key used for green 
%% zone communication, ensuring that only nodes in the green zone can access 
%% the data.
%%
%% @param Key The password for the encrypted volume.
%% @param Opts A map of configuration options.
%% @returns ok (implicit) in all cases, with detailed event logs of the results.
try_mount_encrypted_volume(Key, Opts) ->
    ?event(debug_volume, {try_mount_encrypted_volume, start}),
    % Set up options for volume mounting with default paths
    VolumeOpts = Opts#{
        priv_volume_key => Key,
        volume_skip_decryption => <<"true">>
    },
    % Call the dev_volume:mount function to handle the complete process
    case dev_volume:mount(undefined, undefined, VolumeOpts) of
        {ok, Result} ->
            ?event(debug_volume, {volume_mount, success, Result}),
            ok;
        {error, Error} ->
            ?event(debug_volume, {volume_mount, error, Error}),
            ok % Still return ok as this is an optional operation
    end.

%%% ===================================================================
%%% Encryption Helper Functions
%%% ===================================================================

%% @doc Encrypt data using AES-256-GCM with the green zone shared key.
%%
%% Accepts only binary payloads. Encrypt and decrypt are reciprocal for
%% binaries: decrypt_data(Enc, IV, Opts) returns the same binary passed to
%% encrypt_data. Encoding/decoding (e.g. term_to_binary/binary_to_term) is
%% the caller's responsibility.
%%
%% @param Data Binary to encrypt (non-binary returns {error, not_binary})
%% @param Opts Server configuration options containing priv_green_zone_aes
%% @returns {ok, {EncryptedData, IV}} where EncryptedData includes the auth tag,
%%          or {error, Reason} if no AES key, non-binary data, or encryption fails
encrypt_data(Data, Opts) when is_binary(Data) ->
    case hb_opts:get(priv_green_zone_aes, undefined, Opts) of
        undefined ->
            {error, no_green_zone_aes_key};
        AESKey ->
            try
                % Generate random IV
                IV = crypto:strong_rand_bytes(16),
                % Encrypt using AES-256-GCM
                {EncryptedData, Tag} = crypto:crypto_one_time_aead(
                    aes_256_gcm,
                    AESKey,
                    IV,
                    Data,
                    <<>>,
                    true
                ),
                % Combine encrypted data and tag
                Combined = <<EncryptedData/binary, Tag/binary>>,
                {ok, {Combined, IV}}
            catch
                Error:Reason ->
                    {error, {encryption_failed, Error, Reason}}
            end
    end;
encrypt_data(_Data, _Opts) ->
    {error, not_binary}.

%% @doc Decrypt data using AES-256-GCM with the green zone shared key.
%%
%% Returns the same binary that was passed to encrypt_data/2. Decoding
%% (e.g. binary_to_term) is the caller's responsibility.
%%
%% @param Combined The encrypted data with authentication tag appended
%% @param IV The initialization vector used during encryption
%% @param Opts Server configuration options containing priv_green_zone_aes
%% @returns {ok, DecryptedData} or {error, Reason}
decrypt_data(Combined, IV, Opts) ->
    case hb_opts:get(priv_green_zone_aes, undefined, Opts) of
        undefined ->
            {error, no_green_zone_aes_key};
        AESKey ->
            try
                % Separate ciphertext and authentication tag
                CipherLen = byte_size(Combined) - 16,
                case CipherLen >= 0 of
                    false ->
                        {error, invalid_encrypted_data_length};
                    true ->
                        <<Ciphertext:CipherLen/binary, Tag:16/binary>> = 
                            Combined,
                        % Decrypt using AES-256-GCM
                        DecryptedBin = crypto:crypto_one_time_aead(
                            aes_256_gcm,
                            AESKey,
                            IV,
                            Ciphertext,
                            <<>>,
                            Tag,
                            false
                        ),
                        {ok, DecryptedBin}
                end
            catch
                Error:Reason ->
                    {error, {decryption_failed, Error, Reason}}
            end
    end.

%%% ===================================================================
%%% Test Functions
%%% ===================================================================

%% @doc Test RSA operations with the existing wallet structure.
%%
%% This test function verifies that encryption and decryption using the RSA keys
%% from the wallet work correctly. It creates a new wallet, encrypts a test
%% message with the RSA public key, and then decrypts it with the RSA private
%% key, asserting that the decrypted message matches the original.
rsa_wallet_integration_test() ->
    % Create a new wallet using ar_wallet
    Wallet = ar_wallet:new(),
    {{KeyType, Priv, Pub}, {KeyType, Pub}} = Wallet,
    % Create test message
    PlainText = <<"HyperBEAM integration test message.">>,
    % Create RSA public key record for encryption
    RsaPubKey = #'RSAPublicKey'{
        publicExponent = 65537,
        modulus = crypto:bytes_to_integer(Pub)
    },
    % Encrypt using public key
    Encrypted = public_key:encrypt_public(PlainText, RsaPubKey),
    % Create RSA private key record for decryption
    RSAPrivKey = #'RSAPrivateKey'{
        publicExponent = 65537,
        modulus = crypto:bytes_to_integer(Pub),
        privateExponent = crypto:bytes_to_integer(Priv)
    },
    % Verify decryption works
    Decrypted = public_key:decrypt_private(Encrypted, RSAPrivKey),
    % Verify roundtrip
    ?assertEqual(PlainText, Decrypted),
    % Verify wallet structure
    ?assertEqual(KeyType, {rsa, 65537}).