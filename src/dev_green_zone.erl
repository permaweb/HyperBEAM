%%% @doc The green zone device, which provides secure communication and identity
%%% management between trusted nodes.
%%%
%%% It handles node initialization, joining existing green zones, key exchange,
%%% and node identity cloning. All operations are protected by hardware 
%%% commitment and encryption.
-module(dev_green_zone).
-export([info/1, info/3, join/3, init/3, become/3, key/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

%% @doc Controls which functions are exposed via the device API.
%%
%% This function defines the security boundary for the green zone device by
%% explicitly listing which functions are available through the API.
%%
%% @param _ Ignored parameter
%% @returns A map with the `exports' key containing a list of allowed functions
info(_) -> 
    #{ exports => [info, init, join, become, key] }.

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
            <<"Green Zone secure communication and identity management for trusted nodes">>,
        <<"version">> => <<"1.0">>,
        <<"api">> => #{
            <<"info">> => #{
                <<"description">> => <<"Get device info">>
            },
            <<"init">> => #{
                <<"description">> => <<"Initialize the green zone">>,
                <<"details">> => 
                    <<"Sets up the node's cryptographic identity with wallet and AES key">>
            },
            <<"join">> => #{
                <<"description">> => <<"Join an existing green zone">>,
                <<"required_node_opts">> => #{
                    <<"green_zone_peer_location">> => <<"Target peer's address">>,
                    <<"green_zone_peer_id">> => <<"Target peer's unique identifier">>
                },
                <<"optional_node_opts">> => #{
                    <<"green_zone_adopt_config">> => 
                        <<"Whether to adopt peer's configuration (default: true)">>
                }
            },
            <<"key">> => #{
                <<"description">> => <<"Retrieve and encrypt the node's private key">>,
                <<"details">> => 
                    <<"Returns the node's private key encrypted with the shared AES key">>
            },
            <<"become">> => #{
                <<"description">> => <<"Clone the identity of a target node">>,
                <<"required_node_opts">> => #{
                    <<"green_zone_peer_location">> => <<"Target peer's address">>,
                    <<"green_zone_peer_id">> => <<"Target peer's unique identifier">>
                }
            }
        }
    },
    {ok, #{<<"status">> => 200, <<"body">> => InfoBody}}.

%% @doc Provides the default required options for a green zone.
%%
%% This function defines the baseline security requirements for nodes in a green zone:
%% 1. Restricts loading of remote devices and only allows trusted signers
%% 2. Limits to preloaded devices from the initiating machine
%% 3. Enforces specific store configuration
%% 4. Prevents route changes from the defaults
%% 5. Requires matching hooks across all peers
%% 6. Disables message scheduling to prevent conflicts
%% 7. Enforces a permanent state to prevent further configuration changes
%%
%% @param Opts A map of configuration options from which to derive defaults
%% @returns A map of required configuration options for the green zone
-spec default_zone_required_opts(Opts :: map()) -> map().
default_zone_required_opts(Opts) ->
    #{
        trusted_device_signers => hb_opts:get(trusted_device_signers, [], Opts),
        load_remote_devices => hb_opts:get(load_remote_devices, false, Opts),
        preload_devices => hb_opts:get(preload_devices, [], Opts),
        % store => hb_opts:get(store, [], Opts),
        routes => hb_opts:get(routes, [], Opts),
        on => hb_opts:get(on, undefined, Opts),
        scheduling_mode => disabled,
        initialized => permanent
    }.


%% @doc Initialize the green zone for a node.
%%
%% This function performs the following operations:
%% 1. Validates the node's history to ensure this is a valid initialization
%% 2. Retrieves or creates a required configuration for the green zone
%% 3. Ensures a wallet (keypair) exists or creates a new one
%% 4. Generates a new 256-bit AES key for secure communication
%% 5. Updates the node's configuration with these cryptographic identities
%%
%% Config options in Opts map:
%% - green_zone_required_config: (Optional) Custom configuration requirements
%% - priv_wallet: (Optional) Existing wallet to use instead of creating a new one
%% - priv_green_zone_aes: (Optional) Existing AES key, if already part of a zone
%%
%% @param _M1 Ignored parameter
%% @param _M2 May contain a `required-config' map for custom requirements
%% @param Opts A map of configuration options
%% @returns `{ok, Binary}' on success with confirmation message, or
%% `{error, Binary}' on failure with error message.
-spec init(M1 :: term(), M2 :: term(), Opts :: map()) -> {ok, binary()} | {error, binary()}.
init(_M1, _M2, Opts) ->
    ?event(green_zone, {init, start}),
    case hb_opts:validate_node_history(Opts) of
        {ok, 1} ->
            RequiredConfig = hb_opts:get(
                <<"green_zone_required_config">>,
                default_zone_required_opts(Opts),
                Opts
            ),
            ?event(green_zone, {init, required_config, RequiredConfig}),
            % Check if a wallet exists; create one if absent.
            NodeWallet = case hb_opts:get(priv_wallet, undefined, Opts) of
                undefined -> 
                    ?event(green_zone, {init, wallet, missing}),
                    hb:wallet();
                ExistingWallet ->
                    ?event(green_zone, {init, wallet, found}),
                    ExistingWallet
            end,
            % Generate a new 256-bit AES key if we have not already joined
            % a green zone.
            GreenZoneAES =
                case hb_opts:get(priv_green_zone_aes, undefined, Opts) of
                    undefined ->
                        ?event(green_zone, {init, aes_key, generated}),
                        crypto:strong_rand_bytes(32);
                    ExistingAES ->
                        ?event(green_zone, {init, aes_key, found}),
                        ExistingAES
                end,
            % Store the wallet, AES key, and an empty trusted nodes map.
            hb_http_server:set_opts(Opts#{
                priv_wallet => NodeWallet,
                priv_green_zone_aes => GreenZoneAES,
                trusted_nodes => #{},
                green_zone_required_opts => RequiredConfig
            }),
            ?event(green_zone, {init, complete}),
            {ok, <<"Green zone initialized successfully.">>};
        {error, Reason} ->
            {error, Reason}
    end.
    

%% @doc Initiates the join process for a node to enter an existing green zone.
%%
%% This function performs the following operations depending on the state:
%% 1. Validates the node's history to ensure proper initialization
%% 2. Checks for target peer information (location and ID)
%% 3. If target peer is specified:
%%    a. Generates a commitment report for the peer
%%    b. Prepares and sends a POST request to the target peer
%%    c. Verifies the response and decrypts the returned zone key
%%    d. Updates local configuration with the shared AES key
%% 4. If no peer is specified, processes the join request locally
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
-spec join(M1 :: term(), M2 :: term(), Opts :: map()) ->
        {ok, map()} | {error, binary()}.
join(M1, M2, Opts) ->
    ?event(green_zone, {join, start}),
    case hb_opts:validate_node_history(Opts, 0, 1) of
        {ok, _N} ->
            PeerLocation = hb_opts:get(<<"green_zone_peer_location">>, undefined, Opts),
            PeerID = hb_opts:get(<<"green_zone_peer_id">>, undefined, Opts),
            ?event(green_zone, {join_peer, PeerLocation, PeerID}),
            if (PeerLocation =:= undefined) or (PeerID =:= undefined) ->
                validate_join(M1, M2, Opts);
            true ->
                join_peer(PeerLocation, PeerID, M1, M2, Opts)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Encrypts and provides the node's private key for secure sharing.
%%
%% This function performs the following operations:
%% 1. Retrieves the shared AES key and the node's wallet
%% 2. Verifies that the node is part of a green zone (has a shared AES key)
%% 3. Generates a random initialization vector (IV) for encryption
%% 4. Encrypts the node's private key using AES-256-GCM with the shared key
%% 5. Returns the encrypted key and IV for secure transmission
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
-spec key(M1 :: term(), M2 :: term(), Opts :: map()) -> 
    {ok, map()} | {error, binary()}.
key(_M1, _M2, Opts) ->
    ?event(green_zone, {get_key, start}),
    % Retrieve the shared AES key and the node's wallet.
    GreenZoneAES = hb_opts:get(priv_green_zone_aes, undefined, Opts),
    {{KeyType, Priv, Pub}, _PubKey} = hb_opts:get(priv_wallet, undefined, Opts),
    ?event(green_zone, 
        {get_key, wallet, hb_util:human_id(ar_wallet:to_address(Pub))}),
    case GreenZoneAES of
        undefined ->
            % Log error if no shared AES key is found.
            ?event(green_zone, {get_key, error, <<"no aes key">>}),
            {error, <<"Node not part of a green zone.">>};
        _ ->
            % Generate an IV and encrypt the node's private key using AES-256-GCM.
            IV = crypto:strong_rand_bytes(16),
            {EncryptedKey, Tag} = crypto:crypto_one_time_aead(
                aes_256_gcm,
                GreenZoneAES,
                IV,
                term_to_binary({KeyType, Priv, Pub}),
                <<>>,
                true
            ),
            
            % Log successful encryption of the private key.
            ?event(green_zone, {get_key, encrypt, complete}),
            {ok, #{
                <<"status">>        => 200,
                <<"encrypted_key">> => 
                    base64:encode(<<EncryptedKey/binary, Tag/binary>>),
                <<"iv">>            => base64:encode(IV)
            }}
    end.

%% @doc Clones the identity of a target node in the green zone.
%%
%% This function performs the following operations:
%% 1. Retrieves target node location and ID from the configuration
%% 2. Verifies that the local node has a valid shared AES key
%% 3. Requests the target node's encrypted key via its key endpoint
%% 4. Verifies the response is from the expected peer
%% 5. Decrypts the target node's private key using the shared AES key
%% 6. Updates the local node's wallet with the target node's identity
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
-spec become(M1 :: term(), M2 :: term(), Opts :: map()) ->
        {ok, map()} | {error, binary()}.
become(_M1, _M2, Opts) ->
    ?event(green_zone, {become, start}),
    % 1. Retrieve the target node's address from the incoming message.
    NodeLocation = hb_opts:get(<<"green_zone_peer_location">>, undefined, Opts),
    NodeID = hb_opts:get(<<"green_zone_peer_id">>, undefined, Opts),
    % 2. Check if the local node has a valid shared AES key.
    GreenZoneAES = hb_opts:get(priv_green_zone_aes, undefined, Opts),
    case GreenZoneAES of
        undefined ->
            % Shared AES key not found: node is not part of a green zone.
            ?event(green_zone, {become, error, <<"no aes key">>}),
            {error, <<"Node not part of a green zone.">>};
        _ ->
            % 3. Request the target node's encrypted key from its key endpoint.
            ?event(green_zone, {become, getting_key, NodeLocation, NodeID}),
            {ok, KeyResp} = hb_http:get(NodeLocation, 
                                       <<"/~greenzone@1.0/key">>, Opts),
            Signers = hb_message:signers(KeyResp),
            case hb_message:verify(KeyResp, Signers) and 
                 lists:member(NodeID, Signers) of
                false ->
                    % The response is not from the expected peer.
                    {error, <<"Received incorrect response from peer!">>};
                true ->
                    finalize_become(KeyResp, NodeLocation, NodeID, 
                                   GreenZoneAES, Opts)
            end
    end.

finalize_become(KeyResp, NodeLocation, NodeID, GreenZoneAES, Opts) ->
    % 4. Decode the response to obtain the encrypted key and IV.
    Combined =
        base64:decode(
            hb_ao:get(<<"encrypted_key">>, KeyResp, Opts)),
    IV = base64:decode(hb_ao:get(<<"iv">>, KeyResp, Opts)),
    % 5. Separate the ciphertext and the authentication tag.
    CipherLen = byte_size(Combined) - 16,
    <<Ciphertext:CipherLen/binary, Tag:16/binary>> = Combined,
    % 6. Decrypt the ciphertext using AES-256-GCM with the shared AES
    %    key and IV.
    DecryptedBin = crypto:crypto_one_time_aead(
        aes_256_gcm,
        GreenZoneAES,
        IV,
        Ciphertext,
        <<>>,
        Tag,
        false
    ),
    OldWallet = hb_opts:get(priv_wallet, undefined, Opts),
    OldWalletAddr = hb_util:human_id(ar_wallet:to_address(OldWallet)),
    ?event(green_zone, {become, old_wallet, OldWalletAddr}),
    % Print the decrypted binary
    ?event(green_zone, {become, decrypted_bin, DecryptedBin}),
    % 7. Convert the decrypted binary into the target node's keypair.
    {KeyType, Priv, Pub} = binary_to_term(DecryptedBin),
    % Print the keypair
    ?event(green_zone, {become, keypair, Pub}),
    % 8. Update the local wallet with the target node's keypair, thereby
    %    cloning its identity.
    ok = hb_http_server:set_opts(Opts#{
        priv_wallet => {{KeyType, Priv, Pub}, {KeyType, Pub}}
    }),
    % Print the updated wallet address
    Wallet = hb_opts:get(priv_wallet, undefined, Opts),
    ?event(green_zone,
        {become, wallet, hb_util:human_id(ar_wallet:to_address(Wallet))}
    ),
    ?event(green_zone, {become, update_wallet, complete}),
    {ok, #{
        <<"status">> => 200,
        <<"message">> => <<"Successfully adopted target node identity">>,
        <<"peer-location">> => NodeLocation,
        <<"peer-id">> => NodeID
    }}.

%% @doc Processes a join request to a specific peer node.
%%
%% This function handles the client-side join flow when connecting to a peer:
%% 1. Verifies the node is not already in a green zone
%% 2. Optionally adopts configuration from the target peer
%% 3. Generates a hardware-backed commitment report
%% 4. Sends a POST request to the peer's join endpoint
%% 5. Verifies the response signature
%% 6. Decrypts the returned AES key
%% 7. Updates local configuration with the shared key
%% 8. Optionally mounts an encrypted volume using the shared key
%%
%% @param PeerLocation The target peer's address
%% @param PeerID The target peer's unique identifier
%% @param _M1 Ignored parameter
%% @param M2 May contain ShouldMount flag to enable encrypted volume mounting
%% @param InitOpts A map of initial configuration options
%% @returns `{ok, Map}' on success with confirmation message, or
%% `{error, Map|Binary}' on failure with error details
-spec join_peer(
    PeerLocation :: binary(),
    PeerID :: binary(),
    M1 :: term(),
    M2 :: term(),
    Opts :: map()) -> {ok, map()} | {error, map() | binary()}.
join_peer(PeerLocation, PeerID, _M1, M2, InitOpts) ->
    % Check here if the node is already part of a green zone.
    GreenZoneAES = hb_opts:get(priv_green_zone_aes, undefined, InitOpts),
    case (GreenZoneAES == undefined) andalso 
         maybe_set_zone_opts(PeerLocation, PeerID, M2, InitOpts) of
        {ok, Opts} ->
            Wallet = hb_opts:get(priv_wallet, undefined, Opts),
            {ok, Report} = dev_snp:generate(#{}, #{}, Opts),
            WalletPub = element(2, Wallet),
            ?event(green_zone, {remove_uncommitted, Report}),
            MergedReq = hb_ao:set(
                Report, 
                <<"public-key">>,
                base64:encode(term_to_binary(WalletPub)),
                Opts
            ),
            % Create an committed join request using the wallet.
            Req = hb_message:commit(MergedReq, Wallet),
            ?event({join_req, {explicit, Req}}),
            ?event({verify_res, hb_message:verify(Req)}),
            % Log that the commitment report is being sent to the peer.
            ?event(green_zone, {join, sending_commitment, PeerLocation, PeerID, Req}),
            case hb_http:post(PeerLocation, <<"/~greenzone@1.0/join">>, Req, Opts) of
                {ok, Resp} ->
                    % Log the response received from the peer.
                    ?event(green_zone, {join, join_response, PeerLocation, PeerID, Resp}),
                    % Ensure that the response is from the expected peer, avoiding
                    % the risk of a man-in-the-middle attack.
                    Signers = hb_message:signers(Resp),
                    ?event(green_zone, {join, signers, Signers}),
                    IsVerified = hb_message:verify(Resp, Signers),
                    ?event(green_zone, {join, verify, IsVerified}),
                    IsPeerSigner = lists:member(PeerID, Signers),
                    ?event(green_zone, {join, peer_is_signer, IsPeerSigner, PeerID}),	
                    case IsPeerSigner andalso IsVerified of
                        false ->
                            % The response is not from the expected peer.
                            {error, <<"Received incorrect response from peer!">>};
                        true ->
                            % Extract the encrypted shared AES key (zone-key) 
                            % from the response.
                            ZoneKey = hb_ao:get(<<"zone-key">>, Resp, Opts),
                            % Decrypt the zone key using the local node's
                            % private key.
                            {ok, AESKey} = decrypt_zone_key(ZoneKey, Opts),
                            % Update local configuration with the retrieved
                            % shared AES key.
                            ?event(green_zone, {oldOpts, {explicit, InitOpts}}),
                            ?event(green_zone, {newOpts, {explicit, Opts}}),
                            NewOpts = Opts#{
                                priv_green_zone_aes => AESKey
                            },
                            hb_http_server:set_opts(NewOpts),
                            ?event(successfully_joined_greenzone),
                            try_mount_encrypted_volume(AESKey, NewOpts),
                            {ok, #{ 
                                <<"body">> => 
                                    <<"Node joined green zone successfully.">>, 
                                <<"status">> => 200
                            }}
                    end;
                {error, Reason} ->
                    {error, #{<<"status">> => 400, <<"reason">> => Reason}};
                {unavailable, Reason} ->
                    ?event(green_zone, {
                        join_error,
                        peer_unavailable,
                        PeerLocation,
                        PeerID,
                        Reason
                    }),
                    {error, #{
                        <<"status">> => 503,
                        <<"body">> => <<"Peer node is unreachable.">>
                    }}
            end;
        false ->
            ?event(green_zone, {join, already_joined}),
            {error, <<"Node already part of green zone.">>};
        {error, Reason} ->
            % Log the error and return the initial options.
            ?event(green_zone, {join, error, Reason}),
            {error, Reason}
    end.

%% @doc Adopts configuration from a peer when joining a green zone.
%%
%% This function handles the conditional adoption of peer configuration:
%% 1. Checks if adoption is enabled (default: true)
%% 2. Requests required configuration from the peer
%% 3. Verifies the authenticity of the configuration
%% 4. Creates a node message with appropriate settings
%% 5. Updates the local node configuration
%%
%% Config options:
%% - green_zone_adopt_config: Controls configuration adoption (boolean, list, or binary)
%%
%% @param PeerLocation The location of the peer node to join
%% @param PeerID The ID of the peer node to join
%% @param Req The request message with adoption preferences
%% @param InitOpts A map of initial configuration options
%% @returns `{ok, Map}' with updated configuration on success, or
%% `{error, Binary}' if configuration retrieval fails
-spec maybe_set_zone_opts(
    PeerLocation :: binary(),
    PeerID :: binary(),
    Req :: map(),
    InitOpts :: map()) -> {ok, map()} | {error, binary()}.
maybe_set_zone_opts(PeerLocation, PeerID, Req, InitOpts) ->
    case hb_opts:get(<<"green_zone_adopt_config">>, true, InitOpts) of
        false ->
            % The node operator does not want to adopt the peer's config. Return
            % the initial options unchanged.
            {ok, InitOpts};
        AdoptConfig ->
            ?event(green_zone, 
                {adopt_config, AdoptConfig, PeerLocation, PeerID, InitOpts}
            ),
            % Request the required config from the peer.
            RequiredConfigRes =
                hb_http:get(
                    PeerLocation,
                    <<"/~meta@1.0/info/green_zone_required_opts">>,
                    InitOpts
                ),
            % Ensure the response is okay.
            ?event({req_opts_get_result, RequiredConfigRes}),
            case RequiredConfigRes of
                {error, Reason} ->
                    % Log the error and return the initial options.
                    ?event(green_zone, 
                        {join_error, get_req_opts_failed, Reason}
                    ),
                    {error, <<"Could not get required config from peer.">>};
                {ok, RequiredConfig} ->
                    % Print the required config response.
                    Signers = hb_message:signers(RequiredConfig),
                    ?event(green_zone, {req_conf_signers, {explicit, Signers}}),
                    % Extract and log the verification steps
                    IsVerified = hb_message:verify(RequiredConfig, Signers),
                    ?event(green_zone, 
                        {req_opts, {verified, IsVerified}, {signers, Signers}}
                    ),
                    % Combined check
                    case lists:member(PeerID, Signers) andalso IsVerified of
                        false ->
                            % The response is not from the expected peer.
                            {
                                error, 
                                <<"Peer gave invalid signature for required config.">>
                            };
                        true ->
                            % Generate the node message that should be set prior
                            % to joining a green zone.
                            NodeMessage =
                                calculate_node_message(
                                    RequiredConfig, 
                                    Req, 
                                    AdoptConfig
                                ),
                            % Adopt the node message.
                            hb_http_server:set_opts(NodeMessage, InitOpts)
                    end
            end
    end.

%% @doc Generate the node message that should be set prior to joining 
%% a green zone.
%%
%% This function takes a required opts message, a request message, and an 
%% `adopt-config' value. The `adopt-config' value can be a boolean, a list of
%% fields that should be included in the node message from the request, or a
%% binary string of fields to include, separated by commas.
%%
%% @param RequiredOpts The required configuration options from the peer node.
%% @param Req The request message containing configuration options.
%% @param AdoptConfig Boolean, list, or binary string indicating which fields
%% to adopt.
%% @returns A map containing the merged configuration to be used as the
%% node message.
calculate_node_message(RequiredOpts, Req, true) ->
    % Remove irrelevant fields from the request.
    StrippedReq =
        maps:without(
            [
                <<"green_zone_adopt_config">>, <<"green_zone_peer_location">>,
                <<"green_zone_peer_id">>, <<"path">>, <<"method">>
            ],
            hb_message:uncommitted(Req)
        ),
    % Convert atoms to binaries in RequiredOpts to prevent
    % binary_to_existing_atom errors.
    % The required config should override the request, if necessary.
    maps:merge(StrippedReq, RequiredOpts);
calculate_node_message(RequiredOpts, Req, <<"true">>) ->
    calculate_node_message(RequiredOpts, Req, true);
calculate_node_message(RequiredOpts, Req, List) when is_list(List) ->
    calculate_node_message(RequiredOpts, maps:with(List, Req), true);
calculate_node_message(RequiredOpts, Req, BinList) when is_binary(BinList) ->
    calculate_node_message(RequiredOpts, hb_util:list(BinList), Req).

%%%--------------------------------------------------------------------
%%% Internal Functions
%%%--------------------------------------------------------------------

%% @doc Validates an incoming join request from another node.
%%
%% This function handles the server-side join flow when receiving a connection
%% request:
%% 1. Validates the peer's configuration meets required standards
%% 2. Extracts the commitment report and public key from the request
%% 3. Verifies the hardware-backed commitment report
%% 4. Adds the joining node to the trusted nodes list
%% 5. Encrypts the shared AES key with the peer's public key
%% 6. Returns the encrypted key to the requesting node
%%
%% @param M1 Ignored parameter
%% @param Req The join request containing commitment report and public key
%% @param Opts A map of configuration options
%% @returns `{ok, Map}' on success with encrypted AES key, or
%% `{error, Binary}' on failure with error message
-spec validate_join(M1 :: term(), Req :: map(), Opts :: map()) ->
        {ok, map()} | {error, binary()}.
validate_join(_M1, Req, Opts) ->
    case validate_peer_opts(Req, Opts) of
        true -> do_nothing;
        false -> throw(invalid_join_request)
    end,
    ?event(green_zone, {join, start}),
    % Retrieve the commitment report and address from the join request.
    Report = hb_ao:get(<<"report">>, Req, Opts),
    NodeAddr = hb_ao:get(<<"address">>, Req, Opts),
    ?event(green_zone, {join, extract, {node_addr, NodeAddr}}),
    % Retrieve and decode the joining node's public key.
    EncodedPubKey = hb_ao:get(<<"public-key">>, Req, Opts),
    RequesterPubKey = case EncodedPubKey of
        not_found -> not_found;
        Encoded -> binary_to_term(base64:decode(Encoded))
    end,
    ?event(green_zone, {join, public_key, ok}),
    % Verify the commitment report provided in the join request.
    case dev_snp:verify(Req, #{<<"target">> => <<"self">>}, Opts) of
        {ok, true} ->
            % Commitment verified.
            ?event(green_zone, {join, commitment, verified}),
            % Retrieve the shared AES key used for encryption.
            GreenZoneAES = hb_opts:get(priv_green_zone_aes, undefined, Opts),
            % Retrieve the local node's wallet to extract its public key.
            {WalletPubKey, _} = hb_opts:get(priv_wallet, undefined, Opts),
            % Add the joining node's details to the trusted nodes list.
            add_trusted_node(NodeAddr, Report, RequesterPubKey, Opts),
            % Log the update of trusted nodes.
            ?event(green_zone, {join, update, trusted_nodes, ok}),
            % Encrypt the shared AES key with the joining node's public key.
            EncryptedPayload = encrypt_payload(GreenZoneAES, RequesterPubKey),
            % Log completion of AES key encryption.
            ?event(green_zone, {join, encrypt, aes_key, complete}),
            {ok, #{
                <<"body">>         => <<"Node joined green zone successfully.">>,
                <<"node-address">> => NodeAddr,
                <<"zone-key">>     => base64:encode(EncryptedPayload),
                <<"public-key">>   => WalletPubKey
            }};
        {ok, false} ->
            % Commitment failed.
            ?event(green_zone, {join, commitment, failed}),
            {error, <<"Received invalid commitment report.">>};
        Error ->
            % Error during commitment verification.
            ?event(green_zone, {join, commitment, error, Error}),
            Error
    end.

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
-spec validate_peer_opts(Req :: map(), Opts :: map()) -> boolean().
validate_peer_opts(Req, Opts) ->
    ?event(green_zone, {validate_peer_opts, start, Req}),
    % Get the required config from the local node's configuration.
    RequiredConfig =
        hb_ao:normalize_keys(
            hb_opts:get(green_zone_required_opts, #{}, Opts)),
    ?event(green_zone, {validate_peer_opts, required_config, RequiredConfig}),
    PeerOpts =
        hb_ao:normalize_keys(
            hb_ao:get(<<"node-message">>, Req, undefined, Opts)),
    ?event(green_zone, {validate_peer_opts, peer_opts, PeerOpts}),
    % Add the required config itself to the required options of the peer. This
    % enforces that the new peer will also enforce the required config on peers
    % that join them.
    FullRequiredOpts = RequiredConfig#{
        green_zone_required_opts => RequiredConfig
    },
    ?event(green_zone, 
        {validate_peer_opts, full_required_opts, FullRequiredOpts}
    ),
    % Debug: Check if PeerOpts is a map
    ?event(green_zone, 
        {validate_peer_opts, is_map_peer_opts, is_map(PeerOpts)}
    ),
    % Debug: Get node_history safely
    NodeHistory = hb_ao:get(<<"node_history">>, PeerOpts, [], Opts),
    ?event(green_zone, {validate_peer_opts, node_history, NodeHistory}),
    % Debug: Check length of node_history
    case NodeHistory of
        List when length(List) =< 1 ->
            ?event(green_zone, 
                {validate_peer_opts, history_check, correct_length}
            ),
            % Debug: Try the match check separately
            try
                MatchCheck =
                    hb_message:match(PeerOpts, FullRequiredOpts, only_present) ==
                        true,
                ?event(green_zone, 
                    {validate_peer_opts, match_check, MatchCheck}
                ),
                % Final result
                ?event(green_zone, 
                    {validate_peer_opts, final_result, MatchCheck}
                ),
                MatchCheck
            catch
                Error:Reason:Stacktrace ->
                    ?event(green_zone,
                        {validate_peer_opts,
                            match_error,
                            {Error, Reason, Stacktrace}
                        }
                    ),
                    false
            end;
        false -> {error, not_a_list}
    end.

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
-spec add_trusted_node(
    NodeAddr :: binary(),
    Report :: map(),
    RequesterPubKey :: term(), Opts :: map()) -> ok.
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
-spec encrypt_payload(AESKey :: binary(), RequesterPubKey :: term()) -> binary().
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
-spec decrypt_zone_key(EncZoneKey :: binary(), Opts :: map()) ->
        {ok, binary()} | {error, binary()}.
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
%% The encryption key used for the volume is the same AES key used for green zone
%% communication, ensuring that only nodes in the green zone can access the data.
%%
%% @param AESKey The AES key obtained from joining the green zone.
%% @param Opts A map of configuration options.
%% @returns ok (implicit) in all cases, with detailed event logs of the results.
try_mount_encrypted_volume(AESKey, Opts) ->
    ?event(green_zone, {try_mount_encrypted_volume, start}),
    % Set up options for volume mounting with default paths
    VolumeOpts = Opts#{
        volume_key => AESKey
    },
    % Call the dev_volume:mount function to handle the complete process
    case dev_volume:mount(undefined, undefined, VolumeOpts) of
        {ok, Result} ->
            ?event(green_zone, {volume_mount, success, Result}),
            ok;
        {error, Error} ->
            ?event(green_zone, {volume_mount, error, Error}),
            ok % Still return ok as this is an optional operation
    end.

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

