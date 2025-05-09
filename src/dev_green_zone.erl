-module(dev_green_zone).
-moduledoc """
The green zone device, which provides secure communication and identity
management between trusted nodes. It handles node initialization, joining
existing green zones, key exchange, and node identity cloning. All operations
are protected by hardware commitment and encryption.
""".
-export([join/3, init/3, become/3, key/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

-doc """
The default required options for a green zone. These are intended as
sane basic requirements for a green zone:
- The node will not load remote devices (or trust extra peers).
- The node will use only the default preloaded devices (found on the
  initiating machine).
- The node uses the default store configuration.
- The node will not change its routes from the defaults.
- The peer's hooks are the same as the local node's.
- The node will not schedule messages. Without coordination, peers in the 
  green zone will schedule messages without regard for avoiding
  double-assignment of slots.
- The node must be in a permanent state (no further configuration changes
  being allowed).
Each of these options is derived from the present node's configuration.
""".
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


-doc """
Initialize the green zone.
Sets up the node's cryptographic identity by ensuring that a wallet (keypair)
exists and generating a shared AES key for secure communication. The wallet,
AES key, and an empty trusted nodes list are stored in the node's configuration.
@param M1 Ignored parameter.
@param M2 Optionally contains a `required-config' map. If not provided, the
          default required config (derived from the nodes base configuration)
          will be used.
@param Opts A map containing configuration options. If the wallet is not already
            provided (under key `priv_wallet'), a new one will be created.
@returns {ok, Msg} where Msg is a binary confirmation message.
""".
-spec init(M1 :: term(), M2 :: term(), Opts :: map()) -> {ok, binary()}.
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
    

-doc """
Initiate the join process for a node (Node B).
When Node B wishes to join an existing green zone, it sends a GET request to
its local join endpoint. This request includes a header with the target peer's
address (Node A).

Based on the presence of a peer address:
  - If the target peer is specified, Node B internally routes the request to 
    the join_peer flow, where it generates an commitment report and prepares
    a POST request to forward to Node A.
  - If no peer address is present, the join request is processed locally via
    the validate_join flow.
@param M1 The join request message containing a header with the target peer's
          address.
@param M2 Ignored parameter.
@param Opts A map of configuration options.
@returns {ok, Map} on success with join response details, or {error, Reason}
         on failure.
""".
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

-doc """
Retrieve and encrypt the node's private key.
Encrypts the node's private key using the shared AES key in AES-256-GCM mode. 
It returns the encrypted key along with the initialization vector (IV) needed
for decryption.
@param M1 Ignored parameter.
@param M2 Ignored parameter.
@param Opts A map of configuration options. Must include keys `priv_wallet'
           and `priv_green_zone_aes'.
@returns {ok, Map} on success, where Map contains:
          - status: 200
          - encrypted_key: the encrypted private key (Base64 encoded)
          - iv: the initialization vector (Base64 encoded)
         Returns {error, Reason} if the node is not part of the green zone.
""".
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

-doc """
Clone the identity of a target node.
Allows a node to adopt the identity of a target node by retrieving the target
node's encrypted private key and IV, decrypting it using the shared AES key,
and updating the local node's wallet with the target node's keypair.
@param M1 The message containing the target node's encrypted private key and IV.
@param M2 Ignored parameter.
@param Opts A map of configuration options. Must include `priv_green_zone_aes'.
@returns {ok, Map} on success, where Map includes:
          - status: 200
          - message: confirmation text
          - node: the target node's address
         Returns {error, Reason} if the node is not part of the green zone.
""".
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

%%%--------------------------------------------------------------------
%%% Internal Functions
%%%--------------------------------------------------------------------

-doc """
Process an internal join request when a target peer is specified.
In this flow (executed on Node B):
  1. Node B generates an commitment report and prepares a POST request.
  2. It then forwards the POST request to Node A's join endpoint.
  3. Upon receiving a response from Node A, Node B decrypts the returned 
     zone-key (an encrypted shared AES key) using its local private key, then
     updates its configuration with the shared AES key.
@param PeerLocation The target peer's (Node A's) address.
@param PeerID The target peer's unique identifier.
@param M1 Ignored parameter.
@param M2 Ignored parameter.
@param Opts A map of configuration options.
@returns {ok, Map} on success with a confirmation message, or {error, Map} on failure.
""".
-spec join_peer(
    PeerLocation :: binary(),
    PeerID :: binary(),
    M1 :: term(),
    M2 :: term(),
    Opts :: map()) -> {ok, map()} | {error, map()}.
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
                            ShouldMount = hb_ao:get(<<"should_mount">>, M2, false, NewOpts),
                            case ShouldMount of
                                true ->
                                    try_mount_encrypted_volume(AESKey, NewOpts);
                                false ->
                                    ?event(debug, <<"Not mounting encrypted volume.">>)
                            end,

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

-doc """
If the operator requests it, the node can automatically adopt the 
necessary configuration to join a green zone. `adopt-config' can be a boolean,
a list of fields that should be included in the node message, alongside the
required config of the green zone they are joining.
@param PeerLocation The location of the peer node to join.
@param PeerID The ID of the peer node to join.
@param Req The request message containing the `adopt-config' parameter.
@param InitOpts A map of initial configuration options.
@returns {ok, Opts} where Opts is the updated configuration map, or
         {error, Reason} if the configuration adoption fails.
""".
maybe_set_zone_opts(PeerLocation, PeerID, Req, InitOpts) ->
    case hb_opts:get(<<"green_zone_adopt_config">>, true, InitOpts) of
        false ->
            % The node operator does not want to adopt the peer's config. Return
            % the initial options unchanged.
            {ok, InitOpts};
        AdoptConfig ->
            ?event(green_zone, {adopt_config, AdoptConfig, PeerLocation, PeerID, InitOpts}),
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
                    ?event(green_zone, {join_error, get_req_opts_failed, Reason}),
                    {error, <<"Could not get required config from peer.">>};
                {ok, RequiredConfig} ->
                    % Print the required config response.
                    Signers = hb_message:signers(RequiredConfig),
                    ?event(green_zone, {req_conf_signers, {explicit, Signers}}),
                    % Extract and log the verification steps
                    IsVerified = hb_message:verify(RequiredConfig, Signers),
                    ?event(green_zone, {req_opts, {verified, IsVerified}, {signers, Signers}}),
                    % Combined check
                    case lists:member(PeerID, Signers) andalso IsVerified of
                        false ->
                            % The response is not from the expected peer.
                            {error, <<"Peer gave invalid signature for required config.">>};
                        true ->
                            % Generate the node message that should be set prior to 
                            % joining a green zone.
                            NodeMessage =
                                calculate_node_message(RequiredConfig, Req, AdoptConfig),
                            % Adopt the node message.
                            hb_http_server:set_opts(NodeMessage, InitOpts)
                    end
            end
    end.

-doc """
Generate the node message that should be set prior to joining a green zone.
This function takes a required opts message, a request message, and an `adopt-config'
value. The `adopt-config' value can be a boolean, a list of fields that should be
included in the node message from the request, or a binary string of fields to
include, separated by commas.
@param RequiredOpts The required configuration options from the peer node.
@param Req The request message containing configuration options.
@param AdoptConfig Boolean, list, or binary string indicating which fields to adopt.
@returns A map containing the merged configuration to be used as the node message.
""".
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
    % Convert atoms to binaries in RequiredOpts to prevent binary_to_existing_atom errors
    % The required config should override the request, if necessary.
    maps:merge(StrippedReq, RequiredOpts);
calculate_node_message(RequiredOpts, Req, <<"true">>) ->
    calculate_node_message(RequiredOpts, Req, true);
calculate_node_message(RequiredOpts, Req, List) when is_list(List) ->
    calculate_node_message(RequiredOpts, maps:with(List, Req), true);
calculate_node_message(RequiredOpts, Req, BinList) when is_binary(BinList) ->
    calculate_node_message(RequiredOpts, hb_util:list(BinList), Req).

-doc """
Validate an incoming join request.
When Node A receives a POST join request from Node B, this routine is executed:
  1. It extracts the commitment report, the requesting node's address, and 
     the encoded public key.
  2. It verifies the commitment report included in the request.
  3. If the report is valid, Node A adds Node B to its list of trusted nodes.
  4. Node A then encrypts the shared AES key (zone-key) with Node B's public 
     key and returns it along with its public key.
@param M1 Ignored parameter.
@param Req The join request message containing the commitment report and 
          other join details.
@param Opts A map of configuration options.
@returns {ok, Map} on success with join response details, or {error, Reason}
         if verification fails.
""".
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
    ?event(green_zone, {validate_peer_opts, full_required_opts, FullRequiredOpts}),
    % Debug: Check if PeerOpts is a map
    ?event(green_zone, {validate_peer_opts, is_map_peer_opts, is_map(PeerOpts)}),
    % Debug: Get node_history safely
    NodeHistory = hb_ao:get(<<"node_history">>, PeerOpts, [], Opts),
    ?event(green_zone, {validate_peer_opts, node_history, NodeHistory}),
    % Debug: Check length of node_history
    case NodeHistory of
        List when length(List) =< 1 ->
            ?event(green_zone, {validate_peer_opts, history_check, correct_length}),
            % Debug: Try the match check separately
            try
                MatchCheck =
                    hb_message:match(PeerOpts, FullRequiredOpts, only_present) ==
                        true,
                ?event(green_zone, {validate_peer_opts, match_check, MatchCheck}),
                % Final result
                ?event(green_zone, {validate_peer_opts, final_result, MatchCheck}),
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
    

-doc """
Add a joining node's details to the trusted nodes list.
Updates the local configuration with the new trusted node's commitment report
and public key.
@param NodeAddr The joining node's address.
@param Report The commitment report provided by the joining node.
@param RequesterPubKey The joining node's public key.
@param Opts A map of configuration options.
@returns ok.
""".
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

-doc """
Encrypt the shared AES key with the requester's RSA public key.
Encrypts the shared AES key using the RSA public key provided by the joining
node. The RSA public key is extracted from a tuple and converted into a
record suitable for encryption.
@param AESKey The shared AES key (256-bit binary).
@param RequesterPubKey The requester's public RSA key.
@returns The AES key encrypted with the RSA public key.
""".
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

-doc """
Decrypt the zone AES key using the node's RSA private key.
Decrypts the encrypted zone AES key using the RSA private key from the node's
wallet.
@param EncZoneKey The encrypted zone AES key (Base64 encoded or binary).
@param Opts A map of configuration options.
@returns {ok, DecryptedKey} on success, where DecryptedKey is the shared AES key.
""".
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

-doc """
Attempts to mount an encrypted volume using the green zone AES key.
This function handles the complete process of secure storage setup:
1. Checks if the base device exists
2. Checks if the partition exists on the device
3. If the partition exists, attempts to mount it
4. If the partition doesn't exist, creates it, formats it with LUKS 
   encryption using the green zone AES key, and mounts it

The encryption key used for the volume is the same AES key used for green zone
communication, ensuring that only nodes in the green zone can access the data.

@param AESKey The AES key obtained from joining the green zone.
@param Opts A map of configuration options.
@returns ok (implicit) in all cases, with detailed event logs of the results.
""".
% Attempts to mount an encrypted disk using the green zone AES key
try_mount_encrypted_volume(AESKey, Opts) ->
    Device = <<"/dev/sdc">>,
    Partition = <<Device/binary, "1">>,
    PartitionType = <<"ext4">>,
    VolumeName = <<"hyperbeam_secure">>,
    MountPoint = <<"/root/mnt/", VolumeName/binary>>,
    StorePath = <<MountPoint/binary, "/store">>,
    
    ?event(green_zone, {try_mount_encrypted_volume, start}),
    ?event(green_zone, {try_mount_encrypted_volume, device, Device}),
    ?event(green_zone, {try_mount_encrypted_volume, partition, Partition}),
    ?event(green_zone, {try_mount_encrypted_volume, partition_type, PartitionType}),
    ?event(green_zone, {try_mount_encrypted_volume, volume_name, VolumeName}),
    ?event(green_zone, {try_mount_encrypted_volume, mount_point, MountPoint}),
    

    % First check if the base device exists
    case hb_volume:check_for_device(Device) of
        false ->
            % Base device doesn't exist
            ?event(green_zone, 
                    {device_check, error, <<"Base device not found">>});
        true ->
            % Check if the partition exists
            case hb_volume:check_for_device(Partition) of
                true ->
                    % Partition exists, try mounting it
                    ?event(green_zone, {mount_volume, attempt, Partition}),
                    case hb_volume:mount_disk(Partition, AESKey, 
                                                MountPoint, VolumeName) of
                        {ok, MountResult} ->
                            ?event(green_zone, 
                                    {mount_volume, success, MountResult}),
                            % Update store paths to use the mounted volume
                            CurrentStore = hb_opts:get(store, [], Opts),
                            case hb_volume:change_node_store(StorePath, CurrentStore) of
                                {ok, #{<<"store">> := NewStore} = StoreResult} ->
                                    ?event(green_zone, 
                                            {store_update, success, StoreResult}),
                                    % Update the node's configuration with the new store
                                    ok = hb_http_server:set_opts(
                                        Opts#{
                                            store => NewStore
                                        }
                                    ),
                                    ?event(green_zone, {store_update, config_updated});
                                {error, StoreError} ->
                                    ?event(green_zone, 
                                            {store_update, error, StoreError})
                            end;
                        {error, MountError} ->
                            ?event(green_zone, 
                                    {mount_volume, error, MountError})
                    end;
                false ->
                    % Partition doesn't exist, create it
                    ?event(green_zone, {create_partition, attempt, Device}),
                    case hb_volume:create_partition(Device, PartitionType) of
                        {ok, PartitionResult} ->
                            ?event(green_zone, 
                                    {partition_create, success, PartitionResult}),
                            % Format the new partition
                            case hb_volume:format_disk(Partition, AESKey) of
                                {ok, FormatResult} ->
                                    ?event(green_zone, 
                                            {format_disk, success, FormatResult}),
                                    % Try mounting the formatted partition
                                    case hb_volume:mount_disk(
                                            Partition, AESKey, 
                                            MountPoint, VolumeName) of
                                        {ok, RetryMountResult} ->
                                            ?event(green_zone, 
                                                    {mount_volume, success, 
                                                    RetryMountResult}),
                                            % Update store paths to use the mounted volume
                                            CurrentStore = hb_opts:get(store, [], Opts),
                                            case hb_volume:change_node_store(StorePath, CurrentStore) of
                                                {ok, #{<<"store">> := NewStore} = StoreResult} ->
                                                    ?event(green_zone, 
                                                            {store_update, success, StoreResult}),
                                                    % Update the node's configuration with the new store
                                                    ok = hb_http_server:set_opts(Opts#{
                                                        store => NewStore
                                                    }),
                                                    ?event(green_zone, {store_update, config_updated});
                                                {error, StoreError} ->
                                                    ?event(green_zone, 
                                                            {store_update, error, StoreError})
                                            end;
                                        {error, RetryMountError} ->
                                            ?event(green_zone, 
                                                    {mount_volume, error, 
                                                    RetryMountError})
                                    end;
                                {error, FormatError} ->
                                    ?event(green_zone, 
                                            {format_disk, error, FormatError})
                            end;
                        {error, PartitionError} ->
                            ?event(green_zone, 
                                    {partition_create, error, PartitionError})
                    end
            end
    end.

-doc """
Test RSA operations with the existing wallet structure.
This test function verifies that encryption and decryption using the RSA keys
from the wallet work correctly. It creates a new wallet, encrypts a test
message with the RSA public key, and then decrypts it with the RSA private
key, asserting that the decrypted message matches the original.
""".
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

