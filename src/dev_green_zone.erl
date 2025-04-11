-module(dev_green_zone).
-moduledoc """
The green zone device, which provides secure communication and identity
management between trusted nodes. It handles node initialization, joining
existing green zones, key exchange, and node identity cloning. All operations
are protected by hardware commitment and encryption.
""".
-export([join/3, init/3, become/3, key/3, 
         default_zone_required_opts/1, default_zone_bypass_opts/1]).
-export([list_partitions/3, create_partition/3, format_disk/3, mount_disk/3, change_node_store/3]).
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
- The peer's preprocessor and postprocessor are the same as the local node's.
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
        store => hb_opts:get(store, [], Opts),
        routes => hb_opts:get(routes, [], Opts),
        preprocessor => hb_opts:get(preprocessor, undefined, Opts),
        postprocessor => hb_opts:get(postprocessor, undefined, Opts),
        scheduling_mode => disabled,
        initialized => permanent
	}.

-doc """
Defines options that are allowed to differ from the required options.
These options will bypass the validation check when a node attempts to join
a green zone. This allows for flexibility in certain configuration aspects
while still maintaining the core security requirements.
@param Opts A map containing configuration options.
@returns List of option keys that can bypass the required options validation.
""".
default_zone_bypass_opts(_Opts) ->
    [
        % List of option keys that can be different between nodes
        store
    ].

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
init(_M1, M2, Opts) ->
    ?event(green_zone, {init, start}),
	RequiredConfig =
        hb_ao:get(
            <<"required-config">>,
            M2,
            default_zone_required_opts(Opts),
            Opts
        ),
    % Get the bypass options
    BypassOpts =
        hb_ao:get(
            <<"bypass-config">>,
            M2,
            default_zone_bypass_opts(Opts),
            Opts
        ),
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
    ?event(green_zone, {init, aes_key, generated}),
    % Store the wallet, AES key, and an empty trusted nodes map.
    ok = hb_http_server:set_opts(Opts#{
        priv_wallet => NodeWallet,
        priv_green_zone_aes => GreenZoneAES,
        trusted_nodes => #{},
		green_zone_required_opts => RequiredConfig,
        green_zone_bypass_opts => BypassOpts
    }),
    ?event(green_zone, {init, complete}),
    {ok, <<"Green zone initialized successfully.">>}.

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
	PeerLocation = hb_ao:get(<<"peer-location">>, M1, undefined, Opts),
	PeerID = hb_ao:get(<<"peer-id">>, M1, undefined, Opts),
	?event(green_zone, {join_peer, PeerLocation, PeerID}),
	if (PeerLocation =:= undefined) or (PeerID =:= undefined) ->
		validate_join(M1, M2, Opts);
	true ->
		join_peer(PeerLocation, PeerID, M1, M2, Opts)
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
become(_M1, M2, Opts) ->
    ?event(green_zone, {become, start}),
    % 1. Retrieve the target node's address from the incoming message.
    NodeLocation = hb_ao:get(<<"peer-location">>, M2, Opts),
    NodeID = hb_ao:get(<<"peer-id">>, M2, Opts),
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
			MergedReq = hb_ao:set(
				Report, 
				<<"public-key">>,
				base64:encode(term_to_binary(WalletPub)),
				Opts
			),
			% Create an committed join request using the wallet.
			Req = hb_message:commit(MergedReq, Wallet),
			?event({join_req, Req}),
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
                            hb_http_server:set_opts(Opts#{
                                priv_green_zone_aes => AESKey
                            }),
							?event(successfully_joined_greenzone),
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
    case hb_ao:get(<<"adopt-config">>, Req, true, InitOpts) of
        false ->
            % The node operator does not want to adopt the peer's config. Return
            % the initial options unchanged.
            {ok, InitOpts};
        AdoptConfig ->
			?event(green_zone, 
                {adopt_config, AdoptConfig, PeerLocation, PeerID, InitOpts}),
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
							dev_meta:adopt_node_message(NodeMessage, InitOpts)
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
                <<"adopt-config">>, <<"peer-location">>,
                <<"peer-id">>, <<"path">>, <<"method">>
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
    
    % Get the list of options that can bypass validation from configuration
    BypassOpts = hb_opts:get(
		green_zone_bypass_opts, 
		default_zone_bypass_opts(Opts), 
		Opts
	),
    ?event(green_zone, {validate_peer_opts, bypass_opts, BypassOpts}),
    
    % Remove bypass options from required config for validation
    FilteredRequiredConfig = maps:without(BypassOpts, RequiredConfig),
    ?event(green_zone, {validate_peer_opts, 
		filtered_required_config, 
		FilteredRequiredConfig
	}),
    
    PeerOpts =
        hb_ao:normalize_keys(
            hb_ao:get(<<"node-message">>, Req, undefined, Opts)),
    ?event(green_zone, {validate_peer_opts, peer_opts, PeerOpts}),
    
    % Add the required config itself to the required options of the peer. This
    % enforces that the new peer will also enforce the required config on peers
    % that join them.
	FullRequiredOpts = FilteredRequiredConfig#{
		green_zone_required_opts => RequiredConfig
	},
    ?event(green_zone, {validate_peer_opts, full_required_opts, FullRequiredOpts}),
    
    % Debug: Check if PeerOpts is a map
    ?event(green_zone, 
          {validate_peer_opts, is_map_peer_opts, is_map(PeerOpts)}),
    
    % Debug: Get node_history safely
    NodeHistory = hb_ao:get(<<"node_history">>, PeerOpts, [], Opts),
    ?event(green_zone, {validate_peer_opts, node_history, NodeHistory}),
    
    % Debug: Check length of node_history
    HistoryCheck = case is_list(NodeHistory) of
        true -> length(NodeHistory) =< 1;
        false -> {error, not_a_list}
    end,
    ?event(green_zone, {validate_peer_opts, history_check, HistoryCheck}),
    
    % Debug: Try the match check separately
    MatchCheck = try
        Result = hb_message:match(PeerOpts, FullRequiredOpts, only_present),
        ?event(green_zone, {validate_peer_opts, match_check, Result}),
        Result
    catch
        Error:Reason:Stacktrace ->
            ?event(green_zone, 
                  {validate_peer_opts, match_error, 
                   {Error, Reason, Stacktrace}}),
            false
    end,
    
    % Final result
    FinalResult = MatchCheck andalso (HistoryCheck =:= true),
    ?event(green_zone, {validate_peer_opts, final_result, FinalResult}),
    FinalResult.

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
List available partitions in the system.
This function uses the system command 'fdisk -l' to get a list of all partitions
and returns the formatted output.
@param M1 Ignored parameter.
@param M2 Ignored parameter.
@param Opts A map of configuration options.
@returns {ok, Map} where Map contains the partition information.
""".
-spec list_partitions(M1 :: term(), M2 :: term(), Opts :: map()) ->
    {ok, map()} | {error, binary()}.
list_partitions(_M1, _M2, _Opts) ->
    ?event(green_zone, {disk, list_partitions, start}),
    case os:cmd("sudo fdisk -l") of
        {error, Reason} ->
            ?event(green_zone, {disk, list_partitions, error, Reason}),
            {error, list_to_binary(io_lib:format("Failed to list partitions: ~p", 
                                                [Reason]))};
        Output ->
            ?event(green_zone, {disk, list_partitions, complete}),
            {ok, #{
                <<"status">> => 200,
                <<"partitions">> => list_to_binary(Output)
            }}
    end.

-doc """
Create a partition on a disk device.
This function creates a GPT partition table on the specified device and
creates a single partition that occupies the entire disk.
@param M1 A map containing the device path.
@param M2 A map containing optional parameters like partition type.
@param Opts A map of configuration options.
@returns {ok, Map} on success where Map includes status and partition information.
""".
-spec create_partition(M1 :: map(), M2 :: map(), Opts :: map()) ->
    {ok, map()} | {error, binary()}.
create_partition(M1, M2, Opts) ->
    ?event(green_zone, {disk, create_partition, start}),
    
    % Get the device path from the request
    Device = hb_ao:get(<<"device">>, M1, undefined, Opts),
    
    % Get optional parameters
    PartType = hb_ao:get(<<"partition_type">>, M2, <<"ext4">>, Opts),
    
    % Validate device path
    case Device of
        undefined ->
            ?event(green_zone, 
                  {disk, create_partition, error, <<"no device specified">>}),
            {error, <<"No device path specified.">>};
        _ ->
            % Create a GPT partition table
            MklabelCmd = "sudo parted " ++ binary_to_list(Device) ++ " mklabel gpt",
            MklabelResult = os:cmd(MklabelCmd),
            
            % Check if creating the partition table succeeded
            case string:find(MklabelResult, "Error") of
                nomatch ->
                    % Create a single partition occupying the entire disk
                    MkpartCmd = "sudo parted -a optimal " ++ binary_to_list(Device) 
                                ++ " mkpart primary " ++ binary_to_list(PartType) 
                                ++ " 0% 100%",
                    MkpartResult = os:cmd(MkpartCmd),
                    
                    % Check if creating the partition succeeded
                    case string:find(MkpartResult, "Error") of
                        nomatch ->
                            % Print partition information
                            PrintCmd = "sudo parted " ++ binary_to_list(Device) 
                                      ++ " print",
                            PartitionInfo = os:cmd(PrintCmd),
                            
                            ?event(green_zone, {disk, create_partition, complete}),
                            {ok, #{
                                <<"status">> => 200,
                                <<"message">> => 
                                    <<"Partition created successfully.">>,
                                <<"device">> => Device,
                                <<"partition_info">> => 
                                    list_to_binary(PartitionInfo)
                            }};
                        _ ->
                            ?event(green_zone, 
                                  {disk, create_partition, error, 
                                   list_to_binary(MkpartResult)}),
                            {error, list_to_binary(MkpartResult)}
                    end;
                _ ->
                    ?event(green_zone, 
                          {disk, create_partition, error, 
                           list_to_binary(MklabelResult)}),
                    {error, list_to_binary(MklabelResult)}
            end
    end.

-doc """
Format a disk or partition with LUKS encryption.
This function formats the specified device with LUKS encryption using the
provided AES key for encryption.
@param M1 A map containing the device path.
@param M2 A map containing encryption options.
@param Opts A map of configuration options, must include the AES key.
@returns {ok, Map} on success where Map includes the status and confirmation message.
""".
-spec format_disk(M1 :: map(), M2 :: map(), Opts :: map()) ->
    {ok, map()} | {error, binary()}.
format_disk(M1, M2, Opts) ->
    ?event(green_zone, {disk, format, start}),
    
    % Get the device path from the request
    Device = hb_ao:get(<<"device">>, M1, undefined, Opts),
    
    % Get the encryption key (use the green zone AES key if not specified)
    GreenZoneAES = hb_opts:get(priv_green_zone_aes, undefined, Opts),
    EncKey = hb_ao:get(<<"encryption_key">>, M2, GreenZoneAES, Opts),
    
    % Validate device path and encryption key
    case {Device, EncKey} of
        {undefined, _} ->
            ?event(green_zone, {disk, format, error, <<"no device specified">>}),
            {error, <<"No device path specified.">>};
        {_, undefined} ->
            ?event(green_zone, {disk, format, error, <<"no encryption key">>}),
            {error, <<"No encryption key available for LUKS formatting.">>};
        _ ->
            % Ensure tmp directory exists
            os:cmd("sudo mkdir -p /root/tmp"),
            KeyFile = "/root/tmp/luks_key_" ++ os:getpid(),
            file:write_file(KeyFile, EncKey, [raw]),
            
            % Format the partition using LUKS with the provided key
            % Using --batch to avoid prompts, --key-file to use our key
            FormatCmd = "sudo cryptsetup luksFormat --batch-mode --key-file " 
                        ++ KeyFile ++ " " ++ binary_to_list(Device),
            FormatResult = os:cmd(FormatCmd),
            
            % Remove the temporary key file
            os:cmd("sudo shred -u " ++ KeyFile),
            
            % Check if the command succeeded
            case string:find(FormatResult, "failed") of
                nomatch ->
                    ?event(green_zone, {disk, format, complete}),
                    {ok, #{
                        <<"status">> => 200,
                        <<"message">> => 
                            <<"Disk formatted with LUKS encryption successfully.">>
                    }};
                _ ->
                    ?event(green_zone, {disk, format, error, 
                                       list_to_binary(FormatResult)}),
                    {error, list_to_binary(FormatResult)}
            end
    end.

-doc """
Mount a LUKS-encrypted disk.
This function opens a LUKS-encrypted partition using the provided AES key,
mounts it to the specified mount point, and updates the configuration with
the mount information.
@param M1 A map containing the device path and mount point.
@param M2 A map containing encryption options.
@param Opts A map of configuration options, must include the AES key.
@returns {ok, Map} on success where Map includes the status and confirmation message.
""".
-spec mount_disk(M1 :: map(), M2 :: map(), Opts :: map()) ->
    {ok, map()} | {error, binary()}.
mount_disk(M1, M2, Opts) ->
    ?event(green_zone, {disk, mount, start}),
    
    % Get parameters from the request
    Device = hb_ao:get(<<"device">>, M1, undefined, Opts),
    MountPoint = hb_ao:get(<<"mount_point">>, M1, 
                          <<"/root/mnt/encrypted_data">>, Opts),
    VolumeName = hb_ao:get(<<"volume_name">>, M1, 
                          <<"primary_encrypted_volume">>, Opts),
    
    % Get the encryption key (use the green zone AES key if not specified)
    GreenZoneAES = hb_opts:get(priv_green_zone_aes, undefined, Opts),
    EncKey = hb_ao:get(<<"encryption_key">>, M2, GreenZoneAES, Opts),
    
    % Validate device path and encryption key
    case {Device, EncKey} of
        {undefined, _} ->
            ?event(green_zone, {disk, mount, error, <<"no device specified">>}),
            {error, <<"No device path specified.">>};
        {_, undefined} ->
            ?event(green_zone, {disk, mount, error, <<"no encryption key">>}),
            {error, <<"No encryption key available to open LUKS volume.">>};
        _ ->
            % Ensure tmp directory exists
            os:cmd("sudo mkdir -p /root/tmp"),
            KeyFile = "/root/tmp/luks_key_" ++ os:getpid(),
            file:write_file(KeyFile, EncKey, [raw]),
            
            % Open the LUKS volume
            OpenCmd = "sudo cryptsetup luksOpen --key-file " ++ KeyFile ++ " " 
                      ++ binary_to_list(Device) ++ " " 
                      ++ binary_to_list(VolumeName),
            OpenResult = os:cmd(OpenCmd),
            
            % Remove the temporary key file
            os:cmd("sudo shred -u " ++ KeyFile),
            
            % Check if opening the LUKS volume succeeded
            case string:find(OpenResult, "failed") of
                nomatch ->
                    % Create mount point if it doesn't exist
                    os:cmd("sudo mkdir -p " ++ binary_to_list(MountPoint)),
                    
                    % Mount the unlocked LUKS volume
                    MountCmd = "sudo mount /dev/mapper/" 
                               ++ binary_to_list(VolumeName) 
                               ++ " " ++ binary_to_list(MountPoint),
                    MountResult = os:cmd(MountCmd),
                    
                    % Check if mounting succeeded
                    case string:find(MountResult, "failed") of
                        nomatch ->
                            % Save the mount information in the configuration
                            ok = hb_http_server:set_opts(Opts#{
                                priv_encrypted_disk => #{
                                    device => Device,
                                    mount_point => MountPoint,
                                    volume_name => VolumeName
                                }
                            }),
                            ?event(green_zone, {disk, mount, complete}),
                            {ok, #{
                                <<"status">> => 200,
                                <<"message">> => 
                                    <<"Encrypted disk mounted successfully.">>,
                                <<"mount_point">> => MountPoint
                            }};
                        _ ->
                            % Close the LUKS volume if mounting failed
                            os:cmd("sudo cryptsetup luksClose " 
                                  ++ binary_to_list(VolumeName)),
                            ?event(green_zone, {disk, mount, error, 
                                              list_to_binary(MountResult)}),
                            {error, list_to_binary(MountResult)}
                    end;
                _ ->
                    ?event(green_zone, {disk, mount, error, 
                                      list_to_binary(OpenResult)}),
                    {error, list_to_binary(OpenResult)}
            end
    end.

-doc """
Change the node's data store location to the mounted encrypted disk.
This function updates the node's configuration to use the mounted encrypted
disk as its data store.
@param M1 A map containing the new store path.
@param M2 Ignored parameter.
@param Opts A map of configuration options.
@returns {ok, Map} on success where Map includes the status and confirmation message,
         or {error, Reason} if the operation fails.
""".
-spec change_node_store(M1 :: map(), M2 :: term(), Opts :: map()) ->
    {ok, map()} | {error, binary()}.
change_node_store(M1, _M2, Opts) ->
    ?event(green_zone, {disk, change_store, start}),
    
    % Get the mounted disk information
    EncryptedDisk = hb_opts:get(priv_encrypted_disk, undefined, Opts),
    
    % Get the new store path (default to the mount point + '/store')
    NewStorePath = case EncryptedDisk of
        undefined ->
            hb_ao:get(<<"store_path">>, M1, undefined, Opts);
        #{mount_point := MountPoint} ->
            hb_ao:get(<<"store_path">>, M1, 
                     <<MountPoint/binary, "/store">>, Opts)
    end,
    
    % Validate the store path
    case NewStorePath of
        undefined ->
            ?event(green_zone, {disk, change_store, error, <<"no store path">>}),
            {error, <<"No encrypted disk mounted or store path specified.">>};
        _ ->
            % Create the store directory if it doesn't exist
            os:cmd("sudo mkdir -p " ++ binary_to_list(NewStorePath)),
            
            % Get the current store configuration
            CurrentStore = hb_opts:get(store, [], Opts),
            
            % Update the store configuration with the new path
            NewStore = update_store_config(CurrentStore, NewStorePath),
            
            % Update the node's configuration with the new store
            ok = hb_http_server:set_opts(Opts#{
                store => NewStore
            }),
            
            ?event(green_zone, {disk, change_store, complete}),
            {ok, #{
                <<"status">> => 200,
                <<"message">> => 
                    <<"Node store updated to use encrypted disk.">>,
                <<"store_path">> => NewStorePath
            }}
    end.

-doc """
Update the store configuration with a new base path.
This is a helper function for change_node_store.
@param StoreConfig The current store configuration.
@param NewPath The new base path for the store.
@returns Updated store configuration.
""".
-spec update_store_config(StoreConfig :: term(), NewPath :: binary()) -> term().
update_store_config(StoreConfig, NewPath) when is_list(StoreConfig) ->
    % For a list, update each element
    [update_store_config(Item, NewPath) || Item <- StoreConfig];
update_store_config(#{<<"store-module">> := Module} = StoreConfig, NewPath) 
  when is_map(StoreConfig) ->
    % Handle various store module types differently
    case Module of
        hb_store_fs ->
            % For filesystem store, replace prefix with the new path
            StoreConfig#{<<"prefix">> => NewPath};
        hb_store_rocksdb ->
            % For RocksDB store, replace prefix with the new path
            StoreConfig#{<<"prefix">> => NewPath};
        hb_store_gateway ->
            % For gateway store, recursively update nested store configurations
            NestedStore = maps:get(<<"store">>, StoreConfig, []),
            StoreConfig#{
                <<"store">> => update_store_config(NestedStore, NewPath)
            };
        _ ->
            % For any other store type, update the prefix
            StoreConfig#{<<"prefix">> => NewPath}
    end;
update_store_config(StoreConfig, _NewPath) ->
    % Return unchanged for any other format
    StoreConfig.

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