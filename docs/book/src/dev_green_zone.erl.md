# dev_green_zone

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_green_zone.erl)

The green zone device, which provides secure communication and identity
management between trusted nodes.
It handles node initialization, joining existing green zones, key exchange,
and node identity cloning. All operations are protected by hardware 
commitment and encryption.

---

## Exported Functions

- `become/3`
- `info/1`
- `info/3`
- `init/3`
- `is_trusted/3`
- `join/3`
- `key/3`

---

### info

The green zone device, which provides secure communication and identity
Controls which functions are exposed via the device API.

```erlang
info(_) -> 
    #{ exports => [info, init, join, become, key, is_trusted] }.
```

### info

Provides information about the green zone device and its API.
Provides the default required options for a green zone.
Replace values of <<"self">> in a configuration map with corresponding values from Opts.
Returns `true` if the request is signed by a trusted node.

```erlang
-spec replace_self_values(Config :: map(), Opts :: map()) -> map().
replace_self_values(Config, Opts) ->
    maps:map(
        fun(Key, Value) ->
            case Value of
                <<"self">> ->
                    hb_opts:get(Key, not_found, Opts);
                _ ->
                    Value
            end
        end,
        Config
    ).
```

```erlang
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
%%
%%
%%
%%
```

### is_trusted

Provides information about the green zone device and its API.
Provides the default required options for a green zone.
Replace values of <<"self">> in a configuration map with corresponding values from Opts.
Returns `true` if the request is signed by a trusted node.

```erlang
-spec replace_self_values(Config :: map(), Opts :: map()) -> map().
replace_self_values(Config, Opts) ->
    maps:map(
        fun(Key, Value) ->
            case Value of
                <<"self">> ->
                    hb_opts:get(Key, not_found, Opts);
                _ ->
                    Value
            end
        end,
        Config
    ).
```

```erlang
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
```

### join

Initiates the join process for a node to enter an existing green zone.

```erlang
-spec join(M1 :: term(), M2 :: term(), Opts :: map()) ->
        {ok, map()} | {error, binary()}.
```

```erlang
join(M1, M2, Opts) ->
    ?event(green_zone, {join, start}),
    PeerLocation = hb_opts:get(<<"green_zone_peer_location">>, undefined, Opts),
    PeerID = hb_opts:get(<<"green_zone_peer_id">>, undefined, Opts),
    Identities = hb_opts:get(identities, #{}, Opts),
    HasGreenZoneIdentity = maps:is_key(<<"green-zone">>, Identities),
    ?event(green_zone, {join_peer, PeerLocation, PeerID, HasGreenZoneIdentity}),
    if (not HasGreenZoneIdentity) andalso (PeerLocation =/= undefined) andalso (PeerID =/= undefined) ->
        join_peer(PeerLocation, PeerID, M1, M2, Opts);
    true ->
        validate_join(M1, M2, hb_cache:ensure_all_loaded(Opts, Opts))
    end.
```

### key

Encrypts and provides the node's private key for secure sharing.

```erlang
-spec key(M1 :: term(), M2 :: term(), Opts :: map()) -> 
    {ok, map()} | {error, binary()}.
```

```erlang
key(_M1, _M2, Opts) ->
    ?event(green_zone, {get_key, start}),
    % Retrieve the shared AES key and the node's wallet.
```

### become

Clones the identity of a target node in the green zone.

```erlang
-spec become(M1 :: term(), M2 :: term(), Opts :: map()) ->
        {ok, map()} | {error, binary()}.
```

```erlang
become(_M1, _M2, Opts) ->
    ?event(green_zone, {become, start}),
    % 1. Retrieve the target node's address from the incoming message.
```

### finalize_become

```erlang
finalize_become(KeyResp, NodeLocation, NodeID, GreenZoneAES, Opts) ->
    % 4. Decode the response to obtain the encrypted key and IV.
```

### join_peer

Processes a join request to a specific peer node.

```erlang
-spec join_peer(
    PeerLocation :: binary(),
    PeerID :: binary(),
    M1 :: term(),
    M2 :: term(),
    Opts :: map()) -> {ok, map()} | {error, map() | binary()}.
```

```erlang
join_peer(PeerLocation, PeerID, _M1, _M2, InitOpts) ->
    % Check here if the node is already part of a green zone.
```

### validate_join

Validates an incoming join request from another node.

```erlang
-spec validate_join(M1 :: term(), Req :: map(), Opts :: map()) ->
        {ok, map()} | {error, binary()}.
```

```erlang
validate_join(M1, Req, Opts) ->
    case validate_peer_opts(Req, Opts) of
        true -> do_nothing;
        false -> throw(invalid_join_request)
    end,
    ?event(green_zone, {join, start}),
    % Retrieve the commitment report and address from the join request.
```

### add_trusted_node

Adds a node to the trusted nodes list with its commitment report.

```erlang
-spec add_trusted_node(
    NodeAddr :: binary(),
    Report :: map(),
    RequesterPubKey :: term(), Opts :: map()) -> ok.
```

```erlang
add_trusted_node(NodeAddr, Report, RequesterPubKey, Opts) ->
    % Retrieve the current trusted nodes map.
```

### decrypt_zone_key

Decrypts an AES key using the node's RSA private key.

```erlang
-spec decrypt_zone_key(EncZoneKey :: binary(), Opts :: map()) ->
        {ok, binary()} | {error, binary()}.
```

```erlang
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
```

### try_mount_encrypted_volume

Attempts to mount an encrypted volume using the green zone AES key.

```erlang
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
```

### rsa_wallet_integration_test

Test RSA operations with the existing wallet structure.

```erlang
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
```

---

*Generated from [dev_green_zone.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_green_zone.erl)*
