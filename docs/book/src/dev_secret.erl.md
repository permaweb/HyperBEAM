# dev_secret

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_secret.erl)

A device that allows a node to create, export, and commit messages with
secrets that are stored on the node itself. Users of this device must specify
an `access-control` message which requests are validated against before 
access to secrets is granted.
This device is intended for use in situations in which the node is trusted
by the user, for example if it is running on their own machine or in a
TEE-protected environment that they deem to be secure.
# Authentication Flow
Each secret is associated with an `access-control` message and a list of
`controllers` that may access it. The `access-control` system is pluggable
-- users may configure their messages to call any AO-Core device that is
executable on the host node. The default `access-control` message uses the
`~cookie@1.0` device's `generate` and `verify` keys to authenticate users.
During secret generation:
1. This device creates the secret and determines its `committer` address.
2. The device invokes the caller's `access-control` message with the `commit`
   path and the `keyid` in the request.
3. The `access-control` message sets up authentication (e.g., creates cookies,
   secrets) and returns a response, containing a commitment with a `keyid`
   field. This `keyid` is used to identify the user's 'access secret' which
   grants them the ability to use the device's 'hidden' secret in the future.
4. This device stores both the secret and the initialized `access-control`
   message, as well as its other metadata.
5. This device returns the initialized `access-control` message with the
   secret's `keyid` added to the `body` field.
During secret operations (commit, export, etc.):
1. This device retrieves the stored `access-control` message for the
   secret either from persistent storage or from the node message's private
   element. The keyid of the `access secret` is either provided by the 
   user in the request, or is determined from a provided `secret` parameter
   in the request.
2. This device calls the `access-control` message with path `verify` and
   the user's request.
3. The `access-control` message verifies the request (e.g., checks cookies,
   provided authentication credentials, etc.).
4. If verification passes, the device performs the requested operation.
5. If verification fails, a 400 error is returned.
# Access Control Message Requirements
Access control messages are fully customizable by callers, but must support
two paths:
`/commit`: Called during secret generation to bind the `access-control`
           template message to the given `keyid` (secret reference).
 - Input:  Request message containing `keyid` field with the secret's `keyid`
           in the `body` field.
 - Output: Response message with authentication setup (cookies, tokens, etc.).
           This message will be used as the `Base` message for the `verify`
           path.
`/verify`: Called before allowing an operation that requires access to a
           secret to proceed.
  - Base:    The initialized `access-control` message from the `commit` path.
  - Request: Caller's request message with authentication credentials.
  - Output:  `false` if an error has occurred. If the request is valid, the
           `access-control` message should return either `true` or a modification
           of the request message which will be used for any subsequent
           operations.
The default `access-control` message is `~cookie@1.0`, which uses HTTP
cookies with secrets to authenticate users.
# Secret Generation Parameters
The following parameters are supported by the `generate` key:
```
/generate
    - `access-control` (optional): The `access-control` message to use.
                 Defaults to `#{<<"device">> => <<"cookie@1.0">>}`.
    - `keyid` (optional): The `keyid` of the secret to generate. If not
                 provided, the secret's address will be used as the name.
    - `persist` (optional): How the node should persist the secret. Options:
      - `client`: The secret is generated on the server, but not persisted.
                 The full secret key is returned for the user to store.
      - `in-memory`: The wallet is generated on the server and persisted only
                 in local memory, never written to disk.
      - `non-volatile`: The wallet is persisted to non-volatile storage on 
                 the node. The store used by this option is segmented from
                 the node's main storage, configurable via the `priv_store`
                 node message option.
    - `controllers` (optional): A list of controllers that may access the
                 secret. Defaults to the node's `wallet_admin` option if set,
                 or its operator address if not.
    - `required-controllers` (optional): The number of controllers that must
                 sign the secret for it to be valid. Defaults to `1`.
    The response will contain authentication setup (such as cookies) from the
    `access-control` message, plus the secret's `keyid` in the `body` field.
    The secret's key is not returned to the user unless the `persist` option
    is set to `client`. If it is, the `~cookie@1.0` device will be employed
    to set the user's cookie with the secret.
/import
    Parameters:
    - `key` (optional): The JSON-encoded secret to import.
    - `cookie` (optional): A structured-fields cookie containing a map with
      a `key` field which is a JSON-encoded secret.
    - `access-control` (optional): The `access-control` message to use.
    - `persist` (optional): How the node should persist the secret. The
      supported options are as with the `generate` key.
    Imports a secret for hosting from the user. Executes as `generate` does,
    except that it expects the key to store to be provided either directly
    via the `key` parameter as a `keyid` field in the cookie Structured-Fields
    map. Support for loading the key from the cookie is provided such that
    a previously-generated secret by the user can have its persistence mode
    changed.
/list
    Parameters:
    - `keyids` (optional): A list of `keyid`s to list. If not provided,
      all secrets will be listed via the `keyid` that is must be provided
      in order to access them.
    Lists all hosted secrets on the node by the `keyid` that is used to
    access them. If `keyids` is provided, only the secrets with those
    `keyid`s will be listed.
/commit
    Parameters:
    - `keyid` (optional): The `keyid` of the secret to commit with.
    - Authentication credentials as required by the `access-control` message.
    Commits the given message using the specified secret after authentication.
    If no `keyid` parameter is provided, the request's authentication data
    (such as cookies) must contain secret identification.
/export
    Parameters:
    - `keyids` (optional): A list of `keyid`s to export, or `all` to
      export all secrets for which the request passes authentication.
    Exports a given secret or set of secrets. If multiple secrets are
    requested, the result is a message with form `keyid => #{ `key` =>
    JSON-encoded secret, `access-control` => `access-control` message,
    `controllers` => [address, ...], `required-controllers` => integer,
    `persist` => `client` | `in-memory` | `non-volatile` }'.
    A secret will be exported if:
    - The given request passes each requested secret's `access-control`
      message; or
    - The request passes each requested secret's `controllers` parameter
      checks.
/sync
    Parameters:
    - `node`: The peer node to pull secrets from.
    - `as` (optional): The identity it should use when signing its request
      to the remote peer.
    - `keyids` (optional): A list of `keyid`s to export, or `all` to load
      every available secret. Defaults to `all`.
    Attempts to download all (or a given subset of) secrets from the given
    node and import them. If the `keyids` parameter is provided, only the
    secrets with those `keyid`s will be imported. The `as` parameter is
    used to inform the node which key it should use to sign its request to
    the remote peer, such that its request validates against the secret's
    `access-control` messages on the remote peer.
'''

---

## Exported Functions

- `commit/3`
- `export/3`
- `generate/3`
- `import/3`
- `list/3`
- `sync/3`

---

### generate

A device that allows a node to create, export, and commit messages with
Generate a new wallet for a user and register it on the node. If the

```erlang
generate(Base, Request, Opts) ->
    case request_to_wallets(Base, Request, Opts) of
        [] ->
            % No wallets found, create a new one.
```

### import

Import a wallet for hosting on the node. Expects the keys to be either

```erlang
import(Base, Request, Opts) ->
    Wallets =
        case hb_maps:find(<<"key">>, Request, Opts) of
            {ok, Keys} when is_list(Keys) ->
                [ wallet_from_key(Key) || Key <- Keys ];
            {ok, Key} ->
                [ wallet_from_key(hb_escape:decode_quotes(Key)) ];
            error ->
                request_to_wallets(Base, Request, Opts)
        end,
    case Wallets of
        [] ->
            {error, <<"No viable wallets found to import.">>};
        Wallets ->
            import_wallets(Wallets, Base, Request, Opts)
    end.
```

### import_wallets

Register a series of wallets, returning a summary message with the

```erlang
import_wallets(Wallets, Base, Request, Opts) ->
    Res =
        lists:foldl(
            fun(Wallet, Acc) ->
                case register_wallet(Wallet, Base, Request, Opts) of
                    {ok, RegRes} ->
                        % Merge the private element of the registration response
                        % into the accumulator.
```

### wallet_from_key

Transform a wallet key serialized form into a wallet.

```erlang
wallet_from_key(Key) when is_binary(Key) ->
    ar_wallet:from_json(Key);
```

### wallet_from_key

Transform a wallet key serialized form into a wallet.

```erlang
wallet_from_key(Key) ->
    Key.
```

### register_wallet

Register a wallet on the node.

```erlang
register_wallet(Wallet, Base, Request, Opts) ->
    % Find the wallet's address.
```

### persist_registered_wallet

Persist a wallet and return the auth response. Optionally takes a

```erlang
persist_registered_wallet(WalletDetails, Opts) ->
    persist_registered_wallet(WalletDetails, #{}, Opts).
```

### persist_registered_wallet

```erlang
persist_registered_wallet(WalletDetails, RespBase, Opts) ->
    % Add the wallet address as the body of the response.
```

### list

List all hosted wallets

```erlang
list(_Base, _Request, Opts) ->
    {ok, list_wallets(Opts)}.
```

### commit

Sign a message with a wallet.

```erlang
commit(Base, Request, Opts) ->
    ?event({commit_invoked, {base, Base}, {request, Request}}),
    case request_to_wallets(Base, Request, Opts) of
        [] -> {error, <<"No wallets found to sign with.">>};
        WalletDetailsList ->
            ?event(
                {commit_signing,
                    {request, Request},
                    {wallet_list, WalletDetailsList}
                }
            ),
            {
                ok,
                lists:foldl(
                    fun(WalletDetails, Acc) ->
                        ?event(
                            {invoking_commit_message,
                                {message, Acc},
                                {wallet, WalletDetails}
                            }
                        ),
                        commit_message(Acc, WalletDetails, Opts)
                    end,
                    Base,
                    WalletDetailsList
                )
            }
    end.
```

### request_to_wallets

Take a request and return the wallets it references. Performs validation

```erlang
request_to_wallets(Base, Request, Opts) ->
    % Get the wallet references or keys from the request or cookie.
```

### load_and_verify

Load a wallet from a keyid and verify we have the authority to access it.

```erlang
load_and_verify({wallet, WalletKey}, _Base, _Request, _Opts) ->
    % Return the wallet key.
```

### load_and_verify

```erlang
load_and_verify({secret, KeyID, _}, _Base, Request, Opts) ->
    % Get the wallet from the node's options.
```

### verify_controllers

Validate if a calling message has the required `controllers` for the

```erlang
verify_controllers(WalletDetails, Request, Opts) ->
    RequiredControllers =
        hb_util:int(hb_maps:get(<<"required-controllers">>, WalletDetails, 1, Opts)),
    Controllers =
        parse_controllers(
            hb_maps:get(<<"controllers">>, WalletDetails, [], Opts),
            Opts
        ),
    PresentControllers =
        lists:filter(
            fun(Signer) ->
                lists:member(Signer, Controllers)
            end,
            hb_message:signers(Request, Opts)
        ),
    length(PresentControllers) >= RequiredControllers.
```

### verify_auth

Verify a wallet for a given request.

```erlang
verify_auth(WalletDetails, Req, Opts) ->
    AuthBase = hb_maps:get(<<"access-control">>, WalletDetails, #{}, Opts),
    AuthRequest =
        Req#{
            <<"path">> => <<"verify">>,
            <<"committer">> =>
                hb_maps:get(<<"committer">>, WalletDetails, undefined, Opts)
        },
    ?event({verify_wallet, {auth_base, AuthBase}, {request, AuthRequest}}),
    hb_ao:resolve(AuthBase, AuthRequest, Opts).
```

### wallets_from_cookie

Parse cookie from a message to extract wallets.

```erlang
wallets_from_cookie(Msg, Opts) ->
    % Parse the cookie as a Structured-Fields map.
```

### commit_message

Sign a message using hb_message:commit, taking either a wallet as a 

```erlang
commit_message(Message, NonMap, Opts) when not is_map(NonMap) ->
    commit_message(Message, #{ <<"wallet">> => NonMap }, Opts);
```

### commit_message

Sign a message using hb_message:commit, taking either a wallet as a 

```erlang
commit_message(Message, #{ <<"wallet">> := Key }, Opts) when is_binary(Key) ->
    commit_message(Message, ar_wallet:from_json(Key), Opts);
```

### commit_message

Sign a message using hb_message:commit, taking either a wallet as a 

```erlang
commit_message(Message, #{ <<"wallet">> := Key }, Opts) ->
    ?event({committing_with_proxy, {message, Message}, {wallet, Key}}),
    hb_message:commit(Message, Opts#{ priv_wallet => Key }).
```

### export

Export wallets from a request. The request should contain a source of

```erlang
export(Base, Request, Opts) ->
    PrivOpts = priv_store_opts(Opts),
    ModReq =
        case hb_ao:get(<<"keyids">>, Request, not_found, Opts) of
            <<"all">> ->
                AllLocalWallets = list_wallets(Opts),
                Request#{ <<"keyids">> => AllLocalWallets };
            _ -> Request
        end,
    ?event({export, {base, Base}, {request, ModReq}}),
    case request_to_wallets(Base, ModReq, Opts) of
        [] -> {error, <<"No wallets found to export.">>};
        Wallets ->
            {
                ok,
                lists:map(
                    fun(Wallet) ->
                        Loaded = hb_cache:ensure_all_loaded(Wallet, PrivOpts),
                        ?event({exported, {wallet, Loaded}}),
                        Loaded
                    end,
                    Wallets
                )
            }
    end.    
```

### sync

Sync wallets from a remote node

```erlang
sync(_Base, Request, Opts) ->
    case hb_ao:get(<<"node">>, Request, undefined, Opts) of
        undefined ->
            {error, <<"Node not specified.">>};
        Node ->
            Wallets = hb_maps:get(<<"keyids">>, Request, <<"all">>, Opts),
            SignAsOpts =
                case hb_ao:get(<<"as">>, Request, undefined, Opts) of
                    undefined -> Opts;
                    SignAs -> hb_opts:as(SignAs, Opts)
                end,
            ExportRequest =
                (hb_message:commit(
                    #{ <<"keyids">> => Wallets },
                    SignAsOpts
                ))#{ <<"path">> => <<"/~secret@1.0/export">> },
            ?event({sync, {export_req, ExportRequest}}),
            case hb_http:get(Node, ExportRequest, SignAsOpts) of
                {ok, ExportResponse} ->
                    ExportedWallets = export_response_to_list(ExportResponse, #{}),
                    ?event({sync, {received_wallets, ExportedWallets}}),
                    % Import each wallet. Ignore wallet imports that fail.
```

### secrets_to_keyids

Convert a key to a wallet reference.

```erlang
secrets_to_keyids(Secrets) when is_list(Secrets) ->
    [ hd(secrets_to_keyids(Secret)) || Secret <- Secrets ];
```

### secrets_to_keyids

Convert a key to a wallet reference.
Parse the exportable setting for a wallet and return a list of addresses

```erlang
secrets_to_keyids(Secret) when is_binary(Secret) ->
    ?event({secrets_to_keyids, {secret, Secret}}),
    KeyID = dev_codec_httpsig_keyid:secret_key_to_committer(Secret),
    [ {secret, <<"secret:", KeyID/binary>>, Secret} ].
```

### parse_controllers

Convert a key to a wallet reference.
Parse the exportable setting for a wallet and return a list of addresses

```erlang
parse_controllers(default, Opts) ->
    case hb_opts:get(wallet_admin, undefined, Opts) of
        undefined -> 
            case hb_opts:get(operator, undefined, Opts) of
                undefined ->
                    [hb_util:human_id(hb_opts:get(priv_wallet, undefined, Opts))];
                Op -> [hb_util:human_id(Op)]
            end;
        Admin -> [Admin]
    end;
```

### parse_controllers

Convert a key to a wallet reference.
Parse the exportable setting for a wallet and return a list of addresses

```erlang
parse_controllers(true, Opts) -> parse_controllers(default, Opts);
```

### parse_controllers

Convert a key to a wallet reference.
Parse the exportable setting for a wallet and return a list of addresses

```erlang
parse_controllers(false, _Opts) -> [];
```

### parse_controllers

Convert a key to a wallet reference.
Parse the exportable setting for a wallet and return a list of addresses

```erlang
parse_controllers(Addresses, _Opts) when is_list(Addresses) -> Addresses;
```

### parse_controllers

Convert a key to a wallet reference.
Parse the exportable setting for a wallet and return a list of addresses
Store a wallet in the appropriate location.

```erlang
parse_controllers(Address, _Opts) when is_binary(Address) -> [Address].
```

### store_wallet

Convert a key to a wallet reference.
Parse the exportable setting for a wallet and return a list of addresses
Store a wallet in the appropriate location.

```erlang
store_wallet(in_memory, KeyID, Details, Opts) ->
    % Get existing wallets
    CurrentWallets = hb_opts:get(priv_wallet_hosted, #{}, Opts),
    % Add new wallet
    UpdatedWallets = CurrentWallets#{ KeyID => Details },
    ?event({wallet_store, {updated_wallets, UpdatedWallets}}),
    % Update the node's options with the new wallets.
```

### store_wallet

```erlang
store_wallet(non_volatile, KeyID, Details, Opts) ->
    % Find the private store of the node.
```

### find_wallet

Find the wallet by name or address in the node's options.

```erlang
find_wallet(KeyID, Opts) ->
    case find_wallet(in_memory, KeyID, Opts) of
        not_found -> find_wallet(non_volatile, KeyID, Opts);
        Wallet -> Wallet
    end.
```

### find_wallet

Loop over the wallets and find the reference to the wallet.

```erlang
find_wallet(in_memory, KeyID, Opts) ->
    Wallets = hb_opts:get(priv_wallet_hosted, #{}, Opts),
    ?event({find_wallet, {keyid, KeyID}, {wallets, Wallets}}),
    case hb_maps:find(KeyID, Wallets, Opts) of
        {ok, Wallet} -> Wallet;
        error -> not_found
    end;
```

### find_wallet

Loop over the wallets and find the reference to the wallet.

```erlang
find_wallet(non_volatile, KeyID, Opts) ->
    PrivOpts = priv_store_opts(Opts),
    Store = hb_opts:get(priv_store, undefined, PrivOpts),
    Resolved = hb_store:resolve(Store, <<"wallet@1.0/", KeyID/binary>>),
    case hb_cache:read(Resolved, PrivOpts) of
        {ok, Wallet} ->
            WalletDetails = hb_maps:get(KeyID, Wallet, not_found, PrivOpts),
            hb_cache:ensure_all_loaded(WalletDetails, PrivOpts);
        _ -> not_found
    end.
```

### list_wallets

Generate a list of all hosted wallets.

```erlang
list_wallets(Opts) ->
    list_wallets(in_memory, Opts) ++ list_wallets(non_volatile, Opts).
```

### list_wallets

```erlang
list_wallets(in_memory, Opts) ->
    hb_maps:keys(hb_opts:get(priv_wallet_hosted, #{}, Opts));
```

### list_wallets

Generate a new `Opts` message with the `priv_store` as the only `store`

```erlang
list_wallets(non_volatile, Opts) ->
    PrivOpts = priv_store_opts(Opts),
    hb_cache:ensure_all_loaded(hb_cache:list(<<"wallet@1.0/">>, PrivOpts), PrivOpts).
```

### priv_store_opts

Generate a new `Opts` message with the `priv_store` as the only `store`

```erlang
priv_store_opts(Opts) ->
    hb_private:opts(Opts).
```

### export_response_to_list

Convert an export response into a list of wallet details. This is

```erlang
export_response_to_list(ExportResponse, Opts) ->
    hb_util:numbered_keys_to_list(ExportResponse, Opts).
```

### addresses_to_binary

Convert a list of addresses to a binary string. If the input is a

```erlang
addresses_to_binary(Addresses) when is_list(Addresses) ->
    hb_util:bin(string:join(
        lists:map(fun hb_util:list/1, Addresses),
        ", "
    ));
```

### addresses_to_binary

Convert a list of addresses to a binary string. If the input is a

```erlang
addresses_to_binary(Address) when is_binary(Address) ->
    Address.
```

### binary_to_addresses

Convert a binary string to a list of addresses. If the input is a

```erlang
binary_to_addresses(AddressesBin) when is_binary(AddressesBin) ->
    binary:split(AddressesBin, <<",">>, [global]);
```

### binary_to_addresses

Convert a binary string to a list of addresses. If the input is a

```erlang
binary_to_addresses(Addresses) when is_list(Addresses) ->
    Addresses.
```

### test_wallet_generate_and_verify

Helper function to test wallet generation and verification flow.

```erlang
test_wallet_generate_and_verify(GeneratePath, ExpectedName, CommitParams) ->
    Node = hb_http_server:start_node(#{
        priv_wallet => ar_wallet:new()
    }),
    % Generate wallet with specified parameters
    {ok, GenResponse} = hb_http:get(Node, GeneratePath, #{}),
    % Should get wallet name in body, wallet-address, and auth cookie
    ?assertMatch(#{<<"body">> := _}, GenResponse),
    WalletAddr = maps:get(<<"wallet-address">>, GenResponse),
    case ExpectedName of
        undefined -> 
            % For unnamed wallets, just check it's a non-empty binary
            ?assert(is_binary(WalletAddr) andalso byte_size(WalletAddr) > 0);
        _ -> 
            % For named wallets, check exact match
            ?assertEqual(ExpectedName, WalletAddr)            
    end,
    ?assertMatch(#{ <<"priv">> := #{ <<"cookie">> := _ } }, GenResponse),
    #{ <<"priv">> := Priv } = GenResponse,
    % Now verify by signing a message
    TestMessage =
        maps:merge(
            #{
                <<"device">> => <<"secret@1.0">>,
                <<"path">> => <<"commit">>,
                <<"body">> => <<"Test message">>,
                <<"priv">> => Priv
            },
            CommitParams
        ),
    ?event({signing_with_cookie, {test_message, TestMessage}}),
    {ok, SignedMessage} = hb_http:post(Node, TestMessage, #{}),
    % Should return signed message with correct signer
    ?assertMatch(#{ <<"body">> := <<"Test message">> }, SignedMessage),
    ?assert(hb_message:signers(SignedMessage, #{}) =:= [WalletAddr]).
```

### client_persist_generate_and_verify_test

```erlang
client_persist_generate_and_verify_test() ->
    test_wallet_generate_and_verify(
        <<"/~secret@1.0/generate?persist=client">>,
        undefined,
        #{}
    ).
```

### cookie_wallet_generate_and_verify_test

```erlang
cookie_wallet_generate_and_verify_test() ->
    test_wallet_generate_and_verify(
        <<"/~secret@1.0/generate?persist=in-memory">>,
        undefined,
        #{}
    ).
```

### non_volatile_persist_generate_and_verify_test

```erlang
non_volatile_persist_generate_and_verify_test() ->
    test_wallet_generate_and_verify(
        <<"/~secret@1.0/generate?persist=non-volatile">>,
        undefined,
        #{}
    ).
```

### import_wallet_with_key_test

```erlang
import_wallet_with_key_test() ->
    Node = hb_http_server:start_node(#{
        priv_wallet => ar_wallet:new()
    }),
    % Create a test wallet key to import (in real scenario from user).
```

### list_wallets_test

```erlang
list_wallets_test() ->
    Node = hb_http_server:start_node(#{
        priv_wallet => ar_wallet:new()
    }),
    % Generate some wallets first.
```

### commit_with_cookie_wallet_test

```erlang
commit_with_cookie_wallet_test() ->
    Node = hb_http_server:start_node(#{
        priv_wallet => ar_wallet:new()
    }),
    % Generate a client wallet to get a cookie with full wallet key.
```

### export_wallet_test

```erlang
export_wallet_test() ->
    Node = hb_http_server:start_node(#{}),
    % Generate a wallet to export.
```

### export_non_volatile_wallet_test

```erlang
export_non_volatile_wallet_test() ->
        Node = hb_http_server:start_node(#{
            priv_wallet => ar_wallet:new()
        }),
        % Generate a wallet to export.
```

### export_individual_batch_wallets_test

```erlang
export_individual_batch_wallets_test() ->
    Node =
        hb_http_server:start_node(
            AdminOpts =
                #{
                    priv_wallet => AdminWallet = ar_wallet:new()
                }
        ),
    % Generate multiple wallets and collect auth cookies.
```

### export_batch_all_wallets_test

```erlang
export_batch_all_wallets_test() ->
    % Remove all previous cached wallets.
```

### sync_wallets_test

```erlang
sync_wallets_test() ->
    % Remove all previous cached wallets.
```

### sync_non_volatile_wallets_test

```erlang
sync_non_volatile_wallets_test() ->
    % Remove all the previous cached wallets.
```

---

*Generated from [dev_secret.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_secret.erl)*
