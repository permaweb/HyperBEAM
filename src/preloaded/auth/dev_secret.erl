%%% @doc A device that allows a node to create, export, and commit messages with
%%% secrets that are stored on the node itself. Users of this device must specify
%%% an `access-control' message which requests are validated against before 
%%% access to secrets is granted.
%%% 
%%% This device is intended for use in situations in which the node is trusted
%%% by the user, for example if it is running on their own machine or in a
%%% protected environment that they deem to be secure.
%%% 
%%% # Authentication Flow
%%% 
%%% Each secret is associated with an `access-control' message and a list of
%%% `controllers' that may access it. The `access-control' system is pluggable
%%% -- users may configure their messages to call any AO-Core device that is
%%% executable on the host node. The default `access-control' message uses the
%%% `~cookie@1.0' device's `generate' and `verify' keys to authenticate users.
%%% 
%%% During secret generation:
%%% 1. This device creates the secret and determines its `committer' address.
%%% 2. The device invokes the caller's `access-control' message with the `commit'
%%%    path.
%%% 3. The `access-control' message sets up authentication (e.g., creates cookies,
%%%    secrets) and returns a response that records the `committer' of the
%%%    user's 'access secret': the hash of the secret, from which the `keyid'
%%%    that grants them the ability to use the device's 'hidden' secret in the
%%%    future is derived.
%%% 4. This device stores both the secret and the initialized `access-control'
%%%    message, as well as its other metadata.
%%% 5. This device returns the initialized `access-control' message with the
%%%    secret's `keyid' added to the `body' field.
%%% 
%%% During secret operations (commit, export, etc.):
%%% 1. This device retrieves the stored `access-control' message for the
%%%    secret either from persistent storage or from the node message's private
%%%    element. The keyid of the `access secret' is either provided by the 
%%%    user in the request, or is determined from a provided `secret' parameter
%%%    in the request.
%%% 2. This device calls the `access-control' message with path `verify' and
%%%    the user's request.
%%% 3. The `access-control' message verifies the request (e.g., checks cookies,
%%%    provided authentication credentials, etc.).
%%% 4. If verification passes, the device performs the requested operation.
%%% 5. If verification fails, a 400 error is returned.
%%% 
%%% # Access Control Message Requirements
%%% 
%%% Access control messages are fully customizable by callers, but must support
%%% two paths:
%%% 
%%% `/commit': Called during secret generation to bind the `access-control'
%%%            template message to the user's access secret.
%%%  - Input:  The caller's request message, optionally carrying the `secret'
%%%            to bind.
%%%  - Output: Response message with authentication setup (cookies, tokens,
%%%            etc.) and the `committer' of the access secret recorded as a
%%%            key. The node stores this message and uses it as the `Base'
%%%            message for the `verify' path, so it must carry only
%%%            commitments that the node can verify itself: an HMAC under the
%%%            user's secret is not one of them.
%%% 
%%% `/verify': Called before allowing an operation that requires access to a
%%%            secret to proceed.
%%%   - Base:    The initialized `access-control' message from the `commit' path.
%%%   - Request: Caller's request message with authentication credentials.
%%%   - Output:  `false' if an error has occurred. If the request is valid, the
%%%            `access-control' message should return either `true' or a modification
%%%            of the request message which will be used for any subsequent
%%%            operations.
%%% 
%%% The default `access-control' message is `~cookie@1.0', which uses HTTP
%%% cookies with secrets to authenticate users.
%%% 
%%% # Secret Generation Parameters
%%% 
%%% The following parameters are supported by the `generate' key:
%%% 
%%% ```
%%% /generate
%%%     - `access-control' (optional): The `access-control' message to use.
%%%                  Defaults to `#{<<"device">> => <<"cookie@1.0">>}'.
%%%     - `keyid' (optional): The `keyid' of the secret to generate. If not
%%%                  provided, the secret's address will be used as the name.
%%%     - `persist' (optional): How the node should persist the secret. Options:
%%%       - `client': The secret is generated on the server, but not persisted.
%%%                  The full secret key is returned for the user to store.
%%%       - `in-memory': The wallet is generated on the server and persisted only
%%%                  in local memory, never written to disk.
%%%       - `non-volatile': The wallet is persisted to non-volatile storage on 
%%%                  the node. The store used by this option is segmented from
%%%                  the node's main storage, configurable via the `priv_store'
%%%                  node message option.
%%%     - `controllers' (optional): A list of controllers that may access the
%%%                  secret. Defaults to the node's `wallet_admin' option if set,
%%%                  or its operator address if not.
%%%     - `required-controllers' (optional): The number of controllers that must
%%%                  sign the secret for it to be valid. Defaults to `1'.
%%%     
%%%     The response will contain authentication setup (such as cookies) from the
%%%     `access-control' message, plus the secret's `keyid' in the `body' field.
%%%     The secret's key is not returned to the user unless the `persist' option
%%%     is set to `client'. If it is, the `~cookie@1.0' device will be employed
%%%     to set the user's cookie with the secret.
%%% 
%%% /import
%%%     Parameters:
%%%     - `key' (optional): The JSON-encoded secret to import.
%%%     - `cookie' (optional): A structured-fields cookie containing a map with
%%%       a `key' field which is a JSON-encoded secret.
%%%     - `access-control' (optional): The `access-control' message to use.
%%%     - `persist' (optional): How the node should persist the secret. The
%%%       supported options are as with the `generate' key.
%%% 
%%%     Imports a secret for hosting from the user. Executes as `generate' does,
%%%     except that it expects the key to store to be provided either directly
%%%     via the `key' parameter as a `keyid' field in the cookie Structured-Fields
%%%     map. Support for loading the key from the cookie is provided such that
%%%     an existing secret of the user can have its persistence mode
%%%     changed.
%%% 
%%% /list
%%%     Parameters:
%%%     - `keyids' (optional): A list of `keyid's to list. If not provided,
%%%       all secrets will be listed via the `keyid' that is must be provided
%%%       in order to access them.
%%% 
%%%     Lists all hosted secrets on the node by the `keyid' that is used to
%%%     access them. If `keyids' is provided, only the secrets with those
%%%     `keyid's will be listed.
%%% 
%%% /commit
%%%     Parameters:
%%%     - `keyid' (optional): The `keyid' of the secret to commit with.
%%%     - Authentication credentials as required by the `access-control' message.
%%% 
%%%     Commits the given message using the specified secret after authentication.
%%%     If no `keyid' parameter is provided, the request's authentication data
%%%     (such as cookies) must contain secret identification.
%%% 
%%% /export
%%%     Parameters:
%%%     - `keyids' (optional): A list of `keyid's to export, or `all' to
%%%       export all secrets for which the request passes authentication. A
%%%       controller that authorizes the export must sign `keyids'. To export
%%%       every secret that it controls, it signs `keyids => all'.
%%% 
%%%     Exports a given secret or set of secrets. If multiple secrets are
%%%     requested, the result is a message with form `keyid => #{ `key` =>
%%%     JSON-encoded secret, `access-control' => `access-control' message,
%%%     `controllers' => [address, ...], `required-controllers' => integer,
%%%     `persist' => `client' | `in-memory' | `non-volatile' }'.
%%% 
%%%     A secret will be exported if:
%%%     - The given request passes each requested secret's `access-control'
%%%       message; or
%%%     - The secret's controllers signed the request's `keyids', and `keyids'
%%%       contains the secret's `keyid' or is `all'.
%%% 
%%% /sync
%%%     Parameters:
%%%     - `node': The peer node to pull secrets from.
%%%     - `as' (optional): The identity it should use when signing its request
%%%       to the remote peer.
%%%     - `keyids' (optional): A list of `keyid's to export, or `all' to load
%%%       every available secret. Defaults to `all'.
%%% 
%%%     Attempts to download all (or a given subset of) secrets from the given
%%%     node and import them. If the `keyids' parameter is provided, only the
%%%     secrets with those `keyid's will be imported. The `as' parameter is
%%%     used to inform the node which key it should use to sign its request to
%%%     the remote peer, such that its request validates against the secret's
%%%     `access-control' messages on the remote peer.
%%% 
%%%     The request must be signed by the node's `wallet_admin', or by its
%%%     operator address if no `wallet_admin' is set. The response lists the
%%%     `keyid's of the imported secrets.
%%% '''
-module(dev_secret).
-device_libraries([lib_meta, lib_process, lib_secret]).
-export([generate/3, import/3, list/3, commit/3, export/3, sync/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

-define(DEFAULT_AUTH_DEVICE, <<"cookie@1.0">>).

%% @doc Generate a new wallet for a user and register it on the node. If the
%% request references a wallet that it may access, we return the wallet's
%% `access-control' message instead, as for a new wallet.
-spec generate(#{ _ => _ }, #{ committer => binary(), _ => _ }, #{ _ => _ }) ->
    {ok, #{ body := binary(), _ => _ }} | {error, _}.
generate(Base, Request, Opts) ->
    case request_to_wallets(Base, Request, Opts) of
        [] ->
            % No wallets found, create a new one from the pool. The pool
            % bounds concurrent generation; a timed-out request must not
            % generate a wallet in the request process.
            case ar_wallet:request_pooled_wallet(?DEFAULT_KEY_TYPE) of
                {ok, Wallet} ->
                    register_wallet(Wallet, Base, Request, Opts);
                timeout ->
                    {error, <<"Wallet generation timed out.">>}
            end;
        [WalletDetails] ->
            % The wallet is keyed by the `keyid' of its access secret.
            Committer =
                hb_maps:get(<<"committer">>, WalletDetails, undefined, Opts),
            ?event({priv_details, WalletDetails}),
            % Wallet found: reply with its `access-control' message.
            {
                ok,
                (hb_maps:get(<<"access-control">>, WalletDetails, #{}, Opts))#{
                    <<"cache-control">> => <<"no-store">>,
                    <<"body">> =>
                        <<"secret:", Committer/binary>>
                }
            }
    end.

%% @doc Import a wallet for hosting on the node. Expects the keys to be either
%% provided as a list of keys, or a single key in the `key' field. If neither
%% are provided, the keys are extracted from the cookie.
-spec import(
    #{ _ => _ },
    #{ key => [binary()] | binary(), _ => _ },
    #{ _ => _ }
) -> {ok, #{ _ => _ }} | {error, _}.
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

%% @doc Register a series of wallets, returning a summary message with the
%% list of imported wallets, as well as merged cookies.
import_wallets(Wallets, Base, Request, Opts) ->
    Res =
        lists:foldl(
            fun(Wallet, Acc) ->
                case register_wallet(Wallet, Base, Request, Opts) of
                    {ok, RegRes} ->
                        % Merge the private element of the registration response
                        % into the accumulator.
                        WalletAddress = hb_maps:get(<<"wallet-address">>, RegRes, Opts),
                        OldImported = hb_maps:get(<<"imported">>, Acc, [], Opts),
                        Merged =
                            hb_private:merge(
                                Acc,
                                RegRes,
                                Opts
                            ),
                        Merged#{
                            <<"imported">> => [ WalletAddress | OldImported ]
                        };
                    {error, _} -> Acc
                end
            end,
            #{},
            Wallets
        ),
    {ok,
        Res#{
            <<"body">> =>
                addresses_to_binary(hb_maps:get(<<"imported">>, Res, [], Opts))
        }
    }.

%% @doc Transform a wallet key serialized form into a wallet.
wallet_from_key(Key) when is_binary(Key) ->
    ar_wallet:from_json(Key);
wallet_from_key(Key) ->
    Key.

%% @doc Register a wallet on the node.
register_wallet(Wallet, Base, Request, Opts) ->
    % Find the wallet's address.
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    % Determine how to persist the wallet.
    PersistMode = hb_ao:get(<<"persist">>, Request, <<"in-memory">>, Opts),
    % Get the authentication message from the request. If the message is a path
    % or a message with a `path' field, we resolve it to get the base.
    {ok, BaseAccessControl} =
        case hb_ao:get(<<"access-control">>, Base, undefined, Opts) of
            undefined ->
                ?event(
                    debug_auth,
                    {defaulting_access_control, {base, Base}, {request, Request}}
                ),
                {ok, #{ <<"device">> => ?DEFAULT_AUTH_DEVICE }};
            AuthPath when is_binary(AuthPath) ->
                hb_ao:resolve(AuthPath, Opts);
            Msg ->
                case hb_maps:is_key(<<"path">>, Msg, Opts) of
                    true -> hb_ao:resolve(Msg, Opts);
                    false -> {ok, Msg}
                end
        end,
    AccessControl =
        BaseAccessControl#{
            <<"wallet-address">> => hb_util:human_id(Address)
        },
    Controllers =
        hb_ao:get(<<"controllers">>, Request, default, Opts),
    RequiredControllers =
        hb_util:int(hb_ao:get(<<"required-controllers">>, Request, 1, Opts)),
    % Call authentication device to set up auth. Pass the wallet address as the
    % nonce. Some auth devices may use the nonce to track the messages that
    % they have committed.
    AuthRequest =
        case hb_ao:get(<<"secret">>, Base, undefined, Opts) of
            undefined ->
                Request#{
                    <<"path">> => <<"commit">>
                };
            Secret ->
                Request#{
                    <<"path">> => <<"commit">>,
                    <<"secret">> => Secret
                }
        end,
    ?event({register_wallet, {access_control, AccessControl}, {priv_request, AuthRequest}}),
    case hb_ao:resolve(AccessControl, AuthRequest, Opts) of
        {ok, InitializedAuthMsg} ->
            ?event({register_wallet_success, {initialized_auth_msg, InitializedAuthMsg}}),
            % Find the committer of the access secret.
            {ok, Committer} =
                hb_maps:find(<<"committer">>, InitializedAuthMsg, Opts),
            % Store wallet details.
            WalletDetails =
                #{
                    <<"wallet">> => ar_wallet:to_json(Wallet),
                    <<"address">> => hb_util:human_id(Address),
                    <<"persist">> => PersistMode,
                    <<"access-control">> => hb_private:reset(InitializedAuthMsg),
                    <<"committer">> => Committer,
                    <<"controllers">> => parse_controllers(Controllers, Opts),
                    <<"required-controllers">> => RequiredControllers
                },
            persist_registered_wallet(WalletDetails, InitializedAuthMsg, Opts);
        {error, Reason} ->
            ?event({register_wallet_error, {reason, Reason}}),
            {error, Reason}
    end.

%% @doc Persist a wallet and return the auth response. Optionally takes a
%% response base that is used as the message to build upon for the eventual
%% user-response.
persist_registered_wallet(WalletDetails, Opts) ->
    persist_registered_wallet(WalletDetails, #{}, Opts).
persist_registered_wallet(WalletDetails, RespBase, Opts) ->
    % Add the wallet address as the body of the response.
    Address = hb_maps:get(<<"address">>, WalletDetails, undefined, Opts),
    ?event({resp_base, {auth_resp, RespBase}, {priv_wallet_details, WalletDetails}}),
    % The wallet is keyed by the `keyid' of its access secret.
    Committer = hb_maps:get(<<"committer">>, WalletDetails, undefined, Opts),
    KeyID = <<"secret:", Committer/binary>>,
    Base = RespBase#{
        <<"body">> => KeyID, <<"cache-control">> => <<"no-store">>
    },
    % Determine how to persist the wallet.
    case hb_maps:get(<<"persist">>, WalletDetails, <<"in-memory">>, Opts) of
        <<"client">> ->
            ?event({priv_wallet_details, WalletDetails}),
            % Find the necessary wallet details to set the cookie on the client.
            JSONKey = hb_maps:get(<<"wallet">>, WalletDetails, undefined, Opts),
            % Don't store, set the cookie in the response. The key is a
            % credential: page scripts may not read its cookie, and browsers
            % send it only over HTTPS, from this site's pages.
            hb_ao:resolve(
                Base#{ <<"device">> => <<"cookie@1.0">> },
                #{
                    <<"path">> => <<"store">>,
                    <<"wallet-", Address/binary>> =>
                        #{
                            <<"value">> => hb_escape:encode_quotes(JSONKey),
                            <<"attributes">> =>
                                #{
                                    <<"path">> => <<"/">>,
                                    <<"samesite">> => <<"strict">>
                                },
                            <<"flags">> => [<<"httponly">>, <<"secure">>]
                        }
                },
                Opts
            );
        PersistMode ->
            case find_wallet(KeyID, Opts) of
                not_found ->
                    % Store wallet and return auth response with wallet info.
                    store_wallet(
                        hb_util:key_to_atom(PersistMode, existing),
                        KeyID,
                        WalletDetails,
                        Opts
                    ),
                    ?event(
                        {stored_and_returning,
                            {auth_response, Base},
                            {priv_wallet_details, WalletDetails}
                        }
                    ),
                    % Return auth response with wallet info added.
                    {ok, Base};
                _ ->
                    ?event(
                        {register_wallet_error,
                            {reason, wallet_keyid_already_hosted}
                        }
                    ),
                    {error, <<"Wallet keyid already hosted on node.">>}
            end
    end.

%% @doc List all hosted wallets
-spec list(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) -> {ok, [_]}.
list(_Base, _Request, Opts) ->
    {ok, list_wallets(Opts)}.

%% @doc Sign a message with a wallet.
-spec commit(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, #{ _ => _ }} | {error, binary()}.
commit(Base, Request, Opts) ->
    ?event({commit_invoked, {priv_base, Base}, {priv_request, Request}}),
    case request_to_wallets(Base, Request, Opts) of
        [] -> {error, <<"No wallets found to sign with.">>};
        WalletDetailsList ->
            ?event(
                {commit_signing,
                    {priv_request, Request},
                    {priv_wallet_list, WalletDetailsList}
                }
            ),
            {
                ok,
                lists:foldl(
                    fun(WalletDetails, Acc) ->
                        ?event(
                            {invoking_commit_message,
                                {priv_message, Acc},
                                {priv_wallet, WalletDetails}
                            }
                        ),
                        commit_message(Acc, WalletDetails, Opts)
                    end,
                    Base,
                    WalletDetailsList
                )
            }
    end.

%% @doc Take a request and return the wallets it references. Performs validation
%% of access rights for the wallets before returning them.
request_to_wallets(Base, Request, Opts) ->
    % Get the wallet references or keys from the request or cookie.
    ?event({request_to_wallets, {priv_base, Base}, {priv_request, Request}}),
    Keys =
        hb_ao:get_first(
            [
                {Request, <<"secret">>},
                {Base, <<"secret">>}
            ],
            <<"all">>,
            Opts
        ),
    ?event({request_to_wallets, {priv_keys, Keys}}),
    WalletKeyIDs =
        case hb_maps:get(<<"keyids">>, Request, not_found, Opts) of
            not_found ->
                case Keys of
                    <<"all">> ->
                            % Get the wallet name from the cookie.
                        wallets_from_cookie(Request, Opts);
                    _ -> secrets_to_keyids(Keys)
                end;
        KeyIDs -> lists:map(fun(KeyID) ->
                    Wallet = find_wallet(KeyID, Opts),
                    {secret, KeyID, hb_maps:get(<<"wallet">>, Wallet, Opts) }
                end, KeyIDs)
    end,
    ?event({attempting_to_load_wallets, {priv_keyids, WalletKeyIDs}, {priv_request, Request}}),
    lists:filtermap(
        fun(WalletKeyID) ->
            case load_and_verify(WalletKeyID, Base, Request, Opts) of
                {ok, WalletDetails} ->
                    ?event({request_to_wallets, {priv_loaded_wallet, WalletDetails}}),
                    {true, WalletDetails};
                {error, Reason} ->
                    ?event(
                        {failed_to_load_wallet,
                            {priv_keyid, WalletKeyID},
                            {reason, Reason}
                        }
                    ),
                    false
            end
        end,
        WalletKeyIDs
    ).

%% @doc Load a wallet from a keyid and verify we have the authority to access it.
load_and_verify({wallet, WalletKey}, _Base, _Request, _Opts) ->
    % Return the wallet key.
    Wallet = ar_wallet:from_json(WalletKey),
    {ok, #{
        <<"wallet">> => WalletKey,
        <<"address">> => hb_util:human_id(ar_wallet:to_address(Wallet)),
        <<"persist">> => <<"client">>,
        <<"committer">> => hb_opts:committer(#{ <<"priv-wallet">> => Wallet })
    }};
load_and_verify({secret, KeyID, _}, _Base, Request, Opts) ->
    % Get the wallet from the node's options.
    case find_wallet(KeyID, Opts) of
        not_found -> {error, <<"Wallet not hosted on node.">>};
        WalletDetails ->
            case verify_controllers(WalletDetails, KeyID, Request, Opts) of
                true ->
                    % If the request is already signed by an exporter
                    % return the request as-is with the wallet.
                    {ok, WalletDetails};
                false ->
                    case verify_auth(WalletDetails, Request, Opts) of
                        {ok, true} ->
                            {ok, WalletDetails};
                        {ok, false} ->
                            {error, <<"Verification failed.">>};
                        {error, Reason} ->
                            {error, Reason}
                    end
            end
    end.

%% @doc Validate if a calling message has the required `controllers' for the
%% given wallet. Count a controller only if it signed `keyids', and `keyids'
%% contains the wallet's `keyid' or is `all'. A commitment over other keys,
%% such as `hashpath', does not authorize an export.
verify_controllers(WalletDetails, KeyID, Request, Opts) ->
    RequiredControllers =
        hb_util:int(hb_maps:get(<<"required-controllers">>, WalletDetails, 1, Opts)),
    Controllers =
        [
            hb_committer:uri(Controller, Opts)
        ||
            Controller <-
                parse_controllers(
                    hb_maps:get(<<"controllers">>, WalletDetails, [], Opts),
                    Opts
                )
        ],
    SigningControllers =
        [
            Controller
        ||
            Controller <- lists:usort(Controllers),
            {ok, Signed} <- [lib_meta:authorized([Controller], Request, Opts)],
            is_listed_key(KeyID, hb_maps:get(<<"keyids">>, Signed, [], Opts))
        ],
    length(SigningControllers) >= RequiredControllers.

%% @doc Whether a `keyids' value contains the given `keyid', or is `all'.
is_listed_key(_KeyID, <<"all">>) -> true;
is_listed_key(KeyID, KeyIDs) when is_list(KeyIDs) -> lists:member(KeyID, KeyIDs);
is_listed_key(_KeyID, _KeyIDs) -> false.

%% @doc Verify a wallet for a given request.
verify_auth(WalletDetails, Req, Opts) ->
    AuthBase = hb_maps:get(<<"access-control">>, WalletDetails, #{}, Opts),
    AuthRequest = Req#{ <<"path">> => <<"verify">> },
    ?event({verify_wallet, {auth_base, AuthBase}, {priv_request, AuthRequest}}),
    hb_ao:resolve(AuthBase, AuthRequest, Opts).

%% @doc Parse cookie from a message to extract wallets.
wallets_from_cookie(Msg, Opts) ->
    % Parse the cookie as a Structured-Fields map.
    ParsedCookie =
        try hb_ao:resolve(
            Msg#{ <<"device">> => <<"cookie@1.0">> },
            #{ <<"path">> => <<"extract">>, <<"format">> => <<"cookie">> },
            Opts
        ) of
            {ok, CookieMsg} -> CookieMsg
        catch _:_ -> {error, <<"Invalid cookie format.">>}
        end,
        ?event({parsed_cookie, {priv_cookie, ParsedCookie}}),
    % Get the wallets that we should be able to access from the parsed cookie.
    % We determine their type from the `type-' prefix of the key.
    lists:flatten(lists:filtermap(
        fun({<<"secret-", Address/binary >>, Key}) ->
            DecodedKey = hb_escape:decode_quotes(Key),
            ?event({wallet_from_cookie, {priv_key, DecodedKey},{ address, Address}}),
            {true, secrets_to_keyids(DecodedKey)};
           ({<<"wallet-", Address/binary >>, Key}) ->
            DecodedKey = hb_escape:decode_quotes(Key),
            ?event({wallet_from_cookie, {priv_key, DecodedKey}, {address, Address}}),
            {true, [{wallet, DecodedKey}]};
           ({_Irrelevant, _}) -> false
        end,
        hb_maps:to_list(ParsedCookie, Opts)
    )).

%% @doc Sign a message using hb_message:commit, taking either a wallet as a 
%% JSON-encoded string or a wallet details message with a `key' field.
commit_message(Message, NonMap, Opts) when not is_map(NonMap) ->
    commit_message(Message, #{ <<"wallet">> => NonMap }, Opts);
commit_message(Message, #{ <<"wallet">> := Key }, Opts) when is_binary(Key) ->
    commit_message(Message, #{ <<"wallet">> => ar_wallet:from_json(Key) }, Opts);
commit_message(Message, #{ <<"wallet">> := Key }, Opts) ->
    ?event({committing_with_proxy, {priv_message, Message}, {priv_wallet, Key}}),
    hb_message:commit(Message, Opts#{ <<"priv-wallet">> => Key }).

%% @doc Export wallets from a request. The request should contain a source of
%% wallets (cookies, keys, or wallet names), or a specific list/name of a
%% wallet to authenticate and export.
-spec export(
    #{ _ => _ },
    #{ keyids => [binary()] | binary(), _ => _ },
    #{ _ => _ }
) -> {ok, [_]} | {error, binary()}.
export(Base, Request, Opts) ->
    % A reply that is not a bundle gives each wallet as a link, which writes
    % the wallet, with its key, to the node's cache.
    case hb_util:bool(hb_ao:get(<<"accept-bundle">>, Request, false, Opts)) of
        true -> do_export(Base, Request, Opts);
        false -> {error, <<"An export is sent only as a bundle.">>}
    end.

do_export(Base, Request, Opts) ->
    PrivOpts = priv_store_opts(Opts),
    ModReq =
        case hb_ao:get(<<"keyids">>, Request, not_found, Opts) of
            <<"all">> ->
                AllLocalWallets = list_wallets(Opts),
                Request#{ <<"keyids">> => AllLocalWallets };
            _ -> Request
        end,
    ?event({export, {priv_base, Base}, {priv_request, ModReq}}),
    case request_to_wallets(Base, ModReq, Opts) of
        [] -> {error, <<"No wallets found to export.">>};
        Wallets ->
            {
                ok,
                lists:map(
                    fun(Wallet) ->
                        Loaded = hb_cache:ensure_all_loaded(Wallet, PrivOpts),
                        ?event({exported, {priv_wallet, Loaded}}),
                        Loaded#{ <<"cache-control">> => <<"no-store">> }
                    end,
                    Wallets
                )
            }
    end.    

%% @doc Sync wallets from a remote node. Returns the `keyid's of the imported
%% wallets.
-spec sync(
    #{ _ => _ },
    #{
        node => binary(),
        as => binary(),
        keyids => [binary()] | binary(),
        _ => _
    },
    #{ _ => _ }
) -> {ok, [binary()]} | {error, _}.
sync(_Base, Request, Opts) ->
    maybe
        % Read the node, the identity to sign as, and the keyids only from the
        % keys that the node's controllers signed: its `wallet_admin', or its
        % operator address if no `wallet_admin' is set.
        Controllers = parse_controllers(default, Opts),
        {ok, Signed} ?= lib_meta:authorized(Controllers, Request, Opts),
        Node = hb_ao:get(<<"node">>, Signed, undefined, Opts),
        true ?= Node =/= undefined orelse {error, <<"Node not specified.">>},
        Wallets = hb_maps:get(<<"keyids">>, Signed, <<"all">>, Opts),
        {ok, SignAsOpts} ?=
            case hb_ao:get(<<"as">>, Signed, undefined, Opts) of
                undefined -> {ok, Opts};
                SignAs -> hb_opts:as(SignAs, Opts)
            end,
        ExportRequest =
            (hb_message:commit(
                #{ <<"keyids">> => Wallets },
                SignAsOpts
            ))#{
                <<"path">> => <<"/~secret@1.0/export">>,
                <<"accept-bundle">> => <<"true">>
            },
        ?event({sync, {export_req, ExportRequest}}),
        case hb_http:get(Node, ExportRequest, SignAsOpts) of
            {ok, ExportResponse} ->
                ExportedWallets = export_response_to_list(ExportResponse, #{}),
                ?event({sync, {priv_received_wallets, ExportedWallets}}),
                % Import each wallet. Ignore wallet imports that fail.
                {
                    ok,
                    lists:filtermap(
                        fun(Wallet) ->
                            ?event({sync, {importing, {priv_wallet, Wallet}}}),
                            case persist_registered_wallet(Wallet, Opts) of
                                {ok, #{ <<"body">> := KeyID }} ->
                                    ?event({sync, {imported, KeyID}}),
                                    {true, KeyID};
                                {error, Reason} ->
                                    ?event({sync, {process_import_error, Reason}}),
                                    false
                            end
                        end,
                        ExportedWallets
                    )
                };
            {error, Reason} ->
                ?event({sync, {error, Reason}}),
                {error, Reason}
        end
    end.

%%% Helper functions

%% @doc Convert a key to a wallet reference.
secrets_to_keyids(Secrets) when is_list(Secrets) ->
    [ hd(secrets_to_keyids(Secret)) || Secret <- Secrets ];
secrets_to_keyids(Secret) when is_binary(Secret) ->
    ?event({secrets_to_keyids, {priv_secret, Secret}}),
    KeyID = lib_secret:to_committer(Secret),
    [ {secret, <<"secret:", KeyID/binary>>, Secret} ].

%% @doc Parse the exportable setting for a wallet and return a list of the
%% committers which are allowed to export the wallet.
parse_controllers(default, Opts) ->
    case hb_opts:get(wallet_admin, undefined, Opts) of
        undefined -> 
            case hb_opts:get(operator, undefined, Opts) of
                undefined ->
                    [hb_opts:committer(Opts)];
                Op -> [Op]
            end;
        Admin -> [Admin]
    end;
parse_controllers(true, Opts) -> parse_controllers(default, Opts);
parse_controllers(false, _Opts) -> [];
parse_controllers(Addresses, _Opts) when is_list(Addresses) -> Addresses;
parse_controllers(Address, _Opts) when is_binary(Address) -> [Address].

%% @doc Store a wallet in the appropriate location.
store_wallet(in_memory, KeyID, Details, Opts) ->
    hb_http_server:update_opts(
        fun(NodeMsg) ->
            % Get existing wallets from the node message.
            CurrentWallets = hb_opts:get(priv_wallet_hosted, #{}, NodeMsg),
            % Add new wallet
            UpdatedWallets = CurrentWallets#{ KeyID => Details },
            ?event({wallet_store, {priv_updated_wallets, UpdatedWallets}}),
            % Update the node's options with the new wallets.
            hb_http_server:set_opts(
                NodeMsg#{ <<"priv-wallet-hosted">> => UpdatedWallets }
            )
        end,
        Opts
    ),
    ok;
store_wallet(non_volatile, KeyID, Details, Opts) ->
    % Find the private store of the node.
    PrivOpts = priv_store_opts(Opts),
    {ok, Msg} = hb_cache:write(#{ KeyID => Details }, PrivOpts),
    % Link the wallet to the store.
    ok = hb_cache:link(Msg, <<"wallet@1.0/", KeyID/binary>>, PrivOpts).

%% @doc Find the wallet by name or address in the node's options.
find_wallet(KeyID, Opts) ->
    case find_wallet(in_memory, KeyID, Opts) of
        not_found -> find_wallet(non_volatile, KeyID, Opts);
        Wallet -> Wallet
    end.

%% @doc Loop over the wallets and find the reference to the wallet.
find_wallet(in_memory, KeyID, Opts) ->
    Wallets = hb_opts:get(priv_wallet_hosted, #{}, Opts),
    ?event({find_wallet, {keyid, KeyID}, {priv_wallets, Wallets}}),
    case hb_maps:find(KeyID, Wallets, Opts) of
        {ok, Wallet} -> Wallet;
        error -> not_found
    end;
find_wallet(non_volatile, KeyID, Opts) ->
    PrivOpts = priv_store_opts(Opts),
    case hb_cache:read(<<"wallet@1.0/", KeyID/binary>>, PrivOpts) of
        {ok, Wallet} ->
            WalletDetails = hb_maps:get(KeyID, Wallet, not_found, PrivOpts),
            hb_cache:ensure_all_loaded(WalletDetails, PrivOpts);
        _ ->
            not_found
    end.

%% @doc Generate a list of all hosted wallets.
list_wallets(Opts) ->
    list_wallets(in_memory, Opts) ++ list_wallets(non_volatile, Opts).
list_wallets(in_memory, Opts) ->
    hb_maps:keys(hb_opts:get(priv_wallet_hosted, #{}, Opts));
list_wallets(non_volatile, Opts) ->
    PrivOpts = priv_store_opts(Opts),
    hb_cache:ensure_all_loaded(hb_cache:list(<<"wallet@1.0/">>, PrivOpts), PrivOpts).

%% @doc Generate a new `Opts' message with the `priv_store' as the only `store'
%% option.
priv_store_opts(Opts) ->
    (hb_private:opts(Opts))#{
        <<"store">> => hb_opts:get(priv_store, [], Opts)
    }.

%% @doc Convert an export response into a list of wallet details. This is
%% necessary because if a received result over HTTP is a list with a 
%% commitment attached, it will result in a message with numbered keys but
%% also additional keys for the commitment etc.
export_response_to_list(ExportResponse, Opts) ->
    hb_util:numbered_keys_to_list(ExportResponse, Opts).

%% @doc Convert a list of addresses to a binary string. If the input is a
%% binary already, it is returned as-is.
addresses_to_binary(Addresses) when is_list(Addresses) ->
    hb_util:bin(string:join(
        lists:map(fun hb_util:list/1, Addresses),
        ", "
    ));
addresses_to_binary(Address) when is_binary(Address) ->
    Address.

%% @doc Convert a binary string to a list of addresses. If the input is a
%% list already, it is returned as-is.
binary_to_addresses(AddressesBin) when is_binary(AddressesBin) ->
    binary:split(AddressesBin, <<",">>, [global]);
binary_to_addresses(Addresses) when is_list(Addresses) ->
    Addresses.


%%% Tests

%% @doc Helper function to test wallet generation and verification flow.
test_wallet_generate_and_verify(GeneratePath, ExpectedName, CommitParams) ->
    Node = hb_http_server:start_node(#{
        <<"priv-wallet">> => ar_wallet:new()
    }),
    NodeMsg = hb_http_server:get_opts(),
    % Generate wallet with specified parameters
    {ok, GenResponse} = hb_http:get(Node, GeneratePath, #{}),
    % Only `priv-wallet-hosted' changes in the node message.
    ?assertEqual(
        NodeMsg,
        maps:without([<<"priv-wallet-hosted">>], hb_http_server:get_opts())
    ),
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
    % Generating again with the cookie does not return the wallet.
    {ok, Regenerated} =
        hb_http:get(
            Node,
            #{ <<"path">> => GeneratePath, <<"priv">> => Priv },
            #{}
        ),
    ?assertNot(maps:is_key(<<"wallet">>, Regenerated)),
    % Now verify by signing a message
    TestMessage =
        maps:merge(
            #{
                <<"device">> => <<"secret@1.0">>,
                <<"path">> => <<"//commit">>,
                <<"body">> => <<"Test message">>,
                <<"priv">> => Priv
            },
            CommitParams
        ),
    ?event({signing_with_cookie, {test_message, TestMessage}}),
    {ok, SignedMessage} = hb_http:post(Node, TestMessage, #{}),
    % HTTP replies should preserve secret-wallet commitments and add hashpath-only
    % transport commitments.
    hb_test_utils:assert_hashpath_reply(
        SignedMessage,
        hb_committer:uri(WalletAddr),
        <<"Test message">>
    ).

client_persist_generate_and_verify_test() ->
    test_wallet_generate_and_verify(
        <<"/~secret@1.0/generate?persist=client">>,
        undefined,
        #{}
    ).

cookie_wallet_generate_and_verify_test() ->
    test_wallet_generate_and_verify(
        <<"/~secret@1.0/generate?persist=in-memory">>,
        undefined,
        #{}
    ).

non_volatile_persist_generate_and_verify_test() ->
    test_wallet_generate_and_verify(
        <<"/~secret@1.0/generate?persist=non-volatile">>,
        undefined,
        #{}
    ),
    % A hosted wallet is written to and read from the private store only.
    Public = #{ <<"store-module">> => hb_store_fs, <<"name">> => <<"pub">> },
    Private = #{ <<"store-module">> => hb_store_fs, <<"name">> => <<"priv">> },
    Opts =
        priv_store_opts(
            #{ <<"store">> => [Public], <<"priv-store">> => [Private] }
        ),
    ?assertEqual([Private], hb_opts:get(store, [], Opts)).

import_wallet_with_key_test() ->
    Node = hb_http_server:start_node(#{
        <<"priv-wallet">> => ar_wallet:new()
    }),
    % Create a test wallet key to import (in real scenario from user).
    TestWallet = ar_wallet:new(),
    % WalletAddress = hb_util:human_id(TestWallet),
    WalletKey = hb_escape:encode_quotes(ar_wallet:to_json(TestWallet)),
    WalletAddress = hb_util:human_id(ar_wallet:to_address(TestWallet)),
    % Import the wallet with a specific name.
    ImportUrl =
        <<"/~secret@1.0/import?wallet=imported-wallet&persist=in-memory&key=",
            WalletKey/binary>>,
    {ok, ImportResponse} = hb_http:get(Node, ImportUrl, #{}),
    ?event({resp, ImportResponse, WalletAddress}),
    Imported = hb_maps:get(<<"imported">>, ImportResponse, #{}),
    % Response should come from auth device with wallet name in body.
    % Wallet name is the address of the wallet.
    ?assertMatch([WalletAddress], Imported),
    % Should include cookie setup from auth device.
    ?assertMatch(#{ <<"priv">> := #{ <<"cookie">> := _ } }, ImportResponse).

list_wallets_test() ->
    Node = hb_http_server:start_node(#{
        <<"priv-wallet">> => ar_wallet:new()
    }),
    % Generate some wallets first.
    {ok, Base} =
        hb_http:get(
            Node,
            <<"/~secret@1.0/generate?persist=in-memory">>,
            #{}
        ),
        ?event({base, Base}),
    {ok, Req} =
        hb_http:get(
            Node,
            <<"/~secret@1.0/generate?persist=in-memory">>,
            #{}
        ),
    WalletAddress1 = maps:get(<<"body">>, Base),
    WalletAddress2 = maps:get(<<"body">>, Req),
    ?assertEqual(WalletAddress1, maps:get(<<"body">>, Base)),
    ?assertEqual(WalletAddress2, maps:get(<<"body">>, Req)),
    % List all wallets (no authentication required for listing).
    {ok, Wallets} = hb_http:get(Node, <<"/~secret@1.0/list">>, #{}),
    % Each wallet entry should be a wallet name.
    ?assert(
        lists:all(
            fun(Wallet) -> lists:member(Wallet, hb_maps:values(Wallets)) end,
            [WalletAddress1, WalletAddress2]
        )
    ).

commit_with_cookie_wallet_test() ->
    Node = hb_http_server:start_node(#{
        <<"priv-wallet">> => ar_wallet:new()
    }),
    % Generate a client wallet to get a cookie with full wallet key.
    {ok, GenResponse} =
        hb_http:get(Node, <<"/~secret@1.0/generate?persist=client">>, #{}),
    WalletName = maps:get(<<"wallet-address">>, GenResponse),
    #{ <<"priv">> := Priv } = GenResponse,
    % Page scripts, other sites and plain HTTP do not get the wallet's cookie.
    ?assertMatch(
        #{
            <<"attributes">> :=
                #{ <<"path">> := <<"/">>, <<"samesite">> := <<"strict">> },
            <<"flags">> := [<<"httponly">>, <<"secure">>]
        },
        hb_maps:get(
            hb_util:lowercase_key(<<"wallet-", WalletName/binary>>),
            maps:get(<<"cookie">>, Priv)
        )
    ),
    % Use the cookie to sign a message (no wallet parameter needed).
    TestMessage = #{
        <<"device">> => <<"secret@1.0">>,
        <<"path">> => <<"//commit">>,
        <<"body">> => <<"Test data">>,
        <<"priv">> => Priv
    },
    {ok, SignedMessage} = hb_http:post(Node, TestMessage, #{}),
    % HTTP replies should preserve secret-wallet commitments and add hashpath-only
    % transport commitments.
    hb_test_utils:assert_hashpath_reply(
        SignedMessage,
        hb_committer:uri(WalletName),
        <<"Test data">>
    ).

export_wallet_test() ->
    Node = hb_http_server:start_node(#{}),
    % Generate a wallet to export.
    {ok, GenResponse} =
        hb_http:get(
            Node,
            <<"/~secret@1.0/generate?persist=in-memory">>,
            #{}
        ),
    #{ <<"priv">> := Priv } = GenResponse,
    WalletAddress = maps:get(<<"wallet-address">>, GenResponse),
    % Export the wallet with authentication.
    {ok, ExportResponse} =
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"/~secret@1.0/export/1">>,
                <<"accept-bundle">> => <<"true">>,
                <<"priv">> => Priv
            },
            #{}
        ),
    ?event({export_test, {export_response, ExportResponse}}),
    % Should return wallet details including key, auth, exportable, persist.
    ?assertMatch(#{<<"wallet">> := _, <<"persist">> := <<"in-memory">>}, ExportResponse),
    % The exported key gives `e' in its fewest octets, as RFC 7518 requires.
    #{ <<"e">> := E } = hb_json:decode(maps:get(<<"wallet">>, ExportResponse)),
    ?assertEqual(<<"AQAB">>, E),
    ?assert(maps:is_key(<<"access-control">>, ExportResponse)),
    ?assert(maps:is_key(<<"controllers">>, ExportResponse)),
    % A reply with the wallet's key has the header `cache-control: no-store'.
    ?assertEqual(<<"no-store">>, maps:get(<<"cache-control">>, ExportResponse)),
    % Should return the correct wallet address in the response.
    ?assertEqual(WalletAddress, maps:get(<<"address">>, ExportResponse)),
    AccessControl = maps:get(<<"access-control">>, ExportResponse),
    ?assertEqual(WalletAddress, maps:get(<<"wallet-address">>, AccessControl)).

%% @doc Ensure that a secret needing two controllers is not exported for a
%% request that one controller signed twice. A wallet that signs twice is one
%% signer, so it does not meet the threshold.
export_requires_distinct_controllers_test() ->
    A = ar_wallet:new(),
    B = ar_wallet:new(),
    Node = hb_http_server:start_node(#{ <<"priv-wallet">> => ar_wallet:new() }),
    {ok, #{ <<"body">> := KeyID }} =
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"/~secret@1.0/generate">>,
                <<"persist">> => <<"in-memory">>,
                <<"controllers">> =>
                    [
                        hb_test_utils:committer(A),
                        hb_test_utils:committer(B)
                    ],
                <<"required-controllers">> => 2
            },
            #{}
        ),
    ExportBody = #{ <<"device">> => <<"secret@1.0">>, <<"keyids">> => [KeyID] },
    Export =
        fun(Signed) ->
            hb_http:get(
                Node,
                Signed#{
                    <<"path">> => <<"/~secret@1.0/export">>,
                    <<"accept-bundle">> => <<"true">>
                },
                #{}
            )
        end,
    % Signed by both controllers, the secret is exported.
    {ok, _} =
        Export(
            hb_message:commit(
                hb_message:commit(ExportBody, #{ <<"priv-wallet">> => A }),
                #{ <<"priv-wallet">> => B }
            )
        ),
    % A request signed twice by one controller has two commitments, but only
    % one signer, so the secret is not exported.
    ?assertMatch(
        {error, _},
        Export(
            hb_message:commit(
                hb_message:commit(ExportBody, #{ <<"priv-wallet">> => A }),
                #{ <<"priv-wallet">> => A },
                #{ <<"device">> => <<"httpsig@1.0">>, <<"bundle">> => true }
            )
        )
    ).

export_non_volatile_wallet_test() ->
        Node = hb_http_server:start_node(#{
            <<"priv-wallet">> => ar_wallet:new()
        }),
        % Generate a wallet to export.
        {ok, GenResponse} =
            hb_http:get(
                Node,
                <<"/~secret@1.0/generate?persist=non-volatile">>,
                #{}
            ),
        #{ <<"priv">> := Priv } = GenResponse,
        % Export the wallet with authentication.
        {ok, ExportResponse} =
            hb_http:get(
                Node,
                #{
                    <<"device">> => <<"secret@1.0">>,
                    <<"path">> => <<"//export/1">>,
                    <<"accept-bundle">> => <<"true">>,
                    <<"priv">> => Priv
                },
                #{}
            ),
        % Should return wallet details including key, auth, exportable, persist.
        ?assertMatch(
            #{<<"wallet">> := _, <<"persist">> := <<"non-volatile">>},
            ExportResponse
        ),
        ?assert(maps:is_key(<<"access-control">>, ExportResponse)),
        ?assert(maps:is_key(<<"controllers">>, ExportResponse)).

export_individual_batch_wallets_test() ->
    Node =
        hb_http_server:start_node(
            AdminOpts =
                #{
                    <<"priv-wallet">> => AdminWallet = ar_wallet:new()
                }
        ),
    % Generate multiple wallets and collect auth cookies.
    {ok, #{ <<"body">> := WalletKeyID1 }} =
        hb_http:get(
            Node,
            <<"/~secret@1.0/generate?persist=in-memory&exportable=",
                (hb:address(AdminWallet))/binary>>,
            #{}
        ),
    {ok, #{ <<"body">> := WalletKeyID2 }} =
        hb_http:get(
            Node,
            <<"/~secret@1.0/generate?persist=in-memory&exportable=",
                (hb:address(AdminWallet))/binary>>,
            #{}
        ),
    % Export all wallets.
    {ok, ExportAllResponse} =
        hb_http:get(
            Node,
            (hb_message:commit(
                #{
                    <<"device">> => <<"secret@1.0">>,
                    <<"keyids">> => [WalletKeyID1, WalletKeyID2]
                },
                AdminOpts
            ))#{
                <<"path">> => <<"/~secret@1.0/export">>,
                <<"accept-bundle">> => <<"true">>
            },
            #{}
        ),

    % Export single wallet by address.
    {ok, ExportWallet1Response} =
    hb_http:get(
        Node,
        (hb_message:commit(
            #{
                <<"device">> => <<"secret@1.0">>,
                <<"keyids">> => [WalletKeyID1]
            },
            AdminOpts
        ))#{
            <<"path">> => <<"/~secret@1.0/export">>,
            <<"accept-bundle">> => <<"true">>
        },
        #{}
    ),
    
    ?assert(is_map(ExportAllResponse)),
    ?assert(is_map(ExportWallet1Response)),
    ExportedAllWallets =
        [
            <<
                "secret:",
                (hb_maps:get(<<"committer">>, Wallet, undefined, #{}))/binary
            >>
        ||
            Wallet <- export_response_to_list(ExportAllResponse, #{})
        ],
    ExportedSingleWallets =
        [
            <<
                "secret:",
                (hb_maps:get(<<"committer">>, Wallet, undefined, #{}))/binary
            >>
        ||
            Wallet <- export_response_to_list(ExportWallet1Response, #{})
        ],
    ?event({exported_wallets, {exported_wallets, ExportedAllWallets}}),
    ?assert(length(ExportedAllWallets) >= 2),
    ?assert(length(ExportedSingleWallets) == 1),
    % Each exported wallet should have the required structure.
    lists:foreach(
        fun(Addr) ->
            ?assert(lists:member(Addr, ExportedAllWallets))
        end,
        [WalletKeyID1, WalletKeyID2]
    ),
    ?assert(lists:member(WalletKeyID1, ExportedSingleWallets)).

            

export_batch_all_wallets_test() ->
    % Remove all previous cached wallets.
    hb_store:reset(hb_opts:get(priv_store, no_wallet_store, #{})),
    Node =
        hb_http_server:start_node(
            AdminOpts =
                #{
                    <<"priv-wallet">> => AdminWallet = ar_wallet:new()
                }
        ),
    % Generate multiple wallets and collect auth cookies.
    {ok, #{ <<"wallet-address">> := WalletAddr1 }} =
        hb_http:get(
            Node,
            <<"/~secret@1.0/generate?persist=in-memory&exportable=",
                (hb:address(AdminWallet))/binary>>,
            #{}
        ),
    {ok, #{ <<"wallet-address">> := WalletAddr2 }} =
        hb_http:get(
            Node,
            <<"/~secret@1.0/generate?persist=in-memory&exportable=",
                (hb:address(AdminWallet))/binary>>,
            #{}
        ),
    % Export all wallets.
    {ok, ExportResponse} =
        hb_http:get(
            Node,
            (hb_message:commit(
                #{
                    <<"device">> => <<"secret@1.0">>,
                    <<"keyids">> => <<"all">>
                },
                AdminOpts
            ))#{
                <<"path">> => <<"/~secret@1.0/export">>,
                <<"accept-bundle">> => <<"true">>
            },
            #{}
        ),
    ?event({export_batch_test, {export_response, ExportResponse}}),
    ?assert(is_map(ExportResponse)),
    ExportedWallets =
        [
            hb_maps:get(<<"address">>, Wallet, undefined, #{})
        ||
            Wallet <- export_response_to_list(ExportResponse, #{})
        ],
    ?event({exported_wallets, {exported_wallets, ExportedWallets}}),
    ?assert(length(ExportedWallets) >= 2),
    % Each exported wallet should have the required structure.
    lists:foreach(
        fun(Addr) ->
            ?assert(lists:member(Addr, ExportedWallets))
        end,
        [WalletAddr1, WalletAddr2]
    ).

sync_wallets_test() ->
    % Remove all previous cached wallets.
    hb_store:reset(hb_opts:get(priv_store, no_wallet_store, #{})),
    % Start a node with the key of the second node's wallet admin as its
    % `admin' identity.
    AdminWallet = ar_wallet:new(),
    Node =
        hb_http_server:start_node(#{
            <<"priv-wallet">> => Node1Wallet = ar_wallet:new(),
            <<"identities">> =>
                #{ <<"admin">> => #{ <<"priv-wallet">> => AdminWallet } }
        }),
    {ok, Address} = hb_http:get(Node, <<"/~meta@1.0/info/address">>, #{}),
    % Start a second node to sync from.
    Node2 =
        hb_http_server:start_node(#{
            <<"priv-wallet">> => ar_wallet:new(),
            <<"wallet-admin">> => hb_test_utils:committer(AdminWallet)
        }),
    % Generate a wallet on the second node.
    {ok, GenResponse} =
        hb_http:get(
            Node2,
            <<"/~secret@1.0/generate?persist=in-memory">>,
            #{}
        ),
    WalletKeyID = maps:get(<<"body">>, GenResponse),
    % A sync request that is not signed by the first node is refused.
    ?assertMatch(
        {error, _},
        hb_http:get(
            Node,
            <<"/~secret@1.0/sync?node=", Node2/binary, "&as=admin">>,
            #{}
        )
    ),
    % Sign the sync requests with the key of the first node.
    Sync =
        fun(As) ->
            hb_http:get(
                Node,
                (hb_message:commit(
                    #{ <<"node">> => Node2, <<"as">> => As },
                    #{ <<"priv-wallet">> => Node1Wallet }
                ))#{ <<"path">> => <<"/~secret@1.0/sync">> },
                #{}
            )
        end,
    % A sync as an identity that the first node does not hold fails.
    ?assertMatch({error, _}, Sync(<<"x">>)),
    % A `node' supplied only as an unsigned query parameter is not acted on:
    % the controller signs `as' alone, so the request has no signed node.
    ?assertMatch(
        {error, _},
        hb_http:get(
            Node,
            (hb_message:commit(
                #{ <<"as">> => <<"admin">> },
                #{ <<"priv-wallet">> => Node1Wallet }
            ))#{ <<"path">> =>
                <<"/~secret@1.0/sync?node=", Node2/binary>> },
            #{}
        )
    ),
    % Test sync to the first node from the second, signed as `admin'. The
    % response contains the `keyid' of the imported wallet, and not its key.
    {ok, SyncResponse} = Sync(<<"admin">>),
    ?assertEqual([WalletKeyID], export_response_to_list(SyncResponse, #{})),
    % The first node keeps its own key.
    ?assertEqual(
        {ok, Address},
        hb_http:get(Node, <<"/~meta@1.0/info/address">>, #{})
    ),
    % Get the wallet list from the first node.
    {ok, WalletList} = hb_http:get(Node, <<"/~secret@1.0/list">>, #{}),
    ?event({sync_wallets_test, {wallet_list, WalletList}}),
    % Should return a map of successfully imported wallets or list of names.
    ?assert(lists:member(WalletKeyID, hb_maps:values(WalletList))).

sync_non_volatile_wallets_test() ->
    % Remove all the previous cached wallets.
    hb_store:reset(hb_opts:get(priv_store, no_wallet_store, #{})),
    Node =
        hb_http_server:start_node(#{
            <<"priv-wallet">> => Node1Wallet = ar_wallet:new()
        }),
    % Start a second node to sync from.
    Node2 =
        hb_http_server:start_node(#{
            <<"priv-wallet">> => ar_wallet:new(),
            <<"wallet-admin">> => hb_test_utils:committer(Node1Wallet)
        }),
    % Generate a wallet on the second node.
    {ok, GenResponse} =
        hb_http:get(
            Node2,
            <<"/~secret@1.0/generate?persist=non-volatile">>,
            #{}
        ),
    WalletName = maps:get(<<"body">>, GenResponse),
    % Test sync to the first node from the second, signed by the first node.
    {ok, _} =
        hb_http:get(
            Node,
            (hb_message:commit(
                #{ <<"node">> => Node2 },
                #{ <<"priv-wallet">> => Node1Wallet }
            ))#{ <<"path">> => <<"/~secret@1.0/sync">> },
            #{}
        ),
    % Get the wallet list from the first node.
    {ok, WalletList} = hb_http:get(Node, <<"/~secret@1.0/list">>, #{}),
    ?event({sync_wallets_test, {wallet_list, WalletList}}),
    % Should return a map of successfully imported wallets or list of names.
    ?assert(lists:member(WalletName, hb_maps:values(WalletList))).

%% @doc A generated wallet's stored access-control message must be verifiable
%% by the node, and only a caller presenting the cookie secret may use the
%% wallet.
stored_access_control_verifies_test() ->
    Node = hb_http_server:start_node(#{
        <<"priv-wallet">> => ar_wallet:new()
    }),
    {ok, GenResponse} =
        hb_http:get(
            Node,
            <<"/~secret@1.0/generate?persist=non-volatile">>,
            #{}
        ),
    #{ <<"body">> := KeyID, <<"priv">> := Priv } = GenResponse,
    WalletAddr = maps:get(<<"wallet-address">>, GenResponse),
    Committer = hb_util:remove_scheme_prefix(KeyID),
    % Generating again with the cookie answers with the same `keyid'.
    {ok, Regenerated} =
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"/~secret@1.0/generate?persist=non-volatile">>,
                <<"priv">> => Priv
            },
            #{}
        ),
    ?assertEqual(KeyID, maps:get(<<"body">>, Regenerated)),
    % The stored access-control message records the committer of the cookie
    % secret and carries no commitment by it.
    {ok, ExportResponse} =
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"/~secret@1.0/export/1">>,
                <<"accept-bundle">> => <<"true">>,
                <<"priv">> => Priv
            },
            #{}
        ),
    AccessControl = maps:get(<<"access-control">>, ExportResponse),
    ?assertEqual(Committer, maps:get(<<"committer">>, AccessControl)),
    ?assertEqual([], hb_message:signers(AccessControl, #{})),
    ?assert(
        hb_message:verify(
            AccessControl,
            #{ <<"ids">> => <<"all">> },
            #{}
        )
    ),
    % A caller presenting a wrong secret under the same cookie name is refused,
    % while the right secret signs.
    {ok, WrongCookie} =
        hb_ao:resolve(
            #{ <<"device">> => <<"cookie@1.0">>, <<"priv">> => Priv },
            #{
                <<"path">> => <<"store">>,
                <<"secret-", Committer/binary>> =>
                    hb_util:encode(crypto:strong_rand_bytes(64))
            },
            #{}
        ),
    SignRequest =
        #{
            <<"device">> => <<"secret@1.0">>,
            <<"path">> => <<"//commit">>,
            <<"keyids">> => [KeyID],
            <<"body">> => <<"Test message">>
        },
    ?assertNotMatch(
        {ok, _},
        hb_http:post(
            Node,
            SignRequest#{ <<"priv">> => maps:get(<<"priv">>, WrongCookie) },
            #{}
        )
    ),
    {ok, SignedMessage} =
        hb_http:post(Node, SignRequest#{ <<"priv">> => Priv }, #{}),
    hb_test_utils:assert_hashpath_reply(
        SignedMessage,
        hb_committer:uri(WalletAddr),
        <<"Test message">>
    ).
