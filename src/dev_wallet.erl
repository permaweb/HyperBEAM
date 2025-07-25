%%% @doc A device that allows a node to manage wallets on behalf of users. Users
%%% are able to create wallets, then have the node sign their messages and
%%% potentially continue execution upon the result. This device is intended for
%%% use in situations in which the node is trusted by the user, for example if
%%% it is running on their own machine or in a TEE-protected environment that 
%%% they deem to be secure.
%%% 
%%% # Authentication Flow
%%% 
%%% Each wallet is associated with an authentication message that controls access.
%%% The authentication system is pluggable -- users can employ any authentication
%%% message as they desire. The default authentication message is empty aside 
%%% the device being set to `cookie@1.0`.
%%% 
%%% During wallet generation:
%%% 1. The wallet device creates the wallet and determines its name (provided by
%%%    the user or derived from the wallet address).
%%% 2. The wallet device calls the authentication message with path `generate'
%%%    and the wallet name in the request.
%%% 3. The authentication device sets up authentication (e.g., creates cookies,
%%%    secrets) and returns a response.
%%% 4. The wallet device stores both the wallet and the authentication result, 
%%%    as well as its other metadata.
%%% 5. The wallet device returns the authentication response with the wallet name
%%%    added to the `body' field.
%%% 
%%% During wallet operations (commit, export, etc.):
%%% 1. The wallet device retrieves the stored authentication message for the
%%%    wallet either from persistent storage or from the node's message.
%%% 2. The wallet device calls the authentication message with path `verify' and
%%%    the user's request.
%%% 3. The authentication device verifies the request (e.g., checks cookies).
%%% 4. If verification passes, the wallet device performs the requested operation
%%% 5. If verification fails, a 400 error is returned.
%%% 
%%% # Authentication Message Requirements
%%% 
%%% Authentication devices must support two paths:
%%% 
%%% `/generate': Called during wallet creation.
%%%  - Input: Request message containing `name' field with wallet's name.
%%%  - Output: Response message with authentication setup (cookies, tokens, etc.).
%%%            This message will be used as the `Base' message for the `verify'
%%%            path.
%%% 
%%% `/verify': Called during wallet operations.
%%%   - Base: The output of the `generate' path.
%%%   - Request: User's request message with authentication credentials.
%%%   - Output: `false' if an error has occurred. If the request is valid, the
%%%            authentication message should return either `true' or a modification
%%%            of the request message which will be used for any subsequent
%%%            operations.
%%% 
%%% The default authentication device `~cookie@1.0' uses HTTP cookies with
%%% secrets to authenticate users.
%%% 
%%% # Wallet Device Keys
%%% 
%%% As well as by direct invocation, this device may be used as a `preprocessor'
%%% hook on the node, allowing it to sign requests for users before they are
%%% executed.
%%% 
%%% The keys provided by the device are as follows:
%%% ```
%%% /generate
%%%     Parameters:
%%%     - `auth' (optional): The authentication message to use. Defaults to
%%%       `#{<<"device">> => <<"cookie@1.0">>}' if not provided.
%%%     - `wallet' (optional): The name of the wallet to generate. If not provided,
%%%       the wallet's address will be used as the name.
%%%     - `persist' (optional): How the node should persist the wallet. Options:
%%%       - `client': The wallet is generated on the server, but not persisted.
%%%                  The full wallet key is returned for the user to store.
%%%       - `in-memory': The wallet is generated on the server and persisted only
%%%                  in local memory, never written to disk.
%%%       - `non-volatile': The wallet is persisted to non-volatile storage on 
%%%                  the node.
%%%     - `exportable' (optional): Whether the wallet should be exportable to
%%%                  remote peers via the `export' key (and its security
%%%                  mechanisms). If a wallet address or list of addresses is
%%%                  provided, the wallet is exportable only via those addresses.
%%%                  Defaults to the node's `wallet_admin' option if set, or its
%%%                  operator address if not.
%%% 
%%%     Generates a new wallet and sets up authentication for it. The steps are:
%%%     1. Creates a new wallet.
%%%     2. Determines the wallet name (provided name or wallet address).
%%%     3. Calls the authentication device with path `generate' and the wallet name.
%%%     4. Stores the wallet and authentication result.
%%%     5. Returns the authentication device's response with wallet name in `body'.
%%%     
%%%     The response will contain authentication setup (such as cookies) from the
%%%     authentication device, plus the wallet name in the `body' field and the 
%%%     wallet's address in the `wallet-address' field. The wallet's key is not
%%%     returned to the user unless the `persist' option is set to `client'. If
%%%     it is, the `~cookie@1.0' device will be employed to set the user's cookie
%%%     with the wallet's key.
%%% 
%%% /import
%%%     Parameters:
%%%     - `wallet' (optional): The name of the wallet to import.
%%%     - `key' (optional): The JSON-encoded wallet to import.
%%%     - `cookie' (optional): A structured-fields cookie containing a map with
%%%       a `key' field which is a JSON-encoded wallet.
%%%     - `auth' (optional): The authentication message to use.
%%%     - `persist' (optional): How the node should persist the wallet. The
%%%       supported options are as with the `generate' key.
%%% 
%%%     Imports a wallet for hosting from the user. Executes as `generate' does,
%%%     except that it expects the key to store to be provided either directly
%%%     via the `key' parameter as a `key' field in the cookie Structured-Fields
%%%     map. Support for loading the key from the cookie is provided such that
%%%     a previously-generated wallet by the user can have its persistence mode
%%%     changed.
%%% 
%%% /list
%%% 
%%%     Lists all hosted wallets on the node.
%%% 
%%% /commit
%%%     Parameters:
%%%     - `wallet' (optional): The name of the wallet to sign with.
%%%     - Authentication credentials as required by the wallet's authentication
%%%       message (e.g., cookies for `~cookie@1.0' device).
%%% 
%%%     Signs the given message using the specified wallet after authentication.
%%%     The process:
%%%     1. Identifies the wallet (from `wallet' parameter or authentication data).
%%%     2. Retrieves the stored authentication message for that wallet.
%%%     3. Calls the authentication device with path `verify' to validate the
%%%        request.
%%%     4. If authentication succeeds, signs the entire request message using
%%%        the wallet.
%%%     5. Returns the signed message with signature attached.
%%%     
%%%     If no `wallet' parameter is provided, the request's authentication data
%%%     (such as cookies) must contain wallet identification.
%%% 
%%% /export
%%%     Parameters:
%%%     - `wallet' (optional): The name of the wallet to export.
%%%     - `cookie' (optional): The cookie to use for the wallet.
%%%     - `batch' (optional): A list of wallet names to export, or `all' to
%%%       export all wallets for which the request passes authentication.
%%% 
%%%     Exports a given wallet or set of wallets (in JSON-encoded form). If
%%%     multiple wallets are requested, the result is a message with form
%%%     `name => #{ `key` => JSON-encoded wallet, `auth' => authentication
%%%     message, `exportable' => [address, ...], `persist' => `client' |
%%%     `in-memory' | `non-volatile' }'.
%%% 
%%%     A wallet will be exported if:
%%%     - The given request passes the wallet's authentication message; or
%%%     - The `exportable' field is set to `true` and the request is signed by
%%%       the `wallet_admin' key; or
%%%     - The `exportable' field is set to a list of addresses and the request
%%%       is signed by one of those addresses.
%%% 
%%% /sync
%%%     Parameters:
%%%     - `node': The node to pull wallets from.
%%%     - `as' (optional): The identity it should use when signing its request
%%%       to the remote peer.
%%%     - `batch' (optional): A list of wallet names to export, or `all' to load
%%%       every available wallet. Defaults to `all'.
%%% 
%%%     Attempts to download all wallets from the given node and import them.
%%% '''
-module(dev_wallet).
-export([generate/3, import/3, list/3, commit/3, export/3, sync/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Generate a new wallet for a user and register it on the node.
generate(Base, Request, Opts) ->
    % Create the new wallet, then join the combined code path with the import key.
    Wallet = ar_wallet:new(),
    register_wallet(Wallet, Base, Request, Opts).

%% @doc Import a wallet for hosting on the node. Expects the keys to be either
%% provided as a list of keys in the `keys' field, or a single key in the
%% `key' field. If the `keys' field is provided, the `key' field is ignored.
%% If neither are provided, the keys are extracted from the cookie.
import(Base, Request, Opts) ->
    Wallets =
        case hb_maps:find(<<"keys">>, Request, Opts) of
            {ok, Keys} ->
                [ wallet_from_key(Key) || Key <- Keys ];
            error ->
                case hb_maps:find(<<"key">>, Request, Opts) of
                    {ok, Key} ->
                        [ wallet_from_key(Key) ];
                    error ->
                        % Find the keys from the cookie.
                        request_to_wallets(Base, Request, Opts)
                end
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
                        % Merge the cookie from the registration response into
                        % the accumulator.
                        NewAddr = hb_maps:get(<<"body">>, RegRes, <<"">>, Opts),
                        OldImported = hb_maps:get(<<"imported">>, Acc, [], Opts),
                        NewCookie = hb_maps:get(<<"set-cookie">>, RegRes, #{}, Opts),
                        {ok, ParsedNewCookie} = dev_codec_cookie:from(NewCookie, #{}, Opts),
                        Acc#{
                            <<"set-cookie">> =>
                                hb_maps:merge(
                                    hb_maps:get(
                                        <<"set-cookie">>,
                                        Acc,
                                        #{},
                                        Opts
                                    ),
                                    ParsedNewCookie
                                ),
                            <<"imported">> => [ NewAddr | OldImported ]
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
register_wallet(Wallet, _Base, Request, Opts) ->
    % Find the wallet's address.
    {PrivKey, _} = Wallet,
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    % Determine how to persist the wallet.
    PersistMode = hb_ao:get(<<"persist">>, Request, <<"in-memory">>, Opts),
    % Get the authentication message from the request. If the message is a path
    % or a message with a `path' field, we resolve it to get the base.
    {ok, BaseAuthMsg} =
        case hb_ao:get(<<"auth">>, Request, undefined, Opts) of
            undefined ->
                {ok, #{ <<"device">> => <<"cookie@1.0">> }};
            AuthPath when is_binary(AuthPath) ->
                hb_ao:resolve(AuthPath, Opts);
            Msg ->
                case hb_maps:is_key(<<"path">>, Msg, Opts) of
                    true -> hb_ao:resolve(Msg, Opts);
                    false -> {ok, Msg}
                end
        end,
    AuthMsg =
        BaseAuthMsg#{
            <<"wallet-address">> => hb_util:human_id(Address)
        },
    Exportable = hb_ao:get(<<"exportable">>, Request, default, Opts),
    % Call authentication device to set up auth. Pass the wallet address as the
    % nonce. Some auth devices may use the nonce to track the messages that
    % they have committed.
    AuthRequest =
        Request#{
            <<"path">> => <<"commit">>,
            <<"nonce">> => Address
        },
    ?event({register_wallet, {auth_msg, AuthMsg}, {request, AuthRequest}}),
    case hb_ao:resolve(AuthMsg, AuthRequest, Opts) of
        {ok, InitializedAuthMsg} ->
            % Find the new signer address.
            PriorSigners = hb_message:signers(AuthMsg, Opts),
            NewSigners = hb_message:signers(InitializedAuthMsg, Opts),
            [Committer] = NewSigners -- PriorSigners,
            % Store wallet details.
            WalletDetails =
                #{
                    <<"key">> => ar_wallet:to_json(PrivKey),
                    <<"address">> => hb_util:human_id(Address),
                    <<"persist">> => PersistMode,
                    <<"auth">> => InitializedAuthMsg,
                    <<"committer">> => Committer,
                    <<"exportable">> => parse_exportable(Exportable, Opts)
                },
            persist_registered_wallet(WalletDetails, InitializedAuthMsg, Opts);
        {error, Reason} ->
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
    Base = RespBase#{ <<"body">> => Address },
    % Determine how to persist the wallet.
    case hb_maps:get(<<"persist">>, WalletDetails, <<"in-memory">>, Opts) of
        <<"client">> ->
            % Find the necessary wallet details to set the cookie on the client.
            JSONKey = hb_maps:get(<<"key">>, WalletDetails, undefined, Opts),
            % Don't store, set the cookie in the response.
            hb_ao:resolve(
                Base#{ <<"device">> => <<"cookie@1.0">> },
                #{
                    <<"path">> => <<"set-cookie">>,
                    <<"key-", Address/binary>> => JSONKey
                },
                Opts
            );
        PersistMode ->
            % Store wallet and return auth response with wallet info.
            store_wallet(
                hb_util:key_to_atom(PersistMode, existing),
                Address,
                WalletDetails,
                Opts
            ),
            ?event(
                {stored_and_returning,
                    {auth_response, Base},
                    {wallet_details, WalletDetails}
                }
            ),
            % Return auth response with wallet info added.
            {ok, Base}
    end.

%% @doc List all hosted wallets
list(_Base, _Request, Opts) ->
    {ok, list_wallets(Opts)}.

%% @doc Sign a message with a wallet.
commit(Base, Request, Opts) ->
    case request_to_wallets(Base, Request, Opts) of
        [] -> {error, <<"No wallets found to sign with.">>};
        WalletDetailsList ->
            ?event(
                {signing,
                    {request, Request},
                    {wallet_list, WalletDetailsList}
                }
            ),
            {
                ok,
                lists:foldl(
                    fun(WalletDetails, Acc) ->
                        sign_message(Acc, WalletDetails, Opts)
                    end,
                    Request,
                    WalletDetailsList
                )
            }
    end.

%% @doc Take a request and return the wallets it references. Performs validation
%% of access rights for the wallets before returning them.
request_to_wallets(Base, Request, Opts) ->
    % Get the wallet references or keys from the request or cookie.
    ExplicitWallets =
        hb_ao:get_first(
            [
                {Request, <<"wallets">>},
                {Request, <<"wallet">>}
            ],
            <<"all">>,
            Opts
        ),
    Wallets =
        case ExplicitWallets of
            <<"all">> ->
                % Get the wallet name from the cookie.
                wallets_from_cookie(Request, Opts);
            FoundWallets when is_list(FoundWallets) ->
                [ {wallet_addr, FoundWallet} || FoundWallet <- FoundWallets ];
            FoundWalletName when is_binary(FoundWalletName) ->
                [ {wallet_addr, FoundWalletName} ]
        end,
    ?event({attempting_to_load_wallets, {wallets, Wallets}, {request, Request}}),
    lists:filtermap(
        fun(WalletRef) ->
            case load_and_verify_access_wallet(WalletRef, Base, Request, Opts) of
                {ok, WalletDetails} -> {true, WalletDetails};
                {error, Reason} ->
                    ?event(
                        {failed_to_load_wallet,
                            {wallet_addr, WalletRef},
                            {reason, Reason}
                        }
                    ),
                    false
            end
        end,
        Wallets
    ).

%% @doc Load a wallet from a wallet reference and verify we have the authority
%% to access it.
load_and_verify_access_wallet({wallet_key, WalletKey}, _Base, _Request, _Opts) ->
    % Return the wallet key.
    {ok, #{ <<"key">> => WalletKey }};
load_and_verify_access_wallet({wallet_addr, WalletName}, Base, Request, Opts) ->
    % Get the wallet from the node's options.
    case find_wallet(WalletName, Opts) of
        not_found -> {error, <<"Wallet not hosted on node.">>};
        WalletDetails ->
            case validate_export_signers(WalletDetails, Request, Opts) of
                true ->
                    % If the request is already signed by an exporter
                    % return the request as-is with the wallet.
                    {ok, WalletDetails};
                false ->
                    case verify_wallet(Base, WalletDetails, Opts) of
                        {ok, true} ->
                            {ok, WalletDetails};
                        {ok, false} ->
                            {error, <<"Verification failed.">>};
                        {error, Reason} ->
                            {error, Reason}
                    end
            end
    end.

%% @doc Validate if a caller is an `exportable' for the given wallet.
validate_export_signers(WalletDetails, Request, Opts) ->
    Exportable =
        parse_exportable(
            hb_maps:get(<<"exportable">>, WalletDetails, [], Opts),
            Opts
        ),
    lists:any(
        fun(Signer) ->
            lists:member(Signer, Exportable)
        end,
        hb_message:signers(Request, Opts)
    ).

%% @doc Verify a wallet for a given request.
verify_wallet(Base, WalletDetails, Opts) ->
    AuthBase = hb_maps:get(<<"auth">>, WalletDetails, #{}, Opts),
    AuthRequest =
        Base#{
            <<"path">> => <<"verify">>,
            <<"committer">> =>
                hb_maps:get(<<"committer">>, WalletDetails, undefined, Opts)
        },
    ?event({verify_wallet, {auth_base, AuthBase}, {request, AuthRequest}}),
    hb_ao:resolve(AuthBase, AuthRequest, Opts).

%% @doc Parse cookie from a message to extract wallets.
wallets_from_cookie(Msg, Opts) ->
    % Parse the cookie as a Structured-Fields map.
    ?event(x,{wallets_from_cookie, {msg, {explicit, Msg}}}),
    ParsedCookie =
        try dev_codec_cookie:from(Msg, #{}, Opts) of
            {ok, CookieMsg} -> CookieMsg
        catch _:_ -> {error, <<"Invalid cookie format.">>}
        end,
    ?event({wallets_from_cookie, {parsed_cookie, ParsedCookie}, {msg, Msg}}),
    % Get the wallets that we should be able to access from the parsed cookie.
    % We determine their type from the `type-' prefix of the key.
    lists:flatten(lists:filtermap(
        fun({<<"key-", _Address/binary >>, Key}) ->
            {true, {wallet_key, ar_wallet:from_json(Key)}};
           ({<<"nonces-", _/binary>>, NoncesBin}) ->
            Nonces = binary_to_addresses(NoncesBin),
            {true, [{wallet_addr, Nonce} || Nonce <- Nonces]};
           ({_Irrelevant, _}) -> false
        end,
        hb_maps:to_list(ParsedCookie, Opts)
    )).

%% @doc Sign a message using hb_message:commit, taking either a wallet as a 
%% JSON-encoded string or a wallet details message with a `key' field.
sign_message(Message, NonMap, Opts) when not is_map(NonMap) ->
    sign_message(Message, #{ <<"key">> => NonMap }, Opts);
sign_message(Message, #{ <<"key">> := Key }, Opts) when is_binary(Key) ->
    sign_message(Message, ar_wallet:from_json(Key), Opts);
sign_message(Message, #{ <<"key">> := Key }, Opts) ->
    WalletOpts = Opts#{priv_wallet => Key},
    hb_message:commit(Message, WalletOpts).

%% @doc Export wallets from a request. The request should contain a source of
%% wallets (cookies, keys, or wallet names), or a specific list/name of a
%% wallet to authenticate and export.
export(Base, Request, Opts) ->
    PrivOpts = priv_store_opts(Opts),
    ModReq =
        case hb_ao:get(<<"wallets">>, Request, not_found, Opts) of
            <<"all">> ->
                AllLocalWallets = list_wallets(Opts),
                Request#{ <<"wallets">> => AllLocalWallets };
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

%% @doc Sync wallets from a remote node
sync(_Base, Request, Opts) ->
    case hb_ao:get(<<"node">>, Request, undefined, Opts) of
        undefined ->
            {error, <<"Node not specified.">>};
        Node ->
            Wallets = hb_maps:get(<<"wallets">>, Request, <<"all">>, Opts),
            SignAsOpts =
                case hb_ao:get(<<"as">>, Request, undefined, Opts) of
                    undefined -> Opts;
                    SignAs -> hb_opts:as(SignAs, Opts)
                end,
            ExportRequest =
                (hb_message:commit(
                    #{ <<"wallets">> => Wallets },
                    SignAsOpts
                ))#{ <<"path">> => <<"/~wallet@1.0/export">> },
            ?event({sync, {export_req, ExportRequest}}),
            case hb_http:get(Node, ExportRequest, SignAsOpts) of
                {ok, ExportResponse} ->
                    ExportedWallets = export_response_to_list(ExportResponse, #{}),
                    ?event({sync, {received_wallets, ExportedWallets}}),
                    % Import each wallet. Ignore wallet imports that fail.
                        lists:filtermap(
                            fun(Wallet) ->
                                ?event({sync, {importing, {wallet, Wallet}}}),
                                case persist_registered_wallet(Wallet, SignAsOpts) of
                                    {ok, #{ <<"body">> := Address }} ->
                                        ?event({sync, {imported, Address}}),
                                        {true, Address};
                                    {error, Reason} ->
                                        ?event({sync, {process_import_error, Reason}}),
                                        false
                                end
                            end,
                            ExportedWallets
                        ),
                    {ok, ExportedWallets};
                {error, Reason} ->
                    ?event({sync, {error, Reason}}),
                    {error, Reason}
            end
    end.

%%% Helper functions

%% @doc Parse the exportable setting for a wallet and return a list of addresses
%% which are allowed to export the wallet.
parse_exportable(default, Opts) ->
    case hb_opts:get(wallet_admin, undefined, Opts) of
        undefined -> 
            case hb_opts:get(operator, undefined, Opts) of
                undefined ->
                    [hb_util:human_id(hb_opts:get(priv_wallet, undefined, Opts))];
                Op -> [hb_util:human_id(Op)]
            end;
        Admin -> [Admin]
    end;
parse_exportable(true, Opts) -> parse_exportable(default, Opts);
parse_exportable(false, _Opts) -> [];
parse_exportable(Addresses, _Opts) when is_list(Addresses) -> Addresses;
parse_exportable(Address, _Opts) when is_binary(Address) -> [Address].

%% @doc Store a wallet in the appropriate location.
store_wallet(in_memory, Address, Details, Opts) ->
    % Get existing wallets
    CurrentWallets = hb_opts:get(priv_wallet_hosted, #{}, Opts),
    % Add new wallet
    UpdatedWallets = CurrentWallets#{ Address => Details },
    ?event({wallet_store, {updated_wallets, UpdatedWallets}}),
    % Update the node's options with the new wallets.
    hb_http_server:set_opts(Opts#{ priv_wallet_hosted => UpdatedWallets }),
    ok;
store_wallet(non_volatile, Address, Details, Opts) ->
    % Find the private store of the node.
    PrivOpts = priv_store_opts(Opts),
    {ok, Msg} = hb_cache:write(#{ Address => Details }, PrivOpts),
    PrivStore = hb_opts:get(priv_store, undefined, PrivOpts),
    % Link the wallet to the store.
    ok = hb_store:make_link(PrivStore, Msg, <<"wallet@1.0/", Address/binary>>).

%% @doc Find the wallet by name or address in the node's options.
find_wallet(Address, Opts) ->
    case find_wallet(in_memory, Address, Opts) of
        not_found -> find_wallet(non_volatile, Address, Opts);
        Wallet -> Wallet
    end.

% Loop over the wallets and find the wallet message whose committer is the cookie address.
find_wallet(in_memory, Addr, Opts) ->
    Wallets = hb_opts:get(priv_wallet_hosted, #{}, Opts),
    ?event({find_wallet, {address, Addr}, {wallets, Wallets}}),
    case hb_maps:find(Addr, Wallets, Opts) of
        {ok, Wallet} -> Wallet;
        error -> not_found
    end;

find_wallet(non_volatile, Name, Opts) ->
    PrivOpts = priv_store_opts(Opts),
    Store = hb_opts:get(priv_store, undefined, PrivOpts),
    Resolved = hb_store:resolve(Store, <<"wallet@1.0/", Name/binary>>),
    case hb_cache:read(Resolved, PrivOpts) of
        {ok, Wallet} ->
            WalletDetails = hb_maps:get(Name, Wallet, not_found, PrivOpts),
            hb_cache:ensure_all_loaded(WalletDetails, PrivOpts);
        _ -> not_found
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
    Opts#{ store => hb_opts:get(priv_store, no_wallet_store, Opts) }.

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
        priv_wallet => ar_wallet:new()
    }),
    % Generate wallet with specified parameters
    {ok, GenResponse} = hb_http:get(Node, GeneratePath, #{}),
    % Should get wallet name in body, wallet-address, and auth cookie
    ?assertMatch(#{<<"body">> := _}, GenResponse),
    WalletAddr = maps:get(<<"body">>, GenResponse),
    case ExpectedName of
        undefined -> 
            % For unnamed wallets, just check it's a non-empty binary
            ?assert(is_binary(WalletAddr) andalso byte_size(WalletAddr) > 0);
        _ -> 
            % For named wallets, check exact match
            ?assertEqual(ExpectedName, WalletAddr)            
    end,
    ?assert(maps:is_key(<<"set-cookie">>, GenResponse)),
    AuthCookie = maps:get(<<"set-cookie">>, GenResponse, <<>>),
    % Now verify by signing a message
    TestMessage =
        maps:merge(
            #{
                <<"device">> => <<"wallet@1.0">>,
                <<"path">> => <<"commit">>,
                <<"body">> => <<"Test message">>
            },
            CommitParams
        ),
    TestMessageWithCookie = hb_private:set(TestMessage, <<"cookie">>, AuthCookie, #{}),
    ?event({signing_with_cookie, {test_message, TestMessageWithCookie}}),
    {ok, SignedMessage} = hb_http:post(Node, TestMessageWithCookie, #{}),
    % Should return signed message with correct signer
    ?assertMatch(#{ <<"body">> := <<"Test message">> }, SignedMessage),
    ?assert(hb_message:signers(SignedMessage, #{}) =:= [WalletAddr]).

client_persist_generate_and_verify_test() ->
    test_wallet_generate_and_verify(
        <<"/~wallet@1.0/generate?persist=client">>,
        undefined,
        #{}
    ).

cookie_wallet_generate_and_verify_test() ->
    test_wallet_generate_and_verify(
        <<"/~wallet@1.0/generate?persist=in-memory">>,
        undefined,
        #{}
    ).

non_volatile_persist_generate_and_verify_test() ->
    test_wallet_generate_and_verify(
        <<"/~wallet@1.0/generate?persist=non-volatile">>,
        undefined,
        #{}
    ).

import_wallet_with_key_test() ->
    Node = hb_http_server:start_node(#{
        priv_wallet => ar_wallet:new()
    }),
    % Create a test wallet key to import (in real scenario from user).
    TestWallet = ar_wallet:new(),
    WalletAddress = hb_util:human_id(TestWallet),
    WalletKey = ar_wallet:to_json(TestWallet),
    % Import the wallet with a specific name.
    ImportUrl =
        <<"/~wallet@1.0/import?wallet=imported-wallet&persist=in-memory&key=",
            WalletKey/binary>>,
    {ok, ImportResponse} = hb_http:get(Node, ImportUrl, #{}),
    % Response should come from auth device with wallet name in body.
    % Wallet name is the address of the wallet.
    ?assertMatch(#{<<"body">> := WalletAddress}, ImportResponse),
    % Should include cookie setup from auth device.
    ?assert(maps:is_key(<<"set-cookie">>, ImportResponse)).

list_wallets_test() ->
    Node = hb_http_server:start_node(#{
        priv_wallet => ar_wallet:new()
    }),
    % Generate some wallets first.
    {ok, Msg1} =
        hb_http:get(
            Node,
            <<"/~wallet@1.0/generate?persist=in-memory">>,
            #{}
        ),
    {ok, Msg2} =
        hb_http:get(
            Node,
            <<"/~wallet@1.0/generate?persist=in-memory">>,
            #{}
        ),
    WalletAddress1 = maps:get(<<"body">>, Msg1),
    WalletAddress2 = maps:get(<<"body">>, Msg2),
    ?assertEqual(WalletAddress1, maps:get(<<"body">>, Msg1)),
    ?assertEqual(WalletAddress2, maps:get(<<"body">>, Msg2)),
    % List all wallets (no authentication required for listing).
    {ok, Wallets} = hb_http:get(Node, <<"/~wallet@1.0/list">>, #{}),
    % Each wallet entry should be a wallet name.
    ?assert(
        lists:all(
            fun(Wallet) -> lists:member(Wallet, hb_maps:values(Wallets)) end,
            [WalletAddress1, WalletAddress2]
        )
    ).

commit_with_cookie_wallet_test() ->
    Node = hb_http_server:start_node(#{
        priv_wallet => ar_wallet:new()
    }),
    % Generate a client wallet to get a cookie with full wallet key.
    {ok, GenResponse} =
        hb_http:get(Node, <<"/~wallet@1.0/generate?persist=client">>, #{}),
    WalletName = maps:get(<<"body">>, GenResponse),
    ?event(x, {gen_response, {generated_wallet_response, GenResponse}}),
    AuthCookie =
        maps:get(
            <<"set-cookie">>,
            GenResponse,
            hb_private:get(<<"cookie">>, GenResponse, <<>>)
        ),
    % Use the cookie to sign a message (no wallet parameter needed).
    TestMessage = #{
        <<"device">> => <<"wallet@1.0">>,
        <<"path">> => <<"commit">>,
        <<"body">> => <<"Test data">>
    },
    TestMessageWithCookie = hb_private:set(TestMessage, <<"cookie">>, AuthCookie, #{}),
    ?event(x, {test_message, {post_test_message, TestMessageWithCookie}}),
    {ok, SignedMessage} = hb_http:post(Node, TestMessageWithCookie, #{}),
    % Should return the signed message with signature attached.
    ?assert(hb_message:signers(SignedMessage, #{}) =:= [WalletName]).

export_wallet_test() ->
    Node = hb_http_server:start_node(#{}),
    % Generate a wallet to export.
    {ok, GenResponse} =
        hb_http:get(
            Node,
            <<"/~wallet@1.0/generate?persist=in-memory">>,
            #{}
        ),
    AuthCookie = maps:get(<<"set-cookie">>, GenResponse),
    WalletAddress = maps:get(<<"body">>, GenResponse),
    ?event({export_test, {auth_cookie, AuthCookie}}),
    % Export the wallet with authentication.
    {ok, ExportResponse} =
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"/~wallet@1.0/export/1">>,
                <<"priv">> => #{
                    <<"cookie">> => AuthCookie
                }
            },
            #{}
        ),
    ?event({export_test, {export_response, ExportResponse}}),
    % Should return wallet details including key, auth, exportable, persist.
    ?assertMatch(#{<<"key">> := _, <<"persist">> := <<"in-memory">>}, ExportResponse),
    ?assert(maps:is_key(<<"auth">>, ExportResponse)),
    ?assert(maps:is_key(<<"exportable">>, ExportResponse)),
    % Should return the correct wallet address in the response.
    ?assertEqual(WalletAddress, maps:get(<<"address">>, ExportResponse)),
    Auth = maps:get(<<"auth">>, ExportResponse),
    ?assertEqual(WalletAddress, maps:get(<<"wallet-address">>, Auth)).

export_non_volatile_wallet_test() ->
        Node = hb_http_server:start_node(#{
            priv_wallet => ar_wallet:new()
        }),
        % Generate a wallet to export.
        {ok, GenResponse} =
            hb_http:get(
                Node,
                <<"/~wallet@1.0/generate?persist=non-volatile">>,
                #{}
            ),
        AuthCookie = maps:get(<<"set-cookie">>, GenResponse),
        ?event({export_test, {auth_cookie, AuthCookie}}),
        % Export the wallet with authentication.
        {ok, ExportResponse} =
            hb_http:get(
                Node,
                #{
                    <<"device">> => <<"wallet@1.0">>,
                    <<"path">> => <<"export/1">>,
                    <<"priv">> => #{    
                        <<"cookie">> => AuthCookie
                    }
                },
                #{}
            ),
        % Should return wallet details including key, auth, exportable, persist.
        ?assertMatch(#{<<"key">> := _, <<"persist">> := <<"non-volatile">>}, ExportResponse),
        ?assert(maps:is_key(<<"auth">>, ExportResponse)),
        ?assert(maps:is_key(<<"exportable">>, ExportResponse)).

export_individual_batch_wallets_test() ->
    Node =
        hb_http_server:start_node(
            AdminOpts =
                #{
                    priv_wallet => AdminWallet = ar_wallet:new()
                }
        ),
    % Generate multiple wallets and collect auth cookies.
    {ok, #{ <<"body">> := WalletAddr1 }} =
        hb_http:get(
            Node,
            <<"/~wallet@1.0/generate?persist=in-memory&exportable=",
                (hb_util:human_id(AdminWallet))/binary>>,
            #{}
        ),
    {ok, #{ <<"body">> := WalletAddr2 }} =
        hb_http:get(
            Node,
            <<"/~wallet@1.0/generate?persist=in-memory&exportable=",
                (hb_util:human_id(AdminWallet))/binary>>,
            #{}
        ),
    % Export all wallets.
    {ok, ExportAllResponse} =
        hb_http:get(
            Node,
            (hb_message:commit(
                #{
                    <<"device">> => <<"wallet@1.0">>,
                    <<"wallets">> => [WalletAddr1, WalletAddr2]
                },
                AdminOpts
            ))#{ <<"path">> => <<"/~wallet@1.0/export">> },
            #{}
        ),

    % Export single wallet by address.
    {ok, ExportWallet1Response} =
    hb_http:get(
        Node,
        (hb_message:commit(
            #{
                <<"device">> => <<"wallet@1.0">>,
                <<"wallets">> => [WalletAddr1]
            },
            AdminOpts
        ))#{ <<"path">> => <<"/~wallet@1.0/export">> },
        #{}
    ),
    
    ?assert(is_map(ExportAllResponse)),
    ?assert(is_map(ExportWallet1Response)),
    ExportedAllWallets =
        [
            hb_maps:get(<<"address">>, Wallet, undefined, #{})
        ||
            Wallet <- export_response_to_list(ExportAllResponse, #{})
        ],
    ExportedSingleWallets =
        [
            hb_maps:get(<<"address">>, Wallet, undefined, #{})
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
        [WalletAddr1, WalletAddr2]
    ),
    ?assert(lists:member(WalletAddr1, ExportedSingleWallets)).

            

export_batch_all_wallets_test() ->
    % Remove all previous cached wallets.
    hb_store:reset(hb_opts:get(priv_store, no_wallet_store, #{})),
    Node =
        hb_http_server:start_node(
            AdminOpts =
                #{
                    priv_wallet => AdminWallet = ar_wallet:new()
                }
        ),
    % Generate multiple wallets and collect auth cookies.
    {ok, #{ <<"body">> := WalletAddr1 }} =
        hb_http:get(
            Node,
            <<"/~wallet@1.0/generate?persist=in-memory&exportable=",
                (hb_util:human_id(AdminWallet))/binary>>,
            #{}
        ),
    {ok, #{ <<"body">> := WalletAddr2 }} =
        hb_http:get(
            Node,
            <<"/~wallet@1.0/generate?persist=in-memory&exportable=",
                (hb_util:human_id(AdminWallet))/binary>>,
            #{}
        ),
    % Export all wallets.
    {ok, ExportResponse} =
        hb_http:get(
            Node,
            (hb_message:commit(
                #{
                    <<"device">> => <<"wallet@1.0">>,
                    <<"wallets">> => <<"all">>
                },
                AdminOpts
            ))#{ <<"path">> => <<"/~wallet@1.0/export">> },
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
    Node =
        hb_http_server:start_node(#{
            priv_wallet => Node1Wallet = ar_wallet:new()
        }),
    % Start a second node to sync from.
    Node2 =
        hb_http_server:start_node(#{
            priv_wallet => ar_wallet:new(),
            wallet_admin => hb_util:human_id(Node1Wallet)
        }),
    % Generate a wallet on the second node.
    {ok, GenResponse} =
        hb_http:get(
            Node2,
            <<"/~wallet@1.0/generate?persist=in-memory">>,
            #{}
        ),
    WalletName = maps:get(<<"body">>, GenResponse),
    % Test sync to the first node from the second.
    {ok, _} =
        hb_http:get(
            Node,
            <<"/~wallet@1.0/sync?node=", Node2/binary, "&wallets=all">>,
            #{}
        ),
    % Get the wallet list from the first node.
    {ok, WalletList} = hb_http:get(Node, <<"/~wallet@1.0/list">>, #{}),
    ?event({sync_wallets_test, {wallet_list, WalletList}}),
    % Should return a map of successfully imported wallets or list of names.
    ?assert(lists:member(WalletName, hb_maps:values(WalletList))).

sync_non_volatile_wallets_test() ->
    % Remove all the previous cached wallets.
    hb_store:reset(hb_opts:get(priv_store, no_wallet_store, #{})),
    Node =
        hb_http_server:start_node(#{
            priv_wallet => Node1Wallet = ar_wallet:new()
        }),
    % Start a second node to sync from.
    Node2 =
        hb_http_server:start_node(#{
            priv_wallet => ar_wallet:new(),
            wallet_admin => hb_util:human_id(Node1Wallet)
        }),
    % Generate a wallet on the second node.
    {ok, GenResponse} =
        hb_http:get(
            Node2,
            <<"/~wallet@1.0/generate?persist=non-volatile">>,
            #{}
        ),
    WalletName = maps:get(<<"body">>, GenResponse),
    % Test sync to the first node from the second.
    {ok, _} =
        hb_http:get(
            Node,
            <<"/~wallet@1.0/sync?node=", Node2/binary, "&wallets=all">>,
            #{}
        ),
    % Get the wallet list from the first node.
    {ok, WalletList} = hb_http:get(Node, <<"/~wallet@1.0/list">>, #{}),
    ?event({sync_wallets_test, {wallet_list, WalletList}}),
    % Should return a map of successfully imported wallets or list of names.
    ?assert(lists:member(WalletName, hb_maps:values(WalletList))).