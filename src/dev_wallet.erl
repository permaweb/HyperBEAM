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

%% @doc Import a wallet for hosting on the node.
import(Base, Request, Opts) ->
    case hb_ao:get(<<"key">>, Request, undefined, Opts) of
        undefined ->
            % Import from cookie.
            case request_to_wallet(Base, Request, Opts) of
                {ok, ModRequest, WalletDetails} ->
                    register_wallet(WalletDetails, Base, ModRequest, Opts);
                {error, Reason} ->
                    {error, Reason}
            end;
        Key ->
            % Import from the key provided.
            register_wallet(
                if is_binary(Key) -> ar_wallet:from_json(Key);
                true -> Key
                end,
                Base,
                Request,
                Opts
            )
    end.

%% @doc Register a wallet on the node.
register_wallet(Wallet, _Base, Request, Opts) ->
    % Get the wallet name from the request or cookie.
    WalletName = hb_ao:get(<<"wallet">>, Request, undefined, Opts),
    PersistMode = hb_ao:get(<<"persist">>, Request, <<"in-memory">>, Opts),
    % Get the authentication message from the request. If the message is a path
    % or a message with a `path' field, we resolve it to get the base.
    {ok, AuthMsg} =
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
    Exportable = hb_ao:get(<<"exportable">>, Request, default, Opts),
    % Find the wallet's address.
    {PrivKey, _} = Wallet,
    Address = ar_wallet:to_address(Wallet),
    % Determine final wallet name.
    FinalWalletName =
        case WalletName of
            undefined -> hb_util:human_id(Address);
            Name -> Name
        end,
    % Call authentication device to set up auth.
    AuthRequest =
        Request#{
            <<"path">> => <<"commit">>,
            <<"name">> => FinalWalletName
        },
    ?event({register_wallet, {auth_msg, AuthMsg}, {request, AuthRequest}}),
    case hb_ao:resolve(AuthMsg, AuthRequest, Opts) of
        {ok, AuthResponse} ->
            % Store wallet details.
            WalletDetails =
                #{
                    <<"key">> => JSONKey = ar_wallet:to_json(PrivKey),
                    <<"address">> => hb_util:human_id(Address),
                    <<"persist">> => PersistMode,
                    <<"auth">> => AuthResponse,
                    <<"exportable">> => parse_exportable(Exportable, Opts)
                },
            case PersistMode of
                <<"client">> ->
                    % Don't store, set the cookie in the response.
                    hb_ao:resolve(
                        AuthResponse#{
                            <<"device">> => <<"cookie@1.0">>,
                            <<"body">> => FinalWalletName,
                            <<"wallet-address">> => hb_util:human_id(Address)
                        },
                        #{
                            <<"path">> => <<"set-cookie">>,
                            <<"key">> => JSONKey
                        },
                        Opts
                    );
                _ ->
                    % Store wallet and return auth response with wallet info.
                    store_wallet(
                        hb_util:key_to_atom(PersistMode, existing),
                        FinalWalletName,
                        WalletDetails,
                        Opts
                    ),
                    ModResponse =
                        AuthResponse#{
                            <<"body">> => FinalWalletName,
                            <<"wallet-address">> => hb_util:human_id(Address)
                        },
                    ?event(
                        {stored_and_returning,
                            {auth_response, ModResponse},
                            {wallet_details, WalletDetails}
                        }
                    ),
                    % Return auth response with wallet info added.
                    {ok, ModResponse}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc List all hosted wallets
list(_Base, _Request, Opts) ->
    {ok, list_wallets(Opts)}.

%% @doc Sign a message with a wallet.
commit(Base, RawRequest, Opts) ->
    case request_to_wallet(Base, RawRequest, Opts) of
        {ok, Request, WalletDetails} ->
            ?event(
                {signing,
                    {raw_request, RawRequest},
                    {request, Request},
                    {wallet_details, WalletDetails}
                }
            ),
            {ok, sign_message(Request, WalletDetails, Opts)};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Take a request and return its associated wallet, validating the request
%% as necessary.
request_to_wallet(Base, Request, Opts) ->
    % Get the wallet name from the request or cookie.
    Wallet =
        case hb_ao:get(<<"wallet">>, Request, undefined, Opts) of
            undefined ->
                % Get the wallet name from the cookie.
                wallet_from_cookie(Request, Opts);
            FoundWalletName -> {wallet_name, FoundWalletName}
        end,
    case Wallet of
        {wallet_name, WalletName} ->
            % Get the wallet from the node's options.
            case find_wallet(WalletName, Opts) of
                not_found -> {error, <<"Wallet not hosted on node.">>};
                WalletDetails ->
                    case validate_export_signers(WalletDetails, Request, Opts) of
                        true ->
                            % If the request is already signed by an exporter
                            % return the request as-is with the wallet.
                            {ok, Request, WalletDetails};
                        false ->
                            case verify_wallet(Base, WalletDetails, Opts) of
                                {ok, NewRequest} ->
                                    {ok, NewRequest, WalletDetails};
                                {error, Reason} ->
                                    {error, Reason}
                            end
                    end
            end;
        {wallet_key, WalletKey} ->
            % Sign the message.
            {ok, Request, #{ <<"key">> => WalletKey }}
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
    AuthRequest = Base#{ <<"path">> => <<"verify">> },
    ?event({verify_wallet, {auth_base, AuthBase}, {request, AuthRequest}}),
    hb_ao:resolve(AuthBase, AuthRequest, Opts).

%% @doc Parse cookie from a message to extract wallet key or name.
wallet_from_cookie(Msg, Opts) ->
    % Parse the cookie as a Structured-Fields map.
    Parsed =
        try dev_codec_cookie:from(Msg, #{}, Opts) of
            {ok, CookieMsg} -> CookieMsg
        catch _:_ -> {error, <<"Invalid cookie format.">>}
        end,
    case hb_maps:find(<<"key">>, Parsed, Opts) of
        {ok, Key} ->
            {wallet_key, ar_wallet:from_json(Key)};
        _ ->
            case hb_maps:find(<<"name">>, Parsed, Opts) of
                {ok, WalletName} -> {wallet_name, WalletName};
                _ ->
                    ?event({invalid_cookie, {msg, Msg}, {parsed, Parsed}}),
                    {error, <<"Invalid cookie contents.">>}
            end
    end.

%% @doc Sign a message using hb_message:commit, taking either a wallet as a 
%% JSON-encoded string or a wallet details message with a `key' field.
sign_message(Message, NonMap, Opts) when not is_map(NonMap) ->
    sign_message(Message, #{ <<"key">> => NonMap }, Opts);
sign_message(Message, #{ <<"key">> := Key }, Opts) when is_binary(Key) ->
    sign_message(Message, ar_wallet:from_json(Key), Opts);
sign_message(Message, #{ <<"key">> := Key }, Opts) ->
    WalletOpts = Opts#{priv_wallet => Key},
    hb_message:commit(Message, WalletOpts).

%% @doc Export a wallet or set of wallets
export(Base, RawRequest, Opts) ->
    ?event({export, {base, Base}, {request, RawRequest}}),
    case hb_ao:get(<<"batch">>, RawRequest, undefined, Opts) of
        undefined -> export_single(Base, RawRequest, Opts);
        <<"all">> -> export_batch(Base, RawRequest, list_wallets(Opts), Opts);
        Names -> export_batch(Base, RawRequest, Names, Opts)
    end.

%% @doc Export a single wallet.
export_single(Base, Request, Opts) ->
    case request_to_wallet(Base, Request, Opts) of
        {ok, _, WalletDetails} ->
            {ok, WalletDetails};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Export a batch of wallets.
export_batch(Base, Request, Names, Opts) ->
    ExportedWallets =
        lists:filtermap(
            fun(Name) ->
                case export_single(Base, Request#{ <<"wallet">> => Name }, Opts) of
                    {ok, Exported} ->
                        {true, {Name, Exported}};
                    {error, _Reason} ->
                        false
                end
            end,
            Names
        ),
    Res = hb_maps:from_list(ExportedWallets),
    ?event({export_batch, {res, Res}}),
    {ok, Res}.

%% @doc Sync wallets from a remote node
sync(_Base, Request, Opts) ->
    case hb_ao:get(<<"node">>, Request, undefined, Opts) of
        undefined ->
            {error, <<"Node not specified.">>};
        Node ->
            Wallets = hb_maps:get(<<"batch">>, Request, <<"all">>, Opts),
            SignAsOpts =
                case hb_ao:get(<<"as">>, Request, undefined, Opts) of
                    undefined -> Opts;
                    SignAs -> hb_opts:as(SignAs, Opts)
                end,
            ExportRequest =
                (hb_message:commit(
                    #{ <<"batch">> => Wallets },
                    SignAsOpts
                ))#{ <<"path">> => <<"/~wallet@1.0/export">> },
            case hb_http:get(Node, ExportRequest, SignAsOpts) of
                {ok, ExportedWallets} ->
                    % Import each wallet.
                    hb_maps:map(
                        fun(Name, Wallet) when ?IS_ID(Name) ->
                            case import(Node, Wallet, SignAsOpts) of
                                {ok, _} -> ok;
                                {error, Reason} -> error(Reason)
                            end;
                           (_Name, _Wallet) -> ignore_non_wallet
                        end,
                        ExportedWallets
                    ),
                    {ok, hb_maps:keys(ExportedWallets)};
                {error, Reason} -> {error, Reason}
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
store_wallet(in_memory, Name, Details, Opts) ->
    % Get existing wallets
    CurrentWallets = hb_opts:get(priv_wallet_hosted, #{}, Opts),
    % Add new wallet
    UpdatedWallets = CurrentWallets#{ Name => Details },
    ?event({wallet_store, {updated_wallets, UpdatedWallets}}),
    % Update the node's options with the new wallets.
    hb_http_server:set_opts(Opts#{ priv_wallet_hosted => UpdatedWallets }),
    ok;
store_wallet(non_volatile, Name, Details, Opts) ->
    % Find the private store of the node.
    PrivOpts = priv_store_opts(Opts),
    {ok, Msg} = hb_cache:write(#{ Name => Details }, PrivOpts),
    PrivStore = hb_opts:get(priv_store, undefined, PrivOpts),
    % Link the wallet to the store.
    ok = hb_store:make_link(PrivStore, <<"wallet@1.0/", Name/binary>>, Msg).

%% @doc Find the wallet by name or address in the node's options.
find_wallet(Name, Opts) ->
    case find_wallet(in_memory, Name, Opts) of
        undefined -> find_wallet(non_volatile, Name, Opts);
        Wallet -> Wallet
    end.
find_wallet(in_memory, Name, Opts) ->
    hb_maps:get(Name, hb_opts:get(priv_wallet_hosted, #{}, Opts), not_found, Opts);
find_wallet(non_volatile, Name, Opts) ->
    PrivOpts = priv_store_opts(Opts),
    Store = hb_opts:get(priv_store, undefined, PrivOpts),
    {ok, Resolved} = hb_store:resolve(Store, <<"wallet@1.0/", Name/binary>>),
    hb_cache:read(Store, Resolved).

%% @doc Generate a list of all hosted wallets.
list_wallets(Opts) ->
    list_wallets(in_memory, Opts) ++ list_wallets(non_volatile, Opts).
list_wallets(in_memory, Opts) ->
    hb_maps:keys(hb_opts:get(priv_wallet_hosted, #{}, Opts));
list_wallets(non_volatile, Opts) ->
    PrivOpts = priv_store_opts(Opts),
    hb_cache:list(<<"wallet@1.0/">>, PrivOpts).

%% @doc Generate a new `Opts' message with the `priv_store' as the only `store'
%% option.
priv_store_opts(Opts) ->
    Opts#{ store => hb_opts:get(priv_store, no_wallet_store, Opts) }.

%%% Tests

%% @doc Helper function to test wallet generation and verification flow.
test_wallet_generate_and_verify(GeneratePath, ExpectedName, CommitParams) ->
    Node = hb_http_server:start_node(#{
        priv_wallet => ar_wallet:new()
    }),
    % Generate wallet with specified parameters
    {ok, GenResponse} = hb_http:get(Node, GeneratePath, #{}),
    % Should get wallet name in body, wallet-address, and auth cookie
    ?assertMatch(#{<<"body">> := _, <<"wallet-address">> := _}, GenResponse),
    WalletName = maps:get(<<"body">>, GenResponse),
    WalletID = maps:get(<<"wallet-address">>, GenResponse),
    case ExpectedName of
        undefined -> 
            % For unnamed wallets, just check it's a non-empty binary
            ?assert(is_binary(WalletName) andalso byte_size(WalletName) > 0);
        _ -> 
            % For named wallets, check exact match
            ?assertEqual(ExpectedName, WalletName)            
    end,
    ?assert(maps:is_key(<<"set-cookie">>, GenResponse)),
    AuthCookie = maps:get(<<"set-cookie">>, GenResponse, <<>>),
    % Now verify by signing a message
    TestMessage =
        maps:merge(
            #{
                <<"device">> => <<"wallet@1.0">>,
                <<"path">> => <<"commit">>,
                <<"cookie">> => AuthCookie,
                <<"body">> => <<"Test message">>
            },
            CommitParams
        ),
    {ok, SignedMessage} = hb_http:post(Node, TestMessage, #{}),
    % Should return signed message with correct signer
    ?assertMatch(#{ <<"body">> := <<"Test message">> }, SignedMessage),
    ?assert(hb_message:signers(SignedMessage, #{}) =:= [WalletID]).

client_persist_generate_and_verify_test() ->
    test_wallet_generate_and_verify(
        <<"/~wallet@1.0/generate?persist=client">>,
        undefined,
        #{}
    ).

named_wallet_generate_and_verify_test() ->
    test_wallet_generate_and_verify(
        <<"/~wallet@1.0/generate?persist=in-memory&wallet=my-named-wallet">>,
        <<"my-named-wallet">>,
        #{ <<"wallet">> => <<"my-named-wallet">> }
    ).

unnamed_wallet_generate_and_verify_test() ->
    test_wallet_generate_and_verify(
        <<"/~wallet@1.0/generate?persist=in-memory">>,
        undefined,
        #{}
    ).

import_wallet_with_key_test() ->
    Node = hb_http_server:start_node(#{
        priv_wallet => ar_wallet:new()
    }),
    % Create a test wallet key to import (in real scenario from user).
    TestWallet = ar_wallet:new(),
    WalletKey = ar_wallet:to_json(TestWallet),
    % Import the wallet with a specific name.
    ImportUrl =
        <<"/~wallet@1.0/import?wallet=imported-wallet&persist=in-memory&key=",
            WalletKey/binary>>,
    {ok, ImportResponse} = hb_http:get(Node, ImportUrl, #{}),
    % Response should come from auth device with wallet name in body.
    ?assertMatch(#{<<"body">> := <<"imported-wallet">>}, ImportResponse),
    % Should include cookie setup from auth device.
    ?assert(maps:is_key(<<"set-cookie">>, ImportResponse)).

list_wallets_test() ->
    Node = hb_http_server:start_node(#{
        priv_wallet => ar_wallet:new()
    }),
    % Generate some wallets first.
    {ok, _} =
        hb_http:get(
            Node,
            <<"/~wallet@1.0/generate?persist=in-memory&wallet=wallet-1">>,
            #{}
        ),
    {ok, _} =
        hb_http:get(
            Node,
            <<"/~wallet@1.0/generate?persist=in-memory&wallet=wallet-2">>,
            #{}
        ),
    % List all wallets (no authentication required for listing).
    {ok, Wallets} = hb_http:get(Node, <<"/~wallet@1.0/list">>, #{}),
    ?event({wallet_list, {wallets, Wallets}}),
    % Each wallet entry should be a wallet name.
    ?assert(
        lists:all(
            fun(Wallet) -> lists:member(Wallet, hb_maps:values(Wallets)) end,
            [<<"wallet-1">>, <<"wallet-2">>]
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
    AuthCookie =
        maps:get(
            <<"set-cookie">>,
            GenResponse,
            maps:get(<<"cookie">>, GenResponse, <<>>)
        ),
    % Use the cookie to sign a message (no wallet parameter needed).
    TestMessage = #{
        <<"device">> => <<"wallet@1.0">>,
        <<"path">> => <<"commit">>,
        <<"cookie">> => AuthCookie,
        <<"body">> => <<"Test data">>
    },
    {ok, SignedMessage} = hb_http:post(Node, TestMessage, #{}),
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
    ?event({export_test, {auth_cookie, AuthCookie}}),
    % Export the wallet with authentication.
    {ok, ExportResponse} =
        hb_http:get(
            Node,
            #{
                <<"device">> => <<"wallet@1.0">>,
                <<"path">> => <<"export">>,
                <<"cookie">> => AuthCookie
            },
            #{}
        ),
    % Should return wallet details including key, auth, exportable, persist.
    ?assertMatch(#{<<"key">> := _, <<"persist">> := <<"in-memory">>}, ExportResponse),
    ?assert(maps:is_key(<<"auth">>, ExportResponse)),
    ?assert(maps:is_key(<<"exportable">>, ExportResponse)).

export_batch_wallets_test() ->
    Node =
        hb_http_server:start_node(
            AdminOpts =
                #{
                    priv_wallet => AdminWallet = ar_wallet:new()
                }
        ),
    % Generate multiple wallets and collect auth cookies.
    {ok, _} =
        hb_http:get(
            Node,
            <<"/~wallet@1.0/generate?persist=in-memory&wallet=batch1&exportable=",
                (hb_util:human_id(AdminWallet))/binary>>,
            #{}
        ),
    {ok, _} =
        hb_http:get(
            Node,
            <<"/~wallet@1.0/generate?persist=in-memory&wallet=batch2&exportable=",
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
                    <<"batch">> => <<"all">>
                },
                AdminOpts
            ))#{ <<"path">> => <<"/~wallet@1.0/export">> },
            #{}
        ),
    ?assert(is_map(ExportResponse)),
    ?assert(maps:size(ExportResponse) >= 2),
    ?event({export_batch_test, {export_response, ExportResponse}}),
    % Each exported wallet should have the required structure.
    lists:foreach(
        fun(Name) ->
            ?assert(hb_maps:is_key(Name, ExportResponse))
        end,
        [<<"batch1">>, <<"batch2">>]
    ).

sync_wallets_test() ->
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
            <<"/~wallet@1.0/sync?node=", Node2/binary, "&batch=all">>,
            #{}
        ),
    % Get the wallet list from the first node.
    {ok, WalletList} = hb_http:get(Node, <<"/~wallet@1.0/list">>, #{}),
    ?event({sync_wallets_test, {wallet_list, WalletList}}),
    % Should return a map of successfully imported wallets or list of names.
    ?assert(lists:member(WalletName, hb_maps:values(WalletList))).