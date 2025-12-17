%%% @doc This device provides an interface for IoT device attestation and registration.
%%%
%%% The device acts as a gatekeeper for device registration and handles the complete
%%% attestation flow including:
%%% 1. Device registration with public key, wallet, and signed hash
%%% 2. Registration status checking
%%% 3. Attestation challenge generation with nonces
%%% 4. Attestation response verification
%%%
%%% The registration process is controlled entirely by this device - it is not
%%% directly accessible. All registration operations must go through this device.
%%%
%%% Required configuration in Opts:
%%% - registration_process_id: The process ID of the registration process
%%% - user_process_prefix: Optional prefix for user nonce processes (default: "nonce-")
-module(dev_attestation).
-export([register/3, check_registration/3, attest/3, verify/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Register a new IoT device.
%%
%% This function registers a device with:
%% - public-key: Public key from secure element
%% - wallet: Arweave wallet address
%% - signed-hash: Hash signed by secure element (verifiable by public key)
%% - multiple-wallet: "allowed" or "not_allowed" (default: "not_allowed")
%%
%% The device checks if already registered and enforces multiple_wallet rules.
%%
%% @param _Base Ignored parameter
%% @param Req The registration request message
%% @param Opts Configuration options
%% @returns `{ok, Map}' with registration status, or `{error, Reason}' on failure
-spec register(_Base :: term(), Req :: map(), Opts :: map()) ->
    {ok, map()} | {error, term()}.
register(_Base, Req, Opts) ->
    ?event(attestation_register, {request, Req}),
    maybe
        % Extract and validate inputs
        {ok, PublicKey} ?= extract_required(<<"public-key">>, Req, Opts),
        {ok, Wallet} ?= extract_required(<<"wallet">>, Req, Opts),
        {ok, SignedHash} ?= extract_required(<<"signed-hash">>, Req, Opts),
        MultipleWallet = hb_ao:get(<<"multiple-wallet">>, Req, <<"not_allowed">>, Opts),
        
        % Validate inputs
        ok ?= validate_registration_inputs(PublicKey, Wallet, SignedHash, MultipleWallet),
        
        % Check if already registered
        {ok, CheckResult} ?= check_registration_internal(PublicKey, Wallet, Opts),
        AlreadyRegistered = hb_ao:get(<<"registered">>, CheckResult, false, Opts),
        
        % Handle registration based on current status
        Result ?= case AlreadyRegistered of
            true ->
                case MultipleWallet of
                    <<"allowed">> ->
                        % Allow adding another wallet to existing public key
                        ?event(attestation_register, {adding_wallet, {public_key, PublicKey}, {wallet, Wallet}}),
                        do_register(PublicKey, Wallet, SignedHash, MultipleWallet, Opts);
                    _ ->
                        {error, <<"Device already registered and multiple_wallet not allowed">>}
                end;
            false ->
                % New registration
                ?event(attestation_register, {new_registration, {public_key, PublicKey}, {wallet, Wallet}}),
                do_register(PublicKey, Wallet, SignedHash, MultipleWallet, Opts)
        end,
        ?event(attestation_register, {result, Result}),
        Result
    else
        {error, Reason} -> {error, Reason};
        Error -> {error, Error}
    end.

%% @doc Check if a device is registered.
%%
%% This function checks registration status for a public key and optionally
%% a specific wallet. If wallet is not provided, checks if public key exists
%% with any wallet.
%%
%% @param _Base Ignored parameter
%% @param Req The check request message
%% @param Opts Configuration options
%% @returns `{ok, Map}' with registration status, or `{error, Reason}' on failure
-spec check_registration(_Base :: term(), Req :: map(), Opts :: map()) ->
    {ok, map()} | {error, term()}.
check_registration(_Base, Req, Opts) ->
    ?event(attestation_check, {request, Req}),
    maybe
        {ok, PublicKey} ?= extract_required(<<"public-key">>, Req, Opts),
        Wallet = hb_ao:get(<<"wallet">>, Req, <<>>, Opts),
        
        % Check registration
        {ok, Result} ?= case Wallet of
            <<>> ->
                % Check if public key exists (any wallet)
                check_registration_internal(PublicKey, undefined, Opts);
            _ ->
                % Check specific wallet + public key combination
                check_registration_internal(PublicKey, Wallet, Opts)
        end,
        ?event(attestation_check, {result, Result}),
        {ok, Result}
    else
        {error, Reason} -> {error, Reason};
        Error -> {error, Error}
    end.

%% @doc Generate an attestation challenge with nonce.
%%
%% This function:
%% 1. Checks if device is registered
%% 2. Gets or creates user's nonce process
%% 3. Generates a unique nonce
%% 4. Returns challenge with nonce
%%
%% @param _Base Ignored parameter
%% @param Req The attestation request message
%% @param Opts Configuration options
%% @returns `{ok, Map}' with nonce and challenge, or `{error, Reason}' on failure
-spec attest(_Base :: term(), Req :: map(), Opts :: map()) ->
    {ok, map()} | {error, term()}.
attest(_Base, Req, Opts) ->
    ?event(attestation_attest, {request, Req}),
    maybe
        % Extract required fields
        {ok, Wallet} ?= extract_required(<<"wallet">>, Req, Opts),
        {ok, PublicKey} ?= extract_required(<<"public-key">>, Req, Opts),
        {ok, CodeHash} ?= extract_required(<<"code-hash">>, Req, Opts),
        
        % 1. CHECK REGISTRATION FIRST
        {ok, RegistrationStatus} ?= check_registration_internal(PublicKey, Wallet, Opts),
        IsRegistered = hb_ao:get(<<"registered">>, RegistrationStatus, false, Opts),
        true ?= IsRegistered orelse {error, <<"Device not registered. Please register first.">>},
        
        % 2. Get or create user's nonce process
        {ok, UserProcessID} ?= get_or_create_user_process(Wallet, Opts),
        ?event(attestation_attest, {user_process, UserProcessID}),
        
        % 3. Generate nonce via user's process
        {ok, NonceResult} ?= generate_nonce(UserProcessID, Req, Opts),
        Nonce = hb_ao:get(<<"nonce">>, NonceResult, Opts),
        true ?= (Nonce =/= not_found) orelse {error, <<"Failed to generate nonce">>},
        
        % 4. Construct and return challenge
        Challenge = construct_challenge(Wallet, PublicKey, CodeHash, Nonce),
        ?event(attestation_attest, {challenge_generated, {nonce, Nonce}}),
        {ok, #{
            <<"nonce">> => Nonce,
            <<"challenge">> => Challenge,
            <<"registered">> => true
        }}
    else
        {error, Reason} -> {error, Reason};
        Error -> {error, Error}
    end.

%% @doc Verify an attestation response.
%%
%% This function:
%% 1. Checks if device is registered
%% 2. Verifies nonce hasn't been used
%% 3. Verifies signature
%% 4. Marks nonce as used
%%
%% @param _Base Ignored parameter
%% @param Req The verification request message
%% @param Opts Configuration options
%% @returns `{ok, Map}' with verification result, or `{error, Reason}' on failure
-spec verify(_Base :: term(), Req :: map(), Opts :: map()) ->
    {ok, map()} | {error, term()}.
verify(_Base, Req, Opts) ->
    ?event(attestation_verify, {request, Req}),
    maybe
        % Extract required fields
        {ok, Wallet} ?= extract_required(<<"wallet">>, Req, Opts),
        {ok, PublicKey} ?= extract_required(<<"public-key">>, Req, Opts),
        {ok, Nonce} ?= extract_required(<<"nonce">>, Req, Opts),
        {ok, Response} ?= extract_required(<<"response">>, Req, Opts),
        {ok, Signature} ?= extract_required(<<"signature">>, Req, Opts),
        
        % 1. Check registration
        {ok, RegistrationStatus} ?= check_registration_internal(PublicKey, Wallet, Opts),
        IsRegistered = hb_ao:get(<<"registered">>, RegistrationStatus, false, Opts),
        true ?= IsRegistered orelse {error, <<"Device not registered">>},
        
        % 2. Get user's nonce process
        {ok, UserProcessID} ?= get_user_process_id(Wallet, Opts),
        
        % 3. Verify nonce hasn't been used
        {ok, NonceCheck} ?= verify_nonce(UserProcessID, Nonce, Opts),
        IsUsed = hb_ao:get(<<"is_used">>, NonceCheck, true, Opts),
        false ?= IsUsed orelse {error, <<"Nonce already used">>},
        
        % 4. Verify signature
        {ok, SigValid} ?= verify_signature(PublicKey, Response, Signature, Opts),
        true ?= SigValid orelse {error, <<"Invalid signature">>},
        
        % 5. Mark nonce as used
        ok ?= mark_nonce_used(UserProcessID, Nonce, Opts),
        
        ?event(attestation_verify, {verified, true}),
        {ok, #{
            <<"verified">> => true,
            <<"nonce">> => Nonce,
            <<"public-key">> => PublicKey,
            <<"wallet">> => Wallet
        }}
    else
        {error, Reason} -> {error, Reason};
        Error -> {error, Error}
    end.

%%% Internal Helper Functions

%% @doc Extract a required field from request, returning error if not found
extract_required(Key, Req, Opts) ->
    case hb_ao:get(Key, Req, not_found, Opts) of
        not_found ->
            ErrorMsg = <<"Missing required field: ", Key/binary>>,
            {error, ErrorMsg};
        Value ->
            {ok, Value}
    end.

%% @doc Validate registration inputs
validate_registration_inputs(PublicKey, Wallet, SignedHash, MultipleWallet) ->
    % Validate public key is binary and reasonable size
    case is_binary(PublicKey) andalso byte_size(PublicKey) > 0 of
        false -> {error, <<"Invalid public-key format">>};
        true ->
            % Validate wallet is binary and looks like an address
            case is_binary(Wallet) andalso byte_size(Wallet) > 0 of
                false -> {error, <<"Invalid wallet format">>};
                true ->
                    % Validate signed hash is binary
                    case is_binary(SignedHash) andalso byte_size(SignedHash) > 0 of
                        false -> {error, <<"Invalid signed-hash format">>};
                        true ->
                            % Validate multiple_wallet value
                            case MultipleWallet of
                                <<"allowed">> -> ok;
                                <<"not_allowed">> -> ok;
                                _ -> {error, <<"multiple-wallet must be 'allowed' or 'not_allowed'">>}
                            end
                    end
            end
    end.

%% @doc Check registration status internally (calls registration process)
check_registration_internal(PublicKey, Wallet, Opts) ->
    maybe
        {ok, RegistrationProcessID} ?= get_registration_process_id(Opts),
        
        % Build request body
        RequestBody = case Wallet of
            undefined ->
                #{
                    <<"Action">> => <<"InternalCheck">>,
                    <<"PublicKey">> => PublicKey
                };
            _ ->
                #{
                    <<"Action">> => <<"InternalCheck">>,
                    <<"PublicKey">> => PublicKey,
                    <<"Wallet">> => Wallet
                }
        end,
        
        % Call registration process's internal check handler
        {ok, Result} ?= hb_ao:resolve(
            RegistrationProcessID,
            #{
                <<"path">> => <<"compute/now/results">>,
                <<"body">> => RequestBody
            },
            Opts
        ),
        {ok, Result}
    else
        {error, Reason} -> {error, Reason};
        Error -> {error, Error}
    end.

%% @doc Perform registration (calls registration process)
do_register(PublicKey, Wallet, SignedHash, MultipleWallet, Opts) ->
    maybe
        {ok, RegistrationProcessID} ?= get_registration_process_id(Opts),
        
        % Build registration request
        RequestBody = #{
            <<"Action">> => <<"InternalRegister">>,
            <<"PublicKey">> => PublicKey,
            <<"Wallet">> => Wallet,
            <<"SignedHash">> => SignedHash,
            <<"MultipleWallet">> => MultipleWallet,
            <<"Timestamp">> => erlang:system_time(second)
        },
        
        % Call registration process's internal register handler
        {ok, Result} ?= hb_ao:resolve(
            RegistrationProcessID,
            #{
                <<"path">> => <<"schedule">>,
                <<"method">> => <<"POST">>,
                <<"body">> => RequestBody
            },
            Opts
        ),
        {ok, Result}
    else
        {error, Reason} -> {error, Reason};
        Error -> {error, Error}
    end.

%% @doc Get registration process ID from options
get_registration_process_id(Opts) ->
    case hb_opts:get(registration_process_id, undefined, Opts) of
        undefined ->
            {error, <<"Registration process not configured. Set registration_process_id in options.">>};
        ProcessID ->
            {ok, ProcessID}
    end.

%% @doc Get or create user's nonce process
get_or_create_user_process(Wallet, Opts) ->
    case get_user_process_id(Wallet, Opts) of
        {ok, ProcessID} ->
            {ok, ProcessID};
        {error, not_found} ->
            % Auto-create user process
            create_user_nonce_process(Wallet, Opts)
    end.

%% @doc Get user's nonce process ID
get_user_process_id(Wallet, Opts) ->
    UserProcessPrefix = hb_opts:get(user_process_prefix, <<"nonce-">>, Opts),
    UserProcessName = <<UserProcessPrefix/binary, Wallet/binary>>,
    
    % Try to lookup via node-process device or local-name device
    case hb_ao:resolve(
        #{ <<"device">> => <<"local-name@1.0">> },
        #{
            <<"path">> => <<"lookup">>,
            <<"key">> => UserProcessName,
            <<"load">> => true
        },
        Opts
    ) of
        {ok, ProcessID} when is_binary(ProcessID) ->
            {ok, ProcessID};
        {ok, ProcessMsg} when is_map(ProcessMsg) ->
            % Got process message, extract ID
            {ok, hb_message:id(ProcessMsg, all, Opts)};
        {error, not_found} ->
            {error, not_found};
        Error ->
            Error
    end.

%% @doc Create a new user nonce process
create_user_nonce_process(Wallet, Opts) ->
    ?event(attestation, {creating_user_process, {wallet, Wallet}}),
    
    % Create process definition
    ProcessDef = #{
        <<"device">> => <<"process@1.0">>,
        <<"execution-device">> => <<"lua@5.3a">>,
        <<"scheduler-device">> => <<"scheduler@1.0">>,
        <<"module">> => #{
            <<"body">> => get_nonce_process_lua_code(),
            <<"content-type">> => <<"text/x-lua">>
        },
        <<"wallet">> => Wallet
    },
    
    % Commit and spawn the process
    Signed = hb_message:commit(ProcessDef, Opts, <<"httpsig@1.0">>),
    ProcessID = hb_message:id(Signed, signed, Opts),
    
    % Initialize by posting to schedule
    {ok, _} = hb_ao:resolve(
        Signed,
        #{
            <<"path">> => <<"schedule">>,
            <<"method">> => <<"POST">>,
            <<"body">> => Signed
        },
        Opts
    ),
    
    % Register with local-name device
    UserProcessPrefix = hb_opts:get(user_process_prefix, <<"nonce-">>, Opts),
    UserProcessName = <<UserProcessPrefix/binary, Wallet/binary>>,
    
    case hb_ao:resolve(
        #{ <<"device">> => <<"local-name@1.0">> },
        #{
            <<"path">> => <<"set">>,
            <<"key">> => UserProcessName,
            <<"value">> => ProcessID
        },
        Opts
    ) of
        {ok, _} ->
            ?event(attestation, {user_process_created, {process_id, ProcessID}, {wallet, Wallet}}),
            {ok, ProcessID};
        Error ->
            Error
    end.

%% @doc Generate nonce via user's process
generate_nonce(UserProcessID, _Req, Opts) ->
    {ok, NonceResult} = hb_ao:resolve(
        UserProcessID,
        #{
            <<"path">> => <<"schedule">>,
            <<"method">> => <<"POST">>,
            <<"body">> => #{
                <<"Action">> => <<"GenerateNonce">>
            }
        },
        Opts
    ),
    {ok, NonceResult}.

%% @doc Verify nonce hasn't been used
verify_nonce(UserProcessID, Nonce, Opts) ->
    {ok, Result} = hb_ao:resolve(
        UserProcessID,
        #{
            <<"path">> => <<"now/results">>,
            <<"body">> => #{
                <<"Action">> => <<"VerifyNonce">>,
                <<"Nonce">> => Nonce
            }
        },
        Opts
    ),
    {ok, Result}.

%% @doc Mark nonce as used
mark_nonce_used(UserProcessID, Nonce, Opts) ->
    case hb_ao:resolve(
        UserProcessID,
        #{
            <<"path">> => <<"schedule">>,
            <<"method">> => <<"POST">>,
            <<"body">> => #{
                <<"Action">> => <<"MarkNonceUsed">>,
                <<"Nonce">> => Nonce
            }
        },
        Opts
    ) of
        {ok, _} -> ok;
        Error -> Error
    end.

%% @doc Construct challenge from components
construct_challenge(Wallet, PublicKey, CodeHash, Nonce) ->
    % Create a challenge string that includes all components
    ChallengeData = <<
        "wallet:", Wallet/binary,
        "|public-key:", PublicKey/binary,
        "|code-hash:", CodeHash/binary,
        "|nonce:", Nonce/binary
    >>,
    % Return as base64url encoded for easy transmission
    hb_util:encode(ChallengeData).

%% @doc Verify signature using public key
%%
%% This function verifies that the signature was created by the secure element
%% using the public key. The exact verification method depends on your secure
%% element's signature scheme (RSA, ECDSA, etc.).
%%
%% TODO: Implement actual signature verification based on your secure element's
%% signature format. You may need to:
%% 1. Parse the public key format (PEM, DER, etc.)
%% 2. Determine signature algorithm (RSA-PSS, ECDSA, etc.)
%% 3. Construct the message that was signed (may include nonce, response, etc.)
%% 4. Use crypto:verify/5 or ar_wallet:verify/4 based on your scheme
verify_signature(PublicKey, Response, Signature, Opts) ->
    % Construct what should have been signed
    % Adjust this based on your actual signing scheme
    DataToVerify = <<Response/binary, PublicKey/binary>>,
    
    % Decode signature
    try
        SignatureBin = hb_util:decode(Signature),
        
        % TODO: Replace with actual signature verification
        % Example for RSA-PSS-SHA512 (similar to Arweave):
        % case parse_public_key(PublicKey) of
        %     {ok, ParsedKey} ->
        %         ar_wallet:verify(
        %             ParsedKey,
        %             DataToVerify,
        %             SignatureBin,
        %             sha512
        %         );
        %     {error, _} -> {ok, false}
        % end
        
        % For now, basic validation - signature must be present and non-empty
        % In production, implement proper cryptographic verification
        IsValid = (byte_size(SignatureBin) > 0) andalso (byte_size(PublicKey) > 0),
        ?event(attestation, {signature_verification, {is_valid, IsValid}, {note, <<"TODO: Implement actual crypto verification">>}}),
        {ok, IsValid}
    catch
        _:Error ->
            ?event(attestation, {signature_verification_error, Error}),
            {ok, false}
    end.

%% @doc Get Lua code for user nonce process
get_nonce_process_lua_code() ->
    <<"Nonces = Nonces or {}\n"
      "UsedNonces = UsedNonces or {}\n"
      "\n"
      "function generate_unique_nonce()\n"
      "    local nonce = tostring(math.random(1000000000, 9999999999)) .. tostring(os.time())\n"
      "    return nonce\n"
      "end\n"
      "\n"
      "Handlers.add(\"GenerateNonce\", function(msg)\n"
      "    local nonce = generate_unique_nonce()\n"
      "    \n"
      "    -- Check it's not already used\n"
      "    while UsedNonces[nonce] do\n"
      "        nonce = generate_unique_nonce()\n"
      "    end\n"
      "    \n"
      "    -- Store it\n"
      "    Nonces[nonce] = true\n"
      "    \n"
      "    return {nonce = nonce}\n"
      "end)\n"
      "\n"
      "Handlers.add(\"VerifyNonce\", function(msg)\n"
      "    local nonce = msg.Nonce\n"
      "    local isUsed = UsedNonces[nonce] == true\n"
      "    return {is_used = isUsed, nonce = nonce}\n"
      "end)\n"
      "\n"
      "Handlers.add(\"MarkNonceUsed\", function(msg)\n"
      "    local nonce = msg.Nonce\n"
      "    UsedNonces[nonce] = true\n"
      "    Nonces[nonce] = nil  -- Remove from active nonces\n"
      "    return {status = \"marked_used\", nonce = nonce}\n"
      "end)\n">>.

