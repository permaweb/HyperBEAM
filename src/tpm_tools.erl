-module(tpm_tools).
-export([setup_keys/0, generate_attestation/0, get_attestation_info/0]).

-include("include/ao.hrl").
-ao_debug(print).

%% Step 1: Set up primary and attestation keys
setup_keys() ->
    ?c("Starting key setup..."),
    %% Create primary key (primary.ctx)
    CommandPrimary = "/usr/bin/tpm2_createprimary -C e -g sha256 -G rsa -c /tmp/primary.ctx",
    ?c({"Running command", CommandPrimary}),
    case run_command(CommandPrimary) of
        {ok, Output} ->
            ?c({"Primary key created successfully", Output}),
            if_error_in_output(Output, fun () -> 
                %% Create attestation key (ak.pub, ak.priv)
                CommandCreateAK = "/usr/bin/tpm2_create -C /tmp/primary.ctx -G rsa -u /tmp/ak.pub -r /tmp/ak.priv",
                ?c({"Running command", CommandCreateAK}),
                case run_command(CommandCreateAK) of
                    {ok, OutputAK} ->
                        ?c({"Attestation key created successfully", OutputAK}),
                        if_error_in_output(OutputAK, fun () ->
                            %% Load attestation key (ak.ctx)
                            CommandLoadAK = "/usr/bin/tpm2_load -C /tmp/primary.ctx -u /tmp/ak.pub -r /tmp/ak.priv -c /tmp/ak.ctx",
                            ?c({"Running command", CommandLoadAK}),
                            case run_command(CommandLoadAK) of
                                {ok, OutputLoad} -> 
                                    ?c({"Attestation key loaded successfully", OutputLoad}),
                                    if_error_in_output(OutputLoad, fun() -> 
                                        %% Export the AK public key to ak.pub
                                        export_ak_public_key()
                                    end, {error, {failed_to_load_ak, OutputLoad}});
                                Error -> Error
                            end
                        end, {error, {failed_to_create_ak, OutputAK}});
                    Error -> Error
                end
            end, {error, {failed_to_create_primary, Output}});
        Error -> Error
    end.

%% Helper to export the AK public key
export_ak_public_key() ->
    Command = "tpm2_readpublic -c /tmp/ak.ctx -o /tmp/ak.pub -f pem",
    ?c({"Running command", Command}),
    case run_command(Command) of
        {ok, Output} -> 
            if_error_in_output(Output, fun() -> 
                ?c("AK public key exported successfully"),
                ok 
            end, {error, {failed_to_export_ak, Output}});
        Error -> Error
    end.

%% Generate an attestation using the wallet address as the nonce
generate_attestation() ->
    W = ao:wallet(),
    Addr = ar_wallet:to_address(W),

    %% Convert the address to base64url format with manual replacements for `+`, `/`, and `=`
    Base64Addr = base64:encode(Addr),
    HumanReadableAddr = re:replace(re:replace(Base64Addr, "\\+", "-", [global, {return, list}]), "/", "_", [global, {return, list}]),
    HumanReadableAddrNoPad = re:replace(HumanReadableAddr, "=+$", "", [global, {return, list}]),
    ?c({"Retrieved Arweave wallet address (base64url, no padding)", HumanReadableAddrNoPad}),

    %% Use the address in hex format as the nonce
    NonceHex = binary_to_list(binary:encode_hex(Addr)),
    Command = build_quote_command(NonceHex),
    ?c({"Running command", Command}),

    case run_command(Command) of
        {ok, Output} -> 
            Result = if_error_in_output(Output, 
                fun () -> 
                    Data = parse_quote(Output),
                    ?c({"Attestation generated, binary data:", Data}),
                    HumanReadable = debug_human_readable_quote(),
                    ?c({"Debug - Human-readable Quote:", HumanReadable}),
                    {ok, Data}
                end,
                {error, {failed, Output}}),
            Result;
        Error -> Error
    end.
    
%% Provide the necessary information for remote verification
get_attestation_info() ->
    ?c("Gathering attestation info for remote verification"),
    case file:read_file("/tmp/quote.msg") of
        {ok, QuoteData} ->
            ?c({"Quote data read successfully", QuoteData}),
            case file:read_file("/tmp/quote.sig") of
                {ok, SignatureData} ->
                    ?c({"Signature data read successfully", SignatureData}),
                    case file:read_file("/tmp/ak.pub") of
                        {ok, AkPubData} ->
                            ?c({"AK public key data read successfully", AkPubData}),
                            {ok, #{quote => QuoteData, signature => SignatureData, ak_pub => AkPubData}};
                        {error, Reason} -> 
                            ?c({"Failed to read AK public key file", Reason}),
                            {error, {failed_to_read_ak_pub, Reason}}
                    end;
                {error, Reason} -> 
                    ?c({"Failed to read signature file", Reason}),
                    {error, {failed_to_read_signature, Reason}}
            end;
        {error, Reason} -> 
            ?c({"Failed to read quote file", Reason}),
            {error, {failed_to_read_quote, Reason}}
    end.

%% Helper function to run tpm2_print for human-readable debug output of the quote file
debug_human_readable_quote() ->
    Command = "tpm2_print -t TPMS_ATTEST /tmp/quote.msg",
    ?c({"Running command", Command}),
    os:cmd(Command).

%% Helper function to run a shell command and handle errors
run_command(Command) ->
    ?c({"Executing command", Command}),
    Output = os:cmd(Command),
    ?c({"Command output", Output}),
    {ok, list_to_binary(Output)}.

%% Helper to build the quote command for the given nonce
build_quote_command(NonceHex) ->
    Command = "/usr/bin/tpm2_quote -Q --key-context /tmp/ak.ctx -l sha256:0,1 " ++
              "--message /tmp/quote.msg --signature /tmp/quote.sig --qualification " ++ NonceHex,
    ?c({"Built quote command", Command}),
    Command.

%% Helper to parse the output from tpm2_quote and read quote data
parse_quote(_) ->
    ?c("Parsing quote file"),
    case file:read_file("/tmp/quote.msg") of
        {ok, Data} -> 
            ?c({"Quote data parsed successfully", Data}),
            Data;
        {error, Reason} -> 
            ?c({"Error reading quote file", Reason}),
            <<>> % Return empty binary if there’s an error
    end.

%% Helper to check for "ERROR" in output and proceed or return error
if_error_in_output(Output, SuccessFun, ErrorValue) ->
    case binary:match(Output, <<"ERROR">>) of
        nomatch -> SuccessFun();
        _ -> 
            ?c({"Error detected in command output", Output}),
            ErrorValue
    end.
