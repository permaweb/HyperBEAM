-module(gotpm_tools).
-export([generate_attestation/0, get_attestation_info/0, check_tee/0]).

-include("include/ao.hrl").
-ao_debug(print).

%% Check for TDX or SEV-SNP environment
check_tee() ->
    case os:cmd("test -e /dev/tdx_guest && echo tdx || (test -e /dev/sev-guest && echo sev-snp)") of
        "tdx\n" -> 
            ?c("TDX environment detected"),
            "tdx";
        "sev-snp\n" -> 
            ?c("SEV-SNP environment detected"),
            "sev-snp";
        _ -> 
            ?c("No TEE environment detected"),
            undefined
    end.

%% Generate an attestation using the wallet address as the nonce
generate_attestation() ->
    %% Check for TDX or SEV-SNP and log the result
    TeeEnvironment = check_tee(),
    ?c({"TEE Environment detected for attestation", TeeEnvironment}),

    W = ao:wallet(),
    Addr = ar_wallet:to_address(W),

    %% Convert the address to base64url format with manual replacements for `+`, `/`, and `=`
    Base64Addr = base64:encode(Addr),
    HumanReadableAddr = re:replace(re:replace(Base64Addr, "\\+", "-", [global, {return, list}]), "/", "_", [global, {return, list}]),
    HumanReadableAddrNoPad = re:replace(HumanReadableAddr, "=+$", "", [global, {return, list}]),
    ?c({"Retrieved Arweave wallet address (base64url, no padding)", HumanReadableAddrNoPad}),

    %% Use the address in hex format as the nonce
    NonceHex = binary_to_list(binary:encode_hex(Addr)),

    %% Pad the tee-nonce to 64 bytes (128 hex characters) for both TDX and SEV-SNP
    TeeNonceHex = case TeeEnvironment of
        "tdx" -> pad_to_size(NonceHex, 128);
        "sev-snp" -> pad_to_size(NonceHex, 128);
        _ -> NonceHex % No padding needed for other environments
    end,

    %% Build gotpm attest command
    Command = build_attest_command(NonceHex, TeeNonceHex, TeeEnvironment),
    ?c({"Running command", Command}),

    case run_command(Command) of
        {ok, Output} -> 
            Result = if_error_in_output(Output, 
                fun () -> 
                    Data = parse_quote(),
                    ?c({"Attestation generated, binary data:", Data}),
                    HumanReadable = debug_human_readable_quote(),
                    ?c({"Debug - Human-readable Quote:", HumanReadable}),
                    {ok, Data}
                end,
                {error, {failed, Output}}),
            Result;
        Error -> Error
    end.

%% Helper to build the attest command for the given nonce
build_attest_command(NonceHex, TeeNonceHex, TeeEnvironment) ->
    %% Add TEE technology if present; use TeeNonceHex to meet TDX and SEV-SNP requirement for 64 bytes
    TEEParams = case TeeEnvironment of
                    undefined -> "";
                    _ -> " --tee-nonce " ++ TeeNonceHex ++ " --tee-technology " ++ TeeEnvironment
                end,
    Command = "gotpm attest --key AK --nonce " ++ NonceHex ++ TEEParams ++
              " --output /tmp/quote.msg --format textproto",
    ?c({"Built attest command", Command}),
    Command.

%% Provide the necessary information for remote verification
get_attestation_info() ->
    ?c("Gathering attestation info for remote verification"),
    case file:read_file("/tmp/quote.msg") of
        {ok, QuoteData} ->
            ?c({"Quote data read successfully", QuoteData}),
            case file:read_file("/tmp/ak.pub") of
                {ok, AkPubData} ->
                    ?c({"AK public key data read successfully", AkPubData}),
                    {ok, #{quote => QuoteData, ak_pub => AkPubData}};
                {error, Reason} -> 
                    ?c({"Failed to read AK public key file", Reason}),
                    {error, {failed_to_read_ak_pub, Reason}}
            end;
        {error, Reason} -> 
            ?c({"Failed to read quote file", Reason}),
            {error, {failed_to_read_quote, Reason}}
    end.

%% Helper function to run gotpm for human-readable debug output of the quote file
debug_human_readable_quote() ->
    %% Manually decode or print the binary data in /tmp/quote.msg
    case file:read_file("/tmp/quote.msg") of
        {ok, Data} -> binary_to_list(Data);
        {error, Reason} -> "Failed to read quote file: " ++ io_lib:format("~p", [Reason])
    end.

%% Helper function to run a shell command and handle errors
run_command(Command) ->
    ?c({"Executing command", Command}),
    Output = os:cmd(Command),
    ?c({"Command output", Output}),
    {ok, list_to_binary(Output)}.

%% Helper to parse the output from gotpm attest and read quote data
parse_quote() ->
    ?c("Parsing quote file"),
    case file:read_file("/tmp/quote.msg") of
        {ok, Data} -> 
            ?c({"Quote data parsed successfully", Data}),
            Data;
        {error, Reason} -> 
            ?c({"Error reading quote file", Reason}),
            <<>> % Return empty binary if there’s an error
    end.

%% Helper to pad the nonce to a specific byte size (in hex characters)
pad_to_size(NonceHex, RequiredLength) ->
    Padding = lists:duplicate(RequiredLength - length(NonceHex), $0),
    Padding ++ NonceHex.

%% Helper to check for "ERROR" in output and proceed or return error
if_error_in_output(Output, SuccessFun, ErrorValue) ->
    case binary:match(Output, <<"ERROR">>) of
        nomatch -> SuccessFun();
        _ -> 
            ?c({"Error detected in command output", Output}),
            ErrorValue
    end.
