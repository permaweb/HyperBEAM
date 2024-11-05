%%%-------------------------------------------------------------------
%% @doc supersu public API
%% @end
%%%-------------------------------------------------------------------

-module(ao_app).

-behaviour(application).

-export([start/2, stop/1]).
-export([attest_key/0, verify_attestation/0]).

-include("include/ao.hrl").

-ao_debug(print).

start(_StartType, _StartArgs) ->
    ao_sup:start_link(),
    ok = su_registry:start(),
    _TimestampServer = su_timestamp:start(),
    {ok, _} = ao_http_router:start().

stop(_State) ->
    ok.

attest_key() ->
    W = ao:wallet(),
    Addr = ar_wallet:to_address(W),

    % Pad the address to 32 bytes (64 hex characters) for the TPM nonce
    Nonce = pad_to_size(Addr, 32),
    io:format("Generated TPM Nonce: ~s~n", [Nonce]),  % Print the TPM nonce

    % Pad the address to 64 bytes (128 hex characters) for the TEE nonce
    TeeNonce = pad_to_size(Addr, 64),
    io:format("Generated TEE Nonce: ~s~n", [TeeNonce]),  % Print the TEE nonce

    % Determine tee-technology based on the existence of TEE devices
    TeeTech = case os:cmd("test -e /dev/tdx_guest && echo tdx || (test -e /dev/sev-guest && echo sev-snp)") of
        "tdx\n" -> "tdx";
        "sev-snp\n" -> "sev-snp";
        _ -> {error, "No TEE device found"}
    end,

    % Proceed if a valid TEE technology is found
    case TeeTech of
        {error, _} -> {error, "Required TEE device not found"};
        _ ->
            % Create the output file name as a binary string
            OutputFile = <<Nonce/binary, ".pb">>,  % Concatenate Nonce and ".pb" to create OutputFile as a binary
            Cmd = lists:flatten(io_lib:format("sudo gotpm attest --key AK --nonce ~s --tee-nonce ~s --tee-technology ~s --output ~s --format binarypb", 
                                                [Nonce, TeeNonce, TeeTech, OutputFile])),

            % Print the command for debugging
            io:format("Executing command: ~s~n", [Cmd]),

            CommandResult = os:cmd(Cmd),

            case CommandResult of
                "" ->
                    % If CommandResult is empty, it means the command was successful
                    io:format("Attestation report saved to: ~s~n", [OutputFile]),

                    % Read the generated attestation report from the file
                    {ok, File} = file:open(OutputFile, [read]),
                    {ok, AttestationBinary} = file:read_file(File),
                    file:close(File),

                    % Store the attestation binary in ao_store with Nonce as the key
                    ok = ao_store:write(ao:get(store), Nonce, AttestationBinary),
                    io:format("Attestation report stored in ao_store with key: ~s~n", [Nonce]),

                    % Sign and upload the transaction
                    Signed = ar_bundles:sign_item(
                        #tx{
                            tags = [
                                {<<"Type">>, <<"TEE-Attestation">>},
                                {<<"Address">>, ar_util:id(Addr)}
                            ],
                            data = AttestationBinary
                        },
                        W
                    ),
                    ?c(Signed),
                    ao_client:upload(Signed),
                    ok;
                _ ->
                    {error, "Unexpected output format from gotpm attest command", CommandResult}
            end
    end.

    

verify_attestation() ->
    W = ao:wallet(),
    Addr = ar_wallet:to_address(W),

    % Regenerate the nonce
    Nonce = pad_to_size(Addr, 32),
    TeeNonce = pad_to_size(Addr, 64),

    % Retrieve the attestation report from ao_store using Nonce as the key
    case ao_store:read(ao:get(store), Nonce) of
        {ok, AttestationReport} ->
            TempFile = "/tmp/attestation_report.pb",
            {ok, File} = file:open(TempFile, [write]),
            
            % Write the binary attestation report directly to the file
            file:write(File, AttestationReport), 
            file:close(File),

            % Log the contents of the attestation report
            io:format("Stored attestation report in ~s: ~p~n", [TempFile, AttestationReport]),

            % Read back the attestation report for debugging
            {ok, ReadBack} = file:read_file(TempFile),
            io:format("Read back attestation report: ~p~n", [ReadBack]),

            % Construct the verification command using the temporary file
            Cmd = lists:flatten(io_lib:format("sudo gotpm verify debug --input ~s --nonce ~s --tee-nonce ~s --format binarypb", 
                                                [TempFile, Nonce, TeeNonce])),
            io:format("Verification command: ~s~n", [Cmd]),
            CommandResult = os:cmd(Cmd),

            % Clean up the temporary file and handle the result
            % file:delete(TempFile),
            case CommandResult of
                "" -> {ok, "Attestation report verified successfully"};
                _ -> {error, "Failed to verify attestation report", CommandResult}
            end;

        not_found -> {error, "Attestation report not found in ao_store"};
        {error, Reason} -> {error, "Failed to retrieve attestation report from ao_store", Reason}
    end.



% Pads an address to the specified byte size (in hex characters)
pad_to_size(Addr, SizeInBytes) ->
    HexAddr = binary:encode_hex(Addr),
    RequiredLength = SizeInBytes * 2,  % Convert bytes to hex characters
    Padding = RequiredLength - byte_size(HexAddr),
    lists:duplicate(Padding, $0) ++ HexAddr.

%% internal functions
