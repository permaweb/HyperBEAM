%%% @doc Certificate operations for SNP commitment reports.
%%%
%%% This module handles fetching certificates from AMD KDS (Key Distribution
%%% Service) and converting between PEM and DER certificate formats.

%%% Certificates are not cached; each fetch goes to the network.
-module(snp_certificates).
-export([fetch_cert_chain/1, fetch_vcek/6, pem_to_der_chain/1, pem_cert_to_der/1,
         fetch_verification_certificates/6]).
-include("include/hb.hrl").
-include("include/snp_constants.hrl").
-include("include/snp_guids.hrl").

%% @doc Fetches the AMD certificate chain (ASK + ARK) for the given SEV product name.
%% @param SevProdName SEV product name (e.g., "Milan"). Defaults to "Milan" if not provided.
%% @returns {ok, CertChainPEM} on success, {error, Reason} on failure
-spec fetch_cert_chain(SevProdName :: undefined | binary() | string()) -> 
    {ok, binary()} | {error, term()}.
fetch_cert_chain(SevProdName) ->
    Product = normalize_sev_product(SevProdName),
    Path = lists:flatten([?KDS_VCEK_PATH, "/", Product, "/cert_chain"]),
    URL = ?KDS_CERT_SITE ++ Path,
    ?event(snp, {fetch_cert_chain_http_request, #{
        url => URL,
        product => Product
    }}),
    {TimeMicros, Result} = timer:tc(fun() -> do_http_get(URL) end),
    TimeMs = TimeMicros / 1000,
    case Result of
        {ok, CertChainPEM} = SuccessResult ->
            ?event(snp_short, {fetch_cert_chain_success, #{
                size => byte_size(CertChainPEM),
                time_ms => TimeMs
            }}),
            SuccessResult;
        Error ->
            ?event(snp_error, {fetch_cert_chain_error, #{
                operation => <<"fetch_cert_chain">>,
                error => Error,
                url => URL,
                product => Product,
                time_ms => TimeMs,
                suggestion => <<"Check network connectivity and AMD KDS availability. Verify product name is correct (e.g., 'Milan').">>
            }}),
            Error
    end.

%% @doc Fetches the VCEK certificate for the given chip ID and TCB version.
%% @param ChipId 64-byte binary chip ID
%% @param BootloaderSPL Bootloader SPL version (u8, 0-255)
%% @param TeeSPL TEE SPL version (u8, 0-255)
%% @param SnpSPL SNP SPL version (u8, 0-255)
%% @param UcodeSPL Microcode SPL version (u8, 0-255)
%% @param SevProdName Optional SEV product name. Defaults to "Milan".
%% @returns {ok, VcekDER} on success, {error, Reason} on failure
-spec fetch_vcek(ChipId :: binary(), BootloaderSPL :: integer(), 
    TeeSPL :: integer(), SnpSPL :: integer(), UcodeSPL :: integer(),
    SevProdName :: undefined | binary() | string()) -> 
    {ok, binary()} | {error, term()}.
fetch_vcek(ChipId, BootloaderSPL, TeeSPL, SnpSPL, UcodeSPL, SevProdName) ->
    case snp_validation:validate_chip_id(ChipId) of
        {error, Reason} -> {error, {invalid_chip_id, Reason}};
        {ok, ValidChipId} ->
            case snp_validation:validate_spl_values(BootloaderSPL, TeeSPL, SnpSPL, UcodeSPL) of
                {error, Reason} -> {error, Reason};
                ok ->
                    Product = normalize_sev_product(SevProdName),
                    HwId = hb_util:list(hb_util:to_hex(ValidChipId)),
                    Path = lists:flatten([
                        ?KDS_VCEK_PATH, "/", Product, "/", HwId,
                        "?blSPL=", hb_util:list(hb_util:bin(BootloaderSPL)),
                        "&teeSPL=", hb_util:list(hb_util:bin(TeeSPL)),
                        "&snpSPL=", hb_util:list(hb_util:bin(SnpSPL)),
                        "&ucodeSPL=", hb_util:list(hb_util:bin(UcodeSPL))
                    ]),
                    URL = ?KDS_CERT_SITE ++ Path,
                    ?event(snp, {fetch_vcek_http_request, #{
                        url => URL,
                        product => Product,
                        chip_id_hex => HwId,
                        spl_values => #{
                            bootloader => BootloaderSPL,
                            tee => TeeSPL,
                            snp => SnpSPL,
                            ucode => UcodeSPL
                        }
                    }}),
                    {TimeMicros, Result} = timer:tc(fun() -> do_http_get(URL) end),
                    TimeMs = TimeMicros / 1000,
                    case Result of
                        {ok, VcekDER} = SuccessResult ->
                            ?event(snp_short, {fetch_vcek_success, #{
                                size => byte_size(VcekDER),
                                time_ms => TimeMs
                            }}),
                            SuccessResult;
                        Error ->
                            ?event(snp_error, {fetch_vcek_error, #{
                                operation => <<"fetch_vcek">>,
                                error => Error,
                                url => URL,
                                time_ms => TimeMs,
                                suggestion => <<"Check network connectivity and AMD KDS availability. Verify chip ID and SPL values are correct.">>
                            }}),
                            Error
                    end
            end
    end.

%% @doc Convert PEM certificate chain to DER-encoded binary.
%% Parses PEM certificates and concatenates their DER encodings.
%% @param CertChainPEM Binary containing PEM-encoded certificates (ASK + ARK)
%% @returns Binary containing concatenated DER-encoded certificates (ASK DER + ARK DER)
-spec pem_to_der_chain(CertChainPEM :: binary()) -> binary() | {error, term()}.
pem_to_der_chain(CertChainPEM) ->
    % Validate input is binary and appears to be PEM format
    case is_binary(CertChainPEM) andalso byte_size(CertChainPEM) > 0 of
        false ->
            ActualType = snp_util:get_type_name(CertChainPEM),
            ActualSize = case is_binary(CertChainPEM) of
                true -> byte_size(CertChainPEM);
                false -> 0
            end,
            ?event(snp_error, {pem_to_der_chain_invalid_input, #{
                operation => <<"pem_to_der_chain">>,
                actual_type => ActualType,
                actual_size => ActualSize,
                expected => <<"non-empty binary">>
            }}),
            {error, <<"Certificate chain validation failed: expected non-empty binary, got ", 
                ActualType/binary, " of size ", (hb_util:bin(integer_to_list(ActualSize)))/binary,
                ". Ensure the certificate chain is a valid PEM-encoded binary.">>};
        true ->
            % Basic PEM format validation (should start with -----BEGIN)
            case snp_validation:validate_pem_binary(CertChainPEM) of
                {error, Reason} ->
                    Preview = case byte_size(CertChainPEM) > 50 of
                        true -> <<(binary:part(CertChainPEM, 0, 50))/binary, <<"...">>/binary>>;
                        false -> CertChainPEM
                    end,
                    ?event(snp_error, {pem_to_der_chain_invalid_format, #{
                        operation => <<"pem_to_der_chain">>,
                        actual_preview => Preview,
                        expected => <<"PEM format starting with '-----BEGIN'">>
                    }}),
                    {error, Reason};
                {ok, _} ->
                    {PemTimeMicros, PemResult} = timer:tc(fun() ->
                        try
                            % Parse PEM certificates using public_key
                            Certs = public_key:pem_decode(CertChainPEM),
                            case length(Certs) of
                                N when N >= ?CERT_CHAIN_MIN_SIZE ->
                                    % Extract certificates and convert to DER format
                                    % Order: ASK first, then ARK (as per SEV spec and PEM order)
                                    DERBinaries = [public_key:der_encode('Certificate', public_key:pem_entry_decode(Cert)) || Cert <- Certs],
                                    % Concatenate DER binaries
                                    << <<DER/binary>> || DER <- DERBinaries >>;
                                ActualCount ->
                                    ?event(snp_error, {pem_to_der_chain_insufficient_certs, #{
                                        operation => <<"pem_to_der_chain">>,
                                        actual_count => ActualCount,
                                        expected_min => ?CERT_CHAIN_MIN_SIZE,
                                        expected_certs => <<"ASK + ARK">>
                                    }}),
                                    {error, <<"Certificate chain validation failed: expected at least ", 
                                        (hb_util:bin(integer_to_list(?CERT_CHAIN_MIN_SIZE)))/binary,
                                        " certificates (ASK + ARK), got ", 
                                        (hb_util:bin(integer_to_list(ActualCount)))/binary,
                                        ". Ensure the certificate chain contains both ASK and ARK certificates.">>}
                            end
                        catch
                            Error:Reason ->
                                ?event(snp_error, {pem_to_der_chain_parse_error, #{
                                    operation => <<"pem_to_der_chain">>,
                                    error => Error,
                                    reason => Reason,
                                    suggestion => <<"Check that the PEM data is valid and properly formatted. Each certificate should be between '-----BEGIN CERTIFICATE-----' and '-----END CERTIFICATE-----' markers.">>
                                }}),
                                {error, {pem_parse_error, Error, Reason}}
                        end
                    end),
                    PemTimeMs = PemTimeMicros / 1000,
                    ?event(snp, {pem_to_der_chain_time_ms, PemTimeMs}),
                    PemResult
            end
    end.

%% @doc Convert a single PEM certificate to DER.
%% @param CertPEM Binary containing PEM-encoded certificate
%% @returns Binary containing DER-encoded certificate
-spec pem_cert_to_der(CertPEM :: binary()) -> binary() | {error, term()}.
pem_cert_to_der(CertPEM) ->
    % Validate input is binary and appears to be PEM format
    case is_binary(CertPEM) andalso byte_size(CertPEM) > 0 of
        false ->
            ActualType = snp_util:get_type_name(CertPEM),
            ActualSize = case is_binary(CertPEM) of
                true -> byte_size(CertPEM);
                false -> 0
            end,
            ?event(snp_error, {pem_cert_to_der_invalid_input, #{
                operation => <<"pem_cert_to_der">>,
                actual_type => ActualType,
                actual_size => ActualSize,
                expected => <<"non-empty binary">>
            }}),
            {error, <<"Certificate validation failed: expected non-empty binary, got ", 
                ActualType/binary, " of size ", (hb_util:bin(integer_to_list(ActualSize)))/binary,
                ". Ensure the certificate is a valid PEM-encoded binary.">>};
        true ->
            % Basic PEM format validation
            case snp_validation:validate_pem_binary(CertPEM) of
                {error, Reason} ->
                    Preview = case byte_size(CertPEM) > 50 of
                        true -> <<(binary:part(CertPEM, 0, 50))/binary, <<"...">>/binary>>;
                        false -> CertPEM
                    end,
                    ?event(snp_error, {pem_cert_to_der_invalid_format, #{
                        operation => <<"pem_cert_to_der">>,
                        actual_preview => Preview,
                        expected => <<"PEM format starting with '-----BEGIN'">>
                    }}),
                    {error, Reason};
                {ok, _} ->
                    try
                        Certs = public_key:pem_decode(CertPEM),
                        case length(Certs) of
                            ?CERT_SINGLE ->
                                [Cert] = Certs,
                                CertDER = public_key:pem_entry_decode(Cert),
                                public_key:der_encode('Certificate', CertDER);
                            0 ->
                                ?event(snp_error, {pem_cert_to_der_no_certs, #{
                                    operation => <<"pem_cert_to_der">>,
                                    actual_count => 0,
                                    expected => <<"exactly 1 certificate">>
                                }}),
                                {error, <<"Certificate parsing failed: PEM data contains no certificates. Ensure the PEM data includes a certificate between '-----BEGIN CERTIFICATE-----' and '-----END CERTIFICATE-----' markers.">>};
                            ActualCount ->
                                ?event(snp_error, {pem_cert_to_der_multiple_certs, #{
                                    operation => <<"pem_cert_to_der">>,
                                    actual_count => ActualCount,
                                    expected => <<"exactly 1 certificate">>,
                                    suggestion => <<"Use pem_to_der_chain/1 for multiple certificates">>
                                }}),
                                {error, <<"Certificate parsing failed: expected exactly 1 certificate, got ", 
                                    (hb_util:bin(integer_to_list(ActualCount)))/binary,
                                    ". For multiple certificates, use pem_to_der_chain/1 instead.">>}
                        end
                    catch
                        Error:Reason ->
                            ?event(snp_error, {pem_cert_to_der_parse_error, #{
                                operation => <<"pem_cert_to_der">>,
                                error => Error,
                                reason => Reason,
                                suggestion => <<"Check that the PEM data is valid and properly formatted. The certificate should be between '-----BEGIN CERTIFICATE-----' and '-----END CERTIFICATE-----' markers.">>
                            }}),
                            {error, {pem_parse_error, Error, Reason}}
                    end
            end
    end.

%% Helper to normalize SEV product name to list format
-spec normalize_sev_product(undefined | binary() | string()) -> string().
normalize_sev_product(undefined) -> ?DEFAULT_SEV_PRODUCT;
normalize_sev_product(<<>>) -> ?DEFAULT_SEV_PRODUCT;
normalize_sev_product("") -> ?DEFAULT_SEV_PRODUCT;
normalize_sev_product(P) when is_binary(P) -> hb_util:list(P);
normalize_sev_product(P) when is_list(P) -> P.

%% Validate SPL values are in valid u8 range (0-255)

%% Internal helper to make HTTP GET requests
%% Uses hb_http_client for consistency with HyperBEAM HTTP infrastructure
-spec do_http_get(URL :: binary() | string()) -> {ok, binary()} | {error, term()}.
do_http_get(URL) when is_list(URL) ->
    do_http_get(hb_util:bin(URL));
do_http_get(URL) when is_binary(URL) ->
    % Validate URL is not empty
    case byte_size(URL) > 0 of
        false ->
            ?event(snp_error, {do_http_get_empty_url, #{
                operation => <<"do_http_get">>,
                actual => <<"empty binary">>,
                expected => <<"non-empty URL string or binary">>
            }}),
            {error, <<"HTTP request failed: URL cannot be empty. Provide a valid URL string or binary.">>};
        true ->
            case uri_string:parse(URL) of
                #{scheme := Scheme, host := Host} = URI ->
                    Port = case Scheme of
                        <<"https">> -> ?HTTP_PORT_HTTPS;
                        "https" -> ?HTTP_PORT_HTTPS;
                        _ -> ?HTTP_PORT_HTTP
                    end,
                    HostBin = hb_util:bin(Host),
                    Peer = case Scheme of
                        <<"https">> -> <<"https://", HostBin/binary, ":", (hb_util:bin(Port))/binary>>;
                        "https" -> <<"https://", HostBin/binary, ":", (hb_util:bin(Port))/binary>>;
                        _ -> <<"http://", HostBin/binary, ":", (hb_util:bin(Port))/binary>>
                    end,
                    Path = hb_maps:get(path, URI, <<"/">>, #{}),
                    Query = hb_maps:get(query, URI, undefined, #{}),
                    FullPath = case Query of
                        undefined -> Path;
                        <<>> -> Path;
                        "" -> Path;
                        Q when is_binary(Q) -> <<Path/binary, "?", Q/binary>>;
                        Q when is_list(Q) -> <<Path/binary, "?", (hb_util:bin(Q))/binary>>
                    end,
                    Request = #{
                        peer => Peer,
                        method => <<"GET">>,
                        path => FullPath,
                        headers => #{},
                        body => <<>>
                    },
                    ?event(snp, {do_http_get_request, #{
                        url => URL,
                        peer => Peer,
                        path => FullPath
                    }}),
                    case hb_http_client:request(Request, #{}) of
                        {ok, ?HTTP_STATUS_OK, _Headers, Body} -> 
                            ?event(snp_short, {do_http_get_success, byte_size(Body)}),
                            {ok, Body};
                        {ok, Status, _Headers, _Body} -> 
                            ?event(snp_error, {do_http_get_status_error, #{
                                operation => <<"do_http_get">>,
                                url => URL,
                                actual_status => Status,
                                expected_status => ?HTTP_STATUS_OK,
                                suggestion => <<"Check if the URL is correct and the server is responding. Status codes: 404=not found, 500=server error, etc.">>
                            }}),
                            {error, {http_error, Status}};
                        {error, Reason} -> 
                            ?event(snp_error, {do_http_get_request_error, #{
                                operation => <<"do_http_get">>,
                                url => URL,
                                error => Reason,
                                suggestion => <<"Check network connectivity, DNS resolution, and firewall settings. Verify the URL is accessible.">>
                            }}),
                            {error, Reason}
                    end;
                Error ->
                    ?event(snp_error, {do_http_get_invalid_url, #{
                        operation => <<"do_http_get">>,
                        url => URL,
                        parse_error => Error,
                        expected => <<"valid URL with scheme and host (e.g., 'https://example.com/path')">>
                    }}),
                    {error, {invalid_url, Error}}
            end
    end;
do_http_get(InvalidURL) ->
    ActualType = case is_binary(InvalidURL) of
        true -> <<"binary">>;
        false -> case is_list(InvalidURL) of
            true -> <<"list">>;
            false -> <<"other">>
        end
    end,
    ?event(snp_error, {do_http_get_invalid_type, #{
        operation => <<"do_http_get">>,
        actual_type => ActualType,
        expected => <<"binary or string (list)">>
    }}),
    {error, <<"HTTP request failed: URL must be a binary or string, got ", 
        ActualType/binary, ". Convert the URL to a binary or string before calling.">>}.

%% @doc Fetch both certificate chain and VCEK for verification.
%% This is a convenience function that fetches both certificates needed for
%% report signature verification in a single call.
%% @param ChipId The chip ID (64 bytes)
%% @param BootloaderSPL Bootloader SPL value (0-255)
%% @param TeeSPL TEE SPL value (0-255)
%% @param SnpSPL SNP SPL value (0-255)
%% @param UcodeSPL Microcode SPL value (0-255)
%% @returns {CertChainPEM, VcekDER} tuple with both certificates
-spec fetch_verification_certificates(ChipId :: binary(), BootloaderSPL :: integer(),
    TeeSPL :: integer(), SnpSPL :: integer(), UcodeSPL :: integer(), NodeOpts :: map()) -> 
    {binary(), binary()}.
fetch_verification_certificates(ChipId, BootloaderSPL, TeeSPL, SnpSPL, UcodeSPL, NodeOpts) ->
    ?event(snp_short, {fetching_cert_chain_start}),
    Family = hb_opts:get(<<"cpu_family">>, undefined, NodeOpts),
    {ok, CertChainPEM} = fetch_cert_chain(Family),
    ?event(snp_short, {cert_chain_fetched, byte_size(CertChainPEM)}),
    
    ?event(snp, {fetching_vcek_start, #{
        chip_id => hb_util:to_hex(ChipId),
        bootloader => BootloaderSPL,
        tee => TeeSPL,
        snp => SnpSPL,
        microcode => UcodeSPL
    }}),
    {ok, VcekDER} = fetch_vcek(ChipId, BootloaderSPL, TeeSPL, SnpSPL, UcodeSPL, Family),
    ?event(snp_short, {vcek_fetched, byte_size(VcekDER)}),
    {CertChainPEM, VcekDER}.

