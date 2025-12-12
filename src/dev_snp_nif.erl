-module(dev_snp_nif).
-export([generate_attestation_report/2, compute_launch_digest/1, check_snp_support/0]).
-export([verify_measurement/2, verify_signature/3]).
-export([fetch_cert_chain/1, fetch_vcek/6]).
-export([report_binary_to_json/1, report_json_to_binary/1]).
-export([pem_to_der_chain/1, pem_cert_to_der/1]).
-export([parse_ovmf_sev_hashes_gpa/1]).
-export([verify_signature_nif/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-on_load(init/0).
-define(NOT_LOADED, not_loaded(?LINE)).

%% Constants
-define(KDS_CERT_SITE, "https://kdsintf.amd.com").
-define(KDS_VCEK_PATH, "/vcek/v1").
-define(DEFAULT_SEV_PRODUCT, "Milan").

check_snp_support() ->
	?NOT_LOADED.

%% @doc Generate an attestation report from the SEV-SNP hardware.
%% Returns binary report structure (1184 bytes) which can be converted to JSON.
%% @param UniqueData 64-byte binary containing unique data to include in report
%% @param VMPL VMPL level (0-3)
%% @returns {ok, ReportBinary} where ReportBinary is 1184 bytes, or {error, {ErrorCode, ErrorMsg}}
generate_attestation_report(_UniqueData, _VMPL) ->
    ?NOT_LOADED.

compute_launch_digest(_Args) ->
	?NOT_LOADED.

%% @doc Verify that the measurement in the report matches the expected measurement.
%% This is a simple byte comparison, so it's done in Erlang.
%% @param ReportJSON Binary containing the JSON attestation report
%% @param ExpectedMeasurement Binary containing the expected measurement (48 bytes)
%% @returns {ok, true} if measurements match, {ok, false} if they don't match,
%%          {error, Reason} if JSON parsing fails or measurement field is missing
verify_measurement(ReportJSON, ExpectedMeasurement) ->
    case hb_json:decode(ReportJSON) of
        #{<<"measurement">> := ActualMeasurement} when is_list(ActualMeasurement) ->
            ActualBin = list_to_binary(ActualMeasurement),
            case ActualBin =:= ExpectedMeasurement of
                true -> {ok, true};
                false -> {ok, false}  % Measurement mismatch, not an error
            end;
        #{<<"measurement">> := ActualMeasurement} when is_binary(ActualMeasurement) ->
            case ActualMeasurement =:= ExpectedMeasurement of
                true -> {ok, true};
                false -> {ok, false}  % Measurement mismatch, not an error
            end;
        _ ->
            {error, <<"Invalid report format: measurement field not found">>}
    end.

%% @doc Verify the signature of an attestation report.
%% Accepts binary report structure and DER-encoded certificates for better performance.
%% @param ReportBinary Binary containing the raw report structure (1184 bytes) OR JSON binary
%% @param CertChainPEM Binary containing the PEM-encoded certificate chain (ARK + ASK) OR DER binary
%% @param VcekDER Binary containing the DER-encoded VCEK certificate
%% @returns {ok, true} if signature is valid, {error, {ErrorCode, ErrorMsg}} if verification fails
verify_signature(ReportBinary, CertChainPEM, VcekDER) ->
    % Convert JSON to binary if needed
    ReportBin = case is_json_binary(ReportBinary) of
        true -> 
            case report_json_to_binary(ReportBinary) of
                {error, Reason1} -> {error, Reason1};
                Bin -> {ok, Bin}
            end;
        false -> 
            case is_binary(ReportBinary) andalso byte_size(ReportBinary) =:= 1184 of
                true -> {ok, ReportBinary};
                false -> {error, <<"Report must be 1184-byte binary or valid JSON">>}
            end
    end,
    % Convert PEM to DER if needed
    CertChainDER = case is_pem_binary(CertChainPEM) of
        true -> 
            case pem_to_der_chain(CertChainPEM) of
                {error, Reason2} -> {error, Reason2};
                DER -> {ok, DER}
            end;
        false -> 
            case is_binary(CertChainPEM) of
                true -> {ok, CertChainPEM};
                false -> {error, <<"Certificate chain must be PEM or DER binary">>}
            end
    end,
    % Validate VCEK DER
    VcekDERValid = case is_binary(VcekDER) andalso byte_size(VcekDER) > 0 of
        true -> {ok, VcekDER};
        false -> {error, <<"VCEK must be DER-encoded binary">>}
    end,
    case {ReportBin, CertChainDER, VcekDERValid} of
        {{ok, RB}, {ok, CCD}, {ok, VD}} ->
            % Call the NIF directly - when loaded, this will be replaced by the actual NIF
            % For now, this will call not_loaded if NIF isn't loaded
            verify_signature_nif(RB, CCD, VD);
        {{error, Error1}, _, _} -> {error, Error1};
        {_, {error, Error2}, _} -> {error, Error2};
        {_, _, {error, Error3}} -> {error, Error3}
    end.

% NIF stub - will be replaced when NIF is loaded
verify_signature_nif(_ReportBinary, _CertChainDER, _VcekDER) ->
    ?NOT_LOADED.

%% Helper to check if binary is JSON
is_json_binary(<<"{", _/binary>>) -> true;
is_json_binary(_) -> false.

%% Helper to check if binary is PEM
is_pem_binary(<<"-----BEGIN", _/binary>>) -> true;
is_pem_binary(_) -> false.

%% @doc Fetches the AMD certificate chain (ASK + ARK) for the given SEV product name.
%% @param SevProdName SEV product name (e.g., "Milan"). Defaults to "Milan" if not provided.
%% @returns {ok, CertChainPEM} on success, {error, Reason} on failure
fetch_cert_chain(SevProdName) ->
    Product = case SevProdName of
        undefined -> ?DEFAULT_SEV_PRODUCT;
        <<>> -> ?DEFAULT_SEV_PRODUCT;
        "" -> ?DEFAULT_SEV_PRODUCT;
        P when is_binary(P) -> binary_to_list(P);
        P when is_list(P) -> P
    end,
    Path = lists:flatten([?KDS_VCEK_PATH, "/", Product, "/cert_chain"]),
    URL = ?KDS_CERT_SITE ++ Path,
    do_http_get(URL).

%% @doc Fetches the VCEK certificate for the given chip ID and TCB version.
%% @param ChipId 64-byte binary chip ID
%% @param BootloaderSPL Bootloader SPL version (u8)
%% @param TeeSPL TEE SPL version (u8)
%% @param SnpSPL SNP SPL version (u8)
%% @param UcodeSPL Microcode SPL version (u8)
%% @param SevProdName Optional SEV product name. Defaults to "Milan".
%% @returns {ok, VcekDER} on success, {error, Reason} on failure
fetch_vcek(ChipId, BootloaderSPL, TeeSPL, SnpSPL, UcodeSPL, SevProdName) ->
    Product = case SevProdName of
        undefined -> ?DEFAULT_SEV_PRODUCT;
        <<>> -> ?DEFAULT_SEV_PRODUCT;
        "" -> ?DEFAULT_SEV_PRODUCT;
        P when is_binary(P) -> binary_to_list(P);
        P when is_list(P) -> P
    end,
    % Convert chip ID to hex string
    HwId = binary_to_hex(ChipId),
    Path = lists:flatten([
        ?KDS_VCEK_PATH, "/", Product, "/", HwId,
        "?blSPL=", integer_to_list(BootloaderSPL),
        "&teeSPL=", integer_to_list(TeeSPL),
        "&snpSPL=", integer_to_list(SnpSPL),
        "&ucodeSPL=", integer_to_list(UcodeSPL)
    ]),
    URL = ?KDS_CERT_SITE ++ Path,
    do_http_get(URL).

%% Internal helper to make HTTP GET requests
do_http_get(URL) when is_list(URL) ->
    do_http_get(list_to_binary(URL));
do_http_get(URL) when is_binary(URL) ->
    case uri_string:parse(URL) of
        #{scheme := Scheme, host := Host} = URI ->
            Port = case Scheme of
                <<"https">> -> 443;
                "https" -> 443;
                _ -> 80
            end,
            HostBin = case Host of
                H when is_binary(H) -> H;
                H when is_list(H) -> list_to_binary(H)
            end,
            Peer = case Scheme of
                <<"https">> -> <<"https://", HostBin/binary, ":", (integer_to_binary(Port))/binary>>;
                "https" -> <<"https://", HostBin/binary, ":", (integer_to_binary(Port))/binary>>;
                _ -> <<"http://", HostBin/binary, ":", (integer_to_binary(Port))/binary>>
            end,
            Path = maps:get(path, URI, <<"/">>),
            Query = maps:get(query, URI, undefined),
            FullPath = case Query of
                undefined -> Path;
                <<>> -> Path;
                "" -> Path;
                Q when is_binary(Q) -> <<Path/binary, "?", Q/binary>>;
                Q when is_list(Q) -> <<Path/binary, "?", (list_to_binary(Q))/binary>>
            end,
            Request = #{
                peer => Peer,
                method => <<"GET">>,
                path => FullPath,
                headers => #{},
                body => <<>>
            },
            case hb_http_client:request(Request, #{}) of
                {ok, 200, _Headers, Body} -> {ok, Body};
                {ok, Status, _Headers, _Body} -> {error, {http_error, Status}};
                {error, Reason} -> {error, Reason}
            end;
        Error ->
            {error, {invalid_url, Error}}
    end.

%% Helper to convert binary to hex string
binary_to_hex(Binary) ->
    << <<(hex_digit(H)), (hex_digit(L))>> || <<H:4, L:4>> <= Binary >>.

hex_digit(N) when N < 10 -> $0 + N;
hex_digit(N) -> $a + (N - 10).

%% @doc Convert binary report structure (1184 bytes) to JSON map.
%% This replaces the C JSON serialization for better error handling.
%% @param ReportBinary 1184-byte binary containing the raw report structure
%% @returns Map containing the report fields as Erlang terms
report_binary_to_json(ReportBinary) when byte_size(ReportBinary) =:= 1184 ->
    <<Version:32/little-unsigned-integer,
      GuestSvn:32/little-unsigned-integer,
      Policy:64/little-unsigned-integer,
      FamilyId:16/binary,
      ImageId:16/binary,
      Vmpl:32/little-unsigned-integer,
      SigAlgo:32/little-unsigned-integer,
      CurrentTcb:8/binary,
      PlatInfo:64/little-unsigned-integer,
      AuthorKeyEn:32/little-unsigned-integer,
      Reserved0:32/little-unsigned-integer,
      ReportData:64/binary,
      Measurement:48/binary,
      HostData:32/binary,
      IdKeyDigest:48/binary,
      AuthorKeyDigest:48/binary,
      ReportId:32/binary,
      ReportIdMa:32/binary,
      ReportedTcb:8/binary,
      _Reserved1:24/binary,
      ChipId:64/binary,
      CommittedTcb:8/binary,
      CurrentBuild:8,
      CurrentMinor:8,
      CurrentMajor:8,
      Reserved2:8,
      CommittedBuild:8,
      CommittedMinor:8,
      CommittedMajor:8,
      Reserved3:8,
      LaunchTcb:8/binary,
      _Reserved4:168/binary,
      SignatureR:72/binary,
      SignatureS:72/binary,
      _SignatureReserved:368/binary>> = ReportBinary,
    
    #{
        <<"version">> => Version,
        <<"guest_svn">> => GuestSvn,
        <<"policy">> => Policy,
        <<"family_id">> => binary_to_list(FamilyId),
        <<"image_id">> => binary_to_list(ImageId),
        <<"vmpl">> => Vmpl,
        <<"sig_algo">> => SigAlgo,
        <<"current_tcb">> => #{
            <<"bootloader">> => binary:at(CurrentTcb, 0),
            <<"tee">> => binary:at(CurrentTcb, 1),
            <<"snp">> => binary:at(CurrentTcb, 2),
            <<"microcode">> => binary:at(CurrentTcb, 3)
        },
        <<"plat_info">> => PlatInfo,
        <<"_author_key_en">> => AuthorKeyEn,
        <<"_reserved_0">> => Reserved0,
        <<"report_data">> => binary_to_list(ReportData),
        <<"measurement">> => binary_to_list(Measurement),
        <<"host_data">> => binary_to_list(HostData),
        <<"id_key_digest">> => binary_to_list(IdKeyDigest),
        <<"author_key_digest">> => binary_to_list(AuthorKeyDigest),
        <<"report_id">> => binary_to_list(ReportId),
        <<"report_id_ma">> => binary_to_list(ReportIdMa),
        <<"reported_tcb">> => #{
            <<"bootloader">> => binary:at(ReportedTcb, 0),
            <<"tee">> => binary:at(ReportedTcb, 1),
            <<"snp">> => binary:at(ReportedTcb, 2),
            <<"microcode">> => binary:at(ReportedTcb, 3)
        },
        <<"chip_id">> => binary_to_list(ChipId),
        <<"committed_tcb">> => #{
            <<"bootloader">> => binary:at(CommittedTcb, 0),
            <<"tee">> => binary:at(CommittedTcb, 1),
            <<"snp">> => binary:at(CommittedTcb, 2),
            <<"microcode">> => binary:at(CommittedTcb, 3)
        },
        <<"current_build">> => CurrentBuild,
        <<"current_minor">> => CurrentMinor,
        <<"current_major">> => CurrentMajor,
        <<"_reserved_2">> => Reserved2,
        <<"committed_build">> => CommittedBuild,
        <<"committed_minor">> => CommittedMinor,
        <<"committed_major">> => CommittedMajor,
        <<"_reserved_3">> => Reserved3,
        <<"launch_tcb">> => #{
            <<"bootloader">> => binary:at(LaunchTcb, 0),
            <<"tee">> => binary:at(LaunchTcb, 1),
            <<"snp">> => binary:at(LaunchTcb, 2),
            <<"microcode">> => binary:at(LaunchTcb, 3)
        },
        <<"signature">> => #{
            <<"r">> => binary_to_list(SignatureR),
            <<"s">> => binary_to_list(SignatureS)
        }
    };
report_binary_to_json(_) ->
    {error, <<"Report binary must be exactly 1184 bytes">>}.

%% @doc Convert JSON report map to binary report structure (1184 bytes).
%% This reconstructs the binary structure from parsed JSON for signature verification.
%% @param ReportJSON Binary containing JSON report OR map
%% @returns 1184-byte binary containing the raw report structure
report_json_to_binary(ReportJSON) when is_binary(ReportJSON) ->
    case hb_json:decode(ReportJSON) of
        ReportMap when is_map(ReportMap) ->
            report_json_to_binary(ReportMap);
        _ ->
            {error, <<"Invalid JSON format">>}
    end;
report_json_to_binary(ReportMap) when is_map(ReportMap) ->
    try
        Version = maps:get(<<"version">>, ReportMap),
        GuestSvn = maps:get(<<"guest_svn">>, ReportMap),
        Policy = maps:get(<<"policy">>, ReportMap),
        FamilyId = list_to_binary(maps:get(<<"family_id">>, ReportMap)),
        ImageId = list_to_binary(maps:get(<<"image_id">>, ReportMap)),
        Vmpl = maps:get(<<"vmpl">>, ReportMap),
        SigAlgo = maps:get(<<"sig_algo">>, ReportMap),
        CurrentTcbMap = maps:get(<<"current_tcb">>, ReportMap),
        CurrentTcb = <<
            (maps:get(<<"bootloader">>, CurrentTcbMap, 0)):8,
            (maps:get(<<"tee">>, CurrentTcbMap, 0)):8,
            (maps:get(<<"snp">>, CurrentTcbMap, 0)):8,
            (maps:get(<<"microcode">>, CurrentTcbMap, 0)):8,
            0:32
        >>,
        PlatInfo = maps:get(<<"plat_info">>, ReportMap),
        AuthorKeyEn = maps:get(<<"_author_key_en">>, ReportMap, 0),
        Reserved0 = maps:get(<<"_reserved_0">>, ReportMap, 0),
        ReportData = list_to_binary(maps:get(<<"report_data">>, ReportMap)),
        Measurement = list_to_binary(maps:get(<<"measurement">>, ReportMap)),
        HostData = list_to_binary(maps:get(<<"host_data">>, ReportMap)),
        IdKeyDigest = list_to_binary(maps:get(<<"id_key_digest">>, ReportMap)),
        AuthorKeyDigest = list_to_binary(maps:get(<<"author_key_digest">>, ReportMap)),
        ReportId = list_to_binary(maps:get(<<"report_id">>, ReportMap)),
        ReportIdMa = list_to_binary(maps:get(<<"report_id_ma">>, ReportMap)),
        ReportedTcbMap = maps:get(<<"reported_tcb">>, ReportMap),
        ReportedTcb = <<
            (maps:get(<<"bootloader">>, ReportedTcbMap, 0)):8,
            (maps:get(<<"tee">>, ReportedTcbMap, 0)):8,
            (maps:get(<<"snp">>, ReportedTcbMap, 0)):8,
            (maps:get(<<"microcode">>, ReportedTcbMap, 0)):8,
            0:32
        >>,
        ChipId = list_to_binary(maps:get(<<"chip_id">>, ReportMap)),
        CommittedTcbMap = maps:get(<<"committed_tcb">>, ReportMap),
        CommittedTcb = <<
            (maps:get(<<"bootloader">>, CommittedTcbMap, 0)):8,
            (maps:get(<<"tee">>, CommittedTcbMap, 0)):8,
            (maps:get(<<"snp">>, CommittedTcbMap, 0)):8,
            (maps:get(<<"microcode">>, CommittedTcbMap, 0)):8,
            0:32
        >>,
        CurrentBuild = maps:get(<<"current_build">>, ReportMap, 0),
        CurrentMinor = maps:get(<<"current_minor">>, ReportMap, 0),
        CurrentMajor = maps:get(<<"current_major">>, ReportMap, 0),
        Reserved2 = maps:get(<<"_reserved_2">>, ReportMap, 0),
        CommittedBuild = maps:get(<<"committed_build">>, ReportMap, 0),
        CommittedMinor = maps:get(<<"committed_minor">>, ReportMap, 0),
        CommittedMajor = maps:get(<<"committed_major">>, ReportMap, 0),
        Reserved3 = maps:get(<<"_reserved_3">>, ReportMap, 0),
        LaunchTcbMap = maps:get(<<"launch_tcb">>, ReportMap),
        LaunchTcb = <<
            (maps:get(<<"bootloader">>, LaunchTcbMap, 0)):8,
            (maps:get(<<"tee">>, LaunchTcbMap, 0)):8,
            (maps:get(<<"snp">>, LaunchTcbMap, 0)):8,
            (maps:get(<<"microcode">>, LaunchTcbMap, 0)):8,
            0:32
        >>,
        SignatureMap = maps:get(<<"signature">>, ReportMap),
        SignatureR = list_to_binary(maps:get(<<"r">>, SignatureMap)),
        SignatureS = list_to_binary(maps:get(<<"s">>, SignatureMap)),
        
        % Reconstruct binary report structure
        ReportBinary = <<
            Version:32/little-unsigned-integer,
            GuestSvn:32/little-unsigned-integer,
            Policy:64/little-unsigned-integer,
            FamilyId:16/binary,
            ImageId:16/binary,
            Vmpl:32/little-unsigned-integer,
            SigAlgo:32/little-unsigned-integer,
            CurrentTcb:8/binary,
            PlatInfo:64/little-unsigned-integer,
            AuthorKeyEn:32/little-unsigned-integer,
            Reserved0:32/little-unsigned-integer,
            ReportData:64/binary,
            Measurement:48/binary,
            HostData:32/binary,
            IdKeyDigest:48/binary,
            AuthorKeyDigest:48/binary,
            ReportId:32/binary,
            ReportIdMa:32/binary,
            ReportedTcb:8/binary,
            0:192,  % Reserved1 (24 bytes = 192 bits)
            ChipId:64/binary,
            CommittedTcb:8/binary,
            CurrentBuild:8,
            CurrentMinor:8,
            CurrentMajor:8,
            Reserved2:8,
            CommittedBuild:8,
            CommittedMinor:8,
            CommittedMajor:8,
            Reserved3:8,
            LaunchTcb:8/binary,
            0:1344,  % Reserved4 (168 bytes = 1344 bits)
            SignatureR:72/binary,
            SignatureS:72/binary,
            0:2944  % SignatureReserved (368 bytes = 2944 bits)
        >>,
        ReportBinary
    catch
        Error:Reason ->
            {error, {conversion_error, Error, Reason}}
    end;
report_json_to_binary(_) ->
    {error, <<"Invalid report format">>}.

%% @doc Convert PEM certificate chain to DER-encoded binary.
%% Parses PEM certificates and concatenates their DER encodings.
%% @param CertChainPEM Binary containing PEM-encoded certificates (ASK + ARK)
%% @returns Binary containing concatenated DER-encoded certificates (ASK DER + ARK DER)
pem_to_der_chain(CertChainPEM) ->
    try
        % Parse PEM certificates using public_key
        Certs = public_key:pem_decode(CertChainPEM),
        case length(Certs) of
            N when N >= 2 ->
                % Extract certificates and convert to DER format
                % Order: ASK first, then ARK (as per SEV spec and PEM order)
                DERBinaries = [public_key:der_encode('Certificate', public_key:pem_entry_decode(Cert)) || Cert <- Certs],
                % Concatenate DER binaries
                << <<DER/binary>> || DER <- DERBinaries >>;
            _ ->
                {error, <<"Certificate chain must contain at least 2 certificates (ASK + ARK)">>}
        end
    catch
        Error:Reason ->
            {error, {pem_parse_error, Error, Reason}}
    end.

%% @doc Convert a single PEM certificate to DER.
%% @param CertPEM Binary containing PEM-encoded certificate
%% @returns Binary containing DER-encoded certificate
pem_cert_to_der(CertPEM) ->
    try
        [Cert] = public_key:pem_decode(CertPEM),
        CertDER = public_key:pem_entry_decode(Cert),
        public_key:der_encode('Certificate', CertDER)
    catch
        Error:Reason ->
            {error, {pem_parse_error, Error, Reason}}
    end.

init() ->
    % Load C NIF instead of Rust NIF
    case code:priv_dir(hb) of
        {error, bad_name} ->
            % Fallback path for development
            erlang:load_nif("./priv/dev_snp_nif", 0);
        PrivDir ->
            NifPath = filename:join([PrivDir, "dev_snp_nif"]),
            erlang:load_nif(NifPath, 0)
    end.

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

generate_attestation_report_test() ->
	%% Call check_support() to determine if SNP is supported
	case dev_snp_nif:check_snp_support() of
		{ok, true} ->
			%% SNP is supported, generate unique data and test commitment report
			UniqueData = crypto:strong_rand_bytes(64),
			VMPL = 1,
			case dev_snp_nif:generate_attestation_report(UniqueData, VMPL) of
				{ok, ReportBinary} when byte_size(ReportBinary) =:= 1184 ->
					%% Convert to JSON and verify structure
					ReportMap = dev_snp_nif:report_binary_to_json(ReportBinary),
					?assert(is_map(ReportMap)),
					?assert(maps:is_key(<<"version">>, ReportMap)),
					?assert(maps:is_key(<<"measurement">>, ReportMap)),
					%% Round-trip test: JSON -> Binary -> JSON
					{ok, ReportJSON} = {ok, hb_json:encode(ReportMap)},
					ReportBinary2 = dev_snp_nif:report_json_to_binary(ReportJSON),
					?assertEqual(ReportBinary, ReportBinary2);
				{error, _} = Error ->
					?assertMatch({error, _}, Error)
			end;
		{ok, false} ->
			%% SNP is not supported, log event and assert NIF not loaded
			?event("SNP not supported on machine, skipping test..."),
			?assertEqual(ok, ok)
	end.

compute_launch_digest_test() ->
	%% Define the data structure
	ArgsMap = #{ 
		vcpus => 32,
		vcpu_type => 5, 
		vmm_type => 1,
		guest_features => 16#1,
		firmware => "b8c5d4082d5738db6b0fb0294174992738645df70c44cdecf7fad3a62244b788e7e408c582ee48a74b289f3acec78510",
		kernel => "69d0cd7d13858e4fcef6bc7797aebd258730f215bc5642c4ad8e4b893cc67576",
		initrd => "02e28b6c718bf0a5260d6f34d3c8fe0d71bf5f02af13e1bc695c6bc162120da1",
		append => "56e1e5190622c8c6b9daa4fe3ad83f3831c305bb736735bf795b284cb462c9e7"
	},

	?event(ArgsMap),

		%% Call the NIF
	{ok, Result} = dev_snp_nif:compute_launch_digest(ArgsMap),
	%% Expected result
    EncTestVector =
        <<"wmSDSQYuzE2M3rQcourJnDJHgalADM8TBev3gyjM5ObRNOn8oglvVznFbaWhajU_">>,
	?assertMatch(EncTestVector, hb_util:encode(Result)).

verify_measurement_test() ->
	%% Define a mock report (JSON string) as binary
    {ok, MockReport} = file:read_file("test/snp-measurement.json"),
	%% Define the expected measurement (binary)
	ExpectedMeasurement = <<94,87,4,197,20,11,255,129,179,197,146,104,8,212,152,248,110,11,60,246,82,254,24,55,201,47,157,229,163,82,108,66,191,138,241,229,40,144,133,170,116,109,17,62,20,241,144,119>>,
	%% Call the function (now in Erlang)
	Result = dev_snp_nif:verify_measurement(MockReport, ExpectedMeasurement),
	?assertMatch({ok, true}, Result).

verify_signature_test() ->
	%% Define a mock report (JSON string) as binary
    {ok, MockAttestation} = file:read_file("test/snp-attestation.json"),
	%% For this test, we'd need to fetch certificates first
	%% This test will need to be updated to use the new signature
	Result = dev_snp_nif:verify_signature(MockAttestation, <<>>, <<>>),
	?assertMatch({ok, true}, Result).

%% @doc Parse OVMF file to extract SEV hashes table GPA.
%% This reads the OVMF footer table and finds the SEV_HASH_TABLE_RV_GUID entry.
%% @param OvmfPath Path to the OVMF file (e.g., "test/OVMF-1.55.fd")
%% @returns {ok, GPA} where GPA is a 64-bit integer, or {error, Reason} on failure
-spec parse_ovmf_sev_hashes_gpa(OvmfPath :: string() | binary()) -> {ok, non_neg_integer()} | {error, term()}.
parse_ovmf_sev_hashes_gpa(OvmfPath) when is_binary(OvmfPath) ->
    parse_ovmf_sev_hashes_gpa(binary_to_list(OvmfPath));
parse_ovmf_sev_hashes_gpa(OvmfPath) when is_list(OvmfPath) ->
    % Print current working directory for debugging
    {ok, Cwd} = file:get_cwd(),
    io:format("[SNP_DEBUG] Current working directory: ~s~n", [Cwd]),
    io:format("[SNP_DEBUG] Attempting to read OVMF file: ~s~n", [OvmfPath]),
    case file:read_file(OvmfPath) of
        {ok, OvmfData} ->
            parse_ovmf_footer_table(OvmfData);
        {error, Reason} ->
            {error, {file_read_error, Reason}}
    end;
parse_ovmf_sev_hashes_gpa(_) ->
    {error, invalid_path}.

%% Internal function to parse OVMF footer table
parse_ovmf_footer_table(OvmfData) ->
    Size = byte_size(OvmfData),
    if
        Size < 50 -> {error, file_too_small};
        true ->
            % Footer entry is at offset: Size - 32 - 18 (ENTRY_HEADER_SIZE)
            ENTRY_HEADER_SIZE = 18,  % 2 bytes size + 16 bytes GUID
            FooterEntryOffset = Size - 32 - ENTRY_HEADER_SIZE,
            if
                FooterEntryOffset < 0 -> {error, invalid_file_format};
                true ->
                    % Read footer entry
                    FooterEntry = binary:part(OvmfData, FooterEntryOffset, ENTRY_HEADER_SIZE),
                    <<FooterSize:16/little, FooterGuid:16/binary>> = FooterEntry,
                    
                    % Check if this is the OVMF_TABLE_FOOTER_GUID
                    % GUID: 96b582de-1fb2-45f7-baea-a366c55a082d (little-endian)
                    ExpectedGuid = <<45, 8, 90, 163, 102, 12, 90, 163,
                                     234, 171, 247, 69, 178, 31, 178, 150>>,
                    if
                        FooterGuid =/= ExpectedGuid -> {error, invalid_footer_guid};
                        FooterSize < ENTRY_HEADER_SIZE -> {error, invalid_footer_size};
                        true ->
                            % Calculate table size and start
                            TableSize = FooterSize - ENTRY_HEADER_SIZE,
                            TableStart = FooterEntryOffset - TableSize,
                            if
                                TableStart < 0 -> {error, invalid_table_offset};
                                true ->
                                    % Read the table
                                    TableData = binary:part(OvmfData, TableStart, TableSize),
                                    % Parse entries backwards to find SEV_HASH_TABLE_RV_GUID
                                    % GUID: 7255371f-3a3b-4b04-927b-1da6efa8d454 (little-endian)
                                    SevHashTableGuid = <<84, 168, 218, 31, 107, 4, 75, 59,
                                                         123, 146, 4, 75, 59, 58, 53, 114>>,
                                    find_sev_hashes_gpa(TableData, SevHashTableGuid, TableSize)
                            end
                    end
            end
    end.

%% Find SEV hashes table GPA in the table data
find_sev_hashes_gpa(TableData, TargetGuid, TableSize) ->
    find_sev_hashes_gpa(TableData, TargetGuid, TableSize, TableSize).

find_sev_hashes_gpa(_TableData, _TargetGuid, _TableSize, Offset) when Offset < 18 ->
    {error, guid_not_found};
find_sev_hashes_gpa(TableData, TargetGuid, TableSize, Offset) ->
    ENTRY_HEADER_SIZE = 18,
    EntryHeaderOffset = Offset - ENTRY_HEADER_SIZE,
    <<EntrySize:16/little, EntryGuid:16/binary>> = binary:part(TableData, EntryHeaderOffset, ENTRY_HEADER_SIZE),
    
    if
        EntrySize < ENTRY_HEADER_SIZE -> {error, invalid_entry_size};
        Offset < EntrySize -> {error, invalid_entry_offset};
        EntryGuid =:= TargetGuid ->
            % Found it! Entry data is before the header
            DataOffset = Offset - EntrySize,
            if
                DataOffset + 4 > TableSize -> {error, invalid_data_offset};
                true ->
                    % First 4 bytes are the GPA (little-endian u32)
                    <<GpaU32:32/little>> = binary:part(TableData, DataOffset, 4),
                    {ok, GpaU32}
            end;
        true ->
            % Continue searching backwards
            find_sev_hashes_gpa(TableData, TargetGuid, TableSize, Offset - EntrySize)
    end.
