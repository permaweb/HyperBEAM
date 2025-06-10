%%% @doc This device offers an interface for validating AMD SEV-SNP commitments,
%%% as well as generating them, if called in an appropriate environment.
-module(dev_snp).
-export([generate/3, verify/3, trusted/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(COMMITTED_PARAMETERS, [vcpus, vcpu_type, vmm_type, guest_features,
    firmware, kernel, initrd, append]).

%%% Test constants
%% Matching commitment report is found in `test/snp-commitment' in 
%% `dev_codec_flat:serialize/1''s format. Alternatively, set the `TEST_NODE'
%% constant to a live node to run the tests against it.
-define(TEST_NODE, undefined).
-define(TEST_TRUSTED_SOFTWARE, #{
    vcpus => 1,
    vcpu_type => 5, 
    vmm_type => 1,
    guest_features => 1,
    firmware =>
        <<
            "b8c5d4082d5738db6b0fb0294174992738645df70c44cdecf7fad3a62244b788e"
                "7e408c582ee48a74b289f3acec78510"
        >>,
    kernel =>
        <<"69d0cd7d13858e4fcef6bc7797aebd258730f215bc5642c4ad8e4b893cc67576">>,
    initrd =>
        <<"853ebf56bc6ba5f08bd5583055a457898ffa3545897bee00103d3066b8766f5c">>,
    append =>
        <<"6cb8a0082b483849054f93b203aa7d98439736e44163d614f79380ca368cc77e">>
}).

real_node_test() ->
    application:ensure_all_started(hb),
    if ?TEST_NODE == undefined ->
        {skip, <<"Test node not set.">>};
    true ->
        {ok, Report} =
            hb_http:get(
                ?TEST_NODE,
                <<"/~snp@1.0/generate">>,
                #{
                    <<"is-trusted-device">> => <<"snp@1.0">>
                }
            ),
        ?event({snp_report_rcvd, Report}),
        ?event({report_verifies, hb_message:verify(Report)}),
        Result =
            verify(
                Report,
                #{ <<"target">> => <<"self">> },
                #{ snp_trusted => [?TEST_TRUSTED_SOFTWARE] }
            ),
        ?event({snp_validation_res, Result}),
        ?assertEqual({ok, true}, Result)
    end.


%% @doc Verify an commitment report message; validating the identity of a 
%% remote node, its ephemeral private address, and the integrity of the report.
%% The checks that must be performed to validate the report are:
%% 1. Verify the address and the node message ID are the same as the ones
%%    used to generate the nonce.
%% 2. Verify the address that signed the message is the same as the one used
%%    to generate the nonce.
%% 3. Verify that the debug flag is disabled.
%% 4. Verify that the firmware, kernel, and OS (VMSAs) hashes, part of the
%%    measurement, are trusted.
%% 5. Verify the measurement is valid.
%% 6. Verify the report's certificate chain to hardware root of trust.
verify(M1, M2, NodeOpts) ->
    ?event(snp_verify, verify_called),
    % Search for a `body' key in the message, and if found use it as the source
    % of the report. If not found, use the message itself as the source.
    MsgWithJSONReport =
        hb_util:ok(
            hb_message:with_only_committed(
                hb_ao:get(<<"body">>, M2, M2, NodeOpts#{ hashpath => ignore }),
                NodeOpts
            )
        ),
    % Normalize the request message
    ReportJSON = hb_ao:get(<<"report">>, MsgWithJSONReport, NodeOpts),
    Report = hb_json:decode(ReportJSON),
    Msg =
        maps:merge(
            maps:without([<<"report">>], MsgWithJSONReport),
            Report
        ),
    % Step 1: Verify the nonce.
    Address = hb_ao:get(<<"address">>, Msg, NodeOpts),
    ?event({snp_address, Address}),
    NodeMsgID =
        case hb_ao:get(<<"node-message">>, Msg, NodeOpts#{ hashpath => ignore }) of
            undefined ->
                case hb_ao:get(<<"node-message-id">>, Msg, NodeOpts) of
                    undefined -> {error, missing_node_msg_id};
                    ID -> ID
                end;
            NodeMsg -> hb_util:ok(dev_message:id(NodeMsg, #{}, NodeOpts))
        end,
    ?event({snp_node_msg_id, NodeMsgID}),
    Nonce = hb_util:decode(hb_ao:get(<<"nonce">>, Msg, NodeOpts)),
    ?event({snp_nonce, Nonce}),
    NonceMatches = report_data_matches(Address, NodeMsgID, Nonce),
    ?event({nonce_matches, NonceMatches}),
    % Step 2: Verify the address and the signature.
    Signers = hb_message:signers(MsgWithJSONReport),
    ?event({snp_signers, {explicit, Signers}}),
    SigIsValid = hb_message:verify(MsgWithJSONReport, Signers),
    ?event({snp_sig_is_valid, SigIsValid}),
    AddressIsValid = lists:member(Address, Signers),
    ?event({address_is_valid, AddressIsValid, {signer, Signers}, {address, Address}}),
    % Step 3: Verify that the debug flag is disabled.
    DebugDisabled = not is_debug(Msg),
    ?event({debug_disabled, DebugDisabled}),
    % Step 4: Verify measurement data (firmware, kernel, OS image) is trusted.
    IsTrustedSoftware = execute_is_trusted(M1, Msg, NodeOpts),
    ?event({trusted_software, IsTrustedSoftware}),
    % Step 5: Verify the measurement against the report's measurement.
    Args =
        maps:from_list(
            lists:map(
                fun({Key, Val}) -> {binary_to_existing_atom(Key), Val} end,
                maps:to_list(
                    maps:with(
                        lists:map(
                            fun atom_to_binary/1,
                            ?COMMITTED_PARAMETERS
                        ),
                        hb_ao:get(<<"local-hashes">>, Msg, NodeOpts)
                    )
                )
            )
        ),
    ?event({args, { explicit, Args}}),
    {ok,Expected} = dev_snp_nif:compute_launch_digest(Args),
    ExpectedBin = list_to_binary(Expected),
    ?event({expected_measurement, ExpectedBin}),
    Measurement = hb_ao:get(<<"measurement">>, Msg, NodeOpts),
    ?event({measurement, {explicit,Measurement}}),
    {Status, MeasurementIsValid} =
        dev_snp_nif:verify_measurement(
            ReportJSON,
            ExpectedBin
        ),
    ?event({status, Status}),
    ?event({measurement_is_valid, MeasurementIsValid}),
    % Step 6: Check the report's integrity.
    {ok, ReportIsValid} = dev_snp_nif:verify_signature(ReportJSON),
    ?event({report_is_valid, ReportIsValid}),
    Valid =
        lists:all(
            fun({ok, Bool}) -> Bool; (Bool) -> Bool end,
            [
                NonceMatches,
                SigIsValid,
                AddressIsValid,
                DebugDisabled,
                IsTrustedSoftware,
                MeasurementIsValid,
                ReportIsValid
            ]
        ),
    ?event({final_validation_result, Valid}),
    {ok, hb_util:bin(Valid)}.

%% @doc Generate an commitment report and emit it as a message, including all of 
%% the necessary data to generate the nonce (ephemeral node address + node
%% message ID), as well as the expected measurement (firmware, kernel, and VMSAs
%% hashes).
generate(_M1, _M2, Opts) ->
    ?event({generate_opts, {explicit, Opts}}),
    Wallet = hb_opts:get(priv_wallet, no_viable_wallet, Opts),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    % ?event({snp_wallet, Wallet}),
    % Remove the `priv*' keys from the options.
    {ok, PublicNodeMsgID} =
        dev_message:id(
                NodeMsg = hb_private:reset(Opts),
                #{ <<"committers">> => <<"none">> },
                Opts
            ),
    RawPublicNodeMsgID = hb_util:native_id(PublicNodeMsgID),
    ?event({snp_node_msg, NodeMsg}),
    ?event({snp_node_msg_id, byte_size(RawPublicNodeMsgID)}),
    ?event({snp_node_msg_id_bin, {explicit, io:format("~p", [RawPublicNodeMsgID])}}),
    % Generate the commitment report.
    ?event({snp_address,  byte_size(Address)}),
    ReportData = generate_nonce(Address, RawPublicNodeMsgID),
    ?event({snp_report_data, byte_size(ReportData)}),

    LocalHashes = hd(hb_opts:get(snp_trusted, [#{}], Opts)),
    ?event(snp_local_hashes, {explicit, LocalHashes}),
    
    {ok, ReportJSON} = dev_snp_nif:generate_attestation_report(ReportData, 1),
    ?event({snp_report_json, ReportJSON}),

    ?event(
        {snp_report_generated,
            {nonce, ReportData},
            {report, ReportJSON}
        }
    ),
    ReportMsg = hb_message:commit(#{
        <<"local-hashes">> => LocalHashes,
        <<"nonce">> => hb_util:encode(ReportData),
        <<"address">> => Address,
        <<"node-message">> => NodeMsg,
        <<"report">> => ReportJSON
    }, Wallet),
    
    ?event({verify_res, hb_message:verify(ReportMsg)}),
    ?event({snp_report_msg, ReportMsg}),
    {ok, ReportMsg}.

%% @doc Ensure that the node's debug policy is disabled.
is_debug(Report) ->
    (hb_ao:get(<<"policy">>, Report, #{}) band (1 bsl 19)) =/= 0.

%% @doc Ensure that all of the software hashes are trusted. The caller may set
%% a specific device to use for the `is-trusted' key. The device must then
%% implement the `trusted' resolver.
execute_is_trusted(M1, Msg, NodeOpts) ->
    % Generate a modified version of the base message, with the 
    % `is-trusted-device' key set as the device, if provided by the caller.
    % If not provided, use the default resolver (this module's `trusted'
    % function).
    ModM1 =
        case hb_ao:get(<<"is-trusted-device">>, M1, NodeOpts) of
            not_found -> M1#{ <<"device">> => <<"snp@1.0">> };
            Device -> {as, Device, M1}
        end,
    LocalHashes = hb_ao:get(<<"local-hashes">>, Msg, NodeOpts),
    Result = lists:all(
        fun(ReportKey) ->
            ?event(trusted, {report_key, {explicit, ReportKey}}),
            ReportVal = hb_ao:get(ReportKey, LocalHashes, NodeOpts),
            ?event(trusted, {report_val, {explicit, ReportVal}}),
            QueryMsg = #{
                <<"path">> => <<"trusted">>,
                <<"key">> => ReportKey,
                <<"body">> => ReportVal
            },
            ?event(trusted, {query_msg, {explicit, QueryMsg}}),
            % ?event({is_trusted_query, {base, ModM1}, {query, QueryMsg}}),
            % Resolve the query message against the modified base message.
            {ok, KeyIsTrusted} = hb_ao:resolve(ModM1, QueryMsg, NodeOpts),
            % ?event(
            %     {is_software_component_trusted,
            %         {key, ReportKey},
            %         {trusted, ReportKey},
            %         {result, KeyIsTrusted}
            %     }
            % ),
            KeyIsTrusted
        end,
        ?COMMITTED_PARAMETERS
    ),
    ?event({is_all_software_trusted, Result}),
    {ok, Result}.

%% @doc Validates if a given message parameter matches a trusted value from the SNP trusted list
%% Returns {ok, true} if the message is trusted, {ok, false} otherwise
trusted(_Msg1, Msg2, NodeOpts) ->
    % Extract the key name to check and the expected value from the message
    Key = hb_ao:get(<<"key">>, Msg2, NodeOpts),
    Body = hb_ao:get(<<"body">>, Msg2, not_found, NodeOpts),
    ?event(trusted, {key, {explicit, Key}}),
    ?event(trusted, {body, {explicit, Body}}),
    %% Get trusted software list from node options
    % This is the set of approved configurations for attestation
    TrustedSoftware = hb_opts:get(snp_trusted, [#{}], NodeOpts),
    %% Check if the value exists in any of the trusted maps in the list
    IsTrusted = 
        case TrustedSoftware of
            % Handle empty trusted software list
            [] -> 
                false;
            % Process list of trusted configurations
            [_|_] when is_list(TrustedSoftware) ->
                % Check if any trusted configuration matches
                lists:any(
                    fun(TrustedMap) ->
                        % Check if this entry is a valid map
                        is_map(TrustedMap) andalso
                        % Get the value for the specified key from the trusted entry
                        case hb_ao:get(Key, TrustedMap, not_found, NodeOpts) of
                            not_found -> false;
                            PropertyName ->
                                ?event(trusted, {property_name, { explicit, PropertyName}}),
                                % Compare to see if it matches the expected value
                                PropertyName == Body
                        end
                    end,
                    TrustedSoftware
                );
            
            % Handle other cases (should not normally happen)
            _ -> false
        end,
    %% Return the trust validation result
    {ok, IsTrusted}.

%% @doc Ensure that the report data matches the expected report data.
report_data_matches(Address, NodeMsgID, ReportData) ->
    ?event({generated_nonce, {explicit, generate_nonce(Address, NodeMsgID)}}),
    ?event({expected_nonce, {explicit, ReportData}}),
    generate_nonce(Address, NodeMsgID) == ReportData.

%% @doc Generate the nonce to use in the commitment report.
generate_nonce(RawAddress, RawNodeMsgID) ->
    Address = hb_util:native_id(RawAddress),
    NodeMsgID = hb_util:native_id(RawNodeMsgID),
    << Address/binary, NodeMsgID/binary >>.

%% Generate an commitment report and emit it via HTTP.
% generate_test() ->
% 	Trusted =
% 		#{
% 			vcpus => 1,
% 			vcpu_type => 5, 
% 			vmm_type => 1,
% 			guest_features => 16#1,
% 			firmware =>
%               "b8c5d4082d5738db6b0fb0294174992738645df70c44cdecf7fad3a62244b7"
%                   "88e7e408c582ee48a74b289f3acec78510",
% 			kernel =>
%               "69d0cd7d13858e4fcef6bc7797aebd258730f215bc5642c4ad8e4b893cc67576",
% 			initrd =>
%               "02e28b6c718bf0a5260d6f34d3c8fe0d71bf5f02af13e1bc695c6bc162120da1",
% 			append =>
%               "56e1e5190622c8c6b9daa4fe3ad83f3831c305bb736735bf795b284cb462c9e7"
% 		},
%     Wallet = ar_wallet:new(),
%     Addr = hb_util:human_id(ar_wallet:to_address(Wallet)),
%     Node = hb_http_server:start_node(
%         #{
%             force_signed => true,
%             priv_wallet => Wallet,
% 			snp_hashes => Trusted
%         }
%     ),
%     {ok, Report} = hb_http:get(Node, <<"/\~snp@1.0/generate">>, #{}),
%     ?event({snp_report_rcvd, Report}),
%     ?assertEqual(Addr, hb_ao:get(<<"address">>, Report, #{})),
% 	ValidationRes = verify(#{ <<"trusted">> => Trusted}, #{ <<"body">> => Report }, #{}),
% 	?event({snp_validation_res, ValidationRes}),
%     ?assertEqual({ok, true}, ValidationRes).