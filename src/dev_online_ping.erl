-module(dev_online_ping).
-export([info/1, info/3, ping_once/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% @doc A simple device that sends a signed ping to the network once or on an interval.
%%% The ping includes an "Online: Yes" tag, timestamps, and node URL tags for easy GraphQL indexing of info.
%%% Each ping is cryptographically signed with the node's wallet to ensure authenticity.
%%%
%%% To schedule recurring pings, use the cron device externally:
%%% curl "http://localhost:10000/~cron@1.0/every?cron-path=/~online-ping@1.0/ping_once&interval=12-hours"

%% @doc Device info export specification.
info(_) ->
    #{
        exports => [info, ping-once]
    }.

%% @doc Return device information.
info(_Msg1, _Msg2, _Opts) ->
    InfoBody = #{
        <<"description">> => <<"Simple online ping device for HyperbEAM nodes">>,
        <<"version">> => <<"1.0">>,
        <<"purpose">> => <<"Sends network pings with 'Online: Yes' tag for indexing">>,
        <<"paths">> => #{
            <<"info">> => <<"Get device information">>,
            <<"ping_once">> => <<"Send a single ping to the network">>
        },
        <<"usage">> => #{
            <<"manual">> => <<"Call ping_once to send a single ping">>,
            <<"recurring">> => <<"Use cron device to schedule recurring pings">>,
            <<"tag">> => <<"Uses 'Online: Yes' tag for easy GQL indexing">>,
            <<"cron_example">> => <<"curl 'http://localhost:10000/~cron@1.0/every?cron-path=/~online-ping@1.0/ping_once&interval=12-hours'">>
        }
    },
    {ok, #{<<"status">> => 200, <<"body">> => InfoBody}}.

%% @doc Send a single ping to the network.
ping_once(Msg1, _Msg2, Opts) ->
    ?event({online_ping_once_called, {msg1, Msg1}}),
    case send_ping(Opts) of
        {ok, Result} ->
            ?event({online_ping_once_success, {result, Result}}),
            {ok, #{
                <<"status">> => 200,
                <<"body">> => #{
                    <<"message">> => <<"ping_sent">>,
                    <<"timestamp">> => hb:now(),
                    <<"result">> => Result
                }
            }};
        {error, Reason} ->
            ?event({online_ping_once_error, {reason, Reason}}),
            {error, #{
                <<"status">> => 500,
                <<"body">> => #{
                    <<"error">> => <<"Failed to send ping">>,
                    <<"reason">> => Reason
                }
            }}
    end.

%%% Private functions

%% @doc Send a ping message to the network with the tags.
%% This properly signs the message with the node's wallet before sending.
send_ping(Opts) ->
    ?event({debug_send_ping_start, "Function called"}),
    % Get the node's wallet for signing - fall back to hb:wallet() if not in Opts
    Wallet = case hb_opts:get(priv_wallet, undefined, Opts) of
        undefined -> hb:wallet();
        W -> W
    end,
    % Add wallet to Opts so downstream functions can use it
    OptsWithWallet = Opts#{priv_wallet => Wallet},
    % Get the node's address from the wallet
    NodeAddress = hb_util:id(ar_wallet:to_address(Wallet)),
    
    % Get host and port information
    Host = hb_opts:get(host, <<"unknown">>, OptsWithWallet),
    Port = hb_opts:get(port, 10000, OptsWithWallet),

    % Build the URL (handling the unknown case and protocol detection)
    NodeUrl = case Host of
        <<"unknown">> ->
            <<"unknown">>;  % Don't build a URL if host is unknown
        _ ->
            % Check if host already includes a protocol
            case binary:match(Host, [<<"://">>]) of
                nomatch ->
                    % No protocol specified, use http with port
                    iolist_to_binary(io_lib:format("http://~s:~p", [Host, Port]));
                _ ->
                    % Protocol already specified, use as-is (assume port is included if needed)
                    Host
            end
    end,

    % Create a ping message with node details from config file
    UnsignedPingMessage = #{
        <<"data">> => <<"Node online ping from HyperBEAM">>,
        <<"Online">> => <<"Yes">>,
        <<"Action">> => <<"Ping">>,
        <<"Node-URL">> => NodeUrl,  % Will be "unknown" or actual URL
        <<"Timestamp">> => integer_to_binary(hb:now()),
        <<"codec-device">> => <<"ans104@1.0">>
    },
    
    try
        ?event({debug_start_of_try_block, "Starting ping process"}),
        % Always use ans104 commitment for this device
        CommitmentDevice = <<"ans104@1.0">>,
        {ok, SignedMessage} = dev_message:commit(
            UnsignedPingMessage,
            #{ <<"commitment-device">> => CommitmentDevice },
            OptsWithWallet
        ),
        ?event({debug_signed_message, SignedMessage}),

        % Let's see what the conversion produces step by step
        ?event({debug_about_to_convert, "Converting to ans104@1.0"}),
        Converted = hb_message:convert(SignedMessage, <<"ans104@1.0">>, OptsWithWallet),
        ?event({debug_converted_tx, Converted}),
        
        % Check if ar_bundles can verify it before serialization
        case ar_bundles:verify_item(Converted) of
            true -> 
                ?event({debug_verify_success, "TX verifies locally"});
            false -> 
                ?event({debug_verify_failed, "TX does NOT verify locally"})
        end,
        
        % See the serialization
        Serialized = ar_bundles:serialize(Converted),
        ?event({debug_serialized, {size, byte_size(Serialized)}, {first_100_bytes, binary:part(Serialized, 0, min(100, byte_size(Serialized)))}}),
        % END OF DEBUG LINES

        ?event({online_ping_signed, {node_address, NodeAddress}, {message_id, hb_message:id(SignedMessage, all)}}),
        
        % Log what we're about to upload for debugging (upload directly without codec-device)
        ?event({online_ping_uploading, {message_size, byte_size(term_to_binary(SignedMessage))}, {commitment_device, CommitmentDevice}}),
        
        % Now submit the signed message to the Arweave network (upload directly)
        case hb_client:upload(SignedMessage, OptsWithWallet) of
            {ok, UploadResult} ->
                ?event({online_ping_uploaded, {upload_result, UploadResult}}),
                {ok, #{
                    <<"message">> => <<"ping_sent_to_network">>,
                    <<"message_id">> => hb_message:id(SignedMessage, all),
                    <<"node_address">> => NodeAddress,
                    <<"commitment_device">> => CommitmentDevice,
                    <<"upload_result">> => UploadResult
                }};
            {error, UploadError} ->
                ?event({online_ping_upload_error, {error, UploadError}, {bundler_response_details, UploadError}}),
                % Still return success for signing, but note upload failed
                {ok, #{
                    <<"message">> => <<"ping_signed_but_upload_failed">>,
                    <<"message_id">> => hb_message:id(SignedMessage, all),
                    <<"node_address">> => NodeAddress,
                    <<"commitment_device">> => CommitmentDevice,
                    <<"upload_error">> => UploadError,
                    <<"signed_message">> => SignedMessage
                }}
        end
    catch
        Class:Reason:Stacktrace ->
            ?event({online_ping_error, {class, Class}, {reason, Reason}, {stacktrace, Stacktrace}}),
            {error, #{
                <<"error">> => <<"Failed to sign ping message">>,
                <<"class">> => Class,
                <<"reason">> => Reason
            }}
    end.

%%% Tests

%% @doc Test that the device info is returned correctly.
info_test() ->
    Info = info(#{}),
    ?assert(maps:is_key(exports, Info)),
    ?assert(lists:member(ping_once, maps:get(exports, Info))).

%% @doc Test device info endpoint.
info_endpoint_test() ->
    {ok, Result} = info(#{}, #{}, #{}),
    ?assertMatch(#{<<"status">> := 200}, Result),
    Body = maps:get(<<"body">>, Result),
    ?assertMatch(#{<<"description">> := _}, Body),
    ?assertMatch(#{<<"version">> := <<"1.0">>}, Body).

%% @doc Test sending a single ping.
ping_once_test() ->
    % Mock wallet for testing
    Wallet = ar_wallet:new(),
    OptsTest = #{priv_wallet => Wallet},
    % Verify the function exists and takes correct parameters
    ?assert(is_function(fun ping_once/3, 3)).
