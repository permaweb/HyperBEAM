-module(hb_spawn_red_ao).
-export([run_test/0]).
-include("include/hb.hrl").

run_test () ->
  Wallet = ar_wallet:new(),
  WalletAddress = hb_util:encode(ar_wallet:to_address(Wallet)),
  Authority = list_to_binary(binary_to_list(WalletAddress) ++ ",fcoN_xJeisVsPXA-trzVAuIiqO3ydLQxM-L4XbrQKzY,jnbRhoH3JGTdRz0Y9X-gh-eosrbIpdxs58DPTtlOVE8"),
  LegacyMsg = #{
        <<"Type">> => <<"Process">>,
        <<"Data-Protocol">> => <<"ao">>,
        <<"Variant">> => <<"ao.TN.1">>,
        <<"Module">> => <<"URgYpPQzvxxfYQtjrIQ116bl3YBfcImo3JEnNo8Hlrk">>,
        <<"Scheduler">> => <<"_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA">>,
        <<"Name">> => <<"[BETA-1.2] Green Zone AO">>,
        <<"data">> => <<"1984">>,
        <<"On-Boot">> => <<"LxLUDGJD2QLnyIVO_boJnUbVnyXjSuV0d90VNCt6I0g">>,
        <<"Authority">> => Authority,
        <<"ParentToken">> => <<"0syT13r0s0tgPmIed95bJnuSqaD29HQNN8D3ElLSrsc">>,
        <<"Ticker">> => <<"BETA-GZ">>,
        <<"Denomination">> => <<"12">>
  },
  
  {ok, TX} = dev_codec_ans104:to(LegacyMsg, #{}, #{}),
  
  SignedTX = ar_bundles:sign_item(TX, Wallet),
  
  ANS104Bytes = ar_bundles:serialize(SignedTX),
  
  LEGACY_URL = "https://mu.ao-testnet.xyz",
  Headers = [{"Content-Type", "application/octet-stream"}, {"Accept", "application/json"}],
  HTTPOptions = [{autoredirect, false}],
  Options = [],
  
  ProcessId = hb_util:encode(SignedTX#tx.id),
  
  case httpc:request(post, {LEGACY_URL, Headers, "application/octet-stream", ANS104Bytes}, HTTPOptions, Options) of
    {ok, {{_Version, StatusCode, _ReasonPhrase}, ResponseHeaders, Body}} ->
      io:format("HTTP Response: ~p~n", [StatusCode]),
      io:format("Response Headers: ~p~n", [ResponseHeaders]),
      io:format("Response Body: ~s~n", [Body]);
    {error, Reason} ->
      io:format("HTTP Error: ~p~n", [Reason])
  end,

  % Send authorities update message
  WalletAddressStr = binary_to_list(WalletAddress),
  AuthoritiesData = list_to_binary("ao.authorities = {'" ++ WalletAddressStr ++ "', 'fcoN_xJeisVsPXA-trzVAuIiqO3ydLQxM-L4XbrQKzY', 'jnbRhoH3JGTdRz0Y9X-gh-eosrbIpdxs58DPTtlOVE8'}"),
  
  AuthoritiesMsg = #{
    <<"Type">> => <<"Message">>,
    <<"Data-Protocol">> => <<"ao">>,
    <<"Variant">> => <<"ao.TN.1">>,
    <<"target">> => ProcessId,
    <<"data">> => AuthoritiesData,
    <<"Action">> => <<"Eval">>
  },
  
  {ok, AuthTX} = dev_codec_ans104:to(AuthoritiesMsg, #{}, #{}),
  SignedAuthTX = ar_bundles:sign_item(AuthTX, Wallet),
  AuthANS104Bytes = ar_bundles:serialize(SignedAuthTX),
  
  case httpc:request(post, {LEGACY_URL, Headers, "application/octet-stream", AuthANS104Bytes}, HTTPOptions, Options) of
    {ok, {{_Version2, AuthStatusCode, _ReasonPhrase2}, _AuthResponseHeaders, AuthBody}} ->
      io:format("Authorities Update Response: ~p~n", [AuthStatusCode]),
      io:format("Authorities Update Body: ~s~n", [AuthBody]);
    {error, AuthReason} ->
      io:format("Authorities Update Error: ~p~n", [AuthReason])
  end,

  % Load AO holder wallet
  HolderWallet = ar_wallet:load_keyfile("/test/wallet.json"),
  HolderAddress = hb_util:encode(ar_wallet:to_address(HolderWallet)),
  
  % Create a new wallet to topup
  TopupWallet = ar_wallet:new(),
  TopupAddress = hb_util:encode(ar_wallet:to_address(TopupWallet)),
  
  io:format("AO Holder Address: ~s~n", [HolderAddress]),
  io:format("Topup Address: ~s~n", [TopupAddress]),
  
  % Transfer AO to Beta Green Zone AO
  io:format("Transferring AO to Beta Green Zone AO...~n"),
  TransferMsg = #{
    <<"Type">> => <<"Message">>,
    <<"Data-Protocol">> => <<"ao">>,
    <<"Variant">> => <<"ao.TN.1">>,
    <<"target">> => <<"0syT13r0s0tgPmIed95bJnuSqaD29HQNN8D3ElLSrsc">>, % ao-token
    <<"Action">> => <<"Transfer">>,
    <<"Quantity">> => <<"50">>,
    <<"Recipient">> => ProcessId % beta-gz-ao-token
  },
  
  {ok, TransferTX} = dev_codec_ans104:to(TransferMsg, #{}, #{}),
  SignedTransferTX = ar_bundles:sign_item(TransferTX, HolderWallet),
  io:format("SignedTransferTX ID: ~s~n", [hb_util:encode(SignedTransferTX#tx.id)]),
  TransferANS104Bytes = ar_bundles:serialize(SignedTransferTX),
  
  case httpc:request(post, {LEGACY_URL, Headers, "application/octet-stream", TransferANS104Bytes}, HTTPOptions, Options) of
    {ok, {{_Version5, TransferStatusCode, _ReasonPhrase5}, _TransferResponseHeaders, TransferBody}} ->
      io:format("Transfer Response: ~p~n", [TransferStatusCode]),
      io:format("Transfer ID: ~s~n", [TransferBody]),
      TransferBody;
    {error, TransferReason} ->
      io:format("Transfer Error: ~p~n", [TransferReason]),
      <<>>
  end,

  ok.
  