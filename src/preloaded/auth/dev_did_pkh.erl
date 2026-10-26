%%% @doc A device that implements the `did:pkh:' DID method. A `did:pkh:' URI
%%% identifies the committer of a key by its blockchain account, which is
%%% in the CAIP-10 form. The account of each type of key is as follows:
%%% <pre>
%%%     eip155:1:ADDRESS         An Ethereum key. The address is checksummed.
%%%     solana:5eykt4UsFv8P8NJdTREpY1vzqKqZKvdp:ADDRESS
%%%                              A Solana key. The address is the key in
%%%                              base58.
%%% </pre>
%%% The `did@1.0' device reads a `did:pkh:' URI with this device, giving it
%%% the account as the `body' of the committer. The committer of an RSA,
%%% Ed25519 or ECDSA key is `did-ar@1.0'.
%%%
%%% A committer that is read from its URI has its account as its `body'. A
%%% committer with a key has the public key as its `owner', the
%%% signature type of the key as its `type', and the private key as `d' in
%%% its `priv' message. The `commit' key signs the request's
%%% `body' with the algorithm of the request's `type', and returns the
%%% `signature' and the `owner'. The `verify' key checks that the request's
%%% `owner' is a key of the committer's account, and that the `signature' of
%%% the `body' verifies with that key.
-module(dev_did_pkh).
-implements(<<"did-pkh@1.0">>).
-device_libraries([lib_did]).
-export([from/3, to/3, commit/3, verify/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc The CAIP-2 reference of Solana mainnet. It is the first 32 characters
%% of the base58 hash of the network's genesis block,
%% `5eykt4UsFv8P8NJdTREpY1vzqKqZKvdpKuc147dw2N9d'.
-define(SOLANA_MAINNET, <<"5eykt4UsFv8P8NJdTREpY1vzqKqZKvdp">>).

%% @doc Read the committer of a `did:pkh:' URI. `hb_uri' gives the account as
%% the `body' of the message, which is already the committer. We refuse an
%% account that `to' does not give for any key.
-spec from(
    #{ body := binary(), _ => _ },
    #{ type := binary(), _ => _ },
    #{ _ => _ }
) -> {ok, #{ body := binary(), _ => _ }} | {error, _}.
from(Committer, #{ <<"type">> := <<"uri">> }, _Opts) ->
    case is_account(maps:get(<<"body">>, Committer)) of
        true -> {ok, Committer};
        false -> {error, #{ <<"error">> => <<"invalid-account">> }}
    end.

%% @doc Write the `did:pkh:' URI of a committer, from its account.
-spec to(
    #{ body => binary(), owner => binary(), type => binary(), _ => _ },
    #{ type := binary(), _ => _ },
    #{ _ => _ }
) -> {ok, binary()} | {error, _}.
to(Committer, #{ <<"type">> := <<"uri">> }, Opts) ->
    maybe
        {ok, Account} ?= account(Committer, Opts),
        {ok, <<"did:pkh:", Account/binary>>}
    end.

%% @doc Sign the request's `body' with the committer's key, using the
%% algorithm of the request's `type'. If the key is of another type, we return
%% an error.
-spec commit(
    #{ owner := binary(), type := binary(), priv := #{ d := binary() }, _ => _ },
    #{ body := binary(), type := binary(), _ => _ },
    #{ _ => _ }
) -> {ok, #{ signature := binary(), owner := binary() }} | {error, _}.
commit(Committer, Req, _Opts) ->
    lib_did:commit(Committer, Req).

%% @doc Verify a signature of the request's `body'. The request's `owner' must
%% be a key of the committer's account, and the `signature' must verify with
%% that key.
-spec verify(
    #{ body => binary(), owner => binary(), type => binary(), _ => _ },
    #{
        body := binary(),
        signature := binary(),
        owner := binary(),
        type := binary(),
        _ => _
    },
    #{ _ => _ }
) -> {ok, boolean()}.
verify(Committer, Req, Opts) ->
    case account(Committer, Opts) of
        {ok, Account} ->
            case lib_did:verify(Account, Req, fun key_account/2) of
                {ok, true} ->
                    {ok,
                        hb_committer:matches_commitment(
                            <<"did:pkh:", Account/binary>>,
                            Req,
                            Opts
                        )};
                Verified -> Verified
            end;
        {error, _} -> {ok, false}
    end.

%% @doc Return the account of a committer: its `body' if it has one, else the
%% account of its key.
account(#{ <<"body">> := Account }, Opts) ->
    {ok, hb_cache:ensure_loaded(Account, Opts)};
account(#{ <<"owner">> := Owner, <<"type">> := Type }, _Opts) ->
    key_account(Owner, lib_did:key_type(Type)).

%% @doc Return the CAIP-10 account of a public key, using the address that
%% `ar_wallet:to_address/2' gives for it. Keys of other types have no account.
key_account(Owner, KeyType)
        when KeyType =:= ?ETHEREUM_KEY_TYPE;
            KeyType =:= ?TYPED_ETHEREUM_KEY_TYPE ->
    {ok, <<"eip155:1:", (ar_wallet:to_address(Owner, KeyType))/binary>>};
key_account(Owner, ?SOLANA_KEY_TYPE) ->
    Address = ar_wallet:to_address(Owner, ?SOLANA_KEY_TYPE),
    {ok, <<"solana:", ?SOLANA_MAINNET/binary, ":", Address/binary>>};
key_account(_Owner, _KeyType) ->
    {error, #{ <<"error">> => <<"unsupported-key-type">> }}.

%% @doc Check that an account is one that `key_account/2' returns for a key. The
%% address of an Ethereum account carries its EIP-55 checksum, and the address
%% of a Solana account is the base58 encoding of 32 bytes.
is_account(Account = <<"eip155:1:0x", Hex:40/binary>>) ->
    re:run(Hex, <<"\\A[0-9a-fA-F]+\\z">>, [{capture, none}]) =:= match andalso
        key_account(<<"0x", Hex/binary>>, ?TYPED_ETHEREUM_KEY_TYPE) =:=
            {ok, Account};
is_account(<<"solana:", Reference:32/binary, ":", Address/binary>>)
        when Reference =:= ?SOLANA_MAINNET ->
    case hb_util:base58_decode(Address) of
        {ok, Key} -> byte_size(Key) == 32;
        error -> false
    end;
is_account(_Account) ->
    false.

%%% Tests

%% @doc Ensure that a committer's URI is `did:pkh:' followed by its account,
%% and that it reads back as a committer with the same URI.
uri_test() ->
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Ethereum = ar_wallet:new(?ETHEREUM_KEY_TYPE),
    {ok, URI} = hb_uri:to(Ethereum, #{ <<"scheme">> => <<"did">> }, Opts),
    ?assertMatch(<<"did:pkh:eip155:1:0x", _:40/binary>>, URI),
    {ok, Read} = hb_uri:from(URI, Opts),
    ?assertEqual(
        {ok, URI},
        hb_uri:to(Read, #{ <<"scheme">> => <<"did">> }, Opts)
    ),
    % Check the URI of the CAIP-10 example account on Solana mainnet.
    Solana =
        #{
            <<"device">> => <<"did-pkh@1.0">>,
            <<"owner">> =>
                hb_util:decode(
                    <<"X5BVS7PYwvyCtu5ZxJqqFD5399Sag-lWzh2-8XpD-AU">>
                ),
            <<"type">> => <<"solana">>
        },
    ?assertEqual(
        {ok, <<"did:pkh:solana:5eykt4UsFv8P8NJdTREpY1vzqKqZKvdp:",
            "7S3P4HxJpyyigGzodYwHtCxZyUQe9JiBMHyRWXArAaKv">>},
        hb_uri:to(Solana, #{ <<"scheme">> => <<"did">> }, Opts)
    ),
    % Check that the committer of the Solana URI gives the same URI after it is
    % written to the store and read back, with its 84-byte `body' as a link.
    {ok, SolanaURI} = hb_uri:to(Solana, #{ <<"scheme">> => <<"did">> }, Opts),
    {ok, SolanaCommitter} = hb_uri:from(SolanaURI, Opts),
    {ok, SolanaID} = hb_cache:write(SolanaCommitter, Opts),
    {ok, SolanaRead} = hb_cache:read(SolanaID, Opts),
    ?assertEqual(
        {ok, SolanaURI},
        hb_uri:to(SolanaRead, #{ <<"scheme">> => <<"did">> }, Opts)
    ).

%% @doc Ensure that a committer's signature verifies with the committer read
%% from its URI, and that a signature by the key of another account does not.
commit_verify_test() ->
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Committer = ar_wallet:new(?ETHEREUM_KEY_TYPE),
    {ok, URI} = hb_uri:to(Committer, #{ <<"scheme">> => <<"did">> }, Opts),
    {ok, Read} = hb_uri:from(URI, Opts),
    Req = #{ <<"body">> => <<"base">>, <<"type">> => <<"ethereum">> },
    {ok, Signed} = hb_ao:raw(Committer, Req#{ <<"path">> => <<"commit">> }, Opts),
    Verify = Req#{ <<"path">> => <<"verify">> },
    ?assertEqual({ok, true}, hb_ao:raw(Read, maps:merge(Verify, Signed), Opts)),
    Other = ar_wallet:new(?ETHEREUM_KEY_TYPE),
    {ok, OtherSigned} =
        hb_ao:raw(Other, Req#{ <<"path">> => <<"commit">> }, Opts),
    ?assertEqual(
        {ok, false},
        hb_ao:raw(Read, maps:merge(Verify, OtherSigned), Opts)
    ).
