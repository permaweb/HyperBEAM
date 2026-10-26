%%% @doc A device that implements the `did:ar:' DID method. A `did:ar:' URI
%%% contains the address of the committer of an Arweave key: the base64url
%%% SHA-256 of an RSA, Ed25519 or ECDSA public key. In host form (see
%%% `hb_uri'), it is the label `uri-did-ar-<L><B>' that `hb_committer'
%%% describes, which may also state the type of the key, or encode the ID of a
%%% message.
%%% <pre>
%%%     did:ar:ADDRESS
%%%     uri-did-ar-LB
%%% </pre>
%%% The `did@1.0' device reads both forms with this device. `from' gives the
%%% committer of an address, with the address as its `body', and the signature
%%% type of the key as its `type' if the label states a key type. A label of an
%%% ID gives the message with that ID. `to' gives `did:ar:<address>', or with
%%% `characters => host' the label with no key type, which is the URI that
%%% commitments hold (see `hb_committer').
%%%
%%% A committer with a key has the public key as its `owner', the
%%% signature type of the key as its `type', and the private key as `d' in
%%% its `priv' message. The `commit' key signs the request's `body' with the
%%% algorithm of the request's `type', and returns the `signature' and the
%%% `owner'. The `verify' key checks that the request's `owner' is a key of the
%%% committer's address, of the committer's key type if it has a `type', and
%%% that the `signature' of the `body' verifies with that key.
-module(dev_did_ar).
-implements(<<"did-ar@1.0">>).
-device_libraries([lib_did]).
-export([from/3, to/3, commit/3, verify/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Read the resource of a `did:ar:' URI: `hb_uri' gives the rest of the
%% URI as the `body' of the message. In host form, the rest is a label of an
%% address or an ID. We refuse an address or a label in any other form.
-spec from(
    #{ body := binary(), _ => _ },
    #{ type := binary(), characters => binary(), _ => _ },
    #{ _ => _ }
) -> {ok, _} | {error, _}.
from(Committer = #{ <<"body">> := Body }, Req = #{ <<"type">> := <<"uri">> },
        Opts) ->
    case hb_maps:get(<<"characters">>, Req, <<"all">>, Opts) of
        <<"host">> ->
            read_label(
                hb_committer:decode(<<"uri-did-ar-", Body/binary>>),
                Committer,
                Opts
            );
        _ ->
            case hb_util:is_human_id(Body) of
                true -> {ok, Committer};
                false -> {error, #{ <<"error">> => <<"invalid-account">> }}
            end
    end.

%% @doc Read a decoded label: the committer of an address, or the message with
%% an ID.
read_label({ok, {address, undefined}, Address}, Committer, _Opts) ->
    {ok, Committer#{ <<"body">> => Address }};
read_label({ok, {address, KeyType}, Address}, Committer, _Opts) ->
    {ok,
        Committer#{
            <<"body">> => Address,
            <<"type">> => ar_tx:serialize_sig_type(KeyType)
        }
    };
read_label({ok, {id, _Device}, ID}, _Committer, Opts) ->
    hb_cache:read(ID, Opts);
read_label(error, _Committer, _Opts) ->
    {error, #{ <<"error">> => <<"invalid-account">> }}.

%% @doc Give the `did:ar:' URI of a committer, from its address. With
%% `characters => host', give the URI in host form, with no key type.
-spec to(
    #{ body => binary(), owner => binary(), type => binary(), _ => _ },
    #{ type := binary(), characters => binary(), _ => _ },
    #{ _ => _ }
) -> {ok, binary()} | {error, _}.
to(Committer, Req = #{ <<"type">> := <<"uri">> }, Opts) ->
    maybe
        {ok, Address} ?= address(Committer),
        case hb_maps:get(<<"characters">>, Req, <<"all">>, Opts) of
            <<"host">> ->
                {ok, hb_committer:encode({address, undefined}, Address)};
            _ ->
                {ok, <<"did:ar:", Address/binary>>}
        end
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
%% be a key of the committer's address, of the committer's key type if it has
%% a `type', and the `signature' must verify with that key.
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
    case address(Committer) of
        {ok, Address} ->
            KeyAddress =
                fun(Owner, KeyType) ->
                    key_address(Owner, KeyType, Committer)
                end,
            case lib_did:verify(Address, Req, KeyAddress) of
                {ok, true} ->
                    {ok,
                        hb_committer:matches_commitment(
                            hb_committer:encode(
                                {address, undefined},
                                Address
                            ),
                            Req,
                            Opts
                        )};
                Verified -> Verified
            end;
        {error, _} -> {ok, false}
    end.

%% @doc Return the address of a committer: its `body' if it has one, else the
%% address of its key.
address(#{ <<"body">> := Address }) ->
    {ok, Address};
address(#{ <<"owner">> := Owner, <<"type">> := Type }) ->
    key_address(Owner, lib_did:key_type(Type)).

%% @doc Return the address of a public key for a committer: the committer's
%% key type, if it has a `type', must be the type of the key.
key_address(Owner, KeyType, #{ <<"type">> := Type }) ->
    case lib_did:key_type(Type) of
        KeyType -> key_address(Owner, KeyType);
        _ -> {error, #{ <<"error">> => <<"unsupported-key-type">> }}
    end;
key_address(Owner, KeyType, _Committer) ->
    key_address(Owner, KeyType).

%% @doc Return the address of a public key, as `ar_wallet:to_address/2' gives
%% it. Keys of other types have no `did:ar:' address.
key_address(Owner, KeyType)
        when KeyType =:= ?RSA_KEY_TYPE;
            KeyType =:= ?EDDSA_KEY_TYPE;
            KeyType =:= ?ECDSA_KEY_TYPE ->
    {ok, hb_util:human_id(ar_wallet:to_address(Owner, KeyType))};
key_address(_Owner, _KeyType) ->
    {error, #{ <<"error">> => <<"unsupported-key-type">> }}.

%%% Tests

%% @doc Ensure that a committer's URI is `did:ar:' followed by its address, or
%% the label of its address in host form, and that both read back as a
%% committer with the same URIs.
uri_test() ->
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Committer = ar_wallet:new(?RSA_KEY_TYPE),
    Address = hb_util:human_id(ar_wallet:to_address(Committer)),
    Host = #{ <<"scheme">> => <<"did">>, <<"characters">> => <<"host">> },
    {ok, URI} = hb_uri:to(Committer, #{ <<"scheme">> => <<"did">> }, Opts),
    ?assertEqual(<<"did:ar:", Address/binary>>, URI),
    {ok, HostURI} = hb_uri:to(Committer, Host, Opts),
    ?assertEqual(hb_committer:encode({address, undefined}, Address), HostURI),
    lists:foreach(
        fun(Read) ->
            {ok, ReadCommitter} = hb_uri:from(Read, Opts),
            ?assertEqual({ok, HostURI}, hb_uri:to(ReadCommitter, Host, Opts))
        end,
        [URI, HostURI]
    ),
    % The committer of an Ed25519 node is that of its address.
    Ed25519 = #{ <<"priv-wallet">> => ar_wallet:new(?EDDSA_KEY_TYPE) },
    ?assertEqual(
        hb_opts:committer(Ed25519),
        hb_committer:uri(hb_opts:address(Ed25519))
    ),
    % A label of an ID gives the message with that ID.
    {ok, ID} = hb_cache:write(#{ <<"a">> => <<"1">> }, Opts),
    ?assertEqual(
        {ok, <<"1">>},
        hb_ao:resolve(hb_committer:encode({id, undefined}, ID), <<"a">>, Opts)
    ).

%% @doc Ensure that a committer's signature verifies with the committer read
%% from its URI, and that a signature by the key of another address does not.
%% A label that states another key type than the key's does not verify the
%% signature either.
commit_verify_test() ->
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Committer = ar_wallet:new(?RSA_KEY_TYPE),
    Address = hb_util:human_id(ar_wallet:to_address(Committer)),
    {ok, URI} = hb_uri:to(Committer, #{ <<"scheme">> => <<"did">> }, Opts),
    {ok, Read} = hb_uri:from(URI, Opts),
    Req = #{ <<"body">> => <<"base">>, <<"type">> => <<"rsa-pss-sha512">> },
    {ok, Signed} =
        hb_ao:raw(Committer, Req#{ <<"path">> => <<"commit">> }, Opts),
    Verify = maps:merge(Req#{ <<"path">> => <<"verify">> }, Signed),
    ?assertEqual({ok, true}, hb_ao:raw(Read, Verify, Opts)),
    Other = ar_wallet:new(?RSA_KEY_TYPE),
    {ok, OtherSigned} =
        hb_ao:raw(Other, Req#{ <<"path">> => <<"commit">> }, Opts),
    ?assertEqual(
        {ok, false},
        hb_ao:raw(Read, maps:merge(Verify, OtherSigned), Opts)
    ),
    lists:foreach(
        fun({KeyType, Verified}) ->
            {ok, Labelled} =
                hb_uri:from(
                    hb_committer:encode({address, KeyType}, Address),
                    Opts
                ),
            ?assertEqual({ok, Verified}, hb_ao:raw(Labelled, Verify, Opts))
        end,
        [{?RSA_KEY_TYPE, true}, {?ECDSA_KEY_TYPE, false}]
    ).
