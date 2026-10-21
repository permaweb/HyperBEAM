%%% @doc The `commit' and `verify' keys of the committers of DID methods,
%%% `did-ar@1.0' and `did-pkh@1.0'. Each device gives the account of a key in
%%% the form of its method, and these functions sign and check signatures
%%% with the key.
-module(lib_did).
-export([commit/2, verify/3, key_type/1]).
-include("include/hb.hrl").

%% @doc Sign the request's `body' with the committer's key, using the
%% algorithm of the request's `type'. If the key is of another type, we return
%% an error.
commit(Committer, #{ <<"body">> := Base, <<"type">> := Type }) ->
    #{ <<"owner">> := Owner, <<"type">> := CommitterType } = Committer,
    KeyType = key_type(CommitterType),
    case key_type(Type) of
        KeyType ->
            {
                ok,
                #{
                    <<"signature">> =>
                        ar_wallet:sign(Committer, Base, digest(Type)),
                    <<"owner">> => Owner
                }
            };
        _ -> {error, #{ <<"error">> => <<"unsupported-key-type">> }}
    end.

%% @doc Verify a signature of the request's `body'. `KeyAccount' gives the
%% account of the request's `owner' for the key type of the request's `type',
%% which must be `Account', the account of the committer. The `signature'
%% must verify with that key.
verify(Account, Req = #{ <<"owner">> := Owner, <<"type">> := Type },
        KeyAccount) ->
    KeyType = key_type(Type),
    case KeyAccount(Owner, KeyType) of
        {ok, Account} ->
            #{ <<"body">> := Base, <<"signature">> := Sig } = Req,
            Wallet =
                #{
                    <<"type">> => ar_tx:serialize_sig_type(KeyType),
                    <<"owner">> => Owner
                },
            {ok, ar_wallet:verify(Wallet, Base, Sig, digest(Type))};
        _ -> {ok, false}
    end;
verify(_Account, _Req, _KeyAccount) ->
    {ok, false}.

%% @doc Convert a signature type to the type of key that signs with it. The
%% signature type is either `rsa-pss-sha512', from HTTP message signatures,
%% or one of Arweave's signature types.
key_type(<<"rsa-pss-sha512">>) -> ?RSA_KEY_TYPE;
key_type(Type) -> ar_tx:deserialize_sig_type(Type).

%% @doc Return the hash algorithm of a signature type: SHA-512 for
%% `rsa-pss-sha512', and SHA-256 for Arweave's signature types.
digest(<<"rsa-pss-sha512">>) -> sha512;
digest(_Type) -> sha256.
