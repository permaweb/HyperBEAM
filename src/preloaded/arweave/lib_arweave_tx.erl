%%% @doc The `#tx{}' record boundary for Arweave layer-one transactions.
%%%
%%% Vendored `ar_tx', `ar_block' and `ar_wallet' work on records; every device
%%% above them works on dashed-key `structured@1.0' messages. This module is
%%% the single place the two meet for a layer-one transaction.
%%% `lib_arweave_common' owns the same boundary for ANS-104 data items, which
%%% are a different shape with a different signature preimage, so the two do
%%% not overlap.
%%%
%%% The message spells Arweave's wire names in dashed form -- `last-tx', not
%%% the record's `anchor' -- and carries every byte-valued field base64URL
%%% encoded, tag names and values included. Tags hold arbitrary bytes, so
%%% encoding them is what makes the message representable at all.
%%%
%%% `signature-type' is carried explicitly rather than inferred. Arweave's JSON
%%% sends an empty `owner' for an ECDSA transaction and expects the public key
%%% to be recovered from the signature, so a message that kept only the
%%% recovered key would read back as RSA and fail to verify.
-module(lib_arweave_tx).
-export([to_tx/2, from_tx/2, to_json_struct/2, from_json_struct/1]).
-include("include/hb.hrl").

%% @doc Convert a transaction message into the record the vendored modules
%% take.
to_tx(Msg, Opts) ->
    Owner = field(<<"owner">>, Msg, ?DEFAULT_OWNER, Opts),
    SignatureType =
        signature_type(hb_maps:get(<<"signature-type">>, Msg, <<>>, Opts)),
    #tx{
        format = int(<<"format">>, Msg, 2, Opts),
        id = field(<<"id">>, Msg, ?DEFAULT_ID, Opts),
        anchor = field(<<"last-tx">>, Msg, ?DEFAULT_ANCHOR, Opts),
        owner = Owner,
        owner_address = ar_wallet:to_address(Owner, SignatureType),
        tags = tags(hb_maps:get(<<"tags">>, Msg, [], Opts), Opts),
        target = field(<<"target">>, Msg, ?DEFAULT_TARGET, Opts),
        quantity = int(<<"quantity">>, Msg, ?DEFAULT_QUANTITY, Opts),
        data = field(<<"data">>, Msg, ?DEFAULT_DATA, Opts),
        data_size = int(<<"data-size">>, Msg, ?DEFAULT_DATA_SIZE, Opts),
        data_root = field(<<"data-root">>, Msg, ?DEFAULT_DATA_ROOT, Opts),
        reward = int(<<"reward">>, Msg, ?DEFAULT_REWARD, Opts),
        signature = field(<<"signature">>, Msg, ?DEFAULT_SIG, Opts),
        denomination = int(<<"denomination">>, Msg, 0, Opts),
        signature_type = SignatureType
    }.

%% @doc Convert a transaction record into its message form.
from_tx(TX, _Opts) ->
    #{
        <<"format">> => TX#tx.format,
        <<"id">> => hb_util:encode(TX#tx.id),
        <<"last-tx">> => hb_util:encode(TX#tx.anchor),
        <<"owner">> => hb_util:encode(TX#tx.owner),
        <<"tags">> =>
            [
                #{
                    <<"name">> => hb_util:encode(Name),
                    <<"value">> => hb_util:encode(Value)
                }
            ||
                {Name, Value} <- TX#tx.tags
            ],
        <<"target">> => hb_util:encode(TX#tx.target),
        <<"quantity">> => TX#tx.quantity,
        <<"data">> => hb_util:encode(TX#tx.data),
        <<"data-size">> => TX#tx.data_size,
        <<"data-root">> => hb_util:encode(TX#tx.data_root),
        <<"reward">> => TX#tx.reward,
        <<"signature">> => hb_util:encode(TX#tx.signature),
        <<"denomination">> => TX#tx.denomination,
        <<"signature-type">> => signature_type_name(TX#tx.signature_type)
    }.

%% @doc Build a record from the JSON structure Arweave serves. Delegates to
%% the vendored codec, which is also what recovers an ECDSA transaction's
%% public key from its signature.
from_json_struct(Struct) ->
    ar_tx:json_struct_to_tx(Struct).

%% @doc Render a record as the JSON structure Arweave serves. An ECDSA
%% transaction carries no public key on the wire -- the verifier recovers it --
%% so the recovered key is dropped again here.
%% VENDOR: `ar_tx:tx_to_json_struct/1' emits the recovered key, where upstream
%% `ar_serialize:tx_to_json_struct/1' (src/ar_serialize.erl:1499) emits `<<>>'.
%% Remove this clause once the vendored encoder matches upstream.
to_json_struct(TX = #tx{ signature_type = ?ECDSA_KEY_TYPE }, Opts) ->
    hb_maps:put(<<"owner">>, <<>>, ar_tx:tx_to_json_struct(TX), Opts);
to_json_struct(TX, _Opts) ->
    ar_tx:tx_to_json_struct(TX).

%%% Internal functions.

%% @doc Read a base64URL field, decoded to the raw bytes the record holds.
field(Key, Msg, Default, Opts) ->
    case hb_maps:get(Key, Msg, not_found, Opts) of
        not_found -> Default;
        Value -> hb_util:decode(Value)
    end.

int(Key, Msg, Default, Opts) ->
    hb_util:int(hb_maps:get(Key, Msg, Default, Opts)).

tags(Tags, Opts) ->
    [
        {
            hb_util:decode(hb_maps:get(<<"name">>, Tag, <<>>, Opts)),
            hb_util:decode(hb_maps:get(<<"value">>, Tag, <<>>, Opts))
        }
    ||
        Tag <- hb_util:message_to_ordered_list(Tags, Opts)
    ].

%% @doc Map the wire spelling of a signature type onto the record's tuple. The
%% mapping is explicit because an unknown name must be an error rather than a
%% coerced atom, and because a transaction with no stated type predates ECDSA
%% and is therefore RSA.
signature_type(<<"ecdsa-secp256k1">>) -> ?ECDSA_KEY_TYPE;
signature_type(<<"rsa-65537">>) -> ?RSA_KEY_TYPE;
signature_type(<<>>) -> ?RSA_KEY_TYPE;
signature_type(Unknown) -> throw({unknown_signature_type, Unknown}).

signature_type_name(?ECDSA_KEY_TYPE) -> <<"ecdsa-secp256k1">>;
signature_type_name(?RSA_KEY_TYPE) -> <<"rsa-65537">>.
