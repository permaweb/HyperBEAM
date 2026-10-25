%%% @doc The URIs of committers. A committer is a message whose device has the
%%% keys of the Committer API: `to' with `type => uri' gives its URI, `from'
%%% reads the URI back, `commit' signs a binary and `verify' checks a
%%% signature.
%%%
%%% A place that meets a committer, such as a node's `operator', its trusted
%%% signers, a scheduler location or the accounts of a ledger, takes it in any
%%% of these forms: a committer message, a message that it committed, a URI in
%%% either form (see `hb_uri'), or an Arweave address. `uri/2' gives each of
%%% them in one form, so that the URIs of one committer compare equal.
%%% Commitments, signers lists and ledgers use this form:
%%% <ul>
%%%   <li>The committer of an Arweave key, whose address is the SHA-256 of the
%%%       key (an RSA, Ed25519 or ECDSA key), is `did-ar@1.0'. Its URI is
%%%       `did:ar:<address>' in host form with no key type:
%%%       `uri-did-ar-<L><B>'.</li>
%%%   <li>The URI of any other committer is the one that its device gives.</li>
%%% </ul>
%%%
%%% `uri-did-ar-<L><B>' is 63 characters, one DNS label, and encodes an address
%%% or the ID of a message:
%%% <pre>
%%%     L   The 43 base64url characters of the address or ID, in lowercase,
%%%         with each `_' given as `-'.
%%%     B   9 base36 characters (`0'-`9', `a'-`z'), zero-padded: a 46-bit
%%%         integer. Its 43 most significant bits are one for each character
%%%         of L, the first character first: 1 for a capital letter, or for a
%%%         `_' given as `-'. Its 3 least significant bits are the type.
%%% </pre>
%%% The type, most significant bit first:
%%% <pre>
%%%     000     An address, of a key of no stated type.
%%%     010     The address of an RSA key.
%%%     100     The address of an Ed25519 key.
%%%     110     The address of an ECDSA (secp256k1) key.
%%%     001     The ID of a message that is not a base-layer transaction.
%%%     011     The ID of a base-layer transaction (`tx@1.0').
%%%     101     Reserved.
%%%     111     Reserved.
%%% </pre>
%%% The low bit is 1 for an ID. The two high bits of an address are the type
%%% of its key, numbered as ANS-104 numbers signature types: 1 RSA, 2 Ed25519,
%%% and 3 ECDSA on secp256k1, the curve of ANS-104's type 3. For example, the
%%% RSA address `kY9RAgTJEImkBpiKgVeXrsGV02T-D4dI3ZvSpnn7HSk' is
%%% `uri-did-ar-ky9ragtjeimkbpikgvexrsgv02t-d4di3zvspnn7hsk8xyj2kegi', and
%%% the URI of its committer is
%%% `uri-did-ar-ky9ragtjeimkbpikgvexrsgv02t-d4di3zvspnn7hsk8xyj2kegg'.
%%%
%%% Each pair of an address or ID and a type has one label. `decode/1'
%%% refuses a label in any other form: of another length, with a character
%%% outside the alphabets above, with a digit whose bit is set, with an
%%% integer above 2^46 - 1 or a reserved type, or whose address or ID is not
%%% in the form that `hb_util:human_id/1' gives.
-module(hb_committer).
-export([uri/1, uri/2, matches_commitment/3, address/1, encode/2, decode/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Return the URI of a committer given as a binary, in the form that
%% compares equal (see the module doc). An Arweave address, or a `did:ar:' URI
%% of an address in either form, gives the URI of its `did-ar@1.0' committer.
%% A query after it, such as the `?hint=' of a scheduler, is kept. Any other
%% URI is returned as it is. Any other binary is not a committer: we return it
%% as it is, so that it matches no signer, and log it as an `invalid_committer'
%% event. Any other term is returned as it is.
uri(Value) when is_binary(Value) ->
    [Main | Query] = binary:split(Value, <<"?">>),
    case address(Main) of
        {ok, Address} ->
            iolist_to_binary(
                lists:join(
                    <<"?">>,
                    [encode({address, undefined}, Address) | Query]
                )
            );
        error ->
            case hb_uri:scheme(Value) of
                {ok, _Scheme, _Rest} -> Value;
                _ ->
                    ?event(committer, {invalid_committer, Value}),
                    Value
            end
    end;
uri(Value) ->
    Value.

%% @doc Return the URI of a committer given in any form, as `uri/1' does for
%% a binary. A message that holds commitments gives the URI of its one signer,
%% and a committer message gives the URI that its device's `to' gives with
%% `characters => host'. Any other value is returned as it is. We leave out
%% the query of a binary, such as the `?hint=' of a scheduler, so that the
%% URIs of one committer compare equal.
uri(Msg = #{ <<"commitments">> := _ }, Opts) ->
    case hb_message:signers(Msg, Opts) of
        [Signer] -> Signer;
        _ -> Msg
    end;
uri(Committer = #{ <<"device">> := _ }, Opts) ->
    ToURI =
        #{
            <<"path">> => <<"to">>,
            <<"type">> => <<"uri">>,
            <<"characters">> => <<"host">>
        },
    case hb_ao:raw(Committer, ToURI, Opts) of
        {ok, URI} -> URI;
        {error, _} -> Committer
    end;
uri(Value, _Opts) when is_binary(Value) ->
    [Main | _Query] = binary:split(Value, <<"?">>),
    uri(Main);
uri(Value, _Opts) ->
    uri(Value).

%% @doc Return whether a commitment's `committer', if present, is the given
%% committer. A commitment device, such as `httpsig@1.0', builds the signature
%% base from the committed keys and gives the committer the commitment without
%% its `commitment-device'. A commitment that still gives one came to the
%% committer directly, with a `body' that nothing ties to the committed keys,
%% so it does not match.
matches_commitment(_Committer, #{ <<"commitment-device">> := _ }, _Opts) ->
    false;
matches_commitment(Committer, Commitment, Opts) ->
    case hb_maps:find(<<"committer">>, Commitment, Opts) of
        error -> true;
        {ok, Declared} -> uri(Declared, Opts) =:= uri(Committer, Opts)
    end.

%% @doc Return the address of an Arweave committer: an address, or a `did:ar:'
%% URI of an address in either form. Returns `error' for any other value.
address(<<"did:ar:", Address/binary>>) ->
    human_id(Address);
address(Label = <<"uri-did-ar-", _/binary>>) ->
    case decode(Label) of
        {ok, {address, _KeyType}, Address} -> {ok, Address};
        _ -> error
    end;
address(Address) ->
    human_id(Address).

%% @doc Return a binary that is a human-readable ID.
human_id(Bin) ->
    case hb_util:is_human_id(Bin) of
        true -> {ok, Bin};
        false -> error
    end.

%% @doc Encode an address or a message ID as a `uri-did-ar-' label of a type:
%% `{address, KeyType}' for an address, with `undefined' for no key type, or
%% `{id, Device}' for the ID of a message, with `<<"tx@1.0">>' for a
%% base-layer transaction and `undefined' for any other.
encode(Type, ID) ->
    Human = hb_util:human_id(ID),
    Lower = binary:replace(hb_util:to_lower(Human), <<"_">>, <<"-">>, [global]),
    Bits = << <<(case_bit(C)):1>> || <<C>> <= Human >>,
    {TypeBits, Type} = lists:keyfind(Type, 2, types()),
    <<Value:46>> = <<Bits/bitstring, TypeBits:3>>,
    Base36 = hb_util:bin(io_lib:format("~9.36.0b", [Value])),
    <<"uri-did-ar-", Lower/binary, Base36/binary>>.

%% @doc Return the bit of a character of an ID in its label: 1 for a capital
%% letter or a `_', else 0.
case_bit(C) when C >= $A, C =< $Z; C == $_ -> 1;
case_bit(_C) -> 0.

%% @doc Decode a `uri-did-ar-' label to its type and the address or ID that it
%% holds. Returns `error' for a binary in any other form.
decode(<<"uri-did-ar-", Lower:43/binary, Base36:9/binary>>) ->
    maybe
        {ok, Value} ?= base36(Base36, 0),
        true ?= Value < 1 bsl 46 orelse error,
        <<Bits:43/bitstring, TypeBits:3>> = <<Value:46>>,
        {ok, Type} ?= type(TypeBits),
        {ok, ID} ?= restore_case(Lower, Bits, <<>>),
        true ?= hb_util:is_human_id(ID) orelse error,
        {ok, Type, ID}
    end;
decode(_Bin) ->
    error.

%% @doc Return the type of the type bits of a label. Returns `error' for a
%% reserved one.
type(TypeBits) ->
    case lists:keyfind(TypeBits, 1, types()) of
        {TypeBits, Type} -> {ok, Type};
        false -> error
    end.

%% @doc The types of a label and their bits (see the module doc).
types() ->
    [
        {2#000, {address, undefined}},
        {2#010, {address, ?RSA_KEY_TYPE}},
        {2#100, {address, ?EDDSA_KEY_TYPE}},
        {2#110, {address, ?ECDSA_KEY_TYPE}},
        {2#001, {id, undefined}},
        {2#011, {id, <<"tx@1.0">>}}
    ].

%% @doc Read base36 digits in lowercase (`0'-`9', `a'-`z') as an integer.
%% Returns `error' for any other character.
base36(<<>>, Value) ->
    {ok, Value};
base36(<<C, Rest/binary>>, Value) when C >= $0, C =< $9 ->
    base36(Rest, Value * 36 + C - $0);
base36(<<C, Rest/binary>>, Value) when C >= $a, C =< $z ->
    base36(Rest, Value * 36 + C - $a + 10);
base36(_Bin, _Value) ->
    error.

%% @doc Restore the case of an ID from its lowercase characters and one bit
%% for each: a set bit gives a capital letter, or `_' for `-'. Returns `error'
%% for a character that is not `a'-`z', `0'-`9' or `-', and for a digit whose
%% bit is set.
restore_case(<<>>, <<>>, ID) ->
    {ok, ID};
restore_case(<<C, Lower/binary>>, <<0:1, Bits/bitstring>>, ID)
        when C >= $a, C =< $z; C >= $0, C =< $9; C == $- ->
    restore_case(Lower, Bits, <<ID/binary, C>>);
restore_case(<<C, Lower/binary>>, <<1:1, Bits/bitstring>>, ID)
        when C >= $a, C =< $z ->
    restore_case(Lower, Bits, <<ID/binary, (C - 32)>>);
restore_case(<<$-, Lower/binary>>, <<1:1, Bits/bitstring>>, ID) ->
    restore_case(Lower, Bits, <<ID/binary, $_>>);
restore_case(_Lower, _Bits, _ID) ->
    error.

%%% Tests

%% @doc The labels of the CAIP-10 example address, with and without its key
%% type, a random address and ID of each type, and labels in other forms.
label_test() ->
    Address = <<"kY9RAgTJEImkBpiKgVeXrsGV02T-D4dI3ZvSpnn7HSk">>,
    Lower = <<"ky9ragtjeimkbpikgvexrsgv02t-d4di3zvspnn7hsk">>,
    Label = <<"uri-did-ar-", Lower/binary, "8xyj2kegi">>,
    ?assertEqual(Label, encode({address, ?RSA_KEY_TYPE}, Address)),
    ?assertEqual({ok, {address, ?RSA_KEY_TYPE}, Address}, decode(Label)),
    ?assertEqual(
        <<"uri-did-ar-", Lower/binary, "8xyj2kegg">>,
        encode({address, undefined}, Address)
    ),
    lists:foreach(
        fun({_Bits, Type}) ->
            ID = hb_util:human_id(crypto:strong_rand_bytes(32)),
            ?assertEqual({ok, Type, ID}, decode(encode(Type, ID)))
        end,
        types()
    ),
    % Each label below differs from `Label' in one way, and is refused: 62
    % characters, capitals, capitals or a sign in the base36 characters, the
    % bit of the digit `9' set, the reserved type `101', the integer 2^46, and
    % an address ending in `l', which sets bits after its 32 bytes.
    lists:foreach(
        fun(Bad) -> ?assertEqual(error, decode(Bad)) end,
        [
            binary:part(Label, 0, 62),
            string:uppercase(Label),
            <<"uri-did-ar-", Lower/binary, "8XYJ2KEGI">>,
            <<"uri-did-ar-", Lower/binary, "-xyj2kegi">>,
            <<"uri-did-ar-", Lower/binary, "c27ed8rw2">>,
            <<"uri-did-ar-", Lower/binary, "8xyj2kegl">>,
            <<"uri-did-ar-", Lower/binary, "oxyydezgg">>,
            <<"uri-did-ar-", (binary:part(Lower, 0, 42))/binary, "l8xyj2kegi">>
        ]
    ).

%% @doc Every form of an Arweave committer gives the URI of its `did-ar@1.0'
%% committer, and any other URI is kept.
uri_test() ->
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Wallet = ar_wallet:new(),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    URI = encode({address, undefined}, Address),
    ?assertEqual(URI, uri(Address)),
    ?assertEqual(URI, uri(<<"did:ar:", Address/binary>>)),
    ?assertEqual(URI, uri(encode({address, ?RSA_KEY_TYPE}, Address))),
    ?assertEqual(<<URI/binary, "?hint=x">>, uri(<<Address/binary, "?hint=x">>)),
    % `uri/2' leaves the query out.
    ?assertEqual(URI, uri(<<Address/binary, "?hint=x">>, Opts)),
    ?assertEqual(URI, uri(Wallet, Opts)),
    Signed =
        hb_message:commit(
            #{ <<"a">> => <<"1">> },
            Opts#{ <<"priv-wallet">> => Wallet }
        ),
    ?assertEqual(URI, uri(Signed, Opts)),
    ?assertEqual({ok, Address}, address(URI)),
    ?assertEqual(error, address(encode({id, undefined}, Address))),
    ?assertEqual(<<"hmac:constant:ao">>, uri(<<"hmac:constant:ao">>)),
    ?assertEqual(<<"not a committer">>, uri(<<"not a committer">>)).
