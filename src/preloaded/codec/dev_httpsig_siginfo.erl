%% @doc A module for converting between commitments and their encoded `signature'
%% and `signature-input' keys.
-module(dev_httpsig_siginfo).
-export([commitments_to_siginfo/3, siginfo_to_commitments/3]).
-export([committed_keys_to_siginfo/1, to_siginfo_keys/3, from_siginfo_keys/3]).
-export([add_derived_specifiers/1, remove_derived_specifiers/1]).
-export([keyid_to_committer/1, derived_commitment_id/1, signature_input/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% A list of components that are `derived' in the context of RFC-9421 from the
%%% request message. A key that is one of these components is signed and sent
%%% as that component: `path' as `@path'. `@query-param' also needs the query
%%% parameter that it selects, so a `query-param' key is signed as a field.
-define(DERIVED_COMPONENTS, [
    <<"method">>,
    <<"target-uri">>,
    <<"authority">>,
    <<"scheme">>,
    <<"request-target">>,
    <<"path">>,
    <<"query">>
    % <<"status">> % Some libraries do not support it
]).

%% @doc Generate a `signature' and `signature-input' key pair from a commitment
%% map.
commitments_to_siginfo(_Msg, Comms, _Opts) when ?IS_EMPTY_MESSAGE(Comms) ->
    #{};
commitments_to_siginfo(Msg, Comms, Opts) ->
    % Emit a SF item per commitment. `CommID' is threaded through so
    % `commitment_to_sf_siginfo/4' can add an `id' parameter whenever the
    % decoder-side derivation would not reproduce the sender's map key.
    {Sigs, SigInputs} =
        maps:fold(
            fun(CommID, Commitment, {Sigs, SigInputs}) ->
                {ok, SigNameRaw, SFSig, SFSigInput} =
                    commitment_to_sf_siginfo(Msg, CommID, Commitment, Opts),
                SigName = <<"comm-", SigNameRaw/binary>>,
                {
                    Sigs#{ SigName => SFSig },
                    SigInputs#{ SigName => SFSigInput }
                }
            end,
            {#{}, #{}},
            Comms
        ),
    #{
        <<"signature">> =>
            hb_util:bin(hb_structured_fields:dictionary(Sigs)),
        <<"signature-input">> =>
            hb_util:bin(hb_structured_fields:dictionary(SigInputs))
    }.

%% @doc Generate a `signature' and `signature-input' key pair from a given
%% commitment.
commitment_to_sf_siginfo(Msg, CommID, Commitment, Opts) ->
    % Extract the signature from the commitment. A commitment of another
    % device without one, such as that of an unsigned item, is sent with an
    % empty signature.
    Signature = hb_util:decode(maps:get(<<"signature">>, Commitment, <<>>)),
    % Extract the keys present in the commitment.
    CommittedKeys = to_siginfo_keys(Msg, Commitment, Opts),
    ?event_debug({normalized_for_enc, CommittedKeys, {commitment, Commitment}}),
    % Generate the name of the signature. A commitment without a signature is
    % named by its ID.
    NameSource =
        case Signature of
            <<>> -> CommID;
            _ -> Signature
        end,
    SigName = hb_util:to_lower(hb_util:human_id(crypto:hash(sha256, NameSource))),
    % If the decoder's derivation would not reproduce the sender's map key,
    % transport it explicitly as an `id' parameter. Content-addressed devices
    % (e.g. `~ipfs@1.0') key on a CID that is not a function of `Sig'; HMAC,
    % RSA-PSS, and other `h(Sig)'-keyed devices never pay this cost.
    DerivedID = derived_commitment_id(Signature),
    WithID =
        case CommID of
            undefined -> Commitment;
            DerivedID -> Commitment;
            _         -> Commitment#{ <<"id">> => CommID }
        end,
    % Generate the signature input and signature structured-fields. These can
    % then be placed into a dictionary with other commitments and transformed
    % into their binary representations.
    SFSig = {item, {binary, Signature}, []},
    SFSigInput = signature_input(CommittedKeys, WithID, Opts),
    ?event_debug(
        {sig_input,
            {string,
                hb_util:bin(
                    hb_structured_fields:dictionary(
                        #{ <<"comm">> => SFSigInput }
                    )
                )
            }
        }
    ),
    {ok, SigName, SFSig, SFSigInput}.

%% @doc Default commitment ID derivation used on both encode and decode
%% when no explicit `id' parameter is present. 32-byte sigs are used
%% directly; longer sigs are rehashed with sha-256.
derived_commitment_id(Sig) when byte_size(Sig) == 32 ->
    hb_util:human_id(Sig);
derived_commitment_id(Sig) ->
    hb_util:human_id(crypto:hash(sha256, Sig)).

%% @doc Generate the `signature-input' of a commitment: the inner list of its
%% components, with the other keys of the commitment as its parameters, sorted.
%% The `alg' parameter contains the `commitment-device' and `type' of the
%% commitment. The `@signature-params' line of a signature base is this list,
%% so the signature covers every parameter that is sent. HTTPSig derives the
%% committer of a commitment from its `keyid', so it does not send it.
signature_input(Components, Commitment = #{
        <<"commitment-device">> := <<"httpsig@1.0">>, <<"committer">> := _ },
        Opts) ->
    signature_input(Components, maps:remove(<<"committer">>, Commitment), Opts);
signature_input(Components, Commitment, Opts) ->
    Params = Commitment#{ <<"alg">> => commitment_to_alg(Commitment, Opts) },
    {ok, [SignatureInput]} =
        hb_structured_fields:to_list(
            [
                {
                    list,
                    Components,
                    [
                        {Key, param_value(maps:get(Key, Params))}
                    ||
                        Key <-
                            lists:sort(
                                maps:keys(Params) --
                                    [
                                        <<"committed">>,
                                        <<"signature">>,
                                        <<"type">>,
                                        <<"commitment-device">>
                                    ]
                            ),
                        not hb_private:is_private(Key)
                    ]
                }
            ]
        ),
    SignatureInput.

%% @doc Convert the value of a parameter for `hb_structured_fields'. Integers,
%% such as `created', are sent as integers, and other values as the text that
%% `encode_param/1' returns for them.
param_value(Value) when is_integer(Value) -> Value;
param_value(Value) -> encode_param(Value).

%% @doc Lists start with a comma and a colon per element. Maps start with a
%% colon and contain comma-separated key:value pairs. Nested values are escaped.
encode_param(Value) ->
    encode_param(Value, "%").
encode_param(List, _Delimiters) when is_list(List) ->
    <<",", (hb_util:bin([
        <<":", (escape_param(encode_param(Value, "%:,")))/binary>>
    ||
        Value <- List
    ]))/binary>>;
encode_param(Map, _Delimiters) when is_map(Map) ->
    <<":", (hb_util:bin(lists:join(<<", ">>, [
        <<
            (escape_param(Key))/binary, ":",
            (escape_param(encode_param(Value, "%:,")))/binary
        >>
    ||
        {Key, Value} <- maps:to_list(Map)
    ])))/binary>>;
encode_param(Value, Delimiters) ->
    case escape_param(hb_util:bin(Value), Delimiters) of
        <<C, Rest/binary>> when C == $,; C == $: ->
            <<(hb_escape:encode(<<C>>))/binary, Rest/binary>>;
        Bin -> Bin
    end.

%% @doc Percent-encode leading container delimiters in plain values, and all
%% delimiters in nested values. Percent and bytes outside printable ASCII are
%% escaped in both forms. Structured fields escape quotes and backslashes.
escape_param(Bin) ->
    escape_param(Bin, "%:,").
escape_param(Bin, Delimiters) ->
    hb_util:bin([
        case lists:member(C, Delimiters) orelse C < 16#20 orelse C > 16#7e of
            true -> hb_escape:encode(<<C>>);
            false -> C
        end
    ||
        C <- hb_util:list(Bin)
    ]).

%% @doc Take a message with a `signature' and `signature-input' key pair and
%% return a map of commitments.
siginfo_to_commitments(
        Msg =
            #{
                <<"signature">> := <<"comm-", SFSigBin/binary>>,
                <<"signature-input">> := <<"comm-", SFSigInputBin/binary>>
            },
        BodyKeys,
        Opts) ->
    % Parse the signature and signature-input structured-fields.
    SFSigs = hb_structured_fields:parse_dictionary(SFSigBin),
    SFSigsInputs = hb_structured_fields:parse_dictionary(SFSigInputBin),
    % Group parsed signature inputs and signatures into tuple pairs by their
    % name.
    CommitmentSFs =
        [
            {SFSig, element(2, lists:keyfind(SFSigName, 1, SFSigsInputs))}
        ||
            {SFSigName, SFSig} <- SFSigs
        ],
    % Convert each tuple into a commitment and its ID.
    CommitmentMessages =
        lists:map(
            fun ({SFSig, SFSigInput}) ->
                {ok, ID, Commitment} =
                    sf_siginfo_to_commitment(
                        Msg,
                        BodyKeys,
                        SFSig,
                        SFSigInput,
                        Opts
                    ),
                {ID, Commitment}
            end,
            CommitmentSFs
        ),
    % Convert the list of commitments into a map.
    maps:from_list(CommitmentMessages);
siginfo_to_commitments(_Msg, _BodyKeys, _Opts) ->
    % If the message does not contain a `signature' or `signature-input' key,
    % we return an empty map.
    #{}.

%% @doc Take a signature and signature-input as parsed structured-fields and 
%% return a commitment.
sf_siginfo_to_commitment(Msg, BodyKeys, SFSig, SFSigInput, Opts) ->
    % Extract the signature and signature-input from the structured-fields.
    {item, {binary, Sig}, []} = SFSig,
    {list, SigInput, ParamsKV} = SFSigInput,
    % Generate a commitment message from the signature-input parameters.
    Commitment1 =
        maps:from_list(
            lists:map(
                fun ({Key, {binary, Bin}}) -> {Key, hb_util:encode(Bin)};
                    ({Key, BareItem}) ->
                    Item =
                        case hb_structured_fields:from_bare_item(BareItem) of
                            Res when is_binary(Res) ->
                                decode_param(Res);
                            Res ->
                                Res
                        end,
                    {Key, Item}
                end,
                ParamsKV
            )
        ),
    % Generate the `commitment-device' key and optionally, its `type' key from
    % the `alg' key.
    CommitmentDeviceKeys = commitment_to_device_specifiers(Commitment1, Opts),
    % Merge the commitment parameters with the commitment device, removing the
    % `alg' key.
    Commitment2 =
        maps:merge(
            CommitmentDeviceKeys,
            maps:remove(<<"alg">>, Commitment1)
        ),
    % Generate the committed keys by parsing the signature-input list. Other
    % devices list their `committed' keys in order, percent-encoded.
    RawCommittedKeys =
        [
            Key
        ||
            {item, {string, Key}, []} <- SigInput
        ],
    CommittedKeys =
        case Commitment2 of
            #{ <<"commitment-device">> := <<"httpsig@1.0">> } ->
                from_siginfo_keys(Msg, BodyKeys, RawCommittedKeys);
            _ ->
                lists:map(fun hb_escape:decode/1, RawCommittedKeys)
        end,
    % Merge and cleanup the output:
    % 1. Decode `keyid' and `signature' to raw bytes. An empty signature of
    %    another device is that of a commitment without one.
    % 2. Filter undefined keys.
    % 3. Use the transported `id' parameter when present (content-addressed
    %    devices), otherwise fall back to `derived_commitment_id/1'.
    % 4. Keep a transported committer, or derive it from the HTTPSig keyid.
    Commitment3 =
        case {Sig, Commitment2} of
            {<<>>, #{ <<"commitment-device">> := Device }}
                    when Device =/= <<"httpsig@1.0">> ->
                Commitment2#{ <<"committed">> => CommittedKeys };
            _ ->
                Commitment2#{
                    <<"signature">> => hb_util:encode(Sig),
                    <<"committed">> => CommittedKeys
                }
        end,
    {ID, Commitment4} =
        case maps:take(<<"id">>, Commitment3) of
            {ExplicitID, Stripped} -> {ExplicitID, Stripped};
            error                  -> {derived_commitment_id(Sig), Commitment3}
        end,
    KeyID = maps:get(<<"keyid">>, Commitment4, <<>>),
    Commitment5 =
        case keyid_to_committer(KeyID) of
            undefined ->
                Commitment4;
            Committer ->
                Commitment4#{
                    <<"committer">> =>
                        maps:get(<<"committer">>, Commitment4, Committer)
                }
        end,
    % Return the commitment and calculated ID.
    {ok, ID, Commitment5}.

%% @doc Read the container delimiters before unescaping each value. The same
%% form applies to every parameter and to nested values.
decode_param(<<",", List/binary>>) ->
    maybe
        [<<>> | Values] ?= binary:split(List, <<":">>, [global]),
        [decode_param(hb_escape:decode(Value)) || Value <- Values]
    end;
decode_param(<<":">>) -> #{};
decode_param(<<":", Map/binary>>) ->
    maps:from_list(
        lists:map(
            fun(Pair) ->
                [Key, Value] = binary:split(Pair, <<":">>),
                {hb_escape:decode(Key), decode_param(hb_escape:decode(Value))}
            end,
            binary:split(Map, <<", ">>, [global])
        )
    );
decode_param(Bin) ->
    hb_escape:decode(Bin).

%% @doc Normalize a list of AO-Core keys to their equivalents in `httpsig@1.0'
%% format. This involves:
%% - If the HTTPSig message given has an `ao-body-key' key and the committed keys
%%   list contains it, we replace it in the list with the `body' key and add the
%%   `ao-body-key' key.
%% - If the list contains a `body' key, we replace it with the `content-digest'
%%   key.
%% - Otherwise, we return the list unchanged.
%% Commitments of devices other than `httpsig@1.0' send their `committed'
%% lists as they are, in order and percent-encoded: those devices verify them.
to_siginfo_keys(Msg, Commitment = #{
        <<"commitment-device">> := <<"httpsig@1.0">> }, Opts) ->
    % The commitment is encoded over the keys of the message that it lists.
    Keys =
        lists:map(
            fun hb_ao:normalize_key/1,
            hb_util:message_to_ordered_list(
                maps:get(<<"committed">>, Commitment, []),
                Opts
            )
        ),
    {ok, _EncMsg, EncComm, _} =
        dev_httpsig:normalize_for_encoding(
            maps:with(Keys, linkify(Msg, Commitment, Opts)),
            Commitment,
            Opts
        ),
    maps:get(<<"committed">>, EncComm);
to_siginfo_keys(_Msg, Commitment, Opts) ->
    lists:map(
        fun hb_escape:encode/1,
        hb_util:message_to_ordered_list(
            maps:get(<<"committed">>, Commitment, []),
            Opts
        )
    ).

%% @doc Replace the messages nested in a message with their links, unless the
%% commitment is made over a bundle. A commitment that is not made over a
%% bundle covers the nested messages by their links, even when the message
%% carries them in full.
linkify(Msg, Commitment, Opts) ->
    case hb_util:atom(maps:get(<<"bundle">>, Commitment, false)) of
        true -> Msg;
        false -> hb_link:normalize(Msg, discard, Opts)
    end.

%% @doc Normalize a list of `httpsig@1.0' keys to their equivalents in AO-Core
%% format. Replace `content-digest' with the body keys, in alphabetical order,
%% remove component prefixes, and restore the body's original key. A
%% `multipart/' `content-type' header with a `boundary' gives the encoding of
%% the body, and is not a message key. The message's own `content-type' may be
%% in the body instead.
from_siginfo_keys(HTTPEncMsg, BodyKeys, SigInfoCommitted) ->
    % 1. Replace the `content-digest' component with the body keys, then remove
    %    specifiers from the other keys and decode them. A `content-digest'
    %    key of the message is one of the body keys. A key of the body that
    %    is also a component of its own, such as a message that a commitment
    %    covers by its link, keeps the place of that component. The
    %    `ao-body-key' component gives the key of the body: the message's own
    %    `ao-body-key' is one of the body keys.
    ComponentKeys =
        fun(<<"content-type">>) ->
                ContentType =
                    maps:get(<<"content-type">>, HTTPEncMsg, undefined),
                case dev_httpsig_conv:boundary(ContentType) of
                    false -> [<<"content-type">>];
                    _ -> []
                end;
           (<<"ao-body-key">>) -> [];
           (<<"@", Key/binary>>) -> [hb_escape:decode(Key)];
           (Key) -> [hb_escape:decode(Key)]
        end,
    Listed =
        lists:flatmap(
            ComponentKeys,
            SigInfoCommitted -- [<<"content-digest">>]
        ),
    WithBody =
        lists:flatmap(
            fun(<<"content-digest">>) -> lists:sort(BodyKeys -- Listed);
               (Component) -> ComponentKeys(Component)
            end,
            SigInfoCommitted
        ),
    % 2. Replace the `body' key again with the value of the `ao-body-key'
    %    header, if its component is present.
    ?event_debug(
        {from_siginfo_keys,
            {body_keys, BodyKeys},
            {raw_committed, SigInfoCommitted},
            {with_body, {explicit, WithBody}}
        }
    ),
    ListWithoutBodyKey =
        case lists:member(<<"ao-body-key">>, SigInfoCommitted) of
            true ->
                WithOrigBodyKey =
                    hb_util:list_replace(
                        WithBody,
                        <<"body">>,
                        maps:get(<<"ao-body-key">>, HTTPEncMsg)
                    ),
                ?event_debug({with_orig_body_key, WithOrigBodyKey}),
                WithOrigBodyKey;
            false ->
                WithBody
        end,
    Normalized = hb_ao:normalize_keys(ListWithoutBodyKey),
    List = hb_util:message_to_ordered_list(Normalized),
    ?event_debug({from_siginfo_keys, {list, List}}),
    List.

%% @doc Convert committed keys to their siginfo format. This involves removing
%% the `body' key from the committed keys, if present, and replacing it with
%% the `content-digest' key.
committed_keys_to_siginfo(Msg) when is_map(Msg) ->
    committed_keys_to_siginfo(hb_util:message_to_ordered_list(Msg));
committed_keys_to_siginfo([]) -> [];
committed_keys_to_siginfo([<<"body">> | Rest]) ->
    [<<"content-digest">> | Rest];
committed_keys_to_siginfo([Key | Rest]) ->
    [Key | committed_keys_to_siginfo(Rest)].

%% @doc Convert an `alg` to a commitment device. If the `alg' has the form of
%% a device specifier (`x@y.z...[/type]'), return the device. Otherwise, we 
%% assume that the `alg' is a `type' of the `httpsig@1.0' algorithm.
%% `type' is an optional key that allows for subtyping of the algorithm. When 
%% provided, in the `alg' it is parsed and returned as the `type' key in the
%% commitment message.
commitment_to_device_specifiers(Commitment, Opts) when is_map(Commitment) ->
    commitment_to_device_specifiers(maps:get(<<"alg">>, Commitment), Opts);
commitment_to_device_specifiers(Alg, _Opts) ->
    case binary:split(Alg, <<"@">>) of
        [Type] ->
            % The `alg' is not a device specifier, so we assume that it is a
            % type of the `httpsig@1.0' algorithm.
            #{
                <<"commitment-device">> => <<"httpsig@1.0">>,
                <<"type">> => Type
            };
        [DevName, Specifiers] ->
            % The `alg' is a device specifier. We determine if a type is present
            % by splitting on the `/` character.
            case binary:split(Specifiers, <<"/">>) of
                [_Version] ->
                    % The `alg' is a device specifier without a type.
                    #{
                        <<"commitment-device">> => Alg
                    };
                [Version, Type] ->
                    % The `alg' is a device specifier with a type.
                    #{
                        <<"commitment-device">> =>
                            <<DevName/binary, "@", Version/binary>>,
                        <<"type">> => Type
                    }
            end
    end.

%% @doc Calculate an `alg' string from a commitment message, using its 
%% `commitment-device' and optionally, its `type' key.
commitment_to_alg(#{ <<"commitment-device">> := <<"httpsig@1.0">>, <<"type">> := Type }, _Opts) ->
    Type;
commitment_to_alg(Commitment, _Opts) ->
    Type =
        case maps:get(<<"type">>, Commitment, undefined) of
            undefined -> <<>>;
            TypeSpecifier -> <<"/", TypeSpecifier/binary>>
        end,
    CommitmentDevice = maps:get(<<"commitment-device">>, Commitment),
    <<CommitmentDevice/binary, Type/binary>>.

%% @doc Convert a `keyid' to the committer of a commitment. The `keyid' is the
%% URI of the committer that made the commitment. The key of an
%% `hmac:constant:' keyid is known to everyone, so its commitments are unsigned.
%% Unsigned commitments and commitments without a `keyid' have no committer, so
%% we return `undefined'.
keyid_to_committer(<<>>) -> undefined;
keyid_to_committer(<<"hmac:constant:", _/binary>>) -> undefined;
keyid_to_committer(KeyID) -> KeyID.

%% @doc Normalize key parameters to ensure their names are correct for inclusion
%% in the `signature-input' and associated keys.
add_derived_specifiers(ComponentIdentifiers) ->
    % Remove the @ prefix from the component identifiers, if present.
    Stripped =
        lists:map(
            fun(<<"@", Key/binary>>) -> Key; (Key) -> Key end,
            ComponentIdentifiers
        ),
    % Add the @ prefix to the component identifiers, if they are derived.
    lists:flatten(
        lists:map(
            fun(Key) ->
                case lists:member(Key, ?DERIVED_COMPONENTS) of
                    true -> << "@", Key/binary >>;
                    false -> Key
                end
            end,
            Stripped
        )
    ).

%% @doc Remove derived specifiers from a list of component identifiers.
remove_derived_specifiers(ComponentIdentifiers) ->
    lists:map(
        fun(<<"@", Key/binary>>) ->
            Key;
        (Key) ->
            Key
        end,
        ComponentIdentifiers
    ).

%%% Tests.

parse_alg_test() ->
    ?assertEqual(
        commitment_to_device_specifiers(#{ <<"alg">> => <<"rsa-pss-sha512">> }, #{}),
        #{
            <<"commitment-device">> => <<"httpsig@1.0">>,
            <<"type">> => <<"rsa-pss-sha512">>
        }
    ),
    ?assertEqual(
        commitment_to_device_specifiers(
            #{ <<"alg">> => <<"ans104@1.0/rsa-pss-sha256">> },
            #{}),
        #{
            <<"commitment-device">> => <<"ans104@1.0">>,
            <<"type">> => ?RSA_SIGN_TYPE
        }
    ).

%% @doc Test that tag values with special characters are correctly encoded and
%% decoded.
escaped_value_test() ->
    KeyID = crypto:strong_rand_bytes(32),
    Committer = hb_util:human_id(ar_wallet:to_address(KeyID)),
    Signature = crypto:strong_rand_bytes(512),
    ID = hb_util:human_id(crypto:hash(sha256, Signature)),
    Commitment = #{
        <<"committed">> => [],
        <<"committer">> => Committer,
        <<"commitment-device">> => <<"tx@1.0">>,
        <<"keyid">> => <<"publickey:", (hb_util:encode(KeyID))/binary>>,
        <<"original-tags">> => #{
            <<"1">> => #{
                <<"name">> => <<"Key">>,
                <<"value">> => <<"value">>
            },
            <<"2">> => #{
                <<"name">> => <<"Quotes">>,
                <<"value">> => <<"{\"function\":\"mint\"}">>
            }
        },
        <<"signature">> => hb_util:encode(Signature),
        <<"type">> => ?RSA_SIGN_TYPE
    },
    SigInfo = commitments_to_siginfo(#{}, #{ ID => Commitment }, #{}),
    Commitments = siginfo_to_commitments(SigInfo, #{}, #{}),
    ?event(debug_test, {siginfo, {explicit, SigInfo}}),
    ?event(debug_test, {commitments, {explicit, Commitments}}),
    ?assertEqual(#{ ID => Commitment }, Commitments),
    lists:foreach(
        fun({Key, Value}) ->
            Comm = Commitment#{ Key => Value },
            ?assertEqual(
                #{ ID => Comm },
                siginfo_to_commitments(
                    commitments_to_siginfo(#{}, #{ ID => Comm }, #{}),
                    #{},
                    #{}
                )
            )
        end,
        [
            {<<"extra">>, <<"a, b">>},
            {<<"extra">>, <<"x:y:z">>},
            {<<"extra">>, <<"v1, 2:k:dg">>},
            {<<"keyid">>, <<"constant:ao, x">>},
            {<<"keyid">>, <<"constant:a:b">>},
            {<<"extra">>, <<",::value">>},
            {<<"extra">>, <<":,,value">>},
            {<<"nonce">>, <<"%41, :\"\\", 0, 255>>},
            {<<"extra">>, <<"value">>},
            {<<"extra">>, [<<"value">>]},
            {<<"extra">>, [<<"a, b">>, <<"x:y:z">>, <<"%41">>]},
            {<<"extra">>, []},
            {<<"extra">>, [<<>>]},
            {<<"extra">>, #{}},
            {<<"extra">>, #{ <<"a:b, %">> => [<<0, 255>>, #{ <<>> => <<>> }] }}
        ]
    ),
    lists:foreach(
        fun({Value, Encoded}) -> ?assertEqual(Encoded, encode_param(Value)) end,
        [
            {<<"constant:ao">>, <<"constant:ao">>},
            {<<"a, b">>, <<"a, b">>},
            {<<"x:y:z">>, <<"x:y:z">>},
            {<<"v1, 2:k:dg">>, <<"v1, 2:k:dg">>},
            {<<",::value">>, <<"%2c::value">>},
            {<<":,,value">>, <<"%3a,,value">>}
        ]
    ).

%% @doc Ordinary map values remain readable, and nested values and keys with
%% delimiters or bytes outside printable ASCII decode to themselves.
original_tag_names_test() ->
    Tag = fun(Name, Value) -> #{ <<"name">> => Name, <<"value">> => Value } end,
    Tags =
        #{
            <<"1">> => Tag(<<"Action">>, <<"Transfer">>),
            <<"2">> => Tag(<<"a:b, c">>, <<"x">>),
            <<"3">> => Tag(<<"%41">>, <<"y">>),
            <<"4">> => Tag(<<255>>, <<"z">>)
        },
    Wire = encode_param(Tags),
    ?assertNotEqual(nomatch, binary:match(Wire, <<"Action">>)),
    ?assertEqual(Tags, decode_param(Wire)).
