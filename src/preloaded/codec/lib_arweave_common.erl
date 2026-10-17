%%% @doc Shared Arweave codec helpers.
-module(lib_arweave_common).
-export([from/3]).
-export([fields/3, tags/2, data/5, committed/6, base/5, verify_identity/3]).
-export([sign/4, order_tags/3]).
-export([verify_committed_keys/5]).
-export([add_commitment/4]).
-export([with_commitments/8]).
-export([bundle_hint/4, data/3, tags/5, excluded_tags/3]).
-export([signed_children_hint/3]).
-export([to/3, to/6, siginfo/4, fields_to_tx/4, decode/1]).
-export([bundle_header/2, bundle_header/3]).
-include("include/hb.hrl").

-define(ANS104_BASE_FIELDS, [<<"anchor">>, <<"target">>]).

%% @doc Convert an ANS-104 item into its message form. An item whose only tag
%% is `ao-type: binary' is a bare binary.
from(Binary, _Req, _Opts) when is_binary(Binary) ->
    {ok, Binary};
from(TX, Req, Opts) when is_record(TX, tx) ->
    case TX#tx.tags of
        [{<<"ao-type">>, <<"binary">>}] -> {ok, TX#tx.data};
        _ -> from_item(TX, Req, Opts)
    end.

from_item(RawTX, Req, Opts) ->
    TX = ar_bundles:deserialize(ar_tx:normalize(RawTX)),
    Fields = fields(TX, <<>>, Opts),
    Tags = tags(TX, Opts),
    Data = data(TX, Req, Tags, fun lib_arweave_common:from/3, Opts),
    Keys = committed(?ANS104_BASE_FIELDS, TX, Fields, Tags, Data, Opts),
    Base = base(Keys, Fields, Tags, Data, Opts),
    FieldCommitments = fields(TX, ?FIELD_PREFIX, Opts),
    {ok,
        with_commitments(
            ?ANS104_BASE_FIELDS, TX, <<"ans104@1.0">>, FieldCommitments,
            Tags, Base, Keys, Opts
        )
    }.

%% @doc Recursively encode a nested message as an `ans104@1.0' #tx record.
to(Binary, _Req, _Opts) when is_binary(Binary) ->
    {ok, #tx{ tags = [{<<"ao-type">>, <<"binary">>}], data = Binary }};
to(TX, _Req, _Opts) when is_record(TX, tx) ->
    {ok, TX};
to(TABM, Req, Opts) when is_map(TABM) ->
    {ok,
        to(
            <<"ans104@1.0">>, TABM, Req,
            fun ?MODULE:fields_to_tx/4,
            fun ?MODULE:excluded_tags/3,
            Opts
        )};
to(Other, _Req, _Opts) ->
    throw({invalid_tx, Other}).

to(Device, TABM, Req, FieldsFun, ExcludedTagsFun, Opts) ->
    MaybeCommitment =
        hb_message:commitment(
            #{ <<"commitment-device">> => Device },
            TABM,
            Opts
        ),
    Data = data(TABM, Req, Opts),
    ?event({calculated_data, Data}),
    TX0 = siginfo(TABM, MaybeCommitment, FieldsFun, Opts),
    ?event({found_siginfo, TX0}),
    TX1 = TX0#tx{ data = Data },
    Tags = tags(
        TX1,
        MaybeCommitment,
        TABM,
        ExcludedTagsFun(TX1, TABM, Opts),
        Opts
    ),
    ?event({calculated_tags, Tags}),
    TX = TX1#tx{ tags = Tags },
    ?event({tx_before_id_gen, TX}),
    try tag_count(ar_tx:normalize(TX), TABM)
    catch
        Type:Error:Stacktrace ->
            ?event({
                {reset_ids_error, Error},
                {tx_without_data, {explicit, TX}}}),
            ?event({prepared_tx_before_ids,
                {tags, {explicit, TX#tx.tags}},
                {data, TX#tx.data}
            }),
            erlang:raise(Type, Error, Stacktrace)
    end.

%% @doc Throw `too_many_keys' if a normalized item has more tags than ANS-104
%% allows. `ar_tx:normalize/1' adds three tags to an item whose data is a
%% bundle, so the tags are counted after it.
tag_count(TX, TABM) when length(TX#tx.tags) > ?MAX_TAG_COUNT ->
    throw({too_many_keys, TABM});
tag_count(TX, _TABM) ->
    TX.

%% @doc Return `Signed', the message of an item signed from `Msg', with the
%% commitments of `Msg' and its own. The item replaces an unsigned commitment
%% of `Msg' by the same device, whose tags and fields it carries.
add_commitment(Device, Msg, Signed, Opts) ->
    Kept =
        hb_message:without_commitments(
            #{
                <<"commitment-device">> => Device,
                <<"type">> => <<"unsigned-sha256">>
            },
            Msg,
            Opts
        ),
    Signed#{
        <<"commitments">> =>
            maps:merge(
                maps:get(<<"commitments">>, Kept, #{}),
                maps:get(<<"commitments">>, Signed)
            )
    }.

%% @doc Return a TABM message containing the fields of the given decoded
%% ANS-104 data item that should be included in the base message.
fields(Item, Prefix, Opts) ->
    lists:foldl(
        fun hb_maps:merge/2,
        #{},
        [
            target_field(Item, Prefix, Opts),
            anchor_field(Item, Prefix, Opts)
        ]
    ).

target_field(Item, Prefix, _Opts) ->
    case Item#tx.target of
        ?DEFAULT_TARGET -> #{};
        Target -> #{<<Prefix/binary, "target">> => hb_util:encode(Target)}
    end.

anchor_field(Item, Prefix, _Opts) ->
    case Item#tx.anchor of
        ?DEFAULT_ANCHOR -> #{};
        Anchor -> #{<<Prefix/binary, "anchor">> => hb_util:encode(Anchor)}
    end.

%% @doc Return a TABM of the raw tags of the item, including all metadata
%% (e.g. `ao-type', `ao-data-key', etc.)
tags(Item, Opts) ->
    hb_ao:normalize_keys(
        deduplicating_from_list(Item#tx.tags, Opts),
        Opts
    ).

%% @doc Normalize a tag name as `structured@1.0' normalizes a key.
normalize_key(Key) -> hb_util:lowercase_key(hb_ao:normalize_key(Key)).

%% @doc Return a TABM of the keys and values found in the data field of the
%% item.
data(Item, Req, Tags, FromFun, Opts) ->
    % If the data field is empty, we return an empty map. If it is a map, we
    % return it as such. Otherwise, we return a map with the data key set to
    % the raw data value. This handles unbundling nested messages, as well as
    % applying the `ao-data-key' tag if given.
    DataKey = maps:get(<<"ao-data-key">>, Tags, <<"data">>),
    case {DataKey, Item#tx.data} of
        {_, ?DEFAULT_DATA} -> #{};
        {DataKey, Empty} when Empty =:= #{}, Item#tx.manifest =:= undefined ->
            % A list bundle of no items is kept as its bytes: as a message of
            % no keys, it would encode as an item with no data.
            {undefined, Bundle} = ar_bundles:serialize_bundle(list, [], false),
            #{ DataKey => Bundle };
        {DataKey, Map} when is_map(Map) ->
            % If the data is a map, we need to recursively turn its children
            % into messages from their tx representations.
            hb_ao:normalize_keys(
                hb_maps:map(
                    fun(_, InnerValue) ->
                        hb_util:ok(FromFun(InnerValue, Req, Opts))
                    end,
                    Map,
                    Opts
                ),
                Opts
            );
        {DataKey, Data} -> #{ DataKey => Data }
    end.

%% @doc Calculate the list of committed keys for an item, based on its
%% components (fields, tags, and data).
committed(FieldKeys, Item, Fields, Tags, Data, Opts) ->
    hb_util:unique(
        data_keys(Data, Opts) ++
        tag_keys(Item, Opts) ++
        field_keys(FieldKeys, Fields, Tags, Data, Opts)
    ).

%% @doc Return the list of the keys from the fields TABM.
field_keys(FieldKeys, BaseFields, Tags, Data, Opts) ->
    lists:filter(
        fun(Key) ->
            hb_maps:is_key(Key, BaseFields, Opts) orelse
            hb_maps:is_key(Key, Tags, Opts) orelse
            hb_maps:is_key(Key, Data, Opts)
        end,
        FieldKeys
    ).

%% @doc Return the list of the keys from the data TABM.
data_keys(Data, Opts) ->
    hb_util:to_sorted_keys(Data, Opts).

%% @doc Return the list of the keys from the tags TABM. Filter all metadata
%% tags: `ao-data-key', `ao-types', `bundle-format', `bundle-version'.
%% We also filter `data` as we don't preserve the a data *field* via
%% `field-data` in the commitment. That means if we promote a `data` tag to
%% a key on the TABM, it will be interpreted as the message's actual data.
%% Instead if a user has provided a `data` tag, we'll preserve it in
%% `original-tags` but will strip it from the top-level message keys.
%% Tags with private names (`hb_private:is_private/1`) are kept in
%% `original-tags` the same way, as `structured@1.0` drops private keys.
tag_keys(Item, _Opts) ->
    MetaTags = [
        <<"bundle-format">>,
        <<"bundle-version">>,
        <<"bundle-map">>,
        <<"ao-data-key">>,
        <<"data">>
    ],
    lists:filtermap(
        fun({Tag, _}) ->
            NormalizedTag = normalize_key(Tag),
            case lists:member(NormalizedTag, MetaTags) orelse
                    hb_private:is_private(NormalizedTag) of
                true -> false;
                false -> {true, NormalizedTag}
            end
        end,
        Item#tx.tags
    ).

%% @doc Return the complete message for an item, less its commitments. The
%% precidence order for choosing fields to place into the base message is:
%% 1. Data
%% 2. Tags
%% 3. Fields
base(CommittedKeys, Fields, Tags, Data, Opts) ->
    hb_maps:from_list(
        lists:map(
            fun(Key) ->
                case hb_maps:find(Key, Data, Opts) of
                    error ->
                        case hb_maps:find(Key, Fields, Opts) of
                            error ->
                                case hb_maps:find(Key, Tags, Opts) of
                                    error -> throw({missing_key, Key});
                                    {ok, Value} -> {Key, Value}
                                end;
                            {ok, Value} -> {Key, Value}
                        end;
                    {ok, Value} -> {Key, Value}
                end
            end,
            CommittedKeys
        )
    ).

%% @doc Check that a commitment's ID and committer are those of the item
%% encoded from the message. The item's values need no check here:
%% `commitment_to_tx/4' takes the commitment's fields and tags only while the
%% message holds their values, and gives a changed message its own, which the
%% item's signature then does not verify.
verify_identity(Item, TABM, Opts) ->
    [{ID, Commitment}] = maps:to_list(maps:get(<<"commitments">>, TABM)),
    case maps:get(<<"type">>, Commitment) of
        <<"unsigned-sha256">> -> not maps:is_key(<<"committer">>, Commitment);
        _ ->
            hb_util:human_id(ID) =:= hb_util:human_id(Item#tx.id) andalso
                maps:get(<<"committer">>, Commitment, unset) =:=
                    committer(Item, Opts)
    end.

%% @doc Return the URI of the committer of an item's signature: the
%% `did-pkh@1.0' committer of the item's owner. If the key has no account, we
%% return `unset'.
committer(Item, Opts) ->
    Committer =
        #{
            <<"device">> => <<"did-pkh@1.0">>,
            <<"owner">> => Item#tx.owner,
            <<"type">> => ar_tx:serialize_sig_type(Item#tx.signature_type)
        },
    ToURI = #{ <<"path">> => <<"to">>, <<"type">> => <<"uri">> },
    case hb_ao:raw(Committer, ToURI, Opts) of
        {ok, URI} -> URI;
        {error, _} -> unset
    end.

%% @doc Sign the item or transaction of a message with the committer of the
%% request, using the signature type of the request's `type'. The committer's
%% public key becomes the owner of the item, which the signature covers. The
%% tags are signed in the order of the request's `committed' keys.
sign(Msg, TX, Req = #{ <<"committer">> := Committer, <<"type">> := Type },
        Opts) ->
    Owner = hb_maps:get(<<"owner">>, Committer, ?DEFAULT_OWNER, Opts),
    Unsigned =
        TX#tx{
            owner = Owner,
            signature_type = ar_tx:deserialize_sig_type(Type),
            tags = order_tags(TX#tx.tags, Req, Opts)
        },
    CommitRes =
        hb_ao:raw(
            Committer,
            #{
                <<"path">> => <<"commit">>,
                <<"body">> => ar_tx:generate_signature_data_segment(Unsigned),
                <<"type">> => Type
            },
            Opts
        ),
    case CommitRes of
        {ok, #{ <<"signature">> := Sig, <<"owner">> := Owner }} ->
            ar_tx:reset_ids(Unsigned#tx{ signature = Sig });
        CommitError -> throw({cannot_commit, CommitError, Msg})
    end.

%% @doc Order the tags of an item as a request lists its `committed' keys. The
%% tags of a bundle come first, and the tags that the request does not list
%% follow the listed ones, in the order they had. A request without a
%% `committed' list leaves the tags as they are.
order_tags(Tags, #{ <<"committed">> := Committed }, Opts) ->
    Order =
        [
            normalize_key(Key)
        ||
            Key <-
                ?BUNDLE_KEYS ++ hb_util:message_to_ordered_list(Committed, Opts)
        ],
    Rank =
        fun({Name, _}) ->
            length(
                lists:takewhile(
                    fun(Key) -> Key =/= normalize_key(Name) end,
                    Order
                )
            )
        end,
    lists:sort(fun(A, B) -> Rank(A) =< Rank(B) end, Tags);
order_tags(Tags, _Req, _Opts) ->
    Tags.

%% @doc Check that a commitment's committed keys are the keys of the item
%% encoded from the message, and that the message gives those keys the types the
%% item does. The keys are the item's data keys, tags and fields, as decoding
%% the item gives them; nested items are not decoded, as only their keys are
%% needed. The item carries the signed types in its `ao-types' tag: an untyped
%% signed value is a binary, so a message that lists it as another type does not
%% verify.
verify_committed_keys(FieldKeys, Item, FieldsFun, TABM, Opts) ->
    [Commitment] = maps:values(maps:get(<<"commitments">>, TABM)),
    TX = ar_bundles:deserialize(Item),
    Fields = FieldsFun(TX, <<>>, Opts),
    Tags = tags(TX, Opts),
    Data = data(TX, #{}, Tags, fun(_, _, _) -> {ok, <<>>} end, Opts),
    CommittedKeys = hb_maps:get(<<"committed">>, Commitment, #{}, Opts),
    lists:sort(committed(FieldKeys, TX, Fields, Tags, Data, Opts)) =:=
        lists:sort(hb_util:message_to_ordered_list(CommittedKeys)) andalso
        holds_types(
            maps:get(<<"ao-types">>, Tags, <<>>),
            Commitment,
            TABM,
            Opts
        ).

%% @doc Return a message with the appropriate commitments added to it.
with_commitments(
        BaseFields, Item, Device, FieldCommitments,
        Tags, Base, CommittedKeys, Opts) ->
    case Item#tx.signature of
        ?DEFAULT_SIG ->
            case normal_tags(BaseFields, Item#tx.tags) of
                true -> Base;
                false ->
                    with_unsigned_commitment(
                        BaseFields, Item, Device, FieldCommitments, Tags, Base,
                        CommittedKeys, Opts)
            end;
        _ -> with_signed_commitment(
                BaseFields, Item, Device, FieldCommitments, Tags, Base,
                CommittedKeys, Opts)
    end.

%% @doc Returns a commitments message for an item, containing an unsigned
%% commitment.
with_unsigned_commitment(
        BaseFields, Item, Device, CommittedFields, Tags,
        UncommittedMessage, CommittedKeys, Opts) ->
    ID = hb_util:human_id(Item#tx.unsigned_id),
    UncommittedMessage#{
        <<"commitments">> => #{
            ID =>
                filter_unset(
                    hb_maps:merge(
                        CommittedFields,
                        #{
                            <<"commitment-device">> => Device,
                            <<"committed">> => CommittedKeys,
                            <<"type">> => <<"unsigned-sha256">>,
                            <<"bundle">> => bundle_commitment_key(Tags, Opts),
                            <<"original-tags">> => original_tags(
                                BaseFields, Item, Opts)
                        },
                        Opts
                    ),
                    Opts
                )
        }
    }.

%% @doc Returns a commitments message for an item, containing a signed
%% commitment.
with_signed_commitment(
        BaseFields, Item, Device, FieldCommitments, Tags,
        UncommittedMessage, CommittedKeys, Opts) ->
    ID = hb_util:human_id(Item#tx.id),
    ExtraCommitments = hb_maps:merge(
        FieldCommitments,
        hb_maps:with(?BUNDLE_KEYS, Tags),
        Opts
    ),
    Commitment =
        filter_unset(
            hb_maps:merge(
                ExtraCommitments,
                #{
                    <<"commitment-device">> => Device,
                    <<"committer">> => committer(Item, Opts),
                    <<"committed">> => CommittedKeys,
                    <<"signature">> => hb_util:encode(Item#tx.signature),
                    <<"owner">> => hb_util:encode(Item#tx.owner),
                    <<"type">> => ar_tx:serialize_sig_type(Item#tx.signature_type),
                    <<"bundle">> => bundle_commitment_key(Tags, Opts),
                    <<"original-tags">> => original_tags(
                        BaseFields, Item, Opts)
                },
                Opts
            ),
            Opts
        ),
    UncommittedMessage#{
        <<"commitments">> => #{
            ID => Commitment
        }
    }.

%% @doc Return the bundle key for an item: whether its nested messages are
%% bundled in its data. An item whose nested messages are not bundled links
%% each with a tag whose value is an `ao:' URI, and bundles only the values
%% that are too large for tags.
bundle_commitment_key(Tags, Opts) ->
    hb_util:bin(
        hb_maps:is_key(<<"bundle-format">>, Tags, Opts) andalso
            not lists:any(
                fun({Key, Value}) -> ?IS_LINK(hb_link:decode(Key, Value)) end,
                hb_maps:to_list(Tags, Opts)
            )
    ).

%% @doc Check whether every tag name is a key as `structured@1.0' reads it, and
%% no name repeats. Other tags need `original-tags' to keep their bytes and
%% order.
normal_tags(BaseFields, Tags) ->
    ReservedFields = [<<"ao-data-key">>, <<"ao-types">>, <<"data">> | BaseFields],
    Keys = [Key || {Key, _} <- Tags],
    length(Keys) =:= length(lists:usort(Keys)) andalso
        lists:all(
            fun({Key, _}) ->
                normalize_key(Key) =:= Key andalso
                not hb_private:is_private(Key) andalso
                not lists:member(Key, ReservedFields)
            end,
            Tags
        ).

%% @doc Return the original tags of an item if it is applicable. Otherwise,
%% return `undefined'.
original_tags(BaseFields, Item, _Opts) ->
    case normal_tags(BaseFields, Item#tx.tags) of
        true -> unset;
        false -> encoded_tags_to_map(Item#tx.tags)
    end.

%% @doc Convert an ANS-104 encoded tag list into a HyperBEAM-compatible map.
encoded_tags_to_map(Tags) ->
    hb_util:list_to_numbered_message(
        lists:map(
            fun({Key, Value}) ->
                #{
                    <<"name">> => Key,
                    <<"value">> => Value
                }
            end,
            Tags
        )
    ).

%% @doc Remove all undefined values from a map.
filter_unset(Map, Opts) ->
    hb_maps:filter(
        fun(_, Value) ->
            case Value of
                unset -> false;
                _ -> true
            end
        end,
        Map,
        Opts
    ).

%% @doc Deduplicate a list of key-value pairs by key, generating a list of
%% values for each normalized key if there are duplicates.
deduplicating_from_list(Tags, Opts) ->
    % Aggregate any duplicated tags into an ordered list of values.
    Aggregated =
        lists:foldl(
            fun({Key, Value}, Acc) ->
                NormKey = normalize_key(Key),
                case hb_maps:get(NormKey, Acc, undefined, Opts) of
                    undefined -> hb_maps:put(NormKey, Value, Acc, Opts);
                    Existing when is_list(Existing) ->
                        hb_maps:put(NormKey, Existing ++ [Value], Acc, Opts);
                    ExistingSingle ->
                        hb_maps:put(NormKey, [ExistingSingle, Value], Acc, Opts)
                end
            end,
            #{},
            Tags
        ),
    ?event({deduplicating_from_list, {aggregated, Aggregated}}),
    % Convert aggregated values into a structured-field list.
    Res =
        hb_maps:map(
            fun(_Key, Values) when is_list(Values) ->
                % Convert Erlang lists of binaries into a structured-field list.
                iolist_to_binary(
                    hb_structured_fields:list(
                        [
                            {item, {string, Value}, []}
                        ||
                            Value <- Values
                        ]
                    )
                );
            (_Key, Value) ->
                Value
            end,
            Aggregated,
            Opts
        ),
    ?event({deduplicating_from_list, {result, Res}}),
    Res.

%%% Encoding helpers.

%% @doc Apply the `bundle' hint from the commitment for `Device', signed or
%% unsigned. Returns `not_found' when no commitment for `Device' exists.
bundle_hint(Device, Msg, Req, Opts) ->
    case hb_message:commitment(
            #{
                <<"commitment-device">> => Device
            },
            Msg,
            Opts) of
        {ok, _, Commitment} ->
            case hb_util:atom(
                    hb_maps:get(<<"bundle">>, Commitment, not_found, Opts)) of
                not_found -> {ok, Req};
                Value -> {ok, Req#{ <<"bundle">> => Value }}
            end;
        _ -> not_found
    end.

%% @doc Turn off bundling for a message with a nested message signed by a
%% device other than `ans104@1.0': its nested messages are then linked. An item
%% in a bundle can hold only its own ANS-104 signature, so bundling would drop
%% the nested message's other commitments.
signed_children_hint(Msg, {ok, Req = #{ <<"bundle">> := true }}, Opts) ->
    Children =
        hb_maps:values(
            hb_maps:without([<<"commitments">>, <<"priv">>], Msg, Opts),
            Opts
        ),
    case lists:any(fun(Child) -> signed_by_other(Child, Opts) end, Children) of
        true -> {ok, Req#{ <<"bundle">> => false }};
        false -> {ok, Req}
    end;
signed_children_hint(_Msg, Hint, _Opts) ->
    Hint.

%% @doc Check whether a value is a message with a signed commitment of a device
%% other than `ans104@1.0'.
signed_by_other(Value, Opts) ->
    lists:any(
        fun(Commitment) ->
            hb_maps:get(<<"commitment-device">>, Commitment, undefined, Opts)
                =/= <<"ans104@1.0">>
        end,
        hb_maps:values(
            hb_message:commitments(#{ <<"committer">> => '_' }, Value, Opts),
            Opts
        )
    ).

%% @doc Calculate the fields for a message, returning an initial TX record.
siginfo(Message, {ok, _, Commitment}, FieldsFun, Opts) ->
    commitment_to_tx(Message, Commitment, FieldsFun, Opts);
siginfo(Message, not_found, FieldsFun, Opts) ->
    FieldsFun(#tx{}, <<>>, Message, Opts);
siginfo(Message, multiple_matches, _FieldsFun, _Opts) ->
    throw({multiple_ans104_commitments_unsupported, Message}).

%% @doc Convert a commitment to a base TX record. Its fields and tags are the
%% commitment's while the message holds their values. A message changed after
%% signing is given its own fields and committed keys' values instead, so the
%% commitment's signature does not verify the item.
commitment_to_tx(Message, Commitment, FieldsFun, Opts) ->
    Signature =
        hb_util:decode(
            maps:get(<<"signature">>, Commitment, hb_util:encode(?DEFAULT_SIG))
        ),
    Owner =
        case hb_maps:find(<<"owner">>, Commitment, Opts) of
            {ok, EncodedOwner} -> hb_util:decode(EncodedOwner);
            error -> ?DEFAULT_OWNER
        end,
    Tags =
        case hb_maps:find(<<"original-tags">>, Commitment, Opts) of
            {ok, OriginalTags} -> original_tags_to_tags(OriginalTags);
            error -> []
        end,
    SignatureType = ar_tx:deserialize_sig_type(
        maps:get(<<"type">>, Commitment)
    ),
    ?event({commitment_owner, Owner}),
    ?event({commitment_signature, Signature}),
    ?event({commitment_signature_type, SignatureType}),
    ?event({commitment_tags, Tags}),
    TX = #tx{
        owner = Owner,
        signature = Signature,
        signature_type = SignatureType,
        tags = Tags
    },
    case holds_signed_values(Commitment, Tags, Message, Opts) of
        true -> FieldsFun(TX, ?FIELD_PREFIX, Commitment, Opts);
        false -> FieldsFun(TX#tx{ tags = [] }, <<>>, Message, Opts)
    end.

%% @doc Check that a message still holds the values of the fields and tags
%% that its commitment restores, and the types that the tags give to its
%% committed keys. Keys added after signing are not compared.
holds_signed_values(Commitment, OriginalTags, TABM, Opts) ->
    Fields =
        maps:from_list(
            [
                {Key, Value}
            ||
                {<<"field-", Key/binary>>, Value}
                    <- hb_maps:to_list(Commitment, Opts)
            ]
        ),
    holds(Fields, TABM, Opts) andalso
        holds_tags(OriginalTags, maps:keys(Fields), Commitment, TABM, Opts).

%% @doc Check that a message holds the values of its commitment's tags, and
%% the types that the `ao-types' tag gives to its committed keys. A tag with
%% the name of a field is skipped: decoding takes the field's value.
holds_tags([], _FieldKeys, _Commitment, _TABM, _Opts) ->
    true;
holds_tags(OriginalTags, FieldKeys, Commitment, TABM, Opts) ->
    Tags = deduplicating_from_list(OriginalTags, Opts),
    Keys =
        hb_util:list_without(
            [<<"ao-types">> | FieldKeys],
            tag_keys(#tx{ tags = OriginalTags }, Opts)
        ),
    holds(maps:with(Keys, Tags), TABM, Opts) andalso
        holds_types(
            maps:get(<<"ao-types">>, Tags, <<>>),
            Commitment,
            TABM,
            Opts
        ).

%% @doc Check that a message holds the given values for their keys. Lazy
%% values, as in a message read from a cache, are loaded before comparing.
holds(Values, TABM, Opts) ->
    hb_cache:ensure_all_loaded(Values, Opts) ==
        hb_cache:ensure_all_loaded(maps:with(maps:keys(Values), TABM), Opts).

%% @doc Check that the message gives its committed keys the types that the
%% commitment's `ao-types' tag gives them. Equal `ao-types' values need no
%% decoding.
holds_types(Types, _Commitment, #{ <<"ao-types">> := Types }, _Opts) ->
    true;
holds_types(SignedTypes, Commitment, TABM, Opts) ->
    Committed =
        [
            normalize_key(Key)
        ||
            Key <-
                hb_util:message_to_ordered_list(
                    hb_maps:get(<<"committed">>, Commitment, #{}, Opts)
                )
        ],
    maps:with(Committed, decode_types(SignedTypes, Opts)) ==
        maps:with(
            Committed,
            decode_types(hb_maps:get(<<"ao-types">>, TABM, <<>>, Opts), Opts)
        ).

%% @doc Decode an `ao-types' value to a map of normalized keys to types.
decode_types(<<>>, _Opts) ->
    #{};
decode_types(Types, Opts) ->
    {ok, Decoded} =
        hb_ao:raw(<<"structured@1.0">>, <<"decode-types">>, Types, #{}, Opts),
    maps:fold(
        fun(Key, Type, Acc) -> Acc#{ normalize_key(Key) => Type } end,
        #{},
        Decoded
    ).

%% @doc Convert a HyperBEAM-compatible message into an ANS-104 tag list.
original_tags_to_tags(TagMap) ->
    OrderedList = hb_util:message_to_ordered_list(hb_private:reset(TagMap)),
    ?event({ordered_tagmap, {explicit, OrderedList}, {input, {explicit, TagMap}}}),
    lists:map(
        fun(#{ <<"name">> := Key, <<"value">> := Value }) ->
            {Key, Value}
        end,
        OrderedList
    ).

fields_to_tx(TX, Prefix, Map, Opts) ->
    Anchor =
        case hb_maps:find(<<Prefix/binary, "anchor">>, Map, Opts) of
            {ok, EncodedAnchor} ->
                case decode(EncodedAnchor) of
                    {ok, DecodedAnchor} when ?IS_ID(DecodedAnchor) ->
                        DecodedAnchor;
                    _ -> ?DEFAULT_ANCHOR
                end;
            error -> ?DEFAULT_ANCHOR
        end,
    Target =
        case hb_maps:find(<<Prefix/binary, "target">>, Map, Opts) of
            {ok, EncodedTarget} ->
                case decode(EncodedTarget) of
                    {ok, DecodedTarget} when ?IS_ID(DecodedTarget) ->
                        DecodedTarget;
                    _ -> ?DEFAULT_TARGET
                end;
            error -> ?DEFAULT_TARGET
        end,
    ?event({fields_to_tx, {prefix, Prefix}, {anchor, Anchor}, {target, Target}}),
    TX#tx{
        anchor = Anchor,
        target = Target
    }.

%% @doc Decode the base64url string of a field. A string other than the
%% encoding of the bytes it decodes to, such as one with other padding bits, is
%% refused: a signature over the bytes covers that encoding alone.
decode(Encoded) ->
    maybe
        {ok, Decoded} ?= hb_util:safe_decode(Encoded),
        true ?= hb_util:encode(Decoded) =:= Encoded orelse {error, invalid},
        {ok, Decoded}
    end.

%% @doc Calculate the data field for a message.
data(TABM, Req, Opts) ->
    DataKey = inline_key(TABM),
    UnencodedNestedMsgs = data_messages(TABM, Opts),
    NestedMsgs =
        hb_maps:map(
            fun(_, Msg) ->
                hb_util:ok(to(Msg, Req, Opts))
            end,
            UnencodedNestedMsgs,
            Opts
        ),
    DataVal = hb_maps:get(DataKey, TABM, ?DEFAULT_DATA),
    ?event(debug_data, {data_val, DataVal}),
    case {DataVal, hb_maps:size(NestedMsgs, Opts)} of
        {Binary, 0} when is_binary(Binary) ->
            Binary;
        {?DEFAULT_DATA, _} ->
            NestedMsgs;
        {DataVal, _} ->
            NestedMsgs#{ DataKey => hb_util:ok(to(DataVal, Req, Opts)) }
    end.

%% @doc Calculate data messages for large tag values or nested messages. A
%% message's own `ao-type: binary' is carried in the data too, so that only the
%% item of a bare binary has that tag.
data_messages(TABM, Opts) when is_map(TABM) ->
    UncommittedTABM =
        hb_maps:without(
            [<<"commitments">>, <<"data">>, <<"target">>],
            hb_private:reset(TABM),
            Opts
        ),
    DataMessages = hb_maps:filter(
        fun(Key, Value) ->
            case is_map(Value) of
                true -> true;
                false ->
                    byte_size(Value) > ?MAX_TAG_VALUE_SIZE
                    orelse byte_size(Key) > ?MAX_TAG_NAME_SIZE
                    orelse {Key, Value} =:= {<<"ao-type">>, <<"binary">>}
            end
        end,
        UncommittedTABM,
        Opts
    ),
    TagCount = map_size(UncommittedTABM) - map_size(DataMessages),
    if TagCount > ?MAX_TAG_COUNT ->
        throw({too_many_keys, UncommittedTABM});
    true ->
        DataMessages
    end.

%% @doc Calculate the tags field for a data item.
tags(#tx{ tags = ExistingTags }, _, _, _, _) when ExistingTags =/= [] ->
    ExistingTags;
tags(TX, MaybeCommitment, TABM, ExcludedTagKeys, Opts) ->
    CommittedTagKeys = committed_tag_keys(MaybeCommitment, TABM, Opts),
    DataKeysToExclude =
        case TX#tx.data of
            Data when is_map(Data)-> maps:keys(Data);
            _ -> []
        end,
    TagKeys = hb_util:list_without(
        ExcludedTagKeys ++ DataKeysToExclude,
        CommittedTagKeys
    ),
    bundle_tags_to_tags(MaybeCommitment) ++
        committed_tag_keys_to_tags(TABM, TagKeys, Opts).

committed_tag_keys({ok, _, Commitment}, _TABM, Opts) ->
    lists:map(
        fun hb_ao:normalize_key/1,
        hb_util:message_to_ordered_list(
            hb_util:ok(
                hb_maps:find(<<"committed">>, Commitment, Opts)
            )
        )
    );
committed_tag_keys(not_found, TABM, Opts) ->
    hb_util:list_without(
        [<<"commitments">>],
        hb_util:to_sorted_keys(hb_private:reset(TABM), Opts)
    );
committed_tag_keys(multiple_matches, TABM, _Opts) ->
    throw({multiple_ans104_commitments_unsupported, TABM}).

%% @doc Return a list of base fields that should be excluded from the tags.
excluded_tags(TX, TABM, Opts) ->
    exclude_target_tag(TX, TABM, Opts) ++
    exclude_anchor_tag(TX, TABM, Opts).

exclude_target_tag(TX, TABM, Opts) ->
    case {TX#tx.target, hb_maps:get(<<"target">>, TABM, undefined, Opts)} of
        {?DEFAULT_TARGET, _} -> [];
        {FieldTarget, TagTarget} when FieldTarget =/= TagTarget ->
            [<<"target">>];
        _ -> []
    end.

exclude_anchor_tag(TX, TABM, Opts) ->
    case {TX#tx.anchor, hb_maps:get(<<"anchor">>, TABM, undefined, Opts)} of
        {?DEFAULT_ANCHOR, _} -> [];
        {FieldAnchor, TagAnchor} when FieldAnchor =/= TagAnchor ->
            [<<"anchor">>];
        _ -> []
    end.

%% @doc Apply the `ao-data-key' to the committed keys.
committed_tag_keys_to_tags(TABM, Committed, Opts) ->
    DataKey = inline_key(TABM),
    ?event(
        {tags_before_data_key,
            {tag_keys, Committed},
            {data_key, DataKey},
            {tabm, TABM}
        }),
    case DataKey of
        <<"data">> -> [];
        _ -> [{<<"ao-data-key">>, DataKey}]
    end ++
    lists:map(
        fun(Key) ->
            case hb_maps:find(Key, TABM, Opts) of
                error -> throw({missing_committed_key, Key});
                {ok, Value} -> {Key, Value}
            end
        end,
        [Key || Key <- Committed,
            Key =/= DataKey orelse hb_maps:get(Key, TABM, none, Opts) == <<>>]
    ).

bundle_tags_to_tags({ok, _, Commitment}) ->
    lists:flatmap(
        fun(Key) ->
            case hb_maps:find(Key, Commitment) of
                {ok, Value} -> [{Key, Value}];
                error -> []
            end
        end,
        ?BUNDLE_KEYS
    );
bundle_tags_to_tags(_) ->
    [].

%% @doc Determine if an `ao-data-key' should be added to the message.
inline_key(Msg) ->
    InlineKey = maps:get(<<"ao-data-key">>, Msg, undefined),
    case {
        InlineKey,
        maps:get(<<"data">>, Msg, ?DEFAULT_DATA) == ?DEFAULT_DATA,
        maps:is_key(<<"body">>, Msg)
            andalso not ?IS_LINK(
                hb_link:decode(maps:get(<<"body">>, Msg, undefined))
            )
    } of
        {Explicit, _, _} when Explicit =/= undefined ->
            InlineKey;
        {_, true, true} ->
            <<"body">>;
        _ ->
            <<"data">>
    end.

%% @doc Read and decode the bundle header index at the given global start
%% offset, returning the header size alongside the decoded index entries.
bundle_header(BundleStartOffset, Opts) ->
    bundle_header(BundleStartOffset, infinity, Opts).
bundle_header(BundleStartOffset, MaxSize, Opts) ->
    case hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"chunk">>,
            <<"offset">> => BundleStartOffset + 1
        },
        Opts
    ) of
        {ok, FirstChunk} ->
            case ar_bundles:bundle_header_size(FirstChunk) of
                invalid_bundle_header ->
                    {error, invalid_bundle_header};
                HeaderSize when HeaderSize > MaxSize ->
                    {error, invalid_bundle_header};
                HeaderSize ->
                    case read_bundle_header(
                        BundleStartOffset, HeaderSize, FirstChunk, Opts
                    ) of
                        {ok, HeaderBin} ->
                            case ar_bundles:decode_bundle_header(HeaderBin) of
                                {_Items, BundleIndex} ->
                                    {ok, HeaderSize, BundleIndex};
                                invalid_bundle_header ->
                                    {error, invalid_bundle_header}
                            end;
                        Error ->
                            Error
                    end
            end;
        Error ->
            Error
    end.

%% @doc Read exactly the bytes needed to decode a bundle header.
read_bundle_header(_BundleStartOffset, HeaderSize, FirstChunk, _Opts)
        when HeaderSize =< byte_size(FirstChunk) ->
    {ok, binary:part(FirstChunk, 0, HeaderSize)};
read_bundle_header(BundleStartOffset, HeaderSize, FirstChunk, Opts) ->
    RemainingSize = HeaderSize - byte_size(FirstChunk),
    case hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"chunk">>,
            <<"offset">> => BundleStartOffset + byte_size(FirstChunk) + 1,
            <<"length">> => RemainingSize
        },
        Opts
    ) of
        {ok, RemainingChunk} ->
            {ok, <<FirstChunk/binary, RemainingChunk/binary>>};
        Error ->
            Error
    end.
