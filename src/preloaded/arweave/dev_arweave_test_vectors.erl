%%% @doc Test vectors for the Arweave codecs, `tx@1.0' and `ans104@1.0'.
-module(dev_arweave_test_vectors).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Options for each test: a new store and the node's wallet.
test_opts() ->
    #{
        <<"store">> => hb_test_utils:test_store(),
        <<"priv-wallet">> => hb:wallet()
    }.

%% @doc Commitment identities belong to the signature or unsigned digest.
commitment_identity_test_() ->
    [
        {binary_to_list(Codec) ++ ": " ++ atom_to_list(Kind), fun() ->
            Opts = test_opts(),
            Msg = case Kind of
                signed ->
                    hb_message:commit(
                        #{ <<"body">> => <<"original">> }, Opts, Codec);
                unsigned ->
                    hb_message:convert(
                        ar_tx:normalize(#tx{
                            format = Format,
                            tags = [{<<"Example-Key">>, <<"value">>}],
                            data = <<"original">>
                        }),
                        <<"structured@1.0">>, Codec, Opts
                    )
            end,
            Verify = fun(M) ->
                hb_message:verify(M,
                    #{ <<"ids">> => <<"all">> }, Opts)
            end,
            [{ID, Commitment}] = maps:to_list(maps:get(<<"commitments">>, Msg)),
            FalseID = hb_util:human_id(<<0:256>>),
            ?assert(Verify(Msg)),
            ?assertNot(Verify(Msg#{ <<"commitments">> =>
                #{ FalseID => Commitment } })),
            ?assertNot(Verify(Msg#{ <<"commitments">> =>
                #{ ID => Commitment#{ <<"committer">> => FalseID } } }))
        end}
    || {Codec, Format} <- [{<<"ans104@1.0">>, ans104}, {<<"tx@1.0">>, 2}],
        Kind <- [signed, unsigned]
    ].

%% @doc Reserved private tags keep their signed bytes without becoming local
%% private state or losing verification through conversion and storage.
private_tags_test_() ->
    [
        {binary_to_list(Codec) ++ ": " ++ binary_to_list(Name) ++
                " / " ++ atom_to_list(Typed), fun() ->
            Opts = test_opts(),
            Raw = #tx{
                format = Format,
                tags = [{Name, <<"True">>}, {<<"public">>, <<"1">>}] ++
                    case Typed of
                        true -> [{<<"ao-types">>, <<"integer=(\"public\")">>}];
                        false -> []
                    end
            },
            Signed = case Codec of
                <<"tx@1.0">> -> ar_tx:sign(Raw, hb:wallet());
                <<"ans104@1.0">> -> ar_bundles:sign_item(Raw, hb:wallet())
            end,
            lists:foreach(
                fun(TX) ->
                    Msg = hb_message:convert(
                        TX, <<"structured@1.0">>, Codec, Opts),
                    Back = hb_message:convert(Msg, Codec, Opts),
                    ?assertEqual(TX#tx.tags, Back#tx.tags),
                    ?assertEqual(TX#tx.id, Back#tx.id),
                    ?assertNot(lists:any(fun hb_private:is_private/1,
                        maps:keys(Msg))),
                    ?assert(hb_message:verify(Msg, all, Opts))
                end,
                [ar_tx:normalize(Raw), Signed]
            ),
            Msg = hb_message:convert(
                Signed, <<"structured@1.0">>, Codec, Opts),
            {ok, _} = hb_cache:write(Msg, Opts),
            ID = hb_util:human_id(Signed#tx.id),
            {ok, Cached} = hb_cache:read(ID, Opts),
            ?assert(hb_message:verify(Cached, all, Opts)),
            Back = hb_message:convert(Cached, Codec, Opts),
            ?assertEqual(Signed#tx.tags, Back#tx.tags),
            ?assertEqual(Signed#tx.id, Back#tx.id),
            Private = Msg#{ <<"priv">> => #{ <<"secret">> => <<"local">> } },
            ?assertEqual(Signed#tx.tags,
                (hb_message:convert(Private, Codec, Opts))#tx.tags),
            ?assertNot(hb_message:verify(
                Msg#{ <<"public">> => <<"changed">> }, all, Opts)),
            Commitments = maps:get(<<"commitments">>, Msg),
            Commitment = maps:get(ID, Commitments),
            Tags = maps:get(<<"original-tags">>, Commitment),
            ChangedTag = (maps:get(<<"1">>, Tags))#{
                <<"value">> => <<"False">>
            },
            ?assertNot(hb_message:verify(Msg#{ <<"commitments">> =>
                Commitments#{ ID => Commitment#{ <<"original-tags">> =>
                    Tags#{ <<"1">> => ChangedTag } } } }, all, Opts))
        end}
    || {Codec, Format} <- [{<<"tx@1.0">>, 2}, {<<"ans104@1.0">>, ans104}],
        Name <- [<<"Private">>, <<"private">>, <<"priv">>, <<"PRIV-Tag">>,
            <<"private-field+link">>, <<"Private-Field+link">>],
        Typed <- [false, true]
    ].

%% @doc A commitment's `original-tags' with another value than the item's tags
%% does not verify, even when the message carries the tag's key.
forged_original_tags_test_() ->
    [
        {binary_to_list(Codec) ++ ": forged public tag", fun() ->
            Opts = test_opts(),
            Raw = #tx{
                format = Format,
                tags = [
                    {<<"action">>, <<"Transfer">>},
                    {<<"b">>, <<"2">>}
                ]
            },
            Signed = case Codec of
                <<"ans104@1.0">> -> ar_bundles:sign_item(Raw, hb:wallet());
                <<"tx@1.0">> -> ar_tx:sign(Raw, hb:wallet())
            end,
            Msg = hb_message:convert(
                Signed, <<"structured@1.0">>, Codec, Opts
            ),
            [{ID, Commitment}] = maps:to_list(
                maps:get(<<"commitments">>, Msg)
            ),
            Forged = Commitment#{
                <<"original-tags">> => #{
                    <<"1">> => #{
                        <<"name">> => <<"action">>,
                        <<"value">> => <<"Forged">>
                    }
                }
            },
            ?assertNot(
                hb_message:verify(
                    Msg#{ <<"commitments">> => #{ ID => Forged } },
                    all,
                    Opts
                )
            )
        end}
    || {Codec, Format} <- [{<<"tx@1.0">>, 2}, {<<"ans104@1.0">>, ans104}]
    ].

%% @doc A private link is not a committed key either, whatever `ao-types'
%% declares. Removing or changing its signed tag fails verification.
private_link_binding_test_() ->
    [
        {binary_to_list(Codec) ++ ": " ++ binary_to_list(TypeKey), fun() ->
            Opts = test_opts(),
            Raw = #tx{ format = Format, tags = [
                {Name, <<"ao:", (hb_util:human_id(<<1:256>>))/binary>>},
                {<<"ao-types">>, <<"list=(\"", TypeKey/binary, "\")">>},
                {<<"public">>, <<"ok">>}
            ] },
            Signed = case Codec of
                <<"tx@1.0">> -> ar_tx:sign(Raw, hb:wallet());
                <<"ans104@1.0">> -> ar_bundles:sign_item(Raw, hb:wallet())
            end,
            TABM = hb_message:convert(Signed, tabm, Codec, Opts),
            Key = hb_util:to_lower(Name),
            [Commitment] = maps:values(maps:get(<<"commitments">>, TABM)),
            CommittedKeys =
                hb_util:message_to_ordered_list(
                    maps:get(<<"committed">>, Commitment)
                ),
            ?assertNot(lists:member(Key, CommittedKeys)),
            [_ | Tags] = Signed#tx.tags,
            lists:foreach(
                fun(ChangedTags) ->
                    Changed =
                        hb_message:convert(
                            Signed#tx{ tags = ChangedTags },
                            tabm,
                            Codec,
                            Opts
                        ),
                    ?assertEqual({ok, false},
                        hb_ao:raw(Codec, <<"verify">>, Changed, #{}, Opts))
                end,
                [Tags, [{Name, hb_util:human_id(<<2:256>>)} | Tags]]
            )
        end}
    ||
        {Codec, Format} <- [{<<"tx@1.0">>, 2}, {<<"ans104@1.0">>, ans104}],
        {Name, TypeKey} <- [
            {<<"private-field">>, <<"private-field">>},
            {<<"Private-Field">>, <<"Private-Field">>},
            {<<"Private-Field">>, <<"%50rivate-%46ield">>},
            {<<"private-field+link">>, <<"private-field%2blink">>},
            {<<"Private">>, <<"priv-unused">>},
            {<<"Private">>, <<"%50riv-unused">>}
        ]
    ].

%% @doc Private tags preserve duplicate names, order and signed values.
%% A commitment with no public message keys verifies as any other.
duplicate_private_tags_test_() ->
    [
        {binary_to_list(Codec) ++ ": " ++ atom_to_list(Public), fun() ->
            Opts = test_opts(),
            Raw = #tx{ format = Format, tags = [
                {<<"Private">>, <<"first">>},
                {<<"private">>, <<"second">>},
                {<<"Private">>, <<"third">>}
            ] ++ case Public of
                true -> [{<<"public">>, <<"kept">>}];
                false -> []
            end },
            Signed = case Codec of
                <<"tx@1.0">> -> ar_tx:sign(Raw, hb:wallet());
                <<"ans104@1.0">> -> ar_bundles:sign_item(Raw, hb:wallet())
            end,
            Msg = hb_message:convert(Signed, <<"structured@1.0">>, Codec, Opts),
            Back = hb_message:convert(Msg, Codec, Opts),
            ?assertEqual({Signed#tx.tags, Signed#tx.data, Signed#tx.id},
                {Back#tx.tags, Back#tx.data, Back#tx.id}),
            ?assert(case Codec of
                <<"tx@1.0">> -> ar_tx:verify(Back);
                <<"ans104@1.0">> -> ar_bundles:verify_item(Back)
            end),
            ?assert(hb_message:verify(Msg, all, Opts)),
            [{ID, Commitment}] = maps:to_list(maps:get(<<"commitments">>, Msg)),
            Tags = maps:get(<<"original-tags">>, Commitment),
            Reordered = Msg#{ <<"commitments">> =>
                #{ ID => Commitment#{ <<"original-tags">> => Tags#{
                    <<"1">> => maps:get(<<"2">>, Tags),
                    <<"2">> => maps:get(<<"1">>, Tags)
                } } } },
            ?assertNot(hb_message:verify(Reordered, all, Opts)),
            ReorderedTX = hb_message:convert(Reordered, Codec, Opts),
            ?assertNot(case Codec of
                <<"tx@1.0">> -> ar_tx:verify(ReorderedTX);
                <<"ans104@1.0">> -> ar_bundles:verify_item(ReorderedTX)
            end)
        end}
    || {Codec, Format} <- [{<<"tx@1.0">>, 2}, {<<"ans104@1.0">>, ans104}],
        Public <- [false, true]
    ].

rsa_wallet_not_match_message_ed25519_type_test() ->
    Opts = #{<<"priv-wallet">> => ar_wallet:new(?RSA_KEY_TYPE)},
    SignatureType = ?EDDSA_SIGN_TYPE,
    Msg = #{<<"a">> => <<"b">>},
    ?assertThrow(
        {cannot_commit, {error, #{ <<"error">> := <<"unsupported-key-type">> }}, _},
        hb_message:commit(
            Msg,
            Opts,
            #{<<"commitment-device">> => <<"ans104@1.0">>, <<"type">> => SignatureType}
        )).

ed25519_wallet_not_match_message_rsa_type_test() ->
    Opts = #{<<"priv-wallet">> => ar_wallet:new(?EDDSA_KEY_TYPE)},
    SignatureType = ?RSA_SIGN_TYPE,
    Msg = #{<<"a">> => <<"b">>},
    ?assertThrow(
        {cannot_commit, {error, #{ <<"error">> := <<"unsupported-key-type">> }}, _},
        hb_message:commit(
            Msg,
            Opts,
            #{<<"commitment-device">> => <<"ans104@1.0">>, <<"type">> => SignatureType}
        )).

ethereum_wallet_not_match_message_rsa_type_test() ->
    Opts = #{<<"priv-wallet">> => ar_wallet:new(?ETHEREUM_KEY_TYPE)},
    SignatureType = ?RSA_SIGN_TYPE,
    Msg = #{<<"a">> => <<"b">>},
    ?assertThrow(
        {cannot_commit, {error, #{ <<"error">> := <<"unsupported-key-type">> }}, _},
        hb_message:commit(
            Msg,
            Opts,
            #{<<"commitment-device">> => <<"ans104@1.0">>, <<"type">> => SignatureType}
        )).

ethereum_wallet_match_message_ethereum_type_test() ->
    Opts = #{<<"priv-wallet">> => ar_wallet:new(?ETHEREUM_KEY_TYPE)},
    SignatureType = <<"ethereum">>,
    Msg = #{<<"a">> => <<"b">>},
    ?assert(is_map(hb_message:commit(
            Msg,
            Opts,
            #{<<"commitment-device">> => <<"ans104@1.0">>, <<"type">> => SignatureType}
        ))).
