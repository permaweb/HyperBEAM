%%% @doc Local H1 regressions: receipt origins must not load omitted values.
-module(hb_hashpath_h1_test_vectors).
-include_lib("eunit/include/eunit.hrl").

%% @doc Receipt generation must not load a photo that Vary discarded.
receipt_loads_ignored_photo_test() ->
    Options = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"attested-store">> => hb_test_utils:test_store(),
        <<"cache-control">> => [<<"no-cache">>, <<"no-store">>],
        <<"spawn-worker">> => false,
        <<"await-inprogress">> => false
    },
    % Calculate the photo's address, but do not put its bytes in the store.
    PhotoID = hb_message:id(<<"photo bytes">>, all, Options),
    PhotoLink = {link, <<"data/", PhotoID/binary>>, #{}},
    % This device's spec selects required and deep.slot, but NOT photo.
    Base = #{
        <<"device">> => <<"test-device@1.0">>,
        <<"required">> => 7,
        <<"deep">> => #{ <<"slot">> => 8 },
        <<"photo">> => PhotoLink
    },
    Request = #{
        <<"path">> => <<"vary-projection">>,
        <<"deep-request">> => #{ <<"slot">> => 9 }
    },
    % Without a receipt, resolution succeeds. The device never needs the photo.
    {ok, ResultWithoutReceipt} = hb_ao:resolve(
        Base, Request, Options#{ <<"hashpath">> => ignore }
    ),
    ?assertEqual(
        #{
            <<"device">> => <<"test-device@1.0">>,
            <<"required">> => 7,
            <<"deep">> => #{ <<"slot">> => 8 }
        },
        maps:get(<<"base">>, ResultWithoutReceipt)
    ),
    % Ask for the same computation WITH a receipt. Nothing else changes.
    % The result succeeds, but its receipt needs the omitted photo bytes.
    {ok, ResultWithReceiptRequested} = hb_ao:resolve(
        Base, Request, Options#{ <<"hashpath">> => update }
    ),
    ?assertEqual(ResultWithoutReceipt,
        hb_private:reset(ResultWithReceiptRequested)),
    ?assertMatch(#{
        <<"hashpath-status">> := <<"unavailable">>,
        <<"hashpath-reason">> := <<"unresolved-original-input">>
    }, hb_private:from_message(ResultWithReceiptRequested)),
    ?assertEqual({error, hashpath_unavailable},
        hb_path:hashpath(ResultWithReceiptRequested, Options)).

%% @doc An available photo is still not fetched just to create a receipt.
available_photo_is_skipped_test() ->
    Options = options(enabled),
    {Base, Request} = inputs(),
    {ok, PhotoPath} = hb_cache:write(<<"photo bytes">>, Options),
    PhotoLink = {link, PhotoPath, #{}},
    WithPhoto = Base#{ <<"photo">> => PhotoLink },
    {ok, Result} = hb_ao:resolve(WithPhoto, Request, Options),
    ?assertEqual(<<"unavailable">>,
        maps:get(<<"hashpath-status">>, hb_private:from_message(Result))),
    % The caller's link remains loadable and carries no internal guard.
    ?assertEqual(<<"photo bytes">>, hb_cache:ensure_loaded(PhotoLink, Options)),
    {ok, Context} = hb_ao:resolve(WithPhoto, Request,
        Options#{ <<"return-context">> => true }),
    ?assertEqual(WithPhoto, maps:get(<<"base">>, Context)),
    ?assertEqual({error, hashpath_unavailable}, hb_hashpath:format(Context, Options)),
    % A loaded photo can be addressed normally, and leaves no stale diagnostic.
    {ok, WithReceipt} = hb_ao:resolve(
        Base#{ <<"photo">> => <<"photo bytes">>,
            <<"priv">> => hb_private:from_message(Result) }, Request, Options),
    ?assertNot(maps:is_key(<<"hashpath-status">>,
        hb_private:from_message(WithReceipt))),
    ?assert(hb_hashpath:verify_all(hb_path:hashpath(WithReceipt, Options), Options)).

%% @doc A cache hit still reports this caller's inability to make a receipt.
cached_result_with_missing_photo_test() ->
    Options = options(enabled),
    {Base, Request} = inputs(),
    {ok, FirstResult} = hb_ao:resolve(Base, Request, Options),
    PhotoID = hb_message:id(<<"unstored photo">>, all, Options),
    {ok, CachedResult} = hb_ao:resolve(
        Base#{ <<"photo">> => {link, <<"data/", PhotoID/binary>>, #{}} },
        Request, Options#{ <<"cache-control">> => [<<"only-if-cached">>] }),
    ?assertEqual(hb_private:reset(FirstResult),
        hb_cache:ensure_all_loaded(hb_private:reset(CachedResult), Options)),
    ?assertEqual({error, hashpath_unavailable},
        hb_path:hashpath(CachedResult, Options)).

%% @doc An overlaid photo stays lazy and cannot inherit a stale receipt.
overlay_clears_stale_receipt_test() ->
    Options = options(enabled),
    PhotoID = hb_message:id(<<"unstored photo">>, all, Options),
    PhotoLink = {link, <<"data/", PhotoID/binary>>, #{}},
    Base = #{
        <<"device">> => <<"test-device@1.0">>,
        <<"counter">> => 1,
        <<"photo">> => PhotoLink,
        <<"priv">> => #{ <<"hashpath">> => <<"stale receipt">>,
            <<"hashpath-result">> => <<"stale association">>, <<"note">> => 123 }
    },
    {ok, Result} = hb_ao:resolve(Base, <<"vary-overlay">>, Options),
    ?assertEqual(2, maps:get(<<"counter">>, Result)),
    ?assertEqual(PhotoLink, maps:get(<<"photo">>, Result)),
    Private = hb_private:from_message(Result),
    ?assertEqual(123, maps:get(<<"note">>, Private)),
    ?assertNot(maps:is_key(<<"hashpath">>, Private)),
    ?assertNot(maps:is_key(<<"hashpath-result">>, Private)),
    ?assertEqual({error, hashpath_unavailable}, hb_path:hashpath(Result, Options)),
    ?assertEqual(#{ <<"note">> => 123 },
        hb_private:from_message(hb_hashpath:reset(Result))).

%% @doc Even a store-scoped link is stopped before its value is loaded.
non_loading_link_guard_test() ->
    Options = options(disabled),
    {ok, PhotoPath} = hb_cache:write(<<"photo bytes">>, Options),
    ProtectedLink = {link, PhotoPath, #{ <<"load">> => false,
        <<"store">> => maps:get(<<"store">>, Options) }},
    ?assertThrow({link_loading_disabled, PhotoPath},
        hb_cache:ensure_loaded(ProtectedLink, Options)).

%% @doc A discarded indirect link need not even load its target's address.
missing_omitted_link_pointer_test() ->
    Options = options(enabled),
    {Base, Request} = inputs(),
    PhotoLink = {link, <<"missing-photo-pointer">>,
        #{ <<"type">> => <<"link">>, <<"lazy">> => true }},
    {ok, WithoutReceipt} = hb_ao:resolve(Base#{ <<"photo">> => PhotoLink },
        Request, Options#{ <<"hashpath">> => ignore }),
    {ok, WithReceiptRequested} = hb_ao:resolve(Base#{ <<"photo">> => PhotoLink },
        Request, Options),
    ?assertEqual(hb_private:reset(WithoutReceipt),
        hb_private:reset(WithReceiptRequested)),
    ?assertEqual({error, hashpath_unavailable},
        hb_path:hashpath(WithReceiptRequested, Options)).

%% @doc Continue a known receipt without the photo, but reject stale ancestry.
continue_without_photo_test() ->
    Options = options(enabled),
    Photo = <<"unstored photo">>,
    PhotoID = hb_message:id(Photo, all, Options),
    PhotoLink = {link, <<"data/", PhotoID/binary>>, #{}},
    % The commitment supplies the original ID without fetching the photo.
    AddressedBase = hb_message:commit(#{
        <<"device">> => <<"test-device@1.0">>,
        <<"counter">> => 1,
        <<"photo">> => Photo
    }, Options, #{ <<"type">> => <<"unsigned">> }),
    Base = AddressedBase#{ <<"photo">> => PhotoLink },
    {ok, FirstResult} = hb_ao:resolve(Base, <<"vary-overlay">>, Options),
    FirstHashpath = hb_path:hashpath(FirstResult, Options),
    ?assert(is_binary(FirstHashpath)),
    ?assertEqual(2, maps:get(<<"counter">>, FirstResult)),
    ?assertEqual(PhotoLink, maps:get(<<"photo">>, FirstResult)),
    % Changing only private notes does not change the returned state.
    WithNote = hb_private:set(FirstResult, #{ <<"note">> => 123 }, Options),
    {ok, SecondResult} = hb_ao:resolve(WithNote, <<"vary-overlay">>, Options),
    SecondHashpath = hb_path:hashpath(SecondResult, Options),
    ?assertEqual(3, maps:get(<<"counter">>, SecondResult)),
    ?assertEqual(FirstHashpath, maps:get(<<"base-id">>,
        hb_hashpath:context(SecondHashpath, Options))),
    % A public change must not continue the receipt for counter=2.
    Changed = FirstResult#{ <<"counter">> => 20 },
    {ok, ChangedResult} = hb_ao:resolve(Changed, <<"vary-overlay">>, Options),
    ?assertEqual(21, maps:get(<<"counter">>, ChangedResult)),
    ?assertEqual({error, hashpath_unavailable},
        hb_path:hashpath(ChangedResult, Options)),
    % Neither transition needed to put the photo in the store.
    ?assertEqual({error, not_found},
        hb_cache:read(<<"data/", PhotoID/binary>>, Options)),
    % Once the original witness is supplied, both transitions verify.
    {ok, _} = hb_cache:write(AddressedBase, Options),
    ?assert(hb_hashpath:verify_all(FirstHashpath, Options)),
    ?assert(hb_hashpath:verify_all(SecondHashpath, Options)),
    {ok, Loaded} = hb_hashpath:load(SecondHashpath, Options),
    ?assertEqual(3, hb_maps:get(<<"counter">>, Loaded, undefined, Options)).

%% @doc Exercise each omitted location with and without a known original ID.
omitted_origin_test_() ->
    [
        {atom_to_list(Location) ++ "/" ++ atom_to_list(Identity) ++ "/" ++
            atom_to_list(Storage), fun() ->
                omitted_origin(Location, Identity, Storage, #{})
            end}
    || Location <- [base, request, nested],
       Identity <- [unknown, known],
       Storage <- [disabled, enabled]
    ].

%% @doc Ignoring receipts leaves the schema-discarded values untouched.
ignored_origin_test_() ->
    [{atom_to_list(Location), fun() ->
        omitted_origin(Location, unknown, disabled,
            #{ <<"hashpath">> => ignore })
    end} || Location <- [base, request, nested]].

%% @doc A value placeholder and an explicit link do not have the same identity.
link_identity_test() ->
    Options = options(disabled),
    Payload = <<"omitted payload">>,
    {ok, PayloadPath} = hb_cache:write(Payload, Options),
    PayloadID = hb_message:id(Payload, all, Options),
    Literal = #{ <<"unused">> => Payload },
    Lazy = #{ <<"unused">> => {link, PayloadPath, #{}} },
    Explicit = #{ <<"unused">> => {link, PayloadID,
        #{ <<"type">> => <<"link">>, <<"lazy">> => false }} },
    LiteralID = hb_message:id(Literal, all, Options),
    ?assertEqual(LiteralID, hb_message:id(Lazy, all, Options)),
    ?assertNotEqual(LiteralID, hb_message:id(Explicit, all, Options)).

%% @doc The same omitted payload can already be represented as an explicit link.
explicit_origin_test_() ->
    [{atom_to_list(Storage), fun() ->
        omitted_origin(base, explicit, Storage, #{})
    end} || Storage <- [disabled, enabled]].

%% @doc A retained required value must still be loaded by Vary.
missing_selected_test() ->
    Options = options(disabled),
    {Base, Request} = inputs(),
    Missing = {link, hb_util:human_id(crypto:strong_rand_bytes(32)), #{}},
    ?assertMatch({error, _}, hb_ao:resolve(
        Base#{ <<"required">> => Missing }, Request, Options)).

%% @doc Keep the original identity while removing access to an unused payload.
omitted_origin(Location, Identity, Storage, ExtraOptions) ->
    Options = maps:merge(options(Storage), ExtraOptions),
    {Base, Request} = inputs(),
    Payload = <<"omitted payload">>,
    PayloadID = hb_message:id(Payload, all, Options),
    PayloadPath = <<"data/", PayloadID/binary>>,
    ?assertEqual({error, not_found}, hb_cache:read(PayloadPath, Options)),
    Original = case Location of request -> Request; _ -> Base end,
    WithPayload = set_unused(Location, Payload, Original),
    Addressed = case Identity of
        known -> hb_message:commit(WithPayload, Options,
            #{ <<"type">> => <<"unsigned">> });
        _ -> WithPayload
    end,
    Missing = case Identity of
        explicit -> {link, PayloadID,
            #{ <<"type">> => <<"link">>, <<"lazy">> => false }};
        _ -> {link, PayloadPath, #{}}
    end,
    WithMissing = set_unused(Location, Missing, Addressed),
    case Identity of
        known -> ?assertEqual(hb_message:id(Addressed, all, Options),
            hb_message:id(WithMissing, all, Options));
        _ -> ok
    end,
    {InputBase, InputRequest} = case Location of
        request -> {Base, WithMissing};
        _ -> {WithMissing, Request}
    end,
    {ok, Result} = hb_ao:resolve(InputBase, InputRequest, Options),
    SelectedBase = maps:get(<<"base">>, Result),
    SelectedRequest = maps:get(<<"request">>, Result),
    ?assertEqual(7, maps:get(<<"required">>, SelectedBase)),
    ?assertEqual(#{ <<"slot">> => 8 }, maps:get(<<"deep">>, SelectedBase)),
    ?assertEqual(#{ <<"slot">> => 9 },
        maps:get(<<"deep-request">>, SelectedRequest)),
    ?assertNot(maps:is_key(<<"unused">>, SelectedBase)),
    ?assertNot(maps:is_key(<<"unused">>, SelectedRequest)),
    Private = hb_private:from_message(Result),
    case Identity of
        known ->
            case Storage of
                enabled ->
                    ?assertEqual(<<"witnesses-not-stored">>,
                        maps:get(<<"hashpath-status">>, Private)),
                    ?assertEqual(
                        case Location of request -> [<<"request">>]; _ -> [<<"base">>] end,
                        maps:get(<<"hashpath-unstored">>, Private));
                disabled -> ?assertNot(maps:is_key(<<"hashpath-status">>, Private))
            end,
            % The receipt remains verifiable when its witnesses are available.
            Hashpath = hb_path:hashpath(Result, Options),
            {ok, _} = hb_cache:write(Payload, Options),
            {ok, _} = hb_cache:write(InputBase, Options),
            {ok, _} = hb_cache:write(InputRequest, Options),
            ?assert(hb_hashpath:verify_all(Hashpath, Options)),
            {ok, Loaded} = hb_hashpath:load(Hashpath, Options),
            ?assertEqual(hb_private:reset(Result), hb_private:reset(
                hb_cache:ensure_all_loaded(Loaded, Options)));
        explicit when Storage == enabled ->
            ?assert(hb_hashpath:verify_all(
                hb_path:hashpath(Result, Options), Options));
        unknown when not is_map_key(<<"hashpath">>, ExtraOptions) ->
            ?assertEqual(<<"unavailable">>, maps:get(<<"hashpath-status">>, Private)),
            ?assertNot(maps:is_key(<<"hashpath">>, Private));
        _ -> ok
    end.

%% @doc Insert a leaf at a location omitted by the projection schema.
set_unused(nested, Value, Message) ->
    Child = maps:get(<<"deep">>, Message),
    Message#{ <<"deep">> => Child#{ <<"unused">> => Value }};
set_unused(_, Value, Message) -> Message#{ <<"unused">> => Value }.

%% @doc Use real device resolution and fresh stores for every case.
options(Storage) -> #{
    <<"store">> => hb_test_utils:test_store(),
    <<"attested-store">> => hb_test_utils:test_store(),
    <<"cache-control">> => case Storage of
        disabled -> [<<"no-cache">>, <<"no-store">>];
        enabled -> [<<"no-cache">>, <<"always">>]
    end,
    <<"spawn-worker">> => false,
    <<"await-inprogress">> => false
}.

%% @doc Only the schema-selected members are needed by this device.
inputs() -> {
    #{ <<"device">> => <<"test-device@1.0">>, <<"required">> => 7,
       <<"deep">> => #{ <<"slot">> => 8 } },
    #{ <<"path">> => <<"vary-projection">>,
       <<"deep-request">> => #{ <<"slot">> => 9 } }
}.
