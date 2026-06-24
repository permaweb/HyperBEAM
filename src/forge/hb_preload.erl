%%% @doc Build a `preloaded-store' from a set of packaged HyperBEAM devices.
%%%
%%% The preloaded-store is a normal {@link hb_store} store containing:
%%% <ul>
%%%   <li>One signed `Device-Specification' message per device.</li>
%%%   <li>A signed `name@1.0'-compatible resolver message that maps each
%%%       human-readable device name to the ID of its specification
%%%       message.</li>
%%%   <li>One signed `application/beam-archive' implementation message per
%%%       device, each with an `implements-device' key that refers to the
%%%       ID of the device's specification message.</li>
%%% </ul>
%%%
%%% The ID of the resulting preloaded device index can then be used as a
%%% `resolver' source for the `~name@1.0' device, if the preloaded-store is in
%%% scope.
%%%
%%% Public API:
%%% <ul>
%%%   <li>{@link build/3}        build the preloaded-store from packages</li>
%%%   <li>{@link build_dir/4}    build to a specific directory and return paths</li>
%%% </ul>
-module(hb_preload).
-export([build/3, build_groups/4, build_dir/4]).
-include("include/hb.hrl").

%% @doc Build a preloaded-store at `OutputDir' from a list of packaged
%% device groups (the output of {@link hb_packager:package_all/2}). Each
%% device's specification and implementation messages are signed with
%% `Wallet' and the name -> spec-ID resolver is published as a signed
%% index message.
%%
%% Returns `{ok, #{ store := StoreCfg, index := IndexID,
%%                  specs := #{ Name => SpecID }, impls := [ImplID] }}'.
build(Pkgs, Wallet, Opts) ->
    Dir = hb_maps:get(<<"output-dir">>, Opts, default_dir(), Opts),
    build_dir(Pkgs, Wallet, Dir, Opts).

%% @doc Package source groups and build a preloaded-store in `OutputDir'.
build_groups(Groups, Wallet, OutputDir, Opts) ->
    hb_forge_seed:with_forge_bootstrap(
        Opts,
        fun(SeedOpts) ->
            build_dir(
                hb_packager:package_all(Groups, SeedOpts),
                Wallet,
                OutputDir,
                SeedOpts
            )
        end
    ).

%% @doc Write a complete signed preloaded-store from already-built packages.
build_dir(Pkgs, Wallet, OutputDir, Opts) ->
    % Create a store config for the preloaded-store at the output directory.
    OutputBin = hb_util:bin(OutputDir),
    ?event(
        preload,
        {build_start, {output, OutputBin}, {devices, length(Pkgs)}}
    ),
    StoreCfg =
        #{
            <<"store-module">> => hb_store_lmdb,
            <<"name">> => OutputBin,
            <<"capacity">> => 1024 * 1024 * 1024,
            <<"lock">> => false
        },
    % Reset store before building for deterministic re-builds.
    hb_store:reset(StoreCfg, #{ <<"reset">> => <<"all">> }, Opts),
    hb_store:start(StoreCfg, #{}, Opts),
    LocalOpts =
        Opts#{
            <<"store">> => [StoreCfg],
            <<"priv-wallet">> => Wallet
        },
    % Sign and write each spec + each impl message; collect signed IDs.
    {SpecIDs, ImplIDs} =
        lists:foldl(
            fun(Pkg, {SpecAcc, ImplAcc}) ->
                {SignedSpecID, SignedImplID} = persist_pkg(Pkg, LocalOpts),
                Name = maps:get(device_name, Pkg),
                {
                    SpecAcc#{ Name => SignedSpecID },
                    [SignedImplID | ImplAcc]
                }
            end,
            {#{}, []},
            Pkgs
        ),
    IndexMsg = build_index_message(SpecIDs),
    IndexID = persist_signed(IndexMsg, LocalOpts),
    ok = hb_store:link(
        StoreCfg,
        #{ ?PRELOADED_INDEX_KEY => IndexID },
        Opts
    ),
    ?event(preload, {build_complete, {output, OutputBin}, {index, IndexID}}),
    ok = hb_store:stop(StoreCfg, #{}, Opts),
    {
        ok,
        #{
            store => StoreCfg,
            index => IndexID,
            specs => SpecIDs,
            impls => lists:reverse(ImplIDs),
            pkgs => Pkgs
        }
    }.

%% @doc Write a package's specification and implementation to the store.
%% Returns `{SignedSpecID, SignedImplID}'.
persist_pkg(Pkg, Opts) ->
    SignedSpecID =
        persist_signed(hb_packager:spec_message(Pkg, Opts), Opts),
    SignedImplID =
        persist_signed(
            hb_packager:impl_message(Pkg, SignedSpecID, Opts),
            Opts
        ),
    {SignedSpecID, SignedImplID}.

%% @doc Sign an unsigned message and write it to the store. Returns the signed ID.
persist_signed(Unsigned, Opts) ->
    Signed = hb_message:commit(Unsigned, Opts),
    {ok, _StoredID} = hb_cache:write(Signed, Opts),
    signed_id(Signed, Opts).

%% @doc Extract the signed ID from a signed message.
signed_id(Msg, Opts) ->
    SignedIDs =
        lists:sort(
            [
                ID
            ||
                {ID, #{ <<"committer">> := _ }} <-
                    maps:to_list(hb_maps:get(<<"commitments">>, Msg, #{}, Opts))
            ]
        ),
    case SignedIDs of
        [ID | _] -> ID;
        [] -> error({preload_message_not_signed, Msg})
    end.

%% @doc Build the unsigned resolver message. The message is kept
%% flat so every field lives at a single store key without inducing
%% sub-message links.
%%
%% Each device name is written directly as `<Name>' -> signed spec ID
%% for `name@1.0' lookups.
build_index_message(SpecIDs) ->
    maps:fold(
        fun(Name, SpecID, Acc) -> Acc#{ Name => SpecID } end,
        #{},
        SpecIDs
    ).

%% @doc Return the default preloaded-store build path.
default_dir() ->
    hb_util:bin(filename:join([<<"_build">>, <<"preloaded-store">>])).

%%% --------------------------------------------------------------------
%%% Tests
%%% --------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

build_signs_and_indexes_test() ->
    Dir =
        filename:join([
            <<"/tmp">>,
            <<"hb_preload_test_",
                (integer_to_binary(erlang:system_time()))/binary>>
        ]),
    % Build the fixture with the minimal seed devices needed to sign.
    SrcDir = hb_packager_test_vectors:test_fixture_dir(),
    [Group] = hb_packager:scan([SrcDir], #{}),
    Groups =
        [Group] ++
        hb_packager:scan(
            [<<"src/preloaded">>],
            #{
                <<"device-roots">> =>
                    [dev_message, dev_httpsig, dev_structured]
            }
        ),
    Wallet = ar_wallet:new(),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    {ok, Result} =
        build_groups(
            Groups,
            Wallet,
            Dir,
            #{ <<"bootstrap-device-src">> => [<<"src/preloaded">>] }
        ),
    Pkg =
        lists:foldl(
            fun(P, Acc) ->
                case maps:get(device_name, P) of
                    <<"test-pkg@1.0">> -> P;
                    _ -> Acc
                end
            end,
            undefined,
            maps:get(pkgs, Result)
        ),
    Store = maps:get(store, Result),
    IndexID = maps:get(index, Result),
    SpecIDs = maps:get(specs, Result),
    ImplIDs = maps:get(impls, Result),
    % The fixture plus seed devices are recorded.
    ?assert(maps:is_key(maps:get(device_name, Pkg), SpecIDs)),
    ?assert(length(ImplIDs) >= 1),
    % Index ID must be a 43-char human ID.
    ?assert(byte_size(IndexID) == 43),
    % Store must be an LMDB store at our dir.
    ?assertMatch(#{ <<"store-module">> := hb_store_lmdb }, Store),
    % The stable index link and raw index ID must both resolve names.
    Name = maps:get(device_name, Pkg),
    NodeOpts = #{ <<"store">> => [Store] },
    SpecID = maps:get(Name, SpecIDs),
    ?assertEqual(
        {ok, SpecID},
        hb_store:read(
            Store,
            <<?PRELOADED_INDEX_KEY/binary, "/", Name/binary>>,
            NodeOpts
        )
    ),
    {ok, Got} =
        hb_store:read(Store, <<IndexID/binary, "/", Name/binary>>, NodeOpts),
    ?assertEqual(SpecID, Got),
    ?assertEqual(
        {error, not_found},
        hb_store:read(Store, <<IndexID/binary, "/type">>, NodeOpts)
    ),
    ?assertEqual({ok, Address}, signer(Store, IndexID, NodeOpts)),
    ?assertEqual({ok, Address}, signer(Store, SpecID, NodeOpts)),
    lists:foreach(
        fun(ImplID) ->
            ?assertEqual({ok, Address}, signer(Store, ImplID, NodeOpts))
        end,
        ImplIDs
    ).

signer(Store, ID, Opts) ->
    hb_store:read(
        Store,
        <<ID/binary, "/commitments/", ID/binary, "/committer">>,
        Opts
    ).

-endif.
