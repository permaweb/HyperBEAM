%%% @doc Build a `preloaded-store' LMDB store from a set of
%%% packaged HyperBEAM devices.
%%%
%%% The preloaded-store is a normal {@link hb_store_lmdb} store containing:
%%% <ul>
%%%   <li>One signed `Device-Specification' message per device.</li>
%%%   <li>One signed `application/beam-archive' implementation message per
%%%       device.</li>
%%%   <li>A signed `name@1.0'-compatible resolver message that maps each
%%%       human-readable device name to the ID of its specification
%%%       message.</li>
%%% </ul>
%%%
%%% The index ID lets the runtime add the resolver message to
%%% `name-resolvers' while resolving packaged devices.
%%%
%%% Public API:
%%% <ul>
%%%   <li>{@link build/3}        build the preloaded-store from packages</li>
%%%   <li>{@link build_dir/4}    build to a specific directory and return paths</li>
%%%   <li>{@link write_index_header/2} write a `-define' header so the
%%%       generated index ID can be embedded in the default node config</li>
%%% </ul>
-module(hb_preload).

-export([build/3, build_dir/4]).
-export([write_index_header/2]).

-include("include/hb.hrl").

-define(INDEX_HEADER_MACRO, "PRELOADED_DEVICES_INDEX_MESSAGE_ID").

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

build_dir(Pkgs, Wallet, OutputDir, Opts) ->
    % Create a store config for the preloaded-store at the output directory.
    OutputBin = hb_util:bin(OutputDir),
    StoreCfg =
        #{
            <<"store-module">> => hb_store_lmdb,
            <<"name">> => OutputBin
        },
    %% Reset store before building for deterministic re-builds.
    hb_store:reset(StoreCfg, #{ <<"reset">> => <<"all">> }, Opts),
    hb_store:start(StoreCfg, #{}, Opts),
    %% The build-time signing flow needs a usable codec device before
    %% any preloaded-store exists. We point the resolver at the source
    %% modules via `<<"device-bootstrap">>' for the duration of this
    %% build.  Runtime nodes never set that opt.
    LocalOpts =
        Opts#{
            <<"store">> => [StoreCfg],
            <<"priv-wallet">> => Wallet,
            <<"device-bootstrap">> => hb_packager:bootstrap_device_map()
        },
    %% Sign and write each spec + each impl message; collect signed IDs.
    {SpecIDs, ImplIDs} =
        lists:foldl(
            fun(Pkg, {SpecAcc, ImplAcc}) ->
                % Write each package to the store.
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
    % Build the resolver message. The runtime prepends it to
    % `name-resolvers' when loading packaged devices.
    IndexMsg = build_index_message(SpecIDs),
    % Write the index message to the store.
    IndexID = persist_signed(IndexMsg, LocalOpts),
    ok = hb_store:stop(StoreCfg, #{}, Opts),
    {
        ok,
        #{
            store => StoreCfg,
            index => IndexID,
            specs => SpecIDs,
            impls => lists:reverse(ImplIDs)
        }
    }.

%% @doc Write a package to the store by writing its spec and implementation messages.
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

default_dir() ->
    hb_util:bin(filename:join(["_build", "preloaded-store"])).

%% @doc Write the build-time index ID to a generated `.hrl' file so the
%% default node configuration can embed it at compile time.
write_index_header(IndexID, HeaderPath) ->
    PathBin = hb_util:bin(HeaderPath),
    PathStr = binary_to_list(PathBin),
    ok = filelib:ensure_dir(PathStr),
    Body =
        iolist_to_binary(
            [
                <<"%% Generated by hb_preload - do not edit.\n">>,
                <<"-define(">>, ?INDEX_HEADER_MACRO,
                <<", <<\"">>, IndexID, <<"\">>).\n">>
            ]
        ),
    ok = file:write_file(PathStr, Body).

%%% --------------------------------------------------------------------
%%% Tests
%%% --------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

build_signs_and_indexes_test() ->
    Dir =
        filename:join(["/tmp",
            "hb_preload_test_" ++ integer_to_list(erlang:system_time())]),
    %% Use the packager's fixture helper to build a single-device set.
    SrcDir = hb_packager:test_fixture_dir(),
    [Group] = hb_packager:scan([SrcDir], #{}),
    Pkg = hb_packager:package(Group, #{}),
    Wallet = ar_wallet:new(),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    {ok, Result} = build_dir([Pkg], Wallet, Dir, #{}),
    Store = maps:get(store, Result),
    IndexID = maps:get(index, Result),
    SpecIDs = maps:get(specs, Result),
    ImplIDs = maps:get(impls, Result),
    %% One spec, one impl recorded.
    ?assertEqual(1, map_size(SpecIDs)),
    ?assertMatch([_], ImplIDs),
    %% Index ID must be a 43-char human ID.
    ?assert(byte_size(IndexID) == 43),
    %% Store must be an LMDB store at our dir.
    ?assertMatch(#{ <<"store-module">> := hb_store_lmdb }, Store),
    %% Reading <IndexID>/<Name> from the store must return the spec ID.
    Name = maps:get(device_name, Pkg),
    NodeOpts = #{ <<"store">> => [Store] },
    {ok, Got} =
        hb_store:read(Store, <<IndexID/binary, "/", Name/binary>>, NodeOpts),
    SpecID = maps:get(Name, SpecIDs),
    [ImplID] = ImplIDs,
    ?assertEqual(SpecID, Got),
    ?assertEqual(
        {error, not_found},
        hb_store:read(Store, <<IndexID/binary, "/type">>, NodeOpts)
    ),
    ?assertEqual({ok, Address}, signer(Store, IndexID, NodeOpts)),
    ?assertEqual({ok, Address}, signer(Store, SpecID, NodeOpts)),
    ?assertEqual({ok, Address}, signer(Store, ImplID, NodeOpts)).

signer(Store, ID, Opts) ->
    hb_store:read(
        Store,
        <<ID/binary, "/commitments/", ID/binary, "/committer">>,
        Opts
    ).

write_index_header_emits_macro_test() ->
    Dir =
        filename:join(["/tmp",
            "hb_preload_hdr_" ++ integer_to_list(erlang:system_time())]),
    HdrPath = filename:join(Dir, "hb_preloaded_index.hrl"),
    ok = filelib:ensure_dir(HdrPath),
    write_index_header(<<"abcdef">>, HdrPath),
    {ok, HdrBin} = file:read_file(HdrPath),
    ?assertMatch({_, _}, binary:match(HdrBin, <<"abcdef">>)).

-endif.
