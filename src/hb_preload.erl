%%% @doc Build a `preloaded-store' LMDB store from a set of
%%% packaged HyperBEAM devices.
%%%
%%% The preloaded-store is a normal {@link hb_store_lmdb} store containing:
%%% <ul>
%%%   <li>One signed `Device-Specification' message per device.</li>
%%%   <li>One signed `application/beam' implementation message per device.</li>
%%%   <li>A signed `Device-Index' provider message that maps each
%%%       human-readable device name to the ID of its specification
%%%       message — `name@1.0'-compatible.</li>
%%% </ul>
%%%
%%% The index ID lets the runtime bootstrap by reading
%%% ``<<IndexID/binary, "/name@1.0">>'' from the preloaded-store before
%%% any device has been loaded.
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

-define(INDEX_TYPE, <<"Device-Index">>).
-define(VARIANT, <<"ao.N.1">>).
-define(INDEX_HEADER_MACRO, "PRELOADED_DEVICES_INDEX_MESSAGE_ID").

%% @doc Build a preloaded-store at `OutputDir' from a list of packaged
%% device groups (the output of {@link hb_packager:package_all/2}). Each
%% device's specification and implementation messages are signed with
%% `Wallet' and the name -> spec-ID map is published as a signed index
%% message.
%%
%% Returns `{ok, #{ store := StoreCfg, index := IndexID,
%%                  specs := #{ Name => SpecID }, impls := [ImplID] }}'.
build(Pkgs, Wallet, Opts) ->
    Dir = hb_maps:get(<<"output-dir">>, Opts, default_dir(), Opts),
    build_dir(Pkgs, Wallet, Dir, Opts).

build_dir(Pkgs, Wallet, OutputDir, Opts) ->
    OutputBin = hb_util:bin(OutputDir),
    StoreCfg = #{
        <<"store-module">> => hb_store_lmdb,
        <<"name">> => OutputBin
    },
    %% Reset store before building: deterministic re-builds.
    hb_store:reset(StoreCfg, #{ <<"reset">> => <<"all">> }, Opts),
    hb_store:start(StoreCfg, #{}, Opts),
    %% The build-time signing flow needs a usable codec device before
    %% any preloaded-store exists. We point the resolver at the source
    %% modules via `<<"device-bootstrap">>' for the duration of this
    %% build.  Runtime nodes never set that opt.
    Bootstrap = build_bootstrap_map(),
    LocalOpts = (Opts)#{
        <<"store">> => [StoreCfg],
        <<"priv-wallet">> => Wallet,
        <<"device-bootstrap">> => Bootstrap
    },
    %% Sign and write each spec + each impl message; collect signed IDs.
    {SpecIDs, ImplIDs} =
        lists:foldl(
            fun(Pkg, {SpecAcc, ImplAcc}) ->
                {SpecID, ImplID} = persist_pkg(Pkg, LocalOpts),
                Name = maps:get(device_name, Pkg),
                {SpecAcc#{ Name => SpecID },
                 [ImplID | ImplAcc]}
            end,
            {#{}, []},
            Pkgs
        ),
    % Build the index/provider message. The runtime needs the standard
    % `name@version -> spec-ID' lookup for name@1.0 compatibility.
    IndexID = persist_signed(build_index_message(SpecIDs), LocalOpts),
    ok = hb_store:stop(StoreCfg, #{}, Opts),
    {ok, #{
        store => StoreCfg,
        index => IndexID,
        specs => SpecIDs,
        impls => lists:reverse(ImplIDs)
    }}.

persist_pkg(Pkg, Opts) ->
    SpecID = persist_signed(hb_packager:spec_message(Pkg, Opts), Opts),
    ImplID = persist_signed(hb_packager:impl_message(Pkg, SpecID, Opts), Opts),
    {SpecID, ImplID}.

persist_signed(Unsigned, Opts) ->
    Signed = hb_message:commit(Unsigned, Opts),
    {ok, _StoredID} = hb_cache:write(Signed, Opts),
    signed_id(Signed, Opts).

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

%% @doc Build the unsigned index/provider message. The map is kept
%% flat so every field lives at a single store key without inducing
%% sub-message links.
%%
%% Each device name is written directly as `<Name>' -> signed spec ID
%% for `name@1.0' lookups.
build_index_message(SpecIDs) ->
    Base = #{
        <<"data-protocol">> => <<"ao">>,
        <<"variant">> => ?VARIANT,
        <<"type">> => ?INDEX_TYPE
    },
    maps:fold(
        fun(Name, SpecID, Acc) -> Acc#{ Name => SpecID } end,
        Base,
        SpecIDs
    ).

default_dir() ->
    hb_util:bin(filename:join(["_build", "preloaded-store"])).

%% @doc The minimal name -> source-module map needed to sign messages
%% during the build. Add new codecs here when they become required for
%% bootstrap (e.g. when the default commitment device changes).
build_bootstrap_map() ->
    hb_packager:bootstrap_device_map().

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
    ?assertEqual(maps:get(Name, SpecIDs), Got).

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
