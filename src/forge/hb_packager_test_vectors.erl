%%% @doc End-to-end tests for the device packaging pipeline:
%%% module source -> packager -> preloaded store -> runtime device resolution.
%%%
%%% Each test builds a self-contained `preloaded-store' from a tiny
%%% in-memory device, points the runtime at that store, and asserts on
%%% the behaviour of {@link hb_device:load/2}.
-module(hb_packager_test_vectors).
-export([test_fixture_dir/0]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Build a runtime opts map that uses a freshly-built preloaded-store.
setup() ->
    SrcDir = test_fixture_dir(),
    ok =
        filelib:ensure_dir(
            filename:join([SrcDir, <<"priv">>, <<"share">>, <<"data">>])
        ),
    ok =
        file:write_file(
            filename:join([SrcDir, <<"priv">>, <<"share">>, <<"data">>]),
            <<"runtime-priv">>
        ),
    Groups =
        hb_packager:scan([SrcDir], #{})
            ++ hb_packager:scan(
                [<<"src/preloaded">>],
                #{
                    <<"device-roots">> =>
                        [dev_name, dev_message, dev_httpsig, dev_structured]
                }
            ),
    Wallet = ar_wallet:new(),
    PreloadDir =
        hb_util:bin(
            filename:join([
                <<"/tmp">>,
                <<"hb_pkg_rt_",
                    (integer_to_binary(erlang:system_time()))/binary>>
            ])
        ),
    {ok, Result} =
        hb_preload:build_groups(
            Groups,
            Wallet,
            PreloadDir,
            #{ <<"bootstrap-device-src">> => [<<"src/preloaded">>] }
        ),
    Pkgs = maps:get(pkgs, Result),
    [Pkg] =
        [
            Package
        ||
            Package <- Pkgs,
            maps:get(device_name, Package) =:= <<"test-pkg@1.0">>
        ],
    % Use the encode/0 form ar_wallet uses internally so this matches
    % whatever `hb_message:signers/2' returns for impl messages.
    Address = hb_util:encode(ar_wallet:to_address(Wallet)),
    Store = maps:get(store, Result),
    Index = maps:get(index, Result),
    SpecIDs = maps:get(specs, Result),
    SpecID = maps:get(<<"test-pkg@1.0">>, SpecIDs),
    %% Scan the just-built store directly (the loader reads the
    %% preloaded store the same way); a non-raw match would route
    %% through `~match@1.0', which this minimal store does not contain.
    {ok, [ImplID | _]} =
        hb_cache:match(
            #{
                <<"data-protocol">> => <<"ao">>,
                <<"variant">> => <<"ao.N.1">>,
                <<"implements-device">> => SpecID
            },
            #{ <<"store">> => Store, <<"cache-read-mode">> => raw }
        ),
    Opts = #{
        <<"store">> => [Store],
        <<"preloaded-store">> => Store,
        <<"preloaded-devices-index">> => Index,
        <<"trusted-device-signers">> => [Address],
        <<"priv-wallet">> => Wallet
    },
    {Pkg, Opts, SpecIDs, ImplID}.

teardown(_) -> ok.

%% Build the EUnit fixture so each case gets a fresh preloaded-store;
%% this prevents test-to-test bleed from earlier setups that signed
%% with different wallets.
all_runtime_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            runtime_case(Name, Fun)
        ||
            {Name, Fun} <- runtime_tests()
        ]
    }.

%% @doc Return the runtime assertions shared by the foreach fixture.
runtime_tests() ->
    [
        {"module name matches", fun module_name_matches/4},
        {"priv data matches", fun priv_data_matches/4},
        {"trusted device id matches", fun trusted_device_id_matches/4},
        {"preloaded index matches", fun preloaded_index_matches/4}
    ].

%% @doc Wrap one assertion in the fixture tuple expected by EUnit.
runtime_case(Name, Fun) ->
    fun({Pkg, Opts, SpecIDs, ImplID}) ->
        {Name, fun() -> Fun(Pkg, Opts, SpecIDs, ImplID) end}
    end.

module_name_matches(Pkg, Opts, _, _) ->
    Name = maps:get(device_name, Pkg),
    {ok, Mod} = hb_device_load:reference(Name, Opts),
    ?assert(hb_device_name:is_generated(Mod)),
    ?assertEqual(maps:get(module_name, Pkg), Mod).

priv_data_matches(Pkg, Opts, _, _) ->
    Name = maps:get(device_name, Pkg),
    {ok, Mod} = hb_device_load:reference(Name, Opts),
    Dir = hb_device_archive:implementation_dir(Mod),
    {ok, Body} =
        file:read_file(
            filename:join([Dir, <<"share">>, <<"data">>])
        ),
    ?assertEqual(<<"runtime-priv">>, Body).

%% An implementation ID pinned in `trusted-devices' loads from the
%% high-trust map even when no signer is trusted.
trusted_device_id_matches(Pkg, Opts, SpecIDs, ImplID) ->
    SpecID = maps:get(<<"test-pkg@1.0">>, SpecIDs),
    Other = hb_util:human_id(crypto:strong_rand_bytes(32)),
    IDOpts = Opts#{
        <<"trusted-device-signers">> => [Other],
        <<"trusted-devices">> => #{ SpecID => ImplID }
    },
    {ok, Mod} = hb_device_load:reference(SpecID, IDOpts),
    ?assertEqual(maps:get(module_name, Pkg), Mod).

preloaded_index_matches(_Pkg, Opts, _, _) ->
    Index = maps:get(<<"preloaded-devices-index">>, Opts),
    Store = maps:get(<<"preloaded-store">>, Opts),
    {ok, Got} =
        hb_store:read(Store, <<Index/binary, "/test-pkg@1.0">>, Opts),
    ?assert(byte_size(Got) == 43).

%%% --------------------------------------------------------------------
%%% Packager unit tests
%%% --------------------------------------------------------------------
%% Build a temporary source directory with a minimal device root and
%% one helper module, then exercise the full scan/package pipeline.
test_fixture_dir() ->
    Tmp =
        filename:join([
            <<"/tmp">>,
            <<"hb_packager_test_",
                (integer_to_binary(erlang:system_time()))/binary>>
        ]),
    ok = filelib:ensure_dir(filename:join(Tmp, <<".keep">>)),
    Stamp = integer_to_binary(erlang:unique_integer([positive])),
    Root = <<
        "%% fixture ", Stamp/binary, "\n"
        "%%% @doc Test device - packager fixture.\n"
        "%%% Lines of moduledoc become the spec body.\n"
        "-module(dev_test_pkg).\n"
        "-export([echo/3, hello/3, hello_via_capture/3]).\n"
        "\n"
        "echo(_Base, Req, _Opts) -> {ok, Req}.\n"
        "hello(Base, _Req, Opts) ->\n"
        "    Greeting = dev_test_pkg_helper:greet(Base, Opts),\n"
        "    {ok, Greeting}.\n"
        "hello_via_capture(Base, _Req, Opts) ->\n"
        "    Greeting = (fun dev_test_pkg_helper:greet/2)(Base, Opts),\n"
        "    {ok, Greeting}.\n"
    >>,
    Helper = <<
        "-module(dev_test_pkg_helper).\n"
        "-export([greet/2]).\n"
        "\n"
        "greet(_Base, _Opts) -> <<\"hello\">>.\n"
    >>,
    ok = file:write_file(
        filename:join(Tmp, <<"dev_test_pkg.erl">>), Root),
    ok = file:write_file(
        filename:join(Tmp, <<"dev_test_pkg_helper.erl">>), Helper),
    Tmp.

dynamic_dispatch_fixture_dir() ->
    Tmp =
        filename:join([
            <<"/tmp">>,
            <<"hb_packager_dynamic_test_",
                (integer_to_binary(erlang:system_time()))/binary>>
        ]),
    ok = filelib:ensure_dir(filename:join(Tmp, <<".keep">>)),
    Root = <<
        "-module(dev_dyn_pkg).\n"
        "-export([call/3]).\n"
        "\n"
        "call(Base, _Req, Opts) ->\n"
        "    {ok, dev_dyn_pkg_helper:dispatch(greet, Base, Opts)}.\n"
    >>,
    Helper = <<
        "-module(dev_dyn_pkg_helper).\n"
        "-export([dispatch/3, greet/2]).\n"
        "\n"
        "dispatch(F, Base, Opts) -> dev_dyn_pkg_helper:F(Base, Opts).\n"
        "greet(_Base, _Opts) -> <<\"hello\">>.\n"
    >>,
    ok = file:write_file(
        filename:join(Tmp, <<"dev_dyn_pkg.erl">>), Root),
    ok = file:write_file(
        filename:join(Tmp, <<"dev_dyn_pkg_helper.erl">>), Helper),
    Tmp.

priv_fixture_dir() ->
    Tmp = test_fixture_dir(),
    PrivDir = filename:join([Tmp, <<"priv">>, <<"dev_test_pkg">>]),
    ok = filelib:ensure_dir(filename:join([PrivDir, <<"bin">>, <<"tool">>])),
    ok = file:write_file(filename:join([PrivDir, <<"bin">>, <<"tool">>]),
        <<"#!/bin/sh\n">>),
    ok = filelib:ensure_dir(filename:join([PrivDir, <<"share">>, <<"data">>])),
    ok = file:write_file(filename:join([PrivDir, <<"share">>, <<"data">>]),
        <<"fixture-data">>),
    Tmp.

scan_groups_root_with_helper_test() ->
    Dir = test_fixture_dir(),
    Groups = hb_packager:scan([Dir], #{}),
    ?assertMatch([_], Groups),
    [#{ root := Root, helpers := Helpers, files := Files }] = Groups,
    ?assertEqual(dev_test_pkg, Root),
    ?assertMatch([{dev_test_pkg_helper, _}], Helpers),
    ?assertEqual(2, map_size(Files)).

scan_groups_transitive_helpers_under_root_test() ->
    Tmp =
        filename:join([
            <<"/tmp">>,
            <<"hb_packager_transitive_test_",
                (integer_to_binary(erlang:system_time()))/binary>>
        ]),
    ok = filelib:ensure_dir(filename:join(Tmp, <<".keep">>)),
    write_module(Tmp, dev_test_tree, <<"-export([ok/3]).\n">>),
    write_module(Tmp, dev_test_tree_branch, <<"-export([ok/0]).\n">>),
    write_module(Tmp, dev_test_tree_branch_leaf, <<"-export([ok/0]).\n">>),
    [#{ root := Root, helpers := Helpers, files := Files }] =
        hb_packager:scan([Tmp], #{}),
    ?assertEqual(dev_test_tree, Root),
    ?assertEqual(
        [dev_test_tree_branch, dev_test_tree_branch_leaf],
        [H || {H, _} <- Helpers]
    ),
    ?assertEqual(3, map_size(Files)).

write_module(Dir, Mod, Body) ->
    Name = atom_to_binary(Mod, utf8),
    ok = file:write_file(
        filename:join(Dir, <<Name/binary, ".erl">>),
        <<"-module(", Name/binary, ").\n", Body/binary>>
    ).

generated_module_name_pattern_test() ->
    Hash = base32:encode(crypto:hash(sha256, <<"abc">>), [lower, nopad]),
    Mod = hb_device_name:generated(<<"message@1.0">>, Hash),
    Bin = atom_to_binary(Mod, utf8),
    ?assertMatch(<<"_hb_device_message_1_0_", _/binary>>, Bin),
    ?assert(hb_device_name:is_generated(Mod)),
    ?assertMatch({<<"message_1_0">>, _}, hb_device_name:parts(Mod)).

package_emits_root_only_exports_test() ->
    Dir = test_fixture_dir(),
    [Group] = hb_packager:scan([Dir], #{}),
    Pkg = package_for_test(Group),
    Mod = maps:get(module_name, Pkg),
    ?assert(hb_device_name:is_generated(Mod)),
    ok = load_pkg_archive(Pkg),
    Exports = lists:sort(Mod:module_info(exports)),
    % Root exports plus module_info.
    ?assert(lists:member({echo, 3}, Exports)),
    ?assert(lists:member({hello, 3}, Exports)),
    ?assert(lists:member({hello_via_capture, 3}, Exports)),
    ?assertNot(lists:member({greet, 2}, Exports)).

package_helper_not_loaded_separately_test() ->
    Dir = test_fixture_dir(),
    [Group] = hb_packager:scan([Dir], #{}),
    Pkg = package_for_test(Group),
    Mod = maps:get(module_name, Pkg),
    {ok, Modules, _} = hb_device_archive:contents(maps:get(archive, Pkg)),
    [HelperMod] = [M || {M, _, _} <- Modules, M =/= Mod],
    % Ensure helper isn't loaded yet.
    code:purge(dev_test_pkg_helper),
    code:delete(dev_test_pkg_helper),
    ok = load_pkg_archive(Pkg),
    {ok, Greeting} = Mod:hello(#{}, #{}, #{}),
    ?assertEqual(<<"hello">>, Greeting),
    {ok, CapturedGreeting} = Mod:hello_via_capture(#{}, #{}, #{}),
    ?assertEqual(<<"hello">>, CapturedGreeting),
    % The source helper is not loaded; only the generated helper is.
    ?assertEqual(false, code:is_loaded(dev_test_pkg_helper)),
    ?assertMatch({file, _}, code:is_loaded(HelperMod)).

dynamic_internal_dispatch_supported_test() ->
    Dir = dynamic_dispatch_fixture_dir(),
    [Group] = hb_packager:scan([Dir], #{}),
    Pkg = package_for_test(Group),
    Mod = maps:get(module_name, Pkg),
    ?assert(hb_device_name:is_generated(Mod)),
    code:purge(dev_dyn_pkg_helper),
    code:delete(dev_dyn_pkg_helper),
    ok = load_pkg_archive(Pkg),
    ?assertEqual({ok, <<"hello">>}, Mod:call(#{}, #{}, #{})),
    ?assertEqual(false, code:is_loaded(dev_dyn_pkg_helper)).

archive_contains_ebin_and_priv_entries_test() ->
    Dir = priv_fixture_dir(),
    [Group] = hb_packager:scan([Dir], #{}),
    Pkg = package_for_test(Group),
    Msg = hb_packager:impl_message(Pkg, <<"spec-id">>, #{}),
    ?assertEqual(false, maps:is_key(<<"requires-system-architecture">>, Msg)),
    ArchPkg =
        hb_forge_seed:with_forge_bootstrap(
            #{
                <<"bootstrap-device-src">> => [<<"src/preloaded">>],
                <<"requires-system-architecture">> => true
            },
            fun(Opts) -> hb_packager:package(Group, Opts) end
        ),
    ArchMsg = hb_packager:impl_message(ArchPkg, <<"spec-id">>, #{}),
    ?assertEqual(
        hb_util:bin(erlang:system_info(system_architecture)),
        maps:get(<<"requires-system-architecture">>, ArchMsg)
    ),
    {ok, Files} = zip:unzip(maps:get(archive, Pkg), [memory]),
    ByPath =
        maps:from_list([{hb_util:bin(Path), Body} || {Path, Body} <- Files]),
    ?assert(maps:is_key(
        <<"ebin/", (atom_to_binary(maps:get(module_name, Pkg), utf8))/binary,
            ".beam">>,
        ByPath
    )),
    ?assertEqual(<<"#!/bin/sh\n">>, maps:get(<<"priv/bin/tool">>, ByPath)),
    ?assertEqual(<<"fixture-data">>, maps:get(<<"priv/share/data">>, ByPath)).

derived_implements_uses_module_name_test() ->
    Dir = test_fixture_dir(),
    [Group] = hb_packager:scan([Dir], #{}),
    % No `-implements' attribute, so the device name is derived from the
    % module name -- a pure property of the scanned group.
    ?assertEqual(<<"test-pkg@1.0">>, hb_packager:group_device_name(Group)).

%% A device's identity is the unsigned AO-Core message ID of its source
%% set. Changing any source byte must change that ID.
source_id_changes_with_content_test() ->
    Dir = test_fixture_dir(),
    [Group] = hb_packager:scan([Dir], #{}),
    ID1 = maps:get(source_id, package_for_test(Group)),
    HelperPath = filename:join(Dir, <<"dev_test_pkg_helper.erl">>),
    {ok, Old} = file:read_file(HelperPath),
    ok = file:write_file(HelperPath, <<Old/binary, "%% noise\n">>),
    [Group2] = hb_packager:scan([Dir], #{}),
    ID2 = maps:get(source_id, package_for_test(Group2)),
    ?assert(?IS_ID(ID1)),
    ?assert(?IS_ID(ID2)),
    ?assertNotEqual(ID1, ID2).

%% The cryptographic anchor: the generated module name embeds the
%% lowercase-base32 of the source ID's native bytes, so the loaded
%% module is provably bound to the exact source it was built from.
module_name_embeds_source_id_test() ->
    Dir = test_fixture_dir(),
    [Group] = hb_packager:scan([Dir], #{}),
    Pkg = package_for_test(Group),
    SourceID = maps:get(source_id, Pkg),
    ?assert(?IS_ID(SourceID)),
    Hash = base32:encode(hb_util:native_id(SourceID), [lower, nopad]),
    Expected =
        hb_device_name:generated(maps:get(device_name, Pkg), Hash),
    ?assertEqual(Expected, maps:get(module_name, Pkg)).

%% Package through the real forge path: the seed codecs are loaded under
%% their natural names so `hb_message:id/3' can compute the source ID.
package_for_test(Group) ->
    hb_forge_seed:with_forge_bootstrap(
        #{ <<"bootstrap-device-src">> => [<<"src/preloaded">>] },
        fun(Opts) -> hb_packager:package(Group, Opts) end
    ).

%% Load an archive exactly as the runtime does (sans signature checks).
load_pkg_archive(Pkg) ->
    ok = hb_device_archive:load(maps:get(archive, Pkg)).
