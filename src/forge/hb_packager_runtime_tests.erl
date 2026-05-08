%%% @doc End-to-end tests for the device packaging pipeline:
%%% packager -> preload -> runtime resolution.
%%%
%%% Each test builds a self-contained `preloaded-store' from a tiny
%%% in-memory device, points the runtime at that store, and asserts on
%%% the behaviour of {@link hb_ao_device:load/2}.
-module(hb_packager_runtime_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% Build a runtime opts map that uses a freshly-built preloaded-store
%% and a per-test volatile device-store cache.
setup() ->
    SrcDir = hb_packager:test_fixture_dir(),
    ok = filelib:ensure_dir(filename:join([SrcDir, "priv", "share", "data"])),
    ok = file:write_file(filename:join([SrcDir, "priv", "share", "data"]),
        <<"runtime-priv">>),
    Groups =
        hb_packager:scan([SrcDir], #{})
            ++ hb_packager:scan(
                ["src/preloaded"],
                #{ <<"device-roots">> => [dev_name, dev_message] }
            ),
    Pkgs = [hb_packager:package(Group, #{}) || Group <- Groups],
    [Pkg] =
        [
            Package
        ||
            Package <- Pkgs,
            maps:get(device_name, Package) =:= <<"test-pkg@1.0">>
        ],
    Wallet = ar_wallet:new(),
    % Use the encode/0 form ar_wallet uses internally so this matches
    % whatever `hb_message:signers/2' returns for impl messages.
    Address = hb_util:encode(ar_wallet:to_address(Wallet)),
    PreloadDir =
        list_to_binary(filename:join(["/tmp",
            "hb_pkg_rt_" ++ integer_to_list(erlang:system_time())])),
    {ok, Result} = hb_preload:build_dir(Pkgs, Wallet, PreloadDir, #{}),
    Store = maps:get(store, Result),
    Index = maps:get(index, Result),
    SpecIDs = maps:get(specs, Result),
    SpecID = maps:get(<<"test-pkg@1.0">>, SpecIDs),
    {ok, [ImplID | _]} =
        hb_cache:match(#{
            <<"data-protocol">> => <<"ao">>,
            <<"variant">> => <<"ao.N.1">>,
            <<"implements-device">> => SpecID
        }, #{ <<"store">> => Store }),
    DevStore = hb_test_utils:test_store(),
    Opts = #{
        <<"store">> => [Store],
        <<"preloaded-store">> => Store,
        <<"preloaded-devices-index">> => Index,
        <<"device-store">> => DevStore,
        % The build wallet's address is what the preloaded-store
        % messages are signed by, and the runtime must enforce that
        % trust even on the bootstrap/direct load path.
        <<"trusted-device-signers">> => [Address],
        <<"priv-wallet">> => Wallet
    },
    {Pkg, Opts, SpecIDs, ImplID}.

teardown(_) -> ok.

%% Build the EUnit fixture so each case gets a fresh preloaded-store
%% and fresh device-store cache; this prevents test-to-test bleed
%% from earlier setups that signed with different wallets.
all_runtime_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            fun({Pkg, Opts, _, _}) ->
                fun() ->
                    Name = maps:get(device_name, Pkg),
                    {ok, Mod} = hb_ao_device:load(Name, Opts),
                    ?assert(hb_packager:is_generated_module(Mod)),
                    ?assertEqual(maps:get(module_name, Pkg), Mod)
                end
            end,
            fun({Pkg, Opts, _, _}) ->
                fun() ->
                    Name = maps:get(device_name, Pkg),
                    {ok, Mod1} = hb_ao_device:load(Name, Opts),
                    {ok, Mod2} = hb_ao_device:load(Name, Opts),
                    ?assertEqual(Mod1, Mod2),
                    DevStore = maps:get(<<"device-store">>, Opts),
                    {ok, Cached} =
                        hb_store:read(DevStore,
                            <<"devices/", Name/binary>>, Opts),
                    ?assertEqual(
                        atom_to_binary(maps:get(module_name, Pkg), utf8),
                        Cached)
                end
            end,
            fun({Pkg, Opts, _, _}) ->
                fun() ->
                    Name = maps:get(device_name, Pkg),
                    {ok, Mod} = hb_ao_device:load(Name, Opts),
                    Dir = hb_ao_device:implementation_dir(Mod),
                    {ok, Body} =
                        file:read_file(
                            filename:join([Dir, "share", "data"])
                        ),
                    ?assertEqual(<<"runtime-priv">>, Body)
                end
            end,
            fun({Pkg, Opts, _, _}) ->
                fun() ->
                    % Trust enforcement applies on the bootstrap/direct
                    % load path too. With trust restricted to an
                    % unrelated signer the load must fail.
                    Name = maps:get(device_name, Pkg),
                    Other = hb_util:human_id(crypto:strong_rand_bytes(32)),
                    BadOpts = Opts#{
                        <<"trusted-device-signers">> => [Other],
                        <<"device-store">> => hb_test_utils:test_store()
                    },
                    ?assertMatch(
                        {error, _},
                        hb_ao_device:load(Name, BadOpts))
                end
            end,
            fun({Pkg, Opts, SpecIDs, ImplID}) ->
                fun() ->
                    SpecID = maps:get(<<"test-pkg@1.0">>, SpecIDs),
                    Other = hb_util:human_id(crypto:strong_rand_bytes(32)),
                    IDOpts = Opts#{
                        <<"trusted-device-signers">> => [Other],
                        <<"trusted-devices">> => [ImplID],
                        <<"device-store">> => hb_test_utils:test_store()
                    },
                    {ok, Mod} = hb_ao_device:load(SpecID, IDOpts),
                    ?assertEqual(maps:get(module_name, Pkg), Mod)
                end
            end,
            fun({_Pkg, Opts, _, _}) ->
                fun() ->
                    Index = maps:get(<<"preloaded-devices-index">>, Opts),
                    Store = maps:get(<<"preloaded-store">>, Opts),
                    {ok, Got} =
                        hb_store:read(Store,
                            <<Index/binary, "/test-pkg@1.0">>, Opts),
                    ?assert(byte_size(Got) == 43)
                end
            end
        ]
    }.

unpackaged_atom_is_rejected_test() ->
    ?assertMatch(
        {error, #{ <<"error">> := <<"device-must-be-packaged">> }},
        hb_ao_device:load(dev_message, #{})
    ),
    ?assertMatch(
        {error, #{ <<"error">> := <<"device-must-be-packaged">> }},
        hb_ao_device:load(
            dev_message,
            #{ <<"device-bootstrap">> => hb_packager:bootstrap_device_map() }
        )
    ).
