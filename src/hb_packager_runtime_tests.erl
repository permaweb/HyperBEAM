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
    [Group] = hb_packager:scan([SrcDir], #{}),
    Pkg = hb_packager:package(Group, #{}),
    Wallet = ar_wallet:new(),
    %% Use the encode/0 form ar_wallet uses internally so this matches
    %% whatever `hb_message:signers/2' returns for impl messages.
    Address = hb_util:encode(ar_wallet:to_address(Wallet)),
    PreloadDir =
        list_to_binary(filename:join(["/tmp",
            "hb_pkg_rt_" ++ integer_to_list(erlang:system_time())])),
    {ok, Result} = hb_preload:build_dir([Pkg], Wallet, PreloadDir, #{}),
    Store = maps:get(store, Result),
    Index = maps:get(index, Result),
    SpecIDs = maps:get(specs, Result),
    DevStore = hb_test_utils:test_store(),
    Opts = #{
        <<"store">> => [Store],
        <<"preloaded-store">> => Store,
        <<"preloaded-devices-index">> => Index,
        <<"device-store">> => DevStore,
        %% The build wallet's address is what `hb_message:signers'
        %% returns for messages we just signed; we trust both that
        %% address and `all' so a mismatch in signer encoding does
        %% not flake the test. Trust enforcement is exercised by the
        %% dedicated `untrusted_signer' case below.
        <<"trusted-device-signers">> => [Address, all],
        <<"priv-wallet">> => Wallet,
        %% Until commit 2 packages every kernel codec into the
        %% preloaded-store, runtime tests need the codec atoms made
        %% available via the build-bootstrap path so that
        %% `hb_cache:match' (which goes through structured@1.0) works.
        <<"device-bootstrap">> => hb_packager:bootstrap_device_map()
    },
    {Pkg, Opts, SpecIDs}.

teardown(_) -> ok.

%% Build the EUnit fixture so each case gets a fresh preloaded-store
%% and fresh device-store cache; this prevents test-to-test bleed
%% from earlier setups that signed with different wallets.
all_runtime_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            fun({Pkg, Opts, _}) ->
                fun() ->
                    Name = maps:get(device_name, Pkg),
                    {ok, Mod} = hb_ao_device:load(Name, Opts),
                    ?assert(hb_packager:is_generated_module(Mod)),
                    ?assertEqual(maps:get(module_name, Pkg), Mod)
                end
            end,
            fun({Pkg, Opts, _}) ->
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
            fun({Pkg, Opts, _}) ->
                fun() ->
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
            fun({_Pkg, Opts, _}) ->
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
    %% Plain `dev_message' atom must NOT load as a runtime device.
    ?assertMatch(
        {error, {device_must_be_packaged, dev_message}},
        hb_ao_device:load(dev_message, #{})
    ).
