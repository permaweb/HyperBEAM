%%% @doc Build a filesystem-backed preload store for packaged devices.
-module(hb_device_preloader).
-export([build/0, build/1]).

-define(DEFAULT_SRC_DIR, "src").
-define(DEFAULT_OUT_DIR, "_build/default/packaged-devices").
-define(DEFAULT_STORE_DIR, "_build/default/preloaded-device-store").
-define(DEFAULT_METADATA_FILE, "_build/default/preloaded-device-metadata.eterm").

%% @doc Build the default preloaded device store.
build() ->
    build(#{}).

%% @doc Build a preloaded device store from the configured device sources.
build(Opts) ->
    SrcDir = maps:get(src_dir, Opts, ?DEFAULT_SRC_DIR),
    OutDir = maps:get(out_dir, Opts, ?DEFAULT_OUT_DIR),
    StoreDir = hb_util:bin(maps:get(store_dir, Opts, ?DEFAULT_STORE_DIR)),
    MetadataFile = maps:get(metadata_file, Opts, ?DEFAULT_METADATA_FILE),
    Wallet = wallet(Opts),
    Store = #{
        <<"store-module">> => hb_store_fs,
        <<"name">> => StoreDir
    },
    StoreOpts = store_opts(Store, Wallet),
    hb_store:reset(Store),
    ok = hb_store:start(Store),
    Results =
        hb_device_packager:package_devices(#{
            src_dir => SrcDir,
            out_dir => OutDir,
            roots => maps:get(roots, Opts, all),
            spec => maps:get(spec, Opts, undefined),
            specs => maps:get(specs, Opts, #{}),
            print => maps:get(print, Opts, true)
        }),
    Devices = lists:map(fun(Result) -> write_device(Result, StoreOpts) end, Results),
    Metadata =
        #{
            <<"store">> => Store,
            <<"metadata-file">> => hb_util:bin(MetadataFile),
            <<"signer">> => hb_util:human_id(ar_wallet:to_address(Wallet)),
            <<"name-resolver">> =>
                maps:from_list(
                    [
                        {maps:get(<<"name">>, Device), maps:get(<<"spec-id">>, Device)}
                    ||
                        Device <- Devices
                    ]
                ),
            <<"devices">> => Devices
        },
    ok = write_metadata(MetadataFile, Metadata),
    Metadata.

%% @doc Return the wallet used to sign preload messages.
wallet(Opts) ->
    case maps:get(wallet, Opts, undefined) of
        undefined ->
            hb:wallet(maps:get(key, Opts, "hyperbeam-key.json"));
        Wallet ->
            Wallet
    end.

%% @doc Options used while signing and writing preload messages.
store_opts(Store, Wallet) ->
    DeviceStore = #{
        <<"store-module">> => hb_store_volatile,
        <<"name">> => <<"preload-build-device-cache">>
    },
    ok = hb_store:start(DeviceStore),
    ok =
        hb_store:write(
            DeviceStore,
            #{
                <<"devices/structured@1.0">> => <<"dev_structured">>,
                <<"devices/httpsig@1.0">> => <<"dev_httpsig">>
            },
            #{}
        ),
    #{
        <<"store">> => Store,
        <<"match-index">> => Store,
        <<"device-store">> => DeviceStore,
        <<"priv-wallet">> => Wallet,
        <<"commitment-device">> => dev_httpsig
    }.

%% @doc Sign and write one packaged device spec and implementation.
write_device(
    #{
        root := Root,
        name := Name,
        implements := Implements,
        module := Module,
        beam := Beam,
        exports := Exports,
        files := Files,
        spec := Spec
    },
    Opts
) ->
    SpecID = write_spec(Root, Name, Implements, Exports, Spec, Opts),
    ImplMsg =
        sign(hb_ao:normalize_keys(
            #{
                <<"data-protocol">> => <<"ao">>,
                <<"variant">> => <<"ao.N.1">>,
                <<"type">> => <<"device-implementation">>,
                <<"content-type">> => <<"application/beam">>,
                <<"implements-device">> => SpecID,
                <<"module-name">> => hb_util:bin(Module),
                <<"requires-otp-release">> =>
                    hb_util:bin(erlang:system_info(otp_release)),
                <<"body">> => Beam
            },
            Opts
        ), Opts),
    {ok, _ImplUnsignedID} = hb_cache:write(ImplMsg, Opts),
    ImplID = hb_message:id(ImplMsg, signed, Opts),
    #{
        <<"name">> => Name,
        <<"root-module">> => hb_util:bin(Root),
        <<"module-name">> => hb_util:bin(Module),
        <<"spec-id">> => SpecID,
        <<"implementation-id">> => ImplID,
        <<"files">> => [hb_util:bin(File) || {_Mod, File} <- Files]
    }.

%% @doc Sign a message with the preload commitment device.
sign(Msg, Opts) ->
    hb_message:commit(Msg, Opts, #{ <<"commitment-device">> => dev_httpsig }).

%% @doc Write the implemented device spec, unless it is already an ID.
write_spec(_Root, _Name, Implements, _Exports, _Spec, _Opts)
        when is_binary(Implements), byte_size(Implements) == 43 ->
    Implements;
write_spec(Root, _Name, Implements, Exports, Spec, Opts) ->
    SpecMsg =
        sign(#{
            <<"data-protocol">> => <<"ao">>,
            <<"variant">> => <<"ao.N.1">>,
            <<"type">> => <<"device-spec">>,
            <<"name">> => Implements,
            <<"root-module">> => hb_util:bin(Root),
            <<"exports">> => encode_exports(Exports),
            <<"content-type">> => maps:get(<<"content-type">>, Spec),
            <<"body">> => maps:get(<<"body">>, Spec)
        }, Opts),
    {ok, _SpecUnsignedID} = hb_cache:write(SpecMsg, Opts),
    hb_message:id(SpecMsg, signed, Opts).

%% @doc Encode export pairs into structured values.
encode_exports(Exports) ->
    [
        #{
            <<"function">> => hb_util:bin(Function),
            <<"arity">> => Arity
        }
    ||
        {Function, Arity} <- Exports
    ].

%% @doc Write the generated metadata as an Erlang term file.
write_metadata(File, Metadata) ->
    ok = filelib:ensure_dir(File),
    file:write_file(File, io_lib:format("~p.~n", [Metadata])).
