%%% @doc `rebar3 device publish' — package, sign and upload device
%%% specifications and implementations to Arweave.
%%%
%%% Publishing reuses the same packaging pipeline as `device preload'
%%% but routes the signed messages through `dev_arweave' instead of a
%%% local preloaded store.
-module(plugin_prv_publish).
-export([init/1, do/1, format_error/1]).

-define(PROVIDER, publish).

init(State) ->
    plugin_args:provider(
        State,
        ?PROVIDER,
        ?MODULE,
        "rebar3 device publish --key wallet.json",
        "Sign and upload packaged devices to Arweave.",
        "Package and sign device specs + implementations, then upload them."
    ).

do(State) ->
    Args = plugin_args:parse(State, "_build/device-publish-store"),
    KeyPath = maps:get(<<"key">>, Args),
    Wallet = plugin_args:load_wallet(KeyPath),
    {ok, Preload} =
        plugin_prv_preload:run(
            Args#{ <<"device-roots">> => all },
            #{}
        ),
    NodeOpts =
        #{
            <<"priv-wallet">> => Wallet,
            <<"preloaded-store">> => maps:get(store, Preload),
            <<"preloaded-devices-index">> => maps:get(index, Preload),
            <<"bootstrap-device-src">> => plugin_args:bootstrap_preloaded_dirs(),
            <<"store">> =>
                [#{ <<"store-module">> => hb_store_arweave }]
        },
    Pkgs = hb_packager:package_all(plugin_args:scan_devices(Args), NodeOpts),
    % Sign and upload each package.
    Results =
        lists:map(
            fun(Pkg) ->
                % Sign and upload the specification message.
                Spec =
                    hb_message:commit(
                        hb_packager:spec_message(Pkg, NodeOpts),
                        NodeOpts
                    ),
                {ok, SpecID} = hb_cache:write(Spec, NodeOpts),
                % Sign and upload the implementation message.
                Impl =
                    hb_message:commit(
                        hb_packager:impl_message(Pkg, SpecID, NodeOpts),
                        NodeOpts
                    ),
                {ok, ImplID} = hb_cache:write(Impl, NodeOpts),
                #{
                    device_name => maps:get(device_name, Pkg),
                    spec_id => SpecID,
                    impl_id => ImplID
                }
            end,
            Pkgs
        ),
    lists:foreach(
        fun(#{ device_name := Name, spec_id := SID, impl_id := IID }) ->
            rebar_api:info("device publish: ~s spec=~s impl=~s",
                [Name, SID, IID])
        end,
        Results
    ),
    {ok, State}.

format_error(Reason) ->
    io_lib:format("device publish failed: ~p", [Reason]).
