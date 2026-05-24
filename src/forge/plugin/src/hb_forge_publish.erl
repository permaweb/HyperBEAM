%%% @doc `rebar3 device publish' - package, sign and upload device
%%% specifications and implementations to Arweave.
%%%
%%% Publishing reuses the same packaging pipeline as `device preload'
%%% but routes the signed messages through `dev_arweave' instead of a
%%% local preloaded store.
-module(hb_forge_publish).
-export([init/1, do/1, format_error/1]).

-define(PROVIDER, publish).

%% @doc Register the `publish' provider with rebar3.
init(State) ->
    hb_forge_args:provider(
        State,
        ?PROVIDER,
        ?MODULE,
        "rebar3 device publish --key wallet.json",
        "Sign and upload packaged devices to Arweave.",
        "Package and sign device specs + implementations, then upload them."
    ).

%% @doc Package, sign, and upload selected devices through the Arweave store.
do(State) ->
    case hb_forge_args:maybe_help(State, ?MODULE) of
        true -> {ok, State};
        false -> do_run(State)
    end.

do_run(State) ->
    Args = hb_forge_args:parse(State, <<"_build/device-publish-store">>),
    KeyPath = maps:get(<<"key">>, Args),
    Wallet = hb_forge_args:load_wallet(KeyPath),
    {ok, Preload} =
        hb_forge_preload:run(
            Args#{ <<"device-roots">> => all },
            #{}
        ),
    NodeOpts =
        #{
            <<"priv-wallet">> => Wallet,
            <<"preloaded-store">> => maps:get(store, Preload),
            <<"preloaded-devices-index">> => maps:get(index, Preload),
            <<"bootstrap-device-src">> =>
                hb_forge_args:bootstrap_preloaded_dirs(),
            <<"store">> =>
                [#{ <<"store-module">> => hb_store_arweave }]
        },
    % Sign and upload each package.
    lists:foreach(
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
            rebar_api:info(
                "device publish: ~s spec=~s impl=~s",
                [maps:get(device_name, Pkg), SpecID, ImplID]
            )
        end,
        hb_packager:package_all(
            hb_forge_args:scan_devices(Args),
            NodeOpts
        )
    ),
    {ok, State}.

%% @doc Render provider failures for rebar3.
format_error(Reason) ->
    io_lib:format("device publish failed: ~p", [Reason]).
