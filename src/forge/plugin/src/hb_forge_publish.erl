%%% @doc `rebar3 device publish' - package, sign and upload device
%%% specifications and implementations to Arweave.
%%%
%%% Publishing reuses the packager, then uploads the signed messages through
%%% HyperBEAM's Arweave client.
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

%% @doc Package, sign, and upload selected devices.
do(State) ->
    case hb_forge_args:maybe_help(State, ?MODULE) of
        true -> {ok, State};
        false -> do_run(State)
    end.

do_run(State) ->
    Args = hb_forge_args:parse(State, <<"_build/device-publish-store">>),
    KeyPath = maps:get(<<"key">>, Args),
    PublishCodec = maps:get(<<"publish-codec">>, Args),
    Wallet = hb_forge_args:load_wallet(KeyPath),
    Opts =
        (hb_forge_args:package_opts(Args))#{
            <<"priv-wallet">> => Wallet,
            <<"prometheus">> => false,
            <<"commitment-device">> => PublishCodec
        },
    {ok, _} = application:ensure_all_started(hackney),
    case hb_http_client:start_link(Opts) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    NodeOpts = hb_forge_seed:with_forge_bootstrap(Opts, fun(Seed) -> Seed end),
    % Sign and upload each package.
    lists:foreach(
        fun(Pkg) ->
            % Sign and upload the specification message.
            Spec =
                hb_message:commit(
                    hb_packager:spec_message(Pkg, NodeOpts),
                    NodeOpts,
                    PublishCodec
                ),
            {ok, _} = hb_client_remote:upload(Spec, NodeOpts, PublishCodec),
            SpecID = hb_message:id(Spec, all, NodeOpts),
            % Sign and upload the implementation message.
            Impl =
                hb_message:commit(
                    hb_packager:impl_message(Pkg, SpecID, NodeOpts),
                    NodeOpts,
                    PublishCodec
                ),
            {ok, _} = hb_client_remote:upload(Impl, NodeOpts, PublishCodec),
            ImplID = hb_message:id(Impl, all, NodeOpts),
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
