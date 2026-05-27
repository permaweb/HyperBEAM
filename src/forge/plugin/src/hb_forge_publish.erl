%%% @doc `rebar3 device publish' - package, sign and upload device
%%% specifications and implementations to Arweave.
%%%
%%% Publishing reuses the packager, then uploads the signed messages through
%%% HyperBEAM's Arweave client.
-module(hb_forge_publish).
-export([init/1, do/1, format_error/1]).
-include("../../../core/include/ar.hrl").

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
    ExternalSigner = maps:get(<<"external-signer">>, Args),
    Wallet = hb_forge_args:load_wallet(KeyPath),
    case {ExternalSigner, PublishCodec} of
        {undefined, _} -> ok;
        {_, <<"tx@1.0">>} -> ok;
        _ ->
            rebar_api:abort(
                "--external-signer requires --publish-codec tx@1.0, got ~s",
                [PublishCodec]
            )
    end,
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
            Spec = sign(
                hb_packager:spec_message(Pkg, NodeOpts),
                spec,
                Pkg,
                NodeOpts,
                PublishCodec,
                ExternalSigner,
                maps:get(<<"output-dir">>, Args)
            ),
            {ok, _} = hb_client_remote:upload(Spec, NodeOpts, PublishCodec),
            SpecID = hb_message:id(Spec, all, NodeOpts),
            % Sign and upload the implementation message.
            Impl = sign(
                hb_packager:impl_message(Pkg, SpecID, NodeOpts),
                impl,
                Pkg,
                NodeOpts,
                PublishCodec,
                ExternalSigner,
                maps:get(<<"output-dir">>, Args)
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

%% @doc Sign with HyperBEAM's wallet flow or an external tx@1.0 signer.
sign(Msg, _Stage, _Pkg, Opts, PublishCodec, undefined, _OutputDir) ->
    hb_message:commit(Msg, Opts, PublishCodec);
sign(Msg, Stage, Pkg, Opts, _PublishCodec, ExternalSigner, OutputDir) ->
    UnsignedTX = hb_message:convert(
        Msg,
        <<"tx@1.0">>,
        <<"structured@1.0">>,
        Opts
    ),
    {UnsignedPath, SignedPath} = signing_paths(OutputDir, Pkg, Stage),
    ok = filelib:ensure_dir(UnsignedPath),
    ok = file:write_file(
        UnsignedPath,
        hb_json:encode(ar_tx:tx_to_json_struct(UnsignedTX))
    ),
    run_external_signer(ExternalSigner, UnsignedPath, SignedPath),
    {ok, SignedJSON} = file:read_file(SignedPath),
    SignedTX0 = ar_tx:json_struct_to_tx(hb_json:decode(SignedJSON)),
    SignedTX = maybe_restore_data(UnsignedTX, SignedTX0),
    case ar_tx:verify(SignedTX) of
        true ->
            hb_message:convert(
                SignedTX,
                <<"structured@1.0">>,
                <<"tx@1.0">>,
                Opts
            );
        false ->
            rebar_api:abort(
                "external signer returned an invalid tx: ~s",
                [SignedPath]
            )
    end.

signing_paths(OutputDir, Pkg, Stage) ->
    Base = iolist_to_binary(
        [
            maps:get(device_name, Pkg),
            <<".">>,
            atom_to_binary(Stage),
            <<".tx.json">>
        ]
    ),
    Dir = filename:join(OutputDir, <<"signing">>),
    {
        filename:join(Dir, <<"unsigned.", Base/binary>>),
        filename:join(Dir, <<"signed.", Base/binary>>)
    }.

run_external_signer(ExternalSigner, UnsignedPath, SignedPath) ->
    Env = [
        {"IN", hb_util:list(UnsignedPath)},
        {"OUT", hb_util:list(SignedPath)}
    ],
    case rebar_utils:sh(
        hb_util:list(ExternalSigner),
        [{env, Env}, {return_on_error, true}]
    ) of
        {ok, _} ->
            ok;
        {error, {Code, Output}} ->
            rebar_api:abort(
                "external signer failed with exit code ~p: ~s",
                [Code, Output]
            )
    end.

maybe_restore_data(UnsignedTX, #tx{ data = ?DEFAULT_DATA } = SignedTX) ->
    SignedTX#tx{ data = UnsignedTX#tx.data };
maybe_restore_data(_UnsignedTX, SignedTX) ->
    SignedTX.

%% @doc Render provider failures for rebar3.
format_error(Reason) ->
    io_lib:format("device publish failed: ~p", [Reason]).
