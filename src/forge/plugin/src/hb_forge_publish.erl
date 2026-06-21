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
    hb_forge_args:run_provider(State, ?MODULE, fun publish/1).

publish(State) ->
    Args = hb_forge_args:parse(State, <<"_build/device-publish-store">>),
    KeyPath = maps:get(<<"key">>, Args),
    PublishCodec = maps:get(<<"publish-codec">>, Args),
    DryRun = maps:get(<<"dry-run">>, Args),
    Wallet = hb_forge_args:load_wallet(KeyPath),
    Signer = hb:address(Wallet),
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
            SpecID = hb_message:id(Spec, all, NodeOpts),
            % Sign and upload the implementation message.
            Impl =
                hb_message:commit(
                    hb_packager:impl_message(Pkg, SpecID, NodeOpts),
                    NodeOpts,
                    PublishCodec
                ),
            ImplID = hb_message:id(Impl, all, NodeOpts),
            case DryRun of
                true ->
                    ok;
                false ->
                    {ok, _} = upload(Spec, NodeOpts, PublishCodec),
                    {ok, _} = upload(Impl, NodeOpts, PublishCodec)
            end,
            Action =
                case DryRun of
                    true -> "Signed device (dry run)";
                    false -> "Published device"
                end,
            rebar_api:info(
                "~s: ~s; Specification ID: ~s; Implementation ID: ~s; Signer: ~s.",
                [Action, maps:get(device_name, Pkg), SpecID, ImplID, Signer]
            )
        end,
        hb_packager:package_all(
            hb_forge_args:scan_devices(Args),
            NodeOpts
        )
    ),
    {ok, State}.

upload(Msg, Opts, <<"ans104@1.0">>) ->
    case hb_opts:get(bundler_ans104, not_found, Opts) of
        not_found ->
            {error, no_ans104_bundler};
        Bundler ->
            upload_ans104(Bundler, Msg, Opts)
    end;
upload(Msg, Opts, Codec) ->
    hb_client_remote:upload(Msg, Opts, Codec).

%% @doc Upload an ANS-104 bundle directly to the bundler endpoint.
%% Forge publishes package messages directly to the HyperBEAM bundler route.
upload_ans104(Bundler, Msg, Opts) ->
    {ok, CommittedMsg} =
        hb_message:with_only_committed(hb_private:reset(Msg), Opts),
    Body =
        ar_bundles:serialize(
            hb_message:convert(
                CommittedMsg,
                #{
                    <<"device">> => <<"ans104@1.0">>,
                    <<"bundle">> => true
                },
                Opts
            )
        ),
    Req = #{
        peer => hb_util:bin(Bundler),
        path => <<"/~bundler@1.0/tx">>,
        method => <<"POST">>,
        headers => #{
            <<"codec-device">> => <<"ans104@1.0">>,
            <<"content-type">> => <<"application/ans104">>,
            <<"accept-bundle">> => <<"true">>
        },
        body => Body
    },
    case hb_http_client:request(Req, Opts) of
        {ok, Status, Headers, RespBody} ->
            Result = #{
                <<"status">> => Status,
                <<"headers">> => Headers,
                <<"body">> => RespBody
            },
            if
                Status < 400 -> {ok, Result};
                Status < 500 -> {error, Result};
                true -> {failure, Result}
            end;
        Error ->
            Error
    end.

%% @doc Render provider failures for rebar3.
format_error(Reason) ->
    io_lib:format("device publish failed: ~p", [Reason]).
