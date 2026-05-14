%%% @doc `rebar3 device publish' — package, sign and upload device
%%% specifications and implementations to Arweave.
%%%
%%% Publishing reuses the same packaging pipeline as `device preload'
%%% but routes the signed messages through L1 Arweave transactions so
%%% they are visible to GraphQL indexers.
-module(plugin_prv_publish).
-export([init/1, do/1, format_error/1]).

-define(NAMESPACE, device).
-define(PROVIDER, publish).

init(State) ->
    % Create the provider.
    Provider =
        providers:create([
            {name, ?PROVIDER},
            {namespace, ?NAMESPACE},
            {module, ?MODULE},
            {bare, true},
            {deps, [{default, app_discovery}, {default, compile}]},
            {example, "rebar3 device publish --key wallet.json"},
            {opts, plugin_args:opts()},
            {short_desc, "Sign and upload packaged devices to Arweave."},
            {desc,
                "Package and sign each device's spec + impl messages as "
                "Arweave L1 transactions, then publish them via the "
                "configured Arweave gateway."
            }
        ]),
    {ok, rebar_state:add_provider(State, Provider)}.

do(State) ->
    Args = plugin_args:parse(State, "_build/device-publish-store"),
    Dirs = maps:get(<<"device-src">>, Args),
    Roots = maps:get(<<"device-roots">>, Args, all),
    KeyPath = maps:get(<<"key">>, Args),
    Wallet = load_wallet(KeyPath),
    {ok, Preload} =
        plugin_prv_preload:run(
            Args#{ <<"device-roots">> => all },
            #{}
        ),
    NodeOpts =
        #{
            <<"priv-wallet">> => Wallet,
            <<"preloaded-store">> => maps:get(store, Preload),
            <<"preloaded-devices-index">> => maps:get(index, Preload)
        },
    % Scan the source directory for root device groups.
    Groups = hb_packager:scan(Dirs, #{ <<"device-roots">> => Roots }),
    % Package each device group.
    Pkgs = [hb_packager:package(G, NodeOpts) || G <- Groups],
    hb_http_client:init(#{}),
    % Sign and upload each package.
    Results =
        lists:map(
            fun(Pkg) ->
                % Sign and upload the specification message.
                SpecMsg = hb_packager:spec_message(Pkg, NodeOpts),
                Spec =
                    hb_message:commit(
                        SpecMsg,
                        NodeOpts,
                        <<"ans104@1.0">>
                    ),
                {ok, _} = hb_cache:write(Spec, NodeOpts),
                SpecID = upload(Spec, NodeOpts),
                % Sign and upload the implementation message.
                ImplMsg = hb_packager:impl_message(Pkg, SpecID, NodeOpts),
                Impl =
                    hb_message:commit(
                        ImplMsg,
                        NodeOpts,
                        <<"ans104@1.0">>
                    ),
                {ok, _} = hb_cache:write(Impl, NodeOpts),
                ImplID = upload(Impl, NodeOpts),
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
            rebar_api:info("device publish4: ~s spec=~s impl=~s",
                [Name, SID, IID])
        end,
        Results
    ),
    {ok, State}.

load_wallet(undefined) -> hb:wallet();
load_wallet(Path) -> hb:wallet(binary_to_list(hb_util:bin(Path))).

upload(Msg, Opts) ->
    TxMsg =
        hb_message:with_commitments(
            #{ <<"commitment-device">> => <<"ans104@1.0">> },
            Msg,
            Opts
        ),
    ID = hb_message:id(TxMsg, signed, Opts),
    case dev_arweave:tx(
        TxMsg,
        #{
            <<"method">> => <<"POST">>,
            <<"target">> => <<"base">>
        },
        Opts
    ) of
        {ok, #{ <<"id">> := ID }} ->
            ID;
        {ok, #{ <<"id">> := OtherID }} ->
            error({publish_id_mismatch, ID, OtherID});
        {ok, Res} ->
            error({publish_upload_failed, ID, Res});
        {error, Reason} ->
            error({publish_upload_failed, ID, Reason})
    end.

format_error(Reason) ->
    io_lib:format("device publish failed: ~p", [Reason]).
