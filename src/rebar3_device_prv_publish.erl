%%% @doc `rebar3 device publish' — package, sign and upload device
%%% specifications and implementations to Arweave.
%%%
%%% Publishing reuses the same packaging pipeline as `device preload'
%%% but routes the signed messages through `dev_arweave' instead of a
%%% local preloaded store.
-module(rebar3_device_prv_publish).
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
            {opts, rebar3_device_args:opts()},
            {short_desc, "Sign and upload packaged devices to Arweave."},
            {desc,
                "Package and sign each device's spec + impl messages, then "
                "publish them via the configured Arweave bundler."
            }
        ]),
    {ok, rebar_state:add_provider(State, Provider)}.

do(State) ->
    Args = rebar3_device_args:parse(State),
    Dirs = maps:get(<<"device-src">>, Args),
    Roots = maps:get(<<"device-roots">>, Args, all),
    KeyPath = maps:get(<<"key">>, Args),
    Wallet = load_wallet(KeyPath),
    % Configure the node options so cache write uploads to Arweave.
    NodeOpts =
        #{
            <<"priv-wallet">> => Wallet,
            <<"store">> =>
                [#{ <<"store-module">> => hb_store_arweave }]
        },
    % Scan the source directory for root device groups.
    Groups = hb_packager:scan(Dirs, #{ <<"device-roots">> => Roots }),
    % Package each device group.
    Pkgs = [hb_packager:package(G, NodeOpts) || G <- Groups],
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

load_wallet(undefined) -> hb:wallet();
load_wallet(Path) -> hb:wallet(binary_to_list(hb_util:bin(Path))).

format_error(Reason) ->
    io_lib:format("device publish failed: ~p", [Reason]).
