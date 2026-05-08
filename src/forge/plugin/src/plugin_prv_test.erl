%%% @doc `rebar3 device test' — package devices, build a preloaded-store,
%%% and run their EUnit suites with the resulting store mounted as the
%%% node's `preloaded-store'.
%%%
%%% This is the developer's primary smoke-test loop: every device under
%%% the configured source directory is packaged, every device's spec/
%%% impl message is signed and indexed, and then its root module EUnit
%%% suites run against the just-built store.
-module(plugin_prv_test).
-export([init/1, do/1, format_error/1]).

-define(NAMESPACE, device).
-define(PROVIDER, test).

init(State) ->
    % Create the provider.
    Provider =
        providers:create([
            {name, ?PROVIDER},
            {namespace, ?NAMESPACE},
            {module, ?MODULE},
            {bare, true},
            {deps, [{default, app_discovery}, {default, compile}]},
            {example, "rebar3 device test"},
            {opts, plugin_args:opts()},
            {short_desc, "Run device EUnit against a fresh preloaded-store."},
            {desc,
                "Package + preload the discovered devices, then run "
                "the device root EUnit suites with the resulting "
                "store as the node's preloaded-store."
            }
        ]),
    {ok, rebar_state:add_provider(State, Provider)}.

do(State) ->
    Args = plugin_args:parse(State, "_build/device-test-store"),
    % Build a complete store from the configured source set so selected
    % device tests can resolve their dependencies.
    {ok, Result} =
        plugin_prv_preload:run(
            Args#{ <<"device-roots">> => all },
            #{}
        ),
    Dirs = maps:get(<<"device-src">>, Args),
    Roots = maps:get(<<"device-roots">>, Args, all),
    % Scan the source directory for root device groups.
    Groups = hb_packager:scan(Dirs, #{ <<"device-roots">> => Roots }),
    % Test every source module in each selected package namespace.
    Modules = lists:usort(lists:append([group_modules(G) || G <- Groups])),
    case Modules of
        [] ->
            rebar_api:info("device test: nothing to test", []),
            {ok, State};
        _ ->
            % Run the tests against the preloaded-store.
            ModuleArg = string:join([atom_to_list(M) || M <- Modules], ","),
            Store = maps:get(store, Result),
            StorePath = maps:get(<<"name">>, Store),
            Index = maps:get(index, Result),
            InnerCmd =
                "HB_PRELOADED_STORE=" ++ shell_quote(StorePath) ++
                " HB_PRELOADED_DEVICES_INDEX=" ++ shell_quote(Index) ++
                " rebar3 eunit --module=" ++ ModuleArg,
            Cmd = "sh -c " ++ shell_quote(InnerCmd),
            rebar_api:info(
                "device test: running ~s against preloaded-store ~s",
                [ModuleArg, StorePath]
            ),
            case rebar_utils:sh(Cmd, [{return_on_error, true}]) of
                {ok, Output} ->
                    % Print the test output.
                    rebar_api:info("~ts", [Output]),
                    {ok, State};
                {error, {Code, Output}} ->
                    % Print the test error output.
                    rebar_api:error("~ts", [Output]),
                    {error, format_error({eunit_failed, Code})}
            end
    end.

shell_quote(Value) ->
    "'" ++
        string:replace(
            binary_to_list(hb_util:bin(Value)),
            "'",
            "'\\''",
            all
        ) ++
        "'".

format_error(Reason) ->
    io_lib:format("device test failed: ~p", [Reason]).

%% @doc Return every source module in a package namespace.
group_modules(G) ->
    [maps:get(root, G)] ++ [H || {H, _Path} <- maps:get(helpers, G)].
