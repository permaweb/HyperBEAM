%%% @doc remote scheduler with legacy net tests 
-module(hb_legacy_tests).

-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-ifdef(ENABLE_GENESIS_WASM).
%% @doc Create hb node for testing.
create_node() ->
    ComputeStore = hb_test_utils:test_store(),
    hb_http_server:start_node(
        #{
            priv_wallet => ar_wallet:new(),
            store => [
                ComputeStore,
                #{
                    <<"store-module">> => hb_store_gateway,
                    <<"subindex">> => [
                        #{
                            <<"name">> => <<"Data-Protocol">>,
                            <<"value">> => <<"ao">>
                        }
                    ],
                    <<"local-store">> => [ComputeStore]
                },
                #{
                    <<"store-module">> => hb_store_gateway,
                    <<"local-store">> => [ComputeStore]
                }
            ]
         }
    ).

%% @doc Get a process message to be used to spawn legacy net processes.
get_process_message(ClientOpts, Keys) ->
    % for legacy net process it is important that 
    % Type, Data-Protocol, Variant, Scheduler, Module
    % are upper case
    hb_message:commit(
        maps:merge( 
            #{
                <<"Type">> => <<"Process">>,
                <<"Data-Protocol">> => <<"ao">>,
                <<"Variant">> => <<"ao.TN.1">>,
                <<"Name">> => <<"legacy-1">>,
                <<"Module">> => <<"ISShJH1ij-hPPt9St5UFFr_8Ys3Kj5cyg7zrMGt7H9s">>,
                <<"Scheduler">> => <<"_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA">>,
                <<"device">> => <<"process@1.0">>,
                <<"execution-device">> => <<"genesis-wasm@1.0">>,
                <<"scheduler-device">> => <<"scheduler@1.0">>,
                <<"push-device">> => <<"push@1.0">>
            },
            Keys
        ),
        ClientOpts,
        #{ 
          <<"commitment-device">> => <<"ans104@1.0">>,
          <<"accept-bundle">> => <<"true">>,
          <<"accept">> => <<"application/json">>
         }
    ).

%% @doc spawn a legacy net process using remote scheduler 
legacy_spawn_process_test() -> 
    ClientOpts = 
        #{
            store => [hb_test_utils:test_store()],
            priv_wallet => ar_wallet:new()
         },
    Compute = create_node(),
    ProcessMsg = get_process_message(ClientOpts, #{
        <<"Module">> => <<"ISShJH1ij-hPPt9St5UFFr_8Ys3Kj5cyg7zrMGt7H9s">>,
        <<"Scheduler">> => <<"_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA">>
    }),
    % Create the push path
    {ok, PushRes} = 
        hb_http:post(
          Compute,
          ProcessMsg#{ <<"path">> => <<"push">>, <<"codec-device">> => <<"ans104@1.0">> },
          ClientOpts
        ),
    ?event({push_res, PushRes}),
    ?assertEqual(200, hb_maps:get(~"status", PushRes, 404, #{})),
    ok.

%% @doc spawn ln process and compute a ln message using remote scheduler
legacy_process_msg_test() ->
    ClientOpts = 
        #{
            store => [hb_test_utils:test_store()],
            priv_wallet => ar_wallet:new()
        },
    Compute = create_node(),
    % Spawn legacy process
    ProcessMsg = get_process_message(ClientOpts, #{
        <<"Module">> => <<"ISShJH1ij-hPPt9St5UFFr_8Ys3Kj5cyg7zrMGt7H9s">>,
        <<"Scheduler">> => <<"_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA">>
    }),
    % Create the push path
    {ok, ProcessInfo} = 
        hb_http:post(
          Compute,
          ProcessMsg#{ <<"path">> => <<"push">>, <<"codec-device">> => <<"ans104@1.0">> },
          ClientOpts
        ),
    ProcessID = hb_maps:get(<<"process">>, ProcessInfo, ClientOpts),
    ?event(tom, {id, {explicit, ProcessID}}),
    % Create a message for a legacy process with Balance action
    Message = hb_message:commit(
        #{
            <<"type">> => <<"Message">>,
            <<"data-protocol">> => <<"ao">>,
            <<"variant">> => <<"ao.TN.1">>,
            <<"data">> => <<"Hello">>,
            <<"target">> => ProcessID
         },
        ClientOpts,
        #{ <<"commitment-device">> => <<"ans104@1.0">> }
    ),
    % Create the push path
    PushPath = <<ProcessID/binary, "~process@1.0/push">>,
    {_Status, PushRes} = 
        hb_http:post(
          Compute,
          Message#{ <<"path">> => PushPath, <<"codec-device">> => <<"ans104@1.0">> },
          ClientOpts
        ),
    ?event({push_res, PushRes}),
    ?assertEqual(200, hb_maps:get(~"status", PushRes, 404, #{})),
    ok.

-endif.