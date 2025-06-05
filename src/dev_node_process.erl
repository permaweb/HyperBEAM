%%% @doc A device that implements the singleton pattern for processes specific
%%% to an individual node. This device uses the `local-name@1.0' device to
%%% register processes with names locally, persistenting them across reboots.
%%% 
%%% Definitions of singleton processes are expected to be found with their 
%%% names in the `node_processes' section of the node message.
-module(dev_node_process).
-export([info/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Register a default handler for the device. Inherits `keys' and `set'
%% from the default device.
info(_Opts) ->
    #{
        default => fun lookup/4,
        excludes => [<<"set">>, <<"keys">>]
    }.

%% @doc Lookup a process by name.
lookup(Name, _Base, Req, Opts) ->
    ?event(node_process, {lookup, {name, Name}}),
    LookupRes =
        hb_ao:resolve(
            #{ <<"device">> => <<"local-name@1.0">> },
            #{ <<"path">> => <<"lookup">>, <<"key">> => Name, <<"load">> => true },
            Opts
        ),
    case LookupRes of
        {ok, ProcessID} ->
            hb_cache:read(ProcessID, Opts);
        {error, not_found} ->
            case hb_ao:get(<<"spawn">>, Req, true, Opts) of
                true ->
                    spawn_register(Name, Opts);
                false ->
                    {error, not_found}
            end
    end.

%% @doc Spawn a new process according to the process definition found in the 
%% node message, and register it with the given name.
spawn_register(Name, Opts) ->
    case hb_opts:get(node_processes, #{}, Opts) of
        #{ Name := BaseDef } ->
            % We have found the base process definition. Augment it with the 
            % node's address as necessary, then commit to the result.
            ?event(node_process, {registering, {name, Name}, {base_def, BaseDef}}),
            Signed = hb_message:commit(augment_definition(BaseDef, Opts), Opts),
            ID = hb_message:id(Signed, signed, Opts),
            ?event(node_process, {spawned, {name, Name}, {process, Signed}}),
            % `POST' to the schedule device for the process to start its sequence.
            {ok, Assignment} =
                hb_ao:resolve(
                    Signed,
                    #{
                        <<"path">> => <<"schedule">>,
                        <<"method">> => <<"POST">>,
                        <<"body">> => Signed
                    },
                    Opts
                ),
            ?event(node_process, {initialized, {name, Name}, {assignment, Assignment}}),
            RegResult =
                dev_local_name:direct_register(
                    #{ <<"key">> => Name, <<"value">> => ID },
                    Opts
                ),
            ?event(node_process, {registered, {name, Name}, {process_id, ID}}),
            case RegResult of
                {ok, _} ->
                    {ok, Signed};
                {error, Err} ->
                    {error, #{
                        <<"status">> => 500,
                        <<"body">> => <<"Failed to register process.">>,
                        <<"details">> => Err
                    }}
            end;
        _ ->
            % We could not find the base process definition for the given name
            % in the node message.
            {error, not_found}
    end.

%% @doc Augment the given process definition with the node's address.
augment_definition(BaseDef, Opts) ->
    Address =
        hb_util:human_id(
            ar_wallet:to_address(
                hb_opts:get(priv_wallet, no_viable_wallet, Opts)
            )
        ),
    hb_ao:set(
        BaseDef,
        #{
            <<"scheduler">> => Address
        }
    ).

%%% Tests

%%% The name that should be used for the singleton process during tests.
-define(TEST_NAME, <<"test-node-process">>).

%% @doc Helper function to generate a test environment and its options.
generate_test_opts() ->
    {ok, Module} = file:read_file(<<"test/test.lua">>),
    generate_test_opts(#{
        ?TEST_NAME => #{
            <<"device">> => <<"process@1.0">>,
            <<"execution-device">> => <<"lua@5.3a">>,
            <<"scheduler-device">> => <<"scheduler@1.0">>,
            <<"module">> => #{
                <<"content-type">> => <<"text/x-lua">>,
                <<"body">> => Module
            }
        }
    }).
generate_test_opts(Defs) ->
    #{
        store =>
            [
                #{
                    <<"store-module">> => hb_store_fs,
                    <<"prefix">> =>
                        <<
                            "cache-TEST-",
                            (integer_to_binary(os:system_time(millisecond)))/binary
                        >>
                }
            ],
        node_processes => Defs,
        priv_wallet => ar_wallet:new()
    }.

lookup_no_spawn_test() ->
    Opts = generate_test_opts(),
    ?assertEqual(
        {error, not_found},
        lookup(<<"name1">>, #{}, #{}, Opts)
    ).

lookup_spawn_test() ->
    Opts = generate_test_opts(),
    Res1 = {_, Process1} =
        hb_ao:resolve(
            #{ <<"device">> => <<"node-process@1.0">> },
            ?TEST_NAME,
            Opts
        ),
    ?assertMatch(
        {ok, #{ <<"device">> := <<"process@1.0">> }},
        Res1
    ),
    {ok, Process2} = hb_ao:resolve(
        #{ <<"device">> => <<"node-process@1.0">> },
        ?TEST_NAME,
        Opts
    ),
    ?assertEqual(Process1, Process2).

%% @doc Test that a process can be spawned, executed upon, and its result retrieved.
lookup_execute_test() ->
    Opts = generate_test_opts(),
    Res1 =
        hb_ao:resolve_many(
            [
                #{ <<"device">> => <<"node-process@1.0">> },
                ?TEST_NAME,
                #{
                    <<"path">> => <<"schedule">>,
                    <<"method">> => <<"POST">>,
                    <<"body">> =>
                        hb_message:commit(
                            #{
                                <<"path">> => <<"compute">>,
                                <<"test-key">> => <<"test-value">>
                            },
                            Opts
                        )
                }
            ],
            Opts
        ),
    ?assertMatch(
        {ok, #{ <<"slot">> := 1 }},
        Res1
    ),
    ?assertMatch(
        42,
        hb_ao:get(
            << ?TEST_NAME/binary, "/now/results/output/body" >>,
            #{ <<"device">> => <<"node-process@1.0">> },
            Opts
        )
    ).