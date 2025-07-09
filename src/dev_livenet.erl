%%% @doc Livenet scheduler availability and staking system for AO.
%%% This device manages scheduler registration, availability monitoring,
%%% and economic incentives for reliable scheduler operation.
-module(dev_livenet).
-export([info/1, info/3, join_network/3, schedule/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Exported function for getting device info, controls which functions are
%% exposed via the device API.
info(_) -> 
    #{ exports => [info, join_network, schedule] }.

%% @doc HTTP info response providing information about this device
info(_Msg1, _Msg2, _Opts) ->
    InfoBody = #{
        <<"description">> => <<"Livenet scheduler availability and staking system">>,
        <<"version">> => <<"1.0">>,
        <<"api">> => #{
            <<"info">> => #{
                <<"description">> => <<"Get device info">>
            },
            <<"join_network">> => #{
                <<"description">> => <<"Register scheduler in livenet with staking parameters">>,
                <<"livenet_registration_opts">> => [#{
                    <<"scheduler-id">> => <<"ID of the scheduler to register">>,
                    <<"max-request-cost">> => <<"Maximum cost per request in AO tokens">>,
                    <<"token-per-failed-request">> => <<"Penalty amount per failed request">>,
                    <<"max-penalties-per-epoch">> => <<"Maximum penalties allowed per epoch">>,
                    <<"lock-duration">> => <<"Staking lock duration in milliseconds">>,
                    <<"min-complainers">> => <<"Minimum number of complainers required for slashing">>,
                    <<"node-types">> => <<"Required node types for complaints (e.g. sev-snp,tdx,jacked-in)">>,
                    <<"stake-amount">> => <<"Amount of AO tokens to stake">>
                }]
            },
            <<"schedule">> => #{
                <<"description">> => <<"Schedule message with availability monitoring">>,
                <<"method">> => <<"POST">>
            }
        }
    },
    {ok, InfoBody}.

%% @doc Join network function - allows schedulers to register with their service parameters
%% All user-defined variables are passed through Opts
join_network(_M1, M2, Opts) ->
    ?event({livenet_join_network, {msg, M2}, {opts, Opts}}),
    
    % Extract parameters from message and options
    % User-defined variables are in Opts
    SchedulerID = hb_ao:get(<<"scheduler-id">>, M2, undefined, Opts),
    
    % TODO: Implement registration logic
    % - Validate parameters
    % - Store service registration
    % - Integrate with staking system
    
    {ok, #{
        <<"status">> => <<"success">>,
        <<"message">> => <<"Scheduler registration placeholder">>,
        <<"scheduler_id">> => SchedulerID
    }}.

%% @doc Schedule function with availability monitoring
schedule(M1, M2, Opts) ->
    ?event({livenet_schedule, {m1, M1}, {m2, M2}, {opts, Opts}}),
    
    % TODO: Implement scheduling with monitoring
    % - Check if scheduler is registered in livenet
    % - Attempt scheduling
    % - Monitor availability and initiate complaints if needed
    
    {ok, <<"Schedule placeholder - monitoring not yet implemented">>}.