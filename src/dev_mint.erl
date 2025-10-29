%%% @doc A device implementing a fair proportional distribution minting scheme.
%%% 
%%% The device maintains three sets of records:
%%% 
%%% 1. A set of _Quantities_ of given _Resources_ per _Address_.
%%% 2. A set of _Resources_ with _Weights_ and _Oracles_.
%%% 3. A set of metadata fields:
%%%   -> The expected total supply of the resource to mint.
%%%   -> The cycle count.
%%%   -> The cycle period.
%%%   -> The start timestamp.
%%%   -> The proportion of the total supply to be minted per cycle.
%%%   -> An `Client` address to send `Mint` messages to.
%%%   -> An optional address that is able to update the mint process state
%%%      metadata.
-module(dev_mint).
-export([compute/3]).

-define(AO_TO_ARMS(X), X * 1_000_000_000_000).
-define(ARMS_TO_AO(X), X div 1_000_000_000_000).

compute(State, Req, Opts) ->
    maybe
        {ok, _} = hb_ao:resolve({as, <<"security@1.0">>, State}, Req, Opts),
        {ok, NewState} = maybe_mint(State, Req, Opts)
    else
        {error, Reason} ->
            {error, Reason}
    end.

%%% Internal functions

%% @doc Execute as many mint cycles as are necessary to progress the process.
maybe_mint(State, Req, Opts) ->
    case dev_mint_math:should_mint(State, Req, Opts) of
        true ->
            case dev_mint_math:mint(State, Req, Opts) of
              {ok, NewState} -> dev_mint_math:should_mint(NewState, Req, Opts);
              Else -> Else
            end;
        false -> {ok, State}
    end.

%%% Test Utilities

%% @doc Return a random resource ID in human-readable (43-character) format.
random_id() ->
    hb_util:human_id(crypto:strong_rand_bytes(32)).

%% @doc Generate a set of random resource weights.
random_resources(N) ->
    random_resources(N, 10_000_000).
random_resources(N, Max) ->
    lists:map(
        fun(_) ->
            {
                random_id(),
                #{
                    <<"weight">> => rand:uniform(?AO_TO_ARMS(Max)),
                    <<"oracle">> => random_id()
                }
            }
        end,
        lists:seq(1, N)
    ).

%%% Tests

single_resource_test() ->
    ResID = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Client = random_id(),
    State = #{
        <<"device">> => <<"mint@1.0">>,
        <<"client">> => Client,
        <<"resources">> => #{
            ResID => #{
                <<"weight">> => 1,
                <<"oracle">> => <<"oracle1">>
            }
        },
        <<"total-supply">> => ?AO_TO_ARMS(1_000_000_000),
        <<"period">> => 1000,
        <<"start-time">> => 1714339200
    },
