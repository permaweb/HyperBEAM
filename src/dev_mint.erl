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

compute(State, Req, Opts) ->
    maybe
        {ok, _} = hb_ao:resolve({as, <<"security@1.0">>, State}, Req, Opts),
        {ok, NewState} = maybe_mint(State, Req, Opts)
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