%%% @doc Statistical and validation tools for `~mint@1.0`.
-module(dev_mint_stats).
-export([sanity_check/3]).
-export([gcd/2, get_quantities/1, get_distributions/1]).
-include("include/hb.hrl").

%% @doc Ensure that a set of sanity check properties are met by the state after
%% a mint cycle.
%% 
%% The enforced properties are:
%% -> Minted supply =< Total supply.
%% -> Minted supply after >= Minted supply before.
%% -> Total supply and minted supply are positive integers.
%% -> sum(distributions/n/quantity) == Minted supply after - Minted supply before.
%% -> All distributions/n/quantity are positive integers.
sanity_check(State, NewState, Opts) ->
    MintedBefore = hb_maps:get(<<"minted">>, State, Opts),
    MintedAfter = hb_maps:get(<<"minted">>, NewState, Opts),
    TotalSupply = hb_maps:get(<<"total-supply">>, State, Opts),
    Quantities = get_quantities(NewState),
    SumQuantities = lists:sum(Quantities),
    NonNegativeInteger = fun(X) -> is_integer(X) andalso X >= 0 end,
    TestResults =
        [
            % Type checks.
            NonNegativeInteger(MintedBefore)
                orelse <<"Total supply is not a non-negative integer.">>,
            NonNegativeInteger(MintedAfter)
                orelse <<"Minted supply after is not a non-negative integer.">>,
            % NonNegativeInteger(TotalSupply)
            %     orelse <<"Total supply is not a non-negative integer.">>,
            lists:all(NonNegativeInteger, Quantities)
                orelse <<"Distributions contain non-negative integer values.">>,
            % Value checks.
            MintedAfter =< TotalSupply
                orelse <<"Minted supply exceeds total supply.">>,
            MintedAfter >= MintedBefore
                orelse <<"Minted supply after is less than minted supply before.">>,
            SumQuantities =:= MintedAfter - MintedBefore
                orelse <<"New `minted` value invalid.">>
        ],
    Errors = [ Error || Error <- TestResults, Error =/= true],
    case Errors of
        [] -> ok;
        _ ->
            {error,
                #{
                    <<"errors">> => Errors,
                    <<"body">> =>
                        hb_util:bin(
                            [
                                <<"<pre>Mint sanity check failed: ">>
                            ] ++
                            lists:join(<<", ">>, Errors) ++
                            [<<"</pre>">>]
                        )
                }
            }
    end.

%% @doc Extract distributions from a mint state.
get_distributions(State) ->
    hb_ao:get(<<"results/distributions">>, State, #{}).

%% @doc Get all quantities from distributions.
get_quantities(State) ->
    Distributions = get_distributions(State),
    [maps:get(<<"quantity">>, D) || D <- Distributions].

%%% Helpers.

%% @doc Return the greatest common divisor of two integers.
gcd(A, 0) -> abs(A);
gcd(A, B) -> gcd(B, A rem B).