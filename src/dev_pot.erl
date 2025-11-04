-module(dev_pot).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

core_test() ->
    Addr1 = <<"addr1">>,
    Addr2 = <<"addr2">>,
    Chi0 = 1,
    S0 = #{
        <<"t">> => 0,
        <<"last-drip">> => 0,
        <<"chi">> => Chi0,
        <<"rate">> => 1.1,
        <<"balances">> => #{ Addr1 => pie_usr(1, Chi0) },
        <<"total">> => 1
    },
    S1 = modify(Addr1, 1, S0),
    S2 = modify(Addr2, 4, S1),
    ?event({t_0, balances(S2)}),
    S3 = drip(S2#{ <<"t">> => 1 }),
    ?event({t_1, balances(S3)}),
    S4 = drip(S3#{ <<"t">> => 2 }),
    ?event({t_2, balances(S4)}),
    S5 = modify(Addr1, 6, S4),
    S6 = drip(S5#{ <<"t">> => 3 }),
    ?event({t_3, balances(S6)}).

%%% Model.

pie_usr(Balance, Chi0) ->
    Balance / Chi0.

set_rate(Rate, S) ->
    (drip(S))#{ <<"rate">> => Rate }.

drip(S = #{ <<"last-drip">> := Last, <<"chi">> := Chi, <<"t">> := T, <<"rate">> := R }) ->
    NewChi = Chi * math:pow(R, T - Last),
    S#{ <<"chi">> => NewChi, <<"last-drip">> => T }.

%%% Helpers.

balance(Addr, #{ <<"chi">> := Chi, <<"balances">> := Balances }) ->
    maps:get(Addr, Balances, 0) * Chi.

balances(S) ->
    maps:map(
        fun(Addr, _) -> balance(Addr, S) end,
        maps:get(<<"balances">>, S, #{})
    ).

modify(Addr, Amount, S) ->
    ExistingBalance = balance(Addr, NewS = drip(S)),
    NewS#{
        <<"balances">> =>
            maps:put(
                Addr,
                pie_usr(ExistingBalance + Amount, maps:get(<<"chi">>, NewS)),
                maps:get(<<"balances">>, NewS, #{})
            ),
        <<"total">> => maps:get(<<"total">>, NewS, 0) + Amount
    }.