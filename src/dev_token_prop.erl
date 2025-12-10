-module(dev_token_prop).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

simulated_usage_test() ->
    hb_prop:state_machine(
        #{
            state => initial_state(),
            request_gen => requests(),
            properties => properties(),
            next => fun next/4,
            opts => #{}
        }
    ).

initial_state() ->
    #{
        <<"t">> => 0,
        <<"users">> => []
    }.

requests() ->
    [
        fun(S, Opts) -> request(Action, S, Opts) end
    ||
        Action <- [mint, transfer]
    ].
request(mint, S = #{ <<"t">> := T }, _Opts) ->
    #{
        <<"path">> => <<"mint">>,
        <<"timestamp">> => T + 1
    };
request(transfer, S, Opts) ->
    Users = hb_ao:keys(hb_maps:get(<<"users">>, S, #{}, Opts), Opts),
    {ok, UserOpts} =
        hb_opts:as(
            hb_prop:pick(Users),
            Opts
        ),
    schedule(
        S,
        hb_message:commit(
            #{
                <<"action">> => <<"Transfer">>,
                <<"quantity">> => hb_prop:int(1, 100),
                <<"recipient">> => hb_prop:pick(Users)
            },
            UserOpts
        ),
        Opts
    ).

schedule(S, Msg, Opts) ->
    {ok, #{ <<"slot">> := Slot }} =
        hb_ao:resolve(
            S,
            #{ <<"path">> => <<"schedule">>, <<"body">> => Msg },
            Opts
        ),
    #{ <<"path">> => <<"compute">>, <<"slot">> => Slot }.


properties() ->
    [
        fun verify_transfer_recipient/4,
        fun verify_transfer_supply/4
    ].

verify_transfer_recipient(Old, Req = #{ <<"path">> := <<"transfer">> }, New, Opts) ->
    Recipient = hb_maps:get(<<"recipient">>, Req, #{}, Opts),
    StartingBalance = hb_ao:get(<<"balances/", Recipient/binary>>, Old, #{}, Opts),
    EndingBalance = hb_ao:get(<<"balances/", Recipient/binary>>, New, #{}, Opts),
    StartingBalance =< EndingBalance.

verify_transfer_supply(Old, #{ <<"path">> := <<"compute">> }, New, Opts) ->
    StartingSupply = hb_ao:get(<<"total-supply">>, Old, #{}, Opts),
    EndingSupply = hb_ao:get(<<"total-supply">>, New, #{}, Opts),
    StartingSupply == EndingSupply.

%% @doc If the request was for the computation of a slot, we can advance the state.
%% If not, we discard the result and return the old state.
next(_Old, #{ <<"path">> := <<"compute">> }, New, _Opts) ->
    New;
next(Old, _, _New, _Opts) ->
    Old.