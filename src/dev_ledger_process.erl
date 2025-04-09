%%% @doc A simple ledger implementing the p4@1.0 interface, using a process@1.0
%%% as its source-of-truth. Ignores requests to credit the ledger, instead 
%%% allowing the process to manage balances itself.
-module(dev_ledger_process).
-export([balance/3, debit/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Get the balance of a user.
balance(LedgerMsg, RawReq, NodeMsg) ->
    Target =
        case hb_ao:get(<<"request">>, RawReq, NodeMsg#{ hashpath => ignore }) of
            not_found -> hd(hb_message:signers(RawReq));
            Req -> hd(hb_message:signers(Req))
        end,
    Results = 
        ledger_call(
            LedgerMsg,
            #{ <<"action">> => <<"balance">>, <<"address">> => Target },
            NodeMsg
        ),
    try
        {
            ok,
            binary_to_integer(hb_ao:get(<<"data">>, Results, #{}))
        }
    catch error:badarg ->
        {error, #{
            <<"status">> => 500,
            <<"error">> => <<"bad_ledger">>,
            <<"results">> => Results,
            <<"body">> => <<"Bad ledger response on balance request.">>
        }}
    end.

debit(State, Req, NodeMsg) ->
    case hb_ao:get(<<"type">>, Req, NodeMsg#{ hashpath => ignore }) of
        <<"pre">> ->
            % Get the balance and check if it is enough to cover the debit. Do
            % not proceed with the request yet.
            BalanceStr = balance(State, Req, NodeMsg),
            Amount = hb_ao:get(<<"amount">>, Req, 0, NodeMsg),
            Balance = binary_to_integer(BalanceStr),
            {ok, (Balance - Amount) >= 0};
        <<"post">> ->
            InnerReq = hb_ao:get(<<"request">>, Req, NodeMsg),
            Target = hd(hb_message:signers(InnerReq)),
            Amount = hb_ao:get(<<"amount">>, Req, NodeMsg),
            ?event({debit_call, {req, #{
                <<"action">> => <<"debit">>,
                <<"address">> => Target,
                <<"amount">> => Amount
            }}}),
            Result =
                ledger_call(
                    State,
                    #{
                        <<"action">> => <<"debit">>,
                        <<"address">> => Target,
                        <<"amount">> => Amount
                    },
                    NodeMsg
                ),
            ?event({ledger_call, {resp, Result}}),
            % Always return true
            {ok, true}
    end.

%% @doc Get the result of a message on the ledger process.
ledger_call(LedgerMsg, Req, NodeMsg) ->
    Process = hb_ao:get(<<"process">>, LedgerMsg, NodeMsg),
    ProcessID = hb_ao:get(<<"id">>, Process, NodeMsg),
    SignedReq = hb_message:commit(Req#{ <<"target">> => ProcessID }, NodeMsg),
    ScheduleMsg = hb_message:commit(#{
        <<"path">> => <<"schedule">>,
        <<"method">> => <<"POST">>,
        <<"body">> => SignedReq
    }, NodeMsg),
    {ok, #{ <<"slot">> := Slot }} = hb_ao:resolve(Process, ScheduleMsg, #{}),
    ?event({ledger_call, {slot, Slot}, {process, Process}, {schedule_msg, ScheduleMsg}}),
    {ok, Resp} =
        hb_ao:resolve_many(
            [
                Process,
                #{ <<"path">> => <<"compute">>, <<"slot">> => Slot },
                #{ <<"path">> => <<"results">> }
            ],
            #{ spawn_worker => true }
        ),
    ?event({ledger_call, {resp, Resp}}),
    Resp.

%%% Tests

setup() ->
    Wallet = ar_wallet:new(),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    TokenAddress = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Process = dev_process:test_aos_process(),
    hb_cache:write(Process, #{}),
    {ok, LedgerScript} = file:read_file("aos/ledger.lua"),
    dev_process:schedule_aos_call(Process, LedgerScript),
    dev_process:schedule_aos_call(
        Process,
        <<"TokenAddress = \"", TokenAddress/binary, "\"">>
    ),
    ProcessorMsg =
        #{
            <<"device">> => <<"p4@1.0">>,
            <<"ledger">> =>
                #{
                    <<"device">> => <<"ledger-process@1.0">>,
                    <<"process">> => Process
                },
            <<"pricing">> =>
                #{
                    <<"device">> => #{
                        % Statically price every request at 10 for testing.
                        <<"estimate">> => fun(_) -> {ok, 10} end
                    }
                }
        },
    {
        Address,
        Wallet,
        Process,
        TokenAddress,
        #{
            operator => Address,
            simple_pay_price => 10,
            priv_wallet => Wallet,
            preprocessor => ProcessorMsg,
            postprocessor => ProcessorMsg
        }
    }.

topup(Address, Process, _TokenAddress, Amount, NodeMsg) ->
    ledger_call(
        #{
            <<"process">> => Process
        },
        #{
            <<"action">> => <<"credit">>,
            <<"sender">> => Address,
            <<"amount">> => integer_to_binary(Amount)
        },
        NodeMsg
    ).

get_balance(Node, Wallet) ->
    hb_http:get(
        Node,
        hb_message:commit(
            #{ <<"path">> => <<"/~p4@1.0/balance">> },
            Wallet
        ),
        #{}
    ).

balance_test() ->
    {_Address, Wallet, _Process, _TokenAddress, Opts} = setup(),
    Node = hb_http_server:start_node(Opts),
    Res = get_balance(Node, Wallet),
    ?event({get_balance, Res}),
    ?assertMatch({ok, #{ <<"body">> := 0 }}, Res).

debit_test_() ->
    {timeout, 30, fun() ->
        {Address, Wallet, Process, TokenAddress, Opts} = setup(),
        Node = hb_http_server:start_node(Opts#{
            p4_non_chargable_routes =>
                [
                    #{ <<"template">> => <<"/~p4@1.0/balance">> }
                ]
        }),
        % Top up the wallet with 100 tokens.
        topup(Address, Process, TokenAddress, 100, Opts),
        Res = get_balance(Node, Wallet),
        ?event({get_balance, Res}),
        ?assertMatch({ok, #{ <<"body">> := 100 }}, Res),
        % Do an action that will be charged
        hb_http:get(
            Node,
            hb_message:commit(
                #{ <<"path">> => <<"/k&v=1/v">> },
                Wallet
            ),
            #{}
        ),
        Res2 = get_balance(Node, Wallet),
        ?event({get_balance2, Res2}),
        ?assertMatch({ok, #{ <<"body">> := 70 }}, Res2)
    end}.
