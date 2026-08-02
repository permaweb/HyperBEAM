%%% @doc A device for borrowing native Arweave value against a process token.
%%%
%%% Like `~arweave-swap@1.0', the AR leg never enters the process. The process
%%% only controls the token ledger it shares with its execution device. A loan
%%% therefore escrows token collateral, observes ordinary layer-1 transfers for
%%% funding and repayment, and releases or transfers the collateral according to
%%% the observed payments and block heights.
%%%
%%% This device is intended for `~arweave-scheduler@1.0' in `all' mode. Funding
%%% and repayment are normal AR transfers between wallets, so the process must
%%% see data-free transactions that are not addressed to it.
-module(dev_arweave_loan).
-implements(<<"arweave-loan@1.0">>).
-export([info/0, compute/3, set/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(BALANCES, <<"balances">>).
-define(SEEN_LOANS, <<"seen-loans">>).
-define(DEFAULT_COLLATERAL_QUANTITY, 1).

%% @doc Every scheduled slot routes to `compute'. Do not exclude `set' or
%% unknown keys: in all-mode a stranger's path tag is data, and falling through
%% to `message@1.0' could wedge or mutate the process.
info() ->
    #{ default => fun router/4 }.

router(_Key, Base, Assignment, Opts) ->
    compute(Base, Assignment, Opts).

%% @doc Permit process machinery to restore the device key, and route every
%% other set-like scheduled message through the loan state machine.
set(Base, Req, Opts) ->
    case hb_maps:keys(Req, Opts) -- [<<"path">>, <<"set-mode">>] of
        [<<"device">>] ->
            {ok,
                Base#{
                    <<"device">> =>
                        hb_maps:get(<<"device">>, Req, undefined, Opts)
                }
            };
        _ -> compute(Base, Req, Opts)
    end.

%% @doc Apply one scheduled assignment.
compute(Base, Assignment, Opts) ->
    Height = hb_util:int(field(<<"block-height">>, Assignment, 0, Opts)),
    ProcID = field(<<"process">>, Assignment, <<>>, Opts),
    Body = field(<<"body">>, Assignment, #{}, Opts),
    Advanced = advance(Base, Height, Opts),
    case tx_field(Body, <<"target">>, <<>>, Opts) of
        ProcID -> {ok, control(Advanced, Body, Height, Opts)};
        Target -> {ok, payment(Advanced, Body, Target, Height, Opts)}
    end.

%%% Loan lifecycle

control(Base, Body, Height, Opts) ->
    case field(<<"action">>, Body, <<>>, Opts) of
        <<"make-loan">> -> make_loan(Base, Body, Height, Opts);
        <<"cancel-loan">> -> cancel_loan(Base, Body, Opts);
        <<"claim-default">> -> claim_default(Base, Body, Height, Opts);
        _ -> Base
    end.

%% @doc Open a loan request and escrow the collateral immediately.
make_loan(Base, Body, Height, Opts) ->
    maybe
        {ok, Borrower} ?= signer(Body, Opts),
        {ok, Principal} ?= amount(<<"principal">>, Body, Opts),
        {ok, Repayment} ?= amount(<<"repayment">>, Body, Opts),
        {ok, FundingWindow} ?= amount(<<"funding-deadline">>, Body, Opts),
        {ok, MaturityWindow} ?= amount(<<"maturity">>, Body, Opts),
        {ok, Quantity} ?=
            amount(
                <<"collateral-quantity">>,
                Body,
                ?DEFAULT_COLLATERAL_QUANTITY,
                Opts
            ),
        Recipient = field(<<"recipient">>, Body, Borrower, Opts),
        BorrowerSet = hb_util:bool(field(<<"borrower-set">>, Body, true, Opts)),
        true ?= Principal >= 1,
        true ?= Repayment >= 1,
        true ?= Repayment >= Principal,
        true ?= FundingWindow >= 1,
        true ?= MaturityWindow >= 1,
        true ?= Quantity >= 1,
        true ?= is_address(Recipient),
        false ?= has_live_sale(Base, Opts),
        [] ?= live_loans(Base, Opts),
        true ?= balance(Base, Borrower, Opts) >= Quantity,
        LoanID = hb_util:human_id(hb_message:id(Body, signed, Opts)),
        false ?= loan_seen(Base, LoanID, Opts),
        Loan =
            #{
                <<"loan-id">> => LoanID,
                <<"status">> => <<"open">>,
                <<"borrower">> => Borrower,
                <<"principal">> => Principal,
                <<"repayment">> => Repayment,
                <<"recipient">> => Recipient,
                <<"collateral-quantity">> => Quantity,
                <<"borrower-set">> => BorrowerSet,
                <<"created-at">> => Height,
                <<"funding-deadline">> => Height + FundingWindow,
                <<"maturity-duration">> => MaturityWindow
            },
        ?event(
            {loan_opened,
                {loan, LoanID},
                {borrower, Borrower},
                {principal, Principal},
                {repayment, Repayment}
            }
        ),
        deadlines(put_loan(debit(Base, Borrower, Quantity, Opts), Loan, Opts), Opts)
    else
        _ -> Base
    end.

%% @doc Cancel an unfunded loan and return the collateral to the borrower.
cancel_loan(Base, Body, Opts) ->
    maybe
        {ok, Signer} ?= signer(Body, Opts),
        {ok, Loan} ?= find_loan(Base, Body, Opts),
        #{
            <<"status">> := <<"open">>,
            <<"borrower">> := Signer,
            <<"collateral-quantity">> := Quantity
        } ?= Loan,
        ?event({loan_cancelled, {loan, maps:get(<<"loan-id">>, Loan)}}),
        deadlines(drop_loan(credit(Base, Signer, Quantity, Opts), Loan, Opts), Opts)
    else
        _ -> Base
    end.

%% @doc After maturity, move the collateral to the lender. Anyone may trigger
%% the deterministic claim, but the message must still have a single signer.
claim_default(Base, Body, Height, Opts) ->
    maybe
        {ok, _Caller} ?= signer(Body, Opts),
        {ok, Loan} ?= find_loan(Base, Body, Opts),
        #{
            <<"status">> := <<"active">>,
            <<"lender">> := Lender,
            <<"maturity">> := Maturity,
            <<"collateral-quantity">> := Quantity
        } ?= Loan,
        true ?= Height > Maturity,
        ?event({loan_default_claimed, {loan, maps:get(<<"loan-id">>, Loan)}}),
        deadlines(drop_loan(credit(Base, Lender, Quantity, Opts), Loan, Opts), Opts)
    else
        _ -> Base
    end.

%% @doc Observe ordinary AR transfers for funding and repayment.
payment(Base, Body, Target, Height, Opts) ->
    case find_loan(Base, Body, Opts) of
        {ok, Loan = #{ <<"status">> := <<"open">> }} ->
            fund(Base, Body, Target, Height, Loan, Opts);
        {ok, Loan = #{ <<"status">> := <<"active">> }} ->
            repay(Base, Body, Target, Height, Loan, Opts);
        _ -> Base
    end.

fund(Base, Body, Target, Height, Loan, Opts) ->
    maybe
        {ok, Lender} ?= signer(Body, Opts),
        #{
            <<"borrower">> := Borrower,
            <<"principal">> := Principal,
            <<"recipient">> := Recipient,
            <<"funding-deadline">> := Deadline,
            <<"maturity-duration">> := MaturityDuration
        } ?= Loan,
        true ?= Target =:= Recipient,
        {ok, Paid} ?= hb_util:safe_int(tx_field(Body, <<"quantity">>, 0, Opts)),
        true ?= Paid >= Principal,
        true ?= Height =< Deadline,
        RepaymentRecipient = field(<<"repayment-recipient">>, Body, Lender, Opts),
        true ?= is_address(RepaymentRecipient),
        false ?= Lender =:= Borrower,
        false ?= Lender =:= Recipient,
        FundingTX = hb_util:human_id(hb_message:id(Body, signed, Opts)),
        Active =
            Loan#{
                <<"status">> => <<"active">>,
                <<"lender">> => Lender,
                <<"repayment-recipient">> => RepaymentRecipient,
                <<"maturity">> => Height + MaturityDuration,
                <<"funding-tx">> => FundingTX
            },
        ?event({loan_funded, {loan, maps:get(<<"loan-id">>, Loan)}, {lender, Lender}}),
        deadlines(put_loan(Base, Active, Opts), Opts)
    else
        _ -> Base
    end.

repay(Base, Body, Target, Height, Loan, Opts) ->
    maybe
        {ok, _Payer} ?= signer(Body, Opts),
        #{
            <<"borrower">> := Borrower,
            <<"repayment">> := Repayment,
            <<"repayment-recipient">> := Recipient,
            <<"maturity">> := Maturity,
            <<"collateral-quantity">> := Quantity
        } ?= Loan,
        true ?= Target =:= Recipient,
        {ok, Paid} ?= hb_util:safe_int(tx_field(Body, <<"quantity">>, 0, Opts)),
        true ?= Paid >= Repayment,
        true ?= Height =< Maturity,
        ?event({loan_repaid, {loan, maps:get(<<"loan-id">>, Loan)}}),
        deadlines(drop_loan(credit(Base, Borrower, Quantity, Opts), Loan, Opts), Opts)
    else
        _ -> Base
    end.

%%% Clock

advance(Base, Height, Opts) ->
    Dated = Base#{ <<"loan-height">> => Height },
    case state(<<"next-loan-deadline">>, Base, 0, Opts) of
        Next when Next > 0, Height >= Next ->
            deadlines(
                lists:foldl(
                    fun(Loan, Acc) -> expire(Acc, Loan, Height, Opts) end,
                    Dated,
                    loans(Base, Opts)
                ),
                Opts
            );
        _ -> Dated
    end.

expire(
        Base,
        Loan =
            #{
                <<"status">> := <<"open">>,
                <<"borrower">> := Borrower,
                <<"collateral-quantity">> := Quantity,
                <<"funding-deadline">> := Deadline
            },
        Height,
        Opts) when Height > Deadline ->
    ?event({loan_funding_expired, {loan, maps:get(<<"loan-id">>, Loan)}}),
    drop_loan(credit(Base, Borrower, Quantity, Opts), Loan, Opts);
expire(Base, _Loan, _Height, _Opts) ->
    Base.

deadlines(Base, Opts) ->
    FundingDeadlines =
        [
            Deadline + 1
        ||
            #{ <<"status">> := <<"open">>, <<"funding-deadline">> := Deadline }
                <- loans(Base, Opts)
        ],
    Base#{
        <<"next-loan-deadline">> =>
            case FundingDeadlines of
                [] -> 0;
                _ -> lists:min(FundingDeadlines)
            end
    }.

%%% State helpers

state(Key, Base, Default, Opts) ->
    hb_ao:get(Key, {as, <<"message@1.0">>, Base}, Default, Opts).

field(Key, Msg, Default, Opts) ->
    hb_maps:get(Key, Msg, Default, Opts).

loans(Base, Opts) ->
    [
        Loan
    ||
        Loan = #{ <<"loan-id">> := _ } <-
            hb_maps:values(
                hb_cache:ensure_all_loaded(loan_book(Base, Opts), Opts),
                Opts
            )
    ].

loan_book(Base, Opts) -> state(<<"loans">>, Base, #{}, Opts).

live_loans(Base, Opts) ->
    [Loan || Loan = #{ <<"status">> := Status } <- loans(Base, Opts), is_live(Status)].

is_live(<<"open">>) -> true;
is_live(<<"active">>) -> true;
is_live(_) -> false.

find_loan(Base, Body, Opts) ->
    case
        hb_cache:ensure_all_loaded(
            hb_maps:get(
                field(<<"loan-id">>, Body, <<>>, Opts),
                loan_book(Base, Opts),
                not_found,
                Opts
            ),
            Opts
        )
    of
        Loan = #{ <<"loan-id">> := _ } -> {ok, Loan};
        _ -> not_found
    end.

seen_loans(Base, Opts) -> state(?SEEN_LOANS, Base, #{}, Opts).

loan_seen(Base, LoanID, Opts) ->
    hb_maps:get(LoanID, seen_loans(Base, Opts), false, Opts) =:= true
        orelse
            hb_maps:get(LoanID, loan_book(Base, Opts), not_found, Opts)
                =/= not_found.

remember_loan(Base, LoanID, Opts) ->
    Base#{
        ?SEEN_LOANS =>
            hb_maps:put(
                LoanID,
                true,
                seen_loans(Base, Opts),
                Opts
            )
    }.

put_loan(Base, Loan = #{ <<"loan-id">> := LoanID }, Opts) ->
    remember_loan(
        Base#{
            <<"loans">> =>
                hb_maps:put(
                    LoanID,
                    Loan,
                    loan_book(Base, Opts),
                    Opts
                )
        },
        LoanID,
        Opts
    ).

drop_loan(Base, #{ <<"loan-id">> := LoanID }, Opts) ->
    remember_loan(
        Base#{
            <<"loans">> => hb_maps:without([LoanID], loan_book(Base, Opts), Opts)
        },
        LoanID,
        Opts
    ).

has_live_sale(Base, Opts) ->
    lists:any(
        fun
            (#{ <<"status">> := <<"open">> }) -> true;
            (#{ <<"status">> := <<"reserved">> }) -> true;
            (_) -> false
        end,
        orders(Base, Opts)
    ).

orders(Base, Opts) ->
    [
        Order
    ||
        Order = #{ <<"order-id">> := _ } <-
            hb_maps:values(
                hb_cache:ensure_all_loaded(state(<<"orders">>, Base, #{}, Opts), Opts),
                Opts
            )
    ].

balance(Base, Address, Opts) ->
    hb_util:int(state([?BALANCES, Address], Base, 0, Opts)).

credit(Base, _Address, 0, _Opts) -> Base;
credit(Base, Address, Amount, Opts) ->
    Base#{
        ?BALANCES =>
            hb_maps:put(
                Address,
                balance(Base, Address, Opts) + Amount,
                state(?BALANCES, Base, #{}, Opts),
                Opts
            )
    }.

debit(Base, Address, Amount, Opts) -> credit(Base, Address, -Amount, Opts).

amount(Key, Body, Opts) -> amount(Key, Body, 0, Opts).

amount(Key, Body, Default, Opts) ->
    hb_util:safe_int(field(Key, Body, Default, Opts)).

tx_field(Body, Field, Default, Opts) ->
    case hb_message:commitment(#{ <<"commitment-device">> => <<"tx@1.0">> }, Body, Opts) of
        {ok, _ID, Commitment} ->
            hb_maps:get(<<"field-", Field/binary>>, Commitment, Default, Opts);
        _ ->
            Default
    end.

signer(Body, Opts) ->
    case hb_message:signers(Body, Opts) of
        [Signer] -> {ok, hb_util:human_id(Signer)};
        _ -> not_found
    end.

is_address(Address) ->
    ?IS_ID(Address) andalso byte_size(Address) =:= 43.

%%% Tests

test_opts() -> #{ <<"priv-wallet">> => ar_wallet:new() }.

process_id() -> hb_util:human_id(<<1:256>>).

party() ->
    Wallet = ar_wallet:new(),
    {Wallet, hb_util:human_id(ar_wallet:to_address(Wallet))}.

base(Balances) -> #{ ?BALANCES => Balances }.

tx(Wallet, Fields) ->
    hb_message:commit(
        Fields,
        #{ <<"priv-wallet">> => Wallet },
        #{ <<"commitment-device">> => <<"tx@1.0">> }
    ).

apply_tx(Base, Body, Height, Opts) ->
    {ok, New} =
        compute(
            Base,
            #{
                <<"process">> => process_id(),
                <<"slot">> => 1,
                <<"block-height">> => Height,
                <<"body">> => Body
            },
            Opts
        ),
    New.

tick(Base, Height, Opts) ->
    apply_tx(Base, #{ <<"target">> => <<"someone-else">> }, Height, Opts).

make_loan(Wallet, Principal, Repayment, FundingDeadline, Maturity) ->
    tx(
        Wallet,
        #{
            <<"target">> => process_id(),
            <<"action">> => <<"make-loan">>,
            <<"principal">> => hb_util:bin(Principal),
            <<"repayment">> => hb_util:bin(Repayment),
            <<"funding-deadline">> => hb_util:bin(FundingDeadline),
            <<"maturity">> => hb_util:bin(Maturity)
        }
    ).

loan_action(Wallet, Action, LoanID) ->
    tx(
        Wallet,
        #{
            <<"target">> => process_id(),
            <<"action">> => Action,
            <<"loan-id">> => LoanID
        }
    ).

pay(Wallet, To, Winston, LoanID) ->
    tx(
        Wallet,
        #{
            <<"target">> => To,
            <<"quantity">> => hb_util:bin(Winston),
            <<"loan-id">> => LoanID
        }
    ).

tag_only_tx(Wallet, Tags) ->
    Signed = ar_tx:sign(#tx{ format = 2, reward = 1, tags = Tags }, Wallet),
    hb_message:convert(Signed, <<"structured@1.0">>, <<"tx@1.0">>, #{}).

tag_only_payment(Wallet, To, Winston, LoanID) ->
    tag_only_tx(
        Wallet,
        [
            {<<"target">>, To},
            {<<"quantity">>, hb_util:bin(Winston)},
            {<<"loan-id">>, LoanID}
        ]
    ).

only_loan(Base, Opts) ->
    [Loan] = loans(Base, Opts),
    Loan.

loan_id(Base, Opts) -> maps:get(<<"loan-id">>, only_loan(Base, Opts)).

make_loan_escrows_test() ->
    Opts = test_opts(),
    {Borrower, BorrowerAddr} = party(),
    Opened = apply_tx(base(#{ BorrowerAddr => 1 }), make_loan(Borrower, 100, 120, 10, 20), 50, Opts),
    Loan = only_loan(Opened, Opts),
    ?assertEqual(0, balance(Opened, BorrowerAddr, Opts)),
    ?assertEqual(<<"open">>, maps:get(<<"status">>, Loan)),
    ?assertEqual(60, maps:get(<<"funding-deadline">>, Loan)),
    ?assertEqual(20, maps:get(<<"maturity-duration">>, Loan)).

sale_conflict_blocks_loan_test() ->
    Opts = test_opts(),
    {Borrower, BorrowerAddr} = party(),
    Base =
        (base(#{ BorrowerAddr => 1 }))#{
            <<"orders">> =>
                #{
                    <<"order">> =>
                        #{
                            <<"order-id">> => <<"order">>,
                            <<"status">> => <<"open">>
                        }
                }
        },
    Result = apply_tx(Base, make_loan(Borrower, 100, 120, 10, 20), 50, Opts),
    ?assertEqual([], loans(Result, Opts)),
    ?assertEqual(1, balance(Result, BorrowerAddr, Opts)).

non_ar_recipient_is_rejected_test() ->
    Opts = test_opts(),
    {Borrower, BorrowerAddr} = party(),
    BadRecipient = binary:copy(<<"a">>, 42),
    Open =
        tx(
            Borrower,
            #{
                <<"target">> => process_id(),
                <<"action">> => <<"make-loan">>,
                <<"principal">> => <<"100">>,
                <<"repayment">> => <<"120">>,
                <<"funding-deadline">> => <<"10">>,
                <<"maturity">> => <<"20">>,
                <<"recipient">> => BadRecipient
            }
        ),
    Result = apply_tx(base(#{ BorrowerAddr => 1 }), Open, 50, Opts),
    ?assertEqual([], loans(Result, Opts)),
    ?assertEqual(1, balance(Result, BorrowerAddr, Opts)).

cancel_open_loan_test() ->
    Opts = test_opts(),
    {Borrower, BorrowerAddr} = party(),
    Opened = apply_tx(base(#{ BorrowerAddr => 1 }), make_loan(Borrower, 100, 120, 10, 20), 50, Opts),
    Cancelled = apply_tx(Opened, loan_action(Borrower, <<"cancel-loan">>, loan_id(Opened, Opts)), 51, Opts),
    ?assertEqual([], loans(Cancelled, Opts)),
    ?assertEqual(1, balance(Cancelled, BorrowerAddr, Opts)).

funding_wrong_target_is_ignored_test() ->
    Opts = test_opts(),
    {Borrower, BorrowerAddr} = party(),
    {Lender, _LenderAddr} = party(),
    {_, WrongTarget} = party(),
    Opened = apply_tx(base(#{ BorrowerAddr => 1 }), make_loan(Borrower, 100, 120, 10, 20), 50, Opts),
    Result = apply_tx(Opened, pay(Lender, WrongTarget, 100, loan_id(Opened, Opts)), 55, Opts),
    ?assertEqual(<<"open">>, maps:get(<<"status">>, only_loan(Result, Opts))).

tag_only_payment_fields_are_ignored_test() ->
    Opts = test_opts(),
    {Borrower, BorrowerAddr} = party(),
    {Lender, _LenderAddr} = party(),
    Opened = apply_tx(base(#{ BorrowerAddr => 1 }), make_loan(Borrower, 100, 120, 10, 20), 50, Opts),
    LoanID = loan_id(Opened, Opts),
    Tagged = tag_only_payment(Lender, BorrowerAddr, 100, LoanID),
    ?assertEqual(BorrowerAddr, hb_ao:get(<<"target">>, Tagged, not_found, Opts)),
    ?assertEqual(<<>>, tx_field(Tagged, <<"target">>, <<>>, Opts)),
    Result = apply_tx(Opened, Tagged, 55, Opts),
    ?assertEqual(<<"open">>, maps:get(<<"status">>, only_loan(Result, Opts))).

funding_activates_loan_test() ->
    Opts = test_opts(),
    {Borrower, BorrowerAddr} = party(),
    {Lender, LenderAddr} = party(),
    Opened = apply_tx(base(#{ BorrowerAddr => 1 }), make_loan(Borrower, 100, 120, 10, 20), 50, Opts),
    Funded = apply_tx(Opened, pay(Lender, BorrowerAddr, 100, loan_id(Opened, Opts)), 55, Opts),
    Loan = only_loan(Funded, Opts),
    ?assertEqual(<<"active">>, maps:get(<<"status">>, Loan)),
    ?assertEqual(LenderAddr, maps:get(<<"lender">>, Loan)),
    ?assertEqual(LenderAddr, maps:get(<<"repayment-recipient">>, Loan)),
    ?assertEqual(75, maps:get(<<"maturity">>, Loan)).

repayment_releases_collateral_test() ->
    Opts = test_opts(),
    {Borrower, BorrowerAddr} = party(),
    {Lender, LenderAddr} = party(),
    Opened = apply_tx(base(#{ BorrowerAddr => 1 }), make_loan(Borrower, 100, 120, 10, 20), 50, Opts),
    Funded = apply_tx(Opened, pay(Lender, BorrowerAddr, 100, loan_id(Opened, Opts)), 55, Opts),
    Repaid = apply_tx(Funded, pay(Borrower, LenderAddr, 120, loan_id(Funded, Opts)), 70, Opts),
    ?assertEqual([], loans(Repaid, Opts)),
    ?assertEqual(1, balance(Repaid, BorrowerAddr, Opts)).

claim_default_transfers_collateral_test() ->
    Opts = test_opts(),
    {Borrower, BorrowerAddr} = party(),
    {Lender, LenderAddr} = party(),
    {Caller, _CallerAddr} = party(),
    Opened = apply_tx(base(#{ BorrowerAddr => 1 }), make_loan(Borrower, 100, 120, 10, 20), 50, Opts),
    Funded = apply_tx(Opened, pay(Lender, BorrowerAddr, 100, loan_id(Opened, Opts)), 55, Opts),
    Claimed = apply_tx(Funded, loan_action(Caller, <<"claim-default">>, loan_id(Funded, Opts)), 76, Opts),
    ?assertEqual([], loans(Claimed, Opts)),
    ?assertEqual(0, balance(Claimed, BorrowerAddr, Opts)),
    ?assertEqual(1, balance(Claimed, LenderAddr, Opts)).

funding_expiry_returns_collateral_test() ->
    Opts = test_opts(),
    {Borrower, BorrowerAddr} = party(),
    Opened = apply_tx(base(#{ BorrowerAddr => 1 }), make_loan(Borrower, 100, 120, 10, 20), 50, Opts),
    Expired = tick(Opened, 61, Opts),
    ?assertEqual([], loans(Expired, Opts)),
    ?assertEqual(1, balance(Expired, BorrowerAddr, Opts)).
