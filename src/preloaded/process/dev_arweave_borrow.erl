%%% @doc A device for borrowing native Arweave value against a process token.
%%%
%%% Like `~arweave-swap@1.0', the AR leg never enters the process. The process
%%% only controls the token ledger it shares with its execution device. A position
%%% therefore escrows token collateral, observes ordinary layer-1 transfers for
%%% funding and repayment, and releases or transfers the collateral according to
%%% the observed payments and block heights.
%%%
%%% This device runs before `~token@1.0' in a native `~stack@1.0'. Control
%%% transactions target the token process. Funding and repayment are normal AR
%%% transfers between wallets with one `Assign-To' tag naming the process.
-module(dev_arweave_borrow).
-implements(<<"arweave-borrow@1.0">>).
-export([init/3, compute/3, normalize/3, snapshot/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(BALANCES, <<"balances">>).
-define(SEEN_POSITIONS, <<"seen-positions">>).
-define(SEEN_PAYMENTS, <<"seen-position-payments">>).
-define(DEFAULT_COLLATERAL_QUANTITY, 1).
-define(DEFAULT_RESERVATION_DURATION, 20).

%% @doc Necessary hooks for compliance with the `execution-device' standard.
init(Base, _Req, _Opts) -> {ok, Base}.
normalize(Base, _Req, _Opts) -> {ok, Base}.
snapshot(Base, _Req, _Opts) -> {ok, Base}.

%% @doc Apply one scheduled assignment.
compute(Base, Assignment, Opts) ->
    Height = hb_util:int(field(<<"block-height">>, Assignment, 0, Opts)),
    ProcID = field(<<"process">>, Assignment, <<>>, Opts),
    Body = field(<<"body">>, Assignment, #{}, Opts),
    Advanced = advance(Base, Height, Opts),
    case tx_field(Body, <<"target">>, <<>>, Opts) of
        ProcID -> control(Advanced, Body, Height, Opts);
        Target -> payment(Advanced, Body, Target, Height, Opts)
    end.

%%% Position lifecycle

control(Base, Body, Height, Opts) ->
    case action(Body, Opts) of
        <<"open-position">> -> {skip, open_position(Base, Body, Height, Opts)};
        <<"cancel-position">> -> {skip, cancel_position(Base, Body, Opts)};
        <<"reserve-position">> ->
            {skip,
                case find_position(Base, Body, Opts) of
                    {ok, Position} ->
                        reserve_position(Base, Body, Height, Position, Opts);
                    _ -> Base
                end};
        <<"claim-collateral">> -> {skip, claim_collateral(Base, Body, Height, Opts)};
        <<"set">> ->
            case live_positions(Base, Opts) of
                [] -> {ok, Base};
                _ -> {skip, Base}
            end;
        _ -> {ok, Base}
    end.

%% @doc Open a position request and escrow the collateral immediately.
open_position(Base, Body, Height, Opts) ->
    maybe
        {ok, PositionOwner} ?= signer(Body, Opts),
        {ok, Principal} ?= amount(<<"principal">>, Body, Opts),
        {ok, Repayment} ?= amount(<<"repayment">>, Body, Opts),
        {ok, FundingWindow} ?= amount(<<"funding-deadline">>, Body, Opts),
        {ok, MaturityWindow} ?= amount(<<"maturity">>, Body, Opts),
        {ok, ReservationDuration} ?=
            amount(
                <<"reservation-duration">>,
                Body,
                ?DEFAULT_RESERVATION_DURATION,
                Opts
            ),
        {ok, MinimumFee} ?= amount(<<"minimum-fee">>, Body, 0, Opts),
        {ok, Quantity} ?=
            amount(
                <<"collateral-quantity">>,
                Body,
                ?DEFAULT_COLLATERAL_QUANTITY,
                Opts
            ),
        Recipient = field(<<"recipient">>, Body, PositionOwner, Opts),
        true ?= Principal >= 1,
        true ?= Repayment >= 1,
        true ?= Repayment >= Principal,
        true ?= FundingWindow >= 1,
        true ?= MaturityWindow >= 1,
        true ?= ReservationDuration >= 1,
        true ?= MinimumFee >= 0,
        true ?= Quantity >= 1,
        true ?= is_address(Recipient),
        true ?= supported_stack(Base, Opts),
        false ?= has_live_sale(Base, Opts),
        [] ?= live_positions(Base, Opts),
        true ?= balance(Base, PositionOwner, Opts) >= Quantity,
        PositionID = hb_util:human_id(hb_message:id(Body, signed, Opts)),
        false ?= position_seen(Base, PositionID, Opts),
        Position =
            #{
                <<"position-id">> => PositionID,
                <<"status">> => <<"open">>,
                <<"position-owner">> => PositionOwner,
                <<"principal">> => Principal,
                <<"repayment">> => Repayment,
                <<"recipient">> => Recipient,
                <<"collateral-quantity">> => Quantity,
                <<"created-at">> => Height,
                <<"funding-deadline">> => Height + FundingWindow,
                <<"reservation-duration">> => ReservationDuration,
                <<"minimum-fee">> => MinimumFee,
                <<"maturity-duration">> => MaturityWindow
            },
        ?event(
            {position_opened,
                {position, PositionID},
                {position_owner, PositionOwner},
                {principal, Principal},
                {repayment, Repayment}
            }
        ),
        put_position(debit(Base, PositionOwner, Quantity, Opts), Position, Opts)
    else
        _ -> Base
    end.

%% @doc Give one funder an exclusive window in which to fund an open position.
reserve_position(Base, Body, Height, Position, Opts) ->
    maybe
        {ok, Funder} ?= signer(Body, Opts),
        #{
            <<"status">> := <<"open">>,
            <<"position-owner">> := PositionOwner,
            <<"recipient">> := Recipient,
            <<"funding-deadline">> := FundingDeadline,
            <<"reservation-duration">> := Duration,
            <<"minimum-fee">> := Fee
        } ?= Position,
        true ?= Height =< FundingDeadline,
        false ?= Funder =:= PositionOwner,
        false ?= Funder =:= Recipient,
        {ok, Reward} ?=
            hb_util:safe_int(tx_field(Body, <<"reward">>, 0, Opts)),
        true ?= Reward >= Fee,
        Reserved =
            hb_ao:set(
                Position,
                #{
                    <<"status">> => <<"reserved">>,
                    <<"funder">> => Funder,
                    <<"reserved-until">> =>
                        erlang:min(Height + Duration, FundingDeadline)
                },
                Opts
            ),
        ?event(
            {position_reserved,
                {position, maps:get(<<"position-id">>, Position)},
                {funder, Funder}
            }
        ),
        put_position(Base, Reserved, Opts)
    else
        _ -> Base
    end.

%% @doc Cancel an unfunded position and return its collateral to the owner.
cancel_position(Base, Body, Opts) ->
    maybe
        {ok, Signer} ?= signer(Body, Opts),
        {ok, Position} ?= find_position(Base, Body, Opts),
        #{
            <<"status">> := <<"open">>,
            <<"position-owner">> := Signer,
            <<"collateral-quantity">> := Quantity
        } ?= Position,
        ?event(
            {position_cancelled,
                {position, maps:get(<<"position-id">>, Position)}}
        ),
        drop_position(credit(Base, Signer, Quantity, Opts), Position, Opts)
    else
        _ -> Base
    end.

%% @doc After maturity, move the collateral to the funder. Anyone may trigger
%% the deterministic claim, but the message must still have a single signer.
claim_collateral(Base, Body, Height, Opts) ->
    maybe
        {ok, _Caller} ?= signer(Body, Opts),
        {ok, Position} ?= find_position(Base, Body, Opts),
        #{
            <<"status">> := <<"active">>,
            <<"funder">> := Funder,
            <<"maturity">> := Maturity,
            <<"collateral-quantity">> := Quantity
        } ?= Position,
        true ?= Height > Maturity,
        ?event(
            {collateral_claimed,
                {position, maps:get(<<"position-id">>, Position)}}
        ),
        drop_position(credit(Base, Funder, Quantity, Opts), Position, Opts)
    else
        _ -> Base
    end.

%% @doc Observe ordinary AR transfers for funding and repayment.
payment(Base, Body, Target, Height, Opts) ->
    case find_position(Base, Body, Opts) of
        {ok, _Position = #{ <<"status">> := <<"open">> }} ->
            {skip, Base};
        {ok, Position = #{ <<"status">> := <<"reserved">> }} ->
            {skip, fund(Base, Body, Target, Height, Position, Opts)};
        {ok, Position = #{ <<"status">> := <<"active">> }} ->
            {skip, repay(Base, Body, Target, Height, Position, Opts)};
        _ -> {ok, Base}
    end.

fund(Base, Body, Target, Height, Position, Opts) ->
    maybe
        {ok, Signer} ?= signer(Body, Opts),
        #{
            <<"position-owner">> := PositionOwner,
            <<"funder">> := Signer,
            <<"principal">> := Principal,
            <<"recipient">> := Recipient,
            <<"funding-deadline">> := Deadline,
            <<"reserved-until">> := ReservedUntil,
            <<"maturity-duration">> := MaturityDuration
        } ?= Position,
        true ?= Target =:= Recipient,
        {ok, Paid} ?= hb_util:safe_int(tx_field(Body, <<"quantity">>, 0, Opts)),
        true ?= Paid >= Principal,
        true ?= Height =< Deadline,
        true ?= Height =< ReservedUntil,
        true ?= is_address(Signer),
        false ?= Signer =:= PositionOwner,
        false ?= Signer =:= Recipient,
        FundingTX = hb_util:human_id(hb_message:id(Body, signed, Opts)),
        false ?= payment_seen(Base, FundingTX, Opts),
        Active =
            hb_ao:set(
                Position,
                #{
                    <<"status">> => <<"active">>,
                    <<"repayment-recipient">> => Signer,
                    <<"maturity">> => Height + MaturityDuration,
                    <<"funding-tx">> => FundingTX
                },
                Opts
            ),
        ?event(
            {position_funded,
                {position, maps:get(<<"position-id">>, Position)},
                {funder, Signer}
            }
        ),
        remember_payment(put_position(Base, Active, Opts), FundingTX, Opts)
    else
        _ -> Base
    end.

repay(Base, Body, Target, Height, Position, Opts) ->
    maybe
        {ok, _Payer} ?= signer(Body, Opts),
        #{
            <<"position-owner">> := PositionOwner,
            <<"repayment">> := Repayment,
            <<"repayment-recipient">> := Recipient,
            <<"maturity">> := Maturity,
            <<"collateral-quantity">> := Quantity
        } ?= Position,
        true ?= Target =:= Recipient,
        {ok, Paid} ?= hb_util:safe_int(tx_field(Body, <<"quantity">>, 0, Opts)),
        true ?= Paid >= Repayment,
        true ?= Height =< Maturity,
        RepaymentTX = hb_util:human_id(hb_message:id(Body, signed, Opts)),
        false ?= payment_seen(Base, RepaymentTX, Opts),
        ?event({position_repaid, {position, maps:get(<<"position-id">>, Position)}}),
        remember_payment(
            drop_position(credit(Base, PositionOwner, Quantity, Opts), Position, Opts),
            RepaymentTX,
            Opts
        )
    else
        _ -> Base
    end.

%%% Clock

advance(Base, Height, Opts) ->
    lists:foldl(
        fun(Position, Acc) -> expire(Acc, Position, Height, Opts) end,
        Base,
        positions(Base, Opts)
    ).

expire(
        Base,
        Position =
            #{
                <<"status">> := Status,
                <<"position-owner">> := PositionOwner,
                <<"collateral-quantity">> := Quantity,
                <<"funding-deadline">> := Deadline
            },
        Height,
        Opts)
        when Height > Deadline,
             Status =:= <<"open">> orelse Status =:= <<"reserved">> ->
    ?event({position_funding_expired, {position, maps:get(<<"position-id">>, Position)}}),
    drop_position(credit(Base, PositionOwner, Quantity, Opts), Position, Opts);
expire(
        Base,
        Position =
            #{
                <<"status">> := <<"reserved">>,
                <<"reserved-until">> := ReservedUntil
            },
        Height,
        Opts) when Height > ReservedUntil ->
    Reopened =
        hb_ao:set(
            Position,
            #{
                <<"status">> => <<"open">>,
                <<"funder">> => unset,
                <<"reserved-until">> => unset
            },
            Opts
        ),
    put_position(Base, Reopened, Opts);
expire(Base, _Position, _Height, _Opts) ->
    Base.

%%% State helpers

state(Key, Base, Default, Opts) ->
    hb_ao:get(Key, {as, <<"message@1.0">>, Base}, Default, Opts).

field(Key, Msg, Default, Opts) ->
    hb_maps:get(Key, Msg, Default, Opts).

action(Body, Opts) ->
    case field(<<"action">>, Body, <<>>, Opts) of
        Action when is_binary(Action) ->
            hb_util:to_lower(hb_ao:normalize_key(Action));
        _ -> <<>>
    end.

positions(Base, Opts) ->
    [
        Position
    ||
        Position = #{ <<"position-id">> := _ } <-
            hb_maps:values(
                hb_cache:ensure_all_loaded(position_book(Base, Opts), Opts),
                Opts
            )
    ].

position_book(Base, Opts) -> state(<<"positions">>, Base, #{}, Opts).

live_positions(Base, Opts) ->
    [Position || Position = #{ <<"status">> := Status } <- positions(Base, Opts), is_live(Status)].

is_live(<<"open">>) -> true;
is_live(<<"reserved">>) -> true;
is_live(<<"active">>) -> true;
is_live(_) -> false.

%% @doc Borrowing may escrow balances only in its canonical native stack.
supported_stack(Base, Opts) ->
    StackBase = {as, <<"message@1.0">>, Base},
    hb_ao:get(<<"execution-device">>, StackBase, not_found, Opts)
        =:= <<"stack@1.0">>
        andalso
            hb_ao:get(
                [<<"device-stack">>, <<"1">>],
                StackBase,
                not_found,
                Opts
            ) =:= <<"arweave-borrow@1.0">>
        andalso
            hb_ao:get(
                [<<"device-stack">>, <<"2">>],
                StackBase,
                not_found,
                Opts
            ) =:= <<"token@1.0">>
        andalso
            hb_ao:get(
                [<<"device-stack">>, <<"3">>],
                StackBase,
                not_found,
                Opts
            ) =:= not_found.

find_position(Base, Body, Opts) ->
    case
        hb_cache:ensure_all_loaded(
            hb_maps:get(
                field(<<"position-id">>, Body, <<>>, Opts),
                position_book(Base, Opts),
                not_found,
                Opts
            ),
            Opts
        )
    of
        Position = #{ <<"position-id">> := _ } -> {ok, Position};
        _ -> not_found
    end.

seen_positions(Base, Opts) -> state(?SEEN_POSITIONS, Base, #{}, Opts).

position_seen(Base, PositionID, Opts) ->
    hb_maps:get(PositionID, seen_positions(Base, Opts), false, Opts) =:= true
        orelse
            hb_maps:get(PositionID, position_book(Base, Opts), not_found, Opts)
                =/= not_found.

remember_position(Base, PositionID, Opts) ->
    replace_key(
        Base,
        ?SEEN_POSITIONS,
        replace_key(seen_positions(Base, Opts), PositionID, true, Opts),
        Opts
    ).

seen_payments(Base, Opts) -> state(?SEEN_PAYMENTS, Base, #{}, Opts).

payment_seen(Base, TXID, Opts) ->
    hb_maps:get(TXID, seen_payments(Base, Opts), false, Opts) =:= true.

remember_payment(Base, TXID, Opts) ->
    replace_key(
        Base,
        ?SEEN_PAYMENTS,
        replace_key(seen_payments(Base, Opts), TXID, true, Opts),
        Opts
    ).

put_position(Base, Position = #{ <<"position-id">> := PositionID }, Opts) ->
    remember_position(
        replace_key(
            Base,
            <<"positions">>,
            replace_key(position_book(Base, Opts), PositionID, Position, Opts),
            Opts
        ),
        PositionID,
        Opts
    ).

drop_position(Base, #{ <<"position-id">> := PositionID }, Opts) ->
    remember_position(
        replace_key(
            Base,
            <<"positions">>,
            replace_key(position_book(Base, Opts), PositionID, unset, Opts),
            Opts
        ),
        PositionID,
        Opts
    ).

%% @doc Replace one message key without deep-merging its old value back in.
replace_key(Base, Key, Value, Opts) ->
    hb_util:ok(
        hb_ao:raw(
            <<"message@1.0">>,
            <<"set">>,
            hb_cache:ensure_loaded(Base, Opts),
            #{ <<"set-mode">> => <<"explicit">>, Key => Value },
            Opts
        ),
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
    Balances = state(?BALANCES, Base, #{}, Opts),
    {ok, Updated} =
        hb_ao:resolve(
            Balances,
            #{
                <<"path">> => <<"set">>,
                Address => balance(Base, Address, Opts) + Amount
            },
            Opts
        ),
    replace_key(Base, ?BALANCES, Updated, Opts).

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

test_opts() ->
    hb:init(),
    #{ <<"priv-wallet">> => ar_wallet:new() }.

process_id() -> hb_util:human_id(<<1:256>>).

party() ->
    Wallet = ar_wallet:new(),
    {Wallet, hb_util:human_id(ar_wallet:to_address(Wallet))}.

base(Balances) ->
    #{
        <<"device">> => <<"arweave-borrow@1.0">>,
        <<"execution-device">> => <<"stack@1.0">>,
        <<"device-stack">> =>
            #{
                <<"1">> => <<"arweave-borrow@1.0">>,
                <<"2">> => <<"token@1.0">>
            },
        ?BALANCES => Balances
    }.

tx(Wallet, Fields) ->
    hb_message:commit(
        Fields,
        #{ <<"priv-wallet">> => Wallet },
        #{ <<"commitment-device">> => <<"tx@1.0">> }
    ).

apply_tx(Base, Body, Height, Opts) ->
    Result =
        hb_ao:resolve(
            Base#{ <<"device">> => <<"arweave-borrow@1.0">> },
            assignment(Body, Height),
            Opts
        ),
    case Result of
        {ok, New} -> New;
        {skip, New} -> New
    end.

assignment(Body, Height) ->
    #{
        <<"path">> => <<"compute">>,
        <<"process">> => process_id(),
        <<"slot">> => 1,
        <<"block-height">> => Height,
        <<"body">> => Body
    }.

tick(Base, Height, Opts) ->
    apply_tx(Base, #{ <<"target">> => <<"someone-else">> }, Height, Opts).

open_position(Wallet, Principal, Repayment, FundingDeadline, Maturity) ->
    tx(
        Wallet,
        #{
            <<"target">> => process_id(),
            <<"action">> => <<"open-position">>,
            <<"principal">> => hb_util:bin(Principal),
            <<"repayment">> => hb_util:bin(Repayment),
            <<"funding-deadline">> => hb_util:bin(FundingDeadline),
            <<"maturity">> => hb_util:bin(Maturity)
        }
    ).

open(Base, Wallet, FundingDeadline, Opts) ->
    apply_tx(
        Base,
        open_position(Wallet, 100, 120, FundingDeadline, 20),
        50,
        Opts
    ).

position_action(Wallet, Action, PositionID) ->
    tx(
        Wallet,
        #{
            <<"target">> => process_id(),
            <<"action">> => Action,
            <<"position-id">> => PositionID
        }
    ).

reserve_position(Wallet, PositionID) -> reserve_position(Wallet, PositionID, 0).

reserve_position(Wallet, PositionID, Reward) ->
    tx(
        Wallet,
        #{
            <<"target">> => process_id(),
            <<"action">> => <<"reserve-position">>,
            <<"position-id">> => PositionID,
            <<"reward">> => Reward
        }
    ).

reserve(Base, Wallet, Height, Opts) ->
    apply_tx(
        Base,
        reserve_position(Wallet, position_id(Base, Opts)),
        Height,
        Opts
    ).

pay(Wallet, To, Winston, PositionID) ->
    tx(
        Wallet,
        #{
            <<"target">> => To,
            <<"quantity">> => hb_util:bin(Winston),
            <<"position-id">> => PositionID
        }
    ).

tag_only_tx(Wallet, Tags) ->
    Signed = ar_tx:sign(#tx{ format = 2, reward = 1, tags = Tags }, Wallet),
    hb_message:convert(Signed, <<"structured@1.0">>, <<"tx@1.0">>, #{}).

tag_only_payment(Wallet, To, Winston, PositionID) ->
    tag_only_tx(
        Wallet,
        [
            {<<"target">>, To},
            {<<"quantity">>, hb_util:bin(Winston)},
            {<<"position-id">>, PositionID}
        ]
    ).

only_position(Base, Opts) ->
    [Position] = positions(Base, Opts),
    Position.

position_id(Base, Opts) ->
    maps:get(<<"position-id">>, only_position(Base, Opts)).

open_position_escrows_test() ->
    Opts = test_opts(),
    {PositionOwner, PositionOwnerAddr} = party(),
    Opened = open(base(#{ PositionOwnerAddr => 1 }), PositionOwner, 10, Opts),
    Position = only_position(Opened, Opts),
    ?assertEqual(0, balance(Opened, PositionOwnerAddr, Opts)),
    ?assertEqual(<<"open">>, maps:get(<<"status">>, Position)),
    ?assertEqual(60, maps:get(<<"funding-deadline">>, Position)),
    ?assertEqual(20, maps:get(<<"maturity-duration">>, Position)).

wrong_stack_does_not_escrow_test() ->
    Opts = test_opts(),
    {PositionOwner, PositionOwnerAddr} = party(),
    Wrong =
        (base(#{ PositionOwnerAddr => 1 }))#{
            <<"device-stack">> =>
                #{
                    <<"1">> => <<"token@1.0">>,
                    <<"2">> => <<"arweave-borrow@1.0">>
                }
        },
    Result = open(Wrong, PositionOwner, 10, Opts),
    ?assertEqual([], positions(Result, Opts)),
    ?assertEqual(1, balance(Result, PositionOwnerAddr, Opts)).

sale_conflict_blocks_position_test() ->
    Opts = test_opts(),
    {PositionOwner, PositionOwnerAddr} = party(),
    Base =
        (base(#{ PositionOwnerAddr => 1 }))#{
            <<"orders">> =>
                #{
                    <<"order">> =>
                        #{
                            <<"order-id">> => <<"order">>,
                            <<"status">> => <<"open">>
                        }
                }
        },
    Result = open(Base, PositionOwner, 10, Opts),
    ?assertEqual([], positions(Result, Opts)),
    ?assertEqual(1, balance(Result, PositionOwnerAddr, Opts)).

non_ar_recipient_is_rejected_test() ->
    Opts = test_opts(),
    {PositionOwner, PositionOwnerAddr} = party(),
    BadRecipient = binary:copy(<<"a">>, 42),
    Open =
        tx(
            PositionOwner,
            #{
                <<"target">> => process_id(),
                <<"action">> => <<"open-position">>,
                <<"principal">> => <<"100">>,
                <<"repayment">> => <<"120">>,
                <<"funding-deadline">> => <<"10">>,
                <<"maturity">> => <<"20">>,
                <<"recipient">> => BadRecipient
            }
        ),
    Result = apply_tx(base(#{ PositionOwnerAddr => 1 }), Open, 50, Opts),
    ?assertEqual([], positions(Result, Opts)),
    ?assertEqual(1, balance(Result, PositionOwnerAddr, Opts)).

cancel_open_position_test() ->
    Opts = test_opts(),
    {PositionOwner, PositionOwnerAddr} = party(),
    Opened = open(base(#{ PositionOwnerAddr => 1 }), PositionOwner, 10, Opts),
    Cancelled =
        apply_tx(
            Opened,
            position_action(
                PositionOwner,
                <<"cancel-position">>,
                position_id(Opened, Opts)
            ),
            51,
            Opts
        ),
    ?assertEqual([], positions(Cancelled, Opts)),
    ?assertEqual(1, balance(Cancelled, PositionOwnerAddr, Opts)).

reservation_is_exclusive_test() ->
    Opts = test_opts(),
    {PositionOwner, PositionOwnerAddr} = party(),
    {Funder, FunderAddr} = party(),
    {Interloper, _InterloperAddr} = party(),
    Opened = open(base(#{ PositionOwnerAddr => 1 }), PositionOwner, 100, Opts),
    Reserved = reserve(Opened, Funder, 51, Opts),
    Cancelled =
        apply_tx(
            Reserved,
            position_action(PositionOwner, <<"cancel-position">>, position_id(Reserved, Opts)),
            52,
            Opts
        ),
    Paid =
        apply_tx(
            Cancelled,
            pay(Interloper, PositionOwnerAddr, 100, position_id(Cancelled, Opts)),
            53,
            Opts
        ),
    Position = only_position(Paid, Opts),
    ?assertEqual(<<"reserved">>, maps:get(<<"status">>, Position)),
    ?assertEqual(FunderAddr, maps:get(<<"funder">>, Position)).

unreserved_funding_is_ignored_test() ->
    Opts = test_opts(),
    {PositionOwner, PositionOwnerAddr} = party(),
    {Funder, _FunderAddr} = party(),
    Opened = open(base(#{ PositionOwnerAddr => 1 }), PositionOwner, 10, Opts),
    Result =
        apply_tx(
            Opened,
            pay(Funder, PositionOwnerAddr, 100, position_id(Opened, Opts)),
            55,
            Opts
        ),
    ?assertEqual(<<"open">>, maps:get(<<"status">>, only_position(Result, Opts))).

funding_wrong_target_is_ignored_test() ->
    Opts = test_opts(),
    {PositionOwner, PositionOwnerAddr} = party(),
    {Funder, _FunderAddr} = party(),
    {_, WrongTarget} = party(),
    Opened = open(base(#{ PositionOwnerAddr => 1 }), PositionOwner, 10, Opts),
    Reserved = reserve(Opened, Funder, 51, Opts),
    Result =
        apply_tx(
            Reserved,
            pay(Funder, WrongTarget, 100, position_id(Reserved, Opts)),
            55,
            Opts
        ),
    ?assertEqual(<<"reserved">>, maps:get(<<"status">>, only_position(Result, Opts))).

tag_only_payment_fields_are_ignored_test() ->
    Opts = test_opts(),
    {PositionOwner, PositionOwnerAddr} = party(),
    {Funder, _FunderAddr} = party(),
    Opened = open(base(#{ PositionOwnerAddr => 1 }), PositionOwner, 10, Opts),
    PositionID = position_id(Opened, Opts),
    Reserved = reserve(Opened, Funder, 51, Opts),
    Tagged = tag_only_payment(Funder, PositionOwnerAddr, 100, PositionID),
    ?assertEqual(PositionOwnerAddr, hb_ao:get(<<"target">>, Tagged, not_found, Opts)),
    ?assertEqual(<<>>, tx_field(Tagged, <<"target">>, <<>>, Opts)),
    Result = apply_tx(Reserved, Tagged, 55, Opts),
    ?assertEqual(<<"reserved">>, maps:get(<<"status">>, only_position(Result, Opts))).

funding_activates_position_test() ->
    Opts = test_opts(),
    {PositionOwner, PositionOwnerAddr} = party(),
    {Funder, FunderAddr} = party(),
    Opened = open(base(#{ PositionOwnerAddr => 1 }), PositionOwner, 10, Opts),
    Reserved = reserve(Opened, Funder, 51, Opts),
    Funded =
        apply_tx(
            Reserved,
            pay(Funder, PositionOwnerAddr, 100, position_id(Reserved, Opts)),
            55,
            Opts
        ),
    Position = only_position(Funded, Opts),
    ?assertEqual(<<"active">>, maps:get(<<"status">>, Position)),
    ?assertEqual(FunderAddr, maps:get(<<"funder">>, Position)),
    ?assertEqual(FunderAddr, maps:get(<<"repayment-recipient">>, Position)),
    ?assertEqual(75, maps:get(<<"maturity">>, Position)).

repayment_releases_collateral_test() ->
    Opts = test_opts(),
    {PositionOwner, PositionOwnerAddr} = party(),
    {Funder, FunderAddr} = party(),
    Opened = open(base(#{ PositionOwnerAddr => 1 }), PositionOwner, 10, Opts),
    Reserved = reserve(Opened, Funder, 51, Opts),
    Funded =
        apply_tx(
            Reserved,
            pay(Funder, PositionOwnerAddr, 100, position_id(Reserved, Opts)),
            55,
            Opts
        ),
    Repaid =
        apply_tx(
            Funded,
            pay(PositionOwner, FunderAddr, 120, position_id(Funded, Opts)),
            70,
            Opts
        ),
    ?assertEqual([], positions(Repaid, Opts)),
    ?assertEqual(1, balance(Repaid, PositionOwnerAddr, Opts)).

claim_collateral_transfers_collateral_test() ->
    Opts = test_opts(),
    {PositionOwner, PositionOwnerAddr} = party(),
    {Funder, FunderAddr} = party(),
    {Caller, _CallerAddr} = party(),
    Opened = open(base(#{ PositionOwnerAddr => 1 }), PositionOwner, 10, Opts),
    Reserved = reserve(Opened, Funder, 51, Opts),
    Funded =
        apply_tx(
            Reserved,
            pay(Funder, PositionOwnerAddr, 100, position_id(Reserved, Opts)),
            55,
            Opts
        ),
    Claimed =
        apply_tx(
            Funded,
            position_action(
                Caller,
                <<"claim-collateral">>,
                position_id(Funded, Opts)
            ),
            76,
            Opts
        ),
    ?assertEqual([], positions(Claimed, Opts)),
    ?assertEqual(0, balance(Claimed, PositionOwnerAddr, Opts)),
    ?assertEqual(1, balance(Claimed, FunderAddr, Opts)).

funding_expiry_returns_collateral_test() ->
    Opts = test_opts(),
    {PositionOwner, PositionOwnerAddr} = party(),
    Opened = open(base(#{ PositionOwnerAddr => 1 }), PositionOwner, 10, Opts),
    Expired = tick(Opened, 61, Opts),
    ?assertEqual([], positions(Expired, Opts)),
    ?assertEqual(1, balance(Expired, PositionOwnerAddr, Opts)).

reservation_lapses_test() ->
    Opts = test_opts(),
    {PositionOwner, PositionOwnerAddr} = party(),
    {Funder, _FunderAddr} = party(),
    Opened = open(base(#{ PositionOwnerAddr => 1 }), PositionOwner, 100, Opts),
    Reserved = reserve(Opened, Funder, 51, Opts),
    ?assertEqual(71, maps:get(<<"reserved-until">>, only_position(Reserved, Opts))),
    Lapsed = tick(Reserved, 72, Opts),
    Position = only_position(Lapsed, Opts),
    ?assertEqual(<<"open">>, maps:get(<<"status">>, Position)),
    ?assertEqual(false, maps:is_key(<<"funder">>, Position)).

reserved_funding_expiry_returns_collateral_test() ->
    Opts = test_opts(),
    {PositionOwner, PositionOwnerAddr} = party(),
    {Funder, _FunderAddr} = party(),
    Opened = open(base(#{ PositionOwnerAddr => 1 }), PositionOwner, 10, Opts),
    Reserved = reserve(Opened, Funder, 51, Opts),
    Expired = tick(Reserved, 61, Opts),
    ?assertEqual([], positions(Expired, Opts)),
    ?assertEqual(1, balance(Expired, PositionOwnerAddr, Opts)).

minimum_fee_uses_committed_reward_test() ->
    Opts = test_opts(),
    {PositionOwner, PositionOwnerAddr} = party(),
    {Funder, _FunderAddr} = party(),
    Open =
        tx(
            PositionOwner,
            #{
                <<"target">> => process_id(),
                <<"action">> => <<"open-position">>,
                <<"principal">> => <<"100">>,
                <<"repayment">> => <<"120">>,
                <<"funding-deadline">> => <<"100">>,
                <<"maturity">> => <<"20">>,
                <<"minimum-fee">> => <<"2">>
            }
        ),
    Opened = apply_tx(base(#{ PositionOwnerAddr => 1 }), Open, 50, Opts),
    PositionID = position_id(Opened, Opts),
    Signed =
        ar_tx:sign(
            #tx{
                format = 2,
                target = hb_util:native_id(process_id()),
                reward = 1,
                tags =
                    [
                        {<<"action">>, <<"reserve-position">>},
                        {<<"position-id">>, PositionID},
                        {<<"reward">>, <<"999">>}
                    ]
            },
            Funder
        ),
    Spoofed = hb_message:convert(Signed, <<"structured@1.0">>, <<"tx@1.0">>, #{}),
    Rejected = apply_tx(Opened, Spoofed, 51, Opts),
    ?assertEqual(<<"open">>, maps:get(<<"status">>, only_position(Rejected, Opts))),
    Accepted = apply_tx(Rejected, reserve_position(Funder, PositionID, 2), 52, Opts),
    ?assertEqual(<<"reserved">>, maps:get(<<"status">>, only_position(Accepted, Opts))).

stack_routing_test() ->
    Opts = test_opts(),
    {PositionOwner, PositionOwnerAddr} = party(),
    Set =
        tx(
            PositionOwner,
            #{
                <<"target">> => process_id(),
                <<"action">> => <<"Set">>,
                <<"execution-device">> => <<"token@1.0">>
            }
        ),
    Tail =
        #{
            compute =>
                fun(State, _Req, ResolveOpts) ->
                    {ok, hb_ao:set(State, <<"tail-called">>, true, ResolveOpts)}
                end
        },
    Stack =
        fun(State) ->
            State#{
                <<"device">> => <<"stack@1.0">>,
                <<"device-stack">> =>
                    #{ <<"1">> => <<"arweave-borrow@1.0">>, <<"2">> => Tail }
            }
        end,
    {ok, Passed} = hb_ao:resolve(Stack(base(#{})), assignment(Set, 50), Opts),
    ?assertEqual(true, state(<<"tail-called">>, Passed, false, Opts)),
    Unknown = position_action(PositionOwner, <<"reserve-position">>, <<"unknown">>),
    {ok, Consumed} = hb_ao:resolve(Stack(base(#{})), assignment(Unknown, 50), Opts),
    ?assertEqual(false, state(<<"tail-called">>, Consumed, false, Opts)),
    Opened = open(base(#{ PositionOwnerAddr => 1 }), PositionOwner, 10, Opts),
    {ok, Blocked} = hb_ao:resolve(Stack(Opened), assignment(Set, 51), Opts),
    ?assertEqual(false, state(<<"tail-called">>, Blocked, false, Opts)).

trie_balances_survive_cache_test() ->
    Opts = test_opts(),
    {PositionOwner, PositionOwnerAddr} = party(),
    {ok, Balances} =
        hb_ao:resolve(
            #{ <<"device">> => <<"trie@1.0">> },
            #{ <<"path">> => <<"set">>, PositionOwnerAddr => 1 },
            Opts
        ),
    Opened = open(base(Balances), PositionOwner, 10, Opts),
    Cancelled =
        apply_tx(
            Opened,
            position_action(PositionOwner, <<"cancel-position">>, position_id(Opened, Opts)),
            51,
            Opts
        ),
    {ok, ID} = hb_cache:write(Cancelled, Opts),
    {ok, Cached} = hb_cache:read(ID, Opts),
    ?assertEqual(1, balance(Cached, PositionOwnerAddr, Opts)),
    ?assertEqual([], positions(Cached, Opts)).
