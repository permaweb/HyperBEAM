%%% @doc Settle one token-collateralized borrowing position against native AR.
%%%
%%% The device is the first element of a `~stack@1.0', immediately before
%%% `~token@1.0'. It escrows collateral in the token's `balances' ledger; AR
%%% always moves directly between wallets in ordinary layer-1 transactions.
%%% `~arweave-scheduler@1.0' assigns those transfers to the process through one
%%% `Assign-To' tag.
%%%
%%% A process holds at most one position, in one of three states:
%%% <ul>
%%%   <li>`open': `open-position' has escrowed the requested
%%%       `collateral-quantity'. Its terms are `principal' and `repayment' in
%%%       winston, a relative `funding-deadline' and `maturity' in blocks, and
%%%       optional `recipient', `reservation-duration' and `minimum-fee'.</li>
%%%   <li>`reserved': `reserve-position' names the `position-id' and commits one
%%%       funder for a bounded window. Its transaction reward must cover the
%%%       position's minimum fee.</li>
%%%   <li>`active': the reserved funder has transferred at least `principal' AR
%%%       to the position's recipient. A transfer of at least `repayment' AR to
%%%       the funder before maturity returns the collateral to its owner. After
%%%       maturity, `claim-collateral' transfers it to the funder.</li>
%%% </ul>
%%%
%%% `cancel-position' returns collateral while a position is open. Funding and
%%% repayment transfers carry the `position-id' and assign themselves to the
%%% token process. The device reads their native target and quantity from the
%%% `tx@1.0' commitment rather than from forgeable tags.
%%%
%%% Borrowing actions and transfers naming a live position stop the stack after
%%% this device. Unrelated assignments continue to the token. A token `Set' may
%%% remove the stack only while no position exists.
-module(dev_arweave_borrow).
-implements(<<"arweave-borrow@1.0">>).
-export([init/3, compute/3, normalize/3, snapshot/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(BALANCES, <<"balances">>).
-define(DEFAULT_COLLATERAL_QUANTITY, 1).
-define(DEFAULT_RESERVATION_DURATION, 20).

%% @doc Necessary hooks for compliance with the `execution-device' standard.
init(Base, _Req, _Opts) -> {ok, Base}.
normalize(Base, _Req, _Opts) -> {ok, Base}.
snapshot(Base, _Req, _Opts) -> {ok, Base}.

%% @doc Advance deadlines, then route one scheduled L1 transaction.
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

%% @doc Apply a control transaction addressed to the process.
control(Base, Body, Height, Opts) ->
    case action(Body, Opts) of
        <<"open-position">> -> {skip, open_position(Base, Body, Height, Opts)};
        <<"cancel-position">> -> {skip, cancel_position(Base, Body, Opts)};
        <<"reserve-position">> -> {skip, reserve_position(Base, Body, Height, Opts)};
        <<"claim-collateral">> -> {skip, claim_collateral(Base, Body, Height, Opts)};
        <<"set">> ->
            case position(Base, Opts) of
                not_found -> {ok, Base};
                _Position -> {skip, Base}
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
        true ?= FundingWindow >= 1,
        true ?= MaturityWindow >= 1,
        true ?= ReservationDuration >= 1,
        true ?= MinimumFee >= 0,
        true ?= Quantity >= 1,
        true ?= is_address(Recipient),
        true ?= supported_stack(Base, Opts),
        not_found ?= position(Base, Opts),
        true ?= balance(Base, PositionOwner, Opts) >= Quantity,
        PositionID = hb_util:human_id(hb_message:id(Body, signed, Opts)),
        Position =
            #{
                <<"position-id">> => PositionID,
                <<"status">> => <<"open">>,
                <<"position-owner">> => PositionOwner,
                <<"principal">> => Principal,
                <<"repayment">> => Repayment,
                <<"recipient">> => Recipient,
                <<"collateral-quantity">> => Quantity,
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
reserve_position(Base, Body, Height, Opts) ->
    maybe
        {ok, Funder} ?= signer(Body, Opts),
        {ok, Position} ?= find_position(Base, Body, Opts),
        #{
            <<"position-id">> := PositionID,
            <<"status">> := <<"open">>,
            <<"position-owner">> := PositionOwner,
            <<"recipient">> := Recipient,
            <<"funding-deadline">> := FundingDeadline,
            <<"reservation-duration">> := Duration,
            <<"minimum-fee">> := Fee
        } ?= Position,
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
                {position, PositionID},
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
            <<"position-id">> := PositionID,
            <<"status">> := <<"open">>,
            <<"position-owner">> := Signer,
            <<"collateral-quantity">> := Quantity
        } ?= Position,
        ?event({position_cancelled, {position, PositionID}}),
        drop_position(credit(Base, Signer, Quantity, Opts), Opts)
    else
        _ -> Base
    end.

%% @doc After maturity, move the collateral to the funder. The transition is
%% deterministic, so any transaction may trigger it.
claim_collateral(Base, Body, Height, Opts) ->
    maybe
        {ok, Position} ?= find_position(Base, Body, Opts),
        #{
            <<"position-id">> := PositionID,
            <<"status">> := <<"active">>,
            <<"funder">> := Funder,
            <<"maturity">> := Maturity,
            <<"collateral-quantity">> := Quantity
        } ?= Position,
        true ?= Height > Maturity,
        ?event({collateral_claimed, {position, PositionID}}),
        drop_position(credit(Base, Funder, Quantity, Opts), Opts)
    else
        _ -> Base
    end.

%% @doc Observe ordinary AR transfers for funding and repayment.
payment(Base, Body, Target, Height, Opts) ->
    case find_position(Base, Body, Opts) of
        {ok, #{ <<"status">> := <<"open">> }} ->
            {skip, Base};
        {ok, Position = #{ <<"status">> := <<"reserved">> }} ->
            {skip, fund(Base, Body, Target, Height, Position, Opts)};
        {ok, Position = #{ <<"status">> := <<"active">> }} ->
            {skip, repay(Base, Body, Target, Height, Position, Opts)};
        _ -> {ok, Base}
    end.

%% @doc Activate a reserved position with its funder's native AR transfer.
fund(Base, Body, Target, Height, Position, Opts) ->
    maybe
        {ok, Signer} ?= signer(Body, Opts),
        #{
            <<"position-id">> := PositionID,
            <<"funder">> := Signer,
            <<"principal">> := Principal,
            <<"recipient">> := Recipient,
            <<"maturity-duration">> := MaturityDuration
        } ?= Position,
        true ?= Target =:= Recipient,
        {ok, Paid} ?= hb_util:safe_int(tx_field(Body, <<"quantity">>, 0, Opts)),
        true ?= Paid >= Principal,
        Active =
            hb_ao:set(
                Position,
                #{
                    <<"status">> => <<"active">>,
                    <<"maturity">> => Height + MaturityDuration
                },
                Opts
            ),
        ?event(
            {position_funded,
                {position, PositionID},
                {funder, Signer}
            }
        ),
        put_position(Base, Active, Opts)
    else
        _ -> Base
    end.

%% @doc Release collateral after a sufficient, timely transfer to the funder.
repay(Base, Body, Target, Height, Position, Opts) ->
    maybe
        #{
            <<"position-id">> := PositionID,
            <<"position-owner">> := PositionOwner,
            <<"repayment">> := Repayment,
            <<"funder">> := Funder,
            <<"maturity">> := Maturity,
            <<"collateral-quantity">> := Quantity
        } ?= Position,
        true ?= Target =:= Funder,
        {ok, Paid} ?= hb_util:safe_int(tx_field(Body, <<"quantity">>, 0, Opts)),
        true ?= Paid >= Repayment,
        true ?= Height =< Maturity,
        ?event({position_repaid, {position, PositionID}}),
        drop_position(credit(Base, PositionOwner, Quantity, Opts), Opts)
    else
        _ -> Base
    end.

%%% Clock

%% @doc Materialize any deadline crossed by this assignment.
advance(Base, Height, Opts) ->
    case position(Base, Opts) of
        not_found -> Base;
        Position -> expire(Base, Position, Height, Opts)
    end.

%% @doc Return collateral when the position's funding window closes.
expire(
        Base,
        #{
                <<"position-id">> := PositionID,
                <<"status">> := Status,
                <<"position-owner">> := PositionOwner,
                <<"collateral-quantity">> := Quantity,
                <<"funding-deadline">> := Deadline
            },
        Height,
        Opts)
        when Height > Deadline,
             Status =:= <<"open">> orelse Status =:= <<"reserved">> ->
    ?event({position_funding_expired, {position, PositionID}}),
    drop_position(credit(Base, PositionOwner, Quantity, Opts), Opts);
%% @doc Reopen a position whose funder did not pay during its reservation.
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

%% @doc Read process state without resolving through the active device.
state(Key, Base, Default, Opts) ->
    hb_ao:get(Key, {as, <<"message@1.0">>, Base}, Default, Opts).

%% @doc Read one field of an untrusted scheduled message as plain data.
field(Key, Msg, Default, Opts) ->
    hb_maps:get(Key, Msg, Default, Opts).

%% @doc Normalize an action using the deployed token's matching semantics.
action(Body, Opts) ->
    case field(<<"action">>, Body, <<>>, Opts) of
        Action when is_binary(Action) ->
            hb_util:to_lower(hb_ao:normalize_key(Action));
        _ -> <<>>
    end.

%% @doc Read the process's one live position, loading through cache links.
position(Base, Opts) ->
    case
        hb_cache:ensure_all_loaded(
            state(<<"position">>, Base, not_found, Opts),
            Opts
        )
    of
        Position = #{ <<"position-id">> := _ } -> Position;
        _ -> not_found
    end.

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

%% @doc Return the live position only when the transaction names it exactly.
find_position(Base, Body, Opts) ->
    PositionID = field(<<"position-id">>, Body, <<>>, Opts),
    case position(Base, Opts) of
        Position = #{ <<"position-id">> := PositionID } -> {ok, Position};
        _ -> not_found
    end.

%% @doc Replace the process's live position without deep-merging old terms.
put_position(Base, Position, Opts) ->
    replace_key(Base, <<"position">>, Position, Opts).

%% @doc Remove the completed position from process state.
drop_position(Base, Opts) ->
    replace_key(Base, <<"position">>, unset, Opts).

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

%% @doc Read one address from the token ledger shared with `~token@1.0'.
balance(Base, Address, Opts) ->
    hb_util:int(state([?BALANCES, Address], Base, 0, Opts)).

%% @doc Add token units to one address without replacing the ledger device.
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

%% @doc Remove token units from one address.
debit(Base, Address, Amount, Opts) -> credit(Base, Address, -Amount, Opts).

%% @doc Parse a numeric protocol field without throwing on foreign input.
amount(Key, Body, Opts) -> amount(Key, Body, 0, Opts).

amount(Key, Body, Default, Opts) ->
    hb_util:safe_int(field(Key, Body, Default, Opts)).

%% @doc Read a native transaction field from its `tx@1.0' commitment.
tx_field(Body, Field, Default, Opts) ->
    CommitmentReq = #{ <<"commitment-device">> => <<"tx@1.0">> },
    case hb_message:commitment(CommitmentReq, Body, Opts) of
        {ok, _ID, Commitment} ->
            hb_maps:get(<<"field-", Field/binary>>, Commitment, Default, Opts);
        _ ->
            Default
    end.

%% @doc Return the address of a message's sole signer.
signer(Body, Opts) ->
    case hb_message:signers(Body, Opts) of
        [Signer] -> {ok, hb_util:human_id(Signer)};
        _ -> not_found
    end.

%% @doc Whether a value is a canonical human-readable Arweave address.
is_address(Address) ->
    is_binary(Address) andalso byte_size(Address) =:= 43.

%%% Tests

-define(PROCESS, <<"pRoCeSs000000000000000000000000000000000000">>).

%% @doc Isolate each test's wallet-backed store.
test_opts() ->
    hb:init(),
    #{ <<"priv-wallet">> => ar_wallet:new() }.

%% @doc Return a wallet and its native address.
party() ->
    Wallet = ar_wallet:new(),
    {Wallet, hb_util:human_id(ar_wallet:to_address(Wallet))}.

%% @doc Build the only execution stack in which collateral may be escrowed.
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

%% @doc Commit a synthetic L1 transaction with the production codec.
tx(Wallet, Fields) ->
    hb_message:commit(
        Fields,
        #{ <<"priv-wallet">> => Wallet },
        #{ <<"commitment-device">> => <<"tx@1.0">> }
    ).

%% @doc Apply a transaction through AO-Core at one block height.
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

%% @doc Build the scheduler assignment delivered to the execution stack.
assignment(Body, Height) ->
    #{
        <<"path">> => <<"compute">>,
        <<"process">> => ?PROCESS,
        <<"slot">> => 1,
        <<"block-height">> => Height,
        <<"body">> => Body
    }.

%% @doc Advance deadlines with an unrelated assignment.
tick(Base, Height, Opts) ->
    apply_tx(Base, #{ <<"target">> => <<"someone-else">> }, Height, Opts).

open_tx(Wallet, Principal, Repayment, FundingDeadline, Maturity) ->
    tx(
        Wallet,
        #{
            <<"target">> => ?PROCESS,
            <<"action">> => <<"open-position">>,
            <<"principal">> => hb_util:bin(Principal),
            <<"repayment">> => hb_util:bin(Repayment),
            <<"funding-deadline">> => hb_util:bin(FundingDeadline),
            <<"maturity">> => hb_util:bin(Maturity)
        }
    ).

%% @doc Open the standard test position at height 50.
open(Base, Wallet, FundingDeadline, Opts) ->
    apply_tx(
        Base,
        open_tx(Wallet, 100, 120, FundingDeadline, 20),
        50,
        Opts
    ).

%% @doc Build a control transaction naming one position.
position_action(Wallet, Action, PositionID) ->
    tx(
        Wallet,
        #{
            <<"target">> => ?PROCESS,
            <<"action">> => Action,
            <<"position-id">> => PositionID
        }
    ).

reserve_tx(Wallet, PositionID) -> reserve_tx(Wallet, PositionID, 0).

reserve_tx(Wallet, PositionID, Reward) ->
    tx(
        Wallet,
        #{
            <<"target">> => ?PROCESS,
            <<"action">> => <<"reserve-position">>,
            <<"position-id">> => PositionID,
            <<"reward">> => Reward
        }
    ).

%% @doc Reserve the current test position.
reserve(Base, Wallet, Height, Opts) ->
    apply_tx(
        Base,
        reserve_tx(Wallet, position_id(Base, Opts)),
        Height,
        Opts
    ).

%% @doc Build an ordinary native transfer assigned to the process.
pay(Wallet, To, Winston, PositionID) ->
    tx(
        Wallet,
        #{
            <<"target">> => To,
            <<"quantity">> => hb_util:bin(Winston),
            <<"position-id">> => PositionID
        }
    ).

%% @doc Build a transaction whose payment-shaped values exist only as tags.
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

position_id(Base, Opts) ->
    maps:get(<<"position-id">>, position(Base, Opts)).

position_status(Base, Opts) ->
    maps:get(<<"status">>, position(Base, Opts)).

%% @doc Build the standard active position used by settlement tests.
active_position(Opts) ->
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
    {Funded, PositionOwner, PositionOwnerAddr, FunderAddr}.

%% @doc Opening a position moves its collateral out of the owner's balance.
open_position_escrows_test() ->
    Opts = test_opts(),
    {PositionOwner, PositionOwnerAddr} = party(),
    Opened = open(base(#{ PositionOwnerAddr => 1 }), PositionOwner, 10, Opts),
    Position = position(Opened, Opts),
    ?assertEqual(0, balance(Opened, PositionOwnerAddr, Opts)),
    ?assertEqual(<<"open">>, maps:get(<<"status">>, Position)),
    ?assertEqual(60, maps:get(<<"funding-deadline">>, Position)),
    ?assertEqual(20, maps:get(<<"maturity-duration">>, Position)).

%% @doc A standalone or reordered device cannot touch the token ledger.
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
    ?assertEqual(not_found, position(Result, Opts)),
    ?assertEqual(1, balance(Result, PositionOwnerAddr, Opts)).

%% @doc Opening without the requested collateral changes nothing.
insufficient_collateral_does_not_open_test() ->
    Opts = test_opts(),
    {PositionOwner, PositionOwnerAddr} = party(),
    Result = open(base(#{ PositionOwnerAddr => 0 }), PositionOwner, 10, Opts),
    ?assertEqual(not_found, position(Result, Opts)),
    ?assertEqual(0, balance(Result, PositionOwnerAddr, Opts)).

%% @doc A recipient must be a native Arweave address.
non_ar_recipient_is_rejected_test() ->
    Opts = test_opts(),
    {PositionOwner, PositionOwnerAddr} = party(),
    BadRecipient = binary:copy(<<"a">>, 42),
    Open =
        tx(
            PositionOwner,
            #{
                <<"target">> => ?PROCESS,
                <<"action">> => <<"open-position">>,
                <<"principal">> => <<"100">>,
                <<"repayment">> => <<"120">>,
                <<"funding-deadline">> => <<"10">>,
                <<"maturity">> => <<"20">>,
                <<"recipient">> => BadRecipient
            }
        ),
    Result = apply_tx(base(#{ PositionOwnerAddr => 1 }), Open, 50, Opts),
    ?assertEqual(not_found, position(Result, Opts)),
    ?assertEqual(1, balance(Result, PositionOwnerAddr, Opts)).

%% @doc The owner may cancel an open position and recover its collateral.
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
    ?assertEqual(not_found, position(Cancelled, Opts)),
    ?assertEqual(1, balance(Cancelled, PositionOwnerAddr, Opts)).

%% @doc Reservation blocks both owner cancellation and another funder's payment.
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
            position_action(
                PositionOwner,
                <<"cancel-position">>,
                position_id(Reserved, Opts)
            ),
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
    Position = position(Paid, Opts),
    ?assertEqual(<<"reserved">>, maps:get(<<"status">>, Position)),
    ?assertEqual(FunderAddr, maps:get(<<"funder">>, Position)).

%% @doc Funding cannot activate a position before reservation.
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
    ?assertEqual(<<"open">>, position_status(Result, Opts)).

%% @doc A transfer to anyone other than the fixed recipient cannot fund.
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
    ?assertEqual(<<"reserved">>, position_status(Result, Opts)).

%% @doc Tags cannot impersonate the committed target or quantity fields.
tag_only_payment_fields_are_ignored_test() ->
    Opts = test_opts(),
    {PositionOwner, PositionOwnerAddr} = party(),
    {Funder, _FunderAddr} = party(),
    Opened = open(base(#{ PositionOwnerAddr => 1 }), PositionOwner, 10, Opts),
    PositionID = position_id(Opened, Opts),
    Reserved = reserve(Opened, Funder, 51, Opts),
    Tagged = tag_only_payment(Funder, PositionOwnerAddr, 100, PositionID),
    ?assertEqual(
        PositionOwnerAddr,
        hb_ao:get(<<"target">>, Tagged, not_found, Opts)
    ),
    ?assertEqual(<<>>, tx_field(Tagged, <<"target">>, <<>>, Opts)),
    Result = apply_tx(Reserved, Tagged, 55, Opts),
    ?assertEqual(<<"reserved">>, position_status(Result, Opts)).

%% @doc Timely funding fixes the funder and maturity height.
funding_activates_position_test() ->
    Opts = test_opts(),
    {Funded, _PositionOwner, _PositionOwnerAddr, FunderAddr} = active_position(Opts),
    Position = position(Funded, Opts),
    ?assertEqual(<<"active">>, maps:get(<<"status">>, Position)),
    ?assertEqual(FunderAddr, maps:get(<<"funder">>, Position)),
    ?assertEqual(75, maps:get(<<"maturity">>, Position)).

%% @doc Timely repayment returns collateral to the position owner.
repayment_releases_collateral_test() ->
    Opts = test_opts(),
    {Funded, PositionOwner, PositionOwnerAddr, FunderAddr} = active_position(Opts),
    Repaid =
        apply_tx(
            Funded,
            pay(PositionOwner, FunderAddr, 120, position_id(Funded, Opts)),
            70,
            Opts
        ),
    ?assertEqual(not_found, position(Repaid, Opts)),
    ?assertEqual(1, balance(Repaid, PositionOwnerAddr, Opts)).

%% @doc After maturity, a permissionless claim pays collateral to the funder.
claim_collateral_transfers_collateral_test() ->
    Opts = test_opts(),
    {Funded, _PositionOwner, PositionOwnerAddr, FunderAddr} = active_position(Opts),
    {Caller, _CallerAddr} = party(),
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
    ?assertEqual(not_found, position(Claimed, Opts)),
    ?assertEqual(0, balance(Claimed, PositionOwnerAddr, Opts)),
    ?assertEqual(1, balance(Claimed, FunderAddr, Opts)).

%% @doc An unfunded position returns collateral after its funding deadline.
funding_expiry_returns_collateral_test() ->
    Opts = test_opts(),
    {PositionOwner, PositionOwnerAddr} = party(),
    Opened = open(base(#{ PositionOwnerAddr => 1 }), PositionOwner, 10, Opts),
    Expired = tick(Opened, 61, Opts),
    ?assertEqual(not_found, position(Expired, Opts)),
    ?assertEqual(1, balance(Expired, PositionOwnerAddr, Opts)).

%% @doc An unpaid reservation reopens while its funding window remains live.
reservation_lapses_test() ->
    Opts = test_opts(),
    {PositionOwner, PositionOwnerAddr} = party(),
    {Funder, _FunderAddr} = party(),
    Opened = open(base(#{ PositionOwnerAddr => 1 }), PositionOwner, 100, Opts),
    Reserved = reserve(Opened, Funder, 51, Opts),
    ?assertEqual(71, maps:get(<<"reserved-until">>, position(Reserved, Opts))),
    Lapsed = tick(Reserved, 72, Opts),
    Position = position(Lapsed, Opts),
    ?assertEqual(<<"open">>, maps:get(<<"status">>, Position)),
    ?assertEqual(false, maps:is_key(<<"funder">>, Position)).

%% @doc The funding deadline retires a reserved position as well as an open one.
reserved_funding_expiry_returns_collateral_test() ->
    Opts = test_opts(),
    {PositionOwner, PositionOwnerAddr} = party(),
    {Funder, _FunderAddr} = party(),
    Opened = open(base(#{ PositionOwnerAddr => 1 }), PositionOwner, 10, Opts),
    Reserved = reserve(Opened, Funder, 51, Opts),
    Expired = tick(Reserved, 61, Opts),
    ?assertEqual(not_found, position(Expired, Opts)),
    ?assertEqual(1, balance(Expired, PositionOwnerAddr, Opts)).

%% @doc Reservation fees use the committed reward, never a same-named tag.
minimum_fee_uses_committed_reward_test() ->
    Opts = test_opts(),
    {PositionOwner, PositionOwnerAddr} = party(),
    {Funder, _FunderAddr} = party(),
    Open =
        tx(
            PositionOwner,
            #{
                <<"target">> => ?PROCESS,
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
                target = hb_util:native_id(?PROCESS),
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
    ?assertEqual(<<"open">>, position_status(Rejected, Opts)),
    Accepted = apply_tx(Rejected, reserve_tx(Funder, PositionID, 2), 52, Opts),
    ?assertEqual(<<"reserved">>, position_status(Accepted, Opts)).

%% @doc Stack flow passes unrelated work and consumes every borrowing action.
stack_routing_test() ->
    Opts = test_opts(),
    {PositionOwner, PositionOwnerAddr} = party(),
    Set =
        tx(
            PositionOwner,
            #{
                <<"target">> => ?PROCESS,
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

%% @doc Escrow updates preserve a linked trie ledger across a cache round trip.
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
            position_action(
                PositionOwner,
                <<"cancel-position">>,
                position_id(Opened, Opts)
            ),
            51,
            Opts
        ),
    {ok, ID} = hb_cache:write(Cancelled, Opts),
    {ok, Cached} = hb_cache:read(ID, Opts),
    ?assertEqual(1, balance(Cached, PositionOwnerAddr, Opts)),
    ?assertEqual(not_found, position(Cached, Opts)).
