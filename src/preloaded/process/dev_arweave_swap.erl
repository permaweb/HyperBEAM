%%% @doc A device for selling a process's tokens for native Arweave value.
%%%
%%% The two halves of the trade live in different places, and that asymmetry
%%% is the whole design. The token half lives here: this device debits and
%%% credits the `balances' submessage that a `~token@1.0' implementation keeps
%%% in the same process's base, so it can hold tokens in escrow and pay them
%%% out with certainty. The Arweave half does not live here at all -- AR moves
%%% directly from buyer to seller as an ordinary layer-1 transfer, which no
%%% process can hold, redirect or refund.
%%%
%%% A process using this device is therefore sequenced by
%%% `~arweave-scheduler@1.0' in its `all' mode: every base-layer transaction
%%% becomes a slot, so a payment between two addresses that the process is not
%%% a party to is nonetheless something the process sees, and can settle
%%% against. Its process message reads:
%%% <pre>
%%%     scheduler-device: arweave-scheduler@1.0
%%%     scheduler-mode:   all
%%%     execution-device: arweave-swap@1.0
%%% </pre>
%%%
%%% The protocol is four messages:
%%% <ul>
%%%   <li>`make-offer' (to the process, from the seller), carrying
%%%       `offer-quantity' in token units, `asking' in winston, `deposit' in
%%%       token units and a `deadline' block height. The seller's
%%%       `offer-quantity + deposit' moves into escrow at once, so delivery is
%%%       never in doubt: the goods are already held before any buyer commits
%%%       anything. The offered amount cannot be called `quantity': that is a
%%%       transaction's own value field, so the codec would carry it as winston
%%%       of AR sent to the process -- an address with no key, which would
%%%       destroy it.</li>
%%%   <li>`register-interest' (to the process, from a buyer), naming an
%%%       `order-id'. It moves no value. It buys exclusivity: for
%%%       `swap-reservation-blocks' the order is that buyer's alone and the
%%%       seller cannot cancel it. That window is what makes paying safe --
%%%       without it a buyer races the seller's cancellation, having already
%%%       sent AR that nobody can claw back.</li>
%%%   <li>`cancel-order' (to the process, from the seller), naming an
%%%       `order-id'. Returns the goods, but not the deposit (see below).</li>
%%%   <li>The payment itself: an ordinary transfer whose `target' is the
%%%       order's `recipient', whose `quantity' is at least the `asking'
%%%       winston, tagged with the `order-id'. This is the message the `all'
%%%       mode exists to deliver.</li>
%%% </ul>
%%%
%%% The `deposit' is the seller's bond against a payment the protocol cannot
%%% honour. Once AR has been sent it cannot be returned, so the only lever
%%% remaining is a token-denominated bond, and it outlives the goods: cancelling
%%% or expiring an order releases the goods immediately but holds the `deposit'
%%% until `deadline + swap-cancel-grace'. A payment that lands in that window
%%% against goods that are gone or spoken for is paid the deposit instead. A
%%% seller who strands nobody always gets it back. Neither the order's creator
%%% nor its recipient may pay it: a self-transfer costs only a network fee, and
%%% would otherwise let a seller settle their own order to escape the bond.
%%% Because any other payer may claim that bond, a seller should not post a
%%% deposit worth more than the price they are asking, or paying for it becomes
%%% a trade in itself.
%%%
%%% Deadlines are Arweave block heights, read from the `block-height' that
%%% `all'-mode assignments carry. That is the only clock the device has, and
%%% deliberately so: reading the chain tip during a compute would be
%%% non-deterministic, and `~process@1.0' caches every slot result forever.
-module(dev_arweave_swap).
-implements(<<"arweave-swap@1.0">>).
%%% AO-Core API functions:
-export([info/0, compute/3, set/3, keys/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The balances submessage this device settles in. Owned by the process's
%%% token implementation; the swap only moves value inside it.
-define(BALANCES, <<"balances">>).
%%% The number of blocks an order is reserved for by `register-interest'.
-define(DEFAULT_RESERVATION_BLOCKS, 10).
%%% The number of blocks after an order's deadline during which a payment may
%%% still be compensated from the deposit.
-define(DEFAULT_CANCEL_GRACE, 20).

%% @doc Every state transition in this device is driven by the schedule, never
%% by a direct request, so every key routes to `compute'.
%%
%% The key a slot resolves is chosen by the scheduled transaction's own `path'
%% tag, and this process is sequenced by every transaction on Arweave: whatever
%% a stranger writes there must be applied like any other message. So there is
%% no `exports' list -- a key outside it would fall through to `~message@1.0',
%% answer `not_found', fail its slot and wedge the process permanently -- and no
%% `excludes' list either, since an excluded key is handed to `~message@1.0'
%% instead, whose `set' would let a passer-by write the process's own balances
%% and whose `keys' would replace the state with a list of key names. `set' and
%% `keys' are therefore implemented here.
%%
%% `info' is deliberately arity 0. A device's `info' is always exported, and had
%% it taken the base as an argument it would also answer the `info' key, so a
%% transaction tagged `path: info' would replace the process state with this
%% map.
info() ->
    #{ default => fun router/4 }.

%% @doc Apply any scheduled message, whatever it asked to be routed to.
router(_Key, Base, Assignment, Opts) ->
    compute(Base, Assignment, Opts).

%% @doc Setting the device is honoured, and nothing else is. `lib_process' puts
%% the process's own device back with a `set' after every slot, and reading the
%% state as a message is itself a device set, so refusing those would leave the
%% device unable to read itself. A scheduled message that asks to be routed to
%% `set' is applied like any other message instead of being allowed to write the
%% state directly.
%%
%% The device is written here rather than delegated to `~message@1.0', because
%% delegating means viewing this state as a message, which is another device
%% set: it would arrive back here forever.
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

%% @doc Listing the state's keys is not a state transition, so a scheduled
%% message asking for it is applied as one.
keys(Base, Req, Opts) -> compute(Base, Req, Opts).

%% @doc Apply one assignment to the swap's state.
%%
%% In `all' mode the overwhelming majority of slots are unrelated Arweave
%% traffic, so the classification below is ordered by cost: advance the clock,
%% then compare a single field -- the transaction's `target' -- against the
%% process, and only then look at tags. A transaction that is neither addressed
%% to the process nor a payment against a live order leaves the state exactly as
%% it was.
%% The process is identified from the assignment rather than from the process
%% message, which `lib_process:process_id/3' would re-verify the signature of on
%% every one of the network's transactions.
compute(Base, Assignment, Opts) ->
    Height = hb_util:int(hb_ao:get(<<"block-height">>, Assignment, 0, Opts)),
    ProcID = hb_ao:get(<<"process">>, Assignment, <<>>, Opts),
    Body = hb_ao:get(<<"body">>, Assignment, #{}, Opts),
    Advanced = advance(Base, Height, Opts),
    case tx_field_target(Body, Opts) of
        ProcID -> {ok, control(Advanced, Body, Height, Opts)};
        Target -> {ok, payment(Advanced, Body, Target, Height, Opts)}
    end.

%%% Order lifecycle

%% @doc Route a transaction addressed to the process by its `action'. An
%% unknown action is not an error: in this mode anyone may send the process
%% anything, and a process that could be wedged by a stranger's transaction
%% would not survive its own schedule.
control(Base, Body, Height, Opts) ->
    case hb_ao:get(<<"action">>, Body, <<>>, Opts) of
        <<"make-offer">> -> make_offer(Base, Body, Height, Opts);
        <<"cancel-order">> -> cancel_order(Base, Body, Opts);
        <<"register-interest">> -> register_interest(Base, Body, Height, Opts);
        _ -> Base
    end.

%% @doc Open an order, moving the goods and the seller's bond into escrow.
%% Nothing is written unless the whole offer is admissible, so a rejected offer
%% is indistinguishable from a transaction that was never sent.
make_offer(Base, Body, Height, Opts) ->
    maybe
        {ok, Seller} ?= signer(Body, Opts),
        {ok, Quantity} ?= amount(<<"offer-quantity">>, Body, Opts),
        {ok, Asking} ?= amount(<<"asking">>, Body, Opts),
        {ok, Deposit} ?= amount(<<"deposit">>, Body, Opts),
        {ok, Deadline} ?= amount(<<"deadline">>, Body, Opts),
        Recipient = hb_ao:get(<<"recipient">>, Body, Seller, Opts),
        true ?= Quantity >= 1,
        true ?= Asking >= 1,
        true ?= Deposit >= 0,
        true ?= Deadline > Height,
        true ?= balance(Base, Seller, Opts) >= Quantity + Deposit,
        OrderID = hb_util:human_id(hb_message:id(Body, signed, Opts)),
        not_found ?= hb_maps:get(OrderID, order_book(Base, Opts), not_found, Opts),
        Order =
            #{
                <<"order-id">> => OrderID,
                <<"creator">> => Seller,
                <<"recipient">> => Recipient,
                <<"quantity">> => Quantity,
                <<"asking">> => Asking,
                <<"deposit">> => Deposit,
                <<"deadline">> => Deadline,
                <<"created-at">> => Height,
                <<"status">> => <<"open">>
            },
        ?event(
            {swap_order_opened,
                {order, OrderID},
                {seller, Seller},
                {quantity, Quantity},
                {asking, Asking}
            }
        ),
        note(
            deadlines(
                put_order(
                    debit(Base, Seller, Quantity + Deposit, Opts),
                    Order,
                    Opts
                ),
                Opts
            ),
            <<"order-opened">>,
            OrderID,
            Opts
        )
    else
        _ -> Base
    end.

%% @doc Withdraw an order that nobody has reserved, returning the goods. The
%% deposit stays escrowed until the grace period ends: a payment may already be
%% in flight against this order, and it is the deposit that compensates it.
cancel_order(Base, Body, Opts) ->
    maybe
        {ok, Signer} ?= signer(Body, Opts),
        {ok, Order} ?= find_order(Base, Body, Opts),
        #{ <<"creator">> := Creator, <<"status">> := Status } = Order,
        true ?= Signer =:= Creator,
        true ?= Status =:= <<"open">>,
        ?event({swap_order_cancelled, {order, order_id(Order)}}),
        note(
            deadlines(
                release_goods(Base, Order, Creator, <<"cancelled">>, Opts),
                Opts
            ),
            <<"order-cancelled">>,
            order_id(Order),
            Opts
        )
    else
        _ -> Base
    end.

%% @doc Reserve an open order for the sender. The reservation is exclusive and
%% freezes the seller out of cancelling, which is precisely what makes it safe
%% for the sender to part with their AR.
register_interest(Base, Body, Height, Opts) ->
    maybe
        {ok, Buyer} ?= signer(Body, Opts),
        {ok, Order} ?= find_order(Base, Body, Opts),
        #{ <<"status">> := Status, <<"deadline">> := Deadline } = Order,
        true ?= Status =:= <<"open">>,
        true ?= Height < Deadline,
        Until = min(Deadline, Height + reservation_blocks(Base, Opts)),
        ?event(
            {swap_interest_registered,
                {order, order_id(Order)},
                {buyer, Buyer},
                {until, Until}
            }
        ),
        note(
            deadlines(
                put_order(
                    Base,
                    Order#{
                        <<"status">> => <<"reserved">>,
                        <<"buyer">> => Buyer,
                        <<"reserved-until">> => Until
                    },
                    Opts
                ),
                Opts
            ),
            <<"interest-registered">>,
            order_id(Order),
            Opts
        )
    else
        _ -> Base
    end.

%%% Settlement

%% @doc Settle against a layer-1 payment. The transaction is not addressed to
%% the process at all: it is a transfer between two user addresses that names an
%% `order-id', and the process sees it only because it is sequenced by every
%% transaction on the network. It counts as payment for an order when it is
%% addressed to that order's `recipient' and carries at least the asking
%% winston.
%%
%% Underpayment is ignored rather than partially filled -- the value never
%% passed through the process, so there is nothing to refund the difference
%% from.
payment(Base, Body, Target, Height, Opts) ->
    maybe
        {ok, Buyer} ?= signer(Body, Opts),
        {ok, Order} ?= find_order(Base, Body, Opts),
        #{
            <<"creator">> := Creator,
            <<"recipient">> := Recipient,
            <<"asking">> := Asking,
            <<"deadline">> := Deadline
        } = Order,
        true ?= Target =:= Recipient,
        % A seller paying themselves costs nothing but a network fee, and would
        % otherwise let them settle their own order -- taking the goods back
        % along with the bond, and leaving a real buyer's payment to arrive
        % against an order that is already spent.
        false ?= Buyer =:= Creator,
        false ?= Buyer =:= Recipient,
        {ok, Paid} ?= tx_field_quantity(Body, Opts),
        true ?= Paid >= Asking,
        true ?= Height =< Deadline + cancel_grace(Base, Opts),
        settle(Base, Order, Buyer, Height, Body, Opts)
    else
        _ -> Base
    end.

%% @doc Pay a matched payment. The goods go to the buyer and the bond returns
%% to the seller when the order was theirs to buy; otherwise the buyer takes the
%% bond in compensation, because they have paid for something they cannot
%% receive.
settle(Base, Order, Buyer, Height, Body, Opts) ->
    PaymentID = hb_util:human_id(hb_message:id(Body, signed, Opts)),
    Settled =
        Order#{
            <<"status">> => <<"settled">>,
            <<"settled-at">> => Height,
            <<"payment-tx">> => PaymentID
        },
    case claimable(Order, Buyer, Height) of
        true ->
            #{ <<"creator">> := Creator, <<"quantity">> := Quantity } = Order,
            ?event(
                {swap_order_settled,
                    {order, order_id(Order)},
                    {buyer, Buyer},
                    {quantity, Quantity}
                }
            ),
            note(
                deadlines(
                    put_order(
                        credit(
                            credit(Base, Buyer, Quantity, Opts),
                            Creator,
                            deposit(Order),
                            Opts
                        ),
                        Settled#{ <<"quantity">> => 0, <<"deposit">> => 0 },
                        Opts
                    ),
                    Opts
                ),
                <<"order-settled">>,
                order_id(Order),
                Opts
            );
        false ->
            % The goods are gone or promised to somebody else, but the buyer
            % has already paid. The bond is what they get instead, and it can
            % only be paid out once.
            compensate(Base, Order, Buyer, PaymentID, Opts)
    end.

%% @doc Pay a stranded buyer the seller's bond.
compensate(Base, Order, Buyer, PaymentID, Opts) ->
    case deposit(Order) of
        0 -> Base;
        Deposit ->
            ?event(
                {swap_payment_compensated,
                    {order, order_id(Order)},
                    {buyer, Buyer},
                    {deposit, Deposit}
                }
            ),
            note(
                deadlines(
                    put_order(
                        credit(Base, Buyer, Deposit, Opts),
                        Order#{
                            <<"deposit">> => 0,
                            <<"payment-tx">> => PaymentID
                        },
                        Opts
                    ),
                    Opts
                ),
                <<"payment-compensated">>,
                order_id(Order),
                Opts
            )
    end.

%% @doc Whether an order's goods are the payer's to take: an order nobody has
%% reserved is first-come, and a reserved one is its buyer's until the
%% reservation lapses.
claimable(#{ <<"quantity">> := 0 }, _Buyer, _Height) -> false;
claimable(#{ <<"status">> := <<"open">> }, _Buyer, _Height) -> true;
claimable(Order = #{ <<"status">> := <<"reserved">> }, Buyer, Height) ->
    maps:get(<<"buyer">>, Order, <<>>) =:= Buyer
        andalso Height =< maps:get(<<"reserved-until">>, Order, 0);
claimable(_Order, _Buyer, _Height) -> false.

%%% The clock

%% @doc Advance the process's notion of the chain to the height of the
%% assignment being applied, retiring whatever fell due in between.
%%
%% Every transaction on the network is a slot here, so this runs on the order of
%% a thousand times per block: the common path is one integer comparison against
%% the next height at which anything at all happens, and only crossing it walks
%% the orders.
advance(Base, Height, Opts) ->
    case hb_util:int(state(<<"next-deadline">>, Base, 0, Opts)) of
        Next when Next > 0, Height >= Next ->
            deadlines(sweep(Base, Height, Opts), Opts);
        _ ->
            Base#{ <<"swap-height">> => Height }
    end.

%% @doc Apply every deadline that the given height has reached: reservations
%% lapse, unsold orders return their goods, and orders past their grace period
%% return the residual bond.
sweep(Base, Height, Opts) ->
    lists:foldl(
        fun(Order, Acc) -> expire(Acc, Order, Height, Opts) end,
        Base#{ <<"swap-height">> => Height },
        orders(Base, Opts)
    ).

expire(Base, Order = #{ <<"status">> := <<"reserved">> }, Height, Opts) ->
    case Height > maps:get(<<"reserved-until">>, Order, 0) of
        true ->
            % The reservation has lapsed, so the order is open to anyone again
            % -- and being open, it may be due to expire in this same sweep.
            Reopened =
                maps:without(
                    [<<"buyer">>, <<"reserved-until">>],
                    Order#{ <<"status">> => <<"open">> }
                ),
            expire(put_order(Base, Reopened, Opts), Reopened, Height, Opts);
        false -> Base
    end;
expire(Base, Order = #{ <<"status">> := <<"open">>, <<"deadline">> := Deadline },
        Height, Opts) when Height >= Deadline ->
    ?event({swap_order_expired, {order, order_id(Order)}}),
    release_goods(Base, Order, maps:get(<<"creator">>, Order), <<"expired">>, Opts);
expire(Base, Order, Height, Opts) ->
    % Only the bond is left outstanding, and the window in which a payment
    % could still claim it has closed.
    Deadline = maps:get(<<"deadline">>, Order, 0),
    case
        deposit(Order) > 0 andalso Height > Deadline + cancel_grace(Base, Opts)
    of
        true ->
            ?event({swap_deposit_retired, {order, order_id(Order)}}),
            put_order(
                credit(
                    Base,
                    maps:get(<<"creator">>, Order),
                    deposit(Order),
                    Opts
                ),
                Order#{ <<"deposit">> => 0 },
                Opts
            );
        false -> Base
    end.

%% @doc Record the next height at which any order needs attention, so that the
%% slots in between cost a single comparison. Zero means nothing is pending.
deadlines(Base, Opts) ->
    Grace = cancel_grace(Base, Opts),
    Heights =
        lists:flatten(
            [ order_deadlines(Order, Grace) || Order <- orders(Base, Opts) ]
        ),
    Next =
        case Heights of
            [] -> 0;
            _ -> lists:min(Heights)
        end,
    Base#{ <<"next-deadline">> => Next }.

order_deadlines(Order = #{ <<"status">> := <<"reserved">> }, Grace) ->
    [maps:get(<<"reserved-until">>, Order, 0) + 1
        | order_deadlines(Order#{ <<"status">> => <<"open">> }, Grace)];
order_deadlines(Order = #{ <<"status">> := <<"open">>, <<"deadline">> := D }, Grace) ->
    case quantity(Order) of
        0 -> retirement(Order, Grace);
        _ -> [D | retirement(Order, Grace)]
    end;
order_deadlines(Order, Grace) ->
    retirement(Order, Grace).

retirement(Order, Grace) ->
    case deposit(Order) of
        0 -> [];
        _ -> [maps:get(<<"deadline">>, Order, 0) + Grace + 1]
    end.

%%% State helpers

%% @doc Return an order's goods to its creator, leaving the bond escrowed for
%% whatever grace remains.
release_goods(Base, Order, Creator, Status, Opts) ->
    put_order(
        credit(Base, Creator, quantity(Order), Opts),
        Order#{ <<"quantity">> => 0, <<"status">> => Status },
        Opts
    ).

%% @doc Read a key of the process's own state.
%%
%% While a slot is being computed the state carries this device, so a plain read
%% of one of its keys would resolve that key *through this device* and land back
%% in `compute'. Every read of the state is therefore taken as a message. Writes
%% have the same hazard one level down -- setting a nested path resolves the
%% keys above it on the way -- so the device only ever writes whole top-level
%% keys.
state(Key, Base, Default, Opts) ->
    hb_ao:get(Key, {as, <<"message@1.0">>, Base}, Default, Opts).

%% @doc Read the orders currently held, as plain maps. The state may have been
%% written to the process cache and read back since it was last touched, so it
%% is loaded through the link layer, and anything that is not an order is
%% ignored rather than assumed away.
orders(Base, Opts) ->
    Orders = hb_cache:ensure_all_loaded(order_book(Base, Opts), Opts),
    [
        order(Held)
    ||
        Held <-
            [ hb_maps:get(ID, Orders, #{}, Opts) || ID <- hb_ao:keys(Orders, Opts) ],
        is_map(Held),
        maps:is_key(<<"order-id">>, Held)
    ].

order_book(Base, Opts) -> state(<<"orders">>, Base, #{}, Opts).

%% @doc Read an order with its numbers as numbers. Between slots the state is
%% written to the process cache and read back, so nothing that a comparison or
%% a sum depends on is assumed to have survived as an integer term -- a height
%% that came back as a binary would sort above every integer, and a deadline
%% would then simply never fall due.
order(Held) ->
    lists:foldl(
        fun(Key, Order) ->
            case maps:find(Key, Order) of
                {ok, Value} -> Order#{ Key => hb_util:int(Value) };
                error -> Order
            end
        end,
        Held,
        [
            <<"quantity">>,
            <<"deposit">>,
            <<"asking">>,
            <<"deadline">>,
            <<"created-at">>,
            <<"reserved-until">>,
            <<"settled-at">>
        ]
    ).

%% @doc Read the order a message names, if the process holds it.
%%
%% The name comes from a stranger's transaction, so it is looked up as a key of
%% the order book rather than resolved as a path: a path would let
%% `order-id: <id>/creator' reach an address, and a reserved name like `keys'
%% reach a list, either of which would then be read as an order and fail the
%% slot. Whatever comes back must look like an order before it is treated as
%% one.
find_order(Base, Body, Opts) ->
    Held =
        hb_maps:get(
            hb_ao:get(<<"order-id">>, Body, <<>>, Opts),
            order_book(Base, Opts),
            not_found,
            Opts
        ),
    case hb_cache:ensure_all_loaded(Held, Opts) of
        Order when is_map(Order) ->
            case maps:is_key(<<"order-id">>, Order) of
                true -> {ok, order(Order)};
                false -> not_found
            end;
        _ -> not_found
    end.

%% @doc Write an order back, replacing the one held rather than merging over
%% it, so that a lapsed reservation leaves no buyer behind.
put_order(Base, Order, Opts) ->
    Base#{
        <<"orders">> =>
            hb_maps:put(
                order_id(Order),
                Order,
                order_book(Base, Opts),
                Opts
            )
    }.

order_id(Order) -> maps:get(<<"order-id">>, Order).

quantity(Order) -> hb_util:int(maps:get(<<"quantity">>, Order, 0)).

deposit(Order) -> hb_util:int(maps:get(<<"deposit">>, Order, 0)).

%% @doc Read an address's token balance from the ledger this process shares.
%% Only the one entry is read: the ledger may be large, and the rest of it is
%% none of this device's business.
balance(Base, Address, Opts) ->
    hb_util:int(state([?BALANCES, Address], Base, 0, Opts)).

credit(Base, _Address, 0, _Opts) -> Base;
credit(Base, Address, Amount, Opts) ->
    settle_balance(Base, Address, balance(Base, Address, Opts) + Amount, Opts).

debit(Base, Address, Amount, Opts) ->
    settle_balance(Base, Address, balance(Base, Address, Opts) - Amount, Opts).

settle_balance(Base, Address, Value, Opts) ->
    Base#{
        ?BALANCES =>
            hb_maps:put(
                Address,
                Value,
                state(?BALANCES, Base, #{}, Opts),
                Opts
            )
    }.

%% @doc Report what the slot did, in the results of the slot itself.
note(Base, Event, OrderID, _Opts) ->
    Base#{
        <<"results">> => #{ <<"event">> => Event, <<"order-id">> => OrderID }
    }.

%% @doc Read a number a stranger wrote. Every figure in the protocol arrives as
%% a tag on somebody else's transaction, and this process is sequenced by all of
%% them: coercing `deadline: tomorrow' with `hb_util:int/1' would raise out of
%% the enclosing `maybe' -- which catches mismatches, not exceptions -- and fail
%% that slot on every node, for good. A value that is not a number is simply not
%% an admissible message.
amount(Key, Body, Opts) ->
    hb_util:safe_int(hb_ao:get(Key, Body, 0, Opts)).

%% @doc Read a value from the real L1 transaction fields recorded in the
%% `tx@1.0' commitment. Top-level keys may come from tags with the same names, so
%% payment routing and amount checks must not use them.
tx_field(Body, Field, Default, Opts) ->
    case hb_message:commitment(#{ <<"commitment-device">> => <<"tx@1.0">> }, Body, Opts) of
        {ok, _ID, Commitment} ->
            hb_maps:get(<<"field-", Field/binary>>, Commitment, Default, Opts);
        _ ->
            Default
    end.

tx_field_target(Body, Opts) ->
    tx_field(Body, <<"target">>, <<>>, Opts).

tx_field_quantity(Body, Opts) ->
    hb_util:safe_int(tx_field(Body, <<"quantity">>, 0, Opts)).

%% @doc The single signer of a message. A message with any other number of
%% signers is not attributable to one party, so it cannot open, cancel, reserve
%% or pay for anything.
signer(Body, Opts) ->
    case hb_message:signers(Body, Opts) of
        [Signer] -> {ok, hb_util:human_id(Signer)};
        _ -> not_found
    end.

reservation_blocks(Base, Opts) ->
    hb_util:int(
        state(
            <<"swap-reservation-blocks">>,
            Base,
            ?DEFAULT_RESERVATION_BLOCKS,
            Opts
        )
    ).

cancel_grace(Base, Opts) ->
    hb_util:int(
        state(<<"swap-cancel-grace">>, Base, ?DEFAULT_CANCEL_GRACE, Opts)
    ).

%%% Tests

%%% The tests drive `compute/3' directly with synthetic assignments, exactly as
%%% `~process@1.0' would: the device reads chain data but performs no I/O, so a
%%% whole trade can be played out without a weave.

-define(PROCESS, <<"pRoCeSs000000000000000000000000000000000000">>).

test_opts() -> #{ <<"priv-wallet">> => ar_wallet:new() }.

%% @doc A party to a trade: a wallet and the address it signs as.
party() ->
    Wallet = ar_wallet:new(),
    {Wallet, hb_util:human_id(ar_wallet:to_address(Wallet))}.

%% @doc A process base holding the given balances and nothing else.
base(Balances) ->
    #{ ?BALANCES => Balances }.

%% @doc An L1 transaction, committed as the base layer commits them.
tx(Wallet, Fields) ->
    hb_message:commit(
        Fields,
        #{ <<"priv-wallet">> => Wallet },
        #{ <<"commitment-device">> => <<"tx@1.0">> }
    ).

%% @doc A transaction that carries trade keys as tags only.
tag_only_tx(Wallet, Tags) ->
    Signed = ar_tx:sign(#tx{ format = 2, reward = 1, tags = Tags }, Wallet),
    hb_message:convert(Signed, <<"structured@1.0">>, <<"tx@1.0">>, #{}).

%% @doc Sequence a transaction into the process at a block height, as
%% `~arweave-scheduler@1.0' in `all' mode does.
apply_tx(Base, Body, Height, Opts) ->
    {ok, New} =
        compute(
            Base,
            #{
                <<"process">> => ?PROCESS,
                <<"slot">> => 1,
                <<"block-height">> => Height,
                <<"body">> => Body
            },
            Opts
        ),
    New.

%% @doc Advance the process to a height without anything happening, which is
%% what the network's unrelated traffic does.
tick(Base, Height, Opts) ->
    apply_tx(Base, #{ <<"target">> => <<"someone-else">> }, Height, Opts).

offer(Wallet, Quantity, Asking, Deposit, Deadline) ->
    tx(
        Wallet,
        #{
            <<"target">> => ?PROCESS,
            <<"action">> => <<"make-offer">>,
            <<"offer-quantity">> => hb_util:bin(Quantity),
            <<"asking">> => hb_util:bin(Asking),
            <<"deposit">> => hb_util:bin(Deposit),
            <<"deadline">> => hb_util:bin(Deadline)
        }
    ).

order_action(Wallet, Action, OrderID) ->
    tx(
        Wallet,
        #{
            <<"target">> => ?PROCESS,
            <<"action">> => Action,
            <<"order-id">> => OrderID
        }
    ).

%% @doc The payment leg: an ordinary transfer to the seller that the process is
%% not addressed by, naming the order it settles.
pay(Wallet, To, Winston, OrderID) ->
    tx(
        Wallet,
        #{
            <<"target">> => To,
            <<"quantity">> => hb_util:bin(Winston),
            <<"order-id">> => OrderID
        }
    ).

tag_only_transfer(Wallet, To, Winston, OrderID) ->
    tag_only_tx(
        Wallet,
        [
            {<<"target">>, To},
            {<<"quantity">>, hb_util:bin(Winston)},
            {<<"order-id">>, OrderID}
        ]
    ).

only_order(Base, Opts) ->
    [Order] = orders(Base, Opts),
    Order.

balance_of(Base, Address, Opts) -> balance(Base, Address, Opts).

%% @doc Opening an offer moves the goods and the bond into escrow, and leaves
%% the order open.
make_offer_escrows_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    Base = base(#{ SellerAddr => 100 }),
    Opened = apply_tx(Base, offer(Seller, 10, 500, 5, 200), 100, Opts),
    Order = only_order(Opened, Opts),
    ?assertEqual(85, balance_of(Opened, SellerAddr, Opts)),
    ?assertEqual(<<"open">>, maps:get(<<"status">>, Order)),
    ?assertEqual(10, quantity(Order)),
    ?assertEqual(5, deposit(Order)),
    ?assertEqual(SellerAddr, maps:get(<<"recipient">>, Order)).

%% @doc An offer for more than the seller holds changes nothing at all.
make_offer_insufficient_balance_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    Base = base(#{ SellerAddr => 4 }),
    Result = apply_tx(Base, offer(Seller, 10, 500, 5, 200), 100, Opts),
    ?assertEqual([], orders(Result, Opts)),
    ?assertEqual(4, balance_of(Result, SellerAddr, Opts)).

%% @doc A deadline that has already passed is not an offer.
make_offer_stale_deadline_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    Base = base(#{ SellerAddr => 100 }),
    Result = apply_tx(Base, offer(Seller, 10, 500, 5, 100), 100, Opts),
    ?assertEqual([], orders(Result, Opts)).

%% @doc Tag-only trade keys are metadata and do not route control slots.
tag_only_target_is_metadata_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    Tagged =
        tag_only_tx(
            Seller,
            [
                {<<"target">>, ?PROCESS},
                {<<"action">>, <<"make-offer">>},
                {<<"offer-quantity">>, <<"10">>},
                {<<"asking">>, <<"500">>},
                {<<"deposit">>, <<"5">>},
                {<<"deadline">>, <<"200">>}
            ]
        ),
    ?assertEqual(?PROCESS, hb_ao:get(<<"target">>, Tagged, not_found, Opts)),
    ?assertEqual(<<>>, tx_field_target(Tagged, Opts)),
    Result = apply_tx(base(#{ SellerAddr => 100 }), Tagged, 100, Opts),
    ?assertEqual([], orders(Result, Opts)),
    ?assertEqual(100, balance_of(Result, SellerAddr, Opts)).

%% @doc The whole trade: the buyer pays the seller directly on layer one, and
%% the process -- which is not a party to that payment -- settles it.
settlement_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    Base = base(#{ SellerAddr => 100 }),
    Opened = apply_tx(Base, offer(Seller, 10, 500, 5, 200), 100, Opts),
    OrderID = order_id(only_order(Opened, Opts)),
    Settled =
        apply_tx(
            Opened,
            pay(Buyer, SellerAddr, 500, OrderID),
            120,
            Opts
        ),
    Order = only_order(Settled, Opts),
    ?assertEqual(<<"settled">>, maps:get(<<"status">>, Order)),
    % The buyer has the goods; the seller has their bond back and is out the
    % tokens they sold.
    ?assertEqual(10, balance_of(Settled, BuyerAddr, Opts)),
    ?assertEqual(90, balance_of(Settled, SellerAddr, Opts)).

%% @doc Paying less than the asking price settles nothing: the process never
%% held the value, so it cannot refund a partial fill.
underpayment_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    Opened =
        apply_tx(base(#{ SellerAddr => 100 }), offer(Seller, 10, 500, 5, 200), 100, Opts),
    OrderID = order_id(only_order(Opened, Opts)),
    Result = apply_tx(Opened, pay(Buyer, SellerAddr, 499, OrderID), 120, Opts),
    ?assertEqual(<<"open">>, maps:get(<<"status">>, only_order(Result, Opts))),
    ?assertEqual(0, balance_of(Result, BuyerAddr, Opts)).

%% @doc A payment that names the order but is addressed to somebody else is not
%% a payment for it.
payment_to_wrong_address_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    {_, Stranger} = party(),
    Opened =
        apply_tx(base(#{ SellerAddr => 100 }), offer(Seller, 10, 500, 5, 200), 100, Opts),
    OrderID = order_id(only_order(Opened, Opts)),
    Result = apply_tx(Opened, pay(Buyer, Stranger, 500, OrderID), 120, Opts),
    ?assertEqual(<<"open">>, maps:get(<<"status">>, only_order(Result, Opts))),
    ?assertEqual(0, balance_of(Result, BuyerAddr, Opts)).

%% @doc Tag-only trade keys are metadata and do not count as the payment leg.
tag_only_transfer_is_metadata_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    Opened =
        apply_tx(base(#{ SellerAddr => 100 }), offer(Seller, 10, 500, 5, 200), 100, Opts),
    OrderID = order_id(only_order(Opened, Opts)),
    Tagged = tag_only_transfer(Buyer, SellerAddr, 500, OrderID),
    ?assertEqual(SellerAddr, hb_ao:get(<<"target">>, Tagged, not_found, Opts)),
    ?assertEqual(<<"500">>, hb_ao:get(<<"quantity">>, Tagged, not_found, Opts)),
    ?assertEqual(<<>>, tx_field_target(Tagged, Opts)),
    ?assertEqual({ok, 0}, tx_field_quantity(Tagged, Opts)),
    Result = apply_tx(Opened, Tagged, 120, Opts),
    ?assertEqual(<<"open">>, maps:get(<<"status">>, only_order(Result, Opts))),
    ?assertEqual(0, balance_of(Result, BuyerAddr, Opts)),
    ?assertEqual(85, balance_of(Result, SellerAddr, Opts)).

%% @doc Cancelling returns the goods at once, but holds the bond for as long as
%% a payment could still be in flight.
cancel_returns_goods_but_holds_bond_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    Opened =
        apply_tx(base(#{ SellerAddr => 100 }), offer(Seller, 10, 500, 5, 200), 100, Opts),
    OrderID = order_id(only_order(Opened, Opts)),
    Cancelled =
        apply_tx(Opened, order_action(Seller, <<"cancel-order">>, OrderID), 110, Opts),
    Order = only_order(Cancelled, Opts),
    ?assertEqual(<<"cancelled">>, maps:get(<<"status">>, Order)),
    ?assertEqual(0, quantity(Order)),
    ?assertEqual(5, deposit(Order)),
    ?assertEqual(95, balance_of(Cancelled, SellerAddr, Opts)),
    % Once the grace period has passed with no payment, the bond comes back.
    Retired = tick(Cancelled, 200 + ?DEFAULT_CANCEL_GRACE + 1, Opts),
    ?assertEqual(0, deposit(only_order(Retired, Opts))),
    ?assertEqual(100, balance_of(Retired, SellerAddr, Opts)).

%% @doc Only the seller may cancel their order.
cancel_by_stranger_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Stranger, _} = party(),
    Opened =
        apply_tx(base(#{ SellerAddr => 100 }), offer(Seller, 10, 500, 5, 200), 100, Opts),
    OrderID = order_id(only_order(Opened, Opts)),
    Result =
        apply_tx(Opened, order_action(Stranger, <<"cancel-order">>, OrderID), 110, Opts),
    ?assertEqual(<<"open">>, maps:get(<<"status">>, only_order(Result, Opts))).

%% @doc A reserved order cannot be pulled out from under the buyer who reserved
%% it. This is the guarantee that makes paying safe.
reservation_blocks_cancellation_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, _} = party(),
    Opened =
        apply_tx(base(#{ SellerAddr => 100 }), offer(Seller, 10, 500, 5, 200), 100, Opts),
    OrderID = order_id(only_order(Opened, Opts)),
    Reserved =
        apply_tx(
            Opened,
            order_action(Buyer, <<"register-interest">>, OrderID),
            110,
            Opts
        ),
    ?assertEqual(<<"reserved">>, maps:get(<<"status">>, only_order(Reserved, Opts))),
    Attempted =
        apply_tx(
            Reserved,
            order_action(Seller, <<"cancel-order">>, OrderID),
            111,
            Opts
        ),
    ?assertEqual(<<"reserved">>, maps:get(<<"status">>, only_order(Attempted, Opts))).

%% @doc A reservation is exclusive while it lasts: somebody else's payment does
%% not take the goods, it takes the bond.
reservation_is_exclusive_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, _} = party(),
    {Interloper, InterloperAddr} = party(),
    Opened =
        apply_tx(base(#{ SellerAddr => 100 }), offer(Seller, 10, 500, 5, 200), 100, Opts),
    OrderID = order_id(only_order(Opened, Opts)),
    Reserved =
        apply_tx(Opened, order_action(Buyer, <<"register-interest">>, OrderID), 110, Opts),
    Result = apply_tx(Reserved, pay(Interloper, SellerAddr, 500, OrderID), 111, Opts),
    Order = only_order(Result, Opts),
    ?assertEqual(<<"reserved">>, maps:get(<<"status">>, Order)),
    ?assertEqual(10, quantity(Order)),
    ?assertEqual(0, deposit(Order)),
    ?assertEqual(5, balance_of(Result, InterloperAddr, Opts)).

%% @doc A reservation lapses on its own, without anybody sending anything: the
%% network's own traffic carries the clock forward.
reservation_lapses_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, _} = party(),
    Opened =
        apply_tx(base(#{ SellerAddr => 100 }), offer(Seller, 10, 500, 5, 200), 100, Opts),
    OrderID = order_id(only_order(Opened, Opts)),
    Reserved =
        apply_tx(Opened, order_action(Buyer, <<"register-interest">>, OrderID), 110, Opts),
    ?assertEqual(
        110 + ?DEFAULT_RESERVATION_BLOCKS,
        maps:get(<<"reserved-until">>, only_order(Reserved, Opts))
    ),
    Lapsed = tick(Reserved, 110 + ?DEFAULT_RESERVATION_BLOCKS + 1, Opts),
    ?assertEqual(<<"open">>, maps:get(<<"status">>, only_order(Lapsed, Opts))).

%% @doc An unsold order returns its goods when its deadline passes.
expiry_returns_goods_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    Opened =
        apply_tx(base(#{ SellerAddr => 100 }), offer(Seller, 10, 500, 5, 200), 100, Opts),
    ?assertEqual(85, balance_of(Opened, SellerAddr, Opts)),
    Expired = tick(Opened, 200, Opts),
    Order = only_order(Expired, Opts),
    ?assertEqual(<<"expired">>, maps:get(<<"status">>, Order)),
    ?assertEqual(95, balance_of(Expired, SellerAddr, Opts)),
    ?assertEqual(5, deposit(Order)).

%% @doc A buyer who pays for an expired order within the grace period is paid
%% the seller's bond: they parted with value for goods that were gone.
late_payment_takes_the_bond_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    Opened =
        apply_tx(base(#{ SellerAddr => 100 }), offer(Seller, 10, 500, 5, 200), 100, Opts),
    OrderID = order_id(only_order(Opened, Opts)),
    Expired = tick(Opened, 201, Opts),
    Paid = apply_tx(Expired, pay(Buyer, SellerAddr, 500, OrderID), 202, Opts),
    ?assertEqual(5, balance_of(Paid, BuyerAddr, Opts)),
    ?assertEqual(0, deposit(only_order(Paid, Opts))),
    ?assertEqual(95, balance_of(Paid, SellerAddr, Opts)).

%% @doc The bond is paid out once. A second late payment gets nothing, because
%% there is nothing left to compensate it with.
bond_pays_out_once_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {First, FirstAddr} = party(),
    {Second, SecondAddr} = party(),
    Opened =
        apply_tx(base(#{ SellerAddr => 100 }), offer(Seller, 10, 500, 5, 200), 100, Opts),
    OrderID = order_id(only_order(Opened, Opts)),
    Expired = tick(Opened, 201, Opts),
    Once = apply_tx(Expired, pay(First, SellerAddr, 500, OrderID), 202, Opts),
    Twice = apply_tx(Once, pay(Second, SellerAddr, 500, OrderID), 203, Opts),
    ?assertEqual(5, balance_of(Twice, FirstAddr, Opts)),
    ?assertEqual(0, balance_of(Twice, SecondAddr, Opts)).

%% @doc Settling twice is not possible: the goods left with the first payment.
double_settlement_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    {Late, LateAddr} = party(),
    Opened =
        apply_tx(base(#{ SellerAddr => 100 }), offer(Seller, 10, 500, 5, 200), 100, Opts),
    OrderID = order_id(only_order(Opened, Opts)),
    Settled = apply_tx(Opened, pay(Buyer, SellerAddr, 500, OrderID), 120, Opts),
    Again = apply_tx(Settled, pay(Late, SellerAddr, 500, OrderID), 121, Opts),
    ?assertEqual(10, balance_of(Again, BuyerAddr, Opts)),
    ?assertEqual(0, balance_of(Again, LateAddr, Opts)),
    ?assertEqual(90, balance_of(Again, SellerAddr, Opts)).

%% @doc A payment naming an order the process has never heard of is ordinary
%% Arweave traffic, and is ignored.
unknown_order_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, _} = party(),
    Opened =
        apply_tx(base(#{ SellerAddr => 100 }), offer(Seller, 10, 500, 5, 200), 100, Opts),
    Result =
        apply_tx(
            Opened,
            pay(Buyer, SellerAddr, 500, <<"nOtAnOrDeR0000000000000000000000000000000000">>),
            120,
            Opts
        ),
    ?assertEqual(<<"open">>, maps:get(<<"status">>, only_order(Result, Opts))).

%% @doc Unrelated traffic -- the overwhelming majority of what this process is
%% sequenced by -- moves the clock and nothing else.
unrelated_traffic_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    Opened =
        apply_tx(base(#{ SellerAddr => 100 }), offer(Seller, 10, 500, 5, 200), 100, Opts),
    Ticked = tick(Opened, 150, Opts),
    ?assertEqual(150, hb_ao:get(<<"swap-height">>, Ticked, 0, Opts)),
    ?assertEqual(<<"open">>, maps:get(<<"status">>, only_order(Ticked, Opts))),
    ?assertEqual(85, balance_of(Ticked, SellerAddr, Opts)).

%% @doc A stranger's transaction may ask to be routed anywhere -- the key a slot
%% resolves comes from the sender's own `path' tag, and this process is
%% sequenced by every transaction on Arweave. It must be applied like any other,
%% not fail its slot.
stray_path_is_applied_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Stranger, _} = party(),
    Opened =
        apply_tx(base(#{ SellerAddr => 100 }), offer(Seller, 10, 500, 5, 200), 100, Opts),
    Strayed =
        hb_ao:resolve(
            Opened#{ <<"device">> => <<"arweave-swap@1.0">> },
            #{
                <<"path">> => <<"withdraw-everything">>,
                <<"process">> => ?PROCESS,
                <<"slot">> => 2,
                <<"block-height">> => 150,
                <<"body">> =>
                    tx(Stranger, #{ <<"target">> => <<"somebody-else">> })
            },
            Opts
        ),
    ?assertMatch({ok, _}, Strayed),
    {ok, State} = Strayed,
    % The state still carries this device here, because the test resolves it
    % directly rather than through `lib_process:run_as', which puts the
    % process's own device back afterwards. Read it as a message.
    ?assertEqual(150, hb_util:int(state(<<"swap-height">>, State, 0, Opts))),
    ?assertEqual(85, balance_of(State, SellerAddr, Opts)).

%% @doc An order read back with its numbers encoded as binaries -- which is how
%% it returns from the process cache -- still falls due. Comparing a height
%% against a binary would silently never fire.
encoded_order_still_expires_test() ->
    Opts = test_opts(),
    {_, SellerAddr} = party(),
    Encoded =
        #{
            ?BALANCES => #{ SellerAddr => 85 },
            <<"next-deadline">> => <<"200">>,
            <<"orders">> =>
                #{
                    <<"order-1">> =>
                        #{
                            <<"order-id">> => <<"order-1">>,
                            <<"creator">> => SellerAddr,
                            <<"recipient">> => SellerAddr,
                            <<"quantity">> => <<"10">>,
                            <<"asking">> => <<"500">>,
                            <<"deposit">> => <<"5">>,
                            <<"deadline">> => <<"200">>,
                            <<"status">> => <<"open">>
                        }
                }
        },
    Expired = tick(Encoded, 200, Opts),
    ?assertEqual(<<"expired">>, maps:get(<<"status">>, only_order(Expired, Opts))),
    ?assertEqual(95, balance_of(Expired, SellerAddr, Opts)).

%% @doc A reservation that lapses in the same sweep that its order expires is
%% resolved in order, and leaves no stale buyer behind.
lapse_and_expire_together_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, _} = party(),
    Opened =
        apply_tx(base(#{ SellerAddr => 100 }), offer(Seller, 10, 500, 5, 195), 100, Opts),
    OrderID = order_id(only_order(Opened, Opts)),
    Reserved =
        apply_tx(Opened, order_action(Buyer, <<"register-interest">>, OrderID), 190, Opts),
    % Reserved to 195 (capped at the deadline); at 196 both are due.
    Swept = tick(Reserved, 196, Opts),
    Order = only_order(Swept, Opts),
    ?assertEqual(<<"expired">>, maps:get(<<"status">>, Order)),
    ?assertEqual(false, maps:is_key(<<"buyer">>, Order)),
    ?assertEqual(95, balance_of(Swept, SellerAddr, Opts)).

%% @doc The offered amount is not carried as `quantity'. That key is the
%% transaction's own value field, so the codec would send it as winston of AR to
%% the process -- an address with no key. An offer that uses it opens nothing.
offer_quantity_is_not_winston_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    Wrong =
        tx(
            Seller,
            #{
                <<"target">> => ?PROCESS,
                <<"action">> => <<"make-offer">>,
                <<"quantity">> => <<"10">>,
                <<"asking">> => <<"500">>,
                <<"deposit">> => <<"5">>,
                <<"deadline">> => <<"200">>
            }
        ),
    Result = apply_tx(base(#{ SellerAddr => 100 }), Wrong, 100, Opts),
    ?assertEqual([], orders(Result, Opts)),
    ?assertEqual(100, balance_of(Result, SellerAddr, Opts)).

%% @doc A figure that is not a number is an inadmissible message, not a failed
%% slot: anyone may send this process anything, and a slot that raises can never
%% be recomputed.
non_numeric_tag_is_ignored_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    Nonsense =
        tx(
            Seller,
            #{
                <<"target">> => ?PROCESS,
                <<"action">> => <<"make-offer">>,
                <<"offer-quantity">> => <<"10">>,
                <<"asking">> => <<"500">>,
                <<"deposit">> => <<"5">>,
                <<"deadline">> => <<"tomorrow">>
            }
        ),
    Result = apply_tx(base(#{ SellerAddr => 100 }), Nonsense, 100, Opts),
    ?assertEqual([], orders(Result, Opts)),
    ?assertEqual(100, balance_of(Result, SellerAddr, Opts)).

%% @doc An `order-id' is caller-supplied text. Path-like values and reserved
%% keys must not reach anything that is then read as an order.
reserved_order_id_names_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, _} = party(),
    Opened =
        apply_tx(base(#{ SellerAddr => 100 }), offer(Seller, 10, 500, 5, 200), 100, Opts),
    OrderID = order_id(only_order(Opened, Opts)),
    Names =
        [
            <<OrderID/binary, "/creator">>,
            <<"keys">>,
            <<"id">>,
            <<"commitments">>
        ],
    lists:foreach(
        fun(Name) ->
            Result = apply_tx(Opened, pay(Buyer, SellerAddr, 500, Name), 120, Opts),
            ?assertEqual(
                <<"open">>,
                maps:get(<<"status">>, only_order(Result, Opts))
            )
        end,
        Names
    ).

%% @doc A seller cannot buy their own order. Paying oneself costs only a network
%% fee, so it would otherwise take back the goods and the bond, leaving a real
%% buyer to pay for an order that is already spent.
seller_cannot_settle_own_order_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    Opened =
        apply_tx(base(#{ SellerAddr => 100 }), offer(Seller, 10, 500, 5, 200), 100, Opts),
    OrderID = order_id(only_order(Opened, Opts)),
    Result = apply_tx(Opened, pay(Seller, SellerAddr, 500, OrderID), 120, Opts),
    ?assertEqual(<<"open">>, maps:get(<<"status">>, only_order(Result, Opts))),
    ?assertEqual(85, balance_of(Result, SellerAddr, Opts)).

%% @doc Nor can they claim their own bond once the order has expired.
seller_cannot_claim_own_bond_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    Opened =
        apply_tx(base(#{ SellerAddr => 100 }), offer(Seller, 10, 500, 5, 200), 100, Opts),
    OrderID = order_id(only_order(Opened, Opts)),
    Expired = tick(Opened, 201, Opts),
    Result = apply_tx(Expired, pay(Seller, SellerAddr, 500, OrderID), 202, Opts),
    ?assertEqual(5, deposit(only_order(Result, Opts))),
    ?assertEqual(95, balance_of(Result, SellerAddr, Opts)).

%% @doc A scheduled message routed to `set' or `keys' is applied like any other.
%% Handing either to `~message@1.0' would let a passer-by write the process's own
%% balances, or replace its state with a list of key names.
reserved_paths_are_applied_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Stranger, _} = party(),
    Opened =
        apply_tx(base(#{ SellerAddr => 100 }), offer(Seller, 10, 500, 5, 200), 100, Opts),
    lists:foreach(
        fun(Path) ->
            {ok, State} =
                hb_ao:resolve(
                    Opened#{ <<"device">> => <<"arweave-swap@1.0">> },
                    #{
                        <<"path">> => Path,
                        <<"process">> => ?PROCESS,
                        <<"slot">> => 2,
                        <<"block-height">> => 150,
                        <<"balances">> => #{ SellerAddr => 10000000 },
                        <<"body">> =>
                            tx(Stranger, #{ <<"target">> => <<"somebody-else">> })
                    },
                    Opts
                ),
            ?assertEqual(85, balance_of(State, SellerAddr, Opts)),
            ?assertEqual(
                <<"open">>,
                maps:get(<<"status">>, only_order(State, Opts))
            )
        end,
        [<<"set">>, <<"keys">>, <<"info">>]
    ).

%% @doc The next height at which anything happens is recorded, so that the
%% slots in between cost a single comparison rather than a walk of the orders.
next_deadline_tracked_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, _} = party(),
    Opened =
        apply_tx(base(#{ SellerAddr => 100 }), offer(Seller, 10, 500, 5, 200), 100, Opts),
    ?assertEqual(200, hb_ao:get(<<"next-deadline">>, Opened, 0, Opts)),
    OrderID = order_id(only_order(Opened, Opts)),
    Reserved =
        apply_tx(Opened, order_action(Buyer, <<"register-interest">>, OrderID), 110, Opts),
    % The reservation lapses before the deadline, so it is the next event.
    ?assertEqual(
        110 + ?DEFAULT_RESERVATION_BLOCKS + 1,
        hb_ao:get(<<"next-deadline">>, Reserved, 0, Opts)
    ).
