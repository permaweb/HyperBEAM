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
%%% `~arweave-scheduler@1.0' in its `all' mode: every data-free base-layer
%%% transaction becomes a slot, so a payment between two addresses that the
%%% process is not a party to is nonetheless something the process sees, and
%%% can settle against. Its process message reads:
%%% <pre>
%%%     scheduler-device: arweave-scheduler@1.0
%%%     scheduler-mode:   all
%%%     execution-device: arweave-swap@1.0
%%% </pre>
%%%
%%% The protocol is four messages:
%%% <ul>
%%%   <li>`make-offer' (to the process, from the seller), carrying
%%%       `offer-quantity' in token units, `asking' in winston, and what the
%%%       seller asks of a buyer to reserve it: a `minimum-fee' in winston, a
%%%       `deposit' in token units, and a `deadline' in blocks. Any of the
%%%       three may be zero. The `offer-quantity' moves into escrow at once, so
%%%       delivery is never in doubt: the goods are held before any buyer
%%%       commits anything, and the seller stakes nothing further. The offered
%%%       amount cannot be called `quantity': that is a transaction's own value
%%%       field, so the codec would carry it as winston of AR sent to the
%%%       process -- an address with no key, which would destroy it.</li>
%%%   <li>`register-interest' (to the process, from a buyer), naming an
%%%       `order-id'. It buys the exclusive right to complete the sale: for
%%%       `deadline' blocks the order is that buyer's alone and the seller
%%%       cannot withdraw it. That window is what makes paying safe -- without
%%%       it a buyer races the seller's withdrawal, having already sent AR that
%%%       nobody can claw back. Registering pays the order's `minimum-fee' as
%%%       the registration's own transaction `reward', which sends it where
%%%       Arweave sends rewards -- to miners and the endowment -- rather than
%%%       stranding it at the process's address, which is a transaction id with
%%%       no key behind it. Being denominated in AR it asks nothing of a buyer
%%%       who holds none of the token.</li>
%%%   <li>`cancel-order' (to the process, from the seller), naming an
%%%       `order-id'. Returns the escrowed goods and removes the offer. It is
%%%       refused while a reservation stands.</li>
%%%   <li>The payment itself: an ordinary transfer whose `target' is the
%%%       order's `recipient', whose `quantity' is at least the `asking'
%%%       winston, tagged with the `order-id'. This is the message the `all'
%%%       mode exists to deliver.</li>
%%% </ul>
%%%
%%% An offer does not expire: it stands until its seller withdraws it or a
%%% buyer completes it.
%%%
%%% The `deposit' is the <em>buyer's</em> collateral against taking exclusivity
%%% and then abandoning it -- pledged when they register, returned when they
%%% pay, forfeit to the seller when their reservation lapses unpaid. It is
%%% denominated in the process's own token because that is the only thing this
%%% device can decline to honour: a registration naming an offer that is
%%% already gone takes nothing. AR could not serve, because AR settles on the
%%% weave whatever any process decides.
%%%
%%% Paying an open offer without registering first is permitted and unwise:
%%% the seller may withdraw, or another buyer complete it, while the payment is
%%% in flight.
%%%
%%% The book holds live offers only, so opening an offer and withdrawing it
%%% leaves the process exactly as it was. Escrowed goods, pledged collateral
%%% and balances always sum to the supply.
%%%
%%% Reservations are measured in Arweave block heights, read from the
%%% `block-height' that `all'-mode assignments carry. That is the only clock
%%% the device has, and deliberately so: reading the chain tip during a compute
%%% would be non-deterministic, and `~process@1.0' caches every slot result
%%% forever.
-module(dev_arweave_swap).
-implements(<<"arweave-swap@1.0">>).
%%% AO-Core API functions:
-export([info/0, compute/3, set/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The balances submessage this device settles in. Owned by the process's
%%% token implementation; the swap only moves value inside it.
-define(BALANCES, <<"balances">>).
%%% How long a reservation lasts when an offer does not say, counted in blocks
%%% from the buyer's own `register-interest'.
-define(DEFAULT_DEADLINE, 20).

%% @doc Every state transition in this device is driven by the schedule, never
%% by a direct request, so every key routes to `compute'.
%%
%% The key a slot resolves is chosen by the scheduled transaction's own `path'
%% tag, and this process is sequenced by every data-free transaction on Arweave:
%% whatever a stranger writes there must be applied like any other message. So
%% there is no `exports' list -- a key outside it would fall through to `~message@1.0',
%% answer `not_found', fail its slot and wedge the process permanently -- and no
%% `excludes' list either, since an excluded key is handed to `~message@1.0'
%% instead, whose `set' would let a passer-by write the process's own balances
%% and whose `keys' would replace the state with a list of key names. `set' is
%% therefore implemented here.
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
%% every data-free transaction.
compute(Base, Assignment, Opts) ->
    Height = hb_util:int(field(<<"block-height">>, Assignment, 0, Opts)),
    ProcID = field(<<"process">>, Assignment, <<>>, Opts),
    Body = field(<<"body">>, Assignment, #{}, Opts),
    Advanced = advance(Base, Height, Opts),
    case tx_field(Body, <<"target">>, <<>>, Opts) of
        ProcID -> {ok, control(Advanced, Body, Height, Opts)};
        Target -> {ok, payment(Advanced, Body, Target, Height, Opts)}
    end.

%%% Order lifecycle

%% @doc Route a transaction addressed to the process by its `action'. An
%% unknown action is not an error: in this mode anyone may send the process
%% anything, and a process that could be wedged by a stranger's transaction
%% would not survive its own schedule.
control(Base, Body, Height, Opts) ->
    case field(<<"action">>, Body, <<>>, Opts) of
        <<"make-offer">> -> make_offer(Base, Body, Height, Opts);
        <<"cancel-order">> -> cancel_order(Base, Body, Opts);
        <<"register-interest">> -> register_interest(Base, Body, Height, Opts);
        _ -> Base
    end.

%% @doc Open an offer, moving the goods into escrow.
%% Nothing is written unless the whole offer is admissible, so a rejected offer
%% is indistinguishable from a transaction that was never sent.
make_offer(Base, Body, Height, Opts) ->
    maybe
        {ok, Seller} ?= signer(Body, Opts),
        {ok, Quantity} ?= amount(<<"offer-quantity">>, Body, Opts),
        {ok, Asking} ?= amount(<<"asking">>, Body, Opts),
        {ok, Deposit} ?= amount(<<"deposit">>, Body, Opts),
        {ok, Fee} ?= amount(<<"minimum-fee">>, Body, Opts),
        {ok, Deadline} ?= amount(<<"deadline">>, Body, ?DEFAULT_DEADLINE, Opts),
        Recipient = field(<<"recipient">>, Body, Seller, Opts),
        true ?= Quantity >= 1,
        true ?= Asking >= 1,
        true ?= Deposit >= 0,
        true ?= Fee >= 0,
        true ?= Deadline >= 1,
        % The seller escrows the goods and nothing else: the deposit is asked
        % of the buyer, not staked by the seller. A seller whose whole holding
        % is the thing being sold -- the single unit of a name -- can therefore
        % still make an offer that asks collateral.
        true ?= balance(Base, Seller, Opts) >= Quantity,
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
                <<"minimum-fee">> => Fee,
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
        put_order(debit(Base, Seller, Quantity, Opts), Order, Opts)
    else
        _ -> Base
    end.

%% @doc Withdraw an offer nobody has reserved, returning the escrowed goods and
%% removing it from the book. A reservation blocks it: that is what the buyer's
%% fee and collateral bought.
cancel_order(Base, Body, Opts) ->
    maybe
        {ok, Signer} ?= signer(Body, Opts),
        {ok, Order} ?= find_order(Base, Body, Opts),
        #{
            <<"order-id">> := OrderID,
            <<"status">> := <<"open">>,
            <<"creator">> := Signer,
            <<"quantity">> := Quantity
        } ?= Order,
        ?event({swap_order_cancelled, {order, OrderID}}),
        drop_order(credit(Base, Signer, Quantity, Opts), Order, Opts)
    else
        _ -> Base
    end.

%% @doc Buy the exclusive right to complete an order.
%%
%% For `deadline' blocks the order is this buyer's alone: the seller cannot
%% withdraw it and nobody else can complete it. That is what makes it safe to
%% send AR, which no process can hold, redirect or refund.
%%
%% Two things are asked, either of which an offer may set to zero. The
%% `minimum-fee' is paid as this transaction's own `reward' -- burned to miners
%% and the endowment rather than collected, so it costs a buyer only what they
%% were spending anyway and makes idle registrations expensive. The `deposit'
%% is collateral, pledged from the buyer's own balance and forfeit to the
%% seller if the reservation lapses unpaid. It is denominated in the token
%% because a pledge is only honoured while there is an offer to honour it
%% against: a registration naming an order that has gone takes nothing at all.
register_interest(Base, Body, Height, Opts) ->
    maybe
        {ok, Buyer} ?= signer(Body, Opts),
        {ok, Order} ?= find_order(Base, Body, Opts),
        #{
            <<"order-id">> := OrderID,
            <<"status">> := <<"open">>,
            <<"minimum-fee">> := Fee,
            <<"deposit">> := Deposit,
            <<"deadline">> := Deadline
        } ?= Order,
        {ok, Paid} ?= hb_util:safe_int(tx_field(Body, <<"reward">>, 0, Opts)),
        true ?= Paid >= Fee,
        true ?= balance(Base, Buyer, Opts) >= Deposit,
        Until = Height + Deadline,
        ?event(
            {swap_interest_registered,
                {order, OrderID},
                {buyer, Buyer},
                {deposit, Deposit},
                {until, Until}
            }
        ),
        deadlines(
            put_order(
                debit(Base, Buyer, Deposit, Opts),
                Order#{
                    <<"status">> => <<"reserved">>,
                    <<"buyer">> => Buyer,
                    <<"reserved-until">> => Until
                },
                Opts
            ),
            Opts
        )
    else
        _ -> Base
    end.

%% @doc Settle against a layer-1 payment. The transaction is not addressed to
%% the process at all: it is a transfer between two user addresses that names an
%% `order-id', and the process sees it only because it is sequenced by every
%% data-free transaction on the network. It counts as payment for an order when
%% it is addressed to that order's `recipient' and carries at least the asking
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
            <<"asking">> := Asking
        } = Order,
        true ?= Target =:= Recipient,
        % A seller completing their own order from the address that made it
        % would take the goods straight back. These two comparisons close that;
        % they cannot close a second wallet, and nothing here can.
        false ?= Buyer =:= Creator,
        false ?= Buyer =:= Recipient,
        {ok, Paid} ?= hb_util:safe_int(tx_field(Body, <<"quantity">>, 0, Opts)),
        true ?= Paid >= Asking,
        % An open order is first-come; a reserved one is its buyer's alone
        % until the reservation lapses.
        true ?= claimable(Order, Buyer, Height),
        settle(Base, Order, Buyer, Opts)
    else
        _ -> Base
    end.

%% @doc Complete an order: the goods pass to the buyer, and the collateral they
%% pledged to reserve it comes back to them. The AR was always the seller's
%% directly -- no process ever held it, and none could return it.
%%
%% The order then leaves the book, so a later payment naming it finds nothing
%% and does nothing. That is the only honest answer available: the goods are no
%% longer there to give, and the AR is not there to refund.
settle(Base, Order, Buyer, Opts) ->
    #{
        <<"order-id">> := OrderID,
        <<"status">> := Status,
        <<"quantity">> := Quantity,
        <<"deposit">> := Deposit
    } = Order,
    % A buyer who reserved the offer pledged its deposit; one who paid an open
    % offer outright pledged nothing, and has nothing to get back.
    Pledged =
        case Status of
            <<"reserved">> -> Deposit;
            _ -> 0
        end,
    ?event(
        {swap_order_settled,
            {order, OrderID},
            {buyer, Buyer},
            {quantity, Quantity}
        }
    ),
    deadlines(
        drop_order(
            credit(credit(Base, Buyer, Quantity, Opts), Buyer, Pledged, Opts),
            Order,
            Opts
        ),
        Opts
    ).

%% @doc Whether an order's goods are the payer's to take: an order nobody has
%% reserved is first-come, and a reserved one is its buyer's until the
%% reservation lapses.
claimable(#{ <<"status">> := <<"open">> }, _Buyer, _Height) -> true;
claimable(
        #{
            <<"status">> := <<"reserved">>,
            <<"buyer">> := Buyer,
            <<"reserved-until">> := Until
        },
        Buyer,
        Height) ->
    Height =< Until;
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
    Dated = Base#{ <<"swap-height">> => Height },
    case state(<<"next-deadline">>, Base, 0, Opts) of
        Next when Next > 0, Height >= Next ->
            deadlines(
                lists:foldl(
                    fun(Order, Acc) -> expire(Acc, Order, Height, Opts) end,
                    Dated,
                    orders(Base, Opts)
                ),
                Opts
            );
        _ -> Dated
    end.

%% @doc Lapse a reservation the height has outlived: the buyer's collateral goes
%% to the seller, who has been held all this time, and the offer opens again. An
%% offer never expires, so this is the only thing the clock does here.

expire(
        Base,
        Order =
            #{
                <<"order-id">> := OrderID,
                <<"status">> := <<"reserved">>,
                <<"creator">> := Seller,
                <<"deposit">> := Deposit,
                <<"reserved-until">> := Until
            },
        Height,
        Opts) when Height > Until ->
    ?event({swap_reservation_lapsed, {order, OrderID}, {forfeit, Deposit}}),
    put_order(
        credit(Base, Seller, Deposit, Opts),
        hb_maps:without(
            [<<"buyer">>, <<"reserved-until">>],
            Order#{ <<"status">> => <<"open">> },
            Opts
        ),
        Opts
    );
expire(Base, _Order, _Height, _Opts) -> Base.

%% @doc Record the next height at which a reservation lapses, so that the slots
%% in between cost a single comparison. Zero means nothing is pending.
deadlines(Base, Opts) ->
    Lapses =
        [
            Until + 1
        ||
            #{ <<"status">> := <<"reserved">>, <<"reserved-until">> := Until }
                <- orders(Base, Opts)
        ],
    Base#{
        <<"next-deadline">> =>
            case Lapses of
                [] -> 0;
                _ -> lists:min(Lapses)
            end
    }.

%%% State helpers

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

%% @doc Read a field from an untrusted scheduled message as plain data.
field(Key, Msg, Default, Opts) ->
    hb_maps:get(Key, Msg, Default, Opts).

%% @doc Read the orders currently held, as plain maps. The state may have been
%% written to the process cache and read back since it was last touched, so it
%% is loaded through the link layer, and anything that is not an order is
%% ignored rather than assumed away.
orders(Base, Opts) ->
    [
        Order
    ||
        Order = #{ <<"order-id">> := _ } <-
            hb_maps:values(
                hb_cache:ensure_all_loaded(order_book(Base, Opts), Opts),
                Opts
            )
    ].

order_book(Base, Opts) -> state(<<"orders">>, Base, #{}, Opts).

%% @doc Read the order a message names, if the process holds it.
%%
%% The name comes from a stranger's transaction, so it is looked up as a key of
%% the order book rather than resolved as a path: a path would let
%% `order-id: <id>/creator' reach an address, and a reserved name like `keys'
%% reach a list, either of which would then be read as an order and fail the
%% slot. Whatever comes back must look like an order before it is treated as
%% one.
find_order(Base, Body, Opts) ->
    case
        hb_cache:ensure_all_loaded(
            hb_maps:get(
                field(<<"order-id">>, Body, <<>>, Opts),
                order_book(Base, Opts),
                not_found,
                Opts
            ),
            Opts
        )
    of
        Order = #{ <<"order-id">> := _ } -> {ok, Order};
        _ -> not_found
    end.

%% @doc Write an order back, replacing the one held rather than merging over
%% it, so that a lapsed reservation leaves no buyer behind.
put_order(Base, Order = #{ <<"order-id">> := OrderID }, Opts) ->
    Base#{
        <<"orders">> =>
            hb_maps:put(
                OrderID,
                Order,
                order_book(Base, Opts),
                Opts
            )
    }.

%% @doc Forget an order. What the book holds is the offers still open to be
%% taken, so a completed or withdrawn one leaves no trace: opening an offer and
%% withdrawing it again returns the process to exactly where it was.
drop_order(Base, #{ <<"order-id">> := OrderID }, Opts) ->
    Base#{
        <<"orders">> =>
            hb_maps:without([OrderID], order_book(Base, Opts), Opts)
    }.

%% @doc Read an address's token balance from the ledger this process shares.
%% Only the one entry is read: the ledger may be large, and the rest of it is
%% none of this device's business.
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

%% @doc Read a number a stranger wrote. Every figure in the protocol arrives as
%% a tag on somebody else's transaction, and this process is sequenced by all of
%% them: coercing `deadline: tomorrow' with `hb_util:int/1' would raise out of
%% the enclosing `maybe' -- which catches mismatches, not exceptions -- and fail
%% that slot on every node, for good. A value that is not a number is simply not
%% an admissible message.
amount(Key, Body, Opts) -> amount(Key, Body, 0, Opts).

amount(Key, Body, Default, Opts) ->
    hb_util:safe_int(field(Key, Body, Default, Opts)).

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

%% @doc The single signer of a message. A message with any other number of
%% signers is not attributable to one party, so it cannot open, cancel, reserve
%% or pay for anything.
signer(Body, Opts) ->
    case hb_message:signers(Body, Opts) of
        [Signer] -> {ok, hb_util:human_id(Signer)};
        _ -> not_found
    end.

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

%% @doc Do an order's numbers survive the process cache as numbers?
%%
%% `order/1' re-coerces every numeric key on the way out, on the stated grounds
%% that a value written between slots may come back as a binary. If that is not
%% so, the coercion is doing nothing at all.
order_numbers_survive_the_cache_test() ->
    Opts = test_opts(),
    Written = #{ <<"orders">> => #{ <<"o">> => #{
        <<"order-id">> => <<"o">>,
        <<"quantity">> => 10,
        <<"deadline">> => 20,
        <<"reserved-until">> => 1966680
    } } },
    {ok, ID} = hb_cache:write(Written, Opts),
    {ok, Read} = hb_cache:read(ID, Opts),
    Loaded = hb_cache:ensure_all_loaded(Read, Opts),
    Order = hb_maps:get(<<"o">>, hb_maps:get(<<"orders">>, Loaded, #{}, Opts), #{}, Opts),
    ?event({round_tripped, {order, Order}}),
    ?assertEqual(10, maps:get(<<"quantity">>, Order)),
    ?assertEqual(1966680, maps:get(<<"reserved-until">>, Order)).

%% @doc A message this device reads is a stranger's, and a key it does not carry
%% must be answered by this device's default, not by code the stranger picked.
%%
%% `all' mode hands every data-free transaction on Arweave to this process, so
%% the `device' key of each is the author's to choose. Reading plainly
%% dispatches on it, and a device answers only for absent keys -- so the attack
%% is to omit one. Here
%% `recipient' is omitted: the seller of an order is meant to be paid by the
%% default, and a chosen device answering that key would redirect the goods.
strangers_device_cannot_answer_for_an_absent_key_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {_, ThiefAddr} = party(),
    Hostile =
        tx(
            Seller,
            #{
                <<"target">> => ?PROCESS,
                <<"action">> => <<"make-offer">>,
                <<"offer-quantity">> => <<"1">>,
                <<"asking">> => <<"10">>,
                <<"deposit">> => <<"0">>,
                <<"deadline">> => <<"100">>,
                %% No `recipient'. The script answers for it.
                <<"device">> => <<"lua@5.3a">>,
                <<"module">> =>
                    #{
                        <<"content-type">> => <<"text/x-lua">>,
                        <<"body">> =>
                            <<"function recipient(base, req)\n"
                                "  return \"ok\", \"", ThiefAddr/binary, "\"\n"
                                "end\n">>
                    }
            }
        ),
    Opened = apply_tx(base(#{ SellerAddr => 1 }), Hostile, 10, Opts),
    [Order] = orders(Opened, Opts),
    %% The goods come back to the seller, because that is what this device
    %% decided in the absence of an instruction.
    ?assertEqual(SellerAddr, maps:get(<<"recipient">>, Order, undefined)),
    ?assertNotEqual(ThiefAddr, maps:get(<<"recipient">>, Order, undefined)).

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

%% @doc A foreign transaction's device cannot interpret absent control fields.
foreign_device_is_data_test() ->
    Opts = test_opts(),
    {Sender, SenderAddr} = party(),
    {_, Recipient} = party(),
    Base = base(#{ SenderAddr => 1 }),
    Foreign =
        tx(
            Sender,
            #{
                <<"target">> => Recipient,
                <<"device">> => <<"reference@1.0">>
            }
        ),
    Untouched = apply_tx(Base, Foreign, 100, Opts),
    ?assertEqual(1, balance(Untouched, SenderAddr, Opts)),
    ?assertEqual([], orders(Untouched, Opts)).

%% @doc Opening an offer moves the goods into escrow, and leaves
%% the order open.
make_offer_escrows_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    Base = base(#{ SellerAddr => 100 }),
    Opened = apply_tx(Base, offer(Seller, 10, 500, 5, 20), 100, Opts),
    Order = only_order(Opened, Opts),
    ?assertEqual(90, balance(Opened, SellerAddr, Opts)),
    ?assertEqual(<<"open">>, maps:get(<<"status">>, Order)),
    ?assertEqual(10, maps:get(<<"quantity">>, Order)),
    ?assertEqual(5, maps:get(<<"deposit">>, Order)),
    ?assertEqual(SellerAddr, maps:get(<<"recipient">>, Order)).

%% @doc An offer for more than the seller holds changes nothing at all.
make_offer_insufficient_balance_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    Base = base(#{ SellerAddr => 4 }),
    Result = apply_tx(Base, offer(Seller, 10, 500, 5, 20), 100, Opts),
    ?assertEqual([], orders(Result, Opts)),
    ?assertEqual(4, balance(Result, SellerAddr, Opts)).

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
    ?assertEqual(<<>>, tx_field(Tagged, <<"target">>, <<>>, Opts)),
    Result = apply_tx(base(#{ SellerAddr => 100 }), Tagged, 100, Opts),
    ?assertEqual([], orders(Result, Opts)),
    ?assertEqual(100, balance(Result, SellerAddr, Opts)).

%% @doc The whole trade: the buyer pays the seller directly on layer one, and
%% the process -- which is not a party to that payment -- settles it.
settlement_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    Base = base(#{ SellerAddr => 100 }),
    Opened = apply_tx(Base, offer(Seller, 10, 500, 5, 20), 100, Opts),
    OrderID = maps:get(<<"order-id">>, only_order(Opened, Opts)),
    Settled =
        apply_tx(
            Opened,
            pay(Buyer, SellerAddr, 500, OrderID),
            120,
            Opts
        ),
    % The offer is complete, so it is no longer an offer.
    ?assertEqual([], orders(Settled, Opts)),
    ?assertEqual(10, balance(Settled, BuyerAddr, Opts)),
    ?assertEqual(90, balance(Settled, SellerAddr, Opts)).

%% @doc Paying less than the asking price settles nothing: the process never
%% held the value, so it cannot refund a partial fill.
underpayment_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    Opened =
        apply_tx(base(#{ SellerAddr => 100 }), offer(Seller, 10, 500, 5, 20), 100, Opts),
    OrderID = maps:get(<<"order-id">>, only_order(Opened, Opts)),
    Result = apply_tx(Opened, pay(Buyer, SellerAddr, 499, OrderID), 120, Opts),
    ?assertEqual(<<"open">>, maps:get(<<"status">>, only_order(Result, Opts))),
    ?assertEqual(0, balance(Result, BuyerAddr, Opts)).

%% @doc A payment that names the order but is addressed to somebody else is not
%% a payment for it.
payment_to_wrong_address_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    {_, Stranger} = party(),
    Opened =
        apply_tx(base(#{ SellerAddr => 100 }), offer(Seller, 10, 500, 5, 20), 100, Opts),
    OrderID = maps:get(<<"order-id">>, only_order(Opened, Opts)),
    Result = apply_tx(Opened, pay(Buyer, Stranger, 500, OrderID), 120, Opts),
    ?assertEqual(<<"open">>, maps:get(<<"status">>, only_order(Result, Opts))),
    ?assertEqual(0, balance(Result, BuyerAddr, Opts)).

%% @doc Tag-only trade keys are metadata and do not count as the payment leg.
tag_only_transfer_is_metadata_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    Opened =
        apply_tx(base(#{ SellerAddr => 100 }), offer(Seller, 10, 500, 5, 20), 100, Opts),
    OrderID = maps:get(<<"order-id">>, only_order(Opened, Opts)),
    Tagged = tag_only_transfer(Buyer, SellerAddr, 500, OrderID),
    ?assertEqual(SellerAddr, hb_ao:get(<<"target">>, Tagged, not_found, Opts)),
    ?assertEqual(<<"500">>, hb_ao:get(<<"quantity">>, Tagged, not_found, Opts)),
    ?assertEqual(<<>>, tx_field(Tagged, <<"target">>, <<>>, Opts)),
    ?assertEqual({ok, 0}, hb_util:safe_int(tx_field(Tagged, <<"quantity">>, 0, Opts))),
    Result = apply_tx(Opened, Tagged, 120, Opts),
    ?assertEqual(<<"open">>, maps:get(<<"status">>, only_order(Result, Opts))),
    ?assertEqual(0, balance(Result, BuyerAddr, Opts)),
    ?assertEqual(90, balance(Result, SellerAddr, Opts)).

%% @doc Only the seller may cancel their order.
cancel_by_stranger_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Stranger, _} = party(),
    Opened =
        apply_tx(base(#{ SellerAddr => 100 }), offer(Seller, 10, 500, 5, 20), 100, Opts),
    OrderID = maps:get(<<"order-id">>, only_order(Opened, Opts)),
    Result =
        apply_tx(Opened, order_action(Stranger, <<"cancel-order">>, OrderID), 110, Opts),
    ?assertEqual(<<"open">>, maps:get(<<"status">>, only_order(Result, Opts))).

%% @doc A reserved order cannot be pulled out from under the buyer who reserved
%% it. This is the guarantee that makes paying safe.
reservation_prevents_cancellation_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    Opened =
        apply_tx(
            base(#{ SellerAddr => 100, BuyerAddr => 5 }),
            offer(Seller, 10, 500, 5, 20),
            100,
            Opts
        ),
    OrderID = maps:get(<<"order-id">>, only_order(Opened, Opts)),
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
%% not take the goods.
reservation_is_exclusive_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    {Interloper, InterloperAddr} = party(),
    Opened =
        apply_tx(
            base(#{ SellerAddr => 100, BuyerAddr => 5 }),
            offer(Seller, 10, 500, 5, 20),
            100,
            Opts
        ),
    OrderID = maps:get(<<"order-id">>, only_order(Opened, Opts)),
    Reserved =
        apply_tx(Opened, order_action(Buyer, <<"register-interest">>, OrderID), 110, Opts),
    % The buyer's collateral is pledged out of their balance.
    ?assertEqual(0, balance(Reserved, BuyerAddr, Opts)),
    Result = apply_tx(Reserved, pay(Interloper, SellerAddr, 500, OrderID), 111, Opts),
    Order = only_order(Result, Opts),
    % The stranger's AR bought nothing: the order is not theirs to complete, and
    % there is nothing here that could give it back.
    ?assertEqual(<<"reserved">>, maps:get(<<"status">>, Order)),
    ?assertEqual(BuyerAddr, maps:get(<<"buyer">>, Order)),
    ?assertEqual(10, maps:get(<<"quantity">>, Order)),
    ?assertEqual(0, balance(Result, InterloperAddr, Opts)).

%% @doc A reservation lapses on its own, without anybody sending anything: the
%% network's own traffic carries the clock forward.
reservation_lapses_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    Opened =
        apply_tx(
            base(#{ SellerAddr => 100, BuyerAddr => 5 }),
            offer(Seller, 10, 500, 5, 20),
            100,
            Opts
        ),
    OrderID = maps:get(<<"order-id">>, only_order(Opened, Opts)),
    Reserved =
        apply_tx(Opened, order_action(Buyer, <<"register-interest">>, OrderID), 110, Opts),
    ?assertEqual(
        110 + 20,
        maps:get(<<"reserved-until">>, only_order(Reserved, Opts))
    ),
    Lapsed = tick(Reserved, 110 + 20 + 1, Opts),
    ?assertEqual(<<"open">>, maps:get(<<"status">>, only_order(Lapsed, Opts))).

%% @doc Settling twice is not possible: the goods left with the first payment.
double_settlement_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    {Late, LateAddr} = party(),
    Opened =
        apply_tx(base(#{ SellerAddr => 100 }), offer(Seller, 10, 500, 5, 20), 100, Opts),
    OrderID = maps:get(<<"order-id">>, only_order(Opened, Opts)),
    Settled = apply_tx(Opened, pay(Buyer, SellerAddr, 500, OrderID), 120, Opts),
    Again = apply_tx(Settled, pay(Late, SellerAddr, 500, OrderID), 121, Opts),
    ?assertEqual(10, balance(Again, BuyerAddr, Opts)),
    ?assertEqual(0, balance(Again, LateAddr, Opts)),
    ?assertEqual(90, balance(Again, SellerAddr, Opts)).

%% @doc A payment naming an order the process has never heard of is ordinary
%% Arweave traffic, and is ignored.
unknown_order_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    Opened =
        apply_tx(
            base(#{ SellerAddr => 100, BuyerAddr => 5 }),
            offer(Seller, 10, 500, 5, 20),
            100,
            Opts
        ),
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
        apply_tx(base(#{ SellerAddr => 100 }), offer(Seller, 10, 500, 0, 20), 100, Opts),
    Ticked = tick(Opened, 150, Opts),
    ?assertEqual(150, hb_ao:get(<<"swap-height">>, Ticked, 0, Opts)),
    ?assertEqual(<<"open">>, maps:get(<<"status">>, only_order(Ticked, Opts))),
    ?assertEqual(90, balance(Ticked, SellerAddr, Opts)).

%% @doc A stranger's transaction may ask to be routed anywhere -- the key a slot
%% resolves comes from the sender's own `path' tag, and this process is
%% sequenced by every data-free transaction on Arweave. It must be applied like
%% any other, not fail its slot.
stray_path_is_applied_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Stranger, _} = party(),
    Opened =
        apply_tx(base(#{ SellerAddr => 100 }), offer(Seller, 10, 500, 5, 20), 100, Opts),
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
    ?assertEqual(90, balance(State, SellerAddr, Opts)).

%% @doc Collateral returns to the buyer when they complete the sale.
settlement_returns_the_collateral_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    Opened =
        apply_tx(
            base(#{ SellerAddr => 100, BuyerAddr => 5 }),
            offer(Seller, 10, 500, 5, 20),
            100,
            Opts
        ),
    OrderID = maps:get(<<"order-id">>, only_order(Opened, Opts)),
    Reserved =
        apply_tx(Opened, order_action(Buyer, <<"register-interest">>, OrderID), 110, Opts),
    ?assertEqual(0, balance(Reserved, BuyerAddr, Opts)),
    Settled = apply_tx(Reserved, pay(Buyer, SellerAddr, 500, OrderID), 115, Opts),
    ?assertEqual([], orders(Settled, Opts)),
    % The goods, and the collateral back.
    ?assertEqual(15, balance(Settled, BuyerAddr, Opts)),
    ?assertEqual(90, balance(Settled, SellerAddr, Opts)).

%% @doc A buyer who cannot cover the collateral cannot reserve.
collateral_must_be_held_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    Opened =
        apply_tx(
            base(#{ SellerAddr => 100, BuyerAddr => 4 }),
            offer(Seller, 10, 500, 5, 20),
            100,
            Opts
        ),
    OrderID = maps:get(<<"order-id">>, only_order(Opened, Opts)),
    Tried =
        apply_tx(Opened, order_action(Buyer, <<"register-interest">>, OrderID), 110, Opts),
    ?assertEqual(<<"open">>, maps:get(<<"status">>, only_order(Tried, Opts))),
    ?assertEqual(4, balance(Tried, BuyerAddr, Opts)).

%% @doc A registration naming an offer that has gone takes nothing.
%%
%% This is why collateral is denominated in the token: the process can decline
%% to honour a pledge when there is nothing to honour it against. AR could not
%% serve, because AR settles on the weave whatever this device decides.
collateral_is_not_taken_for_a_vanished_offer_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    Before = base(#{ SellerAddr => 100, BuyerAddr => 5 }),
    Opened = apply_tx(Before, offer(Seller, 10, 500, 5, 20), 100, Opts),
    OrderID = maps:get(<<"order-id">>, only_order(Opened, Opts)),
    Withdrawn =
        apply_tx(Opened, order_action(Seller, <<"cancel-order">>, OrderID), 105, Opts),
    Late =
        apply_tx(
            Withdrawn,
            order_action(Buyer, <<"register-interest">>, OrderID),
            110,
            Opts
        ),
    ?assertEqual(5, balance(Late, BuyerAddr, Opts)),
    ?assertEqual([], orders(Late, Opts)).

%% @doc Opening an offer and withdrawing it leaves the process as it was.
offer_and_withdrawal_are_a_round_trip_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    Before = base(#{ SellerAddr => 100 }),
    Opened = apply_tx(Before, offer(Seller, 10, 500, 5, 20), 100, Opts),
    OrderID = maps:get(<<"order-id">>, only_order(Opened, Opts)),
    After =
        apply_tx(Opened, order_action(Seller, <<"cancel-order">>, OrderID), 105, Opts),
    ?assertEqual([], orders(After, Opts)),
    ?assertEqual(100, balance(After, SellerAddr, Opts)),
    ?assertEqual(0, hb_util:int(state(<<"next-deadline">>, After, 0, Opts))).

%% @doc Units are conserved across a whole trade: what is escrowed, what is
%% pledged and what is held always sum to the supply.
supply_is_conserved_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    Held =
        fun(State) ->
            balance(State, SellerAddr, Opts) + balance(State, BuyerAddr, Opts)
                + lists:sum(
                    [
                        % Goods are escrowed by every offer; collateral only by
                        % one somebody has reserved.
                        case maps:get(<<"status">>, Order) of
                            <<"reserved">> -> maps:get(<<"quantity">>, Order) + maps:get(<<"deposit">>, Order);
                            _ -> maps:get(<<"quantity">>, Order)
                        end
                    ||
                        Order <- orders(State, Opts)
                    ]
                )
        end,
    Before = base(#{ SellerAddr => 100, BuyerAddr => 5 }),
    ?assertEqual(105, Held(Before)),
    Opened = apply_tx(Before, offer(Seller, 10, 500, 5, 20), 100, Opts),
    ?assertEqual(105, Held(Opened)),
    OrderID = maps:get(<<"order-id">>, only_order(Opened, Opts)),
    Reserved =
        apply_tx(Opened, order_action(Buyer, <<"register-interest">>, OrderID), 110, Opts),
    ?assertEqual(105, Held(Reserved)),
    Settled = apply_tx(Reserved, pay(Buyer, SellerAddr, 500, OrderID), 115, Opts),
    ?assertEqual(105, Held(Settled)),
    Lapsed = tick(apply_tx(Before, offer(Seller, 10, 500, 5, 20), 100, Opts), 300, Opts),
    ?assertEqual(105, Held(Lapsed)).

%% @doc A lapsed reservation pays the seller and reopens the offer.
%%
%% The collateral is the buyer's answer for having frozen the seller out. When
%% the window passes unpaid it is the seller's, and the offer -- which never
%% expires -- is open to anyone again.
lapse_forfeits_collateral_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    Opened =
        apply_tx(
            base(#{ SellerAddr => 100, BuyerAddr => 5 }),
            offer(Seller, 10, 500, 5, 20),
            100,
            Opts
        ),
    OrderID = maps:get(<<"order-id">>, only_order(Opened, Opts)),
    Reserved =
        apply_tx(Opened, order_action(Buyer, <<"register-interest">>, OrderID), 190, Opts),
    ?assertEqual(0, balance(Reserved, BuyerAddr, Opts)),
    Lapsed = tick(Reserved, 190 + 20 + 1, Opts),
    Order = only_order(Lapsed, Opts),
    ?assertEqual(<<"open">>, maps:get(<<"status">>, Order)),
    ?assertEqual(false, maps:is_key(<<"buyer">>, Order)),
    % The seller keeps 90 escrowed-out plus the 5 forfeited to them.
    ?assertEqual(95, balance(Lapsed, SellerAddr, Opts)),
    ?assertEqual(0, balance(Lapsed, BuyerAddr, Opts)).

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
    ?assertEqual(100, balance(Result, SellerAddr, Opts)).

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
    ?assertEqual(100, balance(Result, SellerAddr, Opts)).

%% @doc An `order-id' is caller-supplied text. Path-like values and reserved
%% keys must not reach anything that is then read as an order.
reserved_order_id_names_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    Opened =
        apply_tx(
            base(#{ SellerAddr => 100, BuyerAddr => 5 }),
            offer(Seller, 10, 500, 5, 20),
            100,
            Opts
        ),
    OrderID = maps:get(<<"order-id">>, only_order(Opened, Opts)),
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
%% fee, so it would otherwise take back the goods, leaving a real
%% buyer to pay for an order that is already spent.
seller_cannot_settle_own_order_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    Opened =
        apply_tx(base(#{ SellerAddr => 100 }), offer(Seller, 10, 500, 5, 20), 100, Opts),
    OrderID = maps:get(<<"order-id">>, only_order(Opened, Opts)),
    Result = apply_tx(Opened, pay(Seller, SellerAddr, 500, OrderID), 120, Opts),
    ?assertEqual(<<"open">>, maps:get(<<"status">>, only_order(Result, Opts))),
    ?assertEqual(90, balance(Result, SellerAddr, Opts)).

%% @doc A scheduled message routed to `set' or `keys' is applied like any other.
%% Handing either to `~message@1.0' would let a passer-by write the process's own
%% balances, or replace its state with a list of key names.
reserved_paths_are_applied_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Stranger, _} = party(),
    Opened =
        apply_tx(base(#{ SellerAddr => 100 }), offer(Seller, 10, 500, 5, 20), 100, Opts),
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
            ?assertEqual(90, balance(State, SellerAddr, Opts)),
            ?assertEqual(
                <<"open">>,
                maps:get(<<"status">>, only_order(State, Opts))
            )
        end,
        [<<"set">>, <<"keys">>, <<"info">>]
    ).

%% @doc An offer that charges to register turns away a registration that does
%% not pay the fee, and lets one that does through. The fee is the
%% registration's own reward, so it is paid to the network rather than to an
%% address nobody holds.
minimum_fee_gates_registration_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, _} = party(),
    Charged =
        tx(
            Seller,
            #{
                <<"target">> => ?PROCESS,
                <<"action">> => <<"make-offer">>,
                <<"offer-quantity">> => <<"10">>,
                <<"asking">> => <<"500">>,
                <<"deposit">> => <<"0">>,
                <<"minimum-fee">> => <<"1000">>,
                <<"deadline">> => <<"200">>
            }
        ),
    Opened = apply_tx(base(#{ SellerAddr => 100 }), Charged, 100, Opts),
    OrderID = maps:get(<<"order-id">>, only_order(Opened, Opts)),
    Free =
        apply_tx(
            Opened,
            order_action(Buyer, <<"register-interest">>, OrderID),
            110,
            Opts
        ),
    ?assertEqual(<<"open">>, maps:get(<<"status">>, only_order(Free, Opts))),
    % Value sent to the process is not the fee: it would be stranded there, and
    % it is the reward that the order asks for.
    Stranded =
        apply_tx(
            Opened,
            tx(
                Buyer,
                #{
                    <<"target">> => ?PROCESS,
                    <<"action">> => <<"register-interest">>,
                    <<"order-id">> => OrderID,
                    <<"quantity">> => <<"100000">>
                }
            ),
            110,
            Opts
        ),
    ?assertEqual(<<"open">>, maps:get(<<"status">>, only_order(Stranded, Opts))),
    Underpaid =
        apply_tx(
            Opened,
            registration(Buyer, OrderID, 999),
            110,
            Opts
        ),
    ?assertEqual(<<"open">>, maps:get(<<"status">>, only_order(Underpaid, Opts))),
    Reserved = apply_tx(Opened, registration(Buyer, OrderID, 1000), 110, Opts),
    ?assertEqual(
        <<"reserved">>,
        maps:get(<<"status">>, only_order(Reserved, Opts))
    ).

%% @doc An order that charges nothing is still free to register on.
no_minimum_fee_registers_free_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    Opened =
        apply_tx(
            base(#{ SellerAddr => 100, BuyerAddr => 5 }),
            offer(Seller, 10, 500, 5, 20),
            100,
            Opts
        ),
    OrderID = maps:get(<<"order-id">>, only_order(Opened, Opts)),
    Reserved =
        apply_tx(
            Opened,
            order_action(Buyer, <<"register-interest">>, OrderID),
            110,
            Opts
        ),
    ?assertEqual(
        <<"reserved">>,
        maps:get(<<"status">>, only_order(Reserved, Opts))
    ).

%% @doc A seller whose whole holding is the single unit they are selling can
%% still offer it: an offer escrows the goods and asks nothing else of them.
single_unit_offer_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    Opened =
        apply_tx(base(#{ SellerAddr => 1 }), offer(Seller, 1, 500, 0, 20), 100, Opts),
    Order = only_order(Opened, Opts),
    ?assertEqual(1, maps:get(<<"quantity">>, Order)),
    ?assertEqual(0, maps:get(<<"deposit">>, Order)),
    ?assertEqual(0, balance(Opened, SellerAddr, Opts)).

%% @doc A registration paying a fee, as the reward on its own transaction.
registration(Wallet, OrderID, Winston) ->
    tx(
        Wallet,
        #{
            <<"target">> => ?PROCESS,
            <<"action">> => <<"register-interest">>,
            <<"order-id">> => OrderID,
            <<"reward">> => hb_util:bin(Winston)
        }
    ).

%% @doc The next height at which anything happens is recorded, so that the
%% slots in between cost a single comparison rather than a walk of the orders.
next_deadline_tracked_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    Opened =
        apply_tx(
            base(#{ SellerAddr => 100, BuyerAddr => 5 }),
            offer(Seller, 10, 500, 5, 20),
            100,
            Opts
        ),
    % An offer alone has nothing pending: it never expires.
    ?assertEqual(0, hb_ao:get(<<"next-deadline">>, Opened, 0, Opts)),
    OrderID = maps:get(<<"order-id">>, only_order(Opened, Opts)),
    Reserved =
        apply_tx(Opened, order_action(Buyer, <<"register-interest">>, OrderID), 110, Opts),
    % A reservation is the only thing a clock waits for.
    ?assertEqual(
        110 + 20 + 1,
        hb_ao:get(<<"next-deadline">>, Reserved, 0, Opts)
    ).
