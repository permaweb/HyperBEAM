%%% @doc A device for selling a process's tokens for native Arweave value.
%%%
%%% The two halves of the trade live in different places, and that asymmetry is
%%% the whole design. The token half lives here: this device debits and credits
%%% the `balances' submessage that a `~token@1.0' implementation keeps in the
%%% same process's base, so it can hold tokens in escrow and pay them out with
%%% certainty. The Arweave half does not live here at all -- AR moves directly
%%% from buyer to seller as an ordinary layer-1 transfer, which no process can
%%% hold, redirect or refund.
%%%
%%% A process using this device is therefore sequenced by
%%% `~arweave-scheduler@1.0' in its `all' mode: every base-layer transaction
%%% becomes a slot, so a payment between two addresses that the process is not a
%%% party to is nonetheless something the process sees, and can settle against.
%%% Its process message reads:
%%% <pre>
%%%     scheduler-device: arweave-scheduler@1.0
%%%     scheduler-mode:   all
%%%     execution-device: arweave-swap@1.0
%%% </pre>
%%%
%%% The protocol is four messages:
%%% <ul>
%%%   <li>`make-offer' (to the process, from the seller), carrying
%%%       `offer-quantity' in token units, `asking' in winston, a `deadline'
%%%       in blocks and a `minimum-fee' in winston. The `offer-quantity' moves
%%%       into escrow at once, so delivery is never in doubt: the goods are held
%%%       before any buyer commits anything. The offered amount cannot be called
%%%       `quantity': that is a transaction's own value field, so the codec would
%%%       carry it as winston of AR sent to the process -- an address with no key
%%%       behind it, which would destroy it.</li>
%%%   <li>`register-interest' (to the process, from a buyer), naming an
%%%       `order-id'. It buys the exclusive right to complete the sale: for
%%%       `deadline' blocks the order is that buyer's alone and the seller
%%%       cannot withdraw it. That window is what makes paying safe -- without
%%%       it a buyer races the seller's withdrawal, having already sent AR that
%%%       nobody can claw back.</li>
%%%   <li>`cancel-order' (to the process, from the seller), naming an
%%%       `order-id'. Returns the escrowed goods and removes the offer. It is
%%%       refused while a reservation stands.</li>
%%%   <li>The payment itself: an ordinary transfer whose `target' is the order's
%%%       `creator', whose `quantity' is at least the `asking' winston, tagged
%%%       with the `order-id'. This is the message the `all' mode exists to
%%%       deliver.</li>
%%% </ul>
%%%
%%% Exclusivity is the only thing a buyer is sold before they pay, so the
%%% `minimum-fee' is the only thing that can be asked for it: taking a
%%% reservation costs the seller their right to withdraw, and a free option is
%%% worth abusing. The fee is paid as the registration's own transaction
%%% `reward', which sends it where Arweave sends rewards -- to miners and the
%%% endowment -- rather than stranding it at the process's address. Being
%%% denominated in AR it asks nothing of a buyer who holds none of the token,
%%% which is the whole population of buyers when the thing being sold is a name
%%% whose entire supply is the single unit on offer.
%%%
%%% An offer does not expire; only a reservation does. Paying an unreserved
%%% offer is permitted and unwise: the seller may withdraw, or another buyer
%%% complete it, while the payment is in flight.
%%%
%%% The book holds live offers only, so opening an offer and withdrawing it
%%% leaves the process exactly as it was, and escrowed goods plus balances
%%% always sum to the supply.
%%%
%%% Reservations are measured in Arweave block heights, read from the
%%% `block-height' that `all'-mode assignments carry. That is the only clock the
%%% device has, and deliberately so: reading the chain tip during a compute
%%% would be non-deterministic, and `~process@1.0' caches every slot result
%%% forever. A lapsed reservation is not retired by the clock but simply ignored
%%% once the height has passed it, so the network's traffic costs this process
%%% nothing.
-module(dev_arweave_swap).
-implements(<<"arweave-swap@1.0">>).
%%% AO-Core API functions:
-export([info/0, compute/3, set/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The balances submessage this device settles in. Owned by the process's token
%%% implementation; the swap only moves value inside it.
-define(BALANCES, <<"balances">>).

%% @doc Every state transition in this device is driven by the schedule, never
%% by a direct request, so every key routes to `compute'.
%%
%% The key a slot resolves is chosen by the scheduled transaction's own `path'
%% tag (see `lib_scheduler:base_assignment/4'), and this process is sequenced by
%% every transaction on Arweave: whatever a stranger writes there must be
%% applied like any other message. So there is no `exports' list -- a key
%% outside it would fall through to `~message@1.0', answer `not_found', fail its
%% slot and wedge the process permanently -- and no `excludes' list either,
%% since an excluded key is handed to `~message@1.0' instead, whose `keys' would
%% replace the state with a list of key names. A module function of a key's name
%% takes precedence over this handler, so the device exports only `set', which
%% it must.
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

%% @doc Setting the device is honoured, and nothing else is.
%% `lib_process:run_as' puts the process's own device back with a `set' after
%% every slot, so refusing it would leave the process wearing this device. A
%% scheduled message that asks to be routed to `set' is applied like any other
%% message instead of being allowed to write the state directly.
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
%% traffic, so the classification below is ordered by cost: compare a single
%% field -- the transaction's `target' -- against the process, and only then
%% look at tags. A transaction that is neither addressed to the process nor a
%% payment against a live order leaves the state exactly as it was.
%%
%% The process is identified from the assignment rather than from the process
%% message, which `lib_process:process_id/3' would re-verify the signature of on
%% every one of the network's transactions.
compute(Base, Assignment, Opts) ->
    Height = hb_util:int(hb_maps:get(<<"block-height">>, Assignment, 0, Opts)),
    ProcID = hb_maps:get(<<"process">>, Assignment, <<>>, Opts),
    Body = hb_maps:get(<<"body">>, Assignment, #{}, Opts),
    case tx_field(Body, <<"target">>, <<>>, Opts) of
        % Sent to nobody, and so neither to us nor to a seller. Most of the
        % weave is data, and this is the cheapest thing the device can say
        % about it -- as well as keeping an assignment with no `process' from
        % aliasing every one of them into `control'.
        <<>> -> {ok, Base};
        ProcID -> {ok, control(Base, Body, Height, Opts)};
        Target -> {ok, payment(Base, Body, Target, Height, Opts)}
    end.

%%% Order lifecycle

%% @doc Route a transaction addressed to the process by its `action'. An unknown
%% action is not an error: in this mode anyone may send the process anything,
%% and a process that could be wedged by a stranger's transaction would not
%% survive its own schedule.
control(Base, Body, Height, Opts) ->
    case hb_util:to_lower(hb_maps:get(<<"action">>, Body, <<>>, Opts)) of
        <<"make-offer">> -> make_offer(Base, Body, Height, Opts);
        <<"cancel-order">> -> cancel_order(Base, Body, Height, Opts);
        <<"register-interest">> -> register_interest(Base, Body, Height, Opts);
        _ -> Base
    end.

%% @doc Open an offer, moving the goods into escrow. Nothing is written unless
%% the whole offer is admissible, so a rejected offer is indistinguishable from
%% a transaction that was never sent.
%%
%% The order is named by the transaction that opened it, and an id already in
%% the book is left alone: escrowing twice against one name would debit the
%% seller for goods that only one order can deliver.
make_offer(Base, Body, Height, Opts) ->
    maybe
        {ok, Seller} ?= signer(Body, Opts),
        {ok, Quantity} ?= amount(<<"offer-quantity">>, Body, Opts),
        {ok, Asking} ?= amount(<<"asking">>, Body, Opts),
        {ok, Fee} ?= amount(<<"minimum-fee">>, Body, Opts),
        {ok, Deadline} ?= amount(<<"deadline">>, Body, Opts),
        true ?= Quantity >= 1,
        true ?= Asking >= 1,
        true ?= Fee >= 0,
        true ?= Deadline >= 1,
        % An offer asks only that the seller hold what they are offering. A
        % seller whose whole holding is the thing being sold -- the single unit
        % of a name -- has nothing left to stake, and asking for more would put
        % a name beyond sale.
        true ?= balance(Base, Seller, Opts) >= Quantity,
        OrderID = hb_util:human_id(hb_message:id(Body, signed, Opts)),
        not_found ?= hb_maps:get(OrderID, order_book(Base, Opts), not_found, Opts),
        ?event(
            {swap_order_opened,
                {order, OrderID},
                {seller, Seller},
                {quantity, Quantity},
                {asking, Asking}
            }
        ),
        put_order(
            debit(Base, Seller, Quantity, Opts),
            #{
                <<"order-id">> => OrderID,
                <<"creator">> => Seller,
                <<"quantity">> => Quantity,
                <<"asking">> => Asking,
                <<"minimum-fee">> => Fee,
                <<"deadline">> => Deadline
            },
            Opts
        )
    else
        Refused -> refused(Base, <<"make-offer">>, Refused)
    end.

%% @doc Withdraw an offer nobody holds, returning the escrowed goods and
%% removing it from the book. A live reservation blocks it: that is what the
%% buyer's fee bought.
cancel_order(Base, Body, Height, Opts) ->
    maybe
        {ok, Signer} ?= signer(Body, Opts),
        {ok, Order} ?= find_order(Base, Body, Opts),
        #{
            <<"order-id">> := OrderID,
            <<"creator">> := Signer,
            <<"quantity">> := Quantity
        } ?= Order,
        false ?= reserved(Order, Height),
        ?event({swap_order_cancelled, {order, OrderID}}),
        drop_order(credit(Base, Signer, Quantity, Opts), Order, Opts)
    else
        Refused -> refused(Base, <<"cancel-order">>, Refused)
    end.

%% @doc Buy the exclusive right to complete an order.
%%
%% For `deadline' blocks the order is this buyer's alone: the seller cannot
%% withdraw it and nobody else can complete it. That is what makes it safe to
%% send AR, which no process can hold, redirect or refund.
%%
%% The order's `minimum-fee' is paid as this transaction's own `reward' -- spent
%% to miners and the endowment rather than collected, so it costs a buyer only
%% what they were spending anyway and makes idle registrations expensive. The
%% reward is read from the transaction's real fields: a tag of the same name
%% would otherwise let a buyer claim to have paid it.
register_interest(Base, Body, Height, Opts) ->
    maybe
        {ok, Buyer} ?= signer(Body, Opts),
        {ok, Order} ?= find_order(Base, Body, Opts),
        false ?= reserved(Order, Height),
        #{
            <<"order-id">> := OrderID,
            <<"minimum-fee">> := Fee,
            <<"deadline">> := Deadline
        } ?= Order,
        {ok, Paid} ?= hb_util:safe_int(tx_field(Body, <<"reward">>, 0, Opts)),
        true ?= Paid >= Fee,
        Until = Height + Deadline,
        ?event(
            {swap_interest_registered,
                {order, OrderID},
                {buyer, Buyer},
                {until, Until}
            }
        ),
        put_order(
            Base,
            Order#{ <<"buyer">> => Buyer, <<"reserved-until">> => Until },
            Opts
        )
    else
        Refused -> refused(Base, <<"register-interest">>, Refused)
    end.

%% @doc Settle against a layer-1 payment. The transaction is not addressed to
%% the process at all: it is a transfer between two user addresses that names an
%% `order-id', and the process sees it only because it is sequenced by every
%% transaction on the network. It counts as payment for an order when it is
%% addressed to that order's creator and carries at least the asking winston.
%%
%% Underpayment is ignored rather than partially filled -- the value never
%% passed through the process, so there is nothing to refund the difference
%% from.
%%
%% The order then leaves the book, so a later payment naming it finds nothing
%% and does nothing. That is the only honest answer available: the goods are no
%% longer there to give, and the AR is not there to refund.
payment(Base, Body, Target, Height, Opts) ->
    maybe
        {ok, Order} ?= find_order(Base, Body, Opts),
        #{
            <<"order-id">> := OrderID,
            <<"creator">> := Creator,
            <<"quantity">> := Quantity,
            <<"asking">> := Asking
        } ?= Order,
        true ?= Target =:= Creator,
        {ok, Buyer} ?= signer(Body, Opts),
        % A seller completing their own order would take the goods straight back
        % for the price of a network fee. This closes that; it cannot close a
        % second wallet, and nothing here can.
        false ?= Buyer =:= Creator,
        {ok, Paid} ?= hb_util:safe_int(tx_field(Body, <<"quantity">>, 0, Opts)),
        true ?= Paid >= Asking,
        true ?= claimable(Order, Buyer, Height),
        ?event(
            {swap_order_settled,
                {order, OrderID},
                {buyer, Buyer},
                {quantity, Quantity}
            }
        ),
        drop_order(credit(Base, Buyer, Quantity, Opts), Order, Opts)
    else
        Refused -> refused(Base, <<"payment">>, Refused)
    end.

%% @doc Leave the state exactly as it was. A refusal writes nothing by design --
%% a rejected message is indistinguishable from one that was never sent -- so
%% this event is the only trace of why a trade did not happen.
refused(Base, Action, Reason) ->
    ?event({swap_refused, {action, Action}, {reason, Reason}}),
    Base.

%%% The clock

%% @doc Whether a reservation still stands over an order at this height. Nothing
%% retires a lapsed one: an order the height has outrun is simply read as unheld.
%% That is the same answer a sweep would reach, without walking the book on a
%% process that every transaction on the network is a slot of.
reserved(#{ <<"reserved-until">> := Until }, Height) -> Height =< Until;
reserved(_Order, _Height) -> false.

%% @doc Whether an order's goods are a payer's to take: an order nobody holds is
%% first-come, and a held one is its buyer's until the hold lapses.
claimable(Order = #{ <<"buyer">> := Buyer }, Payer, Height) ->
    Payer =:= Buyer orelse not reserved(Order, Height);
claimable(_Order, _Payer, _Height) -> true.

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

%% @doc Read a value from the real layer-1 transaction fields recorded in the
%% `tx@1.0' commitment. Top-level keys may come from tags of the same name
%% whenever the field itself is absent, so payment routing and amount checks
%% must not use them.
tx_field(Body, Field, Default, Opts) ->
    case
        hb_message:commitment(
            #{ <<"commitment-device">> => <<"tx@1.0">> },
            Body,
            Opts
        )
    of
        {ok, _ID, Commitment} ->
            hb_maps:get(<<"field-", Field/binary>>, Commitment, Default, Opts);
        _ -> Default
    end.

%% @doc The single signer of a message. A message with any other number of
%% signers is not attributable to one party, so it cannot open, cancel, reserve
%% or pay for anything.
signer(Body, Opts) ->
    case hb_message:signers(Body, Opts) of
        [Signer] -> {ok, hb_util:human_id(Signer)};
        _ -> not_found
    end.

%% @doc Read a number a stranger wrote. Every figure in the protocol arrives as
%% a tag on somebody else's transaction, and this process is sequenced by all of
%% them: coercing `deadline: tomorrow' with `hb_util:int/1' would raise out of
%% the enclosing `maybe' -- which catches mismatches, not exceptions -- and fail
%% that slot on every node, for good. A value that is not a number is simply not
%% an admissible message.
amount(Key, Body, Opts) ->
    hb_util:safe_int(hb_maps:get(Key, Body, 0, Opts)).

%%% The order book

order_book(Base, Opts) -> state(<<"orders">>, Base, #{}, Opts).

%% @doc Read the order a message names, if the process holds it.
%%
%% The name comes from a stranger's transaction, so it is looked up as a key of
%% the order book rather than resolved as a path: a path would let
%% `order-id: <id>/creator' reach an address, and a reserved name like `keys'
%% reach a list, either of which would then be read as an order and fail the slot.
%% Whatever comes back must look like an order before it is treated as one. The
%% book may have been written to the process cache and read back since it was
%% last touched, so it is loaded through the link layer.
find_order(Base, Body, Opts) ->
    case
        hb_cache:ensure_all_loaded(
            hb_maps:get(
                hb_maps:get(<<"order-id">>, Body, <<>>, Opts),
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

%% @doc Write an order back, replacing the one held rather than merging over it.
put_order(Base, Order = #{ <<"order-id">> := OrderID }, Opts) ->
    Base#{
        <<"orders">> =>
            hb_maps:put(OrderID, Order, order_book(Base, Opts), Opts)
    }.

%% @doc Forget an order. What the book holds is the offers still open to be
%% taken, so a completed or withdrawn one leaves no trace: opening an offer and
%% withdrawing it again returns the process to exactly where it was.
drop_order(Base, #{ <<"order-id">> := OrderID }, Opts) ->
    Base#{
        <<"orders">> => hb_maps:without([OrderID], order_book(Base, Opts), Opts)
    }.

%%% The ledger

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

%% @doc A transaction that carries trade keys as tags only, leaving the real
%% transaction fields empty.
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

offer(Wallet, Quantity, Asking, Deadline) ->
    tx(
        Wallet,
        #{
            <<"target">> => ?PROCESS,
            <<"action">> => <<"make-offer">>,
            <<"offer-quantity">> => hb_util:bin(Quantity),
            <<"asking">> => hb_util:bin(Asking),
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

%% @doc Reserve, and withdraw, an order at a height.
reserve(Base, Wallet, OrderID, Height, Opts) ->
    apply_tx(
        Base,
        order_action(Wallet, <<"register-interest">>, OrderID),
        Height,
        Opts
    ).

cancel(Base, Wallet, OrderID, Height, Opts) ->
    apply_tx(Base, order_action(Wallet, <<"cancel-order">>, OrderID), Height, Opts).

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

%% @doc Advance the process to a height without anything happening, which is
%% what the network's unrelated traffic does.
tick(Base, Height, Opts) ->
    apply_tx(Base, #{ <<"target">> => <<"someone-else">> }, Height, Opts).

%% @doc The orders the process holds, as plain maps. Reading the book as a
%% message leaves a `hashpath' beside the orders, so anything that is not an
%% order is ignored rather than assumed away.
book(Base, Opts) ->
    [
        Order
    ||
        Order = #{ <<"order-id">> := _ } <-
            hb_maps:values(
                hb_cache:ensure_all_loaded(order_book(Base, Opts), Opts),
                Opts
            )
    ].

only_order(Base, Opts) ->
    [Order] = book(Base, Opts),
    Order.

order_id(Base, Opts) -> maps:get(<<"order-id">>, only_order(Base, Opts)).

%% @doc An offer, opened, ready to be traded against.
opened(Seller, SellerAddr, Opts) ->
    apply_tx(base(#{ SellerAddr => 100 }), offer(Seller, 10, 500, 20), 100, Opts).

%% @doc A message this device reads is a stranger's, and a key it does not carry
%% must be answered by this device, not by code the stranger picked.
%%
%% `all' mode hands every transaction on Arweave to this process, so the
%% `device' key of each is the author's to choose. Reading plainly dispatches on
%% it, and a device answers only for absent keys -- so the attack is to omit
%% one. Here the `action' is omitted and the script answers for it: were the key
%% resolved rather than read, a transaction addressed to the process could name
%% whichever action suited it while committing to none.
strangers_device_cannot_answer_for_an_absent_key_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    Hostile =
        tx(
            Seller,
            #{
                <<"target">> => ?PROCESS,
                <<"offer-quantity">> => <<"1">>,
                <<"asking">> => <<"10">>,
                <<"deadline">> => <<"100">>,
                %% No `action'. The script answers for it.
                <<"device">> => <<"lua@5.3a">>,
                <<"module">> =>
                    #{
                        <<"content-type">> => <<"text/x-lua">>,
                        <<"body">> =>
                            <<"function action(base, req)\n"
                                "  return \"ok\", \"make-offer\"\n"
                                "end\n">>
                    }
            }
        ),
    ?assertEqual(
        <<"make-offer">>,
        hb_ao:get(<<"action">>, Hostile, not_found, Opts)
    ),
    Tried = apply_tx(base(#{ SellerAddr => 1 }), Hostile, 10, Opts),
    ?assertEqual([], book(Tried, Opts)),
    ?assertEqual(1, balance(Tried, SellerAddr, Opts)).

%% @doc Trade keys carried as tags are metadata, not the transaction's own
%% fields: they route no control slot and pay for nothing. A transaction's
%% `target' and `quantity' are the only things the base layer itself moved.
tags_are_not_transaction_fields_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    Tagged =
        tag_only_tx(
            Seller,
            [
                {<<"target">>, ?PROCESS},
                {<<"action">>, <<"make-offer">>},
                {<<"offer-quantity">>, <<"10">>},
                {<<"asking">>, <<"500">>},
                {<<"deadline">>, <<"200">>}
            ]
        ),
    % The tags do reach the keys of the same names, and the fields do not.
    ?assertEqual(?PROCESS, hb_ao:get(<<"target">>, Tagged, not_found, Opts)),
    ?assertEqual(<<>>, tx_field(Tagged, <<"target">>, <<>>, Opts)),
    Ignored = apply_tx(base(#{ SellerAddr => 100 }), Tagged, 100, Opts),
    ?assertEqual([], book(Ignored, Opts)),
    ?assertEqual(100, balance(Ignored, SellerAddr, Opts)),
    % Nor does a tagged transfer settle an order that was opened properly.
    Opened = opened(Seller, SellerAddr, Opts),
    OrderID = order_id(Opened, Opts),
    TaggedPay =
        tag_only_tx(
            Buyer,
            [
                {<<"target">>, SellerAddr},
                {<<"quantity">>, <<"500">>},
                {<<"order-id">>, OrderID}
            ]
        ),
    Result = apply_tx(Opened, TaggedPay, 120, Opts),
    ?assertEqual([OrderID], [order_id(Result, Opts)]),
    ?assertEqual(0, balance(Result, BuyerAddr, Opts)).

%% @doc A stranger's transaction may ask to be routed anywhere, including to
%% keys `~message@1.0' would otherwise answer. Each is applied like any other
%% message rather than handing back the state, a list of key names, or an info
%% map as the new state.
stray_paths_are_applied_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Stranger, _} = party(),
    Opened = opened(Seller, SellerAddr, Opts),
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
                        ?BALANCES => #{ SellerAddr => 10000000 },
                        <<"body">> =>
                            tx(Stranger, #{ <<"target">> => <<"somebody-else">> })
                    },
                    Opts
                ),
            ?assertEqual(90, balance(State, SellerAddr, Opts)),
            ?assertEqual(1, length(book(State, Opts)))
        end,
        [<<"set">>, <<"keys">>, <<"info">>, <<"withdraw-everything">>]
    ).

%% @doc Opening an offer moves the goods into escrow.
make_offer_escrows_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    Opened = opened(Seller, SellerAddr, Opts),
    Order = only_order(Opened, Opts),
    ?assertEqual(90, balance(Opened, SellerAddr, Opts)),
    ?assertEqual(10, maps:get(<<"quantity">>, Order)),
    ?assertEqual(500, maps:get(<<"asking">>, Order)),
    ?assertEqual(SellerAddr, maps:get(<<"creator">>, Order)).

%% @doc A seller may offer everything they hold -- which for a name is the
%% single unit being sold -- and not a unit more.
offer_is_bounded_by_the_balance_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    Whole =
        apply_tx(base(#{ SellerAddr => 1 }), offer(Seller, 1, 500, 20), 100, Opts),
    ?assertEqual(1, maps:get(<<"quantity">>, only_order(Whole, Opts))),
    ?assertEqual(0, balance(Whole, SellerAddr, Opts)),
    Beyond =
        apply_tx(base(#{ SellerAddr => 4 }), offer(Seller, 5, 500, 20), 100, Opts),
    ?assertEqual([], book(Beyond, Opts)),
    ?assertEqual(4, balance(Beyond, SellerAddr, Opts)).

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
                <<"deadline">> => <<"200">>
            }
        ),
    Result = apply_tx(base(#{ SellerAddr => 100 }), Wrong, 100, Opts),
    ?assertEqual([], book(Result, Opts)),
    ?assertEqual(100, balance(Result, SellerAddr, Opts)).

%% @doc An action is matched however it is cased, as the token device this
%% settles alongside matches it. Two devices sharing one ledger and one schedule
%% must not disagree about what a message says.
action_case_is_ignored_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    Cased =
        tx(
            Seller,
            #{
                <<"target">> => ?PROCESS,
                <<"action">> => <<"Make-Offer">>,
                <<"offer-quantity">> => <<"10">>,
                <<"asking">> => <<"500">>,
                <<"deadline">> => <<"20">>
            }
        ),
    Opened = apply_tx(base(#{ SellerAddr => 100 }), Cased, 100, Opts),
    ?assertEqual(10, maps:get(<<"quantity">>, only_order(Opened, Opts))).

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
                <<"deadline">> => <<"tomorrow">>
            }
        ),
    Result = apply_tx(base(#{ SellerAddr => 100 }), Nonsense, 100, Opts),
    ?assertEqual([], book(Result, Opts)),
    ?assertEqual(100, balance(Result, SellerAddr, Opts)).

%% @doc An order is named by the transaction that opened it, so a second
%% sighting of that transaction must not escrow the goods twice.
duplicate_offer_escrows_once_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    Offer = offer(Seller, 10, 500, 20),
    Opened = apply_tx(base(#{ SellerAddr => 100 }), Offer, 100, Opts),
    Again = apply_tx(Opened, Offer, 101, Opts),
    ?assertEqual(1, length(book(Again, Opts))),
    ?assertEqual(90, balance(Again, SellerAddr, Opts)).

%% @doc Only the seller may withdraw their offer.
cancel_by_stranger_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Stranger, _} = party(),
    Opened = opened(Seller, SellerAddr, Opts),
    OrderID = order_id(Opened, Opts),
    Result =
        cancel(Opened, Stranger, OrderID, 110, Opts),
    ?assertEqual([OrderID], [order_id(Result, Opts)]),
    ?assertEqual(90, balance(Result, SellerAddr, Opts)).

%% @doc Opening an offer and withdrawing it leaves the process exactly as it
%% was, and the order is then spent: registering against it takes nothing.
offer_and_withdrawal_are_a_round_trip_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    Opened = opened(Seller, SellerAddr, Opts),
    OrderID = order_id(Opened, Opts),
    After =
        cancel(Opened, Seller, OrderID, 105, Opts),
    ?assertEqual([], book(After, Opts)),
    ?assertEqual(100, balance(After, SellerAddr, Opts)),
    Late =
        reserve(After, Buyer, OrderID, 110, Opts),
    ?assertEqual([], book(Late, Opts)),
    ?assertEqual(0, balance(Late, BuyerAddr, Opts)).

%% @doc A reserved order cannot be pulled out from under the buyer who reserved
%% it. This is the guarantee that makes paying safe. Once the window passes the
%% seller has it back.
reservation_prevents_cancellation_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, _} = party(),
    Opened = opened(Seller, SellerAddr, Opts),
    OrderID = order_id(Opened, Opts),
    Reserved =
        reserve(Opened, Buyer, OrderID, 110, Opts),
    ?assertEqual(130, maps:get(<<"reserved-until">>, only_order(Reserved, Opts))),
    Attempted =
        cancel(Reserved, Seller, OrderID, 111, Opts),
    ?assertEqual([OrderID], [order_id(Attempted, Opts)]),
    Lapsed =
        cancel(Reserved, Seller, OrderID, 131, Opts),
    ?assertEqual([], book(Lapsed, Opts)),
    ?assertEqual(100, balance(Lapsed, SellerAddr, Opts)).

%% @doc A reservation is exclusive while it lasts -- somebody else's payment
%% does not take the goods -- and only while it lasts.
reservation_is_exclusive_until_it_lapses_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, _} = party(),
    {Interloper, InterloperAddr} = party(),
    Opened = opened(Seller, SellerAddr, Opts),
    OrderID = order_id(Opened, Opts),
    Reserved =
        reserve(Opened, Buyer, OrderID, 110, Opts),
    % The stranger's AR buys nothing: the order is not theirs to complete, and
    % there is nothing here that could give it back.
    Early = apply_tx(Reserved, pay(Interloper, SellerAddr, 500, OrderID), 130, Opts),
    ?assertEqual([OrderID], [order_id(Early, Opts)]),
    ?assertEqual(0, balance(Early, InterloperAddr, Opts)),
    % Once the window passes the offer is anybody's again.
    Late = apply_tx(Reserved, pay(Interloper, SellerAddr, 500, OrderID), 131, Opts),
    ?assertEqual([], book(Late, Opts)),
    ?assertEqual(10, balance(Late, InterloperAddr, Opts)).

%% @doc A second buyer cannot take a reservation out from under the first, but
%% may have it once the window passes.
reservation_is_not_overwritten_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    {Other, OtherAddr} = party(),
    Opened = opened(Seller, SellerAddr, Opts),
    OrderID = order_id(Opened, Opts),
    Reserved =
        reserve(Opened, Buyer, OrderID, 110, Opts),
    Contested =
        reserve(Reserved, Other, OrderID, 120, Opts),
    ?assertEqual(BuyerAddr, maps:get(<<"buyer">>, only_order(Contested, Opts))),
    Relet =
        reserve(Reserved, Other, OrderID, 131, Opts),
    ?assertEqual(OtherAddr, maps:get(<<"buyer">>, only_order(Relet, Opts))).

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
                <<"minimum-fee">> => <<"1000">>,
                <<"deadline">> => <<"200">>
            }
        ),
    Opened = apply_tx(base(#{ SellerAddr => 100 }), Charged, 100, Opts),
    OrderID = order_id(Opened, Opts),
    Held = fun(State) -> maps:is_key(<<"buyer">>, only_order(State, Opts)) end,
    Free =
        reserve(Opened, Buyer, OrderID, 110, Opts),
    ?assertNot(Held(Free)),
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
    ?assertNot(Held(Stranded)),
    ?assertNot(Held(apply_tx(Opened, registration(Buyer, OrderID, 999), 110, Opts))),
    ?assert(Held(apply_tx(Opened, registration(Buyer, OrderID, 1000), 110, Opts))).

%% @doc The whole trade: the buyer pays the seller directly on layer one, and
%% the process -- which is not a party to that payment -- settles it.
settlement_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    Opened = opened(Seller, SellerAddr, Opts),
    OrderID = order_id(Opened, Opts),
    Reserved =
        reserve(Opened, Buyer, OrderID, 110, Opts),
    Settled = apply_tx(Reserved, pay(Buyer, SellerAddr, 500, OrderID), 115, Opts),
    % The offer is complete, so it is no longer an offer.
    ?assertEqual([], book(Settled, Opts)),
    ?assertEqual(10, balance(Settled, BuyerAddr, Opts)),
    ?assertEqual(90, balance(Settled, SellerAddr, Opts)).

%% @doc A payment settles only if it was really made, to the right address, for
%% at least the asking price, against an order the process holds.
payment_must_be_the_one_asked_for_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    {_, Stranger} = party(),
    Opened = opened(Seller, SellerAddr, Opts),
    OrderID = order_id(Opened, Opts),
    Rejected =
        fun(Body) ->
            Result = apply_tx(Opened, Body, 120, Opts),
            ?assertEqual([OrderID], [order_id(Result, Opts)]),
            ?assertEqual(0, balance(Result, BuyerAddr, Opts))
        end,
    % Too little: the process never held the value, so it cannot refund a
    % partial fill.
    Rejected(pay(Buyer, SellerAddr, 499, OrderID)),
    % Paid to somebody else.
    Rejected(pay(Buyer, Stranger, 500, OrderID)),
    % Naming an order this process has never heard of, which is what the
    % overwhelming majority of the network's traffic looks like.
    Unknown = <<"nOtAnOrDeR000000000000000000000000000000000">>,
    Rejected(pay(Buyer, SellerAddr, 500, Unknown)),
    % Paid by the seller, from the address that made the offer: a self-transfer
    % costs only a network fee, and would take the goods straight back.
    Self = apply_tx(Opened, pay(Seller, SellerAddr, 500, OrderID), 120, Opts),
    ?assertEqual([OrderID], [order_id(Self, Opts)]),
    ?assertEqual(90, balance(Self, SellerAddr, Opts)).

%% @doc Settling twice is not possible: the goods left with the first payment.
double_settlement_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    {Late, LateAddr} = party(),
    Opened = opened(Seller, SellerAddr, Opts),
    OrderID = order_id(Opened, Opts),
    Settled = apply_tx(Opened, pay(Buyer, SellerAddr, 500, OrderID), 120, Opts),
    Again = apply_tx(Settled, pay(Late, SellerAddr, 500, OrderID), 121, Opts),
    ?assertEqual(10, balance(Again, BuyerAddr, Opts)),
    ?assertEqual(0, balance(Again, LateAddr, Opts)),
    ?assertEqual(90, balance(Again, SellerAddr, Opts)).

%% @doc An `order-id' is caller-supplied text. Path-like values and reserved
%% keys must not reach anything that is then read as an order.
reserved_order_id_names_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, _} = party(),
    Opened = opened(Seller, SellerAddr, Opts),
    OrderID = order_id(Opened, Opts),
    lists:foreach(
        fun(Name) ->
            Result = apply_tx(Opened, pay(Buyer, SellerAddr, 500, Name), 120, Opts),
            ?assertEqual([OrderID], [order_id(Result, Opts)])
        end,
        [<<OrderID/binary, "/creator">>, <<"keys">>, <<"id">>, <<"commitments">>]
    ).

%% @doc Unrelated traffic -- the overwhelming majority of what this process is
%% sequenced by -- changes nothing at all.
unrelated_traffic_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    Opened = opened(Seller, SellerAddr, Opts),
    ?assertEqual(Opened, tick(Opened, 150, Opts)).

%% @doc Units are conserved across a whole trade: what is escrowed and what is
%% held always sum to the supply.
supply_is_conserved_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    Held =
        fun(State) ->
            balance(State, SellerAddr, Opts) + balance(State, BuyerAddr, Opts)
                + lists:sum(
                    [
                        maps:get(<<"quantity">>, Order)
                    ||
                        Order <- book(State, Opts)
                    ]
                )
        end,
    Before = base(#{ SellerAddr => 100, BuyerAddr => 5 }),
    ?assertEqual(105, Held(Before)),
    Opened = apply_tx(Before, offer(Seller, 10, 500, 20), 100, Opts),
    ?assertEqual(105, Held(Opened)),
    OrderID = order_id(Opened, Opts),
    Reserved =
        reserve(Opened, Buyer, OrderID, 110, Opts),
    ?assertEqual(105, Held(Reserved)),
    Settled = apply_tx(Reserved, pay(Buyer, SellerAddr, 500, OrderID), 115, Opts),
    ?assertEqual(105, Held(Settled)),
    ?assertEqual(105, Held(tick(Reserved, 300, Opts))).

%% @doc An order's figures are compared with the height of a later slot, and the
%% state between two slots is written to the process cache and read back. A
%% `reserved-until' that returned as a binary would order above every integer,
%% and the reservation would never lapse.
reservation_survives_the_cache_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, _} = party(),
    {Interloper, InterloperAddr} = party(),
    Opened = opened(Seller, SellerAddr, Opts),
    OrderID = order_id(Opened, Opts),
    Reserved =
        reserve(Opened, Buyer, OrderID, 110, Opts),
    {ok, ID} = hb_cache:write(Reserved, Opts),
    {ok, Read} = hb_cache:read(ID, Opts),
    Cached = hb_cache:ensure_all_loaded(Read, Opts),
    ?assertEqual(130, maps:get(<<"reserved-until">>, only_order(Cached, Opts))),
    Late = apply_tx(Cached, pay(Interloper, SellerAddr, 500, OrderID), 131, Opts),
    ?assertEqual([], book(Late, Opts)),
    ?assertEqual(10, balance(Late, InterloperAddr, Opts)).
