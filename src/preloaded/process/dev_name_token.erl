%%% @doc A name: a token with a single unit, whose holder decides what the name
%%% resolves to.
%%%
%%% The device is `token-1.0''s shape carrying `~reference@1.0''s payload. From
%%% the token it takes the state layout -- `name', `ticker', `denomination',
%%% `total-supply' and `balances' on a flat `~process@1.0' message -- the
%%% `transfer' action and its notices, and the rule that decides who may `set':
%%% the holder of the whole supply. From the reference it takes what `set'
%%% actually writes -- a linked message whose keys the name then resolves
%%% through to.
%%%
%%% Because the supply is one indivisible unit, those two things compose into a
%%% name: exactly one address holds it, that address alone may say what the name
%%% points at, and transferring the unit hands over that right with it. Selling
%%% the unit -- with `~arweave-swap@1.0', which settles against the same
%%% `balances' -- sells the name.
%%%
%%% The protocol is two messages:
%%% <ul>
%%%   <li>`transfer', carrying `recipient' and `quantity'. The sender is the
%%%       message's signer. A `Debit-Notice' and a `Credit-Notice' are emitted
%%%       with the same keys `token-1.0' uses.</li>
%%%   <li>`set', carrying either a `reference-value' -- the message the name is
%%%       to resolve to -- or nothing, in which case the name inherits the keys
%%%       of the `set' message itself, as `~reference@1.0' defines it. Only the
%%%       holder of the whole supply may send it, judged against the balances as
%%%       they stand at that slot.</li>
%%% </ul>
%%%
%%% There is no mint path, so the supply is fixed at spawn: a name cannot be
%%% diluted into existing twice.
%%%
%%% Ordering is the schedule's. `~reference@1.0' has to reconstruct which of
%%% several competing `set's won -- by a signer-declared timestamp, tie-broken
%%% by weave position -- because it is assembled from loose layer-1 messages. A
%%% process is handed that order already: the latest `set' in slot order wins,
%%% and there is nothing to declare and nothing to tie-break.
%%%
%%% Reads are paths into the state -- `/now/balances/<address>',
%%% `/now/total-supply', `/now/value/<key>' -- and not device keys, which is how
%%% `token-1.0' reads too. `compute' is the only key this device answers; every
%%% other one is `~message@1.0''s.
-module(dev_name_token).
-implements(<<"name-token@1.0">>).
%%% AO-Core API functions:
-export([compute/3, init/3, snapshot/3, normalize/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The balances submessage, shared with any device that settles in this token.
-define(BALANCES, <<"balances">>).
%%% The linked message whose keys the name resolves through to.
-define(VALUE, <<"value">>).

%% @doc The process lifecycle keys `~process@1.0' calls on an execution device.
%% This device's state is a plain message and its whole contents, so there is
%% nothing to boot, nothing to serialize that is not already here, and nothing
%% to restore. Declaring them is what keeps `~message@1.0' -- which owns every
%% other key of this state -- from answering `not_found' and failing the slot.
init(Base, _Req, _Opts) -> {ok, Base}.
snapshot(Base, _Req, _Opts) -> {ok, Base}.
normalize(Base, _Req, _Opts) -> {ok, Base}.

%% @doc Apply one assignment to the name's state. This is the device's only
%% key: `~arweave-scheduler@1.0' pins `path: compute' on every `all'-mode
%% assignment, so no stranger's tag can choose another.
%%
%% A name that is for sale is settled in the same balances it keeps, so every
%% message is offered to the selling device first -- including the ones this
%% device would otherwise ignore, since the payment that buys a name is a
%% transfer between two other addresses. Which device that is, is a scalar key
%% on the process; see `swap/3' for why it is not a `~stack@1.0'.
compute(Base, Assignment, Opts) ->
    Sold = swap(seed(Base, Opts), Assignment, Opts),
    Body = hb_maps:get(<<"body">>, Assignment, #{}, Opts),
    ProcID = hb_maps:get(<<"process">>, Assignment, <<>>, Opts),
    case tx_field(Body, <<"target">>, <<>>, Opts) of
        % Sent to nobody, and so not to us: an absent `process' must not alias
        % an absent `target' and address every such transaction to this name.
        <<>> -> {ok, Sold};
        ProcID -> {ok, action(Sold, Body, Opts)};
        _ ->
            % Not addressed to this name. Under `all' mode that is almost every
            % transaction on the network.
            {ok, Sold}
    end.

%% @doc Hand the message to the device that sells this name, if it has one, and
%% take back the state it produces.
%%
%% This is a `~stack@1.0' written out by hand, and deliberately so. A process
%% spawned as an Arweave transaction can only carry flat, scalar tags: the codec
%% turns a submessage or a list into a `+link' to content that is not on the
%% weave, so nothing else can read it back. `device-stack' is a list, so a stack
%% cannot survive the spawn -- but `swap-device' is one word.
%%
%% A device that cannot answer leaves the name untouched, and that includes one
%% that raises: `hb_device:message_to_device/2' throws for a name it cannot
%% resolve, and it throws from outside `hb_ao''s own guard. The device is named
%% by a scalar tag fixed at the spawn and changeable never, so a seller who
%% mis-spells it would otherwise mint a name that fails its every slot, on every
%% node, for good. Failing here instead fails the sale closed and leaves the
%% name open: nothing can be offered, so nothing can be bought, and the name
%% still works as a name.
swap(Base, Assignment, Opts) ->
    case hb_ao:get(<<"swap-device">>, Base, not_found, Opts) of
        not_found -> Base;
        Device ->
            try hb_ao:resolve(Base#{ <<"device">> => Device }, Assignment, Opts) of
                {ok, Settled} -> Settled#{ <<"device">> => <<"name-token@1.0">> };
                Other ->
                    ?event(
                        {name_token_swap_declined,
                            {device, Device},
                            {res, Other}
                        }
                    ),
                    Base
            catch
                Class:Reason ->
                    ?event(
                        {name_token_swap_failed,
                            {device, Device},
                            {error, {Class, Reason}}
                        }
                    ),
                    Base
            end
    end.

%% @doc Give the name its single unit, and whatever it was minted pointing at,
%% the first time it computes.
%%
%% Neither can be written into the spawn directly: `balances' and the linked
%% message are submessages, and a submessage does not cross the chain. What does
%% cross is a scalar -- `initial-holder', an address, and `initial-value', the
%% id of whatever the name should resolve to. Values keep their case, where keys
%% are lowercased and a lowercased address is a different address.
%%
%% Seeding a value at spawn is what makes a name cheap to mint. Without it, a
%% freshly spawned name says nothing until somebody sends it a `set', and the
%% first message addressed to a process pays Arweave's fee for creating that
%% address -- so every name would cost that before it could resolve at all.
%%
%% Both are seeded together, in the one slot where the name has no balances yet,
%% so a value the holder later replaces is never handed back to them.
seed(Base, Opts) ->
    case {hb_ao:get(?BALANCES, Base, not_found, Opts),
            hb_ao:get(<<"initial-holder">>, Base, not_found, Opts)} of
        {not_found, Holder} when Holder =/= not_found ->
            Supply = supply(Base, Opts),
            ?event({name_token_seeded, {holder, Holder}, {supply, Supply}}),
            seed_value(Base#{ ?BALANCES => #{ Holder => Supply } }, Opts);
        _ -> Base
    end.

%% @doc A name minted pointing somewhere resolves there from its first slot. The
%% value is a message of its own -- `{ target: <id> }' -- so that a `set' can
%% later replace it with anything at all without the shape changing underneath
%% whatever is reading it.
seed_value(Base, Opts) ->
    case hb_ao:get(<<"initial-value">>, Base, not_found, Opts) of
        not_found -> Base;
        Target ->
            ?event({name_token_seeded_value, {target, Target}}),
            Base#{ ?VALUE => #{ <<"target">> => Target } }
    end.

%% @doc Route a message addressed to the name by its `action'. Matching is
%% case-insensitive, as `token-1.0' matches. An unknown action leaves the state
%% untouched rather than failing the slot, which would stop the process on every
%% node for good.
action(Base, Body, Opts) ->
    case hb_util:to_lower(hb_maps:get(<<"action">>, Body, <<>>, Opts)) of
        <<"transfer">> -> transfer(Base, Body, Opts);
        <<"set">> -> set_value(Base, Body, Opts);
        _ -> Base
    end.

%%% The token

%% @doc Move units between balances. Nothing is written unless the whole
%% transfer is admissible, so a rejected one is indistinguishable from a message
%% that was never sent.
transfer(Base, Body, Opts) ->
    maybe
        {ok, Sender} ?= signer(Body, Opts),
        Recipient = hb_maps:get(<<"recipient">>, Body, not_found, Opts),
        true ?= is_binary(Recipient),
        {ok, Quantity} ?=
            hb_util:safe_int(hb_maps:get(<<"quantity">>, Body, 0, Opts)),
        true ?= Quantity >= 1,
        true ?= balance(Base, Sender, Opts) >= Quantity,
        ?event(
            {name_token_transfer,
                {from, Sender},
                {to, Recipient},
                {quantity, Quantity}
            }
        ),
        notices(
            credit(debit(Base, Sender, Quantity, Opts), Recipient, Quantity, Opts),
            Sender,
            Recipient,
            Quantity
        )
    else
        _ -> Base
    end.

%% @doc Emit the pair of notices that `token-1.0' emits for a transfer, with the
%% same keys. They are the slot's results; whether anything delivers them is the
%% process's business, not this device's.
notices(Base, Sender, Recipient, Quantity) ->
    Base#{
        <<"results">> =>
            #{
                <<"outbox">> =>
                    #{
                        <<"1">> =>
                            #{
                                <<"target">> => Sender,
                                <<"action">> => <<"Debit-Notice">>,
                                <<"recipient">> => Recipient,
                                <<"quantity">> => Quantity
                            },
                        <<"2">> =>
                            #{
                                <<"target">> => Recipient,
                                <<"action">> => <<"Credit-Notice">>,
                                <<"sender">> => Sender,
                                <<"quantity">> => Quantity
                            }
                    }
            }
    }.

%%% The name

%% @doc Write the message the name resolves to. The value is the message's
%% `reference-value' if it carries one, and otherwise the message itself, less
%% the keys that carried it here -- exactly the choice `~reference@1.0' offers.
set_value(Base, Body, Opts) ->
    maybe
        {ok, Signer} ?= signer(Body, Opts),
        true ?= owns_supply(Base, Signer, Opts),
        ?event({name_token_set, {by, Signer}}),
        Base#{ ?VALUE => value_of(Body, Opts) }
    else
        _ -> Base
    end.

%% @doc The message a `set' is asking the name to resolve to. With no
%% `reference-value' the `set' is its own value, less the transaction envelope
%% the base layer wrapped it in and the `action' that addressed it here: what
%% the name says is what the sender wrote, not how it reached us.
value_of(Body, Opts) ->
    case hb_maps:get(<<"reference-value">>, Body, not_found, Opts) of
        not_found ->
            hb_maps:without(
                [
                    <<"action">>,
                    <<"target">>,
                    <<"quantity">>,
                    <<"anchor">>,
                    <<"reward">>,
                    <<"commitments">>
                ],
                hb_cache:ensure_all_loaded(Body, Opts),
                Opts
            );
        Value -> hb_cache:ensure_all_loaded(Value, Opts)
    end.

%% @doc Whether an address may speak for the name: it must hold the whole
%% supply. The rule is evaluated against the balances as they stand, so the
%% authority moves with the unit, with no separate owner field to keep in step.
owns_supply(Base, Address, Opts) ->
    balance(Base, Address, Opts) >= supply(Base, Opts).

%% @doc How many units there are. The figure is a scalar tag on the spawn and
%% `hb_util:int/1' raises on anything that is not a number, so a name minted
%% with `total-supply: one' reads as the single unit a name is -- the same
%% answer as a name that named no supply at all -- rather than as a process that
%% raises on its every slot, for good, on every node.
supply(Base, Opts) ->
    hb_util:ok_or(
        hb_util:safe_int(hb_ao:get(<<"total-supply">>, Base, 1, Opts)),
        1
    ).

%%% State

%% @doc Read a value from the real layer-1 transaction fields. A `target' tag is
%% not an address: only the field the base layer itself moved value to decides
%% who a transaction was sent to. See `dev_arweave_swap:tx_field/4'.
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
%% signers is not attributable to one party, so it can neither move the unit nor
%% speak for the name.
signer(Body, Opts) ->
    case hb_message:signers(Body, Opts) of
        [Signer] -> {ok, hb_util:human_id(Signer)};
        _ -> not_found
    end.

balance(Base, Address, Opts) ->
    hb_util:int(hb_ao:get([?BALANCES, Address], Base, 0, Opts)).

%% @doc Write one balance back. Only whole top-level keys are written: setting a
%% nested path would resolve the keys above it through this device on the way
%% down.
credit(Base, _Address, 0, _Opts) -> Base;
credit(Base, Address, Amount, Opts) ->
    Base#{
        ?BALANCES =>
            hb_maps:put(
                Address,
                balance(Base, Address, Opts) + Amount,
                hb_ao:get(?BALANCES, Base, #{}, Opts),
                Opts
            )
    }.

debit(Base, Address, Amount, Opts) -> credit(Base, Address, -Amount, Opts).

%%% Tests

%%% The tests drive `compute/3' directly with synthetic assignments, exactly as
%%% `~process@1.0' would. Below them is the live-network driver that seeded the
%%% permanent fixtures, and the fixture tests that replay them.

%%% A process id, which must be a real 43-character address: the gate below
%%% reads the transaction field the base layer moved value to, and anything else
%%% is not an address the codec can carry there.
-define(PROCESS, <<"nAmEtOkEn0000000000000000000000000000000000">>).

test_opts() -> #{ <<"priv-wallet">> => ar_wallet:new() }.

party() ->
    Wallet = ar_wallet:new(),
    {Wallet, hb_util:human_id(ar_wallet:to_address(Wallet))}.

%% @doc A name held by one address, as it stands at spawn.
name_held_by(Address) ->
    #{
        <<"name">> => <<"test-name">>,
        <<"ticker">> => <<"NAME">>,
        <<"denomination">> => 0,
        <<"total-supply">> => 1,
        ?BALANCES => #{ Address => 1 }
    }.

tx(Wallet, Fields) ->
    hb_message:commit(
        Fields,
        #{ <<"priv-wallet">> => Wallet },
        #{ <<"commitment-device">> => <<"tx@1.0">> }
    ).

%% @doc Sequence a transaction into the process, as `~arweave-scheduler@1.0' in
%% `all' mode does. The `path' is part of an assignment -- see
%% `lib_scheduler:base_assignment/4' -- and the swap device is handed the whole
%% assignment, so an assignment without one is not one this device would ever
%% see.
apply_tx(Base, Body, Opts) ->
    {ok, New} =
        compute(
            Base,
            #{
                <<"path">> => <<"compute">>,
                <<"process">> => ?PROCESS,
                <<"slot">> => 1,
                <<"body">> => Body
            },
            Opts
        ),
    New.

transfer_tx(Wallet, Recipient, Quantity) ->
    tx(
        Wallet,
        #{
            <<"target">> => ?PROCESS,
            <<"action">> => <<"transfer">>,
            <<"recipient">> => Recipient,
            <<"quantity">> => hb_util:bin(Quantity)
        }
    ).

set_tx(Wallet, Fields) ->
    tx(Wallet, Fields#{ <<"target">> => ?PROCESS, <<"action">> => <<"set">> }).

held_by(Base, Address, Opts) -> balance(Base, Address, Opts).

value(Base, Opts) ->
    hb_cache:ensure_all_loaded(hb_ao:get(?VALUE, Base, #{}, Opts), Opts).

says(Base, Key, Opts) -> hb_ao:get(Key, value(Base, Opts), not_found, Opts).

%% @doc A `target' tag is not an address. A transaction addressed to nobody
%% carries the process's own id at the key a plain read would find, so the gate
%% reads the field the base layer moved value to instead -- the same rule
%% `~arweave-swap@1.0' settles payments by.
tags_are_not_transaction_fields_test() ->
    Opts = test_opts(),
    {_, OwnerAddr} = party(),
    {Stranger, StrangerAddr} = party(),
    Signed =
        ar_tx:sign(
            #tx{
                format = 2,
                reward = 1,
                tags =
                    [
                        {<<"target">>, ?PROCESS},
                        {<<"action">>, <<"transfer">>},
                        {<<"recipient">>, StrangerAddr},
                        {<<"quantity">>, <<"1">>}
                    ]
            },
            Stranger
        ),
    Steal = hb_message:convert(Signed, <<"structured@1.0">>, <<"tx@1.0">>, #{}),
    ?assertEqual(?PROCESS, hb_ao:get(<<"target">>, Steal, not_found, Opts)),
    ?assertEqual(<<>>, tx_field(Steal, <<"target">>, <<>>, Opts)),
    Tried = apply_tx(name_held_by(OwnerAddr), Steal, Opts),
    ?assertEqual(1, held_by(Tried, OwnerAddr, Opts)),
    ?assertEqual(0, held_by(Tried, StrangerAddr, Opts)).

%% @doc A foreign transaction's device cannot interpret absent envelope fields.
foreign_device_is_data_test() ->
    Opts = test_opts(),
    {Owner, OwnerAddr} = party(),
    Foreign = tx(Owner, #{ <<"device">> => <<"reference@1.0">> }),
    Untouched = apply_tx(name_held_by(OwnerAddr), Foreign, Opts),
    ?assertEqual(1, held_by(Untouched, OwnerAddr, Opts)).

%% @doc The unit moves, and the pair of notices `token-1.0' emits go with it.
transfer_moves_the_unit_test() ->
    Opts = test_opts(),
    {Owner, OwnerAddr} = party(),
    {_, BuyerAddr} = party(),
    Moved =
        apply_tx(name_held_by(OwnerAddr), transfer_tx(Owner, BuyerAddr, 1), Opts),
    ?assertEqual(0, held_by(Moved, OwnerAddr, Opts)),
    ?assertEqual(1, held_by(Moved, BuyerAddr, Opts)),
    Outbox =
        hb_ao:get(<<"results/outbox">>, {as, <<"message@1.0">>, Moved}, #{}, Opts),
    ?assertEqual(
        <<"Debit-Notice">>,
        hb_ao:get(<<"1/action">>, Outbox, not_found, Opts)
    ),
    ?assertEqual(BuyerAddr, hb_ao:get(<<"1/recipient">>, Outbox, not_found, Opts)),
    ?assertEqual(
        <<"Credit-Notice">>,
        hb_ao:get(<<"2/action">>, Outbox, not_found, Opts)
    ),
    ?assertEqual(OwnerAddr, hb_ao:get(<<"2/sender">>, Outbox, not_found, Opts)).

%% @doc Nobody can send what they do not hold.
transfer_beyond_balance_test() ->
    Opts = test_opts(),
    {Owner, OwnerAddr} = party(),
    {Stranger, _} = party(),
    {_, ElsewhereAddr} = party(),
    Base = name_held_by(OwnerAddr),
    TooMuch = apply_tx(Base, transfer_tx(Owner, ElsewhereAddr, 2), Opts),
    ?assertEqual(1, held_by(TooMuch, OwnerAddr, Opts)),
    NotTheirs = apply_tx(Base, transfer_tx(Stranger, ElsewhereAddr, 1), Opts),
    ?assertEqual(0, held_by(NotTheirs, ElsewhereAddr, Opts)).

%% @doc The holder says what the name resolves to, by handing over a message.
set_writes_the_linked_message_test() ->
    Opts = test_opts(),
    {Owner, OwnerAddr} = party(),
    Set =
        apply_tx(
            name_held_by(OwnerAddr),
            set_tx(
                Owner,
                #{
                    <<"reference-value">> =>
                        #{
                            <<"content-type">> => <<"text/html">>,
                            <<"body">> => <<"<h1>hello</h1>">>
                        }
                }
            ),
            Opts
        ),
    ?assertEqual(<<"text/html">>, says(Set, <<"content-type">>, Opts)),
    ?assertEqual(<<"<h1>hello</h1>">>, says(Set, <<"body">>, Opts)).

%% @doc With no `reference-value', the name inherits the keys of the `set'
%% itself -- less the keys that carried it here.
set_without_value_inherits_own_keys_test() ->
    Opts = test_opts(),
    {Owner, OwnerAddr} = party(),
    Set =
        apply_tx(
            name_held_by(OwnerAddr),
            set_tx(Owner, #{ <<"greeting">> => <<"ahoy">> }),
            Opts
        ),
    ?assertEqual(<<"ahoy">>, says(Set, <<"greeting">>, Opts)),
    ?assertEqual(not_found, says(Set, <<"action">>, Opts)),
    ?assertEqual(not_found, says(Set, <<"target">>, Opts)).

%% @doc The authority is the holding, not a recorded owner: it goes with the
%% unit and needs nothing kept in step. This is what makes a name sellable.
authority_follows_the_unit_test() ->
    Opts = test_opts(),
    {Owner, OwnerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    Before =
        apply_tx(
            name_held_by(OwnerAddr),
            set_tx(Owner, #{ <<"points-at">> => <<"seller">> }),
            Opts
        ),
    ?assertEqual(<<"seller">>, says(Before, <<"points-at">>, Opts)),
    Sold = apply_tx(Before, transfer_tx(Owner, BuyerAddr, 1), Opts),
    % The seller has handed over the unit, and with it the right to speak.
    Stale =
        apply_tx(
            Sold,
            set_tx(Owner, #{ <<"points-at">> => <<"seller again">> }),
            Opts
        ),
    ?assertEqual(<<"seller">>, says(Stale, <<"points-at">>, Opts)),
    Fresh = apply_tx(Sold, set_tx(Buyer, #{ <<"points-at">> => <<"buyer">> }), Opts),
    ?assertEqual(<<"buyer">>, says(Fresh, <<"points-at">>, Opts)),
    ?assertEqual(1, held_by(Fresh, BuyerAddr, Opts)),
    ?assertEqual(0, held_by(Fresh, OwnerAddr, Opts)).

%% @doc A partial holding is not the whole supply, so it does not carry the
%% authority. (A name is indivisible, but the rule is the whole supply.)
partial_holding_cannot_set_test() ->
    Opts = test_opts(),
    {Owner, OwnerAddr} = party(),
    {_, OtherAddr} = party(),
    Split =
        (name_held_by(OwnerAddr))#{
            <<"total-supply">> => 2,
            ?BALANCES => #{ OwnerAddr => 1, OtherAddr => 1 }
        },
    Result =
        apply_tx(Split, set_tx(Owner, #{ <<"greeting">> => <<"half mine">> }), Opts),
    ?assertEqual(#{}, value(Result, Opts)).

%% @doc There is no mint path, so the supply cannot grow -- and an unknown
%% action is ignored rather than failing the slot, whatever its casing.
actions_are_matched_by_name_only_test() ->
    Opts = test_opts(),
    {Owner, OwnerAddr} = party(),
    {_, BuyerAddr} = party(),
    Minted =
        apply_tx(
            name_held_by(OwnerAddr),
            tx(
                Owner,
                #{
                    <<"target">> => ?PROCESS,
                    <<"action">> => <<"mint">>,
                    <<"quantity">> => <<"1000">>
                }
            ),
            Opts
        ),
    ?assertEqual(1, held_by(Minted, OwnerAddr, Opts)),
    ?assertEqual(1, hb_util:int(hb_ao:get(<<"total-supply">>, Minted, 0, Opts))),
    % A known action is matched however it is cased, as `token-1.0' matches.
    Moved =
        apply_tx(
            name_held_by(OwnerAddr),
            tx(
                Owner,
                #{
                    <<"target">> => ?PROCESS,
                    <<"action">> => <<"Transfer">>,
                    <<"recipient">> => BuyerAddr,
                    <<"quantity">> => <<"1">>
                }
            ),
            Opts
        ),
    ?assertEqual(1, held_by(Moved, BuyerAddr, Opts)).

%% @doc A transaction that is not addressed to this name is left alone. Under
%% `all' mode that is almost every transaction on the network.
unrelated_traffic_test() ->
    Opts = test_opts(),
    {Stranger, _} = party(),
    {_, OwnerAddr} = party(),
    Base = name_held_by(OwnerAddr),
    Result =
        apply_tx(
            Base,
            tx(
                Stranger,
                #{ <<"target">> => <<"somebody-else">>, <<"quantity">> => <<"1">> }
            ),
            Opts
        ),
    ?assertEqual(1, held_by(Result, OwnerAddr, Opts)).

%% @doc A name spawned as an Arweave transaction carries only scalars, so it
%% names its first holder and what it points at rather than holding a `balances'
%% submessage and a linked message. Both appear the first time it computes, so a
%% name resolves without anybody having to send it anything.
spawn_seeds_the_holder_and_the_value_test() ->
    Opts = test_opts(),
    {Owner, OwnerAddr} = party(),
    Spawned =
        #{
            <<"name">> => <<"pn-test-1">>,
            <<"total-supply">> => 1,
            <<"initial-holder">> => OwnerAddr,
            <<"initial-value">> => <<"aBcDeFgHiJkLmNoPqRsTuVwXyZ0123456789_-aBcDe">>
        },
    % Any message at all brings the name to life, including one that is not
    % addressed to it.
    Alive =
        apply_tx(Spawned, tx(Owner, #{ <<"target">> => <<"elsewhere">> }), Opts),
    ?assertEqual(1, held_by(Alive, OwnerAddr, Opts)),
    ?assertEqual(
        <<"aBcDeFgHiJkLmNoPqRsTuVwXyZ0123456789_-aBcDe">>,
        says(Alive, <<"target">>, Opts)
    ),
    % And the holder can immediately point it somewhere else.
    Reset = apply_tx(Alive, set_tx(Owner, #{ <<"greeting">> => <<"moved">> }), Opts),
    ?assertEqual(<<"moved">>, says(Reset, <<"greeting">>, Opts)),
    ?assertEqual(not_found, says(Reset, <<"target">>, Opts)).

%% @doc A figure that is not a number is read as the single unit a name is.
%% `total-supply' is a scalar tag on the spawn, and it is read outside any
%% `maybe': a name that raised here would fail every slot it ever had.
unreadable_supply_is_one_unit_test() ->
    Opts = test_opts(),
    {Owner, OwnerAddr} = party(),
    Alive =
        apply_tx(
            #{
                <<"name">> => <<"test-name">>,
                <<"total-supply">> => <<"one">>,
                <<"initial-holder">> => OwnerAddr
            },
            tx(Owner, #{ <<"target">> => <<"elsewhere">> }),
            Opts
        ),
    ?assertEqual(1, held_by(Alive, OwnerAddr, Opts)),
    % And the holder of that one unit speaks for the name.
    Set = apply_tx(Alive, set_tx(Owner, #{ <<"greeting">> => <<"mine">> }), Opts),
    ?assertEqual(<<"mine">>, says(Set, <<"greeting">>, Opts)).

%% @doc Seeding happens once. A name whose unit has moved on is not handed a
%% fresh one -- nor its first value back -- by the next message that arrives.
seeding_happens_once_test() ->
    Opts = test_opts(),
    {Owner, OwnerAddr} = party(),
    {Stranger, StrangerAddr} = party(),
    Spawned =
        #{
            <<"name">> => <<"test-name">>,
            <<"total-supply">> => 1,
            <<"initial-holder">> => OwnerAddr,
            <<"initial-value">> => <<"aBcDeFgHiJkLmNoPqRsTuVwXyZ0123456789_-aBcDe">>
        },
    Alive = apply_tx(Spawned, tx(Owner, #{ <<"target">> => <<"elsewhere">> }), Opts),
    Said = apply_tx(Alive, set_tx(Owner, #{ <<"greeting">> => <<"mine">> }), Opts),
    Moved = apply_tx(Said, transfer_tx(Owner, StrangerAddr, 1), Opts),
    Later =
        apply_tx(Moved, tx(Stranger, #{ <<"target">> => <<"elsewhere">> }), Opts),
    ?assertEqual(0, held_by(Later, OwnerAddr, Opts)),
    ?assertEqual(1, held_by(Later, StrangerAddr, Opts)),
    ?assertEqual(<<"mine">>, says(Later, <<"greeting">>, Opts)),
    ?assertEqual(not_found, says(Later, <<"target">>, Opts)).

%% @doc A name with no selling device is simply a name: messages the swap would
%% have handled do nothing, and the name still works. A name whose selling
%% device cannot answer is the same name -- the sale fails closed, not the
%% process.
without_a_working_swap_device_test() ->
    Opts = test_opts(),
    {Owner, OwnerAddr} = party(),
    lists:foreach(
        fun(Base) ->
            Set =
                apply_tx(
                    Base,
                    set_tx(Owner, #{ <<"greeting">> => <<"no swap here">> }),
                    Opts
                ),
            ?assertEqual(<<"no swap here">>, says(Set, <<"greeting">>, Opts)),
            ?assertEqual(not_found, hb_ao:get(<<"orders">>, Set, not_found, Opts))
        end,
        [
            name_held_by(OwnerAddr),
            (name_held_by(OwnerAddr))#{ <<"swap-device">> => <<"nOt-A-dEvIcE@1.0">> }
        ]
    ).

%% @doc A name is sold by the device it names, settling in the balances the name
%% itself keeps -- and the authority goes with the unit, so the buyer speaks for
%% the name and the seller no longer does.
%%
%% The payment is the point: an ordinary transfer between two addresses that the
%% process is not a party to, seen only because every transaction on Arweave is
%% a slot of it.
sold_through_the_swap_device_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    ForSale =
        (name_held_by(SellerAddr))#{ <<"swap-device">> => <<"arweave-swap@1.0">> },
    Offered =
        apply_tx(
            ForSale,
            tx(
                Seller,
                #{
                    <<"target">> => ?PROCESS,
                    <<"action">> => <<"make-offer">>,
                    <<"offer-quantity">> => <<"1">>,
                    <<"asking">> => <<"500">>,
                    <<"deadline">> => <<"20">>
                }
            ),
            Opts
        ),
    % The unit is in escrow, so nobody holds the name and nobody speaks for it.
    ?assertEqual(0, held_by(Offered, SellerAddr, Opts)),
    Mute =
        apply_tx(
            Offered,
            set_tx(Seller, #{ <<"greeting">> => <<"still mine">> }),
            Opts
        ),
    ?assertEqual(not_found, says(Mute, <<"greeting">>, Opts)),
    Paid =
        apply_tx(
            Offered,
            tx(
                Buyer,
                #{
                    <<"target">> => SellerAddr,
                    <<"quantity">> => <<"500">>,
                    <<"order-id">> => order_of(Offered, Opts)
                }
            ),
            Opts
        ),
    ?assertEqual(1, held_by(Paid, BuyerAddr, Opts)),
    ?assertEqual(0, held_by(Paid, SellerAddr, Opts)),
    % And the buyer now says what the name means.
    Spoken =
        apply_tx(Paid, set_tx(Buyer, #{ <<"greeting">> => <<"mine now">> }), Opts),
    ?assertEqual(<<"mine now">>, says(Spoken, <<"greeting">>, Opts)).

%% @doc The id of the one order the name's selling device is holding.
order_of(Base, Opts) ->
    [Order] =
        [
            Held
        ||
            Held = #{ <<"order-id">> := _ } <-
                hb_maps:values(
                    hb_cache:ensure_all_loaded(
                        hb_ao:get(<<"orders">>, Base, #{}, Opts),
                        Opts
                    ),
                    Opts
                )
        ],
    hb_maps:get(<<"order-id">>, Order, not_found, Opts).

%%% The permanent fixtures
%%%
%%% Three stories, played out on mainnet against the real weave, and replayed
%%% here from the blocks that hold them. Every read below is a deterministic
%%% read of the weave capped at a pinned height, so each answer is immutable: no
%%% block after the cap can reach the process.
%%%
%%% The stories pin transaction ids rather than slot numbers, because a slot
%%% number is a fact about the weave -- every transaction on the network takes
%%% one -- while the id is the message itself.
%%%
%%% The driver that posted them is not kept here: it spends real AR, so it can
%%% never run in the battery, and what it produced is already permanent. It is
%%% in the history if a later protocol needs fresh fixtures minted:
%%%
%%%     git show 824816e7c:src/preloaded/process/dev_name_token.erl

%% @doc Node options pinned to a story's last block. `name-resolvers' is what
%% lets `~name@1.0' find the process by its name.
story_opts(MaxHeight, Process) ->
    TestStore = hb_test_utils:test_store(),
    IndexStore = hb_test_utils:test_store(),
    (hb_opts:default_message())#{
        <<"store">> => [
            TestStore,
            #{
                <<"store-module">> => hb_store_arweave,
                <<"name">> => <<"cache-arweave">>,
                <<"index-store">> => [IndexStore]
            },
            #{
                <<"store-module">> => hb_store_gateway,
                <<"local-store">> => [TestStore]
            }
        ],
        <<"arweave-index-store">> => #{ <<"index-store">> => [IndexStore] },
        <<"arweave-index-workers">> => 8,
        <<"arweave-scheduler-confirmation-depth">> => 1,
        <<"arweave-scheduler-max-height">> => MaxHeight,
        <<"name-resolvers">> => [#{ <<"test-name">> => Process }],
        <<"node-host">> => <<"host">>,
        <<"priv-wallet">> => ar_wallet:new()
    }.

%% @doc Synchronize a story's schedule from the network, retrying while the
%% gateway rate-limits us -- the same allowance the scheduler's own fixture
%% tests make. Priming the schedule also puts the process message in the node's
%% cache in its canonical `tx@1.0' decoding, rather than a gateway store's
%% lossier one.
story_sync(_Process, _Opts, 0) -> {error, sync_failed};
story_sync(Process, Opts, Attempts) ->
    case
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave-scheduler@1.0">> },
            #{
                <<"path">> => <<"schedule">>,
                <<"method">> => <<"GET">>,
                <<"target">> => Process
            },
            Opts
        )
    of
        {ok, Schedule} -> {ok, Schedule};
        _ ->
            timer:sleep(5000),
            story_sync(Process, Opts, Attempts - 1)
    end.

%% @doc The slot a transaction was given.
slot_of(Schedule, TXID, Opts) ->
    Assignments =
        hb_ao:normalize_keys(hb_ao:get(<<"assignments">>, Schedule, Opts), Opts),
    % A schedule's assignments are keyed by slot, alongside the keys any
    % committed message carries; only the numbered ones are slots.
    Found =
        [
            Slot
        ||
            {Key, Assignment} <- hb_maps:to_list(Assignments, Opts),
            {ok, Slot} <- [hb_util:safe_int(Key)],
            is_map(Assignment),
            hb_util:human_id(
                hb_message:id(hb_ao:get(<<"body">>, Assignment, Opts), signed, Opts)
            ) =:= TXID
        ],
    case Found of
        [Slot | _] -> Slot;
        [] -> error({not_scheduled, TXID})
    end.

%% @doc Read a process's state as it stood at the end of a given slot, in the
%% form a reader can follow: `<process>~process@1.0/compute&slot=<n>/<path>'.
at(Process, Slot, Path, Opts) ->
    hb_ao:resolve(
        <<
            Process/binary,
            "~process@1.0/compute&slot=",
            (hb_util:bin(Slot))/binary,
            "/",
            Path/binary
        >>,
        Opts#{ <<"hashpath">> => ignore }
    ).

%%% Story one: a name that had to be paid for.
%%%
%%% The seller spawned a name holding its single unit, offered it for 1000
%%% winston with a 100,000,000 winston fee to register, and turned away everyone
%%% who would not pay: a registration that paid only what the network charges
%%% anyway, and one that sent value to the process instead. The buyer who
%%% overpaid the reward reserved it, an interloper's payment bought nothing, and
%%% the buyer's own payment settled it -- an ordinary transfer between two
%%% addresses that the process was never a party to.
%%%
%%%   make-offer          the seller's unit goes into escrow, the order opens
%%%   register:underpaid  a reward of 33,039,920 does not clear a 100,000,000
%%%   fee register:stranded   150,000,000 sent *to the process* is not the fee
%%%   either register:paid       a reward of 150,000,000 does clear it --
%%%   reserved payment:interloper  the underpayer pays anyway, but the order is
%%%   not theirs payment:buyer       the registrant pays -- settled, and the
%%%   name moves set:former-owner    the seller no longer holds it, so is no
%%%   longer heard set:owner           the buyer says what the name means
%%%   stray:path-info     a stranger routes a slot at `info'; nothing breaks
%%%   make-offer:nonsense a deadline of `tomorrow' opens no order
-define(SALE_PROCESS, <<"an95oAK9MlahZI_tKKeG4ykzNN01qfMi2WfJO58o_UU">>).
-define(SELLER, <<"ggltHF0Cnv9ylH3vM1p7amR2vXLMoPLQIUQmAEwLP-k">>).
-define(UNDERPAYER, <<"2yvAwMDrF62hpH_kKTfguatzB9mKVzcM2edAn8KauTQ">>).
-define(BUYER, <<"LW0myHWuv7XcLec19OCDzFJW0P6jXPG_Ao49kfy9Slc">>).
-define(SALE_OFFER, <<"r6lleOybw5_Pz-3EDHjLwAYv8XNGTlLMjT_8pA9e6o0">>).
-define(SALE_UNDERPAID, <<"060IcKkUJ4ggdojenpcvSpTQjbuGDsoDz_jEhs-C4E8">>).
-define(SALE_STRANDED, <<"f31_VyEl5NumgmKUPeokDgPAxbOZj32bFBf3D5wwf_A">>).
-define(SALE_PAID, <<"RJXzg_GbIo7mUs3oNXfG4DI2-1EK7rWipN5YlvFxZFI">>).
-define(SALE_INTERLOPER, <<"L1mrhwV8JY0-n7B2XZu2R9Ox5MjCsdnsSohpNl_UHAg">>).
-define(SALE_PAYMENT, <<"B4F3TSTcLBuivjQ9Rzf-2IyY2svHUIMnfQjHGbegn6c">>).
-define(SALE_STALE_SET, <<"ndgQ_1zZCm3cMlmC42jhLFZHFwo60wVs5znQ14Mip4Q">>).
-define(SALE_OWNER_SET, <<"vc3eQypdoGc4--NMQkah5tYYnG62KkYtpme1JVTpeAU">>).
-define(SALE_STRAY, <<"bwEotEyzbH4AYP_-5WZCZB7TgTRI4bFjVdJl6fjmuhE">>).
-define(SALE_NONSENSE, <<"aYIqfkDstZTyewfRdqNcKHYdfCeC2jqyICD0juRCoC0">>).
-define(SALE_MAX_HEIGHT, 1966084).

sale_story_test_() -> {timeout, 3600, fun sale_story/0}.
sale_story() ->
    Opts = story_opts(?SALE_MAX_HEIGHT, ?SALE_PROCESS),
    {ok, Schedule} = story_sync(?SALE_PROCESS, Opts, 5),
    Slot = fun(TXID) -> slot_of(Schedule, TXID, Opts) end,
    Read = fun(TXID, Path) -> at(?SALE_PROCESS, Slot(TXID), Path, Opts) end,
    Order = ?SALE_OFFER,
    Held = <<"orders/", Order/binary, "/creator">>,
    Reserver = <<"orders/", Order/binary, "/buyer">>,
    % The offer escrows the seller's only unit, and opens the order.
    ?assertEqual({ok, 0}, Read(?SALE_OFFER, <<"balances/", (?SELLER)/binary>>)),
    ?assertEqual({ok, ?SELLER}, Read(?SALE_OFFER, Held)),
    ?assertEqual(
        {ok, 100000000},
        Read(?SALE_OFFER, <<"orders/", Order/binary, "/minimum-fee">>)
    ),
    % A registration paying only what the network charges does not clear a fee
    % set above it, and neither does value sent to the process.
    ?assertMatch({error, not_found}, Read(?SALE_UNDERPAID, Reserver)),
    ?assertMatch({error, not_found}, Read(?SALE_STRANDED, Reserver)),
    % Overpaying the reward does.
    ?assertEqual({ok, ?BUYER}, Read(?SALE_PAID, Reserver)),
    % Somebody else's payment buys nothing while the order is reserved.
    ?assertEqual({ok, ?BUYER}, Read(?SALE_INTERLOPER, Reserver)),
    ?assertMatch(
        {error, not_found},
        Read(?SALE_INTERLOPER, <<"balances/", (?UNDERPAYER)/binary>>)
    ),
    % The registrant's payment settles it, and the name moves.
    ?assertMatch({error, _}, Read(?SALE_PAYMENT, Held)),
    ?assertEqual({ok, 1}, Read(?SALE_PAYMENT, <<"balances/", (?BUYER)/binary>>)),
    ?assertEqual({ok, 0}, Read(?SALE_PAYMENT, <<"balances/", (?SELLER)/binary>>)),
    % The seller no longer holds the name, so is no longer heard. The buyer is.
    ?assertMatch({error, not_found}, Read(?SALE_STALE_SET, <<"value/greeting">>)),
    ?assertEqual(
        {ok, <<"hello from the new owner">>},
        Read(?SALE_OWNER_SET, <<"value/greeting">>)
    ),
    % A stranger routing a slot at `info' changes nothing and breaks nothing, and
    % an offer whose deadline is `tomorrow' opens nothing.
    ?assertEqual({ok, 1}, Read(?SALE_STRAY, <<"balances/", (?BUYER)/binary>>)),
    ?assertMatch(
        {error, not_found},
        Read(?SALE_NONSENSE, <<"orders/", (?SALE_NONSENSE)/binary, "/creator">>)
    ),
    ?assertEqual({ok, 1}, Read(?SALE_NONSENSE, <<"balances/", (?BUYER)/binary>>)).

%%% Story two: an offer withdrawn.
%%%
%%% A seller may take back an order nobody has reserved. Once they have, the
%%% order is spent: registering against it is refused however much is paid, and
%%% a payment against it buys nothing and refunds nobody. A stranger may not
%%% withdraw somebody else's order.
-define(WITHDRAWN_PROCESS, <<"petNFJyilEh0YvFb39FqoL7CeBUvmnuQ73eHSlGLYxI">>).
-define(WITHDRAWN_OFFER, <<"H1aRe1UoXqSW1IA1H4fZf8oUkHRFzgfzv2cw6hsRYGE">>).
-define(WITHDRAWN_CANCEL, <<"zr8xLGjdXJWvWa3VYMjigEk4c47mteGRfUE0cZzX_hc">>).
-define(WITHDRAWN_LATE_REGISTER, <<"m0KwPLpX1zPiR-k9H7Hx16xwGQHRM82O_WgS8E3H1X8">>).
-define(WITHDRAWN_LATE_PAYMENT, <<"nUalA_4J6M-1fV9mx0dDaT4V1GQHGWmcW09G7ZuEOjA">>).
-define(WITHDRAWN_SECOND, <<"0--Z1ngGMHPCrT5Wj9wxFXGapoXoDCq97u2jCuIlZkQ">>).
-define(WITHDRAWN_STRANGER, <<"o6N4lmA5tV318cOEoXTnN5pmBRYGXtIB8K3VaSPCONw">>).
-define(WITHDRAWN_MAX_HEIGHT, 1966093).

withdrawn_story_test_() -> {timeout, 3600, fun withdrawn_story/0}.
withdrawn_story() ->
    Opts = story_opts(?WITHDRAWN_MAX_HEIGHT, ?WITHDRAWN_PROCESS),
    {ok, Schedule} = story_sync(?WITHDRAWN_PROCESS, Opts, 5),
    Slot = fun(TXID) -> slot_of(Schedule, TXID, Opts) end,
    Read = fun(TXID, Path) -> at(?WITHDRAWN_PROCESS, Slot(TXID), Path, Opts) end,
    First = <<"orders/", (?WITHDRAWN_OFFER)/binary, "/creator">>,
    Second = <<"orders/", (?WITHDRAWN_SECOND)/binary, "/creator">>,
    Balance = <<"balances/", (?SELLER)/binary>>,
    % The offer escrows the unit; withdrawing it gives the unit back.
    ?assertEqual({ok, 0}, Read(?WITHDRAWN_OFFER, Balance)),
    ?assertEqual({ok, ?SELLER}, Read(?WITHDRAWN_OFFER, First)),
    ?assertMatch({error, _}, Read(?WITHDRAWN_CANCEL, First)),
    ?assertEqual({ok, 1}, Read(?WITHDRAWN_CANCEL, Balance)),
    % A registration that pays the fee in full is still refused: the order is
    % gone. So is a payment against it, in either direction.
    ?assertMatch({error, _}, Read(?WITHDRAWN_LATE_REGISTER, First)),
    ?assertEqual({ok, 1}, Read(?WITHDRAWN_LATE_PAYMENT, Balance)),
    ?assertMatch(
        {error, not_found},
        Read(?WITHDRAWN_LATE_PAYMENT, <<"balances/", (?BUYER)/binary>>)
    ),
    % The seller offers it again, and a stranger cannot withdraw it.
    ?assertEqual({ok, ?SELLER}, Read(?WITHDRAWN_SECOND, Second)),
    ?assertEqual({ok, ?SELLER}, Read(?WITHDRAWN_STRANGER, Second)),
    ?assertEqual({ok, 0}, Read(?WITHDRAWN_STRANGER, Balance)).

%%% Story three: a name handed over.
%%%
%%% No sale at all -- the token half on its own. The unit moves by `transfer',
%%% and the authority moves with it: the former holder's word stops counting the
%%% moment it does, and the new holder's starts.
-define(HANDOVER_PROCESS, <<"D4uhF_nO_vyPoIhPDZ0kFMyfOnk1ZCFJFkmnxVw7vSs">>).
-define(HANDOVER_FIRST_SET, <<"dx_Dmvnp2FDZdb3wlDpfVvc4k3UCbQRWLXIEwwgGxIA">>).
-define(HANDOVER_TRANSFER, <<"WgX3Ih8Ef7aDzQ_8Ziio_qYSA5z_-OUcCOVjmdC8WV0">>).
-define(HANDOVER_STALE_SET, <<"-epGyJadxPyQK_bibsic_btn133f_J8UCX1AndWIWRs">>).
-define(HANDOVER_NEW_SET, <<"mqpK8TsoWFUJW345gcYLrFmw4nxqyGqpavt7xUaniC0">>).
-define(HANDOVER_MAX_HEIGHT, 1966100).

handover_story_test_() -> {timeout, 3600, fun handover_story/0}.
handover_story() ->
    Opts = story_opts(?HANDOVER_MAX_HEIGHT, ?HANDOVER_PROCESS),
    {ok, Schedule} = story_sync(?HANDOVER_PROCESS, Opts, 5),
    Slot = fun(TXID) -> slot_of(Schedule, TXID, Opts) end,
    Read = fun(TXID, Path) -> at(?HANDOVER_PROCESS, Slot(TXID), Path, Opts) end,
    Seller = <<"balances/", (?SELLER)/binary>>,
    Buyer = <<"balances/", (?BUYER)/binary>>,
    % While the seller holds it, the seller speaks for it.
    ?assertEqual({ok, 1}, Read(?HANDOVER_FIRST_SET, Seller)),
    ?assertEqual(
        {ok, <<"seller">>},
        Read(?HANDOVER_FIRST_SET, <<"value/points-at">>)
    ),
    % The unit moves, and the notices `token-1.0' emits go with it.
    ?assertEqual({ok, 0}, Read(?HANDOVER_TRANSFER, Seller)),
    ?assertEqual({ok, 1}, Read(?HANDOVER_TRANSFER, Buyer)),
    ?assertEqual(
        {ok, <<"Debit-Notice">>},
        Read(?HANDOVER_TRANSFER, <<"results/outbox/1/action">>)
    ),
    ?assertEqual(
        {ok, <<"Credit-Notice">>},
        Read(?HANDOVER_TRANSFER, <<"results/outbox/2/action">>)
    ),
    % The former holder is no longer heard; the new holder is.
    ?assertEqual(
        {ok, <<"seller">>},
        Read(?HANDOVER_STALE_SET, <<"value/points-at">>)
    ),
    ?assertEqual({ok, <<"buyer">>}, Read(?HANDOVER_NEW_SET, <<"value/points-at">>)).

%%% Story four: a name, read as a name.
%%%
%%% The three stories above are read slot by slot, through
%%% `~process@1.0/compute&slot='. This one is read the way a person reads a
%%% name: `~name@1.0' resolves the label to the process, and `/now' computes the
%%% whole schedule at once rather than one slot at a time. The name really was
%%% sold -- the buyer holds the unit and has said what it means -- and the
%%% payment that did it was an ordinary transfer between two addresses that the
%%% process was never a party to.
-define(NAMED_PROCESS, <<"yWRe7v4SZ4_NKV6LkYyNPrFdzEaGh0ckblu-CaGXqG4">>).
-define(NAMED_ORDER, <<"3BApJHeatc9pVuLgjZ_P-HT5hZgRE1Q3I1bdTESgRDM">>).
-define(NAMED_MAX_HEIGHT, 1966044).

name_resolution_test_() -> {timeout, 1800, fun name_resolution/0}.
name_resolution() ->
    Opts = story_opts(?NAMED_MAX_HEIGHT, ?NAMED_PROCESS),
    % A node that serves a name holds it: priming the schedule puts the process
    % in the node's own cache, which is what the resolver then loads.
    {ok, _} = story_sync(?NAMED_PROCESS, Opts, 5),
    ?assertEqual(
        {ok, ?NAMED_PROCESS},
        hb_ao:resolve_many(
            [
                #{ <<"device">> => <<"name@1.0">> },
                #{ <<"path">> => <<"test-name">>, <<"load">> => false }
            ],
            Opts
        )
    ),
    % `test-name.host' is the same lookup: the node's own host is stripped from
    % the request's host, leaving the label to resolve.
    {ok, Resolved} =
        hb_ao:resolve(
            #{ <<"device">> => <<"name@1.0">> },
            #{
                <<"path">> => <<"request">>,
                <<"request">> => #{ <<"host">> => <<"test-name.host">> },
                <<"body">> => [#{ <<"path">> => <<"now">> }]
            },
            Opts
        ),
    [Named | _] = hb_ao:get(<<"body">>, Resolved, [], Opts),
    Loaded = hb_cache:ensure_all_loaded(Named, Opts),
    % The message the host resolved to is this name: it carries the name's own
    % spawn keys, and nothing else on the weave does.
    ?assertEqual(<<"test-name">>, hb_ao:get(<<"name">>, Loaded, not_found, Opts)),
    ?assertEqual(?SELLER, hb_ao:get(<<"initial-holder">>, Loaded, not_found, Opts)),
    ?assertEqual(
        <<"name-token@1.0">>,
        hb_ao:get(<<"execution-device">>, Loaded, not_found, Opts)
    ),
    % And the whole schedule, computed at once: the name is the buyer's, the
    % order it was sold by has left the book, and the new owner has spoken. The
    % process is read from the node's cache, so that it is its canonical `tx@1.0'
    % decoding rather than a gateway store's lossier one.
    {ok, Raw} = hb_cache:read(?NAMED_PROCESS, Opts),
    {ok, State} =
        hb_ao:resolve(hb_cache:ensure_all_loaded(Raw, Opts), <<"now">>, Opts),
    Read =
        fun(Path) ->
            hb_ao:get(Path, {as, <<"message@1.0">>, State}, not_found, Opts)
        end,
    ?assertEqual(1, hb_util:int(Read([?BALANCES, ?BUYER]))),
    ?assertEqual(0, hb_util:int(Read([?BALANCES, ?SELLER]))),
    ?assertEqual(1, hb_util:int(Read(<<"total-supply">>))),
    ?assertEqual(not_found, Read([<<"orders">>, ?NAMED_ORDER, <<"creator">>])),
    ?assertEqual(<<"hello from the new owner">>, Read([?VALUE, <<"greeting">>])),
    ?assertEqual(<<"text/plain">>, Read([?VALUE, <<"content-type">>])).
