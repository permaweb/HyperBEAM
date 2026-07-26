%%% @doc A name: a token with a single unit, whose holder decides what the
%%% name resolves to.
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
%%% `token-1.0' reads too. The device deliberately exports no key list: a name
%%% is meant to be sold by `~arweave-swap@1.0', which requires its process to be
%%% sequenced by every transaction on Arweave, so the key a slot resolves is
%%% chosen by a stranger's `path' tag. An exported `balance' key would let a
%%% passer-by's transaction hand back a balance as the new process state.
-module(dev_name_token).
-implements(<<"name-token@1.0">>).
%%% AO-Core API functions:
-export([info/0, compute/3, set/3, keys/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The balances submessage, shared with any device that settles in this token.
-define(BALANCES, <<"balances">>).
%%% The linked message whose keys the name resolves through to.
-define(VALUE, <<"value">>).
%%% The share of the supply a signer must hold to set the name, in basis
%%% points. The whole supply, unless the token says otherwise.
-define(DEFAULT_THRESHOLD_BPS, 10000).

%% @doc Every key routes to `compute': the schedule drives this device, and
%% under `~arweave-scheduler@1.0''s `all' mode the key a slot resolves comes
%% from a stranger's `path' tag. See `dev_arweave_swap:info/0', which carries
%% the same reasoning at length.
info() ->
    #{ default => fun router/4 }.

%% @doc Apply any scheduled message, whatever it asked to be routed to.
router(_Key, Base, Assignment, Opts) ->
    compute(Base, Assignment, Opts).

%% @doc Apply one assignment to the name's state.
%%
%% A name that is for sale is settled in the same balances it keeps, so every
%% message is offered to the selling device first -- including the ones this
%% device would otherwise ignore, since the payment that buys a name is a
%% transfer between two other addresses. Which device that is, is a scalar key
%% on the process; see `swap/3' for why it is not a `~stack@1.0'.
compute(Base, Assignment, Opts) ->
    Seeded = seed(Base, Opts),
    Sold = swap(Seeded, Assignment, Opts),
    Body = hb_ao:get(<<"body">>, Assignment, #{}, Opts),
    ProcID = hb_ao:get(<<"process">>, Assignment, <<>>, Opts),
    case hb_ao:get(<<"target">>, Body, <<>>, Opts) of
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
%% spawned as an Arweave transaction can only carry flat, scalar tags: the
%% codec turns a submessage or a list into a `+link' to content that is not on
%% the weave, so nothing else can read it back. `device-stack' is a list, so a
%% stack cannot survive the spawn -- but `swap-device' is one word.
swap(Base, Assignment, Opts) ->
    case state(<<"swap-device">>, Base, not_found, Opts) of
        not_found -> Base;
        Device ->
            case hb_ao:resolve(Base#{ <<"device">> => Device }, Assignment, Opts) of
                {ok, Settled} -> Settled#{ <<"device">> => <<"name-token@1.0">> };
                _ -> Base
            end
    end.

%% @doc Give the name its single unit, and whatever it was minted pointing at,
%% the first time it computes.
%%
%% Neither can be written into the spawn directly: `balances' and the linked
%% message are submessages, and a submessage does not cross the chain. What does
%% cross is a scalar -- `initial-holder', an address, and `initial-value', the id
%% of whatever the name should resolve to. Values keep their case, where keys are
%% lowercased and a lowercased address is a different address.
%%
%% Seeding a value at spawn is what makes a name cheap to mint. Without it, a
%% freshly spawned name says nothing until somebody sends it a `set', and the
%% first message addressed to a process pays Arweave's new-account fee -- so
%% every name would cost that before it could resolve at all.
seed(Base, Opts) ->
    seed_value(seed_holding(Base, Opts), Opts).

seed_holding(Base, Opts) ->
    case state(<<"initial-holder">>, Base, not_found, Opts) of
        not_found -> Base;
        Holder ->
            case state(?BALANCES, Base, not_found, Opts) of
                not_found ->
                    Supply = hb_util:int(state(<<"total-supply">>, Base, 1, Opts)),
                    ?event({name_token_seeded, {holder, Holder}, {supply, Supply}}),
                    Base#{ ?BALANCES => #{ Holder => Supply } };
                _ -> Base
            end
    end.

%% @doc A name minted pointing somewhere resolves there from its first slot. The
%% value is a message of its own -- `{ target: <id> }' -- so that a `set' can
%% later replace it with anything at all without the shape changing underneath
%% whatever is reading it.
seed_value(Base, Opts) ->
    case state(<<"initial-value">>, Base, not_found, Opts) of
        not_found -> Base;
        Target ->
            case state(?VALUE, Base, not_found, Opts) of
                not_found ->
                    ?event({name_token_seeded_value, {target, Target}}),
                    Base#{ ?VALUE => #{ <<"target">> => Target } };
                _ -> Base
            end
    end.

%% @doc Route a message addressed to the name by its `action'. Matching is
%% case-insensitive, as `token-1.0' matches. An unknown action leaves the state
%% untouched rather than failing the slot, which would stop the process on every
%% node for good.
action(Base, Body, Opts) ->
    case hb_util:to_lower(hb_ao:get(<<"action">>, Body, <<>>, Opts)) of
        <<"transfer">> -> transfer(Base, Body, Opts);
        <<"set">> -> set_value(Base, Body, Opts);
        _ -> Base
    end.

%%% The token

%% @doc Move units between balances. Nothing is written unless the whole
%% transfer is admissible, so a rejected one is indistinguishable from a
%% message that was never sent.
transfer(Base, Body, Opts) ->
    maybe
        {ok, Sender} ?= signer(Body, Opts),
        Recipient = hb_ao:get(<<"recipient">>, Body, not_found, Opts),
        true ?= is_binary(Recipient),
        {ok, Quantity} ?= hb_util:safe_int(hb_ao:get(<<"quantity">>, Body, 0, Opts)),
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
            credit(
                debit(Base, Sender, Quantity, Opts),
                Recipient,
                Quantity,
                Opts
            ),
            Sender,
            Recipient,
            Quantity,
            Opts
        )
    else
        _ -> Base
    end.

%% @doc Emit the pair of notices that `token-1.0' emits for a transfer, with
%% the same keys. They are the slot's results; whether anything delivers them
%% is the process's business, not this device's.
notices(Base, Sender, Recipient, Quantity, _Opts) ->
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
        Value = value_of(Body, Opts),
        ?event({name_token_set, {by, Signer}}),
        Base#{ ?VALUE => Value }
    else
        _ -> Base
    end.

%% @doc The message a `set' is asking the name to resolve to.
value_of(Body, Opts) ->
    case hb_ao:get(<<"reference-value">>, Body, not_found, Opts) of
        not_found ->
            % The set message itself is the value. The keys that addressed it to
            % this name are not part of what the name says.
            hb_maps:without(
                [
                    <<"action">>,
                    <<"target">>,
                    <<"quantity">>,
                    <<"anchor">>,
                    <<"reward">>,
                    <<"last_tx">>,
                    <<"owner">>,
                    <<"signature">>,
                    <<"commitments">>,
                    <<"priv">>
                ],
                hb_cache:ensure_all_loaded(Body, Opts),
                Opts
            );
        Value -> hb_cache:ensure_all_loaded(Value, Opts)
    end.

%% @doc Whether an address may speak for the name: it must hold the share of the
%% supply the token requires, which is all of it unless the token says
%% otherwise. This is `token-1.0''s `supply-threshold-owner' rule, and it is
%% evaluated against the balances as they stand -- so the authority moves with
%% the unit, with no separate owner field to keep in step.
owns_supply(Base, Address, Opts) ->
    Supply = hb_util:int(state(<<"total-supply">>, Base, 1, Opts)),
    Threshold =
        hb_util:int(
            state(
                <<"set-authority-threshold-bps">>,
                Base,
                ?DEFAULT_THRESHOLD_BPS,
                Opts
            )
        ),
    balance(Base, Address, Opts) * 10000 >= Supply * Threshold.

%%% State

%% @doc Read a key of the process's own state. While a slot is being computed
%% the state carries this device, so a plain read would resolve the key back
%% through `compute'. See `dev_arweave_swap:state/4'.
state(Key, Base, Default, Opts) ->
    hb_ao:get(Key, {as, <<"message@1.0">>, Base}, Default, Opts).

balance(Base, Address, Opts) ->
    hb_util:int(state([?BALANCES, Address], Base, 0, Opts)).

credit(Base, Address, Amount, Opts) ->
    write_balance(Base, Address, balance(Base, Address, Opts) + Amount, Opts).

debit(Base, Address, Amount, Opts) ->
    write_balance(Base, Address, balance(Base, Address, Opts) - Amount, Opts).

%% @doc Write one balance back. Only whole top-level keys are written: setting a
%% nested path would resolve the keys above it through this device on the way
%% down.
write_balance(Base, Address, Value, Opts) ->
    Base#{
        ?BALANCES =>
            hb_maps:put(
                Address,
                Value,
                state(?BALANCES, Base, #{}, Opts),
                Opts
            )
    }.

%% @doc Setting the device is honoured, and nothing else is: `lib_process' puts
%% the process's own device back after every slot, and reading this state as a
%% message is itself a device set. Anything else is a scheduled message and is
%% applied as one. See `dev_arweave_swap:set/3'.
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

%% @doc The single signer of a message. A message with any other number of
%% signers is not attributable to one party, so it can neither move the unit nor
%% speak for the name.
signer(Body, Opts) ->
    case hb_message:signers(Body, Opts) of
        [Signer] -> {ok, hb_util:human_id(Signer)};
        _ -> not_found
    end.

%%% Tests

%%% The tests drive `compute/3' directly with synthetic assignments, exactly as
%%% `~process@1.0' would. Below them is the live-network driver that seeded the
%%% permanent fixture, and the fixture test that replays it.

-define(PROCESS, <<"nAmEtOkEn000000000000000000000000000000000">>).

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

apply_tx(Base, Body, Opts) ->
    {ok, New} =
        compute(
            Base,
            #{
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
    tx(
        Wallet,
        Fields#{ <<"target">> => ?PROCESS, <<"action">> => <<"set">> }
    ).

held_by(Base, Address, Opts) -> balance(Base, Address, Opts).

value(Base, Opts) ->
    hb_cache:ensure_all_loaded(state(?VALUE, Base, #{}, Opts), Opts).

%% @doc The unit moves, and the pair of notices `token-1.0' emits go with it.
transfer_moves_the_unit_test() ->
    Opts = test_opts(),
    {Owner, OwnerAddr} = party(),
    {_, BuyerAddr} = party(),
    Moved =
        apply_tx(name_held_by(OwnerAddr), transfer_tx(Owner, BuyerAddr, 1), Opts),
    ?assertEqual(0, held_by(Moved, OwnerAddr, Opts)),
    ?assertEqual(1, held_by(Moved, BuyerAddr, Opts)),
    Outbox = hb_ao:get(<<"results/outbox">>, {as, <<"message@1.0">>, Moved}, #{}, Opts),
    ?assertEqual(
        <<"Debit-Notice">>,
        hb_ao:get(<<"1/action">>, Outbox, not_found, Opts)
    ),
    ?assertEqual(
        <<"Credit-Notice">>,
        hb_ao:get(<<"2/action">>, Outbox, not_found, Opts)
    ),
    ?assertEqual(
        BuyerAddr,
        hb_ao:get(<<"1/recipient">>, Outbox, not_found, Opts)
    ),
    ?assertEqual(
        OwnerAddr,
        hb_ao:get(<<"2/sender">>, Outbox, not_found, Opts)
    ).

%% @doc Nobody can send what they do not hold.
transfer_beyond_balance_test() ->
    Opts = test_opts(),
    {Owner, OwnerAddr} = party(),
    {Stranger, _} = party(),
    {_, ElsewhereAddr} = party(),
    Base = name_held_by(OwnerAddr),
    ?assertEqual(
        1,
        held_by(apply_tx(Base, transfer_tx(Owner, ElsewhereAddr, 2), Opts), OwnerAddr, Opts)
    ),
    ?assertEqual(
        0,
        held_by(
            apply_tx(Base, transfer_tx(Stranger, ElsewhereAddr, 1), Opts),
            ElsewhereAddr,
            Opts
        )
    ).

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
    ?assertEqual(
        <<"text/html">>,
        hb_ao:get(<<"content-type">>, value(Set, Opts), not_found, Opts)
    ),
    ?assertEqual(
        <<"<h1>hello</h1>">>,
        hb_ao:get(<<"body">>, value(Set, Opts), not_found, Opts)
    ).

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
    Value = value(Set, Opts),
    ?assertEqual(<<"ahoy">>, hb_ao:get(<<"greeting">>, Value, not_found, Opts)),
    ?assertEqual(not_found, hb_ao:get(<<"action">>, Value, not_found, Opts)),
    ?assertEqual(not_found, hb_ao:get(<<"target">>, Value, not_found, Opts)).

%% @doc Anybody who does not hold the name cannot speak for it.
set_by_stranger_test() ->
    Opts = test_opts(),
    {_, OwnerAddr} = party(),
    {Stranger, _} = party(),
    Result =
        apply_tx(
            name_held_by(OwnerAddr),
            set_tx(Stranger, #{ <<"greeting">> => <<"mine now">> }),
            Opts
        ),
    ?assertEqual(#{}, value(Result, Opts)).

%% @doc The authority is the holding, not a recorded owner: it goes with the
%% unit and needs nothing kept in step. This is what makes a name sellable.
authority_follows_the_unit_test() ->
    Opts = test_opts(),
    {Owner, OwnerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    Base = name_held_by(OwnerAddr),
    Before = apply_tx(Base, set_tx(Owner, #{ <<"points-at">> => <<"seller">> }), Opts),
    ?assertEqual(
        <<"seller">>,
        hb_ao:get(<<"points-at">>, value(Before, Opts), not_found, Opts)
    ),
    Sold = apply_tx(Before, transfer_tx(Owner, BuyerAddr, 1), Opts),
    % The seller has handed over the unit, and with it the right to speak.
    Stale = apply_tx(Sold, set_tx(Owner, #{ <<"points-at">> => <<"seller again">> }), Opts),
    ?assertEqual(
        <<"seller">>,
        hb_ao:get(<<"points-at">>, value(Stale, Opts), not_found, Opts)
    ),
    Fresh = apply_tx(Sold, set_tx(Buyer, #{ <<"points-at">> => <<"buyer">> }), Opts),
    ?assertEqual(
        <<"buyer">>,
        hb_ao:get(<<"points-at">>, value(Fresh, Opts), not_found, Opts)
    ),
    ?assertEqual(1, held_by(Fresh, BuyerAddr, Opts)),
    ?assertEqual(0, held_by(Fresh, OwnerAddr, Opts)).

%% @doc A partial holding is not the whole supply, so it does not carry the
%% authority. (A name is indivisible, but the rule is the supply threshold.)
partial_holding_cannot_set_test() ->
    Opts = test_opts(),
    {Owner, OwnerAddr} = party(),
    {_, OtherAddr} = party(),
    Split =
        (name_held_by(OwnerAddr))#{
            <<"total-supply">> => 2,
            ?BALANCES => #{ OwnerAddr => 1, OtherAddr => 1 }
        },
    Result = apply_tx(Split, set_tx(Owner, #{ <<"greeting">> => <<"half mine">> }), Opts),
    ?assertEqual(#{}, value(Result, Opts)).

%% @doc There is no mint path, so the supply cannot grow -- an unknown action
%% is ignored rather than failing the slot.
supply_is_fixed_test() ->
    Opts = test_opts(),
    {Owner, OwnerAddr} = party(),
    Result =
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
    ?assertEqual(1, held_by(Result, OwnerAddr, Opts)),
    ?assertEqual(1, hb_util:int(state(<<"total-supply">>, Result, 0, Opts))).

%% @doc Actions are matched however they are cased, as `token-1.0' matches.
action_case_is_ignored_test() ->
    Opts = test_opts(),
    {Owner, OwnerAddr} = party(),
    {_, BuyerAddr} = party(),
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
            tx(Stranger, #{ <<"target">> => <<"somebody-else">>, <<"quantity">> => <<"1">> }),
            Opts
        ),
    ?assertEqual(1, held_by(Result, OwnerAddr, Opts)).

%% @doc A name spawned as an Arweave transaction carries only scalars, so it
%% names its first holder rather than holding a balances submessage. The unit
%% appears the first time it computes.
seeded_from_initial_holder_test() ->
    Opts = test_opts(),
    {Owner, OwnerAddr} = party(),
    Spawned =
        #{
            <<"name">> => <<"test-name">>,
            <<"total-supply">> => 1,
            <<"initial-holder">> => OwnerAddr
        },
    % Any message at all brings the name to life, including one that is not
    % addressed to it.
    Alive =
        apply_tx(Spawned, tx(Owner, #{ <<"target">> => <<"elsewhere">> }), Opts),
    ?assertEqual(1, held_by(Alive, OwnerAddr, Opts)),
    % And the holder can immediately speak for it.
    Set = apply_tx(Alive, set_tx(Owner, #{ <<"greeting">> => <<"mine">> }), Opts),
    ?assertEqual(
        <<"mine">>,
        hb_ao:get(<<"greeting">>, value(Set, Opts), not_found, Opts)
    ).

%% @doc A name minted pointing somewhere resolves there immediately, without
%% anybody having to send it a message -- which matters because the first message
%% addressed to a process pays to create its account.
seeded_value_test() ->
    Opts = test_opts(),
    {Owner, OwnerAddr} = party(),
    Spawned =
        #{
            <<"name">> => <<"pn-test-1">>,
            <<"total-supply">> => 1,
            <<"initial-holder">> => OwnerAddr,
            <<"initial-value">> => <<"aBcDeFgHiJkLmNoPqRsTuVwXyZ0123456789_-aBcDe">>
        },
    Alive = apply_tx(Spawned, tx(Owner, #{ <<"target">> => <<"elsewhere">> }), Opts),
    ?assertEqual(
        <<"aBcDeFgHiJkLmNoPqRsTuVwXyZ0123456789_-aBcDe">>,
        hb_ao:get(<<"target">>, value(Alive, Opts), not_found, Opts)
    ),
    % And the holder can still point it somewhere else afterwards.
    Reset = apply_tx(Alive, set_tx(Owner, #{ <<"greeting">> => <<"moved">> }), Opts),
    ?assertEqual(
        <<"moved">>,
        hb_ao:get(<<"greeting">>, value(Reset, Opts), not_found, Opts)
    ),
    ?assertEqual(not_found, hb_ao:get(<<"target">>, value(Reset, Opts), not_found, Opts)).

%% @doc Seeding happens once. A name whose unit has moved on is not handed a
%% fresh one by the next message that arrives.
seeding_happens_once_test() ->
    Opts = test_opts(),
    {Owner, OwnerAddr} = party(),
    {Stranger, StrangerAddr} = party(),
    Spawned =
        #{
            <<"name">> => <<"test-name">>,
            <<"total-supply">> => 1,
            <<"initial-holder">> => OwnerAddr
        },
    Alive = apply_tx(Spawned, tx(Owner, #{ <<"target">> => <<"elsewhere">> }), Opts),
    Moved = apply_tx(Alive, transfer_tx(Owner, StrangerAddr, 1), Opts),
    Later =
        apply_tx(Moved, tx(Stranger, #{ <<"target">> => <<"elsewhere">> }), Opts),
    ?assertEqual(0, held_by(Later, OwnerAddr, Opts)),
    ?assertEqual(1, held_by(Later, StrangerAddr, Opts)).

%% @doc A name with no selling device is simply a name: messages the swap would
%% have handled do nothing, and the name still works.
without_swap_device_test() ->
    Opts = test_opts(),
    {Owner, OwnerAddr} = party(),
    Base = name_held_by(OwnerAddr),
    Set = apply_tx(Base, set_tx(Owner, #{ <<"greeting">> => <<"no swap here">> }), Opts),
    ?assertEqual(
        <<"no swap here">>,
        hb_ao:get(<<"greeting">>, value(Set, Opts), not_found, Opts)
    ),
    ?assertEqual(not_found, state(<<"orders">>, Set, not_found, Opts)).

%% @doc A stranger's transaction may ask to be routed anywhere, including to
%% keys `~message@1.0' would otherwise answer. Each is applied like any other
%% message rather than handing back the state, a list of key names, or an info
%% map as the new state.
reserved_paths_are_applied_test() ->
    Opts = test_opts(),
    {_, OwnerAddr} = party(),
    {Stranger, _} = party(),
    Base = name_held_by(OwnerAddr),
    lists:foreach(
        fun(Path) ->
            {ok, State} =
                hb_ao:resolve(
                    Base#{ <<"device">> => <<"name-token@1.0">> },
                    #{
                        <<"path">> => Path,
                        <<"process">> => ?PROCESS,
                        <<"slot">> => 2,
                        ?BALANCES => #{ OwnerAddr => 1000000 },
                        <<"body">> =>
                            tx(Stranger, #{ <<"target">> => <<"somebody-else">> })
                    },
                    Opts
                ),
            ?assertEqual(1, held_by(State, OwnerAddr, Opts))
        end,
        [<<"set">>, <<"keys">>, <<"info">>, <<"balances">>, <<"anything-else">>]
    ).

%%% The live network suite
%%%
%%% Three stories played out on mainnet, against the real weave, with every
%%% path and refusal a name sale can take. It is deliberately NOT a `_test'
%%% function: it posts real transactions and spends real AR. Invoke it by name:
%%%
%%%     HB_LIVE_SUITE=1 HB_PRINT=name_token_live \\
%%%         rebar3 device test --devices dev_name_token \\
%%%             --test dev_name_token:live_suite_test_
%%%
%%% It reports every transaction id and the slot it landed at, which is what
%%% the pinned fixtures below are made of. Each message waits for the previous
%%% one to be mined: the schedule is block order, and two messages in one block
%%% would be ordered by the block's own transaction list rather than by intent.

%% @doc Everything live is reachable only when asked for by name in the
%% environment, so it contributes nothing to the battery.
live_suite_test_() -> live_gated("HB_LIVE_SUITE", fun live_suite/0, 21600).

live_gated(Variable, Fun, Timeout) ->
    case os:getenv(Variable) of
        false -> [];
        _ -> {timeout, Timeout, Fun}
    end.

%%% What the network charges, and what the orders ask for. The fee an order
%%% demands to register is set well above the price of an ordinary transaction,
%%% so that clearing it means deliberately overpaying rather than paying what
%%% the network was going to charge anyway.
-define(LIVE_MINIMUM_FEE, 100000000).
-define(LIVE_PAID_FEE, 150000000).
-define(LIVE_ASKING, 1000).

%% @doc The wallet that pays for the live run.
live_wallet() ->
    ar_wallet:load_keyfile(<<"/Users/sam/Documents/hyperbeam-key.json">>).

%% @doc A counterparty, kept beside the node's own key so that a rerun reuses
%% it rather than paying to create another account.
live_party(Path) ->
    case file:read_file(Path) of
        {ok, Json} -> ar_wallet:from_json(Json);
        _ ->
            Wallet = ar_wallet:new(),
            ok = file:write_file(Path, hb_util:bin(ar_wallet:to_json(Wallet))),
            Wallet
    end.

live_address(Wallet) -> hb_util:human_id(ar_wallet:to_address(Wallet)).

%% @doc Node options for talking to the live network, mirroring the scheduler's
%% own fixture options.
live_opts(Wallet) ->
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
        <<"priv-wallet">> => Wallet
    }.

%% @doc What the network will charge to send a transaction of no size to an
%% address. An address that has never held AR costs thousands of times more to
%% send to, because the transaction creates the account.
live_price(Target, Opts) ->
    Path =
        case Target of
            <<>> -> <<"/price/0">>;
            _ -> <<"/price/0/", Target/binary>>
        end,
    {ok, Res} = hb_http:get(<<"https://arweave.net">>, Path, Opts),
    hb_util:int(hb_ao:get(<<"body">>, Res, <<"0">>, Opts)).

live_anchor(Opts) ->
    {ok, Res} = hb_http:get(<<"https://arweave.net">>, <<"/tx_anchor">>, Opts),
    hb_ao:get(<<"body">>, Res, <<>>, Opts).

live_balance(Address, Opts) ->
    {ok, Res} =
        hb_http:get(
            <<"https://arweave.net">>,
            <<"/wallet/", Address/binary, "/balance">>,
            Opts
        ),
    hb_util:int(hb_ao:get(<<"body">>, Res, <<"0">>, Opts)).

%% @doc Sign a layer-1 transaction and hand it to the network, through the
%% scheduler's own dispatch path, then wait for it to be mined.
%%
%% A `reward' given in the fields is a floor, not a replacement: the network's
%% own price still has to be met. That is how a registration pays an order's
%% `minimum-fee' -- by overpaying the reward, which goes to miners and the
%% endowment rather than to an address with no key behind it.
live_post(Label, Fields, Wallet, Opts) ->
    Target = hb_maps:get(<<"target">>, Fields, <<>>, Opts),
    Floor = hb_util:int(hb_maps:get(<<"reward">>, Fields, <<"0">>, Opts)),
    Reward = max(live_price(Target, Opts), Floor),
    Msg =
        hb_message:commit(
            Fields#{
                <<"anchor">> => live_anchor(Opts),
                <<"reward">> => hb_util:bin(Reward)
            },
            Opts#{ <<"priv-wallet">> => Wallet },
            #{ <<"commitment-device">> => <<"tx@1.0">> }
        ),
    ID = hb_util:human_id(hb_message:id(Msg, signed, Opts)),
    {ok, Res} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave-scheduler@1.0">> },
            #{
                <<"path">> => <<"schedule">>,
                <<"method">> => <<"POST">>,
                <<"body">> => Msg
            },
            Opts
        ),
    Height = live_await(ID, Opts),
    ?event(name_token_live,
        {posted,
            {label, {string, Label}},
            {tx, {string, ID}},
            {signer, {string, live_address(Wallet)}},
            {reward, Reward},
            {status, hb_ao:get(<<"status">>, Res, none, Opts)},
            {height, Height}
        }
    ),
    ID.

%% @doc Wait for a transaction to be mined, returning the height it landed at.
%% While a transaction is pending the gateway answers in prose -- `Pending',
%% `Accepted' -- and only once it is in a block does it answer with the JSON
%% that carries the height.
live_await(ID, Opts) -> live_await(ID, 240, Opts).
live_await(ID, 0, _Opts) -> error({not_mined, ID});
live_await(ID, Attempts, Opts) ->
    case live_height(ID, Opts) of
        not_found ->
            timer:sleep(15000),
            live_await(ID, Attempts - 1, Opts);
        Height -> Height
    end.

live_height(ID, Opts) ->
    case hb_http:get(<<"https://arweave.net">>, <<"/tx/", ID/binary, "/status">>, Opts) of
        {ok, Status} ->
            Body = hb_ao:get(<<"body">>, Status, <<"">>, Opts),
            try hb_json:decode(Body) of
                Decoded ->
                    case hb_ao:get(<<"block_height">>, Decoded, not_found, Opts) of
                        not_found -> not_found;
                        Height -> hb_util:int(Height)
                    end
            catch
                _:_ -> not_found
            end;
        _ -> not_found
    end.

%% @doc Top an account up only if it cannot pay its own way. Creating an
%% account is by far the most expensive part of a run, so a funded counterparty
%% is left alone and a rerun costs nothing here.
live_fund(Address, Need, Seller, Opts) ->
    case live_balance(Address, Opts) of
        Balance when Balance >= Need ->
            ?event(name_token_live,
                {already_funded, {address, {string, Address}}, {balance, Balance}}
            ),
            Balance;
        Balance ->
            ?event(name_token_live,
                {funding, {address, {string, Address}}, {balance, Balance}}
            ),
            live_post(
                <<"fund">>,
                #{
                    <<"target">> => Address,
                    <<"quantity">> => hb_util:bin(Need * 2)
                },
                Seller,
                Opts
            )
    end.

%% @doc Spawn a name on Arweave. Every key is a scalar: a submessage or a list
%% would be written to the weave as a `+link' to content the weave does not
%% hold, and no node could read the process back.
live_spawn(Name, Holder, Wallet, Opts) ->
    live_post(
        <<"spawn:", Name/binary>>,
        #{
            <<"device">> => <<"process@1.0">>,
            <<"type">> => <<"Process">>,
            <<"scheduler-device">> => <<"arweave-scheduler@1.0">>,
            <<"scheduler-mode">> => <<"all">>,
            <<"execution-device">> => <<"name-token@1.0">>,
            <<"swap-device">> => <<"arweave-swap@1.0">>,
            <<"name">> => Name,
            <<"ticker">> => <<"NAME">>,
            <<"denomination">> => <<"0">>,
            <<"total-supply">> => <<"1">>,
            <<"initial-holder">> => Holder,
            <<"test-suite">> => <<"name-token">>
        },
        Wallet,
        Opts
    ).

%% @doc Address a message to a name. The first message to a process pays to
%% create its account and must carry at least a winston to do so.
live_send(Label, ProcID, Fields, Wallet, Opts) ->
    live_post(
        Label,
        Fields#{ <<"target">> => ProcID, <<"quantity">> => <<"1">> },
        Wallet,
        Opts
    ).

%% @doc Report which slot each of a run's transactions landed at, by reading the
%% schedule back and matching each assignment's body against the ids we sent.
%% These are the numbers the fixture tests read state at.
live_slots(ProcID, Named, Opts) ->
    {ok, Schedule} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave-scheduler@1.0">> },
            #{
                <<"path">> => <<"schedule">>,
                <<"method">> => <<"GET">>,
                <<"target">> => ProcID
            },
            Opts
        ),
    Assignments =
        hb_ao:normalize_keys(hb_ao:get(<<"assignments">>, Schedule, Opts), Opts),
    % A schedule's assignments are keyed by slot, alongside the keys any
    % committed message carries; only the numbered ones are slots.
    BySlot =
        [
            {
                Slot,
                hb_util:human_id(
                    hb_message:id(
                        hb_ao:get(<<"body">>, Assignment, Opts),
                        signed,
                        Opts
                    )
                )
            }
        ||
            {Key, Assignment} <- hb_maps:to_list(Assignments, Opts),
            {ok, Slot} <- [hb_util:safe_int(Key)],
            is_map(Assignment)
        ],
    lists:foreach(
        fun({Label, ID}) ->
            Slot =
                case [S || {S, Body} <- BySlot, Body =:= ID] of
                    [Found | _] -> Found;
                    [] -> not_assigned
                end,
            ?event(name_token_live,
                {slot,
                    {process, {string, ProcID}},
                    {label, {string, Label}},
                    {tx, {string, ID}},
                    {slot, Slot}
                }
            )
        end,
        Named
    ),
    ?event(name_token_live,
        {schedule_length, {process, {string, ProcID}}, {slots, length(BySlot)}}
    ),
    ok.

%%% Story one: a name that had to be paid for.
%%%
%%% An offer that charges to register turns away a buyer who will not pay the
%%% fee, and turns away a payment from somebody who never reserved it. The buyer
%%% who does both gets the name -- and with it the right to say what it means,
%%% which the seller loses in the same breath.
live_story_sale(Seller, Poor, Rich, Opts) ->
    SellerAddr = live_address(Seller),
    ProcID = live_spawn(<<"paid-for">>, SellerAddr, Seller, Opts),
    Offer =
        live_send(
            <<"make-offer">>,
            ProcID,
            #{
                <<"action">> => <<"make-offer">>,
                <<"offer-quantity">> => <<"1">>,
                <<"asking">> => hb_util:bin(?LIVE_ASKING),
                <<"deposit">> => <<"0">>,
                <<"minimum-fee">> => hb_util:bin(?LIVE_MINIMUM_FEE),
                <<"deadline">> => <<"99999999">>
            },
            Seller,
            Opts
        ),
    % A registration that pays only what the network charges anyway does not
    % clear a fee set above it.
    Underpaid =
        live_send(
            <<"register:underpaid">>,
            ProcID,
            #{ <<"action">> => <<"register-interest">>, <<"order-id">> => Offer },
            Poor,
            Opts
        ),
    % Nor does value sent to the process, which would only be stranded there.
    Stranded =
        live_post(
            <<"register:stranded">>,
            #{
                <<"target">> => ProcID,
                <<"quantity">> => hb_util:bin(?LIVE_PAID_FEE),
                <<"action">> => <<"register-interest">>,
                <<"order-id">> => Offer
            },
            Poor,
            Opts
        ),
    % Overpaying the reward does.
    Registered =
        live_post(
            <<"register:paid">>,
            #{
                <<"target">> => ProcID,
                <<"quantity">> => <<"1">>,
                <<"reward">> => hb_util:bin(?LIVE_PAID_FEE),
                <<"action">> => <<"register-interest">>,
                <<"order-id">> => Offer
            },
            Rich,
            Opts
        ),
    % The order is now the registrant's alone: somebody else's payment buys
    % nothing, and with no bond posted there is nothing to compensate them with.
    Interloper =
        live_post(
            <<"payment:interloper">>,
            #{
                <<"target">> => SellerAddr,
                <<"quantity">> => hb_util:bin(?LIVE_ASKING),
                <<"order-id">> => Offer
            },
            Poor,
            Opts
        ),
    Payment =
        live_post(
            <<"payment:buyer">>,
            #{
                <<"target">> => SellerAddr,
                <<"quantity">> => hb_util:bin(?LIVE_ASKING),
                <<"order-id">> => Offer
            },
            Rich,
            Opts
        ),
    % The seller no longer holds the name, so no longer speaks for it.
    StaleSet =
        live_send(
            <<"set:former-owner">>,
            ProcID,
            #{ <<"action">> => <<"set">>, <<"greeting">> => <<"still mine">> },
            Seller,
            Opts
        ),
    OwnerSet =
        live_send(
            <<"set:owner">>,
            ProcID,
            #{
                <<"action">> => <<"set">>,
                <<"content-type">> => <<"text/plain">>,
                <<"greeting">> => <<"hello from the new owner">>
            },
            Rich,
            Opts
        ),
    % A stranger may ask a slot to resolve anything at all. It must be applied
    % like any other message: a slot that failed could never be recomputed, and
    % the process would stop on every node for good.
    Stray =
        live_post(
            <<"stray:path-info">>,
            #{
                <<"target">> => ProcID,
                <<"quantity">> => <<"1">>,
                <<"path">> => <<"info">>
            },
            Poor,
            Opts
        ),
    % And a figure that is not a number is an inadmissible message, not a
    % failed slot.
    Nonsense =
        live_send(
            <<"make-offer:nonsense">>,
            ProcID,
            #{
                <<"action">> => <<"make-offer">>,
                <<"offer-quantity">> => <<"1">>,
                <<"asking">> => <<"1000">>,
                <<"deposit">> => <<"0">>,
                <<"deadline">> => <<"tomorrow">>
            },
            Seller,
            Opts
        ),
    live_slots(
        ProcID,
        [
            {<<"make-offer">>, Offer},
            {<<"register:underpaid">>, Underpaid},
            {<<"register:stranded">>, Stranded},
            {<<"register:paid">>, Registered},
            {<<"payment:interloper">>, Interloper},
            {<<"payment:buyer">>, Payment},
            {<<"set:former-owner">>, StaleSet},
            {<<"set:owner">>, OwnerSet},
            {<<"stray:path-info">>, Stray},
            {<<"make-offer:nonsense">>, Nonsense}
        ],
        Opts
    ),
    ProcID.

%%% Story two: an offer withdrawn.
%%%
%%% A seller may take an unreserved order back, and once they have, nothing
%%% more can be done with it: registering is refused and a payment against it
%%% buys nothing. A stranger may not withdraw somebody else's order.
live_story_withdrawn(Seller, Poor, Rich, Opts) ->
    SellerAddr = live_address(Seller),
    ProcID = live_spawn(<<"withdrawn">>, SellerAddr, Seller, Opts),
    Offer =
        live_send(
            <<"make-offer">>,
            ProcID,
            #{
                <<"action">> => <<"make-offer">>,
                <<"offer-quantity">> => <<"1">>,
                <<"asking">> => hb_util:bin(?LIVE_ASKING),
                <<"deposit">> => <<"0">>,
                <<"minimum-fee">> => hb_util:bin(?LIVE_MINIMUM_FEE),
                <<"deadline">> => <<"99999999">>
            },
            Seller,
            Opts
        ),
    Cancelled =
        live_send(
            <<"cancel">>,
            ProcID,
            #{ <<"action">> => <<"cancel-order">>, <<"order-id">> => Offer },
            Seller,
            Opts
        ),
    LateRegister =
        live_post(
            <<"register:after-cancel">>,
            #{
                <<"target">> => ProcID,
                <<"quantity">> => <<"1">>,
                <<"reward">> => hb_util:bin(?LIVE_PAID_FEE),
                <<"action">> => <<"register-interest">>,
                <<"order-id">> => Offer
            },
            Rich,
            Opts
        ),
    LatePayment =
        live_post(
            <<"payment:after-cancel">>,
            #{
                <<"target">> => SellerAddr,
                <<"quantity">> => hb_util:bin(?LIVE_ASKING),
                <<"order-id">> => Offer
            },
            Rich,
            Opts
        ),
    Second =
        live_send(
            <<"make-offer:second">>,
            ProcID,
            #{
                <<"action">> => <<"make-offer">>,
                <<"offer-quantity">> => <<"1">>,
                <<"asking">> => hb_util:bin(?LIVE_ASKING),
                <<"deposit">> => <<"0">>,
                <<"minimum-fee">> => hb_util:bin(?LIVE_MINIMUM_FEE),
                <<"deadline">> => <<"99999999">>
            },
            Seller,
            Opts
        ),
    StrangerCancel =
        live_send(
            <<"cancel:stranger">>,
            ProcID,
            #{ <<"action">> => <<"cancel-order">>, <<"order-id">> => Second },
            Poor,
            Opts
        ),
    live_slots(
        ProcID,
        [
            {<<"make-offer">>, Offer},
            {<<"cancel">>, Cancelled},
            {<<"register:after-cancel">>, LateRegister},
            {<<"payment:after-cancel">>, LatePayment},
            {<<"make-offer:second">>, Second},
            {<<"cancel:stranger">>, StrangerCancel}
        ],
        Opts
    ),
    ProcID.

%%% Story three: a name handed over directly.
%%%
%%% No sale at all -- just the token half. The unit moves, and the authority
%%% moves with it: the former holder's word stops counting the moment it does.
live_story_handover(Seller, Rich, Opts) ->
    SellerAddr = live_address(Seller),
    RichAddr = live_address(Rich),
    ProcID = live_spawn(<<"handed-over">>, SellerAddr, Seller, Opts),
    FirstSet =
        live_send(
            <<"set:before">>,
            ProcID,
            #{ <<"action">> => <<"set">>, <<"points-at">> => <<"seller">> },
            Seller,
            Opts
        ),
    Transfer =
        live_send(
            <<"transfer">>,
            ProcID,
            #{
                <<"action">> => <<"transfer">>,
                <<"recipient">> => RichAddr,
                <<"quantity">> => <<"1">>
            },
            Seller,
            Opts
        ),
    StaleSet =
        live_send(
            <<"set:former-owner">>,
            ProcID,
            #{ <<"action">> => <<"set">>, <<"points-at">> => <<"seller again">> },
            Seller,
            Opts
        ),
    NewSet =
        live_send(
            <<"set:new-owner">>,
            ProcID,
            #{ <<"action">> => <<"set">>, <<"points-at">> => <<"buyer">> },
            Rich,
            Opts
        ),
    live_slots(
        ProcID,
        [
            {<<"set:before">>, FirstSet},
            {<<"transfer">>, Transfer},
            {<<"set:former-owner">>, StaleSet},
            {<<"set:new-owner">>, NewSet}
        ],
        Opts
    ),
    ProcID.

%% @doc The stories a run was asked for, named in the environment as
%% `HB_LIVE_STORIES=sale,withdrawn' or left unset for all of them.
live_stories() ->
    case os:getenv("HB_LIVE_STORIES") of
        false -> all;
        Names ->
            [
                hb_util:atom(string:trim(Name))
            ||
                Name <- string:split(Names, ",", all)
            ]
    end.

live_story(Name, all, Fun) -> live_story(Name, [Name], Fun);
live_story(Name, Wanted, Fun) ->
    case lists:member(Name, Wanted) of
        true -> Fun();
        false ->
            ?event(name_token_live, {story_skipped, {name, Name}}),
            skipped
    end.

%% @doc Mint the five `pn-test-N' names the site's test namespace resolves
%% through, and report what to put in the manifest.
%%
%% Each is spawned already holding its unit and already pointing somewhere, so
%% none of them needs a message sent to it -- which is the whole point of
%% `initial-value', since the first message addressed to a process pays Arweave's
%% new-account fee. Targets alternate between a manifest and another reference,
%% because a name that points at a reference is the shape a person actually wants:
%% a reference can be updated with a bundled data item in seconds, where a name
%% token needs consensus.
%%
%%     HB_LIVE_NAMES=1 HB_PRINT=name_token_live \\
%%         rebar3 device test --devices dev_name_token \\
%%             --test dev_name_token:live_names_test_
live_names_test_() -> live_gated("HB_LIVE_NAMES", fun live_names/0, 7200).

live_names() ->
    Seller = live_wallet(),
    SellerAddr = live_address(Seller),
    Opts = live_opts(Seller),
    Targets = live_name_targets(),
    Minted =
        lists:map(
            fun({Index, Kind, Target}) ->
                Name = <<"pn-test-", (hb_util:bin(Index))/binary>>,
                ProcID =
                    live_post(
                        <<"mint:", Name/binary>>,
                        #{
                            <<"device">> => <<"process@1.0">>,
                            <<"type">> => <<"Process">>,
                            <<"scheduler-device">> => <<"arweave-scheduler@1.0">>,
                            <<"scheduler-mode">> => <<"all">>,
                            <<"execution-device">> => <<"name-token@1.0">>,
                            <<"swap-device">> => <<"arweave-swap@1.0">>,
                            <<"name">> => Name,
                            <<"ticker">> => <<"NAME">>,
                            <<"denomination">> => <<"0">>,
                            <<"total-supply">> => <<"1">>,
                            <<"initial-holder">> => SellerAddr,
                            <<"initial-value">> => Target,
                            <<"test-suite">> => <<"name-token">>
                        },
                        Seller,
                        Opts
                    ),
                ?event(name_token_live,
                    {minted,
                        {name, {string, Name}},
                        {process, {string, ProcID}},
                        {points_at, {string, Target}},
                        {kind, Kind}
                    }
                ),
                {Name, ProcID, Kind, Target}
            end,
            Targets
        ),
    lists:foreach(fun({_, ProcID, _, _}) -> live_await(ProcID, Opts) end, Minted),
    ?event(name_token_live,
        {namespace_entries,
            {holder, {string, SellerAddr}},
            {entries,
                {string,
                    hb_util:bin(
                        lists:flatten(
                            [
                                io_lib:format("~s=~s ", [Name, ProcID])
                            ||
                                {Name, ProcID, _, _} <- Minted
                            ]
                        )
                    )
                }
            }
        }
    ),
    {ok, Minted}.

%% @doc What each test name points at. The manifest is the AO site's own, so a
%% resolved name actually renders something; the references are real
%% `~reference@1.0' inits, so the deeper chain is exercised rather than mocked.
live_name_targets() ->
    % A real Arweave path manifest that a gateway serves today, so a resolved
    % name renders something rather than 404ing.
    Manifest = <<"6oMvmlBUUltTDz_T9pZrEP2QkzpCBGk83Br8XXbqy20">>,
    % Replaced with the test namespace's own reference once it is published; a
    % name pointing at a reference is the shape an owner actually wants, because
    % a reference can be repointed in seconds.
    Reference = <<"6oMvmlBUUltTDz_T9pZrEP2QkzpCBGk83Br8XXbqy20">>,
    [
        {1, manifest, Manifest},
        {2, reference, Reference},
        {3, manifest, Manifest},
        {4, reference, Reference},
        {5, manifest, Manifest}
    ].

%% @doc Play all three stories out on mainnet.
live_suite() ->
    Seller = live_wallet(),
    SellerAddr = live_address(Seller),
    Opts = live_opts(Seller),
    Poor = live_party(<<"name-token-poor.json">>),
    Rich = live_party(<<"name-token-buyer.json">>),
    PoorAddr = live_address(Poor),
    RichAddr = live_address(Rich),
    ?event(name_token_live,
        {parties,
            {seller, {string, SellerAddr}},
            {underpayer, {string, PoorAddr}},
            {buyer, {string, RichAddr}},
            {seller_balance, live_balance(SellerAddr, Opts)}
        }
    ),
    live_fund(PoorAddr, 500000000, Seller, Opts),
    live_fund(RichAddr, 500000000, Seller, Opts),
    % Each story stands alone on its own process, so a rerun can name just the
    % ones it needs rather than paying to create every account again.
    Wanted = live_stories(),
    Sale = live_story(sale, Wanted, fun() -> live_story_sale(Seller, Poor, Rich, Opts) end),
    Withdrawn =
        live_story(
            withdrawn,
            Wanted,
            fun() -> live_story_withdrawn(Seller, Poor, Rich, Opts) end
        ),
    Handover =
        live_story(handover, Wanted, fun() -> live_story_handover(Seller, Rich, Opts) end),
    ?event(name_token_live,
        {suite_complete,
            {sale, {string, Sale}},
            {withdrawn, {string, Withdrawn}},
            {handover, {string, Handover}},
            {buyer, {string, RichAddr}},
            {underpayer, {string, PoorAddr}},
            {seller, {string, SellerAddr}},
            {seller_balance, live_balance(SellerAddr, Opts)}
        }
    ),
    ok.

%%% The permanent fixture
%%%
%%% A name that was really sold on Arweave, by the driver above. Everything
%%% below is a deterministic read of blocks 1966039-1966044 of the weave, so it
%%% is repeatable forever: the seller spawned `test-name' holding its single
%%% unit, offered it for 1000 winston with no bond and a 1000 winston fee to
%%% register, the buyer registered (paying that fee), paid, and then -- owning
%%% the name -- pointed it at a message of their own.
%%%
%%% One transaction per block, so the schedule's order is unambiguous:
%%%
%%%     1966039  the name          yWRe7v4S...
%%%     1966041  make-offer        3BApJHea...  (the order id)
%%%     1966042  register-interest GGPH2lA8...
%%%     1966043  payment           KROLsGpr...  (to the seller, not the process)
%%%     1966044  set               QjmNGlIi...
%%%
%%% The payment is the point: it is an ordinary transfer between two addresses,
%%% the process is not a party to it, and the process sees it only because
%%% `~arweave-scheduler@1.0' is sequencing it by every transaction on the
%%% network. Every transaction in that range is a slot of this
%%% process, not just these five.
-define(FIXTURE_PROCESS, <<"yWRe7v4SZ4_NKV6LkYyNPrFdzEaGh0ckblu-CaGXqG4">>).
-define(FIXTURE_SELLER, <<"ggltHF0Cnv9ylH3vM1p7amR2vXLMoPLQIUQmAEwLP-k">>).
-define(FIXTURE_BUYER, <<"LW0myHWuv7XcLec19OCDzFJW0P6jXPG_Ao49kfy9Slc">>).
-define(FIXTURE_ORDER, <<"3BApJHeatc9pVuLgjZ_P-HT5hZgRE1Q3I1bdTESgRDM">>).
-define(FIXTURE_MAX_HEIGHT, 1966044).

%% @doc Read the fixture's state as of the pinned height. The height cap makes
%% the answer immutable: no block after 1966044 can reach this process.
fixture_opts() ->
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
        <<"arweave-scheduler-max-height">> => ?FIXTURE_MAX_HEIGHT,
        <<"name-resolvers">> => [#{ <<"test-name">> => ?FIXTURE_PROCESS }],
        <<"node-host">> => <<"host">>,
        <<"priv-wallet">> => ar_wallet:new()
    }.

%% @doc Synchronize the fixture's schedule from the network, retrying while the
%% gateway rate-limits us -- the same allowance the scheduler's own fixture
%% tests make.
fixture_sync(_Opts, 0) -> {error, fixture_sync_failed};
fixture_sync(Opts, Attempts) ->
    case
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave-scheduler@1.0">> },
            #{
                <<"path">> => <<"schedule">>,
                <<"method">> => <<"GET">>,
                <<"target">> => ?FIXTURE_PROCESS
            },
            Opts
        )
    of
        {ok, Schedule} -> {ok, Schedule};
        _ ->
            timer:sleep(5000),
            fixture_sync(Opts, Attempts - 1)
    end.

%% @doc Compute the fixture to its latest slot. The schedule is primed first, so
%% that the process message is read back as its canonical `tx@1.0' decoding
%% rather than a gateway store's lossier one.
fixture_state(Opts, Attempts) ->
    {ok, _} = fixture_sync(Opts, Attempts),
    {ok, Raw} = hb_cache:read(?FIXTURE_PROCESS, Opts),
    Process = hb_cache:ensure_all_loaded(Raw, Opts),
    hb_ao:resolve(Process, <<"now">>, Opts).

%% @doc The whole story, read back off the weave: a name that changed hands for
%% AR that never touched the process, and then said something new.
fixture_sale_test_() ->
    {timeout, 1800, fun fixture_sale/0}.
fixture_sale() ->
    Opts = fixture_opts(),
    {ok, State} = fixture_state(Opts, 5),
    Read = fun(Path) -> hb_ao:get(Path, {as, <<"message@1.0">>, State}, not_found, Opts) end,
    % The name is the buyer's: the swap settled a payment it was not paid.
    ?assertEqual(1, hb_util:int(Read([?BALANCES, ?FIXTURE_BUYER]))),
    ?assertEqual(0, hb_util:int(Read([?BALANCES, ?FIXTURE_SELLER]))),
    ?assertEqual(1, hb_util:int(Read(<<"total-supply">>))),
    % The order it went through is settled, and the buyer is recorded as the
    % one who paid.
    ?assertEqual(
        <<"settled">>,
        Read([<<"orders">>, ?FIXTURE_ORDER, <<"status">>])
    ),
    ?assertEqual(
        ?FIXTURE_BUYER,
        Read([<<"orders">>, ?FIXTURE_ORDER, <<"buyer">>])
    ),
    % And the new owner has said what the name points at.
    ?assertEqual(<<"hello from the new owner">>, Read([?VALUE, <<"greeting">>])),
    ?assertEqual(<<"text/plain">>, Read([?VALUE, <<"content-type">>])).

%% @doc The name resolves: `test-name' reaches this instance, both as a bare
%% name and as the label of a host.
fixture_name_resolution_test() ->
    Opts = fixture_opts(),
    % A node that serves a name holds it. Priming the schedule puts the process
    % in the node's own cache, which is what the resolver then loads.
    {ok, _} = fixture_sync(Opts, 5),
    ?assertEqual(
        {ok, ?FIXTURE_PROCESS},
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
    ?assertEqual(
        ?FIXTURE_SELLER,
        hb_ao:get(<<"initial-holder">>, Loaded, not_found, Opts)
    ),
    ?assertEqual(
        <<"name-token@1.0">>,
        hb_ao:get(<<"execution-device">>, Loaded, not_found, Opts)
    ).

%%% Story one, replayed: a name that had to be paid for
%%%
%%% Every transaction below is on mainnet. The reads walk the process forward
%%% one slot at a time, so what the schedule did to the state is visible in the
%%% order it happened, rather than only at the end.
%%%
%%%   make-offer          the seller's unit goes into escrow, the order opens
%%%   register:underpaid  a reward of 33,039,920 does not clear a 100,000,000
%%%                       fee -- the order stays open
%%%   register:stranded   150,000,000 sent *to the process* is not the fee
%%%                       either: it would only be stranded there
%%%   register:paid       a reward of 150,000,000 does clear it -- reserved
%%%   payment:interloper  the underpayer pays anyway, but the order is not
%%%                       theirs and there is no bond to compensate them with
%%%   payment:buyer       the registrant pays -- settled, and the name moves
%%%   set:former-owner    the seller no longer holds it, so is no longer heard
%%%   set:owner           the buyer says what the name means
%%%   stray:path-info     a stranger routes a slot at `info'; nothing breaks
%%%   make-offer:nonsense a deadline of `tomorrow' opens no order
-define(SALE_PROCESS, <<"an95oAK9MlahZI_tKKeG4ykzNN01qfMi2WfJO58o_UU">>).
-define(SALE_SELLER, <<"ggltHF0Cnv9ylH3vM1p7amR2vXLMoPLQIUQmAEwLP-k">>).
-define(SALE_UNDERPAYER, <<"2yvAwMDrF62hpH_kKTfguatzB9mKVzcM2edAn8KauTQ">>).
-define(SALE_BUYER, <<"LW0myHWuv7XcLec19OCDzFJW0P6jXPG_Ao49kfy9Slc">>).
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

%% @doc Node options pinned to a story's last block, so its answer is
%% immutable: no block after it can reach the process.
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

%% @doc Synchronize a story's schedule, retrying while the gateway rate-limits
%% us -- the same allowance the scheduler's own fixture tests make.
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

%% @doc The slot a transaction was given. The stories pin transaction ids
%% rather than slot numbers, because a slot number is a fact about the weave --
%% every transaction on the network takes one -- while the id is the message
%% itself.
slot_of(Schedule, TXID, Opts) ->
    Assignments =
        hb_ao:normalize_keys(hb_ao:get(<<"assignments">>, Schedule, Opts), Opts),
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

sale_story_test_() -> {timeout, 3600, fun sale_story/0}.
sale_story() ->
    Process = ?SALE_PROCESS,
    Opts = story_opts(?SALE_MAX_HEIGHT, Process),
    {ok, Schedule} = story_sync(Process, Opts, 5),
    Slot = fun(TXID) -> slot_of(Schedule, TXID, Opts) end,
    Seller = ?SALE_SELLER,
    Buyer = ?SALE_BUYER,
    Poor = ?SALE_UNDERPAYER,
    Order = ?SALE_OFFER,
    % The offer escrows the seller's only unit, and opens the order.
    Offered = Slot(?SALE_OFFER),
    ?assertEqual({ok, 0}, at(Process, Offered, <<"balances/", Seller/binary>>, Opts)),
    ?assertEqual(
        {ok, <<"open">>},
        at(Process, Offered, <<"orders/", Order/binary, "/status">>, Opts)
    ),
    ?assertEqual(
        {ok, 100000000},
        at(Process, Offered, <<"orders/", Order/binary, "/minimum-fee">>, Opts)
    ),
    % A registration paying only what the network charges does not clear a fee
    % set above it, and neither does value sent to the process.
    Underpaid = Slot(?SALE_UNDERPAID),
    ?assertEqual(
        {ok, <<"open">>},
        at(Process, Underpaid, <<"orders/", Order/binary, "/status">>, Opts)
    ),
    Stranded = Slot(?SALE_STRANDED),
    ?assertEqual(
        {ok, <<"open">>},
        at(Process, Stranded, <<"orders/", Order/binary, "/status">>, Opts)
    ),
    % Overpaying the reward does.
    Paid = Slot(?SALE_PAID),
    ?assertEqual(
        {ok, <<"reserved">>},
        at(Process, Paid, <<"orders/", Order/binary, "/status">>, Opts)
    ),
    ?assertEqual(
        {ok, Buyer},
        at(Process, Paid, <<"orders/", Order/binary, "/buyer">>, Opts)
    ),
    % Somebody else's payment buys nothing while it is reserved, and with no
    % bond posted there is nothing to compensate them with either.
    Interloped = Slot(?SALE_INTERLOPER),
    ?assertEqual(
        {ok, <<"reserved">>},
        at(Process, Interloped, <<"orders/", Order/binary, "/status">>, Opts)
    ),
    ?assertMatch(
        {error, not_found},
        at(Process, Interloped, <<"balances/", Poor/binary>>, Opts)
    ),
    % The registrant's payment settles it, and the name moves.
    Settled = Slot(?SALE_PAYMENT),
    ?assertEqual(
        {ok, <<"settled">>},
        at(Process, Settled, <<"orders/", Order/binary, "/status">>, Opts)
    ),
    ?assertEqual({ok, 1}, at(Process, Settled, <<"balances/", Buyer/binary>>, Opts)),
    ?assertEqual({ok, 0}, at(Process, Settled, <<"balances/", Seller/binary>>, Opts)),
    % The seller no longer holds the name, so is no longer heard.
    Stale = Slot(?SALE_STALE_SET),
    ?assertMatch({error, not_found}, at(Process, Stale, <<"value/greeting">>, Opts)),
    % The buyer is.
    Spoken = Slot(?SALE_OWNER_SET),
    ?assertEqual(
        {ok, <<"hello from the new owner">>},
        at(Process, Spoken, <<"value/greeting">>, Opts)
    ),
    % A stranger routing a slot at `info' changes nothing and breaks nothing:
    % the process still computes, and still holds what it held.
    Stray = Slot(?SALE_STRAY),
    ?assertEqual({ok, 1}, at(Process, Stray, <<"balances/", Buyer/binary>>, Opts)),
    ?assertEqual(
        {ok, <<"hello from the new owner">>},
        at(Process, Stray, <<"value/greeting">>, Opts)
    ),
    % And an offer whose deadline is `tomorrow' opens nothing.
    Nonsense = Slot(?SALE_NONSENSE),
    ?assertMatch(
        {error, not_found},
        at(Process, Nonsense, <<"orders/", (?SALE_NONSENSE)/binary, "/status">>, Opts)
    ),
    ?assertEqual({ok, 1}, at(Process, Nonsense, <<"balances/", Buyer/binary>>, Opts)).

%%% Story two, replayed: an offer withdrawn
%%%
%%% A seller may take back an order nobody has reserved. Once they have, the
%%% order is spent: registering against it is refused however much is paid, and
%%% a payment against it buys nothing and -- there being no bond -- compensates
%%% nobody. A stranger may not withdraw somebody else's order.
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
    Process = ?WITHDRAWN_PROCESS,
    Opts = story_opts(?WITHDRAWN_MAX_HEIGHT, Process),
    {ok, Schedule} = story_sync(Process, Opts, 5),
    Slot = fun(TXID) -> slot_of(Schedule, TXID, Opts) end,
    Seller = ?SALE_SELLER,
    Buyer = ?SALE_BUYER,
    First = ?WITHDRAWN_OFFER,
    Second = ?WITHDRAWN_SECOND,
    % The offer escrows the unit.
    Offered = Slot(?WITHDRAWN_OFFER),
    ?assertEqual({ok, 0}, at(Process, Offered, <<"balances/", Seller/binary>>, Opts)),
    ?assertEqual(
        {ok, <<"open">>},
        at(Process, Offered, <<"orders/", First/binary, "/status">>, Opts)
    ),
    % Withdrawing it gives the unit back.
    Cancelled = Slot(?WITHDRAWN_CANCEL),
    ?assertEqual(
        {ok, <<"cancelled">>},
        at(Process, Cancelled, <<"orders/", First/binary, "/status">>, Opts)
    ),
    ?assertEqual({ok, 1}, at(Process, Cancelled, <<"balances/", Seller/binary>>, Opts)),
    % A registration that pays the fee in full is still refused: the order is
    % no longer open.
    LateRegister = Slot(?WITHDRAWN_LATE_REGISTER),
    ?assertEqual(
        {ok, <<"cancelled">>},
        at(Process, LateRegister, <<"orders/", First/binary, "/status">>, Opts)
    ),
    ?assertMatch(
        {error, not_found},
        at(Process, LateRegister, <<"orders/", First/binary, "/buyer">>, Opts)
    ),
    % And a payment against it moves nothing, in either direction: the goods
    % are back with the seller and there was no bond to compensate anyone from.
    LatePayment = Slot(?WITHDRAWN_LATE_PAYMENT),
    ?assertEqual(
        {ok, <<"cancelled">>},
        at(Process, LatePayment, <<"orders/", First/binary, "/status">>, Opts)
    ),
    ?assertEqual(
        {ok, 1},
        at(Process, LatePayment, <<"balances/", Seller/binary>>, Opts)
    ),
    ?assertMatch(
        {error, not_found},
        at(Process, LatePayment, <<"balances/", Buyer/binary>>, Opts)
    ),
    % The seller offers it again, and a stranger tries to withdraw it.
    Reoffered = Slot(?WITHDRAWN_SECOND),
    ?assertEqual(
        {ok, <<"open">>},
        at(Process, Reoffered, <<"orders/", Second/binary, "/status">>, Opts)
    ),
    Meddled = Slot(?WITHDRAWN_STRANGER),
    ?assertEqual(
        {ok, <<"open">>},
        at(Process, Meddled, <<"orders/", Second/binary, "/status">>, Opts)
    ),
    ?assertEqual({ok, 0}, at(Process, Meddled, <<"balances/", Seller/binary>>, Opts)).

%%% Story three, replayed: a name handed over
%%%
%%% No sale at all -- the token half on its own. The unit moves by `transfer',
%%% and the authority moves with it: the former holder's word stops counting
%%% the moment it does, and the new holder's starts.
-define(HANDOVER_PROCESS, <<"D4uhF_nO_vyPoIhPDZ0kFMyfOnk1ZCFJFkmnxVw7vSs">>).
-define(HANDOVER_FIRST_SET, <<"dx_Dmvnp2FDZdb3wlDpfVvc4k3UCbQRWLXIEwwgGxIA">>).
-define(HANDOVER_TRANSFER, <<"WgX3Ih8Ef7aDzQ_8Ziio_qYSA5z_-OUcCOVjmdC8WV0">>).
-define(HANDOVER_STALE_SET, <<"-epGyJadxPyQK_bibsic_btn133f_J8UCX1AndWIWRs">>).
-define(HANDOVER_NEW_SET, <<"mqpK8TsoWFUJW345gcYLrFmw4nxqyGqpavt7xUaniC0">>).
-define(HANDOVER_MAX_HEIGHT, 1966100).

handover_story_test_() -> {timeout, 3600, fun handover_story/0}.
handover_story() ->
    Process = ?HANDOVER_PROCESS,
    Opts = story_opts(?HANDOVER_MAX_HEIGHT, Process),
    {ok, Schedule} = story_sync(Process, Opts, 5),
    Slot = fun(TXID) -> slot_of(Schedule, TXID, Opts) end,
    Seller = ?SALE_SELLER,
    Buyer = ?SALE_BUYER,
    % While the seller holds it, the seller speaks for it.
    Spoke = Slot(?HANDOVER_FIRST_SET),
    ?assertEqual({ok, 1}, at(Process, Spoke, <<"balances/", Seller/binary>>, Opts)),
    ?assertEqual({ok, <<"seller">>}, at(Process, Spoke, <<"value/points-at">>, Opts)),
    % The unit moves.
    Moved = Slot(?HANDOVER_TRANSFER),
    ?assertEqual({ok, 0}, at(Process, Moved, <<"balances/", Seller/binary>>, Opts)),
    ?assertEqual({ok, 1}, at(Process, Moved, <<"balances/", Buyer/binary>>, Opts)),
    % The notices `token-1.0' emits for a transfer are the slot's results.
    ?assertEqual(
        {ok, <<"Debit-Notice">>},
        at(Process, Moved, <<"results/outbox/1/action">>, Opts)
    ),
    ?assertEqual(
        {ok, <<"Credit-Notice">>},
        at(Process, Moved, <<"results/outbox/2/action">>, Opts)
    ),
    % The former holder is no longer heard: the name still says what it said.
    Stale = Slot(?HANDOVER_STALE_SET),
    ?assertEqual({ok, <<"seller">>}, at(Process, Stale, <<"value/points-at">>, Opts)),
    % The new holder is.
    Spoken = Slot(?HANDOVER_NEW_SET),
    ?assertEqual({ok, <<"buyer">>}, at(Process, Spoken, <<"value/points-at">>, Opts)),
    ?assertEqual({ok, 1}, at(Process, Spoken, <<"balances/", Buyer/binary>>, Opts)).
