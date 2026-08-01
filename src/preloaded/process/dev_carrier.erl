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
%%% or lending the unit -- with devices that settle against the same `balances'
%%% -- sells or escrows the name.
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
%%%       they stand at that slot. A live loan may also keep this right with the
%%%       borrower while the unit is escrowed, if the loan allows it.</li>
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
%%% Reads fall through to the linked value, as they do in `~reference@1.0'.
%%% Every scheduler assignment is explicitly routed to `compute', so a
%%% base-layer transaction's `path' is data and cannot select a device key.
-module(dev_carrier).
-implements(<<"carrier@1.0">>).
%%% AO-Core API functions:
-export([info/0, compute/3, init/3, snapshot/3, normalize/3, request/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The balances submessage, shared with any device that settles in this token.
-define(BALANCES, <<"balances">>).
%%% The linked message whose keys the name resolves through to.
-define(VALUE, <<"value">>).
%%% The share of the supply a signer must hold to set the name, in basis
%%% points. The whole supply, unless the token says otherwise.
-define(DEFAULT_THRESHOLD_BPS, 10000).

%% @doc Resolve unimplemented keys through the value carried by the name.
info() ->
    #{
        default => fun get/4,
        excludes => [<<"keys">>, <<"set">>, <<"set-path">>, <<"remove">>]
    }.

%% @doc The carrier state is already a plain message.
init(Base, _Req, _Opts) -> {ok, Base}.
snapshot(Base, _Req, _Opts) -> {ok, Base}.
normalize(Base, _Req, _Opts) -> {ok, Base}.

%% @doc Resolve a key through the message the name currently carries.
get(Key, Base, Req, Opts) ->
    hb_ao:resolve(value_base(value(Base, Opts), Opts), Req#{ <<"path">> => Key }, Opts).

%% @doc Dereference a carrier process selected by an earlier request hook.
request(_Base, Req, Opts) ->
    maybe
        {ok, [Process | Rest]} ?= hb_maps:find(<<"body">>, Req, Opts),
        true ?= is_map(Process),
        <<"carrier@1.0">> ?=
            hb_maps:get(<<"execution-device">>, Process, undefined, Opts),
        {ok, State} ?=
            hb_ao:resolve(
                Process,
                (hb_maps:get(<<"request">>, Req, #{}, Opts))#{
                    <<"path">> => <<"compute">>
                },
                Opts
            ),
        {ok, Req#{ <<"body">> => [value_base(value(State, Opts), Opts) | Rest] }}
    else
        _ -> {ok, Req}
    end.

%% @doc Apply one assignment to the name's state.
%%
%% A name that is for sale or collateral is settled in the same balances it
%% keeps, so every message is offered to those devices first -- including the
%% ones this device would otherwise ignore, since payments are transfers between
%% two other addresses. Which devices those are, are scalar keys on the process;
%% see `loan/3' and `swap/3' for why this is not a `~stack@1.0'.
compute(Base, Assignment, Opts) ->
    Loaned = loan(seed(Base, Opts), Assignment, Opts),
    Sold = swap(Loaned, Assignment, Opts),
    Body = hb_maps:get(<<"body">>, Assignment, #{}, Opts),
    ProcID = hb_maps:get(<<"process">>, Assignment, <<>>, Opts),
    case tx_field(Body, <<"target">>, <<>>, Opts) of
        <<>> -> {ok, Sold};
        ProcID -> {ok, action(Sold, Body, Opts)};
        _ ->
            % Not addressed to this name. A sparse assignment may still arrive
            % because its `Assign-To' tag named the process.
            {ok, Sold}
    end.

%% @doc Hand the message to the device that lends this name, if it has one, and
%% take back the state it produces.
%%
%% This is a `~stack@1.0' written out by hand, and deliberately so. A process
%% spawned as an Arweave transaction can only carry flat, scalar tags: the
%% codec turns a submessage or a list into a `+link' to content that is not on
%% the weave, so nothing else can read it back. `device-stack' is a list, so a
%% stack cannot survive the spawn -- but `loan-device' is one word.
loan(Base, Assignment, Opts) ->
    case state(<<"loan-device">>, Base, not_found, Opts) of
        not_found -> Base;
        Device ->
            try hb_ao:resolve(Base#{ <<"device">> => Device }, Assignment, Opts) of
                {ok, Settled} -> Settled#{ <<"device">> => <<"carrier@1.0">> };
                _ -> Base
            catch
                _:_ -> Base
            end
    end.

%% @doc Hand the message to the device that sells this name, if it has one, and
%% take back the state it produces.
%%
%% See `loan/3'.
swap(Base, Assignment, Opts) ->
    case state(<<"swap-device">>, Base, not_found, Opts) of
        not_found -> Base;
        Device ->
            try hb_ao:resolve(Base#{ <<"device">> => Device }, Assignment, Opts) of
                {ok, Settled} -> Settled#{ <<"device">> => <<"carrier@1.0">> };
                _ -> Base
            catch
                _:_ -> Base
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
                    Supply = supply(Base, Opts),
                    ?event({carrier_seeded, {holder, Holder}, {supply, Supply}}),
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
                    ?event({carrier_seeded_value, {target, Target}}),
                    Base#{ ?VALUE => #{ <<"target">> => Target } };
                _ -> Base
            end
    end.

%% @doc Route a message addressed to the name by its `action'. Matching is
%% case-insensitive, as `token-1.0' matches. An unknown action leaves the state
%% untouched rather than failing the slot, which would stop the process on every
%% node for good.
action(Base, Body, Opts) ->
    case hb_util:to_lower(field(<<"action">>, Body, <<>>, Opts)) of
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
        Recipient = field(<<"recipient">>, Body, not_found, Opts),
        true ?= is_binary(Recipient),
        {ok, Quantity} ?= hb_util:safe_int(field(<<"quantity">>, Body, 0, Opts)),
        true ?= Quantity >= 1,
        true ?= balance(Base, Sender, Opts) >= Quantity,
        ?event(
            {carrier_transfer,
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
            Quantity
        )
    else
        _ -> Base
    end.

%% @doc Emit the pair of notices that `token-1.0' emits for a transfer, with
%% the same keys. They are the slot's results; whether anything delivers them
%% is the process's business, not this device's.
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
        true ?= can_set(Base, Signer, Opts),
        Value = value_of(Body, Opts),
        ?event({carrier_set, {by, Signer}}),
        Base#{ ?VALUE => Value }
    else
        _ -> Base
    end.

%% @doc The message a `set' is asking the name to resolve to.
value_of(Body, Opts) ->
    case field(<<"reference-value">>, Body, not_found, Opts) of
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
    Supply = supply(Base, Opts),
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

can_set(Base, Address, Opts) ->
    owns_supply(Base, Address, Opts) orelse borrower_can_set(Base, Address, Opts).

borrower_can_set(Base, Address, Opts) ->
    lists:any(
        fun
            (#{
                <<"status">> := <<"active">>,
                <<"borrower">> := Borrower
            } = Loan) when Borrower =:= Address ->
                hb_util:bool(hb_maps:get(<<"borrower-set">>, Loan, true, Opts));
            (_) -> false
        end,
        loans(Base, Opts)
    ).

loans(Base, Opts) ->
    [
        Loan
    ||
        Loan = #{ <<"loan-id">> := _ } <-
            hb_maps:values(
                hb_cache:ensure_all_loaded(
                    state(<<"loans">>, Base, #{}, Opts),
                    Opts
                ),
                Opts
            )
    ].

%% @doc Read the name's supply, falling back to its single unit if malformed.
supply(Base, Opts) ->
    hb_util:ok_or(
        hb_util:safe_int(state(<<"total-supply">>, Base, 1, Opts)),
        1
    ).

%%% State

%% @doc Read a key of the process's own state. While a slot is being computed
%% the state carries this device, so a plain read would resolve the key back
%% through `compute'. See `dev_arweave_swap:state/4'.
state(Key, Base, Default, Opts) ->
    hb_ao:get(Key, {as, <<"message@1.0">>, Base}, Default, Opts).

%% @doc Read a field from an untrusted scheduled message as plain data.
field(Key, Msg, Default, Opts) ->
    hb_maps:get(Key, Msg, Default, Opts).

%% @doc Read a value from the real layer-1 transaction fields.
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

balance(Base, Address, Opts) ->
    hb_util:int(state([?BALANCES, Address], Base, 0, Opts)).

%% @doc Credit one balance by replacing the whole balances submessage.
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
%%% `~process@1.0' would. The process id must be a real 43-character address so
%%% the transaction codec can carry it as the layer-1 target.
-define(PROCESS, <<"cArRiEr000000000000000000000000000000000000">>).

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
                <<"path">> => <<"compute">>,
                <<"body">> => Body
            },
            Opts
        ),
    New.

%% @doc A `target' tag is not an address. A transaction addressed to nobody
%% carries the process's own id at the key a plain read would find, so the gate
%% reads the field the base layer moved value to instead.
tags_are_not_transaction_fields_test() ->
    Opts = test_opts(),
    {_, OwnerAddr} = party(),
    {Stranger, StrangerAddr} = party(),
    Held = name_held_by(OwnerAddr),
    Steal =
        tag_only_tx(
            Stranger,
            [
                {<<"target">>, ?PROCESS},
                {<<"action">>, <<"transfer">>},
                {<<"recipient">>, StrangerAddr},
                {<<"quantity">>, <<"1">>}
            ]
        ),
    ?assertEqual(?PROCESS, hb_ao:get(<<"target">>, Steal, not_found, Opts)),
    ?assertEqual(<<>>, tx_field(Steal, <<"target">>, <<>>, Opts)),
    Tried = apply_tx(Held, Steal, Opts),
    ?assertEqual(1, held_by(Tried, OwnerAddr, Opts)),
    ?assertEqual(0, held_by(Tried, StrangerAddr, Opts)).

%% @doc A transaction that carries its keys as tags only, addressed to nobody.
tag_only_tx(Wallet, Tags) ->
    Signed = ar_tx:sign(#tx{ format = 2, reward = 1, tags = Tags }, Wallet),
    hb_message:convert(Signed, <<"structured@1.0">>, <<"tx@1.0">>, #{}).

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

%% @doc A sparse transaction relevant through `Assign-To', but carrying no
%% carrier or swap action.
assigned_tx(Wallet) ->
    tx(
        Wallet,
        #{
            <<"target">> => <<"elsewhere">>,
            <<"assign-to">> => ?PROCESS
        }
    ).

held_by(Base, Address, Opts) -> balance(Base, Address, Opts).

value(Base, Opts) ->
    hb_cache:ensure_all_loaded(state(?VALUE, Base, #{}, Opts), Opts).

%% @doc Follow the pointer used by an `initial-value', or a directly linked
%% value supplied by a later `set'.
value_base(ID, Opts) when ?IS_ID(ID) ->
    case hb_cache:read(ID, Opts) of
        {ok, Msg} -> Msg;
        _ -> ID
    end;
value_base(Link, Opts) when ?IS_LINK(Link) ->
    hb_cache:ensure_loaded(Link, Opts);
value_base(Value, Opts) ->
    case hb_maps:get(<<"target">>, Value, not_found, Opts) of
        Target when ?IS_ID(Target); ?IS_LINK(Target) ->
            value_base(Target, Opts);
        _ -> Value
    end.

%% @doc A foreign transaction's device cannot interpret absent envelope fields.
foreign_device_is_data_test() ->
    Opts = test_opts(),
    {Owner, OwnerAddr} = party(),
    Foreign =
        tx(
            Owner,
            #{
                <<"device">> => <<"reference@1.0">>
            }
        ),
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

%% @doc An active loan can leave name updates with the borrower while the unit
%% is escrowed.
active_loan_borrower_can_set_test() ->
    Opts = test_opts(),
    {Borrower, BorrowerAddr} = party(),
    Loaned =
        (name_held_by(BorrowerAddr))#{
            ?BALANCES => #{ BorrowerAddr => 0 },
            <<"loans">> =>
                #{
                    <<"loan">> =>
                        #{
                            <<"loan-id">> => <<"loan">>,
                            <<"status">> => <<"active">>,
                            <<"borrower">> => BorrowerAddr,
                            <<"borrower-set">> => true
                        }
                }
        },
    Set = apply_tx(Loaned, set_tx(Borrower, #{ <<"points-at">> => <<"borrower">> }), Opts),
    ?assertEqual(
        <<"borrower">>,
        hb_ao:get(<<"points-at">>, value(Set, Opts), not_found, Opts)
    ).

%% @doc A loan can also lock updates until the collateral is released or
%% claimed.
loan_can_disable_borrower_set_test() ->
    Opts = test_opts(),
    {Borrower, BorrowerAddr} = party(),
    Loaned =
        (name_held_by(BorrowerAddr))#{
            ?BALANCES => #{ BorrowerAddr => 0 },
            <<"loans">> =>
                #{
                    <<"loan">> =>
                        #{
                            <<"loan-id">> => <<"loan">>,
                            <<"status">> => <<"active">>,
                            <<"borrower">> => BorrowerAddr,
                            <<"borrower-set">> => false
                        }
                }
        },
    Result = apply_tx(Loaned, set_tx(Borrower, #{ <<"points-at">> => <<"borrower">> }), Opts),
    ?assertEqual(#{}, value(Result, Opts)).

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

%% @doc A transaction assigned to this name but not addressed to it is left
%% alone when it is not a swap payment.
assigned_non_payment_test() ->
    Opts = test_opts(),
    {Stranger, _} = party(),
    {_, OwnerAddr} = party(),
    Base = name_held_by(OwnerAddr),
    Result =
        apply_tx(
            Base,
            tx(
                Stranger,
                #{
                    <<"target">> => <<"somebody-else">>,
                    <<"quantity">> => <<"1">>,
                    <<"assign-to">> => ?PROCESS
                }
            ),
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
    % The first relevant message brings the name to life.
    Alive = apply_tx(Spawned, assigned_tx(Owner), Opts),
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
    Alive = apply_tx(Spawned, assigned_tx(Owner), Opts),
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

%% @doc An unreadable supply falls back to the single unit a name represents.
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
            assigned_tx(Owner),
            Opts
        ),
    ?assertEqual(1, held_by(Alive, OwnerAddr, Opts)),
    Set = apply_tx(Alive, set_tx(Owner, #{ <<"greeting">> => <<"mine">> }), Opts),
    ?assertEqual(
        <<"mine">>,
        hb_ao:get(<<"greeting">>, value(Set, Opts), not_found, Opts)
    ).

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
    Alive = apply_tx(Spawned, assigned_tx(Owner), Opts),
    Moved = apply_tx(Alive, transfer_tx(Owner, StrangerAddr, 1), Opts),
    Later = apply_tx(Moved, assigned_tx(Stranger), Opts),
    ?assertEqual(0, held_by(Later, OwnerAddr, Opts)),
    ?assertEqual(1, held_by(Later, StrangerAddr, Opts)).

%% @doc A carrier with no swap configured applies its own actions normally.
without_a_swap_device_test() ->
    Opts = test_opts(),
    {Owner, OwnerAddr} = party(),
    Set =
        apply_tx(
            name_held_by(OwnerAddr),
            set_tx(Owner, #{ <<"greeting">> => <<"no swap here">> }),
            Opts
        ),
    ?assertEqual(
        <<"no swap here">>,
        hb_ao:get(<<"greeting">>, value(Set, Opts), not_found, Opts)
    ),
    ?assertEqual(not_found, state(<<"orders">>, Set, not_found, Opts)).

%% @doc A sparse payment names this process in `Assign-To' while its real L1
%% target is the seller. The swap settles it before the carrier applies its own
%% addressed actions, moving the name's unit to the buyer.
swap_payment_moves_the_unit_test() ->
    Opts = test_opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    Opened =
        apply_tx(
            (name_held_by(SellerAddr))#{
                <<"swap-device">> => <<"arweave-swap@1.0">>
            },
            tx(
                Seller,
                #{
                    <<"target">> => ?PROCESS,
                    <<"action">> => <<"make-offer">>,
                    <<"offer-quantity">> => <<"1">>,
                    <<"asking">> => <<"500">>,
                    <<"deposit">> => <<"0">>,
                    <<"deadline">> => <<"20">>
                }
            ),
            Opts
        ),
    [OrderID] =
        [
            ID
        ||
            ID <- hb_maps:keys(state(<<"orders">>, Opened, #{}, Opts), Opts),
            ?IS_ID(ID)
        ],
    Payment =
        tx(
            Buyer,
            #{
                <<"target">> => SellerAddr,
                <<"quantity">> => <<"500">>,
                <<"order-id">> => OrderID,
                <<"assign-to">> => ?PROCESS
            }
        ),
    ?assertEqual(?PROCESS, field(<<"assign-to">>, Payment, not_found, Opts)),
    Sold = apply_tx(Opened, Payment, Opts),
    ?assertEqual(0, held_by(Sold, SellerAddr, Opts)),
    ?assertEqual(1, held_by(Sold, BuyerAddr, Opts)),
    ?assertEqual(
        not_found,
        state([<<"orders">>, OrderID], Sold, not_found, Opts)
    ).

%% @doc Unknown keys resolve through the name's current value.
default_reads_value_test() ->
    Opts = test_opts(),
    ?assertEqual(
        {ok, <<"hello">>},
        hb_ao:resolve(
            #{
                <<"device">> => <<"carrier@1.0">>,
                ?VALUE => #{ <<"greeting">> => <<"hello">> }
            },
            <<"greeting">>,
            Opts
        )
    ).

%% @doc Initial values are pointers to messages, not the pointer wrapper.
initial_value_resolves_target_test() ->
    Store = hb_test_utils:test_store(),
    hb_store:start(Store),
    Opts = #{ <<"store">> => [Store] },
    Target = #{ <<"greeting">> => <<"hello">> },
    {ok, TargetID} = hb_cache:write(Target, Opts),
    ?assertEqual(
        {ok, <<"hello">>},
        hb_ao:resolve(
            #{
                <<"device">> => <<"carrier@1.0">>,
                ?VALUE => #{ <<"target">> => TargetID }
            },
            <<"greeting">>,
            Opts
        )
    ).

%% @doc The request hook unwraps carrier processes and ignores other bases.
request_dereferences_process_test() ->
    Store = hb_test_utils:test_store(),
    hb_store:start(Store),
    Opts = #{ <<"store">> => [Store] },
    Target = #{ <<"greeting">> => <<"hello">> },
    {ok, TargetID} = hb_cache:write(Target, Opts),
    State = #{ ?VALUE => #{ <<"target">> => TargetID } },
    Process =
        #{
            <<"device">> =>
                #{
                    compute =>
                        fun(_Base, _Req, _CallOpts) ->
                            {ok, State}
                        end
                },
            <<"execution-device">> => <<"carrier@1.0">>
        },
    Req = #{ <<"request">> => #{}, <<"body">> => [Process] },
    {ok, Res} = request(#{}, Req, Opts),
    ?assertEqual(
        <<"hello">>,
        hb_ao:get(
            [<<"body">>, 1, <<"greeting">>],
            Res,
            not_found,
            Opts
        )
    ),
    IDReq = Req#{ <<"body">> => [TargetID] },
    ?assertEqual({ok, IDReq}, request(#{}, IDReq, Opts)).
