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
%%% Reads fall through to the linked value, as they do in `~reference@1.0'.
%%% Every `all'-mode assignment is explicitly routed to `compute' by
%%% `~arweave-scheduler@1.0', so a base-layer transaction's `path' is data and
%%% cannot select a device key.
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
        <<>> -> {ok, Sold};
        ProcID -> {ok, action(Sold, Body, Opts)};
        _ ->
            % Not addressed to this name. Under `all' mode that is almost every
            % data-free transaction on the network.
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
        true ?= owns_supply(Base, Signer, Opts),
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
%% `all' mode that is almost every data-free transaction on the network.
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
            tx(Owner, #{ <<"target">> => <<"elsewhere">> }),
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
    Alive = apply_tx(Spawned, tx(Owner, #{ <<"target">> => <<"elsewhere">> }), Opts),
    Moved = apply_tx(Alive, transfer_tx(Owner, StrangerAddr, 1), Opts),
    Later =
        apply_tx(Moved, tx(Stranger, #{ <<"target">> => <<"elsewhere">> }), Opts),
    ?assertEqual(0, held_by(Later, OwnerAddr, Opts)),
    ?assertEqual(1, held_by(Later, StrangerAddr, Opts)).

%% @doc A missing or invalid swap device cannot wedge the name.
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
            ?assertEqual(
                <<"no swap here">>,
                hb_ao:get(<<"greeting">>, value(Set, Opts), not_found, Opts)
            ),
            ?assertEqual(not_found, state(<<"orders">>, Set, not_found, Opts))
        end,
        [
            name_held_by(OwnerAddr),
            (name_held_by(OwnerAddr))#{ <<"swap-device">> => <<"nOt-A-dEvIcE@1.0">> }
        ]
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

%%% The permanent fixtures
%%%
%%% The driver that posted these stories spends real AR and does not belong in
%%% the test battery. It remains recoverable from commit `824816e7c'.
%%%
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
%% every data-free transaction on the network takes one -- while the id is the
%% message itself.
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
    ?assertMatch(
        {error, _},
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
    ?assertMatch(
        {error, _},
        at(Process, Cancelled, <<"orders/", First/binary, "/status">>, Opts)
    ),
    ?assertEqual({ok, 1}, at(Process, Cancelled, <<"balances/", Seller/binary>>, Opts)),
    % A registration that pays the fee in full is still refused: the order is
    % no longer open.
    LateRegister = Slot(?WITHDRAWN_LATE_REGISTER),
    ?assertMatch(
        {error, _},
        at(Process, LateRegister, <<"orders/", First/binary, "/status">>, Opts)
    ),
    ?assertMatch(
        {error, not_found},
        at(Process, LateRegister, <<"orders/", First/binary, "/buyer">>, Opts)
    ),
    % And a payment against it moves nothing, in either direction: the goods
    % are back with the seller and there was no bond to compensate anyone from.
    LatePayment = Slot(?WITHDRAWN_LATE_PAYMENT),
    ?assertMatch(
        {error, _},
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
    ?assertEqual(
        ?SALE_SELLER,
        hb_ao:get(<<"initial-holder">>, Loaded, not_found, Opts)
    ),
    ?assertEqual(
        <<"carrier@1.0">>,
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
    ?assertEqual(1, hb_util:int(Read([?BALANCES, ?SALE_BUYER]))),
    ?assertEqual(0, hb_util:int(Read([?BALANCES, ?SALE_SELLER]))),
    ?assertEqual(1, hb_util:int(Read(<<"total-supply">>))),
    ?assertEqual(not_found, Read([<<"orders">>, ?NAMED_ORDER, <<"creator">>])),
    ?assertEqual(<<"hello from the new owner">>, Read([?VALUE, <<"greeting">>])),
    ?assertEqual(<<"text/plain">>, Read([?VALUE, <<"content-type">>])).
