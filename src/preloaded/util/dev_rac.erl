%%% @doc `~rac@1.0' (Ratcheting Authenticated Channels) provides ordered,
%%% exactly-once, replay-resistant messaging between AO processes.
%%%
%%% It is often necessary for an AO process to consume a stream of authenticated
%%% messages -- events, or state updates -- from another process, applying each
%%% at most once and in a well-defined order. `~rac@1.0' addresses this by
%%% numbering messages rather than identifying them. A sender assigns each
%%% message a monotonic `slot' on a named `channel'; a recipient maintains a
%%% per-sender `ratchet' and admits a message only when its slot satisfies the
%%% channel's ingest rule, rejecting replays and mis-ordered delivery.
%%%
%%% == Channels, slots, and the ratchet ==
%%%
%%% A `channel' is a named, unidirectional stream from a sending process to a
%%% receiving process. Channels are independent and their slots begin at `0'.
%%% The sender records the highest slot it has emitted on each channel and
%%% stamps the next slot onto each outbound message. The recipient records the
%%% highest slot it has admitted from each sender on each channel -- the
%%% `ratchet' -- and advances it as messages are admitted.
%%%
%%% == State ==
%%%
%%% ```
%%% base/rac-outbound/<address>/<channel>:  Highest slot this process has sent
%%%     to <address> on <channel>. Absent => -1; the next slot emitted is 0.
%%% base/rac-inbound/<address>/<channel>:   Highest slot this process has
%%%     admitted from <address> on <channel> (the ratchet). Absent => -1.
%%% message/rac-slot:     The channel slot this message occupies.
%%% message/rac-channel:  The channel name. Absent => `default'.
%%% message/rac-ratchet:  The ingest rule (see `compute'). Absent => `false'.
%%% '''
%%%
%%% `<address>' is the counterparty's process identifier: on the sending side,
%%% the recipient's address; on the receiving side, the sender's address, taken
%%% from the inbound message's `from-process' field (established by `~push@1.0',
%%% and not settable by the sending process's own logic). `rac-outbound' and
%%% `rac-inbound' are the only state this device introduces.
%%%
%%% == Keys ==
%%%
%%% `send': Emit `body' to `recipient' on a channel, stamping it with the
%%% channel's next slot and appending it to the process outbox for delivery.
%%%
%%% ```
%%% send:
%%%     recipient:  Address    (required) destination process.
%%%     body:       Message    (required) the message to send.
%%%     channel?:   Binary     channel name (default `default').
%%%     ratchet?:   false | true | Integer  ingest rule to stamp on the message
%%%                                          (default `false'); see `compute'.
%%% '''
%%%
%%% `send' sets `rac-slot' to `rac-outbound/<recipient>/<channel> + 1' (a
%%% missing counter is `-1', so the first slot is `0'), stamps the outbound
%%% message with `rac-slot' and `target = recipient' (plus `rac-channel' /
%%% `rac-ratchet' when non-default), appends it to `results/outbox', and
%%% advances the outbound counter. Delivery, provenance (`from-process'), and
%%% re-signing to the recipient's policy are performed by `~push@1.0';
%%% `~rac@1.0' only stamps and enqueues.
%%%
%%% `compute': Admit or reject one inbound message according to its channel's
%%% ingest rule, advancing the ratchet on admission. Intended to run once per
%%% assigned message, ahead of application logic. It is `~multipass@1.0'-aware,
%%% mirroring `~dedup@1.0': it acts only on the first pass and returns the base
%%% unchanged on later passes. The inbound message is the request's `body', from
%%% which `compute' reads `rac-slot', `rac-channel' (default `default'),
%%% `rac-ratchet' (default `false'), and the sender (`from-process', or the
%%% message's first committer if absent). Untagged traffic (no `rac-slot')
%%% passes through unmodified. With `Ratchet = rac-inbound/<sender>/<channel>'
%%% (absent => -1) and `Slot = rac-slot', the message is admitted iff:
%%%
%%% ```
%%% rac-ratchet     Admit when                    Semantics
%%% false (default) Slot == Ratchet + 1           strict, in-order, exactly-once
%%% true            Slot > Ratchet                ratchet forward to any later slot
%%% integer N       Slot > Ratchet and Ratchet >= N   ratchet forward once at N
%%% '''
%%%
%%% On admission `compute' sets `rac-inbound/<sender>/<channel> = Slot' and
%%% returns `{ok, Base}'; on rejection it returns `{skip, Base}' -- state is
%%% unchanged and, in a stack, no subsequent device runs for the message.
%%%
%%% == Usage ==
%%%
%%% `~rac@1.0' may be placed in a process's execution stack ahead of the
%%% application device (a `{skip, ...}' from `compute' halts the stack for that
%%% message), or a custom execution device may resolve `compute' against
%%% `{as, ~rac@1.0, Base}' before its own logic, treating `skip' as rejection.
%%% Senders resolve `send' against `{as, ~rac@1.0, Base}' from within compute.
%%% `rac-ratchet = false' gives ordered, exactly-once event ingest; `true' or an
%%% integer gives last-writer-wins state synchronization that tolerates gaps.
-module(dev_rac).
-export([info/1, send/3, compute/3]).
-include("include/hb.hrl").

%% Keys that are structural rather than outbox entries, excluded when
%% numbering a new outbox slot.
-define(NON_ENTRY_KEYS,
    [<<"device">>, <<"priv">>, <<"commitments">>, <<"hashpath">>]).

%% @doc Expose the two channel verbs; every other key (including the
%% message-manipulation and stack-lifecycle keys) falls through to
%% `message@1.0'.
info(_Opts) ->
    #{ exports => [<<"send">>, <<"compute">>] }.

%% @doc Emit `body' to `recipient' on a channel, stamping the channel's next
%% slot and appending the message to the process outbox.
send(Base, Req, Opts) ->
    maybe
        {ok, Recipient} ?= required(<<"recipient">>, Req, Opts),
        {ok, Body} ?= required(<<"body">>, Req, Opts),
        Channel = hb_ao:get(<<"channel">>, Req, <<"default">>, Opts),
        Ratchet = hb_ao:get(<<"ratchet">>, Req, false, Opts),
        Slot =
            last_slot(<<"rac-outbound">>, Base, Recipient, Channel, Opts) + 1,
        Outbound = stamp(Body, Slot, Recipient, Channel, Ratchet),
        ?event(rac,
            {sent, {recipient, Recipient}, {channel, Channel}, {slot, Slot}}
        ),
        Sent = append_outbox(Base, Outbound, Opts),
        {ok, set_slot(<<"rac-outbound">>, Sent, Recipient, Channel, Slot, Opts)}
    end.

%% @doc Fetch a required request field, or a `rac-missing-<field>' error.
required(Key, Req, Opts) ->
    case hb_ao:get(Key, Req, not_found, Opts) of
        not_found -> {error, <<"rac-missing-", Key/binary>>};
        Value -> {ok, Value}
    end.

%% @doc Admit or reject the inbound message under its channel's ingest rule,
%% advancing the ratchet on admission. Runs on the first fold pass only, so
%% `~multipass@1.0' re-passes do not re-apply it.
compute(Base, Req, Opts) ->
    case is_first_pass(Base, Opts) of
        false -> {ok, Base};
        true -> ingest(Base, hb_ao:get(<<"body">>, Req, #{}, Opts), Opts)
    end.

%% @doc Admit or reject one inbound message. Untagged traffic (no `rac-slot')
%% passes through unchanged.
ingest(Base, In, Opts) ->
    case hb_maps:get(<<"rac-slot">>, In, not_found, Opts) of
        not_found -> {ok, Base};
        RawSlot -> admit(Base, In, hb_util:int(RawSlot), Opts)
    end.

%% @doc Read the channel's sender, rule, and ratchet, then admit or reject the
%% slot, advancing `rac-inbound' on admission.
admit(Base, In, Slot, Opts) ->
    Sender = sender(In, Opts),
    Channel = hb_maps:get(<<"rac-channel">>, In, <<"default">>, Opts),
    Rule = ratchet_rule(hb_maps:get(<<"rac-ratchet">>, In, false, Opts)),
    Ratchet = last_slot(<<"rac-inbound">>, Base, Sender, Channel, Opts),
    case admits(Rule, Slot, Ratchet) of
        true ->
            ?event(rac, {admit, {sender, Sender}, {slot, Slot}}),
            {ok,
                set_slot(<<"rac-inbound">>, Base, Sender, Channel, Slot, Opts)};
        false ->
            ?event(rac, {reject, {sender, Sender}, {slot, Slot}}),
            {skip, Base}
    end.

%%% Ingest rules.

%% @doc Normalize the on-message `rac-ratchet' value to an internal rule.
ratchet_rule(N) when is_integer(N) -> {at, N};
ratchet_rule(true) -> ratchet;
ratchet_rule(false) -> strict;
ratchet_rule(<<"true">>) -> ratchet;
ratchet_rule(<<"false">>) -> strict;
ratchet_rule(V) when is_binary(V) ->
    case is_integer_binary(V) of
        true -> {at, hb_util:int(V)};
        false -> strict
    end;
ratchet_rule(_) -> strict.

%% @doc Does `Slot' satisfy the channel's ingest rule against the ratchet?
admits(strict, Slot, Ratchet) -> Slot == Ratchet + 1;
admits(ratchet, Slot, Ratchet) -> Slot > Ratchet;
admits({at, N}, Slot, Ratchet) -> (Slot > Ratchet) andalso (Ratchet >= N).

%%% Message stamping.

%% @doc Stamp the outbound message with its slot and target, plus its channel
%% and ratchet rule when either differs from the default.
stamp(Body, Slot, Recipient, Channel, Ratchet) ->
    Tagged = Body#{ <<"rac-slot">> => Slot, <<"target">> => Recipient },
    WithChannel =
        case Channel of
            <<"default">> -> Tagged;
            _ -> Tagged#{ <<"rac-channel">> => Channel }
        end,
    case Ratchet of
        false -> WithChannel;
        <<"false">> -> WithChannel;
        _ -> WithChannel#{ <<"rac-ratchet">> => Ratchet }
    end.

%%% State access.

%% @doc Read a channel counter, defaulting to `-1' when absent.
last_slot(Dir, Base, Address, Channel, Opts) ->
    hb_util:int(
        hb_ao:get(
            [Dir, Address, Channel],
            {as, <<"message@1.0">>, Base},
            -1,
            Opts
        )
    ).

%% @doc Write a channel counter, replacing the `Dir' sub-map wholesale so the
%% nested update is not deep-merged with the prior value.
set_slot(Dir, Base, Address, Channel, Slot, Opts) ->
    DirMap = hb_ao:get(Dir, {as, <<"message@1.0">>, Base}, #{}, Opts),
    AddrMap = hb_maps:get(Address, DirMap, #{}, Opts),
    NewDirMap = DirMap#{ Address => AddrMap#{ Channel => Slot } },
    hb_ao:set(
        Base,
        #{ <<"set-mode">> => <<"explicit">>, Dir => NewDirMap },
        Opts
    ).

%% @doc Append an entry to `results/outbox' at the next free numeric index,
%% replacing `results' wholesale to avoid deep-merging the outbox map.
append_outbox(Base, Entry, Opts) ->
    Results = hb_ao:get(<<"results">>, {as, <<"message@1.0">>, Base}, #{}, Opts),
    Outbox = hb_maps:get(<<"outbox">>, Results, #{}, Opts),
    Key = hb_util:bin(entry_count(Outbox, Opts) + 1),
    NewResults = Results#{ <<"outbox">> => Outbox#{ Key => Entry } },
    hb_ao:set(
        Base,
        #{ <<"set-mode">> => <<"explicit">>, <<"results">> => NewResults },
        Opts
    ).

%% @doc Count outbox entries, ignoring structural keys.
entry_count(Map, Opts) when is_map(Map) ->
    Keys = hb_maps:keys(Map, Opts),
    length([ K || K <- Keys, not lists:member(K, ?NON_ENTRY_KEYS) ]);
entry_count(_, _) -> 0.

%% @doc The sender of an inbound message: its `from-process' provenance, or its
%% first committer when no provenance is present.
sender(In, Opts) ->
    case hb_maps:get(<<"from-process">>, In, not_found, Opts) of
        not_found -> first_committer(In, Opts);
        Process -> Process
    end.

%% @doc The address of the first committer of a message, or `anonymous' when it
%% carries no commitments.
first_committer(In, Opts) ->
    case hb_message:signers(In, Opts) of
        [Address | _] -> Address;
        _ -> <<"anonymous">>
    end.

%% @doc Is this the first fold pass? Later passes (driven by `~multipass@1.0')
%% must not re-apply the ratchet.
is_first_pass(Base, Opts) ->
    Pass = hb_ao:get(<<"pass">>, {as, <<"message@1.0">>, Base}, 1, Opts),
    hb_util:int(Pass) == 1.

%% @doc Is a binary a base-10 integer literal (optionally negative)?
is_integer_binary(<<"-", Rest/binary>>) when Rest =/= <<>> ->
    is_integer_binary(Rest);
is_integer_binary(Bin) when is_binary(Bin), Bin =/= <<>> ->
    lists:all(fun(C) -> C >= $0 andalso C =< $9 end, binary_to_list(Bin));
is_integer_binary(_) ->
    false.

%%%---------------------------------------------------------------------------
%%% Tests
%%%---------------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

opts() -> #{}.

%% @doc A base state bound to `~rac@1.0'.
base() -> #{ <<"device">> => <<"rac@1.0">> }.

%% @doc An inbound assignment request carrying a message body.
inbound(Fields) -> #{ <<"body">> => Fields }.

%% @doc An inbound message on the default channel from `Sender' at `Slot'.
in(Sender, Slot) ->
    inbound(#{ <<"from-process">> => Sender, <<"rac-slot">> => Slot }).

%% @doc As `in/2', with an explicit ratchet rule.
in(Sender, Slot, Ratchet) ->
    inbound(#{
        <<"from-process">> => Sender,
        <<"rac-slot">> => Slot,
        <<"rac-ratchet">> => Ratchet
    }).

%% Item 1: successive sends stamp slots 0,1,2 and advance the counter.
send_stamps_and_advances_test() ->
    O = opts(),
    R = <<"recipient-addr">>,
    {ok, S1} = send(base(), #{ <<"recipient">> => R, <<"body">> => #{ <<"n">> => 1 } }, O),
    ?assertEqual(0, hb_ao:get(<<"results/outbox/1/rac-slot">>, S1, O)),
    ?assertEqual(0, last_slot(<<"rac-outbound">>, S1, R, <<"default">>, O)),
    {ok, S2} = send(S1, #{ <<"recipient">> => R, <<"body">> => #{ <<"n">> => 2 } }, O),
    ?assertEqual(1, hb_ao:get(<<"results/outbox/2/rac-slot">>, S2, O)),
    ?assertEqual(1, last_slot(<<"rac-outbound">>, S2, R, <<"default">>, O)),
    ?assertEqual(R, hb_ao:get(<<"results/outbox/2/target">>, S2, O)).

%% Item 2: channels and recipients keep independent counters.
send_channels_independent_test() ->
    O = opts(),
    {ok, S1} =
        send(base(),
            #{ <<"recipient">> => <<"a">>, <<"body">> => #{} }, O),
    {ok, S2} =
        send(S1,
            #{ <<"recipient">> => <<"a">>, <<"channel">> => <<"c2">>,
               <<"body">> => #{} }, O),
    {ok, S3} =
        send(S2,
            #{ <<"recipient">> => <<"b">>, <<"body">> => #{} }, O),
    ?assertEqual(0, last_slot(<<"rac-outbound">>, S3, <<"a">>, <<"default">>, O)),
    ?assertEqual(0, last_slot(<<"rac-outbound">>, S3, <<"a">>, <<"c2">>, O)),
    ?assertEqual(0, last_slot(<<"rac-outbound">>, S3, <<"b">>, <<"default">>, O)),
    % The `c2' entry carries the channel tag; the default one does not.
    ?assertEqual(<<"c2">>, hb_ao:get(<<"results/outbox/2/rac-channel">>, S2, O)),
    ?assertEqual(not_found,
        hb_ao:get(<<"results/outbox/1/rac-channel">>, S1, not_found, O)).

%% Item 3 + 5: ordered ingest admits 0,1,2; a gap is rejected then recovered.
compute_ordered_and_gap_test() ->
    O = opts(),
    S = <<"sender-addr">>,
    {ok, B1} = compute(base(), in(S, 0), O),
    ?assertEqual(0, last_slot(<<"rac-inbound">>, B1, S, <<"default">>, O)),
    {ok, B2} = compute(B1, in(S, 1), O),
    ?assertEqual(1, last_slot(<<"rac-inbound">>, B2, S, <<"default">>, O)),
    % Gap: slot 3 while ratchet is at 1 -> skip, unchanged.
    ?assertEqual({skip, B2}, compute(B2, in(S, 3), O)),
    % Recover in order.
    {ok, B3} = compute(B2, in(S, 2), O),
    {ok, B4} = compute(B3, in(S, 3), O),
    ?assertEqual(3, last_slot(<<"rac-inbound">>, B4, S, <<"default">>, O)).

%% Item 4: a replay of an admitted slot is rejected; state unchanged.
compute_replay_rejected_test() ->
    O = opts(),
    S = <<"sender-addr">>,
    {ok, B1} = compute(base(), in(S, 0), O),
    ?assertEqual({skip, B1}, compute(B1, in(S, 0), O)).

%% Item 6: `ratchet=true' jumps to any later slot; lower slots then rejected.
compute_ratchet_jump_test() ->
    O = opts(),
    S = <<"sender-addr">>,
    {ok, B1} = compute(base(), in(S, 5, true), O),
    ?assertEqual(5, last_slot(<<"rac-inbound">>, B1, S, <<"default">>, O)),
    ?assertMatch({skip, _}, compute(B1, in(S, 4, true), O)).

%% Item 7: integer rule waits until the ratchet reaches N, then jumps.
compute_conditional_ratchet_test() ->
    O = opts(),
    S = <<"sender-addr">>,
    % Advance the ratchet 0..5 strictly.
    B5 =
        lists:foldl(
            fun(Slot, Acc) ->
                {ok, Next} = compute(Acc, in(S, Slot), O),
                Next
            end,
            base(),
            lists:seq(0, 5)
        ),
    % Ratchet is at 5: slot 8 with rule 6 is rejected.
    ?assertMatch({skip, _}, compute(B5, in(S, 8, 6), O)),
    % Reach slot 6, then the jump is admitted.
    {ok, B6} = compute(B5, in(S, 6), O),
    {ok, B8} = compute(B6, in(S, 8, 6), O),
    ?assertEqual(8, last_slot(<<"rac-inbound">>, B8, S, <<"default">>, O)),
    % Slot 7 is now never admissible.
    ?assertMatch({skip, _}, compute(B8, in(S, 7, 6), O)).

%% Item 8: two senders ratchet independently on the same channel.
compute_multi_sender_test() ->
    O = opts(),
    A = <<"sender-a">>, Bd = <<"sender-b">>,
    {ok, S1} = compute(base(), in(A, 0), O),
    {ok, S2} = compute(S1, in(Bd, 0), O),
    {ok, S3} = compute(S2, in(A, 1), O),
    ?assertEqual(1, last_slot(<<"rac-inbound">>, S3, A, <<"default">>, O)),
    ?assertEqual(0, last_slot(<<"rac-inbound">>, S3, Bd, <<"default">>, O)).

%% Item 9: later passes are a no-op (multipass-aware).
compute_first_pass_only_test() ->
    O = opts(),
    S = <<"sender-addr">>,
    Base2 = (base())#{ <<"pass">> => 2 },
    ?assertEqual({ok, Base2}, compute(Base2, in(S, 0), O)).

%% Item 10: untagged traffic passes through unmodified.
compute_untagged_passthrough_test() ->
    O = opts(),
    B0 = base(),
    ?assertEqual({ok, B0}, compute(B0, inbound(#{ <<"from-process">> => <<"s">> }), O)),
    ?assertEqual({ok, B0}, compute(B0, #{ <<"path">> => <<"compute">> }, O)).

%% send with no recipient / body is an error.
send_requires_fields_test() ->
    O = opts(),
    ?assertEqual({error, <<"rac-missing-recipient">>},
        send(base(), #{ <<"body">> => #{} }, O)),
    ?assertEqual({error, <<"rac-missing-body">>},
        send(base(), #{ <<"recipient">> => <<"r">> }, O)).

%%% Integration tests.

%% @doc A minimal application device that records the `rac-slot' of every
%% message it is folded over. Placed after `~rac@1.0' in a stack, it therefore
%% only records admitted messages -- a rejected message halts the stack first.
recorder_device() ->
    Record =
        fun(M1, M2) ->
            Slot = hb_ao:get(<<"body/rac-slot">>, M2, <<"?">>, #{}),
            Log = maps:get(<<"applied-log">>, M1, <<>>),
            Entry = <<Log/binary, (hb_util:bin(Slot))/binary, ",">>,
            {ok, M1#{ <<"applied-log">> => Entry }}
        end,
    #{ compute => Record }.

%% `~stack@1.0': rac gates an application device via the skip protocol.
%% Admitted messages reach the recorder; replays and gaps are skipped.
stack_gating_test() ->
    hb:init(),
    Stack =
        #{ <<"device">> => <<"stack@1.0">>,
           <<"device-stack">> =>
               #{ <<"1">> => <<"rac@1.0">>, <<"2">> => recorder_device() } },
    In =
        fun(Slot) ->
            #{ <<"path">> => <<"compute">>,
               <<"body">> =>
                   #{ <<"from-process">> => <<"S">>, <<"rac-slot">> => Slot } }
        end,
    {ok, R0} = hb_ao:resolve(Stack, In(0), #{}),   % admit 0
    {ok, R1} = hb_ao:resolve(R0, In(0), #{}),       % replay -> skip
    {ok, R2} = hb_ao:resolve(R1, In(1), #{}),       % admit 1
    {ok, R3} = hb_ao:resolve(R2, In(3), #{}),       % gap -> skip
    {ok, R4} = hb_ao:resolve(R3, In(2), #{}),       % admit 2
    ?assertEqual(<<"0,1,2,">>,
        hb_ao:get(<<"applied-log">>, {as, <<"message@1.0">>, R4}, <<"?">>, #{})),
    ?assertEqual(2,
        hb_util:int(
            hb_ao:get(
                [<<"rac-inbound">>, <<"S">>, <<"default">>],
                {as, <<"message@1.0">>, R4},
                -1,
                #{}
            )
        )
    ).

%% `~process@1.0' + `~lua@5.3a': a process whose Lua handler drives `send' and
%% `compute' directly via `ao.resolve({"as", "rac@1.0", ...})'. The scheduled
%% stream mixes admissible, replayed, gapped, and outbound messages.
lua_process_test_() ->
    {timeout, 60, fun lua_process/0}.
lua_process() ->
    hb:init(),
    W = ar_wallet:new(),
    Opts =
        #{ <<"store">> => hb_test_utils:test_store(hb_store_lmdb),
           <<"priv-wallet">> => W },
    Addr = hb_util:human_id(ar_wallet:to_address(W)),
    {ok, Module} = file:read_file("test/rac.lua"),
    Proc =
        hb_message:commit(
            #{ <<"device">> => <<"process@1.0">>,
               <<"type">> => <<"Process">>,
               <<"scheduler-device">> => <<"scheduler@1.0">>,
               <<"scheduler">> => Addr,
               <<"scheduler-location">> => Addr,
               <<"execution-device">> => <<"lua@5.3a">>,
               <<"authority">> => Addr,
               <<"module">> =>
                   #{ <<"content-type">> => <<"application/lua">>,
                      <<"body">> => Module } },
            Opts
        ),
    {ok, _} = hb_cache:write(Proc, Opts),
    ProcID = hb_message:id(Proc, all),
    Sender = <<"channel-sender">>,
    Recipient = <<"channel-recipient">>,
    Ingest =
        fun(Slot) ->
            #{ <<"from-process">> => Sender, <<"rac-slot">> => Slot }
        end,
    Messages =
        [ Ingest(0),                       % admit  -> count 1, ratchet 0
          Ingest(0),                       % replay -> reject
          Ingest(1),                       % admit  -> count 2, ratchet 1
          Ingest(3),                       % gap    -> reject
          Ingest(2),                       % admit  -> count 3, ratchet 2
          #{ <<"action">> => <<"send">>,
             <<"recipient">> => Recipient,
             <<"note">> => <<"hi">> } ],
    lists:foreach(
        fun(M) -> schedule_message(Proc, ProcID, M, Opts) end,
        Messages
    ),
    {ok, Final} = hb_ao:resolve(Proc, <<"now">>, Opts),
    M = {as, <<"message@1.0">>, Final},
    ?assertEqual(3, hb_util:int(hb_ao:get(<<"applied-count">>, M, 0, Opts))),
    ?assertEqual(2, counter(<<"rac-inbound">>, Sender, M, Opts)),
    ?assertEqual(0, counter(<<"rac-outbound">>, Recipient, M, Opts)),
    ?assertEqual(0,
        hb_util:int(hb_ao:get(<<"results/outbox/1/rac-slot">>, M, -1, Opts))).

%% @doc Read a default-channel counter for `Party' from a resolved state.
counter(Dir, Party, M, Opts) ->
    Path = <<Dir/binary, "/", Party/binary, "/default">>,
    hb_util:int(hb_ao:get(Path, M, -1, Opts)).

%% @doc Schedule a signed message onto a process (double-commit envelope).
schedule_message(Proc, ProcID, MsgBase, Opts) ->
    Req =
        hb_message:commit(
            #{ <<"path">> => <<"schedule">>,
               <<"method">> => <<"POST">>,
               <<"body">> =>
                   hb_message:commit(
                       MsgBase#{
                           <<"target">> => ProcID,
                           <<"type">> => <<"Message">>,
                           <<"random-seed">> => rand:uniform(1000000)
                       },
                       Opts) },
            Opts
        ),
    {ok, _} = hb_ao:resolve(Proc, Req, Opts).

-endif.
