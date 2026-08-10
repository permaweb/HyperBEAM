%%% @doc `~spectrum@1.0' offers a shared namespace for the permaweb, with
%%% names purchasable in AR and (later) AO tokens. It is intended for use in
%%% an AO node's `~name@1.0' `resolvers' stack.
%%%
%%% == Schema ==
%%%
%%% === `set' and `keys' ===
%%%
%%% Default to the `~message@1.0' device.
%%%
%%% === GET `/[key] { date? }' ===
%%%
%%% The default resolver for keys not explicitly implemented by the device
%%% with custom functionality. Returns `GET /names/[key]/value' where the
%%% Arweave block at `date' is less than `GET /names/[key]/deadline'. If the
%%% Arweave block at `date' is before `grace' but after the `deadline', then
%%% the `Base/grace-notice' message ID is returned in place of the `value'.
%%%
%%% If `Request/load' is true, the device also attempts to dereference the
%%% resolved `/names/[key]/value'.
%%%
%%% === GET `/blocks=Quantity { name, token: ar | ao }' ===
%%%
%%% Calculate the number of blocks a `Quantity' of `Token' tokens would buy
%%% given the system `Base' message. The number of acquired blocks will be
%%% proportionate to both the current name duration (if it is registered at
%%% all), as well as the number of characters in the `Name'.
%%%
%%% === GET `/price=QuantityBlocks { name, token: ar | ao }' ===
%%%
%%% The inverse of the `/blocks' key: given a specific target number of
%%% `QuantityBlocks' to buy, how many `Token' units must we
%%% `/compute/{ path: purchase }' with?
%%%
%%% === GET `/compute' ===
%%%
%%% ...with `body/purchase{ name, value?, data: null, commitments: { _ =&gt;
%%% device: tx@1.0, field-reward, field-target: null } }'
%%%
%%% An Arweave name purchase has been made, with the transaction's `reward'
%%% having been transferred to the endowment and miners.
%%%
%%% First, we must calculate the number of blocks that the name's `deadline'
%%% should be increased by given the reward deposited and its type (only AR to
%%% start with), using `GET /blocks'. Then, we must update its entry in the
%%% registry under the given key with the following form:
%%% <pre>
%%% {
%%%     deadline: (NewDeadline =
%%%         (GET Base/names/[Name]/deadline or CurrentBlockHeight)
%%%             + PurchasedBlocks),
%%%     grace: ((GET Base/names/[Name]/grace or CurrentBlockHeight)
%%%         + ((1 + Base/grace-factor) * PurchasedBlocks)),
%%%     value: (GET Base/names/[Name]/value or Value)
%%% }
%%% </pre>
%%%
%%% Notably, purchasing further blocks for a name does not automatically
%%% entitle one to set its `value'. `value's may only be set for names that
%%% are not already in the name table, or whose `grace' block height has
%%% already passed.
%%%
%%% == Pricing ==
%%%
%%% Flat, for now: `price-per-year' winston buys `blocks-per-year' blocks,
%%% defaulting to a tenth of an AR per year. `flat/5' is the whole curve, and
%%% is handed the name and the lease's remaining duration as well as the
%%% payment, so that a rate varying with either replaces it without touching a
%%% caller. A process may instead name a `pricing-device', asked in its place.
%%% It may return a scalar quote, or a quote carrying opaque `pricing'
%%% metadata that this device retains on the lease.
%%%
%%% == Running one ==
%%%
%%% The scheduler of a `~spectrum@1.0' process is `~arweave-scheduler@1.0', in
%%% its `all' mode -- the only mode that puts a `block-height' on an
%%% assignment, and so the only one in which the deadlines above have a clock:
%%% <pre>
%%%     scheduler-device: arweave-scheduler@1.0
%%%     scheduler-mode:   all
%%%     execution-device: spectrum@1.0
%%%     grace-notice:     &lt;message id&gt;
%%% </pre>
%%% To serve names from it, put the running process in a node's resolver
%%% stack, so that `GET /&lt;name&gt;.&lt;node&gt;/' resolves through it:
%%% <pre>
%%%     name-resolvers: [ "&lt;process&gt;~process@1.0/now/~spectrum@1.0" ]
%%% </pre>
%%%
%%% == Initial namespace ==
%%%
%%% `initial-namespace' may name a cached message containing `model' and
%%% `names'. Each name record must carry its `value' and retained `pricing'
%%% metadata. At init, the records are issued for `initial-nametime' blocks
%%% from `spectrum-height'; their grace uses the process's `grace-factor'.
%%% <pre>
%%%     model: &lt;markov model&gt;
%%%     names:
%%%       &lt;name&gt;:
%%%         value: &lt;resolver target&gt;
%%%         pricing:
%%%           weight: &lt;positive exact-name probability&gt;
%%% </pre>
-module(dev_spectrum).
-implements(<<"spectrum@1.0">>).
-device_libraries([lib_scheduler]).
%%% AO-Core API functions:
-export([info/0, compute/3, init/3, snapshot/3, normalize/3]).
%%% Pricing functions:
-export([blocks/3, price/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The registry of leases, keyed by name.
-define(NAMES, <<"names">>).
%%% The arweave-scheduler cache namespace.
-define(ARWEAVE_SCHEDULER_CACHE, <<"~arweave-scheduler@1.0">>).
%%% The height of the last assignment applied. The only clock a compute has: a
%%% slot that fetched the chain tip would fork the process, and one that
%%% compared against it would decide differently on different nodes. A read may
%%% name a height or a date of its own; see `at/3'.
-define(HEIGHT, <<"spectrum-height">>).
%%% A prepared namespace state to import when the process initializes.
-define(INITIAL_NAMESPACE, <<"initial-namespace">>).
%%% The number of blocks granted to every imported name.
-define(INITIAL_NAMETIME, <<"initial-nametime">>).
%%% Winston per name per year, when the process does not say. A tenth of an AR.
-define(DEFAULT_PRICE_PER_YEAR, 100000000000).
%%% Blocks per year, at Arweave's two-minute block target.
-define(DEFAULT_BLOCKS_PER_YEAR, 262800).
%%% The share of a purchase's blocks that follow its deadline as grace, in
%%% basis points: a ratio has to be an integer to be computed identically on
%%% every node, and basis points are how this codebase writes one.
-define(DEFAULT_GRACE_FACTOR, 1000).
-define(GRACE_BASIS, 10000).
%% @doc Resolve any key that is not one of this device's own through the
%% registry, as a name.
%%
%% `keys' and `set' go to `~message@1.0', which is what lets the state be read
%% and written as a message. That is safe here and would not be under
%% `target'-mode sequencing: in `all' mode `~arweave-scheduler@1.0' overwrites
%% every assignment's `path' with `compute', so a stranger's transaction
%% cannot select a device key however it is tagged.
%%
%% `info' is arity 0 deliberately. A device's `info' is always exported, so an
%% `info/1' would answer the `info' key with this map -- and `info' is a name
%% somebody may buy.
info() ->
    #{
        default => fun get/4,
        excludes => [<<"keys">>, <<"set">>]
    }.

%% @doc Import a prepared namespace the first time the process initializes.
init(Base, _Req, Opts) -> initial_namespace(Base, Opts).
snapshot(Base, _Req, _Opts) -> {ok, Base}.
normalize(Base, _Req, _Opts) -> {ok, Base}.

%%% Reading names

%% @doc Resolve a name to what it carries, if it is live at the moment asked
%% about.
%%
%% Between a name's `deadline' and its `grace' it resolves to the registry's
%% `grace-notice' instead of to its value: the lease has run out, but saying
%% so is more use to a reader than saying nothing, and it is the window in
%% which the holder can still put it right. Past the grace, and before ever
%% being bought, a name is simply not found -- which is also what lets
%% `~name@1.0' carry on to the next resolver in its stack.
get(Key, Base, Req, Opts) ->
    maybe
        {ok, Height} ?= at(Base, Req, Opts),
        {ok, ReadBase} ?= registry_at(Base, Req, Height, Opts),
        {ok, Record} ?= find_name(Key, ReadBase, Opts),
        case standing(Record, Height, Opts) of
            live ->
                maybe_load(
                    hb_maps:get(<<"value">>, Record, <<>>, Opts),
                    Req,
                    Opts
                );
            grace -> notice(ReadBase, Req, Opts);
            expired -> {error, not_found}
        end
    else
        not_found -> {error, not_found};
        {error, not_found} -> {error, not_found}
    end.

%% @doc Where a lease stands against a height: still running, in the window
%% after its deadline in which its holder may still put it right, or gone.
%% This is the only place the clock is interpreted, and both reading a name
%% and buying one ask it.
standing(Record, Height, Opts) ->
    Deadline = number(<<"deadline">>, Record, 0, Opts),
    Grace = number(<<"grace">>, Record, Deadline, Opts),
    case Height of
        At when At < Deadline -> live;
        At when At < Grace -> grace;
        _ -> expired
    end.

%% @doc The message a name in its grace window resolves to in place of its
%% value. A registry that does not carry one says nothing rather than
%% pretending the lease still runs.
notice(Base, Req, Opts) ->
    case state(<<"grace-notice">>, Base, not_found, Opts) of
        not_found -> {error, not_found};
        Notice -> maybe_load(Notice, Req, Opts)
    end.

%% @doc Dereference a name's value if the reader asked for it. Unasked, the
%% value is returned as it is held: `~name@1.0' loads what its resolvers hand
%% back, so loading here as well would be doing it twice.
maybe_load(Value, Req, Opts) ->
    case hb_util:bool(field(<<"load">>, Req, false, Opts)) of
        true when ?IS_ID(Value) -> hb_cache:read(Value, Opts);
        true -> {ok, hb_cache:ensure_loaded(Value, Opts)};
        false -> {ok, Value}
    end.

%% @doc The height a read is asking about.
%%
%% `height' names a block directly; `date' names a moment, which is turned
%% into the height of the last block mined at or before it. Neither is
%% consulted during a compute -- a slot's answer is cached forever, so it may
%% only ever depend on what the schedule itself delivered.
at(Base, Req, Opts) ->
    case field(<<"height">>, Req, not_found, Opts) of
        not_found -> dated(field(<<"date">>, Req, not_found, Opts), Base, Opts);
        Height -> found(hb_util:safe_int(Height))
    end.

dated(not_found, Base, Opts) -> {ok, height(Base, Opts)};
dated(Date, _Base, Opts) -> found(height_at(Date, Opts)).

%% @doc Read from process history when a placed height names chain state the
%% process has already passed.
registry_at(Base, Req, Height, Opts) ->
    case historical_read(Req, Opts) andalso Height < height(Base, Opts) of
        true ->
            case process_message(Base, Opts) of
                {ok, Process, ProcID} ->
                    process_at_height(
                        Process,
                        ProcID,
                        Base,
                        Height,
                        Opts
                    );
                not_process -> {ok, Base};
                Error -> Error
            end;
        false -> {ok, Base}
    end.

%% @doc Return whether a request names an explicit historical moment.
historical_read(Req, Opts) ->
    field(<<"height">>, Req, not_found, Opts) =/= not_found orelse
        field(<<"date">>, Req, not_found, Opts) =/= not_found.

%% @doc Read the computed process state at the end of an Arweave block height.
process_at_height(Process, ProcID, Base, Height, Opts) ->
    maybe
        {ok, CurrentSlot} ?= current_slot(Base, Opts),
        {ok, Slot} ?= slot_at_height(ProcID, Height, CurrentSlot, Opts),
        {ok, State} ?= hb_ao:resolve(
            Process,
            #{ <<"path">> => <<"compute">>, <<"slot">> => Slot },
            Opts#{ <<"hashpath">> => ignore }
        ),
        {ok, State}
    else
        _ -> not_found
    end.

%% @doc Read the process message that owns a computed registry state.
process_message(Base, Opts) ->
    case state(<<"process">>, Base, not_found, Opts) of
        not_found -> not_process;
        Process ->
            try
                Loaded = hb_cache:ensure_loaded(Process, Opts),
                ID =
                    hb_util:human_id(
                        hb_message:id(Loaded, signed, Opts)
                    ),
                {ok, Loaded, ID}
            catch _:_ -> not_found
            end
    end.

%% @doc Read the slot a process-shaped registry state has reached.
current_slot(Base, Opts) ->
    case hb_util:safe_int(state(<<"at-slot">>, Base, not_found, Opts)) of
        {ok, Slot} when Slot >= 0 -> {ok, Slot};
        _ -> {error, not_found}
    end.

%% @doc Find the last cached scheduler slot whose block height is at most
%% `Height'.
slot_at_height(ProcID, Height, CurrentSlot, Opts) ->
    case assignment_height(ProcID, 0, Opts) of
        {ok, SpawnHeight} when Height < SpawnHeight -> {error, not_found};
        {ok, _SpawnHeight} ->
            find_slot_at_height(
                ProcID,
                Height,
                1,
                CurrentSlot,
                0,
                Opts
            );
        _ -> {error, not_found}
    end.

find_slot_at_height(_ProcID, _Height, Low, High, Best, _Opts) when Low > High ->
    {ok, Best};
find_slot_at_height(ProcID, Height, Low, High, Best, Opts) ->
    Slot = (Low + High) div 2,
    case assignment_height(ProcID, Slot, Opts) of
        {ok, At} when At =< Height ->
            find_slot_at_height(ProcID, Height, Slot + 1, High, Slot, Opts);
        {ok, _At} ->
            find_slot_at_height(ProcID, Height, Low, Slot - 1, Best, Opts);
        _ ->
            {error, not_found}
    end.

%% @doc Read the Arweave block height that sequenced one scheduler slot.
assignment_height(ProcID, Slot, Opts) ->
    case lib_scheduler:read_assignment(
        ?ARWEAVE_SCHEDULER_CACHE,
        ProcID,
        Slot,
        Opts
    ) of
        {ok, Assignment} ->
            case hb_util:safe_int(
                field(<<"block-height">>, Assignment, not_found, Opts)
            ) of
                {ok, Height} -> {ok, Height};
                _ -> {error, not_found}
            end;
        _ -> {error, not_found}
    end.

%% @doc A moment the device cannot place is not the moment it would have used
%% had nobody named one. A reader who asks about a date is answered about that
%% date or not at all: answering at the registry's own height instead would be
%% indistinguishable from a live name, and a lapsed lease would read as good.
found({ok, Height}) -> {ok, Height};
found(_Other) -> not_found.

%% @doc The height of the last block mined at or before a moment, found by
%% bisection over the weave. Blocks carry their timestamp and never change, so
%% the search is stable and every step of it is cached.
%%
%% A probe that cannot be fetched ends the search rather than narrowing it. The
%% bound reached so far is a real lower bound on nothing: it is where the
%% search happened to be, and answering with it would put an arbitrarily early
%% height against a lease -- which is to say, would report an expired name as
%% live.
height_at(Date, Opts) ->
    maybe
        {ok, Seconds} ?= seconds(Date),
        {ok, Tip} ?= tip(Opts),
        bisect(0, Tip, Seconds, Opts)
    end.

bisect(Low, High, _Seconds, _Opts) when Low >= High -> {ok, Low};
bisect(Low, High, Seconds, Opts) ->
    Mid = (Low + High + 1) div 2,
    maybe
        {ok, At} ?= timestamp(Mid, Opts),
        case At =< Seconds of
            true -> bisect(Mid, High, Seconds, Opts);
            false -> bisect(Low, Mid - 1, Seconds, Opts)
        end
    end.

timestamp(Height, Opts) ->
    block(#{ <<"block">> => hb_util:bin(Height) }, <<"timestamp">>, Opts).

tip(Opts) -> block(#{ <<"block">> => <<"current">> }, <<"height">>, Opts).

block(Req, Key, Opts) ->
    maybe
        {ok, Block} ?=
            hb_ao:resolve(
                #{ <<"device">> => <<"arweave@2.9">> },
                Req#{ <<"path">> => <<"block">> },
                Opts
            ),
        hb_util:safe_int(hb_ao:get(Key, Block, not_found, Opts))
    end.

%% @doc A moment, given either as unix seconds or as a date.
seconds(Date) ->
    case hb_util:safe_int(Date) of
        {ok, Seconds} -> {ok, Seconds};
        _ -> calendar_seconds(Date)
    end.

calendar_seconds(Date) when is_binary(Date) ->
    % A bare date is its first moment, UTC.
    Stamp =
        case binary:match(Date, <<"T">>) of
            nomatch -> <<Date/binary, "T00:00:00Z">>;
            _ -> Date
        end,
    try
        {ok,
            calendar:rfc3339_to_system_time(
                hb_util:list(Stamp),
                [{unit, second}]
            )
        }
    catch
        _:_ -> {error, invalid}
    end;
calendar_seconds(_Date) -> {error, invalid}.

%%% Pricing
%%%
%%% `blocks' and `price' invert each other up to rounding: what a payment buys,
%%% and what buying costs. Both are asked through `quote/5', which is handed the
%%% whole request -- the `name', the `token' it is paid in, and the `duration'
%%% the name still has to run. The flat curve below reads only the `token', and
%%% only to refuse anything that is not AR. The rate the spec ultimately wants
%%% varies with the length of a name and with how long it is already held, and
%%% a curve that could not see them would have to change every caller as well
%%% as itself.

%% @doc How many blocks of registration a payment buys.
%% `GET /blocks=&lt;winston&gt;&amp;name=&lt;name&gt;&amp;token=ar'
blocks(Base, Req, Opts) ->
    maybe
        {ok, Payment} ?= amount(<<"blocks">>, Req, Opts),
        {ok, Bought, _Pricing} ?=
            quote(Base, <<"blocks">>, Payment, priced(Base, Req, Opts), Opts),
        {ok, Bought}
    end.

%% @doc What a number of blocks of registration costs.
%% `GET /price=&lt;blocks&gt;&amp;name=&lt;name&gt;&amp;token=ar'
price(Base, Req, Opts) ->
    maybe
        {ok, Blocks} ?= amount(<<"price">>, Req, Opts),
        {ok, Cost, _Pricing} ?=
            quote(Base, <<"price">>, Blocks, priced(Base, Req, Opts), Opts),
        {ok, Cost}
    end.

%% @doc Tell the curve how long the name it is being asked about still has to
%% run, which a reader has no way to state and no reason to be trusted about.
priced(Base, Req, Opts) ->
    Height = height(Base, Opts),
    Req#{
        <<"duration">> =>
            remaining(field(<<"name">>, Req, <<>>, Opts), Base, Height, Opts)
    }.

%% @doc Ask the curve. A process that names a `pricing-device' is asked
%% through it, exactly as `dev_carrier' asks its `swap-device' -- and for the
%% same reason it is one scalar key rather than a `~stack@1.0': a process
%% spawned as an Arweave transaction can carry only flat tags.
%% A structured answer may carry opaque metadata to retain on the lease.
quote(Base, Key, Given, Req, Opts) ->
    case state(<<"pricing-device">>, Base, not_found, Opts) of
        not_found -> quoted(flat(Base, Key, Given, Req, Opts));
        Device -> delegated(Base, Device, Key, Given, Req, Opts)
    end.

%% @doc Normalize a scalar quote to the internal response form.
quoted({ok, Answer}) -> {ok, Answer, #{}};
quoted(Error) -> Error.

%% @doc Ask a configured pricing device for a scalar or structured quote.
delegated(Base, Device, Key, Given, Req, Opts) ->
    case
        hb_ao:resolve(
            Base#{ <<"device">> => Device },
            Req#{ <<"path">> => Key, Key => hb_util:bin(Given) },
            Opts
        )
    of
        {ok, Answer} when is_map(Answer) -> structured_quote(Key, Answer, Opts);
        {ok, Answer} -> quoted(hb_util:safe_int(Answer));
        _ -> {error, invalid}
    end.

%% @doc Read a quote and opaque lease metadata from a pricing response.
structured_quote(Key, Answer, Opts) ->
    maybe
        {ok, Quoted} ?=
            hb_util:safe_int(hb_maps:get(Key, Answer, not_found, Opts)),
        Pricing = hb_maps:get(<<"pricing">>, Answer, #{}, Opts),
        true ?= is_map(Pricing),
        {ok, Quoted, Pricing}
    else
        _ -> {error, invalid}
    end.

%% @doc The curve as it stands: a flat rate per name per year, in AR only.
flat(Base, Key, Given, Req, Opts) ->
    Price = setting(<<"price-per-year">>, Base, ?DEFAULT_PRICE_PER_YEAR, Opts),
    Year = setting(<<"blocks-per-year">>, Base, ?DEFAULT_BLOCKS_PER_YEAR, Opts),
    Token = word(<<"token">>, Req, <<"ar">>, Opts),
    case {Token, Key} of
        {<<"ar">>, _} when Price =< 0; Year =< 0; Given < 0 -> {error, invalid};
        % What a payment buys rounds down and what blocks cost rounds up, so
        % that neither direction can be rounded into free registration.
        {<<"ar">>, <<"blocks">>} -> {ok, (Given * Year) div Price};
        {<<"ar">>, <<"price">>} -> {ok, ((Given * Price) + Year - 1) div Year};
        _ -> {error, invalid}
    end.

%% @doc Read a number the process was spawned with. Every key of a process
%% message is a tag on the transaction that spawned it, so one that is not a
%% number falls back to what this device would have used anyway rather than
%% failing every slot that reads it.
setting(Key, Base, Default, Opts) ->
    hb_util:ok_or(hb_util:safe_int(state(Key, Base, Default, Opts)), Default).

%%% Buying names

%% @doc Apply one assignment to the registry.
%%
%% In `all' mode almost every slot is unrelated network traffic, so the common
%% path is the clock and a single comparison against a tag. Nothing walks the
%% registry: a lease expires by the passage of the height it is read against,
%% not by anything a slot does to it.
compute(Base, Assignment, Opts) ->
    Height = hb_util:int(field(<<"block-height">>, Assignment, 0, Opts)),
    Body = field(<<"body">>, Assignment, #{}, Opts),
    Advanced = Base#{ ?HEIGHT => Height },
    % `all' mode overwrites every assignment's `path' with `compute', so a
    % transaction's own `path' arrives here as an ordinary tag.
    case word(<<"path">>, Body, <<>>, Opts) of
        <<"purchase">> -> {ok, purchase(Advanced, Body, Height, Opts)};
        _ -> {ok, Advanced}
    end.

%% @doc Extend a name's lease by what the purchase paid for.
%%
%% Nothing is written unless the whole purchase is admissible, so a refused one
%% leaves the registry indistinguishable from a transaction that was never
%% sent. It is never an error: in `all' mode anyone may send the process
%% anything, and a process a stranger could fail a slot on would not survive
%% its own schedule.
purchase(Base, Body, Height, Opts) ->
    maybe
        Name = field(<<"name">>, Body, not_found, Opts),
        true ?= is_binary(Name),
        {ok, Paid} ?= amount_field(<<"reward">>, Body, Opts),
        Held = held(Name, Base, Height, Opts),
        {ok, Bought, Pricing} ?=
            quote(
                Base,
                <<"blocks">>,
                Paid,
                #{
                    <<"name">> => Name,
                    <<"duration">> => remaining(Name, Base, Height, Opts)
                },
                Opts
            ),
        Deadline = extends(Held, Height, Opts) + Bought,
        Factor = setting(<<"grace-factor">>, Base, ?DEFAULT_GRACE_FACTOR, Opts),
        Grace =
            granted(Held, Height, Opts)
                + (((?GRACE_BASIS + Factor) * Bought) div ?GRACE_BASIS),
        ?event(
            {spectrum_purchased,
                {name, {string, Name}},
                {paid, Paid},
                {bought, Bought},
                {deadline, Deadline},
                {grace, Grace}
            }
        ),
        put_name(
            Base,
            Name,
            priced_lease(
                #{
                    <<"deadline">> => Deadline,
                    <<"grace">> => Grace,
                    <<"value">> => value(Held, Body, Opts)
                },
                Pricing
            ),
            Opts
        )
    else
        _ -> Base
    end.

%% @doc Retain opaque metadata supplied by the configured pricing device.
priced_lease(Record, Pricing) when map_size(Pricing) =:= 0 -> Record;
priced_lease(Record, Pricing) -> Record#{ <<"pricing">> => Pricing }.

%% @doc The height a purchase's grace extends from, as `extends/3' is the
%% height its deadline extends from. A lease that still stands carries its
%% grace forward; a free name starts now.
granted(not_found, Height, _Opts) -> Height;
granted(Record, Height, Opts) -> number(<<"grace">>, Record, Height, Opts).

%% @doc The lease a name is under, if it still stands. One whose grace the
%% chain has passed is not held at all: it is the next buyer's, to take and to
%% point wherever they like.
held(Name, Base, Height, Opts) ->
    maybe
        {ok, Record} ?= find_name(Name, Base, Opts),
        case standing(Record, Height, Opts) of
            expired -> not_found;
            _ -> Record
        end
    end.

%% @doc The blocks a name still has to run. Nothing, once its deadline has
%% passed -- a lease in its grace window is held, but it is not running.
remaining(Name, Base, Height, Opts) ->
    case held(Name, Base, Height, Opts) of
        not_found -> 0;
        Record -> max(number(<<"deadline">>, Record, 0, Opts) - Height, 0)
    end.

%% @doc The height a purchase extends from. A lease that still stands is
%% extended from where it already ends, so that time already paid for is not
%% lost and time already lapsed is not silently restored; a free name starts
%% now.
extends(not_found, Height, _Opts) -> Height;
extends(Record, Height, Opts) -> number(<<"deadline">>, Record, Height, Opts).

%% @doc What the name will resolve to. Buying blocks for a name somebody else
%% holds does not buy the right to say what it means: only a name that is
%% free -- never registered, or past its grace -- hears the `value' of the
%% purchase that takes it.
value(not_found, Body, Opts) ->
    hb_cache:ensure_all_loaded(field(<<"value">>, Body, <<>>, Opts), Opts);
value(Record, _Body, Opts) ->
    hb_maps:get(<<"value">>, Record, <<>>, Opts).

%%% State

%% @doc Read a key of the process's own state.
%%
%% While a slot is being computed the state carries this device, so a plain
%% read of one of its keys would resolve that key through this device and land
%% back in the registry. Every read of the state is therefore taken as a
%% message. See `dev_arweave_swap:state/4'.
state(Key, Base, Default, Opts) ->
    hb_ao:get(Key, {as, <<"message@1.0">>, Base}, Default, Opts).

%% @doc Read a field of an untrusted message as plain data.
field(Key, Msg, Default, Opts) ->
    hb_maps:get(Key, Msg, Default, Opts).

%% @doc Read a word a stranger wrote, lowercased, and the default if what they
%% wrote is not a word at all.
%%
%% Every word this device compares is ASCII, so anything else is simply not one
%% of them -- and must be answered that way rather than raised over.
%% `hb_util:to_lower/1' hands a binary that is not ASCII to
%% `string:lowercase/1', which raises on invalid UTF-8; a raise while a slot is
%% being computed is not a refused message but a permanently failed one, on
%% every node, for good. `hb_util_string:lowercase/1' reports the same
%% condition as a value instead.
word(Key, Msg, Default, Opts) ->
    case field(Key, Msg, Default, Opts) of
        Word when is_binary(Word) ->
            case hb_util_string:lowercase(Word) of
                non_ascii -> Default;
                Lowered -> Lowered
            end;
        _ -> Default
    end.

%% @doc Read a number a stranger wrote. Coercing `reward: tomorrow' with
%% `hb_util:int/1' would raise out of the enclosing `maybe' -- which catches
%% mismatches, not exceptions -- and fail that slot on every node, for good.
amount(Key, Msg, Opts) ->
    hb_util:safe_int(field(Key, Msg, 0, Opts)).

%% @doc The same, for a number the base layer recorded rather than one a tag
%% claims.
amount_field(Field, Body, Opts) ->
    hb_util:safe_int(tx_field(Body, Field, 0, Opts)).

%% @doc A number held in the registry. What is written between slots may come
%% back from the process cache as a binary.
number(Key, Record, Default, Opts) ->
    hb_util:ok_or(
        hb_util:safe_int(hb_maps:get(Key, Record, Default, Opts)),
        Default
    ).

%% @doc Read a value from the real layer-1 transaction fields recorded in the
%% `tx@1.0' commitment. Top-level keys may come from tags with the same names,
%% so what was paid must never be read from one.
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

height(Base, Opts) -> setting(?HEIGHT, Base, 0, Opts).

names(Base, Opts) -> state(?NAMES, Base, #{}, Opts).

%% @doc Find the lease a message names, if the registry holds one.
%%
%% The name comes from a stranger, so it is looked up as a key of the registry
%% rather than resolved as a path: a path would let `foo/value' reach a value
%% and a reserved word like `keys' reach a list, either of which would then be
%% read as a lease. Whatever comes back must look like one before it is
%% treated as one.
find_name(Name, Base, Opts) when is_binary(Name) ->
    case
        hb_cache:ensure_all_loaded(
            hb_maps:get(Name, names(Base, Opts), not_found, Opts),
            Opts
        )
    of
        Record = #{ <<"deadline">> := _ } -> {ok, Record};
        _ -> not_found
    end;
find_name(_Name, _Base, _Opts) -> not_found.

%% @doc Write a lease back, replacing the one held rather than merging over it.
%% Only whole top-level keys are written: setting a nested path resolves the
%% keys above it on the way, which is the same hazard `state/4' avoids.
put_name(Base, Name, Record, Opts) ->
    Base#{ ?NAMES => hb_maps:put(Name, Record, names(Base, Opts), Opts) }.

%% @doc Load and issue the configured initial namespace once.
initial_namespace(Base, Opts) ->
    case {
        state(?INITIAL_NAMESPACE, Base, not_found, Opts),
        state(?NAMES, Base, not_found, Opts)
    } of
        {not_found, _} -> {ok, Base};
        {_, Names} when Names =/= not_found -> {ok, Base};
        {Source, not_found} -> seed_namespace(Base, Source, Opts)
    end.

%% @doc Import a prepared model and issue every prepared name from state.
seed_namespace(Base, Source, Opts) ->
    maybe
        {ok, Initial} ?= load_initial_namespace(Source, Opts),
        Model = hb_maps:get(<<"model">>, Initial, not_found, Opts),
        true ?= Model =/= not_found,
        RawNames = hb_maps:get(?NAMES, Initial, not_found, Opts),
        Names = hb_cache:ensure_loaded(RawNames, Opts),
        true ?= is_map(Names),
        {ok, Height} ?= non_negative_state(?HEIGHT, Base, Opts),
        {ok, Nametime} ?= positive_state(?INITIAL_NAMETIME, Base, Opts),
        Factor = setting(
            <<"grace-factor">>,
            Base,
            ?DEFAULT_GRACE_FACTOR,
            Opts
        ),
        true ?= Factor >= 0,
        {ok, Issued} ?= issue_names(Names, Height, Nametime, Factor, Opts),
        {ok, Base#{ <<"model">> => Model, ?NAMES => Issued }}
    else
        _ -> {error, invalid}
    end.

%% @doc Load an initial namespace supplied as an ID, link, or message.
load_initial_namespace(ID, Opts) when ?IS_ID(ID) -> hb_cache:read(ID, Opts);
load_initial_namespace(Link, Opts) when ?IS_LINK(Link) ->
    try {ok, hb_cache:ensure_loaded(Link, Opts)}
    catch _:_ -> {error, invalid}
    end;
load_initial_namespace(Initial, _Opts) when is_map(Initial) -> {ok, Initial};
load_initial_namespace(_Initial, _Opts) -> {error, invalid}.

%% @doc Give every prepared name the process-configured initial term.
issue_names(Names, Height, Nametime, Factor, Opts) ->
    try
        Deadline = Height + Nametime,
        Grace = Deadline + ((Factor * Nametime) div ?GRACE_BASIS),
        {Issued, Occupancy} =
            hb_maps:fold(
                fun(Name, RawRecord, {Acc, Total}) ->
                    true = is_binary(Name),
                    Record = hb_cache:ensure_loaded(RawRecord, Opts),
                    true = is_map(Record),
                    {ok, Weight} = pricing_weight(Record, Opts),
                    {
                        hb_maps:put(
                            Name,
                            Record#{
                                <<"deadline">> => Deadline,
                                <<"grace">> => Grace
                            },
                            Acc,
                            Opts
                        ),
                        Total + Weight
                    }
                end,
                {#{}, 0.0},
                Names,
                Opts
            ),
        true = Occupancy < 1.0,
        {ok, Issued}
    catch
        _:_ -> {error, invalid}
    end.

%% @doc Read a prepared lease's positive retained weight.
pricing_weight(Record, Opts) ->
    Pricing = hb_maps:get(<<"pricing">>, Record, not_found, Opts),
    case is_map(Pricing) of
        false -> {error, invalid};
        true ->
            case hb_maps:get(<<"weight">>, Pricing, not_found, Opts) of
                Weight when is_integer(Weight), Weight > 0 ->
                    {ok, float(Weight)};
                Weight when is_float(Weight) ->
                    case Weight > 0.0 andalso Weight =:= Weight of
                        true -> {ok, Weight};
                        false -> {error, invalid}
                    end;
                _ -> {error, invalid}
            end
    end.

%% @doc Read a required non-negative integer from process state.
non_negative_state(Key, Base, Opts) ->
    case hb_util:safe_int(state(Key, Base, not_found, Opts)) of
        {ok, Value} when Value >= 0 -> {ok, Value};
        _ -> {error, invalid}
    end.

%% @doc Read a required positive integer from process state.
positive_state(Key, Base, Opts) ->
    case non_negative_state(Key, Base, Opts) of
        {ok, Value} when Value > 0 -> {ok, Value};
        _ -> {error, invalid}
    end.

%%% Tests

%%% The tests drive `compute/3' directly with synthetic assignments, exactly
%%% as `~process@1.0' would: the device reads no chain data while computing, so
%%% a whole registry can be played out without a weave.

%%% A year of blocks at the default rate costs a tenth of an AR, so this is
%%% what a year is worth in winston.
-define(YEAR_PRICE, 100000000000).
-define(YEAR_BLOCKS, 262800).
%%% What a single block costs at that rate, rounded up: the least a purchase
%%% can pay and still buy anything.
-define(BLOCK_PRICE, 380518).

%%% The process id must be a real 43-character address so that the transaction
%%% codec can carry it as the layer-1 target, though this device never reads it.
-define(PROCESS, <<"sPeCtRuM00000000000000000000000000000000000">>).

test_opts() -> #{ <<"priv-wallet">> => ar_wallet:new() }.

%% @doc A party to a purchase: a wallet and the address it signs as.
party() ->
    Wallet = ar_wallet:new(),
    {Wallet, hb_util:human_id(ar_wallet:to_address(Wallet))}.

%% @doc An L1 transaction, committed as the base layer commits them.
tx(Wallet, Fields) ->
    hb_message:commit(
        Fields,
        #{ <<"priv-wallet">> => Wallet },
        #{ <<"commitment-device">> => <<"tx@1.0">> }
    ).

%% @doc A transaction that carries its keys as tags only, paying the reward
%% given whatever the tags claim.
tag_only_tx(Wallet, Tags) -> tag_only_tx(Wallet, 1, Tags).

tag_only_tx(Wallet, Reward, Tags) ->
    Signed =
        ar_tx:sign(#tx{ format = 2, reward = Reward, tags = Tags }, Wallet),
    hb_message:convert(Signed, <<"structured@1.0">>, <<"tx@1.0">>, #{}).

%% @doc A purchase paying the given winston as its reward.
buy(Wallet, Name, Winston) ->
    tx(
        Wallet,
        #{
            <<"path">> => <<"purchase">>,
            <<"name">> => Name,
            <<"reward">> => hb_util:bin(Winston)
        }
    ).

buy_with_value(Wallet, Name, Winston, Value) ->
    tx(
        Wallet,
        #{
            <<"path">> => <<"purchase">>,
            <<"name">> => Name,
            <<"value">> => Value,
            <<"reward">> => hb_util:bin(Winston)
        }
    ).

%% @doc Sequence a transaction into the registry at a block height, as
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

%% @doc Advance the registry to a height without anything happening, which is
%% what the network's unrelated traffic does.
tick(Base, Height, Opts) ->
    apply_tx(Base, #{ <<"target">> => <<"someone-else">> }, Height, Opts).

%% @doc Read a name from the registry as a reader would.
read(Base, Name, Opts) -> get(Name, Base, #{}, Opts).

read_at(Base, Name, Height, Opts) ->
    get(Name, Base, #{ <<"height">> => hb_util:bin(Height) }, Opts).

%% @doc Cache an all-mode Arweave assignment at the block that sequenced it.
write_historical_slot(ProcID, Slot, Height, Body, Opts) ->
    Assignment =
        (lib_scheduler:base_assignment(
            ProcID,
            Slot,
            #{ <<"path">> => <<"compute">> },
            Opts
        ))#{
            <<"block-height">> => Height,
            <<"body">> => Body
        },
    lib_scheduler:write_assignment(?ARWEAVE_SCHEDULER_CACHE, Assignment, Opts).

lease(Base, Name, Key, Opts) ->
    hb_maps:get(
        Key,
        hb_maps:get(Name, names(Base, Opts), #{}, Opts),
        not_found,
        Opts
    ).

%% @doc A year's worth of AR buys a year's worth of blocks, and the name then
%% resolves to what its buyer said it meant.
purchase_registers_a_name_test() ->
    Opts = test_opts(),
    {Buyer, _} = party(),
    Base =
        apply_tx(
            #{},
            buy_with_value(Buyer, <<"hello">>, ?YEAR_PRICE, <<"world">>),
            1000,
            Opts
        ),
    ?assertEqual(1000 + ?YEAR_BLOCKS, lease(Base, <<"hello">>, <<"deadline">>, Opts)),
    ?assertEqual({ok, <<"world">>}, read(Base, <<"hello">>, Opts)).

%% @doc The grace window follows the deadline in proportion to what was bought.
grace_follows_the_deadline_test() ->
    Opts = test_opts(),
    {Buyer, _} = party(),
    Base = apply_tx(#{}, buy(Buyer, <<"hello">>, ?YEAR_PRICE), 1000, Opts),
    Deadline = lease(Base, <<"hello">>, <<"deadline">>, Opts),
    ?assertEqual(
        Deadline + (?YEAR_BLOCKS div 10),
        lease(Base, <<"hello">>, <<"grace">>, Opts)
    ).

%% @doc A tag is not a transaction field. A purchase whose `reward' tag claims
%% a fortune buys only what its real reward paid for -- here one winston,
%% which buys no blocks at all.
tags_are_not_transaction_fields_test() ->
    Opts = test_opts(),
    {Buyer, _} = party(),
    Spoofed =
        tag_only_tx(
            Buyer,
            [
                {<<"path">>, <<"purchase">>},
                {<<"name">>, <<"hello">>},
                {<<"reward">>, hb_util:bin(?YEAR_PRICE)}
            ]
        ),
    ?assertEqual(<<"1">>, tx_field(Spoofed, <<"reward">>, <<"0">>, Opts)),
    Base = apply_tx(#{}, Spoofed, 1000, Opts),
    % A winston buys no blocks, so the lease it writes has already run out.
    ?assertEqual(1000, lease(Base, <<"hello">>, <<"deadline">>, Opts)),
    ?assertEqual({error, not_found}, read(Base, <<"hello">>, Opts)).

%% @doc Buying more blocks for a name somebody else holds extends it and says
%% nothing about what it means.
value_is_not_for_sale_test() ->
    Opts = test_opts(),
    {Holder, _} = party(),
    {Stranger, _} = party(),
    Held =
        apply_tx(
            #{},
            buy_with_value(Holder, <<"hello">>, ?YEAR_PRICE, <<"world">>),
            1000,
            Opts
        ),
    Extended =
        apply_tx(
            Held,
            buy_with_value(Stranger, <<"hello">>, ?YEAR_PRICE, <<"stolen">>),
            2000,
            Opts
        ),
    ?assertEqual({ok, <<"world">>}, read(Extended, <<"hello">>, Opts)),
    ?assertEqual(
        1000 + (2 * ?YEAR_BLOCKS),
        lease(Extended, <<"hello">>, <<"deadline">>, Opts)
    ).

%% @doc Past its grace a name is nobody's, and the next purchase both takes it
%% and says what it means.
expired_name_can_be_retaken_test() ->
    Opts = test_opts(),
    {Holder, _} = party(),
    {Next, _} = party(),
    Held =
        apply_tx(
            #{},
            buy_with_value(Holder, <<"hello">>, ?YEAR_PRICE, <<"world">>),
            1000,
            Opts
        ),
    Gone = lease(Held, <<"hello">>, <<"grace">>, Opts) + 1,
    Retaken =
        apply_tx(
            Held,
            buy_with_value(Next, <<"hello">>, ?YEAR_PRICE, <<"newer">>),
            Gone,
            Opts
        ),
    ?assertEqual({ok, <<"newer">>}, read(Retaken, <<"hello">>, Opts)),
    % The lease restarts from now, not from the deadline it outlived.
    ?assertEqual(
        Gone + ?YEAR_BLOCKS,
        lease(Retaken, <<"hello">>, <<"deadline">>, Opts)
    ).

%% @doc A read at an old block uses the process state at that block, not a
%% future lease record that happens to remain live at the old height.
historical_reads_use_historical_state_test() ->
    Opts = (test_opts())#{ <<"store">> => hb_test_utils:test_store() },
    Process =
        hb_message:commit(
            #{
                <<"device">> => <<"process@1.0">>,
                <<"scheduler-device">> => <<"arweave-scheduler@1.0">>,
                <<"scheduler-mode">> => <<"all">>,
                <<"execution-device">> => <<"spectrum@1.0">>
            },
            Opts
        ),
    ProcID = hb_util:human_id(hb_message:id(Process, signed, Opts)),
    {Holder, _} = party(),
    {Next, _} = party(),
    ok = write_historical_slot(ProcID, 0, 900, Process, Opts),
    ok =
        write_historical_slot(
            ProcID,
            1,
            1000,
            buy_with_value(Holder, <<"hello">>, ?YEAR_PRICE, <<"world">>),
            Opts
        ),
    {ok, Held} =
        hb_ao:resolve(
            Process,
            #{ <<"path">> => <<"compute">>, <<"slot">> => 1 },
            Opts#{ <<"hashpath">> => ignore }
        ),
    Gone = lease(Held, <<"hello">>, <<"grace">>, Opts) + 1,
    ok =
        write_historical_slot(
            ProcID,
            2,
            Gone,
            buy_with_value(Next, <<"hello">>, ?YEAR_PRICE, <<"newer">>),
            Opts
        ),
    {ok, Retaken} =
        hb_ao:resolve(
            Process,
            #{ <<"path">> => <<"compute">>, <<"slot">> => 2 },
            Opts#{ <<"hashpath">> => ignore }
        ),
    Registry = Retaken#{ <<"device">> => <<"spectrum@1.0">> },
    ?assertEqual(
        {error, not_found},
        read_at(Registry, <<"hello">>, 899, Opts)
    ),
    ?assertEqual(
        {error, not_found},
        read_at(Registry, <<"hello">>, 999, Opts)
    ),
    ?assertEqual(
        {ok, <<"world">>},
        read_at(Registry, <<"hello">>, 1001, Opts)
    ),
    ?assertEqual(
        {ok, <<"newer">>},
        read_at(Registry, <<"hello">>, Gone, Opts)
    ).

%% @doc Between deadline and grace a name resolves to the registry's notice,
%% and past the grace to nothing.
grace_and_expiry_test() ->
    Opts = test_opts(),
    {Buyer, _} = party(),
    Bought =
        apply_tx(
            #{ <<"grace-notice">> => <<"expired-notice-id">> },
            buy_with_value(Buyer, <<"hello">>, ?YEAR_PRICE, <<"world">>),
            1000,
            Opts
        ),
    Deadline = lease(Bought, <<"hello">>, <<"deadline">>, Opts),
    Grace = lease(Bought, <<"hello">>, <<"grace">>, Opts),
    ?assertEqual({ok, <<"world">>}, read_at(Bought, <<"hello">>, Deadline - 1, Opts)),
    ?assertEqual(
        {ok, <<"expired-notice-id">>},
        read_at(Bought, <<"hello">>, Deadline, Opts)
    ),
    ?assertEqual(
        {ok, <<"expired-notice-id">>},
        read_at(Bought, <<"hello">>, Grace - 1, Opts)
    ),
    ?assertEqual({error, not_found}, read_at(Bought, <<"hello">>, Grace, Opts)).

%% @doc Unrelated traffic is what ages the registry, and so is the whole of
%% how a lease expires.
%%
%% Nothing here sweeps the registry: no slot retires what has run out. A name
%% stops resolving because the height it is read against has moved past its
%% deadline, and the only thing that moves that height is `compute/3'
%% recording it -- on every slot, including the overwhelming majority that buy
%% nothing. Record it only on purchases and a registry nobody is buying from
%% never expires a name.
unrelated_traffic_ages_the_registry_test() ->
    Opts = test_opts(),
    {Buyer, _} = party(),
    Bought =
        apply_tx(
            #{},
            buy_with_value(Buyer, <<"hello">>, ?YEAR_PRICE, <<"world">>),
            1000,
            Opts
        ),
    ?assertEqual({ok, <<"world">>}, read(Bought, <<"hello">>, Opts)),
    Later = tick(Bought, lease(Bought, <<"hello">>, <<"grace">>, Opts), Opts),
    ?assertEqual({error, not_found}, read(Later, <<"hello">>, Opts)).

%% @doc Resolve a key against the registry the way the runtime does, rather
%% than by calling this module's default handler directly.
through(Registry, Key, Opts) ->
    catch
        hb_ao:resolve(
            Registry#{ <<"device">> => <<"spectrum@1.0">> },
            #{ <<"path">> => Key },
            Opts#{ <<"hashpath">> => ignore }
        ).

%% @doc Nothing a stranger can write to the network fails a slot. Every one of
%% these is sequenced by `all' mode, and a slot that raised would stop the
%% process on every node for good.
nonsense_does_not_wedge_test() ->
    Opts = test_opts(),
    {Buyer, _} = party(),
    Nonsense =
        [
            #{ <<"path">> => <<"purchase">> },
            #{ <<"path">> => 42 },
            #{ <<"path">> => <<"purchase">>, <<"name">> => 42 },
            #{ <<"path">> => <<"purchase">>, <<"name">> => #{ <<"a">> => 1 } },
            % Bytes that are not valid UTF-8 in the word this device folds
            % the case of. `hb_util:to_lower/1' raises on them, and a raise
            % here is a slot that fails on every node for good.
            #{ <<"path">> => <<255, 254>> },
            #{ <<"path">> => <<"purchase", 255>> },
            % The same, built the way an attacker really would: a signed
            % layer-1 transaction whose tags carry the bytes verbatim.
            tag_only_tx(
                Buyer,
                ?YEAR_PRICE,
                [{<<"path">>, <<"purchase", 255>>}, {<<"name">>, <<"hello">>}]
            ),
            #{ <<"path">> => <<"set">>, <<"names">> => <<"gone">> },
            #{ <<"path">> => <<"keys">> },
            #{ <<"path">> => <<"compute">> },
            tag_only_tx(
                Buyer,
                [
                    {<<"path">>, <<"purchase">>},
                    {<<"name">>, <<"hello">>},
                    {<<"reward">>, <<"tomorrow">>}
                ]
            )
        ],
    Held =
        apply_tx(
            #{},
            buy_with_value(Buyer, <<"held">>, ?YEAR_PRICE, <<"world">>),
            1000,
            Opts
        ),
    lists:foldl(
        fun(Body, Acc) ->
            Next = apply_tx(Acc, Body, 1001, Opts),
            % The registry is untouched by every one of them.
            ?assertEqual({ok, <<"world">>}, read(Next, <<"held">>, Opts)),
            Next
        end,
        Held,
        Nonsense
    ).

%% @doc `blocks' and `price' are inverses of one another.
blocks_and_price_are_inverses_test() ->
    Opts = test_opts(),
    ?assertEqual(
        {ok, ?YEAR_BLOCKS},
        blocks(#{}, #{ <<"blocks">> => hb_util:bin(?YEAR_PRICE) }, Opts)
    ),
    ?assertEqual(
        {ok, ?YEAR_PRICE},
        price(#{}, #{ <<"price">> => hb_util:bin(?YEAR_BLOCKS) }, Opts)
    ),
    % Rounding cannot be walked into free registration in either direction: a
    % winston buys no blocks, and a block never costs nothing.
    ?assertEqual({ok, 0}, blocks(#{}, #{ <<"blocks">> => <<"1">> }, Opts)),
    {ok, One} = price(#{}, #{ <<"price">> => <<"1">> }, Opts),
    ?assertEqual(?YEAR_PRICE div ?YEAR_BLOCKS + 1, One),
    ?assertMatch(
        {ok, N} when N >= 1,
        blocks(#{}, #{ <<"blocks">> => hb_util:bin(One) }, Opts)
    ).

%% @doc The rate the process names is the rate that is charged.
process_sets_the_rate_test() ->
    Opts = test_opts(),
    Base =
        #{
            <<"price-per-year">> => <<"1000">>,
            <<"blocks-per-year">> => <<"100">>
        },
    ?assertEqual({ok, 100}, blocks(Base, #{ <<"blocks">> => <<"1000">> }, Opts)),
    ?assertEqual({ok, 1000}, price(Base, #{ <<"price">> => <<"100">> }, Opts)),
    {Buyer, _} = party(),
    Bought = apply_tx(Base, buy(Buyer, <<"hello">>, 1000), 10, Opts),
    ?assertEqual(110, lease(Bought, <<"hello">>, <<"deadline">>, Opts)).

%% @doc Only AR buys names today. A payment in anything else buys nothing
%% rather than being silently taken as AR.
only_ar_is_accepted_test() ->
    Opts = test_opts(),
    ?assertEqual(
        {error, invalid},
        blocks(
            #{},
            #{
                <<"blocks">> => hb_util:bin(?YEAR_PRICE),
                <<"token">> => <<"ao">>
            },
            Opts
        )
    ),
    ?assertMatch(
        {ok, _},
        blocks(
            #{},
            #{
                <<"blocks">> => hb_util:bin(?YEAR_PRICE),
                <<"token">> => <<"AR">>
            },
            Opts
        )
    ).

%% @doc A process may replace the curve entirely without this device changing.
%% The device here charges by the length of the name, which is what the spec
%% ultimately asks for and what the flat curve deliberately does not do.
pricing_device_replaces_the_curve_test() ->
    Opts = test_opts(),
    ByLength =
        #{
            <<"device">> =>
                #{
                    info =>
                        fun() ->
                            #{
                                default =>
                                    fun(Key, _, Req, _) ->
                                        Given =
                                            hb_util:int(
                                                maps:get(Key, Req, <<"0">>)
                                            ),
                                        Name = maps:get(<<"name">>, Req, <<>>),
                                        {ok, Given div byte_size(Name)}
                                    end
                            }
                        end
                }
        },
    Base = #{ <<"pricing-device">> => maps:get(<<"device">>, ByLength) },
    ?assertEqual(
        {ok, 25},
        blocks(Base, #{ <<"blocks">> => <<"100">>, <<"name">> => <<"abcd">> }, Opts)
    ),
    {Buyer, _} = party(),
    Bought = apply_tx(Base, buy(Buyer, <<"abcd">>, 100), 10, Opts),
    ?assertEqual(35, lease(Bought, <<"abcd">>, <<"deadline">>, Opts)).

%% @doc A structured quote retains opaque pricing metadata on the lease.
probability_time_pricing_device_test() ->
    Opts = test_opts(),
    {ok, Trained} =
        hb_ao:resolve(
            #{ <<"device">> => <<"markov@1.0">> },
            #{
                <<"path">> => <<"train">>,
                <<"body">> => [<<"a">>, <<"ab">>, <<"b">>],
                <<"order">> => 1
            },
            Opts#{ <<"hashpath">> => ignore }
        ),
    Base =
        Trained#{
            <<"pricing-device">> => <<"probability-time@1.0">>,
            <<"target-occupancy">> => 0.5,
            <<"price-at-target">> => 1000000000000
        },
    {ok, Cost} =
        price(
            Base,
            #{ <<"price">> => 100, <<"name">> => <<"a">> },
            Opts
        ),
    {Buyer, _} = party(),
    Bought = apply_tx(Base, buy(Buyer, <<"a">>, Cost), 10, Opts),
    ?assertEqual(110, lease(Bought, <<"a">>, <<"deadline">>, Opts)),
    Pricing = lease(Bought, <<"a">>, <<"pricing">>, Opts),
    ?assert(is_float(hb_maps:get(<<"weight">>, Pricing, Opts))),
    {ok, EmptyPrice} =
        price(
            Base,
            #{ <<"price">> => 10, <<"name">> => <<"b">> },
            Opts
        ),
    {ok, OccupiedPrice} =
        price(
            Bought,
            #{ <<"price">> => 10, <<"name">> => <<"b">> },
            Opts
        ),
    ?assert(OccupiedPrice > EmptyPrice).

%% @doc Build one prepared initial-name record from the configured model.
prepared_name(Name, Value, Model, Opts) ->
    {ok, Weight} =
        hb_ao:resolve(
            Model,
            #{
                <<"path">> => <<"likelihood">>,
                <<"body">> => Name,
                <<"include-end">> => true,
                <<"result-mode">> => <<"float">>
            },
            Opts#{ <<"hashpath">> => ignore }
        ),
    #{
        <<"value">> => Value,
        <<"pricing">> => #{ <<"weight">> => Weight }
    }.

%% @doc Exercise the complete beta execution and pricing device composition.
beta_device_stack_test() ->
    Opts =
        (test_opts())#{
            <<"store">> => hb_test_utils:test_store()
        },
    Resolve =
        fun(State, Req) ->
            hb_ao:resolve(
                State,
                Req,
                Opts#{ <<"hashpath">> => ignore }
            )
        end,
    State =
        fun(Message, Path) ->
            hb_ao:get(Path, {as, <<"message@1.0">>, Message}, Opts)
        end,
    Samples = [<<"alpha">>, <<"beta">>, <<"gamma">>],
    Height = 2000000,
    Nametime = 200 * ?YEAR_BLOCKS,
    Deadline = Height + Nametime,
    Grace = Deadline + (Nametime div 20),
    {ok, Trained} =
        hb_ao:resolve(
            #{ <<"device">> => <<"markov@1.0">> },
            #{
                <<"path">> => <<"train">>,
                <<"body">> => Samples,
                <<"order">> => 4
            },
            Opts#{ <<"hashpath">> => ignore }
        ),
    Prepared =
        maps:from_list(
            [
                {
                    Name,
                    (prepared_name(
                        Name,
                        <<Name/binary, "-value">>,
                        Trained,
                        Opts
                    ))#{
                        <<"deadline">> => Deadline,
                        <<"grace">> => Grace
                    }
                }
            || Name <- Samples
            ]
        ),
    Model =
        hb_ao:get(
            <<"model">>,
            {as, <<"message@1.0">>, Trained},
            Opts
        ),
    SignedModel =
        hb_message:commit(
            Model,
            Opts,
            #{ <<"device">> => <<"ans104@1.0">>, <<"bundle">> => true }
        ),
    SignedNames =
        hb_message:commit(
            Prepared,
            Opts,
            #{ <<"device">> => <<"ans104@1.0">>, <<"bundle">> => true }
        ),
    {ok, _} = hb_cache:write(SignedModel, Opts),
    {ok, _} = hb_cache:write(SignedNames, Opts),
    ModelID = hb_message:id(SignedModel, all, Opts),
    NamesID = hb_message:id(SignedNames, all, Opts),
    Process =
        #{
            <<"device">> => <<"process@1.0">>,
            <<"execution-device">> => <<"spectrum@1.0">>,
            <<"scheduler-device">> => <<"arweave-scheduler@1.0">>,
            <<"scheduler-mode">> => <<"all">>,
            <<"pricing-device">> => <<"probability-time@1.0">>,
            <<"probability-device">> => <<"markov@1.0">>,
            <<"model+link">> => ModelID,
            <<"names+link">> => NamesID,
            <<"spectrum-height">> => hb_util:bin(Height),
            <<"grace-factor">> => <<"500">>,
            <<"target-occupancy">> => <<"0.65">>,
            <<"price-at-target">> => <<"1000000000000">>
        },
    SignedProcess =
        hb_message:commit(
            Process,
            Opts,
            #{ <<"device">> => <<"tx@1.0">>, <<"bundle">> => false }
        ),
    ProcessTX =
        hb_message:convert(
            SignedProcess,
            <<"tx@1.0">>,
            <<"structured@1.0">>,
            Opts
        ),
    ?assertEqual(0, ProcessTX#tx.data_size),
    ?assertEqual(<<>>, ProcessTX#tx.data),
    DecodedProcess =
        hb_message:convert(
            ProcessTX,
            <<"structured@1.0">>,
            <<"tx@1.0">>,
            Opts
        ),
    ?assert(hb_message:verify(DecodedProcess, all, Opts)),
    ?assert(?IS_LINK(maps:get(<<"model">>, DecodedProcess))),
    ?assert(?IS_LINK(maps:get(<<"names">>, DecodedProcess))),
    Base =
        (hb_message:uncommitted(DecodedProcess, Opts))#{
            <<"device">> => <<"spectrum@1.0">>
        },
    {ok, Initialized} =
        Resolve(
            Base,
            #{ <<"path">> => <<"init">> }
        ),
    ?assertEqual(
        4,
        State(Initialized, <<"model/order">>)
    ),
    ?assertEqual(
        Deadline,
        State(Initialized, <<"names/alpha/deadline">>)
    ),
    ?assertEqual(
        Grace,
        State(Initialized, <<"names/alpha/grace">>)
    ),
    ?assertEqual(
        {ok, <<"alpha-value">>},
        Resolve(
            Initialized,
            #{ <<"path">> => <<"alpha">>, <<"load">> => false }
        )
    ),
    Request = #{ <<"price">> => 100, <<"name">> => <<"alphabet">> },
    {ok, Cost} = Resolve(Initialized, Request#{ <<"path">> => <<"price">> }),
    {ok, Double} =
        Resolve(
            Initialized#{ <<"price-at-target">> => <<"2000000000000">> },
            Request#{ <<"path">> => <<"price">> }
        ),
    ?assert(abs(Double - (2 * Cost)) =< 1),
    {ok, LowerTarget} =
        Resolve(
            Initialized#{ <<"target-occupancy">> => <<"0.5">> },
            Request#{ <<"path">> => <<"price">> }
        ),
    ?assert(LowerTarget > Cost),
    {ok, BoughtBlocks} =
        Resolve(
            Initialized,
            #{
                <<"path">> => <<"blocks">>,
                <<"blocks">> => Cost,
                <<"name">> => <<"alphabet">>
            }
        ),
    {Buyer, _} = party(),
    {ok, Bought} =
        Resolve(
            Initialized,
            #{
                <<"path">> => <<"compute">>,
                <<"block-height">> => Height + 1,
                <<"body">> => buy(Buyer, <<"alphabet">>, Cost)
            }
        ),
    ?assertEqual(
        Height + 1 + BoughtBlocks,
        State(Bought, <<"names/alphabet/deadline">>)
    ),
    ?assertEqual(
        Height + 1 + ((10500 * BoughtBlocks) div ?GRACE_BASIS),
        State(Bought, <<"names/alphabet/grace">>)
    ),
    ?assertMatch(
        Pricing when is_map(Pricing),
        State(Bought, <<"names/alphabet/pricing">>)
    ),
    ?assertEqual(
        {ok, Bought},
        Resolve(Bought, #{ <<"path">> => <<"init">> })
    ).

%% @doc A lease's numbers come back from the process cache as numbers, or are
%% coerced back into them. `number/4' re-coerces on every read on that stated
%% ground; if it is not so, the coercion is doing nothing at all.
numbers_survive_the_cache_test() ->
    Opts = test_opts(),
    {Buyer, _} = party(),
    Written =
        apply_tx(
            #{},
            buy_with_value(Buyer, <<"hello">>, ?YEAR_PRICE, <<"world">>),
            1000,
            Opts
        ),
    {ok, ID} = hb_cache:write(Written, Opts),
    {ok, Read} = hb_cache:read(ID, Opts),
    Loaded = hb_cache:ensure_all_loaded(Read, Opts),
    ?assertEqual({ok, <<"world">>}, read(Loaded, <<"hello">>, Opts)),
    ?assertEqual(
        {error, not_found},
        read_at(Loaded, <<"hello">>, 1000 + (2 * ?YEAR_BLOCKS), Opts)
    ).

%% @doc A name is looked up as a key of the registry, never resolved as a
%% path: a path would let `hello/value' reach a value and a reserved word
%% reach a list, either of which would then be read as a lease.
paths_are_not_names_test() ->
    Opts = test_opts(),
    {Buyer, _} = party(),
    Base =
        apply_tx(
            #{},
            buy_with_value(Buyer, <<"hello">>, ?YEAR_PRICE, <<"world">>),
            1000,
            Opts
        ),
    ?assertEqual({error, not_found}, read(Base, <<"hello/value">>, Opts)),
    ?assertEqual({error, not_found}, read(Base, <<"names">>, Opts)).

%% @doc A value is dereferenced only when the reader asks for it: `~name@1.0'
%% loads what its resolvers hand back, so loading here unasked would do it
%% twice.
load_dereferences_the_value_test() ->
    Opts = test_opts(),
    {Buyer, _} = party(),
    {ok, ID} = hb_cache:write(#{ <<"deep">> => <<"PING">> }, Opts),
    Base =
        apply_tx(
            #{},
            buy_with_value(Buyer, <<"hello">>, ?YEAR_PRICE, ID),
            1000,
            Opts
        ),
    ?assertEqual({ok, ID}, read(Base, <<"hello">>, Opts)),
    ?assertMatch(
        {ok, #{ <<"deep">> := <<"PING">> }},
        get(<<"hello">>, Base, #{ <<"load">> => true }, Opts)
    ).

%% @doc The registry answers over HTTP, as a device and through a name stack.
http_test() ->
    Opts = test_opts(),
    {Buyer, _} = party(),
    Registry =
        apply_tx(
            #{},
            buy_with_value(Buyer, <<"hello">>, ?YEAR_PRICE, <<"world">>),
            1000,
            Opts
        ),
    Node =
        hb_http_server:start_node(
            #{
                <<"port">> => 0,
                <<"name-resolvers">> =>
                    [Registry#{ <<"device">> => <<"spectrum@1.0">> }]
            }
        ),
    ?assertEqual(
        {ok, ?YEAR_BLOCKS},
        hb_http:get(
            Node,
            <<"/~spectrum@1.0/blocks=", (hb_util:bin(?YEAR_PRICE))/binary,
                "&name=hello&token=ar">>,
            Opts
        )
    ),
    ?assertEqual(
        {ok, ?YEAR_PRICE},
        hb_http:get(
            Node,
            <<"/~spectrum@1.0/price=", (hb_util:bin(?YEAR_BLOCKS))/binary,
                "&name=hello">>,
            Opts
        )
    ),
    % Through `~name@1.0', which is where the registry is meant to sit.
    ?assertEqual(
        {ok, <<"world">>},
        hb_http:get(Node, <<"/~name@1.0/hello&load=false">>, Opts)
    ),
    % A name nobody has bought is a miss, which is what lets `~name@1.0' carry
    % on to the next resolver in its stack -- and, with none left, answer 404.
    ?assertEqual(
        {error, not_found},
        hb_http:get(Node, <<"/~name@1.0/nobody-bought-this">>, Opts)
    ).

%% @doc Grace is extended from where the name's grace already stood, so a
%% purchase adds to it and can never take from it.
%%
%% Were it computed from the purchase alone, a single block bought against a
%% name would leave that name a single block of grace -- so anyone could strip
%% the window protecting a name they do not hold for less than a transaction
%% costs, and, if the name were already inside that window, put its end behind
%% the current height and take the name with the next transaction.
grace_cannot_be_bought_away_test() ->
    Opts = test_opts(),
    {Holder, _} = party(),
    {Thief, _} = party(),
    Held =
        apply_tx(
            #{},
            buy_with_value(Holder, <<"hello">>, ?YEAR_PRICE, <<"world">>),
            1000,
            Opts
        ),
    Deadline = lease(Held, <<"hello">>, <<"deadline">>, Opts),
    Grace = lease(Held, <<"hello">>, <<"grace">>, Opts),
    % One block, bought against a name the buyer does not hold. It extends
    % both heights and shortens neither.
    Dusted = apply_tx(Held, buy(Thief, <<"hello">>, ?BLOCK_PRICE), 1001, Opts),
    ?assertEqual(Deadline + 1, lease(Dusted, <<"hello">>, <<"deadline">>, Opts)),
    ?assertEqual(Grace + 1, lease(Dusted, <<"hello">>, <<"grace">>, Opts)),
    % And the same while the name is inside its window, where a grace that
    % moved backwards would hand the name over rather than merely shorten it.
    InGrace =
        apply_tx(Held, buy(Thief, <<"hello">>, ?BLOCK_PRICE), Deadline + 1, Opts),
    ?assertEqual(Grace + 1, lease(InGrace, <<"hello">>, <<"grace">>, Opts)),
    Taken =
        apply_tx(
            InGrace,
            buy_with_value(Thief, <<"hello">>, ?YEAR_PRICE, <<"stolen">>),
            Deadline + 2,
            Opts
        ),
    ?assertEqual({ok, <<"world">>}, read_at(Taken, <<"hello">>, Deadline + 2, Opts)).

%% @doc A moment the device cannot place is not answered at the registry's own
%% height, which would be indistinguishable from not having asked and would
%% read a lapsed lease as good.
unresolvable_dates_are_not_answered_test() ->
    Opts = test_opts(),
    {Buyer, _} = party(),
    Bought =
        apply_tx(
            #{},
            buy_with_value(Buyer, <<"hello">>, ?YEAR_PRICE, <<"world">>),
            1000,
            Opts
        ),
    % The registry's own clock says the name is live.
    ?assertEqual({ok, <<"world">>}, read(Bought, <<"hello">>, Opts)),
    lists:foreach(
        fun(Asked) ->
            ?assertEqual(
                {error, not_found},
                get(<<"hello">>, Bought, Asked, Opts),
                Asked
            )
        end,
        [
            #{ <<"date">> => <<"last-tuesday">> },
            #{ <<"date">> => <<"2024-13-45">> },
            #{ <<"date">> => <<>> },
            #{ <<"date">> => #{ <<"a">> => 1 } },
            #{ <<"height">> => <<"pear">> }
        ]
    ),
    % A height it can place is answered against that height.
    ?assertEqual({ok, <<"world">>}, read_at(Bought, <<"hello">>, 1001, Opts)),
    ?assertEqual({error, not_found}, read_at(Bought, <<"hello">>, 2000000, Opts)).

%%% Story
%%%
%%% A registry that really exists, on mainnet, whose names were really bought.
%%% Every transaction below is a layer-1 Arweave transaction; the reads walk
%%% the process forward through the schedule that sequenced them, so what the
%%% weave did to the registry is visible in the order it happened.
%%%
%%%   spawn              the registry, as an ordinary transaction whose tags
%%%                      are its process message
%%%   purchase           a hundredth of an AR, paid as the transaction's own
%%%                      reward, addressed to nobody -- which at the default
%%%                      rate of a tenth of an AR a year is 26,280 blocks
%%%   purchase:shadowed  the same payment for `price', which is registered
%%%                      like any other name, and which this device's own
%%%                      `price' key answers ahead of.
-define(STORY_PROCESS, <<"tUx955niagbz4Gy1WEulTCPmLYw0pJHDOKyHl6TzT1k">>).
-define(STORY_NAME, <<"spectrum-1785423919">>).
-define(STORY_VALUE, <<"XjvsdgnCjYW3kfWTB0qPKcVLTBHqBSc1JWkTiSDcJ3M">>).
-define(STORY_PURCHASE, <<"SY5-9KhjWnGyhvMVL6_nBTnPULGKkBE5CZRlD7Aja5w">>).
-define(STORY_SHADOWED, <<"IKauJ3BCqDH39TXX4R6axexIHM7L-FV7RsfhIlzB3X0">>).
-define(STORY_BUY_HEIGHT, 1969707).
-define(STORY_MAX_HEIGHT, 1969708).
%%% A hundredth of an AR at the default rate.
-define(STORY_BOUGHT, 26280).

%% @doc Node options pinned to the story's last block, so its answer is
%% immutable: no block after it can reach the process.
story_opts() ->
    TestStore = hb_test_utils:test_store(),
    IndexStore = hb_test_utils:test_store(),
    (hb_opts:default_message())#{
        <<"port">> => 0,
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
        <<"arweave-scheduler-max-height">> => ?STORY_MAX_HEIGHT,
        <<"name-resolvers">> =>
            [<<?STORY_PROCESS/binary, "~process@1.0/now/~spectrum@1.0">>],
        <<"node-host">> => <<"host">>,
        <<"priv-wallet">> => ar_wallet:new()
    }.

%% @doc Synchronize the story's schedule, retrying while the gateway
%% rate-limits us -- the same allowance the scheduler's own fixture tests make.
story_sync(_Opts, 0) -> {error, sync_failed};
story_sync(Opts, Attempts) ->
    case
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave-scheduler@1.0">> },
            #{
                <<"path">> => <<"schedule">>,
                <<"method">> => <<"GET">>,
                <<"target">> => ?STORY_PROCESS
            },
            Opts
        )
    of
        {ok, Schedule} -> {ok, Schedule};
        _ ->
            timer:sleep(5000),
            story_sync(Opts, Attempts - 1)
    end.

%% @doc The slot a transaction was given. The story pins transaction ids rather
%% than slot numbers, because a slot number is a fact about the weave -- every
%% data-free transaction on the network takes one -- while the id is the
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

%% @doc Read the registry as it stood at the end of a given slot, in the form
%% a reader can follow: `<process>~process@1.0/compute&slot=<n>/<path>'.
state_at(Slot, Opts) ->
    hb_ao:resolve(
        <<
            ?STORY_PROCESS/binary,
            "~process@1.0/compute&slot=",
            (hb_util:bin(Slot))/binary
        >>,
        Opts#{ <<"hashpath">> => ignore }
    ).

state_at(Slot, Path, Opts) ->
    hb_ao:resolve(
        <<
            ?STORY_PROCESS/binary,
            "~process@1.0/compute&slot=",
            (hb_util:bin(Slot))/binary,
            "/",
            Path/binary
        >>,
        Opts#{ <<"hashpath">> => ignore }
    ).

story_test_() -> {timeout, 3600, fun story/0}.
story() ->
    Opts = story_opts(),
    {ok, Schedule} = story_sync(Opts, 5),
    Bought = slot_of(Schedule, ?STORY_PURCHASE, Opts),
    Shadowed = slot_of(Schedule, ?STORY_SHADOWED, Opts),
    Name = ?STORY_NAME,
    Deadline = ?STORY_BUY_HEIGHT + ?STORY_BOUGHT,
    Grace = Deadline + (?STORY_BOUGHT div 10),
    % A hundredth of an AR, paid as a reward to nobody, bought a name.
    ?assertEqual(
        {ok, Deadline},
        state_at(Bought, <<"names/", Name/binary, "/deadline">>, Opts)
    ),
    ?assertEqual(
        {ok, Grace},
        state_at(Bought, <<"names/", Name/binary, "/grace">>, Opts)
    ),
    ?assertEqual(
        {ok, ?STORY_VALUE},
        state_at(Bought, <<"names/", Name/binary, "/value">>, Opts)
    ),
    % The registry's clock is the height that sequenced the slot.
    ?assertEqual({ok, ?STORY_BUY_HEIGHT}, state_at(Bought, ?HEIGHT, Opts)),
    % The second purchase bought `price', which is registered like any other
    % name and which the device's own `price' key answers ahead of.
    ?assertEqual(
        {ok, ?STORY_MAX_HEIGHT + ?STORY_BOUGHT},
        state_at(Shadowed, <<"names/price/deadline">>, Opts)
    ),
    % Read through the device, which is how a name is meant to be asked for.
    {ok, State} = state_at(Shadowed, Opts),
    Registry = State#{ <<"device">> => <<"spectrum@1.0">> },
    ?assertEqual({ok, ?STORY_VALUE}, through(Registry, Name, Opts)),
    % And against the clock, at heights the weave has not reached yet. The
    % answers cannot tell the first two apart -- this registry was spawned
    % with its `grace-notice' set to the same id the name carries -- so the
    % windows themselves are read off the lease the chain actually wrote.
    {ok, Lease} = find_name(Name, State, Opts),
    ?assertEqual(live, standing(Lease, Deadline - 1, Opts)),
    ?assertEqual(grace, standing(Lease, Deadline, Opts)),
    ?assertEqual(grace, standing(Lease, Grace - 1, Opts)),
    ?assertEqual(expired, standing(Lease, Grace, Opts)),
    Ask =
        fun(Height) ->
            get(Name, State, #{ <<"height">> => hb_util:bin(Height) }, Opts)
        end,
    ?assertEqual({ok, ?STORY_VALUE}, Ask(Deadline - 1)),
    ?assertEqual({error, not_found}, Ask(Grace)),
    % The registry sits in the node's `~name@1.0' resolvers, which is what it
    % is for. The name is resolved from the live process, over HTTP.
    Node = hb_http_server:start_node(Opts),
    ?assertEqual(
        {ok, ?STORY_VALUE},
        hb_http:get(
            Node,
            <<"/~name@1.0/", Name/binary, "&load=false">>,
            Opts
        )
    ).
