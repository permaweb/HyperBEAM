%%% @doc An AO-Core interface to Arweave's mining search: the pass a miner makes
%%% over one partition at one VDF step, looking for a chunk it holds whose hash
%%% meets the difficulty the network demands of it.
%%%
%%% The device owns the order of the search and nothing else. Every rule it
%%% applies belongs elsewhere: `~arweave-spora@2.9' derives the mining entropy,
%%% the two recall ranges, the byte each nonce recalls and the two solution
%%% hashes, and packs the sub-chunks a miner hashes; the vendored consensus code
%%% owns the nonce arithmetic and the difficulty each kind of solution must
%%% meet. What is here is which nonces are visited, in which order, and the
%%% shape of what was found.
%%%
%%% Chunks come from a `weave' message, which answers two keys. `range' returns
%%% the packed chunks of a span, which is what a pass hashes; `chunk-proof'
%%% returns the whole of a proof of access at one offset, which is what a
%%% solution carries. `~arweave-storage@2.9' answers both from the storage
%%% modules this node holds, and is the source when the caller names none. A
%%% span the source holds nothing in is a hole rather than a failure: the nonces
%%% it covers yield nothing and the pass continues, which is the conclusion a
%%% miner missing part of its own partition reaches too.
%%%
%%% One cost shapes the search. A replica-2.9 chunk is enciphered with 256 KiB
%%% of entropy assembled from thirty-two separate 8 MiB blobs, so deciphering
%%% one chunk is thirty-two RandomX runs. A partition is held packed and a nonce
%%% hashes the packed sub-chunk directly, so a pass deciphers nothing at all --
%%% only the chunk of the nonce that met the difficulty is unpacked, and only to
%%% fill the proof its block carries. `max-nonces' caps how many nonces a call
%%% spends. That bound is a ceiling on the cost of one call, not a cursor: every
%%% pass enumerates the range from its first nonce, so a bound shortens a search
%%% rather than moving it.
-module(dev_arweave_mining).
-implements(<<"arweave-mining@2.9">>).
-device_libraries([
    lib_arweave_accounts,
    lib_arweave_block,
    lib_arweave_candidate,
    lib_arweave_history,
    lib_arweave_miner,
    lib_arweave_paths,
    lib_arweave_placement,
    lib_arweave_state,
    lib_arweave_tx,
    lib_arweave_vdf_timeline
]).
-export([info/1, mine/3, solve/3, range/3, session/3, declare/3]).
-export([search/3, partitions/3]).
-export([start/3, stop/3, status/3]).
-include("include/hb.hrl").
-include("include/ar_consensus.hrl").

%%% This device, for the passes that search through its own `solve'.
-define(DEVICE, <<"arweave-mining@2.9">>).

%%% The leading zero bits a hash must have for a pass to report it as a partial
%%% proof. Nothing in consensus knows about partials: this is a monitoring
%%% signal, and the only one a miner has that is proportional to the work it is
%%% doing rather than to its luck.
%%%
%%% Twenty-two bits is one hash in four million. A partition searched at every
%%% step is 320 nonces a second, so a miner holding one whole partition reports
%%% one about every three and a half hours, and one holding two reports twice
%%% as often -- rare enough to read, frequent enough to notice stopping.
%%% `arweave-mining-partial-bits' moves it; a node that sets it to zero reports
%%% every hash, which is a thing to do to a test and not to a miner.
-define(PARTIAL_BITS, 22).

%%% The hook a block this node mined is announced on. Nothing in a pass
%%% publishes: what to do with a block is the operator's decision, and this is
%%% where they attach it.
-define(MINED_HOOK, <<"arweave-mined-block">>).

%% @doc Export only the search operations, leaving message manipulation to
%% `message@1.0'.
info(_Base) ->
    #{
        excludes => [<<"keys">>, <<"set">>, <<"set-path">>, <<"remove">>]
    }.

%% @doc Mine one bounded pass on top of the block this node is building on:
%% advance the nonce limiter, search each partition at each step it reaches, and
%% answer with the signed block the first solution entitles this node to.
%%
%% The block is checked before it is answered, by the same
%% `~arweave-block@2.9' validation every block this node accepts passes. A
%% miner that answered with a block it had not checked would be asking its
%% operator to publish one this node itself rejects.
%%
%% A pass does nothing outside itself: it does not publish the block, move the
%% tip, or announce anything. It runs the block through the
%% `arweave-mined-block' hook, which is where an operator attaches whatever
%% announcement their deployment calls for, and a handler that fails fails the
%% pass rather than losing its answer quietly.
%%
%% `parent' is the block being extended, this node's own tip when the caller
%% names none. `steps' is how many nonce limiter steps beyond it the pass
%% walks, `partitions' which partitions it searches at each, `max-nonces' how
%% many nonces it spends on each range of each, `transactions' what the block
%% carries, and `weave' where the chunks are read from. `timestamp' is the
%% moment the block is mined at: the retarget rule derives the difficulty from
%% it, so the search and the block it produces are run against one value.
mine(Base, Req, Opts) ->
    maybe
        {ok, Parent} ?= parent(Base, Req, Opts),
        State = lib_arweave_state:materialize_histories(Parent, Opts),
        Prev = lib_arweave_state:previous_block(State, Opts),
        Info = Prev#block.nonce_limiter_info,
        Step = Info#nonce_limiter_info.global_step_number,
        {ok, Timeline} ?=
            timeline(Prev, Step, requested_steps(Base, Req, Opts), Opts),
        steps(Timeline, pass(State, Prev, Base, Req, Opts), 0, Opts)
    end.

%% @doc How far past the parent a pass walks: at least none of the timeline, at
%% most the interval a block may carry, and by default one step.
requested_steps(Base, Req, Opts) ->
    min(
        ?NONCE_LIMITER_MAX_CHECKPOINTS_COUNT,
        max(0, hb_util:int(get_first(<<"steps">>, Base, Req, 1, Opts)))
    ).

%% @doc The nonce limiter outputs a pass searches, oldest first, each with the
%% step number it belongs to.
%%
%% One walk answers for the whole pass. The interval a block declares runs from
%% the parent's step to its own, so the interval of the pass's last step
%% contains the output of every step below it -- and walking to each step
%% separately would recompute the interval beneath it every time, which is
%% quadratic in a pass whose cost is already the timeline.
timeline(_Prev, _Step, 0, _Opts) ->
    {ok, []};
timeline(Prev, Step, Steps, Opts) ->
    maybe
        {ok, Info} ?=
            lib_arweave_candidate:nonce_limiter(Prev, Step + Steps, Opts),
        Outputs =
            lists:reverse(hb_maps:get(<<"steps">>, Info, [], Opts)),
        {ok, lists:zip(lists:seq(Step + 1, Step + length(Outputs)), Outputs)}
    end.

%% @doc The values every step of a pass is searched under: the chain state it
%% extends and the header form the derivations read, the address and weave the
%% search is bound to, and what the caller asked the pass to spend and carry.
pass(State, Prev, Base, Req, Opts) ->
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    Weave =
        get_first(
            <<"weave">>,
            Base,
            Req,
            #{ <<"device">> => <<"arweave-storage@2.9">> },
            Opts
        ),
    #{
        <<"state">> => State,
        <<"previous">> => Prev,
        <<"timestamp">> =>
            hb_util:int(
                get_first(
                    <<"timestamp">>,
                    Base,
                    Req,
                    os:system_time(second),
                    Opts
                )
            ),
        <<"reward-addr">> => hb_util:encode(ar_wallet:to_address(Wallet)),
        <<"weave-size">> => Prev#block.weave_size,
        <<"partitions">> => partitions(Weave, Base, Req, Opts),
        <<"transactions">> => get_first(<<"transactions">>, Base, Req, [], Opts),
        <<"weave">> => Weave,
        <<"max-nonces">> => get_first(<<"max-nonces">>, Base, Req, [], Opts)
    }.

%% @doc Search one partition at one VDF step, and return the first solution the
%% difficulty admits.
%%
%% The session is the tip a miner is building on, restated as keys: the step's
%% `nonce-limiter-output' and `global-step-number', the `seed' of the block
%% below it, the `partition-number' being searched with the
%% `partition-upper-bound' and `weave-size' the weave stands at, the
%% `reward-addr' the partition is packed to with its `packing-difficulty' and
%% `replica-format', and the `diff' and `height' a solution must satisfy.
%% `weave' is the source the chunks are read from, `~arweave-storage@2.9' if the
%% caller names none, and `max-nonces' bounds how many nonces this call
%% examines.
%%
%% A solution carries the nonce that found it, the hash and preimage the block
%% header commits to, the bytes each range recalled and a proof of access for
%% each, and the session fields a block producer fills its header from. A
%% one-chunk solution omits `recall-byte2' and `poa2' entirely, because the
%% block carrying it declares neither.
%%
%% A pass that finds nothing is not a failure: it reports the number of nonces
%% it examined, which is what tells its caller where the next pass begins.
solve(Base, Req, Opts) ->
    PackingDifficulty =
        hb_util:int(required(<<"packing-difficulty">>, Base, Req, Opts)),
    ReplicaFormat =
        hb_util:int(required(<<"replica-format">>, Base, Req, Opts)),
    maybe
        ok ?=
            replicable(
                hb_util:int(required(<<"height">>, Base, Req, Opts)),
                PackingDifficulty,
                ReplicaFormat
            ),
        {ok, H0, Range1, Range2} ?= ranges(Base, Req, Opts),
        Session =
            #{
                <<"h0">> => H0,
                <<"range1-start">> => Range1,
                <<"range2-start">> => Range2,
                <<"packing-difficulty">> => PackingDifficulty,
                <<"packing">> => packing(PackingDifficulty, Base, Req, Opts),
                <<"diff-pair">> => diff_pair(Base, Req, Opts),
                <<"weave">> =>
                    get_first(
                        <<"weave">>,
                        Base,
                        Req,
                        #{ <<"device">> => <<"arweave-storage@2.9">> },
                        Opts
                    ),
                <<"weave-size">> =>
                    hb_util:int(required(<<"weave-size">>, Base, Req, Opts)),
                <<"partition-number">> =>
                    hb_util:int(
                        required(<<"partition-number">>, Base, Req, Opts)),
                <<"replica-format">> => ReplicaFormat,
                <<"reward-addr">> => required(<<"reward-addr">>, Base, Req, Opts),
                <<"nonce-limiter-output">> =>
                    required(<<"nonce-limiter-output">>, Base, Req, Opts),
                <<"global-step-number">> =>
                    hb_util:int(
                        required(<<"global-step-number">>, Base, Req, Opts)
                    )
            },
        {ok, Chunks1} ?= recall_chunks(<<"range1-start">>, Session, Opts),
        {ok, Chunks2} ?= recall_chunks(<<"range2-start">>, Session, Opts),
        search(
            nonces(Session, Base, Req, Opts),
            Session#{
                <<"chunks1">> => Chunks1,
                <<"chunks2">> => Chunks2
            },
            0,
            Opts
        )
    end.

%% @doc Return the ranges a session recalls and the chunks a search over it
%% would read: `h0', the two range starts, the number of `nonces' the pass
%% enumerates, and one entry in `chunks' per group of nonces that share a chunk,
%% naming the first `nonce' of the group and the `recall-byte' and
%% `recall-byte2' each range yields for it.
%%
%% A byte at or beyond the end of the weave is omitted exactly as the search
%% skips it. A group with no first byte is not listed at all, because no nonce
%% of it can be hashed; a group with no second byte is listed without one,
%% because its nonces can still yield a one-chunk solution.
%%
%% The key answers what a pass would do without doing it: it reads no chunks,
%% packs nothing, and takes only the keys that mapping is determined by --
%% `nonce-limiter-output', `seed', `reward-addr', `partition-number',
%% `partition-upper-bound', `packing-difficulty', `weave-size', and the same
%% optional `max-nonces' a search is bounded with.
range(Base, Req, Opts) ->
    PackingDifficulty =
        hb_util:int(required(<<"packing-difficulty">>, Base, Req, Opts)),
    maybe
        {ok, H0, Range1, Range2} ?= ranges(Base, Req, Opts),
        Session =
            #{
                <<"h0">> => H0,
                <<"range1-start">> => Range1,
                <<"range2-start">> => Range2,
                <<"packing-difficulty">> => PackingDifficulty,
                <<"weave-size">> =>
                    hb_util:int(required(<<"weave-size">>, Base, Req, Opts))
            },
        {ok, Groups} ?= groups(Session, Base, Req, Opts),
        {ok, Chunks} ?= chunks(Groups, Session, Opts),
        {ok,
            #{
                <<"h0">> => H0,
                <<"range1-start">> => Range1,
                <<"range2-start">> => Range2,
                <<"nonces">> => lists:sum([Count || {_First, Count} <- Groups]),
                <<"chunks">> => Chunks
            }
        }
    end.

%% @doc Start mining and keep mining, on the newest nonce-limiter step this
%% node has, for as long as the node runs. One call.
%%
%% The session that results follows `lib_arweave_vdf_timeline', which is
%% re-anchored on every block this node validates -- so a block landing moves
%% the search onto it without the caller doing anything. Every argument `mine'
%% takes, this takes: `partitions', `weave', `max-nonces', `transactions', and
%% `workers' for how many searches may run at once.
%%
%% Answers with what the session is doing, not with a block. A miner that
%% answered with a block would be a miner that had stopped.
start(Base, Req, Opts) ->
    lib_arweave_miner:start(hb_maps:merge(Base, Req, Opts), Opts).

%% @doc Stop searching. The session survives and starts again on the next
%% `start'.
stop(_Base, _Req, Opts) ->
    lib_arweave_miner:stop(Opts).

%% @doc What the session is doing: the newest step it has seen, the newest it
%% has searched, how far behind that leaves it, and the counters since it
%% started. `behind' is the number to watch -- it is how many steps of the
%% chain this machine could not keep up with.
status(_Base, _Req, Opts) ->
    maybe
        {ok, Status} ?= lib_arweave_miner:status(Opts),
        {ok, Status#{ <<"timeline">> => timeline_head(Opts) }}
    end.

%% @doc Where the node's nonce limiter has reached, beside where the session
%% has. The two together answer the only question an operator has: a session
%% that is behind is a machine that cannot keep up, and a session level with a
%% timeline that has stopped is a node waiting for a block rather than a miner
%% that has failed.
timeline_head(Opts) ->
    case lib_arweave_vdf_timeline:head(Opts) of
        not_running -> <<"not-running">>;
        Head -> Head
    end.

%% @doc Search one partition at one step, deriving what that step is searched
%% under and turning a solution into a checked block.
%%
%% This is `session', `solve' and `declare' composed, and it exists so that a
%% mining session can dispatch one resolution per partition per step and hold
%% nothing itself. The derivation belongs on this side of the boundary: it
%% reads the parent and the histories, which is work a scheduler must not be
%% doing in the loop it takes steps on.
%%
%% A step the parent has already passed is `stale-step-number' from `session',
%% and it is returned rather than raised: a timeline running behind the chain
%% offers stale steps continuously, and that is a fact about the machine for a
%% caller to count, not an error to fail on.
search(Base, Req, Opts) ->
    Partition = required(<<"partition-number">>, Base, Req, Opts),
    case session(Base, Req, Opts) of
        {ok, Session} ->
            searched(
                hb_ao:resolve(
                    Session#{
                        <<"device">> => ?DEVICE,
                        <<"partition-number">> => Partition
                    },
                    <<"solve">>,
                    Opts
                ),
                Session,
                Opts
            );
        {error, Error} ->
            {ok,
                #{
                    <<"solution">> => false,
                    <<"nonces-searched">> => 0,
                    <<"stale">> => stale(Error, Opts)
                }
            }
    end.

%% @doc Whether a refusal was the step being behind the parent, which is the
%% one a miner meets in normal running.
stale(Error, Opts) ->
    hb_maps:get(<<"message">>, Error, <<>>, Opts) == <<"stale-step-number">>.

%% @doc Carry the search's answer, and build the block when it found one.
searched({ok, Result}, Session, Opts) ->
    case hb_maps:get(<<"solution">>, Result, false, Opts) of
        true -> declared(Result, Session, Opts);
        false -> {ok, Result#{ <<"stale">> => false }}
    end;
searched({error, Error}, _Session, _Opts) ->
    {error, Error}.

declared(Solution, Session, Opts) ->
    maybe
        {ok, Block} ?=
            declare(
                Session#{ <<"device">> => ?DEVICE },
                #{ <<"solution">> => Solution },
                Opts
            ),
        {ok,
            Solution#{
                <<"stale">> => false,
                <<"block">> => hb_maps:get(<<"block">>, Block, not_found, Opts)
            }
        }
    end.

%% @doc The partitions a weave source holds. A mining session asks once and
%% searches them at every step; they change only when the modules do.
partitions(Base, Req, Opts) ->
    Weave =
        get_first(
            <<"weave">>,
            Base,
            Req,
            #{ <<"device">> => <<"arweave-storage@2.9">> },
            Opts
        ),
    {ok,
        #{
            <<"partitions">> =>
                hb_util:list_to_numbered_message(
                    partitions(Weave, Base, Req, Opts))
        }
    }.

%% @doc The message a search of one step runs against: everything `solve' needs
%% but the partition, and the partitions to search.
%%
%% This is the seam between the scheduler and the search. Every value in it is
%% a derivation of the parent block and the step -- the difficulty the retarget
%% rule gives that height and timestamp, the seed the step falls under, the
%% upper bound a recall range is drawn from -- so a caller holding one of these
%% holds everything a step's search is determined by, and nothing about how the
%% search is driven.
%%
%% The step is an argument rather than something read from a clock, because the
%% seed data depends on it: a step past an entropy reset line takes the epoch
%% after the line, and asking for the wrong step would search the wrong weave.
session(Base, Req, Opts) ->
    Step = hb_util:int(required(<<"global-step-number">>, Base, Req, Opts)),
    Output = required(<<"nonce-limiter-output">>, Base, Req, Opts),
    maybe
        {ok, Parent} ?= parent(Base, Req, Opts),
        State = lib_arweave_state:materialize_histories(Parent, Opts),
        Prev = lib_arweave_state:previous_block(State, Opts),
        Pass = pass(State, Prev, Base, Req, Opts),
        {ok, Parameters} ?=
            lib_arweave_candidate:parameters(
                Prev,
                Step,
                hb_util:int(field(<<"timestamp">>, Pass, Opts)),
                Opts
            ),
        {ok,
            Parameters#{
                <<"nonce-limiter-output">> => Output,
                <<"global-step-number">> => Step,
                <<"state">> => State,
                <<"previous">> => Prev,
                <<"timestamp">> => field(<<"timestamp">>, Pass, Opts),
                <<"transactions">> => field(<<"transactions">>, Pass, Opts),
                <<"reward-addr">> => field(<<"reward-addr">>, Pass, Opts),
                <<"weave-size">> => field(<<"weave-size">>, Pass, Opts),
                <<"weave">> => field(<<"weave">>, Pass, Opts),
                <<"max-nonces">> => field(<<"max-nonces">>, Pass, Opts),
                <<"partitions">> =>
                    hb_util:list_to_numbered_message(
                        field(<<"partitions">>, Pass, Opts))
            }
        }
    end.

%% @doc Turn a solution into the signed block it entitles this node to, check
%% it, and hand it to the `arweave-mined-block' hook.
%%
%% Resolved on a `session' message, so the block is built against exactly the
%% parent and step the search ran under rather than against whatever the tip
%% has become since -- a solution found on one parent is not a solution on
%% another.
declare(Base, Req, Opts) ->
    Solution = required(<<"solution">>, Base, Req, Opts),
    maybe
        {ok, Block} ?= produce(Solution, Base, Opts),
        ok ?= valid(Block, Base, Opts),
        % The hook's own answer is discarded, for the reason `mine' gives.
        {ok, _Announced} ?= hb_hook:on(?MINED_HOOK, Block, Opts),
        {ok, #{ <<"mined">> => true, <<"block">> => Block }}
    end.

%%% Internal functions.

%% @doc Hold only for a session the protocol admits a solution from. The
%% replication format and the packing difficulty are a pair, and a pass over a
%% pair no block of this height may declare can only produce blocks nothing
%% accepts -- including, at packing difficulty 0, one whose nonces address no
%% sub-chunk at all.
replicable(Height, PackingDifficulty, ReplicaFormat) ->
    case
        ar_block:validate_replica_format(
            Height, PackingDifficulty, ReplicaFormat)
    of
        true -> ok;
        false ->
            {error, error_message(<<"invalid-replica-format">>,
                <<"No block of this height may declare that replication "
                    "format at that packing difficulty.">>)}
    end.

%% @doc Hold only for a weave a recall range can be drawn from. Both range
%% starts are taken modulo the upper bound, so a weave with none has no range
%% to search rather than a range of nothing.
searchable(0) ->
    {error, error_message(<<"empty-weave">>,
        <<"A weave with no partition upper bound has nothing to mine.">>)};
searchable(_PartitionUpperBound) ->
    ok.

%% @doc The block a pass extends: the one the caller named, or this node's own
%% tip. A node with no chain has nothing to mine on, which is a state of the
%% node rather than a bad request.
parent(Base, Req, Opts) ->
    case get_first(<<"parent">>, Base, Req, [], Opts) of
        [] -> tip(Opts);
        Parent -> {ok, Parent}
    end.

tip(Opts) ->
    case hb_ao:resolve(#{ <<"device">> => <<"arweave@2.9">> }, <<"tip">>, Opts) of
        {ok, Tip} ->
            {ok, Tip};
        {error, _} ->
            {error,
                #{
                    <<"status">> => 404,
                    <<"message">> => <<"missing-tip">>,
                    <<"detail">> =>
                        <<"This node holds no chain to mine on, and the "
                            "request named no parent block.">>
                }
            }
    end.

%% @doc The partitions a pass searches: the ones the caller named, or the ones
%% the weave says it holds.
%%
%% A miner searches what it stores, so a pass that was told nothing asks the
%% source which partitions its modules cover rather than guessing. A source that
%% does not answer for its modules -- one built for a vector, or a peer -- leaves
%% the first partition, which is the only legal choice on any weave below one
%% partition anyway.
partitions(Weave, Base, Req, Opts) ->
    case get_first(<<"partitions">>, Base, Req, [], Opts) of
        [] -> held(Weave, Opts);
        Partitions ->
            [
                hb_util:int(Partition)
            ||
                Partition <-
                    hb_util:message_to_ordered_list(Partitions, Opts)
            ]
    end.

%% @doc The partitions a weave source holds, oldest first. A module's range may
%% span more than one, and two modules may share one, so the numbers are taken
%% from the range each covers and deduplicated.
held(Weave, Opts) ->
    case catch hb_ao:resolve(Weave, <<"modules">>, Opts) of
        {ok, Modules} -> covered(Modules, Opts);
        _Other -> [0]
    end.

covered(Modules, Opts) ->
    case
        lists:usort(
            lists:flatmap(
                fun(Module) -> spanned(Module, Opts) end,
                hb_util:message_to_ordered_list(Modules, Opts)
            )
        )
    of
        [] -> [0];
        Partitions -> Partitions
    end.

%% @doc The partition numbers one module's range covers.
spanned(Module, Opts) ->
    Start = hb_util:int(hb_maps:get(<<"range-start">>, Module, 0, Opts)),
    End = hb_util:int(hb_maps:get(<<"range-end">>, Module, 0, Opts)),
    Size = ar_block:partition_size(),
    lists:seq(Start div Size, max(Start, End - 1) div Size).

%% @doc Walk the nonce limiter forward one step at a time, searching each step
%% and stopping at the first solution. The forward walk is the one
%% `lib_arweave_candidate' performs for the block itself, so the entropy a step
%% is searched under is the entropy the block goes on to declare.
steps([], _Pass, Searched, _Opts) ->
    {ok,
        #{
            <<"mined">> => false,
            <<"nonces-searched">> => Searched
        }
    };
steps([{StepNumber, Output} | Rest], Pass, Searched, Opts) ->
    Prev = field(<<"previous">>, Pass, Opts),
    maybe
        {ok, Parameters} ?=
            lib_arweave_candidate:parameters(
                Prev,
                StepNumber,
                hb_util:int(field(<<"timestamp">>, Pass, Opts)),
                Opts
            ),
        step(
            field(<<"partitions">>, Pass, Opts),
            Parameters#{
                <<"nonce-limiter-output">> => Output,
                <<"global-step-number">> => StepNumber
            },
            Rest,
            Pass,
            Searched,
            Opts
        )
    end.

%% @doc Search each partition of one step in turn.
step([], _Parameters, StepNumbers, Pass, Searched, Opts) ->
    steps(StepNumbers, Pass, Searched, Opts);
step([Partition | Partitions], Parameters, StepNumbers, Pass, Searched, Opts) ->
    maybe
        {ok, Result} ?= search_partition(Partition, Parameters, Pass, Opts),
        Examined =
            Searched
                + hb_util:int(
                    hb_maps:get(<<"nonces-searched">>, Result, 0, Opts)),
        found(
            hb_maps:get(<<"solution">>, Result, false, Opts),
            Result,
            Partitions,
            Parameters,
            StepNumbers,
            Pass,
            Examined,
            Opts
        )
    end.

%% @doc Search one partition at one step, through this device's own `solve'.
search_partition(Partition, Parameters, Pass, Opts) ->
    hb_ao:resolve(
        Parameters#{
            <<"device">> => ?DEVICE,
            <<"partition-number">> => Partition,
            <<"reward-addr">> => field(<<"reward-addr">>, Pass, Opts),
            <<"weave-size">> => field(<<"weave-size">>, Pass, Opts),
            <<"weave">> => field(<<"weave">>, Pass, Opts),
            <<"max-nonces">> => field(<<"max-nonces">>, Pass, Opts)
        },
        <<"solve">>,
        Opts
    ).

%% @doc Carry on searching, or build the block a solution entitles this node to.
found(false, _Result, Partitions, Parameters, StepNumbers, Pass, Searched,
        Opts) ->
    step(Partitions, Parameters, StepNumbers, Pass, Searched, Opts);
found(true, Solution, _Partitions, _Parameters, _StepNumbers, Pass, Searched,
        Opts) ->
    maybe
        {ok, Block} ?= produce(Solution, Pass, Opts),
        ok ?= valid(Block, Pass, Opts),
        % The hook's own answer is discarded. A handler is where an operator
        % attaches an announcement, not where the block is decided: answering
        % with what a handler returned would let one substitute a message this
        % pass never checked, under a result that says it was mined.
        {ok, _Announced} ?= hb_hook:on(?MINED_HOOK, Block, Opts),
        {ok,
            #{
                <<"mined">> => true,
                <<"block">> => Block,
                <<"nonces-searched">> => Searched
            }
        }
    end.

%% @doc Build the signed block a solution extends the parent with.
produce(Solution, Pass, Opts) ->
    hb_ao:resolve(
        block(Pass, Opts),
        #{
            <<"path">> => <<"produce">>,
            <<"solution">> => Solution,
            <<"transactions">> => field(<<"transactions">>, Pass, Opts),
            <<"timestamp">> => field(<<"timestamp">>, Pass, Opts)
        },
        Opts
    ).

%% @doc Require the block just built to be one this node accepts, under every
%% check it applies to a block from anyone else.
valid(Block, Pass, Opts) ->
    maybe
        {ok, Result} ?=
            hb_ao:resolve(
                block(Pass, Opts),
                #{
                    <<"path">> => <<"validate">>,
                    <<"next">> => Block,
                    <<"transactions">> => field(<<"transactions">>, Pass, Opts)
                },
                Opts
            ),
        accepted(hb_maps:get(<<"valid">>, Result, false, Opts))
    end.

%% @doc Hold only for a block this node's own validation accepted.
accepted(true) ->
    ok;
accepted(false) ->
    {error, error_message(<<"invalid-mined-block">>,
        <<"The block this pass built is not one this node accepts.">>)}.

%% @doc The parent, addressed as the device that applies blocks to it.
block(Pass, Opts) ->
    (field(<<"state">>, Pass, Opts))#{
        <<"device">> => <<"arweave-block@2.9">>
    }.


%% @doc Compute the mining entropy of a partition at a VDF step, and the two
%% ranges of the weave it recalls. Both are `~arweave-spora@2.9' resolutions:
%% H0 is the single RandomX hash of a search, and the ranges are read out of it.
%% The `seed' is the one the block below the solution carries, which is the
%% caller's to establish -- a reset line crossed since then leaves the new
%% block's own seed different from the one H0 was taken over.
ranges(Base, Req, Opts) ->
    PartitionNumber =
        hb_util:int(required(<<"partition-number">>, Base, Req, Opts)),
    UpperBound =
        hb_util:int(required(<<"partition-upper-bound">>, Base, Req, Opts)),
    maybe
        ok ?= searchable(UpperBound),
        {ok, Entropy} ?=
            hb_ao:resolve(
                #{
                    <<"device">> => <<"arweave-spora@2.9">>,
                    <<"nonce-limiter-output">> =>
                        required(<<"nonce-limiter-output">>, Base, Req, Opts),
                    <<"partition-number">> => PartitionNumber,
                    <<"seed">> => required(<<"seed">>, Base, Req, Opts),
                    <<"reward-addr">> =>
                        required(<<"reward-addr">>, Base, Req, Opts),
                    <<"packing-difficulty">> =>
                        required(<<"packing-difficulty">>, Base, Req, Opts)
                },
                <<"h0">>,
                Opts
            ),
        H0 = hb_maps:get(<<"h0">>, Entropy, not_found, Opts),
        {ok, Recall} ?=
            hb_ao:resolve(
                #{
                    <<"device">> => <<"arweave-spora@2.9">>,
                    <<"h0">> => H0,
                    <<"partition-number">> => PartitionNumber,
                    <<"partition-upper-bound">> => UpperBound
                },
                <<"recall-range">>,
                Opts
            ),
        {ok,
            H0,
            hb_util:int(hb_maps:get(<<"range1-start">>, Recall, not_found, Opts)),
            hb_util:int(hb_maps:get(<<"range2-start">>, Recall, not_found, Opts))
        }
    end.

%% @doc Split the nonces of a pass into the groups that share a chunk. The
%% nonces of a range run from zero to the maximum the packing difficulty
%% defines, and consecutive nonces are the successive sub-chunks of one chunk of
%% each range -- so a group is read once and walked, rather than read per nonce.
%% `max-nonces' shortens the pass, and shortens its last group with it.
%%
%% Where one chunk's nonces end and the next chunk's begin is read from the
%% recall byte the protocol gives each nonce, rather than assumed from the
%% number of sub-chunks a chunk holds. The step points are a property of the
%% nonce alone, so they fall at the same nonces in both ranges.
groups(Session, Base, Req, Opts) ->
    maybe
        {ok, Bytes} ?= bytes(nonces(Session, Base, Req, Opts), Session, Opts),
        {ok, runs(Bytes)}
    end.

%% @doc The number of nonces a pass examines: the bound it was given, or the
%% whole range when it was given none.
bound([], Whole) -> Whole;
bound(Nonces, _Whole) -> hb_util:int(Nonces).

%% @doc Return the byte each nonce of a pass recalls from the first range.
bytes([], _Session, _Opts) ->
    {ok, []};
bytes([Nonce | Nonces], Session, Opts) ->
    maybe
        {ok, Byte, _Index} ?= recall(<<"range1-start">>, Nonce, Session, Opts),
        {ok, Rest} ?= bytes(Nonces, Session, Opts),
        {ok, [{Nonce, Byte} | Rest]}
    end.

%% @doc Gather the consecutive nonces recalling one byte into one group, as the
%% first nonce of the group and the number of nonces in it.
runs([]) ->
    [];
runs([{Nonce, Byte} | Rest]) ->
    {Same, Next} = lists:splitwith(fun({_Nonce, B}) -> B == Byte end, Rest),
    [{Nonce, length(Same) + 1} | runs(Next)].

%% @doc Read the chunks of one of a session's recall ranges from the weave.
%%
%% One resolution reads the whole range, which is what a storage module answers
%% in one read of one file. A range is 2.5 MiB at replica-2.9's packing
%% difficulty, so the ten chunks of it are held for the pass rather than fetched
%% again per nonce, and the pass walks nonces over what it read.
%%
%% The chunks come back packed, in the form the partition holds them, because
%% that is the form a nonce hashes. Nothing is unpacked here: deciphering one
%% chunk costs thirty-two 8 MiB RandomX runs, and a pass that paid that per
%% chunk examined would spend its whole budget on nonces that found nothing.
recall_chunks(Range, Session, Opts) ->
    Packing = field(<<"packing">>, Session, Opts),
    Format = field(<<"format">>, Packing, Opts),
    maybe
        {ok, Result} ?=
            hb_ao:resolve(
                field(<<"weave">>, Session, Opts),
                #{
                    <<"path">> => <<"range">>,
                    <<"range-start">> => field(Range, Session, Opts),
                    <<"packing-difficulty">> =>
                        field(<<"packing-difficulty">>, Session, Opts),
                    <<"packing">> => Format,
                    <<"address">> => field(<<"reward-addr">>, Packing, Opts)
                },
                Opts
            ),
        ok ?= packed_as(Format, hb_maps:get(<<"packing">>, Result, <<>>, Opts)),
        ordered(hb_maps:get(<<"chunks">>, Result, #{}, Opts), Opts)
    end.

%% @doc Hold only when a source answers in the packing it was asked for.
%%
%% Nothing downstream can tell the difference. A nonce slices the sub-chunk it
%% addresses out of whatever bytes it is given and hashes them, so a source
%% answering with unpacked chunks would yield hashes that meet no difficulty and
%% a partition that appears to hold nothing -- which reads as bad luck rather
%% than as a misconfigured source.
packed_as(Format, Format) ->
    ok;
packed_as(Format, Answered) ->
    {error, error_message(<<"unsupported-packing">>,
        <<"This node mines a partition packed as `", Format/binary,
            "', and the source answered with one packed as `",
            (hb_util:bin(Answered))/binary, "'.">>)}.

%% @doc Read a range's answer as the pairs a search walks: each chunk's absolute
%% end offset and its packed bytes, oldest first.
ordered(Chunks, Opts) ->
    Pairs =
        [
            {
                hb_util:int(field(<<"absolute-end-offset">>, Chunk, Opts)),
                field(<<"chunk">>, Chunk, Opts)
            }
        ||
            Chunk <- hb_util:message_to_ordered_list(Chunks, Opts)
        ],
    maybe
        ok ?= whole(Pairs),
        {ok, lists:sort(Pairs)}
    end.

%% @doc Hold only when every chunk of a range is a chunk's worth of bytes.
%%
%% A range carries its chunks as bytes rather than as base64url, because a pass
%% hashes them and drops them and encoding a range costs a third of what reading
%% it does. That makes the encoding part of the contract rather than a detail,
%% and an encoded answer is one nothing downstream could tell from a packed one:
%% a nonce would slice a sub-chunk out of the text and hash it, and the
%% partition would read as holding nothing. `packed_as/2' refuses the same
%% silence one layer up.
whole([]) ->
    ok;
whole([{_EndOffset, Chunk} | Rest]) when byte_size(Chunk) == ?DATA_CHUNK_SIZE ->
    whole(Rest);
whole([{EndOffset, Chunk} | _Rest]) ->
    {error, error_message(<<"malformed-range">>,
        <<"A range carries each chunk as ",
            (hb_util:bin(?DATA_CHUNK_SIZE))/binary,
            " bytes, and the source answered with ",
            (hb_util:bin(byte_size(Chunk)))/binary,
            " at offset ", (hb_util:bin(EndOffset))/binary,
            ". A source reached over a codec that cannot carry bytes answers "
            "with text.">>)}.

%% @doc The nonces a pass examines: the whole range, or the bound it was given,
%% whichever is fewer. A bound shortens a search rather than moving it -- every
%% pass enumerates a range from its first nonce.
nonces(Session, Base, Req, Opts) ->
    Whole =
        ar_block:get_max_nonce(
            field(<<"packing-difficulty">>, Session, Opts)) + 1,
    Count =
        max(
            0,
            min(
                Whole,
                bound(get_first(<<"max-nonces">>, Base, Req, [], Opts), Whole)
            )
        ),
    lists:seq(0, Count - 1).

%% @doc Return the chunk of a range holding a byte, or `not_found'. A chunk
%% covers the 256 KiB below its own end offset, which is where the recall byte
%% of every nonce reading it falls.
%%
%% The scan is linear over a range this node has already read: ten chunks at
%% replica-2.9's difficulty, four hundred at packing difficulty zero. A map
%% would not help, because a chunk's end offset is not derivable from a byte
%% inside it -- below the strict data split threshold nothing is bucket aligned.
containing(_Byte, []) ->
    not_found;
containing(Byte, [{EndOffset, Chunk} | Rest]) ->
    case Byte >= EndOffset - ?DATA_CHUNK_SIZE andalso Byte < EndOffset of
        true -> {ok, EndOffset, Chunk};
        false -> containing(Byte, Rest)
    end.

%% @doc Examine the nonces of a pass in order, stopping at the first solution. A
%% nonce is counted as searched once its first hash has been taken, so the count
%% a pass reports is the work it did rather than the range it walked over.
search([], _Session, Searched, Opts) ->
    ?event(arweave_mining, {pass_complete, {searched, Searched}}, Opts),
    {ok,
        #{
            <<"solution">> => false,
            <<"nonces-searched">> => Searched
        }
    };
search([Nonce | Nonces], Session, Searched, Opts) ->
    case nonce(Nonce, Session, Opts) of
        {ok, Found, Examined} ->
            proven(Found, Searched + Examined, Session, Opts);
        {continue, Examined} ->
            search(Nonces, Session, Searched + Examined, Opts);
        {error, Error} ->
            {error, Error}
    end.

%% @doc Examine one nonce: take the sub-chunk of the first range it points at
%% and hash it into H1. A nonce whose chunk this node does not hold is not
%% counted as searched -- no hash was taken over it -- which is what makes a
%% pass over a partition with holes report the work it actually did.
nonce(Nonce, Session, Opts) ->
    maybe
        {ok, Byte, Index} ?= recall(<<"range1-start">>, Nonce, Session, Opts),
        examined(
            within(Byte, field(<<"chunks1">>, Session, Opts), Session, Opts),
            Nonce,
            Byte,
            Index,
            Session,
            Opts
        )
    end.

%% @doc Return the chunk of a range holding a byte of the weave the session
%% names. A byte at or beyond the end of that weave is skipped whatever the
%% source holds there: the weave a session names is the one a solution is proved
%% against, and a byte past its end has no block to prove against at all -- so a
%% source whose own copy runs longer, as a fork's would, cannot draw the search
%% into a solution nothing can validate.
within(Byte, Chunks, Session, Opts) ->
    case Byte >= hb_util:int(field(<<"weave-size">>, Session, Opts)) of
        true -> not_found;
        false -> containing(Byte, Chunks)
    end.

%% @doc Hash the nonce against the chunk its first recall byte falls in, or move
%% on where this node holds no chunk there.
examined(not_found, _Nonce, _Byte, _Index, _Session, _Opts) ->
    {continue, 0};
examined({ok, EndOffset, Chunk}, Nonce, Byte, Index, Session, Opts) ->
    maybe
        {ok, SubChunk} ?= sub_chunk(Chunk, Index),
        {ok, Hash, Preimage} ?=
            hash(
                <<"h1">>,
                #{
                    <<"h0">> => field(<<"h0">>, Session, Opts),
                    <<"nonce">> => Nonce,
                    <<"chunk">> => hb_util:encode(SubChunk)
                },
                Opts
            ),
        partial(<<"h1">>, Hash, Nonce, Byte, Session, Opts),
        second(
            passes(<<"h1">>, Hash, Session, Opts),
            #{
                <<"nonce">> => Nonce,
                <<"recall-byte">> => Byte,
                <<"end-offset">> => EndOffset,
                <<"sub-chunk">> => SubChunk,
                <<"sub-chunk-index">> => Index,
                <<"solution-hash">> => Hash,
                <<"hash-preimage">> => Preimage
            },
            Index,
            Session,
            Opts
        )
    end.

%% @doc Decide whether the second range is consulted for a nonce. A hash that
%% already meets the one-chunk difficulty is a solution as it stands, and
%% upstream drops the nonce's second chunk on the spot: a second proof would
%% only bind the solution to a chunk the block it goes into does not declare.
second(true, Found, _Index, _Session, _Opts) ->
    {ok, Found, 1};
second(false, Found, Index, Session, Opts) ->
    maybe
        {ok, Byte2, _Index2} ?=
            recall(<<"range2-start">>, field(<<"nonce">>, Found, Opts), Session,
                Opts),
        two_chunk(
            within(Byte2, field(<<"chunks2">>, Session, Opts), Session, Opts),
            Found#{ <<"recall-byte2">> => Byte2 },
            Index,
            Session,
            Opts
        )
    end.

%% @doc Take the second range's sub-chunk at the same nonce and hash it together
%% with the first into H2. A nonce whose second chunk this node does not hold
%% yields no two-chunk solution, and the pass moves on -- its first hash still
%% counts as work done.
two_chunk(not_found, _Found, _Index, _Session, _Opts) ->
    {continue, 1};
two_chunk({ok, EndOffset, Chunk}, Found, Index, Session, Opts) ->
    maybe
        {ok, SubChunk} ?= sub_chunk(Chunk, Index),
        {ok, Hash, Preimage} ?=
            hash(
                <<"h2">>,
                #{
                    <<"h0">> => field(<<"h0">>, Session, Opts),
                    <<"h1">> => field(<<"solution-hash">>, Found, Opts),
                    <<"chunk">> => hb_util:encode(SubChunk)
                },
                Opts
            ),
        partial(
            <<"h2">>,
            Hash,
            field(<<"nonce">>, Found, Opts),
            field(<<"recall-byte2">>, Found, Opts),
            Session,
            Opts
        ),
        solved(
            passes(<<"h2">>, Hash, Session, Opts),
            Found#{
                <<"end-offset2">> => EndOffset,
                <<"sub-chunk2">> => SubChunk,
                <<"solution-hash">> => Hash,
                <<"hash-preimage">> => Preimage
            }
        )
    end.

solved(true, Found) -> {ok, Found, 1};
solved(false, _Found) -> {continue, 1}.

%% @doc Take the sub-chunk of a packed chunk that one nonce hashes. At packing
%% difficulty zero a nonce addresses a whole chunk and there is no sub-chunk,
%% which the protocol denotes with an index of -1.
sub_chunk(Chunk, -1) ->
    sized(Chunk, ?DATA_CHUNK_SIZE);
sub_chunk(Chunk, Index) ->
    maybe
        ok ?= sliceable(Chunk, Index),
        {ok,
            binary:part(
                Chunk,
                Index * ?COMPOSITE_PACKING_SUB_CHUNK_SIZE,
                ?COMPOSITE_PACKING_SUB_CHUNK_SIZE
            )
        }
    end.

%% @doc Hold only for a chunk a sub-chunk index addresses. A source answering
%% with anything but a whole packed chunk is misconfigured rather than
%% incomplete, so it ends the pass naming what could not be used.
sliceable(Chunk, Index)
        when byte_size(Chunk) >= (Index + 1) * ?COMPOSITE_PACKING_SUB_CHUNK_SIZE ->
    ok;
sliceable(_Chunk, _Index) ->
    {error, error_message(<<"invalid-chunk-size">>,
        <<"The source answered with fewer bytes than the sub-chunk a nonce "
            "addresses.">>)}.

sized(Chunk, Size) when byte_size(Chunk) == Size ->
    {ok, Chunk};
sized(_Chunk, _Size) ->
    {error, error_message(<<"invalid-chunk-size">>,
        <<"The source answered with something other than a chunk of the "
            "weave.">>)}.

%% @doc Return the byte a nonce recalls from one of a session's ranges, and the
%% index of the sub-chunk it points at within the chunk holding that byte.
%% Both come from `~arweave-spora@2.9/recall-byte', so a search reads exactly
%% the bytes a validator recomputes for the solution it finds.
recall(Range, Nonce, Session, Opts) ->
    maybe
        {ok, Result} ?=
            hb_ao:resolve(
                #{
                    <<"device">> => <<"arweave-spora@2.9">>,
                    <<"range-start">> => field(Range, Session, Opts),
                    <<"nonce">> => Nonce,
                    <<"packing-difficulty">> =>
                        field(<<"packing-difficulty">>, Session, Opts)
                },
                <<"recall-byte">>,
                Opts
            ),
        {ok,
            hb_util:int(hb_maps:get(<<"recall-byte">>, Result, not_found, Opts)),
            hb_util:int(
                hb_maps:get(<<"sub-chunk-index">>, Result, not_found, Opts))
        }
    end.

%% @doc Build the proofs of access the solution carries into a block, and answer
%% with the solution once they are built.
%%
%% This is where the unpacked chunks are materialised, and it is the only place:
%% a proof of access at packing difficulty one or above carries the whole 256
%% KiB unpacked chunk beside the 8 KiB sub-chunk that was hashed, and producing
%% it costs thirty-two RandomX runs per chunk. A nonce that met the difficulty
%% is the one nonce in a partition-year for which that is worth paying.
proven(Found, Searched, Session, Opts) ->
    maybe
        {ok, PoA} ?=
            proof(
                field(<<"recall-byte">>, Found, Opts),
                field(<<"sub-chunk">>, Found, Opts),
                field(<<"sub-chunk-index">>, Found, Opts),
                Session,
                Opts
            ),
        {ok, Complete} ?= second_proof(Found#{ <<"poa">> => PoA }, Session, Opts),
        solution(Complete, Searched, Session, Opts)
    end.

%% @doc Build the second proof, for a solution that declares one. A one-chunk
%% solution declares neither a second recall byte nor a second proof, so nothing
%% is read for it.
second_proof(Found, Session, Opts) ->
    case hb_maps:get(<<"sub-chunk2">>, Found, not_found, Opts) of
        not_found ->
            {ok,
                hb_maps:without(
                    [
                        <<"sub-chunk">>,
                        <<"sub-chunk-index">>,
                        <<"end-offset">>
                    ],
                    Found
                )
            };
        SubChunk2 ->
            maybe
                {ok, PoA2} ?=
                    proof(
                        field(<<"recall-byte2">>, Found, Opts),
                        SubChunk2,
                        field(<<"sub-chunk-index">>, Found, Opts),
                        Session,
                        Opts
                    ),
                {ok,
                    hb_maps:without(
                        [
                            <<"sub-chunk">>,
                            <<"sub-chunk2">>,
                            <<"sub-chunk-index">>,
                            <<"end-offset">>,
                            <<"end-offset2">>
                        ],
                        Found#{ <<"poa2">> => PoA2 }
                    )
                }
            end
    end.

%% @doc Read the proof of access for one recall byte from the weave, and require
%% it to carry the sub-chunk this pass hashed.
%%
%% The check is the seam between the two reads. A pass hashes bytes a range read
%% out of a chunk file; a proof is built from bytes an index placed in the
%% weave. If those disagree -- an index ahead of its data, a chunk file written
%% under a different address -- the block would carry a proof of a chunk nothing
%% hashed, and the difference is invisible until a validator rejects it.
proof(Byte, SubChunk, Index, Session, Opts) ->
    Packing = field(<<"packing">>, Session, Opts),
    maybe
        {ok, Answer} ?=
            hb_ao:resolve(
                field(<<"weave">>, Session, Opts),
                #{
                    <<"path">> => <<"chunk-proof">>,
                    <<"offset">> => Byte,
                    <<"packing">> => field(<<"format">>, Packing, Opts),
                    <<"address">> => field(<<"reward-addr">>, Packing, Opts)
                },
                Opts
            ),
        ok ?=
            carries(
                SubChunk,
                Index,
                hb_util:decode(field(<<"chunk">>, Answer, Opts))
            ),
        {ok,
            #{
                <<"tx-path">> => field(<<"tx-path">>, Answer, Opts),
                <<"data-path">> => field(<<"data-path">>, Answer, Opts),
                <<"chunk">> => hb_util:encode(SubChunk),
                <<"unpacked-chunk">> =>
                    field(<<"unpacked-chunk">>, Answer, Opts)
            }
        }
    end.

%% @doc Hold only when the packed chunk a proof carries holds the sub-chunk that
%% was hashed, at the index the nonce addresses.
%%
%% The index is part of the check rather than a search for the bytes anywhere in
%% the chunk: a validator recomputes the sub-chunk from the nonce, so a chunk
%% carrying these bytes at some other index is a chunk that proves a different
%% solution.
carries(Chunk, -1, Chunk) ->
    ok;
carries(SubChunk, Index, Chunk)
        when byte_size(Chunk) >= (Index + 1) * ?COMPOSITE_PACKING_SUB_CHUNK_SIZE ->
    sliced(
        SubChunk,
        binary:part(
            Chunk,
            Index * ?COMPOSITE_PACKING_SUB_CHUNK_SIZE,
            ?COMPOSITE_PACKING_SUB_CHUNK_SIZE
        )
    );
carries(_SubChunk, _Index, _Chunk) ->
    mismatched().

sliced(SubChunk, SubChunk) -> ok;
sliced(_SubChunk, _Other) -> mismatched().

mismatched() ->
    {error, error_message(<<"chunk-mismatch">>,
        <<"The chunk this node proved is not the chunk it hashed.">>)}.

%% @doc Compute one of the two solution hashes through `~arweave-spora@2.9',
%% returning it with the preimage it was taken over -- which a block header
%% commits to alongside the hash itself.
hash(Key, Request, Opts) ->
    maybe
        {ok, Result} ?=
            hb_ao:resolve(
                Request#{ <<"device">> => <<"arweave-spora@2.9">> },
                Key,
                Opts
            ),
        {ok,
            hb_maps:get(<<"hash">>, Result, not_found, Opts),
            hb_maps:get(<<"preimage">>, Result, not_found, Opts)
        }
    end.

%% @doc Report a hash that cleared the partial-proof bar.
%%
%% A miner that finds nothing for a week is indistinguishable from a miner that
%% is not working, and at mainnet difficulty finding nothing for a week is the
%% expected case. Partials are the same search reported at a bar low enough to
%% clear regularly: their rate is the hash rate, so an operator can see that a
%% partition is being read and hashed, watch the rate move when a module
%% finishes packing, and notice it stop.
%%
%% They are worth nothing to anyone else and are never carried anywhere. This
%% emits an event and returns nothing.
partial(Kind, Hash, Nonce, Byte, Session, Opts) ->
    Bits = leading_zeros(hb_util:decode(Hash)),
    case Bits >= hb_util:int(hb_opts:get(<<"arweave-mining-partial-bits">>,
            ?PARTIAL_BITS, Opts)) of
        true ->
            ?event(arweave_mining,
                {partial_proof,
                    {kind, {string, Kind}},
                    {bits, Bits},
                    {nonce, Nonce},
                    {recall_byte, Byte},
                    {step,
                        hb_util:int(
                            field(<<"global-step-number">>, Session, Opts))},
                    {hash, {string, Hash}}
                },
                Opts
            );
        false ->
            ok
    end.

%% @doc How many zero bits a hash begins with. Counted rather than compared
%% against a threshold value because the number itself is the useful one: the
%% rate at each width says what the hash rate is, and a widening tail says the
%% miner has been running long enough to have been lucky.
leading_zeros(<<0:1, Rest/bitstring>>) ->
    1 + leading_zeros(Rest);
leading_zeros(_Hash) ->
    0.

%% @doc Check a solution hash against the difficulty its kind of solution must
%% meet: a one-chunk solution against the difficulty scaled by the protocol's
%% one-chunk multiplier, a two-chunk solution against the block's own.
passes(<<"h1">>, Hash, Session, Opts) ->
    ar_node_utils:h1_passes_diff_check(
        hb_util:decode(Hash),
        field(<<"diff-pair">>, Session, Opts),
        field(<<"packing-difficulty">>, Session, Opts)
    );
passes(<<"h2">>, Hash, Session, Opts) ->
    ar_node_utils:h2_passes_diff_check(
        hb_util:decode(Hash),
        field(<<"diff-pair">>, Session, Opts),
        field(<<"packing-difficulty">>, Session, Opts)
    ).

%% @doc Build the difficulty pair a solution is checked against: the difficulty
%% the block declares, and the one a one-chunk solution must meet, which is the
%% same number scaled by the protocol's one-chunk multiplier. Taken from the
%% vendored consensus code rather than restated, because a miner computing
%% either differently from its validators would mine solutions nothing accepts.
diff_pair(Base, Req, Opts) ->
    Diff = hb_util:int(required(<<"diff">>, Base, Req, Opts)),
    Height = hb_util:int(required(<<"height">>, Base, Req, Opts)),
    {ar_difficulty:poa1_diff(Diff, Height), Diff}.

%% @doc Name the packing the partition being searched is held in: the format the
%% replica format and packing difficulty determine, for the address mined to.
packing(PackingDifficulty, Base, Req, Opts) ->
    RewardAddr = required(<<"reward-addr">>, Base, Req, Opts),
    #{
        <<"format">> =>
            format(
                ar_block:get_packing(
                    PackingDifficulty,
                    hb_util:decode(RewardAddr),
                    hb_util:int(required(<<"replica-format">>, Base, Req, Opts))
                )
            ),
        <<"reward-addr">> => RewardAddr,
        <<"packing-difficulty">> => PackingDifficulty
    }.

%% @doc Name a packing format on the wire. The mapping is explicit rather than
%% derived, so an unhandled format is an error rather than a coerced atom.
format({spora_2_6, _Addr}) -> <<"spora-2-6">>;
format({composite, _Addr, _Difficulty}) -> <<"composite">>;
format({replica_2_9, _Addr}) -> <<"replica-2-9">>.

%% @doc Return the solution a pass found, in the shape a block producer builds a
%% header from: what the nonce found, and the session it was found in.
solution(Found, Searched, Session, Opts) ->
    ?event(arweave_mining,
        {solution_found,
            {nonce, field(<<"nonce">>, Found, Opts)},
            {searched, Searched}
        },
        Opts
    ),
    {ok,
        Found#{
            <<"solution">> => true,
            <<"partition-number">> =>
                field(<<"partition-number">>, Session, Opts),
            <<"packing-difficulty">> =>
                field(<<"packing-difficulty">>, Session, Opts),
            <<"replica-format">> => field(<<"replica-format">>, Session, Opts),
            <<"reward-addr">> => field(<<"reward-addr">>, Session, Opts),
            <<"h0">> => field(<<"h0">>, Session, Opts),
            <<"nonce-limiter-output">> =>
                field(<<"nonce-limiter-output">>, Session, Opts),
            <<"global-step-number">> =>
                field(<<"global-step-number">>, Session, Opts),
            <<"nonces-searched">> => Searched
        }
    }.

%% @doc List the chunks a pass would read, one entry per group of nonces.
chunks([], _Session, _Opts) ->
    {ok, []};
chunks([{First, _Count} | Groups], Session, Opts) ->
    maybe
        {ok, Byte1, _Index} ?= recall(<<"range1-start">>, First, Session, Opts),
        {ok, Byte2, _} ?= recall(<<"range2-start">>, First, Session, Opts),
        {ok, Rest} ?= chunks(Groups, Session, Opts),
        {ok,
            entry(
                field(<<"weave-size">>, Session, Opts),
                First,
                Byte1,
                Byte2
            ) ++ Rest
        }
    end.

%% @doc Describe one group of a pass: the first nonce of the group, and the byte
%% each range recalls for it. A group whose first byte lies beyond the end of
%% the weave describes nothing at all, because no nonce of it can be hashed.
entry(WeaveSize, _First, Byte1, _Byte2) when Byte1 >= WeaveSize ->
    [];
entry(WeaveSize, First, Byte1, Byte2) when Byte2 >= WeaveSize ->
    [#{ <<"nonce">> => First, <<"recall-byte">> => Byte1 }];
entry(_WeaveSize, First, Byte1, Byte2) ->
    [
        #{
            <<"nonce">> => First,
            <<"recall-byte">> => Byte1,
            <<"recall-byte2">> => Byte2
        }
    ].

%% @doc Read a field from the request, falling back to the base message.
%%
%% These are data fields, not keys to resolve, so they are read with
%% `hb_maps:get/4' rather than `hb_ao:get/4'. Resolving a key against a message
%% that names this device dispatches back into the device -- so reading the
%% `range' key of a `solve' request with `hb_ao:get' would enumerate a pass
%% rather than return what was supplied. `hb_maps:get/4' reads the value
%% directly while still loading it if it is a link.
get_first(Key, Base, Req, Default, Opts) ->
    case hb_maps:get(Key, Req, not_found, Opts) of
        not_found -> hb_maps:get(Key, Base, Default, Opts);
        Value -> Value
    end.

%% @doc Read a key that has no meaningful default.
required(Key, Base, Req, Opts) ->
    case get_first(Key, Base, Req, not_found, Opts) of
        not_found -> throw({'missing-key', Key});
        Value -> Value
    end.

%% @doc Read a field of a message that must carry it: a session the search
%% derived, a proof it built, or the answer a weave source gave -- each of which
%% may hold a value far larger than the message holding it.
field(Key, Message, Opts) ->
    case hb_maps:get(Key, Message, not_found, Opts) of
        not_found -> throw({'missing-key', Key});
        Value -> Value
    end.

%% @doc Build the standard error body.
error_message(Message, Detail) ->
    #{
        <<"status">> => 422,
        <<"message">> => Message,
        <<"detail">> => Detail
    }.
