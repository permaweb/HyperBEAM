%%% @doc The `#nonce_limiter_info{}' record boundary for `~arweave-vdf@2.9'.
%%%
%%% The vendored nonce limiter works on `#nonce_limiter_info{}' and `#block{}';
%%% the device works on dashed-key, `structured@1.0' messages. Every conversion
%%% between the two lives here, so `dev_arweave_vdf' holds no record and builds
%%% no block. The vendored calls that take a block -- `get_seed_data/2',
%%% `validate_last_step_checkpoints/4' and `compute_next_vdf_difficulty/1' --
%%% are wrapped here too, taking messages and returning messages or plain
%%% values. The chain verification itself takes only binaries and integers, so
%%% the device calls it directly.
%%%
%%% The message form uses the protocol's own field names, not the three
%%% Arweave's JSON encoder renames (`ar_serialize.erl:1564'):
%%% `partition-upper-bound' rather than `zone_upper_bound',
%%% `next-partition-upper-bound' rather than `next_zone_upper_bound', and
%%% `steps' rather than `checkpoints'. The last is a trap: JSON has no `steps'
%%% key at all, so a codec that looks for one yields an empty step list and the
%%% chain verifies vacuously.
%%%
%%% `steps' and `last-step-checkpoints' are newest-first in the message, as
%%% they are on the wire. Reversing them for the NIF, which wants ascending, is
%%% the device's business -- a codec that quietly reordered them would make the
%%% message disagree with the block it came from.
-module(lib_arweave_vdf).
-export([seed_data/5, last_step_checkpoints/4, next_vdf_difficulty/5]).
-export([check_step_range/3, threads/3]).
-include("include/hb.hrl").
-include("include/ar_consensus.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Convert a nonce limiter info message into the record. Every scalar is
%% required: the record's own defaults include `?INITIAL_VDF_DIFFICULTY', and a
%% block validated against that constant rather than against its own declared
%% difficulty would diverge from the network silently.
message_to_info(Info, Opts) when is_map(Info) ->
    #nonce_limiter_info{
        output = decode(<<"output">>, Info, Opts),
        prev_output = decode(<<"prev-output">>, Info, Opts),
        seed = decode(<<"seed">>, Info, Opts),
        next_seed = decode(<<"next-seed">>, Info, Opts),
        partition_upper_bound = int(<<"partition-upper-bound">>, Info, Opts),
        next_partition_upper_bound =
            int(<<"next-partition-upper-bound">>, Info, Opts),
        global_step_number = int(<<"global-step-number">>, Info, Opts),
        last_step_checkpoints =
            decode_list(<<"last-step-checkpoints">>, Info, Opts),
        steps = decode_list(<<"steps">>, Info, Opts),
        vdf_difficulty = int(<<"vdf-difficulty">>, Info, Opts),
        next_vdf_difficulty = int(<<"next-vdf-difficulty">>, Info, Opts)
    };
message_to_info(Link, Opts) when ?IS_LINK(Link) ->
    case hb_cache:ensure_loaded(Link, Opts) of
        Info when is_map(Info) -> message_to_info(Info, Opts);
        Invalid -> throw({'invalid-nonce-limiter-info', Invalid})
    end;
message_to_info(Invalid, _Opts) ->
    throw({'invalid-nonce-limiter-info', Invalid}).

%% @doc Return the seed data a block mined at `StepNumber' must declare, given
%% its parent. All five fields rotate together, and only when the step range
%% crosses a reset line; `prev-indep-hash' and `prev-weave-size' are the two
%% values the rotation draws from the parent's header rather than its info.
%%
%% The caller must have established that `StepNumber' is ahead of the parent's:
%% `ar_nonce_limiter:get_seed_data/2' asserts it with a match.
seed_data(StepNumber, PrevInfo, PrevIndepHash, PrevWeaveSize, Opts) ->
    PrevB =
        #block{
            nonce_limiter_info = message_to_info(PrevInfo, Opts),
            indep_hash = hb_util:decode(PrevIndepHash),
            weave_size = PrevWeaveSize
        },
    {Seed, NextSeed, UpperBound, NextUpperBound, VDFDifficulty} =
        ar_nonce_limiter:get_seed_data(StepNumber, PrevB),
    #{
        <<"seed">> => hb_util:encode(Seed),
        <<"next-seed">> => hb_util:encode(NextSeed),
        <<"partition-upper-bound">> => UpperBound,
        <<"next-partition-upper-bound">> => NextUpperBound,
        <<"vdf-difficulty">> => VDFDifficulty
    }.

%% @doc Recompute the 25 checkpoints of a block's final step. Proves that one
%% step of VDF work happened somewhere, anchored on the block's own unverified
%% `steps' list -- see `dev_arweave_vdf:verify_step/3' for why that is not a
%% validity claim.
last_step_checkpoints(Info, PrevInfo, Threads, Opts) ->
    B = #block{ nonce_limiter_info = message_to_info(Info, Opts) },
    PrevB = #block{ nonce_limiter_info = message_to_info(PrevInfo, Opts) },
    ar_nonce_limiter:validate_last_step_checkpoints(
        B,
        PrevB,
        last_step_prev_output(B),
        Threads
    ).

%% @doc Compute the VDF difficulty a block scheduled at `Height' + 1 must
%% declare, from its parent's two difficulties and block time history.
%%
%% The history is folded over only at a retarget height, but it is converted
%% whichever height this is: deciding otherwise would mean restating the
%% retarget condition here, and a second copy of a consensus rule is worse
%% than a list conversion. A caller off a retarget may pass no history at all.
next_vdf_difficulty(Height, VDFDifficulty, NextVDFDifficulty, History, Opts) ->
    PrevB =
        #block{
            height = Height,
            nonce_limiter_info =
                #nonce_limiter_info{
                    vdf_difficulty = VDFDifficulty,
                    next_vdf_difficulty = NextVDFDifficulty
                },
            block_time_history = message_to_history(History, Opts)
        },
    ar_block:compute_next_vdf_difficulty(PrevB).

%% @doc Check that a header carries the consensus suffix of an advancing range.
check_step_range(Info, PrevInfo, Opts) ->
    Count =
        length(
            hb_util:message_to_ordered_list(
                hb_maps:get(<<"steps">>, Info, [], Opts),
                Opts
            )
        ),
    StepNumber = int(<<"global-step-number">>, Info, Opts),
    PrevStepNumber = int(<<"global-step-number">>, PrevInfo, Opts),
    case StepNumber - PrevStepNumber of
        Delta when Delta =< 0 ->
            {error,
                consensus_error(<<"stale-step-number">>,
                    <<"The step number is not ahead of the parent's.">>)};
        Delta when Count =/= min(?NONCE_LIMITER_MAX_CHECKPOINTS_COUNT, Delta) ->
            {error,
                consensus_error(<<"invalid-step-count">>,
                    <<"The step list is not the consensus suffix of the range.">>)};
        _ ->
            ok
    end.

%% @doc Apply the node worker ceiling to a request's VDF thread count.
threads(Base, Req, Opts) ->
    Max =
        max(
            1,
            hb_util:int(
                hb_opts:get(
                    arweave_max_vdf_workers,
                    max(1, erlang:system_info(schedulers) div 2),
                    Opts
                )
            )
        ),
    Default = hb_opts:get(arweave_vdf_threads, Max, Opts),
    Threads =
        case hb_maps:get(<<"arweave-vdf-threads">>, Req, not_found, Opts) of
            not_found ->
                hb_maps:get(
                    <<"arweave-vdf-threads">>, Base, Default, Opts);
            Value ->
                Value
        end,
    min(Max, max(1, hb_util:int(Threads))).

consensus_error(Message, Detail) ->
    #{ <<"status">> => 422, <<"message">> => Message, <<"detail">> => Detail }.

%%% Internal functions.

%% @doc The output the final step was computed from. It is the second entry of
%% the block's own `steps' list, falling back to `prev_output' when the block
%% advanced the timeline by a single step.
last_step_prev_output(#block{ nonce_limiter_info = Info }) ->
    case Info#nonce_limiter_info.steps of
        [_, PrevStepOutput | _] -> PrevStepOutput;
        _ -> Info#nonce_limiter_info.prev_output
    end.

%% @doc Convert a block time history message into the list of
%% `{BlockInterval, VDFInterval, ChunkCount}' triples the retarget folds over.
%% Each field is required: the retarget sums them, so an element that quietly
%% contributed a zero would shift the difficulty without anything saying so.
message_to_history(History, Opts) ->
    [
        {
            int(<<"block-interval">>, Element, Opts),
            int(<<"vdf-interval">>, Element, Opts),
            int(<<"chunk-count">>, Element, Opts)
        }
    ||
        Element <- hb_util:message_to_ordered_list(History, Opts)
    ].

%% @doc Read a base64URL-encoded field.
decode(Key, Info, Opts) ->
    hb_util:decode(required(Key, Info, Opts)).

%% @doc Read a list of base64URL-encoded fields, preserving its order. An
%% absent list is empty rather than a missing key: whether an empty list is
%% valid depends on the operation, so the caller decides. `verify-chain'
%% rejects one; `seed-data' never looks.
decode_list(Key, Info, Opts) ->
    [
        hb_util:decode(Element)
    ||
        Element <-
            hb_util:message_to_ordered_list(
                hb_maps:get(Key, Info, [], Opts),
                Opts
            )
    ].

%% @doc Read an integer field.
int(Key, Info, Opts) ->
    hb_util:int(required(Key, Info, Opts)).

%% @doc Read a field that has no meaningful default.
required(Key, Info, Opts) ->
    case hb_maps:get(Key, Info, not_found, Opts) of
        not_found -> throw({'missing-key', Key});
        Value -> Value
    end.

%%% Tests.

%% @doc Scalar nonce-limiter inputs are rejected promptly through the public
%% AO-Core device path rather than repeatedly trying to load the same value.
malformed_info_is_rejected_promptly_test() ->
    lists:foreach(
        fun(Invalid) ->
            ?assertEqual(
                {throw, {'invalid-nonce-limiter-info', Invalid}},
                prompt_result(
                    fun() ->
                        hb_ao:resolve(
                            #{
                                <<"device">> => <<"arweave-vdf@2.9">>,
                                <<"nonce-limiter-info">> => Invalid,
                                <<"prev-nonce-limiter-info">> => #{}
                            },
                            <<"verify-step">>,
                            #{}
                        )
                    end
                )
            )
        end,
        [<<"not-a-link">>, []]
    ).

%% @doc A real AO link resolves to the same nonce-limiter record as its message.
linked_info_is_preserved_test() ->
    Opts = #{ <<"store">> => [hb_test_utils:test_store()] },
    Info = test_info(),
    {ok, ID} = hb_cache:write(Info, Opts),
    Link =
        {link,
            ID,
            #{ <<"type">> => <<"link">>, <<"lazy">> => false }
        },
    ?assertEqual(message_to_info(Info, Opts), message_to_info(Link, Opts)).

%% @doc Run a potentially recursive conversion outside the EUnit process and
%% return `timeout' if it does not answer within one second.
prompt_result(Fun) ->
    Parent = self(),
    Ref = make_ref(),
    {Pid, Monitor} =
        spawn_monitor(
            fun() ->
                Result =
                    try Fun() of
                        Returned -> {returned, Returned}
                    catch
                        Class:Reason -> {Class, Reason}
                    end,
                Parent ! {Ref, Result}
            end
        ),
    receive
        {Ref, Result} ->
            erlang:demonitor(Monitor, [flush]),
            Result;
        {'DOWN', Monitor, process, Pid, Reason} ->
            {exit, Reason}
    after 1000 ->
        exit(Pid, kill),
        receive {'DOWN', Monitor, process, Pid, _} -> ok end,
        timeout
    end.

%% @doc A complete nonce-limiter message with distinguishable fields.
test_info() ->
    #{
        <<"output">> => hb_util:encode(<<1:256>>),
        <<"prev-output">> => hb_util:encode(<<2:256>>),
        <<"seed">> => hb_util:encode(<<3:384>>),
        <<"next-seed">> => hb_util:encode(<<4:384>>),
        <<"partition-upper-bound">> => 5,
        <<"next-partition-upper-bound">> => 6,
        <<"global-step-number">> => 7,
        <<"last-step-checkpoints">> => [hb_util:encode(<<8:256>>)],
        <<"steps">> => [hb_util:encode(<<9:256>>)],
        <<"vdf-difficulty">> => 10,
        <<"next-vdf-difficulty">> => 11
    }.
