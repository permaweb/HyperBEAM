%%% @doc A simple exemplar decentralized proof of authority consensus algorithm
%%% for AO processes. This device is split into two flows, spanning three
%%% actions.
%%% 
%%% Execution flow:
%%% 1. Initialization.
%%% 2. Validation of incoming messages before execution.
%%% Commitment flow:
%%% 1. Adding commitments to results, either on a CU or MU.
-module(dev_poda).
-export([init/2, execute/3]).
-export([is_user_signed/1]).
-export([push/2]).
-include("include/hb.hrl").
-hb_debug(print).

%%% Execution flow: Initialization.

init(S, Params) ->
    {ok, S, extract_opts(Params)}.

extract_opts(Params) ->
    Authorities =
        lists:filtermap(
            fun({<<"authority">>, Addr}) -> {true, Addr};
                (_) -> false end,
                Params
        ),
    {_, RawQuorum} = lists:keyfind(<<"quorum">>, 1, Params),
    Quorum = binary_to_integer(RawQuorum),
    ?event({poda_authorities, Authorities}),
    #{
        authorities => Authorities,
        quorum => Quorum
    }.

%%% Execution flow: Pre-execution validation.

execute(Outer = #tx { data = #{ <<"body">> := Msg } }, S = #{ <<"pass">> := 1 }, Opts) ->
    case is_user_signed(Msg) of
        true ->
            {ok, S};
        false ->
            case validate(Msg, Opts) of
                true ->
                    ?event({poda_validated, ok}),
                    % Add the validations to the VFS.
                    Comms =
                        maps:to_list(
                            case Msg of 
                                #tx { data = #{ <<"commitments">> := #tx { data = X } }} -> X;
                                #tx { data = #{ <<"commitments">> := X }} -> X;
                                #{ <<"commitments">> := X } -> X
                            end
                        ),
                    VFS1 =
                        lists:foldl(
                            fun({_, Commitment}, Acc) ->
                                Id = ar_bundles:signer(Commitment),
                                Encoded = hb_util:encode(Id),
                                maps:put(
                                    <<"/commitments/", Encoded/binary>>,
                                    Commitment#tx.data,
                                    Acc
                                )
                            end,
                            maps:get(vfs, S, #{}),
                            Comms
                        ),
                    % Update the arg prefix to include the unwrapped message.
                    {ok, S#{ <<"vfs">> => VFS1, <<"arg_prefix">> =>
                        [
                            % Traverse two layers of `/Message/Message' to get
                            % the actual message, then replace `/Message' with it.
                            Outer#tx{
                                data = (Outer#tx.data)#{
                                    <<"body">> => maps:get(<<"body">>, Msg#tx.data)
                                }
                            }
                        ]
                    }};
                {false, Reason} -> return_error(S, Reason)
            end
    end;
execute(_M, S = #{ <<"pass">> := 3, <<"results">> := _Results }, _Opts) ->
    {ok, S};
execute(_M, S, _Opts) ->
    {ok, S}.

validate(Msg, Opts) ->
    validate_stage(1, Msg, Opts).

validate_stage(1, Msg, Opts) when is_record(Msg, tx) ->
    validate_stage(1, Msg#tx.data, Opts);
validate_stage(1, #{ <<"commitments">> := Commitments, <<"body">> := Content }, Opts) ->
    validate_stage(2, Commitments, Content, Opts);
validate_stage(1, _M, _Opts) -> {false, <<"Required PoDA messages missing">>}.

validate_stage(2, #tx { data = Commitments }, Content, Opts) ->
    validate_stage(2, Commitments, Content, Opts);
validate_stage(2, Commitments, Content, Opts) ->
    % Ensure that all commitments are valid and signed by a
    % trusted authority.
    case
        lists:all(
            fun({_, Comm}) ->
                ar_bundles:verify_item(Comm)
            end,
            maps:to_list(Commitments)
        ) of
        true -> validate_stage(3, Content, Commitments, Opts);
        false -> {false, <<"Invalid commitments">>}
    end;

validate_stage(3, Content, Commitments, Opts = #{ <<"quorum">> := Quorum }) ->
    Validations =
        lists:filter(
            fun({_, Comm}) -> validate_commitment(Content, Comm, Opts) end,
            maps:to_list(Commitments)
        ),
    ?event({poda_validations, length(Validations)}),
    case length(Validations) >= Quorum of
        true ->
            ?event({poda_quorum_reached, length(Validations)}),
            true;
        false -> {false, <<"Not enough validations">>}
    end.

validate_commitment(Msg, Comm, Opts) ->
    MsgID = hb_util:encode(ar_bundles:id(Msg, unsigned)),
    AttSigner = hb_util:encode(ar_bundles:signer(Comm)),
    ?event({poda_commitment, {signer, AttSigner, maps:get(authorities, Opts)}, {msg_id, MsgID}}),
    ValidSigner = lists:member(AttSigner, maps:get(authorities, Opts)),
    ValidSignature = ar_bundles:verify_item(Comm),
    RelevantMsg = ar_bundles:id(Comm, unsigned) == MsgID orelse
        (lists:keyfind(<<"commitment-for">>, 1, Comm#tx.tags)
            == {<<"commitment-for">>, MsgID}) orelse
        ar_bundles:member(ar_bundles:id(Msg, unsigned), Comm),
    case ValidSigner and ValidSignature and RelevantMsg of
        false ->
            ?event({poda_commitment_invalid,
                    {commitment, ar_bundles:id(Comm, signed)},
                    {signer, AttSigner},
                    {valid_signer, ValidSigner},
                    {valid_signature, ValidSignature},
                    {relevant_msg, RelevantMsg}}
            ),
            false;
        true -> true
    end.

%%% Execution flow: Error handling.
%%% Skip execution of this message, instead returning an error message.
return_error(S = #{ <<"wallet">> := Wallet }, Reason) ->
    ?event({poda_return_error, Reason}),
    ?debug_wait(10000),
    {skip, S#{
        results => #{
            <<"/outbox">> =>
                ar_bundles:sign_item(
                    #tx{
                        data = Reason,
                        tags = [{<<"error">>, <<"PoDA">>}]
                    },
                    Wallet
                )
        }
    }}.

%%% @doc Determines if a user committed
is_user_signed(#tx { data = #{ <<"body">> := Msg } }) ->
    ?no_prod(use_real_commitment_detection),
    lists:keyfind(<<"from-process">>, 1, Msg#tx.tags) == false;
is_user_signed(_) -> true.

%%% Commitment flow: Adding commitments to results.

%% @doc Hook used by the MU pathway (currently) to add commitments to an
%% outbound message if the computation requests it.
push(_Item, S = #{ <<"results">> := ResultsMsg }) ->
    NewRes = commit_to_results(ResultsMsg, S),
    {ok, S#{ <<"results">> => NewRes }}.

commit_to_results(Msg, S) ->
    case is_map(Msg#tx.data) of
        true ->
            % Add commitments to the outbox and spawn items.
            maps:map(
                fun(Key, IndexMsg) ->
                    ?no_prod("Currently we only commit to the outbox and spawn items."
                        "Make it general?"),
                    case lists:member(Key, [<<"/outbox">>, <<"/spawn">>]) of
                        true ->
                            ?event({poda_starting_to_commit_to_result, Key}),
                            maps:map(
                                fun(_, DeepMsg) -> add_commitments(DeepMsg, S) end,
                                IndexMsg#tx.data
                            );
                        false -> IndexMsg
                    end
                end,
                Msg#tx.data
            );
        false -> Msg
    end.

add_commitments(NewMsg, S = #{ <<"assignment">> := Assignment, <<"store">> := _Store, <<"logger">> := _Logger, <<"wallet">> := Wallet }) ->
    Process = find_process(NewMsg, S),
    case is_record(Process, tx) andalso lists:member({<<"device">>, <<"PODA">>}, Process#tx.tags) of
        true ->
            #{ <<"authorities">> := InitAuthorities, <<"quorum">> := Quorum } =
                extract_opts(Process#tx.tags),
            ?event({poda_push, InitAuthorities, Quorum}),
            % Aggregate validations from other nodes.
            % TODO: Filter out commitments from the current node.
            MsgID = hb_util:encode(ar_bundles:id(NewMsg, unsigned)),
            ?event({poda_add_commitments_from, InitAuthorities, {self,hb:address()}}),
            Commitments = pfiltermap(
                fun(Address) ->
                    case hb_router:find(compute, ar_bundles:id(Process, unsigned), Address) of
                        {ok, ComputeNode} ->
                            ?event({poda_asking_peer_for_commitment, ComputeNode, <<"commit-to">>, MsgID}),
                            Res = hb_client:compute(
                                ComputeNode,
                                ar_bundles:id(Process, signed),
                                ar_bundles:id(Assignment, signed),
                                #{ <<"commit-to">> => MsgID }
                            ),
                            case Res of
                                {ok, Comm} ->
                                    ?event({poda_got_commitment_from_peer, ComputeNode}),
                                    {true, Comm};
                                _ -> false
                            end;
                        _ -> false
                    end
                end,
                ?event(InitAuthorities -- [hb:address()])
            ),
            LocalCommitment = ar_bundles:sign_item(
                #tx{ tags = [{<<"commitment-for">>, MsgID}], data = <<>> },
                Wallet
            ),
            CompleteCommitments =
                ar_bundles:sign_item(
                    ar_bundles:normalize(
                        #tx {
                            data = 
                                maps:from_list(
                                    lists:zipwith(
                                        fun(Index, Comm) -> {integer_to_binary(Index), Comm} end,
                                        lists:seq(1, length([LocalCommitment | Commitments])),
                                        AttList = [LocalCommitment | Commitments]
                                    )
                                )
                        }
                    ),
                    Wallet
                ),
            CommitmentBundle = ar_bundles:sign_item(
                ar_bundles:normalize(
                    #tx{
                        target = NewMsg#tx.target,
                        data = #{
                            <<"commitments">> => CompleteCommitments,
                            <<"body">> => NewMsg
                        }
                    }
                ),
                Wallet
            ),
            ?event({poda_commitment_bundle_signed, {commitments, length(AttList)}}),
            CommitmentBundle;
        false -> NewMsg
    end.

%% @doc Helper function for parallel execution of commitment
%% gathering.
pfiltermap(Pred, List) ->
    Parent = self(),
    Pids = lists:map(fun(X) -> 
        spawn_monitor(fun() -> 
            Result = {X, Pred(X)},
            ?event({pfiltermap, sending_result, self()}),
            Parent ! {self(), Result}
        end)
    end, List),
    ?event({pfiltermap, waiting_for_results, Pids}),
    [
        Res
    ||
        {true, Res} <-
            lists:map(fun({Pid, Ref}) ->
                receive
                    {Pid, {_Item, Result}} ->
                        ?event({pfiltermap, received_result, Pid}),
                        Result;
                    % Handle crashes as filterable events
                    {'DOWN', Ref, process, Pid, _Reason} ->
                        ?event({pfiltermap, crashed, Pid}),
                        false;
                    Other ->
                        ?event({pfiltermap, unexpected_message, Other}),
                        false
                end
            end, Pids)
    ].

%% @doc Find the process that this message is targeting, in order to
%% determine which commitments to add.
find_process(Item, #{ <<"logger">> := _Logger, <<"store">> := Store }) ->
    case Item#tx.target of
        X when X =/= <<>> ->
            ?event({poda_find_process, hb_util:id(Item#tx.target)}),
            {ok, Proc} = hb_cache:read_message(Store, hb_util:id(Item#tx.target)),
            Proc;
        _ ->
            case lists:keyfind(<<"type">>, 1, Item#tx.tags) of
                {<<"type">>, <<"process">>} -> Item;
                _ -> process_not_specified
            end
    end.