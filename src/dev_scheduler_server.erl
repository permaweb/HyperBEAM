%%% @doc A long-lived server that schedules messages for a process.
%%% It acts as a deliberate 'bottleneck' to prevent the server accidentally
%%% assigning multiple messages to the same slot.
-module(dev_scheduler_server).
-export([start/3, schedule/2, stop/1]).
-export([info/1]).
-include_lib("eunit/include/eunit.hrl").

-include("include/hb.hrl").


%% @doc Start a scheduling server for a given computation.
start(ProcID, Proc, Opts) ->
    ?event(scheduling, {starting_scheduling_server, {proc_id, ProcID}}),
    spawn_link(
      fun() ->
              % Before we start, register the scheduler name.
              case hb_name:register({<<"scheduler@1.0">>, ProcID}) of
                  ok -> ok;
                  error ->
                      throw(
                        {another_scheduler_is_already_registered,
                         {proc_id, ProcID}})
              end,
              % Write the process to the cache. We are the provider-of-last-resort
              % for this data.
              hb_cache:write(Proc, Opts),
              case hb_opts:get(scheduling_mode, disabled, Opts) of
                  disabled ->
                      throw({scheduling_disabled_on_node, {requested_for, ProcID}});
                  _ -> ok
              end,
              {CurrentSlot, HashChain} =
                  case dev_scheduler_cache:latest(ProcID, Opts) of
                      not_found ->
                          ?event({starting_new_schedule, {proc_id, ProcID}}),
                          {-1, <<>>};
                      {Slot, Chain} ->
                          ?event(
                            {continuing_schedule,
                             {proc_id, ProcID},
                             {current_slot, Slot},
                             {hash_chain, Chain}}),
                          {Slot, Chain}
                  end,
              ?event(
                {scheduler_got_process_info,
                 {proc_id, ProcID},
                 {current, CurrentSlot},
                 {hash_chain, HashChain}}),
              server(
                #{
                  id => ProcID,
                  current => CurrentSlot,
                  hash_chain => HashChain,
                  wallets => commitment_wallets(Proc, Opts),
                  mode =>
                      hb_opts:get(
                        scheduling_mode,
                        remote_confirmation,
                        Opts),
                  opts => Opts
                 })
      end).


%% @doc Determine the appropriate list of keys to use to commit assignments for
%% a process.
commitment_wallets(ProcMsg, Opts) ->
    SchedulerVal =
        hb_ao:get_first(
          [{ProcMsg, <<"scheduler">>},
           {ProcMsg, <<"scheduler-location">>}],
          [],
          Opts),
    lists:filtermap(
      fun(Scheduler) ->
              case hb_opts:as(Scheduler, Opts) of
                  {ok, #{priv_wallet := Wallet}} -> {true, Wallet};
                  _ -> false
              end
      end,
      dev_scheduler:parse_schedulers(SchedulerVal)).


%% @doc Call the appropriate scheduling server to assign a message.
schedule(AOProcID, Message) when is_binary(AOProcID) ->
    schedule(dev_scheduler_registry:find(AOProcID), Message);
schedule(ErlangProcID, Message) ->
    ErlangProcID ! {schedule, Message, self()},
    receive
        {scheduled, Message, Assignment} ->
            Assignment
    end.


%% @doc Get the current slot from the scheduling server.
info(ProcID) ->
    ?event({getting_info, {proc_id, ProcID}}),
    ProcID ! {info, self()},
    receive {info, Info} -> Info end.


stop(ProcID) ->
    ?event({stopping_scheduling_server, {proc_id, ProcID}}),
    ProcID ! stop.


%% @doc The main loop of the server. Simply waits for messages to assign and
%% returns the current slot.
server(State) ->
    receive
        {schedule, Message, Reply} ->
            server(assign(State, Message, Reply));
        {info, Reply} ->
            Reply ! {info, State},
            server(State);
        stop -> ok
    end.


%% @doc Assign a message to the next slot.
assign(State, Message, ReplyPID) ->
    try
        do_assign(State, Message, ReplyPID)
    catch
        _Class:Reason:Stack ->
            ?event({error_scheduling, Reason, Stack}),
            State
    end.


%% @doc Generate and store the actual assignment message.
do_assign(State, Message, ReplyPID) ->
    % Ensure that only committed keys from the message are included in the
    % assignment.
    {ok, OnlyAttested} =
        hb_message:with_only_committed(
          Message,
          Opts = maps:get(opts, State)),
    HashChain =
        next_hashchain(
          maps:get(hash_chain, State),
          OnlyAttested,
          Opts),
    NextSlot = maps:get(current, State) + 1,
    % Run the signing of the assignment and writes to the disk in a separate
    % process.
    AssignFun =
        fun() ->
                {Timestamp, Height, Hash} = ar_timestamp:get(),
                Assignment =
                    commit_assignment(
                      #{
                        <<"path">> =>
                            case hb_path:from_message(request, Message, Opts) of
                                undefined -> <<"compute">>;
                                Path -> Path
                            end,
                        <<"data-protocol">> => <<"ao">>,
                        <<"variant">> => <<"ao.N.1">>,
                        <<"process">> => hb_util:id(maps:get(id, State)),
                        <<"epoch">> => <<"0">>,
                        <<"slot">> => NextSlot,
                        <<"block-height">> => Height,
                        <<"block-hash">> => hb_util:human_id(Hash),
                        <<"block-timestamp">> => Timestamp,
                        % Note: Local time on the SU, not Arweave
                        <<"timestamp">> => erlang:system_time(millisecond),
                        <<"hash-chain">> => hb_util:id(HashChain),
                        <<"body">> => OnlyAttested
                       },
                      State),
                AssignmentID = hb_message:id(Assignment, all),
                ?event(scheduling,
                       {assigned,
                        {proc_id, maps:get(id, State)},
                        {slot, NextSlot},
                        {assignment, AssignmentID}}),
                maybe_inform_recipient(
                  aggressive,
                  ReplyPID,
                  Message,
                  Assignment,
                  State),
                ?event(starting_message_write),
                ok = dev_scheduler_cache:write(Assignment, Opts),
                maybe_inform_recipient(
                  local_confirmation,
                  ReplyPID,
                  Message,
                  Assignment,
                  State),
                ?event(writes_complete),
                ?event(uploading_assignment),
                hb_client:upload(Assignment, Opts),
                ?event(uploads_complete),
                maybe_inform_recipient(
                  remote_confirmation,
                  ReplyPID,
                  Message,
                  Assignment,
                  State)
        end,
    case hb_opts:get(scheduling_mode, sync, Opts) of
        aggressive ->
            spawn(AssignFun);
        Other ->
            ?event({scheduling_mode, Other}),
            AssignFun()
    end,
    State#{
      current := NextSlot,
      hash_chain := HashChain
     }.


%% @doc Commit to the assignment using all of our appropriate wallets.
commit_assignment(BaseAssignment, State) ->
    Wallets = maps:get(wallets, State),
    lists:foldr(
      fun(Wallet, Assignment) ->
              hb_message:commit(Assignment, Wallet)
      end,
      BaseAssignment,
      Wallets).


%% @doc Potentially inform the caller that the assignment has been scheduled.
%% The main assignment loop calls this function repeatedly at different stages
%% of the assignment process. The scheduling mode determines which stages
%% trigger an update.
maybe_inform_recipient(Mode, ReplyPID, Message, Assignment, State) ->
    case maps:get(mode, State) of
        Mode -> ReplyPID ! {scheduled, Message, Assignment};
        _ -> ok
    end.


%% @doc Create the next element in a chain of hashes that links this and prior
%% assignments.
next_hashchain(HashChain, Message, Opts) ->
    ?event({creating_next_hashchain, {hash_chain, HashChain}, {message, Message}}),
    ID = hb_message:id(Message, all, Opts),
    crypto:hash(
      sha256,
      <<HashChain/binary, ID/binary>>).


%% TESTS


%% @doc Test the basic functionality of the server.
new_proc_test() ->
    Wallet = ar_wallet:new(),
    SignedItem = hb_message:commit(
                   #{<<"data">> => <<"test">>, <<"random-key">> => rand:uniform(10000)},
                   Wallet),
    SignedItem2 = hb_message:commit(
                    #{<<"data">> => <<"test2">>},
                    Wallet),
    SignedItem3 = hb_message:commit(
                    #{
                      <<"data">> => <<"test2">>,
                      <<"deep-key">> =>
                          #{<<"data">> => <<"test3">>}
                     },
                    Wallet),
    dev_scheduler_registry:find(hb_message:id(SignedItem, all), SignedItem),
    schedule(ID = hb_message:id(SignedItem, all), SignedItem),
    schedule(ID, SignedItem2),
    schedule(ID, SignedItem3),
    ?assertMatch(
      #{current := 2},
      dev_scheduler_server:info(dev_scheduler_registry:find(ID))).

% benchmark_test() ->
%     BenchTime = 1,
%     Wallet = ar_wallet:new(),
%     SignedItem = hb_message:commit(
%         #{ <<"data">> => <<"test">>, <<"random-key">> => rand:uniform(10000) },
%         Wallet
%     ),
%     dev_scheduler_registry:find(ID = hb_ao:get(id, SignedItem), true),
%     ?event({benchmark_start, ?MODULE}),
%     Iterations = hb:benchmark(
%         fun(X) ->
%             MsgX = #{
%                 path => <<"Schedule">>,
%                 <<"method">> => <<"POST">>,
%                 <<"body">> =>
%                     #{
%                         <<"type">> => <<"Message">>,
%                         <<"test-val">> => X
%                     }
%             },
%             schedule(ID, MsgX)
%         end,
%         BenchTime
%     ),
%     hb_util:eunit_print(
%         "Scheduled ~p messages in ~p seconds (~.2f msg/s)",
%         [Iterations, BenchTime, Iterations / BenchTime]
%     ),
%     ?assertMatch(
%         #{ current := X } when X == Iterations - 1,
%         dev_scheduler_server:info(dev_scheduler_registry:find(ID))
%     ),
%     ?assert(Iterations > 30).
