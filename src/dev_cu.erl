-module(dev_cu).
-export([push/2, execute/2]).
-include_lib("eunit/include/eunit.hrl").

-include("include/hb.hrl").
-hb_debug(print).


push(Msg, S = #{assignment := Assignment, logger := _Logger}) ->
    ?event(
      {pushing_message,
       {assignment, hb_util:id(Assignment, unsigned)},
       {message, hb_util:id(Msg, unsigned)}}),
    case hb_client:compute(Assignment, Msg) of
        {ok, Results} ->
            ?event(computed_results),
            {ok, S#{results => Results}};
        Error ->
            throw({cu_error, Error})
    end.


execute(CarrierMsg, S) ->
    MaybeBundle = ar_bundles:hd(CarrierMsg),
    Store = hb_opts:get(store),
    Wallet = hb:wallet(),
    {ok, Results} =
        case MaybeBundle of
            #tx{data = #{<<"body">> := _Msg, <<"assignment">> := Assignment}} ->
                % TODO: Execute without needing to call the SU unnecessarily.
                {_, ProcID} = lists:keyfind(<<"process">>, 1, Assignment#tx.tags),
                ?event({dev_cu_computing_from_full_assignment, {process, ProcID}, {slot, hb_util:id(Assignment, signed)}}),
                hb_process:result(ProcID, hb_util:id(Assignment, signed), Store, Wallet);
            _ ->
                case lists:keyfind(<<"process">>, 1, CarrierMsg#tx.tags) of
                    {_, Process} ->
                        {_, Slot} = lists:keyfind(<<"slot">>, 1, CarrierMsg#tx.tags),
                        ?event({dev_cu_computing_from_slot_ref, {process, Process}, {slot, Slot}}),
                        hb_process:result(Process, Slot, Store, Wallet);
                    false ->
                        {error, no_viable_computation}
                end
        end,
    {ResType, ModState = #{results := _ModResults}} =
        case lists:keyfind(<<"commit-to">>, 1, CarrierMsg#tx.tags) of
            {_, RawCommitTo} ->
                CommitTo = hb_util:decode(RawCommitTo),
                ?event({commit_to_only_message, RawCommitTo}),
                case ar_bundles:find(CommitTo, Results) of
                    not_found ->
                        ?event(message_to_commit_to_not_found),
                        {ok,
                         S#{
                           results =>
                               #tx{
                                 tags = [{<<"status">>, 404}],
                                 data = <<"Requested message to commit to not in results bundle.">>
                                }
                          }};
                    _ ->
                        ?event(message_to_commit_to_found),
                        {ok, S#{
                               results => ar_bundles:sign_item(
                                            #tx{
                                              tags = [{<<"status">>, 200},
                                                      {<<"commitment-for">>, RawCommitTo}],
                                              data = <<>>
                                             },
                                            hb:wallet())
                              }}
                end;
            false ->
                {ok, S#{results => Results}}
        end,
    ?event(returning_computed_results),
    %ar_bundles:print(ModResults),
    {ResType, ModState}.
