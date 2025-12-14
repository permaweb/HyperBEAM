%%% @doc A library of common functions for building devices that interact with 
%%% the `~process@1.0` meta-device structure.
-module(dev_process_lib).
-include("include/hb.hrl").
-export([as_process/2, run_as/4, process_id/3, set_results/3, ensure_process_key/2]).
-export([send/3, forwarded_keys/2]).

%% @doc Returns the process ID of the current process.
process_id(Base, Req, Opts) ->
    case hb_ao:get(<<"process">>, Base, Opts#{ hashpath => ignore }) of
        not_found ->
            process_id(ensure_process_key(Base, Opts), Req, Opts);
        Process ->
            Signers = hb_message:signers(Process, Opts),
            case {hb_message:verify(Process, all, Opts), Signers} of
                {false, _} ->
                    ?event({process_not_verified, {process, Process}}),
                    throw({process_not_verified, Process});
                {true, []} ->
                    ?event({process_has_no_signers, {process, Process}}),
                    throw({process_has_no_signers, Process});
                {true, _} ->
                    hb_message:id(
                        Process,
                        hb_util:atom(maps:get(<<"commitments">>, Req, <<"signed">>)),
                        Opts
                    )
            end
    end.

%% @doc Run a message against Base, with the device being swapped out for
%% the device found at `Key'. After execution, the device is swapped back
%% to the original device if the device is the same as we left it.
run_as(Key, Base, Path, Opts) when not is_map(Path) ->
    run_as(Key, Base, #{ <<"path">> => Path }, Opts);
run_as(Key, Base, Req, Opts) ->
    % Store the original device so we can restore it after execution
    BaseDevice = hb_maps:get(<<"device">>, Base, not_found, Opts),
    ?event({running_as, {key, {explicit, Key}}, {req, Req}}),
    % Prepare the message with the specialized device configuration.
    % This sets up the device context for the specific operation type.
    {ok, PreparedMsg} = dev_process:as(Base, Key, Opts),
    ?event(
        debug_run_as,
        {before_resolve, {prepared_msg, PreparedMsg}, {req, Req}},
        Opts
    ),
    % Execute the message through the specialized device.
    {Status, BaseResult} =
        hb_ao:resolve(
            PreparedMsg,
            Req,
            Opts
        ),
    ?event(
        debug_run_as,
        {after_resolve, {status, Status}, {base_result, BaseResult}},
        Opts
    ),
    % Restore the original device context after execution.
    % This ensures the process maintains its identity after device delegation.
    case {Status, BaseResult} of
        {ok, #{ <<"device">> := DeviceSet }} ->
            {ok, hb_ao:set(BaseResult, #{ <<"device">> => BaseDevice }, Opts)};
        _ ->
            ?event({returning_base_result, BaseResult}),
            {Status, BaseResult}
    end.

%% @doc Change the message to for that has the device set as this module.
%% In situations where the key that is `run_as' returns a message with a 
%% transformed device, this is useful.
as_process(Base, Opts) ->
    {ok, Proc} = dev_message:set(Base, #{ <<"device">> => <<"process@1.0">> }, Opts),
    Proc.

%% @doc Set the results of the current process.
set_results(State, Results, Opts) ->
    {ok, hb_ao:set(State, #{ <<"results">> => Results }, Opts)}.


%% @doc Helper function to store a copy of the `process' key in the message.
ensure_process_key(Base, Opts) ->
    case hb_maps:get(<<"process">>, Base, not_found, Opts) of
        not_found ->
            % If the message has lost its signers, we need to re-read it from
            % the cache. This can happen if the message was 'cast' to a different
            % device, leading the signers to be unset.
            {ok, Committed} = hb_message:with_only_committed(Base, Opts),
            ?event(
                {process_key_before_set,
                    {base, Base},
                    {process_msg, Base},
                    {committed, Committed}
                }
            ),
            Res =
                hb_ao:set(
                    hb_message:uncommitted(Base, Opts),
                    #{ <<"process">> => Committed },
                    Opts#{ hashpath => ignore }
                ),
            ?event(
                {set_process_key_res,
                    {base, Base},
                    {process_msg, Base},
                    {res, Res}
                }
            ),
            Res;
        _ -> Base
    end.

%% @doc Extract keys with X- prefix for forwarding in notices
%% Follows AO token pattern: keys beginning with "X-" are forwarded.
forwarded_keys(Req, Opts) ->
    case hb_maps:is_map(Req, Opts) of
        true ->
            hb_maps:fold(
                fun(Key, Value, Acc) when is_binary(Key) ->
                    case byte_size(Key) >= 2 of
                        true ->
                            Prefix = binary:part(Key, 0, 2),
                            case string:lowercase(Prefix) of
                                <<"x-">> -> hb_maps:put(Key, Value, Acc, Opts);
                                _ -> Acc
                            end;
                        false -> Acc
                    end;
                (_Key, _Value, Acc) -> Acc
                end,
                #{},
                Req,
                Opts
            );
        false -> #{}
    end.

%% @doc Add a message or list of messages to the process's outbox.
send(Msg, Base, Opts) when not is_list(Msg) ->
    send([Msg], Base, Opts);
send(Msgs, Base, Opts) ->
    CurrentOutbox = hb_ao:get(<<"results/outbox">>, Base, [], Opts),
    NewOutbox = hb_util:message_to_ordered_list(CurrentOutbox, Opts) ++ Msgs,
    {
        ok,
        hb_ao:set(Base, <<"results/outbox">>, NewOutbox, Opts)
    }.