%%% @doc A library of common functions for building devices that interact with 
%%% the `~process@1.0` meta-device structure.
-module(dev_process_lib).
-include("include/hb.hrl").
-export([as_process/2, run_as/4, process_id/3, set_results/3, ensure_process_key/2]).

%% @doc Returns the process ID of the current process.
process_id(Base, Req, Opts) ->
    ?event(debug_process_id, {process_id, {base, Base}}, Opts),
    case hb_ao:get(<<"process">>, Base, Opts#{ hashpath => ignore }) of
        not_found ->
            ?event(debug_process_id, {process_not_found, ensuring_process_key}),
            BaseWithProcess = ensure_process_key(Base, Opts),
            ?event(debug_process_id, {base_with_process, BaseWithProcess}),
            process_id(BaseWithProcess, Req, Opts);
        RawProcess ->
            ?event(debug_process_id, {process_found, {raw_process, RawProcess}}),
            % TODO: Dont read all commitments (dev_genesis_wasm:dryrun_test_)
            Process =
                hb_cache:read_all_commitments(
                    hb_cache:ensure_all_loaded(RawProcess, Opts),
                    Opts
                ),
                % ),
            ?event(debug_process_id, {process_id, {loaded_process, Process}}),
            Signers = hb_message:signers(Process, Opts),
            ?event(debug_process_id, {signers, {explicit, Signers}}),
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
    ?event(debug_run_as, {running_as, {base, Base}, {base_device, BaseDevice}}, Opts),
    ?event({running_as, {key, {explicit, Key}}, {req, Req}}),
    % Prepare the message with the specialized device configuration.
    % This sets up the device context for the specific operation type.
    {ok, PreparedMsg} =
        hb_ao:resolve(
            ensure_process_key(Base, Opts),
            #{
                <<"path">> => <<"set">>,
                <<"device">> =>
                    DeviceSet =
                        hb_maps:get(
                            << Key/binary, "-device">>,
                            Base,
                            dev_process:default_device(Base, Key, Opts),
                            Opts
                        ),
                % Configure input prefix for proper message routing within the device
                <<"input-prefix">> =>
                    case hb_maps:get(<<"input-prefix">>, Base, not_found, Opts) of
                        not_found -> <<"process">>;
                        Prefix -> Prefix
                    end,
                % Configure output prefixes for result organization
                <<"output-prefixes">> =>
                    hb_maps:get(
                        <<Key/binary, "-output-prefixes">>,
                        Base,
                        undefined, % Undefined in set will be ignored.
                        Opts
                    )
            },
            Opts
        ),
    ?event(debug_prefix,
        {input_prefix, hb_maps:get(<<"output-prefixes">>, PreparedMsg, not_found, Opts)
    }),
    % Execute the message through the specialized device.
    {Status, BaseResult} =
        hb_ao:resolve(
            PreparedMsg,
            Req,
            Opts
        ),
    BaseResultDevice = hb_maps:get(<<"device">>, BaseResult, not_found, Opts),
    % Restore the original device context after execution.
    % This ensures the process maintains its identity after device delegation.
    case {Status, BaseResultDevice} of
        {ok, not_found} ->
            {Status, BaseResult};
        {ok, DeviceSet} ->
            ?event(
                debug_run_as,
                {restoring_device,
                    {base_result, BaseResult},
                    {base_device, BaseDevice}
                }
            ),
            {ok, hb_ao:set(BaseResult, #{ <<"device">> => BaseDevice }, Opts)};
        _ ->
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
    ?event(debug_ensure_process_key, {ensure_process_key, {base, Base}}),
    case hb_maps:get(<<"process">>, Base, not_found, Opts) of
        not_found ->
            % If the message has lost its signers, we need to re-read it from
            % the cache. This can happen if the message was 'cast' to a different
            % device, leading the signers to be unset.
            ?event(debug_ensure_process_key, {process_not_found, getting_process_message}),
            Committed =
                case get_process_message(Base, Opts) of
                    undefined -> Base;
                    Other -> Other
                end,
            ?event(debug_ensure_process_key, {get_process_message, {committed, Committed}}),
            ?event(
                {process_key_before_set,
                    {base, Base},
                    {process_msg, Base},
                    {committed, Committed}
                }
            ),
            Res =
                hb_ao:set(
                    Base,
                    #{ <<"process">> => Committed },
                    Opts#{ hashpath => ignore }
                ),
            ?event(debug_ensure_process_key, {set_process_key_res, {res, Res}}),
            Res;
        _ -> Base
    end.

%% @doc Get the process message from the base message, possibly utilizing 
%% message extensions to find the process message.
get_process_message(Base, Opts) ->
    {ok, Committed} = hb_message:with_only_committed(Base, Opts),
    % TODO: Better way than type: process?
    ?event(
        debug_get_process_message,
        {get_process_message, {base, Base}, {committed, Committed}}
    ),
    case {maps:get(<<"type">>, Committed, not_found), maps:is_key(<<"...">>, Committed)} of
        {<<"Process">>, _} ->
            ?event(debug_get_process_message, {get_process_message, {type, <<"Process">>}}),
            Committed;
        {<<"process">>, _} ->
            ?event(debug_get_process_message, {get_process_message, {type, <<"process">>}}),
            Committed;
        {not_found, true} ->
            ?event(debug_get_process_message, {get_process_message, {not_found, true}}),
            get_process_message(
                hb_maps_raw:get(<<"...">>, Committed, undefined, Opts),
                Opts
            );
        _ ->
            % TODO: More elegant?
            undefined
    end.