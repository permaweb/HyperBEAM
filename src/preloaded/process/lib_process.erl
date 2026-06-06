%%% @doc A library of common functions for building devices that interact with 
%%% the `~process@1.0` meta-device structure.
-module(lib_process).
-include("include/hb.hrl").
-export([
    as_process/2,
    run_as/4,
    process_id/3,
    set_results/3,
    ensure_process_key/2,
    default_device/3,
    %% Cross-package process helpers — see bottom of module.
    cache_write/4,
    cache_latest/2,
    cache_latest/3,
    cache_latest/4,
    cache_path/3,
    assignments_to_aos2/4
]).

%% @doc Returns the process ID of the current process.
process_id(Base, Req, Opts) ->
    case hb_ao:get(<<"process">>, Base, Opts#{ <<"hashpath">> => ignore }) of
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
                            default_device(Base, Key, Opts),
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
    hb_ao:set(Base, #{ <<"device">> => <<"process@1.0">> }, Opts).

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
                    Opts#{ <<"hashpath">> => ignore }
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

%% @doc Returns the default device for a given piece of process functionality.
default_device(Base, Key, Opts) ->
    NormKey = hb_ao:normalize_key(Key),
    case {NormKey, hb_util:deep_get(<<"process/variant">>, Base, Opts)} of
        {<<"execution">>, <<"ao.TN.1">>} -> <<"genesis-wasm@1.0">>;
        _ -> default_device_index(NormKey)
    end.
default_device_index(<<"scheduler">>) -> <<"scheduler@1.0">>;
default_device_index(<<"execution">>) -> <<"genesis-wasm@1.0">>;
default_device_index(<<"push">>) -> <<"push@1.0">>.

%%% --------------------------------------------------------------------
%%% Cross-package process helpers.
%%%
%%% A `lib_*' module declared via `-device_libraries' is compiled into every
%%% package that declares it. The device packager (`hb_device_rename') only
%%% rewrites calls to modules in the calling package's rename map, so a direct
%%% call to a device root in another package is emitted bare and will not
%%% resolve. A shared `lib_*' module is in every declaring package's map, so
%%% routing cross-package functionality through it keeps the calls resolvable in
%%% any package.
%%%
%%% The helpers below are the cross-package entry points to the process-package
%%% cache and scheduler-format functionality that the vm-package devices
%%% (`dev_genesis_wasm', `dev_delegated_compute') rely on. They depend only on
%%% the core `hb_*'/`ar_*' modules, which are never renamed, so they resolve in
%%% whichever package they are compiled into.
%%%
%%% Exports for these are declared in the module's top `-export' block: the Forge
%%% device compiler rejects `-export' attributes placed after function definitions.
%%% --------------------------------------------------------------------

%% @doc Write a process computation result to the cache. Canonical home of the
%% process-result cache writer; `dev_process_cache:write/4' delegates here so the
%% logic is shared (and resolvable) across the process and vm device packages.
cache_write(ProcID, Slot, Msg, Opts) ->
    % Write the item to the cache in the root of the store.
    {ok, Root} = hb_cache:write(hb_private:reset(Msg), Opts),
    % Link the item to the path in the store by slot number.
    SlotNumPath = cache_path(ProcID, Slot, Opts),
    hb_cache:link(Root, SlotNumPath, Opts),
    % Link the item to the message ID path in the store.
    MsgIDPath =
        cache_path(
            ProcID,
            ID = hb_message:id(Msg, uncommitted, Opts),
            Opts
        ),
    ?event(
        {linking_id,
            {proc_id, ProcID},
            {slot, Slot},
            {id, ID},
            {path, MsgIDPath}
        }
    ),
    hb_cache:link(Root, MsgIDPath, Opts),
    % Return the slot number path.
    {ok, SlotNumPath}.

%% @doc Retrieve the latest slot for a given process. Optionally state a limit
%% on the slot number to search for, as well as a required path that the slot
%% must have. Canonical home; `dev_process_cache:latest/_' delegates here.
cache_latest(ProcID, Opts) -> cache_latest(ProcID, [], Opts).
cache_latest(ProcID, RequiredPath, Opts) ->
    cache_latest(ProcID, RequiredPath, undefined, Opts).
cache_latest(ProcID, RawRequiredPath, Limit, RawOpts) ->
    Scope = hb_opts:get(process_cache_scope, local, RawOpts),
    % Normalize the store descriptor to a list of stores.
    UnscopedStore =
        case hb_opts:get(store, no_viable_store, RawOpts) of
            StoreMsg when is_map(StoreMsg) -> [StoreMsg];
            Other -> Other
        end,
    % Apply the scope to the store and update the options message.
    ScopedStore = hb_store:scope(UnscopedStore, Scope),
    Opts = RawOpts#{ <<"store">> => ScopedStore },
    % Convert the required path to a list of _binary_ keys.
    RequiredPath =
        case RawRequiredPath of
            undefined -> [];
            [] -> [];
            _ -> hb_path:term_to_path_parts(RawRequiredPath, Opts)
        end,
    ?event({required_path_converted, {proc_id, ProcID}, {required_path, RequiredPath}}),
    Path = cache_path(ProcID, slot_root, Opts),
    AllSlots = hb_cache:list_numbered(Path, Opts),
    ?event({all_slots, {proc_id, ProcID}, {slots, AllSlots}}),
    CappedSlots =
        case Limit of
            undefined -> AllSlots;
            _ -> lists:filter(fun(Slot) -> Slot =< Limit end, AllSlots)
        end,
    ?event(
        {finding_latest_slot,
            {proc_id, hb_util:human_id(ProcID)},
            {limit, Limit},
            {path, Path},
            {slots_in_range, CappedSlots}
        }
    ),
    % Find the highest slot that has the necessary path.
    BestSlot =
        cache_first_with_path(
            ProcID, RequiredPath, lists:reverse(lists:sort(CappedSlots)), Opts),
    case BestSlot of
        {failure, _} = Failure -> Failure;
        {error, _} = Error -> Error;
        not_found -> {error, not_found};
        SlotNum ->
            {ok, Msg} = hb_cache:read(cache_path(ProcID, SlotNum, Opts), Opts),
            {ok, SlotNum, Msg}
    end.

cache_path(ProcID, Ref, Opts) -> cache_path(ProcID, Ref, [], Opts).
cache_path(ProcID, Ref, PathSuffix, _Opts) ->
    hb_path:to_binary(
        [<<"computed">>, hb_util:human_id(ProcID)] ++
        case Ref of
            Int when is_integer(Int) -> ["slot", integer_to_binary(Int)];
            root -> [];
            slot_root -> ["slot"];
            _ -> [Ref]
        end ++ PathSuffix
    ).

cache_first_with_path(ProcID, RequiredPath, Slots, Opts) ->
    cache_first_with_path(
        ProcID, RequiredPath, Slots, Opts,
        hb_opts:get(store, no_viable_store, Opts)
    ).
cache_first_with_path(_ProcID, _Required, [], _Opts, _Store) -> not_found;
cache_first_with_path(ProcID, RequiredPath, [Slot | Rest], Opts, Store) ->
    RawPath = cache_path(ProcID, Slot, RequiredPath, Opts),
    ?event({trying_slot, {slot, Slot}, {path, RawPath}}),
    case hb_store:read(Store, RawPath, Opts) of
        {error, not_found} ->
            cache_first_with_path(ProcID, RequiredPath, Rest, Opts, Store);
        {failure, _} = Failure -> Failure;
        {error, _} = Error -> Error;
        _ -> Slot
    end.

%% @doc Return legacy net-SU compatible AOS2 results for a set of assignments.
%% Canonical home; `dev_scheduler_formats:assignments_to_aos2/4' delegates here.
assignments_to_aos2(ProcID, Assignments, More, RawOpts) when is_map(Assignments) ->
    assignments_to_aos2(
        ProcID,
        hb_util:message_to_ordered_list(Assignments),
        More,
        format_opts(RawOpts)
    );
assignments_to_aos2(ProcID, Assignments, More, RawOpts) ->
    Opts = format_opts(RawOpts),
    {Timestamp, Height, Hash} = ar_timestamp:get(),
    BodyStruct =
        #{
            <<"page_info">> =>
                #{
                    <<"process">> => hb_util:human_id(ProcID),
                    <<"has_next_page">> => More,
                    <<"timestamp">> => list_to_binary(integer_to_list(Timestamp)),
                    <<"block-height">> => list_to_binary(integer_to_list(Height)),
                    <<"block-hash">> => hb_util:human_id(Hash)
                },
            <<"edges">> =>
                lists:map(
                    fun(Assignment) ->
                        #{
                            <<"cursor">> => assignment_cursor(Assignment, Opts),
                            <<"node">> => assignment_to_aos2(Assignment, Opts)
                        }
                    end,
                    Assignments
                )
        },
    Encoded = hb_json:encode(BodyStruct),
    ?event({body_struct, BodyStruct}),
    ?event({encoded, {explicit, Encoded}}),
    {ok, #{
        <<"content-type">> => <<"application/json">>,
        <<"body">> => Encoded
    }}.

assignment_cursor(Assignment, RawOpts) ->
    hb_ao:get(<<"slot">>, Assignment, format_opts(RawOpts)).

assignment_to_aos2(Assignment, RawOpts) ->
    Opts = format_opts(RawOpts),
    Message = hb_ao:get(<<"body">>, Assignment, Opts),
    AssignmentWithoutBody = hb_maps:without([<<"body">>], Assignment, Opts),
    {ok, MessageStruct} =
        hb_ao:resolve(
            #{ <<"device">> => <<"json-iface@1.0">> },
            #{ <<"path">> => <<"to">>, <<"message">> => Message },
            Opts
        ),
    {ok, AssignmentStruct} =
        hb_ao:resolve(
            #{ <<"device">> => <<"json-iface@1.0">> },
            #{ <<"path">> => <<"to">>, <<"message">> => AssignmentWithoutBody },
            Opts
        ),
    #{
        <<"message">> => MessageStruct,
        <<"assignment">> => AssignmentStruct
    }.

format_opts(Opts) ->
    Opts#{
        <<"hashpath">> => ignore,
        <<"cache-control">> => [<<"no-cache">>, <<"no-store">>],
        <<"await-inprogress">> => false
    }.
