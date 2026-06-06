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
    %% Cross-package helpers (issue #944) — see bottom of module.
    cache_write/4,
    cache_latest/2,
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
%%% Cross-package helpers (issue #944).
%%%
%%% The vm-package devices (`dev_genesis_wasm', `dev_delegated_compute') need
%%% functionality that lives in the process package (`dev_process_cache',
%%% `dev_scheduler_formats'). A DIRECT cross-package call is emitted bare by the
%%% device packager's rename transform (`hb_device_rename') — the called atom is
%%% not in the calling package's rename map — so it `undef's at runtime on a fresh
%%% build (issue #944). `lib_process' is declared via `-device_libraries' by BOTH
%%% packages, so it is compiled into each; routing these calls through it makes
%%% them resolve in either package. The bodies below depend only on the
%%% (never-renamed) `hb_*'/`ar_*' core, so they are self-contained in any package.
%%%
%%% NOTE FOR REVIEWERS: these mirror `dev_process_cache:{write,latest}/_' and
%%% `dev_scheduler_formats:assignments_to_aos2/4'. An alternative is to MOVE the
%%% originals here and have the process-package modules delegate to `lib_process'
%%% (both already declare it) to avoid the duplication — flagged for your call.
%%% (Exports are declared in the module's top `-export' block — the Forge device
%%% compiler rejects `-export' attributes that appear after function definitions.)
%%% --------------------------------------------------------------------

%% @doc Cross-package-safe mirror of `dev_process_cache:write/4'.
cache_write(ProcID, Slot, Msg, Opts) ->
    {ok, Root} = hb_cache:write(hb_private:reset(Msg), Opts),
    SlotNumPath = cache_path(ProcID, Slot, Opts),
    hb_cache:link(Root, SlotNumPath, Opts),
    MsgIDPath = cache_path(ProcID, hb_message:id(Msg, uncommitted, Opts), Opts),
    hb_cache:link(Root, MsgIDPath, Opts),
    {ok, SlotNumPath}.

%% @doc Cross-package-safe mirror of `dev_process_cache:latest/2'.
cache_latest(ProcID, Opts) ->
    cache_latest(ProcID, [], undefined, Opts).
cache_latest(ProcID, RawRequiredPath, Limit, RawOpts) ->
    Scope = hb_opts:get(process_cache_scope, local, RawOpts),
    UnscopedStore =
        case hb_opts:get(store, no_viable_store, RawOpts) of
            StoreMsg when is_map(StoreMsg) -> [StoreMsg];
            Other -> Other
        end,
    ScopedStore = hb_store:scope(UnscopedStore, Scope),
    Opts = RawOpts#{ <<"store">> => ScopedStore },
    RequiredPath =
        case RawRequiredPath of
            undefined -> [];
            [] -> [];
            _ -> hb_path:term_to_path_parts(RawRequiredPath, Opts)
        end,
    Path = cache_path(ProcID, slot_root, Opts),
    AllSlots = hb_cache:list_numbered(Path, Opts),
    CappedSlots =
        case Limit of
            undefined -> AllSlots;
            _ -> lists:filter(fun(Slot) -> Slot =< Limit end, AllSlots)
        end,
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
    case hb_store:read(Store, RawPath, Opts) of
        {error, not_found} ->
            cache_first_with_path(ProcID, RequiredPath, Rest, Opts, Store);
        {failure, _} = Failure -> Failure;
        {error, _} = Error -> Error;
        _ -> Slot
    end.

%% @doc Cross-package-safe mirror of `dev_scheduler_formats:assignments_to_aos2/4'.
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
