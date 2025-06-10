-module(dev_scheduler_cache).
-export([write/2, read/3, list/2, latest/2, read_location/2, write_location/2]).
-include("include/hb.hrl").

%%% Assignment cache functions

%% @doc Write an assignment message into the cache.
write(Assignment, Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    % Write the message into the main cache
    ProcID = hb_ao:get(<<"process">>, Assignment),
    Slot = hb_ao:get(<<"slot">>, Assignment),
    ?event(
        {writing_assignment,
            {proc_id, ProcID},
            {slot, Slot},
            {assignment, Assignment}
        }
    ),
    case hb_cache:write(Assignment, Opts) of
        {ok, RootPath} ->
            % Create symlinks from the message on the process and the 
            % slot on the process to the underlying data.
            hb_store:make_link(
                Store,
                RootPath,
                hb_store:path(
                    Store,
                    [
                        <<"assignments">>,
                        hb_util:human_id(ProcID),
                        hb_ao:normalize_key(Slot)
                    ]
                )
            ),
            ok;
        {error, Reason} ->
            ?event(error, {failed_to_write_assignment, {reason, Reason}}),
            {error, Reason}
    end.

%% @doc Get an assignment message from the cache.
read(ProcID, Slot, Opts) when is_integer(Slot) ->
    read(ProcID, integer_to_list(Slot), Opts);
read(ProcID, Slot, Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    ResolvedPath =
        P2 = hb_store:resolve(
            Store,
            P1 = hb_store:path(Store, [
                "assignments",
                hb_util:human_id(ProcID),
                Slot
            ])
        ),
    ?event({resolved_path, {p1, P1}, {p2, P2}, {resolved, ResolvedPath}}),
    case hb_cache:read(ResolvedPath, Opts) of
        {ok, Assignment} ->
            % If the slot key is not present, the format of the assignment is
            % AOS2, so we need to convert it to the canonical format.
            case hb_ao:get(<<"slot">>, Assignment, Opts) of
                not_found ->
                    Norm = dev_scheduler_formats:aos2_normalize_types(Assignment),
                    {ok, Norm};
                _ ->
                    {ok, Assignment}
            end;
        not_found ->
            ?event(debug_sched, {read_assignment, {res, not_found}}),
            not_found
    end.

%% @doc Get the assignments for a process.
list(ProcID, Opts) ->
    hb_cache:list_numbered(
        hb_store:path(hb_opts:get(store, no_viable_store, Opts), [
            "assignments",
            hb_util:human_id(ProcID)
        ]),
        Opts
    ).

%% @doc Get the latest assignment from the cache.
latest(ProcID, Opts) ->
    ?event({getting_assignments_from_cache, {proc_id, ProcID}, {opts, Opts}}),
    case dev_scheduler_cache:list(ProcID, Opts) of
        [] ->
            ?event({no_assignments_in_cache, {proc_id, ProcID}}),
            not_found;
        Assignments ->
            AssignmentNum = lists:max(Assignments),
            ?event(
                {found_assignment_from_cache,
                    {proc_id, ProcID},
                    {assignment_num, AssignmentNum}
                }
            ),
            {ok, Assignment} = dev_scheduler_cache:read(
                ProcID,
                AssignmentNum,
                Opts
            ),
            {
                AssignmentNum,
                hb_ao:get(
                    <<"hash-chain">>, Assignment, #{ hashpath => ignore })
            }
    end.

%% @doc Read the latest known scheduler location for an address.
read_location(Address, Opts) ->
    Res = hb_cache:read(
        hb_store:path(hb_opts:get(store, no_viable_store, Opts), [
            "scheduler-locations",
            hb_util:human_id(Address)
        ]),
        Opts
    ),
    ?event({read_location_msg, {address, Address}, {res, Res}}),
    Res.

%% @doc Write the latest known scheduler location for an address.
write_location(LocMsg, Opts) ->
    Signers = hb_message:signers(LocMsg),
    ?event({writing_location_msg,
        {signers, Signers},
        {location_msg, LocMsg}
    }),
    case hb_message:verify(LocMsg, all) andalso hb_cache:write(LocMsg, Opts) of
        {ok, RootPath} ->
            lists:foreach(
                fun(Signer) ->
                    hb_store:make_link(
                        hb_opts:get(store, no_viable_store, Opts),
                        RootPath,
                        hb_store:path(
                            hb_opts:get(store, no_viable_store, Opts),
                            [
                                "scheduler-locations",
                                hb_util:human_id(Signer)
                            ]
                        )
                    )
                end,
                Signers
            ),
            ok;
        false ->
            % The message is not valid, so we don't cache it.
            {error, <<"Invalid scheduler location message. Not caching.">>};
        {error, Reason} ->
            ?event(warning, {failed_to_cache_location_msg, {reason, Reason}}),
            {error, Reason}
    end.
