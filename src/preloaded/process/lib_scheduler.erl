%%% @doc A library of functions shared by the scheduler devices
%%% (`~scheduler@1.0' and `~arweave-scheduler@1.0'). Both present the same
%%% consumer interface, so the logic for interpreting a scheduling request --
%%% locating the target process and the message to schedule -- is identical
%%% regardless of how the schedule is sourced. Reading a contiguous range of
%%% assignments from a scheduler cache is likewise shared, parameterized by
%%% the cache module.
-module(lib_scheduler).
-include("include/hb.hrl").
-export([find_target_id/4, find_target_id/3, at_slot/2]).
-export([find_message_to_schedule/3, load_message_to_schedule/3]).
-export([only_committed/2, base_assignment/4, slot_unavailable/0]).
-export([parse_slot_range/2, read_assignment_range/5, read_local_assignments/5]).
-export([cache_opts/1, write_assignment/3, write_linked_assignment/3]).
-export([read_assignment/4, format_opts/1]).
-export([max_assignment_query_len/0]).

%%% The maximum number of assignments that a schedule request returns at a
%%% time.
-define(MAX_ASSIGNMENT_QUERY_LEN, 1000).

%% @doc The maximum number of assignments that a schedule request returns at a
%% time, for callers that need the limit itself (for example, to bound a
%% remote schedule request).
max_assignment_query_len() -> ?MAX_ASSIGNMENT_QUERY_LEN.

%% @doc Find the schedule ID from a given request. The precedence order for
%% search is as follows:
%% 1. `ToSched/id' when `ToSched' has `type: Process'
%% 2. `ToSched/target' when `ToSched' has a `target' key
%% 3. `Req/target'
%% 4. `Req/id' when `Req' has `type: Process'
%% 5. `Base/process/id'
%% 6. `Base/id' when `Base' has `type: Process'
%% 7. `Req/id'
find_target_id(Base, Req, ToSched, Opts) ->
    case hb_ao:get(<<"type">>, ToSched, not_found, Opts) of
        <<"Process">> ->
            lib_process:process_id(ToSched, #{}, Opts);
        _ ->
            case hb_ao:get(<<"target">>, ToSched, not_found, Opts) of
                not_found -> find_target_id(Base, Req, Opts);
                Target -> hb_util:human_id(Target)
            end
    end.
find_target_id(Base, Req, Opts) ->
    TempOpts = Opts#{ <<"hashpath">> => ignore },
    Res = case hb_ao:resolve(Req, <<"target">>, TempOpts) of
        {ok, Target} ->
            % ID found at Req/target
            Target;
        _ ->
            case hb_ao:resolve(Req, <<"type">>, TempOpts) of
                {ok, <<"Process">>} ->
                    % Req is a Process, so the ID is at Req/id
                    lib_process:process_id(Req, #{}, Opts);
                _ ->
                    case hb_ao:resolve(Base, <<"process">>, TempOpts) of
                        {ok, _Process} ->
                            lib_process:process_id(Base, #{}, Opts);
                        _ ->
                            % Does the message have a type of process?
                            case hb_ao:get(<<"type">>, Base, TempOpts) of
                                <<"Process">> ->
                                    % Yes: Base is the process.
                                    lib_process:process_id(Base, #{}, Opts);
                                _ ->
                                    % No: Req is the target process.
                                    lib_process:process_id(Req, #{}, Opts)
                            end
                    end
            end
    end,
    ?event({found_id, {id, Res}, {base, Base}, {req, Req}}),
    Res.

%% @doc Search the given base and request message pair to find the message to
%% schedule. The precedence order for search is as follows:
%% 1. A key in `Req' with the value `self', indicating that the entire message
%%    is the subject.
%% 2. A key in `Req' with another value, present in that message.
%% 3. The body of the message.
%% 4. The message itself.
find_message_to_schedule(Base, Req, Opts) ->
    Subject =
        hb_ao:get(
            <<"subject">>,
            Req,
            not_found,
            Opts#{ <<"hashpath">> => ignore }
        ),
    case Subject of
        <<"base">> -> Base;
        <<"self">> -> Req;
        not_found ->
            hb_ao:get(<<"body">>, Req, Req, Opts#{ <<"hashpath">> => ignore });
        Subject ->
            hb_ao:get(Subject, Req, Opts#{ <<"hashpath">> => ignore })
    end.

%% @doc The slot that the given (process-shaped) base message has been
%% computed to.
at_slot(Base, Opts) ->
    hb_util:int(
        hb_ao:get(
            <<"at-slot">>,
            Base,
            Opts#{ <<"hashpath">> => ignore }
        )
    ).

%% @doc Reduce a message to its committed components, returning a 400 error
%% message if they cannot be validated.
only_committed(Msg, Opts) ->
    case hb_message:with_only_committed(Msg, Opts) of
        {ok, OnlyCommitted} -> {ok, OnlyCommitted};
        {error, Err} ->
            {error,
                #{
                    <<"status">> => 400,
                    <<"body">> => <<"Message invalid: ",
                        "Committed components cannot be validated.">>,
                    <<"reason">> => Err
                }
            }
    end.

%% @doc The keys of an assignment that are shared by every scheduler device:
%% everything except the fields that describe the assignment's position in the
%% scheduler's specific sequencing space (block info, weave offset, et al),
%% which the caller merges in. The `path' is taken from `PathMsg' -- normally
%% the assigned message itself.
base_assignment(ProcID, Slot, PathMsg, Opts) ->
    #{
        <<"path">> =>
            case hb_path:from_message(request, PathMsg, Opts) of
                undefined -> <<"compute">>;
                Path -> hb_path:to_binary(Path)
            end,
        <<"data-protocol">> => <<"ao">>,
        <<"variant">> => <<"ao.N.1">>,
        <<"process">> => ProcID,
        <<"epoch">> => <<"0">>,
        <<"slot">> => Slot,
        <<"body">> => PathMsg,
        <<"type">> => <<"Assignment">>
    }.

%% @doc The error returned when a requested slot is not yet present in the
%% schedule.
slot_unavailable() ->
    {error,
        #{
            <<"status">> => 404,
            <<"reason">> =>
                <<"Requested slot not yet available in schedule.">>
        }
    }.

%% @doc Find and fully load the message to schedule from the given base and
%% request pair. If the message cannot be completely loaded, a 404 error
%% message is returned instead.
load_message_to_schedule(Base, Req, Opts) ->
    RawToSched = find_message_to_schedule(Base, Req, Opts),
    try {ok, hb_cache:ensure_all_loaded(RawToSched, Opts)}
    catch
        error:{necessary_message_not_found, _, _} ->
            {error,
                #{
                    <<"status">> => 404,
                    <<"body">> => <<"Cannot fully load message to schedule.">>
                }
            }
    end.

%% @doc Parse the requested slot range -- `from' and `to' -- from a schedule
%% request. `from' defaults to 0 and is clamped to be non-negative; `to'
%% defaults to `undefined' (the latest slot known to the caller).
parse_slot_range(Req, Opts) ->
    From =
        case hb_ao:get(<<"from">>, Req, not_found, Opts) of
            not_found -> 0;
            FromRes -> max(0, hb_util:int(FromRes))
        end,
    To =
        case hb_ao:get(<<"to">>, Req, not_found, Opts) of
            not_found -> undefined;
            ToRes -> hb_util:int(ToRes)
        end,
    {From, To}.

%% @doc Read the assignments for slots `From'..`RequestedTo' from a scheduler
%% cache, truncating the range to at most `?MAX_ASSIGNMENT_QUERY_LEN' slots.
%% Returns the assignments and whether the request was truncated.
read_assignment_range(CacheMod, ProcID, From, RequestedTo, Opts) ->
    ComputedTo = min(RequestedTo, From + ?MAX_ASSIGNMENT_QUERY_LEN),
    {
        read_local_assignments(CacheMod, ProcID, From, ComputedTo, Opts),
        ComputedTo < RequestedTo
    }.

%% @doc Read a contiguous range of assignments (slots `From'..`To', inclusive)
%% from a scheduler cache, stopping at the first slot that is not present. The
%% cache module is passed in so both scheduler devices can share the traversal:
%% `CacheMod:read(ProcID, Slot, Opts)' must return `{ok, Assignment}' or
%% `not_found'.
read_local_assignments(_CacheMod, _ProcID, From, To, _Opts) when From > To ->
    [];
read_local_assignments(CacheMod, ProcID, From, To, Opts) ->
    case CacheMod:read(ProcID, From, Opts) of
        not_found -> [];
        {ok, Assignment} ->
            [
                Assignment
                | read_local_assignments(CacheMod, ProcID, From + 1, To, Opts)
            ]
    end.

%% @doc Options for generating (and reading messages for) schedule responses:
%% formatting is deterministic, so its resolutions are neither cached nor
%% awaited.
format_opts(Opts) ->
    Opts#{
        <<"hashpath">> => ignore,
        <<"cache-control">> => [<<"no-cache">>, <<"no-store">>],
        <<"await-inprogress">> => false
    }.

%% @doc Merge the scheduler store with the main store. Used before reading
%% from or writing to a scheduler cache.
cache_opts(Opts) ->
    Opts#{
        <<"store">> =>
            hb_opts:get(
                scheduler_store,
                hb_opts:get(store, no_viable_store, Opts),
                Opts
            )
    }.

%% @doc Write an assignment message into a scheduler cache: the message goes
%% into the main cache, and `<Prefix>/assignments/<proc>/<slot>' is linked to
%% its signed ID. The pseudo-path prefix distinguishes the caches of the
%% scheduler devices that share this helper.
write_assignment(Prefix, RawAssignment, RawOpts) ->
    Assignment = hb_cache:ensure_all_loaded(RawAssignment, RawOpts),
    write_linked_assignment(Prefix, Assignment, RawOpts).

%% @doc Write an assignment whose content links are already in the cache.
write_linked_assignment(Prefix, Assignment, RawOpts) ->
    Opts = cache_opts(RawOpts),
    Store = hb_opts:get(store, no_viable_store, Opts),
    ProcID = hb_ao:get(<<"process">>, Assignment, Opts),
    Slot = hb_ao:get(<<"slot">>, Assignment, Opts),
    ?event(
        {writing_assignment,
            {prefix, Prefix},
            {proc_id, ProcID},
            {slot, Slot}
        }
    ),
    case hb_cache:write(Assignment, Opts) of
        {ok, _UnsignedID} ->
            ok = hb_store:link(
                Store,
                #{
                    assignment_path(Prefix, ProcID, Slot) =>
                        hb_message:id(Assignment, signed, Opts)
                },
                Opts
            ),
            ok;
        {error, Reason} ->
            ?event(error, {failed_to_write_assignment, {reason, Reason}}),
            {error, Reason}
    end.

%% @doc Get an assignment message from a scheduler cache. Restores the
%% assignment's unsigned commitment ID (`hb_cache:read' does not normalize
%% commitments) and converts legacy `ao.TN.1' (AOS2) assignments to the
%% canonical format via the scheduler package's formats module.
read_assignment(Prefix, ProcID, Slot, Opts) when is_integer(Slot) ->
    read_assignment(Prefix, ProcID, hb_util:bin(Slot), Opts);
read_assignment(Prefix, ProcID, Slot, RawOpts) ->
    Opts = cache_opts(RawOpts),
    Store = hb_opts:get(store, no_viable_store, Opts),
    Path = assignment_path(Prefix, ProcID, Slot),
    ?event(
        {read_assignment,
            {prefix, Prefix},
            {proc_id, ProcID},
            {slot, Slot}
        }
    ),
    case hb_store:resolve(Store, Path, Opts) of
        {ok, ResolvedPath} ->
            case hb_cache:read(ResolvedPath, Opts) of
                {ok, RawAssignment} ->
                    Assignment =
                        hb_message:normalize_commitments(RawAssignment, Opts),
                    case hb_ao:get(<<"variant">>, Assignment, Opts) of
                        <<"ao.TN.1">> ->
                            Loaded =
                                hb_cache:ensure_all_loaded(Assignment, Opts),
                            {ok,
                                dev_scheduler_formats:aos2_to_assignment(
                                    Loaded,
                                    Opts
                                )
                            };
                        <<"ao.N.1">> ->
                            {ok, hb_cache:ensure_all_loaded(Assignment, Opts)}
                    end;
                {error, not_found} -> not_found
            end;
        {error, not_found} -> not_found
    end.

assignment_path(Prefix, ProcID, Slot) ->
    hb_path:to_binary(
        [
            Prefix,
            <<"assignments">>,
            hb_util:human_id(ProcID),
            hb_ao:normalize_key(Slot)
        ]
    ).
