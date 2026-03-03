%%% @doc A device that deduplicates messages in an evaluation stream, returning
%%% status `skip' if the message has already been seen.
%%%
%%% This device is typically used to ensure that a message is only executed
%%% once, even if assigned multiple times, upon a `~process@1.0' evaluation.
%%% It can, however, be used in many other contexts.
%%%
%%% This device honors the `pass' key if it is present in the message. If so,
%%% it will only run on the first pass. Additionally, the device supports
%%% a `subject-key' key that allows the caller to specify the key whose ID
%%% should be used for deduplication. If the `subject-key' key is not present,
%%% the device will use the `body' of the request as the subject. If the key is
%%% set to `request', the device will use the entire request itself as the
%%% subject.
%%%
%%% This device runs on the first pass of the `compute' key call if executed
%%% in a stack, and not in subsequent passes.
%%%
%%% When a store is available in Opts, this device stores dedup entries as flat
%%% LMDB key-value pairs (O(1) per check/write) rather than in a trie embedded
%%% in M1. This avoids the O(trie_size) HMAC re-sign + LMDB re-write cost per
%%% slot. The trie-based fallback is retained for backward-compatibility when no
%%% store is available (e.g. in unit tests).
-module(dev_dedup).
-export([info/1]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

info(_M1) ->
    #{
        default => fun handle/4,
        exclude => [keys, set, id, commit]
    }.

%% @doc Forward the keys and `set' functions to the message device, handle all
%% others with deduplication. This allows the device to be used in any context
%% where a key is called. If the `dedup-key
handle(<<"keys">>, M1, _M2, _Opts) ->
    dev_message:keys(M1);
handle(<<"set">>, M1, M2, Opts) ->
    dev_message:set(M1, M2, Opts);
handle(Key, M1, M2, Opts) ->
    ?event({dedup_handle, {key, Key}, {base, M1}, {req, M2}}),
    % Find the relevant parameters from the messages. We search for the
    % `dedup-key' key in the first message, and use that value as the key to
    % look for in the second message.
    SubjectKey =
        hb_ao:get_first(
            [
                {{as, <<"message@1.0">>, M1}, <<"dedup-subject">>},
                {{as, <<"message@1.0">>, M2}, <<"dedup-subject">>}
            ],
            <<"body">>,
            Opts
        ),
    % Get the subject of the second message.
    Subject =
        if SubjectKey == <<"request">> ->
            % The subject is the request itself.
            M2;
        true ->
            % The subject is the value of the subject key, which will have
            % defaulted to the `body' key if not set in the base message.
            hb_ao:get_first(
                [
                    {{as, <<"message@1.0">>, M1}, SubjectKey},
                    {{as, <<"message@1.0">>, M2}, SubjectKey}
                ],
                Opts
            )
        end,
    % Is this the first pass, if we are executing in a stack?
    FirstPass = hb_ao:get(<<"pass">>, {as, <<"message@1.0">>, M1}, 1, Opts) == 1,
    ?event({dedup_handle,
        {key, Key},
        {base, M1},
        {req, M2},
        {subject_key, SubjectKey},
        {subject, Subject}
    }),
    case {FirstPass, Subject} of
        {false, _} ->
            % If this is not the first pass, we can skip the deduplication
            % check.
            {ok, M1};
        {true, not_found} ->
            % If the subject key is not present, we can skip the deduplication
            % check.
            {ok, M1};
        {true, _} ->
            SubjectID = hb_message:id(Subject, signed, Opts),
            ?event({dedup_checking, SubjectID}),
            % Try flat LMDB first (O(1)), fall back to trie when no store.
            case try_flat_dedup(SubjectID, M1, M2, Opts) of
                {flat, Result} -> Result;
                no_store -> trie_dedup(SubjectID, M1, M2, Opts)
            end
    end.

%% @doc Attempt O(1) dedup via flat LMDB key-value store.
%% Returns `{flat, Result}' on success, or `no_store' if no store is available.
%%
%% A process-dictionary guard prevents infinite recursion: computing the
%% ProcID namespace key requires looking up the `process' key, which can
%% re-enter this function through the device stack. On recursive entry we
%% return `no_store' immediately so the caller falls back to the trie path.
try_flat_dedup(SubjectID, M1, M2, Opts) ->
    case erlang:get(dev_dedup_resolving) of
        true ->
            % Recursive entry – skip flat LMDB to avoid infinite loop.
            no_store;
        _ ->
            erlang:put(dev_dedup_resolving, true),
            Result = try do_flat_dedup(SubjectID, M1, M2, Opts)
                     catch _:_ -> no_store
                     end,
            erlang:erase(dev_dedup_resolving),
            Result
    end.

do_flat_dedup(SubjectID, M1, M2, Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    case {Store, proc_id_direct(M1, Opts)} of
        {no_viable_store, _} ->
            no_store;
        {_, no_proc} ->
            % No stable process definition key in M1 — cannot safely
            % namespace the flat LMDB key. Fall back to trie.
            no_store;
        {_, ProcID} ->
            DedupKey = hb_store:path(Store, [<<"dedup">>, ProcID, SubjectID]),
            case hb_store:read(Store, DedupKey) of
                {ok, _} ->
                    % Already seen via flat LMDB record.
                    ?event({already_seen_flat, {subject, SubjectID}}),
                    {flat, {skip, M1}};
                not_found ->
                    % Not in flat LMDB; also check old trie as migration
                    % fallback for subjects seen before this optimisation
                    % was deployed.
                    OldTrie = hb_ao:get(
                        <<"dedup">>,
                        {as, <<"message@1.0">>, M1},
                        not_found,
                        Opts
                    ),
                    AlreadySeen =
                        case OldTrie of
                            not_found -> false;
                            T -> hb_ao:get(SubjectID, T, Opts) =/= not_found
                        end,
                    case AlreadySeen of
                        true ->
                            ?event({already_seen_trie_migrated, {subject, SubjectID}}),
                            {flat, {skip, M1}};
                        false ->
                            Slot = hb_maps:get(<<"slot">>, M2, true, Opts),
                            _ = hb_store:write(Store, DedupKey,
                                slot_to_bin(Slot)),
                            ?event({not_seen_flat, {subject, SubjectID}, {slot, Slot}}),
                            % Return M1 with the old trie-based dedup entry removed.
                            % Dedup state now lives in LMDB; carrying the old trie
                            % in M1 inflates the snapshot and slows serialization.
                            {flat, {ok, maps:remove(<<"dedup">>, M1)}}
                    end
            end
    end.

%% @doc Derive a stable process namespace ID using direct Erlang map access
%% (no device dispatch) to avoid re-entering the device handler recursively.
%% Returns `no_proc' when M1 has no `<<"process">>' key, which causes the
%% caller to fall back to the trie path. In production M1 is the process
%% state which always carries a top-level `<<"process">>' key pointing to
%% the immutable process definition (stable across all slots).
proc_id_direct(M1, Opts) ->
    case maps:find(<<"process">>, M1) of
        {ok, Process} when is_map(Process) ->
            hb_message:id(Process, unsigned, Opts);
        _ ->
            no_proc
    end.

%% @doc Convert a slot value (integer or binary) to a binary suitable for
%% storage as an LMDB value.
slot_to_bin(Slot) when is_integer(Slot) -> integer_to_binary(Slot);
slot_to_bin(Slot) when is_binary(Slot)  -> Slot;
slot_to_bin(Slot) -> list_to_binary(io_lib:format("~p", [Slot])).

%% @doc Original trie-based dedup. Used as fallback when no store is available.
trie_dedup(SubjectID, M1, M2, Opts) ->
    DedupTrie =
        hb_ao:get(
            <<"dedup">>,
            {as, <<"message@1.0">>, M1},
            #{ <<"device">> => <<"trie@1.0">> },
            Opts
        ),
    ?event({dedup_checking_trie, DedupTrie}),
    case hb_ao:get(SubjectID, DedupTrie, Opts) of
        not_found ->
            ?event({not_seen_trie, SubjectID}),
            Slot =
                hb_maps:get(
                    <<"slot">>,
                    M2,
                    true,
                    Opts
                ),
            {ok, NewDedupTrie} =
                hb_ao:resolve(
                    DedupTrie,
                    #{ <<"path">> => <<"set">>, SubjectID => Slot },
                    Opts
                ),
            ?event({dedup_updated_trie, NewDedupTrie}),
            hb_ao:resolve(
                M1,
                #{
                    <<"path">> => <<"set">>,
                    <<"set-mode">> => <<"explicit">>,
                    <<"dedup">> => NewDedupTrie
                },
                Opts
            );
        Value ->
            ?event(
                {already_seen_trie,
                    {subject, SubjectID},
                    {dedup_value, Value}
                }
            ),
            {skip, M1}
    end.

%%% Tests

dedup_test() ->
    hb:init(),
    % Create a stack with a dedup device and 2 devices that will append to a
    % `Result' key.
	Msg = #{
		<<"device">> => <<"stack@1.0">>,
        <<"dedup-subject">> => <<"request">>,
		<<"device-stack">> =>
			#{
				<<"1">> => <<"dedup@1.0">>,
				<<"2">> => dev_stack:generate_append_device(<<"+D2">>),
				<<"3">> => dev_stack:generate_append_device(<<"+D3">>)
			},
		<<"result">> => <<"INIT">>
	},
    % Send the same message twice, with the same binary.
    {ok, Req} = hb_ao:resolve(Msg,
        #{ <<"path">> => <<"append">>, <<"bin">> => <<"_">> }, #{}),
    {ok, Res} = hb_ao:resolve(Req,
        #{ <<"path">> => <<"append">>, <<"bin">> => <<"_">> }, #{}),
    % Send the same message twice, with another binary.
    {ok, Msg4} = hb_ao:resolve(Res,
        #{ <<"path">> => <<"append">>, <<"bin">> => <<"/">> }, #{}),
    {ok, Msg5} = hb_ao:resolve(Msg4,
        #{ <<"path">> => <<"append">>, <<"bin">> => <<"/">> }, #{}),
    % Ensure that downstream devices have only seen each message once.
    ?assertMatch(
		#{ <<"result">> := <<"INIT+D2_+D3_+D2/+D3/">> },
		Msg5
	).

dedup_with_multipass_test() ->
    % Create a stack with a dedup device and 2 devices that will append to a
    % `Result' key and a `Multipass' device that will repeat the message for
    % an additional pass. We want to ensure that Multipass is not hindered by
    % the dedup device.
	Msg = #{
		<<"device">> => <<"stack@1.0">>,
        <<"dedup-subject">> => <<"request">>,
		<<"device-stack">> =>
			#{
				<<"1">> => <<"dedup@1.0">>,
				<<"2">> => dev_stack:generate_append_device(<<"+D2">>),
				<<"3">> => dev_stack:generate_append_device(<<"+D3">>),
                <<"4">> => <<"multipass@1.0">>
			},
		<<"result">> => <<"INIT">>,
        <<"passes">> => 2
	},
    % Send the same message twice, with the same binary.
    {ok, Req} = hb_ao:resolve(Msg, #{ <<"path">> => <<"append">>, <<"bin">> => <<"_">> }, #{}),
    {ok, Res} = hb_ao:resolve(Req, #{ <<"path">> => <<"append">>, <<"bin">> => <<"_">> }, #{}),
    % Send the same message twice, with another binary.
    {ok, Msg4} = hb_ao:resolve(Res, #{ <<"path">> => <<"append">>, <<"bin">> => <<"/">> }, #{}),
    {ok, Msg5} = hb_ao:resolve(Msg4, #{ <<"path">> => <<"append">>, <<"bin">> => <<"/">> }, #{}),
    % Ensure that downstream devices have only seen each message once.
    ?assertMatch(
		#{ <<"result">> := <<"INIT+D2_+D3_+D2_+D3_+D2/+D3/+D2/+D3/">> },
		Msg5
	).
