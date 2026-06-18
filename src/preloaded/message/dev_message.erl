%%% @doc The identity device: For non-reserved keys, it simply returns a key 
%%% from the message as it is found in the message's underlying Erlang map. 
%%% Private keys (`priv[.*]') are not included.
%%% Reserved keys are: `id', `commitments', `committers', `keys', `path', 
%%% `set', `remove', `get', and `verify'. Their function comments describe the 
%%% behaviour of the device when these keys are set.
-module(dev_message).
%%% Base AO-Core reserved keys:
-export([info/0, keys/1, keys/2]).
-export([set/3, set_path/3, remove/3, get/3, get/4]).
%%% Commitment-specific keys:
-export([id/1, id/2, id/3]).
-export([commit/3, committed/3, committers/1, committers/2, committers/3, verify/3]).
%%% Non-protocol enforced keys:
-export([index/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").
-define(DEFAULT_ID_DEVICE, <<"httpsig@1.0">>).
-define(DEFAULT_ATT_DEVICE, <<"httpsig@1.0">>).

%% The list of keys that are exported by this device.
-define(DEVICE_KEYS, [
    <<"id">>,
    <<"commitments">>,
    <<"committers">>,
    <<"keys">>,
    <<"path">>,
    <<"set">>,
    <<"remove">>,
    <<"verify">>
]).

%% @doc Return the info for the identity device.
info() ->
    #{
        default => fun dev_message:get/4
    }.

%% @doc Generate an index page for a message, in the event that the `body' and
%% `content-type' of a message returned to the client are both empty. We do this
%% as follows:
%% 1. Find the `default_index' key of the node message. If it is a binary,
%%    it is assumed to be the name of a device, and we execute the resolution
%%    `as` that ID.
%% 2. Merge the base message with the default index message, favoring the default
%%    index message's keys over those in the base message, unless the default
%%    was a device name.
%% 3. Execute the `default_index_path` (base: `index') upon the message,
%%    giving the rest of the request unchanged.
-spec index(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, _} | {error, _}.
index(Msg, Req, Opts) ->
    case hb_opts:get(default_index, not_found, Opts) of
        not_found ->
            {error, <<"No default index message set.">>};
        DefaultIndex ->
            hb_ao:resolve(
                case is_map(DefaultIndex) of
                    true -> maps:merge(Msg, DefaultIndex);
                    false -> {as, DefaultIndex, Msg}
                end,
                Req#{
                    <<"path">> =>
                        case hb_maps:find(<<"path">>, DefaultIndex, Opts) of
                            {ok, Path} -> Path;
                            _ ->
                                hb_opts:get(default_index_path, <<"index">>, Opts)
                        end
                },
                Opts
            )
    end.

%% @doc Return the ID of a message, using the `committers' list if it exists.
%% If the `committers' key is `all', return the ID including all known 
%% commitments -- `none' yields the ID without any commitments. If the 
%% `committers' key is a list/map, return the ID including only the specified 
%% commitments.
%% 
%% The `id-device' key in the message can be used to specify the device that
%% should be used to calculate the ID. If it is not set, the default device
%% (`httpsig@1.0') is used.
%% 
%% Note: This function _does not_ use AO-Core's `get/3' function, as it
%% would require significant computation. We may want to change this
%% if/when non-map message structures are created.
-spec id(binary() | [#{ _ => _ }] | #{ commitments => #{ _ => _ }, _ => _ },
    #{ committers => _,
        'commitment-ids' => _,
        'id-device' => binary(),
        _ => _
    },
    #{ _ => _ }
) -> {ok, binary()}.
id(Base) -> id(Base, #{}).
id(Base, Req) -> id(Base, Req, #{}).
id(Base, _, NodeOpts) when is_binary(Base) ->
    % Return the hashpath of the message in native format, to match the native
    % format of the message ID return.
    {ok, hb_util:human_id(hb_path:hashpath(Base, NodeOpts))};
id(List, Req, NodeOpts) when is_list(List) ->
    % Return the list of IDs for a list of messages.
    id(hb_message:convert(List, tabm, NodeOpts), Req, NodeOpts);
id(RawBase, Req, NodeOpts) ->
    % Ensure that the base message is normalized before proceeding.
    IDOpts =
        maybe_preserve_message_extension(
            RawBase,
            Req,
            NodeOpts#{ <<"linkify-mode">> => discard },
            true
        ),
    Base = ensure_commitments_loaded(RawBase, IDOpts),
    % Remove the commitments from the base message if there are none, after
    % filtering for the committers specified in the request.
    #{ <<"commitments">> := Commitments }
        = with_relevant_commitments(Base, Req, IDOpts),
    ?event(debug_id,
        {generating_ids,
            {selected_commitments, Commitments},
            {req, Req},
            {msg, Base}
        }
    ),
    case hb_maps:keys(Commitments) of
        [] ->
            % If there are no commitments, we must (re)calculate the ID.
            ?event(debug_id, regenerating_id),
            calculate_id(hb_maps:without([<<"commitments">>], Base), Req, IDOpts);
        IDs ->
            % Accumulate the relevant IDs into a single value. This is performed 
            % by module arithmetic of each of the IDs. The effect of this is that:
            % 1. New IDs can be added to the combined ID without requiring any
            %    recalculation of other IDs.
            % 2. New IDs can be added in any order, and will compare to the same
            %    value as if they were added in other orders.
            % 3. Subsequently, combined IDs cannot be used to express ordering of
            %    the underlying commitments.
            % This works for single IDs as well as lists of IDs, because the 
            % accumulation function starts with a buffer of zero encoded as a 
            % 256-bit binary. Subsequently, a single ID on its own 'accumulates' 
            % to itself.
            ?event(debug_id, returning_existing_ids),
            {ok,
                hb_util:human_id(
                    hb_crypto:accumulate(
                        lists:map(fun hb_util:native_id/1, IDs)
                    )
                )
            }
    end.

calculate_id(RawBase, Req, NodeOpts) ->
    % Resolve the ID device up-front so we can plumb it as `hint-device' into
    % the structured->tabm conversion below. This keeps the children's load
    % state consistent with what `commit/3' and `verify/3' would produce.
    IDDev =
        case id_device(RawBase, NodeOpts) of
            {ok, Device} -> Device;
            {error, Error} -> throw({id, Error})
        end,
    % Encode to a TABM. The `bundle' flag (when set on the request) is the
    % caller's intent for the top-level message and applies only to the root;
    % `hint-device' lets the structured codec reproduce each nested
    % commitment's own bundle state per-node, so the id is computed over the
    % same shape `commit/3' and `verify/3' would produce.
    SourceSpec =
        hb_message:add_bundle_hint(
            #{ <<"device">> => <<"structured@1.0">> },
            Req#{ <<"device">> => IDDev },
            NodeOpts
        ),
    Base = hb_message:convert(RawBase, tabm, SourceSpec, NodeOpts),
    ?event(debug_id, {calculate_ids, {base, Base}}),
    ?event(debug_id, {generating_id, {id_device, IDDev}, {base, Base}}),
    % Get the commitment device name from the message, or use the default if
    % it is not set. We can tell if the device is not set (or is the default)
    % by checking whether the resolved device module is this module itself.
    % `hb_ao:raw/5' expects a device name, not a resolved module.
    CommitDev =
        case hb_device:message_to_device(#{ <<"device">> => IDDev }, NodeOpts) of
            ?MODULE -> ?DEFAULT_ID_DEVICE;
            _ -> IDDev
        end,
    ?event(debug_id, {called_id_device, CommitDev}, NodeOpts),
    {ok, #{ <<"commitments">> := Comms} } =
        hb_ao:raw(
            CommitDev,
            <<"commit">>,
            Base,
            Req#{ <<"type">> => <<"unsigned">> },
            NodeOpts
        ),
    ?event(debug_id,
        {generated_id,
            {type, unsigned},
            {commitments, maps:keys(Comms)}
        }
    ),
    {ok, hd(maps:keys(Comms))}.

%% @doc Locate the ID device of a message. The ID device is determined the
%% `device' set in _all_ of the commitments. If no commitments are present,
%% the default device (`httpsig@1.0') is used.
id_device(#{ <<"commitments">> := Commitments }, Opts) ->
    % Get the device from the first commitment.
    UnfilteredDevs =
        hb_maps:map(
            fun(_, #{ <<"commitment-device">> := CommitmentDev }) ->
                CommitmentDev;
            (_, _) -> undefined
            end,
            Commitments,
            Opts
        ),
    % Filter out the undefined devices.
    Devs =
        lists:filter(
            fun(Dev) -> Dev =/= undefined end,
            hb_maps:values(UnfilteredDevs, Opts)
        ),
    % If there are no devices, return the default.
    case Devs of
        [] -> {ok, ?DEFAULT_ID_DEVICE};
        [Dev] -> {ok, Dev};
        [FirstDev|Rest] ->
            % If there are multiple devices amongst the set, err.
            MultiDeviceMessage = lists:all(fun(Dev) -> Dev =:= FirstDev end, Rest),
            case MultiDeviceMessage of
                false -> {error, {multiple_id_devices, Devs}};
                true -> {ok, FirstDev}
            end
    end;
id_device(_, _) ->
    {ok, ?DEFAULT_ID_DEVICE}.

%% @doc Return the committers of a message that are present in the given request.
-spec committers(#{ commitments => #{ _ => _ }, _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, [_]}.
committers(Base) -> committers(Base, #{}).
committers(Base, Req) -> committers(Base, Req, #{}).
committers(Base, Req, NodeOpts) ->
    CommitterBase = committer_lookup_base(Base, NodeOpts),
    case maps:get(<<"commitments">>, CommitterBase, not_found) of
        not_found ->
            case commitment_lookup_mode(NodeOpts) of
                top -> {ok, []};
                inherited ->
                    case hb_maps:get(<<"commitments">>, CommitterBase, not_found, NodeOpts) of
                        not_found -> {ok, []};
                        Commitments -> {ok, committers_from_commitments(Commitments, NodeOpts)}
                    end
            end;
        Commitments ->
            ParentCommitters =
                case map_size(Req) == 0 andalso has_extension_commitment(CommitterBase, NodeOpts) of
                    true ->
                        case extension_parent(CommitterBase, NodeOpts) of
                            {ok, Parent} -> hb_message:signers(Parent, NodeOpts);
                            error -> []
                        end;
                    false -> []
                end,
            {ok,
                lists:usort(
                    committers_from_commitments(Commitments, NodeOpts)
                        ++ ParentCommitters
                )
            }
    end.

committer_lookup_base(Base, NodeOpts) when is_map(Base) ->
    case commitment_lookup_mode(NodeOpts) of
        top -> Base;
        inherited ->
            hb_maps:flatten(
                Base,
                maps:remove(<<"preserve-message-extension">>, NodeOpts)
            )
    end;
committer_lookup_base(Base, _NodeOpts) ->
    Base.

committers_from_commitments(Commitments, NodeOpts) ->
    hb_maps:values(
        hb_maps:filtermap(
            fun(_ID, Commitment) ->
                case hb_maps:get(<<"committer">>, Commitment, undefined, NodeOpts) of
                    undefined -> false;
                    Committer -> {true, Committer}
                end
            end,
            Commitments,
            NodeOpts
        ),
        NodeOpts
    ).

%% @doc Commit to a message, using the `commitment-device' key to specify the
%% device that should be used to commit to the message. If the key is not set,
%% the default device (`httpsig@1.0') is used.
-spec commit(#{ _ => _ },
    #{ 'commitment-device' => binary(), type => binary(), _ => _ },
    #{ _ => _ }
) -> {ok, #{ commitments := #{ _ => _ }, _ => _ }}.
commit(Self, Req, Opts) ->
    {ok, Base} = hb_message:find_target(Self, Req, Opts),
    AttDev =
        case hb_maps:get(<<"commitment-device">>, Req, not_specified, Opts) of
            not_specified ->
                hb_opts:get(commitment_device, no_viable_commitment_device, Opts);
            Dev -> Dev
        end,
    % We _do not_ set the `device' key in the message, as the device will be
    % part of the commitment. Instead, we find the device module's `commit'
    % function and apply it.
    BaseCommitOpts =
        case hb_maps:get(<<"linkify-mode">>, Req, not_specified, Opts) of
            not_specified ->
                case hb_util:atom(hb_maps:get(<<"bundle">>, Req, false, Opts)) of
                    true ->
                        Opts#{
                            <<"linkify-mode">> => false,
                            <<"load-all-commitments">> => true
                        };
                    false ->
                        case hb_maps:get(<<"type">>, Req, <<"signed">>) of
                            <<"unsigned">> ->
                                Opts#{ <<"linkify-mode">> => discard };
                            _ ->
                                Opts#{ <<"linkify-mode">> => offload }
                        end
                end;
            Mode ->
                Opts#{ <<"linkify-mode">> => Mode }
        end,
    CommitOpts = maybe_preserve_message_extension(Base, Req, BaseCommitOpts, true),
    % Encode to a TABM. The `bundle' flag (when set on the request) is the
    % caller's intent for the top-level commit and applies only to the root
    % message; `hint-device' lets the structured codec preserve each nested
    % commitment's own bundle state per-node.
    SourceSpec =
        hb_message:add_bundle_hint(
            #{ <<"device">> => <<"structured@1.0">> },
            Req#{ <<"device">> => AttDev },
            CommitOpts
        ),
    BaseTABM = hb_message:convert(Base, tabm, SourceSpec, CommitOpts),
    Loaded =
        case hb_util:atom(hb_maps:get(<<"bundle">>, Req, false, CommitOpts)) of
            true ->
                load_bundle_children_commitments(
                    hb_cache:ensure_all_loaded(
                        hb_link:decode_all_links(BaseTABM),
                        CommitOpts
                    ),
                    CommitOpts
                );
            false ->
                ensure_commitments_loaded(BaseTABM, CommitOpts)
        end,
    CommitReq =
        maybe_extension_commit_request(
            Loaded,
            Req#{ <<"type">> => maps:get(<<"type">>, Req, <<"signed">>) },
            CommitOpts
        ),
    {ok, Committed} =
        hb_ao:raw(
            AttDev,
            <<"commit">>,
            Loaded,
            CommitReq,
            CommitOpts
        ),
    {ok, hb_message:convert(Committed, <<"structured@1.0">>, tabm, CommitOpts)}.

%% @doc Verify a message. By default, all commitments are verified. The
%% `committers' key in the request can be used to specify that only the 
%% commitments from specific committers should be verified. Similarly, specific
%% commitments can be specified using the `commitments' key.
-spec verify(#{ _ => _ },
    #{ committers => _, 'commitment-ids' => _, commitments => _, _ => _ },
    #{ _ => _ }
) -> {ok, boolean()}.
verify(Self, Req, Opts) ->
    % Get the target message of the verification request.
    {ok, RawBase} = hb_message:find_target(Self, Req, Opts),
    RawWithCommitments = ensure_commitments_loaded(RawBase, Opts),
    VerifyOpts =
        maybe_preserve_message_extension(
            RawWithCommitments,
            Req,
            Opts#{
                <<"linkify-mode">> => discard
            },
            false
        ),
    BaseViewOpts = VerifyOpts#{
        <<"linkify-mode">> => false,
        <<"preserve-message-extension">> => true
    },
    {Base, BaseOpts} =
        case needs_loaded_bundle_view(RawWithCommitments, VerifyOpts) of
            true ->
                loaded_bundle_verify_view(RawWithCommitments, VerifyOpts);
            false ->
                {
                    hb_message:convert(
                        RawWithCommitments,
                        tabm,
                        BaseViewOpts
                    ),
                    VerifyOpts
                }
        end,
    ?event(verify, {verify, {base_found, Base}}),
    Commitments =
        committer_lookup_commitments(Base, commitments(Base, BaseOpts), BaseOpts),
    IDsToVerify =
        commitment_ids_from_request(
            commitment_lookup_base(Base, Req, Commitments),
            Req,
            BaseOpts
        ),
    % Generate the new commitment request base messsage by removing the keys
    % used by this function (path, committers, commitments) and returning the
    % remaining keys. This message will then be merged with each commitment
    % message to generate the final request, allowing the caller to pass
    % additional keys to the commitment device.
    ReqBase =
        maps:without(
            [
                <<"path">>,
                <<"committers">>,
                <<"commitments">>,
                <<"commitment-ids">>
            ],
            Req
        ),
    % Verify the commitments. Stop execution if any fail.
    Res =
        lists:all(
            fun(CommitmentID) ->
                Commitment = maps:get(CommitmentID, Commitments),
                RawCommitmentBase =
                    commitment_raw_base(
                        RawWithCommitments,
                        CommitmentID,
                        Commitment,
                        BaseOpts
                    ),
                {CommitmentBase, CommitmentOpts} =
                    commitment_verify_view(
                        RawCommitmentBase,
                        Base,
                        Commitment,
                        BaseOpts
                    ),
                {ok, Res} =
                    verify_commitment(
                        CommitmentBase,
                        (maps:merge(ReqBase, Commitment))#{
                            <<"commitment-ids">> => [CommitmentID]
                        },
                        CommitmentOpts
                    ),
                ?event(verify,
                    {verify_commitment_res,
                        {commitment_id, CommitmentID},
                        {res, Res}
                    }),
                Res
            end,
            IDsToVerify
        ),
    ?event(verify, {verify, {res, Res}}),
    {ok, Res}.

commitment_raw_base(RawBase, CommitmentID, Commitment, Opts) ->
    Base =
        case hb_opts:get(<<"preserve-message-extension">>, false, Opts) of
            true -> RawBase;
            false -> hb_maps:flatten(RawBase, Opts)
        end,
    Base#{ <<"commitments">> => #{ CommitmentID => Commitment } }.

commitment_verify_view(RawBase, DefaultBase, Commitment, Opts) ->
    case commitment_needs_loaded_bundle_view(
        RawBase,
        DefaultBase,
        Commitment,
        Opts
    ) of
        true -> loaded_bundle_verify_view(RawBase, Opts);
        false ->
            ConvertOpts =
                commitment_convert_opts(
                    RawBase,
                    DefaultBase,
                    Commitment,
                    Opts
                ),
            SourceSpec =
                hb_message:add_bundle_hint(
                    #{ <<"device">> => <<"structured@1.0">> },
                    #{
                        <<"device">> =>
                            maps:get(
                                <<"commitment-device">>,
                                Commitment,
                                undefined
                            )
                    },
                    ConvertOpts
                ),
            {hb_message:convert(RawBase, tabm, SourceSpec, ConvertOpts), Opts}
    end.

commitment_convert_opts(RawBase, DefaultBase, Commitment, Opts) ->
    PreserveOpts = Opts#{ <<"preserve-message-extension">> => true },
    case hb_util:atom(hb_maps:get(<<"bundle">>, Commitment, false, Opts)) of
        true ->
            case
                commitment_has_linked_fields(RawBase, Commitment, Opts)
                    orelse commitment_has_linked_fields(
                        DefaultBase,
                        Commitment,
                        Opts
                    )
            of
                true -> PreserveOpts;
                false -> Opts#{ <<"linkify-mode">> => false }
            end;
        false -> Opts
    end.

loaded_bundle_verify_view(Msg, Opts) ->
    LoadedOpts =
        Opts#{
            <<"linkify-mode">> => false,
            <<"load-all-commitments">> => true
        },
    Loaded =
        load_bundle_commitments_deep(
            hb_cache:ensure_all_loaded(
                hb_link:decode_all_links(Msg),
                LoadedOpts
            ),
            LoadedOpts
        ),
    {
        hb_message:convert(
            Loaded,
            tabm,
            LoadedOpts
        ),
        LoadedOpts
    }.

load_bundle_commitments_deep(Msg, Opts) when is_map(Msg) ->
    Loaded = load_top_commitments(Msg, Opts),
    lists:foldl(
        fun(Key, Acc) ->
            case maps:find(Key, Acc) of
                {ok, Value} ->
                    maps:put(Key, load_bundle_commitments_deep(Value, Opts), Acc);
                error ->
                    Acc
            end
        end,
        Loaded,
        bundle_child_keys(Loaded, Opts)
    );
load_bundle_commitments_deep(List, Opts) when is_list(List) ->
    lists:map(fun(Value) -> load_bundle_commitments_deep(Value, Opts) end, List);
load_bundle_commitments_deep(Value, _Opts) ->
    Value.

load_bundle_children_commitments(Msg, Opts) when is_map(Msg) ->
    maps:map(
        fun(Key, Value) when Key == <<"commitments">> orelse Key == <<"priv">> ->
            Value;
           (_Key, Value) ->
            load_bundle_commitments_deep(Value, Opts)
        end,
        Msg
    );
load_bundle_children_commitments(Value, _Opts) ->
    Value.

load_top_commitments(Msg = #{ <<"commitments">> := Commitments }, _Opts)
        when map_size(Commitments) > 0 ->
    Msg;
load_top_commitments(Msg, Opts) ->
    hb_cache:read_all_commitments(Msg, Opts).

bundle_child_keys(Msg, Opts) ->
    lists:usort(
        lists:flatmap(
            fun({_ID, Commitment}) ->
                case hb_util:atom(
                    hb_maps:get(<<"bundle">>, Commitment, false, Opts)
                ) of
                    true ->
                        numbered_committed_keys(
                            committed_keys_for_commitment(Commitment, Opts)
                        );
                    false ->
                        []
                end
            end,
            maps:to_list(hb_maps:get(<<"commitments">>, Msg, #{}, Opts))
        )
    ).

numbered_committed_keys(Keys) ->
    lists:filtermap(fun numbered_committed_key/1, Keys).

numbered_committed_key(Key) ->
    BinKey = hb_link:remove_link_specifier(hb_util:bin(Key)),
    try binary_to_integer(BinKey) of
        Int when Int > 0 -> {true, BinKey};
        _ -> false
    catch
        _:_ -> false
    end.

commitment_needs_loaded_bundle_view(RawBase, _DefaultBase, Commitment, Opts) ->
    original_tagged_bundle_commitment(Commitment, Opts)
        orelse direct_bundle_commitment_has_linked_fields(
            RawBase,
            Commitment,
            Opts
        )
        orelse needs_loaded_bundle_view(RawBase, Opts).

direct_bundle_commitment_has_linked_fields(Msg, Commitment, Opts) ->
    hb_util:atom(hb_maps:get(<<"bundle">>, Commitment, false, Opts))
        andalso is_map(Msg)
        andalso lists:any(
            fun(Key) -> direct_committed_field_has_link(Msg, Key) end,
            committed_keys_for_commitment(Commitment, Opts)
        ).

direct_committed_field_has_link(Msg, Key) when is_map(Msg) ->
    BinKey = hb_util:bin(Key),
    LinkKey = <<BinKey/binary, "+link">>,
    case maps:find(BinKey, Msg) of
        {ok, Value} -> ?IS_LINK(Value);
        error -> maps:is_key(LinkKey, Msg)
    end;
direct_committed_field_has_link(_Msg, _Key) ->
    false.

commitment_has_linked_fields(Msg, Commitment, Opts) ->
    is_map(Msg)
        andalso lists:any(
            fun(Key) -> committed_field_has_link(Msg, Key) end,
            committed_keys_for_commitment(Commitment, Opts)
        ).

committed_field_has_link(Msg, Key) when is_map(Msg) ->
    BinKey = hb_util:bin(Key),
    LinkKey = <<BinKey/binary, "+link">>,
    case maps:find(BinKey, Msg) of
        {ok, Value} -> has_link_value(Value);
        error -> maps:is_key(LinkKey, Msg)
    end;
committed_field_has_link(_Msg, _Key) ->
    false.

has_link_value(Value) when ?IS_LINK(Value) ->
    true;
has_link_value(Msg) when is_map(Msg) ->
    lists:any(
        fun({_Key, Value}) -> has_link_value(Value) end,
        maps:to_list(maps:without([<<"commitments">>, <<"priv">>], Msg))
    );
has_link_value(List) when is_list(List) ->
    lists:any(fun has_link_value/1, List);
has_link_value(_Value) ->
    false.

has_bundle_commitment(Msg, Opts) ->
    lists:any(
        fun({_ID, Commitment}) ->
            hb_util:atom(hb_maps:get(<<"bundle">>, Commitment, false, Opts))
        end,
        maps:to_list(hb_maps:get(<<"commitments">>, Msg, #{}, Opts))
    ).

needs_loaded_bundle_view(Msg, Opts) ->
    has_bundle_commitment_deep(Msg, Opts)
        andalso (
            not has_bundle_commitment(Msg, Opts)
            orelse has_original_tagged_bundle_commitment(Msg, Opts)
        ).

has_original_tagged_bundle_commitment(Msg, Opts) ->
    lists:any(
        fun({_ID, Commitment}) ->
            original_tagged_bundle_commitment(Commitment, Opts)
        end,
        maps:to_list(hb_maps:get(<<"commitments">>, Msg, #{}, Opts))
    ).

original_tagged_bundle_commitment(Commitment, Opts) ->
    hb_util:atom(hb_maps:get(<<"bundle">>, Commitment, false, Opts))
        andalso
            hb_maps:get(
                <<"original-tags">>,
                Commitment,
                undefined,
                Opts
            ) =/= undefined.

has_bundle_commitment_deep(Msg, Opts) when is_map(Msg) ->
    has_bundle_commitment(Msg, Opts)
        orelse lists:any(
            fun({_Key, Value}) -> has_bundle_commitment_deep(Value, Opts) end,
            maps:to_list(maps:without([<<"commitments">>, <<"priv">>], Msg))
        );
has_bundle_commitment_deep(List, Opts) when is_list(List) ->
    lists:any(fun(Value) -> has_bundle_commitment_deep(Value, Opts) end, List);
has_bundle_commitment_deep(_Msg, _Opts) ->
    false.

%% @doc Execute a function for a single commitment in the context of its
%% parent message.
%% Note: Assumes that the `commitments' key has already been removed from the
%% message if applicable.
verify_commitment(Base, Commitment, Opts) ->
    ?event(verify, {verifying_commitment, {commitment, Commitment}, {msg, Base}}),
    AttDev =
        hb_maps:get(
            <<"commitment-device">>,
            Commitment,
            ?DEFAULT_ATT_DEVICE,
            Opts
        ),
    hb_ao:raw(AttDev, <<"verify">>, Base, Commitment, Opts).

%% @doc Return the list of committed keys from a message.
-spec committed(#{ _ => _ },
    #{ raw => boolean(), committers => _, 'commitment-ids' => _, _ => _ },
    #{ _ => _ }
) -> {ok, [binary()]}.
committed(Self, Req, Opts) ->
    % Get the target message of the verification request and ensure its 
    % commitments are loaded.
    {ok, RawBase} =
        hb_message:find_target(
            Self,
            Req,
            Opts
        ),
    CommittedOpts = maybe_preserve_message_extension(RawBase, Req, Opts, false),
    Base = ensure_commitments_loaded(RawBase, CommittedOpts),
    CommitmentIDs = commitment_ids_from_request(Base, Req, CommittedOpts),
    ?event(debug_commitments,
        {calculating_committed,
            {commitment_ids, CommitmentIDs},
            {req, Req}
        }
    ),
    Commitments = commitments(Base, CommittedOpts),
    % Get the list of committed keys from each committer.
    RawCommittedKeys =
        lists:map(
            fun(CommitmentID) ->
                Commitment = maps:get(CommitmentID, Commitments),
                % The committed keys will be a TABM encoded numbered map
                % so we must decode it to its underlying list of normalized keys
                % for comparison purposes.
                hb_util:message_to_ordered_list(
                    maps:get(<<"committed">>, Commitment),
                    CommittedOpts
                )
            end,
            CommitmentIDs
        ),
    CommitmentKeys =
        normalize_committed_key_lists(
            RawCommittedKeys,
            maps:get(<<"raw">>, Req, false)
        ),
    % Remove commitments that are not in *every* committer's list.
    % To start, we need to create the super-set of committed keys.
    AllCommittedKeys =
        lists:foldr(
            fun(Key, Acc) ->
                case lists:member(Key, Acc) of
                    true -> Acc;
                    false -> [Key | Acc]
                end
            end,
            [],
            lists:flatten(CommitmentKeys)
        ),
    % Next, we filter the list of all committed keys to only include those that
    % are present in every committer's list.
    OnlyCommittedKeys =
        lists:filter(
            fun(Key) ->
                lists:all(
                    fun(CommittedKeys) -> lists:member(Key, CommittedKeys) end,
                    CommitmentKeys
                )
            end,
            AllCommittedKeys
        ),
    ?event(debug_commitments, {only_committed_keys, OnlyCommittedKeys}),
    {ok, OnlyCommittedKeys}.

normalize_committed_key_lists(CommitmentKeys, true) ->
    CommitmentKeys;
normalize_committed_key_lists(CommitmentKeys, false) ->
    lists:map(
        fun(Keys) ->
            lists:map(fun hb_link:remove_link_specifier/1, Keys)
        end,
        CommitmentKeys
    ).

%% @doc Return a message with only the relevant commitments for a given request.
%% See `commitment_ids_from_request/3' for more information on the request format.
with_relevant_commitments(Base, Req, Opts) ->
    Commitments = commitments(Base, Opts),
    CommitmentIDs = commitment_ids_from_request(Base, Req, Opts),
    Base#{ <<"commitments">> => maps:with(CommitmentIDs, Commitments) }.

%% @doc Implements a standardized form of specifying commitment IDs for a
%% message request. The caller may specify a list of committers (by address)
%% or a list of commitment IDs directly. They may specify both, in which case
%% the returned list will be the union of the two lists. In each case, they
%% may specify `all' or `none' for each group. If no specifiers are provided,
%% the default is `all' for commitments -- also implying `all' for committers.
commitment_ids_from_request(Base, Req, Opts) ->
    Commitments = commitments(Base, Opts),
    ReqCommitters =
        case maps:get(<<"committers">>, Req, <<"none">>) of
            X when is_list(X) -> X;
            CommitterDescriptor -> hb_ao:normalize_key(CommitterDescriptor)
        end,
    RawReqCommitments = maps:get(<<"commitment-ids">>, Req, <<"none">>),
    ReqCommitments =
        case RawReqCommitments of
            X2 when is_list(X2) -> X2;
            CommitmentDescriptor -> hb_ao:normalize_key(CommitmentDescriptor)
        end,
    ?event(debug_commitments,
        {commitment_ids_from_request,
            {req_commitments, ReqCommitments},
            {req_committers, ReqCommitters}}
    ),
    % Get the commitments to verify.
    FromCommitmentIDs =
        case ReqCommitments of
            <<"none">> -> [];
            <<"all">> -> hb_maps:keys(Commitments, Opts);
            CommitmentIDs ->
                if is_list(CommitmentIDs) -> CommitmentIDs;
                true -> [CommitmentIDs]
                end
        end,
    FromCommitterAddrs =
        case ReqCommitters of
            <<"none">> ->
                ?event(debug_commitments, no_commitment_ids_for_committers),
                [];
            <<"all">> ->
                {ok, Committers} = committers(Base, Req, Opts),
                ?event(debug_commitments, {commitment_ids_from_committers, Committers}),
                commitment_ids_from_committers(
                    Committers,
                    Commitments,
                    Opts
                );
            RawCommitterAddrs ->
                ?event(
                    debug_commitments,
                    {getting_commitment_ids_for_committers, RawCommitterAddrs}
                ),
                CommitterAddrs =
                    if is_list(RawCommitterAddrs) -> RawCommitterAddrs;
                    true -> [RawCommitterAddrs]
                    end,
                commitment_ids_from_committers(
                    CommitterAddrs,
                    Commitments,
                    Opts
                )
        end,
    Res =
        case FromCommitterAddrs ++ FromCommitmentIDs of
            [] ->
                % The request is for no committers, and no explicit commitments.
                % Subsequently, we return the commitment using the default
                % commitment device, if it exists.
                lists:filter(
                    fun(CommitmentID) ->
                        Comm = maps:get(CommitmentID, Commitments),
                        Dev = maps:get(<<"commitment-device">>, Comm, undefined),
                        case Dev of
                            ?DEFAULT_ATT_DEVICE ->
                                not hb_maps:is_key(<<"committer">>, Comm);
                            _ -> false
                        end
                    end,
                    maps:keys(Commitments)
                );
            FinalCommitmentIDs -> FinalCommitmentIDs
        end,
    ?event(
        debug_commitments,
        {commitment_ids_from_request, {base, Base}, {req, Req}, {res, Res}}
    ),
    Res.

commitments(Base, Opts) ->
    case commitment_lookup_mode(Opts) of
        top -> maps:get(<<"commitments">>, Base, #{});
        inherited -> hb_maps:get(<<"commitments">>, Base, #{}, Opts)
    end.

committer_lookup_commitments(Base, Commitments, Opts) ->
    case is_map(Base) of
        true ->
            Inherited = hb_maps:get(
                <<"commitments">>,
                hb_maps:flatten(Base, maps:remove(<<"preserve-message-extension">>, Opts)),
                #{},
                Opts
            ),
            maps:merge(Inherited, Commitments);
        false ->
            Commitments
    end.

commitment_lookup_base(Base, Req, Commitments) ->
    case maps:get(<<"committers">>, Req, <<"none">>) of
        <<"none">> -> Base;
        _ -> Base#{ <<"commitments">> => Commitments }
    end.

commitment_lookup_mode(Opts) ->
    case hb_opts:get(<<"preserve-message-extension">>, false, Opts) of
        true -> top;
        false -> inherited
    end.

%% @doc Ensure that the `commitments` submessage of a base message is fully
%% present and loaded into local memory. We normally flatten message extensions
%% (`...') so inherited commitments surface to the top. If commitment handling is
%% explicitly preserving the extension edge, the top layer is kept intact instead.
ensure_commitments_loaded(RawM, Opts) ->
    M0 =
        case hb_opts:get(<<"preserve-message-extension">>, false, Opts) of
            true -> RawM;
            false -> hb_maps:flatten(RawM, Opts)
        end,
    case M0 of
        M = #{ <<"commitments">> := L } when ?IS_LINK(L) ->
            M#{ <<"commitments">> => hb_cache:ensure_all_loaded(L, Opts) };
        M ->
            M
    end.

maybe_preserve_message_extension(Msg, Req, Opts, DefaultPreserve) ->
    case should_preserve_message_extension(Msg, Req, Opts, DefaultPreserve) of
        true -> Opts#{ <<"preserve-message-extension">> => true };
        false -> Opts
    end.

should_preserve_message_extension(Msg, Req, Opts, DefaultPreserve) ->
    has_message_extension(Msg)
        andalso (
            (
                explicit_commits_extension(Req, Opts)
                andalso not requests_specific_committers(Req)
            )
            orelse (
                has_extension_commitment(Msg, Opts)
                andalso not requests_specific_committers(Req)
            )
            orelse (
                DefaultPreserve
                andalso not requests_specific_committers(Req)
                andalso maps:get(<<"committed">>, Req, not_found) =:= not_found
            )
        ).

has_message_extension(Msg) when is_map(Msg) ->
    maps:is_key(<<"...">>, Msg) orelse maps:is_key(<<"...+link">>, Msg);
has_message_extension(_Msg) ->
    false.

requests_specific_committers(Req) ->
    case maps:get(<<"committers">>, Req, <<"none">>) of
        <<"none">> -> false;
        <<"all">> -> false;
        _ -> true
    end.

extension_parent(Base, Opts) ->
    case maps:find(<<"...">>, Base) of
        {ok, Parent} ->
            {ok, hb_cache:ensure_loaded(Parent, Opts)};
        error ->
            case maps:find(<<"...+link">>, Base) of
                {ok, ID} when is_binary(ID) ->
                    hb_cache:read(ID, hb_store:scope(Opts, local));
                {ok, Parent} ->
                    {ok, hb_cache:ensure_loaded(Parent, Opts)};
                error ->
                    error
            end
    end.

explicit_commits_extension(Req, Opts) ->
    case maps:get(<<"committed">>, Req, not_found) of
        not_found ->
            false;
        Committed ->
            lists:member(
                <<"...">>,
                lists:map(
                    fun hb_link:remove_link_specifier/1,
                    hb_util:message_to_ordered_list(Committed, Opts)
                )
            )
    end.

has_extension_commitment(Msg, Opts) ->
    Commitments = maps:get(<<"commitments">>, Msg, #{}),
    lists:any(
        fun({_ID, Commitment}) ->
            lists:member(
                <<"...">>,
                committed_keys_for_commitment(Commitment, Opts)
            )
        end,
        hb_maps:to_list(Commitments, Opts)
    ).

maybe_extension_commit_request(Msg, Req, _Opts) ->
    case {has_message_extension(Msg), maps:is_key(<<"committed">>, Req)} of
        {true, false} ->
            Req#{ <<"committed">> => extension_commitment_keys(Msg) };
        _ ->
            Req
    end.

extension_commitment_keys(Msg) ->
    lists:sort(maps:keys(maps:without([<<"commitments">>, <<"priv">>], Msg))).

%% @doc Returns a list of commitment IDs in a commitments map that are relevant
%% for a list of given committer addresses.
commitment_ids_from_committers(CommitterAddrs, Commitments, Opts) ->
    % Get the IDs of all commitments for each committer.
    Comms =
        lists:map(
            fun(RawCommitterAddr) ->
                CommitterAddr = hb_cache:ensure_loaded(RawCommitterAddr, Opts),
                % For each committer, filter the commitments to only
                % include those with the matching committer address.
                IDs = 
                    maps:values(maps:filtermap(
                        fun(ID, Msg) ->
                            % If the committer address matches, return
                            % the ID. If not, ignore the commitment.
                            case hb_maps:get(<<"committer">>, Msg, undefined) of
                                CommitterAddr -> {true, ID};
                                _ -> false
                            end
                        end,
                        Commitments
                    )
                ),
                FallbackIDs =
                    case IDs of
                        [] ->
                            lists:filter(
                                fun(ID) -> maps:is_key(ID, Commitments) end,
                                [RawCommitterAddr, CommitterAddr]
                            );
                        _ ->
                            IDs
                    end,
                {CommitterAddr, FallbackIDs}
            end,
            CommitterAddrs
        ),
    % Check that each committer has at least one commitment.
    EachCommitterHasCommitment =
        lists:all(fun({_, IDs}) -> IDs =/= [] end, Comms),
    % If all committers have at least one commitment, return the
    % IDs of all commitments. If any committer does not have a
    % commitment, error.
    case EachCommitterHasCommitment of
        true -> lists:flatten([ IDs || {_, IDs} <- Comms ]);
        false ->
            % Get the list of committers that do not have a
            % commitment.
            MissingCommitters =
                [
                    MissingCommitter
                ||
                    {MissingCommitter, []} <- Comms
                ],
            throw(
                {verify,
                    {requested_committers_not_found,
                        {missing_commitments, MissingCommitters}
                    }
                }
            )
    end.

%% @doc Deep merge keys in a message. Takes a map of key-value pairs and sets
%% them in the message, overwriting any existing values.
-spec set(#{ _ => _ }, #{ 'set-mode' => binary(), _ => _ }, #{ _ => _ }) ->
    {ok, #{ _ => _ }}.
set(Base, NewValuesMsg, Opts) ->
    OriginalPriv = hb_private:from_message(Base),
	% Filter keys that are in the default device (this one).
    {ok, NewValuesKeys} = keys(NewValuesMsg, Opts),
	KeysToSet =
		lists:filter(
			fun(Key) ->
				not lists:member(Key, ?DEVICE_KEYS ++ [<<"set-mode">>]) andalso
					(hb_maps:get(Key, NewValuesMsg, undefined, Opts) =/= undefined)
			end,
			NewValuesKeys
		),
	% Find keys in the message that are already set (case-insensitive), and 
	% note them for removal.
	ConflictingKeys =
		lists:filter(
			fun(Key) -> lists:member(Key, KeysToSet) end,
			hb_maps:keys(Base, Opts)
		),
    UnsetKeys =
        lists:filter(
            fun(Key) ->
                case hb_maps:get(Key, NewValuesMsg, not_found, Opts) of
                    unset -> true;
                    _ -> false
                end
            end,
            hb_maps:keys(Base, Opts)
        ),
    % Base message with keys-to-unset removed
    BaseValues = hb_private:reset(without_visible(UnsetKeys, Base, Opts)),
    ?event(debug_message_set,
        {performing_set,
            {conflicting_keys, ConflictingKeys},
            {keys_to_unset, UnsetKeys},
            {new_values, NewValuesMsg},
            {original_message, Base}
        }
    ),
    % Create the map of new values
    NewValues = hb_maps:from_list(
        lists:filtermap(
            fun(Key) ->
                case hb_maps:get(Key, NewValuesMsg, undefined, Opts) of
                    undefined -> false;
                    unset -> false;
                    Value -> {true, {Key, Value}}
                end
            end,
            KeysToSet
        )
    ),
    % Calculate if the keys to be set conflict with any committed keys.
    CommittedKeys = committed_keys_for_any(Base, Opts),
    ?event(message_set,
        {setting,
            {committed_keys, CommittedKeys},
            {keys_to_set, KeysToSet},
            {message, Base}
        }
    ),
    TouchedCommittedKeys =
        lists:filtermap(
            fun(Key) ->
                NormKey = hb_ao:normalize_key(Key),
                ?event({checking_committed_key, {key, Key}, {norm_key, NormKey}}),
                Res = case lists:member(NormKey, KeysToSet ++ UnsetKeys) of
                    true -> {true, NormKey};
                    false -> false
                end,
                Res
            end,
            CommittedKeys
        ),
    % Combine the new values with the base. The result EXTENDS the base via the
    % reserved `...' key rather than copying its keys: an `explicit' set lays the
    % new values directly atop the base, while a `deep' set additionally merges
    % nested submessages (see do_deep_merge/3). Either way the base is left intact
    % under `...' and key lookups fall through to it.
    Merged =
        hb_private:set_priv(
            case {CommittedKeys, maps:get(<<"set-mode">>, NewValuesMsg, <<"deep">>)} of
                {[], <<"explicit">>} ->
                    maps:merge(base_values_map(BaseValues), NewValues);
                {[], _} -> do_flat_deep_merge(BaseValues, NewValues, Opts);
                {_, <<"explicit">>} -> NewValues#{ <<"...">> => BaseValues };
                {_, _} -> do_deep_merge(BaseValues, NewValues, Opts)
            end,
            OriginalPriv
        ),
    ChangedAOTypeKeys =
        changed_ao_types_key(Base, Merged, CommittedKeys, Opts),
    OverwrittenCommittedKeys =
        lists:usort(TouchedCommittedKeys ++ ChangedAOTypeKeys),
    ?event({setting, {overwritten_committed_keys, OverwrittenCommittedKeys}}),
    case OverwrittenCommittedKeys of
        [] ->
            ?event(message_set, {no_overwritten_committed_keys, {merged, Merged}}),
            {ok, Merged};
        _ ->
            % We did overwrite some keys, but do their values match the original?
            % If not, we must remove the commitments. Both the base and the result
            % may be `...' extensions, so we compare their flattened (concrete)
            % views -- an overwritten key's value may be inherited through `...'.
            ChangedCommittedKeys =
                lists:usort(
                    changed_keys(Base, Merged, TouchedCommittedKeys, Opts)
                        ++ ChangedAOTypeKeys
                ),
            case ChangedCommittedKeys of
                [] ->
                    ?event(message_set, {set_keys_matched, {merged, Merged}}),
                    {ok, Merged};
                % {error, {Details, {trace, Stacktrace}}} ->
                %     erlang:raise(error, Details, Stacktrace);
                % {mismatch, Type, Path, Val1, Val2} ->
                %     ?event(
                %         set_conflict,
                %         {set_conflict_removing_commitments,
                %             {merged, Merged},
                %             {mismatch, Type},
                %             {path, Path},
                %             {expected, Val1},
                %             {received, Val2}
                %         }
                %     ),
                _ ->
                    % A committed key changed value, so any commitment covering
                    % that key no longer holds.
                    {ok, drop_commitments_for_keys(Merged, ChangedCommittedKeys, Opts)}
            end
    end.

committed_keys_for_any(Base, Opts) ->
    Commitments = hb_maps:get(<<"commitments">>, Base, #{}, Opts),
    lists:usort(
        lists:flatten(
            [
                committed_keys_for_commitment(Commitment, Opts)
            ||
                {_ID, Commitment} <- maps:to_list(Commitments)
            ]
        )
    ).

committed_keys_for_commitment(Commitment, Opts) ->
    lists:map(
        fun hb_link:remove_link_specifier/1,
        hb_util:message_to_ordered_list(
            hb_maps:get(<<"committed">>, Commitment, [], Opts),
            Opts
        )
    ).

changed_keys(Base, Merged, Keys, Opts) ->
    FlatBase = hb_maps:flatten(Base, Opts),
    FlatMerged = hb_maps:flatten(Merged, Opts),
    lists:filter(
        fun(Key) ->
            BaseKey = hb_maps:with([Key], FlatBase, Opts),
            MergedKey = hb_maps:with([Key], FlatMerged, Opts),
            hb_message:match(MergedKey, BaseKey, strict, Opts) =/= true
        end,
        Keys
    ).

changed_ao_types_key(Base, Merged, CommittedKeys, Opts) ->
    case lists:member(<<"ao-types">>, CommittedKeys) of
        false ->
            [];
        true ->
            case ao_types(Base, Opts) =:= ao_types(Merged, Opts) of
                true -> [];
                false -> [<<"ao-types">>]
            end
    end.

ao_types(Msg, Opts) ->
    TABM =
        hb_message:convert(
            hb_message:uncommitted(hb_private:reset(Msg), Opts),
            tabm,
            <<"structured@1.0">>,
            Opts
        ),
    hb_maps:get(<<"ao-types">>, TABM, <<>>, Opts).

drop_commitments_for_keys(Msg, Keys, Opts) ->
    case hb_maps:get(<<"commitments">>, Msg, not_found, Opts) of
        not_found ->
            Msg;
        Commitments ->
            NormKeys = lists:usort(lists:map(fun hb_ao:normalize_key/1, Keys)),
            FilteredCommitments =
                hb_maps:filter(
                    fun(_ID, Commitment) ->
                        not intersects(
                            committed_keys_for_commitment(Commitment, Opts),
                            NormKeys
                        )
                    end,
                    Commitments,
                    Opts
                ),
            maybe_update_commitments(Msg, FilteredCommitments)
    end.

maybe_update_commitments(Msg = #{ <<"...">> := _ }, Commitments)
        when map_size(Commitments) == 0 ->
    Msg#{ <<"commitments">> => #{} };
maybe_update_commitments(Msg = #{ <<"...+link">> := _ }, Commitments)
        when map_size(Commitments) == 0 ->
    Msg#{ <<"commitments">> => #{} };
maybe_update_commitments(Msg, Commitments) when map_size(Commitments) == 0 ->
    maps:remove(<<"commitments">>, Msg);
maybe_update_commitments(Msg, Commitments) ->
    Msg#{ <<"commitments">> => Commitments }.

intersects(A, B) ->
    lists:any(fun(Item) -> lists:member(Item, B) end, A).

%% @doc Deep merge keys into a message, producing a `...' EXTENSION of the base
%% rather than a flat copy. For each new value we either:
%%   - deep-set it onto the base's submessage, when both the new value and the
%%     base's value at that key are messages -- recursing through `set' so the
%%     nested result is itself a `...' extension; or
%%   - lay it directly atop the base (scalars, and keys the base does not hold as
%%     a submessage).
%% The changed keys are then laid over the (unchanged) base via `...'. An empty
%% change set returns the base unchanged. The base is found via `hb_maps:find',
%% which resolves both links and any `...' extension on the base itself.
do_deep_merge(BaseValues, NewValues, Opts) ->
    do_deep_merge(BaseValues, NewValues, is_list(BaseValues), Opts).

do_deep_merge(BaseValues, NewValues, true, Opts) ->
    hb_maps:merge(hb_util:list_to_numbered_message(BaseValues), NewValues, Opts);
do_deep_merge(BaseValues, NewValues, false, Opts) ->
    Changed =
        maps:fold(
            fun(Key, NewValue, Acc) when is_map(NewValue) ->
                case hb_maps:find(Key, BaseValues, Opts) of
                    {ok, BaseValue} when is_map(BaseValue) ->
                        Acc#{
                            Key =>
                                hb_util:ok(
                                    hb_ao:resolve(
                                        BaseValue,
                                        NewValue#{ <<"path">> => <<"set">> },
                                        Opts
                                    ),
                                    Opts
                                )
                        };
                    _ ->
                        Acc#{ Key => NewValue }
                end;
               (Key, NewValue, Acc) ->
                Acc#{ Key => NewValue }
            end,
            #{},
            NewValues
        ),
    case map_size(Changed) of
        0 -> BaseValues;
        _ -> Changed#{ <<"...">> => BaseValues }
    end.

%% @doc Deep merge keys into an uncommitted message. With no commitments to
%% preserve, keep the result concrete instead of creating a `...' extension.
do_flat_deep_merge(BaseValues, NewValues, Opts) ->
    do_flat_deep_merge(BaseValues, NewValues, is_list(BaseValues), Opts).

do_flat_deep_merge(BaseValues, NewValues, true, Opts) ->
    hb_util:deep_merge(
        hb_util:list_to_numbered_message(BaseValues),
        NewValues,
        Opts
    );
do_flat_deep_merge(BaseValues, NewValues, false, Opts) ->
    {WithNestedMerges, StillToDeepMerge} =
        maps:fold(
            fun(Key, NewValue, {Acc, ToDeepMerge})
                    when is_map(NewValue)
                    andalso is_map(map_get(Key, Acc)) ->
                        BaseValue = map_get(Key, Acc),
                        NewValueSet = NewValue#{ <<"path">> => <<"set">> },
                        {
                            Acc#{
                                Key =>
                                    hb_util:ok(
                                        hb_ao:resolve(
                                            BaseValue,
                                            NewValueSet,
                                            Opts
                                        ),
                                        Opts
                                    )
                            },
                            ToDeepMerge
                        };
            (Key, NewValue, {Acc, ToDeepMerge})
                    when is_map(NewValue)
                    andalso ?IS_LINK(map_get(Key, Acc)) ->
                LoadedBaseValue = hb_cache:ensure_loaded(map_get(Key, Acc), Opts),
                case is_map(LoadedBaseValue) of
                    true ->
                        NewValueSet = NewValue#{ <<"path">> => <<"set">> },
                        {
                            Acc#{
                                Key =>
                                    hb_util:ok(
                                        hb_ao:resolve(
                                            LoadedBaseValue,
                                            NewValueSet,
                                            Opts
                                        ),
                                        Opts
                                    )
                            },
                            ToDeepMerge
                        };
                    false ->
                        {Acc, [Key | ToDeepMerge]}
                end;
            (Key, _, {Acc, ToDeepMerge}) ->
                {Acc, [Key | ToDeepMerge]}
            end,
            {BaseValues, []},
            NewValues
        ),
    hb_util:deep_merge(
        WithNestedMerges,
        maps:with(StillToDeepMerge, NewValues),
        Opts
    ).

base_values_map(BaseValues) when is_list(BaseValues) ->
    hb_util:list_to_numbered_message(BaseValues);
base_values_map(BaseValues) ->
    BaseValues.

%% @doc Special case of `set/3' for setting the `path' key. This cannot be set
%% using the normal `set' function, as the `path' is a reserved key, used to
%% transmit the present key that is being executed. Subsequently, to call `path'
%% we would need to set `path' to `set', removing the ability to specify its 
%% new value.
-spec set_path(#{ path => _, _ => _ }, #{ value => _, _ => _ }, #{ _ => _ }) ->
    {ok, #{ _ => _ }} | #{ _ => _ }.
set_path(Base, #{ <<"value">> := Value }, Opts) ->
    set_path(Base, Value, Opts);
set_path(Base, Value, Opts) ->
    % Determine whether the `path' key is committed. If it is, we remove the
    % commitment if the new value is different. We try to minimize work by
    % doing the `hb_maps:get` first, as it is far cheaper than calculating
    % the committed keys.
    case {hb_maps:get(<<"path">>, Base, undefined, Opts), committed_keys_for_any(Base, Opts)} of
        {Value, _} ->
            Base;
        {_, CommittedKeys} ->
            case lists:member(<<"path">>, CommittedKeys) of
                true ->
                    OriginalPriv = hb_private:from_message(Base),
                    BaseValues = hb_private:reset(Base),
                    Merged =
                        case Value of
                            unset ->
                                #{
                                    <<"...">> =>
                                        drop_commitments_for_keys(
                                            without_visible([<<"path">>], BaseValues, Opts),
                                            [<<"path">>],
                                            Opts
                                        )
                                };
                            _ ->
                                #{ <<"path">> => Value, <<"...">> => BaseValues }
                        end,
                    hb_private:set_priv(
                        drop_commitments_for_keys(Merged, [<<"path">>], Opts),
                        OriginalPriv
                    );
                false ->
                    case Value of
                        unset -> {ok, without_visible([<<"path">>], Base, Opts)};
                        _ -> Base#{ <<"path">> => Value }
                    end
            end
    end.

without_visible([], Base, _Opts) ->
    Base;
without_visible(Keys, Base, Opts) ->
    hb_maps:without(Keys, hb_maps:flatten(Base, Opts), Opts).

%% @doc Remove a key or keys from a message.
-spec remove(#{ _ => _ }, #{ item => _, items => [_], _ => _ }, #{ _ => _ }) ->
    {ok, #{ _ => _ }}.
remove(Base, #{ <<"item">> := Key }, Opts) ->
    remove(Base, #{ <<"items">> => [Key] }, Opts);
remove(Base, #{ <<"items">> := Keys }, Opts) ->
    set(
        Base,
        #{ Key => unset || Key <- Keys },
        Opts
    ).

%% @doc Get the public keys of a message.
keys(Msg) ->
	keys(Msg, #{}).

keys(Msg, Opts) when not is_map(Msg) ->
    case hb_ao:normalize_keys(Msg, Opts) of
        NormMsg when is_map(NormMsg) -> keys(NormMsg, Opts);
        _ -> throw(badarg)
    end;
keys(Msg, Opts) ->
    {
        ok,
        lists:filter(
            fun(Key) -> not hb_private:is_private(Key) end,
            hb_maps:keys(hb_message:uncommitted(Msg, Opts), Opts)
        )
    }.

%% @doc Return the value associated with the key as it exists in the message's
%% underlying Erlang map. First check the public keys, then check case-
%% insensitively if the key is a binary.
get(Key, Msg, Opts) -> get(Key, Msg, #{ <<"path">> => <<"get">> }, Opts).
get(Key, Msg, _Req, Opts) ->
    case hb_private:is_private(Key) of
        true -> {error, not_found};
        false ->
            case hb_maps:get(Key, Msg, not_found, Opts) of
                not_found -> case_insensitive_get(Key, Msg, Opts);
                Value -> {ok, Value}
            end
    end.

%% @doc Key matching should be case insensitive, following RFC-9110, so we 
%% implement a case-insensitive key lookup rather than delegating to
%% `hb_maps:get/2'. Encode the key to a binary if it is not already.
case_insensitive_get(Key, Msg, Opts) ->
    NormKey = hb_util:to_lower(hb_util:bin(Key)),
    NormMsg = hb_ao:normalize_keys(Msg, Opts),
    case hb_maps:get(NormKey, NormMsg, not_found, Opts) of
        not_found -> {error, not_found};
        Value -> {ok, Value}
    end.

%%% Tests

%%% Internal module functionality tests:
get_keys_mod_test() ->
    ?assertEqual([a], hb_maps:keys(#{a => 1}, #{})).

is_private_mod_test() ->
    ?assertEqual(true, hb_private:is_private(<<"private">>)),
    ?assertEqual(true, hb_private:is_private(<<"private.foo">>)),
    ?assertEqual(false, hb_private:is_private(<<"a">>)).

%%% Device functionality tests:

keys_from_device_test() ->
    ?assertEqual({ok, [<<"a">>]}, hb_ao:resolve(#{ <<"a">> => 1 }, keys, #{})).

case_insensitive_get_test() ->
	?assertEqual({ok, 1}, case_insensitive_get(<<"a">>, #{ <<"a">> => 1 }, #{})),
%	?assertEqual({ok, 1}, case_insensitive_get(<<"a">>, #{ <<"A">> => 1 }, #{})),
	?assertEqual({ok, 1}, case_insensitive_get(<<"A">>, #{ <<"a">> => 1 }, #{})).
	%?assertEqual({ok, 1}, case_insensitive_get(<<"A">>, #{ <<"A">> => 1 }, #{})).

private_keys_are_filtered_test() ->
    ?assertEqual(
        {ok, [<<"a">>]},
        hb_ao:resolve(#{ <<"a">> => 1, <<"private">> => 2 }, keys, #{})
    ),
    ?assertEqual(
        {ok, [<<"a">>]},
        hb_ao:resolve(#{ <<"a">> => 1, <<"priv_foo">> => 4 }, keys, #{})
    ).

cannot_get_private_keys_test() ->
    ?assertEqual(
        {error, not_found},
        hb_ao:resolve(
            #{ <<"a">> => 1, <<"private_key">> => 2 },
            <<"private_key">>,
            #{ <<"hashpath">> => ignore }
        )
    ).

key_from_device_test() ->
    ?assertEqual({ok, 1}, hb_ao:resolve(#{ <<"a">> => 1 }, <<"a">>, #{})).

remove_test() ->
	Msg = #{ <<"key1">> => <<"Value1">>, <<"key2">> => <<"Value2">> },
	?assertMatch({ok, #{ <<"key2">> := <<"Value2">> }},
		hb_ao:resolve(
            Msg,
            #{ <<"path">> => <<"remove">>, <<"item">> => <<"key1">> },
            #{ <<"hashpath">> => ignore }
        )
    ),
	?assertMatch({ok, #{}},
		hb_ao:resolve(
            Msg,
            #{ <<"path">> => <<"remove">>, <<"items">> => [<<"key1">>, <<"key2">>] },
            #{ <<"hashpath">> => ignore }
        )
    ).

set_conflicting_keys_test() ->
	Base = #{ <<"dangerous">> => <<"Value1">> },
	Req = #{ <<"path">> => <<"set">>, <<"dangerous">> => <<"Value2">> },
	?assertMatch({ok, #{ <<"dangerous">> := <<"Value2">> }},
		hb_ao:resolve(Base, Req, #{})).

unset_with_set_test() ->
	Base = #{ <<"dangerous">> => <<"Value1">> },
	Req = #{ <<"path">> => <<"set">>, <<"dangerous">> => unset },
	?assertMatch({ok, Res} when ?IS_EMPTY_MESSAGE(Res),
		hb_ao:resolve(Base, Req, #{ <<"hashpath">> => ignore })).

deep_unset_test() ->
    Opts = #{ <<"hashpath">> => ignore },
    Base = #{
        <<"test-key1">> => <<"Value1">>,
        <<"deep">> => #{
            <<"test-key2">> => <<"Value2">>,
            <<"test-key3">> => <<"Value3">>
        }
    },
    Req = hb_ao:set(Base, #{ <<"deep/test-key2">> => unset }, Opts),
    ?assert(hb_message:match(#{
            <<"test-key1">> => <<"Value1">>,
            <<"deep">> => #{ <<"test-key3">> => <<"Value3">> }
        },
        Req,
        strict,
        Opts
    )),
    Res = hb_ao:set(Req, <<"deep/test-key3">>, unset, Opts),
    ?assert(hb_message:match(#{
            <<"test-key1">> => <<"Value1">>,
            <<"deep">> => #{}
        },
        Res,
        strict,
        Opts
    )),
    Msg4 = hb_ao:set(Res, #{ <<"deep">> => unset }, Opts),
    ?assert(hb_message:match(#{ <<"test-key1">> => <<"Value1">> }, Msg4, strict, Opts)).

set_ignore_undefined_test() ->
	Base = #{ <<"test-key">> => <<"Value1">> },
	Req = #{ <<"path">> => <<"set">>, <<"test-key">> => undefined },
	?assertEqual(#{ <<"test-key">> => <<"Value1">> },
		hb_private:reset(hb_util:ok(set(Base, Req, #{ <<"hashpath">> => ignore })))).

verify_test_() ->
	{foreach, fun () -> ok end, fun (_) -> ok end, [
		{"RSA", fun () -> test_verify(?RSA_KEY_TYPE) end},
		{"EDDSA", fun () -> test_verify(?EDDSA_KEY_TYPE) end},
        {"Solana", fun () -> test_verify(?SOLANA_KEY_TYPE) end},
        {"Ethereum", fun () -> test_verify(?ETHEREUM_KEY_TYPE) end}
	]}.

test_verify(KeyType) ->
    Unsigned = #{ <<"a">> => <<"b">> },
    Wallet = ar_wallet:new(KeyType),
    Signed = hb_message:commit(Unsigned, #{ <<"priv-wallet">> => Wallet }),
    ?event({signed, Signed}),
    BadSigned = Signed#{ <<"a">> => <<"c">> },
    ?event({bad_signed, BadSigned}),
    ?assertEqual(false, hb_message:verify(BadSigned)),
    ?assertEqual({ok, true},
        hb_ao:resolve(
            #{ <<"device">> => <<"message@1.0">> },
            #{ <<"path">> => <<"verify">>, <<"body">> => Signed },
            #{ <<"hashpath">> => ignore }
        )
    ),
    % Test that we can verify a message without specifying the device explicitly.
    ?assertEqual({ok, true},
        hb_ao:resolve(
            #{},
            #{ <<"path">> => <<"verify">>, <<"body">> => Signed },
            #{ <<"hashpath">> => ignore }
        )
    ).

set_nested_link_test() ->
    Opts = #{ <<"store">> => [hb_test_utils:test_store(hb_store_lmdb)] },

    Base = #{
        <<"balances">> => #{
            <<"device">> => <<"trie@1.0">>,
            <<"aa">> => <<"100">>,
            <<"bb">> => <<"200">>,
            <<"cc">> => <<"300">>
        },
        <<"other-key">> => <<"other-value">>
    },
    {ok, Path} = hb_cache:write(Base, Opts),
    {ok, LinkifiedBase} = hb_cache:read(Path, Opts),
    Req = #{
        <<"other-key">> => <<"new-value">>,
        <<"balances">> => #{
            <<"ab">> => <<"150">>
        }
    },
    {ok, Result} = set(LinkifiedBase, Req, Opts),
    Expected =
    #{
        <<"other-key">> => <<"new-value">>,
        <<"balances">> => #{
            <<"device">> => <<"trie@1.0">>,
            <<"a">> => #{
                <<"a">> => <<"100">>,
                <<"b">> => <<"150">>
            },
            <<"bb">> => <<"200">>,
            <<"cc">> => <<"300">>
        }
    },
    Matches = hb_message:match(Expected, Result, strict, Opts),
    ?assert(Matches).
