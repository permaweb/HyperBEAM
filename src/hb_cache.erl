%%% @doc A cache of AO-Core protocol messages and compute results.
%%%
%%% HyperBEAM stores all paths in key value stores, abstracted by the `hb_store'
%%% module. Each store has its own storage backend, but each works with simple
%%% key-value pairs. Each store can write binary keys at paths, and link between
%%% paths.
%%%
%%% There are three layers to HyperBEAMs internal data representation on-disk:
%%%
%%% 1. The raw binary data, written to the store at the hash of the content.
%%%    Storing binary paths in this way effectively deduplicates the data.
%%% 2. The hashpath-graph of all content, stored as a set of links between
%%%    hashpaths, their keys, and the data that underlies them. This allows
%%%    all messages to share the same hashpath space, such that all requests
%%%    from users additively fill-in the hashpath space, minimizing duplicated
%%%    compute.
%%% 3. Messages, referrable by their IDs (committed or uncommitted). These are
%%%    stored as a set of links commitment IDs and the uncommitted message.
%%%
%%% Before writing a message to the store, we convert it to Type-Annotated
%%% Binary Messages (TABMs), such that each of the keys in the message is
%%% either a map or a direct binary.
%%% 
%%% Nested keys are lazily loaded from the stores, such that large deeply
%%% nested messages where only a small part of the data is actually used are
%%% not loaded into memory unnecessarily. In order to ensure that a message is
%%% loaded from the cache after a `read', we can use the `ensure_loaded/1' and
%%% `ensure_all_loaded/1' functions. Ensure loaded will load the exact value
%%% that has been requested, while ensure all loaded will load the entire 
%%% structure of the message into memory.
%%% 
%%% Lazily loadable `links' are expressed as a tuple of the following form:
%%% `{link, ID, LinkOpts}', where `ID' is the path to the data in the store,
%%% and `LinkOpts' is a map of suggested options to use when loading the data.
%%% In particular, this module ensures to stash the `store' option in `LinkOpts',
%%% such that the `read' function can use the correct store without having to
%%% search unnecessarily. By providing an `Opts' argument to `ensure_loaded' or
%%% `ensure_all_loaded', the caller can specify additional options to use when
%%% loading the data -- overriding the suggested options in the link.
-module(hb_cache).
-export([read_all_commitments/2]).
-export([ensure_loaded/1, ensure_loaded/2, ensure_all_loaded/1, ensure_all_loaded/2]).
-export([read/2, read_resolved/3, write/2, write_binary/3, write_hashpath/2, link/3]).
-export([match/2, list/2, list_numbered/2]).
-export([test_unsigned/1, test_signed/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Ensure that a value is loaded from the cache if it is an ID or a link.
%% If it is not loadable we raise an error. If the value is a message, we will
%% load only the first `layer' of it: Representing all nested messages inside 
%% the result as links. If the value has an associated `type' key in the extra
%% options, we apply it to the read value, 'lazily' recreating a `structured@1.0'
%% form.
ensure_loaded(Msg) ->
    ensure_loaded(Msg, #{}).
ensure_loaded(Msg, Opts) ->
    ensure_loaded([], Msg, Opts).
ensure_loaded(Ref, {Status, Msg}, Opts) when Status == ok; Status == error ->
    {Status, ensure_loaded(Ref, Msg, Opts)};
ensure_loaded(Ref,
        Lk = {link, ID, LkOpts = #{ <<"type">> := <<"link">>, <<"lazy">> := Lazy }},
        RawOpts) ->
    % The link is to a submessage; either in lazy (unresolved) form, or direct
    % form.
    UnscopedOpts = hb_util:deep_merge(RawOpts, LkOpts, RawOpts),
    Opts = hb_store:scope(UnscopedOpts, hb_opts:get(scope, local, LkOpts)),
    Store = hb_opts:get(store, no_viable_store, Opts),
    ?event(debug_cache,
        {loading_multi_link,
            {link, ID},
            {link_opts, LkOpts},
            {store, Store}
        }
    ),
    CacheReadResult = 
        case hb_opts:get(commitment, undefined, Opts) of
            true ->
                do_read_commitment(ID, hb_util:deep_merge(Opts, LkOpts, Opts));
            _ ->
                hb_cache:read(ID, hb_util:deep_merge(Opts, LkOpts, Opts))
        end,
    case CacheReadResult of
        {ok, Next} ->
            ?event(debug_cache,
                {loaded,
                    {link, ID},
                    {store, Store}
                }),
            case Lazy of
                true ->
                    % We have resolved the ID of the submessage, so we continue
                    % to load the submessage itself.
                    ensure_loaded(
                        {link,
                            Next,
                            #{
                                <<"type">> => <<"link">>,
                                <<"lazy">> => false
                            }
                        },
                        Opts
                    );
                false ->
                    % The already had the ID of the submessage, so now we have
                    % the data, we simply return it.
                    Next
            end;
        not_found ->
            report_ensure_loaded_not_found(Ref, Lk, Opts)
    end;
ensure_loaded(Ref, Link = {link, ID, LinkOpts = #{ <<"lazy">> := true }}, RawOpts) ->
    % If the user provided their own options, we merge them and _overwrite_
    % the options that are already set in the link.
    UnscopedOpts = hb_util:deep_merge(RawOpts, LinkOpts, RawOpts),
    Opts = hb_store:scope(UnscopedOpts, hb_opts:get(scope, local, LinkOpts)),
    CacheReadResult = 
        case hb_opts:get(commitment, undefined, Opts) of
            true ->
                do_read_commitment(ID, Opts);
            _ ->
                read(ID, Opts)
        end,
    case CacheReadResult of
        {ok, LoadedMsg} ->
            ?event(caching,
                {lazy_loaded,
                    {link, ID},
                    {msg, LoadedMsg},
                    {link_opts, LinkOpts}
                }
            ),
            case hb_maps:get(<<"type">>, LinkOpts, undefined, Opts) of
                undefined -> LoadedMsg;
                Type -> dev_codec_structured:decode_value(Type, LoadedMsg)
            end;
        not_found ->
            report_ensure_loaded_not_found(Ref, Link, Opts)
    end;
ensure_loaded(Ref, {link, ID, LinkOpts}, Opts) ->
	ensure_loaded(Ref, {link, ID, LinkOpts#{ <<"lazy">> => true}}, Opts);
ensure_loaded(_Ref, Msg, _Opts) when not ?IS_LINK(Msg) ->
    Msg.

%% @doc Report that a value was not found in the cache. If a key is provided,
%% we report that the key was not found, otherwise we report that the link was
%% not found.
report_ensure_loaded_not_found(Ref, Lk, Opts) ->
    ?event(link_error, {link_not_resolvable, {ref, Ref}, {link, Lk}, {opts, Opts}}),
    throw(
        {necessary_message_not_found,
            hb_path:to_binary(lists:reverse(Ref)),
            hb_link:format_unresolved(Lk, Opts, 0)
        }
    ).

%% @doc Ensure that all of the components of a message (whether a map, list,
%% or immediate value) are recursively fully loaded from the stores into memory.
%% This is a catch-all function that is useful in situations where ensuring a
%% message contains no links is important, but it carries potentially extreme
%% performance costs.
ensure_all_loaded(Msg) ->
    ensure_all_loaded(Msg, #{}).
ensure_all_loaded(Msg, Opts) ->
    ensure_all_loaded([], Msg, Opts).
ensure_all_loaded(Ref, Link, Opts) when ?IS_LINK(Link) ->
    ensure_all_loaded(Ref, ensure_loaded(Ref, Link, Opts), Opts);
ensure_all_loaded(Ref, Msg, Opts) when is_map(Msg) ->
    maps:map(fun(K, V) -> ensure_all_loaded([K|Ref], V, Opts) end, Msg);
ensure_all_loaded(Ref, Msg, Opts) when is_list(Msg) ->
    lists:map(
        fun({N, V}) -> ensure_all_loaded([N|Ref], V, Opts) end,
        hb_util:number(Msg)
    );
ensure_all_loaded(Ref, Msg, Opts) ->
    ensure_loaded(Ref, Msg, Opts).

%% @doc List all items in a directory, assuming they are numbered.
list_numbered(Path, Opts) ->
    SlotDir = hb_store:path(hb_opts:get(store, no_viable_store, Opts), Path),
    [ hb_util:int(Name) || Name <- list(SlotDir, Opts) ].

%% @doc List all items under a given path.
list(Path, Opts) when is_map(Opts) and not is_map_key(<<"store-module">>, Opts) ->
    case hb_opts:get(store, no_viable_store, Opts) of
        not_found -> [];
        Store ->
            list(Path, Store)
    end;
list(Path, Store) ->
    ResolvedPath = hb_store:resolve(Store, Path),
    case hb_store:list(Store, ResolvedPath) of
        {ok, Names} -> Names;
        {error, _} -> [];
        not_found -> []
    end.

%% @doc Match a template message against the cache, returning a list of IDs
%% that match the template. We match on the binary representation of values,
%% rather than their types explicitly, such that 'AO-Types' keys that are
%% only partial matches do not cause the match to fail.
match(MatchSpec, Opts) ->
    Spec = hb_message:convert(MatchSpec, tabm, <<"structured@1.0">>, Opts),
    ConvertedMatchSpec =
        maps:map(
            fun(_, Value) ->
                generate_binary_path(Value, Opts)
            end,
            maps:without([<<"ao-types">>], hb_ao:normalize_keys(Spec, Opts))
        ),
    case hb_store:match(hb_opts:get(store, no_viable_store, Opts), ConvertedMatchSpec) of
        {ok, Matches} -> {ok, Matches};
        _ -> not_found
    end.

%% @doc Generate the path at which a binary value should be stored.
generate_binary_path(Bin, Opts) ->
    Hashpath = hb_path:hashpath(Bin, Opts),
    <<"data/", Hashpath/binary>>.

%% @doc Write a message to the cache. For raw binaries, we write the data at
%% the hashpath of the data (by default the SHA2-256 hash of the data). We link
%% the unattended ID's hashpath for the keys (including `/commitments') on the
%% message to the underlying data and recurse. We then link each commitment ID
%% to the uncommitted message, such that any of the committed or uncommitted IDs
%% can be read, and once in memory all of the commitments are available. For
%% deep messages, the commitments will also be read, such that the ID of the
%% outer message (which does not include its commitments) will be built upon
%% the commitments of the inner messages. We do not, however, store the IDs from
%% commitments on signed _inner_ messages. We may wish to revisit this.
write(RawMsg, Opts) when is_map(RawMsg) ->
    {ok, Msg} = hb_message:with_only_committed(RawMsg, Opts),
    TABM = hb_message:convert(Msg, tabm, <<"structured@1.0">>, Opts),
    ?event(debug_cache, {writing_full_message, {msg, TABM}}),
    try
        do_write_message(
            TABM,
            hb_opts:get(store, no_viable_store, Opts),
            Opts
        )
    catch
        Type:Reason:Stacktrace ->
            ?event(error,
                {cache_write_error,
                    {type, Type},
                    {reason, Reason},
                    {stacktrace, {trace, Stacktrace}}
                },
                Opts
            ),
            erlang:raise(Type, Reason, Stacktrace)
    end;
write(List, Opts) when is_list(List) ->
    write(hb_message:convert(List, tabm, <<"structured@1.0">>, Opts), Opts);
write(Bin, Opts) when is_binary(Bin) ->
    do_write_message(Bin, hb_opts:get(store, no_viable_store, Opts), Opts).

do_write_message(Bin, Store, Opts) when is_binary(Bin) ->
    % Write the binary in the store at its calculated content-hash.
    % Return the path.
    ok = hb_store:write(Store, Path = generate_binary_path(Bin, Opts), Bin),
    %lists:map(fun(ID) -> hb_store:make_link(Store, Path, ID) end, AllIDs),
    {ok, Path};
do_write_message(List, Store, Opts) when is_list(List) ->
    do_write_message(
        hb_message:convert(List, tabm, <<"structured@1.0">>, Opts),
        Store,
        Opts
    );
do_write_message(Msg, Store, Opts) when is_map(Msg) ->
    ?event(debug_cache, {writing_message, Msg}),
    % Calculate the IDs of the message.
    UncommittedID = hb_message:id(Msg, none, Opts#{ linkify_mode => discard }),
    AltIDs = calculate_all_ids(Msg, Opts) -- [UncommittedID],
    MsgHashpathAlg = hb_path:hashpath_alg(Msg, Opts),
    ?event(debug_cache, {writing_message, {id, UncommittedID}, {alt_ids, AltIDs}, {original, Msg}}),
    % Write all of the keys of the message into the store.
    hb_store:make_group(Store, UncommittedID),
    maps:map(
        fun(Key, Value) ->
            write_key(UncommittedID, Key, MsgHashpathAlg, Value, Store, Opts)
        end,
        maps:without([<<"priv">>], Msg)
    ),
    % Write the commitments to the store, linking each commitment ID to the
    % uncommitted message.
    lists:map(
        fun(AltID) ->
            ?event(debug_cache,
                {linking_commitment,
                    {uncommitted_id, UncommittedID},
                    {committed_id, AltID}
            }),
            hb_store:make_link(Store, UncommittedID, AltID)
        end,
        AltIDs
    ),
    {ok, UncommittedID}.

%% @doc Write a single key for a message into the store.
write_key(Base, <<"commitments">>, _HPAlg, RawCommitments, Store, Opts) ->
    % The commitments are a special case: We calculate the single-part hashpath
    % for the `baseID/commitments` key, then write each commitment to the store
    % and link it to `baseCommHP/commitmentID`.
    Commitments = prepare_commitments(RawCommitments, Opts),
    CommitmentsBase = commitment_path(Base, Opts),
    ok = hb_store:make_group(Store, CommitmentsBase),
    ?event(
        {writing_commitments,
            {base, Base},
            {commitments_message, Commitments},
            {commitments_base, CommitmentsBase}
        }
    ),
    maps:map(
        fun(BaseCommID, Commitment) ->
            ?event(debug_cache, {writing_commitment, {commitment, Commitment}}),
            {ok, CommMsgID} = do_write_message(Commitment, Store, Opts),
            hb_store:make_link(
                Store,
                CommMsgID,
                << CommitmentsBase/binary, "/", BaseCommID/binary >>
            )
        end,
        Commitments
    ),
    % Link the commitments base to `base/commitments`.
    hb_store:make_link(Store, CommitmentsBase, <<Base/binary, "/commitments">>);
write_key(Base, Key, HPAlg, Value, Store, Opts) ->
    KeyHashPath =
        hb_path:hashpath(
            Base,
            hb_path:to_binary(Key),
            HPAlg,
            Opts
        ),
    {ok, Path} = do_write_message(Value, Store, Opts),
    hb_store:make_link(Store, Path, KeyHashPath),
    {ok, Path}.

%% @doc The `structured@1.0` encoder does not typically encode `commitments`,
%% subsequently, when we encounter a commitments message we prepare its contents
%% separately, then write each to the store.
prepare_commitments(RawCommitments, Opts) ->
    Commitments = ensure_all_loaded(RawCommitments, Opts),
    maps:map(
        fun(_, StructuredCommitment) ->
            hb_message:convert(StructuredCommitment, tabm, Opts)
        end,
        Commitments
    ).

%% @doc Generate the commitment path for a given base path.
commitment_path(Base, Opts) ->
    hb_path:hashpath(<<Base/binary, "/commitments">>, Opts).

%% @doc Calculate the IDs for a message.
calculate_all_ids(Bin, _Opts) when is_binary(Bin) -> [];
calculate_all_ids(Msg, Opts) ->
    Commitments =
        hb_maps:without(
            [<<"priv">>],
            hb_maps:get(<<"commitments">>, Msg, #{}, Opts),
			Opts
        ),
    CommIDs = hb_maps:keys(Commitments, Opts),
    ?event({calculating_ids, {msg, Msg}, {commitments, Commitments}, {comm_ids, CommIDs}}),
    All = hb_message:id(Msg, all, Opts#{ linkify_mode => discard }),
    case lists:member(All, CommIDs) of
        true -> CommIDs;
        false -> [All | CommIDs]
    end.

%% @doc Write a hashpath and its message to the store and link it.
write_hashpath(Msg = #{ <<"priv">> := #{ <<"hashpath">> := HP } }, Opts) ->
    write_hashpath(HP, Msg, Opts);
write_hashpath(MsgWithoutHP, Opts) ->
    write(MsgWithoutHP, Opts).
write_hashpath(HP, Msg, Opts) when is_binary(HP) or is_list(HP) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    ?event({writing_hashpath, {hashpath, HP}, {msg, Msg}, {store, Store}}),
    {ok, Path} = write(Msg, Opts),
    hb_store:make_link(Store, Path, HP),
    {ok, Path}.

%% @doc Write a raw binary keys into the store and link it at a given hashpath.
write_binary(Hashpath, Bin, Opts) ->
    write_binary(Hashpath, Bin, hb_opts:get(store, no_viable_store, Opts), Opts).
write_binary(Hashpath, Bin, Store, Opts) ->
    ?event({writing_binary, {hashpath, Hashpath}, {bin, Bin}, {store, Store}}),
    {ok, Path} = do_write_message(Bin, Store, Opts),
    hb_store:make_link(Store, Path, Hashpath),
    {ok, Path}.

%% @doc Read the message at a path. Returns in `structured@1.0' format: Either a
%% richly typed map or a direct binary.
read(Path, Opts) ->
    StoreReadResult =
        store_read(Path, hb_opts:get(store, no_viable_store, Opts), Opts),
    case StoreReadResult of 
        {ok, Res} ->
            {ok, hb_message:normalize_commitments(Res, Opts)};
        _ -> StoreReadResult
    end.
do_read_commitment(Path, Opts) ->
    store_read(Path, hb_opts:get(store, no_viable_store, Opts), Opts).

%% @doc Load all of the commitments for a message into memory.
read_all_commitments(Msg, Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    UncommittedID = hb_message:id(Msg, none, Opts#{ linkify_mode => discard }),
    CurrentCommitments = hb_maps:get(<<"commitments">>, Msg, #{}, Opts),
    AlreadyLoaded = hb_maps:keys(CurrentCommitments, Opts),
    CommitmentsPath =
        hb_store:resolve(
            Store,
            hb_store:path(Store, [UncommittedID, <<"commitments">>])
        ),
    FoundCommitments =
        case hb_store:list(Store, CommitmentsPath) of
            {ok, CommitmentIDs} ->
                lists:filtermap(
                    fun(CommitmentID) ->
                        ShouldLoad = not lists:member(CommitmentID, AlreadyLoaded),
                        ResolvedCommPath =
                            hb_store:path(
                                Store,
                                [CommitmentsPath, CommitmentID]
                            ),
                        case ShouldLoad andalso do_read_commitment(ResolvedCommPath, Opts) of
                            {ok, Commitment} ->
                                {
                                    true,
                                    {
                                        CommitmentID,
                                        ensure_all_loaded(
                                            Commitment,
                                            Opts#{ commitment => true }
                                        )
                                    }
                                };
                            _ ->
                                false
                        end
                    end,
                    CommitmentIDs
                );
            not_found ->
                []
    end,
    NewCommitments =
        hb_maps:merge(
            CurrentCommitments,
            maps:from_list(FoundCommitments)
        ),
    Msg#{ <<"commitments">> => NewCommitments }.
%% @doc List all of the subpaths of a given path and return a map of keys and
%% links to the subpaths, including their types.
store_read(Path, Store, Opts) ->
    store_read(Path, Path, Store, Opts).
store_read(_Target, _Path, no_viable_store, _) ->
    not_found;
store_read(Target, Path, Store, Opts) ->
    ResolvedFullPath = hb_store:resolve(Store, PathBin = hb_path:to_binary(Path)),
    ?event({reading,
        {original_path, {string, PathBin}},
        {fully_resolved_path, ResolvedFullPath},
        {store, Store}
    }),
    case hb_store:type(Store, ResolvedFullPath) of
        not_found -> not_found;
        simple ->
            ?event({reading_data, ResolvedFullPath}),
            case hb_store:read(Store, ResolvedFullPath) of
                {ok, Bin} -> {ok, Bin};
                not_found -> not_found
            end;
        composite ->
            ?event({reading_composite, ResolvedFullPath}),
            case hb_store:list(Store, ResolvedFullPath) of
                {ok, RawSubpaths} ->
                    Subpaths =
                        lists:map(fun hb_util:bin/1, RawSubpaths),
                    ?event(
                        {listed,
                            {original_path, Path},
                            {subpaths, {explicit, Subpaths}}
                        }
                    ),
                    % Generate links for each of the listed keys. We only list
                    % the target ID given in the case of multiple known
                    % commitments.
                    Msg =
                        prepare_links(
                            Target,
                            ResolvedFullPath,
                            Subpaths,
                            Store,
                            Opts
                        ),
                    ?event(
                        {completed_read,
                            {resolved_path, ResolvedFullPath},
                            {explicit, Msg}
                        }
                    ),
                    {ok, Msg};
                _ ->
                    ?event({empty_composite_message, ResolvedFullPath}),
                    {ok, #{}}
            end
    end.

%% @doc Prepare a set of links from a listing of subpaths.
prepare_links(Target, RootPath, Subpaths, Store, Opts) ->
    {ok, Implicit, Types} = read_ao_types(RootPath, Subpaths, Store, Opts),
    Res =
        maps:from_list(lists:filtermap(
            fun(<<"ao-types">>) -> false;
                (<<"commitments">>) ->
                    % List the commitments for this message, and load them into
                    % memory. If there no commitments at the path, we exclude
                    % commitments from the list of links.
                    CommPath =
                        hb_store:resolve(
                            Store,
                            hb_store:path(
                                Store,
                                [
                                    RootPath,
                                    <<"commitments">>,
                                    Target
                                ]
                            )
                        ),
                    ?event(read_commitment,
                        {reading_commitment,
                            {target, Target},
                            {root_path, RootPath},
                            {commitments_path, CommPath}
                        }
                    ),
                    case do_read_commitment(CommPath, Opts) of
                        {ok, Commitment} ->
                            LoadedCommitment = 
                                ensure_all_loaded(
                                    Commitment,
                                    Opts#{ commitment => true }
                                ),
                            ?event(read_commitment,
                                {found_target_commitment,
                                    {path, CommPath},
                                    {commitment, LoadedCommitment}
                                }
                            ),
                            % We have commitments, so we read each commitment
                            % into memory, and return it as part of the message.
                            {
                                true,
                                {
                                    <<"commitments">>,
                                    #{ Target => LoadedCommitment }
                                }
                            };
                        _ ->
                            false
                    end;
                (Subpath) ->
                    ?event(
                        {returning_link,
                            {subpath, Subpath}
                        }
                    ),
                    SubkeyPath = hb_store:path(Store, [RootPath, Subpath]),
                    case hb_link:is_link_key(Subpath) of
                        false ->
                            % The key is a literal value, not a nested composite
                            % message. Subsequently, we return a resolvable link
                            % to the subpath, leaving the key as-is.
                            {true,
                                {
                                    Subpath,
                                    {link,
                                        SubkeyPath,
                                        (case Types of
                                            #{ Subpath := Type } ->
                                                % We have an `ao-types' entry for the
                                                % subpath, so we return a link to the
                                                % subpath with `lazy' set to `true'
                                                % because we need to resolve the link
                                                % to get the final value.
                                                #{
                                                    <<"type">> => Type,
                                                    <<"lazy">> => true
                                                };
                                            _ ->
                                                % We do not have an `ao-types' entry for the
                                                % subpath, so we return a link to the
                                                % subpath with `lazy' set to `true',
                                                % because the subpath is a literal
                                                % value.
                                                #{
                                                    <<"lazy">> => true
                                                }
                                        end)#{ store => Store }
                                    }
                                }
                            };
                        true ->
                            % The key is an encoded link, so we create a resolvable
                            % link to the underlying link. This requires that we
                            % dereference the link twice in order to get the final
                            % value. Returning the data this way avoids having to
                            % read each of the link keys themselves, which may be
                            % a large quantity.
                            {true,
                                {
                                    binary:part(Subpath, 0, byte_size(Subpath) - 5),
                                    {link, SubkeyPath, #{
                                        <<"type">> => <<"link">>,
                                        <<"lazy">> => true
                                    }}
                                }
                            }
                    end
                end,
            Subpaths
        )),
    Merged = maps:merge(Res, Implicit),
    % Convert the message to an ordered list if the ao-types indicate that it
    % should be so. If it is a message, we ensure that the commitments are 
    % normalized (have an unsigned comm. ID) and loaded into memory.
    case dev_codec_structured:is_list_from_ao_types(Types, Opts) of
        true ->
            hb_util:message_to_ordered_list(Merged, Opts);
        false ->
            case hb_opts:get(lazy_loading, true, Opts) of
                true -> Merged;
                false -> ensure_all_loaded(Merged, Opts)
            end
    end.

%% @doc Read and parse the ao-types for a given path if it is in the supplied
%% list of subpaths, returning a map of keys and their types.
read_ao_types(Path, Subpaths, Store, Opts) ->
    ?event({reading_ao_types, {path, Path}, {subpaths, {explicit, Subpaths}}}),
    case lists:member(<<"ao-types">>, Subpaths) of
        true ->
            {ok, TypesBin} =
                hb_store:read(
                    Store,
                    hb_store:path(Store, [Path, <<"ao-types">>])
                ),
            Types = dev_codec_structured:decode_ao_types(TypesBin, Opts),
            ?event({parsed_ao_types, {types, Types}}),
            {ok, types_to_implicit(Types), Types};
        false ->
            ?event({no_ao_types_key_found, {path, Path}, {subpaths, Subpaths}}),
            {ok, #{}, #{}}
    end.

%% @doc Convert a map of ao-types to an implicit map of types.
types_to_implicit(Types) ->
    maps:filtermap(
        fun(_K, <<"empty-message">>) -> {true, #{}};
           (_K, <<"empty-list">>) -> {true, []};
           (_K, <<"empty-binary">>) -> {true, <<>>};
           (_, _) -> false
        end,
        Types
    ).

%% @doc Read the result of a computation, using heuristics. The supported
%% heuristics are as follows:
%% 1. If the base message is an ID, we try to determine if the message has an
%% explicit device. If it does not, we can simply read the key and return it if
%% it exists, as this is the behavior of `message@1.0'.
%% 2. If the base message is loaded (a map), we determine if it has an explicit,
%% non-direct data access device. If it does, we simply read the key from the
%% message and return it if it exists.
%% 3. If the message has an explicit device, we attempt to read the hashpath to
%% see if it has already been computed.
read_resolved(BaseMsg, Key, Opts) when is_binary(Key) ->
    read_resolved(BaseMsg, #{ <<"path">> => Key }, Opts);
read_resolved({link, ID, LinkOpts}, Req, Opts) ->
    read_resolved(ID, Req, maps:merge(LinkOpts, Opts));
read_resolved(BaseMsgID, Req = #{ <<"path">> := Key }, Opts) when ?IS_ID(BaseMsgID) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    NormKey = hb_ao:normalize_key(Key, Opts),
    case hb_ao_device:is_direct_key_access(BaseMsgID, Req, Opts, Store) of
        unknown -> miss;
        false ->
            ?event(read_cached,
                {found_non_message_device,
                    {key, NormKey}
                }
            ),
            read_hashpath(BaseMsgID, Req, Opts);
        true ->
            % Either the message does not exist in the store, or there is no
            % explicit device in the message. If the message exists this implies
            % that the default (`message@1.0`) device will be used to execute
            % the key. Subsequently, we can simply read the key and return it if
            % it exists.
            ?event(read_cached,
                {skipping_execution_store_lookup,
                    {base_msg, BaseMsgID},
                    {key, NormKey}
                }
            ),
            KeyPath = hb_store:resolve(Store, [BaseMsgID, Key]),
            {hit, read(KeyPath, Opts)}
    end;
read_resolved(BaseMsg, Req = #{ <<"path">> := Key }, Opts) when is_map(BaseMsg) ->
    % The base message is loaded, so we determine if it has an explicit device
    % and perform a direct lookup if it does not.
    NormKey = hb_ao:normalize_key(Key, Opts),
    case hb_ao_device:is_direct_key_access(BaseMsg, Req, Opts) of
        false -> read_hashpath(BaseMsg, Req, Opts);
        true ->
            ?event(read_cached,
                {skip_execution_memory_lookup,
                    {path, NormKey}
                }
            ),
            {hit, read_in_memory_key(BaseMsg, NormKey, Opts)}
    end;
read_resolved(Base, Req, Opts) ->
    read_hashpath(Base, Req, Opts).

%% @doc Return a key from an in-memory message, returning the same form as
%% a store read (`{Status, Value}').
read_in_memory_key(BaseMsg, NormKey, _Opts) ->
    % For now, just wrap maps:find.
    case maps:find(NormKey, BaseMsg) of
        error ->
            ?event(read_cached, {key_not_found, {key, NormKey}}),
            not_found;
        {ok, Value} ->
            ?event(read_cached, {key_found, {key, NormKey}}),
            {ok, Value}
    end.

%% @doc Read the output of a prior computation, given BaseMsg and Req.
read_hashpath(BaseMsgID, ReqID, Opts) when ?IS_ID(BaseMsgID) and ?IS_ID(ReqID) ->
    ?event({cache_lookup, {base, BaseMsgID}, {req, ReqID}, {opts, Opts}}),
    case read(<<BaseMsgID/binary, "/", ReqID/binary>>, Opts) of
        {ok, Msg} -> {hit, {ok, Msg}};
        not_found -> miss
    end;
read_hashpath(BaseMsgID, Req, Opts) when ?IS_ID(BaseMsgID) and is_map(Req) ->
    {ok, ReqID} = dev_message:id(Req, #{ <<"committers">> => <<"all">> }, Opts),
    case read(<<BaseMsgID/binary, "/", ReqID/binary>>, Opts) of
        {ok, Msg} -> {hit, {ok, Msg}};
        not_found -> miss
    end;
read_hashpath(BaseMsg, Req, Opts) when is_map(BaseMsg) and is_map(Req) ->
    case read(hb_path:hashpath(BaseMsg, Req, Opts), Opts) of
        {ok, Msg} -> {hit, {ok, Msg}};
        not_found -> miss
    end;
read_hashpath(_, _, _) -> miss.

%% @doc Make a link from one path to another in the store.
%% Note: Argument order is `link(Src, Dst, Opts)'.
link(Existing, New, Opts) ->
    hb_store:make_link(
        hb_opts:get(store, no_viable_store, Opts),
        Existing,
        New
    ).

%%% Tests

test_unsigned(Data) ->
    #{
        <<"base-test-key">> => <<"base-test-value">>,
        <<"other-test-key">> => Data
    }.

%% Helper function to create signed #tx items.
test_signed(Data) -> test_signed(Data, ar_wallet:new()).
test_signed(Data, Wallet) ->
    hb_message:commit(test_unsigned(Data), Wallet).

test_store_binary(Store) ->
    Bin = <<"Simple unsigned data item">>,
    ?event(debug_store_test, {store, Store}),
    Opts = #{ store => Store },
    {ok, ID} = write(Bin, Opts),
    {ok, RetrievedBin} = read(ID, Opts),
    ?assertEqual(Bin, RetrievedBin).

test_store_unsigned_empty_message(Store) ->
    ?event(debug_store_test, {store, Store}),
    hb_store:reset(Store),
    Item = #{},
    Opts = #{ store => Store },
    {ok, Path} = write(Item, Opts),
    {ok, RetrievedItem} = read(Path, Opts),
    ?event(
        {retrieved_item,
            {path, {string, Path}},
            {expected, Item},
            {got, RetrievedItem}
        }
    ),
    MatchRes = hb_message:match(Item, RetrievedItem, strict, Opts),
    ?event({match_result, MatchRes}),
    ?assert(MatchRes).

test_store_unsigned_nested_empty_message(Store) ->
    ?event(debug_store_test, {store, Store}),
    hb_store:reset(Store),
    Item =
        #{ <<"layer1">> =>
            #{ <<"layer2">> =>
                #{ <<"layer3">> =>
                    #{ <<"a">> => <<"b">>}
                },
                <<"layer3b">> => #{ <<"c">> => <<"d">>},
                <<"layer3c">> => #{}
            }
        },
    Opts = #{ store => Store },
    {ok, Path} = write(Item, Opts),
    {ok, RetrievedItem} = read(Path, Opts),
    ?assert(hb_message:match(Item, RetrievedItem, strict, Opts)).

%% @doc Test storing and retrieving a simple unsigned item
test_store_simple_unsigned_message(Store) ->
    Item = test_unsigned(<<"Simple unsigned data item">>),
    ?event(debug_store_test, {store, Store}),
    Opts = #{ store => Store },
    %% Write the simple unsigned item
    {ok, _Path} = write(Item, Opts),
    %% Read the item back
    ID = hb_util:human_id(hb_ao:get(id, Item)),
    {ok, RetrievedItem} = read(ID, Opts),
    ?assert(hb_message:match(Item, RetrievedItem, strict, Opts)),
    ok.

test_store_ans104_message(Store) ->
    ?event(debug_store_test, {store, Store}),
    hb_store:reset(Store),
    Opts = #{ store => Store },
    Item = #{ <<"type">> => <<"ANS104">>, <<"content">> => <<"Hello, world!">> },
    Committed = hb_message:commit(Item, hb:wallet()),
    {ok, _Path} = write(Committed, Opts),
    CommittedID = hb_util:human_id(hb_message:id(Committed, all)),
    UncommittedID = hb_util:human_id(hb_message:id(Committed, none)),
    ?event({test_message_ids, {uncommitted, UncommittedID}, {committed, CommittedID}}),
    {ok, RetrievedItem} = read(CommittedID, Opts),
    {ok, RetrievedItemU} = read(UncommittedID, Opts),
    ?assert(hb_message:match(Committed, RetrievedItem, strict, Opts)),
    ?assert(hb_message:match(Committed, RetrievedItemU, strict, Opts)),
    ok.

%% @doc Test storing and retrieving a simple unsigned item
test_store_simple_signed_message(Store) ->
    ?event(debug_store_test, {store, Store}),
    Opts = #{ store => Store },
    hb_store:reset(Store),
    Wallet = ar_wallet:new(),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    Item = test_signed(<<"Simple signed data item">>, Wallet),
    ?event({writing_test_message, Item}),
    %% Write the simple unsigned item
    {ok, _Path} = write(Item, Opts),
    % %% Read the item back
    % {ok, UID} = dev_message:id(Item, #{ <<"committers">> => <<"none">> }, Opts),
    % {ok, RetrievedItemUnsig} = read(UID, Opts),
    % ?event({retreived_unsigned_message, {expected, Item}, {got, RetrievedItemUnsig}}),
    % MatchRes = hb_message:match(Item, RetrievedItemUnsig, strict, Opts),
    % ?event({match_result, MatchRes}),
    % ?assert(MatchRes),
    {ok, CommittedID} = dev_message:id(Item, #{ <<"committers">> => [Address] }, Opts),
    {ok, RetrievedItemSigned} = read(CommittedID, Opts),
    ?event({retrieved_signed_message, {expected, Item}, {got, RetrievedItemSigned}}),
    MatchResSigned = 
        hb_message:match(
            Item,
            hb_message:normalize_commitments(RetrievedItemSigned, Opts),
            strict,
            Opts
        ),
    ?event({match_result_signed, MatchResSigned}),
    ?assert(MatchResSigned),
    ok.

%% @doc Test deeply nested item storage and retrieval
test_deeply_nested_complex_message(Store) ->
    ?event(debug_store_test, {store, Store}),
    hb_store:reset(Store),
    Wallet = ar_wallet:new(),
    Opts = #{ store => Store, priv_wallet => Wallet },
    %% Create nested data
    Level3SignedSubmessage = test_signed([1,2,3], Opts#{priv_wallet => Wallet}),
    Outer =
        hb_message:commit(
            #{
                <<"level1">> =>
                    InnerSigned = hb_message:commit(
                        #{
                            <<"level2">> =>
                                #{
                                    <<"level3">> => Level3SignedSubmessage,
                                    <<"e">> => <<"f">>,
                                    <<"z">> => [1,2,3]
                                },
                            <<"c">> => <<"d">>,
                            <<"g">> => [<<"h">>, <<"i">>],
                            <<"j">> => 1337
                        },
                        Opts
                    ),
                <<"a">> => <<"b">>
            },
            Opts
        ),
    UID = hb_message:id(Outer, none, Opts),
    ?event({string, <<"================================================">>}),
    CommittedID = hb_message:id(Outer, signed, Opts),
    ?event({string, <<"================================================">>}),
    ?event({test_message_ids, {uncommitted, UID}, {committed, CommittedID}}),
    %% Write the nested item
    {ok, _} = write(Outer, Opts),
    %% Read the deep value back using subpath
	OuterID = hb_util:human_id(UID),
    {ok, OuterMsg} = read(OuterID, Opts),
	EnsuredLoadedOuter = hb_cache:ensure_all_loaded(OuterMsg, Opts),
    ?event({deep_message, {explicit, EnsuredLoadedOuter}}),
    %% Assert that the retrieved item matches the original deep value
    ?assertEqual(
        [1,2,3],
        hb_ao:get(
            <<"level1/level2/level3/other-test-key">>,
            EnsuredLoadedOuter,
            Opts
        )
    ),
    ?event(
        {deep_message_match,
            {read, EnsuredLoadedOuter},
            {write, Level3SignedSubmessage}
        }
    ),
    ?event({reading_committed_outer, {id, CommittedID}, {expect, Outer}}),
    {ok, CommittedMsg} = read(hb_util:human_id(CommittedID), Opts),
	EnsuredLoadedCommitted = hb_cache:ensure_all_loaded(CommittedMsg, Opts),
	?assertEqual(
        [1,2,3],
        hb_ao:get(
            <<"level1/level2/level3/other-test-key">>,
            EnsuredLoadedCommitted,
            Opts
        )
    ).

test_message_with_list(Store) ->
    hb_store:reset(Store),
    Opts = #{ store => Store },
    Msg = test_unsigned([<<"a">>, <<"b">>, <<"c">>]),
    ?event({writing_message, Msg}),
    {ok, Path} = write(Msg, Opts),
    {ok, RetrievedItem} = read(Path, Opts),
    ?assert(hb_message:match(Msg, RetrievedItem, strict, Opts)).

test_match_message(Store) when map_get(<<"store-module">>, Store) =/= hb_store_lmdb ->
    skip;
test_match_message(Store) ->
    hb_store:reset(Store),
    Opts = #{ store => Store },
    % Write two messages that match the template, and a third that does not.
    {ok, ID1} = hb_cache:write(#{ <<"x">> => <<"1">> }, Opts),
    {ok, ID2} = hb_cache:write(#{ <<"y">> => <<"2">>, <<"z">> => <<"3">> }, Opts),
    {ok, ID2b} = hb_cache:write(#{ <<"x">> => <<"4">>, <<"z">> => <<"3">> }, Opts),
    {ok, ID3} = hb_cache:write(#{ <<"z">> => <<"5">>, <<"c">> => <<"d">> }, Opts),
    % Match the template, and ensure that we get two matches.
    {ok, MatchedItems} = match(#{ <<"z">> => <<"3">> }, Opts),
    ?assertEqual(2, length(MatchedItems)),
    ?assert(
        lists:all(
            fun(ID) ->
                {ok, Msg} = read(ID, Opts),
                hb_maps:get(<<"z">>, Msg, Opts) =:= <<"3">> andalso
                    lists:member(ID, [ID2, ID2b])
            end,
            MatchedItems
        )
    ),
    {ok, MatchedItems2} = match(#{ <<"x">> => <<"4">> }, Opts),
    ?assertEqual(1, length(MatchedItems2)),
    ?assertEqual([ID2b], MatchedItems2).

test_match_linked_message(Store) when map_get(<<"store-module">>, Store) =/= hb_store_lmdb ->
    skip;
test_match_linked_message(Store) ->
    hb_store:reset(Store),
    Opts = #{ store => Store },
    Msg = #{ <<"a">> => Inner = #{ <<"b">> => <<"c">>, <<"d">> => <<"e">> } },
    {ok, _ID} = write(Msg, Opts),
    {ok, [MatchedID]} = match(#{ <<"b">> => <<"c">> }, Opts),
    {ok, Read1} = read(MatchedID, Opts),
    ?assertEqual(
        hb_message:normalize_commitments(
            #{ <<"b">> => <<"c">>, <<"d">> => <<"e">> },
            Opts
        ),
        hb_cache:ensure_all_loaded(Read1, Opts)
    ),
    {ok, [MatchedID2]} = match(#{ <<"a">> => Inner }, Opts),
    {ok, Read2} = read(MatchedID2, Opts),
    ?assertEqual(
        hb_message:normalize_commitments(
            #{ <<"a">> => Inner },
            Opts
        ),
        ensure_all_loaded(Read2, Opts)
    ).

test_match_typed_message(Store) when map_get(<<"store-module">>, Store) =/= hb_store_lmdb ->
    skip;
test_match_typed_message(Store) ->
    hb_store:reset(Store),
    Opts = #{ store => Store },
    % Add some messages that should not match the template, as well as the main
    % message that should match the template.
    write(#{ <<"atom-value">> => atom, <<"wrong">> => <<"wrong">> }, Opts),
    write(#{ <<"integer-value">> => 1337, <<"wrong">> => <<"wrong-2">> }, Opts),
    Msg =
        #{
            <<"int-key">> => 1337,
            <<"other-key">> => <<"other-test-value">>,
            <<"atom-key">> => atom
        },
    {ok, _ID} = write(Msg, Opts),
    {ok, [MatchedID]} = match(#{ <<"int-key">> => 1337 }, Opts),
    {ok, Read1} = read(MatchedID, Opts),
    ?assertEqual(
        hb_message:normalize_commitments(Msg, Opts),
        ensure_all_loaded(Read1, Opts)
    ),
    {ok, [MatchedID2]} = match(#{ <<"atom-key">> => atom }, Opts),
    {ok, Read2} = read(MatchedID2, Opts),
    ?assertEqual(
        hb_message:normalize_commitments(Msg, Opts),
        ensure_all_loaded(Read2, Opts)
    ).

cache_suite_test_() ->
    hb_store:generate_test_suite([
        {"store unsigned empty message",
            fun test_store_unsigned_empty_message/1},
        {"store binary", fun test_store_binary/1},
        {"store unsigned nested empty message",
            fun test_store_unsigned_nested_empty_message/1},
        {"store simple unsigned message", fun test_store_simple_unsigned_message/1},
        {"store simple signed message", fun test_store_simple_signed_message/1},
        {"deeply nested complex message", fun test_deeply_nested_complex_message/1},
        {"message with list", fun test_message_with_list/1},
        {"match message", fun test_match_message/1},
        {"match linked message", fun test_match_linked_message/1},
        {"match typed message", fun test_match_typed_message/1}
    ]).

%% @doc Test that message whose device is `#{}' cannot be written. If it were to
%% be written, it would cause an infinite loop.
test_device_map_cannot_be_written_test() ->
    try
        Opts = #{ store => StoreOpts =
            [#{ <<"store-module">> => hb_store_fs, <<"name">> => <<"cache-TEST">> }] },
        hb_store:reset(StoreOpts),
        Danger = #{ <<"device">> => #{}},
        write(Danger, Opts),
        ?assert(false)
    catch
        _:_:_ -> ?assert(true)
    end.

%% @doc Run a specific test with a given store module.
run_test() ->
    Store = hb_test_utils:test_store(hb_store_lmdb),
    test_match_typed_message(Store).
