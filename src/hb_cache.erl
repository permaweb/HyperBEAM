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
-export([ensure_loaded/1, ensure_loaded/2, ensure_all_loaded/1, ensure_all_loaded/2]).
-export([read/2, read_resolved/3, write/2, write_binary/3, write_hashpath/2, link/3]).
-export([list/2, list_numbered/2]).
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
ensure_loaded(Lk = {link, ID, LkOpts = #{ <<"type">> := <<"link">>, <<"lazy">> := Lazy }}, RawOpts) ->
    % The link is to a submessage; either in lazy (unresolved) form, or direct
    % form.
    Opts = hb_store:scope(RawOpts, local),
    Store = hb_opts:get(store, no_viable_store, Opts),
    ?event(debug_cache,
        {loading_multi_link,
            {link, ID},
            {link_opts, LkOpts},
            {store, Store}
        }
    ),
    case hb_cache:read(ID, hb_util:deep_merge(Opts, LkOpts, Opts)) of
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
            ?event(debug_cache, {lazy_link_not_found, {link, ID}, {link_opts, LkOpts}}),
            throw({necessary_message_not_found, Lk})
    end;
ensure_loaded(Link = {link, ID, LinkOpts = #{ <<"lazy">> := true }}, RawOpts) ->
    % If the user provided their own options, we merge them and _overwrite_
    % the options that are already set in the link.
    Opts = hb_store:scope(RawOpts, local),
    MergedOpts = hb_util:deep_merge(Opts, LinkOpts, Opts),
    case hb_cache:read(ID, MergedOpts) of
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
            throw({necessary_message_not_found_b, Link})
    end;
ensure_loaded({link, ID, LinkOpts}, Opts) ->
	ensure_loaded({link, ID, LinkOpts#{ <<"lazy">> => true}}, Opts);
ensure_loaded(Msg, _Opts) when not ?IS_LINK(Msg) ->
    Msg.

%% @doc Ensure that all of the components of a message (whether a map, list,
%% or immediate value) are recursively fully loaded from the stores into memory.
%% This is a catch-all function that is useful in situations where ensuring a
%% message contains no links is important, but it carries potentially extreme
%% performance costs.
ensure_all_loaded(Msg) ->
    ensure_all_loaded(Msg, #{}).
ensure_all_loaded(Link, Opts) when ?IS_LINK(Link) ->
    ensure_all_loaded(ensure_loaded(Link, Opts), Opts);
ensure_all_loaded(Msg, Opts) when is_map(Msg) ->
    hb_maps:map(fun(_K, V) -> ensure_all_loaded(V, Opts) end, Msg, Opts);
ensure_all_loaded(Msg, Opts) when is_list(Msg) ->
    lists:map(fun(V) -> ensure_all_loaded(V, Opts) end, Msg);
ensure_all_loaded(Msg, Opts) ->
    ensure_loaded(Msg, Opts).

%% @doc List all items in a directory, assuming they are numbered.
list_numbered(Path, Opts) ->
    SlotDir = hb_store:path(hb_opts:get(store, no_viable_store, Opts), Path),
    [ to_integer(Name) || Name <- list(SlotDir, Opts) ].

%% @doc List all items under a given path.
list(Path, Opts) when is_map(Opts) and not is_map_key(<<"store-module">>, Opts) ->
    case hb_opts:get(store, no_viable_store, Opts) of
        no_viable_store -> [];
        Store ->
            list(Path, Store)
    end;
list(Path, Store) ->
    ResolvedPath = hb_store:resolve(Store, Path),
    case hb_store:list(Store, ResolvedPath) of
        {ok, Names} -> Names;
        {error, _} -> [];
        no_viable_store -> []
    end.

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
    TABM = hb_message:convert(RawMsg, tabm, <<"structured@1.0">>, Opts),
    case hb_message:with_only_committed(TABM, Opts) of
        {ok, Msg} ->
            ?event(debug_cache, {writing_full_message, {msg, Msg}}),
            %try
                do_write_message(
                    TABM,
                    hb_opts:get(store, no_viable_store, Opts),
                    Opts
                );
            % catch
            %     Type:Reason:Stacktrace ->
            %         ?event(error,
            %             {cache_write_error,
            %                 {type, Type},
            %                 {reason, Reason},
            %                 {stacktrace, Stacktrace}
            %             },
            %             Opts
            %         ),
            %         {error, no_viable_store}
            % end;
        {error, Err} ->
            {error, Err}
    end;
write(List, Opts) when is_list(List) ->
    write(hb_message:convert(List, tabm, <<"structured@1.0">>, Opts), Opts);
write(Bin, Opts) when is_binary(Bin) ->
    do_write_message(Bin, hb_opts:get(store, no_viable_store, Opts), Opts).

do_write_message(Bin, Store, Opts) when is_binary(Bin) ->
    % Write the binary in the store at its calculated content-hash.
    % Return the path.
    Hashpath = hb_path:hashpath(Bin, Opts),
    ok = hb_store:write(Store, Path = <<"data/", Hashpath/binary>>, Bin),
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
write_key(Base, <<"commitments">>, HPAlg, RawCommitments, Store, Opts) ->
    % Search to see if we already have commitments for this message locally.
    Commitments = prepare_commitments(RawCommitments, Opts),
    LocalStore = hb_store:scope(Store, local),
    case read(<<Base/binary, "/commitments">>, Opts#{ store => LocalStore }) of
        {ok, ExistingCommitments} ->
            % We do, so we need to merge the new commitments with the old ones.
            % We do this by fully loading the existing commitments, converting
            % them to TABM, and merging the maps.
            LoadedExisting = prepare_commitments(ExistingCommitments, Opts),
            Merged = hb_maps:merge(Commitments, LoadedExisting),
            % Write the merged commitments to the store.
            {ok, Path} = do_write_message(Merged, Store, Opts),
            % Link the merged commitments to the message.
            hb_store:make_link(Store, Path, <<Base/binary, "/commitments">>),
            {ok, Path};
        _ ->
            % We do not have any commitments for this message locally, so we
            % write the commitments to the store.
            do_write_key(Base, <<"commitments">>, HPAlg, Commitments, Store, Opts)
    end;
write_key(Base, Key, HPAlg, Value, Store, Opts) ->
    do_write_key(Base, Key, HPAlg, Value, Store, Opts).

do_write_key(Base, Key, HPAlg, Value, Store, Opts) ->
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
    case store_read(Path, hb_opts:get(store, no_viable_store, Opts), Opts) of
        not_found -> not_found;
        {ok, Res} ->
            %?event({applying_types_to_read_message, Res}),
            %Structured = dev_codec_structured:to(Res),
            %?event({finished_read, Structured}),
            {ok, Res}
    end.

%% @doc List all of the subpaths of a given path and return a map of keys and
%% links to the subpaths, including their types.
store_read(_Path, no_viable_store, _) ->
    not_found;
store_read(Path, Store, Opts) ->
    ResolvedFullPath = hb_store:resolve(Store, PathBin = hb_path:to_binary(Path)),
    ?event({reading, {path, PathBin}, {resolved, ResolvedFullPath}, {store, Store}}),
    case hb_store:type(Store, ResolvedFullPath) of
        not_found -> not_found;
        no_viable_store -> not_found;
        simple ->
            ?event({reading_data, ResolvedFullPath}),
            case hb_store:read(Store, ResolvedFullPath) of
                {ok, Bin} -> {ok, Bin};
                {error, _} -> not_found
            end;
        _ ->
            ?event({reading_composite, ResolvedFullPath}),
            case hb_store:list(Store, ResolvedFullPath) of
                {ok, RawSubpaths} ->
                    Subpaths = lists:map(fun hb_util:bin/1, RawSubpaths),
                    ?event(
                        {listed,
                            {original_path, Path},
                            {subpaths, {explicit, Subpaths}}
                        }
                    ),
                    % Generate links for all subpaths except `commitments' and
                    % `ao-types'. `commitments' is always read in its entirety,
                    % such that all messages have their IDs and signatures
                    % locally available.
                    Msg = prepare_links(Path, Subpaths, Store, Opts),
                    ?event({completed_read, {explicit, Msg}}),
                    {ok, Msg};
                _ ->
                    ?event({composite_message_not_found, ResolvedFullPath}),
                    not_found
            end
    end.

%% @doc Prepare a set of links from a listing of subpaths.
prepare_links(RootPath, Subpaths, Store, Opts) ->
    {ok, Implicit, Types} = read_ao_types(RootPath, Subpaths, Store, Opts),
    Res =
        maps:from_list(lists:filtermap(
            fun(<<"ao-types">>) -> false;
                (<<"commitments">>) ->
                    % Ensure that the full commitments map is recursively
                    % loaded into memory.
                    {true,
                        {
                            <<"commitments">>,
                            hb_cache:ensure_all_loaded(
                                {link, 
                                    hb_store:path(
                                        Store,
                                        [
                                            RootPath,
                                            <<"commitments">>
                                        ]
                                    ),
                                    #{
                                        <<"lazy">> => true
                                    }
                                },
                                Opts
                            )
                        }
                    };
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
    % should be so.
    case dev_codec_structured:is_list_from_ao_types(Types, Opts) of
        true ->
            hb_util:message_to_ordered_list(Merged, Opts);
        false ->
            Merged
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

%% @doc Read the output of a prior computation, given Msg1, Msg2, and some
%% options.
read_resolved(MsgID1, MsgID2, Opts) when ?IS_ID(MsgID1) and ?IS_ID(MsgID2) ->
    ?event({cache_lookup, {msg1, MsgID1}, {msg2, MsgID2}, {opts, Opts}}),
    read(<<MsgID1/binary, "/", MsgID2/binary>>, Opts);
read_resolved(MsgID1, Msg2, Opts) when ?IS_ID(MsgID1) and is_map(Msg2) ->
    {ok, MsgID2} = dev_message:id(Msg2, #{ <<"committers">> => <<"all">> }, Opts),
    read(<<MsgID1/binary, "/", MsgID2/binary>>, Opts);
read_resolved(Msg1, Msg2, Opts) when is_map(Msg1) and is_map(Msg2) ->
    read(hb_path:hashpath(Msg1, Msg2, Opts), Opts);
read_resolved(_, _, _) -> not_found.

%% @doc Make a link from one path to another in the store.
%% Note: Argument order is `link(Src, Dst, Opts)'.
link(Existing, New, Opts) ->
    hb_store:make_link(
        hb_opts:get(store, no_viable_store, Opts),
        Existing,
        New
    ).

to_integer(Value) when is_list(Value) ->
    list_to_integer(Value);
to_integer(Value) when is_binary(Value) ->
    binary_to_integer(Value).

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

test_store_binary(Opts) ->
    Bin = <<"Simple unsigned data item">>,
    {ok, ID} = write(Bin, Opts),
    {ok, RetrievedBin} = read(ID, Opts),
    ?assertEqual(Bin, RetrievedBin).

test_store_unsigned_empty_message(Opts) ->
	Store = hb_opts:get(store, no_viable_store, Opts),
    hb_store:reset(Store),
    Item = #{},
    {ok, Path} = write(Item, Opts),
    {ok, RetrievedItem} = read(Path, Opts),
    ?event({retrieved_item, {path, {string, Path}}, {item, RetrievedItem}}),
    ?assert(hb_message:match(Item, RetrievedItem)).

test_store_unsigned_nested_empty_message(Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
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
    {ok, Path} = write(Item, Opts),
    {ok, RetrievedItem} = read(Path, Opts),
    ?assert(hb_message:match(Item, RetrievedItem, strict, Opts)).

%% @doc Test storing and retrieving a simple unsigned item
test_store_simple_unsigned_message(Opts) ->
    Item = test_unsigned(<<"Simple unsigned data item">>),
    %% Write the simple unsigned item
    {ok, _Path} = write(Item, Opts),
    %% Read the item back
    ID = hb_util:human_id(hb_ao:get(id, Item)),
    {ok, RetrievedItem} = read(ID, Opts),
    ?assert(hb_message:match(Item, RetrievedItem, strict, Opts)),
    ok.

test_store_ans104_message(Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    hb_store:reset(Store),
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
test_store_simple_signed_message(Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    hb_store:reset(Store),
    Wallet = ar_wallet:new(),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    Item = test_signed(<<"Simple signed data item">>, Wallet),
    ?event({writing_message, Item}),
    %% Write the simple unsigned item
    {ok, _Path} = write(Item, Opts),
    %% Read the item back
    {ok, UID} = dev_message:id(Item, #{ <<"committers">> => <<"none">> }, Opts),
    {ok, RetrievedItemU} = read(UID, Opts),
    ?event({retreived_unsigned_message, {expected, Item}, {got, RetrievedItemU}}),
    ?assert(hb_message:match(Item, RetrievedItemU, strict, Opts)),
    {ok, CommittedID} = dev_message:id(Item, #{ <<"committers">> => [Address] }, Opts),
    {ok, RetrievedItemS} = read(CommittedID, Opts),
    ?assert(hb_message:match(Item, RetrievedItemS, strict, Opts)),
    ok.

%% @doc Test deeply nested item storage and retrieval
test_deeply_nested_complex_message(Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    hb_store:reset(Store),
    Wallet = ar_wallet:new(),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
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
                        Opts#{ priv_wallet => Wallet }
                    ),
                <<"a">> => <<"b">>
            },
            Opts#{ priv_wallet => Wallet }
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

test_message_with_list(Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    hb_store:reset(Store),
    Msg = test_unsigned([<<"a">>, <<"b">>, <<"c">>]),
    ?event({writing_message, Msg}),
    {ok, Path} = write(Msg, Opts),
    {ok, RetrievedItem} = read(Path, Opts),
    ?assert(hb_message:match(Msg, RetrievedItem, strict, Opts)).

cache_suite_test_() ->
    hb_store:generate_test_suite([
        {"store unsigned empty message", fun test_store_unsigned_empty_message/1},
        {"store unsigned nested empty message", fun test_store_unsigned_nested_empty_message/1},
        {"store binary", fun test_store_binary/1},
        {"store simple unsigned message", fun test_store_simple_unsigned_message/1},
        {"store simple signed message", fun test_store_simple_signed_message/1},
        {"deeply nested complex message", fun test_deeply_nested_complex_message/1},
        {"message with list", fun test_message_with_list/1}
    ]).

%% @doc Test that message whose device is `#{}' cannot be written. If it were to
%% be written, it would cause an infinite loop.
test_device_map_cannot_be_written_test() ->
    try
        Opts = #{ store => StoreOpts =
            [#{ <<"store-module">> => hb_store_fs, <<"prefix">> => <<"cache-TEST">> }] },
        hb_store:reset(StoreOpts),
        Danger = #{ <<"device">> => #{}},
        write(Danger, Opts),
        ?assert(false)
    catch
        _:_:_ -> ?assert(true)
    end.

run_test() ->
    Opts =
        #{
            store =>
                [
                    #{
                        <<"store-module">> => hb_store_fs,
                        <<"prefix">> => <<"cache-TEST">>
                    }
                ]
        },
    test_deeply_nested_complex_message(Opts).