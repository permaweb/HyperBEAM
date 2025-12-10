%% @doc Store standard library
%%
%% Common patterns to access Stores (File System, LMDB, etc).

-module(hb_store_common).
-export([store_read/3]).
-export([resolved_list/2, resolved_type/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Unify resolve and list functions into one call.
resolved_list(Stores, Path) when is_list(Stores) ->
    do_resolved_list(Stores, Path);
resolved_list(Store, Path) -> 
    do_resolved_list([Store], Path).

do_resolved_list([], _Path) ->
    not_found;
do_resolved_list([Store|RemainingStores], Path) ->
    ResolvedPath = hb_store:resolve(Store, Path),
    ?event({resolved_list, {path, Path}, {resolved_path, ResolvedPath}}),
    case hb_store:list(Store, ResolvedPath) of 
        {ok, _} = Result -> Result;
        not_found -> do_resolved_list(RemainingStores, Path)
    end.

%% @doc Unify resolve and type functions into one call.
resolved_type(Stores, Path) when is_list(Stores) ->
    do_resolved_type(Stores, Path);
resolved_type(Store, Path) -> 
    do_resolved_type([Store], Path).

do_resolved_type([], _Path) -> not_found;
do_resolved_type([Store|RemainingStores], Path) -> 
    ResolvedPath = hb_store:resolve(Store, Path),
    ?event({resolved_type, {path, Path}, {resolved_path, ResolvedPath}}),
    case hb_store:type(Store, ResolvedPath) of 
        Result when Result =/= not_found -> Result;
        _ -> do_resolved_type(RemainingStores, Path)
    end.

%% @doc List all of the subpaths of a given path and return a map of keys and
%% links to the subpaths, including their types.
store_read(Path, Store, Opts) ->
    store_read(Path, Path, Store, Opts).
store_read(_Target, _Path, no_viable_store, _) ->
    not_found;
store_read(_Target, _Path, [], _) ->
    not_found;
store_read(Target, Path, Store, Opts) when is_map(Store) ->
    store_read(Target, Path, [Store], Opts);
store_read(Target, Path, [Store | RemainingStores], Opts) ->
    ResolvedFullPath = hb_store:resolve(Store, PathBin = hb_path:to_binary(Path)),
    ?event({reading,
        {original_path, {string, PathBin}},
        {fully_resolved_path, ResolvedFullPath},
        {store, Store}
    }),
    ResolvedFullPathContent = case hb_store:type(Store, ResolvedFullPath) of
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
                not_found ->
                    ?event({empty_composite_message, ResolvedFullPath}),
                    {ok, #{}}
            end
    end,
    case ResolvedFullPathContent of
        {ok, _} = Response -> Response;
        not_found -> store_read(Target, Path, RemainingStores, Opts)
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
                    %% TODO: Maybe improve this? This line bellow is called in 
                    %% hb_cache:do_read_commitment
                    case store_read(CommPath, hb_opts:get(store, no_viable_store, Opts), Opts) of
                        {ok, Commitment} ->
                            LoadedCommitment = 
                                hb_cache:ensure_all_loaded(
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
                false -> hb_cache:ensure_all_loaded(Merged, Opts)
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

%% Tests

%% @doc Read value from Store1 and Store2 when is only available in Store2
multiple_stores_store_read_test() ->
    [_Store1, Store2] = Stores = get_multiple_stores(),
    %% Write test data
    hb_store:make_group(Store2, <<"group1">>),
    hb_store:write(Store2, <<"data/final_id">>, <<"data">>),
    hb_store:make_link(Store2, <<"data/final_id">>, <<"group1/data">>),
    hb_store:make_link(Store2, <<"group1">>, <<"random_id">>),
    %% Check result
    Opts = #{},
    Path = <<"random_id">>,
    Content = store_read(Path, Stores, Opts),
    try 
        ?assertMatch({ok, #{<<"data">> := _}}, Content)
    after
        shutdown_stores(Stores)
    end.

%% @doc Test that resolve and type must be made in the same store, 
%% when multiple stores are provided.
resolved_type_test() -> 
    [_Store1, Store2] = Stores = get_multiple_stores(),
    %% Write test data
    hb_store:make_group(Store2, <<"group1">>),
    hb_store:write(Store2, <<"data/final_id">>, <<"data">>),
    hb_store:make_link(Store2, <<"data/final_id">>, <<"group1/data">>),
    hb_store:make_link(Store2, <<"group1">>, <<"random_id">>),
    %% Check result
    RawPath = <<"random_id/data">>,
    Result = resolved_type(Stores, RawPath),
    try
        ?assertEqual(simple, Result)
    after
        shutdown_stores(Stores)
    end.

%% @doc Test that resolve and list must be made in the same store, 
%% when multiple stores are provided.
resolved_list_test() -> 
    [_Store1, Store2] = Stores = get_multiple_stores(),
    %% Write test data
    hb_store:make_group(Store2, <<"group1">>),
    hb_store:make_group(Store2, <<"group1/group12">>),
    hb_store:write(Store2, <<"data/final_id2">>, <<"7890">>),
    %% Link 
    %% TODO: Not sure if this structure is possible in HB
    hb_store:make_link(Store2, <<"data/final_id2">>, <<"group1/group12/data">>),
    hb_store:make_link(Store2, <<"group1">>, <<"random_id">>),
    %% Check result
    RawPath = <<"random_id/group12">>,
    Result = resolved_list(Stores, RawPath),
    try
        ?assertEqual({ok, [<<"data">>]}, Result)
    after
        shutdown_stores(Stores)
    end.

%% Test utilities

%% @doc Initialize multiple stores
get_multiple_stores() -> 
    get_multiple_stores(hb_store_lmdb).
get_multiple_stores(StoreModule) -> 
    Store1 = hb_test_utils:test_store(StoreModule, <<"store1">>),
    Store2 = hb_test_utils:test_store(StoreModule, <<"store2">>),
    [Store1, Store2].

%% @doc Shutdown multiple stores
shutdown_stores([]) -> ok;
shutdown_stores([Store | RemainingStores]) -> 
    hb_store:reset(Store),
    hb_store:stop(Store),
    shutdown_stores(RemainingStores).
