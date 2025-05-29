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
-module(hb_cache).
-export([read/2, read_resolved/3, write/2, write_binary/3, write_hashpath/2, link/3]).
-export([list/2, list_numbered/2]).
-export([test_unsigned/1, test_signed/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

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
    % Use the _structured_ format for calculating alternative IDs, but the
    % _tabm_ format for writing to the store.
    case hb_message:with_only_committed(RawMsg, Opts) of
        {ok, Msg} ->
            AllIDs = calculate_all_ids(RawMsg, Opts),
            ?event({writing_full_message, {all_ids, AllIDs}, {msg, Msg}}),
            Tabm = hb_message:convert(Msg, tabm, <<"structured@1.0">>, Opts),
            ?event({tabm, Tabm}),
            try do_write_message(
                Tabm,
                AllIDs,
                hb_opts:get(store, no_viable_store, Opts),
                Opts
            )
            catch
                Type:Reason:Stacktrace ->
                    ?event(error,
                        {cache_write_error,
                            {type, Type},
                            {reason, Reason},
                            {stacktrace, Stacktrace}
                        },
                        Opts
                    ),
                    {error, no_viable_store}
            end;
        {error, Err} ->
            {error, Err}
    end;
write(Bin, Opts) when is_binary(Bin) ->
    % When asked to write only a binary, we do not calculate any alternative IDs.
    do_write_message(Bin, [], hb_opts:get(store, no_viable_store, Opts), Opts).

do_write_message(Bin, AllIDs, Store, Opts) when is_binary(Bin) ->
    % Write the binary in the store at its given hash. Return the path.
    Hashpath = hb_path:hashpath(Bin, Opts),
    ok = hb_store:write(Store, Path = <<"data/", Hashpath/binary>>, Bin),
    lists:map(fun(ID) -> hb_store:make_link(Store, Path, ID) end, AllIDs),
    {ok, Path};
do_write_message(Msg, AllIDs, Store, Opts) when is_map(Msg) ->
    % Get the ID of the unsigned message.
    {ok, UncommittedID} =
        dev_message:id(Msg, #{ <<"committers">> => <<"none">> }, Opts),
    AltIDs = AllIDs -- [UncommittedID],
    ?event({writing_message_with_unsigned_id, UncommittedID, {alt_ids, AltIDs}}),
    MsgHashpathAlg = hb_path:hashpath_alg(Msg),
    hb_store:make_group(Store, UncommittedID),
    % Write the keys of the message into the store, rolling the keys into
    % hashpaths (having only two parts) as we do so.
    % We start by writing the group, such that if the message is empty, we
    % still have a group in the store.
    hb_store:make_group(Store, UncommittedID),
    maps:map(
        fun(<<"device">>, Map) when is_map(Map) ->
            ?event(error, {request_to_write_device_map, Map}),
            throw({device_map_cannot_be_written, Map});
        (Key, Value) ->
            ?event({writing_subkey, {key, Key}, {value, Value}}),
            KeyHashPath =
                hb_path:hashpath(
                    UncommittedID,
                    hb_path:to_binary(Key),
                    MsgHashpathAlg,
                    Opts
                ),
            ?event({key_hashpath_from_unsigned, KeyHashPath}),
            ValueAltIDs = calculate_all_ids(Value, Opts),
            {ok, Path} = do_write_message(Value, ValueAltIDs, Store, Opts),
            hb_store:make_link(Store, Path, KeyHashPath),
            ?event(
                {
                    {link, KeyHashPath},
                    {data_path, Path}
                }
            ),
            Path
        end,
        hb_private:reset(Msg)
    ),
    % Write the commitments to the store, linking each commitment ID to the
    % uncommitted message.
    lists:map(
        fun(AltID) ->
            ?event({linking_commitment,
                {uncommitted_id, UncommittedID},
                {committed_id, AltID}
            }),
            hb_store:make_link(Store, UncommittedID, AltID)
        end,
        AltIDs
    ),
    {ok, UncommittedID}.

%% @doc Calculate the IDs for a message.
calculate_all_ids(Bin, _Opts) when is_binary(Bin) -> [];
calculate_all_ids(Msg, _Opts) ->
    Commitments =
        maps:without(
            [<<"priv">>],
            maps:get(<<"commitments">>, Msg, #{})
        ),
    maps:keys(Commitments).

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
    {ok, Path} = do_write_message(Bin, [Hashpath], Store, Opts),
    {ok, Path}.

%% @doc Read the message at a path. Returns in `structured@1.0' format: Either a
%% richly typed map or a direct binary.
read(Path, Opts) ->
    case store_read(Path, hb_opts:get(store, no_viable_store, Opts), Opts) of
        not_found -> not_found;
        {ok, Res} ->
            ?event({applying_types_to_read_message, Res}),
            Structured = dev_codec_structured:to(Res),
            ?event({finished_read, Structured}),
            {ok, Structured}
    end.

%% @doc List all of the subpaths of a given path, read each in turn, returning a
%% flat map. We track the paths that we have already read to avoid circular
%% links.
store_read(Path, Store, Opts) ->
    store_read(Path, Store, Opts, []).
store_read(_Path, no_viable_store, _, _AlreadyRead) ->
    not_found;
store_read(Path, Store, Opts, AlreadyRead) ->
    case lists:member(Path, AlreadyRead) of
        true ->
            ?event(read_error,
                {circular_links_detected,
                    {path, Path},
                    {already_read, AlreadyRead}
                }
            ),
            throw({circular_links_detected, Path, {already_read, AlreadyRead}});
        false ->
            do_read(Path, Store, Opts, AlreadyRead)
    end.

%% @doc Read a path from the store. Unsafe: May recurse indefinitely if circular
%% links are present.
do_read(Path, Store, Opts, AlreadyRead) ->
    ResolvedFullPath = hb_store:resolve(Store, PathToBin = hb_path:to_binary(Path)),
    ?event({reading, {path, PathToBin}, {resolved, ResolvedFullPath}}),
    case hb_store:type(Store, ResolvedFullPath) of
        not_found -> not_found;
        no_viable_store -> not_found;
        simple ->
            case hb_store:read(Store, ResolvedFullPath) of
                {ok, Bin} -> {ok, Bin};
                {error, _} -> not_found
            end;
        _ ->
            case hb_store:list(Store, ResolvedFullPath) of
                {ok, Subpaths} ->
                    ?event(
                        {listed,
                            {original_path, Path},
                            {subpaths, {explicit, Subpaths}}
                        }
                    ),
                    Msg =
                        maps:from_list(
                            lists:map(
                                fun(Subpath) ->
                                    ?event({reading_subpath, {path, Subpath}, {store, Store}}),
                                    Res = store_read(
                                        [ResolvedFullPath, Subpath],
                                        Store,
                                        Opts,
                                        [ResolvedFullPath | AlreadyRead]
                                    ),
                                    case Res of
                                        not_found ->
                                            ?event(error,
                                                {subpath_not_found,
                                                    {parent, Path},
                                                    {resolved_parent, {string, ResolvedFullPath}},
                                                    {subpath, Subpath},
                                                    {all_subpaths, Subpaths},
                                                    {store, Store}
                                                }
                                            ),
                                            TriedPath = hb_path:to_binary([ResolvedFullPath, Subpath]),
                                            throw({subpath_not_found,
                                                {parent, Path},
                                                {resolved_parent, ResolvedFullPath},
                                                {failed_path, TriedPath}
                                            });
                                        {ok, Data} ->
                                            {iolist_to_binary([Subpath]), Data}
                                    end
                                end,
                                Subpaths
                            )
                        ),
                    ?event({read_message, Msg}),
                    {ok, Msg};
                _ -> not_found
            end
    end.

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

%% @doc Test storing and retrieving a simple unsigned item
test_store_simple_unsigned_message(Opts) ->
    Item = test_unsigned(<<"Simple unsigned data item">>),
    %% Write the simple unsigned item
    {ok, _Path} = write(Item, Opts),
    %% Read the item back
    ID = hb_util:human_id(hb_ao:get(id, Item)),
    {ok, RetrievedItem} = read(ID, Opts),
    ?assert(hb_message:match(Item, RetrievedItem)),
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
    ?assert(hb_message:match(Committed, RetrievedItem)),
    ?assert(hb_message:match(Committed, RetrievedItemU)),
    ok.

%% @doc Test storing and retrieving a simple unsigned item
test_store_simple_signed_message(Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    hb_store:reset(Store),
    Wallet = ar_wallet:new(),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    Item = test_signed(#{ <<"l2-test-key">> => <<"l2-test-value">> }, Wallet),
    ?event({writing_message, Item}),
    %% Write the simple unsigned item
    {ok, _Path} = write(Item, Opts),
    %% Read the item back
    {ok, UID} = dev_message:id(Item, #{ <<"committers">> => <<"none">> }, Opts),
    {ok, RetrievedItemU} = read(UID, Opts),
    ?event({retreived_unsigned_message, {expected, Item}, {got, RetrievedItemU}}),
    ?assert(hb_message:match(Item, RetrievedItemU)),
    {ok, CommittedID} = dev_message:id(Item, #{ <<"committers">> => [Address] }, Opts),
    {ok, RetrievedItemS} = read(CommittedID, Opts),
    ?assert(hb_message:match(Item, RetrievedItemS)),
    ok.

%% @doc Test deeply nested item storage and retrieval
test_deeply_nested_complex_message(Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    hb_store:reset(Store),
    Wallet = ar_wallet:new(),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    %% Create nested data
    Level3SignedSubmessage = test_signed([1,2,3], Wallet),
    Outer =
        hb_message:commit(
            #{
                <<"level1">> =>
                    hb_message:commit(
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
                        ar_wallet:new()
                    ),
                <<"a">> => <<"b">>
            },
            Wallet
        ),
    {ok, UID} = dev_message:id(Outer, #{ <<"committers">> => <<"none">> }, Opts),
    ?event({string, <<"================================================">>}),
    {ok, CommittedID} = dev_message:id(Outer, #{ <<"committers">> => [Address] }, Opts),
    ?event({string, <<"================================================">>}),
    ?event({test_message_ids, {uncommitted, UID}, {committed, CommittedID}}),
    %% Write the nested item
    {ok, _} = write(Outer, Opts),
    %% Read the deep value back using subpath
    {ok, DeepMsg} =
        read(
            [
                OuterID = hb_util:human_id(UID),
                <<"level1">>,
                <<"level2">>,
                <<"level3">>
            ],
            Opts
        ),
    ?event({deep_message, DeepMsg}),
    %% Assert that the retrieved item matches the original deep value
    ?assertEqual([1,2,3], hb_ao:get(<<"other-test-key">>, DeepMsg)),
    ?event({deep_message_match, {read, DeepMsg}, {write, Level3SignedSubmessage}}),
    ?assert(hb_message:match(Level3SignedSubmessage, DeepMsg)),
    {ok, OuterMsg} = read(OuterID, Opts),
    ?assert(hb_message:match(Outer, OuterMsg)),
    ?event({reading_committed_outer, {id, CommittedID}, {expect, Outer}}),
    {ok, CommittedMsg} = read(hb_util:human_id(CommittedID), Opts),
    ?assert(hb_message:match(Outer, CommittedMsg)).

test_message_with_message(Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    hb_store:reset(Store),
    Msg = test_unsigned([<<"a">>, <<"b">>, <<"c">>]),
    ?event({writing_message, Msg}),
    {ok, Path} = write(Msg, Opts),
    {ok, RetrievedItem} = read(Path, Opts),
    ?assert(hb_message:match(Msg, RetrievedItem)).

cache_suite_test_() ->
    hb_store:generate_test_suite([
        {"store unsigned empty message", fun test_store_unsigned_empty_message/1},
        {"store binary", fun test_store_binary/1},
        {"store simple unsigned message", fun test_store_simple_unsigned_message/1},
        {"store simple signed message", fun test_store_simple_signed_message/1},
        {"deeply nested complex message", fun test_deeply_nested_complex_message/1},
        {"message with message", fun test_message_with_message/1}
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
    Opts = #{ store => StoreOpts = 
        [#{ <<"store-module">> => hb_store_fs, <<"prefix">> => <<"cache-TEST">> }]},
    test_store_unsigned_empty_message(Opts).