%%% @doc The storage modules an Arweave node holds the weave in.
%%%
%%% A storage module is a range of the weave held in one packing format, on
%%% disk, in the layout the Arweave node writes: a directory named for the
%%% range and the packing, holding fixed-size chunk files under
%%% `chunk_storage'. An operator points this node at a data directory an
%%% Arweave node already filled and it reads what is there, because the layout
%%% is the vendored arithmetic in `ar_chunk_storage' and `ar_storage_module'
%%% rather than anything this node invented.
%%%
%%% What the Arweave node keeps in RocksDB beside those files -- which offsets
%%% are synced, and the Merkle paths that place each chunk in the weave -- this
%%% node keeps in a store of its own, under `index' in the data directory.
%%% Nothing writes into the Arweave node's own `rocksdb' directory, so a data
%%% directory stays readable by the node that built it.
%%%
%%% This module owns the configuration and the exclusion. Every module is
%%% named by the same `arweave-storage-modules' entry it was configured from,
%%% and every mutation of one runs through `exclusive/3', because the writes to
%%% a chunk file, its index and its sync record are one operation and two
%%% passes interleaving them would leave a record claiming bytes that were
%%% never written.
-module(lib_arweave_storage).
-export([modules/1, module/2, find/2, covering/3, id/1, range/1, range/2]).
-export([packing/1, packing_label/1, address/1, packing_difficulty/1]).
-export([data_dir/1, chunk_group_size/1, module_path/2, chunk_dir/2]).
-export([store/1, exclusive/3]).
-export([to_message/1, parse_id/1]).
-include("include/hb.hrl").
-include("include/ar.hrl").
-include("include/ar_chunk_storage.hrl").
-include("include/ar_consensus.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The directory every module's index is kept in, beside `storage_modules'.
-define(INDEX_DIR, "index").

%%% The store capacity one module's index is given. A partition holds some
%%% 13.7 million chunks and an entry is the two Merkle paths that prove one, so
%%% a partition's index runs to twenty-odd gigabytes.
%%%
%%% It is named rather than left to the store's own default because an LMDB
%%% environment reserves its whole capacity in address space at open. A node
%%% has 128 TiB of it, and at the store's 2 TiB default the sixty-fourth
%%% environment a node opens fails -- so a capacity is a claim on a resource
%%% shared with every other store the node keeps.
-define(INDEX_CAPACITY, 64 * 1024 * 1024 * 1024).

%% @doc The storage modules this node is configured to hold, oldest option
%% spelling first. Each is the `{BucketSize, Bucket, Packing}' triple the
%% vendored code names a module by.
modules(Opts) ->
    [
        module(Entry, Opts)
    ||
        Entry <-
            hb_util:message_to_ordered_list(
                hb_opts:get(<<"arweave-storage-modules">>, [], Opts),
                Opts
            )
    ].

%% @doc Read one storage module from its configuration entry. The bucket size
%% defaults to the partition size, which is the only size a miner may use, and
%% the address to the one this node mines to.
module(Entry, Opts) ->
    BucketSize =
        hb_util:int(
            hb_maps:get(
                <<"bucket-size">>, Entry, ar_block:partition_size(), Opts)
        ),
    Bucket = hb_util:int(hb_maps:get(<<"bucket">>, Entry, 0, Opts)),
    {BucketSize, Bucket, entry_packing(Entry, Opts)}.

%% @doc Return the module of the given list with the given identifier, or
%% `not_found'.
find(_StoreID, []) ->
    not_found;
find(StoreID, [Module | Modules]) ->
    case id(Module) of
        StoreID -> Module;
        _ -> find(StoreID, Modules)
    end.

%% @doc Return every configured module holding the given offset in the given
%% packing. A recall range is read from one of these; an offset none of them
%% covers is one this node cannot answer for.
covering(Offset, Packing, Opts) ->
    [
        Module
    ||
        Module <- ar_storage_module:get_all(Offset, modules(Opts)),
        Packing == any orelse ar_storage_module:get_packing(Module) == Packing
    ].

%% @doc The identifier a module's directory is named by. This is the vendored
%% spelling, so a directory an Arweave node created is found under it.
id(Module) ->
    ar_storage_module:id(Module).

%% @doc The offsets a module is responsible for, including the overlap that
%% lets a whole recall range be read from one module.
range(Module) ->
    ar_storage_module:module_range(Module).

range(Module, Overlap) ->
    ar_storage_module:module_range(Module, Overlap).

%% @doc The packing a module holds its chunks in.
packing(Module) ->
    ar_storage_module:get_packing(Module).

%% @doc Name a packing on the wire, in the spelling `~arweave-spora@2.9' takes.
packing_label({spora_2_6, _Addr}) -> <<"spora-2-6">>;
packing_label({composite, _Addr, _Difficulty}) -> <<"composite">>;
packing_label({replica_2_9, _Addr}) -> <<"replica-2-9">>;
packing_label(unpacked) -> <<"unpacked">>;
packing_label(unpacked_padded) -> <<"unpacked-padded">>.

%% @doc The address a module's packing is bound to, or `undefined' for a
%% packing that is bound to none.
address(Module) ->
    ar_storage_module:module_address(Module).

%% @doc The packing difficulty a module's chunks are packed at.
packing_difficulty(Module) ->
    ar_storage_module:module_packing_difficulty(Module).

%% @doc The directory the storage modules live under. An operator points this
%% at the data directory of the Arweave node whose data they are.
data_dir(Opts) ->
    hb_util:list(hb_opts:get(<<"arweave-data-dir">>, <<"arweave-data">>, Opts)).

%% @doc How many bytes of chunk data one chunk file holds. An Arweave node that
%% was run with a different value wrote different files, so this is read rather
%% than assumed.
chunk_group_size(Opts) ->
    hb_util:int(
        hb_opts:get(<<"arweave-chunk-group-size">>, ?CHUNK_GROUP_SIZE, Opts)).

%% @doc The directory one module occupies.
module_path(Module, Opts) ->
    ar_chunk_storage:get_storage_module_path(data_dir(Opts), id(Module)).

%% @doc The directory one module's chunk files occupy.
chunk_dir(Module, Opts) ->
    ar_chunk_storage:get_chunk_storage_path(data_dir(Opts), id(Module)).

%% @doc The store every module's index and sync records are kept in: one
%% store, under `index' in the data directory, beside `storage_modules'.
%%
%% Every key names the module it belongs to, so modules sharing a store do not
%% collide where their ranges overlap. One store rather than one per module,
%% because an LMDB environment reserves its capacity in address space when it
%% is opened: a node that spent one per partition would be spending the
%% resource its cache and its own store are drawn from too. The capacity is
%% sized for what the modules actually hold, for the same reason. Keeping it
%% out of `storage_modules' also leaves an Arweave node's own directories
%% holding only what that node put there.
%%
%% `arweave-storage-index' names a store to use instead, for a deployment that
%% keeps its indexes elsewhere.
store(Opts) ->
    case hb_opts:get(<<"arweave-storage-index">>, [], Opts) of
        [] ->
            #{
                <<"store-module">> => hb_store_lmdb,
                <<"name">> =>
                    hb_util:bin(filename:join([data_dir(Opts), ?INDEX_DIR])),
                <<"capacity">> =>
                    ?INDEX_CAPACITY * max(1, length(modules(Opts)))
            };
        Store ->
            Store
    end.

%% @doc Run `Fun' as the only mutation of one storage module at a time.
%%
%% The chunk file, the index and the sync record of a module are written
%% together and read together, so a second pass running against the same module
%% would write a record for bytes another pass is still writing. Callers queue
%% behind the runner and receive its result, so the serialisation is invisible
%% to them.
%%
%% `hb_name' is BEAM-global, so the name carries the module's own directory:
%% two nodes in one BEAM configured against different data directories are
%% independent, and two configured against the same one are not, which is the
%% truth about the disk they share.
exclusive(Module, Fun, Opts) ->
    Runner = hb_name:singleton(name(Module, Opts), fun runner/0),
    Monitor = erlang:monitor(process, Runner),
    Runner ! {run, self(), Monitor, Fun},
    receive
        {ran, Monitor, Result} ->
            erlang:demonitor(Monitor, [flush]),
            Result;
        {'DOWN', Monitor, process, Runner, Reason} ->
            {error,
                #{
                    <<"status">> => 500,
                    <<"message">> => <<"storage-runner-down">>,
                    <<"detail">> => hb_util:bin(io_lib:format("~p", [Reason]))
                }
            }
    end.

%% @doc Describe a module as an AO-Core message: what it holds, where, and in
%% what form.
to_message(Module) ->
    {BucketSize, Bucket, Packing} = Module,
    {Start, End} = range(Module),
    Base =
        #{
            <<"id">> => hb_util:bin(id(Module)),
            <<"bucket">> => Bucket,
            <<"bucket-size">> => BucketSize,
            <<"packing">> => packing_label(Packing),
            <<"packing-difficulty">> => packing_difficulty(Module),
            <<"range-start">> => Start,
            <<"range-end">> => End
        },
    case address(Module) of
        undefined -> Base;
        Address -> Base#{ <<"address">> => hb_util:encode(Address) }
    end.

%% @doc Read a module out of the identifier its directory is named by. The
%% inverse of `ar_storage_module:id/1', for finding the modules a data
%% directory already holds.
parse_id(StoreID) ->
    case string:split(hb_util:list(StoreID), "storage_module_") of
        ["", Rest] -> parse_fields(string:split(Rest, "_"));
        _ -> not_found
    end.

%%% Internal functions.

%% @doc Read the packing of a configuration entry. A module that names no
%% address takes the one this node mines to, because a module packed for
%% another address is one this node cannot mine from.
entry_packing(Entry, Opts) ->
    case hb_maps:get(<<"packing">>, Entry, <<"replica-2-9">>, Opts) of
        <<"unpacked">> ->
            unpacked;
        <<"replica-2-9">> ->
            {replica_2_9, entry_address(Entry, Opts)};
        <<"spora-2-6">> ->
            {spora_2_6, entry_address(Entry, Opts)};
        <<"composite">> ->
            {composite,
                entry_address(Entry, Opts),
                hb_util:int(
                    hb_maps:get(<<"packing-difficulty">>, Entry, 1, Opts))
            };
        Format ->
            throw({'unsupported-packing', Format})
    end.

%% @doc The address a module's packing is bound to.
entry_address(Entry, Opts) ->
    case hb_maps:get(<<"address">>, Entry, not_found, Opts) of
        not_found ->
            ar_wallet:to_address(hb_opts:get(priv_wallet, hb:wallet(), Opts));
        Address -> hb_util:native_id(Address)
    end.

%% @doc The registration name of one module's runner.
name(Module, Opts) ->
    {arweave_storage_module, module_path(Module, Opts)}.

%% @doc The runner's loop: one task at a time, each replying to its caller.
runner() ->
    receive
        {run, From, Ref, Fun} ->
            From ! {ran, Ref, run(Fun)},
            runner()
    end.

%% @doc Run one task, reporting a crash rather than dying of it: a runner that
%% died would leave every queued caller waiting on a reply that never comes.
run(Fun) ->
    try Fun()
    catch
        Class:Reason:Stacktrace ->
            ?event(warning,
                {arweave_storage_task_failed,
                    {class, Class},
                    {reason, Reason},
                    {stacktrace, {trace, Stacktrace}}
                }
            ),
            {error,
                #{
                    <<"status">> => 500,
                    <<"message">> => <<"storage-task-failed">>,
                    <<"detail">> => hb_util:bin(io_lib:format("~p", [Reason]))
                }
            }
    end.

%% @doc Read the fields of an identifier: a bucket and a packing, or a bucket
%% size, a bucket and a packing. A module whose bucket size is the partition
%% size names only its bucket, which is the form a miner's modules take.
%%
%% The fields are taken from the left one at a time rather than by splitting on
%% every separator, because the packing carries one: an address is base64url,
%% and about half of all addresses contain an underscore.
parse_fields([Bucket, Rest]) ->
    parse_rest(Bucket, string:split(Rest, "_"));
parse_fields(_Fields) ->
    not_found.

%% @doc Decide which of the two forms an identifier is in. The second field is a
%% bucket only when it is an integer and something follows it; otherwise the
%% whole of the rest is the packing, underscores and all.
parse_rest(Bucket, [Second, Third]) ->
    parse_rest(Bucket, Second, Third, catch hb_util:int(Second));
parse_rest(Bucket, [Packing]) ->
    parsed(ar_block:partition_size(), Bucket, Packing).

parse_rest(BucketSize, Bucket, Packing, Integer) when is_integer(Integer) ->
    parsed(BucketSize, Bucket, Packing);
parse_rest(Bucket, Second, Third, _NotInteger) ->
    parsed(ar_block:partition_size(), Bucket, Second ++ "_" ++ Third).

%% @doc Build the module a set of parsed fields names, or `not_found' for a
%% bucket that is not a number or a packing this node has no name for.
parsed(BucketSize, Bucket, Packing) ->
    case catch hb_util:int(Bucket) of
        Number when is_integer(Number) ->
            parsed_packing(BucketSize, Number, parse_packing(Packing));
        _ ->
            not_found
    end.

parsed_packing(_BucketSize, _Bucket, not_found) ->
    not_found;
parsed_packing(BucketSize, Bucket, Packing) ->
    {hb_util:int(BucketSize), Bucket, Packing}.

%% @doc Read the packing an identifier names. The address is base64url, as it
%% is everywhere else this subsystem spells one.
parse_packing("unpacked") ->
    unpacked;
parse_packing(Packing) ->
    case string:split(Packing, ".", all) of
        [Address, "replica", "2", "9"] ->
            {replica_2_9, hb_util:native_id(hb_util:bin(Address))};
        [Address, Difficulty] ->
            {composite,
                hb_util:native_id(hb_util:bin(Address)),
                hb_util:int(Difficulty)
            };
        [Address] ->
            {spora_2_6, hb_util:native_id(hb_util:bin(Address))};
        _ ->
            not_found
    end.

%%% Tests.

%% @doc Every identifier an Arweave node writes reads back as the module it
%% names, and names itself again.
%%
%% The addresses are the case the naive parse gets wrong: an address is
%% base64url, and about half of all addresses contain the same underscore the
%% identifier separates its fields with. The first is the identifier a real
%% Arweave node wrote for a bucket of the mainnet weave.
parse_id_round_trip_test() ->
    lists:foreach(
        fun(ID) ->
            Module = parse_id(ID),
            ?assertNotEqual(not_found, Module),
            ?assertEqual(ID, id(Module))
        end,
        [
            "storage_module_14063501312_27020_"
                "uaV-x-DGYWgKxGs5AsbOY9GDPWTFOz63Q9kPTal0ePA.replica.2.9",
            "storage_module_27020_"
                "f9RjWid39W__dibIykicpypIsjNh8Y9VkCVnWaeOLzg.replica.2.9",
            "storage_module_14063501312_27020_unpacked",
            "storage_module_3_unpacked",
            "storage_module_5_"
                "f9RjWid39W__dibIykicpypIsjNh8Y9VkCVnWaeOLzg.7",
            "storage_module_5_"
                "f9RjWid39W__dibIykicpypIsjNh8Y9VkCVnWaeOLzg"
        ]
    ).

%% @doc A directory that is not a storage module, or one whose bucket is not a
%% number, names no module -- rather than half of one.
parse_id_refuses_test() ->
    ?assertEqual(not_found, parse_id("not_a_module")),
    ?assertEqual(not_found, parse_id("storage_module_x_unpacked")),
    ?assertEqual(not_found, parse_id("rocksdb")),
    ?assertEqual(not_found, parse_id("storage_module_")).

%% @doc A module of the partition size names only its bucket; any other size
%% names both. The two forms are what tells an operator's directory apart from
%% a miner's.
id_forms_test() ->
    Address = hb_util:native_id(<<"f9RjWid39W__dibIykicpypIsjNh8Y9VkCVnWaeOLzg">>),
    ?assertEqual(
        "storage_module_7_f9RjWid39W__dibIykicpypIsjNh8Y9VkCVnWaeOLzg.replica.2.9",
        id({ar_block:partition_size(), 7, {replica_2_9, Address}})
    ),
    ?assertEqual(
        "storage_module_1048576_7_unpacked",
        id({1048576, 7, unpacked})
    ).
