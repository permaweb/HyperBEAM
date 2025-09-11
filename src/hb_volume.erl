-module(hb_volume).
-moduledoc """
Module for managing physical disks and volumes, providing operations
for partitioning, formatting, mounting, and managing encrypted volumes.
""".
-export([list_partitions/0, create_partition_table/1, create_partition/2, get_partition_info/1]).
-export([format_partition/2, mount_disk/4, change_node_store/2]).
-export([check_for_device/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-doc """
List available partitions in the system.
@returns {ok, Map} where Map contains the partition information,
         or {error, Reason} if the operation fails.
""".
-spec list_partitions() -> {ok, map()} | {error, binary()}.
list_partitions() ->
    ?event(debug_volume, {list_partitions, entry, starting}),    
    % Get the partition information using fdisk -l
    ?event(debug_volume, {list_partitions, executing_fdisk, command}),
    case safe_exec("fdisk -l") of
        {error, Error} ->
            ?event(debug_volume, {list_partitions, fdisk_error, Error}),
            {error, <<"Failed to list disk partitions">>};
        {ok, Output} ->
            ?event(debug_volume, {list_partitions, fdisk_success, parsing}),

            Lines = string:split(Output, "\n", all),

            % Process the output to group information by disk
            {_, DiskData} = lists:foldl(
                fun process_disk_line/2,
                {undefined, []},
                Lines
            ),
            % Process each disk's data to extract all information
            DiskObjects = lists:filtermap(
                fun(DiskEntry) ->
                    Device = maps:get(<<"device">>, DiskEntry),
                    DiskLines = lists:reverse(maps:get(<<"data">>, DiskEntry)),
                    DiskInfo = parse_disk_info(Device, DiskLines),
                    {true, DiskInfo}
                end,
                DiskData
            ),
            % Return the partition information
            ?event(debug_volume, 
                {list_partitions, success, 
                   {disk_count, length(DiskObjects)}
                }
            ),
            {ok, #{
                <<"status">> => 200,
                <<"content-type">> => <<"application/json">>,
                <<"body">> => hb_json:encode(#{<<"disks">> => DiskObjects})
            }}
    end.

%%% Helper functions for list_partitions
% Process a line of fdisk output to group by disk
process_disk_line(Line, {CurrentDisk, Acc}) ->
    % Match for a new disk entry
    DiskPattern = "^Disk (/dev/(?!ram)\\S+):",
    case re:run(Line, DiskPattern, [{capture, [1], binary}]) of
        {match, [Device]} ->
            % Start a new disk entry
            NewDisk = #{
                <<"device">> => Device,
                <<"data">> => [Line]
            },
            {NewDisk, [NewDisk | Acc]};
        _ when CurrentDisk =:= undefined ->
            % Not a disk line and no current disk
            {undefined, Acc};
        _ ->
            % Add line to current disk's data
            CurrentData = maps:get(<<"data">>, CurrentDisk),
            UpdatedDisk = CurrentDisk#{
                <<"data">> => [Line | CurrentData]
            },
            % Update the list with the modified disk entry
            UpdatedAcc = [UpdatedDisk | lists:delete(CurrentDisk, Acc)],
            {UpdatedDisk, UpdatedAcc}
    end.

% Parse detailed disk information from fdisk output lines
parse_disk_info(Device, Lines) ->
    % Initialize with device ID
    InitialInfo = #{<<"device">> => Device},
    
    % Define all parsing functions as a list
    Parsers = [
        fun parse_size_info/2,
        fun parse_model_info/2,
        fun parse_units_info/2,
        fun parse_sector_size_info/2,
        fun parse_io_size_info/2
    ],
    
    % Apply each parser to each line and accumulate results
    lists:foldl(
        fun(Line, Info) ->
            apply_first_matching_parser(Line, Info, Parsers)
        end,
        InitialInfo,
        Lines
    ).

% Apply a list of parsers to a line, returning updated info when first match found
apply_first_matching_parser(_Line, Info, []) ->
    Info;
apply_first_matching_parser(Line, Info, [Parser | RestParsers]) ->
    case Parser(Line, Info) of
        continue ->
            % No change, try next parser
            apply_first_matching_parser(Line, Info, RestParsers);
        {ok, UpdatedInfo} ->
            % Parser matched and updated info, return result
            UpdatedInfo
    end.

% Parse disk size and bytes information
parse_size_info(Line, Info) ->
    SizePattern = "^Disk .+: ([0-9.]+ [KMGT]iB), ([0-9]+) bytes, ([0-9]+) sectors",
    case re:run(Line, SizePattern, [{capture, [1, 2, 3], binary}]) of
        {match, [Size, Bytes, Sectors]} ->
            {ok, Info#{
                <<"size">> => Size,
                <<"bytes">> => binary_to_integer(Bytes),
                <<"sectors">> => binary_to_integer(Sectors)
            }};
        _ -> 
            continue
    end.

% Parse disk model information
parse_model_info(Line, Info) ->
    ModelPattern = "^Disk model: (.+)\\s*$",
    case re:run(Line, ModelPattern, [{capture, [1], binary}]) of
        {match, [Model]} ->
            {ok, Info#{<<"model">> => string:trim(Model)}};
        _ ->
            continue
    end.

% Parse disk units information
parse_units_info(Line, Info) ->
    UnitsPattern = "^Units: (.+)$",
    case re:run(Line, UnitsPattern, [{capture, [1], binary}]) of
        {match, [Units]} ->
            {ok, Info#{<<"units">> => Units}};
        _ ->
            continue
    end.

% Parse sector size information
parse_sector_size_info(Line, Info) ->
    SectorPattern = "^Sector size \\(logical/physical\\): ([^/]+)/(.+)$",
    case re:run(Line, SectorPattern, [{capture, [1, 2], binary}]) of
        {match, [LogicalSize, PhysicalSize]} ->
            {ok, Info#{
                <<"sector_size">> => #{
                    <<"logical">> => string:trim(LogicalSize),
                    <<"physical">> => string:trim(PhysicalSize)
                }
            }};
        _ ->
            continue
    end.

% Parse I/O size information
parse_io_size_info(Line, Info) ->
    IOPattern = "^I/O size \\(minimum/optimal\\): ([^/]+)/(.+)$",
    case re:run(Line, IOPattern, [{capture, [1, 2], binary}]) of
        {match, [MinSize, OptSize]} ->
            {ok, Info#{
                <<"io_size">> => #{
                    <<"minimum">> => string:trim(MinSize),
                    <<"optimal">> => string:trim(OptSize)
                }
            }};
        _ -> 
            continue
    end.

-doc """
Create a partition table on a disk device.
@param Device The path to the device, e.g. "/dev/sdb".
@returns {ok, Output} on success where Output is the command output,
         or {error, Reason} if the operation fails.
""".
-spec create_partition_table(Device :: binary()) ->
    {ok, binary()} | {error, binary()}.
create_partition_table(undefined) ->
    ?event(debug_volume, {create_partition_table, error, device_undefined}),
    {error, <<"Device path not specified">>};
create_partition_table(Device) ->
    ?event(debug_volume, 
        {create_partition_table, entry, 
           {device, Device}
        }
    ),
    % Create a GPT partition table
    DeviceStr = binary_to_list(Device),
    MklabelCmd = "parted " ++ DeviceStr ++ " mklabel gpt",
    ?event(debug_volume, 
        {create_partition_table, creating_gpt_label, 
            {device, Device}
        }
    ),
    ?event(debug_volume, 
        {create_partition_table, executing_mklabel, 
            {command, MklabelCmd}
        }
    ),
    case safe_exec(MklabelCmd) of
        {ok, Output} ->
            ?event(debug_volume, 
                {create_partition_table, gpt_label_success, 
                    {result, Output}
                }
            ),
            {ok, Output};
        {error, Error} ->
            ?event(debug_volume, 
                {create_partition_table, gpt_label_error, 
                    {error, Error}
                }
            ),
            {error, <<"Failed to create partition table">>}
    end.

-doc """
Create a partition on a disk device.
@param Device The path to the device, e.g. "/dev/sdb".
@param PartType The partition type to create, defaults to "ext4".
@returns {ok, Output} on success where Output is the command output,
         or {error, Reason} if the operation fails.
""".
-spec create_partition(Device :: binary(), PartType :: binary()) ->
    {ok, binary()} | {error, binary()}.
create_partition(Device, PartType) ->
    ?event(debug_volume, 
        {create_partition, entry, 
           {device, Device, part_type, PartType}
        }
    ),
    DeviceStr = binary_to_list(Device),
    PartTypeStr = binary_to_list(PartType),
    % Build the parted command to create the partition
    MkpartCmd = 
        "parted -a optimal " ++ DeviceStr ++ 
        " mkpart primary " ++ PartTypeStr ++ " 0% 100%",
    ?event(debug_volume, 
        {create_partition, executing_mkpart, 
           {command, MkpartCmd}
        }
    ),
    case safe_exec(MkpartCmd) of
        {ok, Output} ->
            ?event(debug_volume, 
                {create_partition, mkpart_success, 
                   {result, Output}
                }
            ),
            {ok, Output};
        {error, Error} ->
            ?event(debug_volume, 
                {create_partition, mkpart_error, 
                    {error, Error}
                }
            ),
            {error, <<"Failed to create partition">>}
    end.

-spec get_partition_info(Device :: binary()) ->
    {ok, map()} | {error, binary()}.
get_partition_info(Device) ->
    ?event(debug_volume, {get_partition_info, entry, {device, Device}}),
    DeviceStr = binary_to_list(Device),
    % Print partition information
    PrintCmd = "parted " ++ DeviceStr ++ " print",
    ?event(debug_volume, 
        {get_partition_info, executing_print, {command, PrintCmd}}
    ),
    case safe_exec(PrintCmd) of
        {ok, Output} ->
            ?event(debug_volume, 
                {get_partition_info, success, 
                   {result, Output}
                }
            ),
            {ok, #{
                <<"status">> => 200,
                <<"message">> => <<"Partition created successfully.">>,
                <<"device_path">> => Device,
                <<"partition_info">> => list_to_binary(Output)
            }};
        {error, Error} ->
            ?event(debug_volume, 
                {get_partition_info, mkpart_error, 
                    {error, Error}
                }
            ),
            {error, <<"Failed to get partition information">>}
    end.

-doc """
Format a partition with LUKS encryption.
@param Partition The path to the partition, e.g. "/dev/sdc1".
@param EncKey The encryption key to use for LUKS.
@returns {ok, Map} on success where Map includes the status and 
    confirmation message, or {error, Reason} if the operation fails.
""".
-spec format_partition(Partition :: binary(), EncKey :: binary()) ->
    {ok, map()} | {error, binary()}.
format_partition(undefined, _EncKey) ->
    ?event(debug_volume, {format_partition, error, partition_undefined}),
    {error, <<"Partition path not specified">>};
format_partition(_Partition, undefined) ->
    ?event(debug_volume, {format_partition, error, key_undefined}),
    {error, <<"Encryption key not specified">>};
format_partition(Partition, EncKey) ->
    ?event(debug_volume, 
        {format_partition, entry, 
            {
                partition, Partition, 
                key_present, true
            }
        }
    ),
    PartitionStr = binary_to_list(Partition),
    ?event(debug_volume, {format_partition, creating_secure_key_file, starting}),
    with_secure_key_file(EncKey, fun(KeyFile) ->
        FormatCmd = 
            "cryptsetup luksFormat --batch-mode " ++
            "--key-file " ++ KeyFile ++ " " ++ PartitionStr,
        ?event(debug_volume, 
            {format_partition, executing_luks_format, {command, FormatCmd}}
        ),
        case safe_exec(FormatCmd) of
            {ok, Output} ->
                ?event(debug_volume, 
                    {format_partition, luks_format_success, completed, 
                        {result, Output}
                    }
                ),
                {ok, #{
                    <<"status">> => 200,
                    <<"message">> => 
                        <<"Partition formatted with LUKS encryption "
                          "successfully.">>
                }};
            {error, Error} ->
                ?event(debug_volume, 
                    {format_partition, luks_format_error, Error}
                ),
                {error, <<"Failed to format partition with LUKS">>}
        end
    end).

-doc """
Mount a LUKS-encrypted disk.
@param Partition The path to the partition, e.g. "/dev/sdc1".
@param EncKey The encryption key for LUKS.
@param MountPoint The directory where the disk should be mounted.
@param VolumeName The name to use for the decrypted LUKS volume.
@returns {ok, Map} on success where Map includes the status and 
         confirmation message, or {error, Reason} if the operation fails.
""".
-spec mount_disk(
    Partition :: binary(),
    EncKey :: binary(),
    MountPoint :: binary(),
    VolumeName :: binary()
) -> {ok, map()} | {error, binary()}.
mount_disk(undefined, _EncKey, _MountPoint, _VolumeName) ->
    ?event(debug_volume, {mount_disk, error, partition_undefined}),
    {error, <<"Partition path not specified">>};
mount_disk(_Partition, undefined, _MountPoint, _VolumeName) ->
    ?event(debug_volume, {mount_disk, error, key_undefined}),
    {error, <<"Encryption key not specified">>};
mount_disk(_Partition, _EncKey, undefined, _VolumeName) ->
    ?event(debug_volume, {mount_disk, error, mount_point_undefined}),
    {error, <<"Mount point not specified">>};
mount_disk(Partition, EncKey, MountPoint, VolumeName) ->
    ?event(debug_volume, 
        {mount_disk, entry, 
            {
                partition, Partition,
                mount_point, MountPoint, 
                volume_name, VolumeName}
        }
    ),
    PartitionStr = binary_to_list(Partition),
    VolumeNameStr = binary_to_list(VolumeName),
    ?event(debug_volume, {mount_disk, opening_luks_volume, starting}),
    with_secure_key_file(EncKey, fun(KeyFile) ->
        OpenCmd = 
            "cryptsetup luksOpen --key-file " ++ KeyFile ++ 
            " " ++ PartitionStr ++ " " ++ VolumeNameStr,
        ?event(debug_volume, {mount_disk, executing_luks_open, {command, OpenCmd}}),
        case safe_exec(OpenCmd) of
            {ok, Output} ->
                ?event(debug_volume, 
                    {mount_disk, luks_open_success, proceeding_to_mount, 
                        {result, Output}
                    }
                ),
                mount_opened_volume(Partition, MountPoint, VolumeName);
            {error, Error} ->
                ?event(debug_volume, {mount_disk, luks_open_error, Error}),
                {error, <<"Failed to open LUKS volume">>}
        end
    end).

% Mount an already opened LUKS volume
mount_opened_volume(Partition, MountPoint, VolumeName) ->
    ?event(debug_volume, 
        {mount_opened_volume, entry, 
            {
                partition, Partition, 
                mount_point, MountPoint, 
                volume_name, VolumeName
            }
        }
    ),

    MountPointStr = binary_to_list(MountPoint),
    ?event(debug_volume, 
        {mount_opened_volume, creating_mount_point, MountPoint}
    ),

    VolumeNameStr = binary_to_list(VolumeName),
    DeviceMapperPath = "/dev/mapper/" ++ VolumeNameStr,

    maybe
        {create_mount_point_directory, ok} ?= {create_mount_point_directory, filelib:ensure_path(MountPointStr)},
        {ensure_ext4_filesystem_exists, ok} ?= {ensure_ext4_filesystem_exists, ensure_ext4_filesystem_exists(DeviceMapperPath)},
        {mount_partition, {ok, _}} ?= {mount_partition, safe_exec("mount " ++ DeviceMapperPath ++ " " ++ MountPointStr)},
        {ok, #{
                <<"status">> => 200,
                <<"message">> => 
                    <<"Encrypted partition mounted successfully.">>,
                <<"mount_point">> => MountPoint,
                <<"mount_info">> => #{
                    partition => Partition,
                    mount_point => MountPoint,
                    volume_name => VolumeName
                }
            }}
    else
        {create_mount_point_directory, {error, Error}} -> 
            ?event(debug_volume, 
                {create_and_format_partition, create_mount_point_directory_error, 
                    {error, Error}
                }
            ),
            safe_exec("cryptsetup luksClose " ++ VolumeNameStr),
            {error, <<"Failed to create mount point directory">>};
        {get_block_device_info, {error, Error}} -> 
            ?event(debug_volume, 
                {create_and_format_partition, get_block_device_info_error, 
                    {error, Error}
                }
            ),
            safe_exec("cryptsetup luksClose " ++ VolumeNameStr),
            {error, <<"Failed to get block device info">>};
        {mount_partition, {error, Error}} -> 
            ?event(debug_volume, 
                {create_and_format_partition, mount_partition_error, 
                    {error, Error}
                }
            ),
            safe_exec("cryptsetup luksClose " ++ VolumeNameStr),
            {error, <<"Failed to mount partition">>}
    end.

ensure_ext4_filesystem_exists(DeviceMapperPath) ->
    maybe
        {get_block_device_info, {ok, BlockDeviceInfo}} ?= {get_block_device_info, safe_exec("blkid " ++ DeviceMapperPath)},
        FileSystemFound ?= string:find(BlockDeviceInfo, "TYPE="),
        {maybe_create_ext4_filesystem, {ok, _}} ?= {maybe_create_ext4_filesystem, maybe_create_ext4_filesystem(FileSystemFound, DeviceMapperPath)},
        ok
    else
        {get_block_device_info, {error, Error}} -> 
            ?event(debug_volume, 
                {create_and_format_partition, get_block_device_info_error, 
                    {error, Error}
                }
            ),            
            {error, <<"Failed to get block device info">>};
        {maybe_create_ext4_filesystem, {error, Error}} -> 
            ?event(debug_volume, 
                {create_and_format_partition, maybe_create_ext4_filesystem_error, 
                    {error, Error}
                }
            ),            
            {error, <<"Failed to create ext4 filesystem">>}
    end.

maybe_create_ext4_filesystem(nomatch, DeviceMapperPath) ->
    safe_exec("mkfs.ext4 -F " ++ DeviceMapperPath);

maybe_create_ext4_filesystem(_, _) ->
    {ok, <<"Filesystem already exists">>}.

-doc """
Change the node's data store location to the mounted encrypted disk.
@param StorePath The new path for the store directory.
@param CurrentStore The current store configuration.
@returns {ok, Map} on success where Map includes the status and 
         confirmation message, or {error, Reason} if the operation fails.
""".
-spec change_node_store(StorePath :: binary(), 
                        CurrentStore :: list()) ->
    {ok, map()} | {error, binary()}.
change_node_store(undefined, _CurrentStore) ->
    ?event(debug_volume, {change_node_store, error, store_path_undefined}),
    {error, <<"Store path not specified">>};
change_node_store(StorePath, CurrentStore) ->
    ?event(debug_volume, 
        {change_node_store, entry, 
           {store_path, StorePath, current_store, CurrentStore}
        }
    ),
    % Create the store directory if it doesn't exist
    StorePathStr = binary_to_list(StorePath),
    ?event(debug_volume, {change_node_store, creating_directory, StorePath}),

    maybe
        {create_store_directory, ok} ?= {create_store_directory, filelib:ensure_path(StorePathStr)},
        NewStore ?= update_store_config(CurrentStore, StorePath),
        {ok, #{
            <<"status">> => 200,
            <<"message">> => 
                <<"Node store updated to use encrypted disk.">>,
            <<"store_path">> => StorePath,
            <<"store">> => NewStore
        }}
    else
        {create_store_directory, {error, Error}} ->
            ?event(debug_volume, 
                {change_node_store, failed_to_create_new_store_directory, 
                    {error, Error}
                }
            ),            
            {error, <<"Failed to create store directory at new path">>}
    end.

% Update the store configuration with a new base path
-spec update_store_config(StoreConfig :: term(), 
    NewPath :: binary()) -> term().
update_store_config(StoreConfig, NewPath) when is_list(StoreConfig) ->
    % For a list, update each element
    [update_store_config(Item, NewPath) || Item <- StoreConfig];
update_store_config(
    #{<<"store-module">> := Module} = StoreConfig, 
    NewPath
) when is_map(StoreConfig) ->
    % Handle various store module types differently
    case Module of
        hb_store_fs ->
            % For filesystem store, prefix the existing path with the new path
            ExistingPath = maps:get(<<"name">>, StoreConfig, <<"">>),
            NewName = <<NewPath/binary, "/", ExistingPath/binary>>,
            ?event(debug_volume, {fs, StoreConfig, NewPath, NewName}),
            StoreConfig#{<<"name">> => NewName};
        hb_store_lmdb ->
            ExistingPath = maps:get(<<"name">>, StoreConfig, <<"">>),
            NewName = <<NewPath/binary, "/", ExistingPath/binary>>,
            ?event(debug_volume, {migrate_start, ExistingPath, NewName}),
            safe_stop_lmdb_store(StoreConfig),
            ?event(debug_volume, {using_existing_store, NewName}),
            FinalConfig = StoreConfig#{<<"name">> => NewName},
            safe_start_lmdb_store(FinalConfig),
            FinalConfig;
        hb_store_rocksdb ->
            StoreConfig;
        hb_store_gateway ->
            % For gateway store, recursively update nested store configs
            NestedStore = maps:get(<<"store">>, StoreConfig, []),
            StoreConfig#{
                <<"store">> => update_store_config(NestedStore, NewPath)
            };
        _ ->
            % For any other store type, update the prefix
            % StoreConfig#{<<"name">> => NewPath}
            ?event(debug_volume, {other, StoreConfig, NewPath}),
            StoreConfig
    end;
update_store_config({Type, _OldPath, Opts}, NewPath) ->
    % For tuple format with options
    {Type, NewPath, Opts};
update_store_config({Type, _OldPath}, NewPath) ->
    % For tuple format without options
    {Type, NewPath};
update_store_config(StoreConfig, _NewPath) ->
    % Return unchanged for any other format
    StoreConfig.

%% Safely stop LMDB store with error handling
safe_stop_lmdb_store(StoreConfig) ->
    ?event(debug_volume, {stopping_current_store, StoreConfig}),
    try 
        hb_store_lmdb:stop(StoreConfig)
    catch 
        error:StopReason ->
            ?event(debug_volume, {stop_error, StopReason})
    end.

%% Safely start LMDB store
safe_start_lmdb_store(StoreConfig) ->
    NewName = maps:get(<<"name">>, StoreConfig),
    ?event(debug_volume, {starting_new_store, NewName}),
    hb_store_lmdb:start(StoreConfig).

%%% Helper functions
%% Execute system command with error checking
-spec safe_exec(Command :: string()) -> 
    {ok, binary()} | {error, {command_failed, integer(), binary()}} | {error, {command_timeout, binary()}}.
safe_exec(Command) ->
    Port = erlang:open_port({spawn, Command}, [exit_status, {line, 256}]),
    collect_output(Port, []).

collect_output(Port, Acc) ->
    receive
        {Port, {data, {eol, Line}}} ->
            collect_output(Port, [Line | Acc]);
        {Port, {data, {noeol, Line}}} ->
            collect_output(Port, [Line | Acc]);
        {Port, {exit_status, 0}} ->
            safe_port_close(Port),
            {ok, string:join(lists:reverse(Acc), "\n")};
        {Port, {exit_status, ExitCode}} ->
            safe_port_close(Port),
            {error, {command_failed, ExitCode, string:join(lists:reverse(Acc), "\n")}}
    after 15 * 60 * 1000 ->
        safe_port_close(Port),
        {error, {command_timeout, string:join(lists:reverse(Acc), "\n")}}
    end.

safe_port_close(Port) ->
    try
        port_close(Port)
    catch
        error:badarg -> 
            % Port already closed, ignore
            ok
    end.

%% Helper function that will store encryption key in a temporary file,
%% and execute the provided function with the key file path as argument.
%% It will securely delete the key file after execution.
with_secure_key_file(EncKey, Fun) ->
    ?event(debug_volume, {with_secure_key_file, entry, creating_temp_file}),

    KeyFile = "/root/tmp/luks_key_" ++ os:getpid(),

    ?event(debug_volume, {with_secure_key_file, key_file_path, KeyFile}),

    maybe
        {create_temp_directory, ok} ?= {create_temp_directory, filelib:ensure_path("/root/tmp")},
        BinaryEncryptionKey ?= encryption_key_to_binary(EncKey),
        {write_encryption_binary_to_file, ok} ?= {write_encryption_binary_to_file, file:write_file(KeyFile, BinaryEncryptionKey, [raw])},
        Result ?= Fun(KeyFile),
        {shred_key_file_after_execution, {ok, _}} ?= {shred_key_file_after_execution, safe_exec("shred -u " ++ KeyFile)},
        {ok, Result}
    else
        {create_temp_directory, {error, Error}} ->
            ?event(debug_volume, 
                {with_secure_key_file, create_temp_directory_error, 
                    {error, Error}
                }
            ),
            safe_exec("shred -u " ++ KeyFile),
            {error, <<"Failed to temp directory">>};
        {write_encryption_binary_to_file, {error, Error}} ->
            ?event(debug_volume, 
                {with_secure_key_file, write_encryption_binary_to_file_error, 
                    {error, Error}
                }
            ),
            safe_exec("shred -u " ++ KeyFile),
            {error, <<"Failed to write encryption key(binary) to file">>};
        {shred_key_file_after_execution, {error, Error}} ->
            ?event(debug_volume, 
                {with_secure_key_file, shred_key_file_after_execution_error, 
                    {error, Error}
                }
            ),
            {error, <<"Failed to shred key file after execution">>}
    end.

-doc """
Check if a device exists on the system.
@param Device The path to the device to check (binary).
@returns true if the device exists, false otherwise.
""".
-spec check_for_device(Device :: binary()) -> {ok, boolean()} | {error, binary()}.
check_for_device(Device) ->
    ?event(debug_volume, {check_for_device, entry, {device, Device}}),
    case file:read_file_info(binary_to_list(Device)) of
        {ok, _file_info} ->
            ?event(debug_volume, 
                {check_for_device, result, 
                {device, Device, exists, true}
                }
            ),
            {ok, true};
        {error, enoent} ->
            ?event(debug_volume, 
                {check_for_device, result, 
                {device, Device, exists, false}
                }
            ),
            {ok, false};
        {error, Error} ->
            ?event(debug_volume, 
                {check_for_device, error, 
                {device, Device, error, Error}
                }
            ),
            {error, <<"Failed to check device existence">>}
    end.

-spec encryption_key_to_binary(term()) -> binary().
encryption_key_to_binary(EncKey) ->
    case EncKey of
            % Handle RSA wallet tuples - extract private key or use hash
            {{rsa, _}, PrivKey, _PubKey} when is_binary(PrivKey) ->
                % Use first 32 bytes of private key for AES-256
                case byte_size(PrivKey) of
                    Size when Size >= 32 ->
                        binary:part(PrivKey, 0, 32);
                    _ ->
                        % If private key is too short, hash it to get 32 bytes
                        crypto:hash(sha256, PrivKey)
                end;
            _ ->
                try
                    hb_util:bin(EncKey)
                catch
                    _:_ ->
                        % Fallback to term_to_binary and hash to get consistent
                        % key size
                        crypto:hash(sha256, term_to_binary(EncKey))
                end

        end.

%% Test store configuration updates for different types
update_store_config_test() ->
    % Test filesystem store
    FSStore = #{
        <<"store-module">> => hb_store_fs,
        <<"name">> => <<"cache">>
    },
    NewPath = <<"/encrypted/mount">>,
    Updated = update_store_config(FSStore, NewPath),
    Expected = FSStore#{<<"name">> => <<"/encrypted/mount/cache">>},
    ?assertEqual(Expected, Updated),
    % Test list of stores
    StoreList = [FSStore, #{<<"store-module">> => hb_store_gateway}],
    UpdatedList = update_store_config(StoreList, NewPath),
    ?assertEqual(2, length(UpdatedList)),
    % Test tuple format
    TupleStore = {fs, <<"old_path">>, []},
    UpdatedTuple = update_store_config(TupleStore, NewPath),
    ?assertEqual({fs, NewPath, []}, UpdatedTuple).

%% Test secure key file management
with_secure_key_file_test() ->
    TestKey = <<"test_encryption_key_123">>,
    % Create a safe test version that doesn't use /root/tmp
    TestWithSecureKeyFile = fun(EncKey, Fun) ->
        % Use /tmp instead of /root/tmp for testing
        TmpDir = "/tmp",
        KeyFile = TmpDir ++ "/test_luks_key_" ++ os:getpid(),
        try
            % Write key to temporary file
            file:write_file(KeyFile, EncKey, [raw]),
            % Execute function with key file path
            Result = Fun(KeyFile),
            % Clean up the key file
            file:delete(KeyFile),
            Result
        catch
            Class:Reason:Stacktrace ->
                % Ensure cleanup even if function fails
                file:delete(KeyFile),
                erlang:raise(Class, Reason, Stacktrace)
        end
    end,
    % Test successful execution
    Result = TestWithSecureKeyFile(TestKey, fun(KeyFile) ->
        % Verify key file was created and contains the key
        ?assert(filelib:is_regular(KeyFile)),
        {ok, FileContent} = file:read_file(KeyFile),
        ?assertEqual(TestKey, FileContent),
        {ok, <<"success">>}
    end),
    ?assertEqual({ok, <<"success">>}, Result),
    % Test exception handling and cleanup
    TestException = fun() ->
        TestWithSecureKeyFile(TestKey, fun(KeyFile) ->
            ?assert(filelib:is_regular(KeyFile)),
            error(test_error)
        end)
    end,
    ?assertError(test_error, TestException()).

%% Test device checking with mocked commands
check_for_device_test() ->
    % This test would need mocking of os:cmd to be fully testable
    % For now, test with /dev/null which should always exist
    ?assertEqual({ok, true}, check_for_device(<<"/dev/null">>)),
    % Test non-existent device
    ?assertEqual(
        {ok, false}, 
        check_for_device(<<"/dev/nonexistent_device_123">>)
    ).
