-module(hb_volume).
-moduledoc """
Module for managing physical disks and volumes, providing operations
for partitioning, formatting, mounting, and managing encrypted volumes.
""".
-export([list_partitions/0, create_partition/2]).
-export([format_disk/2, mount_disk/4, change_node_store/2]).
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
    case os:cmd("sudo fdisk -l") of
        [] ->
            % Empty output indicates an error
            Reason = <<"Failed to list partitions: no output">>,
            ?event(debug_volume, {list_partitions, fdisk_error, no_output}),
            {error, Reason};
        Output ->
            ?event(debug_volume, {list_partitions, fdisk_success, parsing}),

            % Split output into lines
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
    DiskInfo = #{<<"device">> => Device},
    % Process each line to extract information
    lists:foldl(
        fun parse_disk_line/2,
        DiskInfo,
        Lines
    ).

% Parse a single line of disk information
parse_disk_line(Line, Info) ->
    % Extract disk size and bytes
    SizePattern = "^Disk .+: ([0-9.]+ [KMGT]iB), ([0-9]+) bytes, ([0-9]+) sectors",
    case re:run(Line, SizePattern, [{capture, [1, 2, 3], binary}]) of
        {match, [Size, Bytes, Sectors]} ->
            Info#{
                <<"size">> => Size,
                <<"bytes">> => binary_to_integer(Bytes),
                <<"sectors">> => binary_to_integer(Sectors)
            };
        _ -> 
            parse_disk_model_line(Line, Info)
    end.

% Parse disk model information
parse_disk_model_line(Line, Info) ->
    % Extract disk model
    ModelPattern = "^Disk model: (.+)\\s*$",
    case re:run(Line, ModelPattern, [{capture, [1], binary}]) of
        {match, [Model]} ->
            Info#{<<"model">> => string:trim(Model)};
        _ ->
            parse_disk_units_line(Line, Info)
    end.

% Parse disk units information
parse_disk_units_line(Line, Info) ->
    % Extract units information
    UnitsPattern = "^Units: (.+)$",
    case re:run(Line, UnitsPattern, [{capture, [1], binary}]) of
        {match, [Units]} ->
            Info#{<<"units">> => Units};
        _ ->
            parse_sector_size_line(Line, Info)
    end.

% Parse sector size information
parse_sector_size_line(Line, Info) ->
    % Extract sector size
    SectorPattern = "^Sector size \\(logical/physical\\): ([^/]+)/(.+)$",
    case re:run(Line, SectorPattern, [{capture, [1, 2], binary}]) of
        {match, [LogicalSize, PhysicalSize]} ->
            Info#{
                <<"sector_size">> => #{
                    <<"logical">> => string:trim(LogicalSize),
                    <<"physical">> => string:trim(PhysicalSize)
                }
            };
        _ ->
            parse_io_size_line(Line, Info)
    end.

% Parse I/O size information
parse_io_size_line(Line, Info) ->
    % Extract I/O size
    IOPattern = "^I/O size \\(minimum/optimal\\): ([^/]+)/(.+)$",
    case re:run(Line, IOPattern, [{capture, [1, 2], binary}]) of
        {match, [MinSize, OptSize]} ->
            Info#{
                <<"io_size">> => #{
                    <<"minimum">> => string:trim(MinSize),
                    <<"optimal">> => string:trim(OptSize)
                }
            };
        _ -> 
            Info
    end.

-doc """
Create a partition on a disk device.
@param Device The path to the device, e.g. "/dev/sdb".
@param PartType The partition type to create, defaults to "ext4".
@returns {ok, Map} on success where Map includes status and partition 
         information, or {error, Reason} if the operation fails.
""".
-spec create_partition(Device :: binary(), PartType :: binary()) ->
    {ok, map()} | {error, binary()}.
create_partition(undefined, _PartType) ->
    ?event(debug_volume, {create_partition, error, device_undefined}),
    {error, <<"Device path not specified">>};
create_partition(Device, PartType) ->
    ?event(debug_volume, 
        {create_partition, entry, 
           {device, Device, part_type, PartType}
        }
    ),
    % Create a GPT partition table
    DeviceStr = binary_to_list(Device),
    MklabelCmd = "sudo parted " ++ DeviceStr ++ " mklabel gpt",
    ?event(debug_volume, 
        {create_partition, creating_gpt_label, 
            {device, Device}
        }
    ),
    ?event(debug_volume, 
        {create_partition, executing_mklabel, 
            {command, MklabelCmd}
        }
    ),
    case safe_exec(MklabelCmd) of
        {ok, Result} ->
            ?event(debug_volume, 
                {create_partition, gpt_label_success, 
                    {result, Result}
                }
            ),
            create_actual_partition(Device, PartType);
        {error, ErrorMsg} ->
            ?event(debug_volume, 
                {create_partition, gpt_label_error, 
                    {error, ErrorMsg}
                }
            ),
            {error, ErrorMsg}
    end.

% Create the actual partition after making the GPT label
create_actual_partition(Device, PartType) ->
    ?event(debug_volume, 
        {create_actual_partition, entry, 
           {device, Device, part_type, PartType}
        }
    ),
    DeviceStr = binary_to_list(Device),
    PartTypeStr = binary_to_list(PartType),
    % Build the parted command to create the partition
    MkpartCmd = 
        "sudo parted -a optimal " ++ DeviceStr ++ 
        " mkpart primary " ++ PartTypeStr ++ " 0% 100%",
    ?event(debug_volume, 
        {create_actual_partition, executing_mkpart, 
           {command, MkpartCmd}
        }
    ),
    case safe_exec(MkpartCmd) of
        {ok, Result} ->
            ?event(debug_volume, 
                {create_actual_partition, mkpart_success, 
                   {result, Result}
                }
            ),
            get_partition_info(Device);
        {error, ErrorMsg} ->
            ?event(debug_volume, 
                {create_actual_partition, mkpart_error, 
                    {error, ErrorMsg}
                }
            ),
            {error, ErrorMsg}
    end.

% Get the partition information after creating a partition
get_partition_info(Device) ->
    ?event(debug_volume, {get_partition_info, entry, {device, Device}}),
    DeviceStr = binary_to_list(Device),
    % Print partition information
    PrintCmd = "sudo parted " ++ DeviceStr ++ " print",
    ?event(debug_volume, 
        {get_partition_info, executing_print, {command, PrintCmd}}
    ),
    PartitionInfo = os:cmd(PrintCmd),
    ?event(debug_volume, 
        {get_partition_info, success, partition_created, 
            {result, PartitionInfo}
        }
    ),
    {ok, #{
        <<"status">> => 200,
        <<"message">> => <<"Partition created successfully.">>,
        <<"device_path">> => Device,
        <<"partition_info">> => list_to_binary(PartitionInfo)
    }}.

-doc """
Format a disk or partition with LUKS encryption.
@param Partition The path to the partition, e.g. "/dev/sdc1".
@param EncKey The encryption key to use for LUKS.
@returns {ok, Map} on success where Map includes the status and 
    confirmation message, or {error, Reason} if the operation fails.
""".
-spec format_disk(Partition :: binary(), EncKey :: binary()) ->
    {ok, map()} | {error, binary()}.
format_disk(undefined, _EncKey) ->
    ?event(debug_volume, {format_disk, error, partition_undefined}),
    {error, <<"Partition path not specified">>};
format_disk(_Partition, undefined) ->
    ?event(debug_volume, {format_disk, error, key_undefined}),
    {error, <<"Encryption key not specified">>};
format_disk(Partition, EncKey) ->
    ?event(debug_volume, 
        {format_disk, entry, 
            {
                partition, Partition, 
                key_present, true
            }
        }
    ),
    PartitionStr = binary_to_list(Partition),
    ?event(debug_volume, {format_disk, creating_secure_key_file, starting}),
    with_secure_key_file(EncKey, fun(KeyFile) ->
        FormatCmd = 
            "sudo cryptsetup luksFormat --batch-mode " ++
            "--key-file " ++ KeyFile ++ " " ++ PartitionStr,
        ?event(debug_volume, 
            {format_disk, executing_luks_format, {command, FormatCmd}}
        ),
        case safe_exec(FormatCmd, ["failed"]) of
            {ok, Result} ->
                ?event(debug_volume, 
                    {format_disk, luks_format_success, completed, 
                        {result, Result}
                    }
                ),
                {ok, #{
                    <<"status">> => 200,
                    <<"message">> => 
                        <<"Partition formatted with LUKS encryption "
                          "successfully.">>
                }};
            {error, ErrorMsg} ->
                ?event(debug_volume, 
                    {format_disk, luks_format_error, ErrorMsg}
                ),
                {error, ErrorMsg}
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
            "sudo cryptsetup luksOpen --key-file " ++ KeyFile ++ 
            " " ++ PartitionStr ++ " " ++ VolumeNameStr,
        ?event(debug_volume, {mount_disk, executing_luks_open, {command, OpenCmd}}),
        case safe_exec(OpenCmd, ["failed"]) of
            {ok, Result} ->
                ?event(debug_volume, 
                    {mount_disk, luks_open_success, proceeding_to_mount, 
                        {result, Result}
                    }
                ),
                mount_opened_volume(Partition, MountPoint, VolumeName);
            {error, ErrorMsg} ->
                ?event(debug_volume, {mount_disk, luks_open_error, ErrorMsg}),
                {error, ErrorMsg}
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
    % Create mount point if it doesn't exist
    MountPointStr = binary_to_list(MountPoint),
    ?event(debug_volume, 
        {mount_opened_volume, creating_mount_point, MountPoint}
    ),
    os:cmd("sudo mkdir -p " ++ MountPointStr),
    % Check if filesystem exists on the opened LUKS volume
    VolumeNameStr = binary_to_list(VolumeName),
    DeviceMapperPath = "/dev/mapper/" ++ VolumeNameStr,
    % Check filesystem type
    FSCheckCmd = "sudo blkid " ++ DeviceMapperPath,
    ?event(debug_volume, 
        {mount_opened_volume, checking_filesystem, {command, FSCheckCmd}}
    ),
    FSCheckResult = os:cmd(FSCheckCmd),
    ?event(debug_volume, 
        {mount_opened_volume, filesystem_check_result, FSCheckResult}
    ),
    % Create filesystem if none exists
    case string:find(FSCheckResult, "TYPE=") of
        nomatch ->
            % No filesystem found, create ext4
            ?event(debug_volume, 
                {mount_opened_volume, creating_filesystem, ext4}
            ),
            MkfsCmd = "sudo mkfs.ext4 -F " ++ DeviceMapperPath,
            ?event(debug_volume, 
                {mount_opened_volume, executing_mkfs, {command, MkfsCmd}}
            ),
            MkfsResult = os:cmd(MkfsCmd),
            ?event(debug_volume, 
                {mount_opened_volume, mkfs_result, MkfsResult}
            );
        _ ->
            ?event(debug_volume, 
                {mount_opened_volume, filesystem_exists, skipping_creation}
            )
    end,
    % Mount the unlocked LUKS volume
    MountCmd = "sudo mount " ++ DeviceMapperPath ++ " " ++ MountPointStr,
    ?event(debug_volume, 
        {mount_opened_volume, executing_mount, 
           {command, MountCmd}
        }
    ),
    case safe_exec(MountCmd, ["failed"]) of
        {ok, Result} ->
            ?event(debug_volume, 
                {mount_opened_volume, mount_success, 
                   creating_info, {result, Result}
                }
            ),
            create_mount_info(Partition, MountPoint, VolumeName);
        {error, ErrorMsg} ->
            ?event(debug_volume, 
                {mount_opened_volume, mount_error, 
                   {error, ErrorMsg, closing_luks}
                }
            ),
            % Close the LUKS volume if mounting failed
            os:cmd("sudo cryptsetup luksClose " ++ VolumeNameStr),
            {error, ErrorMsg}
    end.

% Create mount info response
create_mount_info(Partition, MountPoint, VolumeName) ->
    ?event(debug_volume, 
        {create_mount_info, success, 
            {
                partition, Partition, 
                mount_point, MountPoint, 
                volume_name, VolumeName
            }
        }
    ),
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
    }}.

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
    os:cmd("sudo mkdir -p " ++ StorePathStr),
    % Update the store configuration with the new path
    ?event(debug_volume, 
        {change_node_store, updating_config, 
           {current_store, CurrentStore}
        }
    ),
    NewStore = update_store_config(CurrentStore, StorePath),
    % Return the result
    ?event(debug_volume,
        {change_node_store, success, {new_store_config, NewStore}}
    ),
    {ok, #{
        <<"status">> => 200,
        <<"message">> => 
            <<"Node store updated to use encrypted disk.">>,
        <<"store_path">> => StorePath,
        <<"store">> => NewStore
    }}.

%%% Helper functions
%% Execute system command with error checking
safe_exec(Command) ->
    safe_exec(Command, ["Error", "failed", "bad", "error"]).

safe_exec(Command, ErrorKeywords) ->
    Result = os:cmd(Command),
    case check_command_errors(Result, ErrorKeywords) of
        ok -> {ok, Result};
        error -> {error, list_to_binary(Result)}
    end.

%% Check if command result contains error indicators
check_command_errors(Result, Keywords) ->
    case lists:any(fun(Keyword) -> 
        string:find(Result, Keyword) =/= nomatch 
    end, Keywords) of
        true -> error;
        false -> ok
    end.

%% Secure key file management with automatic cleanup
with_secure_key_file(EncKey, Fun) ->
    ?event(debug_volume, {with_secure_key_file, entry, creating_temp_file}),
    os:cmd("sudo mkdir -p /root/tmp"),
    % Get process ID and create filename
    PID = os:getpid(),
    ?event(debug_volume, {with_secure_key_file, process_id, PID}),
    KeyFile = "/root/tmp/luks_key_" ++ PID,
    ?event(debug_volume, {with_secure_key_file, key_file_path, KeyFile}),
    % Check if directory was created successfully
    DirCheck = os:cmd("ls -la /root/tmp/"),
    ?event(debug_volume, {with_secure_key_file, directory_check, DirCheck}),
    try
        % Convert EncKey to binary using hb_util
        BinaryEncKey = case EncKey of
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
            % Handle other complex terms
            _ when not is_binary(EncKey) andalso not is_list(EncKey) ->
                try
                    hb_util:bin(EncKey)
                catch
                    _:_ ->
                        % Fallback to term_to_binary and hash to get consistent
                        % key size
                        crypto:hash(sha256, term_to_binary(EncKey))
                end;
            % Simple cases handled by hb_util:bin
            _ ->
                hb_util:bin(EncKey)
        end,
        WriteResult = file:write_file(KeyFile, BinaryEncKey, [raw]),
        ?event(debug_volume, 
            {with_secure_key_file, write_result, WriteResult}
        ),
        % Check if file was created
        FileExists = filelib:is_regular(KeyFile),
        ?event(debug_volume, 
            {with_secure_key_file, file_exists_check, FileExists}
        ),
        % If file exists, get its info
        case FileExists of
            true ->
                FileInfo = file:read_file_info(KeyFile),
                ?event(debug_volume, 
                    {with_secure_key_file, file_info, FileInfo}
                );
            false ->
                ?event(debug_volume, 
                    {with_secure_key_file, file_not_found, KeyFile}
                )
        end,
        % Execute function with key file path
        ?event(debug_volume, 
            {with_secure_key_file, executing_function, with_key_file}
        ),
        Result = Fun(KeyFile),
        % Always clean up the key file
        ?event(debug_volume, 
            {with_secure_key_file, cleanup, shredding_key_file}
        ),
        os:cmd("sudo shred -u " ++ KeyFile),
        ?event(debug_volume, {with_secure_key_file, success, completed}),
        Result
    catch
        Class:Reason:Stacktrace ->
            ?event(debug_volume, 
                {with_secure_key_file, exception, 
                   {class, Class, reason, Reason, cleanup, starting}
                }
            ),
            % Ensure cleanup even if function fails
            os:cmd("sudo shred -u " ++ KeyFile),
            ?event(debug_volume, 
                {with_secure_key_file, exception_cleanup, completed}
            ),
            erlang:raise(Class, Reason, Stacktrace)
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

-doc """
Check if a device exists on the system.
@param Device The path to the device to check (binary).
@returns true if the device exists, false otherwise.
""".
-spec check_for_device(Device :: binary()) -> boolean().
check_for_device(Device) ->
    ?event(debug_volume, {check_for_device, entry, {device, Device}}),
    Command = 
        io_lib:format(
            "ls -l ~s 2>/dev/null || echo 'not_found'", 
            [binary_to_list(Device)]
        ),
    ?event(debug_volume, {check_for_device, executing_command, ls_check}),
    Result = os:cmd(Command),
    DeviceExists = string:find(Result, "not_found") =:= nomatch,
    ?event(debug_volume, 
        {check_for_device, result, 
           {device, Device, exists, DeviceExists}
        }
    ),
    DeviceExists.

%%% Unit Tests
%% Test helper function error checking
check_command_errors_test() ->
    % Test successful case - no errors
    ?assertEqual(
        ok, 
        check_command_errors(
            "Success: operation completed", 
            ["Error", "failed"]
        )
    ),
    % Test error detection
    ?assertEqual(
        error, 
        check_command_errors(
            "Error: something went wrong", 
            ["Error", "failed"]
        )
    ),
    ?assertEqual(
        error,
        check_command_errors(
            "Operation failed", 
            ["Error", "failed"]
        )
    ),
    % Test case sensitivity
    ?assertEqual(
        ok, 
        check_command_errors(
            "error (lowercase)", 
            ["Error", "failed"]
        )
    ),
    % Test multiple keywords
    ?assertEqual(
        error, 
        check_command_errors(
            "Command failed with Error", 
            ["Error", "failed"]
        )
    ).

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
    ?assertEqual(true, check_for_device(<<"/dev/null">>)),
    % Test non-existent device
    ?assertEqual(
        false, 
        check_for_device(<<"/dev/nonexistent_device_123">>)
    ).

%% Test safe command execution with mocked results
safe_exec_mock_test() ->
    % We can't easily mock os:cmd, but we can test the error checking logic
    % This is covered by check_command_errors_test above
    % Test with default error keywords
    TestResult1 = 
        check_command_errors(
            "Operation completed successfully", 
            ["Error", "failed"]
        ),
    ?assertEqual(ok, TestResult1),
    TestResult2 = 
        check_command_errors(
            "Error: disk not found", 
            ["Error", "failed"]
        ),
    ?assertEqual(error, TestResult2). 