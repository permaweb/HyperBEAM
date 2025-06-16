# [Module hb_volume.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_volume.erl)




<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#change_node_store-2">change_node_store/2</a></td><td></td></tr><tr><td valign="top"><a href="#check_for_device-1">check_for_device/1</a></td><td></td></tr><tr><td valign="top"><a href="#create_actual_partition-2">create_actual_partition/2*</a></td><td></td></tr><tr><td valign="top"><a href="#create_mount_info-3">create_mount_info/3*</a></td><td></td></tr><tr><td valign="top"><a href="#create_partition-2">create_partition/2</a></td><td></td></tr><tr><td valign="top"><a href="#format_disk-2">format_disk/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_partition_info-1">get_partition_info/1*</a></td><td></td></tr><tr><td valign="top"><a href="#list_partitions-0">list_partitions/0</a></td><td></td></tr><tr><td valign="top"><a href="#mount_disk-4">mount_disk/4</a></td><td></td></tr><tr><td valign="top"><a href="#mount_opened_volume-3">mount_opened_volume/3*</a></td><td></td></tr><tr><td valign="top"><a href="#parse_disk_info-2">parse_disk_info/2*</a></td><td></td></tr><tr><td valign="top"><a href="#parse_disk_line-2">parse_disk_line/2*</a></td><td></td></tr><tr><td valign="top"><a href="#parse_disk_model_line-2">parse_disk_model_line/2*</a></td><td></td></tr><tr><td valign="top"><a href="#parse_disk_units_line-2">parse_disk_units_line/2*</a></td><td></td></tr><tr><td valign="top"><a href="#parse_io_size_line-2">parse_io_size_line/2*</a></td><td></td></tr><tr><td valign="top"><a href="#parse_sector_size_line-2">parse_sector_size_line/2*</a></td><td></td></tr><tr><td valign="top"><a href="#process_disk_line-2">process_disk_line/2*</a></td><td></td></tr><tr><td valign="top"><a href="#update_store_config-2">update_store_config/2*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="change_node_store-2"></a>

### change_node_store/2 ###

<pre><code>
change_node_store(StorePath::binary(), CurrentStore::list()) -&gt; {ok, map()} | {error, binary()}
</code></pre>
<br />

<a name="check_for_device-1"></a>

### check_for_device/1 ###

<pre><code>
check_for_device(Device::binary()) -&gt; boolean()
</code></pre>
<br />

<a name="create_actual_partition-2"></a>

### create_actual_partition/2 * ###

`create_actual_partition(Device, PartType) -> any()`

<a name="create_mount_info-3"></a>

### create_mount_info/3 * ###

`create_mount_info(Partition, MountPoint, VolumeName) -> any()`

<a name="create_partition-2"></a>

### create_partition/2 ###

<pre><code>
create_partition(Device::binary(), PartType::binary()) -&gt; {ok, map()} | {error, binary()}
</code></pre>
<br />

<a name="format_disk-2"></a>

### format_disk/2 ###

<pre><code>
format_disk(Partition::binary(), EncKey::binary()) -&gt; {ok, map()} | {error, binary()}
</code></pre>
<br />

<a name="get_partition_info-1"></a>

### get_partition_info/1 * ###

`get_partition_info(Device) -> any()`

<a name="list_partitions-0"></a>

### list_partitions/0 ###

<pre><code>
list_partitions() -&gt; {ok, map()} | {error, binary()}
</code></pre>
<br />

<a name="mount_disk-4"></a>

### mount_disk/4 ###

<pre><code>
mount_disk(Partition::binary(), EncKey::binary(), MountPoint::binary(), VolumeName::binary()) -&gt; {ok, map()} | {error, binary()}
</code></pre>
<br />

<a name="mount_opened_volume-3"></a>

### mount_opened_volume/3 * ###

`mount_opened_volume(Partition, MountPoint, VolumeName) -> any()`

<a name="parse_disk_info-2"></a>

### parse_disk_info/2 * ###

`parse_disk_info(Device, Lines) -> any()`

<a name="parse_disk_line-2"></a>

### parse_disk_line/2 * ###

`parse_disk_line(Line, Info) -> any()`

<a name="parse_disk_model_line-2"></a>

### parse_disk_model_line/2 * ###

`parse_disk_model_line(Line, Info) -> any()`

<a name="parse_disk_units_line-2"></a>

### parse_disk_units_line/2 * ###

`parse_disk_units_line(Line, Info) -> any()`

<a name="parse_io_size_line-2"></a>

### parse_io_size_line/2 * ###

`parse_io_size_line(Line, Info) -> any()`

<a name="parse_sector_size_line-2"></a>

### parse_sector_size_line/2 * ###

`parse_sector_size_line(Line, Info) -> any()`

<a name="process_disk_line-2"></a>

### process_disk_line/2 * ###

`process_disk_line(Line, X2) -> any()`

<a name="update_store_config-2"></a>

### update_store_config/2 * ###

<pre><code>
update_store_config(StoreConfig::term(), NewPath::binary()) -&gt; term()
</code></pre>
<br />

