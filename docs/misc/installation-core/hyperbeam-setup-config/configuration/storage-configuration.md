# HyperBEAM Storage Configuration

This document provides a basic overview of storage configuration in HyperBEAM.

## Storage Backend Overview

HyperBEAM supports multiple storage backends that can be used individually or in combination. When multiple backends are specified, HyperBEAM tries each in sequence until the requested data is found.

## Configuring Storage Backends

Storage backends are configured through the `http_extra_opts.store` setting. Due to the complexity of storage configuration, you **must** use the `hb:start_mainnet/1` approach rather than config.flat.

## Basic Storage Configuration Examples

### Simple File System Storage

The most basic storage configuration uses the file system:

```bash
rebar3 shell --eval "
  hb:start_mainnet(#{
    http_extra_opts => #{
	  force_message => true,
	  store => {hb_store_fs, #{ prefix => \"local-storage\" }}
	  cache_control => [<<\"always\">>]
    }
  }).
"
```

This configuration stores data in a directory named "local-storage".

### Gateway Fallback Configuration

To use local storage with a gateway fallback:

```bash
rebar3 shell --eval "
  hb:start_mainnet(#{
    http_extra_opts => #{
	  force_message => true,
	  store => [{hb_store_fs, #{ prefix => \"mainnet-cache\" }}, {hb_store_gateway, #{}}],
	  cache_control => [<<\"always\">>]
    }
  }).
"
```

This configuration first looks for data in the local file system, then falls back to the Arweave gateway if not found locally.

## Available Storage Backends

HyperBEAM includes these storage backends:

1. **File System Store (hb_store_fs)** - Uses the local file system
2. **RocksDB Store (hb_store_rocksdb)** - Uses RocksDB for efficient key-value storage
3. **Gateway Store (hb_store_gateway)** - Reads data from the Arweave gateway
4. **Remote Node Store (hb_store_remote_node)** - Reads data from another HyperBEAM node
