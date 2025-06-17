# HyperBEAM Configuration Examples

This document provides complete, practical configuration examples for common HyperBEAM deployment scenarios. Each example includes explanations and can be used as a starting point for your own configuration.

## Basic Development Configuration

This is a simple configuration for local development with debugging enabled:

### Simple Options in config.flat

For basic options, you can use a config.flat file:

```
port: 10000
mode: debug
priv_key_location: ./wallet.json

```

### Complete Configuration Using start_mainnet

For a complete configuration including storage:

```bash
rebar3 shell --eval "
  hb:start_mainnet(#{
    port => 10001,
    mode => debug,
    priv_key_location => <<\"./wallet.json\">>,
    
	http_extra_opts =>
		#{
			force_message => true,
			store => [{hb_store_fs, #{ prefix => \"local-cache\" }}, {hb_store_gateway, #{}}],
			cache_control => [<<\"always\">>]
		}
  }).
"
```

**Key features**:

- Development mode enabled
- Simple file system storage
- Extensive debugging options
- Local port 10001

!!! Note
    More examples to come