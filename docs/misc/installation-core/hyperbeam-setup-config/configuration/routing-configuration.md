# HyperBEAM Routing Configuration

This document explains how to configure routing in HyperBEAM.

## Routing System Overview

HyperBEAM's routing system directs incoming requests to appropriate destinations based on path patterns. This allows you to route specific request patterns to different servers.

## Routes Configuration Structure

The `routes` configuration option accepts a list of route definitions. Each route is a map with the following components:

| Component | Type | Description |
|-----------|------|-------------|
| `template` | Binary | Path pattern to match against incoming requests |
| `node` | Map | Single destination configuration |
| `nodes` | List | List of alternative destinations for load balancing |

## Configuring Routes

Due to the complex nature of routing configuration, you **must** use the `hb:start_mainnet/1` approach rather than config.flat.

### Default Routes Example

You can define multiple routes in order of priority:

```bash
rebar3 shell --eval "
  hb:start_mainnet(#{
	routes => [
		#{
			<<\"template\">> => <<\"/result/.*\">>,
			<<\"node\">> => #{ <<\"prefix\">> => <<\"http://localhost:6363\">> }
		},
		#{
			<<\"template\">> => <<\"/graphql\">>,
			<<\"nodes\">> =>
				[
					#{
						<<\"prefix\">> => <<\"https://arweave-search.goldsky.com\">>,
						<<\"opts\">> => #{ http_client => httpc }
					},
					#{
						<<\"prefix\">> => <<\"https://arweave.net\">>,
						<<\"opts\">> => #{ http_client => gun }
					}
				]
		},
		#{
			<<\"template\">> => <<\"/raw\">>,
			<<\"node\">> =>
				#{
					<<\"prefix\">> => <<\"https://arweave.net\">>,
					<<\"opts\">> => #{ http_client => gun }
				}
		}
	]
  }).
"
```

## Route Order Importance

Routes are evaluated in the order they appear in the configuration. When multiple routes could match a request, the first matching route in the list is used. Place more specific routes before general ones.
