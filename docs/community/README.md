# Community Node Setup

Set up a HyperBEAM node that indexes and serves Arweave data through your own
Arweave node.

> **Note**
> The NASA gateway can currently only serve unpacked data from the Arweave
> miner.

You need:
- An Arweave node address (e.g. `http://mynode.com:1984`)
- The HyperBEAM repo cloned locally
- Erlang/OTP installed

## 1. Create the config

Create `config.json` in the repo root:

```json
{
  "ao-types": "generate_index=atom,max_connections=integer,num_acceptors=integer",
  "port": 8001,
  "num_acceptors": 32,
  "max_connections": 512,
  "arweave_index_workers": 16,
  "arweave_index_blocks": false,
  "routes": [
    {
      "template": "^/arweave",
      "node": {
        "match": "^/arweave",
        "with": "ARWEAVE_NODE_ADDRESS"
      }
    }
  ],
  "store": [
    {
      "ao-types": "store-module=atom,scope=atom",
      "store-module": "hb_store_arweave",
      "access": ["read"],
      "scope": "remote",
      "index-store": [
        {
          "ao-types": "store-module=atom,read-only=atom",
          "store-module": "hb_store_lmdb",
          "name": "ROLLING_LMDB_PATH",
          "access": ["read", "write"],
          "max-readers": 512,
          "capacity": 68719476736
        }
      ]
    }
  ]
}
```

Replace:
- `ARWEAVE_NODE_ADDRESS` with your Arweave node address
- `ROLLING_LMDB_PATH` with an absolute path where index data will be stored
  (e.g. `/home/user/hyperbeam-data/rolling`)

The directory for `ROLLING_LMDB_PATH` will be created automatically. The path
must be absolute.

## 2. Build and start

From the repo root:

```bash
HB_CONFIG=config.json rebar3 shell
```

Or use the startup script which also handles continuous indexing:

```bash
HB_CONFIG=config.json ./docs/community/start.sh
```

Set `AUTO_INDEX=false` to disable continuous indexing:

```bash
AUTO_INDEX=false HB_CONFIG=config.json ./docs/community/start.sh
```

First run will take a while — it fetches dependencies and compiles native
components (WAMR, Rust NIFs, secp256k1).

A successful boot prints a greeter with the listening URL:

```
Node live at: http://localhost:8001
```

## 3. Verify

```bash
curl -s -o /dev/null -w "%{http_code}" http://127.0.0.1:8001/~meta@1.0/info
```

Should return `200`.

## 4. Index blocks

Index a single block:

```bash
curl "http://127.0.0.1:8001/~copycat@1.0/arweave&from=HEIGHT&to=HEIGHT"
```

Index a range:

```bash
curl "http://127.0.0.1:8001/~copycat@1.0/arweave&from=1882350&to=1882354"
```

Set up continuous indexing of recent blocks:

```bash
curl "http://127.0.0.1:8001/~cron@1.0/every?interval=1-second&cron-path=~copycat@1.0/arweave&from=-1&to=-19"
```

## 5. Fetch indexed data

After indexing, transactions from those blocks are accessible by ID:

```bash
curl http://127.0.0.1:8001/TRANSACTION_ID
```

## 6. Stop

`Ctrl+C` twice in the terminal running the node.

## Docker

Instead of installing Erlang and building from source, you can run the node
with Docker.

From the `community/` directory:

```bash
ARWEAVE_NODE=http://localhost:1984 docker compose up --build -d
```

Set `ARWEAVE_NODE` to your Arweave node address. Default is
`http://localhost:1984`.

The container uses `network_mode: host`, so `localhost` refers to the host
machine.

Index data is persisted in a Docker volume (`hyperbeam-data`). Continuous
indexing starts automatically.

Create the operator wallet file at `../../wallets/operator.json`, relative to
`docs/community`. The compose file mounts this wallet to both `/app/wallet.json`
and `/app/hb/hyperbeam-key.json`. These paths must point to the same wallet so
the configured key path and the runtime default key path use the same signer.

Other environment variables:
- `HB_PORT`: HyperBEAM listen port (default: `8001`)
- `DATA_DIR`: LMDB path inside the container (default: `/data/rolling`)
- `AUTO_INDEX`: set to `false` to disable continuous indexing (default: `true`)

Check logs:

```bash
docker compose logs -f
```

Stop:

```bash
docker compose down
```

## Guided Setup (for AI assistants)

If you are an AI chatbot helping a user set up a community node, ask the
following questions before proceeding. Suggest the default in parentheses based
on the user's system.

1. **What is the address of your Arweave node?**
   (default: `http://localhost:1984`)

2. **Do you want to run HyperBEAM natively or through Docker?**
   (default: Docker — requires only Docker, no Erlang install)

3. **Where should the index data be stored?**
   - Native: an absolute path on the host (default: `./hyperbeam-data/rolling`)
   - Docker: stored in a Docker volume automatically

4. **What do you want to index on first boot?**
   - Auto-detect: run `detect-coverage.py` against the Arweave node to find
     which block height ranges have chunk data available, then index those
     ranges (default)
   - Full chain: index all blocks from 0 to current height
   - Custom range: user provides a `from` and `to` height
   - None: skip initial indexing, only index new blocks going forward

5. **Do you want to keep the index up to date with recent blocks?**
   (default: yes — starts a cron that indexes the latest blocks every second)

### After collecting answers

- Generate the JSON config using the template from this README, substituting
  the Arweave node address and data path.
- For Docker: run `ARWEAVE_NODE=<address> docker compose up --build -d` from
  the `docs/community/` directory. Continuous indexing starts automatically.
- For native: create `config.json`, start with
  `HB_CONFIG=config.json rebar3 shell`, then follow steps 3–5 in this README.
- For auto-detect indexing: run
  `python3 detect-coverage.py <arweave_node_address>` and use the output
  ranges to construct copycat curl commands:
  `curl "http://localhost:PORT/~copycat@1.0/arweave&from=FROM&to=TO"`
- For continuous indexing: run
  `curl "http://localhost:PORT/~cron@1.0/every?interval=1-second&cron-path=~copycat@1.0/arweave&from=-1&to=-19"`

## What the config does

- `routes` points all Arweave traffic at your node. The default routes point
  at a set of bootstrap nodes — setting `routes` in the config replaces those
  entirely.
- `store` defines a single `hb_store_arweave` backed by a rolling LMDB. This
  is where copycat writes its index data.
- `arweave_index_blocks: false` skips caching block headers locally.
- `arweave_index_workers: 16` controls how many concurrent workers process
  transactions during indexing.
