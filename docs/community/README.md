# Community Node Setup

Set up a HyperBEAM node that indexes and serves Arweave data through your own
Arweave node.

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

Other environment variables:
- `HB_PORT`: HyperBEAM listen port (default: `8001`)
- `DATA_DIR`: LMDB path inside the container (default: `/data/rolling`)

Check logs:

```bash
docker compose logs -f
```

Stop:

```bash
docker compose down
```

## What the config does

- `routes` points all Arweave traffic at your node. The default routes point
  at a set of bootstrap nodes — setting `routes` in the config replaces those
  entirely.
- `store` defines a single `hb_store_arweave` backed by a rolling LMDB. This
  is where copycat writes its index data.
- `arweave_index_blocks: false` skips caching block headers locally.
- `arweave_index_workers: 16` controls how many concurrent workers process
  transactions during indexing.
