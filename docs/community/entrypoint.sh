#!/bin/bash
set -e

ARWEAVE_NODE="${ARWEAVE_NODE:-http://localhost:1984}"
HB_PORT="${HB_PORT:-8001}"
DATA_DIR="${DATA_DIR:-/data/rolling}"
AUTO_INDEX="${AUTO_INDEX:-true}"

cat > /app/config.json <<EOF
{
  "ao-types": "generate_index=atom,max_connections=integer,num_acceptors=integer",
  "port": ${HB_PORT},
  "num_acceptors": 32,
  "max_connections": 512,
  "arweave_index_workers": 16,
  "arweave_index_blocks": false,
  "routes": [
    {
      "template": "^/arweave",
      "node": {
        "match": "^/arweave",
        "with": "${ARWEAVE_NODE}"
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
          "name": "${DATA_DIR}",
          "access": ["read", "write"],
          "max-readers": 512,
          "capacity": 68719476736
        }
      ]
    }
  ]
}
EOF

start_cron() {
    until curl -s -o /dev/null -w "%{http_code}" "http://localhost:${HB_PORT}/~meta@1.0/info" 2>/dev/null | grep -q 200; do
        sleep 2
    done
    curl -s "http://localhost:${HB_PORT}/~cron@1.0/every?interval=1-second&cron-path=~copycat@1.0/arweave&from=-1&to=-19" > /dev/null
    echo "Continuous indexing started."
}

if [ "$AUTO_INDEX" = "true" ]; then
    start_cron &
fi

export HB_CONFIG=/app/config.json
exec /app/hb/bin/hb foreground
