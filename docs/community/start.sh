#!/bin/bash
set -e

HB_PORT="${HB_PORT:-8001}"
AUTO_INDEX="${AUTO_INDEX:-true}"
CONFIG="${HB_CONFIG:-config.json}"

if [ ! -f "$CONFIG" ]; then
    echo "Config not found: $CONFIG"
    echo "Create one using the template in README.md or set HB_CONFIG."
    exit 1
fi

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

HB_CONFIG="$CONFIG" rebar3 shell
