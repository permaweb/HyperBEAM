#!/usr/bin/env bash
set -euo pipefail

export NODE_A_PORT="${NODE_A_PORT:-18734}"
export NODE_B_PORT="${NODE_B_PORT:-18735}"
export NODE_A_CACHE="${NODE_A_CACHE:-_build/odysee-node-a-cache}"
export NODE_B_CACHE="${NODE_B_CACHE:-_build/odysee-node-b-cache}"
export HB_PORT="${HB_DEMO_SUPERVISOR_PORT:-0}"

export PATH="/opt/homebrew/opt/rust/bin:/opt/homebrew/opt/erlang@27/bin:/opt/homebrew/bin:${PATH}"

port_in_use() {
  lsof -nP -iTCP:"$1" -sTCP:LISTEN >/dev/null 2>&1
}

if port_in_use "${NODE_A_PORT}" || port_in_use "${NODE_B_PORT}"; then
  echo "Odysee two-node demo port conflict."
  echo "  NODE_A_PORT=${NODE_A_PORT} in use: $(port_in_use "${NODE_A_PORT}" && echo yes || echo no)"
  echo "  NODE_B_PORT=${NODE_B_PORT} in use: $(port_in_use "${NODE_B_PORT}" && echo yes || echo no)"
  echo
  echo "An earlier demo may already be running. Use the existing nodes, stop them, or override ports:"
  echo "  NODE_A_PORT=19734 NODE_B_PORT=19735 ./scripts/odysee-two-node-demo.sh"
  exit 1
fi

ERL=$(cat <<'ERL'
application:ensure_all_started(hb),

NodeAPort = list_to_integer(os:getenv("NODE_A_PORT")),
NodeBPort = list_to_integer(os:getenv("NODE_B_PORT")),
NodeACache = unicode:characters_to_binary(os:getenv("NODE_A_CACHE")),
NodeBCache = unicode:characters_to_binary(os:getenv("NODE_B_CACHE")),

NodeBLocalStore = #{
    <<"store-module">> => hb_store_fs,
    <<"name">> => NodeBCache
},
NodeBStore = [
    NodeBLocalStore,
    #{
        <<"store-module">> => hb_store_lbry_stream_descriptor
    },
    #{
        <<"store-module">> => hb_store_lbry_blob
    },
    #{
        <<"store-module">> => hb_store_lbry_transaction
    },
    #{
        <<"store-module">> => hb_store_lbry_claim_output,
        <<"walk-ancestry">> => true
    },
    #{
        <<"store-module">> => hb_store_odysee
    }
],
NodeB = hb_http_server:start_node(#{
    <<"port">> => NodeBPort,
    <<"store">> => NodeBStore,
    <<"force-signed">> => false
}),

NodeALocalStore = #{
    <<"store-module">> => hb_store_fs,
    <<"name">> => NodeACache
},
NodeARemoteStore = #{
    <<"store-module">> => hb_store_remote_node,
    <<"node">> => NodeB,
    <<"require-codec">> => <<"ans104@1.0">>,
    <<"verify-remote-read">> => true,
    <<"local-store">> => [NodeALocalStore]
},
NodeA = hb_http_server:start_node(#{
    <<"port">> => NodeAPort,
    <<"store">> => [NodeALocalStore, NodeARemoteStore],
    <<"force-signed">> => false
}),

io:format("~nOdysee two-node demo is running.~n", []),
io:format("  Node A edge/cache: ~s~n", [NodeA]),
io:format("  Node B odysee source: ~s~n", [NodeB]),
io:format("  Verification: Node A verifies Node B native commitments before caching.~n", []),
io:format("~nFrontend env:~n", []),
io:format("  HYPERBEAM_BASE_URL=~s~n", [NodeA]),
io:format("  HYPERBEAM_PLAYBACK_URL=~s~s~n", [NodeA, <<"~odysee-stream@1.0/playback?mode=bytes">>]),
io:format("~nRemote-store check:~n", []),
io:format("  curl -sS -H 'accept: application/json' '~s~s?read=odysee/stream-id/346c1fed0fbc2f0b3ecc8bf3915aa8aaa029c169' | jq .~n", [NodeA, <<"~cache@1.0/read">>]),
io:format("~nStore-native direct ID reads:~n", []),
io:format("  curl -sS -H 'accept: application/json' '~s<descriptor-sd-hash>' | jq .~n", [NodeA]),
io:format("  curl -sS -H 'accept: application/json' '~s<blob-sha384>' | jq .~n", [NodeA]),
io:format("  curl -sS -H 'accept: application/json' '~s<txid>' | jq .~n", [NodeA]),
io:format("  curl -sS -H 'accept: application/json' '~s<txid>:<nout>' | jq .~n", [NodeA]),
io:format("~nOdysee surface read examples:~n", []),
io:format("  curl -sS -H 'accept: application/json' '~s~s?id=odysee/comment/<comment-id>' | jq .~n", [NodeA, <<"~odysee@1.0/source">>]),
io:format("  curl -sS -H 'accept: application/json' '~s~s?id=<comment-id>&kind=comment' | jq .~n~n", [NodeA, <<"~odysee@1.0/source">>]).
ERL
)

rebar3 shell --eval "${ERL}"
