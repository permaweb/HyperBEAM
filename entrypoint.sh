#!/usr/bin/env bash
set -euo pipefail
BUILT_PROFILES="${PROFILES//,/+}"
exec "/app/_build/${BUILT_PROFILES}/rel/hb/bin/hb" "$@"
