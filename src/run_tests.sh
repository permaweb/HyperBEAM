#!/usr/bin/env bash
set -euo pipefail

DIR="src/"
DEFAULT_EXCLUDE="ar_deep_hash,ar_format,ar_rate_limiter,ar_timestamp,ar_wallet,dev_arweave_block_cache,dev_arweave_common,dev_cacheviz,dev_codec_ans104_from,dev_codec_ans104_to,dev_codec_cookie,dev_codec_httpsig_keyid,dev_codec_httpsig_proxy,dev_codec_tx_from,dev_codec_tx_to,dev_copycat,dev_copycat_arweave,dev_cu,dev_delegated_compute,dev_faff,dev_location_cache,dev_lua_lib,dev_match,dev_monitor,dev_poda,dev_process,dev_process_lib,dev_query_arweave,dev_scheduler_formats,dev_volume,hb,hb_ao,hb_ao_device,hb_app,hb_cache_render,hb_debugger,hb_features,hb_format,hb_http_benchmark_tests,hb_http_client,hb_http_client_sup,hb_http_multi,hb_invariant,hb_json,hb_logger,hb_message,hb_metrics_collector,hb_mock_server,hb_process_monitor,hb_router,hb_store_ets,hb_store_fs,hb_store_rocksdb,hb_sup,hb_test_utils,hb_util,rsa_pss"
EXTRA_EXCLUDE=""
LOGS=""
TIMESTAMP=$(date +"%y_%m_%d_%H_%M")
OUTPUT_FILE="../../aos-scrap/txt/hb_test_logs/${TIMESTAMP}.txt"
echo "Log_File_${TIMESTAMP}.txt"
while getopts "d:v:p:" opt; do
  case $opt in
    p) LOGS="$OPTARG" ;;
    d) DIR="$OPTARG" ;;              # optional: override test dir
    v) EXTRA_EXCLUDE="$OPTARG" ;;          # comma-separated list of modules to exclude
    *) echo "Usage: $0 [-d dir] [-v mod1,mod2,...]" >&2; exit 1 ;;
  esac
done
EXCLUDE="$DEFAULT_EXCLUDE,$EXTRA_EXCLUDE"
# Build module list
mods=$(find "$DIR" -name "*.erl" -exec basename {} .erl \; | sort)

# Apply exclusion if provided
if [[ -n "$EXCLUDE" ]]; then
  IFS=',' read -ra exclude_arr <<< "$EXCLUDE"
  for ex in "${exclude_arr[@]}"; do
    mods=$(echo "$mods" | grep -v "^${ex}$" || true)
  done
fi

# Output comma-separated
# Run each module
FAIL=0

while IFS= read -r mod; do
  [[ -z "$mod" ]] && continue
  echo "==> Running $mod" >> "$OUTPUT_FILE" 2>&1
  
  if ! (rm -rf cache* && HB_PRINT="$LOGS" rebar3 as genesis_wasm eunit --module="$mod" >> "$OUTPUT_FILE" 2>&1); then
    echo "❌ $mod failed" >> "$OUTPUT_FILE" 2>&1
    FAIL=1
  fi
done <<< "$mods"

exit $FAIL
