#!/usr/bin/env bash
#
# Run EUnit test modules in parallel across separate BEAM VMs.
# Each VM gets a unique HB_PORT to avoid port conflicts.
#
# Usage:
#   scripts/parallel_tests.sh                    # run all test modules
#   scripts/parallel_tests.sh mod1 mod2 mod3     # run specific modules
#
set -uo pipefail

NPROC=${MAKEFLAGS:+$(echo "$MAKEFLAGS" | grep -oP '(?<=-j)\d+' || true)}
NPROC=${NPROC:-$(nproc 2>/dev/null || echo 16)}
BASE_PORT=19000
EBIN_DIR="_build/test/lib/hb/ebin"
PA_DIRS="_build/test/lib/*/ebin"
LOG_DIR="/tmp/eunit_parallel"

echo "=== Compiling (test profile) ==="
rebar3 as test compile || { echo "Compilation failed"; exit 1; }

if [ $# -gt 0 ]; then
    MODULES="$*"
else
    echo "=== Discovering test modules ==="
    MODULES=$(erl -noshell -pa $PA_DIRS -eval '
        Beams = filelib:wildcard("'"$PA_DIRS"'/*.beam"),
        Mods = lists:usort([list_to_atom(filename:basename(B, ".beam")) || B <- Beams]),
        TestMods = lists:filter(fun(M) ->
            try
                Exports = M:module_info(exports),
                lists:any(fun({F,0}) ->
                    S = atom_to_list(F),
                    lists:suffix("_test", S) orelse lists:suffix("_test_", S);
                (_) -> false end, Exports)
            catch _:_ -> false
            end
        end, Mods),
        [io:format("~s~n", [M]) || M <- lists:sort(TestMods)],
        halt(0).
    ' 2>/dev/null)
fi

MODULE_COUNT=$(echo "$MODULES" | wc -w)

rm -rf "$LOG_DIR"
mkdir -p "$LOG_DIR"

echo "=== Running $MODULE_COUNT modules with $NPROC workers ==="
echo ""

T_START=$(date +%s)

run_module() {
    local mod=$1
    local port=$2
    local logfile="$LOG_DIR/${mod}.log"

    local t0
    t0=$(date +%s%3N 2>/dev/null || python3 -c 'import time; print(int(time.time()*1000))')

    HB_PORT=$port erl -noshell -pa $PA_DIRS -eval "
        application:ensure_all_started(hb),
        case eunit:test($mod, [verbose, {scale_timeouts, 20}]) of
            ok -> halt(0);
            error -> halt(1)
        end.
    " > "$logfile" 2>&1
    local rc=$?

    local t1
    t1=$(date +%s%3N 2>/dev/null || python3 -c 'import time; print(int(time.time()*1000))')
    local elapsed=$(( (t1 - t0) / 1000 ))

    if [ $rc -eq 0 ]; then
        printf "  \033[32mPASS\033[0m  %-45s %3ds\n" "$mod" "$elapsed"
    else
        printf "  \033[31mFAIL\033[0m  %-45s %3ds  (see %s)\n" "$mod" "$elapsed" "$logfile"
    fi
    return $rc
}

export -f run_module
export PA_DIRS LOG_DIR

FAILED_MODULES=0
PORT=$BASE_PORT
PIDS=()

for mod in $MODULES; do
    run_module "$mod" "$PORT" &
    PIDS+=($!)
    PORT=$((PORT + 1))

    # Throttle: when at capacity, wait for any job to finish before launching next
    while [ ${#PIDS[@]} -ge "$NPROC" ]; do
        wait -n -p DONE_PID "${PIDS[@]}" || FAILED_MODULES=$((FAILED_MODULES + 1))
        PIDS=(${PIDS[@]/$DONE_PID/})
    done
done

for pid in "${PIDS[@]}"; do
    wait "$pid" || FAILED_MODULES=$((FAILED_MODULES + 1))
done

T_END=$(date +%s)
ELAPSED=$((T_END - T_START))

# Count test results from eunit summary lines in each log
TOTAL_PASSED=0
TOTAL_FAILED=0
TOTAL_SKIPPED=0
for logfile in "$LOG_DIR"/*.log; do
    [ -f "$logfile" ] || continue
    # "Failed: F.  Skipped: S.  Passed: P."
    line=$(grep -oP 'Failed: \K[0-9]+(?=\.).*Passed: [0-9]+' "$logfile" 2>/dev/null | tail -1)
    if [ -n "$line" ]; then
        TOTAL_FAILED=$((TOTAL_FAILED + $(echo "$line" | grep -oP '^[0-9]+')))
        TOTAL_SKIPPED=$((TOTAL_SKIPPED + $(echo "$line" | grep -oP 'Skipped: \K[0-9]+')))
        TOTAL_PASSED=$((TOTAL_PASSED + $(echo "$line" | grep -oP 'Passed: \K[0-9]+')))
        continue
    fi
    # "All N tests passed." / "N tests passed."
    n=$(grep -oP '[0-9]+(?= tests? passed)' "$logfile" 2>/dev/null | tail -1)
    if [ -n "$n" ]; then
        TOTAL_PASSED=$((TOTAL_PASSED + n))
        continue
    fi
    # "Test passed." (singular = 1)
    if grep -q 'Test passed\.' "$logfile" 2>/dev/null; then
        TOTAL_PASSED=$((TOTAL_PASSED + 1))
    fi
done
TOTAL_CANCELLED=$(cat "$LOG_DIR"/*.log 2>/dev/null | grep -c '\*timed out\*' || true)

echo ""
echo "=== Done in ${ELAPSED}s ==="
echo "    Modules: $MODULE_COUNT"
echo "======================================================="
echo "  Failed: $TOTAL_FAILED.  Skipped: $TOTAL_CANCELLED.  Passed: $TOTAL_PASSED."

if [ "$FAILED_MODULES" -gt 0 ]; then
    echo ""
    echo "Failed module logs in $LOG_DIR/"
    exit 1
fi
