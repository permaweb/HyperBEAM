#!/bin/bash
# E2E Test Script for HyperBEAM
# This script starts HyperBEAM in background mode and tests connectivity
# Works in non-interactive environments like Claude Code

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
LOG_FILE="/tmp/hyperbeam-e2e.log"
PID_FILE="/tmp/hyperbeam-e2e.pid"
PORT="${HB_PORT:-8734}"
PROFILE="${HB_PROFILE:-default}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log() {
    echo -e "${GREEN}[E2E]${NC} $1"
}

warn() {
    echo -e "${YELLOW}[E2E]${NC} $1"
}

error() {
    echo -e "${RED}[E2E]${NC} $1"
}

cleanup() {
    log "Cleaning up..."
    if [ -f "$PID_FILE" ]; then
        PID=$(cat "$PID_FILE")
        if kill -0 "$PID" 2>/dev/null; then
            kill "$PID" 2>/dev/null || true
            sleep 2
            kill -9 "$PID" 2>/dev/null || true
        fi
        rm -f "$PID_FILE"
    fi
    # Also kill any stray beam processes from this script
    pkill -f "beam.smp.*start_mainnet" 2>/dev/null || true
}

start_hyperbeam() {
    log "Starting HyperBEAM (profile: $PROFILE, port: $PORT)..."

    cd "$PROJECT_DIR"

    # Compile first
    log "Compiling..."
    if [ "$PROFILE" = "default" ]; then
        rebar3 compile > /dev/null 2>&1
    else
        rebar3 as "$PROFILE" compile > /dev/null 2>&1
    fi

    # Build ebin paths
    EBINS=""
    if [ "$PROFILE" != "default" ]; then
        for dir in $(find "_build/${PROFILE}/lib" -name ebin -type d 2>/dev/null); do
            EBINS="$EBINS -pa $dir"
        done
    fi
    for dir in $(find _build/default/lib -name ebin -type d 2>/dev/null); do
        EBINS="$EBINS -pa $dir"
    done

    # Start HyperBEAM in background
    erl $EBINS \
        -noinput \
        -s hb start_mainnet \
        > "$LOG_FILE" 2>&1 &

    echo $! > "$PID_FILE"
    log "Started with PID $(cat $PID_FILE)"
}

wait_for_ready() {
    log "Waiting for HyperBEAM to be ready..."
    local max_attempts=30
    local attempt=1

    # Give erlang time to start up
    sleep 5

    while [ $attempt -le $max_attempts ]; do
        # Check if the process is still running
        if [ -f "$PID_FILE" ]; then
            PID=$(cat "$PID_FILE")
            if ! kill -0 "$PID" 2>/dev/null; then
                error "HyperBEAM process died"
                error "Log output:"
                cat "$LOG_FILE" | tail -100
                return 1
            fi
        fi

        # Try to connect - use -f to fail silently on HTTP errors, check for any response
        local http_code
        http_code=$(curl -s -o /dev/null -w "%{http_code}" "http://localhost:${PORT}/~meta@1.0/info" 2>/dev/null || echo "000")

        if [ "$http_code" = "200" ]; then
            log "HyperBEAM is ready! (HTTP $http_code)"
            return 0
        fi

        sleep 1
        attempt=$((attempt + 1))
    done

    error "HyperBEAM failed to start within ${max_attempts} seconds"
    error "Log output:"
    cat "$LOG_FILE" | tail -100
    return 1
}

test_endpoint() {
    log "Testing /~meta@1.0/info endpoint..."
    local response
    response=$(curl -s -w "\n%{http_code}" "http://localhost:${PORT}/~meta@1.0/info")
    local http_code=$(echo "$response" | tail -n1)

    if [ "$http_code" = "200" ]; then
        log "Endpoint test passed (HTTP 200)"
        return 0
    else
        error "Endpoint test failed (HTTP $http_code)"
        return 1
    fi
}

test_aos() {
    if ! command -v aos &> /dev/null; then
        warn "aos CLI not found, skipping aos test"
        return 0
    fi

    log "Testing aos connection (this may take a moment)..."
    # Run aos with a timeout - it will try to connect and may fail if process doesn't exist
    # but we're just checking if the connection is attempted
    timeout 30 aos --url "http://localhost:${PORT}" 2>&1 | head -20 || true
    log "aos connection test completed"
}

show_logs() {
    log "HyperBEAM logs:"
    echo "----------------------------------------"
    tail -50 "$LOG_FILE" 2>/dev/null || echo "No logs available"
    echo "----------------------------------------"
}

usage() {
    echo "Usage: $0 [command]"
    echo ""
    echo "Commands:"
    echo "  start     Start HyperBEAM in background"
    echo "  stop      Stop HyperBEAM"
    echo "  test      Run connectivity tests"
    echo "  logs      Show HyperBEAM logs"
    echo "  full      Start, test, and keep running (default)"
    echo "  cleanup   Stop and cleanup"
    echo ""
    echo "Environment variables:"
    echo "  HB_PORT     Port to use (default: 8734)"
    echo "  HB_PROFILE  Build profile (default: default, options: inference, rocksdb)"
}

case "${1:-full}" in
    start)
        cleanup
        start_hyperbeam
        wait_for_ready
        ;;
    stop|cleanup)
        cleanup
        log "Stopped"
        ;;
    test)
        test_endpoint
        test_aos
        ;;
    logs)
        show_logs
        ;;
    full)
        trap cleanup EXIT
        cleanup
        start_hyperbeam
        if wait_for_ready; then
            test_endpoint
            log "HyperBEAM is running at http://localhost:${PORT}"
            log "To connect with aos: aos --url http://localhost:${PORT}"
            log "Press Ctrl+C to stop"
            # Keep running and show logs
            tail -f "$LOG_FILE"
        else
            exit 1
        fi
        ;;
    -h|--help|help)
        usage
        ;;
    *)
        error "Unknown command: $1"
        usage
        exit 1
        ;;
esac
