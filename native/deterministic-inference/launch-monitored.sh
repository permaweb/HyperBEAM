#!/bin/bash
# Launch script for deterministic-inference server that monitors parent process
# When parent (HyperBEAM) exits, this script will gracefully terminate the server

set -e

# Function to cleanup on exit
cleanup() {
    echo "[launch-monitored] Cleaning up..."
    if [ ! -z "$CHILD_PID" ] && kill -0 "$CHILD_PID" 2>/dev/null; then
        echo "[launch-monitored] Sending SIGTERM to child process $CHILD_PID"
        kill -TERM "$CHILD_PID" 2>/dev/null || true
        
        # Wait for graceful shutdown with timeout
        for i in {1..15}; do
            if ! kill -0 "$CHILD_PID" 2>/dev/null; then
                echo "[launch-monitored] Child process exited gracefully"
                return
            fi
            sleep 1
        done
        
        # Force kill if still alive
        if kill -0 "$CHILD_PID" 2>/dev/null; then
            echo "[launch-monitored] Forcing kill of child process $CHILD_PID"
            kill -KILL "$CHILD_PID" 2>/dev/null || true
        fi
    fi
}

trap cleanup EXIT INT TERM

# Start the child process
"$@" &
CHILD_PID=$!

echo "[launch-monitored] Started child process: $CHILD_PID, parent: $PPID"

# Monitor parent process with shorter sleep interval
while kill -0 "$PPID" 2>/dev/null; do
  # Check if child is still alive
  if ! kill -0 "$CHILD_PID" 2>/dev/null; then
    echo "[launch-monitored] Child process died unexpectedly"
    exit 1
  fi
  sleep 0.5  # Faster response time
done

echo "[launch-monitored] Parent process $PPID exited, initiating cleanup"
exit 0
