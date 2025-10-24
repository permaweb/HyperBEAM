#!/bin/sh
# Launch script for deterministic-inference server that monitors parent process
# When parent (HyperBEAM) exits, this script will gracefully terminate the server

"$@" &
CHILD_PID=$!

while kill -0 "$PPID" 2>/dev/null; do
  sleep 1
done

kill -TERM "$CHILD_PID" 2>/dev/null
