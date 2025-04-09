#!/bin/sh

"$@" &
CHILD_PID=$!

while kill -0 "$PPID" 2>/dev/null; do
  sleep 1
done

kill -TERM "$CHILD_PID" 2>/dev/null