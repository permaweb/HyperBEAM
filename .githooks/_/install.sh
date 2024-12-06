#!/bin/sh

DIR="$(dirname "$0")/.."

FLAG_FILE="$DIR/_/.setup"

if [ ! -f "$FLAG_FILE" ]; then
    echo "Linking Git Hooks 🐶..."
    git config core.hooksPath "$DIR"
    touch "$FLAG_FILE"
fi
