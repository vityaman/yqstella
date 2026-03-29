#!/usr/bin/env bash

# Default generated PrintStella.hs is not so
# pretty, so it should be patched for a more
# natural formatting and payload printing
# support.

set -e

cd "$(dirname "$0")"

DIR="/tmp/SyntaxGen"
TARGET="../src/SyntaxGen"

./codegen.bash "$DIR"

diff -ua "$DIR/PrintStella.hs" "$TARGET/PrintStella.hs" \
    | sed -re '1,2 s/\t.*//' \
    > PrintStella.hs.patch
