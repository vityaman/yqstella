#!/usr/bin/env bash

set -e

cd "$(dirname "$0")"

SRCS=(
    AbsStella.hs
    LexStella.hs
    ParStella.hs
    PrintStella.hs
)

DIR="Syntax"
TARGET="../src/Syntax"

bnfc --haskell -m -p "$DIR" --functor Stella.cf

for file in "${SRCS[@]}"; do
    make "$DIR/$file"
done

rm -rf "$TARGET"
mkdir -p "$TARGET"
for file in "${SRCS[@]}"; do
    cp "$DIR/$file" "$TARGET/$file"
done

rm -rf "$DIR"
rm Makefile
