#!/usr/bin/env bash

set -e

cd "$(dirname "$0")"

SRCS=(
    AbsStella.hs
    LexStella.hs
    ParStella.hs
    PrintStella.hs
)

DIR="SyntaxGen"
TARGET="${1:-../src/SyntaxGen}"

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

if [ -z "$1" ]; then
    patch "$TARGET/PrintStella.hs" PrintStella.hs.patch
fi
