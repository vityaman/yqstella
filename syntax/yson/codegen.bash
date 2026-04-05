#!/usr/bin/env bash

set -e

cd "$(dirname "$0")"

SRCS=(
    AbsYson.hs
    LexYson.hs
    ParYson.hs
    PrintYson.hs
)

PKG="Yson.SyntaxGen"
DIR="Yson/SyntaxGen"
TARGET="${1:-../../src/Yson/SyntaxGen}"

bnfc --haskell -m -p "$PKG" --functor Yson.cf

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
