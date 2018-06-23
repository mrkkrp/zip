#!/usr/bin/env bash
#
# Benchmark Haskell zip library against system zip/unzip executables:
#
# Usage: bench.hs <TEST-FILE>

set -e

SCRATCH=$(mktemp -d --tmpdir zip-bench.XXXX)
HARCHIVE=$SCRATCH/zip-bench-haskell.zip
HARCHIVE_UNPACKED=$SCRATCH/zip-bench-haskell-unpacked
SARCHIVE=$SCRATCH/zip-bench-system.zip
SARCHIVE_UNPACKED=$SCRATCH/zip-bench-system-unpacked

trap "rm -rf $SCRATCH; exit" 0 1 2 3 15

echo "Compressing."

echo "With Haskell zip library."
time haskell-zip-app compress "$1" "$HARCHIVE"

echo "With system-level zip."
time zip -9q "$SARCHIVE" "$1"

echo "Decompressing."

echo "With Haskell zip library."
time haskell-zip-app uncompress "$HARCHIVE" "$HARCHIVE_UNPACKED"

echo "With system-level zip."
time unzip -q "$SARCHIVE" -d "$SARCHIVE_UNPACKED"
