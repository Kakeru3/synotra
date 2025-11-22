#!/bin/bash
# Synotra Runner - Execute .sy files in one command
# Usage: ./run.sh example/hello_world.sy

set -e  # Exit on error

if [ $# -eq 0 ]; then
    echo "Usage: $0 <file.sy>"
    echo "Example: $0 example/hello_world.sy"
    exit 1
fi

SY_FILE="$1"

if [ ! -f "$SY_FILE" ]; then
    echo "Error: File '$SY_FILE' not found"
    exit 1
fi

# Get the base name without extension
BASE_NAME="${SY_FILE%.sy}"
SYI_FILE="${BASE_NAME}.syi"

echo "==> Compiling $SY_FILE..."
./target/release/synotra "$SY_FILE" > "$SYI_FILE"

echo "==> Running $SYI_FILE..."
cd syvm
cargo run --release -- "../$SYI_FILE"
cd ..

echo ""
echo "==> Done!"
