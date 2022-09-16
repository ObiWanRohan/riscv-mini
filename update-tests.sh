#!/bin/bash

# This script updates the tests in this repository from the the riscv-tests repository
# Set $RISCV_TESTS to the path to the riscv-tests repository, and $RISCV as the path to the RISCV toolchain
# The riscv-tests isa Makefile needs to be updated to only build the RV32MI and RV32UI tests.
# Update the riscv-tests/env/p/link.ld with the start address of the processor. The tohost MMIO address also needs to be set

set -e

base_dir="$(dirname $(readlink -f $0))"
test_dir="${base_dir}/tests"

if [[ -z "${RISCV_TESTS}" ]]; then
    echo 'Set $RISCV_TESTS to the path of the riscv-tests repository'
    exit 1
fi

if [[ -z "${RISCV}" ]]; then
    echo 'Set $RISCV to the path of the installed riscv-toolchain'
    exit 1
fi

nproc=$(nproc --ignore 1)

MAKEFLAGS="-j${nproc}"

echo "[$(date)] Updating tests"
# Make ISA tests
cd ${RISCV_TESTS}/isa
make clean
make

echo "[$(date)] Finished compiling tests"

# Copy the tests
cp ./*.hex ${base_dir}/tests/
cp ./*.dump ${base_dir}/tests/

echo "[$(date)] Finished copying tests"

# Benchmarks
echo "[$(date)] Updating benchmarks"

cd ${RISCV_TESTS}/benchmarks
make clean

echo "[$(date)] Compiling benchmarks"
make

echo "[$(date)] Finished compiling"

# Copy the benchmarks
cp ./*.riscv.hex ${base_dir}/tests/benchmarks/
cp ./*.riscv.dump ${base_dir}/tests/benchmarks/

echo "[$(date)] Finished copying"
