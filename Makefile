default: compile

base_dir   = $(abspath .)
src_dir    = $(base_dir)/src/main
gen_dir    = $(base_dir)/generated-src
out_dir    = $(base_dir)/outputs
nproc      = $(shell nproc --ignore 1)		# Keep 1 core for other tasks

SBT       = sbt
SBT_FLAGS = --ivy $(base_dir)/.ivy2

scala_src = $(shell find $(src_dir)/scala -type f -name '*.scala')

sbt:
	$(SBT) $(SBT_FLAGS)

compile: $(gen_dir)/Tile.v

$(gen_dir)/Tile.v: $(scala_src)
	$(SBT) $(SBT_FLAGS) "run $(gen_dir)"

CXXFLAGS += -std=c++11 -Wall -Wno-unused-variable

# compile verilator
VERILATOR = verilator --cc --exe
VERILATOR_FLAGS = --assert -Wno-STMTDLY -O3 --trace --threads $(nproc)\
	--top-module Tile -Mdir $(gen_dir)/VTile.csrc \
	-CFLAGS "$(CXXFLAGS) -include $(gen_dir)/VTile.csrc/VTile.h" 

$(base_dir)/VTile: $(gen_dir)/Tile.v $(src_dir)/cc/top.cc $(src_dir)/cc/mm.cc $(src_dir)/cc/mm.h
	$(VERILATOR) $(VERILATOR_FLAGS) -o $@ $< $(word 2, $^) $(word 3, $^)
	$(MAKE) -C $(gen_dir)/VTile.csrc -f VTile.mk

verilator: $(base_dir)/VTile

# isa tests + benchmarks with verilator
test_hex_files = $(wildcard $(base_dir)/tests/*.hex)
test_out_files = $(foreach f,$(test_hex_files),$(patsubst %.hex,%.out,$(out_dir)/$(notdir $f)))

$(test_out_files): $(out_dir)/%.out: $(base_dir)/VTile $(base_dir)/tests/%.hex
	mkdir -p $(out_dir)
	$^ $(patsubst %.out,%.vcd,$@) 2> $@

run-tests: $(test_out_files)

# run custom benchamrk
custom_bmark_hex ?= $(base_dir)/custom-bmark/main.hex
custom_bmark_out  = $(patsubst %.hex,%.out,$(out_dir)/$(notdir $(custom_bmark_hex)))
$(custom_bmark_hex):
	$(MAKE) -C custom-bmark

$(custom_bmark_out): $(base_dir)/VTile $(custom_bmark_hex)
	mkdir -p $(out_dir)
	$^ $(patsubst %.out,%.vcd,$@) 2> $@

run-custom-bmark: $(custom_bmark_out)

# unit tests + integration tests
test:
	$(SBT) $(SBT_FLAGS) test

# Only runs tests that failed in the previous run
test-quick:
	$(SBT) $(SBT_FLAGS) testQuick

test-core-simple:
	$(SBT) $(SBT_FLAGS) "testOnly mini.CoreSimpleTests"

test-datapath:
	$(SBT) $(SBT_FLAGS) "testOnly mini.DatapathTests"

test-core-simple:
	$(SBT) $(SBT_FLAGS) "testOnly mini.CoreSimpleTests"

test-isa:
	$(SBT) $(SBT_FLAGS) "testOnly mini.CoreISATests"

clean:
	rm -rf $(gen_dir) $(out_dir) test_run_dir

cleanall: clean
	rm -rf target project/target

.PHONY: sbt compile verilator run-tests run-custom-bmark test test-quick test-datapath test-isa clean cleanall
