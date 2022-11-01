# riscv-mini

Author : Rohan Nawathe(rnawathe@outlook.com) and Aakash Gupta(aakashnareshgupta@gmail.com)
Original Author: Donggyu Kim (dgkim@eecs.berkeley.edu)

`riscv-mini` is a simple RISC-V 5-stage pipeline written in Chisel. It was a 3 stage pipeline which we modified.
It has been a crucial example in various project developments,
including [Chisel3](https://github.com/ucb-bar/chisel3.git), [FIRRTL](https://github.com/ucb-bar/firrtl.git),
[Strober](https://bar.eecs.berkeley.edu/projects/strober.html), simulation and verification methodologies.
It implements RV32I of the User-level ISA Version 2.0 and the Machine-level ISA of the Privileged Architecture Version 1.11.
Unlike other simple pipelines, it also contains simple instruction and data caches.

Note that a real-world processor is not the goal of `riscv-mini`.
It is developed as an intermediate example before diving into [rocket-chip](https://github.com/freechipsproject/rocket-chip). 

## Datapath Diagram
![pipeline](diagram.png)

## Getting Started

    $ git clone https://github.com/ucb-bar/riscv-mini.git
    $ cd riscv-mini
    $ make            # generate firrtl & verilog files in generated-src
    
The verilog output file can be used for verilator simulation or the ASIC tool flow.
    
## Running Verilator Simulation

First, generate the verilator binary:

    $ make verilator
    
This will generate `VTile` in the top-level directory.

Now, you can run verilator simulation for a given hex file as follows:

    $ ./VTile <hex file> [<vcd file> 2> <log file>]
    
`<vcd file>` and the pipe to `<log file>` are optional. The waveform is dumped to `dump.vcd` and the execution trace is printed in the screen by default.

The following command runs the whole test hex files in verilator and dumps the traces and the waveforms to the 'outputs' directory:

    $ make run-tests VTILE_CYCLES=100000 TILE_MEM_START_ADDR=0x7FFFF000

### Generating hex binary for verilator
The current tests are from the [`riscv-tests`](https://github.com/riscv-software-src/riscv-tests) repository. They are compiled with `riscv64-unknown-elf-gcc` with `-march=rv32i` and `-mabi=ilp32` compiler options. The ELF binary are exported to hex using `elf2hex`. (Get the elf2hex executable from the [SiFive Github](https://github.com/sifive/elf2hex))

The riscv-tools are setup using [`chipyard`](https://github.com/ucb-bar/chipyard).
A custom linker script has been used to set the start address to `0x200` (This is the PC_START)

You can use the [`update-tests.sh`](./update-tests.sh) script to update the tests in the repository. Set the `RISCV_TESTS` and `RISCV` environment variable, and run the script. There are some changes that need to be made to the riscv-tests isa `Makefile`. Only compile the tests for `rv32ui` and `rv32mi`.

## Unit and Integration Tests with `sbt`

`riscv-mini` provides synthesizable unit & integration tests.
Theres are six sets of unit tests(`ALUTests`, `BrCondTests`, `ImmGenTests`, `CSRTests`, `CacheTests`, `DatapathTests`),
running user-defined test vectors.
To execute them, first launch sbt with `make sbt` and run:

    > testOnly mini.[testname]
  
There are also six sets of integration tests, running the hex files from [riscv-tests](https://github.com/riscv/riscv-tests).
To execute them, also launch `sbt` and run:

    > testOnly mini.[Core|Tile][Simple|ISA|Bmark]Tests
    
`Core` only contains the datapath and the control unit, while `Tile` also contains I$ and D$. `Simple` only runs `rv32ui-p-simple`,
`ISA` runs the whole ISA tests, and `Bmark` runs five benchmarks(`median`, `multiply`, `qsort`, `towers`, `vvadd`). 
Note that all tests in a set run in parallel.

Finally, to run all the tests, just in sbt:

    > test
    
## Running Your Own Program on `riscv-mini`

<!-- TODO : Update this and the script -->
At this point, you may want to implement and exeucte your custom application on `riscv-mini`. In this case, you need to install RISC-V tools for priv 1.11. 
<!-- This repo provides a script to install the correct version of tools. Run the script as follows:

    $ export RISCV=<path to riscv tools for priv 1.11>
    $ ./build-riscv-tools -->
    
It takes a while to install the toolchain, so please be patient.

This repo also provides a template for your own program in `custom-bmark`. Add your c or assembly code and edit `Makefile`. Next, to compile you program, run `make` in `custom-bmark` to generate the binary, dump, and the hex files. Finally, run the following command in the base directory:

    $ make run-custom-bmark
