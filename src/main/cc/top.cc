#include <verilated.h>
#include <iostream>

#if VM_TRACE
#include <verilated_vcd_c.h> // Trace file format header
#endif

#include "mm.h"

using namespace std;

vluint64_t main_time = 0; // Current simulation time
                          // This is a 64-bit integer to reduce wrap over issues and
                          // allow modulus.  You can also use a double, if you wish.

double sc_time_stamp()
{                   // Called by $time in Verilog
  return main_time; // converts to double, to match
                    // what SystemC does
}

VTile *top; // target design
#ifdef VM_TRACE
VerilatedVcdC *tfp;
#endif
mm_magic_t *mem; // target memory

void tick()
{
  top->clock = 1;
  top->eval();
#if VM_TRACE
  if (tfp)
    tfp->dump((double)main_time);
#endif // VM_TRACE
  main_time++;

  top->io_nasti_aw_ready = mem->aw_ready();
  top->io_nasti_ar_ready = mem->ar_ready();
  top->io_nasti_w_ready = mem->w_ready();
  top->io_nasti_b_valid = mem->b_valid();
  top->io_nasti_b_bits_id = mem->b_id();
  top->io_nasti_b_bits_resp = mem->b_resp();
  top->io_nasti_r_valid = mem->r_valid();
  top->io_nasti_r_bits_id = mem->r_id();
  top->io_nasti_r_bits_resp = mem->r_resp();
  top->io_nasti_r_bits_last = mem->r_last();
  memcpy(&top->io_nasti_r_bits_data, mem->r_data(), 8);

  mem->tick(
      top->reset,
      top->io_nasti_ar_valid,
      top->io_nasti_ar_bits_addr,
      top->io_nasti_ar_bits_id,
      top->io_nasti_ar_bits_size,
      top->io_nasti_ar_bits_len,

      top->io_nasti_aw_valid,
      top->io_nasti_aw_bits_addr,
      top->io_nasti_aw_bits_id,
      top->io_nasti_aw_bits_size,
      top->io_nasti_aw_bits_len,

      top->io_nasti_w_valid,
      top->io_nasti_w_bits_strb,
      &top->io_nasti_w_bits_data,
      top->io_nasti_w_bits_last,

      top->io_nasti_r_ready,
      top->io_nasti_b_ready);

  top->clock = 0;
  top->eval();
#if VM_TRACE
  if (tfp)
    tfp->dump((double)main_time);
#endif // VM_TRACE
  main_time++;
}

int main(int argc, char **argv)
{

  const char *vcdFileName = NULL;
  uint64_t timeout = 500;

  const char *hexFileName = NULL;

  const char *helpMessage = "usage: VTile <hex filename> [-v <vcd filename>] [-h | --help] [-t <timeout>]";

  if (argc < 2)
  {
    cerr << "Incorrect Usage.\n"
         << helpMessage << endl;
    return 0;
  }

  for (auto i = 1; i < argc; i++)
  {
    string arg = argv[i];

    if (arg == "-v")
    {
      vcdFileName = argv[i + 1];
      continue;
    }
    else if (arg == "-t")
    {
      timeout = atoll(argv[i + 1]);
      continue;
    }
    else if (arg == "--help" || arg == "-h")
    {
      cerr << helpMessage << endl;
      return 0;
    }
  }

  hexFileName = argv[1];
  if (!vcdFileName)
  {
    vcdFileName = "dump.vcd";
  }

  cout << "Using hex file : '" << hexFileName << "'\n";
  cout << "Writing to VCD File : '" << vcdFileName << "'\n";
  cout << "Max Cycles : '" << timeout << "'\n";

  Verilated::commandArgs(argc, argv);     // Remember args
  top = new VTile;                        // target design
  mem = new mm_magic_t(1L << 32, 8);      // target memory
  load_mem(mem->get_data(), hexFileName); // load hex

#if VM_TRACE                    // If verilator was invoked with --trace
  Verilated::traceEverOn(true); // Verilator must compute traced signals
  VL_PRINTF("Enabling waves...\n");
  tfp = new VerilatedVcdC;
  top->trace(tfp, 99);    // Trace 99 levels of hierarchy
  tfp->open(vcdFileName); // Open the dump file
#endif

  cout << "Starting simulation!\n";

  // reset
  top->reset = 1;
  for (size_t i = 0; i < 5; i++)
  {
    tick();
  }

  // start
  top->reset = 0;
  top->io_host_fromhost_bits = 0;
  top->io_host_fromhost_valid = 0;
  do
  {
    tick();
  } while (!top->io_host_tohost && main_time < timeout);

  int retcode = top->io_host_tohost >> 1;

  // Run for 10 more clocks
  for (size_t i = 0; i < 10; i++)
  {
    tick();
  }

  if (main_time >= timeout)
  {
    cerr << "Simulation terminated by timeout at time " << main_time
         << " (cycle " << main_time / 10 << ")" << endl;
    return EXIT_FAILURE;
  }
  else
  {
    cerr << "Simulation completed at time " << main_time << " (cycle " << main_time / 10 << ")" << endl;
    if (retcode)
    {
      cerr << "TOHOST = " << retcode << endl;
    }
  }

#if VM_TRACE
  if (tfp)
    tfp->close();
  delete tfp;
#endif
  delete top;
  delete mem;

  cout << "Finishing simulation!\n";

  return retcode == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
