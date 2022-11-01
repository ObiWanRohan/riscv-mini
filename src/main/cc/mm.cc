// See LICENSE for license details.

#include "mm.h"
#include <iostream>
#include <iomanip>
#include <fstream>
#include <cstdlib>
#include <cstring>
#include <string>
#include <cassert>

#define combine_word_from_bytes(x) (x[3] << 24 | x[2] << 16 | x[1] << 8 | x[0])

#define cout_hex_word(x) std::hex << std::setw(8) << std::setfill('0') << x

/*
  Create memory of <size> bytes and having a word size of <word_size>
*/
mm_magic_t::mm_magic_t(size_t size, size_t word_size, size_t start_addr):
  data(new uint8_t[size]),
  size(size),
  word_size(word_size), 
  start_addr(start_addr),
  store_inflight(false)
{
  dummy_data.resize(word_size);
}

mm_magic_t::~mm_magic_t()
{
  delete [] data;
}

/*
  Write data to address.
  Only the first <word_size> bytes of <data> are written.
*/
void mm_magic_t::write(uint64_t addr, uint8_t *data) {
  addr %= this->size;

  uint8_t* base = this->data + addr;
  memcpy(base, data, word_size);
}

/*
  Write data to address.
  The data should have a size of <word_size> bytes
*/

void mm_magic_t::write(uint64_t addr, uint8_t *data, uint64_t strb, uint64_t size)
{
  strb &= ((1L << size) - 1) << (addr % word_size);

  std::cout << "[Write] Converting " << cout_hex_word(addr) << " to ";

  addr -= start_addr;
  addr %= this->size;

  std::cout << cout_hex_word(addr) << "\n";

  uint8_t *base = this->data + addr;
  for (int i = 0; i < word_size; i++) {
    if (strb & 1) base[i] = data[i];
    strb >>= 1;
  }
}

/*
  Read data from address
  <word_size> bytes are returned.
*/
std::vector<uint8_t> mm_magic_t::read(uint64_t addr)
{

  std::cout << "[Read] Converting " << std::hex << addr << " to ";

  addr -= this->start_addr;
  addr %= this->size;

  std::cout << addr << "\n";


  uint8_t *base = this->data + addr;

  auto result = std::vector<uint8_t>(base, base + word_size);
  std::cout << "Data : " << cout_hex_word(combine_word_from_bytes(result)) << "\n";

  return result;
}

void mm_magic_t::tick(
  bool reset,
  bool ar_valid,
  uint64_t ar_addr,
  uint64_t ar_id,
  uint64_t ar_size,
  uint64_t ar_len,

  bool aw_valid,
  uint64_t aw_addr,
  uint64_t aw_id,
  uint64_t aw_size,
  uint64_t aw_len,

  bool w_valid,
  uint64_t w_strb,
  void *w_data,
  bool w_last,

  bool r_ready,
  bool b_ready)
{
  bool ar_fire = !reset && ar_valid && ar_ready();
  bool aw_fire = !reset && aw_valid && aw_ready();
  bool w_fire = !reset && w_valid && w_ready();
  bool r_fire = !reset && r_valid() && r_ready;
  bool b_fire = !reset && b_valid() && b_ready;

  if (ar_fire) {
    uint64_t start_addr = (ar_addr / word_size) * word_size;
    for (size_t i = 0; i <= ar_len; i++) {
      auto dat = read(start_addr + i * word_size);
      rresp.push(mm_rresp_t(ar_id, dat, i == ar_len));
    }
  }

  if (aw_fire) {
    store_addr = aw_addr;
    store_id = aw_id;
    store_count = aw_len + 1;
    store_size = 1 << aw_size;
    store_inflight = true;
  }

  if (w_fire) {

    uint8_t* write_data = (uint8_t*)w_data;

    std::cout << "[Write] " << cout_hex_word(store_addr) << "\n";

    write(store_addr, (uint8_t*)w_data, w_strb, store_size);
    store_addr += store_size;
    store_count--;

    if (store_count == 0) {
      store_inflight = false;
      bresp.push(store_id);
      assert(w_last);
    }
  }

  if (b_fire)
    bresp.pop();

  if (r_fire)
    rresp.pop();

  cycle++;

  if (reset) {
    while (!bresp.empty()) bresp.pop();
    while (!rresp.empty()) rresp.pop();
    cycle = 0;
  }
}

void mm_magic_t::load_mem(const char* fn)
{
  int start = 0;
  std::ifstream in(fn);
  if (!in)
  {
    std::cerr << "could not open " << fn << std::endl;
    exit(EXIT_FAILURE);
  }

  std::string line;
  while (std::getline(in, line))
  {
    #define parse_nibble(c) ((c) >= 'a' ? (c)-'a'+10 : (c)-'0')
    for (int i = line.length()-2, j = 0; i >= 0; i -= 2, j++) {
      data[start + j] = (parse_nibble(line[i]) << 4) | parse_nibble(line[i+1]);
    }
    start += line.length()/2;
  }
}
