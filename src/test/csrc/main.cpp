#include <functional>
#include <locale.h>
#include <csignal>
#include "emu.h"

static char mybuf[BUFSIZ];

// junk, link for verilator
std::function<double()> get_sc_time_stamp = []() -> double { return 0; };
double sc_time_stamp() { return get_sc_time_stamp(); }

int main(int argc, const char** argv) {
  printf("Emu compiled at %s, %s\n", __DATE__, __TIME__);

  setbuf(stderr, mybuf);

  // enable thousands separator for printf()
  setlocale(LC_NUMERIC, "");

  if (signal(SIGINT, sig_handler) == SIG_ERR) {
    printf("\ncan't catch SIGINT\n");
  }

  auto emu = new Emulator(argc, argv);

  get_sc_time_stamp = [&emu]() -> double {
    return emu->get_cycles();
  };

  auto args = emu->get_args();
  uint64_t cycles = emu->execute(args.max_cycles, args.max_instr);
  bool is_good_trap = emu->is_good_trap();
  int trapcode = emu->get_trapcode();
  delete emu;

  extern uint32_t uptime(void);
  uint32_t ms = uptime();

  eprintf(ANSI_COLOR_BLUE "Seed=%d Guest cycle spent: %'" PRIu64
      " (this will be different from cycleCnt if emu loads a snapshot)\n" ANSI_COLOR_RESET, args.seed, cycles);
  eprintf(ANSI_COLOR_BLUE "Host time spent: %'dms\n" ANSI_COLOR_RESET, ms);

  // return !is_good_trap;
  return trapcode;
}
