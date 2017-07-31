#include "HsFFI.h"
#include "Rts.h"
#include "initRts.h"

extern void __stginit_GDALPlugin ( void );

void initRts(void) {
  // TODO: allow passing RTS flags from os env maybe
  char *argv[] = {"gdal_HS", "+RTS", "-N", "-A10M", "-H200M", "-qa", NULL};
  char **args  = argv;
  int argc     = sizeof(argv)/sizeof(char*) - 1;
  RtsConfig conf = defaultRtsConfig;
  conf.rts_opts_enabled = RtsOptsAll;
  hs_init_ghc(&argc, &args, conf);
  hs_add_root(__stginit_GDALPlugin);
}
