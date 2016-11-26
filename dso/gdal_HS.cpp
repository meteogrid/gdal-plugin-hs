#include "HsFFI.h"
#include "Rts.h"
#include "gdal_priv.h"
#include "gdal_frmts.h"
#include "GDALPlugin_stub.h"

CPL_C_START
extern void __stginit_GDALPlugin ( void );
void GDALRegister_HS();
CPL_C_END


static void GDALHSUnload (GDALDriver* )
{
  // it is safe to call even if haskell unloads the driver since only the
  // outermost call will exit the rts
  //hs_exit();
}


void GDALRegister_HS()
{
  if( GDALGetDriverByName( "HS" ) != NULL )
      return;
  // TODO: allow passing RTS flags from os env maybe
  char *argv[] = {"gdal_HS", "+RTS", "-N", "-A10M", "-H200M", "-qa", NULL};
  char **args  = argv;
  int argc     = sizeof(argv)/sizeof(char*) - 1;
  RtsConfig conf = defaultRtsConfig;
  conf.rts_opts_enabled = RtsOptsAll;
  hs_init_ghc(&argc, &args, conf);
  hs_add_root(__stginit_GDALPlugin);
  GDALDriver *poDriver = static_cast<GDALDriver*>(hs_gdal_create_driver());
  poDriver->pfnUnloadDriver = GDALHSUnload;
  GetGDALDriverManager()->RegisterDriver( poDriver );
}
