#include "HsFFI.h"
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
  hs_init(NULL, NULL);
  hs_add_root(__stginit_GDALPlugin);
  GDALDriver *poDriver = static_cast<GDALDriver*>(hs_gdal_create_driver());
  poDriver->pfnUnloadDriver = GDALHSUnload;
  GetGDALDriverManager()->RegisterDriver( poDriver );
}
