#include "gdal_priv.h"
#include "gdal_frmts.h"
#include "GDALPlugin_stub.h"
#include "initRts.h"

CPL_C_START
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
  initRts();
  GDALDriver *poDriver = static_cast<GDALDriver*>(hs_gdal_create_driver());
  poDriver->pfnUnloadDriver = GDALHSUnload;
  GetGDALDriverManager()->RegisterDriver( poDriver );
}
