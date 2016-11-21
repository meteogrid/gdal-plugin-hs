#include "HsFFI.h"
#include "gdal_priv.h"
#include "gdal_frmts.h"

#ifdef __GLASGOW_HASKELL__
#include "GDALPlugin_stub.h"
#endif

CPL_C_START
#ifdef __GLASGOW_HASKELL__
extern void __stginit_GDALPlugin ( void );
#endif
void GDALRegister_HS();
CPL_C_END


static void GDALHSUnload (GDALDriver* )
{
  hs_exit();
}


/************************************************************************/
/*                         GDALRegister_HS()                            */
/************************************************************************/

void GDALRegister_HS()

{
  if( GDALGetDriverByName( "HS" ) != NULL )
      return;

  hs_init(NULL, NULL);
#ifdef __GLASGOW_HASKELL__
  hs_add_root(__stginit_GDALPlugin);
#endif
  hs_gdal_register_plugin();
}
  /*
  GDALDriver *poDriver = new GDALDriver();

  poDriver->SetDescription( "HS" );
  poDriver->SetMetadataItem( GDAL_DCAP_RASTER, "YES" );
  //poDriver->SetMetadataItem( GDAL_DCAP_VECTOR, "YES" );
  poDriver->SetMetadataItem( GDAL_DMD_LONGNAME, "Haskell programs (.hs)" );
  poDriver->SetMetadataItem( GDAL_DMD_HELPTOPIC, "frmt_hs.html" );
  poDriver->SetMetadataItem( GDAL_DMD_SUBDATASETS, "YES" );
  poDriver->SetMetadataItem( GDAL_DMD_EXTENSION, "hs" );

  poDriver->pfnOpen = GDALHSOpen;
  poDriver->pfnIdentify = GDALHSIdentify;
  poDriver->pfnUnloadDriver = GDALHSUnload;

  GetGDALDriverManager()->RegisterDriver( poDriver );

  hs_gdal_registerDriverHook();
  */

