#include <stdio.h>
#include "gdal_priv.h"
#include "gdal_frmts.h"
#include "HsFFI.h"

#ifdef __GLASGOW_HASKELL__
#include "GDALPlugin_stub.h"
#endif


CPL_C_START
#ifdef __GLASGOW_HASKELL__
extern void __stginit_GDALPlugin ( void );
#endif
void GDALRegister_HS();
CPL_C_END


/************************************************************************/
/*                       GDALHSOpen()                                   */
/************************************************************************/

static GDALDataset *GDALHSOpen(GDALOpenInfo * poOpenInfo )
{
  return static_cast<GDALDataset*>(
      gdal_hs_openHook ( poOpenInfo->pszFilename ) );
}

/************************************************************************/
/*                       GDALHSIdentify()                               */
/************************************************************************/
static int GDALHSIdentify(GDALOpenInfo * poOpenInfo )
{
  return gdal_hs_identifyHook ( poOpenInfo->pszFilename );
}

/************************************************************************/
/*                       GDALHSDeregister()                             */
/************************************************************************/

static void GDALHSDeregister (GDALDriver* )
{
  if ( gdal_hs_unloadDriverHook() )
    hs_exit();
}

/************************************************************************/
/*                         GDALRegister_HS()                            */
/************************************************************************/

void GDALRegister_HS()

{
  if( GDALGetDriverByName( "HS" ) != NULL )
      return;

  GDALDriver *poDriver = new GDALDriver();

  poDriver->SetDescription( "HS" );
  poDriver->SetMetadataItem( GDAL_DCAP_RASTER, "YES" );
  poDriver->SetMetadataItem( GDAL_DMD_LONGNAME, "Haskell programs (.hs)" );
  poDriver->SetMetadataItem( GDAL_DMD_HELPTOPIC, "frmt_hs.html" );
  poDriver->SetMetadataItem( GDAL_DMD_SUBDATASETS, "YES" );
  poDriver->SetMetadataItem( GDAL_DMD_EXTENSION, "hs" );

  poDriver->pfnOpen = GDALHSOpen;
  poDriver->pfnIdentify = GDALHSIdentify;
  poDriver->pfnUnloadDriver = GDALHSDeregister;

  hs_init(NULL, NULL);
#ifdef __GLASGOW_HASKELL__
  hs_add_root(__stginit_GDALPlugin);
#endif
  GetGDALDriverManager()->RegisterDriver( poDriver );

  gdal_hs_registerDriverHook();
}
