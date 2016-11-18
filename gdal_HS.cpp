#include <stdio.h>
#include "gdal_priv.h"
#include "gdal_frmts.h"
#include "gdal_HS.h"
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
/*                                                                      */
/*                       HSDataset                                      */
/*                                                                      */
/************************************************************************/

class HSRasterBand;

class HSDataset : public GDALDataset
{
friend class HSRasterBand;

public:
  HSDataset();
   ~HSDataset();

  static GDALDataset*  Open( GDALOpenInfo * );
  static int           Identify( GDALOpenInfo * );

  CPLErr               GetGeoTransform( double * padfTransform );
  const char*          GetProjectionRef();

private:
  double adfGeoTransform[6];
  char *pszProjection;
  void Initialize ( const hsDatasetImpl& );
  int (*pfnReadBlock)(int, int, int, void*);
};


/************************************************************************/
/*                                                                      */
/*                            HSRasterBand                              */
/*                                                                      */
/************************************************************************/

class HSRasterBand : public GDALRasterBand
{
    friend class HSDataset;

public:
  HSRasterBand( HSDataset *, int, const hsDatasetImpl& );
  virtual ~HSRasterBand();

  virtual CPLErr IReadBlock( int, int, void * );
  virtual double GetNoDataValue( int *pbSuccess = NULL );

private:

};

/************************************************************************/
/*                       HSDataset::HSDataset()                         */
/************************************************************************/
HSDataset::HSDataset():
  pszProjection(0)
{
  adfGeoTransform[0] = 0.0;
  adfGeoTransform[1] = 1.0;
  adfGeoTransform[2] = 0.0;
  adfGeoTransform[3] = 0.0;
  adfGeoTransform[4] = 0.0;
  adfGeoTransform[5] = 1.0;
}

/************************************************************************/
/*                       HSDataset::~HSDataset()                         */
/************************************************************************/
HSDataset::~HSDataset()
{
  hs_free_fun_ptr ( reinterpret_cast<HsFunPtr>( this->pfnReadBlock ) );
  CPLFree ( this->pszProjection );
}


/************************************************************************/
/*                       HSDataset::Identify()                          */
/************************************************************************/
int HSDataset::Identify( GDALOpenInfo * poOpenInfo )
{
  return gdal_hs_identifyHook ( poOpenInfo->pszFilename );
}

/************************************************************************/
/*                       HSDataset::Open()                              */
/************************************************************************/

GDALDataset *HSDataset::Open( GDALOpenInfo * poOpenInfo )
{
  if ( !Identify( poOpenInfo ) )
    return NULL;

  hsDatasetImpl impl;
  if ( gdal_hs_openHook ( poOpenInfo->pszFilename, &impl ) == 0 ) {
    HSDataset *ds = new HSDataset();
    ds->Initialize( impl );
    return ds;
  }
  return NULL;
}

/************************************************************************/
/*                       HSDataset::Initialize()                        */
/************************************************************************/
void HSDataset::Initialize( const hsDatasetImpl& impl )
{
  this->nRasterXSize  = impl.nRasterXSize;
  this->nRasterYSize  = impl.nRasterYSize;
  this->nBands        = impl.nBands;
  this->pfnReadBlock  = impl.readBlock;
  this->pszProjection = impl.pszProjection;
  memcpy( this->adfGeoTransform, impl.adfGeoTransform, sizeof(double) * 6 );

  for (int i=1; i <= this->nBands; i++) {
    this->SetBand ( i, new HSRasterBand ( this, i, impl ) );
  }

}

/************************************************************************/
/*                       HSDataset::GetGeoTransform()                   */
/************************************************************************/

CPLErr HSDataset::GetGeoTransform( double * padfTransform ) 
{
    memcpy( padfTransform,  adfGeoTransform, sizeof(double) * 6 );
    return CE_None;
}



/************************************************************************/
/*                      HSDataset::GetProjectionRef()                   */
/************************************************************************/

const char *HSDataset::GetProjectionRef() 
{
    return this->pszProjection;
}



/************************************************************************/
/*                       HSRasterBand::HSRasterBand()                         */
/************************************************************************/
HSRasterBand::HSRasterBand( HSDataset *poDS, int nBand,
                            const hsDatasetImpl &impl )
{
  this->poDS = poDS;
  this->nBand = nBand;
  this->nBlockXSize = impl.nBlockXSize;
  this->nBlockYSize = impl.nBlockYSize;
  this->eDataType   = impl.eDataType;
}

/************************************************************************/
/*                       HSDataset::~HSDataset()                         */
/************************************************************************/
HSRasterBand::~HSRasterBand()
{
}

/************************************************************************/
/*                      HSRasterBand::IReadBlock()                      */
/************************************************************************/

CPLErr
HSRasterBand::IReadBlock( int nBlockXOff, int nBlockYOff, void *pImage )
{
  HSDataset *ds = static_cast<HSDataset*>(this->poDS);
  return static_cast<CPLErr>(
    ds->pfnReadBlock( this->nBand, nBlockXOff, nBlockYOff, pImage ) );
}

/************************************************************************/
/*                     HSRasterBand::GetNoDataValue()                   */
/************************************************************************/

double HSRasterBand::GetNoDataValue( int *pbSuccess )
{
    if ( pbSuccess )
      *pbSuccess = TRUE;
    return -999;
}


/************************************************************************/
/*                                                                      */
/*                       Driver hooks                                   */
/*                                                                      */
/************************************************************************/

/************************************************************************/
/*                       GDALHSDeregister()                             */
/************************************************************************/

static void GDALHSDeregister (GDALDriver* )
{
  if ( gdal_hs_unloadDriverHook() == TRUE )
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

  poDriver->pfnOpen = HSDataset::Open;
  poDriver->pfnIdentify = HSDataset::Identify;
  poDriver->pfnUnloadDriver = GDALHSDeregister;

  hs_init(NULL, NULL);
#ifdef __GLASGOW_HASKELL__
  hs_add_root(__stginit_GDALPlugin);
#endif
  GetGDALDriverManager()->RegisterDriver( poDriver );

  gdal_hs_registerDriverHook();
}
