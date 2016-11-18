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
  HSRasterBand( HSDataset *, int, const hsRasterBandImpl& );
  virtual ~HSRasterBand();

  virtual CPLErr IReadBlock( int, int, void * );
  virtual double GetNoDataValue( int *pbSuccess = NULL );

private:
  double noDataValue;
  bool hasNodata;
  int (*const pfnReadBlock)(int, int, void*);

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
  memset(&impl, 0, sizeof(hsDatasetImpl));
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
  memcpy( this->adfGeoTransform, impl.adfGeoTransform, sizeof(double) * 6 );
  this->pszProjection = impl.pszProjection;

  for (int i=0; i < this->nBands; i++) {
    this->SetBand ( i+1, new HSRasterBand ( this, i+1, impl.bands[i] ) );
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
                            const hsRasterBandImpl &impl ):
  pfnReadBlock ( impl.readBlock )
{
  this->poDS = poDS;
  this->nBand = nBand;
  this->nBlockXSize  = impl.nBlockXSize;
  this->nBlockYSize  = impl.nBlockYSize;
  this->eDataType    = impl.eDataType;
  this->noDataValue  = impl.nodata;
  this->hasNodata    = impl.hasNodata;
}

/************************************************************************/
/*                       HSDataset::~HSDataset()                         */
/************************************************************************/
HSRasterBand::~HSRasterBand()
{
  hs_free_fun_ptr ( reinterpret_cast<HsFunPtr>( this->pfnReadBlock ) );
}

/************************************************************************/
/*                      HSRasterBand::IReadBlock()                      */
/************************************************************************/

CPLErr
HSRasterBand::IReadBlock( int nBlockXOff, int nBlockYOff, void *pImage )
{
  HSDataset *ds = static_cast<HSDataset*>(this->poDS);
  return static_cast<CPLErr>(
    this->pfnReadBlock( nBlockXOff, nBlockYOff, pImage ) );
}

/************************************************************************/
/*                     HSRasterBand::GetNoDataValue()                   */
/************************************************************************/

double HSRasterBand::GetNoDataValue( int *pbSuccess )
{
    if ( pbSuccess )
      *pbSuccess = this->hasNodata;
    return this->noDataValue;
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
  if ( gdal_hs_unloadDriverHook() == TRUE ) {
    hs_exit();
  }
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
  //poDriver->SetMetadataItem( GDAL_DCAP_VECTOR, "YES" );
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

/************************************************************************/
/*                         destroyHSDatasetImpl                         */
/************************************************************************/
void destroyHSDatasetImpl (HSDatasetImpl impl)
{
  if ( impl->bands ) {
    for (int i=0; i<impl->nBands; i++) {
      if ( impl->bands[i].readBlock ) {
        hs_free_fun_ptr (
          reinterpret_cast<HsFunPtr>( impl->bands[i].readBlock ) );
      }
    }
    free ( impl->bands );
  }
  CPLFree ( impl->pszProjection );
}
