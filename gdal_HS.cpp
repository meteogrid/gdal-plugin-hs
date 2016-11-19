#include <stdio.h>
#include <iostream>
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
  HSDataset(const hsDatasetImpl&);
  const double adfGeoTransform[6];
  char *pszProjection;
  const HsStablePtr state;
  void (*const pfnDestroyState)( HsStablePtr );
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
  const HsStablePtr state;
  const double noDataValue;
  const bool hasNodata;
  int (*const pfnReadBlock)(const HsStablePtr, const int, const int, void*);

};

/************************************************************************/
/*                       HSDataset::HSDataset()                         */
/************************************************************************/
HSDataset::HSDataset():
  adfGeoTransform{0,1,0,0,0,1},
  pszProjection(0),
  state(0),
  pfnDestroyState(0)
{
}

/************************************************************************/
/*                       HSDataset::HSDataset(impl)                     */
/************************************************************************/
HSDataset::HSDataset(const hsDatasetImpl& impl):
  adfGeoTransform{impl.adfGeoTransform[0]
                 ,impl.adfGeoTransform[1]
                 ,impl.adfGeoTransform[2]
                 ,impl.adfGeoTransform[3]
                 ,impl.adfGeoTransform[4]
                 ,impl.adfGeoTransform[5]},
  pszProjection(impl.pszProjection),
  state(impl.state),
  pfnDestroyState(impl.destroyState)
{
  this->nRasterXSize    = impl.nRasterXSize;
  this->nRasterYSize    = impl.nRasterYSize;
  this->nBands          = impl.nBands;

  for (int i=0; i < this->nBands; i++) {
    HSRasterBand *band = new HSRasterBand ( this, i+1, impl.bands[i] ) ;
    this->SetBand ( i+1, band );
  }
}

/************************************************************************/
/*                       HSDataset::~HSDataset()                         */
/************************************************************************/
HSDataset::~HSDataset()
{
  CPLFree ( this->pszProjection );
  if ( this->pfnDestroyState && this->state ) {
    this->pfnDestroyState ( this->state );
  }
  if ( this->state ) {
    hs_free_stable_ptr ( this->state );
  }
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
    return new HSDataset( impl );
  }
  return NULL;
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
  pfnReadBlock ( impl.readBlock ),
  state ( poDS->state ),
  noDataValue ( impl.nodata ),
  hasNodata ( impl.hasNodata )
{
  this->poDS = poDS;
  this->nBand = nBand;
  this->nBlockXSize  = impl.nBlockXSize;
  this->nBlockYSize  = impl.nBlockYSize;
  this->eDataType    = impl.eDataType;
}

/************************************************************************/
/*                       HSDataset::~HSDataset()                        */
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
  //std::cerr << nBlockXOff << "," << nBlockYOff << std::endl;
  HSDataset *ds = static_cast<HSDataset*>(this->poDS);
  return static_cast<CPLErr>(
    this->pfnReadBlock( this->state, nBlockXOff, nBlockYOff, pImage ) );
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
/*                                                                      */
/* For use in cleanup after haskell exception. Does not free the ptr    */
/* itself. Obviously, the pointer struct members must not be used after */
/* calling this.                                                        */
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
  if ( impl->state ) {
    hs_free_stable_ptr ( impl-> state );
  }
  CPLFree ( impl->pszProjection );
}
