#include "gdal.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct hsRasterBandImpl {
  int nBlockXSize;
  int nBlockYSize;
  GDALDataType eDataType;
  double nodata;
  int hasNodata;
  int (*readBlock)( int, int, void* );
}* HSRasterBandImpl;


typedef struct hsDatasetImpl {
  int nRasterXSize;
  int nRasterYSize;
  int nBands;
  struct hsRasterBandImpl *bands;
  char *pszProjection;
  double adfGeoTransform[6];
}* HSDatasetImpl;

void destroyHSDatasetImpl (HSDatasetImpl impl);

#ifdef __cplusplus
}
#endif

