#include "gdal.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct hsDatasetImpl {
  int nRasterXSize;
  int nRasterYSize;
  int nBlockXSize;
  int nBlockYSize;
  int nBands;
  char *pszProjection;
  GDALDataType eDataType;
  double adfGeoTransform[6];
  int (*readBlock)(int, int, int, void*);
}* HSDatasetImpl;


#ifdef __cplusplus
}
#endif

