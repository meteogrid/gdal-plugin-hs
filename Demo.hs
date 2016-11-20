{-# LANGUAGE OverloadedStrings #-}
module Demo where
import GDAL
import GDAL.Plugin
import GDAL.Internal.GDAL
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Storable as St
import Data.Maybe
import Control.Monad
import qualified Data.Vector.Storable.Mutable as Stm
import Control.Monad.IO.Class ( liftIO )
import Foreign.C.Types
import Foreign.Ptr (Ptr, castPtr)




openDataset :: HSDatasetFactory
openDataset = HSDatasetFactory $ \query -> do
  liftIO (print query)
  let Just (Just path) = lookup "path" query
  liftIO (print path)
  dsIn <- openReadOnly (T.unpack path) GDT_Int16
  liftIO (print "ou yeah")
  srsIn <- datasetProjection dsIn
  gtIn <- fromMaybe (Geotransform 0 1 0 0 0 1) <$> datasetGeotransform dsIn
  bandIn <- getBand 1 dsIn
  bandNd <- bandNodataValue bandIn
  let ds = HSDataset
             { rasterSize   = datasetSize dsIn
             , bands        = rasterBands
             , srs          = srsIn
             , geotransform = gtIn
             }
      rasterBands = [
          HSRasterBand { blockSize = bandBlockSize bandIn
                       , nodata = bandNd
                       , readBlock = fmap (St.map pixelFun) .
                          readBandBlock' bandIn
                        }

        ]
      pixelFun v = if v-1000 > (-1) then v-1000 else v
  return ds

readBandBlock' band ( i :+: j ) = liftIO $ do
  mVec <- Stm.unsafeNew (bandBlockLen band)
  void $ Stm.unsafeWith mVec $ \pBuf ->
    c_readBandBlock
          (castPtr ((\(RasterBandH b) -> b) (unBand band)))
          (fromIntegral i)
          (fromIntegral j)
          (castPtr pBuf)
  St.unsafeFreeze mVec

foreign import ccall "GDALReadBlock" c_readBandBlock ::
  Ptr () -> CInt -> CInt -> Ptr () -> IO CInt
