{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module GDAL.Plugin.Combinators (
    mapExisting
  , mapExistingWith
) where

import GDAL.Plugin.Types

import GDAL
import GDAL.Internal.GDAL
import Data.Int
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Storable as St
import Data.Maybe
import Data.Proxy
import Control.Monad
import Control.DeepSeq
import qualified Data.Vector.Storable.Mutable as Stm
import Control.Monad.IO.Class ( liftIO )
import Foreign.C.Types
import Foreign.Ptr (Ptr, castPtr)
import GHC.Exts ( inline )


   

mapExisting
  :: forall a b. (GDALType a, GDALType b, NFData b)
  => (a -> b)
  -> HSDatasetFactory
mapExisting = mapExistingWith $ \query ->
  let Just (Just path) = lookup "path" query
  in openReadOnly (T.unpack path) (dataType (Proxy :: Proxy a))
{-# INLINE mapExisting #-}

mapExistingWith
  :: forall a b. (GDALType a, GDALType b, NFData b)
  => (forall s. QueryText -> GDAL s (RODataset s a))
  -> (a -> b)
  -> HSDatasetFactory
mapExistingWith opener fun = HSDatasetFactory $ \query -> do
  dsIn <- opener query :: GDAL s (RODataset s a)
  srsIn <- datasetProjection dsIn
  gtIn <- fromMaybe (Geotransform 0 1 0 0 0 1) <$> datasetGeotransform dsIn
  bandIn <- getBand 1 dsIn
  bandNd <- bandNodataValue (bandAs bandIn (dataType (Proxy :: Proxy b)))
  let ds = HSDataset
             { rasterSize   = datasetSize dsIn
             , bands        = rasterBands
             , srs          = srsIn
             , geotransform = gtIn
             }
      rasterBands = [
          HSRasterBand { blockSize = bandBlockSize bandIn
                       , nodata = bandNd
                       , readBlock = fmap (St.map (inline fun)) .
                          readBandBlock' bandIn
                        }

        ]
  return ds
{-# INLINE mapExistingWith #-}

readBandBlock' band ( i :+: j ) = liftIO $ do
  mVec <- Stm.unsafeNew (bandBlockLen band)
  void $ Stm.unsafeWith mVec $ \pBuf ->
    c_readBandBlock
          (castPtr ((\(RasterBandH b) -> b) (unBand band)))
          (fromIntegral i)
          (fromIntegral j)
          (castPtr pBuf)
  St.unsafeFreeze mVec
{-# INLINE readBandBlock' #-}

foreign import ccall "GDALReadBlock" c_readBandBlock ::
  Ptr () -> CInt -> CInt -> Ptr () -> IO CInt
