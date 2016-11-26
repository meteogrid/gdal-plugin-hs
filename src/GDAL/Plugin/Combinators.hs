{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module GDAL.Plugin.Combinators (
    mapExisting
  , mapExistingWith
) where

import GDAL.Plugin.Types
import GDAL.Plugin.Internal as I

import GDAL
import GDAL.Internal.GDAL
import GDAL.Internal.HSDataset
import qualified Data.Text as T
import qualified Data.Vector.Storable as St
import Data.Maybe
import Data.Proxy
import Control.Monad
import Control.DeepSeq
import Control.Monad.IO.Class ( liftIO )
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
mapExistingWith opener fun query = do
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
                          I.readBandBlock bandIn
                        }

        ]
  return ds
{-# INLINE mapExistingWith #-}
