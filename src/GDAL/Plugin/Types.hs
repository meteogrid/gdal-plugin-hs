{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module GDAL.Plugin.Types where

import           GDAL
import           OSR ( SpatialReference )

import           Control.DeepSeq ( NFData (..) )
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Storable as St
import           Foreign.Ptr ( Ptr )

type GDALPath = BS.ByteString
type GDALDatasetH = Ptr ()

type IdentifyHook = GDALPath -> IO Bool

data UnloadAction = ExitRTS | KeepRTS

type UnloadDriverHook = IO UnloadAction

type RegisterDriverHook = IO ()

type ReadBlockHook s a = BlockIx -> GDAL s (St.Vector a)

data HSDataset s = HSDataset
  { rasterSize   :: Size
  , bands        :: [HSRasterBand s]
  , srs          :: Maybe SpatialReference
  , geotransform :: Geotransform
  }

instance NFData (HSDataset s) where
  rnf (HSDataset a b c d) = rnf a `seq` rnf b  `seq` rnf c `seq` rnf d

data HSRasterBand s = forall a. (NFData a, GDALType a) =>
  HSRasterBand
    { blockSize :: !Size
    , nodata    :: !(Maybe a)
    , readBlock :: !(ReadBlockHook s a)
    }

instance NFData (HSRasterBand s) where
  rnf (HSRasterBand a b c) = rnf a `seq` rnf b  `seq` rnf c

hsBandBlockLen = (\(x :+: y) -> x*y) . blockSize
