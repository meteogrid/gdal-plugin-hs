{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RankNTypes #-}
module GDAL.Plugin.Types (
    HSDatasetFactory (..)
  , QueryText
) where

import           GDAL ( GDAL )
import           GDAL.Internal.HSDataset ( HSDataset )
import           Network.HTTP.Types.URI ( QueryText )

newtype HSDatasetFactory = HSDatasetFactory
  { getFactory :: forall s. QueryText -> GDAL s (HSDataset s) }
