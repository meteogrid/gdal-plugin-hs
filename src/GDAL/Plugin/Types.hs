{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RankNTypes #-}
module GDAL.Plugin.Types (
    SomeHSDatasetFactory (..)
  , HSDatasetFactory
  , QueryText
) where

import           GDAL ( GDAL )
import           GDAL.Internal.HSDataset ( HSDataset )
import           Network.HTTP.Types.URI ( QueryText )

type HSDatasetFactory = forall s. QueryText -> GDAL s (HSDataset s)

newtype SomeHSDatasetFactory = SomeHSDatasetFactory
  { getFactory :: HSDatasetFactory }
