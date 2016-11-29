{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module GDAL.Plugin.Types (
    SomeFactory (..)
  , Factory
  , HasFactory (..)
  , QueryText
) where

import           GDAL ( GDAL )
import           GDAL.Internal.HSDataset ( HSDataset )
import           Network.HTTP.Types.URI ( QueryText )

type Factory = forall s. QueryText -> GDAL s (HSDataset s)

class HasFactory a where
  getFactory :: a -> SomeFactory

newtype SomeFactory = SomeFactory { getDataset :: Factory }
instance HasFactory SomeFactory where getFactory = id
