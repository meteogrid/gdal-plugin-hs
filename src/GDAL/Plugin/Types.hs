{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Trustworthy #-}
module GDAL.Plugin.Types (
    SomeFactory
  , someFactory
  , Factory
  , HasFactory (..)
  , QueryText
  , HSDriverOpen (..)
  , Maybe
  , Text
) where

import           GDAL (GDAL)
import           GDAL.Internal.HSDataset (HSDataset)
import           GDAL.Internal.HSDriver (HSDriverOpen(..))
import           Network.HTTP.Types.URI (QueryText)
import           Data.Typeable
import           Data.Text (Text)

type Factory = QueryText -> HSDriverOpen

class HasFactory a where
  getFactory :: a -> Factory

someFactory :: (QueryText -> (forall s. GDAL s (HSDataset s))) -> SomeFactory
someFactory = SomeFactory

newtype SomeFactory = SomeFactory { getDataset :: QueryText -> (forall s. GDAL s (HSDataset s)) }
  deriving Typeable
instance HasFactory SomeFactory where getFactory (SomeFactory fun) = HsdDataset . fun
