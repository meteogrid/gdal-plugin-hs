{-# LANGUAGE Trustworthy #-}
module GDAL.Plugin (
  module GDAL.Plugin.Types
, module GDAL.Plugin.Combinators
) where

-- Must not expose SomeFactory constructor for this module to be Trustworthy!!
import GDAL.Plugin.Types (Factory, SomeFactory, QueryText, getDataset, getFactory)
import GDAL.Plugin.Combinators
