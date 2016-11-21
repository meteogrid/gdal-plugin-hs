{-# LANGUAGE TypeApplications #-}
module Demo where

import GDAL.Plugin
import Data.Int

openDataset = mapExisting chorrada -- (id @Int16 )

chorrada :: Int16 -> Float
chorrada v
  | v>(-1) && v-1000 > (-1) = fromIntegral v - 1000 * 1.7
  | otherwise               = fromIntegral v * 2    * 0.4
