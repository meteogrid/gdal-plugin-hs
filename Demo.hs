module Demo where

import GDAL.Plugin
import Data.Int

-- import System.IO.Unsafe ( unsafePerformIO )

openDataset :: HSDatasetFactory
openDataset = mapExisting fun
  where
    fun v | v>(-1) && v-1000 > (-1) = fromIntegral v - 1000 * 1.2
          | otherwise               = fromIntegral v * 2    * 0.4
    fun :: Int16 -> Float
