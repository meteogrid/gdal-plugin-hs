{-# LANGUAGE QuasiQuotes #-}
module Demo where

import GDAL.Plugin
import Data.Int
import Sigym4.Dimension

times = [cron|0 0 * * *|]

dataset :: HSDatasetFactory
dataset = mapExisting chorrada

chorrada :: Int16 -> Int16
chorrada = subtract 5 . max 5 
