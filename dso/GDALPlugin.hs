module GDALPlugin ( ) where

import GDAL.Plugin.Driver ( installDriver )

hs_gdal_register_plugin :: IO ()
hs_gdal_register_plugin = installDriver

foreign export ccall hs_gdal_register_plugin :: IO ()
