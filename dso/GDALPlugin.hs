{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GDALPlugin ( ) where

import GDAL.Plugin.Compiler ( def, HscTarget(..), cfgSafeModeOn, cfgTarget )
import GDAL.Plugin.Types (SomeFactory)
import GDAL.Plugin.Driver (mkDriver )
import GDAL.Internal.GDAL ( Driver (..), DriverH (..) )
import Data.Proxy
import Foreign.Ptr (Ptr, castPtr)
import Data.Maybe (isJust)
import System.Environment ( lookupEnv )

hs_gdal_create_driver :: IO (Ptr ())
hs_gdal_create_driver = do
  disableSafeMode      <- isJust <$> lookupEnv "GDAL_PLUGIN_HS_UNSAFE"
  compiled             <- isJust <$> lookupEnv "GDAL_PLUGIN_HS_COMPILED"
  Driver (DriverH ptr) <- mkDriver (Proxy :: Proxy SomeFactory)  "HS" def
    { cfgSafeModeOn = not disableSafeMode
    , cfgTarget     = if compiled then HscAsm else HscInterpreted
    }
  return (castPtr ptr)

foreign export ccall hs_gdal_create_driver :: IO (Ptr ())
