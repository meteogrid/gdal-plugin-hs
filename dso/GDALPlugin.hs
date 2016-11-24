module GDALPlugin ( ) where

import GDAL.Plugin.Compiler ( def, Linkage(LinkDyn), cfgLinkage, cfgSafeModeOn )
import GDAL.Plugin.Driver (mkDriver )
import GDAL.Internal.GDAL ( Driver (..), DriverH (..) )
import Foreign.Ptr (Ptr, castPtr)
import Data.Maybe (isJust)
import System.Environment ( lookupEnv )

hs_gdal_create_driver :: IO (Ptr ())
hs_gdal_create_driver = do
  disableSafeMode <- isJust <$> lookupEnv "GDAL_PLUGIN_HS_UNSAFE"
  Driver (DriverH ptr) <- mkDriver def { cfgLinkage = LinkDyn
                                       , cfgSafeModeOn = not disableSafeMode
                                       }
  return (castPtr ptr)

foreign export ccall hs_gdal_create_driver :: IO (Ptr ())
