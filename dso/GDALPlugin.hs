module GDALPlugin ( ) where

import GDAL.Plugin.Compiler ( def, Linkage(LinkDyn), envLinkage )
import GDAL.Plugin.Driver (mkDriver )
import GDAL.Internal.GDAL ( Driver (..), DriverH (..) )
import Foreign.Ptr (Ptr, castPtr)

hs_gdal_create_driver :: IO (Ptr ())
hs_gdal_create_driver = do
  Driver (DriverH ptr) <- mkDriver def { envLinkage = LinkDyn }
  return (castPtr ptr)

foreign export ccall hs_gdal_create_driver :: IO (Ptr ())
