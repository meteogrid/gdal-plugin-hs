{-# LANGUAGE ForeignFunctionInterface #-}
module GDALPlugin (
  gdal_hs_identifyHook
, gdal_hs_openHook
, gdal_hs_registerDriverHook
, gdal_hs_unloadDriverHook
) where

import Foreign.C
import Foreign.C.String (peekCString)
import Foreign.Ptr (Ptr, nullPtr)

type GDALOpenInfo = Ptr ()
type GDALDataset = Ptr ()
type GDALDriver = Ptr ()

type OpenHook = CString -> IO GDALDataset
type IdentifyHook = CString -> IO CInt
type UnloadDriverHook = IO CInt
type RegisterDriverHook = IO ()

gdal_hs_openHook :: OpenHook
gdal_hs_openHook carg = do
  arg <- peekCString carg
  print ("Open: " ++ arg)
  return nullPtr

gdal_hs_identifyHook :: IdentifyHook
gdal_hs_identifyHook carg = do
  arg <- peekCString carg
  print ("Identify: " ++ arg)
  return 1

gdal_hs_registerDriverHook :: RegisterDriverHook
gdal_hs_registerDriverHook =
  print "Register"

gdal_hs_unloadDriverHook :: UnloadDriverHook
gdal_hs_unloadDriverHook = do
  print "Unload"
  return 1


foreign export ccall gdal_hs_openHook           :: OpenHook
foreign export ccall gdal_hs_identifyHook       :: IdentifyHook
foreign export ccall gdal_hs_unloadDriverHook   :: UnloadDriverHook
foreign export ccall gdal_hs_registerDriverHook :: RegisterDriverHook
