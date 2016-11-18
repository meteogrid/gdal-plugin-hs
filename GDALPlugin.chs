{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module GDALPlugin () where

import GDAL ( Geotransform(..), ErrorType(..) )
import OSR ( SpatialReference, srsFromEPSG)

import GDAL.Internal.DataType ( DataTypeK (..) )
import GDAL.Internal.OSR ( withMaybeSRAsCString )
import GDAL.Internal.Util ( fromEnumC )

#include "gdal.h"
#include "cpl_conv.h"
#include "gdal_HS.h"

import qualified Data.ByteString.Char8 as BS
import Data.ByteString (packCString)
import Data.Monoid ((<>))
import Foreign.C
import Foreign.Ptr (Ptr, FunPtr, nullPtr, castPtr)
import Foreign.Storable
import System.IO (stdout)

{# pointer HSDatasetImpl->HSDataset #}

type OpenHook = CString -> HSDatasetImpl -> IO CInt
type IdentifyHook = CString -> IO CInt
type UnloadDriverHook = IO CInt
type RegisterDriverHook = IO ()
type ReadBlockHook = CInt -> CInt -> CInt -> Ptr () -> IO CInt

data HSDataset = HSDataset
  { xRasterSize  :: !Int
  , yRasterSize  :: !Int
  , xBlockSize   :: !Int
  , yBlockSize   :: !Int
  , nBands       :: !Int
  , dtype        :: !DataTypeK
  , srs          :: !(Maybe SpatialReference)
  , geotransform :: !Geotransform
  , readBlock    :: !ReadBlockHook
  }

foreign import ccall safe "wrapper"
  wrapReadBlockHook :: ReadBlockHook -> IO (FunPtr ReadBlockHook)

instance Storable HSDataset where
  sizeOf _    = {#sizeof hsDatasetImpl#}

  alignment _ = {#alignof hsDatasetImpl#}

  peek = error "HSDataset Storable instance does not implement peek"

  poke p HSDataset{..} = do
    pSrs <- withMaybeSRAsCString srs {#call unsafe CPLStrdup as cStrdup #}
    {#set HSDatasetImpl->nRasterXSize#}  p (fromIntegral xRasterSize)
    {#set HSDatasetImpl->nRasterYSize#}  p (fromIntegral yRasterSize)
    {#set HSDatasetImpl->nBlockXSize#}   p (fromIntegral xBlockSize)
    {#set HSDatasetImpl->nBlockYSize#}   p (fromIntegral yBlockSize)
    {#set HSDatasetImpl->nBands#}        p (fromIntegral nBands)
    {#set HSDatasetImpl->pszProjection#} p pSrs
    {#set HSDatasetImpl->eDataType#}     p (fromEnumC dtype)
    flip poke geotransform . castPtr =<< {#get HSDatasetImpl->adfGeoTransform#} p
    {#set HSDatasetImpl->readBlock#} p =<< wrapReadBlockHook readBlock


ok :: CInt
ok = fromEnumC CE_None

failure :: CInt
failure = fromEnumC CE_Failure

gdal_hs_openHook :: OpenHook
gdal_hs_openHook carg impl = do
  arg <- packCString carg
  let ds = HSDataset
             { xRasterSize  = 256
             , yRasterSize  = 128
             , xBlockSize   = 256
             , yBlockSize   = 128
             , nBands       = 10000
             , dtype        = GFloat64
             , srs          = Just mySrs
             , geotransform = Geotransform 0 0.005 0 0 0 (-0.005)
             , readBlock    = \b i j _ -> return ok
             }
      Right mySrs = srsFromEPSG 25830
  poke impl ds
  return ok

true = {#const TRUE #}
false = {#const FALSE #}

gdal_hs_identifyHook :: IdentifyHook
gdal_hs_identifyHook carg = do
  arg <- peekCString carg
  return true

gdal_hs_registerDriverHook :: RegisterDriverHook
gdal_hs_registerDriverHook = return ()

gdal_hs_unloadDriverHook :: UnloadDriverHook
gdal_hs_unloadDriverHook = return false


foreign export ccall gdal_hs_openHook           :: OpenHook
foreign export ccall gdal_hs_identifyHook       :: IdentifyHook
foreign export ccall gdal_hs_unloadDriverHook   :: UnloadDriverHook
foreign export ccall gdal_hs_registerDriverHook :: RegisterDriverHook
