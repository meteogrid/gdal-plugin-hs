{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GDALPlugin () where

import GDAL ( Geotransform(..), ErrorType(..) )
import OSR ( SpatialReference, srsFromEPSG )

import GDAL.Internal.DataType ( DataTypeK (..) )
import GDAL.Internal.OSR ( withMaybeSRAsCString )
import GDAL.Internal.Util ( fromEnumC )

#include "gdal.h"
#include "cpl_conv.h"
#include "gdal_HS.h"

import Control.Exception ( SomeException, uninterruptibleMask_, catch )
import Control.DeepSeq ( force )
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (packCString)
import Data.Monoid ((<>))
import Foreign.C
import Foreign.Marshal.Alloc ( free )
import Foreign.Marshal.Array ( callocArray, pokeArray )
import Foreign.Ptr (Ptr, FunPtr, nullPtr, castPtr)
import Foreign.Storable
import System.IO ( stdout, stderr, hPutStrLn )

{# pointer HSDatasetImpl->HSDataset #}
{# pointer HSRasterBandImpl->HSRasterBand #}

type OpenHook = CString -> HSDatasetImpl -> IO CInt
type IdentifyHook = CString -> IO CInt
type UnloadDriverHook = IO CInt
type RegisterDriverHook = IO ()
type ReadBlockHook = CInt -> CInt -> Ptr () -> IO CInt

data HSDataset = HSDataset
  { xRasterSize  :: !Int
  , yRasterSize  :: !Int
  , bands        :: [HSRasterBand]
  , srs          :: !(Maybe SpatialReference)
  , geotransform :: !Geotransform
  }

pokeHSDataset p HSDataset{..} = uninterruptibleMask_ $ do
  {#set HSDatasetImpl->nRasterXSize#}  p (fromIntegral xRasterSize)
  {#set HSDatasetImpl->nRasterYSize#}  p (fromIntegral yRasterSize)
  {#set HSDatasetImpl->nBands#}        p (fromIntegral (length bands))
  bandsPtr <- callocArray (length bands)
  -- set the pointer before poking the array so we don't lose the reference
  -- and we can cleanup in case a poke fails midway
  {#set HSDatasetImpl->bands#} p (castPtr bandsPtr)
  pokeArray bandsPtr bands
  pSrs <- withMaybeSRAsCString srs {#call unsafe CPLStrdup as cStrdup #}
  {#set HSDatasetImpl->pszProjection#} p pSrs
  flip poke geotransform . castPtr =<< {#get HSDatasetImpl->adfGeoTransform#} p

data HSRasterBand = HSRasterBand
  { xBlockSize   :: !Int
  , yBlockSize   :: !Int
  , dtype        :: !DataTypeK
  , readBlock    :: !ReadBlockHook
  }

foreign import ccall safe "wrapper"
  wrapReadBlockHook :: ReadBlockHook -> IO (FunPtr ReadBlockHook)

pokeHSRasterBand p HSRasterBand{..} = do
  {#set HSRasterBandImpl->nBlockXSize#}   p (fromIntegral xBlockSize)
  {#set HSRasterBandImpl->nBlockYSize#}   p (fromIntegral yBlockSize)
  {#set HSRasterBandImpl->eDataType#}     p (fromEnumC dtype)
  {#set HSRasterBandImpl->readBlock#} p =<< wrapReadBlockHook readBlock

instance Storable HSRasterBand where
  sizeOf _ = {#sizeof hsRasterBandImpl #}
  alignment _ = {#alignof hsRasterBandImpl #}

  peek =
    error "HSRasterBand's Storable's peek has not been implemented yet"
  poke = pokeHSRasterBand


ok :: CInt
ok = fromEnumC CE_None

failure :: CInt
failure = fromEnumC CE_Failure

handleAllExceptions onError action = uninterruptibleMask_ $
  catch (fmap force action) $ \(e::SomeException) -> do
    hPutStrLn stderr (show e)
    onError

gdal_hs_openHook :: OpenHook
gdal_hs_openHook carg impl = handleAllExceptions cleanup $ do
  arg <- packCString carg
  let ds = HSDataset
             { xRasterSize  = 256
             , yRasterSize  = 128
             , bands        = rasterBands
             , srs          = Just mySrs
             , geotransform = Geotransform 0 0.005 0 0 0 (-0.005)
             }
      Right mySrs = srsFromEPSG 25830
      rasterBands = [
          HSRasterBand { xBlockSize = 128
                       , yBlockSize = 256
                       , dtype      = GFloat32
                       , readBlock  = \i j _ -> do
                           print ("band 1", i, j)
                           return ok
                        }
        , HSRasterBand { xBlockSize = 32
                       , yBlockSize = 64
                       , dtype      = GFloat64
                       , readBlock  = \i j _ -> do
                           print ("band 2", i, j)
                           return ok
                        }

        ]
  pokeHSDataset impl ds
  return ok
  where
    cleanup = do
      {#call unsafe destroyHSDatasetImpl#} impl
      return failure

true = {#const TRUE #}
false = {#const FALSE #}

gdal_hs_identifyHook :: IdentifyHook
gdal_hs_identifyHook carg = handleAllExceptions (return false) $ do
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
