{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns #-}
module GDALPlugin () where

import GDAL ( Geotransform(..), ErrorType(..), GDALException (..) )
import GDAL ( GDALType (..), BlockIx, Pair((:+:)), Value (..), Size )
import OSR ( SpatialReference, srsFromEPSG )

import GDAL.Internal.DataType ( DataTypeK (..), hsDataType, toCDouble )
import GDAL.Internal.OSR ( withMaybeSRAsCString )
import GDAL.Internal.Util ( fromEnumC )
import GDAL.Internal.Types.Value ( toGVec, toGVecWithNodata )

#include "gdal.h"
#include "cpl_conv.h"
#include "gdal_HS.h"

import Control.Exception ( SomeException, uninterruptibleMask_, catch, try )
import Control.DeepSeq (force)
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (packCString)
import Data.Maybe (fromMaybe)
import Data.Proxy ( Proxy(Proxy) )
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as St
import Foreign.C
import Foreign.Marshal.Alloc ( free )
import Foreign.Marshal.Array ( callocArray, pokeArray, copyArray )
import Foreign.Ptr (Ptr, FunPtr, nullPtr, castPtr)
import Foreign.Storable
import System.IO ( stdout, stderr, hPutStrLn )

{# pointer HSDatasetImpl->HSDataset #}
{# pointer HSRasterBandImpl->HSRasterBand #}

type GDALPath = BS.ByteString

type COpenHook = CString -> HSDatasetImpl -> IO CInt
type OpenHook = GDALPath -> HSDatasetImpl -> IO ErrorType

type CIdentifyHook = CString -> IO CInt
type IdentifyHook = GDALPath -> IO CInt

data UnloadAction = ExitRTS | KeepRTS

type CUnloadDriverHook = IO CInt
type UnloadDriverHook = IO UnloadAction

type CRegisterDriverHook = IO ()
type RegisterDriverKook = IO ()

type CReadBlockHook = CInt -> CInt -> Ptr () -> IO CInt
type ReadBlockHook a =
  BlockIx -> IO (Either ErrorType (U.Vector (Value a)))

data HSDataset = HSDataset
  { rasterSize   :: !Size
  , bands        :: [HSRasterBand]
  , srs          :: !(Maybe SpatialReference)
  , geotransform :: !Geotransform
  }

pokeHSDataset p HSDataset{..} = uninterruptibleMask_ $ do
  {#set HSDatasetImpl->nRasterXSize#}  p xsize
  {#set HSDatasetImpl->nRasterYSize#}  p ysize
  {#set HSDatasetImpl->nBands#}        p (fromIntegral (length bands))
  bandsPtr <- callocArray (length bands)
  -- set the pointer before poking the array so we don't lose the reference
  -- and we can cleanup in case a poke fails midway
  {#set HSDatasetImpl->bands#} p (castPtr bandsPtr)
  pokeArray bandsPtr bands
  pSrs <- withMaybeSRAsCString srs {#call unsafe CPLStrdup as cStrdup #}
  {#set HSDatasetImpl->pszProjection#} p pSrs
  flip poke geotransform . castPtr =<< {#get HSDatasetImpl->adfGeoTransform#} p
  where
    xsize :+: ysize = fmap fromIntegral rasterSize

data HSRasterBand = forall a. GDALType a =>
  HSRasterBand
    { blockSize :: !Size
    , nodata    :: !(Maybe a)
    , readBlock :: !(ReadBlockHook a)
    }

foreign import ccall safe "wrapper"
  c_wrapReadBlockHook :: CReadBlockHook -> IO (FunPtr CReadBlockHook)

wrapReadBlockHook
  :: GDALType a => Maybe a -> ReadBlockHook a -> IO (FunPtr CReadBlockHook)
wrapReadBlockHook nodata fun = c_wrapReadBlockHook readBlock
  where
    toStVec = maybe toGVec ((Just . ) . toGVecWithNodata) nodata
    readBlock :: CReadBlockHook
    readBlock (fromIntegral -> i) (fromIntegral -> j) (castPtr -> ptr) = do
      eVec <- try (fun (i :+: j))
      case eVec of
        Right (Right uv) ->
          case toStVec uv of
            Just v -> St.unsafeWith v $ \p ->
              copyArray ptr p (St.length v) >> return ok
            Nothing -> do
              hPutStrLn stderr ("WARN: unexpected NoData from readBlock")
              -- Should we zero de output or something?
              return warning
        Right (Left e) -> return (fromEnumC e)
        Left (e :: SomeException) -> do
          hPutStrLn stderr ("Unhandled exception in readBlock: " ++ show e)
          return failure

pokeHSRasterBand :: HSRasterBandImpl -> HSRasterBand -> IO ()
pokeHSRasterBand p HSRasterBand{readBlock=(rb :: ReadBlockHook a),..} = do
  {#set HSRasterBandImpl->nBlockXSize#} p xsize
  {#set HSRasterBandImpl->nBlockYSize#} p ysize
  {#set HSRasterBandImpl->eDataType#}   p (fromEnumC dtype)
  {#set HSRasterBandImpl->nodata#}      p (toCDouble (fromMaybe 0 nodata))
  {#set HSRasterBandImpl->hasNodata#}   p (maybe 0 (const 1) nodata)
  fPtr <- wrapReadBlockHook nodata rb
  {#set HSRasterBandImpl->readBlock#} p fPtr
  where
    dtype = hsDataType (Proxy :: Proxy a)
    xsize :+: ysize = fmap fromIntegral blockSize

instance Storable HSRasterBand where
  sizeOf _ = {#sizeof hsRasterBandImpl #}
  alignment _ = {#alignof hsRasterBandImpl #}

  peek =
    error "HSRasterBand's Storable's peek has not been implemented yet"
  poke = pokeHSRasterBand


ok :: CInt
ok = fromEnumC CE_None

warning :: CInt
warning = fromEnumC CE_None

failure :: CInt
failure = fromEnumC CE_Failure

handleAllExceptions onError action = uninterruptibleMask_ $
  catch (fmap force action) $ \(e::SomeException) -> do
    hPutStrLn stderr (show e)
    onError

gdal_hs_openHook :: COpenHook
gdal_hs_openHook carg impl = handleAllExceptions cleanup $ do
  arg <- packCString carg
  let ds = HSDataset
             { rasterSize   = 256 :+: 128
             , bands        = rasterBands
             , srs          = Just mySrs
             , geotransform = Geotransform 0 0.005 0 0 0 (-0.005)
             }
      Right mySrs = srsFromEPSG 25830
      rasterBands = [
          HSRasterBand { blockSize = 128 :+: 256
                       , nodata = Just (-999)
                       , readBlock  = \ix -> do
                           print ("band 1", ix)
                           let v = U.replicate (128*256) (Value 1)
                               v :: U.Vector (Value Float)
                           return (Right v)
                        }
        , HSRasterBand { blockSize = 32 :+: 64
                       , nodata = Just (-888)
                       , readBlock  = \ix -> do
                           print ("band 2", ix)
                           let v = U.replicate (32*64) (Value 2)
                               v :: U.Vector (Value Double)
                           return (Right v)
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

gdal_hs_identifyHook :: CIdentifyHook
gdal_hs_identifyHook carg = handleAllExceptions (return false) $ do
  arg <- peekCString carg
  return true

gdal_hs_registerDriverHook :: CRegisterDriverHook
gdal_hs_registerDriverHook = return ()

gdal_hs_unloadDriverHook :: CUnloadDriverHook
gdal_hs_unloadDriverHook = return false


foreign export ccall gdal_hs_openHook           :: COpenHook
foreign export ccall gdal_hs_identifyHook       :: CIdentifyHook
foreign export ccall gdal_hs_unloadDriverHook   :: CUnloadDriverHook
foreign export ccall gdal_hs_registerDriverHook :: CRegisterDriverHook
