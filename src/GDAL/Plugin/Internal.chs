{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
module GDAL.Plugin.Internal (
    toGDALDataset
  , toGDALDatasetIO
  , handleAllExceptions
  , true
  , false
  , printErr
  ) where

#include "gdal.h"
#include "cpl_conv.h"
#include "hsdataset.h"

import GDAL.Plugin.Types

import GDAL
import GDAL.Internal.GDAL ( unBand )
import GDAL ( GDAL, Geotransform(..), ErrorType(..), GDALException (..) )
import GDAL ( GDALType (..), BlockIx, Pair((:+:)), Value (..), Size )
import OSR ( SpatialReference, srsFromEPSG )

import GDAL.Internal.GDAL
import GDAL.Internal.DataType ( DataTypeK (..), hsDataType, toCDouble )
import GDAL.Internal.OSR ( withMaybeSRAsCString )
import GDAL.Internal.Util ( fromEnumC, toEnumC )
import GDAL.Internal.Types.Value ( toGVec, toGVecWithNodata )
import GDAL.Internal.Types ( runWithInternalState )
import GDAL.Internal.Types ( GDALInternalState, getInternalState )
import GDAL.Internal.Types ( createGDALInternalState )
import GDAL.Internal.Types ( closeGDALInternalState )

import Control.Exception ( SomeException, uninterruptibleMask_, catch, try )
import Control.DeepSeq (NFData (..) , force)
import Control.Monad
import Control.Monad.IO.Class ( liftIO )
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (packCString)
import Data.Maybe (fromMaybe)
import Data.Proxy ( Proxy(Proxy) )
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as St
import qualified Data.Vector.Storable.Mutable as Stm
import Foreign.C
import Foreign.Marshal.Alloc ( free, callocBytes, alloca )
import Foreign.Marshal.Array ( pokeArray, copyArray )
import Foreign.Ptr (Ptr, FunPtr, nullPtr, nullFunPtr, castPtr)
import Foreign.ForeignPtr
import Foreign.StablePtr ( newStablePtr, castStablePtrToPtr )
import Foreign.StablePtr ( deRefStablePtr, castPtrToStablePtr )
import Foreign.Storable ( Storable (..) )
import System.IO ( stdout, stderr, hPutStrLn )

{# pointer HSDatasetImpl #}
{# pointer HSRasterBandImpl #}

type CReadBlockHook = Ptr () -> CInt -> CInt -> Ptr () -> IO CInt


pokeHSDataset p HSDataset{..} = uninterruptibleMask_ $ do
  {#set HSDatasetImpl->nRasterXSize#}  p xsize
  {#set HSDatasetImpl->nRasterYSize#}  p ysize
  {#set HSDatasetImpl->nBands#}        p (fromIntegral (length bands))
  bandsPtr <- callocBytes (length bands * {#sizeof hsRasterBandImpl#})
  -- set the pointer before poking the array so we don't lose the reference
  -- and we can cleanup in case a poke fails midway
  {#set HSDatasetImpl->bands#} p (castPtr bandsPtr)
  pokeArray bandsPtr bands
  pSrs <- withMaybeSRAsCString srs {#call unsafe CPLStrdup as cStrdup #}
  {#set HSDatasetImpl->pszProjection#} p pSrs
  flip poke geotransform . castPtr =<< {#get HSDatasetImpl->adfGeoTransform#} p
  where
    xsize :+: ysize = fmap fromIntegral rasterSize


foreign import ccall safe "wrapper"
  c_wrapReadBlockHook :: CReadBlockHook -> IO (FunPtr CReadBlockHook)

wrapReadBlockHook
  :: HSRasterBand s -> IO (FunPtr CReadBlockHook)
wrapReadBlockHook b@HSRasterBand{..} = c_wrapReadBlockHook c_readBlock
  where
    c_readBlock :: CReadBlockHook
    c_readBlock statePtr i j destPtr = handleAllExceptions onExc $ do
      let ix = fmap fromIntegral (i :+: j)
      state <- deRefState statePtr
      v <- runWithInternalState (readBlock ix) state
      St.unsafeWith v $ \srcPtr ->
        copyArray (castPtr destPtr) srcPtr bLen
      return ok

    onExc e = do
      printErr ("Unhandled exception in readBlock: ", e)
      return failure

    deRefState :: Ptr () -> IO (GDALInternalState s)
    deRefState = deRefStablePtr . castPtrToStablePtr

    bLen = hsBandBlockLen b

pokeHSRasterBand :: Ptr (HSRasterBand s) -> HSRasterBand s -> IO ()
pokeHSRasterBand p b@HSRasterBand{readBlock=(rb :: ReadBlockHook s a),..} = do
  {#set HSRasterBandImpl->nBlockXSize#} p xsize
  {#set HSRasterBandImpl->nBlockYSize#} p ysize
  {#set HSRasterBandImpl->eDataType#}   p (fromEnumC dtype)
  {#set HSRasterBandImpl->nodata#}      p (toCDouble (fromMaybe 0 nodata))
  {#set HSRasterBandImpl->hasNodata#}   p (maybe 0 (const 1) nodata)
  fPtr <- wrapReadBlockHook b
  {#set HSRasterBandImpl->readBlock#} p fPtr
  where
    dtype = hsDataType (Proxy :: Proxy a)
    xsize :+: ysize = fmap fromIntegral blockSize

instance Storable (HSRasterBand s) where
  sizeOf _ = {#sizeof hsRasterBandImpl #}
  alignment _ = {#alignof hsRasterBandImpl #}

  peek =
    error "HSRasterBand's Storable's peek has not been implemented yet"
  poke = pokeHSRasterBand

printErr = hPutStrLn stderr . show

ok :: CInt
ok = fromEnumC CE_None

warning :: CInt
warning = fromEnumC CE_Warning

failure :: CInt
failure = fromEnumC CE_Failure

handleAllExceptions
  :: NFData a => (SomeException -> IO a) -> IO a -> IO a
handleAllExceptions onError action =
  uninterruptibleMask_ (catch (fmap force action) onError)

toGDALDatasetIO :: GDAL s (HSDataset s) -> IO GDALDatasetH
toGDALDatasetIO mkDataset =
  alloca $ \impl ->
  handleAllExceptions (onExc impl) $ do
    state <- createGDALInternalState
    ds <- runWithInternalState mkDataset state
    pokeHSDataset impl ds
    statePtr <- castStablePtrToPtr <$> newStablePtr state
    {#set HSDatasetImpl->state #} impl statePtr
    destroyState <- c_wrapDestroyState closeInternalStateHook
    {#set HSDatasetImpl->destroyState#} impl destroyState
    {#call unsafe hs_gdal_create_dataset#} impl
  where
    onExc impl e = do
      printErr ("Unhandled exception in toGDALDatasetIO: ", e)
      {#call unsafe destroyHSDatasetImpl#} impl
      return nullPtr

toGDALDataset :: HSDataset s -> GDAL s GDALDatasetH
toGDALDataset ds = do
  state <- getInternalState
  liftIO $ alloca $ \impl -> handleAllExceptions (onExc impl) $ do
    pokeHSDataset impl ds
    statePtr <- castStablePtrToPtr <$> newStablePtr state
    {#set HSDatasetImpl->state #} impl statePtr
    {#set HSDatasetImpl->destroyState#} impl nullFunPtr
    {#call unsafe hs_gdal_create_dataset#} impl
  where
    onExc impl e = do
      printErr ("Unhandled exception in toGDALDatasetIO: ", e)
      {#call unsafe destroyHSDatasetImpl#} impl
      return nullPtr


true, false :: CInt
true = {#const TRUE #}
false = {#const FALSE #}


foreign import ccall safe "wrapper"
  c_wrapDestroyState :: (Ptr () -> IO ()) -> IO (FinalizerPtr ())

closeInternalStateHook
  = handleAllExceptions onExc
  . (closeGDALInternalState <=< deRefStablePtr . castPtrToStablePtr)
  where
    onExc e =
      printErr ("Unhandled exception in destroyState: ", e)
