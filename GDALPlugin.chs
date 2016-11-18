{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
module GDALPlugin () where

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
import GDAL.Internal.Types ( GDALInternalState (..) )

#include "gdal.h"
#include "cpl_conv.h"
#include "gdal_HS.h"

import Control.Exception ( SomeException, uninterruptibleMask_, catch, try )
import Control.DeepSeq (NFData (..) , force)
import Control.Monad
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Trans.Resource ( createInternalState )
import Control.Monad.Trans.Resource ( closeInternalState )
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (packCString)
import Data.Maybe (fromMaybe)
import Data.Proxy ( Proxy(Proxy) )
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as St
import qualified Data.Vector.Storable.Mutable as Stm
import Foreign.C
import Foreign.Marshal.Alloc ( free, callocBytes )
import Foreign.Marshal.Array ( pokeArray, copyArray )
import Foreign.Ptr (Ptr, FunPtr, nullPtr, castPtr)
import Foreign.ForeignPtr
import Foreign.StablePtr ( newStablePtr, castStablePtrToPtr )
import Foreign.StablePtr ( deRefStablePtr, castPtrToStablePtr )
import Foreign.Storable ( Storable (..) )
import System.IO ( stdout, stderr, hPutStrLn )

{# pointer HSDatasetImpl #}
{# pointer HSRasterBandImpl #}

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

type CReadBlockHook = Ptr () -> CInt -> CInt -> Ptr () -> IO CInt
type ReadBlockHook s a = BlockIx -> GDAL s (St.Vector a)

data HSDataset s = HSDataset
  { rasterSize   :: !Size
  , bands        :: [HSRasterBand s]
  , srs          :: !(Maybe SpatialReference)
  , geotransform :: !Geotransform
  }

instance NFData (HSDataset s) where
  rnf (HSDataset a b c d) = rnf a `seq` rnf b  `seq` rnf c `seq` rnf d

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
  {#set HSDatasetImpl->destroyState#} p gdal_hs_destroyStatePtr
  where
    xsize :+: ysize = fmap fromIntegral rasterSize

data HSRasterBand s = forall a. (NFData a, GDALType a) =>
  HSRasterBand
    { blockSize :: !Size
    , nodata    :: !(Maybe a)
    , readBlock :: !(ReadBlockHook s a)
    }

hsBandBlockLen = (\(x :+: y) -> x*y) . blockSize

instance NFData (HSRasterBand s) where
  rnf (HSRasterBand a b c) = rnf a `seq` rnf b  `seq` rnf c

instance NFData ErrorType where
  rnf e = e `seq` ()

foreign import ccall safe "wrapper"
  c_wrapReadBlockHook :: CReadBlockHook -> IO (FunPtr CReadBlockHook)

wrapReadBlockHook
  :: HSRasterBand s -> IO (FunPtr CReadBlockHook)
wrapReadBlockHook b@HSRasterBand{..} = c_wrapReadBlockHook c_readBlock
  where
    bLen = let x :+: y  = blockSize in x*y
    deRefState :: Ptr () -> IO (GDALInternalState s)
    deRefState = fmap GDALInternalState . deRefStablePtr . castPtrToStablePtr
    c_readBlock :: CReadBlockHook
    c_readBlock statePtr
              (fromIntegral -> i)
              (fromIntegral -> j)
              (castPtr -> destPtr) = do
      eVec <- try . runWithInternalState (readBlock (i :+: j))
          =<< deRefState statePtr
      case eVec of
        Right v -> St.unsafeWith v $ \srcPtr -> do
          copyArray destPtr srcPtr bLen
          return ok
        Left (e :: SomeException) -> do
          hPutStrLn stderr ("Unhandled exception in readBlock: " ++ show e)
          return failure

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
  Just arg <- BS.stripPrefix "HS:" <$> packCString carg
  state <- GDALInternalState <$> createInternalState
  ds <- runWithInternalState (openDataset arg) state
  pokeHSDataset impl ds
  statePtr <- castStablePtrToPtr <$> newStablePtr state
  {#set HSDatasetImpl->state #} impl statePtr
  return ok
  where
    cleanup = do
      {#call unsafe destroyHSDatasetImpl#} impl
      return failure

openDataset :: GDALPath -> GDAL s (HSDataset s)
openDataset path = do
  dsIn <- openReadOnly (BS.unpack path) GDT_Int16
  srsIn <- datasetProjection dsIn
  gtIn <- fromMaybe (Geotransform 0 1 0 0 0 1) <$> datasetGeotransform dsIn
  bandIn <- getBand 1 dsIn
  bandNd <- bandNodataValue bandIn
  let ds = HSDataset
             { rasterSize   = datasetSize dsIn
             , bands        = rasterBands
             , srs          = srsIn
             , geotransform = gtIn
             }
      rasterBands = [
          HSRasterBand { blockSize = bandBlockSize bandIn
                       , nodata = bandNd
                       , readBlock = fmap (St.map (subtract 1000)) .
                          readBandBlock' bandIn
                        }

        ]
  return ds

readBandBlock' band ( i :+: j ) = liftIO $ do
  mVec <- Stm.unsafeNew (bandBlockLen band)
  Stm.unsafeWith mVec $ \pBuf ->
    {#call unsafe GDALReadBlock as lolailos #}
          (castPtr ((\(RasterBandH b) -> b) (unBand band)))
          (fromIntegral i)
          (fromIntegral j)
          (castPtr pBuf)
  St.unsafeFreeze mVec

true = {#const TRUE #}
false = {#const FALSE #}

gdal_hs_identifyHook :: CIdentifyHook
gdal_hs_identifyHook carg = handleAllExceptions (return false) $ do
  arg <- packCString carg
  return $ if BS.isPrefixOf "HS:" arg then true else false

gdal_hs_registerDriverHook :: CRegisterDriverHook
gdal_hs_registerDriverHook = return ()

gdal_hs_unloadDriverHook :: CUnloadDriverHook
gdal_hs_unloadDriverHook = return false

gdal_hs_destroyState = handleAllExceptions (return ()) .
  (closeInternalState <=< deRefStablePtr . castPtrToStablePtr)


foreign export ccall gdal_hs_openHook           :: COpenHook
foreign export ccall gdal_hs_identifyHook       :: CIdentifyHook
foreign export ccall gdal_hs_unloadDriverHook   :: CUnloadDriverHook
foreign export ccall gdal_hs_registerDriverHook :: CRegisterDriverHook
foreign export ccall gdal_hs_destroyState       :: Ptr () -> IO ()
foreign import ccall unsafe "&gdal_hs_destroyState"
  gdal_hs_destroyStatePtr :: FunPtr (Ptr () -> IO ())
