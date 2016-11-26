{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module GDAL.Plugin.CombinatorsAcc (
  mapExisting
, module A
) where

import GDAL.Plugin.Types

import GDAL
import GDAL.Internal.GDAL
import GDAL.Internal.HSDataset
import Data.Int
import qualified Data.Text as T
import qualified Data.Vector.Storable as St
import Data.Maybe
import Data.Proxy
import Control.Monad
import Control.DeepSeq
import qualified Data.Vector.Storable.Mutable as Stm
import Control.Monad.IO.Class ( MonadIO(..) )
import Foreign.Marshal.Array
import Foreign.C.Types
import Foreign.Ptr (Ptr, castPtr)
import GHC.Exts ( Constraint )
import           Data.Array.Accelerate.Array.Sugar      (EltRepr)
import           Data.Array.Accelerate                  as A
import qualified Data.Array.Accelerate.LLVM.Native      as CPU
#if HAVE_PTX
import qualified Data.Array.Accelerate.LLVM.PTX         as PTX
#endif
import Data.Array.Accelerate.IO ( BlockPtrs, Vectors, fromPtr, toVectors)
import Prelude as P
import System.IO (hPutStrLn, stderr)


type Lift1Constr a b = ( GDALType a, GDALType b, NFData b
                       , Elt a, Elt b
                       , BlockPtrs (EltRepr a) ~ Ptr a
                       , Vectors (EltRepr b) ~ St.Vector b
                       , Lift Exp b
                       )

data Lifted where
  Lift1 :: Lift1Constr a b
        => (forall s. QueryText -> GDAL s (RODataset s a))
        -> (Exp a -> Exp b)
        -> Lifted



class LiftableFunc f where
  type LiftConstr f :: Constraint
  liftFunc :: f -> Lifted

instance Lift1Constr a b  => LiftableFunc (Exp a -> Exp b) where
  type LiftConstr (Exp a -> Exp b) = Lift1Constr a b
  liftFunc = Lift1 $ \query ->
    let Just (Just path) = lookup "path" query
    in openReadOnly (T.unpack path) (dataType (Proxy :: Proxy a))

mapExisting :: LiftableFunc f => f -> HSDatasetFactory
mapExisting liftable query = case liftFunc liftable of
  Lift1 opener (fun :: Exp a -> Exp b) -> do
    dsIn <- opener query :: GDAL s (RODataset s a)
    srsIn <- datasetProjection dsIn
    gtIn <- fromMaybe (Geotransform 0 1 0 0 0 1) <$> datasetGeotransform dsIn
    bandIn <- getBand 1 dsIn
    bandNd <- bandNodataValue (bandAs bandIn (dataType (Proxy :: Proxy b)))
    funAcc <- getRun1 (A.map fun)
    let ds = HSDataset
             { rasterSize   = datasetSize dsIn
             , bands        = rasterBands
             , srs          = srsIn
             , geotransform = gtIn
             }
        rasterBands = [
          HSRasterBand { blockSize = bandBlockSize bandIn
                       , nodata = bandNd
                       , readBlock = readBandBlock' funAcc bandIn
                        }

          ]
    return ds
{-# INLINE mapExisting #-}

getRun1, getRun1CPU
  :: (MonadIO m, Arrays a, Arrays b)
  => (Acc a -> Acc b) -> m (a -> b)
getRun1CPU acc = do
  liftIO (hPutStrLn stderr "CPU.run1")
  target <- liftIO (CPU.createTarget [0] CPU.unbalancedParIO)
  return (CPU.run1With target acc)

#if HAVE_PTX
getRun1PTX
  :: (MonadIO m, Arrays a, Arrays b)
  => (Acc a -> Acc b) -> m (a -> b)
getRun1PTX acc = do
  liftIO (hPutStrLn stderr "PTX.run1")
  return (PTX.run1 acc)
getRun1 = getRun1PTX
#else
getRun1 = getRun1CPU
#endif

readBandBlock'
  :: forall sh s a b. (
        GDALType a, GDALType b, NFData b
      , Elt a, Elt b
      , BlockPtrs (EltRepr a) ~ Ptr a
      , Vectors (EltRepr b) ~ St.Vector b
      )
  => (Array DIM2 a -> Array DIM2 b)
  -> ROBand s a
  -> BlockIx
  -> GDAL s (St.Vector b)
readBandBlock' fun band ( i :+: j ) = liftIO $ do
  arr <- allocaArray (bandBlockLen band) $ \(pBuf::Ptr a) -> do
    c_readBandBlock
      (castPtr ((\(RasterBandH b) -> b) (unBand band)))
      (P.fromIntegral i)
      (P.fromIntegral j)
      (castPtr pBuf)
    let nx :+: ny = bandBlockSize band
    fromPtr (A.Z:.nx:.ny) pBuf
  let result = fun arr
      resultV = toVectors result
  return resultV
{-# INLINE readBandBlock' #-}

foreign import ccall "GDALReadBlock" c_readBandBlock ::
  Ptr () -> CInt -> CInt -> Ptr () -> IO CInt

