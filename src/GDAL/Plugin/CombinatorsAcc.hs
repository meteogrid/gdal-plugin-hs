{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}


module GDAL.Plugin.CombinatorsAcc (
  module GDAL.Plugin.CombinatorsAcc
, module A
) where

import GDAL.Plugin.Types
import GDAL.Plugin.Internal as I

import GDAL
import GDAL.Internal.HSDataset
import qualified Data.Text as T
import qualified Data.Vector.Storable as St
import Data.Maybe
import Data.Proxy
import Control.Monad
import Control.DeepSeq
import Control.Monad.IO.Class ( MonadIO(..) )
import GHC.Exts ( Constraint )
import           Data.Array.Accelerate.Array.Sugar      (EltRepr)
import           Data.Array.Accelerate                  as A
import qualified Data.Array.Accelerate.LLVM.Native      as CPU
#if HAVE_PTX
import qualified Data.Array.Accelerate.LLVM.PTX         as PTX
#endif
import Data.Array.Accelerate.IO ( Vectors, fromVectors, toVectors)
import Prelude as P
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)
import GHC.Conc ( numCapabilities )


class LiftableFunc f where
  type LiftConstr f :: Constraint
  liftFunc :: f -> Lifted


mapExisting :: LiftableFunc f => f -> SomeFactory
mapExisting liftable = someFactory $ \query -> case liftFunc liftable of
  Lift1 opener (fun :: Exp a -> Exp b) -> do
    bandIn <- opener query
    funAcc <- getRun1 (A.map fun)
    bandNd <- bandNodataValue (bandAs bandIn (dataType (Proxy :: Proxy b)))
    mkDataset bandIn [
      HSRasterBand { blockSize = bsize
                   , nodata = bandNd
                   , readBlock = mkReadBlock1 funAcc bandIn
                   }

      ]

  where
    bsize = 256 :+: 256 :: Size
    mkDataset :: Band s a t -> [HSRasterBand s] -> GDAL s (HSDataset s)
    mkDataset protoBand rasterBands = do
      srsIn <- bandProjection protoBand
      gtIn <- fromMaybe (Geotransform 0 1 0 0 0 1) <$> bandGeotransform protoBand
      return HSDataset
        { rasterSize   = bandSize protoBand
        , bands        = rasterBands
        , srs          = srsIn
        , geotransform = gtIn
        }

    mkReadBlock1
      :: forall s m a b. (MonadIO m, Lift1Constr a b)
      => (Array DIM2 a -> Array DIM2 b)
      -> ROBand s a
      -> BlockIx
      -> m (St.Vector b)
    mkReadBlock1 fun band ix = do
      vIn :: St.Vector a <- I.readBandBlockTranslated band bsize ix
      let blockSh = let nx:+:ny = bsize in Z :. nx :. ny
      return (toVectors . fun . fromVectors blockSh $ vIn)


type Lift1Constr a b = ( GDALType a, GDALType b, NFData b
                       , Elt a, Elt b
                       , Vectors (EltRepr a) ~ St.Vector a
                       , Vectors (EltRepr b) ~ St.Vector b
                       , Lift Exp b
                       )

type Opener a = forall s. QueryText -> GDAL s (ROBand s a)

data Lifted where
  Lift1 :: Lift1Constr a b
        => Opener a
        -> (Exp a -> Exp b)
        -> Lifted


instance Lift1Constr a b  => LiftableFunc (Exp a -> Exp b) where
  type LiftConstr (Exp a -> Exp b) = Lift1Constr a b
  liftFunc = Lift1 $ \query ->
    let path = fromMaybe (error "Need to provide a path argument")
             $ join (lookup "path" query)
        bandStr = fromMaybe "1" (join (lookup "band" query))
        band = fromMaybe (error "band must be an integer") (readMaybe (T.unpack bandStr))
    in openReadOnly (T.unpack path) (dataType (Proxy :: Proxy a)) >>= getBand band


getRun1, getRun1CPU
  :: (MonadIO m, Arrays a, Arrays b)
  => (Acc a -> Acc b) -> m (a -> b)
getRun1CPU acc = do
  liftIO (hPutStrLn stderr ("CPU.run1 " P.++ show numCapabilities))
  target <- liftIO $ case 1 of
    1 -> CPU.createTarget [0]      CPU.unbalancedParIO
    n -> CPU.createTarget [0..n-1] CPU.unbalancedParIO
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
