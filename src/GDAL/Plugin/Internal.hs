{-# LANGUAGE ScopedTypeVariables #-}
module GDAL.Plugin.Internal (
  readBandBlock
, readBandBlockTranslated
) where

import           GDAL hiding (readBandBlock)
import           GDAL.Internal.GDAL hiding (readBandBlock)
import           GDAL.Internal.Util (fromEnumC)

import           Control.Monad
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Data.Proxy (Proxy(Proxy))
import qualified Data.Vector.Storable as St
import qualified Data.Vector.Storable.Mutable as Stm

import            Foreign.C.Types
import            Foreign.Ptr (Ptr, castPtr)

readBandBlock
  :: forall a b m s t. (GDALType b, MonadIO m)
  => Band s a t -> BlockIx -> m (St.Vector b)
readBandBlock band ( i :+: j ) = liftIO $ do
  mVec <- Stm.unsafeNew (bandBlockLen band)
  void $ Stm.unsafeWith mVec $ \pBuf ->
    c_readBandBlock
          (bandPtr band)
          (fromIntegral i)
          (fromIntegral j)
          pBuf
  St.unsafeFreeze mVec

readBandBlockTranslated
  :: forall a b m s t. (GDALType b, MonadIO m)
  => Band s a t -> Size -> BlockIx -> m (St.Vector b)
readBandBlockTranslated srcBand destBlockSize destBlockIx = liftIO $ do
  mVec <- Stm.unsafeNew buffLen
  when (winLen > 0) $ void $ Stm.unsafeWith mVec $ \pBuf ->
    c_rasterIO
      (bandPtr srcBand)
      0 -- GF_Read
      (pFst srcOffset)
      (pSnd srcOffset)
      (pFst srcWinSz)
      (pSnd srcWinSz)
      pBuf
      (pFst srcWinSz)
      (pSnd srcWinSz)
      (fromEnumC dtype)
      0
      (fromIntegral (sizeOfDataType dtype * pFst destBlockSize))
  St.unsafeFreeze mVec
  where
    dtype      = hsDataType (Proxy :: Proxy b)
    buffLen    = let x :+: y = destBlockSize in x*y
    destWindow = Envelope (destBlockSize * destBlockIx)
                          (destBlockSize * (destBlockIx + 1))
    srcWindow  = Envelope (min <$> envelopeMin destWindow <*> bandSize srcBand)
                          (min <$> envelopeMax destWindow <*> bandSize srcBand)
    srcOffset  = fromIntegral <$> envelopeMin srcWindow
    srcWinSz   = fmap fromIntegral (envelopeMax srcWindow) - srcOffset
    winLen     = let x :+: y = srcWinSz in x*y

bandPtr :: Band s a t -> Ptr band
bandPtr = castPtr . (\(RasterBandH b) -> b) . unBand
  

foreign import ccall "GDALReadBlock" c_readBandBlock ::
  Ptr band -> CInt -> CInt -> Ptr buffer -> IO CInt


foreign import ccall "GDALRasterIO" c_rasterIO
  :: Ptr band
  -> CInt -- rw flag
  -> CInt -- offx
  -> CInt -- offy
  -> CInt -- winszx
  -> CInt -- winszy
  -> Ptr buffer
  -> CInt -- buffszx
  -> CInt -- buffszy
  -> CInt -- gtype
  -> CInt -- pixelSpace
  -> CInt -- lineSpace
  -> IO CInt
