module GDAL.Plugin.Internal (
  readBandBlock
) where

import GDAL hiding (readBandBlock)
import GDAL.Internal.GDAL (RasterBandH (..), unBand)

import           Control.Monad
import qualified Data.Vector.Storable as St
import qualified Data.Vector.Storable.Mutable as Stm

import Foreign.C.Types
import Foreign.Ptr (Ptr, castPtr)

readBandBlock band ( i :+: j ) = liftIO $ do
  mVec <- Stm.unsafeNew (bandBlockLen band)
  void $ Stm.unsafeWith mVec $ \pBuf ->
    c_readBandBlock
          (castPtr ((\(RasterBandH b) -> b) (unBand band)))
          (fromIntegral i)
          (fromIntegral j)
          (castPtr pBuf)
  St.unsafeFreeze mVec

foreign import ccall "GDALReadBlock" c_readBandBlock ::
  Ptr () -> CInt -> CInt -> Ptr () -> IO CInt
