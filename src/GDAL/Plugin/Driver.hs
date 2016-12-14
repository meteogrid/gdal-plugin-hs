{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GDAL.Plugin.Driver ( mkDriver, registerDriver ) where

import           GDAL.Plugin.Types
import           GDAL.Plugin.Compiler

import           GDAL
import           GDAL.Internal.GDAL
import           GDAL.Internal.HSDriver

import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe ( fromMaybe )
import           Data.Monoid ((<>))
import           Data.Proxy
import           Data.Typeable
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Network.HTTP.Types.URI ( parseQueryText )
import           System.IO (hPrint, stderr)

mkDriver :: String -> DriverName -> CompilerConfig -> IO (Driver ReadWrite)
mkDriver ctr name@(DriverName bsname) cfg = do
  compiler <- startCompilerWith cfg
    { cfgImports = cfgImports cfg ++ ["GDAL.Plugin"]
    }
  drv <- createDriver HSDriver
    { hsdName     = name
    , hsdIdentify = return . identify
    , hsdOpen     = doOpen (compile compiler)
    }
  mapM_ (\(k,v) -> setMetadataItem Nothing k v drv) meta
  return drv
  where
    identify s = BS.isPrefixOf (bsname<>":") s || BS.isSuffixOf ".hs" s
    meta = [ ("DCAP_RASTER", "YES")
           , ("DMD_LONGNAME", "Haskell programs")
           , ("DMD_SUBDATASETS", "YES")
           , ("DMD_EXTENSION", "hs")
           ]

    doOpen compile' arg = case BS.split ':' arg of
      [bsname', modOrSrc]
        | bsname==bsname'       -> loadMod modOrSrc []
      [bsname', query, modOrSrc]
        | bsname==bsname'       -> loadMod modOrSrc (parseQueryText query)
      [modOrSrc]                -> loadMod modOrSrc []
      [query, modOrSrc]         -> loadMod modOrSrc (parseQueryText query)
      _                         -> return HsdError
      where
        loadMod modOrSrc query = do
          let modName = BS.unpack $
                    fromMaybe modOrSrc (BS.stripSuffix ".hs" modOrSrc)
              (mSymName, query') = popArg "variable" query
              symName = maybe "dataset" T.unpack mSymName
              code = "getFactory (" ++ ctr ++ " " ++ symName  ++ ")"
          eSym <- compile' [modName] code
          case eSym of
            Success factory messages -> do
              T.hPutStrLn stderr messages
              return (factory query')
            Failure exc messages  -> do
              hPrint stderr exc
              T.hPutStrLn stderr messages
              return HsdError

popArg :: Eq a => a -> [(a, Maybe b)] -> (Maybe b, [(a, Maybe b)])
popArg key query = (join (lookup key query), filter ((/=key) . fst) query)
