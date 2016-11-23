{-# LANGUAGE OverloadedStrings #-}
module GDAL.Plugin.Driver ( mkDriver, registerDriver, installDriver ) where

import           GDAL.Plugin.Types
import           GDAL.Plugin.Compiler

import           GDAL
import           GDAL.Internal.HSDriver

import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.Default ( def )
import           Data.Maybe ( fromMaybe )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Network.HTTP.Types.URI ( parseQueryText )
import           System.IO ( stderr )

installDriver :: IO ()
installDriver = mkDriver >>= registerDriver

mkDriver :: IO (Driver ReadWrite)
mkDriver = do
  compiler <- either fail return
           =<< startCompilerWith (def {envImports =[ "GDAL.Plugin" ]})
  drv <- createDriver HSDriver
    { hsdName     = "HS"
    , hsdIdentify = return . BS.isPrefixOf "HS:"
    , hsdOpen     = doOpen (compile compiler)
    }
  mapM_ (\(k,v) -> setMetadataItem Nothing k v drv) meta
  registerDriver drv
  return drv
  where
    meta = [ ("DCAP_RASTER", "YES")
           , ("DMD_LONGNAME", "Haskell programs")
           , ("DMD_SUBDATASETS", "YES")
           , ("DMD_EXTENSION", "hs")
           ]

    doOpen compile' arg = case BS.split ':' arg of
      ["HS", modOrSrc]        -> loadMod modOrSrc []
      ["HS", query, modOrSrc] -> loadMod modOrSrc (parseQueryText query)
      _                       -> return HsdError
      where
        loadMod modOrSrc query = do
          let modName = BS.unpack $
                    fromMaybe modOrSrc (BS.stripSuffix ".hs" modOrSrc)
              (mSymName, query') = popArg "variable" query
              symName = maybe "dataset" T.unpack mSymName
          eSym <- compile' [modName] ("GDAL.Plugin.SomeHSDatasetFactory " ++ symName)
          case eSym of
            Success factory _ -> return (HsdDataset (getFactory factory query))
            Failure _ messages  -> do
              T.hPutStrLn stderr messages
              return HsdError

popArg :: Eq a => a -> [(a, Maybe b)] -> (Maybe b, [(a, Maybe b)])
popArg key query = (join (lookup key query), filter ((==key) . fst) query)