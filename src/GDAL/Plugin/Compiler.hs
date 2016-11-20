{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module GDAL.Plugin.Compiler (
    EvalEnv (..)
  , interpret
  , interpretWithEnv
) where

#define COMPILE_PLUGINS
#define DYNAMIC_LINKING

import Control.Exception (IOException, Handler(Handler), catches)
import Control.Monad (void)
import Control.Monad.IO.Class ( liftIO )
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Builder (Builder, toLazyText , fromText)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Typeable (Typeable, typeOf)
import Data.Monoid (mempty, mappend)
import Data.Default (Default(..))
import GHC hiding (importPaths)
import GHC.Paths (libdir)
import ErrUtils
import HscTypes
import Outputable (initSDocContext, runSDoc)
import DynFlags
import System.IO.Temp (withSystemTempDirectory)
import Unsafe.Coerce (unsafeCoerce)

data EvalEnv
 = EvalEnv {
     envLibdir      :: FilePath
   , envTmpDirTpl   :: FilePath
   , envSearchPath  :: [FilePath]
   , envImports     :: [String]
   , envTargets     :: [String]
   } deriving (Show)


instance Default EvalEnv where
  def
    = EvalEnv {
        envLibdir      = libdir
      , envTmpDirTpl   = ".gdal-hs"
      , envSearchPath  = []
      , envImports     = []
      , envTargets     = []
      }

addOptl   f = alterSettings (\s -> s { sOpt_l   = f : sOpt_l s})

alterSettings :: (Settings -> Settings) -> DynFlags -> DynFlags
alterSettings f dflags = dflags { settings = f (settings dflags) }

interpret
  :: forall a. Typeable a
  => String
  -> IO (Either [Text] a)
interpret = interpretWithEnv def

interpretWithEnv
  :: forall a. Typeable a
  => EvalEnv
  -> String
  -> IO (Either [Text] a)
interpretWithEnv env code = withSystemTempDirectory tpl $ \dir -> do
  logRef <- newIORef mempty :: (IO (IORef Builder))
  let compileAndLoad = do
        dflags <- getSessionDynFlags
        let dflags' = updateWays
                    . dynamicTooMkDynamicDynFlags
                    . updOptLevel 2
                    . setTmpDir dir
                    . setGeneralFlag' Opt_PIC
                    . addOptl "-lHSrts_thr-ghc8.0.1"
                    $ dflags {
                        mainFunIs     = Nothing
                      , safeHaskell   = Sf_Safe
                      , outputHi      = Nothing
                      , outputFile    = Nothing
                      , ghcLink       = LinkInMemory
                      , ghcMode       = CompManager
                      , ways          = [ WayDyn, WayThreaded ]
                      , hscTarget     = HscAsm
                      --, objectDir     = Just dir
                      --, hiDir         = Just dir
                      , importPaths   = envSearchPath env
                      --, log_action    = logHandler logRef
                      , verbosity     = 1
                      }
        void $ setSessionDynFlags dflags'
        defaultCleanupHandler dflags' $ do
          targets <- mapM (`guessTarget` Nothing) (envTargets env)
          setTargets targets
          void $ load LoadAllTargets
          importModules (envImports env)
          fmap (Right . unsafeCoerce) $
            compileExpr $ parens code ++ " :: " ++ show (typeOf (undefined :: a))
      handleEx e = do
        msg <- (map LT.toStrict . LT.lines . toLazyText) <$> readIORef logRef
        return $ Left (if not (null msg) then msg else [T.pack e])

  runGhc (Just (envLibdir env)) compileAndLoad `catches` [
        Handler (\(e :: SourceError) -> handleEx (show e))
      , Handler (\(e :: GhcApiError) -> handleEx (show e))
      , Handler (\(e :: IOException) -> handleEx (show e))
    ]
  where
    tpl = envTmpDirTpl env

importModules:: [String] -> Ghc ()
importModules =
  GHC.setContext . map (GHC.IIDecl . import_)
  where 
    import_ name =
      ( GHC.simpleImportDecl . GHC.mkModuleName $ name )
      { GHC.ideclQualified = False }


{-
-- from http://parenz.wordpress.com/2013/07/23/on-custom-error-handlers-for-ghc-api/
logHandler ref dflags reason srcSpan style msg =
  modifyIORef' ref (mappend printDoc)
  where cntx = initSDocContext dflags style
        locMsg = mkLocMessage severity srcSpan msg
        printDoc = fromText . T.pack . show $ runSDoc locMsg cntx
-}
-- |stolen from hint
parens :: String -> String
parens s = concat ["(let {", foo, " =\n", s, "\n;} in ", foo, ")"]
  where foo = "e_1" ++ filter isDigit s
