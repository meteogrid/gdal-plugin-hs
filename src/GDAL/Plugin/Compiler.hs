{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE GADTs #-}
module GDAL.Plugin.Compiler (
    CompilerEnv (..)
  , Compiler
  , Result (..)
  , startCompilerWith
  , startCompiler
  , stopCompiler
  , compile
) where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception ( IOException, Handler(Handler), catches
                         , SomeException )
import Control.Monad (void)
import Control.Monad.IO.Class ( liftIO )
import Data.Char (isDigit)
import Data.Text (Text)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import Data.Typeable (Typeable, typeOf)
import Data.Monoid (mempty, mappend)
import Data.Default (Default(..))
import GHC hiding (importPaths)
import GHC.Paths (libdir)
import ErrUtils as GHC
import HscTypes as GHC
import Outputable as GHC hiding (parens)
import DynFlags as GHC
import GHC.Exts (unsafeCoerce#)

data Compiler = Compiler
  { compilerTid  :: ThreadId
  , compilerChan :: Chan Request
  }

data CompilerEnv
 = CompilerEnv {
     envLibdir      :: FilePath
   , envImports     :: [String]
   , envSearchPath  :: [FilePath]
   } deriving (Show)

instance Default CompilerEnv where
  def = CompilerEnv
    { envLibdir     = libdir
    , envImports    = []
    , envSearchPath = ["."]
    }

data Request where
  Compile :: forall a. Typeable a
          => [String]
          -> String
          -> MVar (Result a)
          -> Request

type CompilerMessages = Text

data Result a where
  Success :: a             -> CompilerMessages -> Result a
  Failure :: SomeException -> CompilerMessages -> Result a


startCompiler :: IO (Either String Compiler)
startCompiler = startCompilerWith def

startCompilerWith :: CompilerEnv -> IO (Either String Compiler)
startCompilerWith = undefined

stopCompiler :: Compiler -> IO (Either String ())
stopCompiler = undefined

compile
  :: forall a. Typeable a
  => Compiler
  -> [String]
  -> String
  -> IO (Result a)
compile = undefined


{-
interpretWith
  :: forall a. Typeable a
  => CompilerEnv
  -> [String]
  -> String
  -> IO (Either Text a)
interpretWith env targets code = do
  logRef <- newIORef mempty :: (IO (IORef Builder))
  let compileAndLoad = do
        dflags <- getSessionDynFlags
        let dflags' = updateWays
                    . dynamicTooMkDynamicDynFlags
                    . updOptLevel 2
                    . addOptl "-lHSrts_thr-ghc8.0.1"
                    -- . setGeneralFlag' Opt_WarnIsError
                    . flip (foldl wopt_set) [toEnum 0 ..] -- sets all warnings
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
                      , log_action    = mkLogHandler logRef
                      , verbosity     = 1
                      }
        void $ setSessionDynFlags dflags'
        setTargets =<< mapM (`guessTarget` Nothing) targets
        void $ load LoadAllTargets
        importModules (envImports env ++ targets)
        fmap (Right . unsafeCoerce#) $
          compileExpr $ parens code ++ " :: " ++ show (typeOf (undefined :: a))
      handleEx e = do
        msg <- LT.toStrict . toLazyText <$> readIORef logRef
        return $ Left (if not (T.null msg) then msg else T.pack e)

  runGhc (Just (envLibdir env)) compileAndLoad `catches` [
        Handler (\(e :: SourceError) -> handleEx (show e))
      , Handler (\(e :: GhcApiError) -> handleEx (show e))
      , Handler (\(e :: IOException) -> handleEx (show e))
    ]

-}


importModules:: [String] -> Ghc ()
importModules =
  GHC.setContext . map (GHC.IIDecl . import_)
  where 
    import_ name =
      ( GHC.simpleImportDecl . GHC.mkModuleName $ name )
      { GHC.ideclQualified = False }

--
-- |stolen from hint
parens :: String -> String
parens s = concat ["(let {", foo, " =\n", s, "\n;} in ", foo, ")"]
  where foo = "e_1" ++ filter isDigit s

mkLogHandler :: IORef Builder -> DynFlags -> t -> Severity -> SrcSpan -> PprStyle -> MsgDoc -> IO ()
mkLogHandler r df _ severity src style msg =
    let renderErrMsg = GHC.showSDoc df
        errorEntry = mkGhcError renderErrMsg severity src style msg
    in modifyIORef r (`mappend` mappend errorEntry "\n")


mkGhcError :: (GHC.SDoc -> String) -> GHC.Severity -> GHC.SrcSpan -> GHC.PprStyle -> GHC.MsgDoc -> Builder
mkGhcError render severity src_span style msg = fromString niceErrMsg
    where niceErrMsg = render . GHC.withPprStyle style $
                         GHC.mkLocMessage severity src_span msg

addOptl :: String -> DynFlags -> DynFlags
addOptl f = alterSettings (\s -> s { sOpt_l   = f : sOpt_l s})

alterSettings :: (Settings -> Settings) -> DynFlags -> DynFlags
alterSettings f dflags = dflags { settings = f (settings dflags) }

