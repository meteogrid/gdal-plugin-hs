{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE GADTs #-}
module GDAL.Plugin.Compiler (
    CompilerEnv (..)
  , Linkage (..)
  , Compiler
  , Result (..)
  , startCompilerWith
  , startCompiler
  , stopCompiler
  , compile
  , def
) where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception ( IOException, Handler(Handler), catches
                         , SomeException )
import Control.Monad (void, forever)
import Control.Monad.IO.Class ( liftIO )
import Data.Char (isDigit)
import Data.Text (Text)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.IORef (IORef, newIORef, writeIORef, readIORef, modifyIORef)
import Data.Typeable (Typeable, typeOf)
import Data.Monoid (mempty, mappend)
import Data.Default (Default(..))
import GHC hiding (importPaths)
import GHC.Paths (libdir)
import ErrUtils as GHC
import HscTypes as GHC
import Exception ( gtry )
import Outputable as GHC hiding (parens)
import DynFlags as GHC
import GHC.Exts (unsafeCoerce#)

data Compiler = Compiler
  { compilerTid  :: ThreadId
  , compilerChan :: Chan Request
  }

data Linkage = LinkDyn | LinkRTS
  deriving Show

data CompilerEnv = CompilerEnv
  { envLibdir      :: FilePath
  , envImports     :: [String]
  , envSearchPath  :: [FilePath]
  , envLinkage     :: Linkage
  } deriving Show

instance Default CompilerEnv where
  def = CompilerEnv
    { envLibdir     = libdir
    , envImports    = []
    , envSearchPath = ["."]
    , envLinkage    = LinkRTS
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


startCompiler :: IO Compiler
startCompiler = startCompilerWith def

startCompilerWith :: CompilerEnv -> IO Compiler
startCompilerWith env = do
  chan <- newChan
  tid <- forkIO (compilerThread chan env)
  return (Compiler tid chan)

stopCompiler :: Compiler -> IO ()
stopCompiler = killThread . compilerTid

compile
  :: forall a. Typeable a
  => Compiler
  -> [String]
  -> String
  -> IO (Result a)
compile comp targets code = do
  resRef <- newEmptyMVar
  writeChan (compilerChan comp) (Compile targets code resRef)
  takeMVar resRef

compilerThread :: Chan Request -> CompilerEnv -> IO ()
compilerThread chan env = do
  logRef <- newIORef mempty :: (IO (IORef Builder))
  runGhc (Just (envLibdir env)) $ do
    dflags <- getSessionDynFlags
    let dflags' = updOptLevel 2
                . case envLinkage env of
                    LinkDyn -> addOptl "-lHSrts_thr-ghc8.0.1"
                             . dynamicTooMkDynamicDynFlags
                    LinkRTS -> id
                $ dflags {
                    mainFunIs     = Nothing
                  , safeHaskell   = Sf_Safe
                  , outputHi      = Nothing
                  , outputFile    = Nothing
                  , ghcLink       = LinkInMemory
                  , ghcMode       = CompManager
                  --, ways          = [ WayDyn, WayThreaded ]
                  , hscTarget     = HscAsm
                  --, objectDir     = Just dir
                  --, hiDir         = Just dir
                  , importPaths   = envSearchPath env
                  , log_action    = mkLogHandler logRef
                  , verbosity     = 0
                  }
    void $ setSessionDynFlags dflags'
    setGhcOptions defaultGhcOptions
    forever $ do
      req <- liftIO (readChan chan)
      case req of
        Compile targets code resRef -> do
          liftIO (writeIORef logRef mempty)
          eRes <- gtry (compileTargets env targets code)
          liftIO $ do
            msgs <- LT.toStrict . toLazyText
                <$> readIORef logRef
            writeIORef logRef mempty
            putMVar resRef $ case eRes of
              Right r -> Success r msgs
              Left  e -> Failure e msgs


defaultGhcOptions :: [String]
defaultGhcOptions = [ "-fwarn-incomplete-patterns"
                    , "-fwarn-incomplete-uni-patterns"
                    , "-funbox-strict-fields"
                    , "-Wall"
                    ]
compileTargets
  :: forall a. Typeable a
  => CompilerEnv -> [String] -> String -> Ghc a
compileTargets env targets code = do
  liftIO rts_revertCAFs -- make sure old modules can be unloaded
  setTargets =<< mapM (`guessTarget` Nothing) targets
  void $ load LoadAllTargets
  importModules (envImports env ++ targets)
  unsafeCoerce# <$>
    compileExpr (parens code ++ " :: " ++ show (typeOf (undefined :: a)))

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

setGhcOptions :: [String] -> Ghc ()
setGhcOptions opts = do
  old_flags <- GHC.getSessionDynFlags
  (new_flags,not_parsed) <- pdf old_flags opts
  void $ GHC.setSessionDynFlags (updateWays new_flags)
  where
    pdf d = fmap firstTwo . GHC.parseDynamicFlags d . map GHC.noLoc
    firstTwo (a,b,_) = (a, map GHC.unLoc b)

foreign import ccall "revertCAFs" rts_revertCAFs  :: IO ()
