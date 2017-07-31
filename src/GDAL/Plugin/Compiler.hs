{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE GADTs #-}
module GDAL.Plugin.Compiler (
    CompilerConfig (..)
  , Compiler
  , Result (..)
  , startCompilerWith
  , startCompiler
  , stopCompiler
  , compile
  -- * Re-exports
  , HscTarget (..)
  , def
) where

import Control.Concurrent
import Control.Exception ( SomeException )
import Control.Monad (void, forever)
import Control.Monad.IO.Class ( MonadIO (..) )
import Data.Char (isDigit)
import Data.Text (Text)
import Data.String (fromString)
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.IORef (IORef, newIORef, writeIORef, readIORef, modifyIORef)
import Data.Typeable (Typeable, typeOf)
import Data.Monoid (mempty, mappend)
import Data.Default (Default(..))
import GHC hiding (importPaths)
import GHC.Paths (libdir)
import ErrUtils as GHC
import Exception ( ExceptionMonad, gtry, gmask )
import Outputable as GHC hiding (parens)
import DynFlags as GHC
import GHC.Exts (unsafeCoerce#)

data Compiler = Compiler
  { compilerTid       :: ThreadId
  , compilerChan      :: Chan Request
  }


data CompilerConfig = CompilerConfig
  { cfgLibdir      :: FilePath
  , cfgImports     :: [String]
  , cfgSearchPath  :: [FilePath]
  , cfgOptions     :: [String]
  , cfgSafeModeOn  :: Bool
  , cfgVerbosity   :: Int
  , cfgBuildDir    :: FilePath
  , cfgTarget      :: HscTarget
  } deriving Show

instance Default CompilerConfig where
  def = CompilerConfig
    { cfgLibdir     = libdir
    , cfgImports    = []
    , cfgSearchPath = ["."]
    , cfgOptions    = defaultGhcOptions
    , cfgSafeModeOn = True 
    , cfgVerbosity  = 0
    , cfgBuildDir   = "gdal-hs-build"
    , cfgTarget     = HscAsm
    }

data Request where
  Compile :: forall a. Typeable a
          => [String]
          -> String
          -> MVar (Result a)
          -> Request

type CompilerMessages = Text

data Result a
  = Success a             CompilerMessages
  | Failure SomeException CompilerMessages
  deriving Show


startCompiler :: IO Compiler
startCompiler = startCompilerWith def

startCompilerWith :: CompilerConfig -> IO Compiler
startCompilerWith cfg = do
  chan <- newChan
  tid <- forkIO (compilerThread chan cfg)
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

compilerThread :: Chan Request -> CompilerConfig -> IO ()
compilerThread chan cfg = do
  logRef <- newIORef mempty
  runGhc (Just (cfgLibdir cfg)) $ do
    dflags <- getSessionDynFlags
    let dflags' = (if not isInterpreted && GHC.dynamicGhc
                  then addOptl "-lHSrts_thr-ghc8.2.1" --FIXME: Avoid hard-code
                     . dynamicTooMkDynamicDynFlags
                  else id)
                $ dflags {
                    mainFunIs   = Nothing
                  , safeHaskell = if cfgSafeModeOn cfg
                                  then Sf_Safe else Sf_None
                  , ghcLink     = LinkInMemory
                  , ghcMode     = CompManager
                  , hscTarget   = cfgTarget cfg
                  , ways        = ways dflags
                                  ++ [WayThreaded | not isInterpreted]
                  , importPaths = cfgSearchPath cfg
                  , log_action  = mkLogHandler logRef
                  , verbosity   = cfgVerbosity cfg
                  , objectDir   = Just (cfgBuildDir cfg)
                  , stubDir     = Just (cfgBuildDir cfg)
                  , hiDir       = Just (cfgBuildDir cfg)
                  }
        isInterpreted = cfgTarget cfg == HscInterpreted
    setGhcOptions dflags' (cfgOptions cfg)
    forever $ do
      req <- liftIO (readChan chan)
      case req of
        Compile targets code resRef -> gmask $ \restore -> do
          -- gmask to make sure we're not interrupted before putting the result
          -- in the mvar so the requester does not hang (or throw a
          -- "blocked indefinetely on an mvar" exception)
          (eRes, msgs) <- capturingMessages logRef restore $
                          compileTargets cfg targets code
          liftIO . putMVar resRef $ case eRes of
            Right r -> Success r msgs
            Left  e -> Failure e msgs


capturingMessages
  :: (MonadIO m, ExceptionMonad m)
  => IORef Builder -> (m a -> m a) -> m a
  -> m (Either SomeException a, CompilerMessages)
capturingMessages logRef restore act = do
  r <- gtry (restore act)
  liftIO $ do
    msgs <- LT.toStrict . toLazyText <$> readIORef logRef
    writeIORef logRef mempty
    return (r, msgs)

defaultGhcOptions :: [String]
defaultGhcOptions = [ "-fwarn-incomplete-patterns"
                    , "-fwarn-incomplete-uni-patterns"
                    , "-funbox-strict-fields"
                    , "-Wall"
                    , "-O"
                    --, "-fexpose-all-unfoldings"
                    --, "-funfolding-use-threshold500"
                    --, "-funfolding-keeness-factor500"
                    ]
compileTargets
  :: forall a. Typeable a
  => CompilerConfig -> [String] -> String -> Ghc a
compileTargets cfg targets code = do
  liftIO rts_revertCAFs -- make sure old modules can be unloaded
  setTargets =<< mapM (`guessTarget` Nothing) targets
  void $ load LoadAllTargets
  importModules (cfgImports cfg ++ targets)
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

setGhcOptions :: DynFlags -> [String] -> Ghc ()
setGhcOptions old_flags opts = do
  (new_flags,_not_parsed) <- pdf old_flags opts
  void $ GHC.setSessionDynFlags (updateWays new_flags)
  where
    pdf d = fmap firstTwo . GHC.parseDynamicFlags d . map GHC.noLoc
    firstTwo (a,b,_) = (a, map GHC.unLoc b)

foreign import ccall "revertCAFs" rts_revertCAFs  :: IO ()
