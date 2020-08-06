{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Common.Process
  ( createProcess
  , execFlex
  , getProjectBase
  , procCli
  , procNode
  , execCli
  , interruptProcessGroupOf
  , waitForProcess
  ) where

import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bool
import           Data.Either
import           Data.Function
import           Data.Int
import           Data.Maybe (Maybe (..))
import           Data.Monoid
import           Data.Semigroup ((<>))
import           Data.String (String)
import           GHC.Stack (CallStack, HasCallStack)
import           Hedgehog (MonadTest)
import           Hedgehog.Internal.Property (Diff, liftTest, mkTest)
import           Hedgehog.Internal.Source (getCaller)
import           System.Exit (ExitCode)
import           System.IO (Handle, IO)
import           System.Process (CmdSpec (..), CreateProcess (..), ProcessHandle)
import           Text.Show

import qualified Data.List as L
import qualified GHC.Stack as GHC
import qualified Hedgehog as H
import qualified Hedgehog.Internal.Property as H
import qualified System.Environment as IO
import qualified System.Exit as IO
import qualified System.Process as IO

-- | Format argument for a shell CLI command.
--
-- This includes automatically embedding string in double quotes if necessary, including any necessary escaping.
--
-- Note, this function does not cover all the edge cases for shell processing, so avoid use in production code.
argQuote :: String -> String
argQuote arg = if ' ' `L.elem` arg || '"' `L.elem` arg || '$' `L.elem` arg
  then "\"" <> escape arg <> "\""
  else arg
  where escape :: String -> String
        escape ('"':xs) = '\\':'"':escape xs
        escape ('\\':xs) = '\\':'\\':escape xs
        escape ('\n':xs) = '\\':'n':escape xs
        escape ('\r':xs) = '\\':'r':escape xs
        escape ('\t':xs) = '\\':'t':escape xs
        escape ('$':xs) = '\\':'$':escape xs
        escape (x:xs) = x:escape xs
        escape "" = ""

createProcess :: HasCallStack
  => CreateProcess
  -> H.PropertyT IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess cp = GHC.withFrozenCallStack $ do
  case IO.cmdspec cp of
    RawCommand cmd args -> H.annotate $ "Command line: " <> cmd <> " " <> L.intercalate " " args
    ShellCommand cmd -> H.annotate $ "Command line: " <> cmd
  H.evalM . liftIO $ IO.createProcess cp

execFlex :: HasCallStack
  => String
  -> String
  -> [String]
  -> H.PropertyT IO String
execFlex pkgBin envBin arguments = do
  maybeCardanoCli <- liftIO $ IO.lookupEnv envBin
  (exitResult, stdout, stderr) <- case maybeCardanoCli of
    Just cardanoCli -> liftIO $ IO.readProcessWithExitCode cardanoCli arguments ""
    Nothing -> liftIO $ IO.readProcessWithExitCode "cabal" ("exec":"--":pkgBin:arguments) ""
  case exitResult of
    IO.ExitFailure exitCode -> failWithCustom GHC.callStack Nothing . L.unlines $
      [ "Process exited with non-zero exit-code"
      , "━━━━ command ━━━━"
      , pkgBin <> " " <> L.intercalate " " (fmap argQuote arguments)
      , "━━━━ stdout ━━━━"
      , stdout
      , "━━━━ stderr ━━━━"
      , stderr
      , "━━━━ exit code ━━━━"
      , show @Int exitCode
      ]
    IO.ExitSuccess -> return stdout

execCli :: HasCallStack => [String] -> H.PropertyT IO String
execCli = execFlex "cardano-cli" "CARDANO_CLI"

interruptProcessGroupOf :: HasCallStack
  => ProcessHandle
  -> H.PropertyT IO ()
interruptProcessGroupOf hProcess = GHC.withFrozenCallStack $ do
  H.evalM . liftIO $ IO.interruptProcessGroupOf hProcess

waitForProcess :: HasCallStack
  => ProcessHandle
  -> H.PropertyT IO (Maybe ExitCode)
waitForProcess hProcess = GHC.withFrozenCallStack $ do
  H.evalM . liftIO $ catch (fmap Just (IO.waitForProcess hProcess)) $ \(_ :: AsyncCancelled) -> return Nothing

procFlex
  :: HasCallStack
  => String
  -- ^ Cabal package name corresponding to the executable
  -> String
  -- ^ Environment variable pointing to the binary to run
  -> [String]
  -- ^ Arguments to the CLI command
  -> H.PropertyT IO CreateProcess
  -- ^ Captured stdout
procFlex pkg binaryEnv arguments = GHC.withFrozenCallStack . H.evalM $ do
  maybeCardanoCli <- liftIO $ IO.lookupEnv binaryEnv
  cp <- case maybeCardanoCli of
    Just cardanoCli -> return $ IO.proc cardanoCli arguments
    Nothing -> return $ IO.proc "cabal" ("exec":"--":pkg:arguments)
  return $ cp
    { IO.create_group = True
    , IO.std_in = IO.CreatePipe
    }

procCli
  :: HasCallStack
  => [String]
  -- ^ Arguments to the CLI command
  -> H.PropertyT IO CreateProcess
  -- ^ Captured stdout
procCli = procFlex "cardano-cli" "CARDANO_CLI"

procNode
  :: HasCallStack
  => [String]
  -- ^ Arguments to the CLI command
  -> H.PropertyT IO CreateProcess
  -- ^ Captured stdout
procNode = procFlex "cardano-node" "CARDANO_NODE"

getProjectBase :: H.PropertyT IO String
getProjectBase = do
  maybeCardanoCli <- liftIO $ IO.lookupEnv "CARDANO_NODE_SRC"
  case maybeCardanoCli of
    Just path -> return path
    Nothing -> return ".."

-- | Takes a 'CallStack' so the error can be rendered at the appropriate call site.
failWithCustom :: MonadTest m => CallStack -> Maybe Diff -> String -> m a
failWithCustom cs mdiff msg =
  liftTest $ mkTest (Left $ H.Failure (getCaller cs) msg mdiff, mempty)
