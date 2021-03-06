{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Node.Chairman
  ( tests
  ) where

import           Cardano.Prelude
import           Hedgehog (Property, discover)

import qualified Hedgehog as H
import qualified System.Directory as IO
import qualified System.IO as IO
import qualified Test.Common.Base as H
import qualified Test.Common.Process as H

prop_spawnOneNode :: Property
prop_spawnOneNode = H.propertyOnce . H.workspace "temp/chairman" $ \tempDir -> do
  let dbDir = tempDir <> "/db/node-2"
  let socketDir = tempDir <> "/socket"

  H.createDirectoryIfMissing dbDir
  H.createDirectoryIfMissing socketDir

  base <- H.getProjectBase

  dirContents <- liftIO $ IO.listDirectory base

  H.annotateShow $ dirContents

  (Just hIn, _mOut, _mErr, hProcess) <- H.createProcess =<< H.procNode
    [ "run"
    , "--database-path", dbDir
    , "--socket-path", socketDir <> "/node-2-socket"
    , "--port", "3002"
    , "--topology", base <> "/configuration/chairman/defaults/simpleview/topology-node-2.json"
    , "--config", base <> "/configuration/chairman/defaults/simpleview/config-2.yaml"
    , "--signing-key", base <> "/configuration/chairman/defaults/simpleview/genesis/delegate-keys.002.key"
    , "--delegation-certificate", base <> "/configuration/chairman/defaults/simpleview/genesis/delegation-cert.002.json"
    , "--shutdown-ipc", "0"
    ]

  H.threadDelay 10000000

  liftIO $ IO.hClose hIn

  void $ H.waitForProcess hProcess

tests :: IO Bool
tests = H.checkParallel $$discover
