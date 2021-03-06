{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.TextEnvelope.Keys.GenesisUTxOKeys
  ( golden_shelleyGenesisUTxOKeys
  ) where

import           Cardano.Api.Typed (AsType (..), HasTextEnvelope (..))
import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. Check the TextEnvelope serialization format has not changed.
golden_shelleyGenesisUTxOKeys :: Property
golden_shelleyGenesisUTxOKeys = propertyOnce . moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference keys
  referenceVerKey <- noteInputFile "test/Test/golden/shelley/keys/genesis_utxo_keys/verification_key"
  referenceSignKey <- noteInputFile "test/Test/golden/shelley/keys/genesis_utxo_keys/signing_key"

  -- Key filepaths
  verKey <- noteTempFile tempDir "genesis-utxo-verification-key-file"
  signKey <- noteTempFile tempDir "genesis-utxo-signing-key-file"

  -- Generate payment verification key
  void $ execCardanoCLI
    [ "shelley","genesis","key-gen-utxo"
    , "--verification-key-file", verKey
    , "--signing-key-file", signKey
    ]

  let signingKeyType = textEnvelopeType (AsSigningKey AsGenesisUTxOKey)
      verificationKeyType = textEnvelopeType (AsVerificationKey AsGenesisUTxOKey)

  -- Check the newly created files have not deviated from the
  -- golden files
  checkTextEnvelopeFormat verificationKeyType referenceVerKey verKey
  checkTextEnvelopeFormat signingKeyType referenceSignKey signKey
