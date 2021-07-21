{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Burn (tests, trace, runTrace) where

import Control.Lens ((&), (.~))
import Data.Default
import qualified Data.Map as Map
import qualified Ledger.Ada as Ada
import Ledger.Value (Value)
import Plutus.Contract.Test
import qualified Plutus.Trace.Emulator as Emulator
import Test.Tasty
import Week02.Burn

wallet1Amount, wallet2Amount, wallet3Amount :: Integer
wallet1Amount = 10_000_000
wallet2Amount = 20_000_000
wallet3Amount = 30_000_000

runTrace :: IO ()
runTrace = Emulator.runEmulatorTraceIO' def emCfg trace

trace :: Emulator.EmulatorTrace ()
trace = do
  h1 <- Emulator.activateContractWallet (Wallet 1) endpoints
  h2 <- Emulator.activateContractWallet (Wallet 2) endpoints
  h3 <- Emulator.activateContractWallet (Wallet 3) endpoints
  h4 <- Emulator.activateContractWallet (Wallet 4) endpoints

  Emulator.callEndpoint @"give" h1 wallet1Amount
  Emulator.callEndpoint @"give" h2 wallet2Amount
  Emulator.callEndpoint @"give" h3 wallet3Amount
  _ <- Emulator.waitNSlots 1

  Emulator.callEndpoint @"grab" h4 ()

emCfg :: Emulator.EmulatorConfig
emCfg = Emulator.EmulatorConfig $ Left $ Map.fromList [(Wallet i, v) | i <- [1 .. 5]]
  where
    v :: Value
    v = Ada.lovelaceValueOf 1_000_000_000

tests :: TestTree
tests =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "Burn 3 wallets funds to remain in script address"
    ( walletFundsChange (Wallet 1) (Ada.lovelaceValueOf (- wallet1Amount))
        .&&. walletFundsChange (Wallet 2) (Ada.lovelaceValueOf (- wallet2Amount))
        .&&. walletFundsChange (Wallet 3) (Ada.lovelaceValueOf (- wallet3Amount))
        .&&. walletFundsChange (Wallet 4) mempty
    )
    trace
