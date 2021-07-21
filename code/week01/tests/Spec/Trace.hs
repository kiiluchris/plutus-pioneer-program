{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Trace where

import Control.Monad (void)
import qualified Control.Monad.Freer.Extras as Extras
import Data.Default
import qualified Data.Map as M
import qualified Ledger.Ada as Ada
import Ledger.TimeSlot
import qualified Ledger.Value as Value
import Plutus.Trace.Emulator as Emulator
import Wallet.Emulator.Wallet
import Week01.EnglishAuction

currency :: Value.CurrencySymbol
currency = Value.CurrencySymbol "ff"

token1 :: Value.TokenName
token1 = Value.TokenName "T"

token2 :: Value.TokenName
token2 = Value.TokenName "E"

assetClass1 :: Value.AssetClass
assetClass1 = Value.AssetClass (currency, token1)

assetClass2 :: Value.AssetClass
assetClass2 = Value.AssetClass (currency, token2)

defaultWalletFunds :: Integer
defaultWalletFunds = 1_000_000_000

emCfg :: EmulatorConfig
emCfg = EmulatorConfig $ Left $ M.fromList [(Wallet i, v i) | i <- [1 .. 4]]
  where
    v :: Integer -> Value.Value
    v x =
      let tokenFunds = if x == 1 then 1 else 0
       in Ada.lovelaceValueOf defaultWalletFunds
            <> Value.singleton currency token1 tokenFunds
            <> Value.singleton currency token2 tokenFunds

testTrace :: EmulatorTrace () -> IO ()
testTrace = runEmulatorTraceIO' def emCfg

traceAuction :: EmulatorTrace ()
traceAuction = do
  h1 <- Emulator.activateContractWallet (Wallet 1) endpoints
  h2 <- Emulator.activateContractWallet (Wallet 2) endpoints
  h3 <- Emulator.activateContractWallet (Wallet 3) endpoints
  let sp = StartParams (slotToPOSIXTime 20) 100_000_000 currency token1
  let bp1 = BidParams currency token1 100_000_000
  let bp2 = bp1 {bpBid = 200_000_000}
  let cp = CloseParams currency token1

  Emulator.callEndpoint @"start" h1 sp
  void $ Emulator.waitNSlots 1

  Emulator.callEndpoint @"bid" h2 bp1
  void $ Emulator.waitNSlots 1
  Emulator.callEndpoint @"bid" h3 bp2
  void $ Emulator.waitNSlots 1

  Emulator.callEndpoint @"close" h3 cp
  void $ Emulator.waitUntilSlot 21

  Extras.logInfo @String "Auction done"

traceAuctionNoClose :: EmulatorTrace ()
traceAuctionNoClose = do
  h1 <- Emulator.activateContractWallet (Wallet 1) endpoints
  h2 <- Emulator.activateContractWallet (Wallet 2) endpoints
  h3 <- Emulator.activateContractWallet (Wallet 3) endpoints
  let sp = StartParams (slotToPOSIXTime 20) 100_000_000 currency token1
  let bp1 = BidParams currency token1 100_000_000
  let bp2 = bp1 {bpBid = 200_000_000}

  Emulator.callEndpoint @"start" h1 sp
  void $ Emulator.waitNSlots 1

  Emulator.callEndpoint @"bid" h2 bp1
  void $ Emulator.waitNSlots 1
  Emulator.callEndpoint @"bid" h3 bp2
  void $ Emulator.waitNSlots 1

  void $ Emulator.waitUntilSlot 21

  Extras.logInfo @String "Auction done"

traceAuctionConcurrent :: EmulatorTrace ()
traceAuctionConcurrent = do
  h1 <- Emulator.activateContractWallet (Wallet 1) endpoints
  h2 <- Emulator.activateContractWallet (Wallet 2) endpoints
  h3 <- Emulator.activateContractWallet (Wallet 3) endpoints
  h4 <- Emulator.activateContractWallet (Wallet 4) endpoints
  let sp = StartParams (slotToPOSIXTime 20) 100_000_000 currency token1
  let bp1 = BidParams currency token1 100_000_000
  let cp = CloseParams currency token1

  Emulator.callEndpoint @"start" h1 sp
  void $ Emulator.waitNSlots 1

  Emulator.callEndpoint @"bid" h2 bp1
  Emulator.callEndpoint @"bid" h3 bp1
  Emulator.callEndpoint @"bid" h4 bp1
  void $ Emulator.waitNSlots 1

  Emulator.callEndpoint @"close" h3 cp
  void $ Emulator.waitUntilSlot 21

  Extras.logInfo @String "Auction done"
