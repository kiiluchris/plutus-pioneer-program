{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Spec.FortyTwo.Trace (tests, runTrace, traceMultipleGrabNonValid, traceValidGrab) where

import Control.Lens ((&), (.~))
import Data.Default
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Ledger.Ada as Ada
import Ledger.Value (Value)
import Plutus.Contract.Test
import qualified Plutus.Trace.Emulator as Emulator
import Test.Tasty
import Week02.FortyTwo

basicFundSpentPredicate :: TracePredicate
basicFundSpentPredicate =
  ( walletFundsChange (Wallet 1) (Ada.lovelaceValueOf (- wallet1Gift))
      .&&. walletFundsChange (Wallet 2) (Ada.lovelaceValueOf (- wallet2Gift))
      .&&. walletFundsChange (Wallet 3) (Ada.lovelaceValueOf (- wallet3Gift))
  )

tests :: TestTree
tests =
  testGroup
    "Trace Tests"
    [ checkPredicateOptions
        (defaultCheckOptions & emulatorConfig .~ emCfg)
        "Multiple grabs but none is 42"
        ( basicFundSpentPredicate
            .&&. walletFundsChange (Wallet 4) mempty
            .&&. walletFundsChange (Wallet 5) mempty
        )
        traceMultipleGrabNonValid,
      checkPredicateOptions
        (defaultCheckOptions & emulatorConfig .~ emCfg)
        "Wallet 4 grabs 1st, 5 in next slot"
        ( basicFundSpentPredicate
            .&&. walletFundsChange (Wallet 4) (Ada.lovelaceValueOf totalGift)
            .&&. walletFundsChange (Wallet 5) mempty
        )
        traceValidGrab
    ]

traceValidGrab :: Emulator.EmulatorTrace ()
traceValidGrab = do
  (_, _, _, h4, h5) <- baseTrace
  Emulator.callEndpoint @"grab" h4 42
  _ <- Emulator.waitNSlots 1

  Emulator.callEndpoint @"grab" h5 42
  _ <- Emulator.waitNSlots 1

  return ()

traceMultipleGrabNonValid :: Emulator.EmulatorTrace ()
traceMultipleGrabNonValid = do
  (_, _, _, h4, h5) <- baseTrace
  Emulator.callEndpoint @"grab" h4 35
  Emulator.callEndpoint @"grab" h5 100
  _ <- Emulator.waitNSlots 1

  Emulator.callEndpoint @"grab" h4 1
  Emulator.callEndpoint @"grab" h5 1000
  _ <- Emulator.waitNSlots 1

  return ()

baseTrace ::
  Emulator.EmulatorTrace
    ( Emulator.ContractHandle () GiftSchema Text,
      Emulator.ContractHandle () GiftSchema Text,
      Emulator.ContractHandle () GiftSchema Text,
      Emulator.ContractHandle () GiftSchema Text,
      Emulator.ContractHandle () GiftSchema Text
    )
baseTrace = do
  h1 <- Emulator.activateContractWallet (Wallet 1) endpoints
  h2 <- Emulator.activateContractWallet (Wallet 2) endpoints
  h3 <- Emulator.activateContractWallet (Wallet 3) endpoints
  h4 <- Emulator.activateContractWallet (Wallet 4) endpoints
  h5 <- Emulator.activateContractWallet (Wallet 5) endpoints
  _ <- Emulator.waitNSlots 1

  Emulator.callEndpoint @"give" h1 wallet1Gift
  Emulator.callEndpoint @"give" h2 wallet2Gift
  Emulator.callEndpoint @"give" h3 wallet3Gift
  _ <- Emulator.waitNSlots 1

  return (h1, h2, h3, h4, h5)

runTrace :: Emulator.EmulatorTrace () -> IO ()
runTrace = Emulator.runEmulatorTraceIO' def emCfg

emCfg :: Emulator.EmulatorConfig
emCfg = Emulator.EmulatorConfig $ Left $ Map.fromList [(Wallet i, v) | i <- [1 .. 5]]
  where
    v :: Value
    v = Ada.lovelaceValueOf initialFunds

initialFunds :: Integer
initialFunds = 1_000_000_000

wallet1Gift, wallet2Gift, wallet3Gift :: Integer
wallet1Gift = 100_000_000
wallet2Gift = 200_000_000
wallet3Gift = 300_000_000

totalGift :: Integer
totalGift = wallet1Gift + wallet2Gift + wallet3Gift
