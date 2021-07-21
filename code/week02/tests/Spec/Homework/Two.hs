{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Homework.Two where

import Control.Lens ((&), (.~))
import Data.Default
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Ledger.Ada as Ada
import Ledger.Value (Value)
import Plutus.Contract.Test
import qualified Plutus.Trace.Emulator as Emulator
import Test.Tasty
import Week02.Homework2

basicFundSpentPredicate :: TracePredicate
basicFundSpentPredicate =
  ( walletFundsChange (Wallet 1) (Ada.lovelaceValueOf (- wallet1Gift))
      .&&. walletFundsChange (Wallet 2) (Ada.lovelaceValueOf (- wallet2Gift))
      .&&. walletFundsChange (Wallet 3) (Ada.lovelaceValueOf (- wallet3Gift))
  )

tests :: TestTree
tests =
  testGroup
    "Homework 1"
    [ checkPredicateOptions
        (defaultCheckOptions & emulatorConfig .~ emCfg)
        "Invalid grabs"
        ( basicFundSpentPredicate
            .&&. walletFundsChange (Wallet 4) mempty
            .&&. walletFundsChange (Wallet 5) mempty
        )
        traceInvalid,
      checkPredicateOptions
        (defaultCheckOptions & emulatorConfig .~ emCfg)
        "Wallet 4 grabs (True, True)"
        ( basicFundSpentPredicate
            .&&. walletFundsChange (Wallet 4) (Ada.lovelaceValueOf totalGift)
            .&&. walletFundsChange (Wallet 5) mempty
        )
        traceValidGrabTT,
      checkPredicateOptions
        (defaultCheckOptions & emulatorConfig .~ emCfg)
        "Wallet 4 grabs (False, False)"
        ( basicFundSpentPredicate
            .&&. walletFundsChange (Wallet 4) (Ada.lovelaceValueOf totalGift)
            .&&. walletFundsChange (Wallet 5) mempty
        )
        traceValidGrabFF
    ]

traceValidGrabTT :: Emulator.EmulatorTrace ()
traceValidGrabTT = do
  (_, _, _, h4, _) <- baseTrace
  Emulator.callEndpoint @"grab" h4 (MyRedeemer True True)
  _ <- Emulator.waitNSlots 1

  return ()

traceValidGrabFF :: Emulator.EmulatorTrace ()
traceValidGrabFF = do
  (_, _, _, h4, _) <- baseTrace
  Emulator.callEndpoint @"grab" h4 (MyRedeemer False False)
  _ <- Emulator.waitNSlots 1

  return ()

traceInvalid :: Emulator.EmulatorTrace ()
traceInvalid = do
  (_, _, _, h4, _) <- baseTrace
  Emulator.callEndpoint @"grab" h4 (MyRedeemer False True)
  _ <- Emulator.waitNSlots 1

  Emulator.callEndpoint @"grab" h4 (MyRedeemer True False)
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
