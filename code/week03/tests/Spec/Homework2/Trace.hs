{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Homework2.Trace where

import Control.Lens ((&), (.~))
import Control.Monad (void)
import Data.Default (Default (..))
import qualified Data.Map as Map
import Data.Text (Text)
import Ledger (pubKeyHash)
import qualified Ledger.Ada as Ada
import Ledger.Slot (Slot (..))
import Ledger.Time (POSIXTime)
import Ledger.TimeSlot (slotToBeginPOSIXTime)
import Ledger.Value (Value)
import Plutus.Contract.Test
import qualified Plutus.Trace.Emulator as Emulator
import Test.Tasty
import Week03.Homework2

w1, w2, w3 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3

wallets :: [Wallet]
wallets = [w1, w2, w3]

emCfg :: Emulator.EmulatorConfig
emCfg =
  Emulator.EmulatorConfig $
    Left $
      Map.fromList
        [(w, initialFunds) | w <- wallets]

initialFunds :: Value
initialFunds = Ada.lovelaceValueOf 1_000_000_000

vestedAmount :: Integer
vestedAmount = 400_000_000

tests :: TestTree
tests =
  testGroup
    "Homework 2"
    [ checkPredicateOptions
        (defaultCheckOptions & emulatorConfig .~ emCfg)
        "Early Withdrawal by Beneficiary"
        ( walletFundsChange w1 (Ada.lovelaceValueOf (- vestedAmount))
            .&&. walletFundsChange w2 mempty
        )
        traceEarlyWithdrawal,
      checkPredicateOptions
        (defaultCheckOptions & emulatorConfig .~ emCfg)
        "Valid Withdrawal by Beneficiary"
        ( walletFundsChange w1 (Ada.lovelaceValueOf (- vestedAmount))
            .&&. walletFundsChange w2 (Ada.lovelaceValueOf vestedAmount)
        )
        traceValidWithdrawal,
      checkPredicateOptions
        (defaultCheckOptions & emulatorConfig .~ emCfg)
        "Valid Withdrawal by Beneficiary with 3rd Party Attempting Withdrawal"
        ( walletFundsChange w1 (Ada.lovelaceValueOf (- vestedAmount))
            .&&. walletFundsChange w2 (Ada.lovelaceValueOf vestedAmount)
            .&&. walletFundsChange w3 mempty
        )
        traceValidWithdrawalBadActor,
      checkPredicateOptions
        (defaultCheckOptions & emulatorConfig .~ emCfg)
        "Valid Withdrawal by Beneficiary Double Vesting"
        ( walletFundsChange w1 (Ada.lovelaceValueOf (- (vestedAmount * 2)))
            .&&. walletFundsChange w2 (Ada.lovelaceValueOf $ vestedAmount * 2)
        )
        traceDoubleDepositFullWithdrawal,
      checkPredicateOptions
        (defaultCheckOptions & emulatorConfig .~ emCfg)
        "Valid Withdrawal by Beneficiary Double Vesting Early for 2nd Deadline"
        ( walletFundsChange w1 (Ada.lovelaceValueOf (- (vestedAmount * 2)))
            .&&. walletFundsChange w2 (Ada.lovelaceValueOf vestedAmount)
        )
        traceDoubleDepositHalfWithdrawal
    ]

type Handle = Emulator.ContractHandle () VestingSchema Text

vestingPeriodExpirySlot :: Slot
vestingPeriodExpirySlot = 40

vestingPeriodExpiryPOSIXTime :: Slot -> POSIXTime
vestingPeriodExpiryPOSIXTime = slotToBeginPOSIXTime def . (* vestingPeriodExpirySlot)

traceValidWithdrawalBadActor :: Emulator.EmulatorTrace ()
traceValidWithdrawalBadActor = do
  (_, h2, h3) <- traceEarlyWithdrawal'
  void $ Emulator.waitUntilSlot vestingPeriodExpirySlot

  Emulator.callEndpoint @"grab" h3 ()
  void $ Emulator.waitNSlots 1

  Emulator.callEndpoint @"grab" h2 ()
  void $ Emulator.waitNSlots 1

traceDoubleDepositHalfWithdrawal :: Emulator.EmulatorTrace ()
traceDoubleDepositHalfWithdrawal = do
  (h1, h2, _) <- traceEarlyWithdrawal'
  Emulator.callEndpoint @"give" h1 (GiveParams (pubKeyHash $ walletPubKey w2) (vestingPeriodExpiryPOSIXTime 2) vestedAmount)
  void $ Emulator.waitUntilSlot vestingPeriodExpirySlot

  Emulator.callEndpoint @"grab" h2 ()
  void $ Emulator.waitNSlots 1

traceDoubleDepositFullWithdrawal :: Emulator.EmulatorTrace ()
traceDoubleDepositFullWithdrawal = do
  (h1, h2, _) <- traceEarlyWithdrawal'
  Emulator.callEndpoint @"give" h1 (GiveParams (pubKeyHash $ walletPubKey w2) (vestingPeriodExpiryPOSIXTime 1) vestedAmount)
  void $ Emulator.waitUntilSlot vestingPeriodExpirySlot

  Emulator.callEndpoint @"grab" h2 ()
  void $ Emulator.waitNSlots 1

traceValidWithdrawal :: Emulator.EmulatorTrace ()
traceValidWithdrawal = do
  (_, h2, _) <- traceEarlyWithdrawal'
  void $ Emulator.waitUntilSlot vestingPeriodExpirySlot

  Emulator.callEndpoint @"grab" h2 ()
  void $ Emulator.waitNSlots 1

traceEarlyWithdrawal :: Emulator.EmulatorTrace ()
traceEarlyWithdrawal = void traceEarlyWithdrawal'

traceEarlyWithdrawal' :: Emulator.EmulatorTrace (Handle, Handle, Handle)
traceEarlyWithdrawal' = do
  h1 <- Emulator.activateContractWallet w1 endpoints
  h2 <- Emulator.activateContractWallet w2 endpoints
  h3 <- Emulator.activateContractWallet w3 endpoints
  void $ Emulator.waitNSlots 1

  Emulator.callEndpoint @"give" h1 (GiveParams (pubKeyHash $ walletPubKey w2) (vestingPeriodExpiryPOSIXTime 1) vestedAmount)
  void $ Emulator.waitNSlots 1

  Emulator.callEndpoint @"grab" h2 ()
  void $ Emulator.waitNSlots 1

  return (h1, h2, h3)
