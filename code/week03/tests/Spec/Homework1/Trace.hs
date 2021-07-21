{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Homework1.Trace where

import Control.Lens ((&), (.~))
import Control.Monad (forM, void)
import Data.Default (Default (..))
import qualified Data.Map as Map
import Ledger (PubKeyHash, pubKeyHash)
import qualified Ledger.Ada as Ada
import Ledger.Slot (Slot (..))
import Ledger.Time (POSIXTime)
import Ledger.TimeSlot (slotToBeginPOSIXTime)
import Ledger.Value (Value)
import Plutus.Contract.Test
import qualified Plutus.Trace.Emulator as Emulator
import Test.Tasty
import Week03.Homework1

tests :: TestTree
tests =
  testGroup
    "Homework 1"
    [ checkPredicateOptions
        (defaultCheckOptions & emulatorConfig .~ emCfg)
        "Beneficiary 1 withdraws before end of vesting period"
        ( walletFundsChange w1 (Ada.lovelaceValueOf (-200_000_000))
            .&&. walletFundsChange w2 (Ada.lovelaceValueOf 200_000_000)
        )
        traceBeneficiary1Valid,
      checkPredicateOptions
        (defaultCheckOptions & emulatorConfig .~ emCfg)
        "Beneficiary 2 withdraws after end of vesting period"
        (walletFundsChange w1 mempty)
        traceBeneficiary2Valid
    ]

traceBeneficiary1Valid :: Emulator.EmulatorTrace ()
traceBeneficiary1Valid =
  forM wallets (flip Emulator.activateContractWallet endpoints)
    >>= \hs -> case hs of
      (h1 : h2 : _) -> do
        void $ Emulator.waitNSlots 1

        Emulator.callEndpoint @"give" h1 (GiveParams (walletPubKeyHash w2) vestingPeriodExpiryPOSIX 100_000_000)
        void $ Emulator.waitNSlots 1

        Emulator.callEndpoint @"grab" h2 ()
        void $ Emulator.waitNSlots 1

        Emulator.callEndpoint @"give" h1 (GiveParams (walletPubKeyHash w2) vestingPeriodExpiryPOSIX 100_000_000)
        void $ Emulator.waitNSlots 1

        Emulator.callEndpoint @"grab" h2 ()
        void $ Emulator.waitNSlots 1
      _ -> return ()

traceBeneficiary2Valid :: Emulator.EmulatorTrace ()
traceBeneficiary2Valid = do
  forM wallets (flip Emulator.activateContractWallet endpoints)
    >>= \hs -> case hs of
      (h1 : _) -> do
        void $ Emulator.waitNSlots 1

        Emulator.callEndpoint @"give" h1 (GiveParams (walletPubKeyHash w2) vestingPeriodExpiryPOSIX 100_000_000)
        void $ Emulator.waitUntilSlot (vestingPeriodExpirySlot + 1)

        Emulator.callEndpoint @"grab" h1 ()
        void $ Emulator.waitNSlots 1

        Emulator.callEndpoint @"give" h1 (GiveParams (walletPubKeyHash w2) (slotToBeginPOSIXTime def $ vestingPeriodExpirySlot * 2) 100_000_000)
        void $ Emulator.waitUntilSlot (vestingPeriodExpirySlot * 2 + 1)

        Emulator.callEndpoint @"grab" h1 ()
        void $ Emulator.waitNSlots 1
      _ -> return ()

runTests :: IO ()
runTests = defaultMain tests

runTrace :: Emulator.EmulatorTrace () -> IO ()
runTrace = Emulator.runEmulatorTraceIO' def emCfg def

emCfg :: Emulator.EmulatorConfig
emCfg =
  Emulator.EmulatorConfig $
    Left $
      Map.fromList
        [(w, initialFunds) | w <- wallets]

vestingPeriodExpirySlot :: Slot
vestingPeriodExpirySlot = Slot 25

vestingPeriodExpiryPOSIX :: POSIXTime
vestingPeriodExpiryPOSIX = slotToBeginPOSIXTime def vestingPeriodExpirySlot

wallets :: [Wallet]
wallets = [w1, w2, w3, w4, w5]

w1, w2, w3, w4, w5 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3
w4 = Wallet 4
w5 = Wallet 5

initialLovelace :: Integer
initialLovelace = 1_000_000_000

initialFunds :: Value
initialFunds = Ada.lovelaceValueOf initialLovelace

walletPubKeyHash :: Wallet -> PubKeyHash
walletPubKeyHash = pubKeyHash . walletPubKey

posixTime :: Integer -> POSIXTime
posixTime = slotToBeginPOSIXTime def . Slot
