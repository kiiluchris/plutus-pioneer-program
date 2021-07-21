{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Parameterized.Trace where

import Control.Lens ((&), (.~))
import Control.Monad (void)
import Data.Default (Default (..))
import qualified Data.Map as Map
import Ledger (pubKeyHash)
import Ledger.Ada as Ada
import Ledger.Fee (FeeConfig (..))
import Ledger.Slot (Slot (..))
import Ledger.TimeSlot (slotToBeginPOSIXTime)
import Ledger.Value (Value)
import Plutus.Contract.Test
import qualified Plutus.Trace.Emulator as Emulator
import Test.Tasty
import Week03.Parameterized

tests :: TestTree
tests =
  testGroup
    "Parameterized Contract"
    [ checkPredicateOptions
        (defaultCheckOptions & emulatorConfig .~ emCfg)
        "Before Deadline"
        ( walletFundsChange (Wallet 1) (Ada.lovelaceValueOf (- amountVested))
            .&&. walletFundsChange (Wallet 2) mempty
            .&&. walletFundsChange (Wallet 3) mempty
        )
        traceBeforeDeadline,
      checkPredicateOptions
        (defaultCheckOptions & emulatorConfig .~ emCfg)
        "Invalid Beneficiary"
        ( walletFundsChange (Wallet 1) (Ada.lovelaceValueOf (- amountVested))
            .&&. walletFundsChange (Wallet 2) mempty
            .&&. walletFundsChange (Wallet 3) mempty
        )
        traceInvalidBeneficiary,
      checkPredicateOptions
        (defaultCheckOptions & emulatorConfig .~ emCfg)
        "Valid Beneficiary After Vesting Period"
        ( walletFundsChange (Wallet 1) (Ada.lovelaceValueOf (- amountVested))
            .&&. walletFundsChange (Wallet 2) (Ada.lovelaceValueOf amountVested)
            .&&. walletFundsChange (Wallet 3) mempty
        )
        traceValid
    ]

endOfVestingPeriod :: Slot
endOfVestingPeriod = 20

amountVested :: Integer
amountVested = 100_000_000

traceBeforeDeadline :: Emulator.EmulatorTrace ()
traceBeforeDeadline = do
  h1 <- Emulator.activateContractWallet (Wallet 1) endpoints
  h2 <- Emulator.activateContractWallet (Wallet 2) endpoints
  _ <- Emulator.waitNSlots 1

  Emulator.callEndpoint @"give"
    h1
    GiveParams
      { gpBeneficiary = pubKeyHash (walletPubKey (Wallet 2)),
        gpDeadline = slotToBeginPOSIXTime def endOfVestingPeriod,
        gpAmount = 100_000_000
      }
  _ <- Emulator.waitNSlots 1

  Emulator.callEndpoint @"grab" h2 (slotToBeginPOSIXTime def endOfVestingPeriod)
  void $ Emulator.waitNSlots 1

traceInvalidBeneficiary :: Emulator.EmulatorTrace ()
traceInvalidBeneficiary = do
  h1 <- Emulator.activateContractWallet (Wallet 1) endpoints
  h3 <- Emulator.activateContractWallet (Wallet 3) endpoints
  _ <- Emulator.waitNSlots 1

  Emulator.callEndpoint @"give"
    h1
    GiveParams
      { gpBeneficiary = pubKeyHash (walletPubKey (Wallet 2)),
        gpDeadline = slotToBeginPOSIXTime def endOfVestingPeriod,
        gpAmount = 100_000_000
      }
  _ <- Emulator.waitUntilSlot endOfVestingPeriod

  Emulator.callEndpoint @"grab" h3 (slotToBeginPOSIXTime def endOfVestingPeriod)
  void $ Emulator.waitNSlots 1

traceValid :: Emulator.EmulatorTrace ()
traceValid = do
  h1 <- Emulator.activateContractWallet (Wallet 1) endpoints
  h2 <- Emulator.activateContractWallet (Wallet 2) endpoints
  _ <- Emulator.waitNSlots 1

  Emulator.callEndpoint @"give"
    h1
    GiveParams
      { gpBeneficiary = pubKeyHash (walletPubKey (Wallet 2)),
        gpDeadline = slotToBeginPOSIXTime def endOfVestingPeriod,
        gpAmount = 100_000_000
      }
  _ <- Emulator.waitUntilSlot endOfVestingPeriod

  Emulator.callEndpoint @"grab" h2 (slotToBeginPOSIXTime def endOfVestingPeriod)
  void $ Emulator.waitNSlots 1

runTrace' :: Emulator.EmulatorTrace () -> IO ()
runTrace' = Emulator.runEmulatorTraceIO' def emCfg (FeeConfig (Ada.lovelaceOf 100) 1)

emCfg :: Emulator.EmulatorConfig
emCfg =
  Emulator.EmulatorConfig $
    Left $
      Map.fromList
        [(w, initialFunds) | w <- wallets]

wallets :: [Wallet]
wallets = map Wallet [1 .. 5]

initialFunds :: Value
initialFunds = Ada.lovelaceValueOf initialLovelace

initialLovelace :: Integer
initialLovelace = 1_000_000_000
