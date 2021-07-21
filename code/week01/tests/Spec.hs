{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens hiding (elements)
import qualified Ledger.Ada as Ada
import qualified Ledger.Value as Value
import Plutus.Contract.Test
import qualified Spec.Model as Model
import Test.Tasty
import Spec.Trace

testTraceAuction :: TestTree
testTraceAuction =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "successful auction trace"
    ( walletFundsChange (Wallet 1) (Value.assetClassValue assetClass1 (-1) <> Ada.lovelaceValueOf 200_000_000)
        .&&. walletFundsChange (Wallet 3) (Ada.lovelaceValueOf (-200_000_000) <> Value.assetClassValue assetClass1 1)
    )
    traceAuction

testTraceAuctionNoClose :: TestTree
testTraceAuctionNoClose =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "successful AuctionNoClose trace"
    ( walletFundsChange (Wallet 1) (Value.assetClassValue assetClass1 (-1))
        .&&. walletFundsChange (Wallet 3) (Ada.lovelaceValueOf (-200_000_000))
    )
    traceAuctionNoClose

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "auction" [testTraceAuction, testTraceAuctionNoClose, Model.testModelAuction]
