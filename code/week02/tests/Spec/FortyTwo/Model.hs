{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Spec.FortyTwo.Model (runTest, tests) where

import Control.Lens hiding (elements)
import Control.Monad (when)
import qualified Data.Map as Map
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Ledger.Ada as Ada
import Ledger.Value (Value)
import Plutus.Contract.Test
import Plutus.Contract.Test.ContractModel
import qualified Plutus.Trace.Emulator as Emulator
import Test.Tasty
import Test.Tasty.QuickCheck
import Week02.FortyTwo

newtype GiftModel = GiftModel {_gModel :: Integer}
  deriving (Show)

makeLenses ''GiftModel

tests :: TestTree
tests = testProperty "ContractModel Test" prop_FortyTwo

prop_FortyTwo :: Actions GiftModel -> Property
prop_FortyTwo =
  withMaxSuccess 500
    . propRunActionsWithOptions
      (defaultCheckOptions & emulatorConfig .~ emCfg)
      instanceSpec
      (const $ pure True)

runTest :: IO ()
runTest = defaultMain tests

deriving instance Eq (ContractInstanceKey GiftModel w s e)

deriving instance Show (ContractInstanceKey GiftModel w s e)

instance ContractModel GiftModel where
  data Action GiftModel
    = Give Wallet Integer
    | Grab Wallet Integer
    deriving (Show, Eq)

  data ContractInstanceKey GiftModel w s e where
    RunKey :: Wallet -> ContractInstanceKey GiftModel () GiftSchema Text

  instanceTag key _ = fromString $ "instance tag for: " ++ show key

  arbitraryAction _ =
    oneof
      [ Give <$> genWallet <*> genNonNeg,
        Grab <$> genWallet <*> genNonNeg
      ]

  initialState = GiftModel 0

  nextState (Give w v) = do
    bc <- askModelState $ view $ balanceChange w
    when ((Ada.getLovelace $ Ada.fromValue bc) > v) $ do
      deposit w $ Ada.lovelaceValueOf v
      gModel $~ (+ v)
    wait 1
  nextState (Grab w _) = do
    s <- getModelState
    let amount = s ^. contractState . gModel
    withdraw w $ Ada.lovelaceValueOf amount
    gModel $= 0
    wait 1

  perform h _ cmd = case cmd of
    Give w v -> Emulator.callEndpoint @"give" (h $ RunKey w) v
    Grab w v -> Emulator.callEndpoint @"grab" (h $ RunKey w) v

  precondition _ (Give _ _) = True
  precondition s (Grab _ r) = (s ^. contractState . gModel) > 0 && r == 42

instanceSpec :: [ContractInstanceSpec GiftModel]
instanceSpec = [ContractInstanceSpec (RunKey w) w endpoints | w <- wallets]

genNonNeg :: Gen Integer
genNonNeg = getNonNegative <$> arbitrary

genWallet :: Gen Wallet
genWallet = elements wallets

wallets :: [Wallet]
wallets = map Wallet [1 .. 5]

emCfg :: Emulator.EmulatorConfig
emCfg =
  Emulator.EmulatorConfig $
    Left $
      Map.fromList
        [(w, initialFunds) | w <- wallets]

initialFunds :: Value
initialFunds = Ada.lovelaceValueOf initialLovelace

initialLovelace :: Integer
initialLovelace = 1_000_000_000
