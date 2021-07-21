{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Spec.Model where

import Control.Lens hiding (elements)
import Control.Monad (when)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust, isNothing)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Ledger.Ada as Ada
import Ledger.Slot (Slot (..))
import Ledger.TimeSlot (slotToPOSIXTime)
import qualified Ledger.Value as Value
import Plutus.Contract.Test
import Plutus.Contract.Test.ContractModel
import Plutus.Trace.Emulator (EmulatorConfig (..), callEndpoint)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck hiding ((.&&.))
import Week01.EnglishAuction (AuctionSchema, BidParams (..), CloseParams (..), StartParams (..), endpoints)
import Spec.Trace (assetClass1, assetClass2)

data AuctionState = AuctionState
  { _asHighestBid :: !Integer,
    _asHighestBidder :: Maybe Wallet,
    _asMinBid :: !Integer,
    _asDeadline :: !Slot
  }
  deriving (Show)

makeLenses ''AuctionState

newtype AuctionModel = AuctionModel {_aModel :: Map Wallet (Map Value.AssetClass AuctionState)}
  deriving (Show)

makeLenses ''AuctionModel

testModelAuction :: TestTree
testModelAuction = testProperty "auction model" prop_Auction

genAssetClass :: Gen Value.AssetClass
genAssetClass = elements [assetClass1, assetClass2]

genWallet :: Gen Wallet
genWallet = elements wallets

genNonNeg :: Gen Integer
genNonNeg = getNonNegative <$> arbitrary

w1, w2, w3, w4 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3
w4 = Wallet 4

wallets :: [Wallet]
wallets = [w1, w2, w3, w4]

deriving instance Eq (ContractInstanceKey AuctionModel w s e)

deriving instance Show (ContractInstanceKey AuctionModel w s e)

instance ContractModel AuctionModel where
  data Action AuctionModel
    = Start Wallet Value.AssetClass Integer Integer
    | Bid Wallet Wallet Value.AssetClass Integer
    | Close Wallet Wallet Value.AssetClass
    deriving (Show, Eq)

  data ContractInstanceKey AuctionModel w s e where
    AuctionKey :: Wallet -> ContractInstanceKey AuctionModel () AuctionSchema Text

  instanceTag key _ = fromString $ "instance tag for: " ++ show key

  arbitraryAction _ =
    oneof
      [ Start <$> genWallet <*> genAssetClass <*> genNonNeg <*> genNonNeg,
        Bid <$> genWallet <*> genWallet <*> genAssetClass <*> genNonNeg,
        Close <$> genWallet <*> genWallet <*> genAssetClass
      ]

  initialState = AuctionModel Map.empty

  -- Allows for auctions to start as long as you have balance even
  -- if there is an ongoing auction
  nextState (Start wallet a v d) = do
    bc <- askModelState $ view $ balanceChange wallet
    when (Value.assetClassValueOf bc a > 0) $ do
      withdraw wallet $ Value.assetClassValue a 1
      (aModel . at wallet . _Just . at a) $= Just (AuctionState 0 Nothing v (Slot d))
    wait 1
  nextState (Bid owner bidder a v) = do
    s' <- getAuctionState owner a
    when (isJust s') $ do
      bc <- askModelState $ view $ balanceChange bidder
      slot <- askModelState $ view currentSlot
      let s = fromJust s'
          AuctionState {_asMinBid, _asHighestBid, _asDeadline} = s
          isGtMinBid = v > _asMinBid && v > _asHighestBid
          beforeDeadline = _asDeadline > slot
      when (beforeDeadline && isGtMinBid && Ada.getLovelace (Ada.fromValue bc) >= v) $ do
        withdraw bidder $ Ada.lovelaceValueOf v
        maybeDeposit (_asHighestBidder s) $ Ada.lovelaceValueOf _asHighestBid
        (aModel . at owner . _Just . at a . _Just) $~ \st -> st {_asHighestBid = v, _asHighestBidder = Just bidder}
    wait 1
  nextState (Close owner bidder a) = do
    s <- getAuctionState owner a
    when (isJust s) $ do
      slot <- askModelState $ view currentSlot
      let AuctionState {_asHighestBid, _asHighestBidder, _asDeadline} = fromJust s
      let afterDeadline = slot > _asDeadline
      when (afterDeadline && (owner == bidder || _asHighestBidder == Just bidder)) $ do
        maybeDeposit _asHighestBidder $ Value.assetClassValue a 1
        deposit owner $ Ada.lovelaceValueOf _asHighestBid
        (aModel . at owner . _Just . at a) $= Nothing

    wait 1

  perform h _ cmd = case cmd of
    Start w a v d -> let (c, t) = Value.unAssetClass a in callEndpoint @"start" (h $ AuctionKey w) (StartParams (slotToPOSIXTime $ Slot d) v c t)
    Bid _ b a v -> let (c, t) = Value.unAssetClass a in callEndpoint @"bid" (h $ AuctionKey b) (BidParams c t v)
    Close _ b a -> let (c, t) = Value.unAssetClass a in callEndpoint @"close" (h $ AuctionKey b) (CloseParams c t)

  precondition s (Start w a _ _) = isNothing $ getAuctionState' s w a
  precondition s (Bid w _ a _) = isJust $ getAuctionState' s w a
  precondition s (Close w _ a) = isJust $ getAuctionState' s w a

maybeDeposit :: Maybe Wallet -> Value.Value -> Spec AuctionModel ()
maybeDeposit Nothing = return . const ()
maybeDeposit (Just w) = deposit w

instanceSpec :: [ContractInstanceSpec AuctionModel]
instanceSpec = [ContractInstanceSpec (AuctionKey w) w endpoints | w <- wallets]

tokenAssets :: [Value.AssetClass]
tokenAssets = [assetClass1, assetClass2]

getAuctionState' :: ModelState AuctionModel -> Wallet -> Value.AssetClass -> Maybe AuctionState
getAuctionState' s w a = s ^. contractState . aModel ^. at w . _Just ^. at a

getAuctionState :: Wallet -> Value.AssetClass -> Spec AuctionModel (Maybe AuctionState)
getAuctionState w a = do
  s <- getModelState
  return $ getAuctionState' s w a

auctionHasStarted :: Wallet -> Value.AssetClass -> Spec AuctionModel Bool
auctionHasStarted w a = isJust <$> getAuctionState w a

initialTokens :: Integer
initialTokens = 1

initialLovelace :: Integer
initialLovelace = 1_000_000_000

prop_Auction :: Actions AuctionModel -> Property
prop_Auction =
  withMaxSuccess 500
    . propRunActionsWithOptions
      (defaultCheckOptions & emulatorConfig .~ EmulatorConfig (Left d))
      instanceSpec
      (const $ pure True)
  where
    d :: InitialDistribution
    d =
      Map.fromList
        [(w, Ada.lovelaceValueOf initialLovelace <> mconcat (map (`Value.assetClassValue` initialTokens) tokenAssets)) | w <- wallets]

test :: IO ()
test = quickCheck prop_Auction
