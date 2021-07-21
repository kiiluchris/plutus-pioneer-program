{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Week01.PAB (AuctionContracts (..), AuctionArtifacts(..), PABCurrencySymbol(..), PABStartParams(..), PABCloseParams(..), PABBidParams(..), auctionArtifacts) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import GHC.Generics (Generic)
import Data.UUID
import Wallet.Types (ContractInstanceId(..))
import Ledger.Value (TokenName)
import Ledger.Time (POSIXTime)

data AuctionContracts = Auction | Init 
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

instance Pretty AuctionContracts where
  pretty = viaShow

newtype AuctionArtifacts = AuctionArtifacts 
  { aaAuctionCID :: UUID
  }
  deriving (Show, Read)

auctionArtifacts :: ContractInstanceId -> AuctionArtifacts
auctionArtifacts a = AuctionArtifacts (unContractInstanceId a)

data PABCurrencySymbol = PABCurrencySymbol { unCurrencySymbol :: String } deriving (Generic, FromJSON, ToJSON)

data PABStartParams = PABStartParams
    { spDeadline :: !POSIXTime
    , spMinBid   :: !Integer
    , spCurrency :: !PABCurrencySymbol
    , spToken    :: !TokenName
    } deriving (Generic, ToJSON, FromJSON)

data PABBidParams = PABBidParams
    { bpCurrency :: !PABCurrencySymbol
    , bpToken    :: !TokenName
    , bpBid      :: !Integer
    } deriving (Generic, ToJSON, FromJSON)

data PABCloseParams = PABCloseParams
    { cpCurrency :: !PABCurrencySymbol
    , cpToken    :: !TokenName
    } deriving (Generic, ToJSON, FromJSON)

