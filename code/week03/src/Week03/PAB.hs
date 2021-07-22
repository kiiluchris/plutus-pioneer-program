{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Week03.PAB where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data VestingContracts
  = Vesting
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

instance Pretty VestingContracts where
  pretty = viaShow

data VestingArtifacts = VestingArtifacts
  {vaVestingCid :: UUID}
  deriving (Show, Read, Generic, FromJSON, ToJSON)
