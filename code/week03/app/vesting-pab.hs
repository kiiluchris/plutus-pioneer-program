{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Monad (forM_, void)
import Control.Monad.Freer (Eff, Member, interpret, type (~>))
import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Extras.Log (LogMsg)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BS
import Data.Default (Default (..))
import Plutus.PAB.Effects.Contract (ContractEffect (..))
import Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), endpointsToSchemas, handleBuiltin)
import Plutus.PAB.Monitoring.PABLogMsg (PABMultiAgentMsg)
import Plutus.PAB.Simulator (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator as Simulator
import Plutus.PAB.Types (PABError)
import Plutus.PAB.Webserver.Server as PAB.Server
import System.Directory (createDirectoryIfMissing)
import Wallet.Emulator.Wallet
import Wallet.Types (ContractInstanceId (..))
import Week03.PAB
import Week03.Vesting

saveWalletArtifacts :: (MonadIO m) => Wallet -> VestingArtifacts -> m ()
saveWalletArtifacts w va =
  liftIO $
    BS.writeFile
      ("artifacts/wallet-" ++ show (getWallet w) ++ ".json")
      (encode va)

main :: IO ()
main = void $ do
  createDirectoryIfMissing False "artifacts"
  Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin VestingContracts) "Starting Vesting PAB Server"
    shutdown <- PAB.Server.startServerDebug

    forM_ wallets $ \w -> do
      cid <- Simulator.activateContract w Vesting
      let artifacts = VestingArtifacts (unContractInstanceId cid)
      saveWalletArtifacts w artifacts

    Simulator.logBalances @(Builtin VestingContracts) =<< Simulator.currentBalances
    void $ liftIO getLine
    shutdown

handleContractEffects ::
  ( Member (Error PABError) effs,
    Member (LogMsg (PABMultiAgentMsg (Builtin VestingContracts))) effs
  ) =>
  ContractEffect (Builtin VestingContracts)
    ~> Eff effs
handleContractEffects = handleBuiltin getSchema getContract
  where
    getSchema c = case c of
      Vesting -> endpointsToSchemas @VestingSchema
    getContract c = case c of
      Vesting -> SomeBuiltin endpoints

handlers :: SimulatorEffectHandlers (Builtin VestingContracts)
handlers =
  Simulator.mkSimulatorHandlers @(Builtin VestingContracts) def [] $
    interpret handleContractEffects

wallets :: [Wallet]
wallets = map Wallet [1 .. 5]
