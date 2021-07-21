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

import Control.Monad (forM_, void, when)
import Control.Monad.Freer (Eff, Member, interpret, type (~>))
import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Extras.Log (LogMsg)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, Result (Success), fromJSON)
import Data.Monoid (Last (Last))
import qualified Data.Text as T
import Ledger
import Ledger.Constraints
import qualified Ledger.Value as Value
import Plutus.Contract
import qualified Plutus.Contracts.Currency as Currency
import Plutus.PAB.Effects.Contract (ContractEffect (..))
import Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), endpointsToSchemas, handleBuiltin)
import Plutus.PAB.Monitoring.PABLogMsg (PABMultiAgentMsg)
import Plutus.PAB.Simulator (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator as Simulator
import Plutus.PAB.Types (PABError (..))
import qualified Plutus.PAB.Webserver.Server as PAB.Server
import Wallet.Emulator.Types (Wallet (..), walletPubKey)
import Week01.EnglishAuction (AuctionSchema, endpoints)
import Week01.PAB

walletArtifactPath :: Wallet -> String
walletArtifactPath w = "artifacts/wallet-" ++ show (getWallet w) ++ ".cid"

waitForLastState :: FromJSON a => ContractInstanceId -> Simulator.Simulation t a
waitForLastState cid =
  flip Simulator.waitForState cid $ \v -> case fromJSON v of
    (Success (Last (Just x))) -> Just x
    _ -> Nothing

main :: IO ()
main = void $
  Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin AuctionContracts) "Starting Auction PAB webserver"
    shutdown <- PAB.Server.startServerDebug

    initCID <- Simulator.activateContract (Wallet 1) Init
    cs :: Value.CurrencySymbol <- waitForLastState initCID
    _ <- Simulator.waitUntilFinished initCID
    liftIO $ writeFile "artifacts/currency.txt" (show cs)

    forM_ wallets $ \w -> do
      cid <- Simulator.activateContract w Auction
      liftIO $ writeFile (walletArtifactPath w) (show $ auctionArtifacts cid)

    Simulator.waitUntilSlot 60

    bs <- Simulator.currentBalances
    Simulator.logBalances @(Builtin AuctionContracts) bs

    void $ liftIO getLine
    shutdown

wallets :: [Wallet]
wallets = [Wallet i | i <- [1 .. 4]]

handleAuctionContracts ::
  ( Member (Error PABError) effs,
    Member (LogMsg (PABMultiAgentMsg (Builtin AuctionContracts))) effs
  ) =>
  ContractEffect (Builtin AuctionContracts)
    ~> Eff effs
handleAuctionContracts = handleBuiltin getSchema getContract
  where
    getSchema = \case
      Auction -> endpointsToSchemas @AuctionSchema
      Init -> endpointsToSchemas @Empty
    getContract = \case
      Auction -> SomeBuiltin endpoints
      Init -> SomeBuiltin initContract

handlers :: SimulatorEffectHandlers (Builtin AuctionContracts)
handlers =
  Simulator.mkSimulatorHandlers @(Builtin AuctionContracts) [] $
    interpret handleAuctionContracts


initContract :: Contract (Last CurrencySymbol) Empty T.Text ()
initContract = do
  ownPk <- pubKeyHash <$> ownPubKey
  let amount :: Integer = 100_000_000
  cur <-
    mapError
      (T.pack . show)
      ( Currency.mintContract ownPk [("TOK", amount * fromIntegral (length wallets))] ::
          Contract e Empty Currency.CurrencyError Currency.OneShotCurrency
      )
  let cs = Currency.currencySymbol cur
      v = Value.singleton cs "TOK" amount
  logInfo $ "Currency Symbol '" ++ show cs ++ "' for token 'TOK' minted"
  tell $ Last $ Just cs
  forM_ wallets $ \w -> do
    let pkh = pubKeyHash $ walletPubKey w
    when (pkh /= ownPk) $ do
      tx <- submitTx $ mustPayToPubKey pkh v
      awaitTxConfirmed $ txId tx
