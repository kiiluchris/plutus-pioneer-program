{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever, mzero, void)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Aeson hiding (Options)
import Data.Proxy (Proxy (..))
import Data.Text (pack)
import Data.UUID
import Ledger.Slot (Slot (..))
import Ledger.TimeSlot (slotToPOSIXTime)
import Ledger.Value (TokenName (..))
import Network.HTTP.Req
import Network.WebSockets
import Options.Generic
import Plutus.PAB.Webserver.Types (CombinedWSStreamToClient (..))
import Text.Read (readMaybe)
import Week01.PAB

type MinBid = Integer

type Deadline = Integer

data HostActions
  = StartAuction MinBid Deadline
  | CloseAuction
  | Quit
  deriving (Show, Read)

data Options w = Options
  {oWalletPath :: w ::: FilePath <?> "The wallet hosting the auction" <!> "artifacts/wallet-1.cid"}
  deriving (Generic)

instance ParseRecord (Options Wrapped)

deriving instance Show (Options Unwrapped)

syncSlots :: TVar Slot -> IO ()
syncSlots slot = void . forkIO $
  runClient "127.0.0.1" 8080 "/ws" $ \conn ->
    forever $ do
      message <- receiveData conn
      case decode @CombinedWSStreamToClient message of
        Just (SlotChange nextSlot) -> do
          _ <- atomically $ swapTVar slot nextSlot
          return ()
        _ -> pure ()

main :: IO ()
main = do
  optR <- unwrapRecord "Auction host"
  let token = TokenName "TOK"
  slot <- newTVarIO $ Slot 1
  syncSlots slot
  void . runMaybeT . forever $ do
    cs <- PABCurrencySymbol <$> liftIO (readFile "artifacts/currency.txt")
    AuctionArtifacts {..} <- liftIO (read <$> readFile (oWalletPath optR))
    liftIO $ putStrLn "StartAuction [MinBid] [Deadline] | CloseAuction | Quit"
    line <- liftIO getLine
    case readMaybe @HostActions line of
      Nothing -> liftIO $ putStrLn "Could not parse command"
      Just (StartAuction mb d) -> do
        currentSlot <- liftIO $ readTVarIO slot
        liftIO $ putStrLn $ "Current Slot: " <> show currentSlot
        liftIO $ startAuction aaAuctionCID PABStartParams {spDeadline = slotToPOSIXTime (currentSlot + Slot d), spMinBid = mb, spToken = token, spCurrency = cs}
      Just CloseAuction -> do
        liftIO $ closeAuction aaAuctionCID PABCloseParams {cpCurrency = cs, cpToken = token}
      Just Quit -> mzero
  return ()

startAuction :: UUID -> PABStartParams -> IO ()
startAuction uuid sp = do
  print (encode sp)
  runReq defaultHttpConfig $ do
    r <-
      req
        POST
        (http "127.0.0.1" /: "api" /: "new" /: "contract" /: "instance" /: pack (show uuid) /: "endpoint" /: "start")
        (ReqBodyJson sp)
        (Proxy @(JsonResponse ()))
        (port 8080)
    liftIO $
      putStrLn $
        if responseStatusCode r == 200
          then "started auction"
          else "error starting auction"

closeAuction :: UUID -> PABCloseParams -> IO ()
closeAuction uuid cp =
  runReq defaultHttpConfig $ do
    r <-
      req
        POST
        (http "127.0.0.1" /: "api" /: "new" /: "contract" /: "instance" /: pack (show uuid) /: "endpoint" /: "close")
        (ReqBodyJson cp)
        (Proxy @(JsonResponse ()))
        (port 8080)
    liftIO $
      putStrLn $
        if responseStatusCode r == 200
          then "closed auction"
          else "error closing auction"
