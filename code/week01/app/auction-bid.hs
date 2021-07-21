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

import Control.Monad (forever, mzero, void)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Proxy (Proxy (..))
import Data.Text (pack)
import Data.UUID
import Ledger.Value (TokenName (..))
import Network.HTTP.Req
import Options.Generic
import Text.Read (readMaybe)
import Week01.PAB

type Amount = Integer

data BidActions
  = BidAuction Amount
  | CloseAuction
  | Quit
  deriving (Show, Read)

data Options w = Options
  { oWalletPath :: w ::: FilePath <?> "The wallet hosting the auction"
  }
  deriving (Generic)

instance ParseRecord (Options Wrapped)

deriving instance Show (Options Unwrapped)

main :: IO ()
main = do
  optR <- unwrapRecord "Auction host"
  cs <- PABCurrencySymbol <$> readFile "artifacts/currency.txt"
  AuctionArtifacts {..} <- read <$> readFile (oWalletPath optR)
  let token = TokenName "TOK"
  void . runMaybeT . forever $ do
    liftIO $ putStrLn "BidAuction [Amount] | CloseAuction | Quit"
    line <- liftIO getLine
    case readMaybe @BidActions line of
      Nothing -> liftIO $ putStrLn "Could not parse command"
      Just (BidAuction mb) -> do
        bidAuction aaAuctionCID PABBidParams {bpBid = mb, bpToken = token, bpCurrency = cs}
      Just CloseAuction -> do
        closeAuction aaAuctionCID PABCloseParams {cpCurrency = cs, cpToken = token}
      Just Quit -> mzero
    return ()

bidAuction :: MonadIO m => UUID -> PABBidParams -> m ()
bidAuction uuid sp = liftIO $
  runReq defaultHttpConfig $ do
    r <-
      req
        POST
        (http "127.0.0.1" /: "api" /: "new" /: "contract" /: "instance" /: pack (show uuid) /: "endpoint" /: "bid")
        (ReqBodyJson sp)
        (Proxy @(JsonResponse ()))
        (port 8080)
    liftIO $
      putStrLn $
        if responseStatusCode r == 200
          then "bidded for auction"
          else "error bidding for auction"

closeAuction :: MonadIO m => UUID -> PABCloseParams -> m ()
closeAuction uuid cp = liftIO $
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
