{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Spec.FortyTwo (tests) where

import qualified Spec.FortyTwo.Model as M
import qualified Spec.FortyTwo.Trace as T
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "Redeemer = 42 required to validate transaction (5 wallets)"
    [T.tests, M.tests]
