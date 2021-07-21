module Main (main) where

import qualified Spec.Homework1.Trace as One
import qualified Spec.Homework2.Trace as Two
import qualified Spec.Parameterized.Trace as Parameterized
import qualified Spec.Vesting.Trace as Vesting
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Vesting.tests, Parameterized.tests, One.tests, Two.tests]
