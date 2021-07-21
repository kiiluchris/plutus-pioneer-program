module Main (main) where

import qualified Spec.Burn as Burn
import qualified Spec.FortyTwo as FortyTwo
import qualified Spec.Homework as Homework
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Burn.tests, FortyTwo.tests, Homework.tests]
