module Spec.Homework (tests) where

import Test.Tasty
import qualified Spec.Homework.One as One
import qualified Spec.Homework.Two as Two

tests :: TestTree
tests = testGroup "Homework" [One.tests, Two.tests]
