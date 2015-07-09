-- |
-- * Top-level module for all tests

module Main where

--import qualified CoALP.Tests.Unit.Subst  as Subst
--import qualified CoALP.Tests.Unit.Subst  as Subst
import qualified CoALP.Tests.Unit.Unify as Unify
import qualified CoALP.Tests.Unit.Transform as Transform

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "Unit tests"
  [
      Unify.tests
    , Transform.tests
--    Subst.tests
--  , Guards.tests
  ]
