-- |
-- * Unit tests for anti-unification

module CoALP.Tests.Unit.AntiUnify where

import CoALP.AntiUnify
import CoALP.Program (Term(..), Term1, TermA, AnnoVar(..))
--import Data.List (intersperse)

import Test.Tasty
import Test.Tasty.HUnit
--import Test.Tasty.Golden

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Anti-Unification Tests" [
           antiUnifyTests
        ]

antiUnifyTests :: TestTree
antiUnifyTests = testGroup "Anti Unify tests" [
            testCase "D-list test" $
                    antiUnify f1 f2 @?= a1
            , testCase "Tom test" $
                    antiUnify f3 f4 @?= a2
            , testCase "Nested test" $
                    antiUnify f5 f6 @?= a3
            , testCase "Non-Matching top level symbol" $
                    antiUnify f7 f8 @?= a4
            , testCase "Single symbol non-matching in nested terms" $
                    antiUnify f9 f10 @?= a5
            , testCase "Martin example" $
                    antiUnify f11 f12 @?= a6
            , testCase "Applicative form of D-list" $
                    antiUnify f13 f14 @?= a7
            , testCase "Martin Example with starting count of 8" $
                    antiUnifyWithCount f11 f12 8 @?= a8
            , testCase "Tom test with AnnoVar" $
                    antiUnify f15 f16 @?= a9
            , testCase "Martin example with AnnoVar" $
                    antiUnify f17 f18 @?= a10
            , testCase "Martin Example with starting count of 8 and AnnoVar" $
                    antiUnifyWithCount f17 f18 (Ind 8) @?= a11
            ]
  where
            f1 = Fun "Eq" [Fun "D" [Fun "Char" []]] :: Term1
            f2 = Fun "Eq" [Fun "D" [Fun "D" [Fun "Char" []]]] :: Term1
            a1 = Fun "Eq" [Fun "D" [Var 2]] :: Term1
            f3 = Fun "Eq" [Fun "Fix" [Fun "Pair" [], Fun "Pair" []]] :: Term1
            f4 = Fun "Eq" [Fun "Fix" [Fun "Comp" [Fun "Pair" [], Fun "Pair" []], Fun "Pair" []]] :: Term1
            a2 = Fun "Eq" [Fun "Fix" [Var 2, Fun "Pair" []]] :: Term1
            f5 = Fun "f" [Fun "a" []] :: Term1
            f6 = Fun "f" [Fun "f" [Fun "f" [Fun "f" [Fun "a" []]]]] :: Term1
            a3 = Fun "f" [Var 2] :: Term1
            f7 = Fun "f" [] :: Term1
            f8 = Fun "g" [] :: Term1
            a4 = Var 2 :: Term1
            f9 = Fun "p" [Fun "f" [Fun "x" []], Fun "y"[]] :: Term1
            f10 = Fun "p" [Fun "x" [], Fun "y" []] :: Term1
            a5  = Fun "p" [Var 2, Fun "y" []] :: Term1
            f11 = Fun "p" [Fun "a" [], Fun "a" []] :: Term1
            f12 = Fun "p" [Fun "b" [], Fun "c" []] :: Term1
            a6  = Fun "p" [Var 2, Var 4]  :: Term1
            f13 = Fun "App" [Fun "Eq" [], Fun "App" [Fun "D" [], Fun "Char" []]] :: Term1
            f14 = Fun "App" [Fun "Eq" [], Fun "App" [Fun "D" [], Fun "App" [Fun "D" [], Fun "Char" []]]] :: Term1
            a7  = Fun "App" [Fun "Eq" [], Fun "App" [Fun "D" [], Var 2]] :: Term1
            a8  = Fun "p" [Var 16, Var 32] :: Term1
            f15 = Fun "App" [Fun "Eq" [], Fun "App" [Fun "D" [], Fun "Char" []]] :: TermA
            f16 = Fun "App" [Fun "Eq" [], Fun "App" [Fun "D" [], Fun "App" [Fun "D" [], Fun "Char" []]]] :: TermA
            a9  = Fun "App" [Fun "Eq" [], Fun "App" [Fun "D" [], Var (Ind 2)]] :: TermA
            f17 = Fun "p" [Fun "a" [], Fun "a" []] :: TermA
            f18 = Fun "p" [Fun "b" [], Fun "c" []] :: TermA
            a10  = Fun "p" [Var (Ind 2), Var (Ind 4)]  :: TermA
            a11  = Fun "p" [Var (Ind 16), Var (Ind 32)] :: TermA
            
