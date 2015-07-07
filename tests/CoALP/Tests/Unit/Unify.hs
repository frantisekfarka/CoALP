-- |
-- * Unit tests for unification

module CoALP.Tests.Unit.Unify where

import CoALP.Unify (unify)
import CoALP.Program (Term(..),Term1,Subst1)

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests



tests :: TestTree
tests = testGroup "Term and atom MGUs" [
	testCase "A variable against a constant" $
		v1 `unify` c1 @?= Just s1
	, testCase "A constant against a variable" $
		c1 `unify` v1 @?= Just s1
	, testCase "A variable against a constant, inside of an atom" $
		(bit v1) `unify` (bit c1) @?= Just s1
	, testCase "Mutual variables in between terms" $
		(t1) `unify` (t2) @?= Just s12
	]
  where
	v1 = Var 1 :: Term1
	c1 = Fun "c" [] :: Term1
	s1 = [(1, Fun "c" [])] :: Subst1
	bit x = Fun "bit" [x] -- :: Term1 -> Term1
	t1 = Fun "q" [Var 5, Fun "s" [Fun "t" [Var 10]]] :: Term1
	t2 = Fun "q" [Fun "s" [Fun "t" [Var 2]], Var 2] :: Term1
	s12 = [  (5, Fun "s" [Fun "t" [Fun "s" [Fun "t" [Var 10]]]])
		,(2, Fun "s" [Fun "t" [Var 10]])
		]



