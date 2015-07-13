-- |
-- * Unit tests for transformation

module CoALP.Tests.Unit.Transform where

import System.FilePath.Posix (replaceExtension, takeBaseName, replaceFileName)

import CoALP.Transform
import CoALP.Program (Program, Program1,Clause1, Clause(..),Term1,Term(..))
import CoALP.Parser.Parser (parseWithCount)
import CoALP.Parser.PrettyPrint
import Data.List (intersperse)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Transformation Tests" [
          termTests
        , clauseTests
        , programTests
        ]


termTests :: TestTree
termTests = testGroup "Transforming terms" [
            testCase "Transforming a function with no terms" $
                    addVar 1 f1 @?= f2
            , testCase "Transforming a function a single term" $
                    addVar 2 f2 @?= f3
            , testCase "Transforming a function with multiple terms" $
                    addVar 3 f3 @?= f4
            ]
  where
            f1 = Fun "f" [] :: Term1
            f2 = Fun "f" [Var 1] :: Term1
            f3 = Fun "f" [Var 1, Var 2] :: Term1
            f4 = Fun "f" [Var 1, Var 2, Var 3] :: Term1

clauseTests :: TestTree
clauseTests = testGroup "Transforming clauses" [
              testCase "Transform the head of a clause with no body" $
                      transformHead 1 0 1 h1 @?= h2
              , testCase "Transform the head of a clause when the body has a single term" $
                      transformHead 2 1 1 h1 @?= h3
              , testCase "Transform the head of a clause when the body has two terms" $
                      transformHead 2 2 1 h1 @?= h4
              , testCase "Transform the body of a clause with no terms" $
                      transformBody 1 [] @?= []
              , testCase "Transform the body of a clause with a single term" $
                      transformBody 2 [b1] @?= [b2]
              , testCase "Transform the body of a clause with multiple terms" $
                      transformBody 3 [b1,h1,b2] @?= [b3,b4,b5]
              , testCase "Transform clause with no body" $
                      tc1 @?= c2
              , testCase "Transform clause with no body next fresh variable" $
                      count1 @?= 1
              , testCase "Transform clause with body" $
                      tc2 @?= c4
              , testCase "Transform clause with body next fresh variable" $
                      count2 @?= 3
              ]
  where
              h1 = Fun "f" [Var 1] :: Term1
              h2 = Fun "f" [Var 1, Fun "transform-func-1" []] :: Term1
              h3 = Fun "f" [Var 1, Fun "transform-func-1" [Var 2]] :: Term1
              h4 = Fun "f" [Var 1, Fun "transform-func-1" [Var 2, Var 3]] :: Term1
              b1 = Fun "g" [] :: Term1
              b2 = Fun "g" [Var 2] :: Term1
              b3 = Fun "g" [Var 3] :: Term1 
              b4 = Fun "f" [Var 1, Var 4] :: Term1
              b5 = Fun "g" [Var 2, Var 5] :: Term1
              c1 = Clause h1 [] :: Clause1
              c2 = Clause h2 [] :: Clause1
              c3 = Clause h1 [b1] :: Clause1
              c4 = Clause h3 [b2] :: Clause1
              (tc1, count1) = transformClause 1 1 c1
              (tc2, count2) = transformClause 2 1 c3

programTests :: TestTree
programTests = testGroup "Transforming programs" [
               testCase "Transform program with a single clause" $
                       transformProg (prog1, 5) @?= prog3
               , testCase "Transform program with multiple clauses" $
                       transformProg (prog2, 5) @?= prog4
               , goldenTests
               ]
  where
              -- Initial
              var1 = Var 2 :: Term1
              var2 = Var 3 :: Term1
              var3 = Var 4 :: Term1
              fun1 = Fun "eq" [var1] :: Term1
              fun2 = Fun "eq" [var2] :: Term1
              fun3 = Fun "pair" [var1, var2] :: Term1
              fun4 = Fun "eq" [fun3] :: Term1
              fun5 = Fun "eq" [var3] :: Term1
              clause1 = Clause fun4 [fun1,fun2] :: Clause1
              clause2 = Clause fun5 [] :: Clause1
              prog1 = [clause1] :: Program1
              prog2 = [clause1, clause2] :: Program1
              -- Expected
              fun6 = Fun "eq" [var1, Var 5] :: Term1
              fun7 = Fun "eq" [var2, Var 6] :: Term1
              fun8 = Fun "pair" [var1, var2] :: Term1
              fun9 = Fun "transform-func-1" [Var 5, Var 6] :: Term1
              fun10 = Fun "eq" [fun8, fun9] :: Term1
              fun11 = Fun "eq" [Var 4, Fun "transform-func-2" []] :: Term1
              clause3 = Clause fun10 [fun6, fun7] :: Clause1
              clause4 = Clause fun11 [] :: Clause1
              prog3 = [clause3] :: Program1
              prog4 = [clause3, clause4] :: Program1


goldenTests :: TestTree
goldenTests = createGoldenTests (map (goldenLocation ++) goldenFiles)

-- File names for golden tests
goldenFiles :: [String]
goldenFiles = ["btree.golden","eq.golden"]
-- File location for golden tests
goldenLocation :: String
goldenLocation = "tests/CoALP/Tests/Golden/"

createGoldenTests :: [FilePath] -> TestTree
createGoldenTests files = testGroup "File transformaton Golden Tests" (map (\x -> goldenVsFile (testName x) x (outputName x) (transFile (logicName x) (outputName x))) files)
          where testName f = (takeBaseName f) ++ " transformation test"
                logicName f = replaceExtension f ".logic"
                outputName f = replaceFileName (logicName f) ((takeBaseName f) ++ "-transformed.logic")

transFile :: FilePath -> FilePath -> IO ()
transFile source dest = do 
          cnt <- readFile source
          case parseWithCount cnt of
                Left err         -> do
                        putStrLn err
                        return ()
                Right (prg, count) -> do
                        writeBinaryFile dest ((ppProg transformed) ++ "\n")
                        where transformed = transformProg (reverse prg, count+1)

ppProg :: (Show a, Show b, Show c) => Program a b c -> String
ppProg = concat . intersperse "\n" . (map ppClause) 
