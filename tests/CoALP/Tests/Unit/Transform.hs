-- |
-- * Unit tests for transformation

module CoALP.Tests.Unit.Transform where

import System.FilePath.Posix (replaceExtension, takeBaseName, replaceFileName)

import CoALP.Transform
import CoALP.Program (Program, Program1,Clause1, Clause(..),Term1,Term(..))
import CoALP.Guards (getProgramLoops)
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
	, realisationTransformTests
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
goldenLocation = "tests/CoALP/Tests/Golden/Transform/"

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

realisationTransformTests :: TestTree
realisationTransformTests = testGroup "Realization Transformation Tests with Annotation" [
                              annotateTermTests
                            , annotateClauseTests
                            , annotateProgramTests
                            ]

annotateTermTests :: TestTree
annotateTermTests =  testGroup "Term Annotation Tests" [
                      testCase "Annotate single Var" $
                            annotateLast v1 @?= a1
                    , testCase "Annotate last Var in list" $
                            annotateLast v2 @?= a2
                    , testCase "Annotate Function with single Var" $
                            annotateTerm t1 @?= ta1
                    , testCase "Annotate Function with multiple Vars" $
                            annotateTerm t2 @?= ta2
                    ]
          where
                    v1 = [Var 1] :: [Term1]
                    a1 = [Var (-1)] :: [Term1]
                    v2 = map (Var) [1..10] :: [Term1]
                    a2 = (map (Var) [1..9]) ++  [Var (-10)] :: [Term1]
                    t1 = Fun "t1" v1 :: Term1
                    ta1 = Fun "t1" a1 :: Term1
                    t2 = Fun "t2" v2 :: Term1
                    ta2 = Fun "t2" a2 :: Term1

annotateClauseTests :: TestTree
annotateClauseTests =  testGroup "Clause Annotation Tests" [
                       testCase "Annotate Body with one term" $
                             annotateBody b1 t1 @?= a1
                     , testCase "Annotate Body with multiple terms" $
                             annotateBody b2 t2 @?= a2
                     , testCase "Attempt to Annotate term not in Body" $
                             annotateBody b2 t3 @?= b2
                     , testCase "Annotate transformation functions with single var" $
                             annotateTransFunc v1 ts1 @?= as1
                     , testCase "Annotate transformation function with multiple vars" $
                             annotateTransFunc v2 ts2 @?= as2
                     , testCase "Annotate Simple Clause" $
                             annotateClause c1 cv1 @?= ca1
                     , testCase "Annotate Complex Clause" $
                             annotateClause c2 cv2 @?= ca2
                     ]
          where
                    b1 = [Var 2] :: [Term1]
                    t1 = 0 :: Int
                    a1 = [Var (-2)] :: [Term1]
                    b2 = map (Var) [1..10] :: [Term1]
                    t2 = 9 :: Int
                    a2 = (map (Var) [1..9]) ++ [Var (-10)] :: [Term1]
                    t3 = 20 :: Int
                    v1 = 0
                    ts1 = [Var 1, Fun "transform-func-1" [Var 2]]
                    as1 = [Var 1, Fun "transform-func-1" [Var (-2)]]
                    v2  = 3
                    ts2 = [Var 1, Fun "g" [], Fun "transform-func-1" [Var 3, Var 4, Var 5, Var 6]]
                    as2 = [Var 1, Fun "g" [], Fun "transform-func-1" [Var 3, Var 4, Var 5, Var (-6)]]
                    c1  = Clause (Fun "h" ts1) [Fun "h" [Var 1, Var 2]]
                    cv1 = 0
                    ca1 = Clause (Fun "h" as1) [Fun "h" [Var 1, Var (-2)]]
                    c2  = Clause (Fun "h" ts2) [Fun "h" [Var 1, Var 3], Fun "g" [Var 2, Var 4], Fun "f" [Var 5], Fun "g" [Var 6]]
                    cv2 = 3
                    ca2 = Clause (Fun "h" as2) [Fun "h" [Var 1, Var 3], Fun "g" [Var 2, Var 4], Fun "f" [Var 5], Fun "g" [Var (-6)]]
                    
annotateProgramTests :: TestTree
annotateProgramTests =  testGroup "Program Annotation Tests" [
                          testCase "Annotate one clause Program" $
                                annotateProg p1 l1 @?= pa1
                        , testCase "Annotate multi clause Program" $
                                annotateProg p2 l2 @?= pa2
                        , testCase "Annotate clause mulitple times" $
                                annotateProg p3 l3 @?= pa3
                        , testCase "Annotate unguared 2" $
                                annotateProg p4 l4 @?= pa4
                        , annotationGoldenTests
                        ]
          where 
                    ts1 = [Var 1, Fun "g" [], Fun "transform-func-1" [Var 13, Var 14, Var 15, Var 16]]
                    as1 = [Var 1, Fun "g" [], Fun "transform-func-1" [Var 13, Var 14, Var 15, Var (-16)]]
                    c1  = Clause (Fun "h" ts1) [Fun "h" [Var 1, Var 13], Fun "g" [Var 2, Var 14], Fun "f" [Var 15], Fun "g" [Var 16]]
                    ca1 = Clause (Fun "h" as1) [Fun "h" [Var 1, Var 13], Fun "g" [Var 2, Var 14], Fun "f" [Var 15], Fun "g" [Var (-16)]]
                    p1  = [c1]
                    l1  = [(0,3)] -- Clause 0, term 3
                    pa1 = [ca1]
                    ts2 = [Var 1, Var 2, Fun "f" [Var 3], Fun "transform-func-2" [Var 13, Var 14, Var 15, Var 16]]
                    as2 = [Var 1, Var 2, Fun "f" [Var 3], Fun "transform-func-2" [Var 13, Var (-14), Var 15, Var 16]]
                    c2  = Clause (Fun "g" ts2) [Fun "h" [Var 1, Var 13], Fun "g" [Var 2, Var 3, Var 14], Fun "f" [Var 15], Fun "g" [Var 16]]
                    ca2 = Clause (Fun "g" as2) [Fun "h" [Var 1, Var 13], Fun "g" [Var 2, Var 3, Var (-14)], Fun "f" [Var 15], Fun "g" [Var 16]]
                    as3 = [Var 1, Fun "g" [], Fun "transform-func-1" [Var (-13), Var 14, Var 15, Var 16]]
                    ca3 = Clause (Fun "h" as3) [Fun "h" [Var 1, Var (-13)], Fun "g" [Var 2, Var 14], Fun "f" [Var 15], Fun "g" [Var 16]]
                    as4 = [Var 1, Fun "g" [], Fun "transform-func-1" [Var 13, Var (-14), Var 15, Var (-16)]]
                    ca4 = Clause (Fun "h" as4) [Fun "h" [Var 1, Var 13], Fun "g" [Var 2, Var (-14)], Fun "f" [Var 15], Fun "g" [Var (-16)]]
                    p2  = [c1, c2, c1]
                    l2  = [(1,1),(0,3),(2, 0)]
                    pa2 = [ca1, ca2, ca3]
                    p3  = [c1]
                    l3  = [(0,3), (0,1)]
                    pa3 = [ca4]
                    cp1 = Clause (Fun "p" [Fun "1" [], Fun "trans-fun-1" [] ]) []
                    cp2 = Clause (Fun "p" [Var 2, Fun "trans-fun-2" [Var 4, Var 5, Var 6]]) [Fun "q" [Var 2, Var 4], Fun "p2" [Var 2, Var 5], Fun "q" [Var 2, Var 6]]
                    cp3 = Clause (Fun "p2" [Var 3, Fun "trans-fun-3" [Var 7]]) [Fun "p" [Var 3, Var 7]]
                    p4  = [cp1, cp2, cp3]
                    l4  = [(2,0), (1,1)]
                    pa4 = [ cp1
                          , Clause (Fun "p" [Var 2, Fun "trans-fun-2" [Var 4, Var (-5), Var 6]]) [Fun "q" [Var 2, Var 4], Fun "p2" [Var 2, Var (-5)], Fun "q" [Var 2, Var 6]]
                          , Clause (Fun "p2" [Var 3, Fun "trans-fun-3" [Var (-7)]]) [Fun "p" [Var 3, Var (-7)]]
                          ]

annotationGoldenTests :: TestTree
annotationGoldenTests = createAnnotationTests (map (annotationLocation ++) annotationFiles)

-- File names for golden tests
annotationFiles :: [String]
annotationFiles = [ "simpleLoop.golden", "unguarded1.golden", "unguarded2.golden", "unguarded3.golden", "unguarded4.golden", "unguarded5.golden"
                  , "unguarded7.golden", "unguarded8.golden", "unguarded9.golden", "unguarded10.golden", "unguarded11.golden", "unguarded12.golden"
                  , "unguarded13.golden", "unguarded14.golden", "unguarded15.golden", "unguarded16.golden", "unguarded17.golden", "unguarded19.golden"
                  , "unguarded20.golden", "unguarded21.golden", "unguarded22.golden", "unguarded23.golden", "unguarded24.golden", "unguarded25.golden"
                  , "unguarded27.golden"]
-- File location for golden tests
annotationLocation :: String
annotationLocation = "tests/CoALP/Tests/Golden/Annotate/"

createAnnotationTests :: [FilePath] -> TestTree
createAnnotationTests files = testGroup "File Annotation Golden Tests" (map (\x -> goldenVsFile (testName x) x (outputName x) (annoFile (logicName x) (outputName x))) files)
          where testName f = (takeBaseName f) ++ " Annotation test"
                logicName f = replaceExtension f ".logic"
                outputName f = replaceFileName (logicName f) ((takeBaseName f) ++ "-annotated.logic")

annoFile :: FilePath -> FilePath -> IO ()
annoFile source dest = do 
         cnt <- readFile source
         case parseWithCount cnt of
                Left err         -> do
                        putStrLn err
                        return ()
                Right (prg, count) -> do
                        writeBinaryFile dest ((ppProg annotated) ++ "\n")
                        where p = reverse prg
                              transformed = transformProg (p, count+1)
                              annotated = annotateProg transformed (getProgramLoops p)
