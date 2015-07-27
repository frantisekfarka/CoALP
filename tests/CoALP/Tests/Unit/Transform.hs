-- |
-- * Unit tests for transformation

module CoALP.Tests.Unit.Transform where

import System.FilePath.Posix (replaceExtension, takeBaseName, replaceFileName)

import CoALP.Transform
import CoALP.Program (Program, Program1, ProgramA, ClauseA, Clause1, Clause(..), TermA, Term1, Term(..), AnnoVar(..))
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
                    addTerm f1 (Var (Ind 1)) @?= f2
            , testCase "Transforming a function a single term" $
                    addTerm f2 (Var (Ind 2))  @?= f3
            , testCase "Transforming a function with multiple terms" $
                    addTerm f3 (Var (Ind 3)) @?= f4
            ]
  where
            f1 = Fun "f" [] :: TermA
            f2 = Fun "f" [Var (Ind 1)] :: TermA
            f3 = Fun "f" [Var (Ind 1), Var (Ind 2)] :: TermA
            f4 = Fun "f" [Var (Ind 1), Var (Ind 2), Var (Ind 3)] :: TermA

clauseTests :: TestTree
clauseTests = testGroup "Transforming clauses" [
              testCase "Transform clause with no body" $
                      tc1 @?= c2
              , testCase "Transform clause with body" $
                      tc2 @?= c4
              ]
  where
              h1 = Fun "f" [Var (Ind 1)] :: TermA
              h2 = Fun "f" [Var (Ind 1), Fun "transform-func-1" []] :: TermA
              h3 = Fun "f" [Var (Ind 1), Fun "transform-func-1" [Var (Ind 2)]] :: TermA
              b1 = Fun "g" [] :: TermA
              b2 = Fun "g" [Var (Ind 2)] :: TermA
              c1 = Clause h1 [] :: ClauseA
              c2 = Clause h2 [] :: ClauseA
              c3 = Clause h1 [b1] :: ClauseA
              c4 = Clause h3 [b2] :: ClauseA
              tc1 = transformClause (Fun "transform-func-1" []) c1
              tc2 = transformClause (Fun "transform-func-1" [Var (Ind 2)]) c3

programTests :: TestTree
programTests = testGroup "Transforming programs" [
               testCase "Transform program with a single clause" $
                       transformProgA (prog1, 5) @?= prog3
               , testCase "Transform program with multiple clauses" $
                       transformProgA (prog2, 5) @?= prog4
               , goldenTests
               ]
  where
              -- Initial
              var1 = Var (Ind 2) :: TermA
              var2 = Var (Ind 3) :: TermA
              var3 = Var (Ind 4) :: TermA
              fun1 = Fun "eq" [var1] :: TermA
              fun2 = Fun "eq" [var2] :: TermA
              fun3 = Fun "pair" [var1, var2] :: TermA
              fun4 = Fun "eq" [fun3] :: TermA
              fun5 = Fun "eq" [var3] :: TermA
              clause1 = Clause fun4 [fun1,fun2] :: ClauseA
              clause2 = Clause fun5 [] :: ClauseA
              prog1 = [clause1] :: ProgramA
              prog2 = [clause1, clause2] :: ProgramA
              -- Expected
              fun6 = Fun "eq" [var1, Var (Ind 5)] :: TermA
              fun7 = Fun "eq" [var2, Var (Ind 6)] :: TermA
              fun8 = Fun "pair" [var1, var2] :: TermA
              fun9 = Fun "transform-func-1" [Var (Ind 5), Var (Ind 6)] :: TermA
              fun10 = Fun "eq" [fun8, fun9] :: TermA
              fun11 = Fun "eq" [Var (Ind 4), Fun "transform-func-2" []] :: TermA
              clause3 = Clause fun10 [fun6, fun7] :: ClauseA
              clause4 = Clause fun11 [] :: ClauseA
              prog3 = ([clause3], 7) :: (ProgramA, Integer)
              prog4 = ([clause3, clause4], 7) :: (ProgramA, Integer)


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
                        where (transformed,_) = transformProgA (toProgramA $ reverse prg, count+1)

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
                            annotateTermA v1 @?= a1
                    , testCase "Annotate Function with single Var" $
                            annotateTermA t1 @?= ta1
                    , testCase "Annotate Function with multiple Vars" $
                            annotateTermA t2 @?= ta2
                    ]
          where
                    v1 = Var (Ind 1) :: TermA
                    a1 = Var (CoInd 1) :: TermA
                    v2 = map (Var . Ind) [1..10] :: [TermA]
                    a2 = (map (Var . Ind) [1..9]) ++  [Var (CoInd 10)] :: [TermA]
                    t1 = Fun "t1" [v1] :: TermA
                    ta1 = Fun "t1" [a1] :: TermA
                    t2 = Fun "t2" v2 :: TermA
                    ta2 = Fun "t2" a2 :: TermA

annotateClauseTests :: TestTree
annotateClauseTests =  testGroup "Clause Annotation Tests" [
                       testCase "Annotate Body with one term" $
                             annotateNth (annotateTermA) b1 t1 @?= a1
                     , testCase "Annotate Body with multiple terms" $
                             annotateNth (annotateTermA) b2 t2 @?= a2
                     , testCase "Attempt to Annotate term not in Body" $
                             annotateNth (annotateTermA) b2 t3 @?= b2
                     --, testCase "Annotate transformation functions with single var" $
                     --        annotateTransFunc v1 ts1 @?= as1
                     --, testCase "Annotate transformation function with multiple vars" $
                     --        annotateTransFunc v2 ts2 @?= as2
                     , testCase "Annotate Simple Clause" $
                             annotateClause (annotateTermA) c1 cv1 @?= ca1
                     , testCase "Annotate Complex Clause" $
                             annotateClause (annotateTermA) c2 cv2 @?= ca2
                     ]
          where
                    b1 = [Var (Ind 2)] :: [TermA]
                    t1 = 0 :: Int
                    a1 = [Var (CoInd 2)] :: [TermA]
                    b2 = map (Var . Ind) [1..10] :: [TermA]
                    t2 = 9 :: Int
                    a2 = (map (Var . Ind) [1..9]) ++ [Var (CoInd 10)] :: [TermA]
                    t3 = 20 :: Int
                    --v1 = 0
                    ts1 = [Var (Ind 1), Fun "transform-func-1" [Var (Ind 2)]]
                    as1 = [Var (Ind 1), Fun "transform-func-1" [Var (CoInd 2)]]
                    --v2  = 3
                    ts2 = [Var (Ind 1), Fun "g" [], Fun "transform-func-1" [Var (Ind 3), Var (Ind 4), Var (Ind 5), Var (Ind 6)]]
                    as2 = [Var (Ind 1), Fun "g" [], Fun "transform-func-1" [Var (Ind 3), Var (Ind 4), Var (Ind 5), Var (CoInd 6)]]
                    c1  = Clause (Fun "h" ts1) [Fun "h" [Var (Ind 1), Var (Ind 2)]]
                    cv1 = 0
                    ca1 = Clause (Fun "h" as1) [Fun "h" [Var (Ind 1), Var (CoInd 2)]]
                    c2  = Clause (Fun "h" ts2) [Fun "h" [Var (Ind 1), Var (Ind 3)], Fun "g" [Var (Ind 2), Var (Ind 4)], Fun "f" [Var (Ind 5)], Fun "g" [Var (Ind 6)]]
                    cv2 = 3
                    ca2 = Clause (Fun "h" as2) [Fun "h" [Var (Ind 1), Var (Ind 3)]
                          , Fun "g" [Var (Ind 2), Var (Ind 4)], Fun "f" [Var (Ind 5)], Fun "g" [Var (CoInd 6)]]
                    
annotateProgramTests :: TestTree
annotateProgramTests =  testGroup "Program Annotation Tests" [
                          testCase "Annotate one clause Program" $
                                annotateProgA p1 l1 @?= pa1
                        , testCase "Annotate multi clause Program" $
                                annotateProgA p2 l2 @?= pa2
                        , testCase "Annotate clause mulitple times" $
                                annotateProgA p3 l3 @?= pa3
                        , testCase "Annotate unguared 2" $
                                annotateProgA p4 l4 @?= pa4
                        , annotationGoldenTests
                        ]
          where 
                    ts1 = [Var (Ind 1), Fun "g" [], Fun "transform-func-1" [Var (Ind 13), Var (Ind 14), Var (Ind 15), Var (Ind 16)]]
                    as1 = [Var (Ind 1), Fun "g" [], Fun "transform-func-1" [Var (Ind 13), Var (Ind 14), Var (Ind 15), Var (CoInd 16)]]
                    c1  = Clause (Fun "h" ts1) [Fun "h" [Var (Ind 1), Var (Ind 13)], Fun "g" [Var (Ind 2), Var (Ind 14)], Fun "f" [Var (Ind 15)], Fun "g" [Var (Ind 16)]]
                    ca1 = Clause (Fun "h" as1) [Fun "h" [Var (Ind 1), Var (Ind 13)], Fun "g" [Var (Ind 2), Var (Ind 14)], Fun "f" [Var (Ind 15)], Fun "g" [Var (CoInd 16)]]
                    p1  = [c1]
                    l1  = [(0,3)] -- Clause 0, term 3
                    pa1 = [ca1]
                    ts2 = [Var (Ind 1), Var (Ind 2), Fun "f" [Var (Ind 3)], Fun "transform-func-2" [Var (Ind 13), Var (Ind 14), Var (Ind 15), Var (Ind 16)]]
                    as2 = [Var (Ind 1), Var (Ind 2), Fun "f" [Var (Ind 3)], Fun "transform-func-2" [Var (Ind 13), Var (CoInd 14), Var (Ind 15), Var (Ind 16)]]
                    c2  = Clause (Fun "g" ts2) [Fun "h" [Var (Ind 1), Var (Ind 13)], Fun "g" [Var (Ind 2), Var (Ind 3), Var (Ind 14)], Fun "f" [Var (Ind 15)], Fun "g" [Var (Ind 16)]]
                    ca2 = Clause (Fun "g" as2) [Fun "h" [Var (Ind 1), Var (Ind 13)], Fun "g" [Var (Ind 2), Var (Ind 3), Var (CoInd 14)], Fun "f" [Var (Ind 15)], Fun "g" [Var (Ind 16)]]
                    as3 = [Var (Ind 1), Fun "g" [], Fun "transform-func-1" [Var (CoInd 13), Var (Ind 14), Var (Ind 15), Var (Ind 16)]]
                    ca3 = Clause (Fun "h" as3) [Fun "h" [Var (Ind 1), Var (CoInd 13)], Fun "g" [Var (Ind 2), Var (Ind 14)], Fun "f" [Var (Ind 15)], Fun "g" [Var (Ind 16)]]
                    as4 = [Var (Ind 1), Fun "g" [], Fun "transform-func-1" [Var (Ind 13), Var (CoInd 14), Var (Ind 15), Var (CoInd 16)]]
                    ca4 = Clause (Fun "h" as4) [Fun "h" [Var (Ind 1), Var (Ind 13)], Fun "g" [Var (Ind 2), Var (CoInd 14)], Fun "f" [Var (Ind 15)], Fun "g" [Var (CoInd 16)]]
                    p2  = [c1, c2, c1]
                    l2  = [(1,1),(0,3),(2, 0)]
                    pa2 = [ca1, ca2, ca3]
                    p3  = [c1]
                    l3  = [(0,3), (0,1)]
                    pa3 = [ca4]
                    cp1 = Clause (Fun "p" [Fun "1" [], Fun "trans-fun-1" [] ]) []
                    cp2 = Clause (Fun "p" [Var (Ind 2), Fun "trans-fun-2" [Var (Ind 4), Var (Ind 5), Var (Ind 6)]]) [Fun "q" [Var (Ind 2), Var (Ind 4)], Fun "p2" [Var (Ind 2), Var (Ind 5)], Fun "q" [Var (Ind 2), Var (Ind 6)]]
                    cp3 = Clause (Fun "p2" [Var (Ind 3), Fun "trans-fun-3" [Var (Ind 7)]]) [Fun "p" [Var (Ind 3), Var (Ind 7)]]
                    p4  = [cp1, cp2, cp3]
                    l4  = [(2,0), (1,1)]
                    pa4 = [ cp1
                          , Clause (Fun "p" [Var (Ind 2), Fun "trans-fun-2" [Var (Ind 4), Var (CoInd 5), Var (Ind 6)]]) [Fun "q" [Var (Ind 2), Var (Ind 4)], Fun "p2" [Var (Ind 2), Var (CoInd 5)], Fun "q" [Var (Ind 2), Var (Ind 6)]]
                          , Clause (Fun "p2" [Var (Ind 3), Fun "trans-fun-3" [Var (CoInd 7)]]) [Fun "p" [Var (Ind 3), Var (CoInd 7)]]
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
                        where p = toProgramA $ reverse prg
                              (transformed, _) = transformProgA (p, count+1)
                              annotated = annotateProgA transformed (getProgramLoops p)
