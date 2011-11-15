
module Test.Ribot.Generate
    ( generateTests
    ) where

import qualified Data.List as L
import           System.Random
import           Test.HUnit (Assertion, assertBool)
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Text.Ribot.Generate

-- This is from the sample trigrams for the Google Web 1T 5-gram v1.
testTrain :: TextGenerator String
testTrain = mkTextGenerator . L.concat $ trainingData
    where
        trainingData = [ L.replicate 55   ("ceramics", "collectables", "collectibles")
                       , L.replicate 130  ("ceramics", "collectables", "fine")
                       , L.replicate 52   ("ceramics", "collected", "by")
                       , L.replicate 50   ("ceramics", "collectible", "pottery")
                       , L.replicate 45   ("ceramics", "collectibles", "cooking")
                       , L.replicate 144  ("ceramics", "collection", ",")
                       , L.replicate 247  ("ceramics", "collection", ".")
                       , L.replicate 120  ("ceramics", "collection", "</S>")
                       , L.replicate 43   ("ceramics", "collection", "and")
                       , L.replicate 52   ("ceramics", "collection", "at")
                       , L.replicate 68   ("ceramics", "collection", "is")
                       , L.replicate 76   ("ceramics", "collection", "of")
                       , L.replicate 59   ("ceramics", "collection", "|")
                       , L.replicate 66   ("ceramics", "collections", ",")
                       , L.replicate 60   ("ceramics", "collections", ".")
                       , L.replicate 46   ("ceramics", "combined", "with")
                       , L.replicate 69   ("ceramics", "come", "from")
                       , L.replicate 660  ("ceramics", "comes", "from")
                       , L.replicate 109  ("ceramics", "community", ",")
                       , L.replicate 212  ("ceramics", "community", ".")
                       , L.replicate 61   ("ceramics", "community", "for")
                       , L.replicate 53   ("ceramics", "companies", ".")
                       , L.replicate 173  ("ceramics", "companies", "consultants")
                       , L.replicate 4432 ("ceramics", "company", "!")
                       , L.replicate 133  ("ceramics", "company", ",")
                       , L.replicate 92   ("ceramics", "company", ".")
                       , L.replicate 41   ("ceramics", "company", "</S>")
                       , L.replicate 145  ("ceramics", "company", "facing")
                       , L.replicate 181  ("ceramics", "company", "in")
                       , L.replicate 137  ("ceramics", "company", "started")
                       , L.replicate 87   ("ceramics", "company", "that")
                       , L.replicate 76   ("ceramics", "component", "(")
                       , L.replicate 85   ("ceramics", "composed", "of")
                       , L.replicate 56   ("ceramics", "composites", "ferrites")
                       , L.replicate 41   ("ceramics", "composition", "as")
                       , L.replicate 51   ("ceramics", "computer", "graphics")
                       , L.replicate 52   ("ceramics", "computer", "imaging")
                       , L.replicate 92   ("ceramics", "consist", "of")
                       ]

assertMostLikely :: Assertion
assertMostLikely = mapM_ (uncurry aml') inputs
    where
        hmm = testTrain
        aml' obs expected = assertBool ("most likely " ++ (show obs) ++ " =>" ++ (show ml))
                                       (expected == ml)
                            where ml = mostLikely hmm obs
        inputs = [ (("ceramics", "collectables"), Just "fine")
                 , (("ceramics", "collection"), Just ".")
                 , (("ceramics", "comes"), Just "from")
                 , (("ceramics", "company"), Just "!")
                 , (("ceramics", "not here"), Nothing)
                 ]

-- Random numbers will look something like this:
--
--     ghci> setStdGen $ mkStdGen 0
--     ghci> mapM (\x -> randomRIO (0.0, 1.0) >>= return . (\y -> (x, y))) [1..10]
--     [(1,0.9872770353686064),(2,9.953808039426804e-4),(3,0.9737700945697725),(4,0.130280438112095),(5,0.2037222092039883),(6,0.5737648080103099),(7,0.9673066593240947),(8,0.6253049722872674),(9,0.5327889025211334),(10,0.45740725565701723)]

resetRandom :: IO ()
resetRandom = setStdGen $ mkStdGen 0

assertRandomContinuation :: Assertion
assertRandomContinuation = do
    resetRandom
    mapM_ (uncurry app) inputs
    where
        hmm = testTrain
        app obs expected = assertBool ("random continuation " ++ (show obs) ++ " => " ++ (show pp))
                                 (expected == pp)
                            where pp = randomContinuation hmm obs
        inputs = [ (("ceramics", "comes"), Just "from")      -- [(1,0.9872770353686064)
                 , (("ceramics", "community"), Just ".")     -- ,(2,9.953808039426804e-4)
                 , (("ceramics", "company"), Just ".")       -- ,(3,0.9737700945697725)
                 , (("cermaics", "not here"), Nothing)
                 ]
-- ,(4,0.130280438112095)
-- ,(5,0.2037222092039883)
-- ,(6,0.5737648080103099)
-- ,(7,0.9673066593240947)
-- ,(8,0.6253049722872674)
-- ,(9,0.5327889025211334)
-- ,(10,0.45740725565701723)]

assertTriples :: Assertion
assertTriples =
    assertBool ("assertTriples => " ++ (show obs)) (expected == obs)
    where
        expected = [ ('#', '#', '0')
                   , ('#', '0', '1')
                   , ('0', '1', '2')
                   , ('1', '2', '3')
                   , ('2', '3', '4')
                   , ('3', '4', '5')
                   , ('4', '5', '6')
                   , ('5', '6', '7')
                   , ('6', '7', '8')
                   , ('7', '8', '9')
                   , ('8', '9', '#')
                   , ('9', '#', '#')
                   ]
        obs = triples '#' "0123456789"

generateTests :: [Test]
generateTests =
    [ testGroup "generate" [ testCase "most-likely" assertMostLikely
                           , testCase "random-continuation" assertRandomContinuation
                           , testCase "triples" assertTriples
                           ]
    ]

