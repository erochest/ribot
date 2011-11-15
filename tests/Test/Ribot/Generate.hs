
module Test.Ribot.Generate
    ( generateTests
    ) where

import           Test.HUnit (Assertion, assertBool)
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Text.Ribot.Generate

assertTraining :: Assertion
assertTraining = assertBool "assertTraining" False

assertMostLikely :: Assertion
assertMostLikely = assertBool "assertMostLikely" False

assertPropabilityPick :: Assertion
assertPropabilityPick = assertBool "assertPropabilityPick" False

generateTests :: [Test]
generateTests =
    [ testGroup "generate" [ testCase "train" assertTraining
                           , testCase "most-likely" assertMostLikely
                           , testCase "probability-pick" assertPropabilityPick
                           ]
    ]

