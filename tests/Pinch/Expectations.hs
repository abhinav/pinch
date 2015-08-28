module Pinch.Expectations
    ( leftShouldContain
    ) where


import Test.Hspec

-- | Expectation
leftShouldContain :: Show a => Either String a -> String -> Expectation
leftShouldContain (Right a) _ =
    expectationFailure $ "Expected failure but got: " ++ show a
leftShouldContain (Left msg) x = msg `shouldContain` x
infix 1 `leftShouldContain`
