module Pinch.Expectations
    ( leftShouldContain
    , leftShouldContainAll
    ) where


import Test.Hspec

-- | Expectation
leftShouldContain :: Show a => Either String a -> String -> Expectation
leftShouldContain (Right a) _ =
    expectationFailure $ "Expected failure but got: " ++ show a
leftShouldContain (Left msg) x = msg `shouldContain` x
infix 1 `leftShouldContain`

leftShouldContainAll :: Show a => Either String a -> [String] -> Expectation
leftShouldContainAll e = mapM_ (leftShouldContain e)
