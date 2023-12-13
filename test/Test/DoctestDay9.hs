module Test.DoctestDay9 (day9Spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day9 (part1, part2)

givenLines = ["0 3 6 9 12 15",
              "1 3 6 10 15 21",
              "10 13 16 21 30 45"]


day9Spec :: Spec
day9Spec = describe "Day 9" $ do
    describe "Part 1" $ do
        it "acceptance" $ 
            part1 givenLines `shouldBe` 114

        it "predicts one line" $ 
            part1 ["0 3 6 9 12 15"] `shouldBe` 18

    describe "Part 2" $ do
        it "acceptance" $ 
            part2 givenLines `shouldBe` 2

