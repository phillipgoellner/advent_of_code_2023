module Test.DoctestDay6 (day6Spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day6 (part1, part2)

givenLines = ["Time:      7  15   30",
              "Distance:  9  40  200"]


day6Spec :: Spec
day6Spec = describe "Day 6" $ do
    describe "Part 1" $ do
        it "acceptance" $ 
            part1 givenLines `shouldBe` 288

        it "one race" $ 
            part1 ["Time:      7",
                   "Distance:  9"] `shouldBe` 4

    describe "Part 1" $ do
        it "acceptance" $ 
            part2 givenLines `shouldBe` 71503
