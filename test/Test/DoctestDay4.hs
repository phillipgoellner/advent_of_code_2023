module Test.DoctestDay4 (day4Spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day4 (part1, part2)

givenLines = ["Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
              "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
              "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
              "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
              "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
              "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"]


day4Spec :: Spec
day4Spec = describe "Day 4" $ do
    describe "Part 1" $ do
        it "acceptance" $ 
            part1 givenLines `shouldBe` 13
        
        it "winning card is registered" $
            part1 ["Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"] `shouldBe` 1
        
        it "score is calculated" $
            part1 ["Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"] `shouldBe` 8

    describe "Part 2" $ do
        it "acceptance" $ 
            part2 givenLines `shouldBe` 30
