module Test.DoctestDay7 (day7Spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day7 (part1, part2)
import Data.List (sortBy)

givenLines = ["32T3K 765",
              "T55J5 684",
              "KK677 28",
              "KTJJT 220",
              "QQQJA 483"]


day7Spec :: Spec
day7Spec = describe "Day 7" $ do
    describe "Part 1" $ do
        it "acceptance" $ 
            part1 givenLines `shouldBe` 6440

        it "single hand has bid as total winning" $ 
            part1 ["32T3K 765"] `shouldBe` 765

        it "equal kinds of hands are ordered by cards" $ 
            part1 ["KK677 28",
                   "KTJJT 220"] `shouldBe` 276

        it "different kinds of hands are ordered by kind" $ 
            part1 ["T55J5 10", "KK677 20"] `shouldBe` 40

    describe "Part 2" $ do
        it "acceptance" $ 
            part2 givenLines `shouldBe` 5905

        it "J is interpreted as 'T'" $ 
            part2 ["KK677 28", "KTJJT 220"] `shouldBe` 468

        it "J is interpreted as 'T'" $ 
            part2 ["JKKK2 1", "QQQQ2 2"] `shouldBe` 5
