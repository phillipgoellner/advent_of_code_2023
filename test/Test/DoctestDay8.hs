module Test.DoctestDay8 (day8Spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day8 (part1, part2)

givenLines = ["LLR",
              "",
              "AAA = (BBB, BBB)",
              "BBB = (AAA, ZZZ)",
              "ZZZ = (ZZZ, ZZZ)"]


day8Spec :: Spec
day8Spec = describe "Day 8" $ do
    describe "Part 1" $ do
        it "acceptance" $ 
            part1 givenLines `shouldBe` 6

        it "follows non-repeating steps" $ 
            part1 ["RL",
                   "",
                   "AAA = (BBB, CCC)",
                   "BBB = (DDD, EEE)",
                   "CCC = (ZZZ, GGG)",
                   "DDD = (DDD, DDD)",
                   "EEE = (EEE, EEE)",
                   "GGG = (GGG, GGG)",
                   "ZZZ = (ZZZ, ZZZ)"] `shouldBe` 2

