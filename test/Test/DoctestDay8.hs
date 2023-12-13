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

    describe "Part 2" $ do
        it "acceptance" $ 
            part2 ["LR",
                   "",
                   "11A = (11B, XXX)",
                   "11B = (XXX, 11Z)",
                   "11Z = (11B, XXX)",
                   "22A = (22B, XXX)",
                   "22B = (22C, 22C)",
                   "22C = (22Z, 22Z)",
                   "22Z = (22B, 22B)",
                   "XXX = (XXX, XXX)"] `shouldBe` 6


