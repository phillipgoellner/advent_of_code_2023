module Test.DoctestDay10 (day10Spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day10 (part1, part2)

givenLines = ["7-F7-",
              ".FJ|7",
              "SJLL7",
              "|F--J",
              "LJ.LJ"]


day10Spec :: Spec
day10Spec = describe "Day 10" $ do
    describe "Part 1" $ do
        it "acceptance" $ 
            part1 givenLines `shouldBe` 8

        it "parses simple loop without secondary loops" $
            part1 [".....",
                   ".S-7.",
                   ".|.|.",
                   ".L-J.",
                   "....."] `shouldBe` 4

