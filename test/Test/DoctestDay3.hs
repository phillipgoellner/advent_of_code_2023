module Test.DoctestDay3 (day3Spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day3 (part1, part2)

givenLines = [
    "467..114..",
    "...*......",
    "..35..633.",
    "......#...",
    "617*......",
    ".....+.58.",
    "..592.....",
    "......755.",
    "...$.*....",
    ".664.598.."]


day3Spec :: Spec
day3Spec = describe "Day 3" $ do
    describe "Part 1" $
        it "acceptance" $ 
            part1 givenLines `shouldBe` 4361
