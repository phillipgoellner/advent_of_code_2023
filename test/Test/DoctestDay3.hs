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
    describe "Part 1" $ do
        it "acceptance" $ 
            part1 givenLines `shouldBe` 4361

        it "zero for no number next to symbol" $
            part1 ["467..114.."] `shouldBe` 0

        it "number if next to symbol in same line" $
            part1 ["617*......"] `shouldBe` 617

        it "number if next to symbol across lines" $
            part1 ["467..114..", "...*......"] `shouldBe` 467

        it "adds up numbers next to symbols" $
            part1 ["...$.*....", ".664.598.."] `shouldBe` (664 + 598)

    describe "Part 2" $ do
        it "acceptance" $ 
            part2 givenLines `shouldBe` 467835

        it "multiplies numbers for gear ratio" $
            part2 ["467..114..", "...*......", "..35..633."] `shouldBe` (467 * 35)
