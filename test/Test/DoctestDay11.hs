module Test.DoctestDay11 (day11Spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day11 (part1, part2)

givenLines = ["...#......",
              ".......#..",
              "#.........",
              "..........",
              "......#...",
              ".#........",
              ".........#",
              "..........",
              ".......#..",
              "#...#....."]


day11Spec :: Spec
day11Spec = describe "Day 11" $ do
    describe "Part 1" $ do
        it "acceptance" $ 
            part1 givenLines `shouldBe` 374

        it "manhatten distance between galaxy pair" $ 
            part1 ["#.", ".#"] `shouldBe` 2

        it "total manhatten distance between three galaxies" $ 
            part1 ["#.", "##"] `shouldBe` 4

        it "expand empty line" $ 
            part1 ["#.",
                   "..",
                   "#."] `shouldBe` 3

        it "expand empty column" $ 
            part1 ["#.#",
                   "..."] `shouldBe` 3

    describe "Part 2" $ do
        it "acceptance" $ 
            part2 100 givenLines `shouldBe` 8410

        it "multiplicator distance 10" $ 
            part2 10 givenLines `shouldBe` 1030

        it "multiplicator distance 1" $ 
            part2 2 givenLines `shouldBe` 374

        it "multiplicator distance 1 with two empty lines" $ 
            part2 1 ["#.",
                     "..",
                     "..",
                     "#."] `shouldBe` 3
