module Test.DoctestDay5 (day5Spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day5 (part1, part2)

givenLines = ["seeds: 79 14 55 13",
              "",
              "seed-to-soil map:",
              "50 98 2",
              "52 50 48",
              "",
              "soil-to-fertilizer map:",
              "0 15 37",
              "37 52 2",
              "39 0 15",
              "",
              "fertilizer-to-water map:",
              "49 53 8",
              "0 11 42",
              "42 0 7",
              "57 7 4",
              "",
              "water-to-light map:",
              "88 18 7",
              "18 25 70",
              "",
              "light-to-temperature map:",
              "45 77 23",
              "81 45 19",
              "68 64 13",
              "",
              "temperature-to-humidity map:",
              "0 69 1",
              "1 0 69",
              "",
              "humidity-to-location map:",
              "60 56 37",
              "56 93 4"]


-- maps:
-- destination source range-length

day5Spec :: Spec
day5Spec = describe "Day 5" $ do
    describe "Part 1" $ do
        it "acceptance" $
            part1 givenLines `shouldBe` 35

        it "higher number maps through" $
            part1 ["seeds: 432563865",
                   "seed-to-soil map:",
                   "379216444 692683038 140400417",
                   "",
                   "soil-to-fertilizer map:",
                   "",
                   "fertilizer-to-water map:",
                   "",
                   "water-to-light map:",
                   "",
                   "light-to-temperature map:",
                   "",
                   "temperature-to-humidity map:",
                   "",
                   "humidity-to-location map:"] `shouldBe` 432563865

        it "one seed maps correctly" $
            part1 ("seeds: 79" : tail givenLines) `shouldBe` 82

        it "seed with no mappings maps through" $
            part1 ["seeds: 79",
              "",
              "seed-to-soil map:",
              "",
              "soil-to-fertilizer map:",
              "",
              "fertilizer-to-water map:",
              "",
              "water-to-light map:",
              "",
              "light-to-temperature map:",
              "",
              "temperature-to-humidity map:",
              "",
              "humidity-to-location map:"] `shouldBe` 79
