module Test.DoctestDay5 (day5Spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day5 (part1, part2, translateOne)

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

    describe "Part 2" $ do
        it "acceptance" $
            part2 givenLines `shouldBe` 46

        it "82 maps to 46" $
            part2 ("seeds: 82 1" : tail givenLines) `shouldBe` 46

        it "huge numbers map through" $
            part2 ["seeds: 432563865 39236501",
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
                   "humidity-to-location map:"] `shouldBe` 432563865

        it "translates range onto itself for empty mappings" $
            translateOne [] (1, 10) `shouldBe` [(1, 10)]

        it "translates fully outside range" $
            translateOne [(64,68,11),(77,45,23)] (75, 2) `shouldBe` [(75, 2)]

        it "translates fully contained range" $
            translateOne [(49, 21, 20)] (50, 15) `shouldBe` [(22, 15)]

        it "translates wrapping range" $
            translateOne [(50, 200, 20)] (40, 80) `shouldBe` [(40, 10), (200, 20), (70, 50)]

        it "translates intersecting range left" $
            translateOne [(50, 200, 20)] (40, 25) `shouldBe` [(200, 15), (40, 10)]

        it "translates intersecting range right" $
            translateOne [(50, 200, 20)] (60, 25) `shouldBe` [(70, 15), (210, 10)]

        it "translates correctly 1" $
            translateOne [(64,68,13),(45,81,19),(77,45,23)] (77,1) `shouldBe` [(45, 1)]

        it "translates correctly 2" $
            translateOne [(64,68,13),(45,81,19),(77,45,23)] (76,1) `shouldBe` [(80, 1)]

        it "translates correctly 3" $
            translateOne [(64,68,13),(45,81,19),(77,45,23)] (76,2) `shouldBe` [(45, 1), (80, 1)]
