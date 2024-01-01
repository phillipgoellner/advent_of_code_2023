module Main (main) where

import System.IO (hSetEncoding, stderr, stdout, utf8)
import Test.Hspec (describe, hspec)

import Test.DoctestDay3 (day3Spec)
import Test.DoctestDay4 (day4Spec)
import Test.DoctestDay5 (day5Spec)
import Test.DoctestDay6 (day6Spec)
import Test.DoctestDay7 (day7Spec)
import Test.DoctestDay8 (day8Spec)
import Test.DoctestDay9 (day9Spec)
import Test.DoctestDay10 (day10Spec)
import Test.DoctestDay11 (day11Spec)


main :: IO ()
main = do
    -- fix terminal encoding
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

    hspec $ describe "Tests" $ do
        day3Spec
        day4Spec
        day5Spec
        day6Spec
        day7Spec
        day8Spec
        day9Spec
        day10Spec
        day11Spec
