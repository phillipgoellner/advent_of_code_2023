module Main (main) where

import System.IO (hSetEncoding, stderr, stdout, utf8)
import Test.Hspec (describe, hspec)

import Test.DoctestDay3 (day3Spec)


main :: IO ()
main = do
    -- fix terminal encoding
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

    hspec $ describe "Tests" $ do
        day3Spec