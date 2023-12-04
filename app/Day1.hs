module Day1 where

import Data.Char (isDigit)
import Main (printPuzzleAnswer)
import Data.Text (replace, pack, unpack)
    
examplePuzzleLines = ["1abc2",
         "pqr3stu8vwx",
         "a1b2c3d4e5f",
         "treb7uchet"]

part1 = printPuzzleAnswer calibrationValue
part2 = printPuzzleAnswer part2Calbration


calibrationValue :: [String] -> Int
calibrationValue fromLines = sum (map parseOneLine fromLines)

parseOneLine :: String -> Int
parseOneLine line = read [findFirstNumberCharacter line, findLastNumberCharacter line] :: Int

findFirstNumberCharacter :: String -> Char
findFirstNumberCharacter (char : restOfTheWord)
  | isDigit char = char
  | otherwise = findFirstNumberCharacter restOfTheWord

findLastNumberCharacter :: String -> Char
findLastNumberCharacter inWord = reverseSearch inWord '0' where
        reverseSearch (endOfWord : "") lastChar
            | isDigit endOfWord = endOfWord
            | otherwise = lastChar
        reverseSearch (firstChar : restOfTheWord) lastChar
            | isDigit firstChar = reverseSearch restOfTheWord firstChar
            | otherwise = reverseSearch restOfTheWord lastChar


part2Calbration :: [String] -> Int
part2Calbration = calibrationValue . (map replaceNumberWords)


replaceNumberWords :: String -> String
replaceNumberWords inLine = unpack(replaceNumber replacementTexts (pack inLine)) where
        replaceNumber ((old, new) : []) word = replace old new word
        replaceNumber ((old, new) : rest) word = replaceNumber rest (replace old new word)


replacementTexts = map (\ (num, repl) -> (pack num, pack repl) ) [
    ("one", "one1one"),
    ("two", "two2two"),
    ("three", "three3three"),
    ("four", "four4four"),
    ("five", "five5five"),
    ("six", "six6six"),
    ("seven", "seven7seven"),
    ("eight", "eight8eight"),
    ("nine", "nine9nine")]