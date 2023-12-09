module Main where

import System.IO (openFile, IOMode(ReadMode), hClose, hIsEOF, hGetContents, Handle)

import Day1
import Day2
import Day3
import Day4
import Day5

main :: IO ()
main = do
    print "-----------PART 1-----------"
    printPuzzleAnswer "app/inputDay1.txt"  Day1.part1
    printPuzzleAnswer "app/inputDay1.txt"  Day1.part2
    print "-----------PART 2-----------"
    printPuzzleAnswer "app/inputDay2.txt"  Day2.part1
    printPuzzleAnswer "app/inputDay2.txt"  Day2.part2
    print "-----------PART 3-----------"
    printPuzzleAnswer "app/inputDay3.txt"  Day3.part1
    printPuzzleAnswer "app/inputDay3.txt"  Day3.part2
    print "-----------PART 4-----------"
    printPuzzleAnswer "app/inputDay4.txt"  Day4.part1
    printPuzzleAnswer "app/inputDay4.txt"  Day4.part2
    print "-----------PART 5-----------"
    printPuzzleAnswer "app/inputDay5.txt"  Day5.part1
    printPuzzleAnswer "app/inputDay5.txt"  Day5.part2


printPuzzleAnswer :: String -> ([String] -> Int) -> IO ()
printPuzzleAnswer filePath puzzleFunction = do
    handle <- openFile filePath ReadMode
    contents <- hGetContents handle
    print (puzzleFunction (lines contents))
    hClose handle