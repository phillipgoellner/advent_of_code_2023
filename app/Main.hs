module Main where

import System.IO (openFile, IOMode(ReadMode), hClose, hIsEOF, hGetContents, Handle)

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Day10
import Day11


printPuzzleAnswer :: String -> ([String] -> Int) -> IO ()
printPuzzleAnswer filePath puzzleFunction = do
    handle <- openFile filePath ReadMode
    contents <- hGetContents handle
    print (puzzleFunction (lines contents))
    hClose handle


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
    print "-----------PART 6-----------"
    printPuzzleAnswer "app/inputDay6.txt"  Day6.part1
   -- printPuzzleAnswer "app/inputDay6.txt"  Day6.part2
    print "-----------PART 7-----------"
    printPuzzleAnswer "app/inputDay7.txt"  Day7.part1
    printPuzzleAnswer "app/inputDay7.txt"  Day7.part2
    print "-----------PART 8-----------"
    printPuzzleAnswer "app/inputDay8.txt"  Day8.part1
    printPuzzleAnswer "app/inputDay8.txt"  Day8.part2
    print "-----------PART 9-----------"
    printPuzzleAnswer "app/inputDay9.txt"  Day9.part1
    printPuzzleAnswer "app/inputDay9.txt"  Day9.part2
    print "-----------PART 10-----------"
    printPuzzleAnswer "app/inputDay10.txt"  Day10.part1
    printPuzzleAnswer "app/inputDay10.txt"  Day10.part2
    print "-----------PART 11-----------"
    printPuzzleAnswer "app/inputDay11.txt"  Day11.part1
    printPuzzleAnswer "app/inputDay11.txt"  (Day11.part2 1000000)
