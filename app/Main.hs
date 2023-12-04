module Main where

import System.IO (openFile, IOMode(ReadMode), hClose, hIsEOF, hGetContents, Handle)

import Day1

main :: IO ()
main = do
    print "-----------PART 1-----------"
    printPuzzleAnswer "app/inputDay1.txt"  Day1.part1
    printPuzzleAnswer "app/inputDay1.txt"  Day1.part2
    print "-----------PART 2-----------"


printPuzzleAnswer :: String -> ([String] -> Int) -> IO ()
printPuzzleAnswer filePath puzzleFunction = do
    handle <- openFile filePath ReadMode
    contents <- hGetContents handle
    print (puzzleFunction (lines contents))
    hClose handle