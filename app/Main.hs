module Main where

import System.IO (openFile, IOMode(ReadMode), hClose, hIsEOF, hGetContents, Handle)

main :: IO ()
main = putStrLn "Hello, Haskell!"


printPuzzleAnswer :: ([String] -> Int) -> IO ()
printPuzzleAnswer puzzleFunction = do
    handle <- openFile "inputDay1.txt" ReadMode  
    contents <- hGetContents handle
    print (puzzleFunction (lines contents))
    hClose handle