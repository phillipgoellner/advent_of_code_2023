module Day3(part1, part2) where

import Data.Char (isDigit)

part1 = sumOfNumberParts
part2 = sumOfGearRatios

example = [
    "467..114..",
    "...*......",
    "..35..633.",
    "......#...",
    "617*......",
    ".....+.58.",
    "..592.....",
    "......755.",
    "...$.*....",
    ".664.598.."]

type Location = (Int, Int)
type LocatedNumber = (Int, [Location])
type LocatedSymbol = (Char, Location)

-- Part 1

sumOfNumberParts :: [String] -> Int
sumOfNumberParts lines = sum (map fst (filter (nextToOneSymbol (symbolLocations lines)) (numberLocations lines)))


nextToOneSymbol :: [LocatedSymbol] -> LocatedNumber -> Bool
nextToOneSymbol [] (_, locations) = False
nextToOneSymbol ((_, location) : syms) (_, locations)
    | any (areAdjecent location) locations = True
    | otherwise = nextToOneSymbol syms (0, locations)

areAdjecent :: Location -> Location -> Bool
areAdjecent (x, y) (x2, y2)
    | (x - 1) == x2 && (y - 1) == y2  = True
    | (x - 1) == x2 &&       y == y2  = True
    | (x - 1) == x2 && (y + 1) == y2  = True
    |       x == x2 && (y - 1) == y2  = True
    |       x == x2 && (y + 1) == y2  = True
    | (x + 1) == x2 && (y - 1) == y2  = True
    | (x + 1) == x2 &&       y == y2  = True
    | (x + 1) == x2 && (y + 1) == y2  = True
    | otherwise                       = False

numberLocations :: [String] -> [LocatedNumber]
numberLocations = everyLine 0 [] where
    everyLine index numbers [] = numbers
    everyLine index numbers (l : ls) = everyLine (index + 1) (numberLocationsInLine index l ++ numbers) ls


numberLocationsInLine :: Int -> String -> [LocatedNumber]
numberLocationsInLine lineNumber line = eachChar 0 [] ("", []) line where
    eachChar index locatedNumbers ("", []) [] = locatedNumbers
    eachChar index locatedNumbers (numberString, locations) [] = (read (reverse numberString) :: Int, locations) : locatedNumbers
    eachChar index locatedNumbers (numberString, locations) (char : rest)
        | isDigit char = eachChar (index + 1) locatedNumbers (char : numberString, (index, lineNumber) : locations) rest
        | numberString == "" = eachChar (index + 1) locatedNumbers (numberString, locations) rest
        | otherwise = eachChar (index + 1) ((read (reverse numberString) :: Int, locations) : locatedNumbers) ("", []) rest


symbolLocations :: [String] -> [LocatedSymbol]
symbolLocations = everyLine 0 [] where
    everyLine index symbols [] = symbols
    everyLine index symbols (l : ls) = everyLine (index + 1) (symbolLocationsInLine index l ++ symbols) ls


symbolLocationsInLine :: Int -> String -> [LocatedSymbol]
symbolLocationsInLine lineNumber line = eachChar 0 [] (toSymbols line) where
    eachChar index locatedSymbols [] = locatedSymbols
    eachChar index locatedSymbols ((Just sym) : syms) = eachChar (index + 1) ((sym, (index, lineNumber)) : locatedSymbols) syms
    eachChar index locatedSymbols (_ : syms) = eachChar (index + 1) locatedSymbols syms


toSymbols :: String -> [Maybe Char]
toSymbols = map toSymbol where
    toSymbol a
        | isDigit a = Nothing
        | a == '.' = Nothing
        | otherwise = Just a


-- Part 2

sumOfGearRatios :: [String] -> Int
sumOfGearRatios lines = sum (gearRatios lines)

gearRatios :: [String] -> [Int]
gearRatios lines = map (gearRatio (numberLocations lines)) (gearLocations lines) where
        gearRatio numberLocations gear = foldl (*) 1 (map fst (filter (numberNextToSymbol gear) numberLocations))

gearLocations :: [String] -> [LocatedSymbol]
gearLocations lines = filter (isGear (numberLocations lines)) (symbolLocations lines)

isGear :: [LocatedNumber] -> LocatedSymbol -> Bool
isGear numberLocations ('*', location) = length (filter (numberNextToSymbol ('*', location)) (numberLocations)) == 2
isGear _ _ = False

numberNextToSymbol :: LocatedSymbol -> LocatedNumber -> Bool
numberNextToSymbol (_, location) (_, locations) = any (areAdjecent location) locations
