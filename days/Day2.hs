module Day2(part1, part2) where

import Data.Char (isDigit)
import Data.Text (replace, pack, unpack)

examplePuzzleLines = [
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"]

firstGameConfiguration = ((Red, 12), (Green, 13), (Blue, 14))

type Game = (Int, [Round])
type Round = [(Cube, Int)]
data Cube = Red | Green | Blue deriving (Show, Eq)

type GameConfiguration = ((Cube, Int), (Cube, Int), (Cube, Int))


part1 = possibleGames
part2 = allPowers

-- Part 1 possible games

possibleGames :: [String] -> Int
possibleGames lines = sum (map fst (filter (gameIsPossible firstGameConfiguration) (map toGame lines)))


gameIsPossible :: GameConfiguration -> Game -> Bool
gameIsPossible config (_, rounds) = all (roundIsPossible config) rounds

roundIsPossible :: GameConfiguration -> Round -> Bool
roundIsPossible ((Red, reds), (Green, greens), (Blue, blues)) ((Red, cubes) : rest)
    | reds < cubes = False
    | otherwise = roundIsPossible ((Red, reds), (Green, greens), (Blue, blues)) rest
roundIsPossible ((Red, reds), (Green, greens), (Blue, blues)) ((Green, cubes) : rest)
    | greens < cubes = False
    | otherwise = roundIsPossible ((Red, reds), (Green, greens), (Blue, blues)) rest
roundIsPossible ((Red, reds), (Green, greens), (Blue, blues)) ((Blue, cubes) : rest)
    | blues < cubes = False
    | otherwise = roundIsPossible ((Red, reds), (Green, greens), (Blue, blues)) rest
roundIsPossible _ _ = True

-- Part 2

allPowers :: [String] -> Int
allPowers lines = sum (map ((cubePower . minimumViableCubes) . toGame) lines)

cubePower :: GameConfiguration -> Int
cubePower ((Red, a), (Green, b), (Blue, c)) = a * b * c

minimumViableCubes :: Game -> GameConfiguration
minimumViableCubes (_, rounds) = (maxRed rounds, maxGreen rounds, maxBlue rounds)


maxRed :: [Round] -> (Cube, Int)
maxRed rounds = findMax rounds (Red, 0) where
    findMax [] currentMax = currentMax
    findMax (round : rest) currentMax = findMax rest (findInRound round currentMax) where
        findInRound [] curMax = curMax
        findInRound ((Red, amount) : others) (_, maxRed)
            | amount > maxRed = (Red, amount)
            | otherwise = (Red, maxRed)
        findInRound(_ : other) currentMax = findInRound other currentMax

maxGreen :: [Round] -> (Cube, Int)
maxGreen rounds = findMax rounds (Green, 0) where
    findMax [] currentMax = currentMax
    findMax (round : rest) currentMax = findMax rest (findInRound round currentMax) where
        findInRound [] curMax = curMax
        findInRound ((Green, amount) : others) (_, maxGreen)
            | amount > maxGreen = (Green, amount)
            | otherwise = (Green, maxGreen)
        findInRound(_ : other) currentMax = findInRound other currentMax

maxBlue :: [Round] -> (Cube, Int)
maxBlue rounds = findMax rounds (Blue, 0) where
    findMax [] currentMax = currentMax
    findMax (round : rest) currentMax = findMax rest (findInRound round currentMax) where
        findInRound [] curMax = curMax
        findInRound ((Blue, amount) : others) (_, maxBlue)
            | amount > maxBlue = (Blue, amount)
            | otherwise = (Blue, maxBlue)
        findInRound(_ : other) currentMax = findInRound other currentMax

roundToConfig :: Round -> GameConfiguration
roundToConfig [(Red, r), (Green, g), (Blue, b)] = ((Red, r), (Green, g), (Blue, b))
roundToConfig [(Red, r), (Green, g)]            = ((Red, r), (Green, g), (Blue, 0))
roundToConfig [(Red, r), (Blue, b)]             = ((Red, r), (Green, 0), (Blue, b))
roundToConfig [(Green, g), (Blue, b)]           = ((Red, 0), (Green, g), (Blue, b))
roundToConfig [(Red, r)]                        = ((Red, r), (Green, 0), (Blue, 0))
roundToConfig [(Green, g)]                      = ((Red, 0), (Green, g), (Blue, 0))
roundToConfig [(Blue, b)]                       = ((Red, 0), (Green, 0), (Blue, b))
roundToConfig _                                 = ((Red, 0), (Green, 0), (Blue, 0))

-- Parsing

toGame :: String -> Game
toGame line = (parseGameId line, parseRounds line)

parseGameId :: String -> Int
parseGameId line = read (init (head (tail (words line)))) :: Int


parseRounds :: String -> [Round]
parseRounds line = map parseOneRound (words (removeAllSpaces line))

parseOneRound :: String -> Round
parseOneRound line = parse line "" [] where
    parse "" currentNum cubes = cubes
    parse ('r' : chars) currentNum cubes = parse chars "" ((Red, read (reverse currentNum) :: Int) :cubes)
    parse ('g' : chars) currentNum cubes = parse chars "" ((Green, read (reverse currentNum) :: Int) :cubes)
    parse ('b' : chars) currentNum cubes = parse chars "" ((Blue, read (reverse currentNum) :: Int) :cubes)
    parse (char : chars) currentNum cubes = parse chars (char : currentNum) cubes


removeAllSpaces :: String -> String
removeAllSpaces = removeSemicola . removeGameSpace . removeCommaSpace . removeBlueSpace . removeGreenSpace . removeRedSpace

removeGameSpace :: String -> String
removeGameSpace line = replaceAll line "Game " "Game"

removeSemicola :: String -> String
removeSemicola line = replaceAll line ";" ""

removeCommaSpace :: String -> String
removeCommaSpace line = replaceAll line ", " ""

removeBlueSpace :: String -> String
removeBlueSpace line = replaceAll line " blue" "b"

removeRedSpace :: String -> String
removeRedSpace line = replaceAll line " red" "r"

removeGreenSpace :: String -> String
removeGreenSpace line = replaceAll line " green" "g"

replaceAll :: String -> String -> String -> String
replaceAll word old new = unpack (replace (pack old) (pack new) (pack word))