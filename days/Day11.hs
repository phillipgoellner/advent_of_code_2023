module Day11(part1, part2) where

import Data.List

example  = ["...#......",
            ".......#..",
            "#.........",
            "..........",
            "......#...",
            ".#........",
            ".........#",
            "..........",
            ".......#..",
            "#...#....."]

part1 = totalDistance . toGalaxies . expandEmptyLines . expandEmptyColumns
part2 multiplicator lines = (totalDistance . adjustGalaxyCoordinates multiplicator (emptyLineIndices lines) (emptyColumnIndices lines) . toGalaxies) lines


type Galaxy = (Int, Int)

-- Part 1

toGalaxies :: [String] -> [Galaxy]
toGalaxies = lineWise [] 0 where
    lineWise galaxies _ [] = galaxies
    lineWise galaxies y (currentLine : moreLines) = withinLine [] 0 currentLine ++ lineWise galaxies (y + 1) moreLines where
        withinLine gals x [] = gals
        withinLine gals x ('#' : rest) = withinLine ((x, y) : gals) (x + 1) rest
        withinLine gals x (_ : rest) = withinLine gals (x + 1) rest

totalDistance :: [Galaxy] -> Int
totalDistance = sum . map manhattenDistance . galaxyPairs

galaxyPairs :: [Galaxy] -> [(Galaxy, Galaxy)]
galaxyPairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

manhattenDistance :: (Galaxy, Galaxy) -> Int
manhattenDistance ((x, y), (x', y')) = abs (x - x') + abs (y - y')

expandEmptyLines :: [String] -> [String]
expandEmptyLines [] = []
expandEmptyLines (line : lines)
    | lineContainsGalaxies line = line : expandEmptyLines lines
    | otherwise = line : line : expandEmptyLines lines

lineContainsGalaxies :: String -> Bool
lineContainsGalaxies [] = False
lineContainsGalaxies ('#' : _) = True
lineContainsGalaxies (_ : restOfLine) = lineContainsGalaxies restOfLine

expandEmptyColumns :: [String] -> [String]
expandEmptyColumns = withIndex 0 where
    withIndex index lines
        | length (head lines) <= index        = lines
        | columnContainsGalaxies lines index  = withIndex (index + 1) lines
        | otherwise                           = withIndex (index + 2) (map (spliceSpaceInAt (index + 1)) lines)

spliceSpaceInAt :: Int -> String -> String
spliceSpaceInAt index line = take index line ++ ['.'] ++ drop index line

columnContainsGalaxies :: [String] -> Int -> Bool
columnContainsGalaxies lines index
    | length (head lines) <= index  = False
    | otherwise                     = '#' `elem` map (!! index) lines


-- Part 2

emptyLineIndices :: [String] -> [Int]
emptyLineIndices = withIndex 0 [] where
    withIndex index emptyLines [] = emptyLines
    withIndex index emptyLines (l : ls)
        | lineContainsGalaxies l = withIndex (index + 1) emptyLines ls
        | otherwise = withIndex (index + 1) (index : emptyLines) ls

emptyColumnIndices :: [String] -> [Int]
emptyColumnIndices = withIndex 0 [] where
    withIndex index emptyColumns lines
        | length (head lines) <= index = emptyColumns
        | columnContainsGalaxies lines index = withIndex (index + 1) emptyColumns lines
        | otherwise = withIndex (index + 1) (index : emptyColumns) lines

adjustGalaxyCoordinates :: Int -> [Int] -> [Int] -> [Galaxy] -> [Galaxy]
adjustGalaxyCoordinates additionalDistance [] [] galaxies                       = galaxies
adjustGalaxyCoordinates additionalDistance emptyLines emptyColumns galaxies     = adjustY additionalDistance emptyLines (adjustX additionalDistance emptyColumns galaxies)

adjustX :: Int -> [Int] -> [Galaxy] -> [Galaxy]
adjustX _ [] galaxies = galaxies
adjustX additionalDistance (x : xs) galaxies = adjustX additionalDistance xs (map incDist galaxies) where
    incDist (x', y')
        | x < x' = (x' + additionalDistance - 1, y')
        | otherwise = (x', y')

-- [2,1]

--   #.
--   ..
--   ..
--   #.


--   #.
--   ..
--   ..
--   ..
--   ..
--   #.

adjustY :: Int -> [Int] -> [Galaxy] -> [Galaxy]
adjustY _ [] galaxies = galaxies
adjustY additionalDistance (y : ys) galaxies = adjustX additionalDistance ys (map incDist galaxies) where
    incDist (x', y')
        | y < y' = (x', y' + additionalDistance - 1)
        | otherwise = (x', y')