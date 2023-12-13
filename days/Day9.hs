module Day9(part1, part2) where

example = ["0 3 6 9 12 15",
           "1 3 6 10 15 21",
           "10 13 16 21 30 45"]

part1 = sum . map extrapolateNextValue . toSequences
part2 = sum . map extrapolateFirstValue . toSequences

type Sequence = [Int]

-- Part 1

toSequences :: [String] -> [Sequence]
toSequences = map (map read . words)

extrapolateNextValue :: Sequence -> Int
extrapolateNextValue sequence
    | null sequence       = 0
    | all (== 0) sequence = 0
    | otherwise           = last sequence + extrapolateNextValue (derivative sequence) where
        derivative f = zipWith (-) (tail f) (init f)

-- f: 0 3 6 9 12 15
--      0 3 6 9 12 15

-- Part 2

extrapolateFirstValue :: Sequence -> Int
extrapolateFirstValue sequence
    | null sequence       = 0
    | all (== 0) sequence = 0
    | otherwise           = head sequence - extrapolateFirstValue (derivative sequence) where
        derivative f = zipWith (-) (tail f) (init f)
