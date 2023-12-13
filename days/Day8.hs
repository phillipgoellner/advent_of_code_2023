module Day8(part1, part2) where

example = ["LLR",
           "",
           "AAA = (BBB, BBB)",
           "BBB = (AAA, ZZZ)",
           "ZZZ = (ZZZ, ZZZ)"]

part1 = stepsToZZZ
part2 = stepsToZs

type Node = (String, (String, String))

-- Part 1

stepsToZZZ :: [String] -> Int
stepsToZZZ lines = steps (toNodes lines) "AAA" (getInstructions lines) 0 where
    steps _ "ZZZ" _ numberOfSteps = numberOfSteps
    steps nodes location [] numberOfSteps = steps nodes location (getInstructions lines) numberOfSteps
    steps nodes location (instruction : instructions) numberOfSteps = steps nodes (performOneStep instruction location nodes) instructions (numberOfSteps + 1)


getInstructions :: [String] -> String
getInstructions (instr : _) = instr

toNodes :: [String] -> [Node]
toNodes ("" : nodeLines) = map (lineToNode . words) nodeLines where
    lineToNode (location : "=" : left : right : _) = (location, (take 3 (tail left), take 3 right))
toNodes (_ : rest) = toNodes rest


performOneStep :: Char -> String -> [Node] -> String
performOneStep instruction location ((node, next) : nodes)
    | location /= node = performOneStep instruction location nodes
    | otherwise = getSelector instruction next where
        getSelector 'L' = fst
        getSelector 'R' = snd

-- Part 2

stepsToZs :: [String] -> Int
stepsToZs lines = foldl1 lcm (map (steps (toNodes lines) (getInstructions lines) 0) (startNodes lines)) where
    steps nodes [] numberOfSteps location = steps nodes (getInstructions lines) numberOfSteps location
    steps nodes (instruction : instructions) numberOfSteps location
        | isEndNode location = numberOfSteps
        | otherwise = steps nodes instructions (numberOfSteps + 1) (performOneStep instruction location nodes)

startNodes :: [String] -> [[Char]]
startNodes lines = map fst (filter isStartNode (toNodes lines)) where
    isStartNode (_ : _ : 'A' : _, _) = True
    isStartNode _ = False

isEndNode :: String -> Bool
isEndNode (_ : _ : 'Z' : _) = True
isEndNode _ = False
