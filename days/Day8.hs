module Day8(part1, part2) where

example = ["LLR",
           "",
           "AAA = (BBB, BBB)",
           "BBB = (AAA, ZZZ)",
           "ZZZ = (ZZZ, ZZZ)"]

part1 = stepsToZZZ
part2 = error "Part 2 not implemented yet"

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
