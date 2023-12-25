module Day10(part1, part2) where

example = ["7-F7-",
           ".FJ|7",
           "SJLL7",
           "|F--J",
           "LJ.LJ"]

part1 = mostStepsFromStart
part2 = enclosedSpaces

data Node = Node { position :: Position, nodeType :: NodeType } deriving (Eq, Show)
data NodeType = Start | V | H | L | J | F | Seven | None deriving (Eq, Show)
type Position = (Int, Int)


-- Part 1

mostStepsFromStart :: [String] -> Int
mostStepsFromStart lines = round (fromIntegral (stepsToStart lines (findStartingNode lines)) / 2)

stepsToStart :: [String] -> Node -> Int
stepsToStart lines (Node nPos _) = inline lines (nodeFrom lines (head (findStartNeighbours lines nPos))) (Node nPos Start) 0 where
    inline _ (Node _ Start) _ steps = steps
    inline lines currentNode prevNode steps = inline lines (selectNextNode (map (nodeFrom lines) (neighbouringPositions currentNode)) currentNode prevNode) currentNode (steps+1)

selectNextNode :: [Node] -> Node -> Node -> Node
selectNextNode selection currentNode prevNode = inner (filter (/= prevNode) (filter (/= currentNode) selection)) where
    inner filteredNodes
        | length filteredNodes == 1 = head filteredNodes
        | otherwise = selectNextNode (filter (\ (Node _ ntype) -> ntype /= Start) filteredNodes) currentNode prevNode

findStartingNode :: [String] -> Node
findStartingNode lines = Node (findStartingPosition lines) Start

findStartingPosition :: [String] -> Position
findStartingPosition (firstLine : rest) = lineWise 0 0 rest firstLine where
    lineWise lineNumber charNumber _ ('S' : _) = (charNumber, lineNumber)
    lineWise lineNumber charNumber (nextLine : moreLines) [] = lineWise (lineNumber + 1) 0 moreLines nextLine
    lineWise lineNumber charNumber nextLines (currentChar : moreChars) = lineWise lineNumber (charNumber + 1) nextLines moreChars

findStartNeighbours :: [String] -> Position -> [Position]
findStartNeighbours _ (0, 0) = [(0, 1), (1, 0)]
findStartNeighbours lines (0, y) = filter (areConnected lines (0, y)) [(0, y - 1), (0, y + 1), (1, y)]
findStartNeighbours lines (x, 0) = filter (areConnected lines (x, 0)) [(x, 1), (x - 1, 0), (x + 1, 0)]
findStartNeighbours lines (x, y) = filter (areConnected lines (x, y)) [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]

areConnected :: [String] -> Position -> Position -> Bool
areConnected lines (x, y) = inline (lines !! y !! x) where
    inline 'S' (x', y') = areConnected lines (x', y') (x, y)
    inline '|' (x', y') = (x, y - 1) == (x', y') || (x, y + 1) == (x', y')
    inline '-' (x', y') = (x - 1, y) == (x', y') || (x + 1, y) == (x', y')
    inline '7' (x', y') = (x, y - 1) == (x', y') || (x - 1, y) == (x', y')
    inline 'F' (x', y') = (x, y - 1) == (x', y') || (x + 1, y) == (x', y')
    inline 'L' (x', y') = (x, y + 1) == (x', y') || (x + 1, y) == (x', y')
    inline 'J' (x', y') = (x, y + 1) == (x', y') || (x - 1, y) == (x', y')
    inline _ _ = False

nodeFrom :: [String] -> Position -> Node
nodeFrom lines (x, y) = Node (x, y) (determineType (lines !! y !! x)) where
    determineType 'S' = Start
    determineType '-' = H
    determineType '|' = V
    determineType 'L' = L
    determineType 'J' = J
    determineType 'F' = F
    determineType '7' = Seven
    determineType _ = None

neighbouringPositions :: Node -> [Position]
neighbouringPositions (Node (x, y) H) = [(x - 1, y), (x + 1, y)]
neighbouringPositions (Node (x, y) V) = [(x, y - 1), (x, y + 1)]
neighbouringPositions (Node (x, y) L) = [(x, y - 1), (x + 1, y)]
neighbouringPositions (Node (x, y) J) = [(x, y - 1), (x - 1, y)]
neighbouringPositions (Node (x, y) F) = [(x, y + 1), (x + 1, y)]
neighbouringPositions (Node (x, y) Seven) = [(x, y + 1), (x - 1, y)]
neighbouringPositions _ = []


-- Part 2

enclosedSpaces :: [String] -> Int
enclosedSpaces _ = 0
