module Day6(part1, part2) where
    
example = []

part1 = winPermutations
part2 = numberOfWaysToWin . parseTimeAndDistance

winPermutations :: [String] -> Int
winPermutations lines = product (map numberOfWaysToWin (parseTimesAndDistances lines))

-- Part 1

parseTimesAndDistances :: [String] -> [(Int, Int)]
parseTimesAndDistances (times : distances : _) = zip (parseTimes times) (parseDistances distances)

parseTimes :: String -> [Int]
parseTimes (':' : times) = map read (words times)
parseTimes (_ : rest) = parseTimes rest

parseDistances :: String -> [Int]
parseDistances (':' : times) = map read (words times)
parseDistances (_ : rest) = parseDistances rest


numberOfWaysToWin :: (Int, Int) -> Int
numberOfWaysToWin (time, distance) = length (filter (> distance) (calculatePossibleDistances time time [])) where
    calculatePossibleDistances 0 _ possibleDistances = possibleDistances
    calculatePossibleDistances timePressed availableTime possibleDistances = calculatePossibleDistances (timePressed - 1) availableTime (((availableTime - timePressed) * timePressed) : possibleDistances)


-- Part 2


parseTimeAndDistance :: [String] -> (Int, Int)
parseTimeAndDistance (time : distance : _) = (parseTime time, parseDistance distance)

parseTime :: String -> Int
parseTime (':' : times) = read (concat (words times))
parseTime (_ : rest) = parseTime rest

parseDistance :: String -> Int
parseDistance (':' : times) = read (concat (words times))
parseDistance (_ : rest) = parseDistance rest