module Day5(part1, part2, translateOne) where

part1 :: [String] -> Int
part1 lines = minimum (locations lines)
part2 lines = minimum (locationsFromRanges lines)

example = ["seeds: 79 14 55 13",
           "",
           "seed-to-soil map:",
           "50 98 2",
           "52 50 48",
           "",
           "soil-to-fertilizer map:",
           "0 15 37",
           "37 52 2",
           "39 0 15",
           "",
           "fertilizer-to-water map:",
           "49 53 8",
           "0 11 42",
           "42 0 7",
           "57 7 4",
           "",
           "water-to-light map:",
           "88 18 7",
           "18 25 70",
           "",
           "light-to-temperature map:",
           "45 77 23",
           "81 45 19",
           "68 64 13",
           "",
           "temperature-to-humidity map:",
           "0 69 1",
           "1 0 69",
           "",
           "humidity-to-location map:",
           "60 56 37",
           "56 93 4"]

type Mapping = (Int, Int, Int)
type Range = (Int, Int)

-- maps:
-- destination source range-length


locations :: [String] -> [Int]
locations lines = map (sourceToDestination (locationMappings lines) . sourceToDestination (humidityMappings lines) . sourceToDestination (temperatureMappings lines) . sourceToDestination (lightMappings lines) . sourceToDestination (waterMappings lines) . sourceToDestination (fertilizerMappings lines) . sourceToDestination (soilMappings lines)) (seeds lines)


seeds :: [String] -> [Int]
seeds (seedLine : _) = parseSeedLine seedLine where
    parseSeedLine (':' : numbers) = map read (words numbers)
    parseSeedLine (_ : rest) = parseSeedLine rest

performMapping :: String -> [String] -> [Mapping]
performMapping header (start : rest)
    | header /= start = performMapping header rest
    | otherwise = inner rest [] where
        inner ("" : unrelated) mappings = mappings
        inner [] mappings = mappings
        inner (mapping : moreLines) mappings = inner moreLines (rangeLineToMapping mapping : mappings)

soilMappings :: [String] -> [Mapping]
soilMappings = performMapping "seed-to-soil map:"

fertilizerMappings :: [String] -> [Mapping]
fertilizerMappings = performMapping "soil-to-fertilizer map:"

waterMappings :: [String] -> [Mapping]
waterMappings = performMapping "fertilizer-to-water map:"

lightMappings :: [String] -> [Mapping]
lightMappings = performMapping "water-to-light map:"

temperatureMappings :: [String] -> [Mapping]
temperatureMappings = performMapping "light-to-temperature map:"

humidityMappings :: [String] -> [Mapping]
humidityMappings = performMapping "temperature-to-humidity map:"

locationMappings :: [String] -> [Mapping]
locationMappings = performMapping "humidity-to-location map:"


rangeLineToMapping :: String -> Mapping
rangeLineToMapping line = inner (words line) where
    inner (dest : src : length : _) = (read src, read dest, read length)


sourceToDestination :: [Mapping] -> Int -> Int
sourceToDestination [] src = src
sourceToDestination ((s, d, l) : mps) src
    | src - s < 0 = sourceToDestination mps src
    | src - s < l = d + (src - s)
    | otherwise = sourceToDestination mps src


-- Part 2

-- seed 82 -> soil 84 -> fertilizer 84 -> water 84 -> light 77 -> temperature 45 -> humidity 46 -> location 46

locationsFromRanges :: [String] -> [Int]
locationsFromRanges lines = map fst (foldl translate (seedRanges lines) [soilMappings lines, fertilizerMappings lines, waterMappings lines, lightMappings lines, temperatureMappings lines, humidityMappings lines, locationMappings lines])


seedRanges :: [String] -> [Range]
seedRanges (seedLine : _) = parseSeedLine seedLine where
    parseSeedLine (':' : numbers) = processPairs (map read (words numbers)) [] where
        processPairs [] foundSeeds = foundSeeds
        processPairs (left : right : rest) foundSeeds = processPairs rest ((left, right) : foundSeeds)
    parseSeedLine (_ : rest) = parseSeedLine rest


translate :: [Range] -> [Mapping] -> [Range]
translate ranges mappings = concatMap (translateOne mappings) ranges

translateOne :: [Mapping] -> Range -> [Range]
translateOne [] source = [source]
translateOne ((src, dest, length) : mps) (rngStart, rngLength)
    | (rngStart, rngLength) `outsideOf` (src, dest, length) = translateOne mps (rngStart, rngLength)
    | (rngStart, rngLength) `fullyInsideOf` (src, dest, length) = [(rngStart + (dest - src), rngLength)]
    | (rngStart, rngLength) `wrapsAround` (src, dest, length) = translateOne mps (rngStart, src - rngStart) ++ [(dest, length)] ++ translateOne mps (src + length, (rngStart + rngLength) - (src + length))
    | rngStart < src = (dest, rngLength - (src - rngStart)) : translateOne mps (rngStart, src - rngStart)
    | otherwise = translateOne ((src, dest, length) : mps) (src + length, rngLength - ((src + length) - rngStart)) ++ translateOne ((src, dest, length) : mps) (rngStart, (src + length) - rngStart)


outsideOf :: Range -> Mapping -> Bool
outsideOf (rngStart, rngLength) (src, _, length) = (rngStart >= src + length) || (rngStart + rngLength <= src)

fullyInsideOf :: Range -> Mapping -> Bool
fullyInsideOf (rngStart, rngLength) (src, _, length) = (rngStart >= src) && (rngStart + rngLength <= src + length)

wrapsAround :: Range -> Mapping -> Bool
wrapsAround (rngStart, rngLength) (src, _, length) = (rngStart < src) && (rngStart + rngLength > src + length)
