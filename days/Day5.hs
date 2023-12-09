module Day5(part1, part2) where

part1 :: [String] -> Int
part1 lines = minimum (locations lines)
part2 = error "Part 2 not implement yet"

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
