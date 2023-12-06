module Day3(part1, part2) where

part1 = sumOfNumberParts
part2 = error "Part 2 not implemented yet"

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

type Location = ([Int], Int)
type LocatedNumber = (Int, Location)

-- Part 1

sumOfNumberParts :: [String] -> Int
sumOfNumberParts lines = 0


