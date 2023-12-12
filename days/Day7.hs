module Day7(part1, part2) where

import Data.List (sortBy)
import Data.Function (on)
import Data.Char (digitToInt)
import Test.LeanCheck.Stats (counts)

example = ["32T3K 765",
           "T55J5 684",
           "KK677 28",
           "KTJJT 220",
           "QQQJA 483"]

part1 = determineTotalWinnings
part2 = determineTotalWinningsWithJokers

data HandKind = FiveOfAKind |
                FourOfAKind |
                FullHouse |
                ThreeOfAKind |
                TwoPairs |
                OnePair |
                HighCard deriving (Eq, Show)

-- Part 1

determineTotalWinnings :: [String] -> Int
determineTotalWinnings lines = scoreRankedHands (rankHands (map (toTuples . words) lines)) where
    toTuples (hand : bid : _) = (map cardValueToInt hand, read bid :: Int) where
        cardValueToInt 'A' = 14
        cardValueToInt 'K' = 13
        cardValueToInt 'Q' = 12
        cardValueToInt 'J' = 11
        cardValueToInt 'T' = 10
        cardValueToInt a = digitToInt a

scoreRankedHands :: [([Int], Int)] -> Int
scoreRankedHands hands = calculateScore 0 hands (length hands) where
    calculateScore totalScore [] rank = totalScore
    calculateScore totalScore ((_, bid) : hs) rank = calculateScore (totalScore + rank * bid) hs (rank - 1)


rankHands :: [([Int], Int)] -> [([Int], Int)]
rankHands = sortBy sortHands

sortHands :: ([Int], Int) -> ([Int], Int) -> Ordering
sortHands (hand1, _) (hand2, _)
    | handKind hand1 /= handKind hand2 = compareHandKinds (handKind hand1) (handKind hand2)
    | otherwise = sortByCards hand1 hand2 where
        sortByCards (card1 : cards1) (card2 : cards2)
            | card1 == card2 = sortByCards cards1 cards2
            | otherwise = compare card2 card1

handKind :: [Int] -> HandKind
handKind hand = classify (sortBy (flip compare `on` snd) (counts hand)) where
    classify ((_, 5) : _)           = FiveOfAKind
    classify ((_, 4) : _)           = FourOfAKind
    classify ((_, 3) : (_, 2) : _)  = FullHouse
    classify ((_, 3) : _)           = ThreeOfAKind
    classify ((_, 2) : (_, 2) : _)  = TwoPairs
    classify ((_, 2) : _)           = OnePair
    classify _                      = HighCard

compareHandKinds :: HandKind -> HandKind -> Ordering
compareHandKinds FiveOfAKind FiveOfAKind = EQ
compareHandKinds FiveOfAKind _ = LT
compareHandKinds _ FiveOfAKind = GT
compareHandKinds FourOfAKind FourOfAKind = EQ
compareHandKinds FourOfAKind _ = LT
compareHandKinds _ FourOfAKind = GT
compareHandKinds FullHouse FullHouse = EQ
compareHandKinds FullHouse _ = LT
compareHandKinds _ FullHouse = GT
compareHandKinds ThreeOfAKind ThreeOfAKind = EQ
compareHandKinds ThreeOfAKind _ = LT
compareHandKinds _ ThreeOfAKind = GT
compareHandKinds TwoPairs TwoPairs = EQ
compareHandKinds TwoPairs _ = LT
compareHandKinds _ TwoPairs = GT
compareHandKinds OnePair OnePair = EQ
compareHandKinds OnePair _ = LT
compareHandKinds _ OnePair = GT
compareHandKinds _ _ = EQ


-- Part 2

determineTotalWinningsWithJokers :: [String] -> Int
determineTotalWinningsWithJokers lines = scoreRankedHands (rankHandsWithJokers (map (toTuples . words) lines)) where
    toTuples (hand : bid : _) = (map cardValueToInt hand, read bid :: Int) where
        cardValueToInt 'A' = 14
        cardValueToInt 'K' = 13
        cardValueToInt 'Q' = 12
        cardValueToInt 'J' = 1
        cardValueToInt 'T' = 10
        cardValueToInt a = digitToInt a

rankHandsWithJokers :: [([Int], Int)] -> [([Int], Int)]
rankHandsWithJokers = sortBy sortHandsWithJokers

sortHandsWithJokers :: ([Int], Int) -> ([Int], Int) -> Ordering
sortHandsWithJokers (hand1, _) (hand2, _)
    | handKindWithJokers hand1 /= handKindWithJokers hand2 = compareHandKinds (handKindWithJokers hand1) (handKindWithJokers hand2)
    | otherwise = sortByCards hand1 hand2 where
        sortByCards (card1 : cards1) (card2 : cards2)
            | card1 == card2 = sortByCards cards1 cards2
            | otherwise = compare card2 card1

handKindWithJokers :: [Int] -> HandKind
handKindWithJokers hand = classify (sortBy (flip compare `on` snd) (counts hand)) where
    classify ((_, 5) : _)                                       = FiveOfAKind
    classify ((1, 4) : (_, 1) : _)                              = FiveOfAKind
    classify ((_, 4) : (1, 1) : _)                              = FiveOfAKind
    classify ((1, 3) : (_, 2) : _)                              = FiveOfAKind
    classify ((_, 3) : (1, 2) : _)                              = FiveOfAKind
    classify ((_, 4) : _)                                       = FourOfAKind
    classify ((1, 3) : (_, 1) : _)                              = FourOfAKind
    classify ((_, 3) : (1, 1): _)                               = FourOfAKind
    classify ((_, 3) : (_, 1) : (1, 1) : _)                     = FourOfAKind
    classify ((1, 2) : (_, 2) : _)                              = FourOfAKind
    classify ((_, 2) : (1, 2) : _)                              = FourOfAKind
    classify ((_, 3) : (_, 2) : _)                              = FullHouse
    classify ((_, 2) : (_, 2) : (1, 1) : _)                     = FullHouse
    classify ((_, 3) : _)                                       = ThreeOfAKind
    classify ((1, 2) : _)                                       = ThreeOfAKind
    classify ((_, 2) : (1, 1) : (_, 1) : (_, 1) : _)            = ThreeOfAKind
    classify ((_, 2) : (_, 1) : (1, 1) : (_, 1) : _)            = ThreeOfAKind
    classify ((_, 2) : (_, 1) : (_, 1) : (1, 1) : _)            = ThreeOfAKind
    classify ((_, 2) : (_, 2) : _)                              = TwoPairs
    classify ((_, 2) : _)                                       = OnePair
    classify ((1, 1) : (_, 1) : (_, 1) : (_, 1) : (_, 1) : _)   = OnePair
    classify ((_, 1) : (1, 1) : (_, 1) : (_, 1) : (_, 1) : _)   = OnePair
    classify ((_, 1) : (_, 1) : (1, 1) : (_, 1) : (_, 1) : _)   = OnePair
    classify ((_, 1) : (_, 1) : (_, 1) : (1, 1) : (_, 1) : _)   = OnePair
    classify ((_, 1) : (_, 1) : (_, 1) : (_, 1) : (1, 1) : _)   = OnePair
    classify _                                                  = HighCard
