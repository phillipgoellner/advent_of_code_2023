touch app/inputDay$1.txt

echo "module Day$1(part1, part2) where
    
example = []

part1 = error \"Part 1 not implemented yet\"
part2 = error \"Part 2 not implemented yet\"
" > days/Day$1.hs

echo "module Test.DoctestDay$1 (day${1}Spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day$1 (part1, part2)

givenLines = []


day4Spec :: Spec
day4Spec = describe \"Day $1\" $ do
    describe \"Part 1\" $ do
        it \"acceptance\" $ 
            part1 givenLines \`shouldBe\` 0
" > test/Test/DoctestDay$1.hs
