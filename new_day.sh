previousDay=$((${1}-1))

# amend app/Main.hs
sed -i '' "s/.*import Day$previousDay[^\n]*/&\nimport Day$1/" app/Main.hs
sed -i '' "s#Day$previousDay.part2#&\n    print \"-----------PART ${1}-----------\"\n    printPuzzleAnswer \"app/inputDay${1}.txt\"  Day${1}.part1\n    printPuzzleAnswer \"app/inputDay${1}.txt\"  Day${1}.part2#" app/Main.hs

# amend test/Spec.hs
sed -i '' "s/(day${previousDay}Spec)/&\nimport Test.DoctestDay$1 (day${1}Spec)/" test/Spec.hs
sed -i '' "s/        day${previousDay}Spec/&\n        day${1}Spec/" test/Spec.hs

# amend cabal file
sed -i '' "s/, Day$previousDay/&\n                    , Day$1/" advent-of-code2023.cabal
sed -i '' "s/Test.DoctestDay$previousDay/&\n                    , Test.DoctestDay$1/" advent-of-code2023.cabal

#create input file
touch app/inputDay$1.txt

# create source file
echo "module Day$1(part1, part2) where
    
example = []

part1 = error \"Part 1 not implemented yet\"
part2 = error \"Part 2 not implemented yet\"
" > days/Day$1.hs

# create DocTest file
echo "module Test.DoctestDay$1 (day${1}Spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day$1 (part1, part2)

givenLines = []


day${1}Spec :: Spec
day${1}Spec = describe \"Day $1\" $ do
    describe \"Part 1\" $ do
        it \"acceptance\" $ 
            part1 givenLines \`shouldBe\` 0
" > test/Test/DoctestDay$1.hs
