-- 1.
module Jammin where

-- 8.
import Data.List (sortBy, groupBy,maximumBy)

data Fruit =
    Peach
  | Plum
  | Apple
  | Blackberry
  deriving (Eq, Show, Ord) -- 4.

data JamJars =
  Jam {fruit :: Fruit, jars :: Int}  -- 2.
  deriving (Eq, Show, Ord) -- 4.

-- 3. cardinality is 4 (Fruit) * cardinality of Int

row1 = Jam {fruit = Plum, jars = 4} -- construct using record syntax
row2 = Jam Peach 1
row3 = Jam Plum 4
row4 = Jam Blackberry 8
row5 = Jam Apple 4
row6 = Jam Apple 7
allJam = [row1, row2, row3, row4, row5, row6]

-- 5.
rowJars :: [JamJars] -> [Int]
rowJars = map jars

-- 6.
jarsCount :: [JamJars] -> Int
jarsCount = sum . rowJars

-- 7.
mostRow :: [JamJars] -> JamJars
mostRow = maximumBy (\j1 j2 -> compare (jars j1) (jars j2))

-- 9.
compareKind :: JamJars -> JamJars -> Ordering
compareKind (Jam k _) (Jam k' _) = compare k k'
sortJams :: [JamJars] -> [JamJars]
sortJams = sortBy compareKind

-- 10.
groupJam :: [JamJars] -> [[JamJars]]
groupJam = groupBy (\j1 j2 -> fruit j1 == fruit j2) . sortJams

-- different way
-- import Data.Function (on)
-- groupJam = groupBy (==) on (fruit) . sortJams