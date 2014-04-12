module Main where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Set (Set)
import qualified Data.Set as Set

type ItemSet = Set String
type Support = Double
type Confidence = Double

itemSet :: IntMap ItemSet
itemSet = IntMap.fromList
    [   (1, Set.fromList ["Bread", "Milk"])
    ,   (2, Set.fromList ["Bread", "Diaper", "Beer", "Eggs"])
    ,   (3, Set.fromList ["Milk", "Diaper", "Beer", "Coke"])
    ,   (4, Set.fromList ["Bread", "Milk", "Diaper", "Beer"])
    ,   (5, Set.fromList ["Bread", "Milk", "Diaper", "Coke"])
    ]

a :: ItemSet
a = Set.fromList ["Milk", "Bread"]

b :: ItemSet
b = Set.fromList ["Milk", "Bread", "Diaper"]

supportCount :: IntMap ItemSet -> ItemSet -> Int
supportCount table itemSet = IntMap.foldl count 0 table
    where   count acc set = if itemSet `Set.isSubsetOf` set then succ acc else acc

support :: IntMap ItemSet -> ItemSet -> Support
support table itemSet = count / total
    where   count = fromIntegral (supportCount table itemSet)
            total = fromIntegral (IntMap.size table)

confidence :: IntMap ItemSet -> ItemSet -> ItemSet -> Confidence
confidence table a b = countB / countA
    where   countA = fromIntegral (supportCount table a)
            countB = fromIntegral (supportCount table b)


powerset :: Ord a => Set a -> Set (Set a)
powerset s  | Set.null s = Set.singleton Set.empty
            | otherwise = let p = powerset xs in p `Set.union` Set.map (Set.insert x) p
                where   (x, xs) = Set.deleteFindMin s

frequentItemSet :: IntMap ItemSet -> Int -> Set ItemSet
frequentItemSet table minsup = Set.filter enoughSupport subsets
    where   subsets = IntMap.foldl collectSubsets Set.empty table
            collectSubsets acc set = acc `Set.union` (powerset set)
            enoughSupport set = supportCount table set >= minsup

--apriori :: IntMap ItemSet -> Support -> Confidence -> 
--pruneApriori = Support -> IntMap ItemSet -> IntMap ItemSet
--pruneApriori support set =