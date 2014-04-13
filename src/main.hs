module Main where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List
import Data.HashMap.Strict (HashMap)
import qualified    Data.HashMap.Strict as H
import Data.Ord (comparing, Down(..))
import Data.Maybe (fromJust)

type Item = String
type Transaction = Set Item
type OrderedTransaction = [Item]
type Punchcard = HashMap Item Int
type SupportCount = Int
data Tree = Node Item Int [Tree] deriving Show

s :: [Transaction]
s = [   Set.fromList ["f", "a", "c", "d", "g", "i", "m", "p"]
    ,   Set.fromList ["a", "b", "c", "f", "l", "m", "o"]
    ,   Set.fromList ["b", "f", "h", "j", "o"]
    ,   Set.fromList ["b", "c", "k", "s", "p"]
    ,   Set.fromList ["a", "f", "c", "e", "l", "p", "m", "n"]
    ]

buildHeaderTable :: SupportCount -> [Transaction] -> Punchcard
buildHeaderTable minsup = H.fromList . List.sortBy compareTaggedItem . filter ((>= minsup) . snd) . H.toList . List.foldl' accumulateSet H.empty

-- Accumulates the occurence of items
accumulate :: Item -> Punchcard -> Punchcard
accumulate item = H.insertWith (\_ n -> n + 1) item 1

accumulateSet :: Punchcard -> Transaction -> Punchcard
accumulateSet = Set.foldl' (flip accumulate)

reorderTransaction :: Punchcard -> [Transaction] -> [OrderedTransaction]
reorderTransaction ordered transactions = map (untag . sort . reorder) transactions
    where   reorder = Set.foldl' tag []
            tag xs item = case H.lookup item ordered of
                Just x  -> (item, x):xs
                Nothing -> xs
            sort = List.sortBy compareTaggedItem

            untag = map fst

compareTaggedItem :: (Item, Int) -> (Item, Int) -> Ordering
compareTaggedItem (a, x) (b, y) | x == y    = a `compare` b
                                | otherwise = Down x `compare` Down y

--accumTree :: Tree -> Transaction -> Tree
--accumTree (Node x i subs)  = 

--s :: Data
--s = IntMap.fromList
--    [   (1, Set.fromList ["Bread", "Milk"])
--    ,   (2, Set.fromList ["Bread", "Diaper", "Beer", "Eggs"])
--    ,   (3, Set.fromList ["Milk", "Diaper", "Beer", "Coke"])
--    ,   (4, Set.fromList ["Bread", "Milk", "Diaper", "Beer"])
--    ,   (5, Set.fromList ["Bread", "Milk", "Diaper", "Coke"])
--    ]

--a :: ItemSet
--a = Set.fromList ["Milk", "Bread"]

--b :: ItemSet
--b = Set.fromList ["Milk", "Bread", "Diaper"]

----supportCount :: Data -> ItemSet -> Int
----supportCount table itemSet = IntMap.foldl count 0 table
----    where   count acc set = if itemSet `Set.isSubsetOf` set then succ acc else acc

----support :: Data -> ItemSet -> Support
----support table itemSet = count / total
----    where   count = fromIntegral (supportCount table itemSet)
----            total = fromIntegral (IntMap.size table)

----confidence :: Data -> ItemSet -> ItemSet -> Confidence
----confidence table a b = countB / countA
----    where   countA = fromIntegral (supportCount table a)
----            countB = fromIntegral (supportCount table b)


----powerset :: Ord a => Set a -> Set (Set a)
----powerset s  | Set.null s = Set.singleton Set.empty
----            | otherwise = let p = powerset xs in p `Set.union` Set.map (Set.insert x) p
----                where   (x, xs) = Set.deleteFindMin s

----frequentItemSet :: Data -> Int -> Set ItemSet
----frequentItemSet table minsup = Set.filter enoughSupport subsets
----    where   subsets = IntMap.foldl collectSubsets Set.empty table
----            collectSubsets acc set = acc `Set.union` (powerset set)
----            enoughSupport set = supportCount table set >= minsup

------apriori :: IntMap ItemSet -> Support -> Confidence -> 
----pruneApriori :: Int -> Map ItemSet Int -> Map ItemSet Int 
----pruneApriori support set = Map.filter (>= support) set

----joinApriori :: Set ItemSet 