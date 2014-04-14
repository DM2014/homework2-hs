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
type TaggedItem = (Item, Int)
type OrderedTransaction = [Item]
type Punchcard = HashMap Item Int
type SupportCount = Int
data Tree = Leaf | Node Item Int [Tree] deriving Show
type Forest = [Tree]

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

compareTaggedItem :: TaggedItem -> TaggedItem -> Ordering
compareTaggedItem (a, x) (b, y) | x == y    = a `compare` b
                                | otherwise = Down x `compare` Down y

a = ["a", "b", "c"]
b = ["a", "b"]
d = ["a", "x", "y"]

--accumForest :: OrderedTransaction -> Forest -> Forest
--accumForest [] forest = forest
--accumForest (x:xs) [] = [Node x 1 [accumTree xs Leaf]]
--accumForest (x:xs) (t@(Node y _ _):ts)
--    | x == y = accumTree (x:xs) t : ts
--    | otherwise = 

--accumTree :: OrderedTransaction -> Tree -> Tree
--accumTree [] tree = tree
--accumTree (x:xs) Leaf = Node x 1 [accumTree xs Leaf]
--accumTree (x:xs) (Node a n subtrees) 
--      | x == a    = Node a (succ n) (split subtrees)
----    | otherwise = undefined

inPath :: Item -> Tree -> Bool
inPath x Leaf = False
inPath x (Node y _ _)
    | x == y = True
    | otherwise = False

inSubtrees :: Item -> [Tree] -> Bool
inSubtrees x = any (inPath x)

addToSubtree :: Item -> [Tree] -> [Tree]
addToSubtree x [] = [Node x 1 []]
addToSubtree x (Leaf:ts) = addToSubtree x ts
addToSubtree x (t@(Node y n tss):ts)
    | x == y = Node y (succ n) tss : ts
    | otherwise =  t : addToSubtree x ts
--split :: OrderedTransaction -> [Tree] -> [Tree]
--split (x:xs) subtrees
--    | 
--    where   elem' x = foldl (\a) False

--go = accumTree b . accumTree a $ Leaf