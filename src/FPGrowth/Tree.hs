{-# LANGUAGE OverloadedStrings #-}
module FPGrowth.Tree (growForest, drawTree, drawForest) where

import qualified    Data.List as List

import              FPGrowth.Types

--zs :: [Transaction]
--zs = [   Set.fromList ["f", "a", "c", "d", "g", "i", "m", "p"]
--    ,   Set.fromList ["a", "b", "c", "f", "l", "m", "o"]
--    ,   Set.fromList ["b", "f", "h", "j", "o"]
--    ,   Set.fromList ["b", "c", "k", "s", "p"]
--    ,   Set.fromList ["a", "f", "c", "e", "l", "p", "m", "n"]
--    ]

--inPath :: Item -> Tree -> Bool
--inPath _ Leaf = False
--inPath x (Node (y, _) _ _)
--    | x == y = True
--    | otherwise = False

--inSubtrees :: Item -> [Tree] -> Bool
--inSubtrees x = any (inPath x)

infixl 4 ++>

(++>) :: [Tree] -> OrderedTransaction -> [Tree]
subtrees ++> [] = subtrees
[] ++> x:xs = [Node x 1 ([] ++> xs)]
Leaf:ts ++> x:xs = ts ++> x:xs 
t@(Node y n tss):ts ++> x:xs
    | x == y = Node y (succ n) (tss ++> xs) : ts
    | otherwise =  t : (ts ++> x:xs)

growForest :: (Punchcard, [OrderedTransaction]) -> Forest
growForest (punchcard, transactions) = Forest (List.foldl' (++>) [] transactions) punchcard