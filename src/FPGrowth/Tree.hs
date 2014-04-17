{-# LANGUAGE OverloadedStrings #-}
module FPGrowth.Tree (growForest, drawTree, drawForest) where

import qualified    Data.List as List

import              FPGrowth.Types

infixl 4 ++>

(++>) :: [Tree] -> OrderedTransaction -> [Tree]
subtrees            ++> []   = subtrees
[]                  ++> x:xs = [Node x 1 ([] ++> xs)]
Leaf:ts             ++> x:xs = ts ++> x:xs 
t@(Node y n tss):ts ++> x:xs | x == y = Node y (succ n) (tss ++> xs) : ts 
                             | otherwise =  t : (ts ++> x:xs)

growForest :: (Punchcard, [OrderedTransaction]) -> Forest
growForest (punchcard, transactions) = Forest (List.foldl' (++>) [] transactions) punchcard