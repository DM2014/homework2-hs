{-# LANGUAGE OverloadedStrings #-}
module FPGrowth.Tree (growForest, drawTree, drawForest) where

import qualified    Data.List as List
import              Data.Monoid ((<>))
import qualified    Data.ByteString.Lazy as BL
--import qualified    Data.ByteString as B
--import              Data.ByteString (ByteString)
import qualified    Data.ByteString.Lazy.Char8 as BL8

import              FPGrowth.Types

instance Show Tree where
    show = BL8.unpack . BL.concat . drawTree

drawTree :: Tree -> [BL.ByteString]
drawTree Leaf = []
drawTree (Node (ItemC x i) n t) = node : subtrees
    where   node = "|" <> BL.fromStrict x <> " " <> BL8.pack (show i) <> "| " <> BL8.pack (show n) <> "\n"
            subtrees = concat $ map (map prepend . drawTree) t
            prepend = (<>) "      "

drawForest :: Forest -> BL.ByteString
drawForest forest = BL.concat $ map (BL.concat . drawTree) forest
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

growForest :: [OrderedTransaction] -> Forest
growForest = List.foldl' (++>) []