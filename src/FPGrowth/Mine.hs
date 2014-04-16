{-# LANGUAGE OverloadedStrings #-}
module FPGrowth.Mine where

import              FPGrowth.Types
import              FPGrowth.Tree
import              FPGrowth.Transaction
import qualified    Data.Set as Set

import              Data.HashMap.Strict (HashMap)
--import qualified    Data.HashMap.Strict as H


toForest :: Count -> [(OrderedTransaction, Count)] -> Forest
toForest minsup = growForest . permuteTransaction minsup . toTransactions
    where   
            toTransactions :: [(OrderedTransaction, Count)] -> [Transaction]
            toTransactions = concat . map toTransaction
            toTransaction :: (OrderedTransaction, Count) -> [Transaction]
            toTransaction (xs, n) = take n . repeat . Set.fromList $ map item xs

mineCondForest :: ItemC -> Forest -> [(OrderedTransaction, Count)]
mineCondForest x = concat . map (mineCondTree x [])

mineCondTree :: ItemC -> OrderedTransaction -> Tree -> [(OrderedTransaction, Count)]
mineCondTree _ [] Leaf = []
mineCondTree _ prefix Leaf = [(prefix, 0)]
mineCondTree x prefix (Node y n ts) 
    | x == y = [(prefix, n)]
    | x <  y = []
    | x >= y = concat . map (mineCondTree x (y:prefix)) $ ts
----import              Data.Set (Set)
--import qualified    Data.Set as Set
--import qualified    Data.List as List
--import              Data.Ord (Down(..))
--import              Data.Monoid ((<>))
--import qualified    Data.ByteString.Lazy as BL
--import qualified    Data.ByteString as B
--import              Data.ByteString (ByteString)
--import qualified    Data.ByteString.Lazy.Char8 as BL8


--instance Show Tree where
--    show = BL8.unpack . BL.concat . drawTree

--drawTree :: Tree -> [BL.ByteString]
--drawTree Leaf = []
--drawTree (Node x n t) = node : subtrees
--    where   node = "|" <> BL.fromStrict x <> " " <> BL8.pack (show n) <> "\n"
--            subtrees = concat $ map (map prepend . drawTree) t
--            prepend = (<>) "      "

--drawForest :: Forest -> BL.ByteString
--drawForest forest = BL.concat $ map (BL.concat . drawTree) forest
----zs :: [Transaction]
----zs = [   Set.fromList ["f", "a", "c", "d", "g", "i", "m", "p"]
----    ,   Set.fromList ["a", "b", "c", "f", "l", "m", "o"]
----    ,   Set.fromList ["b", "f", "h", "j", "o"]
----    ,   Set.fromList ["b", "c", "k", "s", "p"]
----    ,   Set.fromList ["a", "f", "c", "e", "l", "p", "m", "n"]
----    ]

--inPath :: Item -> Tree -> Bool
--inPath _ Leaf = False
--inPath x (Node y _ _)
--    | x == y = True
--    | otherwise = False

--inSubtrees :: Item -> [Tree] -> Bool
--inSubtrees x = any (inPath x)

--infixl 4 ++>

--(++>) :: [Tree] -> OrderedTransaction -> [Tree]
--subtrees ++> [] = subtrees
--[] ++> x:xs = [Node x 1 ([] ++> xs)]
--Leaf:ts ++> x:xs = ts ++> x:xs 
--t@(Node y n tss):ts ++> x:xs
--    | x == y = Node y (succ n) (tss ++> xs) : ts
--    | otherwise =  t : (ts ++> x:xs)

--growForest :: [OrderedTransaction] -> Forest
--growForest = List.foldl' (++>) []