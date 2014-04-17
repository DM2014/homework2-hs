{-# LANGUAGE OverloadedStrings #-}
module FPGrowth.AssociationRule where

--import              FPGrowth.Types
--import              FPGrowth.Tree
--import              FPGrowth.Transaction
--import qualified    Data.Set as Set

----import              Data.HashMap.Strict (HashMap)
--import qualified    Data.HashMap.Strict as H

--type Path = (Transaction, Count)


--mine :: Count -> Forest -> [Path]
--mine minsup = mine' minsup (Set.empty, 0)

--mine' :: Count -> Path -> Forest -> [Path]
--mine' minsup suffix forest@(Forest _ punchcard) = mine'' minsup suffix forest (processPunchcard punchcard)
--    where   processPunchcard = map (\(k,v) -> ItemC k v) . H.toList

--buildSubForest :: Count -> Forest -> ItemC -> Forest
--buildSubForest minsup forest item' = toForest minsup $ mineCondForest item' forest

--mine'' :: Count -> Path -> Forest -> [ItemC] -> [Path]
--mine'' _ path forest [] = [path]
--mine'' minsup path@(suffix, count') forest items = concat $ [path] : map (\ (ItemC i c) ->
--        mine' minsup (i `Set.insert` suffix, c) (buildSubForest minsup forest (ItemC i c))
--    ) items

--toForest :: Count -> [(OrderedTransaction, Count)] -> Forest
--toForest minsup = growForest . permuteTransaction minsup . toTransactions
--    where   
--            toTransactions :: [(OrderedTransaction, Count)] -> [Transaction]
--            toTransactions = concat . map toTransaction
--            toTransaction :: (OrderedTransaction, Count) -> [Transaction]
--            toTransaction (xs, n) = take n . repeat . Set.fromList $ map item xs

--mineCondForest :: ItemC -> Forest -> [(OrderedTransaction, Count)]
--mineCondForest x (Forest forest _) = concat $ map (mineCondTree x []) forest

--mineCondTree :: ItemC -> OrderedTransaction -> Tree -> [(OrderedTransaction, Count)]
--mineCondTree _ prefix Leaf = [(prefix, 0)]
--mineCondTree x prefix (Node y n ts) 
--    | x == y = [(prefix, n)]
--    | x <  y = []
--    | x >= y = concat . map (mineCondTree x (y:prefix)) $ ts
--mineCondTree _ _ (Node _ _ _) = []
