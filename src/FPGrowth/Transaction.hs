module FPGrowth.Transaction (permuteTransaction) where

import qualified    Data.Set as Set
import qualified    Data.HashMap.Strict as H
import qualified    Data.List as List
import              FPGrowth.Types

buildHeaderTable :: Count -> [Transaction] -> Punchcard
buildHeaderTable minsup = H.filter (>= minsup) . List.foldl' accumulateSet H.empty

-- Accumulates the occurence of items
accumulate :: Item -> Punchcard -> Punchcard
accumulate a = H.insertWith (\_ n -> n + 1) a 1

accumulateSet :: Punchcard -> Transaction -> Punchcard
accumulateSet = Set.foldl' (flip accumulate)

reorderTransaction :: Punchcard -> [Transaction] -> [OrderedTransaction]
reorderTransaction punchcard = filter (not . null) . map (sort . reorder)
    where   reorder = Set.foldl' tag []
            tag xs a = case H.lookup a punchcard of
                Just x  -> (ItemC a x):xs
                Nothing -> xs
            sort = List.sort

permuteTransaction :: Count -> [Transaction] -> (Punchcard, [OrderedTransaction])
permuteTransaction minsup transactions = (punchcard, reorderTransaction punchcard transactions)
    where   punchcard = buildHeaderTable minsup transactions