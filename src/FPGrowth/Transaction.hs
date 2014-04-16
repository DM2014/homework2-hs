module FPGrowth.Transaction (permuteTransaction) where

import qualified    Data.Set as Set
import qualified    Data.HashMap.Strict as H
import qualified    Data.List as List
import              Data.Ord (Down(..))
import              FPGrowth.Types

buildHeaderTable :: Count -> [Transaction] -> Punchcard
buildHeaderTable minsup = H.filter (>= minsup) . List.foldl' accumulateSet H.empty

-- Accumulates the occurence of items
accumulate :: Item -> Punchcard -> Punchcard
accumulate item = H.insertWith (\_ n -> n + 1) item 1

accumulateSet :: Punchcard -> Transaction -> Punchcard
accumulateSet = Set.foldl' (flip accumulate)

reorderTransaction :: Punchcard -> [Transaction] -> [OrderedTransaction]
reorderTransaction ordered = filter (not . null) . map (sort . reorder)
    where   reorder = Set.foldl' tag []
            tag xs item = case H.lookup item ordered of
                Just x  -> (item, x):xs
                Nothing -> xs
            sort = List.sortBy compareTaggedItem
            untag = map fst

compareTaggedItem :: TaggedItem -> TaggedItem -> Ordering
compareTaggedItem (a, x) (b, y) | x == y    = a `compare` b
                                | otherwise = Down x `compare` Down y

permuteTransaction :: Count -> [Transaction] -> [OrderedTransaction]
permuteTransaction minsup transactions = reorderTransaction (buildHeaderTable minsup transactions) transactions