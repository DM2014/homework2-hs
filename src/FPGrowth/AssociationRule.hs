{-# LANGUAGE OverloadedStrings #-}
module FPGrowth.AssociationRule where

import              FPGrowth.Types
import qualified    Data.List as List
import              Data.Set hiding (map, filter)

associate :: Int -> FreqSet -> FreqSet -> Rule
associate total (a, i) (b, j) | (a, i) == (b, j) = NoRule
                              | a `isSubsetOf` b = Rule a diff support confidence
                  | otherwise = NoRule
                  where diff = difference b a
                        support = fromIntegral j / fromIntegral total
                        confidence = fromIntegral j / fromIntegral i

genRule :: Int -> Confidence -> [FreqSet] -> [Rule]
genRule total conf set = List.sort . filter validRule $ concat (map (\a -> map (associate total a) set) set)
    where   validRule NoRule = False
            validRule (Rule _ _ _ c) = c >= conf 
