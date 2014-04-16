{-# LANGUAGE OverloadedStrings #-}

module FPGrowth.Types where

import              Data.ByteString         (ByteString)
import qualified    Data.ByteString.Char8 as B8
import qualified    Data.ByteString.Lazy as BL
import qualified    Data.ByteString.Lazy.Char8 as BL8

import              Data.Set                (Set)
import              Data.HashMap.Strict     (HashMap)
import              Data.Ord (Down(..))
import              Data.Monoid ((<>))

type Item = ByteString
type Transaction = Set Item
data ItemC = ItemC 
    { item :: Item
    , count :: Int
    } deriving (Eq)
type OrderedTransaction = [ItemC]
type Punchcard = HashMap Item Count
type Count = Int
data Tree = Leaf | Node ItemC Int [Tree]
data Forest = Forest [Tree] Punchcard

instance Ord ItemC where
    ItemC a x `compare` ItemC b y | x == y    = a `compare` b
                                  | otherwise = Down x `compare` Down y

instance Show ItemC where
    show (ItemC a x) = B8.unpack a ++ " " ++ show x

instance Show Tree where
    show = BL8.unpack . BL.concat . drawTree

instance Show Forest where
    show = BL8.unpack . drawForest

drawTree :: Tree -> [BL.ByteString]
drawTree Leaf = []
drawTree (Node (ItemC x i) n t) = node : subtrees
    where   node = "|" <> BL.fromStrict x <> " " <> BL8.pack (show i) <> "| " <> BL8.pack (show n) <> "\n"
            subtrees = concat $ map (map prepend . drawTree) t
            prepend = (<>) "      "

drawForest :: Forest -> BL.ByteString
drawForest (Forest forest punchcard) = punchcard' <> forest'
    where   punchcard' = BL8.pack (show punchcard ++ "\n")
            forest' = BL.concat $ map (BL.concat . drawTree) forest