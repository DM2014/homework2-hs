module FPGrowth.Types where

import              Data.ByteString         (ByteString)
import              Data.Set                (Set)
import              Data.HashMap.Strict     (HashMap)
import              Data.Ord (Down(..))

type Item = ByteString
type Transaction = Set Item
data ItemC = ItemC 
    { item :: Item
    , count :: Int
    } deriving (Show, Eq)
type OrderedTransaction = [ItemC]
type Punchcard = HashMap Item Count
type Count = Int
data Tree = Leaf | Node ItemC Int [Tree]
type Forest = [Tree]

instance Ord ItemC where
    ItemC a x `compare` ItemC b y | x == y    = a `compare` b
                                  | otherwise = Down x `compare` Down y
