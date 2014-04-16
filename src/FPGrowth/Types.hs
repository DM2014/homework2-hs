module FPGrowth.Types where

import              Data.ByteString         (ByteString)
import qualified    Data.ByteString.Char8 as B8
import              Data.Set                (Set)
import              Data.HashMap.Strict     (HashMap)
import              Data.Ord (Down(..))

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
type Forest = [Tree]

instance Ord ItemC where
    ItemC a x `compare` ItemC b y | x == y    = a `compare` b
                                  | otherwise = Down x `compare` Down y

instance Show ItemC where
    show (ItemC a x) = B8.unpack a ++ " " ++ show x