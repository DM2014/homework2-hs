module FPGrowth.Types where

import              Data.ByteString         (ByteString)
import              Data.Set                (Set)
import              Data.HashMap.Strict     (HashMap)

type Item = ByteString
type Transaction = Set Item
type TaggedItem = (Item, Int)
type OrderedTransaction = [TaggedItem]
type Punchcard = HashMap Item Count
type Count = Int
data Tree = Leaf | Node TaggedItem Int [Tree]
type Forest = [Tree]
