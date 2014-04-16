{-# LANGUAGE OverloadedStrings #-}

module FPGrowth.Types where

--import              Control.Monad 
--import              Control.Monad.Trans.Resource
--import              Data.Attoparsec.ByteString.Lazy
--import              Data.Monoid ((<>))
import              Data.ByteString         (ByteString)
import              Data.Set                (Set)
--import              Data.Conduit.Attoparsec
--import              Prelude hiding (product, take)
import              Data.HashMap.Strict (HashMap)

type Item = ByteString
type Transaction = Set Item
type TaggedItem = (Item, Int)
type OrderedTransaction = [Item]
type Punchcard = HashMap Item SupportCount
type SupportCount = Int
--data Tree = Leaf | Node Item Int [Tree]
--type Forest = [Tree]
