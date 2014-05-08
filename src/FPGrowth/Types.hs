{-# LANGUAGE OverloadedStrings #-}

module FPGrowth.Types where

import              Data.ByteString.Short   (ShortByteString)
import qualified    Data.ByteString.Char8 as B8
import qualified    Data.ByteString.Lazy as BL
import qualified    Data.ByteString.Short as BS
import qualified    Data.ByteString.Lazy.Char8 as BL8

import qualified    Data.List               as List
import qualified    Data.Set                as Set
import              Data.Set                (Set)
import              Data.Hashable           (Hashable, hashWithSalt)
import              Data.HashMap.Strict     (HashMap)
import              Data.Ord (Down(..))
import              Data.Monoid ((<>))

type Item = ShortByteString
type Transaction = Set Item
data ItemC = ItemC 
    { item :: Item
    , count :: Int
    } deriving (Eq)
type OrderedTransaction = [ItemC]
type Punchcard = HashMap Item Count
type Count = Int
type Support = Double
type Confidence = Double
data Tree = Leaf | Node ItemC Int [Tree]
data Forest = Forest [Tree] Punchcard
type FreqSet = (Transaction, Count)
data Rule = Rule Transaction Transaction Support Confidence
          | NoRule
          deriving Eq

instance Hashable ShortByteString where
    hashWithSalt n = hashWithSalt n . BS.fromShort

instance Ord Rule where
    _ `compare` NoRule = GT
    NoRule `compare` _ = GT
    Rule a b s c `compare` Rule a' b' s' c' | s /= s' = Down s `compare` Down s'
                                            | c /= c' = Down c `compare` Down c'
                                            | a /= a' = a `compare` a'
                                            | otherwise = b `compare` b'


instance Show Rule where
    show NoRule = ""
    show (Rule a b s c) = a' ++ "->" ++ show b ++ " (s = " ++ show (s * 100) ++ "%, c = " ++  show (c * 100) ++ "%)"
        where   a' = BL8.unpack $ showTransaction a
                --b' = BL8.unpack $ showTransaction b

showRule :: Rule -> BL.ByteString
showRule NoRule = ""
showRule (Rule a b _ _) = showTransaction a <> "->" <> showTransaction b

showTransaction :: Transaction -> BL.ByteString
showTransaction set | Set.null set      = "∅"
                    | otherwise         = BL.concat . List.intersperse "," . map (BL.fromStrict . BS.fromShort) . Set.toList $ set
                    
instance Ord ItemC where
    ItemC a x `compare` ItemC b y | x == y    = a `compare` b
                                  | otherwise = Down x `compare` Down y

instance Show ItemC where
    show (ItemC a x) = B8.unpack (BS.fromShort a) ++ " " ++ show x

instance Show Tree where
    show = BL8.unpack . BL.concat . drawTree

instance Show Forest where
    show = BL8.unpack . drawForest

drawTree :: Tree -> [BL.ByteString]
drawTree Leaf = []
drawTree (Node (ItemC x i) n t) = node : subtrees
    where   node = "|" <> BL.fromStrict (BS.fromShort x) <> " " <> BL8.pack (show i) <> "| " <> BL8.pack (show n) <> "\n"
            subtrees = concat $ map (map prepend . drawTree) t
            prepend = (<>) "      "

drawForest :: Forest -> BL.ByteString
drawForest (Forest forest punchcard) = punchcard' <> forest'
    where   punchcard' = BL8.pack (show punchcard ++ "\n")
            forest' = BL.concat $ map (BL.concat . drawTree) forest