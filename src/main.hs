module Main where

--import              Data.Set (Set)
--import qualified    Data.Set as Set
--import              Data.HashMap.Strict (HashMap)
--import qualified    Data.HashMap.Strict as H
--import qualified    Data.List as List
--import              Data.Ord (Down(..))
import              Data.ByteString (ByteString)
import qualified    Data.ByteString.Char8 as B8

import              Data.Conduit
import qualified    Data.Conduit.Binary as CB
import              Control.Monad.Trans.Resource

import qualified    Data.Conduit

import              FPGrowth.Parser
import              FPGrowth.Types

toByteString :: Conduit Transaction (ResourceT IO) ByteString
toByteString = do
    a <- await
    case a of
        Just b -> do
            yield $ B8.pack (show b ++ "\n")
            toByteString
        Nothing -> return ()

main :: IO ()
main = do
    runResourceT $ CB.sourceFile "data/10" $$ CB.lines =$= parserConduit =$= toByteString =$ CB.sinkFile "output"

    print "fuck"
--type Item = String
--type Transaction = Set Item
--type TaggedItem = (Item, Int)
--type OrderedTransaction = [Item]
--type Punchcard = HashMap Item Int
--type SupportCount = Int
--data Tree = Leaf | Node Item Int [Tree]
--type Forest = [Tree]

--instance Show Tree where
--    show = concat . drawTree
--        where   drawTree Leaf = []
--                drawTree (Node x n t) = node : subtrees
--                    where   node = "|" ++ x ++ " " ++ show n ++ "\n"
--                            subtrees = concat $ map (map prepend . drawTree) t
--                            prepend = (++) "      "

--zs :: [Transaction]
--zs = [   Set.fromList ["f", "a", "c", "d", "g", "i", "m", "p"]
--    ,   Set.fromList ["a", "b", "c", "f", "l", "m", "o"]
--    ,   Set.fromList ["b", "f", "h", "j", "o"]
--    ,   Set.fromList ["b", "c", "k", "s", "p"]
--    ,   Set.fromList ["a", "f", "c", "e", "l", "p", "m", "n"]
--    ]

--buildHeaderTable :: SupportCount -> [Transaction] -> Punchcard
--buildHeaderTable minsup = H.fromList . List.sortBy compareTaggedItem . filter ((>= minsup) . snd) . H.toList . List.foldl' accumulateSet H.empty

---- Accumulates the occurence of items
--accumulate :: Item -> Punchcard -> Punchcard
--accumulate item = H.insertWith (\_ n -> n + 1) item 1

--accumulateSet :: Punchcard -> Transaction -> Punchcard
--accumulateSet = Set.foldl' (flip accumulate)

--reorderTransaction :: Punchcard -> [Transaction] -> [OrderedTransaction]
--reorderTransaction ordered transactions = map (untag . sort . reorder) transactions
--    where   reorder = Set.foldl' tag []
--            tag xs item = case H.lookup item ordered of
--                Just x  -> (item, x):xs
--                Nothing -> xs
--            sort = List.sortBy compareTaggedItem
--            untag = map fst

--compareTaggedItem :: TaggedItem -> TaggedItem -> Ordering
--compareTaggedItem (a, x) (b, y) | x == y    = a `compare` b
--                                | otherwise = Down x `compare` Down y

--inPath :: Item -> Tree -> Bool
--inPath _ Leaf = False
--inPath x (Node y _ _)
--    | x == y = True
--    | otherwise = False

--inSubtrees :: Item -> [Tree] -> Bool
--inSubtrees x = any (inPath x)

--infixl 4 ++>

--(++>) :: [Tree] -> OrderedTransaction -> [Tree]
--subtrees ++> [] = subtrees
--[] ++> x:xs = [Node x 1 ([] ++> xs)]
--Leaf:ts ++> x:xs = ts ++> x:xs 
--t@(Node y n tss):ts ++> x:xs
--    | x == y = Node y (succ n) (tss ++> xs) : ts
--    | otherwise =  t : (ts ++> x:xs)

--buildForest :: [OrderedTransaction] -> Forest
--buildForest = List.foldl' (++>) []


--zs' :: [OrderedTransaction]
--zs' = [["f","c","a","m","p"],["f","c","a","b","m"],["f","b"],["c","b","p"],["f","c","a","m","p"]]
--zs'' :: Forest
--zs'' = buildForest $ reorderTransaction (buildHeaderTable 3 zs) zs
--zs''' :: Forest
--zs''' = buildForest zs'