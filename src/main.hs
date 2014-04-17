{-# LANGUAGE OverloadedStrings #-}

module Main where
import              FPGrowth.Parser
import              FPGrowth.Types
import              FPGrowth.Tree
import              FPGrowth.Mine
import              FPGrowth.Transaction

import qualified    Data.HashMap.Strict  as   H
--import qualified    Data.Set as Set

import              Control.Monad.Trans.Resource
--import qualified    Data.ByteString.Lazy as BL
import              Data.ByteString (ByteString)
import qualified    Data.ByteString.Char8 as B8
--import qualified    Data.ByteString.Lazy.Char8 as BL8
import              Data.Conduit
import qualified    Data.Conduit.Binary as CB
import qualified    Data.Conduit.List as CL

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

    let minsup = 10

    --runResourceT $ CB.sourceFile "data/10" $$ parserConduit =$= toByteString =$ CB.sinkFile "output"
    transactions <- runResourceT $ CB.sourceFile "data/100k" $$ parserConduit =$ CL.consume

    --let a = toForest 3 $ mineCondForest (ItemC "C" 3) forest
    --let Forest f p = toForest 3 $ mineCondForest (ItemC "B" 6) forest
    --let punchcard' = H.toList punchcard

    let forest = growForest . permuteTransaction minsup $ transactions
    --let subForestOfB = toForest 3 $ mineCondForest (ItemC "B" 6) forest
    --let subForestOfD = toForest 3 $ mineCondForest (ItemC "D" 6) forest

    print $ mine minsup forest
    --print $ mine subForestOfB []
    --print $ mine subForestOfD []

    --print $ mine [] punchcard'
    --print $ mine (toForest 3 $ mineCondForest (ItemC "D" 6) forest) ((Set.singleton "D"), 6)
    --print $ mine forest []

    --hor

    where   
            --mine forest@(Forest f punchcard) = do
            --    --print "[]"
            --    --print f
            --    hor
            --    mapM (go forest) punchcardList
            --    where   punchcardList = H.toList punchcard

            --go forest (k, v) = do
            --    --print $ "going through " ++ show (ItemC k v)
            --    let subForest@(Forest subF punchcard) = toForest 3 $ mineCondForest (ItemC k v) forest
            --    --print subF

            --    let punchcardList = H.toList punchcard
            --    case punchcardList of
            --        [] -> do
            --            print $ " " ++ show [ItemC k 999]
            --            return ([] :: [ItemC])
            --        list -> do
            --            poop <- mapM (go subForest) punchcardList
            --            --print "----"
            --            print (map ((:) (ItemC k v)) poop)
            --            --print "^--^"

            --            return $ concat $ (map ((:) (ItemC k v)) poop)
                --        mapM (\x -> do 
                --            path <- go forest x
                --            return $ concat $ map (\p -> (ItemC k v):p) path
                --            ) punchcardList


                --mine subForest
    --BL.writeFile "output" (drawForest result)

hor :: IO ()
hor = print ("==============" :: ByteString)

--b = ItemC "B" 6
--d = ItemC "D" 6
--a = ItemC "A" 5
