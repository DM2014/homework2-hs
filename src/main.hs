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
import              System.Environment

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

    args <- getArgs

    if length args == 3 then do
        let sup = read $ args !! 0
        let conf = read $ args !! 1
        let k = read $ args !! 2
        go sup conf k
    else do
        putStrLn "\nusage:"
        putStrLn "  ./dist/build/homework2-hs/homework2-hs <support : Double> <confidence : Double> <k : Int>"
        putStrLn "e.g."
        putStrLn "  cat <dataset> | ./dist/build/homework2-hs/homework2-hs 0.01 0.5 10 +RTS -K1000m -H500m -RTS\n"

go :: Double -> Double -> Int -> IO () 
go sup conf k = do
    --runResourceT $ CB.sourceFile "data/10" $$ parserConduit =$= toByteString =$ CB.sinkFile "output"
    transactions <- runResourceT $ CB.sourceFile "data/1m" $$ parserConduit =$ CL.consume

    let minsup = ceiling (fromIntegral (length transactions) * sup)

    let forest = growForest . permuteTransaction minsup $ transactions

    print $ length $ mine minsup forest
