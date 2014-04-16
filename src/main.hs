{-# LANGUAGE OverloadedStrings #-}

module Main where
import              FPGrowth.Parser
import              FPGrowth.Types
import              FPGrowth.Tree
import              FPGrowth.Mine
import              FPGrowth.Transaction

import              Control.Monad.Trans.Resource
import qualified    Data.ByteString.Lazy as BL
import              Data.ByteString (ByteString)
import qualified    Data.ByteString.Char8 as B8
import qualified    Data.ByteString.Lazy.Char8 as BL8
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
    --runResourceT $ CB.sourceFile "data/10" $$ parserConduit =$= toByteString =$ CB.sinkFile "output"
    transactions <- runResourceT $ CB.sourceFile "data/b" $$ parserConduit =$ CL.consume

    let forest = growForest . permuteTransaction 3 $ transactions
    let a = mineCondForest (ItemC "C" 3) forest
    print (head forest)
    print a
    --BL.writeFile "output" (drawForest result)

--b = ItemC "B" 6
--d = ItemC "D" 6
--a = ItemC "A" 5
