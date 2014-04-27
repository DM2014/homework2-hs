{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Parser (processRawData, processRawData2Transaction, processTransaction) where

import              Control.Monad (replicateM_, void)
import              Control.Monad.Trans.Resource
import              Data.Attoparsec.ByteString
import              Data.ByteString (ByteString)
import              Data.Conduit
import              Data.Conduit.Attoparsec
import qualified    Data.Conduit.Binary as CB
import              Data.Monoid ((<>))
import              Prelude hiding (take)
import              System.IO (stdin, stdout)


import qualified    Data.HashMap.Strict as H
import              Data.Set (Set)
import qualified    Data.Set as Set
import qualified    Data.ByteString as B
import qualified    Data.ByteString.Char8 as B8

data Product = Product !UserID !ProductID deriving (Show)
type ProductID = ByteString
type UserID = ByteString
type Table a = H.HashMap UserID a
type ItemSet = Set ProductID

processRawData :: ResourceT IO ()
processRawData = CB.sourceHandle stdin $$ parserConduit parseSection =$= filterUnknown =$= accumulateProduct H.empty =$= toByteString =$ CB.sinkHandle stdout

processRawData2Transaction :: ResourceT IO ()
processRawData2Transaction = CB.sourceHandle stdin $$ parserConduit parseSection =$= filterUnknown =$= awaitForever (yield . printProduct) =$ CB.sinkHandle stdout
    where   printProduct (Product u p) = u <> " " <> p <> "\n"

processTransaction :: ResourceT IO ()
processTransaction = CB.sourceHandle stdin $$ parserConduit parseTransaction =$= accumulateProduct H.empty =$= toByteString =$ CB.sinkHandle stdout


toByteString :: Conduit (Table ItemSet) (ResourceT IO) ByteString
toByteString = do
    t <- await
    case t of
        --Just table -> yield . B8.pack . show . H.size $ table
        Just table -> mapM_ (yield . toLine) (H.toList table)
        Nothing -> return ()
    where   toLine (_, v) = Set.foldl' (\a b -> a <> " " <> b) B.empty v <> "\n"


filterUnknown :: Conduit Product (ResourceT IO) Product
filterUnknown = do
    result <- await
    case result of
        Just (Product "unknown" _        ) -> filterUnknown
        Just (Product _         "unknown") -> filterUnknown
        Just (Product u         p        ) -> yield (Product u p) >> filterUnknown
        Nothing -> return ()

parserConduit :: Parser Product -> Conduit ByteString (ResourceT IO) Product
parserConduit parser = do
    conduitParserEither parser =$= awaitForever go
    where   go (Left s) = error $ show s
            go (Right (_, p)) = yield p

accumulateProduct :: Table ItemSet -> Conduit Product (ResourceT IO) (Table ItemSet)
accumulateProduct !table = do
    p <- await
    case p of
        Just (Product userID productID) -> let table' = H.insertWith (\new set -> new `Set.union` set) userID (Set.singleton productID) table
                                            in accumulateProduct table'
        Nothing -> yield table





dropLine :: Parser ()
dropLine = skipWhile (/= 0xa) >> void (take 1) 

parseLine :: Parser ByteString
parseLine = do
    a <- takeTill (== 0xa)
    take 1
    return (B.copy a)

-- | raw data

parseProduct :: Parser ProductID
parseProduct = string "product/productId: " >> parseLine

parseUser :: Parser UserID
parseUser = string "review/userId: " >> parseLine

parseTransaction :: Parser Product
parseTransaction = do
    userID <- takeTill (== 0x20)
    take 1
    productID <- takeTill (== 0xa)
    take 1
    return (Product (B.copy userID) (B.copy productID))

parseSection :: Parser Product
parseSection = do
    productID <- parseProduct
    replicateM_ 2 dropLine
    userID <- parseUser
    replicateM_ 6 dropLine
    choice [dropLine, endOfInput]
    return (Product userID productID)