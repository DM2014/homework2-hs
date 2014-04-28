{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Parser (processRawData, readTransaction) where

import              Control.Applicative             ((<$>))
import              Control.Monad                   (replicateM_, void)
import              Control.Monad.Trans.Resource
import              Data.Attoparsec.ByteString
import              Data.ByteString                 (ByteString)
import              Data.Conduit
import              Data.Conduit.Attoparsec
import qualified    Data.Conduit.Binary             as CB
import qualified    Data.Conduit.List               as CL
import qualified    Data.ByteString.Short           as BS
import              Data.ByteString.Short           (ShortByteString)
import qualified    Data.HashMap.Strict             as H
import              Data.Hashable                   (Hashable, hashWithSalt)
import              Data.Monoid                     ((<>))
import qualified    Data.Set                        as Set
import              Data.Set                        (Set)
import              Prelude                         hiding (take)
import              System.IO                       (stdin, stdout)

data Product = Product  {-# UNPACK #-} !UserID 
                        {-# UNPACK #-} !ProductID deriving (Show)
type ProductID = ShortByteString
type UserID = ShortByteString
type Table a = H.HashMap UserID a
type Transaction = Set ProductID

instance Hashable ShortByteString where
    hashWithSalt n = hashWithSalt n . BS.fromShort

processRawData :: IO ()
processRawData = runResourceT $ CB.sourceHandle stdin $$ parserConduit parseSection =$= filterUnknown =$= accumulateProduct H.empty =$= toByteString =$ CB.sinkHandle stdout

readTransaction :: IO [Transaction]
readTransaction = runResourceT $ CB.sourceHandle stdin $$ parserConduit parseTransaction =$= CL.consume


toByteString :: Conduit (Table Transaction) (ResourceT IO) ByteString
toByteString = do
    t <- await
    case t of
        Just table -> mapM_ (yield . toLine) (H.toList table)
        Nothing -> return ()
    where   toLine (_, v) = BS.fromShort $ Set.foldl' (\a b -> a <> " " <> b) BS.empty v <> "\n"


filterUnknown :: Conduit Product (ResourceT IO) Product
filterUnknown = do
    result <- await
    case result of
        Just (Product "unknown" _        ) -> filterUnknown
        Just (Product _         "unknown") -> filterUnknown
        Just (Product u         p        ) -> yield (Product u p) >> filterUnknown
        Nothing -> return ()

parserConduit :: Parser a -> Conduit ByteString (ResourceT IO) a
parserConduit parser = do
    conduitParserEither parser =$= awaitForever go
    where   go (Left s) = error $ show s
            go (Right (_, p)) = yield p

accumulateProduct :: Table Transaction -> Conduit Product (ResourceT IO) (Table Transaction)
accumulateProduct !table = do
    p <- await
    case p of
        Just (Product userID productID) -> let table' = H.insertWith (\new set -> new `Set.union` set) userID (Set.singleton productID) table
                                            in accumulateProduct table'
        Nothing -> yield table

dropLine :: Parser ()
dropLine = skipWhile (/= 0xa) >> void (take 1) 

parseLine :: Parser ShortByteString
parseLine = do
    a <- takeTill (== 0xa)
    take 1
    return (BS.toShort a)

-- | raw data

parseProduct :: Parser ProductID
parseProduct = string "product/productId: " >> parseLine

parseUser :: Parser UserID
parseUser = string "review/userId: " >> parseLine

parseSection :: Parser Product
parseSection = do
    productID <- parseProduct
    replicateM_ 2 dropLine
    userID <- parseUser
    replicateM_ 6 dropLine
    choice [dropLine, endOfInput]
    return (Product userID productID)

-- | Transaction
parseItem :: Parser ProductID
parseItem = BS.toShort <$> takeTill (\c -> c == 0x20 || c == 0xa)

parseTransaction :: Parser Transaction
parseTransaction = do
    transaction <- parseItem `sepBy'` (string " ")
    take 1
    return (Set.fromList transaction)
