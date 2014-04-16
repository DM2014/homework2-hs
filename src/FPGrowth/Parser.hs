{-# LANGUAGE OverloadedStrings #-}

module FPGrowth.Parser where

import              Control.Monad.Trans.Resource
import              Data.Attoparsec.ByteString.Lazy
--import qualified    Data.ByteString.Lazy as BL 
import              Data.ByteString (ByteString)
import qualified    Data.Set as Set

import              FPGrowth.Types
import              Data.Conduit
import              Data.Conduit.Attoparsec
import              Prelude hiding (take)

parserConduit :: Conduit ByteString (ResourceT IO) Transaction
parserConduit = do
    conduitParserEither parseTransaction =$= awaitForever go
    where   go (Left s) = error $ show s
            go (Right (_, p)) = yield p

parseWord :: Parser ByteString
parseWord = do
    w <- takeTill (\ c -> c == 0x20 || c == 0xa)
    skipWhile (== 0x20)
    return w

--a :: BL.ByteString
--a = "A1G40P7TYZTMB4  B0000WLLWE\nA1G40P7TYZTMB4  B0000WLLWE B0000WLLWO B0000WLLWY B0000WLLXI B0000WLLXS B0000WLLYC B0000WLLYW"

parseTransaction :: Parser Transaction
parseTransaction = do
    _ <- parseWord
    xs <- manyTill parseWord (choice [newLine, endOfInput])
    return (Set.fromList xs)

    where   newLine = word8 0xa >> return ()