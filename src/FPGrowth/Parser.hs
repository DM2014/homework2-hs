{-# LANGUAGE OverloadedStrings #-}

module FPGrowth.Parser where

import              Control.Monad.Trans.Resource
import              Data.Attoparsec.ByteString.Lazy
import qualified    Data.ByteString.Lazy as BL 
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
parseWord = takeWhile1 (/= 0x20)

parseTransaction :: Parser Transaction
parseTransaction = do
    (_:xs) <- parseWord `sepBy'` (word8 0x20)
    return (Set.fromList xs)