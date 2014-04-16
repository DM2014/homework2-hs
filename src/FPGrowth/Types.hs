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

type ProductID = ByteString
type Transaction = Set ProductID