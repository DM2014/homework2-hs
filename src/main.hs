module Main where
import              FPGrowth.Parser
import              FPGrowth.Types
import              FPGrowth.Tree
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
    transactions <- runResourceT $ CB.sourceFile "data/1m" $$ parserConduit =$ CL.consume

    let result = growForest . permuteTransaction 10 $ transactions

    BL.writeFile "output" (BL8.pack . show $ take 100 result)
