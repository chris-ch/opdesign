{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Data.Data (Data, Typeable)
import Control.Monad.IO.Class (liftIO)
import System.Console.CmdArgs (def, help, opt, typ, argPos, args, cmdArgsMode, cmdArgsRun, (&=))
import TicksReader (readTicks)
import Conduit ((.|))
import Conduit (Conduit, ConduitM, Sink, ResourceT)
import Conduit (yieldMany, runConduit, mapM_C, mapMC, mapC, iterMC, dropC, decodeUtf8C)
import Conduit (ZipSource, getZipSink, getZipSource, sumC, lengthC, concatMapC)
import Data.Conduit
import qualified Data.Conduit.Combinators as Cmb (print)
import OrderBook (OrderBook, TickData, emptyOrderBook, updateOrderBook)
import Data.Void (Void)
import Data.ByteString (ByteString)
import qualified Data.Conduit.Text as CText (lines)
import Data.Text (Text, pack, unpack)
import Data.List (sortBy, dropWhileEnd)
import OrderBook (TickData, OrderBook, tickFields)

-----------------------------------------------------------

data CommandLine = CommandLine {
    pattern :: String,
    ticks :: FilePath
    } deriving (Show, Data, Typeable)

-- drops possible '\r' endings
dos2unix :: String -> String
dos2unix = dropWhileEnd (== '\r')

commandLine = cmdArgsMode CommandLine{
    pattern = def &= help "pattern for CSV files within archive",
    ticks = def &= argPos 0 &= typ "ARCHIVE"
    }

-----------------------------------------------------------

tickStream :: ConduitM ByteString TickData (ResourceT IO) ()
tickStream = decodeUtf8C
                .| CText.lines
                .| mapC unpack
                .| mapC dos2unix
                .| mapC tickFields
                
                -- .| Cmb.print -- mapM_C (liftIO . print)
                
orderBookStream :: ConduitM ByteString OrderBook (ResourceT IO) ()
orderBookStream = tickStream .| mapC (updateOrderBook emptyOrderBook)

shiftedOrderBookStream :: ConduitM ByteString OrderBook (ResourceT IO) ()
shiftedOrderBookStream = orderBookStream .| dropC 1

outputStream :: ConduitM ByteString Void (ResourceT IO) ()
outputStream = orderBookStream .| Cmb.print

-----------------------------------------------------------
     
main :: IO ()
main = do
    parsedArguments <- cmdArgsRun commandLine
    print $ pattern parsedArguments
    print $ ticks parsedArguments
    readTicks (ticks parsedArguments) (pattern parsedArguments) outputStream

-----------------------------------------------------------

mergeOrderBooks :: ConduitM ByteString OrderBook (ResourceT IO) ()
mergeOrderBooks = getZipConduit
    $ ZipConduit (orderBookStream)
   *> ZipConduit (shiftedOrderBookStream)

-----------------------------------------------------------

average :: Monad m => ConduitM Double Void m Double
average =
    getZipSink (go <$> ZipSink sumC <*> ZipSink lengthC)
  where
    go total len = total / fromIntegral len

-----------------------------------------------------------

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)

indexedFibs :: Source IO (Int, Int)
indexedFibs = getZipSource
    $ (,)
  <$> ZipSource (yieldMany [1..])
  <*> ZipSource (yieldMany fibs)
  
-----------------------------------------------------------

