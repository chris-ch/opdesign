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
import Conduit (ZipSource, getZipSink, getZipSource, sumC, lengthC, concatMapC, foldMapC, takeC, sinkList, scanlC)
import Data.Conduit
import qualified Data.Conduit.Combinators as Cmb (print)
import OrderBook (OrderBook, TickData, emptyOrderBook, updateOrderBook)
import Data.Void (Void)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Conduit.Text as CText (lines)
import Data.Text (Text, pack, unpack)
import Data.List (sortBy, dropWhileEnd)
import Data.Word (Word8)
import OrderBook (TickData, OrderBook, tickFields, fromTickData)

-----------------------------------------------------------

data CommandLine = CommandLine {
    pattern :: String,
    ticks :: FilePath
    } deriving (Show, Data, Typeable)

-- drops possible '\r' endings
dos2unix :: String -> String
dos2unix = dropWhileEnd (== '\r')

commandLine = cmdArgsMode CommandLine{
    pattern = def &= help "pattern for CSV files within archive, for example '/20.*csv'",
    ticks = def &= argPos 0 &= typ "ARCHIVE"
    }

-----------------------------------------------------------

accumulate :: Monad m => ConduitM OrderBook OrderBook m ()
accumulate = scanlC updateOrderBook emptyOrderBook

orderBookStream :: ConduitM ByteString Void (ResourceT IO) ()
orderBookStream = decodeUtf8C
                .| CText.lines
                .| mapC unpack
                .| mapC dos2unix
                .| mapC tickFields
                .| mapC fromTickData
                .| accumulate
                .| Cmb.print

-----------------------------------------------------------

main :: IO ()
main = do
    parsedArguments <- cmdArgsRun commandLine
    print $ pattern parsedArguments
    print $ ticks parsedArguments
    stream <- readTicks (ticks parsedArguments) (pattern parsedArguments) orderBookStream
    runConduit stream


-----------------------------------------------------------
testInputData :: [String]
testInputData = lines "\
\2014-10-28 06:50:00.000000,BEST_BID,8938.0,10.0,S\n\
\2014-10-28 06:50:46.000000,BEST_ASK,8945.0,5.0,S\n\
\2014-10-28 06:50:54.000000,BEST_ASK,8941.0,4.0,S\n\
\2014-10-28 06:50:56.000000,BEST_BID,8940.0,11.0,S\n\
\2014-10-28 06:52:41.000000,BEST_ASK,8943.5,2.0,S\n\
\2014-10-28 06:52:43.000000,BEST_ASK,8950.0,5.0,S\n\
\2014-10-28 06:52:48.000000,BEST_BID,8945.0,2.0,S\n\
\2014-10-28 06:52:52.000000,BEST_BID,8933.0,40.0,S\n\
\2014-10-28 06:52:56.000000,BEST_BID,8945.0,10.0,S\n\
\2014-10-28 06:53:04.000000,BEST_BID,8940.0,6.0,S\n\
\2014-10-28 06:53:05.000000,BEST_BID,8938.5,8.0,S\n\
\"


testTickFields :: IO ()
testTickFields = do
    runConduit $ yieldMany testInputData
                    .| mapC tickFields
                    .| mapC fromTickData
                    .| accumulate
                    .| mapM_C print
