{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Data.Data (Data, Typeable)
import Control.Monad.IO.Class (liftIO)
import System.Console.CmdArgs (def, help, opt, typ, argPos, args, cmdArgsMode, cmdArgsRun, (&=))
import TicksReader (readTicks)
import Conduit ((.|), Conduit, ConduitM, Sink, ResourceT,ZipSink, yieldMany, runConduit, mapM_C, mapMC, mapC, iterMC, dropC, getZipSink, getZipSource, decodeUtf8C)
import qualified Data.Conduit.Combinators as Cmb (print)
import OrderBook (OrderBook, TickData, emptyOrderBook, updateOrderBook)
import Data.Void (Void)
import Data.ByteString (ByteString)
import qualified Data.Conduit.Text as CText (lines)
import Data.Text (Text, pack, unpack)
import Data.List (sortBy, dropWhileEnd)
import OrderBook (TickData, OrderBook, tickFields)

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
    
tickProcessor :: ConduitM TickData OrderBook (ResourceT IO) ()
tickProcessor = mapC $ updateOrderBook emptyOrderBook

tickProcessorPrev :: ConduitM TickData OrderBook (ResourceT IO) ()
tickProcessorPrev = tickProcessor .| dropC 1

run :: FilePath -> String -> IO ()
run ticks pattern = readTicks ticks pattern $ decodeUtf8C
                .| CText.lines
                .| mapC unpack
                .| mapC dos2unix
                .| mapC tickFields
                .| tickProcessor
                .| Cmb.print -- mapM_C (liftIO . print)
main :: IO ()
main = do
    parsedArguments <- cmdArgsRun commandLine
    print $ pattern parsedArguments
    print $ ticks parsedArguments
    run (ticks parsedArguments) (pattern parsedArguments)
