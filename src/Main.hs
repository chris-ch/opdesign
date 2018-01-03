{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Data.Data (Data, Typeable)
import Control.Monad.IO.Class (liftIO)
import System.Console.CmdArgs (def, help, opt, typ, argPos, args, cmdArgsMode, cmdArgsRun, (&=))
import TicksReader (processTicks)
import Conduit ((.|), Conduit, ConduitM, Sink, ResourceT,ZipSink, yieldMany, runConduit, mapM_C, mapMC, mapC, iterMC, dropC, getZipSink, getZipSource)
import OrderBook (OrderBook, TickData, emptyOrderBook, updateOrderBook)
import Data.Void (Void)


data CommandLine = CommandLine {
    pattern :: String,
    ticks :: FilePath
    } deriving (Show, Data, Typeable)

commandLine = cmdArgsMode CommandLine{
    pattern = def &= help "pattern for CSV files within archive",
    ticks = def &= argPos 0 &= typ "ARCHIVE"
    }

tickProcessor :: Conduit TickData (ResourceT IO) OrderBook
tickProcessor = mapC (updateOrderBook emptyOrderBook)

tickProcessorPrev = tickProcessor .| dropC 1

--coupled :: Monad m => ConduitM Double Void m Double
--coupled = getZipSource $ (,) <$> tickProcessor <*> tickProcessorPrev

main :: IO ()
main = do
    parsedArguments <- cmdArgsRun commandLine
    print $ pattern parsedArguments
    print $ ticks parsedArguments
    processTicks (ticks parsedArguments) (pattern parsedArguments) $ tickProcessor .| mapM_C (liftIO . print)
