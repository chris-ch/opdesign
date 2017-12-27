{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Data.Data (Data, Typeable)
import System.Console.CmdArgs (cmdArgs, def, help, opt, summary, typ, argPos, args, cmdArgsMode, cmdArgsRun, (&=))
import TicksReader (processTicks)

data CommandLine = CommandLine {
    pattern :: String,
    ticks :: FilePath
    } deriving (Show, Data, Typeable)

commandLine = cmdArgsMode CommandLine{
    pattern = def &= help "pattern for CSV files within archive",
    ticks = def &= argPos 0 &= typ "ARCHIVE"
    }

{-
data OrderBook = OrderBook {bidVolume :: Volume, bidPrice :: Price, askPrice :: Price, askVolume :: Volume} deriving (Show, Eq)

updateOrderBook orderBook newTick = orderBook
-}

main :: IO ()
main = do
    parsedArguments <- cmdArgsRun commandLine
    print $ pattern parsedArguments
    print $ ticks parsedArguments
    processTicks (ticks parsedArguments) (pattern parsedArguments) $ \tickData -> print tickData
    