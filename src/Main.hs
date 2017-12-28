{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Data.Data (Data, Typeable)
import System.Console.CmdArgs (def, help, opt, typ, argPos, args, cmdArgsMode, cmdArgsRun, (&=))
import TicksReader (processTicks)
import OrderBook (OrderBook, TickData, emptyOrderBook, updateOrderBook)

data CommandLine = CommandLine {
    pattern :: String,
    ticks :: FilePath
    } deriving (Show, Data, Typeable)

commandLine = cmdArgsMode CommandLine{
    pattern = def &= help "pattern for CSV files within archive",
    ticks = def &= argPos 0 &= typ "ARCHIVE"
    }

tickProcessor :: TickData -> OrderBook
tickProcessor tickData = updateOrderBook emptyOrderBook tickData

main :: IO ()
main = do
    parsedArguments <- cmdArgsRun commandLine
    print $ pattern parsedArguments
    print $ ticks parsedArguments
    processTicks (ticks parsedArguments) (pattern parsedArguments) $ print . tickProcessor
    