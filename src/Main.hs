{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Data.Data (Data, Typeable)
import Control.Monad.IO.Class (liftIO)
import System.Console.CmdArgs (def, help, opt, typ, argPos, args, cmdArgsMode, cmdArgsRun, (&=))
import TicksReader (processTicks)
import Conduit ((.|), Conduit, ConduitM, Sink, ResourceT, yieldMany, runConduit, mapM_C, mapMC, mapC, iterMC, stdoutC)
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

--tickProcessor :: TickData -> IO ()
tickProcessor tickData = updateOrderBook emptyOrderBook tickData

--abc :: Conduit TickData (IO) (IO ())
--abc :: ConduitM TickData (IO ()) IO ()
abc :: ConduitM TickData Void (ResourceT IO) ()
abc = mapC tickProcessor .| mapM_C (liftIO . print)

main :: IO ()
main = do
    parsedArguments <- cmdArgsRun commandLine
    print $ pattern parsedArguments
    print $ ticks parsedArguments
    processTicks (ticks parsedArguments) (pattern parsedArguments) abc
