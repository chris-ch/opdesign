{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Data.Data (Data, Typeable)
import Data.Void (Void)
import Data.ByteString (ByteString)
import Conduit ((.|))
import Conduit (Conduit, ConduitM, Sink, ResourceT)
import Conduit (runConduit)

import System.Console.CmdArgs (def, help, opt, typ, argPos, args, cmdArgsMode, cmdArgsRun, (&=))

import qualified Data.Conduit.Combinators as Cmb (print)

import OpDesign.TicksReader (readTicks)
import OpDesign.OrderBookStream (orderBookStream)
import OpDesign.OrderBook (OrderBook)

-----------------------------------------------------------

data CommandLine = CommandLine {
    pattern :: String,
    ticks :: FilePath
    } deriving (Show, Data, Typeable)

opdesign = cmdArgsMode CommandLine{
    pattern = def &= opt ".*/[^.]+.csv" &= help "pattern for CSV files within archive",
    ticks = def &= argPos 0 &= typ "ARCHIVE"
    }

-----------------------------------------------------------
outputStream :: ConduitM ByteString Void (ResourceT IO) ()
outputStream = orderBookStream .| Cmb.print
          
-----------------------------------------------------------
     
main :: IO ()
main = do
    parsedArguments <- cmdArgsRun opdesign
    print $ "pattern for CSV files in archive: '" ++ (pattern parsedArguments) ++ "'"
    print $ "ticks archive file: '" ++ (ticks parsedArguments) ++ "'"
    stream <- readTicks (ticks parsedArguments) (pattern parsedArguments) outputStream
    runConduit stream
