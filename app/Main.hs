{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Data.Data (Data, Typeable)
import Data.Time (TimeZone)
import Data.Timezones.TZ (tzParse)
import Data.Void (Void)
import Data.ByteString (ByteString)
import Conduit ((.|))
import Conduit (Conduit, ConduitT, Sink, ResourceT)
import Conduit (runConduit)

import System.Console.CmdArgs (def, help, opt, typ, argPos, args, cmdArgsMode, cmdArgsRun, (&=))

import qualified Data.Conduit.Combinators as Cmb (print)

import OpDesign.TicksReader (readTicks)
import OpDesign.OrderBookStream (tickStream, orderBookStream)
import OpDesign.OrderBook (OrderBook)

-----------------------------------------------------------

data OpDesign = OpDesign {
    pattern :: String,
    ticks :: FilePath,
    timezone :: String
    } deriving (Show, Data, Typeable)

opdesign = cmdArgsMode OpDesign{
    pattern = def &= opt ".*/[^.]+.csv" &= help "pattern for CSV files within archive",
    timezone = def &= opt "EST" &= help "timezone for dates in archive file",
    ticks = def &= argPos 0 &= typ "ARCHIVE"
    }

-----------------------------------------------------------
outputStream :: TimeZone -> ConduitT ByteString Void (ResourceT IO) ()
outputStream tz = tickStream .| orderBookStream tz .| Cmb.print
          
-----------------------------------------------------------

main :: IO ()
main = do
    parsedArguments <- cmdArgsRun opdesign
    print $ "pattern for CSV files in archive: '" ++ (pattern parsedArguments) ++ "'"
    print $ "ticks archive file: '" ++ (ticks parsedArguments) ++ "'"
    print $ "timezone in archive file: '" ++ (timezone parsedArguments) ++ "'"
    stream <- readTicks (ticks parsedArguments) (pattern parsedArguments) (outputStream (tzParse (timezone parsedArguments)))
    runConduit stream
