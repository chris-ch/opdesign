{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Data.Data (Data, Typeable)
import System.Console.CmdArgs (def, help, opt, typ, argPos, args, cmdArgsMode, cmdArgsRun, (&=))
import Conduit ((.|))
import Conduit (Conduit, ConduitM, Sink, ResourceT)
import Conduit (yieldMany, runConduit, mapM_C, mapC, decodeUtf8C)
import Data.Conduit
import Data.ByteString (ByteString)
import Data.List (dropWhileEnd)
import Data.Word (Word8)
import Data.Text (pack, unpack)

import qualified Data.Conduit.Combinators as Cmb (print)
import qualified Data.Conduit.Text as CText (lines)

import OpDesign.OrderBook (emptyOrderBook, updateOrderBook, fromTickData, tickFields)
import OpDesign.TicksReader (readTicks)

-----------------------------------------------------------

data CommandLine = CommandLine {
    pattern :: String,
    ticks :: FilePath
    } deriving (Show, Data, Typeable)

-- drops possible '\r' endings
dos2unix :: String -> String
dos2unix = dropWhileEnd (== '\r')

opdesign = cmdArgsMode CommandLine{
    pattern = def &= help "pattern for CSV files within archive", -- &= opt ".*/[^.]+.csv",
    ticks = def &= argPos 0 &= typ "ARCHIVE"
    }

-----------------------------------------------------------

--tickStream :: ConduitM ByteString TickData (ResourceT IO) ()
tickStream = decodeUtf8C
                .| CText.lines
                .| mapC unpack
                .| mapC dos2unix
                .| mapC tickFields
                .| mapC fromTickData 
--                .| accumulate
--                .| mapM_C print
                
orderBookStream = tickStream .| mapC (updateOrderBook emptyOrderBook)
outputStream = orderBookStream .| Cmb.print
          
-----------------------------------------------------------
     
main :: IO ()
main = do
    parsedArguments <- cmdArgsRun opdesign
    print $ "pattern for CSV files in archive: '" ++ (pattern parsedArguments) ++ "'"
    print $ "ticks archive file: '" ++ (ticks parsedArguments) ++ "'"
    stream <- readTicks (ticks parsedArguments) (pattern parsedArguments) outputStream
    runConduit stream
