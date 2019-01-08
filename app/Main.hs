{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Data.Data (Data, Typeable)
import Control.Monad.IO.Class (liftIO)
import System.Console.CmdArgs (def, help, opt, typ, argPos, args, cmdArgsMode, cmdArgsRun, (&=))
import OpDesign.TicksReader (readTicks)
import Conduit ((.|))
import Conduit (Conduit, ConduitM, Sink, ResourceT)
import Conduit (yieldMany, runConduit, mapM_C, mapMC, mapC, iterMC, dropC, decodeUtf8C)
import Conduit (ZipSource, getZipSink, getZipSource, sumC, lengthC, concatMapC, foldMapC, takeC, sinkList, foldMC, foldlC, scanlC)
import Data.Conduit
import qualified Data.Conduit.Combinators as Cmb (print)
import OpDesign.OrderBook (OrderBook, TickData, emptyOrderBook, tickFields, updateOrderBook, fromTickData)
import Data.Void (Void)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Conduit.Text as CText (lines)
import Data.Text (Text, pack, unpack)
import Data.List (sortBy, dropWhileEnd)
import Data.Word (Word8)

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

--tickStream :: ConduitM ByteString TickData (ResourceT IO) ()
tickStream = decodeUtf8C
                .| CText.lines
                .| mapC unpack
                .| mapC dos2unix
                .| mapC tickFields
                .| mapC fromTickData 
--                .| accumulate
--                .| mapM_C print
                
orderBookStream = tickStream .| mapC (updateOrderBook emptyOrderBook) -- .| foldMapC id
outputStream = orderBookStream .| Cmb.print
          
-----------------------------------------------------------
     
main :: IO ()
main = do
    parsedArguments <- cmdArgsRun commandLine
    print $ pattern parsedArguments
    print $ ticks parsedArguments
    stream <- readTicks (ticks parsedArguments) (pattern parsedArguments) outputStream
    runConduit stream
