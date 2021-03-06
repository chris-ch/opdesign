module Main where
    
import Prelude (Show, FilePath, String, IO, Maybe(..), Double)
import Prelude (($), (++), (==), (&&))
import Data.Time (UTCTime(..), Day, utctDay)
import Prelude (print, head, last, show)

import Data.Data (Data, Typeable)
import Data.Timezones.TZ (tzParse)
import Data.ByteString (ByteString)


import Conduit (ConduitT, MonadThrow, MonadIO)
import Conduit ((.|))
import Conduit (runConduitRes, mapC)

import System.Console.CmdArgs (CmdArgs, def, help, opt, typ, argPos, cmdArgsMode, cmdArgsRun, (&=))
import System.Console.CmdArgs.Explicit (Mode)

import qualified Data.Conduit.Combinators as Cmb (print)

import Data.Time (TimeZone)
import Numeric (showFFloat)

import Data.Conduit.Log (logC)
import OpDesign.TicksReader (ticksFile)
import OpDesign.OrderBook (OrderBook, date)
import OpDesign.OrderBookStream (cleanStrTicks, toOrderBook, toTickData, trfSample, onlyValid, SamplePeriod(..), tradingHours)
import OpDesign.TradingStrategy (scalpingStrategy, legProfit)
import OpDesign.TradingStrategy (LimitOrder, SinglePortfolioPosition)

import Data.Conduit.List (groupBy)

-----------------------------------------------------------

data OpDesign = OpDesign {
    pattern :: String,
    ticks :: FilePath,
    timezone :: String
    } deriving (Show, Data, Typeable)

opdesign :: Mode (CmdArgs OpDesign)
opdesign = cmdArgsMode OpDesign{
    pattern = def &= opt (".*/[^.]+.csv" :: ByteString) &= help "pattern for CSV files within archive",
    timezone = def &= opt ("EST" :: ByteString) &= help "timezone for dates in archive file",
    ticks = def &= argPos 0 &= typ "ARCHIVE"
    }

main :: IO ()
main = do
    parsedArguments <- cmdArgsRun opdesign
    print $ "pattern for CSV files in archive: '" ++ (pattern parsedArguments) ++ "'"
    print $ "ticks archive file: '" ++ (ticks parsedArguments) ++ "'"
    print $ "timezone used for interpreting archive file content: '" ++ (timezone parsedArguments) ++ "'"
    strTicks <- ticksFile (ticks parsedArguments) (pattern parsedArguments)
    let tz = tzParse $ timezone parsedArguments
    runConduitRes ( strTicks
        .| ticksProcessing tz .| Cmb.print
        )

--ticksProcessing :: (MonadThrow m, MonadIO m) => TimeZone -> ConduitT ByteString (LimitOrder, SinglePortfolioPosition) m ()
ticksProcessing tz = cleanStrTicks
    .| toTickData tz
    .| toOrderBook
    .| trfSample Second
    .| onlyValid
    .| logC "ob.log"
    .| scalpingStrategy
    .| legProfit
    .| mapC (\(x, y) -> show x ++ ": " ++ (toFloatStr y))
    -- .| tradingHours

toFloatStr :: Double -> String
toFloatStr value = showFFloat (Just 6) value ""
