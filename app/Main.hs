module Main where
    
import Prelude (Show, FilePath, String, IO)
import Prelude (($), (++))
import Prelude (print)

import Data.Data (Data, Typeable)
import Data.Timezones.TZ (tzParse)
import Data.ByteString (ByteString)
import Conduit ((.|))
import Conduit (runConduit, runResourceT)

import System.Console.CmdArgs (CmdArgs, def, help, opt, typ, argPos, cmdArgsMode, cmdArgsRun, (&=))
import System.Console.CmdArgs.Explicit (Mode)

import qualified Data.Conduit.Combinators as Cmb (print, last)

import OpDesign.TicksReader (ticksFile)
import OpDesign.OrderBookStream (cleanStrTicks, toOrderBook, toTickData)

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
    output <- runResourceT $ runConduit (strTicks .| cleanStrTicks .| toTickData tz .| toOrderBook .| Cmb.last )
    print $ output
