{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module TicksReader where

import Codec.Archive.Zip (EntrySelector, ZipArchive, withArchive, sourceEntry, getEntries, getEntryName)
import Path (Path, Abs, File)
import Data.Maybe (isJust)
import Path.IO (resolveFile')
import Data.List (sortBy, dropWhileEnd)
import Data.Text (Text, pack, unpack)
import Text.Regex (Regex, matchRegex, mkRegex)
import Data.Map (keys)
import Control.Monad.IO.Class (liftIO)
import Conduit ((.|), ConduitM, Sink, mapM_C, mapMC, mapC)
--import Data.ByteString (ByteString)
import Data.Void (Void)
import qualified Data.Conduit.Text as CText (lines, decode, utf8)
import Data.Time (UTCTime)

import OrderBook (TickData, OrderBook, tickFields)

-- drops possible '\r' endings
dos2unix :: String -> String
dos2unix = dropWhileEnd (== '\r')

--ticksPipe :: (TickData -> IO ()) -> ConduitM ByteString (IO ()) IO ()
ticksPipe processLine = CText.decode CText.utf8 
        .| CText.lines
        .| mapC unpack
        .| mapC dos2unix
        .| mapC tickFields
        .| mapC processLine

processTicksFiles :: Path Abs File -> (TickData -> IO()) -> EntrySelector -> IO ()
processTicksFiles ticksArchivePath processLine entry = withArchive ticksArchivePath $ do
    sourceEntry entry $ ticksPipe processLine .| mapM_C liftIO
    
-- not really useful since only natural ordering is required
customSort :: Ord a => a -> a -> Ordering
customSort elem1 elem2 = compare elem1 elem2

-- checks whether we are processing a valid csv file containing ticks data
isTickFile :: String -> EntrySelector -> Bool
isTickFile tickFilePattern entry = isJust $ matchRegex (mkRegex tickFilePattern) (unpack entryName)
    where
        entryName = getEntryName entry :: Text

extractEntries :: Path Abs File -> IO [EntrySelector]
extractEntries ticksArchivePath = withArchive ticksArchivePath loadEntries
    where
        loadEntries = fmap keys getEntries :: ZipArchive [EntrySelector]

processTicks :: String -> String -> (TickData -> IO ()) -> IO ()
processTicks ticksFile csvFilePattern tickProcessor = do
    ticksArchivePath <- resolveFile' $ ticksFile :: IO (Path Abs File)
    entries <- extractEntries ticksArchivePath :: IO [EntrySelector]
    let csvEntries = sortBy customSort $ filter (isTickFile $ csvFilePattern) entries :: [EntrySelector]
    let processEntryTicks = processTicksFiles ticksArchivePath tickProcessor :: EntrySelector -> IO ()
    mapM_ processEntryTicks csvEntries :: IO ()
    