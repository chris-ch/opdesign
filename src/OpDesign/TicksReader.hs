{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module OpDesign.TicksReader where

import Codec.Archive.Zip (EntrySelector, ZipArchive, withArchive, sourceEntry, getEntries, getEntryName)
import Path (Path, Abs, File)
import Data.Maybe (isJust)
import Data.List (sortBy)
import Data.Text (pack, unpack)
import Text.Regex (Regex, matchRegex, mkRegex)
import Data.Map (keys)
import Conduit ((.|), Conduit, ConduitM, ResourceT, yieldMany, runConduit, mapM_C)
import Data.ByteString (ByteString)
import Data.Void (Void)
import Data.Time (UTCTime)

readTicksFiles :: FilePath -> ConduitM ByteString Void (ResourceT IO) () -> EntrySelector -> IO ()
readTicksFiles ticksArchivePath sinkTicks entry = withArchive ticksArchivePath $ sourceEntry entry sinkTicks

-- not really useful since only natural ordering is required
customSort :: Ord a => a -> a -> Ordering
customSort elem1 elem2 = compare elem1 elem2

-- checks whether we are processing a valid csv file containing ticks data
isTickFile :: String -> EntrySelector -> Bool
isTickFile tickFilePattern entry = isJust $ matchRegex (mkRegex tickFilePattern) (unpack entryName)
    where
        entryName = getEntryName entry

extractEntries :: FilePath -> IO [EntrySelector]
extractEntries ticksArchivePath = withArchive ticksArchivePath loadEntries
    where
        loadEntries = fmap keys getEntries :: ZipArchive [EntrySelector]

readTicks :: FilePath -> String -> ConduitM ByteString Void (ResourceT IO) () -> IO (ConduitM () Void IO ())
readTicks ticksFile csvFilePattern sinkTicks = do
    entries <- extractEntries ticksFile :: IO [EntrySelector]
    let csvEntries = sortBy customSort $ filter (isTickFile csvFilePattern) entries :: [EntrySelector]
    let stream = yieldMany csvEntries .| mapM_C (readTicksFiles ticksFile sinkTicks)
    return stream

