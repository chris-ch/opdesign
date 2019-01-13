{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module OpDesign.TicksReader where

import Codec.Archive.Zip (EntrySelector, ZipArchive, withArchive, sourceEntry, getEntries, getEntryName)
import Data.Maybe (isJust)
import Data.List (sort)
import Data.Text (pack, unpack)
import Text.Regex (Regex, matchRegex, mkRegex)
import Data.Map (keys)
import Conduit ((.|), Conduit, ConduitT, ResourceT, yieldMany, runConduit, mapM_C)
import Data.ByteString (ByteString)
import Data.Void (Void)

readTicksFiles :: FilePath -> ConduitT ByteString Void (ResourceT IO) () -> EntrySelector -> IO ()
readTicksFiles ticksArchivePath sinkTicks entry = withArchive ticksArchivePath $ sourceEntry entry sinkTicks

-- checks whether we are processing a valid csv file containing ticks data
isTickFile :: String -> EntrySelector -> Bool
isTickFile tickFilePattern entry = isJust (matchRegex (mkRegex tickFilePattern) (unpack (getEntryName entry)))

extractEntries :: FilePath -> IO [EntrySelector]
extractEntries ticksArchivePath = withArchive ticksArchivePath loadEntries
    where
        loadEntries = fmap keys getEntries :: ZipArchive [EntrySelector]

selectEntries :: (EntrySelector -> Bool) -> [EntrySelector] -> [EntrySelector]
selectEntries filterOp = sort . filter filterOp

readTicks :: FilePath -> String -> ConduitT ByteString Void (ResourceT IO) () -> IO (ConduitT () Void IO ())
readTicks ticksFile csvFilePattern sinkTicks = do
    entries <- extractEntries ticksFile :: IO [EntrySelector]
    let csvEntries = selectEntries (isTickFile csvFilePattern) entries
    let stream = yieldMany csvEntries .| mapM_C (readTicksFiles ticksFile sinkTicks)
    return stream

