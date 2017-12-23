{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude (IO, Ord, Ordering, Bool, fmap, compare, ($), filter, print, mapM)
import Codec.Archive.Zip (EntrySelector, ZipArchive, withArchive, getEntry, getEntries, getEntryName)
import Path (Path, Abs, File)
import Data.Maybe (isJust)
import Path.IO (resolveFile')
import Data.List (sortBy)
import Data.Text (Text, unpack)
import Text.Regex (Regex, matchRegex, mkRegex)
import Data.ByteString.Char8 (concat)
import Data.Map (keys)
import Data.ByteString (ByteString)

loadTicks :: Path Abs File -> EntrySelector -> IO ByteString
loadTicks ticksArchivePath entry = withArchive ticksArchivePath (getEntry entry)

extractEntries :: Path Abs File -> IO [EntrySelector]
extractEntries ticksArchivePath = withArchive ticksArchivePath loadEntries
    where
        loadEntries = fmap keys getEntries :: ZipArchive [EntrySelector]

-- not really useful since only natural ordering is required
customSort :: Ord a => a -> a -> Ordering
customSort elem1 elem2 = compare elem1 elem2

isTickFile :: EntrySelector -> Bool
isTickFile entry = isJust $ matchRegex tickFilePattern (unpack entryName)
    where
        tickFilePattern = mkRegex "/20.*csv$" :: Regex
        entryName = getEntryName entry :: Text

main :: IO ()
main = do
    ticksArchivePath <- resolveFile' "data/data-small.zip" :: IO (Path Abs File)
    entries <- extractEntries ticksArchivePath :: IO [EntrySelector]
    let csvEntries = sortBy customSort $ filter isTickFile entries :: [EntrySelector]
    print csvEntries
    let ticks = loadTicks ticksArchivePath :: EntrySelector -> IO ByteString
    contents <- mapM ticks csvEntries :: IO [ByteString]
    print $ concat contents        