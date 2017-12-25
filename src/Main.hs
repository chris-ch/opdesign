{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude (IO, Ord, Ordering, Bool, fmap, compare, ($), (==), (++), filter, print, mapM, putStrLn, show, return)
import Codec.Archive.Zip (EntrySelector, ZipArchive, withArchive, sourceEntry, getEntries, getEntryName)
import Path (Path, Abs, File)
import Data.Maybe (isJust)
import Path.IO (resolveFile')
import Data.List (sortBy)
import Data.Text (Text, pack, unpack, splitOn)
import Data.Text.Encoding (decodeUtf8)
import Text.Regex (Regex, matchRegex, mkRegex)
import Data.ByteString.Char8 (concat)
import Data.Map (keys)
import Data.ByteString (ByteString)
import Control.Monad.IO.Class (liftIO)
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT

loadTicks :: Path Abs File -> EntrySelector -> IO ()
loadTicks ticksArchivePath entry = withArchive ticksArchivePath $ do
    sourceEntry entry $ CT.decode CT.utf8
            =$ CT.lines
            =$ CL.mapM_ (\t -> liftIO $ putStrLn $ "Got a line: " ++ show t)
    
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
        tickFilePattern = mkRegex "20.*csv$" :: Regex
        entryName = getEntryName entry :: Text

main :: IO ()
main = do
    --ticksArchivePath <- resolveFile' "data/data-small.zip" :: IO (Path Abs File)
    ticksArchivePath <- resolveFile' "data/GX1%20Index.zip" :: IO (Path Abs File)
    entries <- extractEntries ticksArchivePath :: IO [EntrySelector]
    let csvEntries = sortBy customSort $ filter isTickFile entries :: [EntrySelector]
    print csvEntries
    let ticks = loadTicks ticksArchivePath :: EntrySelector -> IO ()
    contents <- mapM ticks csvEntries :: IO [()]
    return ()
    --let ticks = decodeUtf8 $ concat contents :: Text
    --let lineSep = pack "\r\n" :: Text
    --print $ splitOn lineSep ticks
    