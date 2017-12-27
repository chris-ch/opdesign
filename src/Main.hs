{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}
module Main where

import Prelude (FilePath, IO, Ord, Ordering, Bool, String, Show, fmap, compare, (.), ($), (==), (++), filter, print, mapM, putStrLn, show, return) 
import Codec.Archive.Zip (EntrySelector, ZipArchive, withArchive, sourceEntry, getEntries, getEntryName)
import Path (Path, Abs, File)
import Data.Data (Data, Typeable)
import Data.Maybe (isJust)
import Path.IO (resolveFile')
import Data.List (sortBy, dropWhileEnd)
import Data.Text (Text, pack, unpack, splitOn)
import Text.Regex (Regex, matchRegex, mkRegex)
import Data.Map (keys)
import Control.Monad.IO.Class (liftIO)
import Data.Conduit ((=$=))
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import System.Console.CmdArgs (cmdArgs, def, help, opt, summary, (&=))

{-
data CommandLine = CommandLine {
    pattern :: String,
    tickFile :: FilePath
    } deriving (Show, Data, Typeable)

commandLine = CommandLine{
    pattern = def &= opt ".*csv$" &= help "pattern for CSV files within archive",
    tickFile = def &= help "ticks archive"
    } &= summary "Ticks data extraction"
 -}
         
processTicks :: Path Abs File -> (Text -> IO()) -> EntrySelector -> IO ()
processTicks ticksArchivePath processLine entry = withArchive ticksArchivePath $ do
    sourceEntry entry $ CT.decode CT.utf8
            =$= CT.lines
            =$= CL.mapM_ (\line -> liftIO $ processLine line)
    
extractEntries :: Path Abs File -> IO [EntrySelector]
extractEntries ticksArchivePath = withArchive ticksArchivePath loadEntries
    where
        loadEntries = fmap keys getEntries :: ZipArchive [EntrySelector]

-- not really useful since only natural ordering is required
customSort :: Ord a => a -> a -> Ordering
customSort elem1 elem2 = compare elem1 elem2

isTickFile :: String -> EntrySelector -> Bool
isTickFile tickFilePattern entry = isJust $ matchRegex (mkRegex tickFilePattern) (unpack entryName)
    where
        entryName = getEntryName entry :: Text

dos2unix :: Text -> Text
dos2unix = pack . dropWhileEnd (== '\r') . unpack

main :: IO ()
main = do
    --cmdArgs commandLine
    ticksArchivePath <- resolveFile' "data/data-small.zip" :: IO (Path Abs File)
    --ticksArchivePath <- resolveFile' "data/GX1%20Index.zip" :: IO (Path Abs File)
    entries <- extractEntries ticksArchivePath :: IO [EntrySelector]
    let csvEntries = sortBy customSort $ filter (isTickFile "/20.*csv$") entries :: [EntrySelector]
    print csvEntries
    let ticks = processTicks ticksArchivePath (print . dos2unix)
    contents <- mapM ticks csvEntries :: IO [()]
    return ()