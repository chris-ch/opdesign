module OpDesign.TicksReader where

import Prelude  (Bool, FilePath, String, IO)
import Prelude  (filter, mapM)
import Prelude  ((.), ($))

import Codec.Archive.Zip    (EntrySelector, ZipArchive, withArchive, getEntries, getEntryName, getEntrySource)
import Data.Maybe       (isJust)
import Data.List        (sort)
import Data.Text        (unpack)
import Text.Regex       (matchRegex, mkRegex)
import Data.Map         (keys)
import Conduit          (ConduitT, ResourceT)
import Data.ByteString  (ByteString)
import Control.Monad    (fmap, liftM)

import Data.Foldable    (sequence_)

-- checks whether we are processing a valid csv file containing ticks data
isTickFile :: String -> EntrySelector -> Bool
isTickFile tickFilePattern entry = isJust (matchRegex (mkRegex tickFilePattern) (unpack (getEntryName entry)))

selectEntries :: (EntrySelector -> Bool) -> [EntrySelector] -> [EntrySelector]
selectEntries filterOp = sort . filter filterOp

extractEntries :: FilePath -> IO [EntrySelector]
extractEntries path = withArchive path loadEntries
    where
        loadEntries :: ZipArchive [EntrySelector]
        loadEntries = fmap keys getEntries

listEntries :: FilePath -> String -> IO [EntrySelector] 
listEntries ticksFile filePattern = fmap (selectEntries selector) (extractEntries ticksFile)
        where
            selector = isTickFile filePattern

readTicksFromEntry :: FilePath -> EntrySelector -> IO (ConduitT () ByteString (ResourceT IO) ())
readTicksFromEntry ticksArchivePath entry = withArchive ticksArchivePath $ getEntrySource entry
        
strTicksByEntry :: FilePath -> String -> IO [ConduitT () ByteString (ResourceT IO) ()]
strTicksByEntry ticksFile csvFilePattern = do
    csvEntries <- listEntries ticksFile csvFilePattern
    mapM (readTicksFromEntry ticksFile) csvEntries

strTicks :: FilePath -> String -> IO (ConduitT () ByteString (ResourceT IO) ())
strTicks ticksFile csvFilePattern = liftM sequence_ $ strTicksByEntry ticksFile csvFilePattern
