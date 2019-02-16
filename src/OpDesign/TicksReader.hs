module OpDesign.TicksReader where

import Prelude (Bool, FilePath, String, IO, Monad)
import Prelude (filter, map)
import Prelude ((.), ($))

import Codec.Archive.Zip (EntrySelector, ZipArchive, withArchive, sourceEntry, getEntries, getEntryName, getEntrySource)
import Data.Maybe (isJust)
import Data.List (sort)
import Data.Text (unpack)
import Text.Regex (matchRegex, mkRegex)
import Data.Map (keys)
import Conduit ((.|), ConduitT, ResourceT, yieldMany, mapM_C, MonadIO)
import Data.ByteString (ByteString)
import Data.Void (Void)
import Control.Monad.State (get)
import Control.Monad (join, fmap, return)

--sourceFile :: MonadResource m => Zip -> FilePath -> Source m ByteString

--sourceEntry :: FilePath -> Sink ByteString (ResourceT Archive) a -> Archive a

--withArchive :: FilePath -> Archive a -> IO a

--readTicksFiles' :: FilePath -> EntrySelector -> IO ()
--readTicksFiles' ticksArchivePath entry = withArchive ticksArchivePath entry

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
listEntries ticksFile csvFilePattern = fmap (selectEntries (isTickFile csvFilePattern)) (extractEntries ticksFile)

readTicks :: FilePath -> String -> ConduitT ByteString Void (ResourceT IO) () -> IO (ConduitT () Void IO ())
readTicks ticksFile csvFilePattern sinkTicks = do
    csvEntries <- listEntries ticksFile csvFilePattern
    let stream = yieldMany csvEntries .| mapM_C (readTicksFiles ticksFile sinkTicks)
    return stream

--readTicks' :: FilePath -> String -> ConduitT ByteString Void (ResourceT IO) () -> IO [IO ()]
readTicks' ticksFile csvFilePattern sinkTicks = do
    x <- (listEntries ticksFile csvFilePattern)
    return $ fmap (readTicksFiles ticksFile sinkTicks) x

readTicksFiles :: FilePath -> ConduitT ByteString Void (ResourceT IO) () -> EntrySelector -> IO ()
readTicksFiles ticksArchivePath sinkTicks entry = withArchive ticksArchivePath $ sourceEntry entry sinkTicks


--readTicksFiles :: FilePath -> ConduitT ByteString Void (ResourceT IO) () -> EntrySelector -> IO ()
readTicksFiles' :: FilePath -> EntrySelector -> IO (ConduitT () ByteString (ResourceT IO) ())
readTicksFiles' ticksArchivePath entry = withArchive ticksArchivePath $ getEntrySource entry
