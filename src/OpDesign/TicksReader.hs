module OpDesign.TicksReader where

import Prelude (Bool, FilePath, String, IO, Monad, Maybe(..), Ord, Foldable, traverse, pure, fst)
import Prelude (filter, map, mapM_, mapM)
import Prelude ((.), ($))

import Codec.Archive.Zip (EntrySelector, ZipArchive, withArchive, sourceEntry, getEntries, getEntryName, getEntrySource)
import Data.Maybe (isJust)
import Data.List (sort)
import Data.Text (unpack)
import Text.Regex (matchRegex, mkRegex)
import Data.Map (keys)
import Conduit ((.|), ConduitT, ResourceT, yieldMany, mapM_C, MonadIO, liftIO, yield, Source)
import Data.ByteString (ByteString)
import Data.Void (Void)
import Control.Monad.State (get)
import Control.Monad (join, fmap, return, (>>=))
import Conduit (runConduit, runConduitPure, await)

import Control.Monad.Trans (lift)
import Data.Conduit        (ConduitT, await, sealConduitT, yield, ($$++), sequenceSources)
import Data.Foldable       (toList, sequence_)
import Data.List           (sortOn)

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

readTicks :: FilePath -> String -> ConduitT ByteString Void (ResourceT IO) () -> IO ()
readTicks  ticksFile csvFilePattern sinkTicks = do
    csvEntries <- listEntries ticksFile csvFilePattern
    mapM_ (readTicksFiles ticksFile sinkTicks) csvEntries
    where
        readTicksFiles :: FilePath -> ConduitT ByteString Void (ResourceT IO) () -> EntrySelector -> IO ()
        readTicksFiles ticksArchivePath sinkTicks entry = withArchive ticksArchivePath $ sourceEntry entry sinkTicks

readTicksFiles' :: FilePath -> EntrySelector -> IO (ConduitT () ByteString (ResourceT IO) ())
readTicksFiles' ticksArchivePath entry = withArchive ticksArchivePath $ getEntrySource entry

--readTicks' :: FilePath -> String -> IO [ConduitT () ByteString (ResourceT IO) ()]
readTicks' ticksFile csvFilePattern = do
    csvEntries <- listEntries ticksFile csvFilePattern
    mapM (readTicksFiles' ticksFile) csvEntries


-- chainSources :: Monad m => [ConduitT () a m ()] -> ConduitT () a m ()
-- chainSources (source : otherSources) = do
--     runConduit source
--     chainSources otherSources

-- counterC :: (MonadState b m, Num a, Num b) => ConduitT a b m ()
-- counterC = do
--         x0 <-  await
--         case x0 of
--             Nothing -> return ()
--             Just _ -> do
--                 lift $ modify (+1)
--                 r <- lift get
--                 yield r
--                 counterC

-- genSequence :: (Monad m) => Int -> ConduitT () Int m ()
-- genSequence nextVal = do
--     yield nextVal
--     genSequence (nextVal + 1)

-- | Merge multiple sources into one producer.
mergeSources :: (Monad m) => [ConduitT () a m ()] -> ConduitT () a m ()
mergeSources = sequence_

mergeSources' :: (Monad m) => [ConduitT () a m ()] -> ConduitT () a m ()
mergeSources' = mergeSealed . fmap sealConduitT
  where
    mergeSealed sources = do
        prefetchedSources <- lift $ traverse ($$++ await) sources
        go [(a, s) | (s, Just a) <- prefetchedSources]
    go [] = pure ()
    go sources = do
        let (a, src1) : sources1 = sources
        yield a
        (src2, mb) <- lift $ src1 $$++ await
        let sources2 = case mb of
                Nothing -> sources1
                Just b  -> (b, src2) : sources1
        go sources2

