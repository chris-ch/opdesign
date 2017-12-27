{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module TicksReader where

import Codec.Archive.Zip (EntrySelector, ZipArchive, withArchive, sourceEntry, getEntries, getEntryName)
import Path (Path, Abs, File)
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
import Numeric (readFloat, readSigned, fromRat, showFFloat)
import Data.Time

data TickType = Trade | BestBid | BestAsk deriving (Show, Eq)

parseTickType :: String -> TickType
parseTickType "TRADE" = Trade
parseTickType "BEST_BID" = BestBid
parseTickType "BEST_ASK" = BestAsk

newtype Price = Price Rational deriving (Eq, Ord)
instance Show Price where
    show (Price price) = (showFFloat (Just 6) $ fromRat price) ""
    
newtype Volume = Volume Int deriving (Eq, Ord)
instance Show Volume where
    show (Volume volume) = show volume
    
-- Example: "2014-10-28 06:53:05.000000,TRADE,8938.5,0.0,S"
data TickData = TickData {
    date :: UTCTime,
    tickType :: TickType,
    price :: Price,
    volume :: Volume,
    flag :: String
    } deriving (Show)

processTicksFiles :: Path Abs File -> (Text -> IO()) -> EntrySelector -> IO ()
processTicksFiles ticksArchivePath processLine entry = withArchive ticksArchivePath $ do
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

-- checks whether we are processing a valid csv file containing ticks data
isTickFile :: String -> EntrySelector -> Bool
isTickFile tickFilePattern entry = isJust $ matchRegex (mkRegex tickFilePattern) (unpack entryName)
    where
        entryName = getEntryName entry :: Text

-- drops possible '\r' endings
dos2unix :: String -> String
dos2unix = dropWhileEnd (== '\r')

tickFields :: String -> TickData
tickFields line = TickData { date = fieldDate, tickType = fieldTickType, price = fieldPrice, volume = fieldVolume, flag = fieldFlag}
    where
        fields = map unpack . splitOn "," $ pack line
        fieldDate = (read $ fields!!0)::UTCTime
        fieldTickType = parseTickType $ fields!!1
        fieldPrice = Price $ fst . head $ readSigned readFloat $ fields!!2
        fieldVolume = Volume $ truncate (read $ fields!!3 :: Float)
        fieldFlag = fields!!4

processTicks :: String -> String -> (TickData -> IO ()) -> IO ()
processTicks ticksFile csvFilePattern tickProcessor = do
    ticksArchivePath <- resolveFile' $ ticksFile :: IO (Path Abs File)
    entries <- extractEntries ticksArchivePath :: IO [EntrySelector]
    let csvEntries = sortBy customSort $ filter (isTickFile $ csvFilePattern) entries :: [EntrySelector]
    print csvEntries
    let ticks = processTicksFiles ticksArchivePath $ tickProcessor . tickFields . dos2unix . unpack
    contents <- mapM_ ticks csvEntries :: IO ()
    return ()
    