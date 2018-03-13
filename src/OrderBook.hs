{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module OrderBook where

import Data.Time.Clock (UTCTime)
import Numeric (readFloat, readSigned, fromRat, showFFloat)
import Data.Text (pack, unpack, splitOn)
import Data.Monoid (Monoid)
import Data.Maybe (isJust)

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

tickFields :: String -> TickData
tickFields line = TickData {
    date = fieldDate,
    tickType = fieldTickType,
    price = fieldPrice,
    volume = fieldVolume,
    flag = fieldFlag
    } where
        fields = map unpack . splitOn "," $ pack line
        fieldDate = (read $ fields!!0)::UTCTime
        fieldTickType = parseTickType $ fields!!1
        fieldPrice = Price $ fst . head $ readSigned readFloat $ fields!!2
        fieldVolume = Volume $ truncate (read $ fields!!3 :: Float)
        fieldFlag = fields!!4

data OrderBook = OrderBook {
    bidVolume :: Maybe Volume,
    bidPrice :: Maybe Price,
    askPrice :: Maybe Price,
    askVolume :: Maybe Volume} deriving (Show, Eq)

emptyOrderBook :: OrderBook
emptyOrderBook = OrderBook {
    bidVolume = Nothing,
    bidPrice = Nothing,
    askPrice = Nothing,
    askVolume = Nothing
    }

updateOrderBook :: OrderBook -> TickData -> OrderBook
updateOrderBook prevOrderBook newTick
    | tickType newTick == BestBid = prevOrderBook {
        bidVolume = Just $ volume newTick,
        bidPrice = Just $ price newTick
        }
    | tickType newTick == BestAsk = prevOrderBook {
        askVolume = Just $ volume newTick,
        askPrice = Just $ price newTick
        }
    | otherwise  = prevOrderBook

fromTickData :: TickData -> OrderBook
fromTickData tickData = updateOrderBook emptyOrderBook tickData

updateOrderBook' :: OrderBook -> OrderBook -> OrderBook
updateOrderBook' orderBook orderBookUpdate = OrderBook {
    bidVolume = if isJust (bidVolume orderBookUpdate) then bidVolume orderBookUpdate else Nothing,
    bidPrice = if isJust (bidPrice orderBookUpdate) then bidPrice orderBookUpdate else Nothing,
    askPrice = if isJust (askPrice orderBookUpdate) then askPrice orderBookUpdate else Nothing,
    askVolume = if isJust (askVolume orderBookUpdate) then askVolume orderBookUpdate else Nothing
    }

instance Monoid OrderBook where
  mempty = emptyOrderBook
  mappend orderBook orderBookUpdate = updateOrderBook' orderBook orderBookUpdate
