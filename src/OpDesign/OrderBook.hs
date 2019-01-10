{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module OpDesign.OrderBook where

import Data.Time (UTCTime)
import Numeric (readFloat, readSigned, fromRat, showFFloat)
import Data.Text (pack, unpack, splitOn)
import Data.Monoid (Monoid)

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

fromTickData :: TickData -> OrderBook
fromTickData tickData
    | tickType tickData == BestBid = OrderBook {
        bidVolume = Just $ volume tickData,
        bidPrice = Just $ price tickData,
        askVolume = Nothing,
        askPrice = Nothing

        }
    | tickType tickData == BestAsk = OrderBook {
        bidVolume = Nothing,
        bidPrice = Nothing,
        askVolume = Just $ volume tickData,
        askPrice = Just $ price tickData
        }
    | otherwise  = emptyOrderBook


updateOrderBookField :: Maybe a -> Maybe a -> Maybe a
updateOrderBookField _ (Just newFieldValue) = Just newFieldValue
updateOrderBookField (Just oldFieldValue) Nothing = Just oldFieldValue
updateOrderBookField Nothing Nothing = Nothing

updateOrderBook :: OrderBook -> OrderBook -> OrderBook
updateOrderBook orderBook orderBookUpdate = OrderBook {
    bidVolume = updateOrderBookField (bidVolume orderBook) (bidVolume orderBookUpdate),
    bidPrice = updateOrderBookField (bidPrice orderBook) (bidPrice orderBookUpdate),
    askPrice = updateOrderBookField (askPrice orderBook) (askPrice orderBookUpdate),
    askVolume = updateOrderBookField (askVolume orderBook) (askVolume orderBookUpdate)
    }

