module OpDesign.OrderBook where

import Prelude (Show, Maybe(..), Bool(..), String, Float, Eq, Rational, Int, Ord, Double)
import Prelude (truncate, head, read, show, otherwise, fst, map, error)
import Prelude (($), (==), (/=), (!!), (.), (++), (&&), (>))

import Data.Time (UTCTime, TimeZone)
import Data.Timezones.TZ (asUTC)
import Numeric (readFloat, readSigned, fromRat, showFFloat)
import Data.Text (pack, unpack, splitOn)

data TickType = Trade | BestBid | BestAsk deriving (Show, Eq)

parseTickType :: String -> TickType
parseTickType "TRADE" = Trade
parseTickType "BEST_BID" = BestBid
parseTickType "BEST_ASK" = BestAsk
parseTickType wrongTickType = error $ "Invalid tick type: " ++ wrongTickType

newtype Price = Price Rational deriving (Eq, Ord)
instance Show Price where
    show (Price aPrice) = (toFloatStr (fromRat aPrice :: Double)) ""
        where
            toFloatStr = showFFloat (Just 6) 

fromPrice :: Price -> Rational
fromPrice (Price value) = value

newtype Volume = Volume Int deriving (Eq, Ord)
instance Show Volume where
    show (Volume aVolume) = show aVolume
    
-- Example: "2014-10-28 06:53:05.000000,TRADE,8938.5,0.0,S"
data TickData = TickData {
    tickDate :: UTCTime,
    tickType :: TickType,
    price :: Price,
    volume :: Volume,
    flag :: String
    } deriving (Show)

parseTickData :: TimeZone -> String -> TickData
parseTickData tz line = TickData {
    tickDate = fieldDate,
    tickType = fieldTickType,
    price = fieldPrice,
    volume = fieldVolume,
    flag = fieldFlag
    } where
        fields = map unpack . splitOn "," $ pack line
        fieldDate = asUTC tz (fields!!0)
        fieldTickType = parseTickType $ fields!!1
        fieldPrice = Price $ fst . head $ readSigned readFloat $ fields!!2
        fieldVolume = Volume $ truncate (read $ fields!!3 :: Float)
        fieldFlag = fields!!4

data OrderBook = OrderBook {
    date :: UTCTime,
    bidVolume :: Maybe Volume,
    bidPrice :: Maybe Price,
    askPrice :: Maybe Price,
    askVolume :: Maybe Volume} deriving (Show, Eq)

isValid :: OrderBook -> Bool
isValid OrderBook {date=_, bidVolume=Just (Volume bv), bidPrice=Just (Price bp), askPrice=Just (Price ap), askVolume=Just (Volume av) } = (bv > 0) && (av > 0) && (ap > 0) && (bp /= ap)
isValid _ = False

emptyOrderBook :: UTCTime -> OrderBook
emptyOrderBook timestamp = OrderBook {
    date = timestamp,
    bidVolume = Nothing,
    bidPrice = Nothing,
    askPrice = Nothing,
    askVolume = Nothing
    }

fromTickData :: TickData -> OrderBook
fromTickData tickData
    | tickType tickData == BestBid = OrderBook {
        date = tickDate tickData,
        bidVolume = Just $ volume tickData,
        bidPrice = Just $ price tickData,
        askVolume = Nothing,
        askPrice = Nothing

        }
    | tickType tickData == BestAsk = OrderBook {
        date = tickDate tickData,
        bidVolume = Nothing,
        bidPrice = Nothing,
        askVolume = Just $ volume tickData,
        askPrice = Just $ price tickData
        }
    | otherwise  = emptyOrderBook $ tickDate tickData

updateOrderBook :: OrderBook -> TickData -> OrderBook
updateOrderBook orderBook tickData
    | (tickType tickData == BestBid) = orderBook {
            date = tickDate tickData,
            bidVolume = Just $ volume tickData,
            bidPrice = Just $ price tickData
        }
    | (tickType tickData == BestAsk) = orderBook {
            date = tickDate tickData,
            askVolume = Just $ volume tickData,
            askPrice = Just $ price tickData
        }
    | otherwise  = orderBook

    