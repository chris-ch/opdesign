module OpDesign.OrderBookStream where

import Prelude (Monad, Maybe(..), Rational, String, IO)
import Prelude (otherwise, return, maybe)
import Prelude ((.), ($), (/=), (==), (>>=), (+), (/))

import qualified Data.Conduit.Text as CText (lines)

import Data.List (dropWhileEnd)
import Data.Text (pack, unpack)
import Data.Time (TimeZone)
import Data.ByteString (ByteString)
import Data.Time (UTCTime(..), DiffTime, addUTCTime, secondsToDiffTime)
import Data.Time.LocalTime (TimeOfDay(..), todHour, todMin, todSec, timeToTimeOfDay, timeOfDayToTime)
import Data.Time.Calendar (Day, fromGregorian, toGregorian)
import Conduit ((.|))
import Conduit (ConduitT, ResourceT, await)
import Conduit (mapC, decodeUtf8C, scanlC)

import qualified Conduit as DC (ZipSource(..), getZipSource)

import OpDesign.OrderBook (OrderBook(..), updateOrderBook, fromTickData, tickFields, isValid, fromPrice)

scanl1C :: Monad m => (a -> a -> a) -> ConduitT a a m ()
scanl1C f = await >>= maybe (return ()) (scanlC f)

-- drops possible '\r' endings
dos2unix :: String -> String
dos2unix = dropWhileEnd (== '\r')

tickStream :: ConduitT ByteString String (ResourceT IO) ()
tickStream = decodeUtf8C
                .| CText.lines
                .| mapC unpack
                .| mapC dos2unix

orderBookStream :: Monad m => TimeZone -> ConduitT String OrderBook m ()
orderBookStream tz = mapC (tickFields tz) .| mapC fromTickData .| scanl1C updateOrderBook

trfMidPrice :: Monad m => ConduitT OrderBook (Maybe Rational) m ()
trfMidPrice = mapC midPrice
    where
        midPrice :: OrderBook -> Maybe Rational
        midPrice OrderBook {date=_, bidVolume=Just _, bidPrice=Just bid, askPrice=Just ask, askVolume=Just _ } = Just ((fromPrice bid + fromPrice ask) / 2)
        midPrice _ = Nothing

trfSample :: Monad m => ConduitT OrderBook OrderBook m ()
trfSample = scanl1C lastOfPeriod
    where
        lastOfPeriod orderBook orderBookNext
            | extractMinute (date orderBook) /= extractMinute (date orderBookNext) = OrderBook {date=date orderBook, bidVolume=bidVolume orderBook, bidPrice=bidPrice orderBook, askPrice=askPrice orderBook, askVolume=askVolume orderBook }
            | otherwise = OrderBook {date=date orderBookNext, bidVolume=bidVolume orderBookNext, bidPrice=bidPrice orderBookNext, askPrice=askPrice orderBookNext, askVolume=askVolume orderBookNext }

extractMinute utcDate = todMin (extractTime utcDate)

extractTime :: UTCTime -> TimeOfDay
extractTime = timeToTimeOfDay . utctDayTime

mapMinutes :: Monad m => ConduitT OrderBook (OrderBook, Maybe TimeOfDay) m ()
mapMinutes = mapC $ \orderBook -> (orderBook, Just (extractTime (date orderBook)))

plusOneMin :: UTCTime -> UTCTime
plusOneMin = addUTCTime 60

ceilingMinute :: UTCTime -> UTCTime
ceilingMinute time = UTCTime (utctDay time') (timeOfDayToTime (TimeOfDay hour minute 0))
    where
        time' :: UTCTime
        time' = plusOneMin time
        (TimeOfDay hour minute second) = timeToTimeOfDay (utctDayTime time')
