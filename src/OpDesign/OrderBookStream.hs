module OpDesign.OrderBookStream where

import Prelude (Monad, Maybe(..), Rational, String, Int, Bool)
import Prelude (return, maybe, last, toRational, round, last, head)
import Prelude ((.), ($), (==), (>>=), (+), (/))

import qualified Data.Conduit.Text as CText (lines)

import Data.List (dropWhileEnd)
import Data.Text (unpack)
import Data.Time(UTCTime(..), Day, utctDay)
import Data.Time.Calendar (toGregorian)
import Data.ByteString (ByteString)
import Data.Time (UTCTime(..), addUTCTime)
import Data.Time.LocalTime (TimeOfDay(..), timeToTimeOfDay, timeOfDayToTime)
import Data.Time.Calendar ()
import Conduit ((.|))
import Conduit (ConduitT, await, yield, evalStateC)
import Conduit (mapC, decodeUtf8C, scanlC, filterC)
import Data.Conduit.List (groupBy)
import Conduit (MonadThrow)

import Control.Monad.State (get, put, lift)
import Control.Monad.Trans.State.Strict (StateT)

import Data.Time (TimeZone, NominalDiffTime)

import OpDesign.OrderBook (OrderBook(..), TickData, updateOrderBook, fromTickData, fromPrice, parseTickData, isValid)

scanl1C :: Monad m => (a -> a -> a) -> ConduitT a a m ()
scanl1C f = await >>= maybe (return ()) (scanlC f)

-- drops possible '\r' endings
dos2unix :: String -> String
dos2unix = dropWhileEnd (== '\r')

cleanStrTicks :: (MonadThrow m) => ConduitT ByteString String m ()
cleanStrTicks = decodeUtf8C
                .| CText.lines
                .| mapC unpack
                .| mapC dos2unix

toTickData :: (Monad m) => TimeZone -> ConduitT String TickData m ()
toTickData tz = mapC (parseTickData tz)

type StateOrderBook = Maybe OrderBook
streamOrderBookC :: (Monad m) => ConduitT TickData OrderBook (StateT StateOrderBook m) ()
streamOrderBookC = do
        input <- await
        case input of
            Nothing -> return ()
            Just tick -> do
                prevOrderBook <- lift get
                let updatedOrderBook = makeOrderBook prevOrderBook tick
                lift $ put (Just updatedOrderBook)
                yield updatedOrderBook
                streamOrderBookC
   
makeOrderBook :: (Maybe OrderBook) -> TickData -> OrderBook
makeOrderBook mob t = case mob of
    (Just ob) -> (updateOrderBook ob t)
    Nothing -> (fromTickData t)

toOrderBook :: (Monad m ) => ConduitT TickData OrderBook m ()
toOrderBook = evalStateC Nothing streamOrderBookC

trfMidPrice :: (Monad m ) => ConduitT OrderBook (Maybe Rational) m ()
trfMidPrice = mapC midPrice
    where
        midPrice :: OrderBook -> Maybe Rational
        midPrice OrderBook {date=_, bidVolume=Just _, bidPrice=Just bid, askPrice=Just ask, askVolume=Just _ } = Just ((fromPrice bid + fromPrice ask) / 2)
        midPrice _ = Nothing

data SamplePeriod = Hour | Minute | Second

trfSample :: (Monad m) => SamplePeriod -> ConduitT OrderBook OrderBook m ()
trfSample Hour      = groupBy (samePeriod Hour)     .| mapC last .| mapC (\orderbook -> orderbook {date = ceilingHour (date orderbook)})
trfSample Minute    = groupBy (samePeriod Minute)   .| mapC last .| mapC (\orderbook -> orderbook {date = ceilingMinute (date orderbook)})
trfSample Second    = groupBy (samePeriod Second)   .| mapC last .| mapC (\orderbook -> orderbook {date = ceilingSecond (date orderbook)})

samePeriod :: SamplePeriod -> OrderBook -> OrderBook -> Bool
samePeriod period orderBook orderBookNext = getPeriod period (date orderBook) == getPeriod period (date orderBookNext)

getPeriod :: SamplePeriod -> UTCTime -> Int
getPeriod Hour      utcDate    = todHour (extractTime utcDate)
getPeriod Minute    utcDate    = todMin (extractTime utcDate)
getPeriod Second    utcDate    = (round . toRational) $ todSec (extractTime utcDate)

extractTime :: UTCTime -> TimeOfDay
extractTime = timeToTimeOfDay . utctDayTime

ceilingMinute :: UTCTime -> UTCTime
ceilingMinute time = UTCTime (utctDay time') (timeOfDayToTime (TimeOfDay hour minute 0))
    where
        time' :: UTCTime
        time' = addUTCTime 60 time
        (TimeOfDay hour minute _) = timeToTimeOfDay (utctDayTime time')

ceilingHour :: UTCTime -> UTCTime
ceilingHour time = UTCTime (utctDay time') (timeOfDayToTime (TimeOfDay hour 0 0))
    where
        time' :: UTCTime
        time' = addUTCTime 3600 time
        (TimeOfDay hour _ _) = timeToTimeOfDay (utctDayTime time')

ceilingSecond :: UTCTime -> UTCTime
ceilingSecond time = UTCTime (utctDay time') (timeOfDayToTime (TimeOfDay hour minute second))
    where
        time' :: UTCTime
        time' = addUTCTime 1 time
        (TimeOfDay hour minute second) = timeToTimeOfDay (utctDayTime time')

sequencer :: (Monad m) => NominalDiffTime -> UTCTime -> ConduitT () UTCTime m ()
sequencer period nextVal = do
    yield nextVal
    sequencer period (addUTCTime period nextVal)

onlyValid :: (Monad m) => ConduitT OrderBook OrderBook m ()
onlyValid = filterC isValid

tradingHours :: Monad m => ConduitT OrderBook (Maybe (UTCTime, UTCTime)) m ()
tradingHours = groupBy sameDay 
    .| mapC firstAndLast
        where
            
            sameDay ob1 ob2 =  toGregorian (utctDay (date ob1)) == toGregorian (utctDay (date ob2))

            firstAndLast [] = Nothing
            firstAndLast [unique] = Just (date unique, date unique)
            firstAndLast group = Just (date (head group), date (last group))
