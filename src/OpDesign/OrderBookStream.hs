module OpDesign.OrderBookStream where

import Prelude (Monad, Maybe(..), Rational, String, Int)
import Prelude (return, maybe, last)
import Prelude ((.), ($), (==), (>>=), (+), (/))

import qualified Data.Conduit.Text as CText (lines)

import Data.List (dropWhileEnd)
import Data.Text (unpack)
import Data.Time()
import Data.ByteString (ByteString)
import Data.Time (UTCTime(..), addUTCTime)
import Data.Time.LocalTime (TimeOfDay(..), todMin, timeToTimeOfDay, timeOfDayToTime)
import Data.Time.Calendar ()
import Conduit ((.|))
import Conduit (ConduitT, await, yield, evalStateC)
import Conduit (mapC, decodeUtf8C, scanlC)
import Conduit()
import Data.Conduit.List (groupBy)
import Conduit (MonadThrow)

import Control.Monad.State (get, put, lift)
import Control.Monad.Trans.State.Strict (StateT)

import Data.Time (TimeZone, NominalDiffTime)

import OpDesign.OrderBook (OrderBook(..), TickData, updateOrderBook, fromTickData, fromPrice, parseTickData)

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

trfSample :: (Monad m) => ConduitT OrderBook OrderBook m ()
trfSample = groupBy sameMinute .| mapC last .| mapC (\orderbook -> orderbook {date = ceilingMinute (date orderbook)})
    where
        sameMinute orderBook orderBookNext = extractMinute (date orderBook) == extractMinute (date orderBookNext)

extractMinute :: UTCTime -> Int
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
        (TimeOfDay hour minute _) = timeToTimeOfDay (utctDayTime time')

sequencer :: (Monad m) => NominalDiffTime -> UTCTime -> ConduitT () UTCTime m ()
sequencer period nextVal = do
    yield nextVal
    sequencer period (addUTCTime period nextVal)

