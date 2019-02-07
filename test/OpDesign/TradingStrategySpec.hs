module OpDesign.TradingStrategySpec where

import Prelude (String, Monad, Maybe(..))
import Prelude (read, lines)
import Prelude (($))

import SpecHelper

import Data.Timezones.TZ (tzEST)
import Data.Time (UTCTime)
import Conduit (ConduitT)
import Conduit (yieldMany, runConduitPure,mapC, sinkList)
import Conduit ((.|))

import Data.Conduit.List()
import Data.Conduit.Combinators()

import OpDesign.OrderBookStream (orderBookStream)
import OpDesign.OrderBook (tickFields)

testInputData :: [String]
testInputData = lines "\
\2014-10-28 06:50:00.000000,BEST_BID,8938.0,10.0,S\n\
\2014-10-28 06:50:46.000000,BEST_ASK,8945.0,5.0,S\n\
\2014-10-28 06:50:54.000000,BEST_ASK,8941.0,4.0,S\n\
\2014-10-28 06:50:56.000000,BEST_BID,8940.0,11.0,S\n\
\2014-10-28 06:52:41.000000,BEST_ASK,8943.5,2.0,S\n\
\2014-10-28 06:52:43.000000,BEST_ASK,8950.0,5.0,S\n\
\2014-10-28 06:52:48.000000,BEST_BID,8945.0,2.0,S\n\
\2014-10-28 06:52:52.000000,BEST_BID,8933.0,40.0,S\n\
\2014-10-28 06:52:56.000000,BEST_BID,8945.0,10.0,S\n\
\2014-10-28 06:53:04.000000,BEST_BID,8940.0,6.0,S\n\
\2014-10-28 06:53:05.000000,BEST_BID,8938.5,8.0,S\n\
\"

processOrderBook :: OrderBook -> OrderBook
processOrderBook orderBook = orderBook

strategy :: Monad m => ConduitT OrderBook OrderBook m ()
strategy = mapC processOrderBook

spec :: Spec
spec = describe "Testing trading strategies" $ do

    context "with short test set" $
          it "should generate series of best order books" $
            runConduitPure ( yieldMany testInputData .| mapC (tickFields tzEST) .| orderBookStream .| strategy .| sinkList)
        `shouldBe` [
            OrderBook {date = (read "2014-10-28 11:50:00" :: UTCTime), bidVolume = Just $ Volume 10, bidPrice = Just $ Price 8938.0, askPrice = Nothing, askVolume = Nothing},
            OrderBook {date = (read "2014-10-28 11:50:46" :: UTCTime), bidVolume = Just $ Volume 10, bidPrice = Just $ Price 8938.0, askPrice = Just $ Price 8945.0, askVolume = Just $ Volume 5},
            OrderBook {date = (read "2014-10-28 11:50:54" :: UTCTime), bidVolume = Just $ Volume 10, bidPrice = Just $ Price 8938.0, askPrice = Just $ Price 8941.0, askVolume = Just $ Volume 4},
            OrderBook {date = (read "2014-10-28 11:50:56" :: UTCTime), bidVolume = Just $ Volume 11, bidPrice = Just $ Price 8940.0, askPrice = Just $ Price 8941.0, askVolume = Just $ Volume 4},
            OrderBook {date = (read "2014-10-28 11:52:41" :: UTCTime), bidVolume = Just $ Volume 11, bidPrice = Just $ Price 8940.0, askPrice = Just $ Price 8943.5, askVolume = Just $ Volume 2},
            OrderBook {date = (read "2014-10-28 11:52:43" :: UTCTime), bidVolume = Just $ Volume 11, bidPrice = Just $ Price 8940.0, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5},
            OrderBook {date = (read "2014-10-28 11:52:48" :: UTCTime), bidVolume = Just $ Volume 2, bidPrice = Just $ Price 8945.0, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5},
            OrderBook {date = (read "2014-10-28 11:52:52" :: UTCTime), bidVolume = Just $ Volume 40, bidPrice = Just $ Price 8933.0, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5},
            OrderBook {date = (read "2014-10-28 11:52:56" :: UTCTime), bidVolume = Just $ Volume 10, bidPrice = Just $ Price 8945.0, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5},
            OrderBook {date = (read "2014-10-28 11:53:04" :: UTCTime), bidVolume = Just $ Volume 6, bidPrice = Just $ Price 8940.0, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5},
            OrderBook {date = (read "2014-10-28 11:53:05" :: UTCTime), bidVolume = Just $ Volume 8, bidPrice = Just $ Price 8938.5, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5}
        ]
