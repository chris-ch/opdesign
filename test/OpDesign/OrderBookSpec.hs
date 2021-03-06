module OpDesign.OrderBookSpec where

import SpecHelper

import Prelude (String, Integer, Int, Bool(..))
import Prelude (read, lines, Maybe(..), (+), (>>), ($), (==))
import Data.Ratio ((%))
import Data.Void()
import Data.Time()
import Conduit ()
import Conduit (yieldMany, runConduitPure, mapC, scanlC, dropC, takeC, sinkList)
import Conduit ((.|))
import Data.Time.LocalTime ()
import Data.List()
import Text.Show ()
import Data.Time (UTCTime)
import Data.Timezones.TZ (tzEST, tzUTC)
import Data.Conduit.List(groupBy)
import Data.Conduit.Combinators()
import Conduit()

import OpDesign.OrderBookStream (toOrderBook, toTickData, scanl1C, trfMidPrice, trfSample, ceilingMinute, getPeriod, sequencer, SamplePeriod(..))

testInputData :: [String]
testInputData = lines "\
\2014-10-28 06:50:00.000000,BEST_BID,8938.0,10.0,S\n\
\2014-10-28 06:50:46.000000,BEST_BID,8937.0,5.0,S\n\
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

testInputData2 :: [String]
testInputData2 = lines "\
\2014-10-28 06:50:00.000000,BEST_BID,8938.0,10.0,S\n\
\2014-10-28 06:50:46.000000,BEST_BID,8936.0,5.0,S\n\
\2014-10-28 06:50:46.000000,BEST_BID,8936.5,5.0,S\n\
\2014-10-28 06:50:46.000000,BEST_BID,8937.0,5.0,S\n\
\2014-10-28 06:50:54.000000,BEST_ASK,8941.0,4.0,S\n\
\"

mkUTC :: String -> UTCTime
mkUTC = read

spec :: Spec
spec = describe "Testing reading ticks using pipes" $ do

    context "simple" $
        it "should yield 3" $ 
            (1 + 2 :: Integer)
        `shouldBe` 3

    context "yielding fibonnacci series" $
        it "should produce fibbonaci series" $
            runConduitPure ( yieldMany [1..10 :: Integer] .| scanlC (+) 0 .| (dropC 1 >> sinkList) )
        `shouldBe` [1, 3, 6, 10, 15, 21, 28, 36, 45, 55 :: Integer]

    context "yielding fibonnacci series using scanlC" $
        it "should produce fibbonaci series" $
            runConduitPure ( yieldMany [1..10 :: Integer] .| scanl1C (+) .| sinkList )
        `shouldBe` [1, 3, 6, 10, 15, 21, 28, 36, 45, 55]

    context "inline test data" $
        it "should produce a list of orderbooks" $
            runConduitPure ( yieldMany testInputData .|  toTickData tzUTC .| toOrderBook .| sinkList)
        `shouldBe` [
            OrderBook {date = (read "2014-10-28 06:50:00" :: UTCTime), bidVolume = Just $ Volume 10, bidPrice = Just $ Price 8938.0, askPrice = Nothing, askVolume = Nothing},
            OrderBook {date = (read "2014-10-28 06:50:46" :: UTCTime), bidVolume = Just $ Volume 5, bidPrice = Just $ Price 8937.0, askPrice = Nothing, askVolume = Nothing},
            OrderBook {date = (read "2014-10-28 06:50:54" :: UTCTime), bidVolume = Just $ Volume 5, bidPrice = Just $ Price 8937.0, askPrice = Just $ Price 8941.0, askVolume = Just $ Volume 4},
            OrderBook {date = (read "2014-10-28 06:50:56" :: UTCTime), bidVolume = Just $ Volume 11, bidPrice = Just $ Price 8940.0, askPrice = Just $ Price 8941.0, askVolume = Just $ Volume 4},
            OrderBook {date = (read "2014-10-28 06:52:41" :: UTCTime), bidVolume = Just $ Volume 11, bidPrice = Just $ Price 8940.0, askPrice = Just $ Price 8943.5, askVolume = Just $ Volume 2},
            OrderBook {date = (read "2014-10-28 06:52:43" :: UTCTime), bidVolume = Just $ Volume 11, bidPrice = Just $ Price 8940.0, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5},
            OrderBook {date = (read "2014-10-28 06:52:48" :: UTCTime), bidVolume = Just $ Volume 2, bidPrice = Just $ Price 8945.0, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5},
            OrderBook {date = (read "2014-10-28 06:52:52" :: UTCTime), bidVolume = Just $ Volume 40, bidPrice = Just $ Price 8933.0, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5},
            OrderBook {date = (read "2014-10-28 06:52:56" :: UTCTime), bidVolume = Just $ Volume 10, bidPrice = Just $ Price 8945.0, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5},
            OrderBook {date = (read "2014-10-28 06:53:04" :: UTCTime), bidVolume = Just $ Volume 6, bidPrice = Just $ Price 8940.0, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5},
            OrderBook {date = (read "2014-10-28 06:53:05" :: UTCTime), bidVolume = Just $ Volume 8, bidPrice = Just $ Price 8938.5, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5}
        ]
 
    context "checking valid orderbooks" $
        it "should produce a list of booleans" $
            runConduitPure ( yieldMany testInputData .| toTickData tzUTC .| toOrderBook .| mapC isValid .| sinkList)
        `shouldBe` [False,False,True,True,True,True,True,True,True,True,True]

    context "midprices" $
        it "should produce a list of mid prices" $
            runConduitPure ( yieldMany testInputData .| toTickData tzUTC .| toOrderBook .| trfMidPrice .| sinkList)
        `shouldBe` [Nothing,Nothing,Just 8939,Just (17881 % 2),Just (35767 % 4),Just 8945,Just (17895 % 2),Just (17883 % 2),Just (17895 % 2),Just 8945,Just (35777 % 4)]

    context "minute sampling" $
        it "should produce a list of minute sampled orderbooks" $
            runConduitPure ( yieldMany testInputData .| toTickData tzEST .| toOrderBook .| trfSample Minute .| sinkList)
        `shouldBe` [
            OrderBook {date = (read "2014-10-28 11:51:00" :: UTCTime), bidVolume = Just $ Volume 11, bidPrice = Just $ Price 8940.0, askPrice = Just $ Price 8941.0, askVolume = Just $ Volume 4},
            OrderBook {date = (read "2014-10-28 11:53:00" :: UTCTime), bidVolume = Just $ Volume 10, bidPrice = Just $ Price 8945.0, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5},
            OrderBook {date = (read "2014-10-28 11:54:00" :: UTCTime), bidVolume = Just $ Volume 8, bidPrice = Just $ Price 8938.5, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5}
        ]
 
    context "second sampling" $
        it "should produce a list of second sampled orderbooks" $
            runConduitPure ( yieldMany testInputData2 .| toTickData tzEST .| toOrderBook .| trfSample Second .| takeC 3 .| sinkList)
        `shouldBe` [
            OrderBook {date = mkUTC "2014-10-28 11:50:01", bidVolume = Just $ Volume 10, bidPrice = Just $ Price 8938.0, askPrice = Nothing, askVolume = Nothing},
            OrderBook {date = mkUTC "2014-10-28 11:50:47", bidVolume = Just $ Volume 5, bidPrice = Just $ Price 8937.0, askPrice = Nothing, askVolume = Nothing},
            OrderBook {date = mkUTC "2014-10-28 11:50:55", bidVolume = Just $ Volume 5, bidPrice = Just $ Price 8937.0, askPrice = Just $ Price 8941.0, askVolume = Just $ Volume 4}
        ]
    context "grouped by minute" $
        let
            sameMinute orderBook orderBookNext = getPeriod Minute (date orderBook) == getPeriod Minute (date orderBookNext)
        in
        it "should produce a list of grouped orderbooks" $
            runConduitPure ( yieldMany testInputData .| toTickData tzEST .| toOrderBook .| groupBy sameMinute .| sinkList)
        `shouldBe` [
            [
            OrderBook {date = (read "2014-10-28 11:50:00" :: UTCTime), bidVolume = Just $ Volume 10, bidPrice = Just $ Price 8938.0, askPrice = Nothing, askVolume = Nothing},
            OrderBook {date = (read "2014-10-28 11:50:46" :: UTCTime), bidVolume = Just $ Volume 5, bidPrice = Just $ Price 8937.0, askPrice = Nothing, askVolume = Nothing},
            OrderBook {date = (read "2014-10-28 11:50:54" :: UTCTime), bidVolume = Just $ Volume 5, bidPrice = Just $ Price 8937.0, askPrice = Just $ Price 8941.0, askVolume = Just $ Volume 4},
            OrderBook {date = (read "2014-10-28 11:50:56" :: UTCTime), bidVolume = Just $ Volume 11, bidPrice = Just $ Price 8940.0, askPrice = Just $ Price 8941.0, askVolume = Just $ Volume 4}
            ],
            [
            OrderBook {date = (read "2014-10-28 11:52:41" :: UTCTime), bidVolume = Just $ Volume 11, bidPrice = Just $ Price 8940.0, askPrice = Just $ Price 8943.5, askVolume = Just $ Volume 2},
            OrderBook {date = (read "2014-10-28 11:52:43" :: UTCTime), bidVolume = Just $ Volume 11, bidPrice = Just $ Price 8940.0, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5},
            OrderBook {date = (read "2014-10-28 11:52:48" :: UTCTime), bidVolume = Just $ Volume 2, bidPrice = Just $ Price 8945.0, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5},
            OrderBook {date = (read "2014-10-28 11:52:52" :: UTCTime), bidVolume = Just $ Volume 40, bidPrice = Just $ Price 8933.0, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5},
            OrderBook {date = (read "2014-10-28 11:52:56" :: UTCTime), bidVolume = Just $ Volume 10, bidPrice = Just $ Price 8945.0, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5}
            ],
            [
            OrderBook {date = (read "2014-10-28 11:53:04" :: UTCTime), bidVolume = Just $ Volume 6, bidPrice = Just $ Price 8940.0, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5},
            OrderBook {date = (read "2014-10-28 11:53:05" :: UTCTime), bidVolume = Just $ Volume 8, bidPrice = Just $ Price 8938.5, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5}
            ]
        ]
 
    context "extracting seconds" $
        it "should return seconds component" $ 
            getPeriod Second (read "2014-10-28 06:50:14" :: UTCTime)
        `shouldBe` (14 :: Int)

    context "minute sampling" $
        it "should return next round minute" $ 
            ceilingMinute (read "2014-10-28 06:50:14" :: UTCTime)
        `shouldBe` (read "2014-10-28 06:51:00" :: UTCTime)

    context "minute sampling" $
        it "should return next round minute" $ 
            ceilingMinute (read "2014-10-28 23:59:14" :: UTCTime)
        `shouldBe` (read "2014-10-29 00:00:00" :: UTCTime)

    context "counting seconds" $
        it "should count 5 seconds" $ 
            runConduitPure (sequencer 1 (mkUTC "2014-10-28 23:59:14") .| takeC 5 .| sinkList)
        `shouldBe` [
            mkUTC "2014-10-28 23:59:14",
            mkUTC "2014-10-28 23:59:15",
            mkUTC "2014-10-28 23:59:16",
            mkUTC "2014-10-28 23:59:17",
            mkUTC "2014-10-28 23:59:18"
            ]

    context "counting minutes" $
        it "should count 5 minutes" $ 
            runConduitPure (sequencer 60 (mkUTC "2014-10-28 23:59:14") .| takeC 5 .| sinkList)
        `shouldBe` [
            mkUTC "2014-10-28 23:59:14",
            mkUTC "2014-10-29 00:00:14",
            mkUTC "2014-10-29 00:01:14",
            mkUTC "2014-10-29 00:02:14",
            mkUTC "2014-10-29 00:03:14"
            ]