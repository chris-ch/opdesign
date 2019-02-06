module OpDesign.OrderBookSpec where

import SpecHelper

import Prelude (String, Integer, Bool(..))
import Prelude (read, lines, Maybe(..), (+), (>>), ($))
import Data.Ratio ((%))
import Data.Void()
import Data.Time()
import Conduit ()
import Conduit (yieldMany, runConduitPure, mapC, scanlC, dropC, sinkList)
import Conduit ((.|))

import Data.List()
import Text.Show ()
import Data.Time (UTCTime)
import Data.Timezones.TZ (tzEST, tzUTC)
import Data.Conduit.List()
import Data.Conduit.Combinators()
import Conduit()

import OpDesign.OrderBookStream (orderBookStream, scanl1C, trfMidPrice, trfSample, ceilingMinute)

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
            runConduitPure ( yieldMany testInputData .| orderBookStream tzUTC .| sinkList)
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
            runConduitPure ( yieldMany testInputData .| orderBookStream tzUTC .| mapC isValid .| sinkList)
        `shouldBe` [False,False,True,True,True,True,True,True,True,True,True]

    context "midprices" $
        it "should produce a list of mid prices" $
            runConduitPure ( yieldMany testInputData .| orderBookStream tzUTC .| trfMidPrice .| sinkList)
        `shouldBe` [Nothing,Nothing,Just 8939,Just (17881 % 2),Just (35767 % 4),Just 8945,Just (17895 % 2),Just (17883 % 2),Just (17895 % 2),Just 8945,Just (35777 % 4)]

    context "second sampling" $
        it "should produce a list of minute sampled orderbooks" $
            runConduitPure ( yieldMany testInputData .| orderBookStream tzEST .| trfSample .| sinkList)
        `shouldBe` [
            OrderBook {date = (read "2014-10-28 11:51:00" :: UTCTime), bidVolume = Just $ Volume 11, bidPrice = Just $ Price 8940.0, askPrice = Just $ Price 8941.0, askVolume = Just $ Volume 4},
            OrderBook {date = (read "2014-10-28 11:52:00" :: UTCTime), bidVolume = Just $ Volume 11, bidPrice = Just $ Price 8940.0, askPrice = Just $ Price 8941.0, askVolume = Just $ Volume 4},
            OrderBook {date = (read "2014-10-28 11:53:00" :: UTCTime), bidVolume = Just $ Volume 10, bidPrice = Just $ Price 8945.0, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5},
            OrderBook {date = (read "2014-10-28 11:54:00" :: UTCTime), bidVolume = Just $ Volume 8, bidPrice = Just $ Price 8938.5, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5}
        ]
 
    context "minute sampling" $
        it "should return next round minute" $ 
            ceilingMinute (read "2014-10-28 06:50:14" :: UTCTime)
        `shouldBe` (read "2014-10-28 06:51:00" :: UTCTime)

    context "minute sampling" $
        it "should return next round minute" $ 
            ceilingMinute (read "2014-10-28 23:59:14" :: UTCTime)
        `shouldBe` (read "2014-10-29 00:00:00" :: UTCTime)
