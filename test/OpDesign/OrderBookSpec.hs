module OpDesign.OrderBookSpec where

import SpecHelper

import Conduit (ConduitT)
import Conduit (yieldMany, runConduitPure, mapC, scanlC, decodeUtf8C, sinkList)
import Conduit ((.|))

import Data.Time (UTCTime)
import qualified Data.Conduit.List as CL (scanl, scan, mapAccum, mapAccumM) 
import qualified Data.Conduit.Combinators as Cmb (print)

import OpDesign.OrderBookStream (orderBookStream)

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

spec :: Spec
spec = describe "Testing reading ticks using pipes" $ do

    context "simple" $
        it "should yield 3" $ 
            1 + 2 
        `shouldBe` 3

    context "yielding array of 10 first integers" $
        it "should yield many" $ 
            runConduitPure (yieldMany [1..10] .| sinkList)
        `shouldBe` [1..10]

    context "yielding array of 10 first integers increased by 1" $
        it "should be increased by 1" $
            runConduitPure ( yieldMany [1..10] .| mapC (+ 1) .| sinkList )
        `shouldBe` [2..11]

    context "yielding fibonnacci series" $
        it "should be increased by 1" $
            runConduitPure ( yieldMany [1..10] .| scanlC (+) 0 .| sinkList )
        `shouldBe` [2..11]

    context "using test data" $
          it "should produce a stream of orderbooks" $
            runConduitPure ( yieldMany testInputData .| orderBookStream .| sinkList)
        `shouldBe` [
            OrderBook {date = (read "2014-10-28 06:50:00" :: UTCTime), bidVolume = Just $ Volume 10, bidPrice = Just $ Price 8938.0, askPrice = Nothing, askVolume = Nothing},
            OrderBook {date = (read "2014-10-28 06:50:46" :: UTCTime), bidVolume = Just $ Volume 10, bidPrice = Just $ Price 8938.0, askPrice = Just $ Price 8945.0, askVolume = Just $ Volume 5},
            OrderBook {date = (read "2014-10-28 06:50:54" :: UTCTime), bidVolume = Just $ Volume 10, bidPrice = Just $ Price 8938.0, askPrice = Just $ Price 8941.0, askVolume = Just $ Volume 4},
            OrderBook {date = (read "2014-10-28 06:50:56" :: UTCTime), bidVolume = Just $ Volume 11, bidPrice = Just $ Price 8940.0, askPrice = Just $ Price 8941.0, askVolume = Just $ Volume 4},
            OrderBook {date = (read "2014-10-28 06:52:41" :: UTCTime), bidVolume = Just $ Volume 11, bidPrice = Just $ Price 8940.0, askPrice = Just $ Price 8943.5, askVolume = Just $ Volume 2},
            OrderBook {date = (read "2014-10-28 06:52:43" :: UTCTime), bidVolume = Just $ Volume 11, bidPrice = Just $ Price 8940.0, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5},
            OrderBook {date = (read "2014-10-28 06:52:48" :: UTCTime), bidVolume = Just $ Volume 2, bidPrice = Just $ Price 8945.0, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5},
            OrderBook {date = (read "2014-10-28 06:52:52" :: UTCTime), bidVolume = Just $ Volume 40, bidPrice = Just $ Price 8933.0, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5},
            OrderBook {date = (read "2014-10-28 06:52:56" :: UTCTime), bidVolume = Just $ Volume 10, bidPrice = Just $ Price 8945.0, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5},
            OrderBook {date = (read "2014-10-28 06:53:04" :: UTCTime), bidVolume = Just $ Volume 6, bidPrice = Just $ Price 8940.0, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5},
            OrderBook {date = (read "2014-10-28 06:53:05" :: UTCTime), bidVolume = Just $ Volume 8, bidPrice = Just $ Price 8938.5, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5}
        ]
 
