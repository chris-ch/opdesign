module OpDesign.OrderBookSpec where

import SpecHelper

import Conduit (ConduitM)
import Conduit (yieldMany, runConduit, runConduitPure, mapM_C, mapMC, mapC, iterMC, dropC, decodeUtf8C, sinkList, scanlC)
import Conduit ((.|))
import qualified Data.Conduit.List as CL (scanl, scan, mapAccum, mapAccumM) 
import qualified Data.Conduit.Combinators as Cmb (print)

import OpDesign.OrderBook (OrderBook, TickData, emptyOrderBook, tickFields, updateOrderBook, fromTickData)

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

main :: IO ()
--main = hspec spec
main = print "TEST"

accumulate :: Monad m => ConduitM OrderBook OrderBook m ()
accumulate = scanlC updateOrderBook emptyOrderBook

spec :: Spec
spec = describe "Testing pipes" $ do
    context "simple" $
        it "should yield 3" $ 
            1 + 2 
        `shouldBe` 3

    context "yielding [1..10]" $
        it "should yield many" $ 
            runConduitPure (yieldMany [1..10] .| sinkList)
        `shouldBe` [1..10]

    context "yielding [2..11]" $
        it "should be increased by 1" $
            runConduitPure ( yieldMany [1..10] .| mapC (+ 1) .| sinkList )
        `shouldBe` [2..11]

    context "with test set" $
          it "should be XYZ" $
            runConduitPure ( yieldMany testInputData .| mapC tickFields .| mapC fromTickData .| accumulate .| sinkList)
        `shouldBe` [
            OrderBook {bidVolume = Nothing, bidPrice = Nothing, askPrice = Nothing, askVolume = Nothing},
            OrderBook {bidVolume = Just $ Volume 10, bidPrice = Just $ Price 8938.0, askPrice = Nothing, askVolume = Nothing},
            OrderBook {bidVolume = Just $ Volume 10, bidPrice = Just $ Price 8938.0, askPrice = Just $ Price 8945.0, askVolume = Just $ Volume 5},
            OrderBook {bidVolume = Just $ Volume 10, bidPrice = Just $ Price 8938.0, askPrice = Just $ Price 8941.0, askVolume = Just $ Volume 4},
            OrderBook {bidVolume = Just $ Volume 11, bidPrice = Just $ Price 8940.0, askPrice = Just $ Price 8941.0, askVolume = Just $ Volume 4},
            OrderBook {bidVolume = Just $ Volume 11, bidPrice = Just $ Price 8940.0, askPrice = Just $ Price 8943.5, askVolume = Just $ Volume 2},
            OrderBook {bidVolume = Just $ Volume 11, bidPrice = Just $ Price 8940.0, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5},
            OrderBook {bidVolume = Just $ Volume 2, bidPrice = Just $ Price 8945.0, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5},
            OrderBook {bidVolume = Just $ Volume 40, bidPrice = Just $ Price 8933.0, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5},
            OrderBook {bidVolume = Just $ Volume 10, bidPrice = Just $ Price 8945.0, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5},
            OrderBook {bidVolume = Just $ Volume 6, bidPrice = Just $ Price 8940.0, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5},
            OrderBook {bidVolume = Just $ Volume 8, bidPrice = Just $ Price 8938.5, askPrice = Just $ Price 8950.0, askVolume = Just $ Volume 5}
        ]
