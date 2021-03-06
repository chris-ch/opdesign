module OpDesign.TradingStrategySpec where

import Prelude (String, Monad, Maybe(..), Int, Rational)
import Prelude (read, lines, readFile, length)
import Prelude (($), (<=))

import Paths_opdesign (getDataFileName)

import SpecHelper

import Data.Timezones.TZ (tzEST)
import Data.Time (UTCTime)
import Conduit (ConduitT)
import Conduit (yieldMany, runConduitPure,mapC, sinkList)
import Conduit ((.|))

import Data.Conduit.Merge (zipUpdate)

import OpDesign.OrderBookStream (toOrderBook, toTickData)

testInputData1 :: [String]
testInputData1 = lines "\
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

testInputData2 :: [String]
testInputData2 = lines "\
\2014-10-28 06:49:10.000000,BEST_ASK,24.48,100.0,S\n\
\2014-10-28 06:51:32.000000,BEST_ASK,24.45,35.0,S\n\
\2014-10-28 06:51:24.000000,BEST_BID,24.39,14.0,S\n\
\2014-10-28 06:51:29.000000,BEST_BID,24.40,121.0,S\n\
\2014-10-28 06:51:40.000000,BEST_ASK,24.43,23.0,S\n\
\2014-10-28 06:51:41.000000,BEST_ASK,24.50,65.0,S\n\
\2014-10-28 06:52:26.000000,BEST_BID,24.45,62.0,S\n\
\2014-10-28 06:52:41.000000,BEST_BID,24.33,140.0,S\n\
\2014-10-28 06:52:46.000000,BEST_BID,24.45,220.0,S\n\
\2014-10-28 06:53:14.000000,BEST_BID,24.40,60.0,S\n\
\2014-10-28 06:53:25.000000,BEST_BID,24.38,78.0,S\n\
\"

processOrderBook :: OrderBook -> OrderBook
processOrderBook orderBook = orderBook

passThrough :: Monad m => ConduitT OrderBook OrderBook m ()
passThrough = mapC processOrderBook

testOB :: String -> Int -> Rational -> Rational -> Int -> OrderBook
testOB strDate vb pb pa va = OrderBook {date = (read strDate :: UTCTime), bidVolume = Just $ Volume vb, bidPrice = Just $ Price pb, askPrice = Just $ Price pa, askVolume = Just $ Volume va}

jTestOB :: String -> Int -> Rational -> Rational -> Int -> Maybe OrderBook
jTestOB strDate vb pb pa va = Just $ testOB strDate vb pb pa va

jTestPartialOB :: String -> Maybe Volume -> Maybe Price -> Maybe Price -> Maybe Volume -> Maybe OrderBook
jTestPartialOB strDate vb pb pa va = Just $ testPartialOB strDate vb pb pa va

jEmptyOrderBook :: UTCTime -> Maybe OrderBook
jEmptyOrderBook x = Just $ emptyOrderBook x

testPartialOB :: String -> Maybe Volume -> Maybe Price -> Maybe Price -> Maybe Volume -> OrderBook
testPartialOB strDate vb pb pa va = OrderBook {date = (read strDate :: UTCTime), bidVolume = vb, bidPrice = pb, askPrice = pa, askVolume = va}

spec :: Spec
spec = describe "Testing trading strategies" $ do

    context "with short test set 1" $
        it "should generate series of best order books" $
            runConduitPure ( yieldMany testInputData1 .| toTickData tzEST .| toOrderBook .| passThrough .| sinkList)
        `shouldBe` [
            testPartialOB "2014-10-28 11:50:00" (Just $ Volume 10)  (Just $ Price 8938.0) Nothing Nothing,
            testOB "2014-10-28 11:50:46" 10 8938.0 8945.0 5,
            testOB "2014-10-28 11:50:54" 10 8938.0 8941.0 4,
            testOB "2014-10-28 11:50:56" 11 8940.0 8941.0 4,
            testOB "2014-10-28 11:52:41" 11 8940.0 8943.5 2,
            testOB "2014-10-28 11:52:43" 11 8940.0 8950.0 5,
            testOB "2014-10-28 11:52:48" 2  8945.0 8950.0 5,
            testOB "2014-10-28 11:52:52" 40 8933.0 8950.0 5,
            testOB "2014-10-28 11:52:56" 10 8945.0 8950.0 5,
            testOB "2014-10-28 11:53:04" 6  8940.0 8950.0 5,
            testOB "2014-10-28 11:53:05" 8  8938.5 8950.0 5
           ]

    context "with short test set 2" $
        it "should generate series of best order books" $
            runConduitPure ( yieldMany testInputData2 .| toTickData tzEST .| toOrderBook .| passThrough .| sinkList)
        `shouldBe` [
            testPartialOB "2014-10-28 11:49:10" Nothing Nothing (Just $ Price 24.48) (Just $ Volume 100),
            testPartialOB "2014-10-28 11:51:32" Nothing Nothing (Just $ Price 24.45) (Just $ Volume 35),
            testOB "2014-10-28 11:51:24" 14  24.39 24.45 35,
            testOB "2014-10-28 11:51:29" 121 24.40 24.45 35,
            testOB "2014-10-28 11:51:40" 121 24.40 24.43 23,
            testOB "2014-10-28 11:51:41" 121 24.40 24.50 65,
            testOB "2014-10-28 11:52:26" 62  24.45 24.50 65,
            testOB "2014-10-28 11:52:41" 140 24.33 24.50 65,
            testOB "2014-10-28 11:52:46" 220 24.45 24.50 65,
            testOB "2014-10-28 11:53:14" 60  24.40 24.50 65,
            testOB "2014-10-28 11:53:25" 78  24.38 24.50 65
        ]

    context "with screener" $
        let
            product1 =  (yieldMany testInputData1 .| toTickData tzEST .| toOrderBook)
            product2 =  (yieldMany testInputData2 .| toTickData tzEST .| toOrderBook)
            sortOrderBooks ob1 ob2 = date ob1 <= date ob2

        in
        it "should multiplex order books" $
            runConduitPure ( zipUpdate sortOrderBooks product1 product2 .| sinkList)
        `shouldBe` [
            ( Nothing,                                                                                              jTestPartialOB "2014-10-28 11:49:10" Nothing Nothing (Just $ Price 24.48) (Just $ Volume 100)  ),
            ( jTestPartialOB "2014-10-28 11:50:00" (Just $ Volume 10)  (Just $ Price 8938.0) Nothing Nothing,       jTestPartialOB "2014-10-28 11:49:10" Nothing Nothing (Just $ Price 24.48) (Just $ Volume 100)  ),
            ( jTestOB "2014-10-28 11:50:46" 10 8938.0 8945.0 5,                                                     jTestPartialOB "2014-10-28 11:49:10" Nothing Nothing (Just $ Price 24.48) (Just $ Volume 100)  ),
            ( jTestOB "2014-10-28 11:50:54" 10 8938.0 8941.0 4,                                                     jTestPartialOB "2014-10-28 11:49:10" Nothing Nothing (Just $ Price 24.48) (Just $ Volume 100)  ),
            ( jTestOB "2014-10-28 11:50:56" 11 8940.0 8941.0 4,                                                     jTestPartialOB "2014-10-28 11:49:10" Nothing Nothing (Just $ Price 24.48) (Just $ Volume 100)  ),
            ( jTestOB "2014-10-28 11:50:56" 11 8940.0 8941.0 4,                                                     jTestPartialOB "2014-10-28 11:51:32" Nothing Nothing (Just $ Price 24.45) (Just $ Volume 35)   ),
            ( jTestOB "2014-10-28 11:50:56" 11 8940.0 8941.0 4,                                                     jTestOB "2014-10-28 11:51:24" 14  24.39 24.45 35                                               ),
            ( jTestOB "2014-10-28 11:50:56" 11 8940.0 8941.0 4,                                                     jTestOB "2014-10-28 11:51:29" 121 24.40 24.45 35                                               ),
            ( jTestOB "2014-10-28 11:50:56" 11 8940.0 8941.0 4,                                                     jTestOB "2014-10-28 11:51:40" 121 24.40 24.43 23                                               ),
            ( jTestOB "2014-10-28 11:50:56" 11 8940.0 8941.0 4,                                                     jTestOB "2014-10-28 11:51:41" 121 24.40 24.50 65                                               ),
            ( jTestOB "2014-10-28 11:50:56" 11 8940.0 8941.0 4,                                                     jTestOB "2014-10-28 11:52:26" 62  24.45 24.50 65                                               ),
            ( jTestOB "2014-10-28 11:52:41" 11 8940.0 8943.5 2,                                                     jTestOB "2014-10-28 11:52:41" 140 24.33 24.50 65                                               ),
            ( jTestOB "2014-10-28 11:52:43" 11 8940.0 8950.0 5,                                                     jTestOB "2014-10-28 11:52:41" 140 24.33 24.50 65                                               ),
            ( jTestOB "2014-10-28 11:52:43" 11 8940.0 8950.0 5,                                                     jTestOB "2014-10-28 11:52:46" 220 24.45 24.50 65                                               ),
            ( jTestOB "2014-10-28 11:52:48" 2  8945.0 8950.0 5,                                                     jTestOB "2014-10-28 11:52:46" 220 24.45 24.50 65                                               ),
            ( jTestOB "2014-10-28 11:52:52" 40 8933.0 8950.0 5,                                                     jTestOB "2014-10-28 11:52:46" 220 24.45 24.50 65                                               ),
            ( jTestOB "2014-10-28 11:52:56" 10 8945.0 8950.0 5,                                                     jTestOB "2014-10-28 11:52:46" 220 24.45 24.50 65                                               ),
            ( jTestOB "2014-10-28 11:53:04" 6  8940.0 8950.0 5,                                                     jTestOB "2014-10-28 11:52:46" 220 24.45 24.50 65                                               ),
            ( jTestOB "2014-10-28 11:53:05" 8  8938.5 8950.0 5,                                                     jTestOB "2014-10-28 11:52:46" 220 24.45 24.50 65                                               ),
            ( jTestOB "2014-10-28 11:53:05" 8  8938.5 8950.0 5,                                                     jTestOB "2014-10-28 11:53:14" 60  24.40 24.50 65                                               ),
            ( jTestOB "2014-10-28 11:53:05" 8  8938.5 8950.0 5,                                                     jTestOB "2014-10-28 11:53:25" 78  24.38 24.50 65                                               )
        ]

    context "access data file" $
        it "should access test data file" $
            do
                file <- getDataFileName "data/sampleticks/20150615.csv"
                content <- readFile file
                length content `shouldBe` 7037177