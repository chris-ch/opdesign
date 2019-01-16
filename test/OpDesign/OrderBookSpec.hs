{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module OpDesign.OrderBookSpec where

import SpecHelper

import Prelude (String, Int, Integer, Monad, Monoid, Ord, Num, fromInteger, mappend, read, zipWith, lines, drop, Maybe(..), IO, ($), (<*>), (<$>), (+), (-), (*), (>>))
import Data.Void (Void)
import Conduit (ConduitT, ResourceT)
import Conduit (yield, yieldMany, runConduit, runConduitPure, mapC, takeC, scanlC, foldlC, foldMapC, dropC, sumC, slidingWindowC, decodeUtf8C, sinkList)
import Conduit ((.|))

import Data.List (sum)
import Text.Show (show)
import Data.Time (UTCTime)
import qualified Data.Conduit.List as CL (scanl, scan, mapAccum, mapAccumM) 
import qualified Data.Conduit.Combinators as Cmb (print)
import qualified Conduit as DC (ZipSource(..), getZipSource)

import OpDesign.OrderBookStream (orderBookStream, scanl1C)

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

    context "transforming to string" $
        it "should produce strings" $
            runConduitPure ( yieldMany [1..10] .| mapC show .| sinkList )
        `shouldBe` ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]

    context "grouped values" $
        it "should yield many" $ 
            runConduitPure (yieldMany [[1, 2, 3], [4, 5], [6]] .| sinkList)
        `shouldBe` [[1, 2, 3], [4, 5], [6]]

    context "summing grouped values" $
        it "should yield sum of individual groups" $ 
            runConduitPure (yieldMany [[1, 2, 3], [4, 5], [6]] .|  mapC sum .| sinkList)
        `shouldBe` [6, 9, 6]

    context "yielding array of 10 first integers increased by 1" $
        it "should be increased by 1" $
            runConduitPure ( yieldMany [1..10] .| mapC (+ 1) .| sinkList )
        `shouldBe` [2..11]

    context "summing using foldlC" $
        it "should compute sum of integers up to 10" $
            runConduitPure ( yieldMany [1..10] .| foldlC (+) 0 )
        `shouldBe` 55

    context "yielding fibonnacci series" $
        it "should produce fibbonaci series" $
            runConduitPure ( yieldMany [1..10] .| scanlC (+) 0 .| (dropC 1 >> sinkList) )
        `shouldBe` [1, 3, 6, 10, 15, 21, 28, 36, 45, 55]

    context "yielding fibonnacci series using scanlC" $
        it "should produce fibbonaci series" $
            runConduitPure ( yieldMany [1..10] .| scanl1C (+) .| sinkList )
        `shouldBe` [1, 3, 6, 10, 15, 21, 28, 36, 45, 55]

    context "sliding window" $
        it "should create sliding windows" $
            runConduitPure ( yieldMany [1..10] .| slidingWindowC 4 .| sinkList )
        `shouldBe` [[1, 2, 3, 4], [2, 3, 4, 5], [3, 4, 5, 6], [4, 5, 6, 7], [5, 6, 7, 8], [6, 7, 8, 9], [7, 8, 9, 10]]

    context "sliding window" $
        it "should create sliding windows and compute sums of each group" $
            runConduitPure ( yieldMany [1..10 :: Int] 
                .| (slidingWindowC 4 :: (Monad m) => ConduitT Int [Int] m ())
                .| mapC sum
                .| sinkList )
        `shouldBe` [10, 14, 18, 22, 26, 30, 34]

    context "zipping sources" $
        let
            fibs :: [Int]
            fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)
        
            indexedFibs :: (Monad m) => ConduitT () (Int, Int) m ()
            indexedFibs = DC.getZipSource $ (,) <$> DC.ZipSource (yieldMany [1..]) <*> DC.ZipSource (yieldMany fibs)
        in
        it "should zip 2 sources" $
            runConduitPure ( indexedFibs .| takeC 10 .| sinkList )
        `shouldBe` [(1, 0), (2, 1), (3, 1), (4, 2), (5, 3), (6, 5), (7, 8), (8, 13), (9, 21), (10, 34)]

    context "creating a diff" $
        let
            input :: (Monad m) => ConduitT () Int m ()
            input = yieldMany [1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 1, 1]

            sliding2 :: (Monad m) => ConduitT Int [Int] m ()
            sliding2  = slidingWindowC 2

            diff :: [Int] -> Int
            diff (a:b:_) = b - a
            
            expected :: [Int]
            expected = [0, 0, 0, 1, 0, 0, 0, 1, 0, -2, 0]
        in
        it "should diff input" $
            runConduitPure ( input .| sliding2 .| mapC diff .| sinkList )
        `shouldBe` expected

    context "zipping stream with its own diff" $
        let
            input :: (Monad m) => ConduitT () Int m ()
            input = yieldMany [1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 1, 1]

            sliding2 :: (Monad m) => ConduitT Int [Int] m ()
            sliding2  = slidingWindowC 2

            diff :: [Int] -> Int
            diff (a:b:_) = b - a
            
            joined :: (Monad m) => ConduitT () (Int, Int) m ()
            joined = DC.getZipSource $ (,) <$> DC.ZipSource input <*> DC.ZipSource (yield 0 >> input .| sliding2 .| mapC diff)

            expected :: [(Int, Int)]
            expected = [(1, 0), (1, 0), (1, 0), (1, 0), (2, 1), (2, 0), (2, 0), (2, 0), (3, 1), (3, 0), (1, -2), (1, 0)]
        in
        it "should zip input with diff" $
            runConduitPure ( joined .| sinkList )
        `shouldBe` expected

    context "operator applied on two sources" $
        let
            input :: (Monad m) => ConduitT () Int m ()
            input = yieldMany [1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 1, 1]

            delta :: (Num a) => [a] -> a
            delta (m:n:_) = n - m
            
            shift :: (Monad m, Num a) => Int -> (ConduitT () a m ()) -> (ConduitT () a m ())
            shift count signal = yield (fromInteger 0) >> signal .| slidingWindowC (count + 1) .| mapC delta

            input1 = input
            input2 = shift 1 input

            operator :: (Monad m) => (a -> a -> a) -> (ConduitT () a m ()) -> (ConduitT () a m ()) -> (ConduitT () a m ())
            operator func signal1 signal2 = DC.getZipSource $ func <$> DC.ZipSource signal1 <*> DC.ZipSource signal2

            expected :: [Int]
            expected = [0, 0, 0, 0, 2, 0, 0, 0, 3, 0, -2, 0]
        in
        it "output = input * delta" $
            runConduitPure ( operator (*) input1 input2 .| sinkList )
        `shouldBe` expected

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
 
