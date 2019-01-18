{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module OpDesign.SignalProcessingSpec where

import SpecHelper

import Prelude (String, Int, Integer, Monad, Monoid, Ord, Num, fromInteger, mappend, read, zipWith, last, drop, Maybe(..), IO, ($), (<*>), (<$>), (+), (-), (*), (>>))
import Data.Void (Void)
import Conduit (ConduitT, ResourceT)
import Conduit (yield, yieldMany, runConduit, runConduitPure, mapC, takeC, lastC, scanlC, foldlC, foldMapC, dropC, sumC, slidingWindowC, sinkList)
import Conduit ((.|))

import Data.List (sum)
import Text.Show (show)
import Data.Time (UTCTime)
import qualified Data.Conduit.List as CL (scanl, scan, mapAccum, mapAccumM) 
import qualified Data.Conduit.Combinators as Cmb (print)
import qualified Conduit as DC (ZipSource(..), getZipSource)

import OpDesign.SignalProcessing (Signal, sinusoidal, shift, operator)

spec :: Spec
spec = describe "Testing signal processing operators" $ do

    context "simple" $
        it "should yield 3" $ 
            1 + 2 
        `shouldBe` 3

    context "yielding sinusoidal sequence" $
        it "should generate predited int sequence" $
            runConduitPure (sinusoidal 100 200 .| takeC 120 .| (dropC 116 >> sinkList))
        `shouldBe` [169, 175, 181, 186]

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
            input1 :: Signal Int
            input1 = yieldMany [1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 1, 1]

            input2 :: Signal Int
            input2 = shift 1 input1

            expected :: [Int]
            expected = [0, 0, 0, 0, 2, 0, 0, 0, 3, 0, -2, 0]
        in
        it "output = input * delta" $
            runConduitPure ( operator (*) input1 input2 .| sinkList )
        `shouldBe` expected
