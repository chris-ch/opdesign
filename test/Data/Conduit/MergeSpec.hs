module Data.Conduit.MergeSpec where

import SpecHelper

import Prelude (Int, Integer, Maybe(..), Bool, Monad, Show, Eq)
import Prelude (zipWith, drop, maybe, return)
import Prelude (($), (<=), (>=))
import Conduit (ConduitT)
import Conduit (yieldMany, runConduitPure, takeC, mapC)
import Conduit (sinkList)
import Conduit ((.|))
import Data.Conduit.Internal (zipSources)

import Control.Monad.State (get, put, lift)
import Control.Monad.Trans.State.Strict (StateT)
import Conduit (yield, yieldMany, mapC, slidingWindowC, evalStateC, await, mergeSource)

import Data.Conduit.Merge (mergeC2) --, zipC2)

spec :: Spec
spec = describe "Testing Conduit merging operators" $ do

    context "yielding array of 10 first integers" $
        it "should yield many" $ 
            runConduitPure (yieldMany [1..10 :: Integer] .| sinkList)
        `shouldBe` [1..10]

    context "merging sources (assumes both are monotonic)" $
        let
            naturals1 = yieldMany [10, 20, 30, 40, 50, 60, 70, -10, -20, -10 :: Int]
            naturals2 = yieldMany [1, 2, 3, 4, 5 :: Int]
        in
        it "should zip 2 sources" $
            runConduitPure ( mergeC2 (<=) naturals1 naturals2 .| takeC 100 .| sinkList )
            `shouldBe` [1, 2, 3, 4, 5, 10, 20, 30, 40, 50, 60, 70, -10, -20, -10]

    context "merging monotonic sources with sorting" $
        let
            series1 = yieldMany [2, 4, 6, 8, 16 :: Int]
            series2 = yieldMany [1, 5, 6 :: Int]
        in
        it "should zip 2 sources" $
            runConduitPure ( (mergeTag (<=) series1 series2) .| sinkList )
            `shouldBe` [TaggedItem RightItem 1, TaggedItem LeftItem 2, TaggedItem LeftItem 4, 
                TaggedItem RightItem 5, TaggedItem LeftItem 6, TaggedItem RightItem 6, TaggedItem LeftItem 8, TaggedItem LeftItem 16]

    context "zipping sources as pairs" $
        let
            series1 = yieldMany [2, 4, 6, 8, 16 :: Int]
            series2 = yieldMany [1, 5, 6 :: Int]
        in
        it "should zip 2 sources" $
            runConduitPure ( (mergeTag (<=) series1 series2) .| pairTag .| sinkList )
            `shouldBe` [(Nothing, Just 1), (Just 2, Just 1), (Just 4, Just 1), (Just 4, Just 5), (Just 6, Just 5), (Just 6, Just 6), (Just 8, Just 6), (Just 16, Just 6)]

data MergeTag = LeftItem | RightItem deriving (Show, Eq)
data TaggedItem a = TaggedItem MergeTag a deriving (Show, Eq)
mergeTag :: (Monad m) => (a -> a -> Bool) -> ConduitT () a m () -> ConduitT () a m () -> ConduitT () (TaggedItem a) m ()
mergeTag func series1 series2 = mergeC2 (tagSort func) taggedSeries1 taggedSeries2
                where
                    taggedSeries1 = series1 .| mapC (\item -> TaggedItem LeftItem item)
                    taggedSeries2 = series2 .| mapC (\item -> TaggedItem RightItem item)
                    
tagSort :: (a -> a -> Bool) -> TaggedItem a -> TaggedItem a -> Bool
tagSort func (TaggedItem tag1 item1) (TaggedItem tag2 item2) = func item1 item2

type StateMergePair a = (Maybe a, Maybe a)
pairTagC :: (Monad m) => ConduitT  (TaggedItem a) (StateMergePair a) (StateT (StateMergePair a) m) ()
pairTagC = do
    input <- await
    case input of
        Nothing -> return ()
        Just taggedItem -> do
            stateMergePair <- lift get
            let outputState = updateStateMergePair taggedItem stateMergePair
            lift $ put outputState
            yield outputState -- put logic here
            pairTagC

updateStateMergePair :: TaggedItem a -> StateMergePair a -> StateMergePair a
updateStateMergePair (TaggedItem tag item) (Just leftItem, Just rightItem) = case tag of
    LeftItem -> (Just item, Just rightItem)
    RightItem -> (Just leftItem, Just item)

updateStateMergePair (TaggedItem tag item) (Nothing, Just rightItem) = case tag of
    LeftItem -> (Just item, Just rightItem)
    RightItem -> (Nothing, Just item)

updateStateMergePair (TaggedItem tag item) (Just leftItem, Nothing) = case tag of
    LeftItem -> (Just item, Nothing)
    RightItem -> (Just leftItem, Just item)

updateStateMergePair (TaggedItem tag item) (Nothing, Nothing) = case tag of
    LeftItem -> (Just item, Nothing)
    RightItem -> (Nothing, Just item)

pairTag :: (Monad m) => ConduitT  (TaggedItem a) (StateMergePair a) m ()
pairTag = evalStateC (Nothing, Nothing) pairTagC


-- Just x -> do
--     (prevInputs, prevOutputs) <- lift get :: (Monad m) => ConduitT Rational Rational (StateT StateIIR m) StateIIR
--     let inputs = x : remainder where remainder = init prevInputs
--     let y = sum (zipWith (*) inputs coeffsIn) + sum (zipWith (*) prevOutputs coeffsOut)
--     let outputs = y : remainder where remainder = init prevOutputs
--     lift $ put (inputs, outputs)
--     yield y
--     filterIIRC coeffsIn coeffsOut


-- type StateIntegrator = (Rational, Rational)
-- integratorC :: (Monad m) => ConduitT Rational Rational (StateT StateIntegrator m) ()
-- integratorC = do
--         input <- await
--         case input of
--             Nothing -> return ()
--             Just x1 -> do
--                 (y0, x0) <- lift get
--                 let y1 = y0 + (x1 + x0) / 2
--                 lift $ put (y1, x1)
--                 yield y1
--                 integratorC

-- tfIntegrate :: Rational -> Transfer Rational Rational
-- tfIntegrate initial = evalStateC (initial, initial) integratorC
