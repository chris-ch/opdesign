module OpDesign.SignalProcessingSpec where

import SpecHelper

import Prelude (Int, Integer, Rational, Monad, Maybe(..), Rational(..), Float)
import Prelude (zipWith, drop, maybe, return, fromIntegral, fromInteger, fromRational)
import Prelude (($), (<*>), (<$>), (+), (-), (/), (*), (>>), (>>=), (==))
import Control.Monad.State (State, evalState, get, put)

import Data.Ratio ((%))

import Conduit (ConduitT)
import Conduit (yield, yieldMany, runConduit, runConduitPure, mapC, takeC)
import Conduit (await, scanlC, foldlC, dropC, slidingWindowC, sinkList)
import Conduit ((.|))

import Data.List (sum)
import Text.Show (show)
import Data.Time()
import Data.Conduit.List()
import Data.Conduit.Combinators()
import qualified Conduit as DC (ZipSource(..), getZipSource)

import OpDesign.SignalProcessing (Transfer, Signal, genSinusoid, shift, shift', operator, genStep, genSquare, genConstant, genSequence)
import OpDesign.SignalProcessing (tfNegate, tfIntegrate, tfIIR, genRandom, tfGroupBy, tfCounter, tfScale)

spec :: Spec
spec = describe "Testing signal processing operators" $ do

    context "yielding array of 10 first integers" $
        it "should yield many" $ 
            runConduitPure (yieldMany [1..10 :: Integer] .| sinkList)
        `shouldBe` [1..10]

    context "transforming to string" $
        it "should produce strings" $
            runConduitPure ( yieldMany [1..10 :: Integer] .| mapC show .| sinkList )
        `shouldBe` ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]

    context "grouped values" $
        it "should yield many" $ 
            runConduitPure (yieldMany [[1, 2, 3 :: Integer], [4, 5 :: Integer], [6 :: Integer]] .| sinkList)
        `shouldBe` [[1, 2, 3], [4, 5], [6]]

    context "summing grouped values" $
        it "should yield sum of individual groups" $ 
            runConduitPure (yieldMany [[1, 2, 3 :: Integer], [4, 5 :: Integer], [6 :: Integer]] .|  mapC sum .| sinkList)
        `shouldBe` [6, 9, 6]

    context "grouping" $
        let
            input :: (Monad m) => ConduitT () Int m ()
            input = yieldMany [1, 1, 0, 1, 1, 1, 1, 0, 0]
            expected :: [[Int]]
            expected = [[1, 1], [0], [1, 1, 1, 1], [0, 0]]
        in
        it "should group identical values" $
            runConduitPure (input .| tfGroupBy (==) .| sinkList)
        `shouldBe` expected

    context "yielding array of 10 first integers increased by 1" $
        it "should be increased by 1" $
            runConduitPure ( yieldMany [1..10 :: Integer] .| mapC (+ 1) .| sinkList )
        `shouldBe` [2..11]

    context "summing using foldlC" $
        it "should compute sum of integers up to 10" $
            runConduitPure ( yieldMany [1..10 :: Integer] .| foldlC (+) 0 )
        `shouldBe` 55

    context "yielding sinusoidal sequence" $
        it "should generate predicted int sequence" $
            runConduitPure (genSinusoid 100 200 .| takeC 120 .| (dropC 116 >> sinkList))
        `shouldBe` [169, 175, 181, 186]

    context "yielding step signal" $
        it "should generate predicted int sequence" $
            runConduitPure (genStep 10 .| takeC 20 .| sinkList)
        `shouldBe` [0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1]

    context "yielding square signal" $
        it "should generate predicted int sequence" $
            runConduitPure (genSquare 8 2 .| takeC 22 .| sinkList)
        `shouldBe` [0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,1,1,0,0]

    context "yielding a constant signal" $
        it "should generate predicted int sequence" $
            runConduitPure (genConstant 2 .| takeC 22 .| sinkList)
        `shouldBe` [2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2 :: Integer]

    context "negates input signal" $
        it "should generate predicted int sequence" $
            runConduitPure (genSquare 8 2 .| tfNegate .| takeC 22 .| sinkList)
        `shouldBe` [0,0,0,0,0,0,0,0,-1,-1,0,0,0,0,0,0,0,0,-1,-1,0,0]

    context "sliding window" $
        it "should create sliding windows" $
            runConduitPure ( yieldMany [1..10 :: Integer] .| slidingWindowC 4 .| sinkList )
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
            diff [_] = 0
            diff [] = 0
            
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
            diff [_] = 0
            diff [] = 0
            
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

    context "shifting one item" $
        let
            input :: Signal (Maybe Int)
            input = yieldMany [Just 1, Just 2, Just 3, Just 4]

            expected :: [Maybe Int]
            expected = [Nothing, Just 1, Just 2, Just 3, Just 4]
        in
        it "shifted input" $
            runConduitPure ( input .| shift' Nothing .| sinkList )
        `shouldBe` expected

    context "mixing signal with a shift of itself" $
        let
            input1 :: Signal (Maybe Int)
            input1 = yieldMany [Just 1, Just 2, Just 3, Just 4, Just 5, Just 6]

            input2 :: Signal (Maybe Int)
            input2 = input1 .| shift' Nothing

            opAdd :: (Maybe Int) -> (Maybe Int) -> (Maybe Int)
            opAdd (Just a) (Just b) = Just (a + b)
            opAdd _ _ = Nothing

            expected = [Nothing, Just 3, Just 5, Just 7, Just 9, Just 11]
        in
        it "output = odd numbers" $
            runConduitPure ( operator opAdd input1 input2 .| sinkList )
        `shouldBe` expected

    context "loopback shifted signal into adder" $
        let
            input :: Signal (Maybe Int)
            input = yieldMany [Just 1, Just 2, Just 3, Just 4, Just 5, Just 6]

            output :: Signal (Maybe Int)
            output = operator opAdd input ( output .| shift' (Just 0) )

            opAdd :: (Maybe Int) -> (Maybe Int) -> (Maybe Int)
            opAdd (Just a) (Just b) = Just (a + b)
            opAdd _ _ = Nothing

            expected = [Just 1, Just 3, Just 6, Just 10, Just 15, Just 21]
        in
        it "y(n) = x(n) + y(n-1), y(0) = 0" $
            runConduitPure ( output .| sinkList )
        `shouldBe` expected

    context "integrating input" $
        let
            x :: Signal (Maybe Rational)
            x = genSinusoid 100 200 .| mapC (\val -> Just (fromIntegral val)) .| takeC 120
            
            y :: Signal (Maybe Rational)
            y = operator opAdd yPrev (operator opAdd x xPrev .| tfScale (5 % 10))

            xPrev = x .| shift' (Just 0)
            yPrev = y .| shift' (Just 0)

            opAdd :: (Maybe Rational) -> (Maybe Rational) -> (Maybe Rational)
            opAdd (Just a) (Just b) = Just (a + b)
            opAdd _ _ = Nothing

            toFloat :: Transfer (Maybe Rational) Float
            toFloat = do
                maybeVal <- await
                case maybeVal of
                    Nothing -> return ()
                    Just val -> case val of
                        Nothing -> toFloat
                        Just val2 -> do
                            yield (fromRational val2)
                            toFloat
                        
            expected = [0.1, 0.2]
        in
        it "y = y_prev + 0.5 * (x + x_prev), y_0 = 0" $
            runConduitPure ( y .| toFloat .| sinkList )
        `shouldBe` expected

    context "IIR filter with scanlC" $
        let
            input :: (Monad m) => ConduitT () Int m ()
            input = yieldMany [1, 1, 1, 1, 1, 1, 1, 1]

            integrate :: (Monad m) => ConduitT Int Int m ()
            integrate = await >>= maybe (return ()) (scanlC (+))

            expected :: [Int]
            expected = [1, 2, 3, 4, 5, 6, 7, 8]
        in
        it "output = integ(input)" $ do
            res <- runConduit ( input .| integrate .| sinkList )
            res `shouldBe` expected

    context "Integrator as IIR filter y_1 = y_0 + 0.5 * (x_1 + x_0)" $
        let
            integrator :: [Rational] -> State (Rational, Rational) Rational
            integrator [] = do
                (y_0, _) <- get
                return y_0
                
            integrator (x_1:xs) = do
                (y_0, x_0) <- get
                let y_1 = y_0 + (x_1 + x_0) / 2
                put (y_1, x_1)
                integrator xs
                
            expected = 7.5
        in
        it "shows result according to state" $ do
            let final = evalState (integrator [2, 2, 2, 1, 1, 1, -1, -1]) (0, 0)
            final `shouldBe` expected

    context "Counter using State and Conduit" $
        let
            input :: (Monad m) => ConduitT () Int m ()
            input = yieldMany [1, 1, 1, 1, 1, 1, 1, 1]
            expected = [1, 2, 3, 4, 5, 6, 7, 8]
        in
        it "shows result according to state" $
            (runConduitPure ( input .| tfCounter 0 .| sinkList ))
        `shouldBe` expected

    context "Integrator using State and Conduit" $
        let
            input :: (Monad m) => ConduitT () Rational m ()
            input = yieldMany [2, 2, 2, 1, 1, 1, -1, -1 :: Rational]

            expected = [1, 3, 5, 6.5, 7.5, 8.5, 8.5, 7.5 :: Rational]
        in
        it "shows result according to state" $
            (runConduitPure ( input .| tfIntegrate (0 :: Rational) .| sinkList ))
        `shouldBe` expected

    context "Integrator using IIR filter" $
        let
            input :: (Monad m) => ConduitT () Rational m ()
            input = yieldMany [2, 2, 2, 1, 1, 1, -1, -1]

            filterIIR = tfIIR [1] [1] ([0], [0])

            expected = [2, 4, 6, 7, 8, 9, 8, 7 :: Rational]
        in
        it "shows result according to state" $
            (runConduitPure ( input .| filterIIR .| sinkList ))
        `shouldBe` expected

    context "Trapezoidal integrator using IIR filter" $
        let
            input :: (Monad m) => ConduitT () Rational m ()
            input = yieldMany [2, 2, 2, 1, 1, 1, -1, -1]

            filterIIR = tfIIR [0.5, 0.5] [1] ([0, 0], [0])

            expected = [1, 3, 5, 6.5, 7.5, 8.5, 8.5, 7.5 :: Rational]
        in
        it "shows result according to state" $
            (runConduitPure ( input .| filterIIR .| sinkList ))
        `shouldBe` expected

    context "yielding random sequence" $
        let
            expected = [46,50,42,42,40,42,47,47,48,41]
        in
        it "should generate predicted int sequence" $ do
            x <- (runConduit (genRandom (40, 50) 2 .| takeC 10 .| sinkList))
            x `shouldBe` expected

    context "sequencing int" $
        let
            expected = [3, 4, 5, 6, 7]
        in
        it "should count from 3 up to 7" $ do
            (runConduitPure (genSequence 3 .| takeC 5 .| sinkList))
        `shouldBe` expected
