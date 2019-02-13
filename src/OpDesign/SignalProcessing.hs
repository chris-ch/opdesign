module OpDesign.SignalProcessing where

import Prelude (Int, Monad, Num, Double, Rational, Maybe(..), Bool)
import Prelude (replicate, pi, round, cycle, fromIntegral, fromInteger, sin, return, init, sum, zipWith)
import Prelude (($), (*), (++), (<*>), (<$>), (-), (+), (/), (>>))

import Conduit (ConduitT, Identity)
import Conduit (yield, yieldMany, mapC, slidingWindowC, evalStateC, await, mergeSource)
import Conduit ((.|))
import Data.Conduit.List (groupBy)

import Control.Monad.State (get, put, lift)
import Control.Monad.Trans.State.Strict (StateT)

import System.Random (randomRs, mkStdGen)

import qualified Conduit as DC (ZipSource(..), getZipSource)

type Signal a = ConduitT () a Identity ()
type Transfer a b = ConduitT a b Identity ()

shift :: (Num a) => Int -> Signal a -> Signal a
shift count signal = yield (fromInteger 0) >> signal .| slidingWindowC (count + 1) .| mapC delta
    where
        delta :: (Num a) => [a] -> a
        delta (m:n:_) = n - m 
        delta [_] = 0
        delta [] = 0 

operator :: (a -> a -> a) -> Signal a -> Signal a -> Signal a
operator func signal1 signal2 = DC.getZipSource $ func <$> DC.ZipSource signal1 <*> DC.ZipSource signal2

-- period is measured in number of samples
genSinusoid :: Int -> Int -> Signal Int
genSinusoid period amplitude = yieldMany [round ( (fromIntegral amplitude) * sin (scale period n) ) | n  <- [0..]]
    where
        scale :: Int -> Int -> Double
        scale m n = 2 * pi * fromIntegral n / fromIntegral m

-- delay is measured in number of samples
genStep :: Int -> Signal Int
genStep delay = yieldMany $ (replicate delay 0) ++ cycle [1]

-- countZero: counts how many samples are at zero
-- countOne: counts how many samples ar at one
genSquare :: Int -> Int -> Signal Int
genSquare countZero countOne = yieldMany $ cycle $ (replicate countZero 0) ++ (replicate countOne 1)

genConstant :: (Num a) => a -> Signal a
genConstant k = yieldMany $ cycle [k]

opNegate :: (Num a) => Signal a -> Signal a
opNegate = operator (-) (genConstant 0)

tfNegate :: (Num a) => Transfer a a
tfNegate =  mapC (\x -> -x) 

opAdd :: (Num a) => Signal a -> Signal a -> Signal a
opAdd input1 input2 = operator (+) input1 input2

opSub :: (Num a) => Signal a -> Signal a -> Signal a
opSub input1 input2 = operator (-) input1 input2

opMul :: (Num a) => Signal a -> Signal a -> Signal a
opMul input1 input2 = operator (*) input1 input2

type StateIntegrator = (Rational, Rational)
integratorC :: (Monad m) => ConduitT Rational Rational (StateT StateIntegrator m) ()
integratorC = do
        input <- await
        case input of
            Nothing -> return ()
            Just x1 -> do
                (y0, x0) <- lift get
                let y1 = y0 + (x1 + x0) / 2
                lift $ put (y1, x1)
                yield y1
                integratorC

tfIntegrate :: Rational -> Transfer Rational Rational
tfIntegrate initial = evalStateC (initial, initial) integratorC

type InputsIIR = [Rational]
type OutputsIIR = [Rational]
type StateIIR = (InputsIIR, OutputsIIR)
type CoefficientsInputsIIR = [Rational]
type CoefficientsOutputsIIR = [Rational]

filterIIRC :: (Monad m) => CoefficientsInputsIIR -> CoefficientsOutputsIIR -> ConduitT Rational Rational (StateT StateIIR m) ()
filterIIRC coeffsIn coeffsOut = do
        input <- await :: (Monad m) => ConduitT Rational Rational (StateT StateIIR m) (Maybe Rational)
        case input of
            Nothing -> return ()
            Just x -> do
                (prevInputs, prevOutputs) <- lift get :: (Monad m) => ConduitT Rational Rational (StateT StateIIR m) StateIIR
                let inputs = x : remainder where remainder = init prevInputs
                let y = sum (zipWith (*) inputs coeffsIn) + sum (zipWith (*) prevOutputs coeffsOut)
                let outputs = y : remainder where remainder = init prevOutputs
                lift $ put (inputs, outputs)
                yield y
                filterIIRC coeffsIn coeffsOut

tfIIR :: CoefficientsInputsIIR -> CoefficientsOutputsIIR -> StateIIR -> Transfer Rational Rational
tfIIR coeffsIn coeffsOut (initialIn, initialOut) = evalStateC (initialIn, initialOut) $ filterIIRC coeffsIn coeffsOut

-- random number generator
genRandom :: (Monad m) => (Int, Int) -> Int -> ConduitT () Int m ()
genRandom (bottom, top) seed = yieldMany (randomRs (bottom, top) (mkStdGen seed))

tfGroupBy :: (a -> a -> Bool) -> Transfer a [a]
tfGroupBy = groupBy

-- merging source into conduit
testMergeSource :: (Monad m) => ConduitT () i m ()  -> ConduitT a (i, a) m ()
testMergeSource = mergeSource