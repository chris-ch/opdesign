module OpDesign.SignalProcessing where

import Prelude (Int, Monad, Applicative, Num, Double, Rational, Maybe(..), Bool(..), Integer, Show, Eq, Functor)
import Prelude (replicate, pi, round, cycle, fromIntegral, sin, return, init, sum, zipWith, fmap, pure)
import Prelude (($), (*), (++), (<*>), (<$>), (-), (+), (/), (>>=))

import Conduit (ConduitT, Identity)
import Conduit (yield, yieldMany, mapC, evalStateC, await, mergeSource)
import Conduit ((.|))
import Data.Conduit.List (groupBy)

import Control.Monad.State (get, put, lift, modify, MonadState)
import Control.Monad.Trans.State.Strict (StateT)

import System.Random (randomRs, mkStdGen)

import qualified Conduit as DC (ZipSource(..), ZipConduit(..), getZipSource, getZipConduit)

type Generator a = ConduitT () a Identity ()
type Transfer a b = ConduitT a b Identity ()

data Signal a = Undefined | Signal a deriving (Show, Eq)

instance Functor Signal where
    fmap f signal = case signal of
        Undefined       -> Undefined
        Signal value    -> Signal $ f value

instance Applicative Signal where
      pure = Signal
      Undefined <*> _ = Undefined
      (Signal f) <*> something = fmap f something

instance Monad Signal where
    return = Signal
    signal >>= f = case signal of
        Undefined       -> Undefined
        Signal value    -> f value

shift :: (Signal a) -> Transfer (Signal a) (Signal a)
shift initItem = do
    yield initItem
    keepGoing
    where
        keepGoing = do
            maybeItem <- await
            case maybeItem of
                Nothing -> return ()
                Just item -> do
                    yield $ item
                    keepGoing

operator :: (a -> a -> a) -> Generator (Signal a) -> Generator (Signal a) -> Generator(Signal a)
operator func source1 source2 = DC.getZipSource $ applyFunc <$> DC.ZipSource source1 <*> DC.ZipSource source2
                    where
                        applyFunc (Signal a) (Signal b) = Signal (func a b)
                        applyFunc _ _ = Undefined

-- operator' :: (a -> a -> a) -> Transfer (Signal a) (Signal a)-> Transfer (Signal a) (Signal a) -> Generator (Signal a)
-- operator' func source1 source2 = DC.getZipConduit $ applyFunc <$> DC.ZipConduit source1 <*> DC.ZipConduit source2
--                     where
--                         applyFunc (Signal a) (Signal b) = Signal (func a b)
--                         applyFunc _ _ = Undefined

-- period is measured in number of samples
genSinusoid :: Int -> Int -> Generator (Signal Int)
genSinusoid period amplitude = yieldMany [round ( (fromIntegral amplitude) * sin (scale period n) ) | n  <- [0..]] .| mapC Signal
    where
        scale :: Int -> Int -> Double
        scale m n = 2 * pi * fromIntegral n / fromIntegral m

-- delay is measured in number of samples
genStep :: Int -> Generator (Signal Int)
genStep delay = (yieldMany ((replicate delay 0) ++ cycle [1])) .| mapC Signal

-- countZero: counts how many samples are at zero
-- countOne: counts how many samples ar at one
genSquare :: Int -> Int -> Generator (Signal Int)
genSquare countZero countOne = (yieldMany $ cycle $ (replicate countZero 0) ++ (replicate countOne 1)) .| mapC Signal

genConstant :: (Num a) => a -> Generator (Signal a)
genConstant k = (yieldMany $ cycle [k]) .| mapC Signal

tfNegate :: (Num a) => Transfer (Signal a) (Signal a)
tfNegate =  mapC opposite where
        opposite Undefined = Undefined
        opposite (Signal value) = Signal (-value)

opAdd :: (Num a) => Generator (Signal a) -> Generator (Signal a) -> Generator (Signal a)
opAdd input1 input2 = operator (+) input1 input2

opSub :: (Num a) => Generator (Signal a) -> Generator (Signal a) -> Generator (Signal a)
opSub input1 input2 = operator (-) input1 input2

opMul :: (Num a) => Generator (Signal a) -> Generator (Signal a) -> Generator (Signal a)
opMul input1 input2 = operator (*) input1 input2

tfScale :: (Num a) => a -> Transfer (Signal a) (Signal a)
tfScale scale = mapC (\x -> case x of
    Signal value -> Signal (scale * value)
    Undefined -> Undefined
    ) 

-- random number generator
genRandom :: (Int, Int) -> Int -> Generator (Signal Int)
genRandom (bottom, top) seed = (yieldMany (randomRs (bottom, top) (mkStdGen seed))) .| mapC Signal

tfGroupBy :: (a -> a -> Bool) -> Transfer (Signal a) [Signal a]
tfGroupBy func = (groupBy grouper)
    where
        grouper (Signal val1) (Signal val2) = func val1 val2
        grouper Undefined Undefined = True
        grouper _ _ = False

-- merging source into conduit
testMergeSource :: (Monad m) => ConduitT () i m ()  -> ConduitT a (i, a) m ()
testMergeSource = mergeSource

counterC :: (MonadState b m, Num a, Num b) => ConduitT a b m ()
counterC = do
    x0 <-  await
    case x0 of
        Nothing -> return ()
        Just _ -> do
            lift $ modify (+1)
            r <- lift get
            yield r
            counterC

tfCounter :: (Num a) => Integer -> Transfer a Integer
tfCounter start = evalStateC start counterC

genSequence :: (Num a) => a -> Generator (Signal a)
genSequence nextVal = do
    yield $ Signal nextVal
    genSequence (nextVal + 1)

convertC :: (a -> b) -> Transfer (Signal a) (Signal b)
convertC fromType = mapC converter where
    converter signal = case signal of
        Undefined -> Undefined
        Signal value -> Signal (fromType value)


----------------------------------

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
