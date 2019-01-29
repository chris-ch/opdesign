{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module OpDesign.SignalProcessing where

import Prelude (IO, Int, Monad, Num, Double, Rational, Maybe(..), Fractional)
import Prelude (replicate, pi, round, cycle, map, fromIntegral, fromInteger, sin, return, init, sum, zipWith, take, fst)
import Prelude (($), (*), (++), (<*>), (<$>), (-), (+), (/), (>>), (.), (>>=))

import Conduit (ConduitT, Identity, PrimMonad, PrimState, ResourceT, Conduit)
import Conduit (yield, yieldMany, mapC, slidingWindowC, evalStateC, await, repeatMC, replicateMC, runConduit)
import Conduit ((.|))

import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, State, evalState, get, put, modify, lift)
import Control.Monad.Trans.State.Strict (StateT)

import System.Random (StdGen(..), split, newStdGen, randomR, getStdGen)

import qualified Conduit as DC (ZipSource(..), getZipSource)

type Signal a = ConduitT () a Identity ()
type Transfer a b = ConduitT a b Identity ()

shift :: (Num a) => Int -> Signal a -> Signal a
shift count signal = yield (fromInteger 0) >> signal .| slidingWindowC (count + 1) .| mapC delta
    where
        delta :: (Num a) => [a] -> a
        delta (m:n:_) = n - m 

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

type IntegratorState = (Rational, Rational)

integratorC :: (MonadState IntegratorState m) => ConduitT Rational Rational m ()
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

type IIR_Inputs = [Rational]
type IIR_Outputs = [Rational]
type IIR_State = (IIR_Inputs, IIR_Outputs)
type IIR_CoefficientsInputs = [Rational]
type IIR_CoefficientsOutputs = [Rational]

filterIIRC :: (Monad m) => IIR_CoefficientsInputs -> IIR_CoefficientsOutputs -> ConduitT Rational Rational (StateT IIR_State m) ()
filterIIRC coeffsIn coeffsOut = do
        input <- await :: (Monad m) => ConduitT Rational Rational (StateT IIR_State m) (Maybe Rational)
        case input of
            Nothing -> return ()
            Just x -> do
                (prevInputs, prevOutputs) <- lift get :: (Monad m) => ConduitT Rational Rational (StateT IIR_State m) IIR_State
                let inputs = x : remainder where remainder = init prevInputs
                let y = sum (zipWith (*) inputs coeffsIn) + sum (zipWith (*) prevOutputs coeffsOut)
                let outputs = y : remainder where remainder = init prevOutputs
                lift $ put (inputs, outputs)
                yield y
                filterIIRC coeffsIn coeffsOut

tfIIR :: IIR_CoefficientsInputs -> IIR_CoefficientsOutputs -> IIR_State -> Transfer Rational Rational
tfIIR coeffsIn coeffsOut (initialIn, initialOut) = evalStateC (initialIn, initialOut) $ filterIIRC coeffsIn coeffsOut


-- random number generator
genRandomGenerator :: (MonadIO m) => ConduitT () StdGen m ()
genRandomGenerator = do
    gen0 <- liftIO getStdGen
    loop gen0
    where loop gen = do
            let gen' = fst (split gen)
            yield gen
            loop gen'

genRandom :: (MonadIO m) => ConduitT () Int m ()
genRandom = (genRandomGenerator .| mapC process)
    where
        process :: StdGen -> Int
        process gen = fst $ randomR (40, 50) gen
