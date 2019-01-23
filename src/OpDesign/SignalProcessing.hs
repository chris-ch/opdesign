{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module OpDesign.SignalProcessing where

import Prelude (IO, Int, Monad, Num, Double, Rational, Maybe(..), Fractional)
import Prelude (replicate, pi, round, cycle, map, fromIntegral, fromInteger, sin, return)
import Prelude (($), (*), (++), (<*>), (<$>), (-), (+), (/), (>>))
import Conduit (ConduitT, Identity)
import Conduit (yield, yieldMany, mapC, slidingWindowC, evalStateC, await)
import Conduit ((.|))
import Control.Monad.State (MonadState, State, evalState, get, put, modify, lift)
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

type IIR_5_5_State = (Rational, Rational, Rational, Rational, Rational, Rational, Rational, Rational, Rational, Rational)

filterIIRC :: (MonadState IIR_5_5_State m) => (Rational, Rational, Rational, Rational, Rational) -> (Rational, Rational, Rational, Rational, Rational) -> ConduitT Rational Rational m ()
filterIIRC (a0, a1, a2, a3, a4) (b0, b1, b2, b3, b4) = do
        input <- await
        case input of
            Nothing -> return ()
            Just x -> do
                (y0, y1, y2, y3, y4, x0, x1, x2, x3, x4) <- lift get
                let y = a0 * y0 + a1 * y1 + a2 * y2 + a3 * y3 + a4 * y4 + b0 * x0 + b1 * x1 + b2 * x2 + b3 * x3 + b4 * x4
                lift $ put (y, y0, y1, y2, y3, x, x0, x1, x2, x3)
                yield y
                filterIIRC (a0, a1, a2, a3, a4) (b0, b1, b2, b3, b4)

tfIIR :: (Rational, Rational, Rational, Rational, Rational) -> (Rational, Rational, Rational, Rational, Rational) -> Rational -> Transfer Rational Rational
tfIIR (a0, a1, a2, a3, a4) (b0, b1, b2, b3, b4) initial = evalStateC (initial, initial, initial, initial, initial, initial, initial, initial, initial, initial) $ filterIIRC (a0, a1, a2, a3, a4) (b0, b1, b2, b3, b4)
